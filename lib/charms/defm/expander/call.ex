defmodule Charms.Defm.Expander.Call do
  @moduledoc """
  Expand function calls in Charms defm.
  """
  require Logger
  use Beaver
  alias Charms.Defm.Expander
  alias Charms.Defm.Expander.Std
  alias Charms.Defm.Expander.Dialect
  alias Charms.Defm.Expander.Intrinsic
  alias Charms.Defm.Expander.Resolver
  alias Charms.Struct
  alias MLIR.Dialect.Func
  alias MLIR.Attribute
  require Func
  import Charms.Diagnostic, only: :macros

  defp expand_module_var(module, fun, state, env) do
    with {dialect, _, _} <- module,
         true <- is_atom(dialect) and is_atom(fun),
         true <- Atom.to_string(dialect) |> String.match?(~r/^[[:lower:]]/u),
         true <- MapSet.member?(state.mlir.available_ops, "#{dialect}.#{fun}") do
      {{:dialect, dialect}, state, env}
    else
      _ ->
        Expander.expand(module, state, env)
    end
  end

  @doc "Expands a remote call like `Module.function(arg1, arg2)`"
  def expand_remote({{:., dot_meta, [module, fun]}, meta, args}, state, env)
      when is_atom(fun) and is_list(args) do
    env = %{env | line: dot_meta[:line] || meta[:line] || env.line}

    # Check if it is a dialect operation call like `arith.addi(...)`
    # skip normal remote expansion to prevent being expanded as variable
    {module, state, env} = expand_module_var(module, fun, state, env)

    arity = length(args)
    mfa = {module, fun, arity}

    case module do
      module when is_atom(module) ->
        {return, state, env} = dispatch_remote_call(meta, mfa, args, state, env)
        state = update_in(state.remotes, &[mfa | &1])
        {return, state, env}

      %MLIR.Value{} = struct ->
        Struct.expand_extract_field(struct, fun, state, env)

      {:dialect, dialect} when is_atom(fun) ->
        op = fun

        Dialect.expand(dialect, op, args, [], state, env)
        |> then(fn {op, state, env} -> {MLIR.Operation.results(op), state, env} end)

      {:undefined_variable, dialect} when is_atom(dialect) and is_atom(fun) ->
        op = fun

        Dialect.expand(dialect, op, args, [], state, env)
        |> then(fn {op, state, env} -> {MLIR.Operation.results(op), state, env} end)
    end
  end

  @doc "Expands a local or imported call like `function(arg1, arg2)`"
  def expand_local_or_import({fun, meta, args}, state, env)
      when is_atom(fun) and is_list(args) do
    env = %{env | line: meta[:line] || env.line}
    arity = length(args)

    case Macro.Env.expand_import(env, meta, fun, arity,
           trace: true,
           allow_locals: false,
           check_deprecations: true
         ) do
      {:macro, module, callback} ->
        # This still calls back to the main expander's macro handler
        Expander.expand_macro(meta, module, fun, args, callback, state, env)

      {:function, module, fun} ->
        expand_function(meta, module, fun, args, state, env)

      {:error, :not_found} ->
        expand_local(meta, fun, args, state, env)
    end
  end

  defp create_call_with_resolver(mod, name, args, state, env, infer_arg_fun, infer_return_fun) do
    arity = length(args)

    args =
      for {arg, index} <- Enum.with_index(args) do
        Resolver.resolve_argument_type!(infer_arg_fun, {mod, name, arity}, index, arg, state, env)
      end

    types = Resolver.resolve_return_type!(infer_return_fun, {mod, name, arity}, state, env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      Func.call(args ++ [callee: Attribute.flat_symbol_ref(Charms.Defm.mangling(mod, name))],
        loc: MLIR.Location.from_env(env)
      ) >>> types
    end
    |> then(&{&1, state, env})
  end

  defp expand_call(
         module,
         :%,
         [mod, {:%{}, _struct_meta, [{:|, _, [struct_ast, fields]}]}],
         state,
         env
       )
       when is_atom(mod) do
    {struct, state, env} = Expander.expand(struct_ast, state, env)
    Struct.expand_update_struct(module, mod, struct, fields, state, env)
  end

  defp expand_call(module, :%, [mod, {:%{}, _struct_meta, fields}], state, env)
       when is_atom(mod) do
    Struct.expand_new_struct(module, mod, fields, state, env)
  end

  defp expand_call(mod, name, args, state, env) do
    arity = length(args)

    # By elixir's evaluation order, we should expand the arguments first even the function is not found.

    {args, state, env} = Expander.expand_list(args, state, env)
    {args, state, env} = validate_call_args(args, state, env)

    cond do
      # remote call
      mod != env.module and match?({:module, _}, Code.ensure_compiled(mod)) and Code.loaded?(mod) and
          function_exported?(mod, :__ir__, 0) ->
        state = update_in(state.remotes, &[{mod, name, arity} | &1])

        create_call_with_resolver(
          mod,
          name,
          args,
          state,
          env,
          &mod.infer_argument_type/3,
          &mod.infer_return_type/2
        )

      # local call
      mod == env.module || state.mlir.allow_undefined_function ->
        create_call_with_resolver(
          mod,
          name,
          args,
          state,
          env,
          state.mlir.infer_argument_type,
          state.mlir.infer_return_type
        )

      true ->
        raise_compile_error(
          env,
          "can't resolve #{Exception.format_mfa(mod, name, arity)}, maybe the module is not aliased properly?"
        )
    end
  end

  def validate_call_args(args, state, env) do
    for arg <- args, reduce: {[], state, env} do
      {args, state, env} ->
        case arg do
          %MLIR.Value{} ->
            {[arg | args], state, env}

          %MLIR.Operation{} = arg_op ->
            op = MLIR.Operation.name(arg_op)

            if op == "func.call" do
              callee = Beaver.Walker.attributes(arg_op)["callee"]

              raise_compile_error(
                env,
                "Function call #{to_string(callee) || "(unknown callee)"} does not return a value"
              )
            else
              raise_compile_error(env, "Cannot use operation #{op} as an argument")
            end

          {:undefined_variable, name} when is_atom(name) ->
            raise_compile_error(env, ~s{undefined variable "#{name}"})

          invalid_arg ->
            {arg, state, env} = state.mlir.expand_arg_fallback.(invalid_arg, state, env)
            {[arg | args], state, env}
        end
    end
    |> then(fn {args, state, env} -> {Enum.reverse(args), state, env} end)
  end

  defp expand_get_attribute_as_runtime_attr(env_ptr, attr, state, env) do
    quote do
      alias Charms.Pointer
      alias Charms.Term
      term_ptr = Pointer.allocate(Term.t())
      size = String.length(attr)
      size = value index.casts(size) :: i64()
      zero = const 0 :: i32()
      enif_binary_to_term(env_ptr, attr, size, term_ptr, zero)
      Pointer.load(Term.t(), term_ptr)
    end
    |> Expander.expand_with_bindings(state, env, attr: attr, env_ptr: env_ptr)
  end

  defp expand_special_call(_loc, {module, fun, _arity} = {String, :length, 1}, args, state, env) do
    {args, state, env} = Expander.expand(args, state, env)
    Std.expand(module, fun, args, state, env)
  end

  defp expand_special_call(_loc, {module, fun, _arity} = {Enum, :reduce, 3}, args, state, env) do
    Std.expand(module, fun, args, state, env)
  end

  defp expand_special_call(_loc, {Module, :__get_attribute__, 4}, args, state, env) do
    {args, _state, _env} = Expander.expand(args, state, env)
    attr = apply(Module, :__get_attribute__, args)
    env_ptr = beam_env_from_defm(env, state)

    if env_ptr do
      # if we are in a defm with env, we convert the attribute to enif term at runtime
      {attr, _state, _env} = attr |> :erlang.term_to_binary() |> Expander.expand(state, env)
      expand_get_attribute_as_runtime_attr(env_ptr, attr, state, env)
    else
      # otherwise we just return the attribute as a literal so it can be consumed by intrinsics
      {attr, state, env}
    end
  end

  defp expand_special_call(loc, {module, fun, arity}, args, state, env) do
    cond do
      Expander.export_intrinsics?(module, fun, arity) ->
        intrinsic_impl = module.__intrinsics__(fun, arity)
        Intrinsic.expand(loc, module, intrinsic_impl, args, state, env)

      (struct_type =
         Charms.Struct.retrieve_struct_type(state.mlir.mod)) && module == env.module && fun == :t &&
          arity == 0 ->
        {struct_type, state, env}

      true ->
        expand_call(module, fun, args, state, env)
    end
  end

  defp dispatch_remote_call(meta, {module, fun, arity} = mfa, args, state, env) do
    loc = MLIR.Location.from_env(env)

    case Macro.Env.expand_require(env, meta, module, fun, arity,
           trace: true,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        Expander.expand_macro(meta, module, fun, args, callback, state, env)

      :error ->
        expand_special_call(loc, mfa, args, state, env)
    end
  end

  defp expand_function(_meta, module, fun, args, state, env) do
    # A compiler may want to emit a :remote_function trace in here.
    state = update_in(state.remotes, &[{module, fun, length(args)} | &1])
    loc = MLIR.Location.from_env(env)

    if Expander.export_intrinsics?(module, fun, length(args)) do
      intrinsic_impl = module.__intrinsics__(fun, length(args))
      Intrinsic.expand(loc, module, intrinsic_impl, args, state, env)
    else
      expand_call(module, fun, args, state, env)
    end
  end

  defp expand_local(_meta, fun, args, state, env) do
    # A compiler may want to emit a :local_function trace in here.
    state = update_in(state.locals, &[{fun, length(args)} | &1])
    {args, state, env} = Expander.expand_list(args, state, env)

    try do
      expand_call(env.module, fun, args, state, env)
    rescue
      e ->
        raise_compile_error(
          env,
          "Failed to expand local function #{fun}/#{length(args)}: #{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
        )
    end
  end

  def beam_env_from_defm(_env, state) do
    state.mlir.enif_env
  end

  def beam_env_from_defm!(env, state) do
    if e = beam_env_from_defm(env, state) do
      e
    else
      raise_compile_error(env, "must be a defm with beam env as the first argument")
    end
  end
end
