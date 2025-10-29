defmodule Charms.Defm.Expander.Call do
  @moduledoc """
  Expand function calls in Charms defm.
  """
  use Beaver
  alias Charms.Defm.Expander
  alias MLIR.Dialect.{Func, SCF, MemRef, Index, UB, LLVM}
  alias MLIR.{Attribute, Location}
  require Func
  import Charms.Diagnostic, only: :macros

  @doc "Expands a remote call like `Module.function(arg1, arg2)`"
  def expand_remote({{:., dot_meta, [module, fun]}, meta, args}, state, env)
      when is_atom(fun) and is_list(args) do
    env = %{env | line: dot_meta[:line] || meta[:line] || env.line}
    {module, state, env} = Expander.expand(module, state, env)
    arity = length(args)
    mfa = {module, fun, arity}
    state = update_in(state.remotes, &[mfa | &1])
    loc = Location.from_env(env)

    case module do
      module when is_atom(module) ->
        expand_remote_macro(meta, mfa, args, state, env)

      %MLIR.Value{} = struct ->
        mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
          struct_type = MLIR.Value.type(struct)

          unless MLIR.Type.llvm_struct?(struct_type) do
            raise_compile_error(
              env,
              "Expected a struct type, got: #{MLIR.to_string(struct_type)}"
            )
          end

          defining_module =
            MLIR.CAPI.mlirLLVMStructTypeGetIdentifier(struct_type)
            |> MLIR.to_string()
            |> String.to_atom()

          {dependence, state} =
            if defining_module == env.module do
              {state.mlir.mod, state}
            else
              fetch_dependence_module(defining_module, state)
            end

          position = Charms.Struct.position_of_field!(env, dependence, fun)
          elem_t = MLIR.CAPI.mlirLLVMStructTypeGetElementType(struct_type, position[0])
          field_value = LLVM.extractvalue(struct, position: position, loc: loc) >>> elem_t
          {field_value, state, env}
        end

      _ ->
        [{dialect, _, _}, op] = [module, fun]
        expand_call_as_op(dialect, op, args, state, env)
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
        expand_remote(meta, module, fun, args, state, env)

      {:error, :not_found} ->
        expand_local(meta, fun, args, state, env)
    end
  end

  defp create_call(mod, name, args, types, state, env) do
    op =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        %Beaver.SSA{
          op: "func.call",
          arguments: args ++ [callee: Attribute.flat_symbol_ref(Charms.Defm.mangling(mod, name))],
          ctx: Beaver.Env.context(),
          blk: Beaver.Env.block(),
          loc: MLIR.Location.from_env(env)
        }
        |> Beaver.SSA.put_results(types)
        |> MLIR.Operation.create()
      end

    {MLIR.Operation.results(op), state, env}
  end

  defp create_poison(msg, state, env) do
    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      UB.poison(msg: MLIR.Attribute.string(msg), loc: MLIR.Location.from_env(env)) >>>
        ~t{none}
    end
    |> then(&{&1, state, env})
  end

  # Cache and return a dependence module
  defp fetch_dependence_module(module, state) do
    update_in(
      state.mlir.dependence_modules,
      &Map.put_new_lazy(&1, module, fn ->
        if function_exported?(module, :__ir__, 0) do
          MLIR.Module.create!(module.__ir__(), ctx: state.mlir.ctx)
          |> MLIR.Operation.from_module()
        else
          nil
        end
      end)
    )
    |> then(&{&1.mlir.dependence_modules[module], &1})
  end

  # check if function type's result type is compatible with the annotation
  defp do_resolve_return_type([t], [rt], env) do
    if MLIR.equal?(t, rt) do
      rt
    else
      raise_compile_error(
        env,
        "mismatch type in invocation: #{to_string(t)} vs. #{to_string(rt)}"
      )
    end
  end

  defp do_resolve_return_type([], [rt], _env) do
    rt
  end

  defp do_resolve_return_type(types, [], _env) do
    types
  end

  defp resolve_return_type!(ft, types, env) do
    expected_types =
      case MLIR.CAPI.mlirFunctionTypeGetNumResults(ft) |> Beaver.Native.to_term() do
        0 -> []
        1 -> [MLIR.CAPI.mlirFunctionTypeGetResult(ft, 0)]
      end

    types |> List.wrap() |> do_resolve_return_type(expected_types, env) |> List.wrap()
  end

  defp infer_by_lookup(env, dependence, mod, name, types) do
    symbol_table = dependence |> MLIR.CAPI.mlirSymbolTableCreate()

    sym =
      MLIR.CAPI.mlirSymbolTableLookup(
        symbol_table,
        MLIR.StringRef.create(Charms.Defm.mangling(mod, name))
      )

    if MLIR.null?(sym) do
      raise_compile_error(
        env,
        "function #{name} not found in module #{inspect(mod)}"
      )
    else
      if MLIR.Operation.name(sym) == "func.func" do
        sym[:function_type] |> MLIR.Attribute.unwrap() |> resolve_return_type!(types, env)
      else
        raise_compile_error(env, "symbol #{name} is not a function")
      end
    end
  end

  defp infer_by_resolving(env, name, types, state) do
    # resolve and check if compatible
    with resolver when not is_nil(resolver) and is_function(resolver, 0) <-
           state.mlir.return_types[name],
         {[resolved_t], [t]} <- {resolver.(), List.wrap(types)} do
      if MLIR.equal?(resolved_t, t) do
        types
      else
        raise_compile_error(
          env,
          "function #{name} has an incompatible return type #{to_string(t)}"
        )
      end
    else
      # fail to resolve, return the types as is
      nil ->
        types

      # use resolved type
      {resolved_types, []} ->
        resolved_types
    end
  end

  # expand call and prefix with module if it is a local
  defp decompose_and_expand_call(call, state, env) do
    case Macro.decompose_call(call) do
      {alias, f, args} ->
        {mod, state, env} = Expander.expand(alias, state, env)
        {mod, f, args, state, env}

      {name, args} ->
        state = update_in(state.locals, &[{name, length(args)} | &1])
        {env.module, name, args, state, env}
    end
  end

  def expand_call_of_types(call, types, state, env) do
    {mod, name, args, state, env} = decompose_and_expand_call(call, state, env)
    create_call_of_types(mod, name, args, types, state, env)
  end

  defp create_call_of_types(
         module,
         :%,
         [mod, {:%{}, _struct_meta, [{:|, _, [struct, fields]}]}],
         [] = _types,
         state,
         env
       )
       when is_atom(mod) do
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      {dependence, state} = get_dependence_module(module, mod, state)
      {struct, state, env} = Expander.expand(struct, state, env)
      insert_struct_fields(struct, fields, dependence, state, env, loc)
    end
  end

  defp create_call_of_types(
         module,
         :%,
         [mod, {:%{}, _struct_meta, fields}],
         [] = _types,
         state,
         env
       )
       when is_atom(mod) do
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      {dependence, state} = get_dependence_module(module, mod, state)
      struct = LLVM.mlir_undef(loc: loc) >>> Charms.Struct.retrieve_struct_type(dependence)
      insert_struct_fields(struct, fields, dependence, state, env, loc)
    end
  end

  defp create_call_of_types(mod, name, args, types, state, env) do
    arity = length(args)

    # By elixir's evaluation order, we should expand the arguments first even the function is not found.

    {args, state, env} = Expander.expand(args, state, env)
    {types, state, env} = Expander.expand(types, state, env)

    cond do
      # invalid arguments, create poison
      invalid_arg = Enum.find(args, &(not is_struct(&1, MLIR.Value))) ->
        "Invalid operand #{Macro.to_string(invalid_arg)} when calling #{Exception.format_mfa(mod, name, arity)}"
        |> create_poison(state, env)

      # remote call, inter the type by looking up symbol in the dependence module
      mod != env.module and match?({:module, _}, Code.ensure_compiled(mod)) and Code.loaded?(mod) and
          function_exported?(mod, :__ir__, 0) ->
        state = update_in(state.remotes, &[{mod, name, arity} | &1])
        {dependence, state} = fetch_dependence_module(mod, state)

        with :t <- name,
             0 <- arity,
             struct_type <-
               Charms.Struct.retrieve_struct_type(dependence),
             false <- is_nil(struct_type) do
          {struct_type, state, env}
        else
          _ ->
            infer_by_lookup(env, dependence, mod, name, types)
            |> then(&create_call(mod, name, args, &1, state, env))
        end

      # local call, resolve the expanding the type in callee's definition
      mod == env.module ->
        infer_by_resolving(env, name, types, state)
        |> then(&create_call(mod, name, args, &1, state, env))

      # remote call, but the module is absent, create poison
      true ->
        "Unknown invocation: #{Exception.format_mfa(mod, name, arity)}"
        |> create_poison(state, env)
    end
  end

  defp expand_std(Enum, :reduce, args, state, env) do
    while =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        [l, init, f] = args
        {l, state, env} = Expander.expand(l, state, env)
        {init, state, env} = Expander.expand(init, state, env)
        result_t = MLIR.Value.type(init)

        {{tail_ptr, head_ptr}, _state, _env} =
          quote do
            tail_ptr = Charms.Pointer.allocate(Term.t())
            Charms.Pointer.store(l, tail_ptr)
            head_ptr = Charms.Pointer.allocate(Term.t())
            {tail_ptr, head_ptr}
          end
          |> Expander.expand_with_bindings(state, env, l: l)

        # we compile the Enum.reduce/3 to a scf.while in MLIR
        SCF.while [init] do
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())

              # getting the BEAM env, assuming it is a regular defm with env as the first argument
              env_ptr = beam_env_from_defm!(env, state)

              # the condition of the while loop, consuming the list with enif_get_list_cell
              {condition, _state, _env} =
                quote do
                  enif_get_list_cell(
                    env_ptr,
                    Pointer.load(Term.t(), tail_ptr),
                    head_ptr,
                    tail_ptr
                  ) > 0
                end
                |> Expander.expand_with_bindings(state, env,
                  tail_ptr: tail_ptr,
                  head_ptr: head_ptr,
                  env_ptr: env_ptr
                )

              SCF.condition(condition, acc) >>> []
            end
          end

          # the body of the while loop, compiled from the reducer which is an anonymous function
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())
              {:fn, _, [{:->, _, [[arg_element, arg_acc], body]}]} = f

              # inject head and acc before expanding the body
              state = Expander.put_mlir_var(state, arg_acc, acc)

              {head_val, state, env} =
                quote(do: Charms.Pointer.load(Charms.Term.t(), head_ptr))
                |> Expander.expand_with_bindings(state, env, head_ptr: head_ptr)

              state = Expander.put_mlir_var(state, arg_element, head_val)
              # expand the body
              {body, _state, _env} = Expander.expand(body, state, env)
              SCF.yield(body) >>> []
            end
          end
        end >>> result_t
      end

    {while, state, env}
  end

  defp expand_std(String, :length, args, state, env) do
    {string, state, env} = Expander.expand(args, state, env)

    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
      len = MemRef.dim(string, zero) >>> :infer
    end

    {len, state, env}
  end

  defp validate_call_args!(args, env) do
    case Enum.find(args, &(not is_struct(&1, MLIR.Value))) do
      nil ->
        :ok

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

      invalid_arg ->
        raise_compile_error(env, "Invalid operand: #{inspect(invalid_arg)}")
    end
  end

  defp expand_call_as_op(dialect, op, args, state, env) do
    op = "#{dialect}.#{op}"

    MapSet.member?(state.mlir.available_ops, op) or
      raise_compile_error(
        env,
        "Unknown MLIR operation to create: #{op}, did you mean: #{did_you_mean_op(op)}"
      )

    {args, state, env} = Expander.expand(args, state, env)
    validate_call_args!(args, env)

    try do
      state = Expander.expand_deferrals_before_terminator(op, state)

      %Beaver.SSA{
        op: op,
        arguments: args,
        ctx: state.mlir.ctx,
        blk: state.mlir.blk,
        loc: MLIR.Location.from_env(env),
        results: if(MLIR.Context.infer_type?(state.mlir.ctx, op), do: [:infer], else: [])
      }
      |> MLIR.Operation.create()
      |> then(&{MLIR.Operation.results(&1), state, env})
    rescue
      e ->
        raise_compile_error(env, "Failed to create #{op}: #{Exception.message(e)}")
    end
  end

  def expand_intrinsics(loc, module, intrinsic_impl, args, state, env) do
    try do
      {args, state, env} = Expander.expand(args, state, env)
      state = update_in(state.mlir.required_intrinsic_modules, &MapSet.put(&1, module))

      v =
        apply(module, intrinsic_impl, args).(%Charms.Intrinsic.Opts{
          ctx: state.mlir.ctx,
          blk: state.mlir.blk,
          loc: loc
        })

      case v do
        %m{} when m in [MLIR.Value, MLIR.Type, MLIR.Operation] ->
          {v, state, env}

        {ast = {_, _, _}, bindings} when is_list(bindings) ->
          :ok = Macro.validate(ast)
          Expander.expand_with_bindings(ast, state, env, bindings)

        other ->
          raise_compile_error(
            env,
            "Unexpected return type from intrinsic #{module}.#{intrinsic_impl}: #{inspect(other)}"
          )
      end
    rescue
      e ->
        raise_compile_error(
          env,
          "Failed to expand intrinsic\n#{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
        )
    end
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

  defp expand_magic_macros(_loc, {module, fun, _arity} = {String, :length, 1}, args, state, env) do
    {args, state, env} = Expander.expand(args, state, env)
    expand_std(module, fun, args, state, env)
  end

  defp expand_magic_macros(_loc, {module, fun, _arity} = {Enum, :reduce, 3}, args, state, env) do
    expand_std(module, fun, args, state, env)
  end

  defp expand_magic_macros(_loc, {Module, :__get_attribute__, 4}, args, state, env) do
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

  defp expand_magic_macros(loc, {module, fun, arity}, args, state, env) do
    cond do
      Expander.export_intrinsics?(module, fun, arity) ->
        intrinsic_impl = module.__intrinsics__(fun, arity)
        expand_intrinsics(loc, module, intrinsic_impl, args, state, env)

      (struct_type =
         Charms.Struct.retrieve_struct_type(state.mlir.mod)) && module == env.module && fun == :t &&
          arity == 0 ->
        {struct_type, state, env}

      true ->
        create_call_of_types(module, fun, args, [], state, env)
    end
  end

  defp expand_remote_macro(meta, {module, fun, arity} = mfa, args, state, env) do
    loc = MLIR.Location.from_env(env)

    case Macro.Env.expand_require(env, meta, module, fun, arity,
           trace: true,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        Expander.expand_macro(meta, module, fun, args, callback, state, env)

      :error ->
        expand_magic_macros(loc, mfa, args, state, env)
    end
  end

  defp expand_remote(_meta, module, fun, args, state, env) do
    # A compiler may want to emit a :remote_function trace in here.
    state = update_in(state.remotes, &[{module, fun, length(args)} | &1])
    loc = MLIR.Location.from_env(env)

    cond do
      Expander.export_intrinsics?(module, fun, length(args)) ->
        intrinsic_impl = module.__intrinsics__(fun, length(args))
        expand_intrinsics(loc, module, intrinsic_impl, args, state, env)

      module in [MLIR.Type] ->
        {args, state, env} = Expander.expand_list(args, state, env)

        if fun in [:unranked_tensor, :unranked_tensor!, :vector, :vector!, :complex] do
          args
        else
          args ++ [[ctx: state.mlir.ctx]]
        end
        |> then(
          &{case apply(module, fun, &1) do
             {:ok, t} -> t
             t -> t
           end, state, env}
        )

      true ->
        raise_compile_error(env, "function #{module}.#{fun}/#{length(args)} not found")
    end
  end

  defp expand_local(_meta, fun, args, state, env) do
    # A compiler may want to emit a :local_function trace in here.
    state = update_in(state.locals, &[{fun, length(args)} | &1])
    {args, state, env} = Expander.expand_list(args, state, env)

    try do
      create_call_of_types(env.module, fun, args, [], state, env)
    rescue
      e ->
        raise_compile_error(
          env,
          "Failed to expand local function #{fun}/#{length(args)}: #{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
        )
    end
  end

  defp get_dependence_module(module, mod, state) do
    if module == mod do
      {state.mlir.mod, state}
    else
      fetch_dependence_module(mod, state)
    end
  end

  defp insert_struct_fields(struct, fields, dependence, state, env, loc) do
    struct_type = Charms.Struct.retrieve_struct_type(dependence)

    struct =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        for {k, v} <- fields, reduce: struct do
          struct ->
            position = Charms.Struct.position_of_field!(env, dependence, k)
            struct = LLVM.insertvalue(struct, v, position: position, loc: loc) >>> struct_type
            struct
        end
      end

    {struct, state, env}
  end

  defp beam_env_from_defm(_env, state) do
    state.mlir.enif_env
  end

  defp beam_env_from_defm!(env, state) do
    if e = beam_env_from_defm(env, state) do
      e
    else
      raise_compile_error(env, "must be a defm with beam env as the first argument")
    end
  end

  defp did_you_mean_op(op) do
    MLIR.Dialect.Registry.ops(:all)
    |> Stream.map(&{&1, String.jaro_distance(&1, op)})
    |> Enum.sort(&(elem(&1, 1) >= elem(&2, 1)))
    |> Enum.to_list()
    |> List.first()
    |> elem(0)
  end
end
