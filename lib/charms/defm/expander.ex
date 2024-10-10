defmodule Charms.Defm.Expander do
  @moduledoc false
  use Beaver
  alias MLIR.Attribute
  alias MLIR.Dialect.{Func, CF, SCF, MemRef, Index, Arith, Ub, LLVM}
  require Func
  # Define the environment we will use for expansion.
  # We reset the fields below but we will need to set
  # them accordingly later on.

  defstruct ctx: nil,
            mod: nil,
            blk: nil,
            available_ops: MapSet.new(),
            vars: Map.new(),
            region: nil,
            enif_env: nil

  @env %{
    Macro.Env.prune_compile_info(__ENV__)
    | line: 0,
      file: "nofile",
      module: nil,
      function: nil,
      context_modules: []
  }
  defp env, do: @env

  defp put_env_in_stacktrace(stacktrace, env) do
    [frame] = Macro.Env.stacktrace(env)
    idx = stacktrace |> Enum.find_index(&match?({__MODULE__, _, _, _}, &1))
    List.insert_at(stacktrace, idx, frame)
  end

  defp put_env_in_stacktrace(stacktrace, env, {m, f, a}) do
    [{_, _, _, meta}] = Macro.Env.stacktrace(env)
    frame = {m, f, a, meta}

    idx = stacktrace |> Enum.find_index(&match?({__MODULE__, _, _, _}, &1))
    List.insert_at(stacktrace, idx, frame)
  end

  # This is a proof of concept of how to build language server
  # tooling or a compiler of a custom language on top of Elixir's
  # building blocks.
  #
  # This example itself will focus on the language server use case.
  # The goal is to traverse expressions collecting information which
  # will be stored in the state (which exists in addition to the
  # environment). The expansion also returns an AST, which has little
  # use for language servers but, in compiler cases, the AST is the
  # one which will be further explored and compiled.
  #
  # For compilers, we'd also need two additional features: the ability
  # to programatically report compiler errors and the ability to
  # track variables. This may be added in the future.
  def expand(ast, file) do
    ctx = MLIR.Context.create()
    available_ops = MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx))

    mlir = %__MODULE__{
      ctx: ctx,
      blk: MLIR.Block.create(),
      available_ops: available_ops,
      vars: Map.new(),
      region: nil,
      enif_env: nil
    }

    expand(
      ast,
      %{attrs: [], remotes: [], locals: [], definitions: [], vars: [], mlir: mlir},
      %{env() | file: file}
    )
  end

  def expand_with(ast, env, mlir = %__MODULE__{ctx: ctx}) do
    available_ops = MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx))
    mlir = mlir |> Map.put(:available_ops, available_ops)

    expand(
      ast,
      %{attrs: [], remotes: [], locals: [], definitions: [], vars: [], mlir: mlir},
      env
    )
  end

  defp create_call(mod, name, args, types, state, env) do
    op =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        %Beaver.SSA{
          op: "func.call",
          arguments: args ++ [callee: Attribute.flat_symbol_ref(mangling(mod, name))],
          ctx: Beaver.Env.context(),
          block: Beaver.Env.block(),
          loc: MLIR.Location.from_env(env)
        }
        |> Beaver.SSA.put_results(types)
        |> MLIR.Operation.create()
      end

    {MLIR.Operation.results(op), state, env}
  end

  defp create_poison(msg, state, env) do
    mlir ctx: state.mlir.ctx, block: state.mlir.blk do
      Ub.poison(msg: MLIR.Attribute.string(msg), loc: MLIR.Location.from_env(env)) >>>
        ~t{none}
    end
    |> then(&{&1, state, env})
  end

  defp expand_call_of_types(call, types, state, env) do
    {mod, name, args, state, env} =
      case Macro.decompose_call(call) do
        {alias, f, args} ->
          {mod, state, env} = expand(alias, state, env)
          {mod, f, args, state, env}

        {name, args} ->
          state = update_in(state.locals, &[{name, length(args)} | &1])
          {env.module, name, args, state, env}
      end

    state = update_in(state.remotes, &[{mod, name, length(args)} | &1])
    {args, state, env} = expand(args, state, env)
    {types, state, env} = expand(types, state, env)
    create_call(mod, name, args, types, state, env)
  end

  defp has_implemented_inference(op, ctx) when is_bitstring(op) do
    id = MLIR.CAPI.mlirInferTypeOpInterfaceTypeID()

    op
    |> MLIR.StringRef.create()
    |> MLIR.CAPI.mlirOperationImplementsInterfaceStatic(ctx, id)
    |> Beaver.Native.to_term()
  end

  defp expand_std(Enum, :reduce, args, state, env) do
    while =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        [l, init, f] = args
        {l, state, env} = expand(l, state, env)
        {init, state, env} = expand(init, state, env)
        result_t = MLIR.Value.type(init)
        {list_term_ptr, state} = uniq_mlir_var(state, l)
        tail_ptr = uniq_mlir_var()
        head_ptr = uniq_mlir_var()

        {_, state, env} =
          quote do
            unquote(tail_ptr) = Charms.Pointer.allocate(Term.t())
            Pointer.store(unquote(list_term_ptr), unquote(tail_ptr))
            unquote(head_ptr) = Charms.Pointer.allocate(Term.t())
          end
          |> expand(state, env)

        # we compile the Enum.reduce/3 to a scf.while in MLIR
        SCF.while [init] do
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())

              # getting the BEAM env, assuming it is a regular defm with env as the first argument
              {env_var, state} = env_from_defm!(state)

              # the condition of the while loop, consuming the list with enif_get_list_cell
              {condition, _state, _env} =
                quote do
                  enif_get_list_cell(
                    unquote(env_var),
                    Pointer.load(Term.t(), unquote(tail_ptr)),
                    unquote(head_ptr),
                    unquote(tail_ptr)
                  ) > 0
                end
                |> expand(state, env)

              SCF.condition(condition, acc) >>> []
            end
          end

          # the body of the while loop, compiled from the reducer which is an anonymous function
          region do
            block _(acc >>> result_t) do
              state = put_in(state.mlir.blk, Beaver.Env.block())
              {:fn, _, [{:->, _, [[arg_element, arg_acc], body]}]} = f

              # inject head and acc before expanding the body
              state = put_mlir_var(state, arg_acc, acc)

              {head_val, state, env} =
                quote(do: Charms.Pointer.load(Charms.Term.t(), unquote(head_ptr)))
                |> expand(state, env)

              state = put_mlir_var(state, arg_element, head_val)

              # expand the body
              {body, _state, _env} = expand(body, state, env)
              SCF.yield(List.last(body)) >>> []
            end
          end
        end >>> result_t
      end

    {while, state, env}
  end

  defp expand_std(String, :length, args, state, env) do
    {string, state, env} = expand(args, state, env)

    mlir ctx: state.mlir.ctx, block: state.mlir.blk do
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
      len = MemRef.dim(string, zero) >>> :infer
    end

    {len, state, env}
  end

  defp expand_std(_module, _fun, _args, _state, _env) do
    :not_implemented
  end

  defp expand_body(body, args, arg_types, state, env) do
    mlir ctx: state.mlir.ctx do
      b =
        block do
          MLIR.Block.add_args!(Beaver.Env.block(), arg_types, ctx: Beaver.Env.context())

          arg_values =
            Range.new(0, length(args) - 1)
            |> Enum.map(&MLIR.Block.get_arg!(Beaver.Env.block(), &1))

          state =
            Enum.zip(args, arg_values)
            |> Enum.reduce(state, fn {k, v}, state -> put_mlir_var(state, k, v) end)

          state =
            with [head_arg_type | _] <- arg_types,
                 [{:env, _, nil} | _] <- args,
                 MLIR.Type.equal?(head_arg_type, Beaver.ENIF.Type.env(ctx: state.mlir.ctx)) do
              a = MLIR.Block.get_arg!(Beaver.Env.block(), 0)
              put_in(state.mlir.enif_env, a)
            else
              _ -> state
            end

          state = put_in(state.mlir.blk, Beaver.Env.block())

          {_, state, env} =
            expand(body, state, env)
        end

      {b, state, env}
    end
  end

  # The goal of this function is to traverse all of Elixir special
  # forms. The list is actually relatively small and a good reference
  # is the Elixir type checker: https://github.com/elixir-lang/elixir/blob/494a018abbc88901747c32032ec9e2c408f40608/lib/elixir/lib/module/types/expr.ex
  # Everything that is not a special form, is either a local call,
  # a remote call, or a literal.
  #
  # Besides remember that Elixir has two special contexts: match
  # (in pattern matching) and guards. Guards are relatively simple
  # while matches define variables and need to consider both the
  # usage of `^` and `::` specifiers in binaries.

  ## Containers
  # A language server needs to support all types. A custom compiler
  # needs to choose which ones to support. Don't forget that both
  # lists and maps need to consider the usage of `|`. Binaries need
  # to handle `::` (skipped here for convenience).

  defp expand([_ | _] = list, state, env) do
    expand_list(list, state, env)
  end

  defp expand({left, right}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{left, right}, state, env}
  end

  defp expand({:{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:{}, meta, args}, state, env}
  end

  defp expand({:%{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:%{}, meta, args}, state, env}
  end

  defp expand({:|, meta, [left, right]}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{:|, meta, [left, right]}, state, env}
  end

  defp expand({:<<>>, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:<<>>, meta, args}, state, env}
  end

  ## __block__

  defp expand({:__block__, _, list}, state, env) do
    expand_list(list, state, env)
  end

  ## __aliases__

  defp expand({:__aliases__, meta, [head | tail] = list}, state, env) do
    case Macro.Env.expand_alias(env, meta, list, trace: true) do
      {:alias, alias} ->
        # A compiler may want to emit a :local_function trace in here.
        # Elixir also warns on easy to confuse aliases, such as True/False/Nil.
        {alias, state, env}

      :error ->
        with alias <- Module.concat([head]),
             {:ok, found} <- Keyword.fetch(env.aliases, alias) do
          {found, state, env}
        else
          _ ->
            {head, state, env} = expand(head, state, env)

            if is_atom(head) do
              # A compiler may want to emit a :local_function trace in here.
              {Module.concat([head | tail]), state, env}
            else
              {{:__aliases__, meta, [head | tail]}, state, env}
            end
        end
    end
  end

  ## require, alias, import
  # Those are the main special forms and they require some care.
  #
  # First of all, if __aliases__ is changed to emit traces (which a
  # custom compiler should), we should not emit traces when expanding
  # the first argument of require/alias/import.
  #
  # Second, we must never expand the alias in `:as`. This is handled
  # below.
  #
  # Finally, multi-alias/import/require, such as alias Foo.Bar.{Baz, Bat}
  # is not implemented, check elixir_expand.erl on how to implement it.

  defp expand({form, meta, [arg]}, state, env) when form in [:require, :alias, :import] do
    expand({form, meta, [arg, []]}, state, env)
  end

  defp expand({:alias, meta, [arg, opts]}, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    case arg do
      {:{}, _, aliases} ->
        for alias <- aliases do
          {:alias, meta, [alias, opts]}
        end
        |> expand(state, env)

      # An actual compiler would raise if the alias fails.
      _ ->
        case Macro.Env.define_alias(env, meta, arg, [trace: true] ++ opts) do
          {:ok, env} -> {arg, state, env}
          {:error, _} -> {arg, state, env}
        end
    end
  end

  defp expand({:require, meta, [arg, opts]}, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    # An actual compiler would raise if the module is not defined or if the require fails.
    case Macro.Env.define_require(env, meta, arg, [trace: true] ++ opts) do
      {:ok, env} -> {arg, state, env}
      {:error, _} -> {arg, state, env}
    end
  end

  defp expand({:import, meta, [arg, opts]}, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    # An actual compiler would raise if the module is not defined or if the import fails.
    with true <- is_atom(arg) and Code.ensure_loaded?(arg),
         {:ok, env} <- Macro.Env.define_import(env, meta, arg, [trace: true] ++ opts) do
      {arg, state, env}
    else
      _ -> {arg, state, env}
    end
  end

  @intrinsics Charms.Prelude.__intrinsics__()
  defp expand({fun, _meta, [left, right]}, state, env) when fun in @intrinsics do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    loc = MLIR.Location.from_env(env)

    {Charms.Prelude.handle_intrinsic(fun, [left, right],
       ctx: state.mlir.ctx,
       block: state.mlir.blk,
       loc: loc
     ), state, env}
  end

  ## =/2
  # We include = as an example of how we could handle variables.
  # For example, if you want to store where variables are defined,
  # you would collect this information in expand_pattern/3 and
  # invoke it from all relevant places (such as case, cond, try, etc).

  defp expand({:=, _meta, [left, right]}, state, env) do
    {left, state, env} = expand_pattern(left, state, env)
    {right, state, env} = expand(right, state, env)
    state = put_mlir_var(state, left, right)
    {right, state, env}
  end

  ## quote/1, quote/2
  # We need to expand options and look inside unquote/unquote_splicing.
  # A custom compiler may want to raise on this special form (for example),
  # quoted expressions make no sense if you are writing a language that
  # compiles to C.

  defp expand({:quote, _, [opts]}, state, env) do
    {block, opts} = Keyword.pop(opts, :do)
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(block, state, env)
  end

  defp expand({:quote, _, [opts, block_opts]}, state, env) do
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(Keyword.get(block_opts, :do), state, env)
  end

  ## Pin operator
  # It only appears inside match and it disables the match behaviour.

  defp expand({:^, _meta, [arg]}, state, %{context: context} = env) do
    {b, state, env} = expand(arg, state, %{env | context: nil})
    match?(%MLIR.Block{}, b) || raise Beaver.EnvNotFoundError, MLIR.Block

    br =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        CF.br({b, []}) >>> []
      end

    {br, state, %{env | context: context}}
  end

  ## Remote call

  defp expand({{:., dot_meta, [module, fun]}, meta, args}, state, env)
       when is_atom(fun) and is_list(args) do
    env = %{env | line: dot_meta[:line] || meta[:line] || env.line}
    {module, state, env} = expand(module, state, env)
    arity = length(args)
    mfa = {module, fun, arity}
    state = update_in(state.remotes, &[mfa | &1])
    loc = MLIR.Location.from_env(env)

    if is_atom(module) do
      try do
        case Macro.Env.expand_require(env, meta, module, fun, arity,
               trace: true,
               check_deprecations: false
             ) do
          {:macro, module, callback} ->
            expand_macro(meta, module, fun, args, callback, state, env)

          :error ->
            Code.ensure_loaded(module)

            cond do
              function_exported?(module, :__intrinsics__, 0) and fun in module.__intrinsics__() ->
                {args, state, env} = expand(args, state, env)

                {module.handle_intrinsic(fun, args,
                   ctx: state.mlir.ctx,
                   block: state.mlir.blk,
                   loc: loc
                 ), state, env}

              module == MLIR.Attribute ->
                {args, state, env} = expand(args, state, env)
                {apply(MLIR.Attribute, fun, args), state, env}

              mfa == {Module, :__get_attribute__, 4} ->
                {args, state, env} = expand(args, state, env)
                attr = apply(Module, :__get_attribute__, args) |> :erlang.term_to_binary()
                {env_var, state} = env_from_defm!(state)

                # there is no nested do-block to expand, so it is safe to use regular variable names, as long as the updated state and env are not leaked
                quote do
                  alias Charms.Pointer
                  alias Charms.Term
                  attr = unquote(attr)
                  term_ptr = Pointer.allocate(Term.t())
                  size = String.length(attr)
                  size = value index.casts(size) :: i64()
                  buffer_ptr = Pointer.allocate(i8(), size)
                  buffer = ptr_to_memref(buffer_ptr, size)
                  memref.copy(attr, buffer)
                  zero = const 0 :: i32()
                  enif_binary_to_term(unquote(env_var), buffer_ptr, size, term_ptr, zero)
                  Pointer.load(Term.t(), term_ptr)
                end
                |> expand(state, env)
                |> then(&{List.last(elem(&1, 0)), state, env})

              (res = expand_std(module, fun, args, state, env)) != :not_implemented ->
                res

              true ->
                # By elixir's evaluation order, we should expand the arguments first even if the function is not found.
                {_, state, env} = expand(args, state, env)

                "Unknown intrinsic: #{Exception.format_mfa(module, fun, arity)}"
                |> create_poison(state, env)
            end
        end
      rescue
        e ->
          reraise e, put_env_in_stacktrace(__STACKTRACE__, env, mfa)
      end
    else
      [{dialect, _, _}, op] = [module, fun]

      try do
        op = "#{dialect}.#{op}"

        MapSet.member?(state.mlir.available_ops, op) or
          raise ArgumentError,
                "Unknown MLIR operation to create: #{op}, did you mean: #{did_you_mean_op(op)}"

        {args, state, env} = expand(args, state, env)

        if invalid_arg = Enum.find(args, &(not is_struct(&1, MLIR.Value))) do
          case invalid_arg do
            %MLIR.Operation{} = arg_op ->
              case name = MLIR.Operation.name(arg_op) do
                "func.call" ->
                  callee = Beaver.Walker.attributes(arg_op)["callee"]

                  raise ArgumentError,
                        "#{name} #{to_string(callee) || "(unknown callee)"} doesn't return a value"

                _ ->
                  raise ArgumentError, "#{name} doesn't return a value"
              end

            _ ->
              raise ArgumentError, "Invalid operand: #{inspect(invalid_arg)}"
          end
        end

        %Beaver.SSA{
          op: op,
          arguments: args,
          ctx: state.mlir.ctx,
          block: state.mlir.blk,
          loc: MLIR.Location.from_env(env),
          results: if(has_implemented_inference(op, state.mlir.ctx), do: [:infer], else: [])
        }
        |> MLIR.Operation.create()
        |> then(&{MLIR.Operation.results(&1), state, env})
      rescue
        e ->
          reraise e, put_env_in_stacktrace(__STACKTRACE__, env)
      end
    end
  end

  # Parameterized function call
  defp expand(
         {{:., _parameterized_meta, [parameterized]}, _meta, args},
         state,
         env
       ) do
    {args, state, env} = expand(args, state, env)
    {parameterized, state, env} = expand(parameterized, state, env)

    if is_function(parameterized) do
      {parameterized.(args), state, env}
    else
      raise ArgumentError, "Expected a function, got: #{inspect(parameterized)}"
    end
  end

  ## Imported or local call

  defp expand({fun, meta, args}, state, env) when is_atom(fun) and is_list(args) do
    env = %{env | line: meta[:line] || env.line}
    arity = length(args)

    # For language servers, we don't want to emit traces, nor expand local macros,
    # nor print deprecation warnings. Compilers likely want those set to true.
    case Macro.Env.expand_import(env, meta, fun, arity,
           trace: true,
           allow_locals: false,
           check_deprecations: true
         ) do
      {:macro, module, callback} ->
        try do
          expand_macro(meta, module, fun, args, callback, state, env)
        rescue
          e ->
            reraise e, put_env_in_stacktrace(__STACKTRACE__, env, {module, fun, arity})
        end

      {:function, module, fun} ->
        try do
          expand_remote(meta, module, fun, args, state, env)
        rescue
          e ->
            reraise e, put_env_in_stacktrace(__STACKTRACE__, env, {module, fun, arity})
        end

      {:error, :not_found} ->
        try do
          expand_local(meta, fun, args, state, env)
        rescue
          e ->
            reraise e, put_env_in_stacktrace(__STACKTRACE__, env)
        end
    end
  end

  ## __MODULE__, __DIR__, __ENV__, __CALLER__
  # A custom compiler may want to raise.

  defp expand({:__MODULE__, _, ctx}, state, env) when is_atom(ctx) do
    {env.module, state, env}
  end

  defp expand({:__DIR__, _, ctx}, state, env) when is_atom(ctx) do
    {Path.dirname(env.file), state, env}
  end

  defp expand({:__ENV__, _, ctx}, state, env) when is_atom(ctx) do
    {Macro.escape(env), state, env}
  end

  defp expand({:__CALLER__, _, ctx} = ast, state, env) when is_atom(ctx) do
    {ast, state, env}
  end

  ## var
  # For the language server, we only want to capture definitions,
  # we don't care when they are used.

  defp expand({var, meta, ctx} = ast, state, %{context: :match} = env)
       when is_atom(var) and is_atom(ctx) do
    ctx = Keyword.get(meta, :context, ctx)
    state = update_in(state.vars, &[{var, ctx} | &1])
    {ast, state, env}
  end

  ## Fallback

  @const_prefix "chc"
  defp expand(ast, state, env) when is_binary(ast) do
    s_table = state.mlir.mod |> MLIR.Operation.from_module() |> MLIR.CAPI.mlirSymbolTableCreate()
    sym_name = @const_prefix <> :crypto.hash(:sha256, ast)
    found = MLIR.CAPI.mlirSymbolTableLookup(s_table, MLIR.StringRef.create(sym_name))
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, block: MLIR.Module.body(state.mlir.mod) do
      if MLIR.is_null(found) do
        MemRef.global(ast, sym_name: Attribute.string(sym_name), loc: loc) >>> :infer
      else
        found
      end
      |> then(
        &mlir block: state.mlir.blk do
          name = Attribute.flat_symbol_ref(Attribute.unwrap(&1[:sym_name]))
          MemRef.get_global(name: name, loc: loc) >>> Attribute.unwrap(&1[:type])
        end
      )
    end
    |> then(&{&1, state, env})
  end

  defp expand(ast, state, env) do
    {get_mlir_var(state, ast) || ast, state, env}
  end

  defp did_you_mean_op(op) do
    MLIR.Dialect.Registry.ops(:all)
    |> Stream.map(&{&1, String.jaro_distance(&1, op)})
    |> Enum.sort(&(elem(&1, 1) >= elem(&2, 1)))
    |> Enum.to_list()
    |> List.first()
    |> elem(0)
  end

  defp mangling(mod, func) do
    Module.concat(mod, func)
  end

  # Expands a nil clause body in an if statement, yielding no value.
  defp expand_if_clause_body(nil, state, _env) do
    mlir ctx: state.mlir.ctx, block: state.mlir.blk do
      SCF.yield() >>> []
      []
    end
  end

  # Expands a non-nil clause body in an if statement, yielding the last evaluated value.
  defp expand_if_clause_body(clause_body, state, env) do
    mlir ctx: state.mlir.ctx, block: state.mlir.blk do
      {ret, _, _} = expand(clause_body, state, env)

      case ret |> List.wrap() |> List.last() do
        %MLIR.Operation{} ->
          SCF.yield() >>> []
          []

        %MLIR.Value{} = last ->
          SCF.yield(last) >>> []
          MLIR.Value.type(last)
      end
    end
  end

  ## Macro handling

  # This is going to be the function where you will intercept expansions
  # and attach custom behaviour. As an example, we will capture the module
  # definition, fully replacing the actual implementation. You could also
  # use this to capture module attributes (optionally delegating to the actual
  # implementation), function expansion, and more.
  defp expand_macro(meta, Kernel, :defmodule, [alias, [do: block]], _callback, state, env) do
    {expanded, state, env} = expand(alias, state, env)

    state =
      put_in(
        state.mlir.mod,
        mlir ctx: state.mlir.ctx do
          module sym_name: Attribute.string(expanded) do
          end
        end
      )

    state = put_in(state.mlir.blk, MLIR.CAPI.mlirModuleGetBody(state.mlir.mod))

    if is_atom(expanded) do
      {full, env} = alias_defmodule(meta, alias, expanded, env)
      env = %{env | context_modules: [full | env.context_modules]}

      # The env inside the block is discarded.
      {result, state, _env} = expand(block, state, %{env | module: full})
      {result, state, env}
    else
      # If we don't know the module name, do we still want to expand it here?
      # Perhaps it would be useful for dealing with local functions anyway?
      # But note that __MODULE__ will return nil.
      #
      # The env inside the block is discarded.
      {result, state, _env} = expand(block, state, env)
      {result, state, env}
    end
  end

  defp expand_macro(_meta, Kernel, :def, [call, [do: body]], _callback, state, env) do
    {:"::", _, [call, ret_types]} = call

    {name, args} = Macro.decompose_call(call)
    name = mangling(env.module, name)

    body =
      body
      |> List.wrap()

    {args, arg_types} =
      for {:"::", _, [a, t]} <- args do
        {a, t}
      end
      |> Enum.unzip()

    parent_block = state.mlir.blk

    f =
      mlir ctx: state.mlir.ctx, block: parent_block do
        {ret_types, state, env} = ret_types |> expand(state, env)
        {arg_types, state, env} = arg_types |> expand(state, env)

        if i = Enum.find_index(arg_types, &(!is_struct(&1, MLIR.Type))) do
          raise ArgumentError, "Invalid argument type ##{i + 1}"
        end

        if i = Enum.find_index(ret_types, &(!is_struct(&1, MLIR.Type))) do
          raise ArgumentError, "Invalid return type ##{i + 1}"
        end

        ft = Type.function(arg_types, ret_types, ctx: Beaver.Env.context())

        Func.func _(sym_name: "\"#{name}\"", function_type: ft, loc: MLIR.Location.from_env(env)) do
          region do
          end
        end
      end

    r0 = Beaver.Walker.regions(f) |> Enum.at(0)
    state = put_in(state.mlir.region, r0)

    # create and insert the entry block to expose the state variable
    {blk_entry, state, env} = expand_body(body, args, arg_types, state, env)
    # restore the block back to parent, otherwise consecutive functions will be created nested
    state = put_in(state.mlir.blk, parent_block)

    MLIR.CAPI.mlirRegionInsertOwnedBlock(r0, 0, blk_entry)
    {f, state, env}
  end

  defp expand_macro(_meta, Beaver, :block, args, _callback, state, env) do
    b =
      mlir ctx: state.mlir.ctx do
        block do
          [[do: body]] = args
          body |> expand(put_in(state.mlir.blk, Beaver.Env.block()), env)
        end
      end

    MLIR.CAPI.mlirRegionAppendOwnedBlock(state.mlir.region, b)
    {b, state, env}
  end

  defp expand_macro(_meta, Charms.Defm, :cond_br, [condition, clauses], _callback, state, env) do
    true_body = Keyword.fetch!(clauses, :do)
    false_body = Keyword.fetch!(clauses, :else)
    {condition, state, env} = expand(condition, state, env)

    v =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        true_body =
          block do
            expand(true_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end
          |> tap(&MLIR.CAPI.mlirRegionAppendOwnedBlock(state.mlir.region, &1))

        false_body =
          block do
            expand(false_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end
          |> tap(&MLIR.CAPI.mlirRegionAppendOwnedBlock(state.mlir.region, &1))

        CF.cond_br(
          condition,
          true_body,
          false_body,
          loc: MLIR.Location.from_env(env)
        ) >>> []
      end

    {v, state, env}
  end

  # Expands an `if` expression, handling both true and false clause bodies.
  defp expand_macro(_meta, Kernel, :if, [condition, clauses], _callback, state, env) do
    true_body = Keyword.fetch!(clauses, :do)
    false_body = clauses[:else]
    {condition, state, env} = expand(condition, state, env)
    loc = MLIR.Location.from_env(env)

    v =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        b =
          block _true() do
            ret_t =
              expand_if_clause_body(true_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end

        # TODO: doc about an expression which is a value and an operation
        SCF.if [condition, loc: loc] do
          region do
            MLIR.CAPI.mlirRegionAppendOwnedBlock(Beaver.Env.region(), b)
          end

          region do
            block _false() do
              expand_if_clause_body(false_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
            end
          end
        end >>> ret_t
      end

    {v, state, env}
  end

  defp expand_macro(_meta, Charms.Defm, :while_loop, [expr, [do: body]], _callback, state, env) do
    v =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        SCF.while [] do
          region do
            block _() do
              {condition, _state, _env} =
                expand(expr, put_in(state.mlir.blk, Beaver.Env.block()), env)

              SCF.condition(condition) >>> []
            end
          end

          region do
            block _() do
              expand(body, put_in(state.mlir.blk, Beaver.Env.block()), env)
              SCF.yield() >>> []
            end
          end
        end >>> []
      end

    {v, state, env}
  end

  defp expand_macro(_meta, Charms.Defm, :for_loop, [expr, [do: body]], _callback, state, env) do
    {:<-, _, [{element, index}, {:{}, _, [t, ptr, len]}]} = expr
    {len, state, env} = expand(len, state, env)
    {t, state, env} = expand(t, state, env)
    {ptr, state, env} = expand(ptr, state, env)

    v =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
        lower_bound = zero
        upper_bound = Index.casts(len) >>> Type.index()
        step = Index.constant(value: Attribute.index(1)) >>> Type.index()

        SCF.for [lower_bound, upper_bound, step] do
          region do
            block _body(index_val >>> Type.index()) do
              index_casted = Index.casts(index_val) >>> Type.i64()

              element_ptr =
                LLVM.getelementptr(ptr, index_casted,
                  elem_type: t,
                  rawConstantIndices: ~a{array<i32: -2147483648>}
                ) >>> ~t{!llvm.ptr}

              element_val = LLVM.load(element_ptr) >>> t
              state = put_mlir_var(state, element, element_val)
              state = put_mlir_var(state, index, index_val)
              expand(body, put_in(state.mlir.blk, Beaver.Env.block()), env)
              SCF.yield() >>> []
            end
          end
        end >>> []
      end

    {v, state, env}
  end

  defp expand_macro(_meta, Charms.Defm, :op, [call], _callback, state, env) do
    {call, return_types} = Charms.Defm.decompose_call_with_return_type(call)
    {{dialect, _, _}, op, args} = Macro.decompose_call(call)
    op = "#{dialect}.#{op}"
    {args, state, env} = expand(args, state, env)
    {return_types, state, env} = expand(return_types, state, env)

    return_types =
      case return_types do
        [] ->
          [:infer]

        _ ->
          List.flatten(return_types)
      end

    op =
      %Beaver.SSA{
        op: op,
        arguments: args,
        ctx: state.mlir.ctx,
        block: state.mlir.blk,
        loc: MLIR.Location.from_env(env)
      }
      |> Beaver.SSA.put_results(return_types)
      |> MLIR.Operation.create()

    {op, state, env}
  end

  defp expand_macro(meta, Charms.Defm, :value, [call], callback, state, env) do
    {op, state, env} =
      expand_macro(meta, Charms.Defm, :op, [call], callback, state, env)

    {MLIR.Operation.results(op), state, env}
  end

  defp expand_macro(_, Charms.Defm, :call, [{:"::", _, [call, types]}], _callback, state, env) do
    expand_call_of_types(call, types, state, env)
  end

  defp expand_macro(_meta, Charms.Defm, :call, [call], _callback, state, env) do
    expand_call_of_types(call, [], state, env)
  end

  defp expand_macro(
         meta,
         Charms.Defm,
         :const,
         [{:"::", type_meta, [value, type]}],
         _callback,
         state,
         env
       ) do
    env = %{env | line: type_meta[:line] || meta[:line] || env.line}

    {value, state, env} = expand(value, state, env)
    {type, state, env} = expand(type, state, env)

    value =
      mlir ctx: state.mlir.ctx, block: state.mlir.blk do
        loc = MLIR.Location.from_env(env)

        cond do
          MLIR.CAPI.mlirTypeIsAInteger(type) |> Beaver.Native.to_term() ->
            Arith.constant(value: Attribute.integer(type, value), loc: loc) >>> type

          MLIR.CAPI.mlirTypeIsAFloat(type) |> Beaver.Native.to_term() ->
            Arith.constant(value: Attribute.float(type, value), loc: loc) >>> type

          true ->
            raise ArgumentError, "Unsupported type for const macro: #{to_string(type)}"
        end
      end

    {value, state, env}
  end

  defp expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  defp expand_macro_callback(meta, _module, _fun, args, callback, state, env) do
    callback.(meta, args) |> expand(state, env)
  end

  ## defmodule helpers
  # defmodule automatically defines aliases, we need to mirror this feature here.

  # defmodule Elixir.Alias
  defp alias_defmodule(_meta, {:__aliases__, _, [:"Elixir", _ | _]}, module, env),
    do: {module, env}

  # defmodule Alias in root
  defp alias_defmodule(_meta, {:__aliases__, _, _}, module, %{module: nil} = env),
    do: {module, env}

  # defmodule Alias nested
  defp alias_defmodule(meta, {:__aliases__, _, [h | t]}, _module, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    {:ok, env} = Macro.Env.define_alias(env, meta, module, as: alias, trace: true)

    case t do
      [] -> {module, env}
      _ -> {String.to_atom(Enum.join([module | t], ".")), env}
    end
  end

  # defmodule _
  defp alias_defmodule(_meta, _raw, module, env) do
    {module, env}
  end

  ## Helpers

  defp expand_remote(_meta, Kernel, fun, args, state, env) when fun in @intrinsics do
    loc = MLIR.Location.from_env(env)
    {args, state, env} = expand(args, state, env)

    {Charms.Prelude.handle_intrinsic(fun, args,
       ctx: state.mlir.ctx,
       block: state.mlir.blk,
       loc: loc
     ), state, env}
  end

  defp expand_remote(meta, module, fun, args, state, env) do
    # A compiler may want to emit a :remote_function trace in here.
    state = update_in(state.remotes, &[{module, fun, length(args)} | &1])
    {args, state, env} = expand_list(args, state, env)

    if module in [MLIR.Type] do
      if fun in [:unranked_tensor, :complex, :vector] do
        args
      else
        args ++ [[ctx: state.mlir.ctx]]
      end
      |> then(&{apply(module, fun, &1), state, env})
    else
      {{{:., meta, [module, fun]}, meta, args}, state, env}
    end
  end

  defp expand_local(_meta, fun, args, state, env) do
    # A compiler may want to emit a :local_function trace in here.
    state = update_in(state.locals, &[{fun, length(args)} | &1])
    {args, state, env} = expand_list(args, state, env)
    Code.ensure_loaded!(MLIR.Type)
    loc = MLIR.Location.from_env(env)

    if function_exported?(MLIR.Type, fun, 1) do
      {apply(MLIR.Type, fun, [[ctx: state.mlir.ctx]]), state, env}
    else
      case i =
             Charms.Prelude.handle_intrinsic(fun, args,
               ctx: state.mlir.ctx,
               block: state.mlir.blk,
               loc: loc
             ) do
        :not_handled ->
          create_call(env.module, fun, args, [], state, env)

        _ ->
          {i, state, env}
      end
    end
  end

  defp expand_pattern(pattern, state, %{context: context} = env) do
    {pattern, state, env} = expand(pattern, state, %{env | context: :match})
    {pattern, state, %{env | context: context}}
  end

  defp expand_directive_opts(opts, state, env) do
    opts =
      Keyword.replace_lazy(opts, :as, fn
        {:__aliases__, _, list} -> Module.concat(list)
        other -> other
      end)

    expand(opts, state, env)
  end

  defp expand_list(ast, state, env), do: expand_list(ast, state, env, [])

  defp expand_list([], state, env, acc) do
    {Enum.reverse(acc), state, env}
  end

  defp expand_list([h | t], state, env, acc) do
    {h, state, env} = expand(h, state, env)
    expand_list(t, state, env, [h | acc])
  end

  defp expand_quote(ast, state, env) do
    {_, {state, env}} =
      Macro.prewalk(ast, {state, env}, fn
        # We need to traverse inside unquotes
        {unquote, _, [expr]}, {state, env} when unquote in [:unquote, :unquote_splicing] ->
          {_expr, state, env} = expand(expr, state, env)
          {:ok, {state, env}}

        # If we find a quote inside a quote, we stop traversing it
        {:quote, _, [_]}, acc ->
          {:ok, acc}

        {:quote, _, [_, _]}, acc ->
          {:ok, acc}

        # Otherwise we go on
        node, acc ->
          {node, acc}
      end)

    {ast, state, env}
  end

  defp put_mlir_var(state, name, val) when is_atom(name) do
    update_in(state.mlir.vars, &Map.put(&1, name, val))
  end

  defp put_mlir_var(state, {name, _meta, _ctx}, val) do
    put_mlir_var(state, name, val)
  end

  defp get_mlir_var(state, {name, _meta, _ctx}) do
    Map.get(state.mlir.vars, name)
  end

  defp get_mlir_var(_state, _ast) do
    nil
  end

  defp env_from_defm!(state) do
    if e = state.mlir.enif_env do
      uniq_mlir_var(state, e)
    else
      raise ArgumentError, "must be a defm with beam env as the first argument"
    end
  end

  @var_prefix "chv"
  defp uniq_mlir_var() do
    Macro.var(:"#{@var_prefix}#{System.unique_integer([:positive])}", nil)
  end

  defp uniq_mlir_var(state, val) do
    uniq_mlir_var() |> then(&{&1, put_mlir_var(state, &1, val)})
  end
end
