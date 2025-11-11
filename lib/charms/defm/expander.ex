defmodule Charms.Defm.Expander do
  @moduledoc """
  Expander is a module of functions to compile Elixir AST to MLIR.

  In MLIR, the process of creating IR from source or AST is usually called "import". While in our case, we are following Elixir's convention to call it "expansion". While we are not just expanding Elixir's AST tree, in the end one Elixir function definition `defm` will be compiled to one MLIR `func.func`.

  ## Working with Elixir's parallel compiler
  Charms will try to work with Elixir's parallel compiler in the most well-integrated way. This allows us to compile multiple modules at the same time, which can speed up the compilation process. Another benefit is that all `defm` functions can also be executed at compile time, just like vanilla Elixir functions defined by `def`. Behind the scenes, `func.func` generated from `defm` will also be used as reference to infer and check types of a remote function calls when compiling another module.
  To take deep dive, Elixir has [an official blog post about parallel compiler](https://elixir-lang.org/blog/2012/04/24/a-peek-inside-elixir-s-parallel-compiler/).

  ## Cyclic dependency
  Charms will disallow cyclic dependency. If module A calls module B, module B cannot call module A. This is to prevent infinite recursion and streamline compile-time features. Although this is technically possible as long as function calls and function definitions are following the same signature (like the separate compilation of C/C++ files and linkage), we still make this trade-off for simplicity.

  ## Compile an AST without enough type information
  Being an dynamic language, it is possible for Elixir to have an AST that is valid in Elixir but not in MLIR. For example, Elixir allows to define a serial of nested function calls without any return type, like `a(b(c()))`. In Elixir everything is expression so a function call is assumed to always return a value while in MLIR it is possible to have a function call that does not return anything (equivalent to a void function in C). In this case, Charms will generate a `ub.poison` operation with result type `none`. If the result value of created `ub.poison` op will never be used, nothing will happen. If used, it will raise an error in later verification or passes. This is meant to allow Elixir code to work with the AST with interest only on Elixir semantic keep going without interruption as much as possible, and limit the error information to the type level, instead of leaking it to the syntax level.

  ## Return type convention
  MLIR allows multiple values as a function return, while Elixir only allows one. To keep the convention, internally Charms will always use a list to store function's return type. If the function has only one return, it will be a list with one element. If the function has no return, it will be an empty list. This should streamline the transformation and pattern matching on the function signature.
  """
  use Beaver
  alias MLIR.Attribute
  alias MLIR.Dialect.{Func, CF, SCF, MemRef, Index, Arith, GPU}
  alias Charms.Defm.Expander.MLIRImporter
  alias Charms.Defm.Expander.Call
  require Beaver.MLIR.Dialect.Func
  require GPU
  import Charms.Diagnostic, only: :macros
  # Define the environment we will use for expansion.
  # We reset the fields below but we will need to set
  # them accordingly later on.

  @env %{
    Macro.Env.prune_compile_info(__ENV__)
    | line: 0,
      file: "nofile",
      module: nil,
      function: nil,
      context_modules: []
  }
  defp env, do: @env

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
  # to programmatically report compiler errors and the ability to
  # track variables. This may be added in the future.
  @doc """
  Expand an Elixir AST into MLIR.

  Note that this function will not do any resource management, like destroying MLIR context or module. So this function should be used with care, and mainly for testing or debugging purpose.
  """
  def expand(ast, file) do
    NimblePool.checkout!(Charms.ContextPool, :checkout, fn _, ctx ->
      mlir = __MODULE__.MLIRImporter.new(:best_effort, ctx)

      {expand(
         ast,
         %{attrs: [], remotes: [], locals: [], definitions: [], vars: [], mlir: mlir},
         %{env() | file: file}
       ), ctx}
    end)
  end

  @doc """
  Expand an AST into MLIR.
  """
  def expand_to_mlir(ast, env, %__MODULE__.MLIRImporter{ctx: ctx} = mlir_expander) do
    available_ops = MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx))
    mlir_expander = mlir_expander |> Map.put(:available_ops, available_ops)

    expand(
      ast,
      %{attrs: [], remotes: [], locals: [], definitions: [], vars: [], mlir: mlir_expander},
      env
    )
  end

  defp expand_body(body, args, arg_types, state, env) do
    mlir ctx: state.mlir.ctx do
      b =
        block do
          MLIR.Block.add_args!(Beaver.Env.block(), arg_types, ctx: Beaver.Env.context())

          arg_values =
            Range.new(0, length(args) - 1, 1)
            |> Enum.map(&MLIR.Block.get_arg!(Beaver.Env.block(), &1))

          state =
            Enum.zip(args, arg_values)
            |> Enum.reduce(state, fn {k, v}, state ->
              update_in(state.mlir, &MLIRImporter.put_var(&1, k, v))
            end)

          state =
            with [head_arg_type | _] <- arg_types,
                 MLIR.equal?(head_arg_type, Beaver.ENIF.Type.env(ctx: state.mlir.ctx)),
                 [{:env, _, nil} | _] <- args do
              a = MLIR.Block.get_arg!(Beaver.Env.block(), 0)
              put_in(state.mlir.enif_env, a)
            else
              _ -> state
            end

          state = put_in(state.mlir.blk, Beaver.Env.block())

          {body, state, env} =
            expand(body, state, env)

          case body do
            [%MLIR.Value{} = v] ->
              cond do
                MLIR.Value.argument?(v) ->
                  loc = MLIR.Location.from_env(env)
                  Func.return(v, loc: loc) >>> []

                (op = MLIR.Value.owner!(v)) && MLIR.Operation.name(op) != "func.return" ->
                  Func.return(v, loc: MLIR.Operation.location(op)) >>> []
              end

            _ ->
              # an pass will insert `func.return` of no operand based on function signature
              nil
          end
        end

      {b, state, env}
    end
  end

  # expand with bindings that are not leaked
  def expand_with_bindings(ast, state, env, bindings) do
    state_with_bindings =
      for {var, val} <- bindings, reduce: state do
        state -> update_in(state.mlir, &MLIRImporter.put_var(&1, var, val))
      end

    {v, _state, _env} = expand(ast, state_with_bindings, env)
    {v, state, env}
  end

  defp clear_deferrals(state) do
    put_in(state.mlir.deferrals, [])
  end

  defp expand_deferrals(state) do
    # each defer statement's environment should not inferrer with each other
    # this discourage using binding expression in one-line defer, but it is supported in defer-block
    for {expression, state, env} <- state.mlir.deferrals do
      expand(expression, state, env)
    end
  end

  def expand_deferrals_before_terminator(op, state) do
    is_terminator =
      MLIR.CAPI.beaverIsOpNameTerminator(MLIR.StringRef.create(op), state.mlir.ctx)
      |> Beaver.Native.to_term()

    if is_terminator do
      expand_deferrals(state)
      clear_deferrals(state)
    else
      state
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

  def expand([_ | _] = list, state, env) do
    expand_list(list, state, env)
  end

  def expand({left, right}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{left, right}, state, env}
  end

  def expand({:{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:{}, meta, args}, state, env}
  end

  def expand({:%{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:%{}, meta, args}, state, env}
  end

  def expand({:|, meta, [left, right]}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{:|, meta, [left, right]}, state, env}
  end

  def expand({:<<>>, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:<<>>, meta, args}, state, env}
  end

  ## __block__

  def expand({:__block__, _, list}, state, env) do
    # clear deferrals from enclosing block, they should not escape
    state = clear_deferrals(state)

    expand_list(list, state, env)
    |> tap(fn {_, s, _} -> expand_deferrals(s) end)
    |> then(fn
      {[], _, e} -> raise_compile_error(e, "block can't be empty")
      {l, s, e} when is_list(l) -> {List.last(l), s, e}
      {l, _, e} -> raise_compile_error(e, "Expected a list, got: #{inspect(l)}")
    end)
  end

  ## __aliases__

  def expand({:__aliases__, meta, [head | tail] = list}, state, env) do
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

  def expand({form, meta, [arg]}, state, env) when form in [:require, :alias, :import] do
    expand({form, meta, [arg, []]}, state, env)
  end

  def expand({:alias, meta, [arg, opts]}, state, env) do
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

  def expand({:require, meta, [arg, opts]}, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    # An actual compiler would raise if the module is not defined or if the require fails.
    case Macro.Env.define_require(env, meta, arg, [trace: true] ++ opts) do
      {:ok, env} -> {arg, state, env}
      {:error, _} -> {arg, state, env}
    end
  end

  def expand({:import, meta, [arg, opts]}, state, env) do
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

  @intrinsics Charms.Kernel.macro_intrinsics() ++ Charms.Kernel.intrinsics()
  def expand({fun, _meta, args}, state, env) when fun in @intrinsics do
    loc = MLIR.Location.from_env(env)

    try do
      intrinsic_impl = Charms.Kernel.__intrinsics__(fun, length(args))

      unless intrinsic_impl do
        raise_compile_error(env, "intrinsic implementation not found for #{fun}/#{length(args)}")
      end

      Charms.Defm.Expander.Intrinsic.expand(loc, Charms.Kernel, intrinsic_impl, args, state, env)
      |> then(fn {v, _, _} ->
        if is_list(v) do
          List.last(v)
        else
          v
        end
      end)
      |> tap(fn v ->
        unless match?(%MLIR.Value{}, v) do
          raise_compile_error(env, "Expected a value, got: #{inspect(v)}")
        end
      end)
      |> then(&{&1, state, env})
    rescue
      e ->
        raise_compile_error(
          env,
          "Failed to expand kernel intrinsic #{fun}: #{Exception.message(e)}"
        )
    end
  end

  ## =/2
  # We include = as an example of how we could handle variables.
  # For example, if you want to store where variables are defined,
  # you would collect this information in expand_pattern/3 and
  # invoke it from all relevant places (such as case, cond, try, etc).

  def expand({:=, _meta, [left, right]}, state, env) do
    {left, state, env} = expand_pattern(left, state, env)
    {right, state, env} = expand(right, state, env)
    state = update_in(state.mlir, &MLIRImporter.put_var(&1, left, right))
    {right, state, env}
  end

  ## quote/1, quote/2
  # We need to expand options and look inside unquote/unquote_splicing.
  # A custom compiler may want to raise on this special form (for example),
  # quoted expressions make no sense if you are writing a language that
  # compiles to C.

  def expand({:quote, _, [opts]}, state, env) do
    {block, opts} = Keyword.pop(opts, :do)
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(block, state, env)
  end

  def expand({:quote, _, [opts, block_opts]}, state, env) do
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(Keyword.get(block_opts, :do), state, env)
  end

  ## Pin operator
  # It only appears inside match and it disables the match behaviour.

  def expand({:^, _meta, [arg]}, state, %{context: context} = env) do
    {b, state, env} = expand(arg, state, %{env | context: nil})
    match?(%MLIR.Block{}, b) || raise_compile_error(env, "Expected a block, got: #{inspect(b)}")

    br =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        CF.br({b, []}) >>> []
      end

    {br, state, %{env | context: context}}
  end

  ## Remote call

  def expand({{:., dot_meta, [Access, :get]}, meta, args}, state, env) do
    env = %{env | line: dot_meta[:line] || meta[:line] || env.line}
    {[arr, i], state, env} = expand_list(args, state, env)

    quote do
      Charms.Pointer.element_ptr(arr, i)
      |> Charms.Pointer.load()
    end
    |> expand_with_bindings(state, env, arr: arr, i: i)
  end

  def expand({{:., _dot_meta, [_module, fun]}, _meta, args} = ast, state, env)
      when is_atom(fun) and is_list(args) do
    Call.expand_remote(ast, state, env)
  end

  ## Auxiliary containers
  def expand({:"::", meta, [param, type]}, state, env) do
    {param, state, env} = expand(param, state, env)
    {type, state, env} = expand(type, state, env)
    {{:"::", meta, [param, type]}, state, env}
  end

  ## Imported or local call

  def expand({fun, _meta, args} = ast, state, env) when is_atom(fun) and is_list(args) do
    Call.expand_local_or_import(ast, state, env)
  end

  ## __MODULE__, __DIR__, __ENV__, __CALLER__
  # A custom compiler may want to raise.

  def expand({:__MODULE__, _, ctx}, state, env) when is_atom(ctx) do
    {env.module, state, env}
  end

  def expand({:__DIR__, _, ctx}, state, env) when is_atom(ctx) do
    {Path.dirname(env.file), state, env}
  end

  def expand({:__ENV__, _, ctx}, state, env) when is_atom(ctx) do
    {Macro.escape(env), state, env}
  end

  def expand({:__CALLER__, _, ctx} = ast, state, env) when is_atom(ctx) do
    {ast, state, env}
  end

  ## var
  # For the language server, we only want to capture definitions,
  # we don't care when they are used.

  def expand({var, meta, ctx} = ast, state, %{context: :match} = env)
      when is_atom(var) and is_atom(ctx) do
    ctx = Keyword.get(meta, :context, ctx)
    state = update_in(state.vars, &[{var, ctx} | &1])
    {ast, state, env}
  end

  ## Fallback

  @const_prefix "chc"
  def expand(ast, state, env) when is_binary(ast) do
    s_table = state.mlir.mod |> MLIR.Operation.from_module() |> MLIR.CAPI.mlirSymbolTableCreate()
    sym_name = @const_prefix <> "#{:erlang.phash2(ast)}"
    found = MLIR.CAPI.mlirSymbolTableLookup(s_table, MLIR.StringRef.create(sym_name))
    loc = MLIR.Location.from_env(env)

    mlir ctx: state.mlir.ctx, blk: MLIR.Module.body(state.mlir.mod) do
      if MLIR.null?(found) do
        MemRef.global(ast, sym_name: Attribute.string(sym_name), loc: loc) >>> :infer
      else
        found
      end
      |> then(
        &mlir blk: state.mlir.blk do
          name = Attribute.flat_symbol_ref(Attribute.unwrap(&1[:sym_name]))
          MemRef.get_global(name: name, loc: loc) >>> Attribute.unwrap(&1[:type])
        end
      )
    end
    |> then(&{&1, state, env})
  end

  def expand(ast, state, env) do
    case MLIRImporter.get_var(state.mlir, ast) do
      {:ok, val} ->
        {val, state, env}

      {:undefined_variable, _name} = uv ->
        {uv, state, env}

      {:skip, ast} ->
        {ast, state, env}
    end
  end

  # Expands a nil clause body in an if statement, yielding no value.
  defp expand_if_clause_body(nil, state, _env) do
    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      SCF.yield() >>> []
      []
    end
  end

  # Expands a non-nil clause body in an if statement, yielding the last evaluated value.
  defp expand_if_clause_body(clause_body, state, env) do
    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      {ret, _, _} = expand(clause_body, state, env)

      case ret do
        %MLIR.Operation{} ->
          SCF.yield() >>> []
          []

        %MLIR.Value{} = last ->
          SCF.yield(last) >>> []
          MLIR.Value.type(last)
      end
    end
  end

  # convert op name ast to a string
  defp normalize_dot_op_name(ast) do
    ast |> Macro.to_string() |> String.replace([":", " "], "")
  end

  defp expand_defm(convention, call, body, state, env) do
    {:"::", _, [call, ret_types]} = call

    {name, args} = Macro.decompose_call(call)
    name = Charms.Defm.mangling(env.module, name)

    {args, arg_types} =
      for {:"::", _, [a, t]} <- args do
        {a, t}
      end
      |> Enum.unzip()

    parent_block = state.mlir.blk

    f =
      mlir ctx: state.mlir.ctx, blk: parent_block do
        {ret_types, state, env} = ret_types |> expand(state, env)
        {arg_types, state, env} = arg_types |> expand(state, env)

        if i = Enum.find_index(arg_types, &(!is_struct(&1, MLIR.Type))) do
          raise_compile_error(
            env,
            "invalid argument type ##{i + 1}, #{inspect(Enum.at(arg_types, i))}"
          )
        end

        if Enum.find(List.wrap(ret_types), &(!is_struct(&1, MLIR.Type))) do
          raise_compile_error(env, "invalid return type, #{inspect(ret_types)}")
        end

        ft = Type.function(arg_types, ret_types, ctx: Beaver.Env.context())

        case convention do
          :defm ->
            Func.func _(
                        sym_name: "\"#{name}\"",
                        function_type: ft,
                        loc: MLIR.Location.from_env(env)
                      ) do
              region do
              end
            end

          :defk ->
            Func.func_like _(
                             sym_name: "\"#{name}\"",
                             function_type: ft,
                             "gpu.kernel": MLIR.Attribute.unit(),
                             loc: MLIR.Location.from_env(env)
                           ),
                           "gpu.func" do
              region do
              end
            end
        end
      end

    r0 = Beaver.Walker.regions(f) |> Enum.at(0)
    state = put_in(state.mlir.region, r0)

    {f, state, env} =
      case body do
        {:__block__, _, []} ->
          f = put_in(f[:sym_visibility], MLIR.Attribute.string("private"))
          {f, state, env}

        _ ->
          # create and insert the entry block to expose the state variable
          {blk_entry, state, env} =
            body |> List.wrap() |> expand_body(args, arg_types, state, env)

          MLIR.Region.insert(r0, 0, blk_entry)
          {f, state, env}
      end

    # restore the block back to parent, otherwise consecutive functions will be created nested
    state = put_in(state.mlir.blk, parent_block)
    {f, state, env}
  end

  ## Macro handling

  # This is going to be the function where you will intercept expansions
  # and attach custom behaviour. As an example, we will capture the module
  # definition, fully replacing the actual implementation. You could also
  # use this to capture module attributes (optionally delegating to the actual
  # implementation), function expansion, and more.
  def expand_macro(meta, Kernel, :defmodule, [alias, [do: block]], _callback, state, env) do
    {expanded, state, env} = expand(alias, state, env)

    state =
      put_in(
        state.mlir.mod,
        mlir ctx: state.mlir.ctx do
          module sym_name: Attribute.string(expanded) do
          end
        end
      )

    state = put_in(state.mlir.blk, MLIR.Module.body(state.mlir.mod))

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

  def expand_macro(_meta, Charms, :defmstruct_impl, [module, fields], _callback, state, env) do
    {struct_t, _field_names} = Charms.Defmstruct.Definition.expand(module, fields, state, env)
    {struct_t, state, env}
  end

  def expand_macro(_meta, Charms, :defmstruct, [fields], _callback, state, env) do
    {struct_t, field_names} = Charms.Defmstruct.Definition.expand(env.module, fields, state, env)

    Charms.Struct.save_as_module_attribute(state.mlir.ctx, state.mlir.mod, struct_t, field_names)
    {nil, state, env}
  end

  def expand_macro(_meta, Charms, :defm, [call, [do: body]], _callback, state, env) do
    expand_defm(:defm, call, body, state, env)
  end

  def expand_macro(_meta, Charms, :defk, [call, [do: body]], _callback, state, env) do
    mod = MLIR.Operation.from_module(state.mlir.mod)
    _ = put_in(mod["gpu.container_module"], MLIR.Attribute.unit(ctx: state.mlir.ctx))
    parent_block = state.mlir.blk

    gpu_module =
      MLIR.Operation.with_symbol_table(mod, fn s_table ->
        existing_gpu_module =
          MLIR.CAPI.mlirSymbolTableLookup(
            s_table,
            MLIR.StringRef.create(Charms.GPU.gpu_module_name())
          )

        if MLIR.null?(existing_gpu_module) do
          mlir ctx: state.mlir.ctx, blk: parent_block do
            GPU.module sym_name: Attribute.string(Charms.GPU.gpu_module_name()) do
              region do
                block do
                end
              end
            end >>> []
          end
        else
          existing_gpu_module
        end
      end)

    gpu_block =
      gpu_module |> Beaver.Walker.regions() |> Enum.at(0) |> Beaver.Walker.blocks() |> Enum.at(0)

    state = put_in(state.mlir.blk, gpu_block)
    {result, state, env} = expand_defm(:defk, call, body, state, env)
    state = put_in(state.mlir.blk, parent_block)
    {result, state, env}
  end

  def expand_macro(_meta, Beaver, :block, args, _callback, state, env) do
    b =
      mlir ctx: state.mlir.ctx do
        block do
          [[do: body]] = args
          body |> expand(put_in(state.mlir.blk, Beaver.Env.block()), env)
        end
      end

    MLIR.Region.append(state.mlir.region, b)
    {b, state, env}
  end

  def expand_macro(_meta, Charms.Defm, :cond_br, [condition, clauses], _callback, state, env) do
    true_body = Keyword.fetch!(clauses, :do)
    false_body = Keyword.fetch!(clauses, :else)
    {condition, state, env} = expand(condition, state, env)

    v =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        true_body =
          block do
            expand(true_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end
          |> tap(&MLIR.Region.append(state.mlir.region, &1))

        false_body =
          block do
            expand(false_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end
          |> tap(&MLIR.Region.append(state.mlir.region, &1))

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
  def expand_macro(_meta, Kernel, :if, [condition, clauses], _callback, state, env) do
    true_body = Keyword.fetch!(clauses, :do)
    false_body = clauses[:else]
    {condition, state, env} = expand(condition, state, env)
    loc = MLIR.Location.from_env(env)

    v =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        cond_type = MLIR.Value.type(condition)
        bool_type = Type.i1(ctx: state.mlir.ctx)
        # Ensure the condition is a i1, if not compare it to 0
        condition =
          if MLIR.equal?(cond_type, bool_type) do
            condition
          else
            zero =
              Arith.constant(value: Attribute.integer(cond_type, 0), loc: loc) >>> cond_type

            Arith.cmpi(condition, zero, predicate: Arith.cmp_i_predicate(:sgt), loc: loc) >>>
              Type.i1()
          end

        b =
          block _true() do
            ret_t =
              expand_if_clause_body(true_body, put_in(state.mlir.blk, Beaver.Env.block()), env)
          end

        SCF.if [condition, loc: loc] do
          region do
            MLIR.Region.append(Beaver.Env.region(), b)
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

  def expand_macro(_meta, Charms.Defm, :while, [expr, [do: body]], _callback, state, env) do
    loc = MLIR.Location.from_env(env)

    v =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        SCF.while loc: loc do
          region do
            block _() do
              {condition, _state, _env} =
                expand(expr, put_in(state.mlir.blk, Beaver.Env.block()), env)

              unless match?(%MLIR.Value{}, condition) do
                raise_compile_error(env, "Expected a value, got: #{inspect(condition)}")
              end

              SCF.condition(condition, loc: loc) >>> []
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

  def expand_macro(_meta, Charms.Defm, :for_loop, [expr, [do: body]], _callback, state, env) do
    {:<-, _, [{element, index}, {ptr, len}]} = expr
    {len, state, env} = expand(len, state, env)
    {ptr, state, env} = expand(ptr, state, env)

    if not Charms.Pointer.memref_ptr?(ptr) do
      raise_compile_error(env, "Expected a pointer")
    end

    t = MLIR.Value.type(ptr) |> MLIR.ShapedType.element_type()
    loc = MLIR.Location.from_env(env)

    v =
      mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
        zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
        lower_bound = zero
        upper_bound = Index.casts(len) >>> Type.index()
        step = Index.constant(value: Attribute.index(1)) >>> Type.index()

        SCF.for [lower_bound, upper_bound, step, loc: loc] do
          region do
            block _body(index_val >>> Type.index()) do
              element_val = MemRef.load(ptr, index_val, loc: loc) >>> t
              state = update_in(state.mlir, &MLIRImporter.put_var(&1, element, element_val))
              state = update_in(state.mlir, &MLIRImporter.put_var(&1, index, index_val))
              expand(body, put_in(state.mlir.blk, Beaver.Env.block()), env)
              SCF.yield() >>> []
            end
          end
        end >>> []
      end

    {v, state, env}
  end

  def expand_macro(_meta, Charms.Defm, :launch!, [call, blocks, threads], _callback, state, env) do
    {call, state, env} = expand(call, state, env)
    {blocks, state, env} = expand(blocks, state, env)
    {threads, state, env} = expand(threads, state, env)

    quote do
      Charms.GPU.launch(call, blocks, threads) |> Charms.GPU.await()
    end
    |> expand_with_bindings(state, env, call: call, blocks: blocks, threads: threads)
  end

  def expand_macro(_meta, Charms.Defm, :set!, [index_expression, value], _callback, state, env) do
    {{:., dot_meta, [Access, :get]}, meta, [arr, i]} = index_expression
    env = %{env | line: dot_meta[:line] || meta[:line] || env.line}
    {value, state, env} = expand(value, state, env)
    {i, state, env} = expand(i, state, env)
    {arr, state, env} = expand(arr, state, env)

    quote do
      Charms.Pointer.store(value, Charms.Pointer.element_ptr(arr, i))
    end
    |> expand_with_bindings(state, env, arr: arr, i: i, value: value)
  end

  def expand_macro(_meta, Charms.Defm, :defer, expression, _callback, state, env) do
    state = update_in(state.mlir.deferrals, &[{expression, state, env} | &1])
    {:defer, state, env}
  end

  def expand_macro(_meta, Charms.Defm, :op, [call], _callback, state, env) do
    {call, return_types} = Charms.Definition.Call.decompose_return_signature(call)
    {{dialect, _, _}, op, args} = Macro.decompose_call(call)
    dialect = normalize_dot_op_name(dialect)
    {args, state, env} = expand_list(args, state, env)
    {return_types, state, env} = expand_list(return_types, state, env)
    Charms.Defm.Expander.Dialect.expand(dialect, op, args, return_types, state, env)
  end

  def expand_macro(meta, Charms.Defm, :value, [call], callback, state, env) do
    {op, state, env} =
      expand_macro(meta, Charms.Defm, :op, [call], callback, state, env)

    {MLIR.Operation.results(op), state, env}
  end

  def expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  def expand_macro_callback(meta, _module, _fun, args, callback, state, env) do
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

  def export_intrinsics?(module, fun, arity) do
    match?({:module, _}, Code.ensure_compiled(module)) and Code.ensure_loaded?(module) and
      function_exported?(module, :__intrinsics__, 2) and module.__intrinsics__(fun, arity)
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

  def expand_list(ast, state, env), do: expand_list(ast, state, env, [])

  def expand_list([], state, env, acc) do
    {Enum.reverse(acc), state, env}
  end

  def expand_list([h | t], state, env, acc) do
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
end
