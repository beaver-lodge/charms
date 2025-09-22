defmodule Charms.Defm.Definition do
  @moduledoc """
  Charms.Defm.Definition provides functions to define and compile `defm` functions that can be JIT/AOT compiled to native targets.

  ## The compilation process
  The principle of the compilation process:
  - convert the Elixir code into MLIR IR while preserving the Elixir semantics.
  - work with Elixir's code analysis infrastructure and LSP-like tools.

  With this in mind, the compilation process is divided into these steps:
  1. Convert the Elixir code into MLIR IR, then optimize the IR.
  2. Analyze the AST and IR and generate proper Elixir side effects.
  3. Finally convert the IR into a bytecode representation and persist it as an Elixir string.

  ```mermaid
  graph TD
    A[Elixir Code] -->|Step 1: Convert to MLIR IR| B[MLIR IR]
    B -->|Optimize IR| C[Optimized MLIR IR]
    C -->|Step 2: Analyze AST and IR| D[Generate Elixir Side Effects]
    D --> E[Elixir Function Definitions]
    D --> F[Remote Function Calls]
    C -->|Step 3: Convert IR to Bytecode| G[Bytecode Representation]
    G -->|Persist as Elixir String| H[Persisted Bytecode]
    H -->|Consumed by JIT/AOT Engine| I[Further Optimization]
    I --> J[Native Target Code]
  ```

  The optimization before JIT/AOT should focus on:
    - Complete the semantics which usually absent in the Elixir code, such as the return type of a function.
    - Fill the IR with Elixir's debug information, including code location and function arity.
  Elixir side effects include:
    - Elixir function definitions as placeholder
    - Remote function calls to build the module dependencies
  The IR will be later consumed and further optimized by a JIT/AOT engine. It should be as target independent as possible.
  """

  require Beaver.Env
  use Beaver
  alias MLIR.Dialect.Func
  require Func
  require Logger
  import Charms.Diagnostic

  defstruct [:name, :args, :env, :call, :ret_types, :body, :exported]

  defp new(env, call, body) do
    {call, ret_types} = Charms.Defm.Expander.decompose_call_signature(call)
    call = normalize_call(call)
    {name, args} = Macro.decompose_call(call)
    {:ok, env} = Macro.Env.define_import(env, [], Charms.Defm, warn: false, only: :macros)

    %__MODULE__{
      name: name,
      env: env,
      args: args,
      call: call,
      ret_types: ret_types,
      body: body
    }
  end

  defp do_invoke_args(invoke_args) do
    for {:"::", _, [a, _t]} <- invoke_args do
      a
    end
  end

  defp invoke_args(%__MODULE__{args: [{:"::", _, [{:env, _, _}, _]} | invoke_args]}) do
    do_invoke_args(invoke_args)
  end

  defp invoke_args(%__MODULE__{args: args}) do
    do_invoke_args(args)
  end

  defp invoker(%__MODULE__{env: env, name: name} = d) do
    invoke_args = invoke_args(d)

    quote do
      def unquote(name)(unquote_splicing(invoke_args)) do
        mfa = {unquote(env.module), unquote(name), unquote(invoke_args)}
        {name, arity} = __ENV__.function

        unless __ir_exports__(name, arity) do
          raise ArgumentError,
                """
                Cannot invoke #{Exception.format_mfa(__MODULE__, name, arity)} from Elixir because it is not exported. Function gets exported only if
                - the first parameter is explicitly named as env (think of it as a special argument like "self" in some languages)
                - all arguments are Term.t()
                - returns Term.t()
                Also note that when invoking there is no need to pass env as the first argument, it will be automatically passed by the JIT engine. So a defm f(env, x :: Term.t()) :: Term.t() should be invoked as f(x) in Elixir.
                """
        end

        if @init_at_fun_call do
          {_, %Charms.JIT{engine: engine} = jit} =
            Charms.JIT.init(__MODULE__, name: __ir_digest__())

          Charms.JIT.invoke(engine, mfa)
        else
          &Charms.JIT.invoke(&1, mfa)
        end
      end
    end
  end

  @doc """
  Declare a function that can be JIT compiled and generate Elixir invoker function.

  The call signature will be decomposed and transformed into a normalized form.
  """
  def declare(env, call, body) do
    d = new(env, call, body)

    quote do
      @defm unquote(Macro.escape(d))
      unquote(invoker(d))
    end
  end

  @doc false
  defp normalize_call(call) do
    {name, args} = Macro.decompose_call(call)

    args =
      for i <- Enum.with_index(args) do
        case i do
          # env
          {a = {:env, _, nil}, 0} ->
            quote do
              unquote(a) :: Charms.Env.t()
            end

          # term
          {a = {name, _, context}, version}
          when is_atom(name) and is_atom(context) and is_integer(version) ->
            quote do
              unquote(a) :: Charms.Term.t()
            end

          # typed
          {at = {:"::", _, [_a, _t]}, _} ->
            at
        end
      end

    quote do
      unquote(name)(unquote_splicing(args))
    end
  end

  defp check_poison!(op) do
    Beaver.Walker.postwalk(op, fn op ->
      with %MLIR.Operation{} <- op,
           "ub.poison" <- MLIR.Operation.name(op) do
        loc_str = to_string(MLIR.Operation.location(op))

        compile_err_msg =
          case String.split(loc_str, ":") do
            [f, line, _col] ->
              [file: f, line: String.to_integer(line)]

            _ ->
              [file: loc_str, line: 0]
          end

        if msg = Beaver.Walker.attributes(op)["msg"] do
          MLIR.Attribute.unwrap(msg) |> MLIR.to_string()
        else
          "Poison operation detected in the IR. #{to_string(op)}"
        end
        |> then(&raise(CompileError, [{:description, &1} | compile_err_msg]))
      else
        ir ->
          ir
      end
    end)

    :ok
  end

  @default_visibility "private"
  defp declare_enif(ctx, blk, name_str) do
    mlir ctx: ctx, blk: blk do
      {arg_types, ret_types} = Beaver.ENIF.signature(ctx, String.to_atom(name_str))

      Func.func _(
                  sym_name: MLIR.Attribute.string(name_str),
                  sym_visibility: MLIR.Attribute.string(@default_visibility),
                  function_type: Type.function(arg_types, ret_types)
                ) do
        region do
        end
      end
    end
  end

  defp declared_required_enif(op) do
    mlir ctx: MLIR.context(op), blk: MLIR.Module.body(MLIR.Module.from_operation(op)) do
      declare_enif(Beaver.Env.context(), Beaver.Env.block(), "enif_alloc")
      declare_enif(Beaver.Env.context(), Beaver.Env.block(), "enif_free")
    end
  end

  # if it is single block with no terminator, add a return
  defp append_missing_return(func) do
    with [r] <- Beaver.Walker.regions(func) |> Enum.to_list(),
         [b] <- Beaver.Walker.blocks(r) |> Enum.to_list(),
         last_op = %MLIR.Operation{} <-
           Beaver.Walker.operations(b) |> Enum.to_list() |> List.last(),
         last_op_name <- MLIR.Operation.name(last_op),
         false <- last_op_name == "func.return" do
      case func[:function_type]
           |> MLIR.Attribute.unwrap()
           |> MLIR.CAPI.mlirFunctionTypeGetNumResults()
           |> Beaver.Native.to_term() do
        0 ->
          mlir ctx: MLIR.CAPI.mlirOperationGetContext(func), blk: b do
            Func.return(loc: MLIR.Operation.location(func)) >>> []
          end

        1 ->
          raise ArgumentError,
                "Expected func.return returns a single value, instead we got #{last_op_name}"

        _ ->
          raise ArgumentError, "Multiple return values are not supported yet."
      end
    else
      _ ->
        nil
    end

    :ok
  end

  defp referenced_modules(module) do
    Beaver.Walker.postwalk(module, MapSet.new(), fn
      %MLIR.Operation{} = op, acc ->
        with "func.call" <- MLIR.Operation.name(op),
             callee when not is_nil(callee) <- Beaver.Walker.attributes(op)["callee"] do
          case callee |> to_string do
            "@Elixir$" <> _ = name ->
              acc |> MapSet.put(Charms.Defm.extract_mangled_mod(name))

            _ ->
              acc
          end
          |> then(&{op, &1})
        else
          _ ->
            {op, acc}
        end

      ir, acc ->
        {ir, acc}
    end)
    |> then(fn {_, acc} -> MapSet.to_list(acc) end)
  end

  def do_compile(ctx, definitions, defmstruct_definition) do
    # this function might be called at compile time, so we need to ensure the application is started
    :ok = Application.ensure_started(:kinda)
    :ok = Application.ensure_started(:beaver)
    m = MLIR.Module.create!("", ctx: ctx)

    mlir ctx: ctx, blk: MLIR.Module.body(m) do
      mlir_expander = %Charms.Defm.Expander{
        ctx: ctx,
        blk: Beaver.Env.block(),
        available_ops: MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx)),
        vars: Map.new(),
        region: nil,
        enif_env: nil,
        mod: m
      }

      if defmstruct_definition do
        %Charms.Defmstruct.Definition{
          fields: fields,
          env: env
        } = defmstruct_definition

        quote(do: defmstruct(unquote(fields)))
        |> Charms.Defm.Expander.expand_to_mlir(env, mlir_expander)
      end

      # at this moment, we wrap it in the simplest way, before we decide what expander state to use.
      return_types =
        for %__MODULE__{name: name, env: env, ret_types: ret_types} <- definitions do
          {name,
           fn ->
             {m, _, _} =
               quote do
                 unquote(ret_types)
               end
               |> Charms.Defm.Expander.expand_to_mlir(env, mlir_expander)

             m
           end}
        end
        |> Map.new()

      mlir_expander = %Charms.Defm.Expander{mlir_expander | return_types: return_types}

      required_intrinsic_modules =
        for %__MODULE__{env: env, call: call, ret_types: ret_types, body: body} <- definitions,
            reduce: MapSet.new() do
          required_intrinsic_modules ->
            {_, state, _} =
              quote(do: unquote(call) :: unquote(ret_types))
              |> then(&quote(do: defm(unquote(&1), unquote(body))))
              |> Charms.Defm.Expander.expand_to_mlir(env, mlir_expander)

            MapSet.union(state.mlir.required_intrinsic_modules, required_intrinsic_modules)
        end
    end

    exports =
      m
      |> MLIR.Operation.from_module()
      |> MLIR.Operation.with_symbol_table(fn s_table ->
        for %__MODULE__{name: name, env: env} <- definitions, reduce: [] do
          acc ->
            func_name = Charms.Defm.mangling(env.module, name)

            func =
              MLIR.CAPI.mlirSymbolTableLookup(
                s_table,
                MLIR.StringRef.create(func_name)
              )

            if !MLIR.null?(func) and !MLIR.Dialect.Func.external?(func) do
              args_types =
                Beaver.Walker.regions(func)
                |> Enum.at(0)
                |> Beaver.Walker.blocks()
                |> Enum.at(0)
                |> Beaver.Walker.arguments()
                |> Enum.map(&MLIR.Value.type/1)

              with [env_type | args_types] <- args_types,
                   true <- Enum.all?(args_types, &MLIR.equal?(&1, Beaver.ENIF.Type.term())),
                   true <- MLIR.equal?(env_type, Beaver.ENIF.Type.env()),
                   function_type = func[:function_type] |> MLIR.Attribute.unwrap(),
                   1 <-
                     function_type
                     |> MLIR.CAPI.mlirFunctionTypeGetNumResults()
                     |> Beaver.Native.to_term(),
                   result_type <- MLIR.CAPI.mlirFunctionTypeGetResult(function_type, 0),
                   true <- MLIR.equal?(result_type, Beaver.ENIF.Type.term()) do
                [{name, length(args_types), func_name} | acc]
              else
                _ ->
                  acc
              end
            else
              acc
            end
        end
      end)

    m
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.nested(
      "func.func",
      {"append_missing_return", "func.func", &append_missing_return/1}
    )
    |> Beaver.Composer.append(Charms.Defm.Pass.CreateAbsentFunc)
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.append(
      {"declared-required-enif", "builtin.module", &declared_required_enif/1}
    )
    |> Beaver.Composer.append({"check-poison", "builtin.module", &check_poison!/1})
    |> Beaver.Composer.run!(print: Charms.Debug.step_print?(), verifier: false)
    |> MLIR.Transform.canonicalize()
    |> then(fn op ->
      case Beaver.Composer.run(op, print: Charms.Debug.step_print?(), verifier: true) do
        {:ok, op} ->
          op

        {:error, diagnostics} ->
          raise_compile_error(__ENV__, Beaver.MLIR.Diagnostic.format(diagnostics))
      end
    end)
    |> then(
      &{MLIR.to_string(&1, bytecode: true), referenced_modules(&1), required_intrinsic_modules,
       exports}
    )
  end

  @doc """
  Compile definitions into MLIR module.
  Compile definitions into an MLIR module.

  ## Partial and lazy compilation
  When compiling `defm`, Charms performs preprocessing before the definition is fully translated to the target language or IR. Notable preprocessing steps include:
  - Extracting the return type of the `defm` definition, and wrapping it as an anonymous function to be called on-demand at the invocation site.
  - Determine the MLIR ops available in the definition.
  """
  def compile(definitions, defmstruct_definition) when is_list(definitions) do
    ctx = MLIR.Context.create()

    try do
      do_compile(ctx, definitions, defmstruct_definition)
    after
      MLIR.Context.destroy(ctx)
    end
  end
end
