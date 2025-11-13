defmodule Charms.Definition do
  @moduledoc """
  Charms.Definition provides functions to define and compile `defm` functions that can be JIT/AOT compiled to native targets.

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
  alias MLIR.Dialect.{Func, GPU}
  require Func
  require Logger
  import Charms.Diagnostic

  defstruct [:name, :args, :env, :call, :ret_types, :body, :exported, :convention]

  defp new(env, call, body, opts) do
    convention = Keyword.get(opts, :convention, :defm)
    {call, ret_types} = Charms.Definition.Call.decompose_return_signature(call)
    call = Charms.Definition.Call.normalize(call)
    {name, args} = Macro.decompose_call(call)
    {:ok, env} = Macro.Env.define_import(env, [], Charms.Defm, warn: false, only: :macros)

    %__MODULE__{
      name: name,
      env: env,
      args: args,
      ret_types: ret_types,
      body: body,
      convention: convention
    }
  end

  defp placeholder_args(%__MODULE__{args: args}) do
    placeholder_args(args)
  end

  defp placeholder_args(args) when is_list(args) do
    for {:"::", _, [a, _t]} <- args do
      {var, meta, ctx} = a
      {var, Keyword.put_new(meta, :generated, true), ctx}
    end
  end

  defp invoke_args(%__MODULE__{args: [{:"::", _, [{:env, _, _}, _]} | invoke_args]}) do
    placeholder_args(invoke_args)
  end

  defp invoke_args(%__MODULE__{args: args}) do
    placeholder_args(args)
  end

  defp invoker(%__MODULE__{env: env, name: name} = d) do
    invoke_args = invoke_args(d)
    placeholder_args = placeholder_args(d)

    # generate placeholder to have module system and LSP tools happy
    fallback_placeholder =
      if length(placeholder_args) != length(invoke_args) do
        quote do
          @doc false
          def unquote(name)(unquote_splicing(placeholder_args)) do
            raise ArgumentError,
                  "Call #{Exception.format_mfa(__MODULE__, unquote(name), unquote(length(invoke_args)))} to invoke the JIT compiled function."
          end
        end
      else
        nil
      end

    quote do
      unquote(fallback_placeholder)

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
  def declare(env, call, body, opts \\ []) do
    d = new(env, call, body, opts)
    {name, args, _ret_types} = Charms.Definition.Call.decompose_call(call)
    fa = {name, length(args)}

    infer_type_helpers =
      [
        quote do
          def infer_return_type(unquote(fa), state) do
            {ret_types, _, _} =
              Charms.Defm.Expander.expand(
                unquote(Macro.escape(d.ret_types)),
                state,
                unquote(Macro.escape(env))
              )

            ret_types
          end
        end
        | for {{:"::", _, [_variable, t]}, index} <- Enum.with_index(args) do
            quote do
              def infer_argument_type(unquote(fa), unquote(index), state) do
                {t, _, _} =
                  Charms.Defm.Expander.expand(
                    unquote(Macro.escape(t)),
                    state,
                    unquote(Macro.escape(env))
                  )

                t
              end
            end
          end
      ]

    quote do
      Module.put_attribute(__MODULE__, :__charm_function__, unquote(Macro.escape(d)))
      unquote(invoker(d))
      unquote_splicing(infer_type_helpers)
    end
  end

  # if it is single block with no terminator, add a return
  defp complete_function_termination(func, terminator) do
    with [r] <- Beaver.Walker.regions(func) |> Enum.to_list(),
         [b] <- Beaver.Walker.blocks(r) |> Enum.to_list(),
         last_op = %MLIR.Operation{} <-
           Beaver.Walker.operations(b) |> Enum.to_list() |> List.last(),
         last_op_name <- MLIR.Operation.name(last_op),
         false <- last_op_name == terminator do
      case func[:function_type]
           |> MLIR.Attribute.unwrap()
           |> MLIR.CAPI.mlirFunctionTypeGetNumResults()
           |> Beaver.Native.to_term() do
        0 ->
          mlir ctx: MLIR.context(func), blk: b do
            case terminator do
              "func.return" ->
                Func.return(loc: MLIR.Operation.location(func)) >>> []

              "gpu.return" ->
                GPU.return(loc: MLIR.Operation.location(func)) >>> []
            end
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

  defp infer_argument_type(ast_map, fa, index, state) do
    ast = ast_map[{fa, index}]

    if ast do
      {t_ast, env_ast} = ast

      {t, _, _} =
        Charms.Defm.Expander.expand(
          t_ast,
          state,
          env_ast
        )

      t
    end
  end

  defp infer_return_type(ast_map, fa, state) do
    ast = ast_map[fa]

    if ast do
      {t_ast, env_ast} = ast

      {t, _, _} =
        Charms.Defm.Expander.expand(
          t_ast,
          state,
          env_ast
        )

      t
    end
  end

  defp extract_exports(m, definitions) do
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
  end

  defp compile_to_mlir_module(ctx, definitions, defmstruct_definition) do
    # this function might be called at compile time, so we need to ensure the application is started
    :ok = Application.ensure_started(:kinda)
    :ok = Application.ensure_started(:beaver)
    m = MLIR.Module.create!("", ctx: ctx)

    mlir ctx: ctx, blk: MLIR.Module.body(m) do
      mlir_importer = Charms.Defm.Expander.MLIRImporter.new(:strict_typing, ctx)

      mlir_importer = %Charms.Defm.Expander.MLIRImporter{
        mlir_importer
        | mod: m,
          blk: Beaver.Env.block()
      }

      if defmstruct_definition do
        %Charms.Defmstruct.Definition{
          fields: fields,
          env: env
        } = defmstruct_definition

        quote(do: defmstruct(unquote(fields)))
        |> Charms.Defm.Expander.expand_to_mlir(env, mlir_importer)
      end

      ast_map =
        for %__MODULE__{name: name, env: env, ret_types: ret_types, args: args} <- definitions do
          fa = {name, length(args)}

          [
            {fa, {ret_types, env}}
            | for {arg, index} <- Enum.with_index(args) do
                {:"::", _, [_variable, t]} = arg
                {{fa, index}, {t, env}}
              end
          ]
        end
        |> List.flatten()
        |> Map.new()

      mlir_importer = %Charms.Defm.Expander.MLIRImporter{
        mlir_importer
        | infer_argument_type: &infer_argument_type(ast_map, &1, &2, &3),
          infer_return_type: &infer_return_type(ast_map, &1, &2)
      }

      required_intrinsic_modules =
        for %__MODULE__{
              convention: convention,
              env: env,
              name: name,
              args: args,
              ret_types: ret_types,
              body: body
            } <-
              definitions,
            reduce: MapSet.new() do
          required_intrinsic_modules ->
            {_, state, _} =
              quote(do: unquote(name)(unquote_splicing(args)) :: unquote(ret_types))
              |> then(&quote(do: unquote(convention)(unquote(&1), unquote(body))))
              |> Charms.Defm.Expander.expand_to_mlir(env, mlir_importer)

            MapSet.union(state.mlir.required_intrinsic_modules, required_intrinsic_modules)
        end
    end

    exports = extract_exports(m, definitions)

    m
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.nested(
      "func.func",
      {"complete_function_termination", "func.func",
       &complete_function_termination(&1, "func.return")}
    )
    |> Beaver.Composer.nested(
      "gpu.module",
      [
        {"gpu.func",
         [
           {"complete_function_termination_gpu", "gpu.func",
            &complete_function_termination(&1, "gpu.return")}
         ]}
      ]
    )
    |> Beaver.Composer.append(Charms.Defm.Pass.CreateAbsentFunc)
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.run!(print: Charms.Debug.step_print?(), verifier: false)
    |> MLIR.Transform.canonicalize()
    |> run_composer_with_diagnostics()
    |> then(
      &{MLIR.to_string(&1, bytecode: true), referenced_modules(&1), required_intrinsic_modules,
       exports}
    )
  end

  defp run_composer_with_diagnostics(op) do
    case Beaver.Composer.run(op, print: Charms.Debug.step_print?(), verifier: true) do
      {:ok, op, []} ->
        op

      {:ok, op, diagnostics} ->
        Logger.debug(fn -> "Diagnostics after optimization:\n#{diagnostics}" end)
        op

      {:error, diagnostics} ->
        raise_compile_error(__ENV__, Beaver.MLIR.Diagnostic.format(diagnostics))
    end
  end

  @doc """
  Compile definitions into an MLIR module.

  ## Partial and lazy compilation
  When compiling `defm`, Charms performs preprocessing before the definition is fully translated to the target language or IR. Notable preprocessing steps include:
  - Extracting the return type of the `defm` definition, and wrapping it as an anonymous function to be called on-demand at the invocation site.
  - Determine the MLIR ops available in the definition.
  """
  def compile(definitions, defmstruct_definition) when is_list(definitions) do
    ctx = MLIR.Context.create()

    try do
      compile_to_mlir_module(ctx, definitions, defmstruct_definition)
    after
      MLIR.Context.destroy(ctx)
    end
  end
end
