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

  defstruct [:name, :args, :env, :call, :ret_types, :body, :exported]

  defp new(env, call, body) do
    {call, ret_types} = Charms.Defm.Expander.decompose_call_signature(call)
    call = normalize_call(call)
    {name, args} = Macro.decompose_call(call)
    {:ok, env} = Macro.Env.define_import(env, [], Charms.Defm, warn: false, only: :macros)

    exported =
      case args do
        [{:"::", _, [{:env, _, nil}, _]} | _] ->
          true

        _ ->
          false
      end

    %__MODULE__{
      name: name,
      env: env,
      args: args,
      call: call,
      ret_types: ret_types,
      body: body,
      exported: exported
    }
  end

  defp invoke_args(%__MODULE__{args: args}) do
    [_ | invoke_args] =
      for {:"::", _, [a, _t]} <- args do
        a
      end

    invoke_args
  end

  defp invoker(%__MODULE__{exported: false}), do: nil

  defp invoker(%__MODULE__{exported: true, env: env, name: name} = d) do
    invoke_args = invoke_args(d)

    quote do
      def unquote(name)(unquote_splicing(invoke_args)) do
        mfa = {unquote(env.module), unquote(name), unquote(invoke_args)}

        cond do
          @init_at_fun_call ->
            {_, %Charms.JIT{engine: engine} = jit} = Charms.JIT.init(__MODULE__)
            Charms.JIT.invoke(engine, mfa)

          (engine = Charms.JIT.engine(__MODULE__)) != nil ->
            Charms.JIT.invoke(engine, mfa)

          true ->
            &Charms.JIT.invoke(&1, mfa)
        end
      end
    end
  end

  @doc """
  Declare a function that can be JIT compiled and generate Elixir invoker function.
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
    Beaver.Walker.postwalk(op, fn
      %MLIR.Operation{} = op ->
        if MLIR.Operation.name(op) == "ub.poison" do
          if msg = Beaver.Walker.attributes(op)["msg"] do
            msg = MLIR.CAPI.mlirStringAttrGetValue(msg) |> MLIR.StringRef.to_string()
            msg <> ", " <> to_string(MLIR.Operation.location(op))
          else
            "Poison operation detected in the IR. #{to_string(op)}"
          end
          |> raise
        else
          op
        end

      ir ->
        ir
    end)

    :ok
  end

  # if it is single block with no terminator, add a return
  defp append_missing_return(func) do
    with [r] <- Beaver.Walker.regions(func) |> Enum.to_list(),
         [b] <- Beaver.Walker.blocks(r) |> Enum.to_list(),
         last_op = %MLIR.Operation{} <-
           Beaver.Walker.operations(b) |> Enum.to_list() |> List.last(),
         false <- MLIR.Operation.name(last_op) == "func.return" do
      mlir ctx: MLIR.CAPI.mlirOperationGetContext(last_op), block: b do
        results = Beaver.Walker.results(last_op) |> Enum.to_list()
        Func.return(results) >>> []
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
            "@Elixir." <> _ = name ->
              acc |> MapSet.put(extract_mangled_mod(name))

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

  @doc """
  Compile definitions into MLIR module.
  """
  def compile(definitions) when is_list(definitions) do
    import MLIR.Transforms
    ctx = MLIR.Context.create()
    m = MLIR.Module.create(ctx, "")

    mlir ctx: ctx, block: MLIR.Module.body(m) do
      mlir = %Charms.Defm.Expander{
        ctx: ctx,
        blk: Beaver.Env.block(),
        available_ops: MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx)),
        vars: Map.new(),
        region: nil,
        enif_env: nil,
        mod: m
      }

      for %__MODULE__{env: env, call: call, ret_types: ret_types, body: body} <- definitions do
        quote do
          def(unquote(call) :: unquote(ret_types), unquote(body))
        end
        |> Charms.Defm.Expander.expand_with(env, mlir)
      end
    end

    m
    |> Charms.Debug.print_ir_pass()
    |> MLIR.Pass.Composer.nested(
      "func.func",
      {"append_missing_return", "func.func", &append_missing_return/1}
    )
    |> MLIR.Pass.Composer.nested("func.func", Charms.Defm.Pass.CreateAbsentFunc)
    |> MLIR.Pass.Composer.append({"check-poison", "builtin.module", &check_poison!/1})
    |> canonicalize
    |> MLIR.Pass.Composer.run!(print: Charms.Debug.step_print?())
    |> then(&{MLIR.to_string(&1, bytecode: true), referenced_modules(&1)})
    |> tap(fn _ -> MLIR.Context.destroy(ctx) end)
  end

  defp extract_mangled_mod("@" <> name) do
    name
    |> String.split(".")
    |> then(&Enum.take(&1, length(&1) - 1))
    |> Enum.join(".")
    |> String.to_atom()
  end
end
