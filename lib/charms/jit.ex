defmodule Charms.JIT do
  @moduledoc """
  Compile and execute MLIR modules generated from `Charms.Defm`.
  """
  alias Beaver.MLIR.Dialect.Func
  import Beaver.MLIR.CAPI
  alias Beaver.MLIR
  alias __MODULE__.LockedCache

  defstruct ctx: nil, engine: nil, owner: true, diagnostic_server: nil, diagnostic_handler_id: nil

  defp jit_of_mod(m) do
    import Beaver.MLIR.{Conversion, Transforms}

    m
    |> MLIR.Operation.verify!(debug: true)
    |> MLIR.Pass.Composer.nested("func.func", "llvm-request-c-wrappers")
    |> MLIR.Pass.Composer.nested("func.func", loop_invariant_code_motion())
    |> convert_scf_to_cf
    |> convert_arith_to_llvm()
    |> convert_index_to_llvm()
    |> convert_func_to_llvm()
    |> MLIR.Pass.Composer.append("convert-vector-to-llvm{reassociate-fp-reductions}")
    |> MLIR.Pass.Composer.append("finalize-memref-to-llvm")
    |> reconcile_unrealized_casts
    |> Charms.Debug.print_ir_pass()
    |> MLIR.Pass.Composer.run!(print: Charms.Debug.step_print?())
    |> MLIR.ExecutionEngine.create!(opt_level: 3, object_dump: true)
    |> tap(&beaver_raw_jit_register_enif(&1.ref))
  end

  defp clone_ops(to, from) do
    ops = MLIR.Module.body(from) |> Beaver.Walker.operations()
    s_table = to |> MLIR.Operation.from_module() |> mlirSymbolTableCreate()

    for op <- ops, MLIR.Operation.name(op) in ~w{func.func memref.global} do
      sym = mlirOperationGetAttributeByName(op, mlirSymbolTableGetSymbolAttributeName())
      found = mlirSymbolTableLookup(s_table, MLIR.Attribute.unwrap(sym))
      body = MLIR.Module.body(to)

      if MLIR.is_null(found) do
        mlirBlockAppendOwnedOperation(body, mlirOperationClone(op))
      else
        unless Func.is_external(op) do
          mlirOperationDestroy(found)
          mlirBlockAppendOwnedOperation(body, mlirOperationClone(op))
        end
      end
    end

    mlirSymbolTableDestroy(s_table)
  end

  defp merge_modules(modules, opts \\ []) do
    destroy = opts[:destroy] || true
    [head | tail] = modules

    for module <- tail do
      if MLIR.is_null(module), do: raise("can't merge a null module")
      clone_ops(head, module)
      if destroy, do: MLIR.Module.destroy(module)
    end

    head
  end

  defp do_init(modules) when is_list(modules) do
    ctx = MLIR.Context.create()
    {:ok, diagnostic_server} = GenServer.start(Beaver.Diagnostic.Server, [])
    diagnostic_handler_id = Beaver.Diagnostic.attach(ctx, diagnostic_server)

    modules
    |> Enum.map(fn
      m when is_atom(m) ->
        m.__ir__() |> then(&MLIR.Module.create(ctx, &1))

      s when is_binary(s) ->
        s |> then(&MLIR.Module.create(ctx, &1))

      %MLIR.Module{} = m ->
        m

      other ->
        raise ArgumentError, "Unexpected module type: #{inspect(other)}"
    end)
    |> then(fn op ->
      try do
        op
        |> merge_modules()
        |> jit_of_mod
      rescue
        e ->
          case Charms.Diagnostic.compile_error_message(diagnostic_server) do
            {:ok, dm} ->
              reraise CompileError, dm, __STACKTRACE__

            {:error, _} ->
              reraise e, __STACKTRACE__
          end
      end
    end)
    |> then(
      &%__MODULE__{
        ctx: ctx,
        engine: &1,
        diagnostic_server: diagnostic_server,
        diagnostic_handler_id: diagnostic_handler_id
      }
    )
    |> then(&{:ok, &1})
  end

  defp collect_modules(module, acc \\ [])

  defp collect_modules(module, acc) when is_atom(module) do
    if module in acc do
      acc
    else
      acc = [module | acc]

      module.referenced_modules()
      |> Enum.reduce(acc, fn m, acc ->
        collect_modules(m, acc)
      end)
    end
  end

  defp collect_modules(module, acc), do: [module | acc]

  def init(module, opts \\ [])

  def init({:module, module, binary, _}, opts) when is_atom(module) and is_binary(binary) do
    init(module, opts)
  end

  def init(module, opts) do
    key = Keyword.fetch!(opts, :name)

    {modules, jit} =
      LockedCache.run(key, fn ->
        modules = collect_modules(module)
        {:ok, jit} = do_init(modules)
        {modules, jit}
      end)

    # modules will be nil if cache is hit
    if modules do
      # cache the jit engine for referenced modules
      for m when is_atom(m) <- modules, module != m do
        key = m.__ir_digest__()
        LockedCache.run(key, fn -> {:ok, %__MODULE__{jit | owner: false}} end)
      end

      {:ok, jit}
    else
      {:cached, jit}
    end
  end

  @doc """
  Returns the JIT engine for the given module.
  """
  def engine(key) do
    if jit = LockedCache.get(key), do: jit.engine
  end

  def invoke(%MLIR.ExecutionEngine{ref: ref}, {mod, func, args}) do
    beaver_raw_jit_invoke_with_terms(ref, to_string(Charms.Defm.mangling(mod, func)), args)
  end

  def destroy(key) do
    case LockedCache.get(key) do
      %__MODULE__{
        ctx: ctx,
        engine: engine,
        owner: true,
        diagnostic_server: diagnostic_server,
        diagnostic_handler_id: diagnostic_handler_id
      } ->
        Beaver.Diagnostic.detach(ctx, diagnostic_handler_id)
        MLIR.ExecutionEngine.destroy(engine)
        MLIR.Context.destroy(ctx)
        :ok = GenServer.stop(diagnostic_server)

      nil ->
        :not_found

      _ ->
        :noop
    end
  end
end
