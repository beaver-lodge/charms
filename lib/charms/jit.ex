defmodule Charms.JIT do
  @moduledoc """
  Compile and execute MLIR modules generated from `Charms.Defm`.
  """
  alias Beaver.MLIR.Dialect.Func
  import Beaver.MLIR.CAPI
  alias Beaver.MLIR
  alias __MODULE__.LockedCache
  defstruct engine: nil, owner: true

  defp jit_of_mod(m, dynamic_libraries) do
    import Beaver.MLIR.{Conversion, Transform}

    m
    |> MLIR.verify!()
    |> MLIR.Transform.canonicalize()
    |> Beaver.Composer.append("ownership-based-buffer-deallocation")
    |> Beaver.Composer.append("buffer-deallocation-simplification")
    |> Beaver.Composer.append("bufferization-lower-deallocations")
    |> MLIR.Transform.canonicalize()
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.nested("func.func", "llvm-request-c-wrappers")
    |> Beaver.Composer.nested("func.func", loop_invariant_code_motion())
    |> convert_scf_to_cf
    |> convert_cf_to_llvm()
    |> convert_arith_to_llvm()
    |> convert_index_to_llvm()
    |> convert_func_to_llvm()
    |> Beaver.Composer.append("finalize-memref-to-llvm")
    |> Beaver.Composer.append("convert-vector-to-llvm{reassociate-fp-reductions}")
    |> reconcile_unrealized_casts
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.run!(print: Charms.Debug.step_print?())
    |> MLIR.ExecutionEngine.create!(
      opt_level: 3,
      object_dump: true,
      dirty: :cpu_bound,
      shared_lib_paths: dynamic_libraries
    )
    |> tap(&beaver_raw_jit_register_enif(&1.ref))
  end

  defp clone_ops(to, from) do
    ops = MLIR.Module.body(from) |> Beaver.Walker.operations()
    s_table = to |> MLIR.Operation.from_module() |> mlirSymbolTableCreate()

    for op <- ops, MLIR.Operation.name(op) in ~w{func.func memref.global} do
      sym = mlirOperationGetAttributeByName(op, mlirSymbolTableGetSymbolAttributeName())
      found = mlirSymbolTableLookup(s_table, MLIR.Attribute.unwrap(sym))
      body = MLIR.Module.body(to)

      if MLIR.null?(found) do
        mlirBlockAppendOwnedOperation(body, mlirOperationClone(op))
      else
        unless Func.external?(op) do
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
      if MLIR.null?(module), do: raise("can't merge a null module")
      clone_ops(head, module)
      if destroy, do: MLIR.Module.destroy(module)
    end

    head
  end

  defp do_init(ctx, modules) when is_list(modules) do
    dynamic_libraries = Enum.flat_map(modules, &collect_dynamic_libraries/1) |> Enum.uniq()

    modules
    |> Enum.map(fn
      m when is_atom(m) ->
        m.__ir__() |> then(&MLIR.Module.create!(&1, ctx: ctx))

      s when is_binary(s) ->
        s |> then(&MLIR.Module.create!(&1, ctx: ctx))

      %MLIR.Module{} = m ->
        m

      other ->
        raise ArgumentError, "Unexpected module type: #{inspect(other)}"
    end)
    |> then(fn op ->
      op |> merge_modules() |> jit_of_mod(dynamic_libraries)
    end)
    |> then(
      &%__MODULE__{
        engine: &1
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

  defp collect_dynamic_libraries(module) when is_atom(module) do
    module.dynamic_libraries()
  end

  defp collect_dynamic_libraries(_), do: []

  def init(module, opts \\ [])

  def init({:module, module, binary, _}, opts) when is_atom(module) and is_binary(binary) do
    init(module, opts)
  end

  def init(module, opts) do
    key = Keyword.fetch!(opts, :name)

    {modules, jit} =
      LockedCache.run(key, fn ->
        modules = collect_modules(module)

        {:ok, jit} =
          NimblePool.checkout!(Charms.ContextPool, :checkout, fn _, ctx ->
            {do_init(ctx, modules), ctx}
          end)

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

  def invoke(%MLIR.ExecutionEngine{} = engine, {mod, func, args}) do
    Beaver.ENIF.invoke(engine, to_string(Charms.Defm.mangling(mod, func)), args)
  end

  def destroy(key) do
    case LockedCache.get(key) do
      %__MODULE__{
        engine: engine,
        owner: true
      } ->
        MLIR.ExecutionEngine.destroy(engine)

      nil ->
        :not_found

      _ ->
        :noop
    end
  end
end
