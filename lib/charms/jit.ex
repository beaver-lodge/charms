defmodule Charms.JIT do
  @moduledoc """
  Compile and execute MLIR modules generated from `Charms.Defm`.
  """
  alias Beaver.MLIR.Dialect.Func
  alias Beaver.MLIR
  alias __MODULE__.LockedCache
  defstruct engine: nil, owner: true

  @cuda_libs ~w{libmlir_cuda_runtime.so}
  @runtime_libs ~w{libmlir_runner_utils.so libmlir_c_runner_utils.so}
  def cuda_available? do
    System.find_executable("nvidia-smi") != nil
  end

  defp jit_of_mod(m, dynamic_libraries) do
    import Beaver.MLIR.{Conversion, Transform}
    if :persistent_term.get({__MODULE__, :sigchld_trapped?}, false) == false do
      System.trap_signal(:sigchld, fn -> :ok end)
      :persistent_term.put({__MODULE__, :sigchld_trapped?}, true)
    end
    Charms.Transform.put_gpu_transforms(m)
    MLIR.Context.register_translations(MLIR.context(m))
    dynamic_libraries =
      if(cuda_available?(), do: @cuda_libs, else: [])
      |> Enum.concat(@runtime_libs)
      |> Enum.map(&Path.join([:code.priv_dir(:beaver), "lib", &1]))
      |> Enum.filter(&File.exists?/1)
      |> Enum.concat(dynamic_libraries)

    m
    |> MLIR.verify!()
    |> MLIR.Transform.canonicalize()
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.nested("func.func", "llvm-request-c-wrappers")
    |> Beaver.Composer.nested("func.func", loop_invariant_code_motion())
    |> Beaver.Composer.append("transform-interpreter")
    |> Beaver.Composer.append("gpu-lower-to-nvvm-pipeline{cubin-format=fatbin}")
    |> convert_scf_to_cf()
    |> convert_cf_to_llvm()
    |> convert_arith_to_llvm()
    |> convert_index_to_llvm()
    |> convert_func_to_llvm()
    |> Beaver.Composer.append("gpu-to-llvm")
    |> Beaver.Composer.append("finalize-memref-to-llvm")
    |> Beaver.Composer.append("convert-vector-to-llvm{reassociate-fp-reductions}")
    |> Beaver.Composer.append(Charms.Defm.Pass.UseENIFAlloc)
    |> reconcile_unrealized_casts
    |> Charms.Debug.print_ir_pass()
    |> Beaver.Composer.run!(print: Charms.Debug.step_print?())
    |> MLIR.ExecutionEngine.create!(
      opt_level: 3,
      object_dump: true,
      shared_lib_paths: dynamic_libraries
    )
    |> tap(&Beaver.ENIF.register_symbols/1)
    |> MLIR.ExecutionEngine.init()
  end

  defp clone_ops(to, from) do
    ops = MLIR.Module.body(from) |> Beaver.Walker.operations()
    s_table = to |> MLIR.Operation.from_module() |> MLIR.CAPI.mlirSymbolTableCreate()

    for op <- ops, MLIR.Operation.name(op) in ~w{func.func memref.global} do
      sym = op[MLIR.CAPI.mlirSymbolTableGetSymbolAttributeName()]
      found = MLIR.CAPI.mlirSymbolTableLookup(s_table, MLIR.Attribute.unwrap(sym))
      body = MLIR.Module.body(to)

      if MLIR.null?(found) do
        MLIR.Block.append(body, MLIR.Operation.clone(op))
      else
        unless Func.external?(op) do
          MLIR.Operation.destroy(found)
          MLIR.Block.append(body, MLIR.Operation.clone(op))
        end
      end
    end

    MLIR.CAPI.mlirSymbolTableDestroy(s_table)
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
    if function_exported?(module, :dynamic_libraries, 0) do
      module.dynamic_libraries()
    else
      []
    end
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
