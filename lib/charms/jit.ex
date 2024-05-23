defmodule Charms.JIT do
  alias Beaver.MLIR.Dialect.Func
  import Beaver.MLIR.CAPI
  alias Beaver.MLIR

  defp jit_of_mod(m) do
    import Beaver.MLIR.Conversion

    m
    |> MLIR.Operation.verify!(debug: true)
    |> MLIR.Pass.Composer.nested("func.func", "llvm-request-c-wrappers")
    |> convert_scf_to_cf
    |> convert_arith_to_llvm()
    |> convert_index_to_llvm()
    |> convert_func_to_llvm()
    |> MLIR.Pass.Composer.append("finalize-memref-to-llvm")
    |> reconcile_unrealized_casts
    |> MLIR.Pass.Composer.run!(print: System.get_env("DEFM_PRINT_IR") == "1")
    |> MLIR.ExecutionEngine.create!(opt_level: 3, object_dump: true)
    |> tap(&beaver_raw_jit_register_enif(&1.ref))
  end

  defp clone_func_impl(to, from) do
    ops = MLIR.Module.body(from) |> Beaver.Walker.operations() |> Enum.to_list()
    s_table = to |> MLIR.Operation.from_module() |> mlirSymbolTableCreate()

    for op <- ops, MLIR.Operation.name(op) == "func.func" do
      sym = mlirOperationGetAttributeByName(op, mlirSymbolTableGetSymbolAttributeName())
      found = mlirSymbolTableLookup(s_table, mlirStringAttrGetValue(sym))
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
      if MLIR.is_null(module) do
        raise "can't merge a null module"
      end

      clone_func_impl(head, module)

      if destroy do
        MLIR.Module.destroy(module)
      end
    end

    head
  end

  def init(module, opts \\ [])

  def init(module, opts) when is_atom(module) do
    name = opts[:name] || module
    opts = Keyword.put_new(opts, :name, name)
    init([module], opts)
  end

  def init(modules, opts) do
    ctx = MLIR.Context.create()
    Beaver.Diagnostic.attach(ctx)
    name = opts[:name]
    modules = modules |> List.wrap()

    jit =
      modules
      |> Enum.map(fn
        m when is_atom(m) -> m.__ir__() |> then(&MLIR.Module.create(ctx, &1))
        s when is_binary(s) -> s |> then(&MLIR.Module.create(ctx, &1))
        %MLIR.Module{} = m -> m
      end)
      |> merge_modules()
      |> jit_of_mod

    case {name, modules} do
      {name, [_]} when not is_nil(name) ->
        Agent.start_link(fn -> %{ctx: ctx, jit: jit} end, name: name)

      {nil, modules} ->
        for {module, index} <- Enum.with_index(modules) do
          Agent.start_link(fn -> %{ctx: ctx, jit: jit, owner: index == 0} end, name: module)
        end
    end
  end

  def get(module) do
    if Process.whereis(module) do
      %{jit: jit} = Agent.get(module, & &1)
      jit
    end
  end

  def invoke(jit, {mod, func, args}) do
    beaver_raw_jit_invoke_with_terms(
      jit.ref,
      to_string(Charms.Defm.mangling(mod, func)),
      args
    )
  end

  def invoke(jit, f, args) when is_function(f, length(args)) do
    apply(f, args).(jit)
  end

  def destroy(module) do
    with %{ctx: ctx, jit: jit, owner: true} <- Agent.get(module, & &1) do
      MLIR.ExecutionEngine.destroy(jit)
      MLIR.Context.destroy(ctx)
    end

    Agent.stop(module)
  end
end
