defmodule Charms.Defm.Expander.MLIRImporter do
  use Beaver
  alias MLIR.Dialect.{UB}
  @moduledoc false
  defstruct ctx: nil,
            mod: nil,
            blk: nil,
            available_ops: MapSet.new(),
            vars: Map.new(),
            region: nil,
            enif_env: nil,
            dependence_modules: Map.new(),
            required_intrinsic_modules: MapSet.new(),
            deferrals: [],
            infer_argument_type: nil,
            infer_return_type: nil,
            expand_arg_fallback: nil,
            allow_undefined_function: false

  def put_var(mlir, name, val) when is_atom(name) do
    %{mlir | vars: Map.put(mlir.vars, name, val)}
  end

  def put_var(mlir, {name, _meta, _ctx}, val) do
    put_var(mlir, name, val)
  end

  def get_var(mlir, {name, _meta, ctx}) when is_atom(name) and is_atom(ctx) do
    var = Map.get(mlir.vars, name)

    if var != nil do
      {:ok, var}
    else
      {:undefined_variable, name}
    end
  end

  def get_var(_mlir, ast) do
    {:skip, ast}
  end

  # expand a term to ENIF C representation if no parenting intrinsics expand it.
  defp expand_arg_fallback(i, state, env) do
    mlir ctx: state.mlir.ctx, blk: state.mlir.blk do
      v =
        UB.poison(
          msg: MLIR.Attribute.string("can't compile #{inspect(i)}"),
          loc: MLIR.Location.from_env(env)
        ) >>>
          Beaver.ENIF.Type.term()

      {v, state, env}
    end
  end

  # create a new MLIR importer which stick to Elixir semantics
  # useful for expanding Elixir code into MLIR in best effort manner
  # suitable for using in unit-testing and POC
  def new(:best_effort, ctx) do
    available_ops = MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx))

    %__MODULE__{
      ctx: ctx,
      blk: MLIR.Block.create(),
      available_ops: available_ops,
      vars: Map.new(),
      region: nil,
      enif_env: nil,
      infer_argument_type: fn _fa, _index, state ->
        Beaver.ENIF.Type.term(ctx: state.mlir.ctx)
      end,
      infer_return_type: fn _fa, state -> [Beaver.ENIF.Type.term(ctx: state.mlir.ctx)] end,
      expand_arg_fallback: &expand_arg_fallback/3,
      allow_undefined_function: true
    }
  end

  # create a new MLIR importer which strictly follows Charms Defm semantics
  # suitable for production use and compile for computation
  def new(:strict_typing, ctx) do
    %Charms.Defm.Expander.MLIRImporter{
      ctx: ctx,
      available_ops: MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx)),
      vars: Map.new(),
      region: nil,
      enif_env: nil,
      expand_arg_fallback: fn
        ast, state, env ->
          {ast, state, env}
      end
    }
  end
end
