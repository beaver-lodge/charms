defmodule Charms.Defm.Expander.Dialect do
  @moduledoc """
  Expand dialect operations in Charms defm.
  """
  use Beaver
  alias Charms.Defm.Expander
  import Charms.Diagnostic, only: :macros

  @doc """
  Expand a dialect operation call like `arith.addi(...)`.
  """
  def expand(dialect, op, args, result_types, state, env) do
    op_name = "#{dialect}.#{op}"

    MapSet.member?(state.mlir.available_ops, op_name) or
      raise_compile_error(
        env,
        "Unknown MLIR operation to create: #{op_name}, did you mean: #{did_you_mean_op(op_name)}"
      )

    {args, state, env} = Expander.expand(args, state, env)
    {args, state, env} = Charms.Defm.Expander.Call.validate_call_args(args, state, env)

    try do
      state = Expander.expand_deferrals_before_terminator(op_name, state)

      result_types =
        if result_types == [] do
          if(MLIR.Context.infer_type?(state.mlir.ctx, op_name), do: [:infer], else: [])
        else
          result_types
        end

      %Beaver.SSA{
        op: op_name,
        arguments: args,
        ctx: state.mlir.ctx,
        blk: state.mlir.blk,
        loc: MLIR.Location.from_env(env),
        results: result_types
      }
      |> MLIR.Operation.create()
      |> then(&{&1, state, env})
    rescue
      e ->
        raise_compile_error(env, "Failed to create #{op_name}: #{Exception.message(e)}")
    end
  end

  defp did_you_mean_op(op) do
    MLIR.Dialect.Registry.ops(:all)
    |> Stream.map(&{&1, String.jaro_distance(&1, op)})
    |> Enum.sort(&(elem(&1, 1) >= elem(&2, 1)))
    |> List.first()
    |> elem(0)
  end
end
