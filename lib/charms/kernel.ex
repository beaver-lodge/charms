defmodule Charms.Kernel do
  @moduledoc """
  Intrinsic module to define functions in `Kernel`.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.Arith
  @binary_ops [:!=, :-, :+, :<, :>, :<=, :>=, :==, :*, :/]
  @unary_ops [:!]
  @binary_macro_ops [:&&, :||]

  @compare_ops [:!=, :==, :>, :>=, :<, :<=]
  defp i_predicate(:!=), do: :ne
  defp i_predicate(:==), do: :eq
  defp i_predicate(:>), do: :sgt
  defp i_predicate(:>=), do: :sge
  defp i_predicate(:<), do: :slt
  defp i_predicate(:<=), do: :sle

  defp create_binary(op, operands, type, ctx, blk, loc) do
    mlir ctx: ctx, blk: blk do
      case op do
        op when op in @compare_ops ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(i_predicate(op)), loc: loc) >>>
            Type.i1()

        :- ->
          Arith.subi(operands, loc: loc) >>> type

        :+ ->
          Arith.addi(operands, loc: loc) >>> type

        :&& ->
          Arith.andi(operands, loc: loc) >>> type

        :|| ->
          Arith.ori(operands, loc: loc) >>> type

        :* ->
          Arith.muli(operands, loc: loc) >>> type

        :/ ->
          Arith.divsi(operands, loc: loc) >>> type

        _ ->
          raise ArgumentError, "Unsupported operator: #{inspect(op)}"
      end
    end
  end

  for name <- @binary_ops ++ @binary_macro_ops do
    defintrinsic unquote(name)(left, right) do
      %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

      {operands, type} =
        case {left, right} do
          {%MLIR.Value{} = v, i} when is_integer(i) ->
            [v, Charms.Constant.from_literal(i, v, ctx, blk, loc)]

          {i, %MLIR.Value{} = v} when is_integer(i) ->
            [Charms.Constant.from_literal(i, v, ctx, blk, loc), v]

          {%MLIR.Value{}, %MLIR.Value{}} ->
            if not MLIR.equal?(MLIR.Value.type(left), MLIR.Value.type(right)) do
              raise "args of binary op must be same type"
            end

            [left, right]

          _ ->
            raise ArgumentError,
                  "Invalid arguments for binary operator: #{inspect(left)}, #{inspect(right)}"
        end
        |> then(fn [left, _] = operands -> {operands, MLIR.CAPI.mlirValueGetType(left)} end)

      create_binary(unquote(name), operands, type, ctx, blk, loc)
    end
  end

  defintrinsic !value do
    t = MLIR.Value.type(value)

    unless MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
      raise ArgumentError, "Not an integer type to negate, unsupported type: #{to_string(t)}"
    end

    {quote do
       one = const 1 :: t
       value arith.xori(value, one) :: t
     end, t: t, value: value}
  end

  @doc false
  def intrinsics() do
    @binary_ops
  end

  @doc false
  def macro_intrinsics() do
    @binary_macro_ops ++ @unary_ops
  end
end
