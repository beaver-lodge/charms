defmodule Charms.Kernel do
  @moduledoc """
  Intrinsic module to define functions in `Kernel`.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.Arith
  @binary_ops [:!=, :-, :+, :<, :>, :<=, :>=, :==, :*]
  @unary_ops [:!]
  @binary_macro_ops [:&&, :||]

  defp constant_of_same_type(i, v, %Opts{ctx: ctx, block: block, loc: loc}) do
    mlir ctx: ctx, block: block do
      t = MLIR.CAPI.mlirValueGetType(v)

      if MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
        Arith.constant(value: Attribute.integer(t, i), loc: loc) >>> t
      else
        raise ArgumentError, "Not an integer type for constant, #{to_string(t)}"
      end
    end
  end

  @compare_ops [:!=, :==, :>, :>=, :<, :<=]
  defp i_predicate(:!=), do: :ne
  defp i_predicate(:==), do: :eq
  defp i_predicate(:>), do: :sgt
  defp i_predicate(:>=), do: :sge
  defp i_predicate(:<), do: :slt
  defp i_predicate(:<=), do: :sle

  defp create_binary(op, operands, type, ctx, block, loc) do
    mlir ctx: ctx, block: block do
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

        _ ->
          raise ArgumentError, "Unsupported operator: #{inspect(op)}"
      end
    end
  end

  for name <- @binary_ops ++ @binary_macro_ops do
    defintrinsic unquote(name)(left, right) do
      opts = %Opts{ctx: ctx, block: block, loc: loc} = __IR__

      {operands, type} =
        case {left, right} do
          {%MLIR.Value{} = v, i} when is_integer(i) ->
            [v, constant_of_same_type(i, v, opts)]

          {i, %MLIR.Value{} = v} when is_integer(i) ->
            [constant_of_same_type(i, v, opts), v]

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

      create_binary(unquote(name), operands, type, ctx, block, loc)
    end
  end

  defintrinsic !value do
    t = MLIR.Value.type(value)

    unless MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
      raise ArgumentError, "Not an integer type to negate, unsupported type: #{to_string(t)}"
    end

    quote bind_quoted: [v: value, t: t] do
      one = const 1 :: t
      value arith.xori(v, one) :: t
    end
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
