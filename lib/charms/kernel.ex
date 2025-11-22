defmodule Charms.Kernel do
  @moduledoc """
  Intrinsic module to define functions in `Kernel`.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Arith, Index}
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

  defp i_create_binary(op, operands, type, ctx, blk, loc) do
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

  defp f_predicate(:!=), do: :une
  defp f_predicate(:==), do: :oeq
  defp f_predicate(:>), do: :ogt
  defp f_predicate(:>=), do: :oge
  defp f_predicate(:<), do: :olt
  defp f_predicate(:<=), do: :ole

  defp f_create_binary(op, operands, type, ctx, blk, loc) do
    mlir ctx: ctx, blk: blk do
      case op do
        op when op in @compare_ops ->
          Arith.cmpf(operands, predicate: Arith.cmp_f_predicate(f_predicate(op)), loc: loc) >>>
            Type.i1()

        :- ->
          Arith.subf(operands, loc: loc) >>> type

        :+ ->
          Arith.addf(operands, loc: loc) >>> type

        :* ->
          Arith.mulf(operands, loc: loc) >>> type

        :/ ->
          Arith.divf(operands, loc: loc) >>> type

        _ ->
          raise ArgumentError, "Unsupported operator: #{inspect(op)}"
      end
    end
  end

  defp create_binary(op, left, right, ctx, blk, loc) do
    {type, operands} = cast_and_validate(left, right, ctx, blk, loc)
    dispatch_binary_op(type, op, operands, ctx, blk, loc)
  end

  defp promote_integer_to_index(%MLIR.Value{} = val, ctx, blk, loc) do
    t = MLIR.Value.type(val)

    if MLIR.Type.index?(t) do
      val
    else
      mlir ctx: ctx, blk: blk do
        Index.casts(val, loc: loc) >>> Type.index()
      end
    end
  end

  defp cast_and_validate(left, right, ctx, blk, loc) do
    operands =
      case {left, right} do
        {%MLIR.Value{} = v, i} when is_integer(i) or is_float(i) ->
          [v, Charms.Constant.from_literal(i, v, ctx, blk, loc)]

        {i, %MLIR.Value{} = v} when is_integer(i) or is_float(i) ->
          [Charms.Constant.from_literal(i, v, ctx, blk, loc), v]

        {%MLIR.Value{}, %MLIR.Value{}} ->
          if MLIR.equal?(MLIR.Value.type(left), MLIR.Value.type(right)) do
            [left, right]
          else
            if MLIR.Type.index?(MLIR.Value.type(left)) or MLIR.Type.index?(MLIR.Value.type(right)) do
              [
                promote_integer_to_index(left, ctx, blk, loc),
                promote_integer_to_index(right, ctx, blk, loc)
              ]
            else
              raise "operands of binary op must be same type, got #{MLIR.to_string(MLIR.Value.type(left))} and #{MLIR.to_string(MLIR.Value.type(right))}"
            end
          end

        _ ->
          raise ArgumentError,
                "Invalid arguments for binary operator: #{inspect(left)}, #{inspect(right)}"
      end

    {MLIR.Value.type(hd(operands)), operands}
  end

  defp dispatch_binary_op(type, op, operands, ctx, blk, loc) do
    cond do
      MLIR.Type.integer?(type) or MLIR.Type.index?(type) ->
        i_create_binary(op, operands, type, ctx, blk, loc)

      MLIR.Type.float?(type) ->
        f_create_binary(op, operands, type, ctx, blk, loc)

      true ->
        raise ArgumentError,
              "Unsupported type for binary operator: #{to_string(type)}. Only integer and float types are supported for now."
    end
  end

  for name <- @binary_ops ++ @binary_macro_ops do
    defintr unquote(name)(left, right) do
      %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

      if unquote(name) in [:+, :-] and Charms.Pointer.memref_ptr?(left) do
        case unquote(name) do
          :+ ->
            {quote do
               Charms.Pointer.element_ptr(ptr, offset)
             end, ptr: left, offset: right}

          :- ->
            {quote do
               Charms.Pointer.element_ptr(ptr, 0 - offset)
             end, ptr: left, offset: right}
        end
      else
        create_binary(unquote(name), left, right, ctx, blk, loc)
      end
    end
  end

  defintr !value do
    t = MLIR.Value.type(value)

    unless MLIR.Type.integer?(t) do
      raise ArgumentError, "Not an integer type to negate, unsupported type: #{to_string(t)}"
    end

    {quote do
       one = const 1 :: t
       value arith.xori(value, one) :: t
     end, t: t, value: value}
  end

  @doc false
  def intrinsics() do
    [:rem | @binary_ops]
  end

  @doc false
  def macro_intrinsics() do
    @binary_macro_ops ++ @unary_ops
  end

  defintr rem(dividend, divisor) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    {type, operands} = cast_and_validate(dividend, divisor, ctx, blk, loc)

    mlir ctx: ctx, blk: blk do
      Arith.remsi(operands, loc: loc) >>> type
    end
  end
end
