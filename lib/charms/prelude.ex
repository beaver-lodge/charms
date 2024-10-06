defmodule Charms.Prelude do
  @moduledoc """
  Intrinsic module to define essential functions provided by Charms.
  """
  use Charms.Intrinsic
  alias Beaver.MLIR.Dialect.{Arith, Func}
  @enif_functions Beaver.ENIF.functions()
  @binary_ops [:!=, :-, :+, :<, :>, :<=, :>=, :==, :&&, :*]

  def intrinsics() do
    @enif_functions ++ [:result_at] ++ @binary_ops
  end

  defp constant_of_same_type(i, v, opts) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      t = MLIR.CAPI.mlirValueGetType(v)
      Arith.constant(value: Attribute.integer(t, i)) >>> t
    end
  end

  defp wrap_arg({i, t}, opts) when is_integer(i) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      case i do
        %MLIR.Value{} ->
          i

        i when is_integer(i) ->
          Arith.constant(value: Attribute.integer(t, i)) >>> t
      end
    end
  end

  defp wrap_arg({v, _}, _) do
    v
  end

  def handle_intrinsic(:result_at, [l, i], _opts) when is_list(l) do
    l |> Enum.at(i)
  end

  def handle_intrinsic(:result_at, [%MLIR.Operation{} = op, i], _opts) do
    MLIR.CAPI.mlirOperationGetResult(op, i)
  end

  def handle_intrinsic(op, [left, right], opts) when op in @binary_ops do
    mlir ctx: opts[:ctx], block: opts[:block] do
      operands =
        [left, _] =
        case {left, right} do
          {%MLIR.Value{} = v, i} when is_integer(i) ->
            [v, constant_of_same_type(i, v, opts)]

          {i, %MLIR.Value{} = v} when is_integer(i) ->
            [constant_of_same_type(i, v, opts), v]

          {%MLIR.Value{}, %MLIR.Value{}} ->
            [left, right]
        end

      case op do
        :!= ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:ne)) >>> Type.i1()

        :== ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:eq)) >>> Type.i1()

        :> ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:sgt)) >>> Type.i1()

        :>= ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:sge)) >>> Type.i1()

        :< ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:slt)) >>> Type.i1()

        :<= ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(:sle)) >>> Type.i1()

        :- ->
          Arith.subi(operands) >>> MLIR.CAPI.mlirValueGetType(left)

        :+ ->
          Arith.addi(operands) >>> MLIR.CAPI.mlirValueGetType(left)

        :&& ->
          Arith.andi(operands) >>> MLIR.CAPI.mlirValueGetType(left)

        :* ->
          Arith.muli(operands) >>> MLIR.CAPI.mlirValueGetType(left)
      end
    end
  end

  def handle_intrinsic(name, args, opts) when name in @enif_functions do
    {arg_types, ret_types} = Beaver.ENIF.signature(opts[:ctx], name)
    args = args |> Enum.zip(arg_types) |> Enum.map(&wrap_arg(&1, opts))

    mlir ctx: opts[:ctx], block: opts[:block] do
      Func.call(args, callee: Attribute.flat_symbol_ref("#{name}")) >>>
        case ret_types do
          [ret] ->
            ret

          [] ->
            []
        end
    end
  end

  def handle_intrinsic(_name, _args, _opts) do
    :not_handled
  end

  defintrinsic [:result_at, :!=, :-, :+, :<, :>, :<=, :>=, :==, :&&, :*]
end
