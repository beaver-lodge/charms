defmodule Charms.Prelude do
  @moduledoc """
  Intrinsic module to define essential functions provided by Charms.
  """
  use Charms.Intrinsic
  alias Beaver.MLIR.Dialect.{Arith, Func}
  @enif_functions Beaver.ENIF.functions()
  @binary_ops [:!=, :-, :+, :<, :>, :<=, :>=, :==, :&&, :||, :*]

  defp constant_of_same_type(i, v, opts) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      t = MLIR.CAPI.mlirValueGetType(v)

      if MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
        Arith.constant(value: Attribute.integer(t, i)) >>> t
      else
        raise ArgumentError, "Not an integer type for constant, #{to_string(t)}"
      end
    end
  end

  defp wrap_arg({i, t}, opts) when is_integer(i) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      case i do
        %MLIR.Value{} ->
          i

        i when is_integer(i) ->
          if MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
            Arith.constant(value: Attribute.integer(t, i)) >>> t
          else
            raise ArgumentError, "Not an integer type, #{to_string(t)}"
          end
      end
    end
  end

  defp wrap_arg({v, _}, _) do
    v
  end

  @compare_ops [:!=, :==, :>, :>=, :<, :<=]
  defp i_predicate(:!=), do: :ne
  defp i_predicate(:==), do: :eq
  defp i_predicate(:>), do: :sgt
  defp i_predicate(:>=), do: :sge
  defp i_predicate(:<), do: :slt
  defp i_predicate(:<=), do: :sle

  defp create_binary(op, operands, type, ctx, block) do
    mlir ctx: ctx, block: block do
      case op do
        op when op in @compare_ops ->
          Arith.cmpi(operands, predicate: Arith.cmp_i_predicate(i_predicate(op))) >>> Type.i1()

        :- ->
          Arith.subi(operands) >>> type

        :+ ->
          Arith.addi(operands) >>> type

        :&& ->
          Arith.andi(operands) >>> type

        :|| ->
          Arith.ori(operands) >>> type

        :* ->
          Arith.muli(operands) >>> type
      end
    end
  end

  def handle_intrinsic(:result_at, _params, [l, i], _opts) when is_list(l) do
    l |> Enum.at(i)
  end

  def handle_intrinsic(:result_at, _params, [%MLIR.Operation{} = op, i], _opts) do
    MLIR.CAPI.mlirOperationGetResult(op, i)
  end

  def handle_intrinsic(op, _params, [left, right], opts) when op in @binary_ops do
    {operands, type} =
      case {left, right} do
        {%MLIR.Value{} = v, i} when is_integer(i) ->
          [v, constant_of_same_type(i, v, opts)]

        {i, %MLIR.Value{} = v} when is_integer(i) ->
          [constant_of_same_type(i, v, opts), v]

        {%MLIR.Value{}, %MLIR.Value{}} ->
          [left, right]
      end
      |> then(fn [left, _] = operands -> {operands, MLIR.CAPI.mlirValueGetType(left)} end)

    create_binary(op, operands, type, opts[:ctx], opts[:block])
  end

  def handle_intrinsic(name, _params, args, opts) when name in @enif_functions do
    {arg_types, ret_types} = Beaver.ENIF.signature(opts[:ctx], name)
    args = args |> Enum.zip(arg_types) |> Enum.map(&wrap_arg(&1, opts))

    mlir ctx: opts[:ctx], block: opts[:block] do
      Func.call(args, callee: Attribute.flat_symbol_ref("#{name}"), loc: opts[:loc]) >>>
        case ret_types do
          [ret] ->
            ret

          [] ->
            []
        end
    end
  end

  def handle_intrinsic(_name, _params, _args, _opts) do
    :not_handled
  end

  defintrinsic @enif_functions ++ [:result_at] ++ @binary_ops
end
