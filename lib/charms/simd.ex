defmodule Charms.SIMD do
  @moduledoc """
  Intrinsic module for SIMD types.
  """
  use Charms.Intrinsic
  alias MLIR.Dialect.Arith
  alias MLIR.Type
  alias Charms.Intrinsic.Opts

  @doc """
  Return the constant value of the given `type` and `literal_values`
  """
  defintrinsic new(type, literal_values) do
    %Opts{ctx: ctx, blk: blk} = __IR__

    mlir ctx: ctx, blk: blk do
      element_type = MLIR.CAPI.mlirShapedTypeGetElementType(type)

      if MLIR.null?(element_type) do
        raise "element type is null"
      end

      width = MLIR.CAPI.mlirShapedTypeGetDimSize(type, 0) |> Beaver.Native.to_term()

      if Enum.count(literal_values) != width do
        raise ArgumentError, "expected #{width} values, got #{length(literal_values)}"
      end

      if width <= 0 do
        raise ArgumentError, "width must be a positive integer"
      end

      values = Enum.map(literal_values, &Attribute.integer(element_type, &1))
      value = Attribute.dense_elements(values, type, ctx: ctx)
      Arith.constant(value: value) >>> type
    end
  end

  @doc """
  Return the vector type of the given `type` and `width`
  """
  defintrinsic t(type, width) do
    Type.vector([width], type)
  end
end
