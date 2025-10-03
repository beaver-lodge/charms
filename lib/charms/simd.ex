defmodule Charms.SIMD do
  @moduledoc """
  Intrinsic module for SIMD types.
  """
  use Charms.Intrinsic
  alias MLIR.Dialect.{Arith, Vector, Index}
  alias MLIR.Type
  alias Charms.Intrinsic.Opts

  @doc """
  Return the constant value of the given `type` and `literal_values`
  """
  defintr new(type, literal_values) do
    %Opts{ctx: ctx, blk: blk} = __IR__

    mlir ctx: ctx, blk: blk do
      element_type = MLIR.ShapedType.element_type(type)

      if MLIR.null?(element_type) do
        raise "element type is null"
      end

      width = MLIR.ShapedType.dim_size(type, 0)

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
  defintr t(type, width) do
    Type.vector!([width], type)
  end

  defintr insert(dest, position, value) when is_integer(position) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      static_position = MLIR.Attribute.dense_array([position], Beaver.Native.I64)
      Vector.insert(value, dest, loc: loc, static_position: static_position) >>> :infer
    end
  end

  defintr insert(dest, %MLIR.Value{} = position, value) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      static_position =
        MLIR.Attribute.dense_array(
          [MLIR.ShapedType.dynamic_stride_or_offset()],
          Beaver.Native.I64
        )

      position = Index.casts(position) >>> Type.index()
      Vector.insert(value, dest, position, loc: loc, static_position: static_position) >>> :infer
    end
  end

  defintr extract(source, position) when is_integer(position) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      static_position = MLIR.Attribute.dense_array([position], Beaver.Native.I64)

      Vector.extract(source, loc: loc, static_position: static_position) >>>
        MLIR.Type.element_type(MLIR.Value.type(source))
    end
  end
end
