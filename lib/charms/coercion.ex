defmodule Charms.Coercion do
  @moduledoc """
  Module providing coercion utilities for Charms.
  """
  use Beaver
  alias Charms.Pointer
  alias MLIR.Dialect.Index

  @doc """
  Cast an argument following Charms coercion rules.
  """
  def cast_argument(arg, target_type, ctx, blk, loc) do
    t = MLIR.Value.type(arg)

    cond do
      MLIR.Type.memref?(t) and MLIR.ShapedType.rank(t) in [0, 1] and
        MLIR.Dialect.MemRef.strides_and_offset(t) in [{[], 0}, {[1], 0}] and
          MLIR.equal?(target_type, Pointer.unified_ptr_type(MLIR.ShapedType.element_type(t), ctx)) ->
        Pointer.unify_layout(arg, ctx, blk, loc)

      MLIR.Type.integer?(t) and MLIR.Type.index?(target_type) ->
        mlir ctx: ctx, blk: blk do
          Index.casts(arg, loc: loc) >>> Type.index()
        end

      true ->
        arg
    end
  end
end
