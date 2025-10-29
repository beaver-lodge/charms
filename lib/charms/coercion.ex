defmodule Charms.Coercion do
  @moduledoc """
  Module providing coercion utilities for Charms.
  """
  alias Beaver.MLIR
  alias Charms.Pointer

  @doc """
  Cast an argument following Charms coercion rules.
  """
  def cast_argument(arg, target_type, ctx, blk, loc) do
    t = MLIR.Value.type(arg)

    if MLIR.Type.memref?(t) and MLIR.ShapedType.rank(t) in [0, 1] and
         MLIR.Dialect.MemRef.strides_and_offset(t) in [{[], 0}, {[1], 0}] and
         MLIR.equal?(target_type, Pointer.unified_ptr_type(MLIR.ShapedType.element_type(t), ctx)) do
      Pointer.unify_layout(arg, ctx, blk, loc)
    else
      arg
    end
  end
end
