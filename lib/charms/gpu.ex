defmodule Charms.GPU do
  @moduledoc """
  intrinsics of MLIR GPU dialect
  """
  use Charms.Intrinsic
  alias Charms.Pointer
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Index, GPU}
  alias MLIR.Type

  defintr program_id(dimension \\ :x) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.block_id(dimension: ~a{#gpu<dim #{dimension}>}, loc: loc) >>> Type.index()
    end
  end

  defintr allocate(elem_type, size) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()

      case size do
        i when is_integer(i) ->
          GPU.alloc(
            loc: loc,
            operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([0, 0])
          ) >>> Type.memref!([i], elem_type)

        %MLIR.Value{} ->
          size =
            if Type.index?(MLIR.Value.type(size)) do
              size
            else
              Index.casts(size, loc: loc) >>> Type.index()
            end

          GPU.alloc(size,
            loc: loc,
            operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([1, 0])
          ) >>> Type.memref!([:dynamic], elem_type)
      end
      |> Pointer.offset_ptr(elem_type, zero, ctx, blk, loc)
    end
  end
end
