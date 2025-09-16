defmodule Charms.GPU do
  @moduledoc """
  intrinsics of MLIR GPU dialect
  """
  use Charms.Intrinsic
  alias Charms.Pointer
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Index, GPU, Arith}
  alias MLIR.Type

  defintr block_id(dimension \\ :x) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.block_id(dimension: ~a{#gpu<dim #{dimension}>}, loc: loc) >>> Type.index()
    end
  end

  defintr thread_id(dimension \\ :x) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.thread_id(dimension: ~a{#gpu<dim #{dimension}>}, loc: loc) >>> Type.index()
    end
  end

  defp to_index(size, %Opts{ctx: ctx, blk: blk, loc: loc}) when is_integer(size) do
    mlir ctx: ctx, blk: blk do
      Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), size), loc: loc) >>>
        MLIR.Type.index(ctx: ctx)
    end
  end

  defp to_index(%MLIR.Value{} = size, %Opts{ctx: ctx, blk: blk, loc: loc}) do
    mlir ctx: ctx, blk: blk do
      if Type.index?(MLIR.Value.type(size)) do
        size
      else
        Index.casts(size, loc: loc) >>> Type.index()
      end
    end
  end

  defintr launch(
            kernel,
            grid_size,
            block_size,
            cluster_size \\ nil,
            async_deps \\ [],
            dynamic_shared_memory_size \\ nil
          ) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__
    callee = kernel[:callee]
    gpu_kernels = "GPU.Kernels"
    callee = MLIR.Attribute.symbol_ref(gpu_kernels, [callee], ctx: ctx)
    kernel_args = Beaver.Walker.operands(kernel) |> Enum.to_list()
    MLIR.CAPI.mlirOperationDestroy(kernel)

    mlir ctx: ctx, blk: blk do
      # Handle grid dimensions
      {grid_x, grid_y, grid_z} =
        {to_index(grid_size, __IR__), to_index(1, __IR__), to_index(1, __IR__)}

      # Handle block dimensions
      {block_x, block_y, block_z} =
        {to_index(block_size, __IR__), to_index(1, __IR__), to_index(1, __IR__)}

      GPU.launch_func(
        asyncDependencies: [],
        gridSizeX: grid_x,
        gridSizeY: grid_y,
        gridSizeZ: grid_z,
        blockSizeX: block_x,
        blockSizeY: block_y,
        blockSizeZ: block_z,
        kernelOperands: kernel_args,
        kernel: callee,
        operand_segment_sizes: :infer,
        loc: loc
      ) >>> []
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
            operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([0, 0, 0]),
            # TODO: make it async so it can be compiled as device allocation
            hostShared: MLIR.Attribute.unit()
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
            operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([0, 1, 0]),
            # TODO: make it async so it can be compiled as device allocation
            hostShared: MLIR.Attribute.unit()
          ) >>> Type.memref!([:dynamic], elem_type)
      end
      |> Pointer.offset_ptr(elem_type, zero, ctx, blk, loc)
    end
  end

  defintr dealloc(ptr) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      # Can only convert with exactly one async dependency.
      token = GPU.wait(loc: loc) >>> ~t{!gpu.async.token}
      GPU.dealloc(token, ptr, loc: loc) >>> ~t{!gpu.async.token}
    end
  end

  defintr print(format, args) when is_atom(format) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.printf(args, format: Attribute.string(format), loc: loc) >>> []
    end
  end
end
