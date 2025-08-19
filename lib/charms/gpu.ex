defmodule Charms.GPU do
  @moduledoc """
  intrinsics of MLIR GPU dialect
  """
  use Charms.Intrinsic
  alias Charms.Pointer
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Index, GPU, Arith}
  alias MLIR.Type

  defintr program_id(dimension \\ :x) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.block_id(dimension: ~a{#gpu<dim #{dimension}>}, loc: loc) >>> Type.index()
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
        if is_integer(grid_size) do
          {grid_size, 1, 1}
        else
          grid_size
        end
      grid_x = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), grid_x), loc: loc) >>> MLIR.Type.index(ctx: ctx)
      grid_y = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), grid_y), loc: loc) >>> MLIR.Type.index(ctx: ctx)
      grid_z = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), grid_z), loc: loc) >>> MLIR.Type.index(ctx: ctx)

      # Handle block dimensions
      {block_x, block_y, block_z} =
        if is_integer(block_size) do
          {block_size, 1, 1}
        else
          block_size
        end
      block_x = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), block_x), loc: loc) >>> MLIR.Type.index(ctx: ctx)
      block_y = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), block_y), loc: loc) >>> MLIR.Type.index(ctx: ctx)
      block_z = Arith.constant(value: Attribute.integer(MLIR.Type.index(ctx: ctx), block_z), loc: loc) >>> MLIR.Type.index(ctx: ctx)

      operands =
        [grid_x, grid_y, grid_z, block_x, block_y, block_z] ++
          if cluster_size do
            {cluster_x, cluster_y, cluster_z} = cluster_size
            [cluster_x, cluster_y, cluster_z]
          else
            []
          end ++
          if dynamic_shared_memory_size do
            [dynamic_shared_memory_size]
          else
            []
          end ++
          kernel_args

      gridSize = if is_tuple(grid_size), do: tuple_size(grid_size), else: 1
      blockSize = if is_tuple(block_size), do: tuple_size(block_size), else: 1
      clusterSize = 0
      dynamicSharedMemorySize = if(dynamic_shared_memory_size, do: 1, else: 0)
      asyncObject = 0

      operand_segments =
        [
          length(async_deps),
          gridSize,
          gridSize,
          gridSize,
          blockSize,
          blockSize,
          blockSize,
          clusterSize,
          clusterSize,
          clusterSize,
          dynamicSharedMemorySize,
          length(kernel_args),
          asyncObject
        ]

      operand_segment_sizes = Beaver.MLIR.ODS.operand_segment_sizes(operand_segments).(ctx)

      GPU.launch_func(
        async_deps ++ operands,
        kernel: callee,
        operand_segment_sizes: operand_segment_sizes,
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

  defintr print(format, args) when is_atom(format) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__
    mlir ctx: ctx, blk: blk do
      GPU.printf(args, format: Attribute.string(format), loc: loc) >>> []
    end
  end
end
