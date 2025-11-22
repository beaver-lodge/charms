defmodule Charms.GPU do
  @moduledoc """
  intrinsics of MLIR GPU dialect
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Index, GPU, Arith}

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
      Arith.constant(value: Attribute.integer(Type.index(), size), loc: loc) >>> Type.index()
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

  @doc false
  def gpu_module_name, do: "Charms.GPU.Kernels"

  defintr launch(kernel, grid_size, block_size) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__
    callee = kernel[:callee] || raise "kernel callee not found"
    gpu_kernels = gpu_module_name()
    callee = MLIR.Attribute.symbol_ref(gpu_kernels, [callee], ctx: ctx)
    kernel_args = Beaver.Walker.operands(kernel) |> Enum.to_list()
    MLIR.Operation.destroy(kernel)

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
      ) >>> ~t{!gpu.async.token}
    end
  end

  defintr allocate(elem_type, size, opts \\ [host_shared: false]) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      # 1. Prepare Async/Host configuration
      {base_alloc_args, token_types} =
        if opts[:host_shared] do
          {[hostShared: MLIR.Attribute.unit()], []}
        else
          token = GPU.wait(loc: loc) >>> ~t{!gpu.async.token}
          {[asyncDependencies: token], [~t{!gpu.async.token}]}
        end

      # 2. Normalize Size (Shape vs. Dynamic Operands)
      # Returns: {memref_shape, extra_alloc_args}
      {shape, size_args} =
        case size do
          i when is_integer(i) ->
            {[i], []}

          %MLIR.Value{} ->
            # Ensure the size value is an Index type
            idx =
              if Type.index?(MLIR.Value.type(size)) do
                size
              else
                Index.casts(size, loc: loc) >>> Type.index()
              end

            {[:dynamic], [dynamicSizes: idx]}
        end

      # 3. Single Operation Call
      # Merge base args (async/host) with size args (dynamic sizes)
      (GPU.alloc(base_alloc_args, size_args, loc: loc, operand_segment_sizes: :infer) >>>
         [Type.memref!(shape, elem_type) | token_types])
      |> then(
        &case &1 do
          %MLIR.Value{} = ptr ->
            ptr

          [ptr, token] ->
            # If we created an async token, we wait on it immediately
            # (assuming that is the desired behavior for this specific abstraction)
            GPU.wait(token, loc: loc) >>> []
            ptr
        end
      )
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

  defintr return() do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.return(loc: loc) >>> []
    end
  end

  defintr memcpy(dst, src) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      # Can only convert with exactly one async dependency.
      token = GPU.wait(loc: loc) >>> ~t{!gpu.async.token}
      GPU.memcpy(asyncDependencies: token, dst: dst, src: src, loc: loc) >>> ~t{!gpu.async.token}
    end
  end

  defintr await(dependencies) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.wait(asyncDependencies: dependencies, loc: loc) >>> []
    end
  end

  defintr barrier() do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      GPU.barrier(loc: loc) >>> []
    end
  end
end
