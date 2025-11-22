defmodule Charms.Pointer do
  @moduledoc """
  Intrinsic module to work with pointers.
  """
  use Beaver
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.{Type}
  alias Beaver.MLIR.Dialect.{MemRef, Index, Arith, LLVM}

  defp do_allocate(allocator, ctx, blk, loc, elem_type, size) do
    mlir ctx: ctx, blk: blk do
      case size do
        i when is_integer(i) ->
          allocator.(loc: loc, operand_segment_sizes: :infer) >>>
            Type.memref!([i], elem_type)

        %MLIR.Value{} ->
          size =
            if Type.index?(MLIR.Value.type(size)) do
              size
            else
              Index.casts(size, loc: loc) >>> Type.index()
            end

          allocator.(dynamicSizes: size, loc: loc, operand_segment_sizes: :infer) >>>
            Type.memref!([:dynamic], elem_type)
      end
    end
  end

  @doc """
  Allocates an array of `size` elements of the given `elem_type`, returning a pointer to it.
  """
  defintr allocate(elem_type, size \\ 1) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__
    do_allocate(&MemRef.alloc/1, ctx, blk, loc, elem_type, size)
  end

  @doc """
  Use `alloca` to allocate memory on the stack and hoist it
  to the entry block to prevent stack overflow.
  """
  defintr allocate_local(elem_type, size \\ 1) do
    %Opts{ctx: ctx, loc: loc, entry_blk: blk} = __IR__
    if is_nil(blk), do: raise(ArgumentError, "allocate_local/2 requires an entry block")
    do_allocate(&MemRef.alloca/1, ctx, blk, loc, elem_type, size)
  end

  @doc false
  def memref_ptr?(%MLIR.Type{} = t) do
    MLIR.Type.memref?(t)
  end

  def memref_ptr?(%MLIR.Value{} = ptr) do
    MLIR.Value.type(ptr) |> memref_ptr?()
  end

  defp rank_as_indices(ptr, opts) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = opts

    mlir ctx: ctx, blk: blk do
      zero = Index.constant(value: Attribute.index(0), loc: loc) >>> Type.index()
      rank = MLIR.ShapedType.rank(MLIR.Value.type(ptr))
      if rank == 0, do: [], else: [zero]
    end
  end

  @doc """
  Loads a value of `type` from the given pointer `ptr`.
  """
  defintr load(type, ptr) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    if MLIR.Type.llvm_pointer?(MLIR.Value.type(ptr)) do
      mlir ctx: ctx, blk: blk do
        LLVM.load(ptr, loc: loc) >>> type
      end
    else
      mlir ctx: ctx, blk: blk do
        idx = rank_as_indices(ptr, __IR__)
        MemRef.load(memref: ptr, indices: idx, loc: loc) >>> type
      end
    end
  end

  defintr load(%MLIR.Value{} = ptr) do
    if memref_ptr?(ptr) do
      {quote do
         Charms.Pointer.load(Charms.Pointer.element_type(ptr), ptr)
       end, ptr: ptr}
    else
      raise ArgumentError, "Pointer is not typed, use load/2 to specify the pointer type"
    end
  end

  @doc """
  Stores a value `val` at the given pointer `ptr`.
  """
  defintr store(val, ptr) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      idx = rank_as_indices(ptr, __IR__)
      MemRef.store(value: val, memref: ptr, indices: idx, loc: loc) >>> []
    end
  end

  @doc false
  def unified_ptr_type(%MLIR.Type{} = elem_type, ctx) do
    layout = MLIR.Attribute.strided_layout(:dynamic, [1], ctx: ctx)
    Type.memref!([:dynamic], elem_type, layout: layout, ctx: ctx)
  end

  defp get_offset_and_size(strided_metadata, ctx, blk) do
    mlir ctx: ctx, blk: blk do
      case strided_metadata do
        # 1D MemRef
        [_, offset, size, _stride] ->
          {offset, size}

        # 0D MemRef
        [_, offset] ->
          one = Arith.constant(value: Attribute.integer(Type.index(), 1)) >>> :infer
          {offset, one}
      end
    end
  end

  @doc """
  Make sure the pointer represented as memref has a unified layout
  - 1D
  - stride 1
  - dynamic offset
  - dynamic size
  """
  def unify_layout(%MLIR.Value{} = ptr, ctx, blk, loc, offset \\ 0) do
    mlir ctx: ctx, blk: blk do
      elem_type = MLIR.Value.type(ptr) |> MLIR.ShapedType.element_type()

      static_offsets =
        Attribute.dense_array([MLIR.ShapedType.dynamic_stride_or_offset()], Beaver.Native.I64,
          ctx: ctx
        )

      static_sizes =
        Attribute.dense_array([MLIR.ShapedType.dynamic_size()], Beaver.Native.I64, ctx: ctx)

      static_strides = Attribute.dense_array([1], Beaver.Native.I64, ctx: ctx)

      strided_metadata = MemRef.extract_strided_metadata(ptr, loc: loc) >>> :infer

      {offset_extracted, size} = get_offset_and_size(strided_metadata, ctx, blk)

      offset =
        case offset do
          i when is_integer(i) ->
            Index.constant(value: Attribute.index(i)) >>> Type.index()

          %MLIR.Value{} ->
            t = MLIR.Value.type(offset)

            cond do
              Type.index?(t) ->
                offset

              Type.integer?(t) ->
                Index.casts(offset, loc: loc) >>> Type.index()

              true ->
                raise ArgumentError,
                      "Expected an integer or index type, got #{MLIR.to_string(t)}"
            end
        end

      offset = Arith.addi(offset_extracted, offset, loc: loc) >>> Type.index()

      MemRef.reinterpret_cast(
        source: ptr,
        offsets: offset,
        sizes: size,
        operand_segment_sizes: :infer,
        static_offsets: static_offsets,
        static_sizes: static_sizes,
        static_strides: static_strides,
        loc: loc
      ) >>> unified_ptr_type(elem_type, ctx)
    end
  end

  defintr element_ptr(%MLIR.Type{} = elem_type, ptr, n) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    t = MLIR.Value.type(ptr)
    elem_t = MLIR.ShapedType.element_type(t)

    if not MLIR.equal?(elem_t, elem_type) do
      raise ArgumentError,
            "Expected a pointer of type #{MLIR.to_string(elem_type)}, got #{MLIR.to_string(t)}"
    end

    mlir ctx: ctx, blk: blk do
      unify_layout(ptr, ctx, blk, loc, n)
    end
  end

  @doc """
  Gets the element pointer of `elem_type` for the given base pointer `ptr` and index `n`.
  """
  defintr element_ptr(%MLIR.Value{} = ptr, n) do
    t = MLIR.Value.type(ptr)

    if memref_ptr?(t) do
      {quote do
         Charms.Pointer.element_ptr(elem_type, ptr, n)
       end, ptr: ptr, n: n, elem_type: MLIR.ShapedType.element_type(t)}
    else
      raise ArgumentError, "Pointer is not typed, use element_ptr/3 to specify the pointer type"
    end
  end

  defintr element_type(%MLIR.Value{} = ptr) do
    t = MLIR.Value.type(ptr)

    if memref_ptr?(t) do
      MLIR.ShapedType.element_type(t)
    else
      raise ArgumentError, "Pointer is not typed, element_type/1 expects a typed pointer"
    end
  end

  @doc """
  Return the pointer type
  """
  defintr t() do
    %Opts{ctx: ctx} = __IR__
    Beaver.Deferred.create(~t{!llvm.ptr}, ctx)
  end

  defintr t(%MLIR.Type{} = elem_t) do
    %Opts{ctx: ctx} = __IR__
    unified_ptr_type(elem_t, ctx)
  end

  @doc false
  def extract_raw_pointer(%MLIR.Value{} = ptr, %Opts{ctx: ctx, blk: blk, loc: loc}) do
    t = MLIR.Value.type(ptr)

    cond do
      MLIR.equal?(~t{!llvm.ptr}.(ctx), t) ->
        ptr

      Charms.Pointer.memref_ptr?(t) ->
        mlir ctx: ctx, blk: blk do
          unless MLIR.Type.shaped?(t) do
            raise ArgumentError, "Expected a pointer type, got #{MLIR.to_string(t)}"
          end

          width = MLIR.Type.element_type(t) |> MLIR.Type.width()
          width = Index.constant(value: Attribute.index(width), loc: loc) >>> Type.index()
          ptr_i = MemRef.extract_aligned_pointer_as_index(ptr, loc: loc) >>> Type.index()
          strided_metadata = MemRef.extract_strided_metadata(ptr, loc: loc) >>> :infer
          {offset, _size} = get_offset_and_size(strided_metadata, ctx, blk)
          offset = Arith.muli(offset, width, loc: loc) >>> Type.index()
          ptr_i = Arith.addi(ptr_i, offset, loc: loc) >>> Type.index()
          ptr_i = Arith.index_cast(ptr_i, loc: loc) >>> Type.i64()
          LLVM.inttoptr(ptr_i, loc: loc) >>> ~t{!llvm.ptr}
        end

      true ->
        raise ArgumentError, "Expected a pointer, got #{MLIR.to_string(t)}"
    end
  end

  defintr raw(%MLIR.Value{} = ptr) do
    extract_raw_pointer(ptr, __IR__)
  end

  defintr copy(source, destination, bytes_count) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__
    source = extract_raw_pointer(source, __IR__)
    destination = extract_raw_pointer(destination, __IR__)

    mlir ctx: ctx, blk: blk do
      LLVM.intr_memcpy(destination, source, bytes_count,
        isVolatile: MLIR.Attribute.bool(false),
        loc: loc
      ) >>> []
    end
  end

  @doc """
  Frees memory previously allocated with allocate/1 or allocate/2.
  """
  defintr free(ptr) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    t = MLIR.Value.type(ptr)

    if MLIR.Type.llvm_pointer?(t) do
      mlir ctx: ctx, blk: blk do
        raise ArgumentError, "Cannot free LLVM pointers"
      end
    else
      mlir ctx: ctx, blk: blk do
        MemRef.dealloc(ptr, loc: loc) >>> []
      end
    end
  end

  @doc """
  Get a null raw ptr
  """
  defintr null() do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      LLVM.mlir_zero(loc: loc) >>> ~t{!llvm.ptr}
    end
  end
end
