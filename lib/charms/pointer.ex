defmodule Charms.Pointer do
  @moduledoc """
  Intrinsic module to work with pointers.

  Charms.Pointer should be the "smart pointer" not just comes with lifetime management, but also SIMD and Tensor support.
  """
  use Beaver
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.{Type}
  alias Beaver.MLIR.Dialect.{MemRef, Index, Arith, LLVM}

  @doc """
  Allocates a single element of the given `elem_type`, returning a pointer to it.
  """
  defintr allocate(elem_type) do
    {quote do
       Charms.Pointer.allocate(elem_type, 1)
     end, elem_type: elem_type}
  end

  @doc """
  Allocates an array of `size` elements of the given `elem_type`, returning a pointer to it.
  """
  defintr allocate(elem_type, size) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
      operand_segment_sizes_of_zeros = Beaver.MLIR.ODS.operand_segment_sizes([0, 0])

      case size do
        1 ->
          MemRef.alloca(loc: loc, operand_segment_sizes: operand_segment_sizes_of_zeros) >>>
            Type.memref!([1], elem_type)

        i when is_integer(i) ->
          MemRef.alloc(loc: loc, operand_segment_sizes: operand_segment_sizes_of_zeros) >>>
            Type.memref!([i], elem_type)

        %MLIR.Value{} ->
          size =
            if Type.index?(MLIR.Value.type(size)) do
              size
            else
              Index.casts(size, loc: loc) >>> Type.index()
            end

          MemRef.alloc(dynamicSizes: size, loc: loc, operand_segment_sizes: :infer) >>>
            Type.memref!([:dynamic], elem_type)
      end
      |> offset_ptr(elem_type, zero, ctx, blk, loc)
    end
  end

  @doc false
  def memref_ptr?(%MLIR.Type{} = t) do
    MLIR.Type.memref?(t)
  end

  def memref_ptr?(%MLIR.Value{} = ptr) do
    MLIR.Value.type(ptr) |> memref_ptr?()
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
        zero = Index.constant(value: Attribute.index(0), loc: loc) >>> Type.index()
        MemRef.load(ptr, zero, loc: loc) >>> type
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
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
      MemRef.store(val, ptr, zero, loc: loc) >>> []
    end
  end

  defp ptr_type(%MLIR.Type{} = elem_type, ctx) do
    layout = MLIR.Attribute.strided_layout(:dynamic, [1], ctx: ctx)
    Type.memref!([:dynamic], elem_type, layout: layout, ctx: ctx)
  end

  @doc false
  # cast ptr to a pointer of the given element type with offset
  def offset_ptr(ptr, %MLIR.Type{} = elem_type, offset, ctx, blk, loc) do
    mlir ctx: ctx, blk: blk do
      d = MLIR.CAPI.mlirShapedTypeGetDynamicStrideOrOffset() |> Beaver.Native.to_term()
      static_offsets_or_sizes = Attribute.dense_array([d], Beaver.Native.I64, ctx: ctx)
      static_strides = Attribute.dense_array([1], Beaver.Native.I64, ctx: ctx)

      if MLIR.null?(static_offsets_or_sizes) do
        raise ArgumentError, "Failed to create dense array"
      end

      [_, offset_extracted, size, _stride] =
        MemRef.extract_strided_metadata(ptr, loc: loc) >>> :infer

      offset = Arith.addi(offset_extracted, offset, loc: loc) >>> Type.index()

      MemRef.reinterpret_cast(
        source: ptr,
        offsets: offset,
        sizes: size,
        operand_segment_sizes: :infer,
        static_offsets: static_offsets_or_sizes,
        static_sizes: static_offsets_or_sizes,
        static_strides: static_strides,
        loc: loc
      ) >>> ptr_type(elem_type, ctx)
    end
  end

  defintr element_ptr(%MLIR.Type{} = elem_type, ptr, n) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    t = MLIR.Value.type(ptr)
    elem_t = MLIR.CAPI.mlirShapedTypeGetElementType(t)

    if not MLIR.equal?(elem_t, elem_type) do
      raise ArgumentError,
            "Expected a pointer of type #{MLIR.to_string(elem_type)}, got #{MLIR.to_string(t)}"
    end

    mlir ctx: ctx, blk: blk do
      n =
        case n do
          i when is_integer(i) ->
            Index.constant(value: Attribute.index(i)) >>> Type.index()

          %MLIR.Value{} ->
            t = MLIR.Value.type(n)

            cond do
              Type.index?(t) ->
                n

              Type.integer?(t) ->
                Index.casts(n, loc: loc) >>> Type.index()

              true ->
                raise ArgumentError, "Expected an integer or index type, got #{MLIR.to_string(t)}"
            end
        end

      offset_ptr(ptr, elem_type, n, ctx, blk, loc)
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
       end, ptr: ptr, n: n, elem_type: MLIR.CAPI.mlirShapedTypeGetElementType(t)}
    else
      raise ArgumentError, "Pointer is not typed, use element_ptr/3 to specify the pointer type"
    end
  end

  defintr element_type(%MLIR.Value{} = ptr) do
    t = MLIR.Value.type(ptr)

    if memref_ptr?(t) do
      MLIR.CAPI.mlirShapedTypeGetElementType(t)
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
    ptr_type(elem_t, ctx)
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
          [_, offset, _, _] = MemRef.extract_strided_metadata(ptr, loc: loc) >>> :infer
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

  defintr to_offset(ptr) do
    %Opts{ctx: ctx, blk: blk, loc: loc} = __IR__

    mlir ctx: ctx, blk: blk do
      zero = Index.constant(value: Attribute.index(0)) >>> Type.index()
      elem_type = MLIR.Type.element_type(MLIR.Value.type(ptr))
      offset_ptr(ptr, elem_type, zero, ctx, blk, loc)
    end
  end
end
