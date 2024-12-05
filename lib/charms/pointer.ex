defmodule Charms.Pointer do
  @moduledoc """
  Intrinsic module to work with pointers.
  """
  alias Charms.Pointer
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.{Type}
  alias Beaver.MLIR.Dialect.{LLVM}

  @doc """
  Allocates a single element of the given `elem_type`, returning a pointer to it.
  """
  defintrinsic allocate(elem_type) do
    quote bind_quoted: [elem_type: elem_type] do
      Charms.Pointer.allocate(elem_type, 1)
    end
  end

  @doc """
  Allocates an array of `size` elements of the given `elem_type`, returning a pointer to it.
  """
  defintrinsic allocate(elem_type, size) do
    %Opts{ctx: ctx} = __IR__

    cast =
      case size do
        i when is_integer(i) ->
          quote bind_quoted: [size: i] do
            const size :: i64()
          end

        %MLIR.Value{} ->
          if MLIR.equal?(MLIR.Value.type(size), Type.i64(ctx: ctx)) do
            size
          else
            quote bind_quoted: [size: size] do
              value arith.extsi(size) :: i64()
            end
          end
      end

    quote bind_quoted: [elem_type: elem_type, size: cast] do
      value llvm.alloca(size, elem_type: elem_type) :: Pointer.t()
    end
  end

  @doc """
  Loads a value of `type` from the given pointer `ptr`.
  """
  defintrinsic load(type, ptr) do
    quote bind_quoted: [type: type, ptr: ptr] do
      value llvm.load(ptr) :: type
    end
  end

  @doc """
  Stores a value `val` at the given pointer `ptr`.
  """
  defintrinsic store(val, ptr) do
    quote bind_quoted: [val: val, ptr: ptr] do
      llvm.store(val, ptr)
    end
  end

  @doc """
  Gets the element pointer of `elem_type` for the given base pointer `ptr` and index `n`.
  """
  defintrinsic element_ptr(elem_type, ptr, n) do
    %Opts{ctx: ctx, blk: blk} = __IR__

    mlir ctx: ctx, blk: blk do
      LLVM.getelementptr(ptr, n,
        elem_type: elem_type,
        rawConstantIndices: ~a{array<i32: -2147483648>}
      ) >>> ~t{!llvm.ptr}
    end
  end

  @doc """
  Return the pointer type
  """
  defintrinsic t() do
    %Opts{ctx: ctx} = __IR__
    Beaver.Deferred.create(~t{!llvm.ptr}, ctx)
  end
end
