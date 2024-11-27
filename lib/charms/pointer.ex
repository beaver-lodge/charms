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
    quote do
      Charms.Pointer.allocate(unquote(elem_type), 1)
    end
  end

  @doc """
  Allocates an array of `size` elements of the given `elem_type`, returning a pointer to it.
  """
  defintrinsic allocate(elem_type, size), %Opts{ctx: ctx, args: [_elem_type, size_v]} do
    cast =
      case size_v do
        i when is_integer(i) ->
          quote do
            const unquote(size_v) :: i64()
          end

        %MLIR.Value{} ->
          if MLIR.equal?(MLIR.Value.type(size_v), Type.i64(ctx: ctx)) do
            size
          else
            quote do
              value arith.extsi(unquote(size)) :: i64()
            end
          end
      end

    quote do
      size = unquote(cast)
      value llvm.alloca(size, elem_type: unquote(elem_type)) :: Pointer.t()
    end
  end

  @doc """
  Loads a value of `type` from the given pointer `ptr`.
  """
  defintrinsic load(type, ptr) do
    quote do
      value llvm.load(unquote(ptr)) :: unquote(type)
    end
  end

  @doc """
  Stores a value `val` at the given pointer `ptr`.
  """
  defintrinsic store(val, ptr) do
    quote do
      llvm.store(unquote(val), unquote(ptr))
    end
  end

  @doc """
  Gets the element pointer of `elem_type` for the given base pointer `ptr` and index `n`.
  """
  defintrinsic element_ptr(_elem_type, _ptr, _n), %Opts{
    ctx: ctx,
    block: block,
    args: [elem_type, ptr, n]
  } do
    mlir ctx: ctx, block: block do
      LLVM.getelementptr(ptr, n,
        elem_type: elem_type,
        rawConstantIndices: ~a{array<i32: -2147483648>}
      ) >>> ~t{!llvm.ptr}
    end
  end

  @doc """
  Return the pointer type
  """
  defintrinsic t(), %Opts{ctx: ctx} do
    Beaver.Deferred.create(~t{!llvm.ptr}, ctx)
  end
end
