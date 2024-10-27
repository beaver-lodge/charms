defmodule Charms.Pointer do
  @moduledoc """
  Intrinsic module to work with pointers.
  """
  use Charms.Intrinsic
  alias Beaver.MLIR.{Type, Attribute}
  alias Beaver.MLIR.Dialect.{Arith, LLVM, Index}

  @impl true
  def handle_intrinsic(:allocate, params, [elem_type], opts) do
    handle_intrinsic(:allocate, params, [elem_type, 1], opts)
  end

  def handle_intrinsic(:allocate, _params, [elem_type, size], opts) when is_integer(size) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      size = Arith.constant(value: Attribute.integer(Type.i(32), size)) >>> ~t<i32>

      size =
        if MLIR.CAPI.mlirTypeIsAIndex(MLIR.Value.type(size)) |> Beaver.Native.to_term() do
          Index.casts(size) >>> Type.i64()
        else
          size
        end

      LLVM.alloca(size, elem_type: elem_type) >>> ~t{!llvm.ptr}
    end
  end

  def handle_intrinsic(
        :allocate,
        [elem_type, size],
        [_elem_type = %MLIR.Type{}, size_v = %MLIR.Value{}],
        opts
      ) do
    cast =
      cond do
        not MLIR.equal?(MLIR.Value.type(size_v), Type.i64(ctx: opts[:ctx])) ->
          quote do
            size = value arith.extsi(unquote(size)) :: i64()
          end

        true ->
          size
      end

    quote do
      size = unquote(cast)
      value llvm.alloca(size, elem_type: unquote(elem_type)) :: "!llvm.ptr"
    end
  end

  def handle_intrinsic(:load, _params, [type, ptr], opts) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      LLVM.load(ptr) >>> type
    end
  end

  def handle_intrinsic(:store, _params, [val, ptr], opts) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      LLVM.store(val, ptr) >>> []
    end
  end

  def handle_intrinsic(:element_ptr, _params, [elem_type, ptr, n], opts) do
    mlir ctx: opts[:ctx], block: opts[:block] do
      LLVM.getelementptr(ptr, n,
        elem_type: elem_type,
        rawConstantIndices: ~a{array<i32: -2147483648>}
      ) >>> ~t{!llvm.ptr}
    end
  end

  def handle_intrinsic(:t, _params, [], opts) do
    Beaver.Deferred.from_opts(opts, ~t{!llvm.ptr})
  end

  defintrinsic [:t, :allocate, :load, :store, :element_ptr]
end
