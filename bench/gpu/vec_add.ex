defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU
  @size 200_000
  @block_size 1024
  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = GPU.block_id() * @block_size + GPU.thread_id()

    if i < @size do
      set! c[i], a[i] + b[i]
    end
  end

  defk noop() do
    GPU.return()
  end

  defk barrier() do
    GPU.barrier()
  end

  @grid_size ceil(@size / @block_size)
  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size = Term.to_i64!(env, @size)

    # allocate
    a = GPU.allocate(f32(), size)
    b = GPU.allocate(f32(), size)
    c = GPU.allocate(f32(), size)
    buffer = GPU.allocate(f32(), size, host_shared: true)

    # free
    defer GPU.await([
            GPU.dealloc(a),
            GPU.dealloc(b),
            GPU.dealloc(c),
            GPU.dealloc(buffer)
          ])

    # copy input data to GPU
    movable_list_ptr = tmp! Term.t()
    set! movable_list_ptr[0], l_a
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(a, buffer) |> GPU.await()
    set! movable_list_ptr[0], l_b
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(b, buffer) |> GPU.await()

    # launch kernel
    launch! vec_add(a, b, c), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)
    launch! noop(), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)
    launch! barrier(), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)

    # copy output data back to CPU
    GPU.memcpy(buffer, c) |> GPU.await()
    arr = new! Term.t(), size
    defer free! arr

    for_loop {element, i} <- {buffer, size} do
      element = value arith.extf(element) :: f64()
      set! arr[i], enif_make_double(env, element)
    end

    # convert to Elixir list
    size = value arith.trunci(size) :: i32()
    enif_make_list_from_array(env, arr, size)
  end

  def random_floats() do
    Enum.map(1..@size, fn _ -> :rand.uniform() * 10.0 end)
  end
end
