defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  @block_size 1024
  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = GPU.block_id() * @block_size + GPU.thread_id()
    set! c[i], a[i] + b[i]
    GPU.return()
  end

  @size 10_000
  @grid_size Float.ceil(@size / @block_size) |> Float.round() |> trunc()
  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size = Term.to_i64!(env, @size)

    # allocate
    a_alloc = GPU.allocate(f32(), size)
    b_alloc = GPU.allocate(f32(), size)
    c_alloc = GPU.allocate(f32(), size)
    buffer_alloc = GPU.allocate(f32(), size, host_shared: true)

    # free
    defer GPU.await([
            GPU.dealloc(a_alloc),
            GPU.dealloc(b_alloc),
            GPU.dealloc(c_alloc),
            GPU.dealloc(buffer_alloc)
          ])

    buffer = Pointer.to_offset(buffer_alloc)
    # copy input data to GPU
    movable_list_ptr = ptr! Term.t()
    set! movable_list_ptr[0], l_a
    copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(a_alloc, buffer_alloc) |> GPU.await()
    set! movable_list_ptr[0], l_b
    copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(b_alloc, buffer_alloc) |> GPU.await()

    a = Pointer.to_offset(a_alloc)
    b = Pointer.to_offset(b_alloc)
    c = Pointer.to_offset(c_alloc)
    # launch kernel
    launch! vec_add(a, b, c), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)

    # copy output data back to CPU
    GPU.memcpy(buffer_alloc, c_alloc) |> GPU.await()
    arr = ptr! Term.t(), size

    for_loop {element, i} <- {buffer, size} do
      element = value arith.extf(element) :: f64()
      set! arr[i], enif_make_double(env, element)
    end

    # convert to Elixir list
    size = value arith.trunci(size) :: i32()
    enif_make_list_from_array(env, arr, size)
  end

  defm copy_terms_as_floats(env, tail :: Pointer.t(Term.t()), arr :: Pointer.t(f32())) do
    head = ptr! Term.t()
    zero = const 0 :: i32()
    i_ptr = ptr! i32()
    set! i_ptr[0], zero

    while(
      enif_get_list_cell(
        env,
        tail[0],
        head,
        tail
      ) > 0
    ) do
      double_ptr = ptr! f64()
      enif_get_double(env, head[0], double_ptr)
      i = i_ptr[0]
      set! arr[i], value(arith.truncf(double_ptr[0]) :: f32())
      set! i_ptr[0], i + 1
    end
  end

  def random_floats() do
    Enum.map(1..@size, fn _ -> :rand.uniform() * 10.0 end)
  end
end

defmodule DefkTest do
  use ExUnit.Case, async: true

  test "compiling a simple vector add kernel" do
    a = VecAddKernel.random_floats()
    b = VecAddKernel.random_floats()

    if Charms.JIT.cuda_available?() do
      res = VecAddKernel.main(a, b)

      for {x, y, z} <- Enum.zip([a, b, res]) do
        assert_in_delta x + y, z, 0.0001
      end
    else
      case :os.type() do
        {:unix, :linux} ->
          assert_raise ArgumentError,
                       ~r"CUDA path: /usr/local/cuda does not exist or is not a directory.",
                       fn ->
                         VecAddKernel.main(a, b)
                       end

        _ ->
          assert_raise ArgumentError,
                       ~r"Failed to lookup target for triple 'nvptx64-nvidia-cuda'",
                       fn ->
                         VecAddKernel.main(a, b)
                       end
      end
    end
  end
end
