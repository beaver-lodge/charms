defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = GPU.block_id() * 1024 + GPU.thread_id()
    set! c[i], a[i] + b[i]
    GPU.return()
  end

  @size 10000
  @block_size 1024
  @grid_size Float.ceil(@size / @block_size) |> Float.round() |> trunc()
  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size_ptr = ptr! i64()
    enif_get_int64(env, @size, size_ptr)
    size = size_ptr[0]

    # allocate
    a = GPU.allocate(f32(), size)
    b = GPU.allocate(f32(), size)
    c = GPU.allocate(f32(), size)

    # free
    defer do
      GPU.dealloc(a)
      GPU.dealloc(b)
      GPU.dealloc(c)
    end

    # copy input data to GPU
    movable_list_ptr = ptr! Term.t()
    set! movable_list_ptr[0], l_a
    copy_terms_as_floats(env, movable_list_ptr, a)
    set! movable_list_ptr[0], l_b
    copy_terms_as_floats(env, movable_list_ptr, b)

    # load grid size from module attribute
    grid_size_ptr = ptr! i64()
    enif_get_int64(env, @grid_size, grid_size_ptr)

    # launch kernel
    launch! vec_add(a, b, c), grid_size_ptr[0], 1024

    # copy output data back to CPU
    arr = ptr! Term.t(), size

    for_loop {element, i} <- {c, size} do
      element = value arith.extf(element) :: f64()
      term = enif_make_double(env, element)
      set! arr[i], term
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
end

defmodule DefkTest do
  use ExUnit.Case, async: true

  test "compiling a simple vector add kernel" do
    size = 10000
    l = Enum.to_list(1..size)
    a = l |> Enum.shuffle() |> Enum.map(&(&1 * 1.0))
    b = l |> Enum.shuffle() |> Enum.map(&(&1 * 1.0))

    case :os.type() do
      {:unix, :linux} ->
        res = VecAddKernel.main(a, b)

        for {{x, y, z}, i} <- Enum.with_index(Enum.zip([a, b, res])) do
          assert_in_delta x + y, z, 0.0001, "at index #{i}"
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
