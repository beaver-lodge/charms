defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = GPU.program_id()
    set! c[i], a[i] + b[i]
    # GPU.print(:"Index: %d, A: %f, B: %f, C: %f\n", [i, a[i], b[i], c[i]])
    op gpu.return :: []
  end

  @size 98432
  @block_size 1024
  @grid_size Float.ceil(@size / @block_size) |> Float.round()
  defm main(env, ok :: Term.t()) :: Term.t() do
    size_ptr = ptr! i64()
    enif_get_int64(env, @size, size_ptr)
    size = size_ptr[0]
    a_host = ptr! f32(), size
    b_host = ptr! f32(), size
    c_host = ptr! f32(), size
    a = GPU.allocate(f32(), size)
    b = GPU.allocate(f32(), size)
    c = GPU.allocate(f32(), size)
    launch! vec_add(a, b, c), 97, 1024
    arr = ptr! Term.t(), size

    for_loop {element, i} <- {c, size} do
      element = value arith.extf(element) :: f64()
      term = enif_make_double(env, element)
      set! arr[i], term
    end

    size = value arith.trunci(size) :: i32()
    enif_make_list_from_array(env, arr, size)
  end
end

defmodule DefkTest do
  use ExUnit.Case, async: true

  test "compiling a simple vector add kernel" do
    case :os.type() do
      {:unix, :linux} ->
        res = VecAddKernel.main(:ok)
        Process.sleep(2000)
        Enum.max(res) |> dbg
        assert :ok = res

      _ ->
        assert_raise ArgumentError,
                     ~r"Failed to lookup target for triple 'nvptx64-nvidia-cuda'",
                     fn ->
                       VecAddKernel.main(:ok)
                     end
    end
  end
end
