defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = GPU.program_id()
    set! c[i], a[i] + b[i]
    op gpu.return :: []
  end

  defm main(env, ok :: Term.t()) :: Term.t() do
    a_host = ptr! f32(), 100
    b_host = ptr! f32(), 100
    c_host = ptr! f32(), 100
    a = GPU.allocate(f32(), 100)
    b = GPU.allocate(f32(), 100)
    c = GPU.allocate(f32(), 100)
    launch! vec_add(a, b, c), 2, 1
    ok
  end
end

defmodule DefkTest do
  use ExUnit.Case, async: true

  test "compiling a simple vector add kernel" do
    case :os.type() do
      {:unix, :linux} ->
        assert :ok = VecAddKernel.main(:ok)

      _ ->
        assert_raise ArgumentError,
                     ~r"Failed to lookup target for triple 'nvptx64-nvidia-cuda'",
                     fn ->
                       VecAddKernel.main(:ok)
                     end
    end
  end
end
