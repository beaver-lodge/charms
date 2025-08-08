defmodule VecAddKernel do
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  defk vec_add(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    i = Charms.GPU.program_id()
    set! c[i], a[i] + b[i]
    op gpu.return :: []
  end

  @ok :ok
  defm main(env) :: Term.t() do
    a_host = ptr! f32(), 100
    b_host = ptr! f32(), 100
    c_host = ptr! f32(), 100
    a = GPU.allocate(f32(), 100)
    b = GPU.allocate(f32(), 100)
    c = GPU.allocate(f32(), 100)
    vec_add(a, b, c)
    @ok
  end
end

defmodule DefkTest do
  use ExUnit.Case, async: true

  test "compiling a simple vector add kernel" do
    assert :ok = VecAddKernel.main()
  end
end
