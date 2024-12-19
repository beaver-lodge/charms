defmodule AddTwoIntVec do
  @moduledoc false
  use Charms
  alias Charms.{SIMD, Term, Pointer}

  defm load_list(env, l :: Term.t()) :: SIMD.t(i32(), 8) do
    i_ptr = Pointer.allocate(i32())
    zero = const 0 :: Pointer.element_type(i_ptr)
    Pointer.store(zero, i_ptr)
    init = SIMD.new(SIMD.t(i32(), 8), [0, 0, 0, 0, 0, 0, 0, 0])

    Enum.reduce(l, init, fn x, acc ->
      v_ptr = Pointer.allocate(i32())
      enif_get_int(env, x, v_ptr)
      i = Pointer.load(i_ptr)
      Pointer.store(i + 1, i_ptr)
      Pointer.load(v_ptr) |> vector.insertelement(acc, i)
    end)
  end

  defm add(env, a, b, error) :: Term.t() do
    v1 = load_list(env, a)
    v2 = load_list(env, b)
    v = arith.addi(v1, v2)
    start = const 0 :: i32()

    enif_make_list8(
      env,
      enif_make_int(env, vector.extractelement(v, start)),
      enif_make_int(env, vector.extractelement(v, start + 1)),
      enif_make_int(env, vector.extractelement(v, start + 2)),
      enif_make_int(env, vector.extractelement(v, start + 3)),
      enif_make_int(env, vector.extractelement(v, start + 4)),
      enif_make_int(env, vector.extractelement(v, start + 5)),
      enif_make_int(env, vector.extractelement(v, start + 6)),
      enif_make_int(env, vector.extractelement(v, start + 7))
    )
  end

  defm dummy_load_no_make(env, a, b, error) :: Term.t() do
    v1 = load_list(env, a)
    v2 = load_list(env, b)
    func.return(a)
  end

  defm dummy_return(env, a, b, error) :: Term.t() do
    func.return(a)
  end
end
