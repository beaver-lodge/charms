defmodule AddTwoIntVec do
  @moduledoc false
  use Charms
  alias Charms.{SIMD, Term, Pointer}

  defm load_list(env, l :: Term.t()) :: SIMD.t(i32(), 8) do
    i_ptr = ptr! i32()
    zero = const 0 :: Pointer.element_type(i_ptr)
    set! i_ptr[0], zero
    vec = SIMD.new(SIMD.t(i32(), 8), [0, 0, 0, 0, 0, 0, 0, 0])

    vec = SIMD.insert(vec, 0, zero + 0)
    vec = SIMD.insert(vec, 1, zero + 1)
    vec = SIMD.insert(vec, 2, zero + 2)
    vec = SIMD.insert(vec, 3, zero + 3)
    vec = SIMD.insert(vec, 4, zero + 4)
    vec = SIMD.insert(vec, 5, zero + 5)
    vec = SIMD.insert(vec, 6, zero + 6)
    vec = SIMD.insert(vec, 7, zero + 7)
  end

  defm add(env, a, b, error) :: Term.t() do
    v1 = load_list(env, a)
    v2 = load_list(env, b)
    v = arith.addi(v1, v2)

    enif_make_list8(
      env,
      enif_make_int(env, SIMD.extract(v, 0)),
      enif_make_int(env, SIMD.extract(v, 1)),
      enif_make_int(env, SIMD.extract(v, 2)),
      enif_make_int(env, SIMD.extract(v, 3)),
      enif_make_int(env, SIMD.extract(v, 4)),
      enif_make_int(env, SIMD.extract(v, 5)),
      enif_make_int(env, SIMD.extract(v, 6)),
      enif_make_int(env, SIMD.extract(v, 7))
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
