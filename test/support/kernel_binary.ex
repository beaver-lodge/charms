defmodule KernelBinary do
  @moduledoc false
  use Charms
  alias Charms.Term

  # Integer operations
  defm add_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int + b_int
    enif_make_int(env, c)
  end

  defm sub_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int - b_int
    enif_make_int(env, c)
  end

  defm mul_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int * b_int
    enif_make_int(env, c)
  end

  defm div_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int / b_int
    enif_make_int(env, c)
  end

  defm and_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int && b_int
    enif_make_int(env, c)
  end

  defm or_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int || b_int
    enif_make_int(env, c)
  end

  defm not_int(env, a :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    c = !a_int
    enif_make_int(env, c)
  end

  defm eq_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_int = Term.to_i32!(env, a)
    b_int = Term.to_i32!(env, b)
    c = a_int == b_int
    # Convert boolean to integer for return
    c_i32 = value arith.extui(c) :: i32()
    enif_make_int(env, c_i32)
  end

  # Float operations
  defm add_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_float = Term.to_f64!(env, a)
    b_float = Term.to_f64!(env, b)
    c = a_float + b_float
    enif_make_double(env, c)
  end

  defm sub_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_float = Term.to_f64!(env, a)
    b_float = Term.to_f64!(env, b)
    c = a_float - b_float
    enif_make_double(env, c)
  end

  defm mul_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_float = Term.to_f64!(env, a)
    b_float = Term.to_f64!(env, b)
    c = a_float * b_float
    enif_make_double(env, c)
  end

  defm div_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_float = Term.to_f64!(env, a)
    b_float = Term.to_f64!(env, b)
    c = a_float / b_float
    enif_make_double(env, c)
  end

  defm eq_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_float = Term.to_f64!(env, a)
    b_float = Term.to_f64!(env, b)
    c = a_float == b_float
    # Convert boolean to integer for return
    c_i32 = value arith.extui(c) :: i32()
    enif_make_int(env, c_i32)
  end
end
