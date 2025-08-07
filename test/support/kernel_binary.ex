defmodule KernelBinary do
  @moduledoc false
  use Charms
  alias Charms.Term

  # Integer operations
  defm add_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int + b_int
    enif_make_int(env, c)
  end

  defm sub_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int - b_int
    enif_make_int(env, c)
  end

  defm mul_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int * b_int
    enif_make_int(env, c)
  end

  defm div_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int / b_int
    enif_make_int(env, c)
  end

  defm and_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int && b_int
    enif_make_int(env, c)
  end

  defm or_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int || b_int
    enif_make_int(env, c)
  end

  defm not_int(env, a :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    a_int = a_ptr[0]
    c = !a_int
    enif_make_int(env, c)
  end

  defm eq_int(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! i32()
    b_ptr = ptr! i32()
    enif_get_int(env, a, a_ptr)
    enif_get_int(env, b, b_ptr)
    a_int = a_ptr[0]
    b_int = b_ptr[0]
    c = a_int == b_int
    # Convert boolean to integer for return
    c_i32 = value arith.extui(c) :: i32()
    enif_make_int(env, c_i32)
  end

  # Float operations
  defm add_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! f64()
    b_ptr = ptr! f64()
    enif_get_double(env, a, a_ptr)
    enif_get_double(env, b, b_ptr)
    a_float = a_ptr[0]
    b_float = b_ptr[0]
    c = a_float + b_float
    enif_make_double(env, c)
  end

  defm sub_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! f64()
    b_ptr = ptr! f64()
    enif_get_double(env, a, a_ptr)
    enif_get_double(env, b, b_ptr)
    a_float = a_ptr[0]
    b_float = b_ptr[0]
    c = a_float - b_float
    enif_make_double(env, c)
  end

  defm mul_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! f64()
    b_ptr = ptr! f64()
    enif_get_double(env, a, a_ptr)
    enif_get_double(env, b, b_ptr)
    a_float = a_ptr[0]
    b_float = b_ptr[0]
    c = a_float * b_float
    enif_make_double(env, c)
  end

  defm div_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! f64()
    b_ptr = ptr! f64()
    enif_get_double(env, a, a_ptr)
    enif_get_double(env, b, b_ptr)
    a_float = a_ptr[0]
    b_float = b_ptr[0]
    c = a_float / b_float
    enif_make_double(env, c)
  end

  defm eq_float(env, a :: Term.t(), b :: Term.t()) :: Term.t() do
    a_ptr = ptr! f64()
    b_ptr = ptr! f64()
    enif_get_double(env, a, a_ptr)
    enif_get_double(env, b, b_ptr)
    a_float = a_ptr[0]
    b_float = b_ptr[0]
    c = a_float == b_float
    # Convert boolean to integer for return
    c_i32 = value arith.extui(c) :: i32()
    enif_make_int(env, c_i32)
  end
end
