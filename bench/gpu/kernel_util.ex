defmodule KernelUtil do
  @moduledoc false
  use Charms
  alias Charms.{Term, Pointer}

  defm copy_terms_as_floats(env, tail :: Pointer.t(Term.t()), arr :: Pointer.t(f32())) do
    head = tmp! Term.t()
    zero = const 0 :: i32()
    i_ptr = tmp! i32()
    set! i_ptr[0], zero

    while(enif_get_list_cell(env, tail[0], head, tail) > 0) do
      i = i_ptr[0]
      set! arr[i], value(arith.truncf(Term.to_f64!(env, head[0])) :: f32())
      set! i_ptr[0], i + 1
    end
  end
end
