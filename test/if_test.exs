defmodule IfTest do
  use ExUnit.Case, async: true

  test "if with value" do
    defmodule GetIntIf do
      use Charms
      alias Charms.{Pointer, Term}

      defm get(env, i) :: Term.t() do
        zero = const 0 :: i32()
        one = const 1 :: i32()
        i_ptr = Pointer.allocate(i32())
        enif_get_int(env, i, i_ptr)
        i = i_ptr[0]

        ret =
          if i > 0 do
            one
          else
            zero
          end

        enif_make_int(env, ret)
      end
    end

    assert GetIntIf.get(100) == 1
    assert GetIntIf.get(-100) == 0
  end
end
