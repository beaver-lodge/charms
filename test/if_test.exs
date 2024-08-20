defmodule IfTest do
  use ExUnit.Case

  test "if with value" do
    defmodule GetIntIf do
      use Charms
      alias Charms.{Pointer, Term}

      defm get(env, i) :: Term.t() do
        zero = arith.constant(value: Attribute.integer(i32(), 0))
        one = arith.constant(value: Attribute.integer(i32(), 1))
        i_ptr = Pointer.allocate(i32())
        enif_get_int(env, i, i_ptr)
        i = Pointer.load(i32(), i_ptr)

        ret =
          if(i > 0) do
            one
          else
            zero
          end

        ret = enif_make_int(env, ret)
        func.return(ret)
      end
    end
    |> Charms.JIT.init()

    assert GetIntIf.get(100) == 1
    assert GetIntIf.get(-100) == 0
  end
end
