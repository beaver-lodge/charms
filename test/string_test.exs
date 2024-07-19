defmodule StringTest do
  use ExUnit.Case

  test "create str" do
    defmodule SomeString do
      use Charms
      alias Charms.{Pointer, Term}

      defm get(env) :: Term.t() do
        str = "this is a string"
        str = "this is a string"
        size = arith.constant(value: Attribute.integer(i32(), 4))
        term_ptr = Pointer.allocate(Term.t())
        d_ptr = enif_make_new_binary(env, String.length(str), term_ptr)
        m = ptr_to_memref(d_ptr)
        memref.copy(str, m)
        t = Pointer.load(Term.t(), term_ptr)
        func.return(t)
      end
    end
    |> Charms.JIT.init()

    assert SomeString.get() == "this is a string"
  end
end
