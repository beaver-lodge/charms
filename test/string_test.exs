defmodule StringTest do
  use ExUnit.Case, async: true

  test "create str" do
    defmodule SomeString do
      use Charms
      alias Charms.{Pointer, Term}

      defm get(env) :: Term.t() do
        str = "this is a string!"
        str = "this is a string"
        term_ptr = Pointer.allocate(Term.t())
        size = value index.casts(String.length(str)) :: i64()
        d_ptr = enif_make_new_binary(env, size, term_ptr)
        Pointer.copy(str, d_ptr, size)
        Pointer.load(Term.t(), term_ptr)
      end
    end

    assert SomeString.get() == "this is a string"
  end
end
