defmodule ModAttrTest do
  use ExUnit.Case, async: true

  test "reference an attribute" do
    defmodule SomeAttr do
      use Charms
      alias Charms.{Pointer, Term}

      @a {__MODULE__, :foo, "bar"}
      defm get(env) :: Term.t() do
        func.return(@a)
      end
    end
    |> Charms.JIT.init()

    assert SomeAttr.get() == "this is a string"
  end
end
