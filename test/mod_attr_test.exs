defmodule ModAttrTest do
  use ExUnit.Case, async: true

  test "reference an attribute" do
    defmodule SomeAttr do
      use Charms
      alias Charms.Term

      @a :some_attr
      defm get(env) :: Term.t() do
        func.return(@a)
      end
    end

    assert SomeAttr.get() == :some_attr
  end
end
