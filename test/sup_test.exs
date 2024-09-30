defmodule ChildMod do
  use Charms
  alias Charms.Term

  defm term_roundtrip(env, i) :: Term.t() do
    func.return(i)
  end
end

defmodule SupervisorTest do
  use ExUnit.Case

  test "init module with supervisor" do
    {:ok, _} =
      DynamicSupervisor.start_child(Charms.TestDynamicSupervisor, Charms.child_spec(ChildMod))

    assert ChildMod.term_roundtrip(100) == 100
  end
end
