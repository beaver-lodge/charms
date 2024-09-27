defmodule ChildMod do
  use Charms
  alias Charms.Term

  defm get(env, i) :: Term.t() do
    func.return(i)
  end
end

defmodule SupervisorTest do
  use ExUnit.Case

  test "init module with supervisor" do
    {:ok, _} = DynamicSupervisor.start_child(Charms.TestDynamicSupervisor, {ChildMod, []})
    assert ChildMod.get(100) == 100
  end
end
