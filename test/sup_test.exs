defmodule ChildMod do
  use Charms
  alias Charms.Term

  defm get(env, i) :: Term.t() do
    one = const 1 :: i32()
    one = enif_make_int(env, one)
    func.return(one)
  end
end

defmodule SupervisorTest do
  use ExUnit.Case

  test "init module with supervisor" do
    {:ok, _} = DynamicSupervisor.start_child(Charms.TestDynamicSupervisor, {ChildMod, []})
    assert ChildMod.get(100) == 1
  end
end
