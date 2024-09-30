defmodule SupervisorTest do
  use ExUnit.Case

  test "init module with supervisor" do
    {:ok, pid} =
      DynamicSupervisor.start_child(Charms.TestDynamicSupervisor, Charms.child_spec(ChildMod))

    assert ChildMod.term_roundtrip(100) == 100
    Charms.JIT.destroy(ChildMod)
    refute Process.alive?(pid)
  end

  test "init merged modules with supervisor" do
    {:ok, pid} =
      DynamicSupervisor.start_child(
        Charms.TestDynamicSupervisor,
        Charms.child_spec([ChildMod, ChildMod2], name: ChildModMerged)
      )

    assert ChildModMerged
           |> Charms.JIT.engine()
           |> Charms.JIT.invoke(&ChildMod2.term_roundtrip/1, [100]) == 100

    Charms.JIT.destroy(ChildModMerged)
    refute Process.alive?(pid)
  end
end
