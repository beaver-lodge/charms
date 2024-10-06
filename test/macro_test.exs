defmodule MacroTest do
  use ExUnit.Case, async: true

  test "expand macro" do
    assert 100 == CallMacroMod.term_roundtrip1(100)
    assert 200 == CallMacroMod.term_roundtrip2(200)
    assert :ok = Charms.JIT.destroy(CallMacroMod)
  end
end
