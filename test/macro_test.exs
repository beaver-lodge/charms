defmodule MacroTest do
  use ExUnit.Case

  test "expand macro" do
    Charms.JIT.init(CallMacroMod)
    assert 100 == CallMacroMod.term_roundtrip1(100)
    assert 200 == CallMacroMod.term_roundtrip2(200)
  end
end
