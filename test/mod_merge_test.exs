defmodule ModMergeTest do
  use ExUnit.Case, async: true

  test "attr" do
    assert 1 = SubMod0.get_term(1)
    assert 1 = SubMod1.get_term(1)
  end

  test "func" do
    assert SubMod0 = SubMod0.get_attr()
    assert SubMod1 = SubMod1.get_attr()
    assert SubMod0.get_attr_dup() == SubMod1.get_attr_dup()
  end
end
