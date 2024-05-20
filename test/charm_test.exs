defmodule CharmTest do
  use ExUnit.Case
  doctest Charm

  test "greets the world" do
    assert Charm.hello() == :world
  end
end
