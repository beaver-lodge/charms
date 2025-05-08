defmodule CtypesTest do
  use ExUnit.Case, async: true

  test "calls C sqrt function" do
    # Test the sqrt function
    assert_in_delta SqrtWrapper.sqrt(4.0), 2.0, 0.0001
    assert_in_delta SqrtWrapper.sqrt(9.0), 3.0, 0.0001
    assert_in_delta SqrtWrapper.sqrt(2.25), 1.5, 0.0001
  end
end
