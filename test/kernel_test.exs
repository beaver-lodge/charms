defmodule KernelTest do
  use ExUnit.Case, async: true

  describe "kernel operations" do
    test "integer operations" do
      assert 3 = KernelBinary.add_int(1, 2)
      assert -1 = KernelBinary.sub_int(1, 2)
      assert 4 = KernelBinary.mul_int(2, 2)
      assert 2 = KernelBinary.div_int(4, 2)
      assert 0 = KernelBinary.and_int(1, 0)
      assert 1 = KernelBinary.or_int(1, 0)
      assert 1 = KernelBinary.not_int(0)
      assert 0 = KernelBinary.not_int(1)
      assert 1 = KernelBinary.eq_int(5, 5)
      assert 0 = KernelBinary.eq_int(5, 6)
    end

    test "float operations" do
      assert_in_delta 3.0, KernelBinary.add_float(1.0, 2.0), 0.001
      assert_in_delta -1.0, KernelBinary.sub_float(1.0, 2.0), 0.001
      assert_in_delta 4.0, KernelBinary.mul_float(2.0, 2.0), 0.001
      assert_in_delta 2.0, KernelBinary.div_float(4.0, 2.0), 0.001
      assert 1 = KernelBinary.eq_float(1.0, 1.0)
      assert 0 = KernelBinary.eq_float(1.0, 1.1)
    end
  end
end
