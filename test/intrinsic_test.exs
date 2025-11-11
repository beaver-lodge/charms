defmodule Charms.IntrinsicTest do
  use ExUnit.Case
  import Charms.Intrinsic

  defmodule TestIntrinsic do
    use Charms.Intrinsic

    defintr test_default(arg1, arg2 \\ :default) do
      %{arg1: arg1, arg2: arg2}
    end

    defintr test_when_clause(arg) when is_atom(arg) do
      %{arg: arg, type: :atom}
    end

    defintr test_when_clause(arg) when is_integer(arg) do
      %{arg: arg, type: :integer}
    end
  end

  describe "defintr with default values" do
    test "must be called in defm" do
      assert_raise RuntimeError,
                   ~r"Intrinsic Charms.IntrinsicTest.TestIntrinsic.test_default/2 cannot be called outside of a defm",
                   fn ->
                     TestIntrinsic.test_default(:value1)
                   end
    end

    test "uses default value when not provided" do
      assert :__defintrinsic_test_default__ = TestIntrinsic.__intrinsics__(:test_default, 1)
      result = TestIntrinsic.__defintrinsic_test_default__(1).(%Charms.Intrinsic.Opts{})
      assert result == %{arg1: 1, arg2: :default}
    end

    test "uses provided value when given" do
      assert :__defintrinsic_test_default__ = TestIntrinsic.__intrinsics__(:test_default, 2)
      result = TestIntrinsic.__defintrinsic_test_default__(1, 2).(%Charms.Intrinsic.Opts{})
      assert result == %{arg1: 1, arg2: 2}
    end
  end

  describe "defintr with when clauses" do
    test "matches atom clause" do
      assert :__defintrinsic_test_when_clause__ =
               TestIntrinsic.__intrinsics__(:test_when_clause, 1)

      result =
        TestIntrinsic.__defintrinsic_test_when_clause__(:atom_value).(%Charms.Intrinsic.Opts{})

      assert result == %{arg: :atom_value, type: :atom}
    end

    test "matches integer clause" do
      assert :__defintrinsic_test_when_clause__ =
               TestIntrinsic.__intrinsics__(:test_when_clause, 1)

      result = TestIntrinsic.__defintrinsic_test_when_clause__(123).(%Charms.Intrinsic.Opts{})
      assert result == %{arg: 123, type: :integer}
    end
  end
end
