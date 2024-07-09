defmodule VecAddTest do
  use ExUnit.Case

  test "vec add" do
    {:ok, _} = Charms.JIT.init(AddTwoIntVec)
    a = 1..8 |> Enum.to_list()
    b = List.duplicate(1, 8) |> Enum.to_list()
    assert AddTwoIntVec.add(a, b, :err) == Enum.to_list(2..9)
  end

  test "wrong num of init values" do
    assert_raise ArgumentError, "expected 8 values, got 6", fn ->
      defmodule SixInitValues do
        use Charms
        alias Charms.SIMD

        defm six(env, a, b, error) do
          v1 = SIMD.new(i32(), 8).(1, 1, 1, 1, 1, 1)
          func.return()
        end
      end
    end
  end
end
