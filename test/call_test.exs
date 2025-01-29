defmodule CallTest do
  use ExUnit.Case, async: true

  @tag capture_log: true
  test "if with value" do
    file = "test/call_test.exs"
    line = __ENV__.line + 10

    assert_raise ArgumentError, ~r"Unknown invocation: AbsentMod.absent_fun/2", fn ->
      defmodule CallingAbsentFunc do
        use Charms
        alias Charms.{Pointer, Term}

        defm get(env, i) :: Term.t() do
          AbsentMod.absent_fun(env, i)
          func.return(i)
        end
      end
    end
  end
end
