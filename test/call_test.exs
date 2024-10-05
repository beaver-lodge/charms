defmodule CallTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  test "if with value" do
    line = __ENV__.line + 10

    log =
      capture_log(fn ->
        assert_raise RuntimeError, fn ->
          defmodule CallingAbsentFunc do
            use Charms
            alias Charms.{Pointer, Term}

            defm get(env, i) :: Term.t() do
              AbsentMod.absent_fun(env, i)
              func.return(i)
            end
          end
        end
      end)

    assert log =~ "Unknown intrinsic: AbsentMod.absent_fun/2"
    assert log =~ "test/call_test.exs:#{line}"
  end
end
