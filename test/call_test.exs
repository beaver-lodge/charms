defmodule CallTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  test "if with value" do
    file = "test/call_test.exs"
    line = __ENV__.line + 10

    log =
      capture_log(fn ->
        assert_raise CompileError, fn ->
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

    assert log =~
             "(CompileError) #{file}:#{line}: Unknown invocation: AbsentMod.absent_fun/2"

    assert log =~ "#{file}:#{line}"
  end
end
