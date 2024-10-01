defmodule StackTraceTest do
  use ExUnit.Case, async: true

  test "insert frame into stacktrace" do
    res =
      try do
        defmodule AddTwoIntVecVariableSize do
          use Charms
          alias Charms.{SIMD, Term, Pointer}

          defm add(env, a, b, error) :: Term.t() do
            size = enif_list_length(env, a)

            v1 =
              call load_list(env, a, size) ::
                     SIMD.t(i32(), :variable)
          end
        end
      rescue
        _e ->
          assert [
                   _,
                   _,
                   _,
                   {Charms.SIMD, :t, 2, [file: ~c"test/stacktrace_test.exs", line: 16]},
                   {Charms.Defm, :call, 1, [file: ~c"test/stacktrace_test.exs", line: 15]} | _
                 ] = __STACKTRACE__

          :ok
      end

    assert res == :ok
  end
end
