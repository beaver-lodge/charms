defmodule DeferTest do
  use ExUnit.Case, async: true

  defmodule DeferSendMessage do
    use Charms
    alias Charms.{Term, Pointer}

    @ok :ok
    defm test_defer_with_message(env, pid_term, msg1, msg2, msg3) :: Term.t() do
      pid = Term.to_pid!(env, pid_term)
      null_env = Pointer.null()
      defer enif_send!(env, pid, null_env, msg3)
      defer enif_send!(env, pid, null_env, msg2)
      enif_send!(env, pid, null_env, msg1)
      @ok
    end

    defm test_defer_with_explicit_terminator(env, pid_term, msg) :: Term.t() do
      pid = Term.to_pid!(env, pid_term)
      null_env = Pointer.null()
      defer enif_send!(env, pid, Pointer.null(), msg)
      func.return(@ok)
    end

    defm test_defer_with_do_block(env, pid_term, msg1, msg2, msg3) :: Term.t() do
      pid = Term.to_pid!(env, pid_term)

      defer do
        null_env = Pointer.null()
        enif_send!(env, pid, null_env, msg1)
        enif_send!(env, pid, null_env, msg2)
        enif_send!(env, pid, null_env, msg3)
      end

      func.return(@ok)
    end
  end

  @messages [test: 1, test: 2, test: 3]
  test "order of defer send message" do
    pid = self()
    [msg1, msg2, msg3] = @messages
    assert :ok = DeferSendMessage.test_defer_with_message(pid, msg1, msg2, msg3)
    assert {:messages, @messages} = Process.info(pid, :messages)
  end

  test "defer with explicit terminator" do
    msg = :test
    assert :ok = DeferSendMessage.test_defer_with_explicit_terminator(self(), msg)
    assert_receive ^msg
  end

  test "defer with do-block" do
    pid = self()
    [msg1, msg2, msg3] = @messages
    assert :ok = DeferSendMessage.test_defer_with_do_block(pid, msg1, msg2, msg3)
    assert {:messages, @messages} = Process.info(pid, :messages)
  end
end
