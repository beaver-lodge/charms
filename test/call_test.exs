defmodule CallTest do
  use ExUnit.Case, async: true

  test "call with return type" do
    assert :with == DifferentCalls.return_type_annotation(:with)
  end

  test "call without return type" do
    assert :without == DifferentCalls.no_return_type_annotation(:without)
  end

  describe "can't resolve" do
    test "remote function of defined module" do
      line = __ENV__.line

      assert_raise CompileError,
                   ~r"function or intrinsic DifferentCalls.something/1 is undefined",
                   fn ->
                     defmodule UndefinedFunctionTest do
                       use Charms

                       defm without_call_macro(env, i) do
                         DifferentCalls.something(i)
                       end
                     end
                   end
    end

    test "remote function of undefined module" do
      file = "test/call_test.exs"
      line = __ENV__.line + 10

      assert_raise CompileError, ~r"can't resolve AbsentMod.absent_fun/2", fn ->
        defmodule CallingAbsentFunc do
          use Charms
          alias Charms.Term

          defm get(env, i) :: Term.t() do
            AbsentMod.absent_fun(env, i)
            func.return(i)
          end
        end
      end
    end

    test "local" do
      line = __ENV__.line

      assert_raise CompileError,
                   ~r"function or intrinsic CallTest.UndefinedLocalTest.something/1 is undefined",
                   fn ->
                     defmodule UndefinedLocalTest do
                       use Charms

                       defm call(env, i) do
                         something(i)
                       end
                     end
                   end
    end
  end

  test "remote with env" do
    defmodule RemoteWithEnv do
      use Charms
      alias Charms.Term

      defm get(env, i) :: Term.t() do
        func.return(i)
      end
    end

    assert_raise ArgumentError,
                 "Call CallTest.RemoteWithEnv.get/1 to invoke the JIT compiled function.",
                 fn ->
                   RemoteWithEnv.get(:env, 1)
                 end

    defmodule CallRemoveWithEnv do
      use Charms
      alias Charms.Term

      defm get(env, i) :: Term.t() do
        RemoteWithEnv.get(env, i)
      end
    end

    assert_raise CompileError,
                 ~r"function or intrinsic CallTest.RemoteWithEnv.get/1 is undefined",
                 fn ->
                   defmodule CallRemoveWithArityOf1 do
                     use Charms
                     alias Charms.Term

                     defm get(env, i) :: Term.t() do
                       RemoteWithEnv.get(i)
                     end
                   end
                 end

    defmodule ImportCallRemoveWithEnv do
      use Charms
      alias Charms.Term
      import RemoteWithEnv

      defm get1(env, i) :: Term.t() do
        get(env, i)
      end
    end

    assert 1 = RemoteWithEnv.get(1)
    assert 2 = CallRemoveWithEnv.get(2)
  end

  describe "undefined alias" do
    test "invalid return" do
      assert_raise CompileError,
                   ~r"test/call_test.exs:#{__ENV__.line + 5}: can't resolve Invalid.t/0",
                   fn ->
                     defmodule InvalidRet do
                       use Charms

                       defm my_function(env, arg1, arg2) :: Invalid.t() do
                         func.return(arg2)
                       end
                     end
                   end
    end

    test "invalid arg" do
      assert_raise CompileError,
                   ~r"test/call_test.exs:#{__ENV__.line + 6}: can't resolve Invalid.t/0",
                   fn ->
                     defmodule InvalidArgType do
                       use Charms
                       alias Charms.Term

                       defm my_function(env, arg1 :: Invalid.t(), arg2) :: Term.t() do
                         func.return(arg2)
                       end
                     end
                   end
    end
  end

  describe "imported" do
    test "defm" do
      defmodule DefmBeingImported do
        use Charms
        alias Charms.Term

        defm get(x) :: Term.t() do
          x
        end
      end

      defmodule CallImportedDefm do
        use Charms
        alias Charms.Term
        import DefmBeingImported

        defm get(env, i) :: Term.t() do
          get(i)
        end
      end

      assert 1 = CallImportedDefm.get(1)
    end

    test "intrinsic" do
      defmodule IntrinsicBeingImported do
        use Charms
        use Charms.Intrinsic

        defintr get(x) do
          x
        end
      end

      defmodule CallImportedIntrinsic do
        use Charms
        alias Charms.Term
        import IntrinsicBeingImported

        defm get(env, i) :: Term.t() do
          get(i)
        end
      end

      assert 1 = CallImportedIntrinsic.get(1)
    end
  end
end
