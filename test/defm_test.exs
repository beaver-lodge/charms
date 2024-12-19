defmodule DefmTest do
  import ExUnit.CaptureIO
  use ExUnit.Case, async: true

  test "referenced modules" do
    assert [RefereeMod] == ReferrerMod.referenced_modules()
  end

  test "invalid return of absent alias" do
    assert_raise CompileError,
                 ~r"test/defm_test.exs:#{__ENV__.line + 5}: invalid return type",
                 fn ->
                   defmodule InvalidRet do
                     use Charms

                     defm my_function(env, arg1, arg2) :: Invalid.t() do
                       func.return(arg2)
                     end
                   end
                 end
  end

  test "invalid arg of absent alias" do
    assert_raise CompileError,
                 ~r"test/defm_test.exs:#{__ENV__.line + 6}: invalid argument type #2",
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

  test "only env defm is exported" do
    refute function_exported?(RefereeMod, :term_roundtrip1, 0)
    refute function_exported?(RefereeMod, :term_roundtrip1, 1)
    refute function_exported?(RefereeMod, :term_roundtrip1, 2)
    assert 1 = RefereeMod.term_roundtrip0(1)
    assert 1 = ReferrerMod.term_roundtrip(1)
  end

  describe "different sorts" do
    for s <- [ENIFTimSort, ENIFMergeSort, ENIFQuickSort] do
      test "#{s}" do
        s = unquote(s)
        arr = [5, 4, 3, 2, 1]
        assert s.sort(arr) == Enum.sort(arr)
        assert_raise ArgumentError, "list expected", fn -> s.sort(:what) end

        assert {:cached, %Charms.JIT{}} =
                 Charms.JIT.init(s, name: s.__ir_digest__())

        for i <- 0..1000 do
          arr = 0..i |> Enum.shuffle()
          assert s.sort(arr) == Enum.sort(arr)
        end

        assert :ok = Charms.JIT.destroy(s.__ir_digest__())
        assert :noop = Charms.JIT.destroy(SortUtil.__ir_digest__())
      end
    end
  end

  describe "different calls" do
    test "call with return type" do
      assert :with == DifferentCalls.return_type_annotation(:with)
    end

    test "call without return type" do
      assert :without == DifferentCalls.no_return_type_annotation(:without)
    end

    test "undefined remote function" do
      line = __ENV__.line

      assert_raise CompileError,
                   ~r"Failed to expand macro Elixir.DifferentCalls.something/1.+function something not found in module DifferentCalls",
                   fn ->
                     defmodule Undefined do
                       use Charms

                       defm without_call_macro(env, i) do
                         DifferentCalls.something(i)
                       end
                     end
                   end
    end

    test "wrong return type remote function" do
      assert_raise CompileError,
                   ~r"mismatch type in invocation: f32 vs. i64",
                   fn ->
                     defmodule WrongReturnType do
                       use Charms

                       defm without_call_macro(env, i) do
                         call DifferentCalls.no_return_type_annotation(env, i) :: f32()
                       end
                     end
                   end
    end
  end

  describe "diagnostic" do
    test "doesn't match function result type" do
      assert_raise CompileError, ~r/type of return operand 0.+/, fn ->
        defmodule DoesNotMatchRetType do
          use Charms
          alias Charms.Term

          defm add(env, i) :: i32() do
            func.return(i)
          end
        end
      end
    end
  end

  test "negate" do
    assert_raise CompileError, ~r/Not an integer type to negate, unsupported type: f32/, fn ->
      defmodule NegateFloatType do
        use Charms
        alias Charms.Term

        defm foo() do
          zero = const 0.0 :: f32()
          !zero
        end
      end
    end
  end

  test "type of" do
    defmodule TypeOf do
      use Charms
      alias Charms.{Term, Pointer}

      defm foo(env, a) :: Term.t() do
        b = const 1 :: i32()
        i_ptr = Pointer.allocate(type_of(b))
        enif_get_int(env, a, i_ptr)
        sum = Pointer.load(type_of(b), i_ptr) + b
        enif_make_int(env, sum)
      end
    end

    assert TypeOf.foo(2) == 3
  end

  test "dump" do
    assert capture_io(fn ->
             defmodule DumpTerm do
               use Charms
               alias Charms.Term

               defm d(env, a) :: Term.t() do
                 dump(a)
                 func.return(a)
               end
             end
           end) =~ ~r"block argument.+i64"
  end

  test "enif type mismatch" do
    assert_raise ArgumentError, ~r/Expected a value of type i32, got f32/, fn ->
      defmodule MismatchEnifType do
        use Charms
        alias Charms.Term

        defm foo(env) :: Term.t() do
          zero = const 0.0 :: f32()
          enif_make_int(env, zero)
        end
      end
    end
  end
end
