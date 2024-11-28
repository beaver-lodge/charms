defmodule AddTwoInt do
  use Charms, init: false
  alias Charms.{Pointer, Term}

  defm add_or_error_with_cond_br(env, a, b, error) :: Term.t() do
    ptr_a = Pointer.allocate(i32())
    ptr_b = Pointer.allocate(i32())

    arg_err =
      block do
        func.return(error)
      end

    cond_br enif_get_int(env, a, ptr_a) != 0 do
      cond_br 0 != enif_get_int(env, b, ptr_b) do
        a = Pointer.load(i32(), ptr_a)
        b = Pointer.load(i32(), ptr_b)
        sum = value llvm.add(a, b) :: i32()
        term = enif_make_int(env, sum)
        func.return(term)
      else
        ^arg_err
      end
    else
      ^arg_err
    end
  end

  defm add(env, a, b) :: Term.t() do
    ptr_a = Pointer.allocate(i32())
    ptr_b = Pointer.allocate(i32())

    if !enif_get_int(env, a, ptr_a) || !enif_get_int(env, b, ptr_b) do
      enif_make_badarg(env)
    else
      a = Pointer.load(i32(), ptr_a)
      b = Pointer.load(i32(), ptr_b)
      enif_make_int(env, a + b)
    end
  end
end

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

  test "add two integers" do
    assert {:ok, %Charms.JIT{}} = Charms.JIT.init(AddTwoInt, name: :add_int)
    assert {:cached, %Charms.JIT{}} = Charms.JIT.init(AddTwoInt, name: :add_int)
    engine = Charms.JIT.engine(:add_int)
    assert String.starts_with?(AddTwoInt.__ir__(), "ML\xefR")
    assert AddTwoInt.add(1, 2).(engine) == 3
    assert_raise ArgumentError, fn -> AddTwoInt.add(1, "2").(engine) end
    assert AddTwoInt.add_or_error_with_cond_br(1, 2, :arg_err).(engine) == 3
    assert AddTwoInt.add_or_error_with_cond_br(1, "", :arg_err).(engine) == :arg_err
    assert :ok = Charms.JIT.destroy(:add_int)
  end

  test "quick sort" do
    assert_raise ArgumentError, "list expected", fn -> ENIFQuickSort.sort(:what) end

    arr = [5, 4, 3, 2, 1]
    assert ENIFQuickSort.sort(arr) == Enum.sort(arr)

    assert {:cached, %Charms.JIT{}} =
             Charms.JIT.init(ENIFQuickSort, name: ENIFQuickSort.__ir_digest__())

    for i <- 0..1000 do
      arr = 0..i |> Enum.shuffle()
      assert ENIFTimSort.sort(arr) == Enum.sort(arr)
      assert ENIFQuickSort.sort(arr) == Enum.sort(arr)
      assert ENIFMergeSort.sort(arr) == Enum.sort(arr)
    end

    assert :ok = Charms.JIT.destroy(ENIFQuickSort.__ir_digest__())
    assert :ok = Charms.JIT.destroy(ENIFMergeSort.__ir_digest__())
    assert :ok = Charms.JIT.destroy(ENIFTimSort.__ir_digest__())
    assert :noop = Charms.JIT.destroy(SortUtil.__ir_digest__())
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
end
