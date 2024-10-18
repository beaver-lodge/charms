defmodule DefmTest do
  use ExUnit.Case, async: true

  test "referenced modules" do
    assert [RefereeMod] == ReferrerMod.referenced_modules()
  end

  test "invalid return of absent alias" do
    assert_raise ArgumentError, "invalid return type", fn ->
      defmodule InvalidRet do
        use Charms

        defm my_function(env, arg1, arg2) :: Invalid.t() do
          func.return(arg2)
        end
      end
    end
  end

  test "invalid arg of absent alias" do
    assert_raise ArgumentError, "invalid argument type #2", fn ->
      defmodule InvalidRet do
        use Charms
        alias Charms.Term

        defm my_function(env, arg1 :: Pointer.t(), arg2) :: Term.t() do
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
    defmodule AddTwoInt do
      use Charms, init: false
      alias Charms.{Pointer, Term}

      defm add(env, a, b, error) :: Term.t() do
        ptr_a = Pointer.allocate(i64())
        ptr_b = Pointer.allocate(i64())

        arg_err =
          block do
            func.return(error)
          end

        cond_br enif_get_int64(env, a, ptr_a) != 0 do
          cond_br 0 != enif_get_int64(env, b, ptr_b) do
            a = Pointer.load(i64(), ptr_a)
            b = Pointer.load(i64(), ptr_b)
            sum = value llvm.add(a, b) :: i64()
            term = enif_make_int64(env, sum)
            func.return(term)
          else
            ^arg_err
          end
        else
          ^arg_err
        end
      end
    end

    assert {:ok, %Charms.JIT{}} = Charms.JIT.init(AddTwoInt, name: :add_int)
    assert {:cached, %Charms.JIT{}} = Charms.JIT.init(AddTwoInt, name: :add_int)
    engine = Charms.JIT.engine(:add_int)
    assert String.starts_with?(AddTwoInt.__ir__(), "ML\xefR")
    assert AddTwoInt.add(1, 2, :arg_err).(engine) == 3
    assert AddTwoInt.add(1, "", :arg_err).(engine) == :arg_err
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
      assert :with == DifferentCalls.with_return_type(:with)
    end

    test "call without return type" do
      assert :without == DifferentCalls.without_return_type(:without)
    end

    test "undefined remote function" do
      assert_raise ArgumentError, "function something not found in module DifferentCalls", fn ->
        defmodule Undefined do
          use Charms

          defm without_call_macro(env, i) do
            DifferentCalls.something(i)
          end
        end
      end
    end

    test "wrong return type remote function" do
      assert_raise ArgumentError,
                   "function without_return_type has a different return type f32",
                   fn ->
                     defmodule WrongReturnType do
                       use Charms

                       defm without_call_macro(env, i) do
                         call DifferentCalls.without_return_type(env, i) :: f32()
                       end
                     end
                   end
    end
  end
end
