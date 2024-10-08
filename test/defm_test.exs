defmodule DefmTest do
  use ExUnit.Case, async: true

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

        cond_br(enif_get_int64(env, a, ptr_a) != 0) do
          cond_br(0 != enif_get_int64(env, b, ptr_b)) do
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

    {:ok, %Charms.JIT{}} = Charms.JIT.init(AddTwoInt, name: :add_int)
    engine = Charms.JIT.engine(:add_int)
    assert String.starts_with?(AddTwoInt.__ir__(), "ML\xefR")
    assert AddTwoInt.add(1, 2, :arg_err).(engine) == 3
    assert AddTwoInt.add(1, "", :arg_err).(engine) == :arg_err
    assert :ok = Charms.JIT.destroy(:add_int)

    Charms.JIT.init(AddTwoInt)
    assert AddTwoInt.add(1, 2, :arg_err) == 3
    assert :ok = Charms.JIT.destroy(AddTwoInt)
  end

  test "quick sort" do
    assert_raise ArgumentError, "list expected", fn -> ENIFQuickSort.sort(:what) end

    arr = [5, 4, 3, 2, 1]
    assert ENIFQuickSort.sort(arr) == Enum.sort(arr)

    for i <- 0..1000 do
      arr = 0..i |> Enum.shuffle()
      assert ENIFTimSort.sort(arr) == Enum.sort(arr)
      assert ENIFQuickSort.sort(arr) == Enum.sort(arr)
      assert ENIFMergeSort.sort(arr) == Enum.sort(arr)
    end

    assert :ok = Charms.JIT.destroy(ENIFQuickSort)
    assert :noop = Charms.JIT.destroy(ENIFMergeSort)
    assert :ok = Charms.JIT.destroy(ENIFTimSort)
  end
end
