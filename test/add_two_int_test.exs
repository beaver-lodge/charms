defmodule AddTwoIntTest do
  use ExUnit.Case, async: true

  test "add two integers" do
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
            a = ptr_a[0]
            b = ptr_b[0]
            sum = value llvm.add(a, b) :: i32()
            sum = sum / 1
            sum = sum + 1 - 1
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
          a = ptr_a[0]
          b = ptr_b[0]
          enif_make_int(env, a + b)
        end
      end
    end

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
end
