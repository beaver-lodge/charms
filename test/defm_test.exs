defmodule DefmTest do
  import ExUnit.CaptureIO
  use ExUnit.Case, async: true

  test "undefined variable" do
    assert_raise CompileError,
                 ~r"undefined variable \"xx\"",
                 fn ->
                   defmodule UndefinedVariable do
                     use Charms
                     alias Charms.Term

                     defm my_function(env, arg1, arg2) :: Term.t() do
                       func.return(xx)
                     end
                   end
                 end
  end

  describe "type inference" do
    test "mismatched local call" do
      assert_raise CompileError,
                   ~r"Incompatible type for argument #0 in call to DefmTest.MissMatchedLocalCall.foo/1: expected i64, but got f32",
                   fn ->
                     defmodule MissMatchedLocalCall do
                       use Charms
                       alias Charms.Term

                       defm foo(arg1) :: Term.t() do
                         func.return(arg1)
                       end

                       defm bar() :: Term.t() do
                         foo(const 1.0 :: f32())
                       end
                     end
                   end
    end

    test "mismatched remove call" do
      assert_raise CompileError,
                   ~r"Incompatible type for argument #0 in call to DefmTest.MissMatchedRemoteCall.foo/1: expected i64, but got f32",
                   fn ->
                     defmodule MissMatchedRemoteCall do
                       use Charms
                       alias Charms.Term

                       defm foo(arg1) :: Term.t() do
                         func.return(arg1)
                       end
                     end

                     defmodule MissMatchedRemoteCaller do
                       use Charms
                       alias Charms.Term

                       defm bar() :: Term.t() do
                         MissMatchedRemoteCall.foo(const 1.0 :: f32())
                       end
                     end
                   end
    end
  end

  test "referenced modules" do
    assert [RefereeMod] = ReferrerMod.referenced_modules()
  end

  test "only env defm is exported" do
    refute function_exported?(RefereeMod, :term_roundtrip1, 0)

    assert_raise ArgumentError,
                 ~r"Cannot invoke RefereeMod.term_roundtrip1/1 from Elixir because it is not exported.",
                 fn ->
                   RefereeMod.term_roundtrip1(1)
                 end

    refute function_exported?(RefereeMod, :term_roundtrip1, 2)

    assert 1 = RefereeMod.term_roundtrip0(1)
    assert 1 = ReferrerMod.term_roundtrip(1)
  end

  describe "exports" do
    test "incorrect arg type" do
      defmodule ExportFailures do
        use Charms
        alias Charms.Term

        # Integer operations
        defm wrong_arg_types(env, a :: i32(), b :: i32()) :: Term.t() do
          c = a + b
          enif_make_int(env, c)
        end

        defm wrong_return_types(env, a :: Term.t(), b :: Term.t()) :: i32() do
          const 0 :: i32()
        end

        defm no_env(a :: Term.t(), b :: Term.t()) :: Term.t() do
          a
        end

        defm no_return(env, a :: Term.t(), b :: Term.t()) do
        end
      end

      assert_raise ArgumentError,
                   ~r"Cannot invoke DefmTest.ExportFailures.wrong_arg_types/2 from Elixir because it is not exported.",
                   fn ->
                     ExportFailures.wrong_arg_types(1, 2)
                   end

      assert_raise ArgumentError,
                   ~r"Cannot invoke DefmTest.ExportFailures.wrong_return_types/2 from Elixir because it is not exported.",
                   fn ->
                     ExportFailures.wrong_return_types(1, 2)
                   end

      assert_raise ArgumentError,
                   ~r"Cannot invoke DefmTest.ExportFailures.no_env/2 from Elixir because it is not exported.",
                   fn ->
                     ExportFailures.no_env(1, 2)
                   end

      assert_raise ArgumentError,
                   ~r"Cannot invoke DefmTest.ExportFailures.no_return/2 from Elixir because it is not exported.",
                   fn ->
                     ExportFailures.no_return(1, 2)
                   end
    end
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

  describe "diagnostic" do
    test "doesn't match function result type" do
      assert_raise CompileError, ~r/type of return operand 0.+/, fn ->
        defmodule DoesNotMatchRetType do
          use Charms

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
        i_ptr = ptr! type_of(b)
        if enif_get_int(env, a, i_ptr) == 0, do: unreachable!()
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
    assert_raise CompileError, ~r/Expected arg#1 of type i32, got f32/, fn ->
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

  test "array index expression" do
    defmodule ArrayIndexing do
      use Charms
      alias Charms.Term

      defm foo(env) :: Term.t() do
        dst_arr = ptr! f64(), 2
        defer free! dst_arr
        val = const 1.1 :: f64()
        src_arr = ptr! f64()
        set! src_arr[0], val
        set! dst_arr[1], src_arr[0]
        enif_make_double(env, dst_arr[1])
      end
    end

    assert 1.1 = ArrayIndexing.foo()
  end

  test "last expression in defm" do
    defmodule LastExpression do
      use Charms
      alias Charms.Term

      defm foo(env) :: Term.t() do
        a = enif_make_int(env, 1)
        enif_make_int(env, 2)
        a
      end

      defm bar(env, arg0 :: Term.t()) :: Term.t() do
        enif_make_int(env, 2)
        arg0
      end
    end

    assert 1 = LastExpression.foo()
    assert 1 = LastExpression.bar(1)
  end

  describe "pointer operations" do
    test "alloc with value" do
      defmodule AllocWithValueAsSize do
        use Charms
        alias Charms.Term

        defm foo(env) :: Term.t() do
          size2 = const 2 :: i64()
          size1 = const 1 :: index()
          dst_arr = ptr! f64(), size2
          src_arr = ptr! f64(), size1

          defer do
            free! dst_arr
            free! src_arr
          end

          val = const 1.1 :: f64()
          set! src_arr[0], val
          set! dst_arr[1], src_arr[0]
          enif_make_double(env, dst_arr[1])
        end
      end

      assert 1.1 = AllocWithValueAsSize.foo()
    end

    test "load llvm ptr" do
      defmodule LoadLLVMPtr do
        use Charms
        alias Charms.Term
        alias Charms.Pointer

        defm foo(env) :: Term.t() do
          arr = ptr! f64()
          val = const 1.1 :: f64()
          set! arr[0], val
          llvm_arr = Pointer.raw(arr)
          a = Pointer.load(f64(), llvm_arr)
          enif_make_double(env, a)
        end
      end

      assert 1.1 = LoadLLVMPtr.foo()
    end
  end
end
