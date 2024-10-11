defmodule POCTest do
  alias Charms.Defm.Expander, as: POC
  alias Beaver.MLIR
  use ExUnit.Case, async: true

  defp env(string) do
    quoted = Code.string_to_quoted!(string, columns: true)
    {_, _, env} = POC.expand(quoted, "example.exs")
    env
  end

  defp state(string) do
    quoted = Code.string_to_quoted!(string, columns: true)
    {_, state, _} = POC.expand(quoted, "example.exs")
    state
  end

  defp compile(quoted) do
    {_, state, _} = POC.expand(quoted, "example.exs")
    state.mlir.mod
  end

  defp vars(string), do: Enum.sort(state(string).vars)
  defp locals(string), do: Enum.sort(state(string).locals)
  defp remotes(string), do: Enum.sort(state(string).remotes)

  test "locals" do
    assert locals("foo()") == [foo: 0]
    one = "value(arith.constant(value: Beaver.MLIR.Attribute.integer(i32(), 1)) :: i32())"
    two = "value(arith.constant(value: Beaver.MLIR.Attribute.integer(i32(), 2)) :: i32())"

    assert locals("import Charms.Defm; foo(#{one}, #{two})") == [
             foo: 2,
             i32: 0,
             i32: 0,
             i32: 0,
             i32: 0
           ]

    assert locals("import Charms.Defm; foo(#{one}, call(bar(#{two})::i32()))") == [
             bar: 1,
             foo: 2,
             i32: 0,
             i32: 0,
             i32: 0,
             i32: 0,
             i32: 0
           ]
  end

  # This test shows we can track locals inside containers,
  # as an example of traversal.
  test "containers" do
    assert locals("[foo()]") == [foo: 0]
    assert locals("[foo() | bar()]") == [bar: 0, foo: 0]
    # assert locals("[foo() | bar(1, 2)]") == [bar: 2, foo: 0]
    assert locals("{foo(), bar()}") == [bar: 0, foo: 0]
    # assert locals("{foo(), bar(1, 2)}") == [bar: 2, foo: 0]
    # assert locals("{foo(), bar(1, 2), baz(3)}") == [bar: 2, baz: 1, foo: 0]
    # assert locals("%{foo() => bar(1, 2)}") == [bar: 2, foo: 0]
  end

  # This test shows we can track locals inside unquotes.
  test "quote" do
    assert locals("quote do: foo()") == []
    assert locals("quote do: unquote(foo())") == [foo: 0]
    assert locals("quote line: line(), do: foo()") == [line: 0]
  end

  test "vars" do
    assert vars("var = 123") == [var: nil]

    assert catch_error(vars("^var = 123") == []) == %Beaver.EnvNotFoundError{
             message: "no valid Beaver.MLIR.Block in the environment"
           }
  end

  test "remotes" do
    assert remotes(":lists.flatten([])") == [{:lists, :flatten, 1}]
    assert remotes("List.flatten([])") == [{List, :flatten, 1}]
  end

  describe "defmodule" do
    test "requires module" do
      env = env("defmodule Foo, do: :ok")
      # assert Macro.Env.required?(env, Foo)
      assert env.context_modules == [Foo]
      refute env.module

      env = env("defmodule Foo.Bar, do: :ok")
      # assert Macro.Env.required?(env, Foo.Bar)
      assert env.context_modules == [Foo.Bar]
      refute env.module
    end

    test "alias module" do
      assert {Foo.Bar, :flatten, 1} in remotes(
               "defmodule Foo do defmodule Bar do Bar.flatten([]) end end"
             )

      assert {Bar, :flatten, 1} in remotes(
               "defmodule Foo do defmodule Elixir.Bar do Bar.flatten([]) end end"
             )
    end
  end

  describe "alias/2" do
    test "defines aliases" do
      assert remotes("alias List, as: L; L.flatten([])") == [{List, :flatten, 1}]
    end
  end

  describe "require/2" do
    defmacro discard_require(_discard), do: :ok

    test "requires modules" do
      # The macro discards the content, so if the module is required,
      # the macro is invoked and contents are discarded
      assert locals("POCTest.discard_require(foo())") == [foo: 0]
      assert locals("require POCTest; POCTest.discard_require(foo())") == []
    end
  end

  describe "import/2" do
    defmacro discard_import(_discard), do: :ok

    test "imports modules" do
      # The macro discards the content, so if the module is imported,
      # the macro is invoked and contents are discarded
      assert %FunctionClauseError{
               module: MLIR.Operation.Changeset,
               function: :add_argument,
               arity: 2,
               kind: nil,
               args: nil,
               clauses: nil
             } = catch_error(remotes("discard_import(foo())"))

      # assert locals("discard_import(foo())") == [discard_import: 1, foo: 0]
      assert locals("import POCTest; discard_import(foo())") == []
    end
  end

  describe "def" do
    test "return original arg" do
      quote do
        defmodule ReturnPassedArg do
          import Charms.Defm
          alias Charms.Env
          alias Charms.Term
          def foo(a :: Term.t()) :: Term.t(), do: func.return(a)

          def bar(env :: Env.t(), a :: Term.t()) :: Term.t() do
            b = call foo(a) :: Term.t()
            func.return(b)
          end
        end
      end
      |> compile()
      |> tap(fn ir -> assert to_string(ir) =~ "@Elixir.ReturnPassedArg" end)
      |> MLIR.Operation.verify!()
      |> tap(fn m ->
        {_key, %Charms.JIT{}} = Charms.JIT.init(m, name: :return_this)
        engine = Charms.JIT.engine(:return_this)
        assert Charms.JIT.invoke(engine, {ReturnPassedArg, :bar, [:identical]}) == :identical
        assert :ok = Charms.JIT.destroy(:return_this)
      end)
    end

    test "intrinsic not found" do
      quote do
        defmodule InvalidRemoteCall do
          import Charms.Defm
          alias Charms.Term

          def foo(a :: Term.t()) :: Term.t() do
            Foo.bar(a)
            func.return(a)
          end
        end
      end
      |> compile()
      |> tap(fn m ->
        assert to_string(m) =~ "Unknown intrinsic: Foo.bar/1"
      end)
      |> MLIR.Operation.verify!()
    end

    test "op not found" do
      assert catch_error(
               quote do
                 defmodule ReturnPassedArg do
                   import Charms.Defm
                   alias Charms.Term
                   def foo(a :: Term.t()) :: Term.t(), do: cf.ar(a)
                 end
               end
               |> compile()
             ) == %ArgumentError{
               message: "Unknown MLIR operation to create: cf.ar, did you mean: cf.br"
             }
    end

    test "no return" do
      err = %ArgumentError{
        message: "func.call @Elixir.InvalidLocalCall.dummy doesn't return a value"
      }

      quote do
        defmodule InvalidLocalCall do
          import Charms.Defm
          alias Charms.Term
          def foo(a :: Term.t()) :: Term.t(), do: func.return(dummy(a))
        end
      end
      |> then(&assert catch_error(compile(&1)) == err)
    end
  end
end
