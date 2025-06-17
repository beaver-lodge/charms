defmodule DefmstructTest do
  use ExUnit.Case, async: true

  test "define and access" do
    defmodule NaiveStructDefined do
      use Charms
      alias Charms.Term

      defmstruct a: i32(), b: i64(), c: Term.t()

      defm new(env, c :: Term.t()) :: Term.t() do
        a = const 1 :: i32()
        b = const 2 :: i64()
        struct = %__MODULE__{a: a, b: b, c: c}
        func.return(struct.c)
      end
    end

    assert 2 = NaiveStructDefined.new(2)
  end

  test "field not found" do
    assert_raise CompileError, ~r/field :c not found, available fields: \[:a, :b\]/, fn ->
      defmodule StructFieldNotFound do
        use Charms

        defmstruct a: i32(), b: i64()

        defm new(env) :: i32() do
          struct = %__MODULE__{c: const(1 :: i32())}
        end
      end
    end
  end

  test "helper t() for struct type" do
    defmodule THelper do
      use Charms
      alias Charms.Term

      defmstruct a: i32(), b: i64(), c: Term.t()

      defm id(s :: __MODULE__.t()) :: __MODULE__.t() do
        func.return(s)
      end

      defm new(env, c :: Term.t()) :: Term.t() do
        a = const 1 :: i32()
        b = const 2 :: i64()
        struct = %__MODULE__{a: a, b: b, c: c}
        struct = id(struct)
        func.return(struct.c)
      end
    end

    assert 2 = THelper.new(2)
  end

  test "reference helper t() in another model" do
    defmodule DefiningTHelper do
      use Charms
      alias Charms.Term

      defmstruct a: i32(), b: i64(), c: Term.t()

      defm id(s :: __MODULE__.t()) :: __MODULE__.t() do
        func.return(s)
      end

      defm new(env, c :: Term.t()) :: Term.t() do
        a = const 1 :: i32()
        b = const 2 :: i64()
        struct = %__MODULE__{a: a, b: b, c: c}
        struct = id(struct)
        func.return(struct.c)
      end
    end

    defmodule ReferringTHelper do
      use Charms
      alias Charms.Term

      defm id(s :: DefiningTHelper.t()) :: DefiningTHelper.t() do
        DefiningTHelper.id(s)
      end

      defm new(env, c :: Term.t()) :: Term.t() do
        a = const 1 :: i32()
        b = const 2 :: i64()
        struct = %DefiningTHelper{a: a, b: b, c: c}
        struct = id(struct)
        func.return(struct.c)
      end
    end

    assert 2 = ReferringTHelper.new(2)
  end

  test "nested struct definition and access" do
    defmodule OuterStruct do
      use Charms
      alias Charms.Term

      defmodule InnerStruct do
        use Charms
        defmstruct value: i32()
      end

      defmstruct inner: InnerStruct.t()

      defm new(env) :: Term.t() do
        inner = %InnerStruct{value: const(1 :: i32())}
        outer = %__MODULE__{inner: inner}
        func.return(enif_make_int(env, outer.inner.value))
      end
    end

    assert 1 = OuterStruct.new()
  end

  test "create struct of field with wrong type raises error" do
    assert_raise CompileError, ~r/type mismatch/i, fn ->
      defmodule StructTypeMismatch do
        use Charms
        alias Charms.Term

        defmstruct a: i32(), b: i64()

        defm update(env, s :: __MODULE__.t()) :: __MODULE__.t() do
          updated = %__MODULE__{b: const(1 :: i32())}
          func.return(updated)
        end
      end
    end
  end

  test "update struct fields" do
    defmodule StructUpdate do
      use Charms
      alias Charms.Term

      defmstruct a: i32(), b: i64()

      defm update(env, s :: __MODULE__.t()) :: __MODULE__.t() do
        %__MODULE__{s | a: const(1 :: i32())}
      end

      defm test_update(env) :: Term.t() do
        s = %__MODULE__{a: const(0 :: i32()), b: const(2 :: i64())}
        s = update(env, s)
        enif_make_int(env, s.a)
      end
    end

    assert 1 = StructUpdate.test_update()
  end

  test "define and access pointer field" do
    defmodule PtrFieldStruct do
      use Charms
      alias Charms.{Term, Pointer}

      defmstruct a: i32(), b: i64(), c: Pointer.t()

      defm new(env) :: Term.t() do
        str = "aaa"
        c = Pointer.raw(str)
        a = const 1 :: i32()
        b = const 2 :: i64()
        s = %__MODULE__{a: a, b: b, c: c}
        func.return(enif_make_atom(env, s.c))
      end
    end

    assert :aaa = PtrFieldStruct.new()
  end
end
