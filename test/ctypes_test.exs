defmodule CtypesTest do
  use ExUnit.Case

  test "calls C sqrt function" do
    # Define a Charms function that binds to the C sqrt function
    defmodule SqrtTest do
      require Logger
      use Charms
      alias Charms.{Term, Pointer}

      defbind sqrt_c(x :: f64()) :: f64()

      defm sqrt(env, x :: Term.t()) :: Term.t() do
        f_ptr = Pointer.allocate(f64())
        enif_get_double(env, x, f_ptr)
        f = Pointer.load(f64(), f_ptr)
        r = sqrt_c(f)
        enif_make_double(env, r)
      end

      def dynamic_libraries() do
        so = Path.join(System.tmp_dir!(), "sqrt.so")

        {_, 0} =
          System.cmd("cc", ["-shared", "-fPIC", "-o", so, "test/support/sqrt.c"])

        [so]
      end
    end

    # Test the sqrt function
    assert_in_delta SqrtTest.sqrt(4.0), 2.0, 0.0001
    assert_in_delta SqrtTest.sqrt(9.0), 3.0, 0.0001
    assert_in_delta SqrtTest.sqrt(2.25), 1.5, 0.0001
  end
end
