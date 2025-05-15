defmodule SqrtWrapper do
  @moduledoc false
  require Logger
  use Charms
  alias Charms.{Term, Pointer}

  defbind sqrt_c(x :: f64()) :: f64()

  defm sqrt(env, x :: Term.t()) :: Term.t() do
    f_ptr = Pointer.allocate(f64())
    enif_get_double(env, x, f_ptr)
    f = f_ptr[0]
    r = sqrt_c(f)
    enif_make_double(env, r)
  end

  @csrc "test/support/sqrt.c"
  @external_resource @csrc
  @so Path.join(Mix.Project.app_path(), "sqrt.so")
  {_, 0} = System.cmd("cc", ["-shared", "-fPIC", "-o", @so, @csrc])

  def dynamic_libraries() do
    [@so]
  end
end
