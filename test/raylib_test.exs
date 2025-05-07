defmodule RaylibTest do
  use ExUnit.Case

  test "calls raylib GetScreenWidth function" do
    defmodule ScreenWidthTest do
      use Charms

      defbind init_window_wrapper() :: i32()

      defm init_window(env) :: i32() do
        init_window_wrapper()
      end

      def dynamic_libraries() do
        so = Path.join(System.tmp_dir!(), "raylib_test.so")

        {_, 0} =
          System.cmd("cc", [
            "-shared",
            "-fPIC",
            "-o",
            so,
            "test/support/raylib_test.c",
            "-I/opt/homebrew/include",
            "-L/opt/homebrew/lib",
            "-lraylib"
          ])

        [so]
      end
    end

    assert ScreenWidthTest.init_window() == 800
  end
end
