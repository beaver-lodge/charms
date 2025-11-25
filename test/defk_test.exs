defmodule DefkTest do
  use ExUnit.Case, async: true
  import CUDATestHelper

  test "compiling a simple vector add kernel" do
    a = VecAddKernel.random_floats()
    b = VecAddKernel.random_floats()

    run_cuda_test(fn ->
      res = VecAddKernel.main(a, b)

      for {{x, y, z}, i} <- Enum.zip([a, b, res]) |> Enum.with_index() do
        assert_in_delta x + y, z, 0.0001, "z[#{i}] #{x} + #{y} != #{z}"
      end
    end)
  end
end
