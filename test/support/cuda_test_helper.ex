defmodule CUDATestHelper do
  use ExUnit.Case
  @moduledoc false
  def run_cuda_test(gpu_test_fn, opts \\ []) do
    if Charms.JIT.cuda_available?() do
      ref_impl = Keyword.get(opts, :ref_impl, nil)
      tole = Keyword.get(opts, :tole, 0.0001)
      error_message = Keyword.get(opts, :error_message, nil)
      # Run GPU Kernel
      gpu_res = gpu_test_fn.()

      if ref_impl do
        # Run CPU Reference
        cpu_res = ref_impl.()

        # Validate lengths match
        assert length(gpu_res) == length(cpu_res)

        # Compare results with tole
        for {{gpu_val, cpu_val}, i} <- Enum.zip(gpu_res, cpu_res) |> Enum.with_index() do
          message = error_message || "Mismatch at index #{i}: GPU=#{gpu_val} vs CPU=#{cpu_val}"
          assert_in_delta gpu_val, cpu_val, tole, message
        end
      else
        gpu_res
      end
    else
      # Fallback for non-CUDA environments
      case :os.type() do
        {:unix, :linux} ->
          assert_raise(ArgumentError, ~r"CUDA path", fn ->
            gpu_test_fn.()
          end)

        _ ->
          assert_raise(ArgumentError, ~r"Failed to lookup target", fn ->
            gpu_test_fn.()
          end)
      end
    end
  end
end
