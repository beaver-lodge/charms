defmodule MatMulTest do
  use ExUnit.Case, async: true

  @width SqualMatMulKernel.width()

  # Simple CPU Matrix Multiplication for verification
  defp cpu_matmul(a_flat, b_flat, width) do
    a_rows = Enum.chunk_every(a_flat, width)
    b_rows = Enum.chunk_every(b_flat, width)

    # Transpose B for easier dot product calculation
    b_cols =
      b_rows
      |> Enum.zip()
      |> Enum.map(&Tuple.to_list/1)

    for row <- a_rows, col <- b_cols do
      Enum.zip(row, col)
      |> Enum.map(fn {x, y} -> x * y end)
      |> Enum.sum()
    end
  end

  test "compiling and running naive matmul kernel" do
    a = SqualMatMulKernel.random_matrix()
    b = SqualMatMulKernel.random_matrix()

    if Charms.JIT.cuda_available?() do
      # Run GPU Kernel
      gpu_res = SqualMatMulKernel.main(a, b)

      # Run CPU Reference
      cpu_res = cpu_matmul(a, b, @width)

      # Compare results with tolerance
      # (MatMul accumulates floating point errors, so tolerance is slightly higher)
      for {{gpu_val, cpu_val}, i} <- Enum.zip(gpu_res, cpu_res) |> Enum.with_index() do
        assert_in_delta gpu_val,
                        cpu_val,
                        0.01,
                        "Mismatch at index #{i}: GPU=#{gpu_val} vs CPU=#{cpu_val}"
      end
    else
      # Fallback for non-CUDA environments
      case :os.type() do
        {:unix, :linux} ->
          assert_raise ArgumentError, ~r"CUDA path", fn ->
            SqualMatMulKernel.main(a, b)
          end

        _ ->
          assert_raise ArgumentError, ~r"Failed to lookup target", fn ->
            SqualMatMulKernel.main(a, b)
          end
      end
    end
  end

  {m, k, n} = MatMulKernel.dims()
  @m m
  @k k
  @n n

  # CPU Matrix Multiplication for verification
  # A: (M x K), B: (K x N)
  defp cpu_matmul(a_flat, b_flat, _m, k, n) do
    # A has K columns
    a_rows = Enum.chunk_every(a_flat, k)
    # B has N columns
    b_rows = Enum.chunk_every(b_flat, n)

    # Transpose B (resulting in N rows of length K) for easier dot product
    b_cols =
      b_rows
      |> Enum.zip()
      |> Enum.map(&Tuple.to_list/1)

    # Result is M rows x N columns
    for row <- a_rows, col <- b_cols do
      Enum.zip(row, col)
      |> Enum.map(fn {x, y} -> x * y end)
      |> Enum.sum()
    end
  end

  test "compiling and running MxN matmul kernel" do
    a = MatMulKernel.random_list(@m * @k)
    b = MatMulKernel.random_list(@k * @n)

    if Charms.JIT.cuda_available?() do
      # Run GPU Kernel
      gpu_res = MatMulKernel.main(a, b)

      # Run CPU Reference
      cpu_res = cpu_matmul(a, b, @m, @k, @n)

      for {{gpu_val, cpu_val}, i} <- Enum.zip(gpu_res, cpu_res) |> Enum.with_index() do
        assert_in_delta gpu_val,
                        cpu_val,
                        0.01,
                        "Mismatch at index #{i}: GPU=#{gpu_val} vs CPU=#{cpu_val}"
      end
    else
      # Fallback for non-CUDA environments
      case :os.type() do
        {:unix, :linux} ->
          assert_raise ArgumentError, ~r"CUDA path", fn ->
            MatMulKernel.main(a, b)
          end

        _ ->
          assert_raise ArgumentError, ~r"Failed to lookup target", fn ->
            MatMulKernel.main(a, b)
          end
      end
    end
  end
end
