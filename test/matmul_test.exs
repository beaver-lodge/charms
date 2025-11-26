defmodule MatMulTest do
  use ExUnit.Case, async: true
  import CUDATestHelper

  @width MatMulKernel.Square.Index1D.width()

  # Simple CPU Matrix Multiplication for verification
  defp cpu_matmul(a_flat, b_flat, width) do
    a_rows = Enum.chunk_every(a_flat, width)
    b_rows = Enum.chunk_every(b_flat, width)

    # Transpose B for easier dot product calculation
    b_cols = b_rows |> Enum.zip() |> Enum.map(&Tuple.to_list/1)

    for row <- a_rows, col <- b_cols do
      Enum.zip_reduce(row, col, 0, fn x, y, acc -> x * y + acc end)
    end
  end

  test "compiling and running naive matmul kernel" do
    a = MatMulKernel.Square.Index1D.random_matrix()
    b = MatMulKernel.Square.Index1D.random_matrix()

    run_cuda_test(
      fn -> MatMulKernel.Square.Index1D.main(a, b) end,
      ref_impl: fn -> cpu_matmul(a, b, @width) end
    )
  end

  {m, k, n} = MatMulKernel.Index1D.dims()
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
      Enum.zip_reduce(row, col, 0, fn x, y, acc -> x * y + acc end)
    end
  end

  test "compiling and running MxN matmul kernel" do
    a = MatMulKernel.Index1D.random_list(@m * @k)
    b = MatMulKernel.Index1D.random_list(@k * @n)

    run_cuda_test(
      fn -> MatMulKernel.Index1D.main(a, b) end,
      ref_impl: fn -> cpu_matmul(a, b, @m, @k, @n) end,
      tole: 0.01
    )
  end

  {m_2d, k_2d, n_2d} = MatMulKernel.Index2D.dims()
  @m_2d m_2d
  @k_2d k_2d
  @n_2d n_2d

  test "compiling and running 2D indexing matmul kernel" do
    a = MatMulKernel.Index2D.random_list(@m_2d * @k_2d)
    b = MatMulKernel.Index2D.random_list(@k_2d * @n_2d)

    run_cuda_test(
      fn -> MatMulKernel.Index2D.main(a, b) end,
      ref_impl: fn -> cpu_matmul(a, b, @m_2d, @k_2d, @n_2d) end,
      tole: 0.01
    )
  end
end
