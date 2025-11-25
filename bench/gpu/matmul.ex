defmodule MatMulKernel.Index1D do
  @moduledoc false
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  # Matrix Dimensions
  # A: (M x K), B: (K x N), C: (M x N)
  @m 64
  # Inner dimension (must match cols of A and rows of B)
  @k 128
  @n 32

  @size_a @m * @k
  @size_b @k * @n
  @size_c @m * @n

  @block_size 1024

  # Kernel: C = A * B
  # A is m*k, B is k*n, C is m*n
  defk matmul(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    # Global thread index (maps to C matrix)
    idx = GPU.block_id() * @block_size + GPU.thread_id()

    # Map linear index to matrix coordinates (row, col) of C (m x n)
    row = idx / @n
    col = rem(idx, @n)

    if idx < @size_c do
      # Accumulator for the dot product
      sum_ptr = tmp! f32()
      set! sum_ptr[0], 0.0

      # Iterator k
      k_ptr = tmp! i32()
      set! k_ptr[0], 0

      # Loop over the shared dimension K
      while k_ptr[0] < @k do
        k = k_ptr[0]

        # A[row * k_dim + k]
        val_a = a[row * @k + k]
        # B[k * n_dim + col]
        val_b = b[k * @n + col]

        set! sum_ptr[0], sum_ptr[0] + val_a * val_b
        set! k_ptr[0], k + 1
      end

      # Store result in C
      set! c[idx], sum_ptr[0]
    end
  end

  @grid_size ceil(@size_c / @block_size)

  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size_a = Term.to_i64!(env, @size_a)
    size_b = Term.to_i64!(env, @size_b)
    size_c = Term.to_i64!(env, @size_c)

    # 1. Allocate Device Memory
    a = GPU.allocate(f32(), size_a)
    b = GPU.allocate(f32(), size_b)
    c = GPU.allocate(f32(), size_c)

    # 2. Allocate Host Memory (Dedicated buffers as requested)
    buffer_a = GPU.allocate(f32(), size_a, host_shared: true)
    buffer_b = GPU.allocate(f32(), size_b, host_shared: true)
    buffer_c = GPU.allocate(f32(), size_c, host_shared: true)

    # 3. Cleanup
    defer GPU.await([
            GPU.dealloc(a),
            GPU.dealloc(b),
            GPU.dealloc(c),
            GPU.dealloc(buffer_a),
            GPU.dealloc(buffer_b),
            GPU.dealloc(buffer_c)
          ])

    # 4. Copy Input (Host -> Buffer -> Device)
    movable_list_ptr = tmp! Term.t()

    # Copy A
    set! movable_list_ptr[0], l_a
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer_a)
    GPU.memcpy(a, buffer_a) |> GPU.await()

    # Copy B
    set! movable_list_ptr[0], l_b
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer_b)
    GPU.memcpy(b, buffer_b) |> GPU.await()

    # 5. Launch Kernel
    launch! matmul(a, b, c), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)

    # 6. Copy Output (Device -> Buffer -> Host)
    GPU.memcpy(buffer_c, c) |> GPU.await()

    # 7. Construct Elixir List from Buffer C
    arr = new! Term.t(), size_c
    defer free! arr

    for_loop {element, i} <- {buffer_c, size_c} do
      element = value arith.extf(element) :: f64()
      set! arr[i], enif_make_double(env, element)
    end

    size_c_i32 = value arith.trunci(size_c) :: i32()
    enif_make_list_from_array(env, arr, size_c_i32)
  end

  def random_list(size) do
    Enum.map(1..size, fn _ -> :rand.uniform() end)
  end

  def dims, do: {@m, @k, @n}
end

defmodule MatMulKernel.Square.Index1D do
  @moduledoc false
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  # Matrix Dimensions (N x N)
  # 64 x 64 matrix = 4,096 elements
  @width 64
  @size @width * @width
  @block_size 1024

  # Kernel: C = A * B
  # Uses 1D thread indexing mapped to 2D matrix coordinates
  defk matmul(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    # Global thread index
    idx = GPU.block_id() * @block_size + GPU.thread_id()

    # Map linear index to matrix coordinates (row, col)
    # Note: width must be constant or passed as arg. Using module attr for simplicity.
    row = idx / @width
    col = rem(idx, @width)

    if idx < @size do
      # Accumulator for the dot product
      sum_ptr = tmp! f32()
      set! sum_ptr[0], 0.0

      # Iterator k
      k_ptr = tmp! i32()
      set! k_ptr[0], 0

      while k_ptr[0] < @width do
        k = k_ptr[0]

        # A[row * width + k]
        val_a = a[row * @width + k]
        # B[k * width + col]
        val_b = b[k * @width + col]

        set! sum_ptr[0], sum_ptr[0] + val_a * val_b
        set! k_ptr[0], k + 1
      end

      # Store result in C
      set! c[idx], sum_ptr[0]
    end
  end

  @grid_size ceil(@size / @block_size)

  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size = Term.to_i64!(env, @size)

    # 1. Allocate Device Memory
    a = GPU.allocate(f32(), size)
    b = GPU.allocate(f32(), size)
    c = GPU.allocate(f32(), size)
    buffer = GPU.allocate(f32(), size, host_shared: true)

    # 2. Cleanup
    defer GPU.await([
            GPU.dealloc(a),
            GPU.dealloc(b),
            GPU.dealloc(c),
            GPU.dealloc(buffer)
          ])

    # 3. Copy Input (Host -> Device)
    movable_list_ptr = tmp! Term.t()

    # Copy A
    set! movable_list_ptr[0], l_a
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(a, buffer) |> GPU.await()

    # Copy B
    set! movable_list_ptr[0], l_b
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer)
    GPU.memcpy(b, buffer) |> GPU.await()

    # 4. Launch Kernel
    # We launch enough threads to cover the N*N matrix
    launch! matmul(a, b, c), Term.to_i64!(env, @grid_size), Term.to_i64!(env, @block_size)

    # 5. Copy Output (Device -> Host)
    GPU.memcpy(buffer, c) |> GPU.await()

    # 6. Construct Elixir List from Buffer
    arr = new! Term.t(), size
    defer free! arr

    for_loop {element, i} <- {buffer, size} do
      element = value arith.extf(element) :: f64()
      set! arr[i], enif_make_double(env, element)
    end

    size_i32 = value arith.trunci(size) :: i32()
    enif_make_list_from_array(env, arr, size_i32)
  end

  # Helper to generate data for the test
  def random_matrix() do
    Enum.map(1..@size, fn _ -> :rand.uniform() end)
  end

  def width, do: @width
end

defmodule MatMulKernel.Index2D do
  @moduledoc false
  use Charms
  alias Charms.{Term, Pointer}
  alias Charms.GPU

  # Matrix Dimensions (M x N)
  @m 64
  @k 128
  @n 32

  @size_a @m * @k
  @size_b @k * @n
  @size_c @m * @n

  # 2D block configuration: 16x16 threads per block
  @block_dim_x 16
  @block_dim_y 16

  # Grid dimensions for 2D indexing
  @grid_dim_x ceil(@m / @block_dim_x)
  @grid_dim_y ceil(@n / @block_dim_y)

  # Kernel: C = A * B using 2D indexing
  # A is m*k, B is k*n, C is m*n
  defk matmul(a :: Pointer.t(f32()), b :: Pointer.t(f32()), c :: Pointer.t(f32())) do
    # 2D thread coordinates within block
    thread_x = GPU.thread_id(:x)
    thread_y = GPU.thread_id(:y)

    # 2D block coordinates within grid
    block_x = GPU.block_id(:x)
    block_y = GPU.block_id(:y)

    # Global 2D coordinates for this thread in C matrix
    row = block_x * @block_dim_x + thread_x
    col = block_y * @block_dim_y + thread_y

    if row < @m && col < @n do
      # Accumulator for the dot product
      sum_ptr = tmp! f32()
      set! sum_ptr[0], 0.0

      # Iterator k
      k_ptr = tmp! i32()
      set! k_ptr[0], 0

      # Loop over the shared dimension K
      while k_ptr[0] < @k do
        k = k_ptr[0]

        # A[row * k_dim + k]
        val_a = a[row * @k + k]
        # B[k * n_dim + col]
        val_b = b[k * @n + col]

        set! sum_ptr[0], sum_ptr[0] + val_a * val_b
        set! k_ptr[0], k + 1
      end

      # Store result in C
      c_idx = row * @n + col
      set! c[c_idx], sum_ptr[0]
    end
  end

  defm main(env, l_a :: Term.t(), l_b :: Term.t()) :: Term.t() do
    size_a = Term.to_i64!(env, @size_a)
    size_b = Term.to_i64!(env, @size_b)
    size_c = Term.to_i64!(env, @size_c)

    # 1. Allocate Device Memory
    a = GPU.allocate(f32(), size_a)
    b = GPU.allocate(f32(), size_b)
    c = GPU.allocate(f32(), size_c)

    # 2. Allocate Host Memory (Dedicated buffers as requested)
    buffer_a = GPU.allocate(f32(), size_a, host_shared: true)
    buffer_b = GPU.allocate(f32(), size_b, host_shared: true)
    buffer_c = GPU.allocate(f32(), size_c, host_shared: true)

    # 3. Cleanup
    defer GPU.await([
            GPU.dealloc(a),
            GPU.dealloc(b),
            GPU.dealloc(c),
            GPU.dealloc(buffer_a),
            GPU.dealloc(buffer_b),
            GPU.dealloc(buffer_c)
          ])

    # 4. Copy Input (Host -> Buffer -> Device)
    movable_list_ptr = tmp! Term.t()

    # Copy A
    set! movable_list_ptr[0], l_a
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer_a)
    GPU.memcpy(a, buffer_a) |> GPU.await()

    # Copy B
    set! movable_list_ptr[0], l_b
    KernelUtil.copy_terms_as_floats(env, movable_list_ptr, buffer_b)
    GPU.memcpy(b, buffer_b) |> GPU.await()

    # 5. Launch Kernel with 2D grid configuration
    # Use extended launch with 2D grid and block dimensions
    grid_dims = [Term.to_i64!(env, @grid_dim_x), Term.to_i64!(env, @grid_dim_y)]
    block_dims = [Term.to_i64!(env, @block_dim_x), Term.to_i64!(env, @block_dim_y)]
    launch! matmul(a, b, c), grid_dims, block_dims

    # 6. Copy Output (Device -> Buffer -> Host)
    GPU.memcpy(buffer_c, c) |> GPU.await()

    # 7. Construct Elixir List from Buffer C
    arr = new! Term.t(), size_c
    defer free! arr

    for_loop {element, i} <- {buffer_c, size_c} do
      element = value arith.extf(element) :: f64()
      set! arr[i], enif_make_double(env, element)
    end

    size_c_i32 = value arith.trunci(size_c) :: i32()
    enif_make_list_from_array(env, arr, size_c_i32)
  end

  # Helper to generate data for the test
  def random_list(size) do
    Enum.map(1..size, fn _ -> :rand.uniform() end)
  end

  def dims, do: {@m, @k, @n}
end
