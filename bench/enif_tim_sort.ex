defmodule ENIFTimSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm insertion_sort(arr :: Pointer.t(Term.t()), left :: i32(), right :: i32()) do
    start_i = left + 1
    start = arr + start_i
    n = right - start_i + 1

    for_loop {temp, i} <- {start, n} do
      i = value index.casts(i) :: i32()
      i = i + start_i
      j_ptr = tmp! i32()
      set! j_ptr[0], i - 1

      while(j_ptr[0] >= left && arr[j_ptr[0]] > temp) do
        j = j_ptr[0]
        set! arr[j + 1], arr[j]
        set! j_ptr[0], j - 1
      end

      j = j_ptr[0]
      set! arr[j + 1], temp
    end
  end

  defm tim_sort(arr :: Pointer.t(Term.t()), n :: i32()) do
    run = const 32 :: i32()
    i_ptr = tmp! i32()
    set! i_ptr[0], 0

    while i_ptr[0] < n do
      i = i_ptr[0]
      min = value arith.minsi(i + run - 1, n - 1) :: i32()
      insertion_sort(arr, i, min)
      set! i_ptr[0], i + run
    end

    size_ptr = tmp! i32()
    set! size_ptr[0], run

    while size_ptr[0] < n do
      size = size_ptr[0]

      left_ptr = tmp! i32()
      set! left_ptr[0], 0

      while left_ptr[0] < n do
        left = left_ptr[0]
        mid = left + size - 1
        right = op arith.minsi(left + 2 * size - 1, n - 1) :: i32()
        right = result_at(right, 0)

        if mid < right do
          SortUtil.merge(arr, left, mid, right)
        end

        set! left_ptr[0], left + 2 * size
      end

      set! size_ptr[0], size * 2
    end
  end

  @err %ArgumentError{message: "list expected"}
  defm sort(env, list) :: Term.t() do
    len_ptr = tmp! i32()

    if enif_get_list_length(env, list, len_ptr) != 0 do
      movable_list_ptr = tmp! Term.t()
      set! movable_list_ptr[0], list
      len = len_ptr[0]
      arr = new! Term.t(), len
      SortUtil.copy_terms(env, movable_list_ptr, arr)
      tim_sort(arr, len)
      defer free! arr
      enif_make_list_from_array(env, arr, len)
    else
      enif_raise_exception(env, @err)
    end
  end
end
