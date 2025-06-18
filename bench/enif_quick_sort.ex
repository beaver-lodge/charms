defmodule ENIFQuickSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm swap(a :: Pointer.t(Term.t()), b :: Pointer.t(Term.t())) do
    val_a = a[0]
    val_b = b[0]
    set! a[0], val_b
    set! b[0], val_a
  end

  defm partition(arr :: Pointer.t(Term.t()), low :: i32(), high :: i32()) :: i32() do
    pivot = arr[high]
    i_ptr = Pointer.allocate(i32())
    set! i_ptr[0], low - 1
    start = arr + low

    for_loop {element, j} <- {start, high - low} do
      if enif_compare(element, pivot) < 0 do
        i = i_ptr[0] + 1
        set! i_ptr[0], i
        swap(arr + i, start + j)
      end
    end

    i = i_ptr[0]
    swap(arr + i + 1, arr + high)
    i + 1
  end

  defm do_sort(arr :: Pointer.t(Term.t()), low :: i32(), high :: i32()) do
    if low < high do
      pi = partition(arr, low, high)
      do_sort(arr, low, pi - 1)
      do_sort(arr, pi + 1, high)
    end
  end

  @err %ArgumentError{message: "list expected"}
  defm sort(env, list) :: Term.t() do
    len_ptr = Pointer.allocate(i32())

    if enif_get_list_length(env, list, len_ptr) != 0 do
      movable_list_ptr = Pointer.allocate(Term.t())
      set! movable_list_ptr[0], list
      len = len_ptr[0]
      arr = Pointer.allocate(Term.t(), len)
      SortUtil.copy_terms(env, movable_list_ptr, arr)
      zero = const 0 :: i32()
      do_sort(arr, zero, len - 1)
      enif_make_list_from_array(env, arr, len)
    else
      enif_raise_exception(env, @err)
    end
  end
end
