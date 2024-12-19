defmodule ENIFQuickSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm swap(a :: Pointer.t(Term.t()), b :: Pointer.t(Term.t())) do
    val_a = Pointer.load(a)
    val_b = Pointer.load(b)
    Pointer.store(val_b, a)
    Pointer.store(val_a, b)
  end

  defm partition(arr :: Pointer.t(Term.t()), low :: i32(), high :: i32()) :: i32() do
    pivot_ptr = Pointer.element_ptr(arr, high)
    pivot = Pointer.load(pivot_ptr)
    i_ptr = Pointer.allocate(i32())
    Pointer.store(low - 1, i_ptr)
    start = Pointer.element_ptr(arr, low)

    for_loop {element, j} <- {start, high - low} do
      if enif_compare(element, pivot) < 0 do
        i = Pointer.load(i_ptr) + 1
        Pointer.store(i, i_ptr)
        swap(Pointer.element_ptr(arr, i), Pointer.element_ptr(start, j))
      end
    end

    i = Pointer.load(i_ptr)
    swap(Pointer.element_ptr(arr, i + 1), Pointer.element_ptr(arr, high))
    func.return(i + 1)
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
      Pointer.store(list, movable_list_ptr)
      len = Pointer.load(len_ptr)
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
