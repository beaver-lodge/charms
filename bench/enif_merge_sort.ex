defmodule ENIFMergeSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm do_sort(arr :: Pointer.t(), l :: i32(), r :: i32()) do
    if l < r do
      two = const 2 :: i32()
      m = op arith.divsi(l + r, two) :: i32()
      m = result_at(m, 0)
      do_sort(arr, l, m)
      do_sort(arr, m + 1, r)
      call SortUtil.merge(arr, l, m, r)
    end

    func.return
  end

  @err %ArgumentError{message: "list expected"}
  defm sort(env, list) :: Term.t() do
    len_ptr = Pointer.allocate(i32())

    if enif_get_list_length(env, list, len_ptr) != 0 do
      movable_list_ptr = Pointer.allocate(Term.t())
      Pointer.store(list, movable_list_ptr)
      len = Pointer.load(i32(), len_ptr)
      arr = Pointer.allocate(Term.t(), len)
      call SortUtil.copy_terms(env, movable_list_ptr, arr)
      zero = const 0 :: i32()
      do_sort(arr, zero, len - 1)
      enif_make_list_from_array(env, arr, len)
    else
      enif_raise_exception(env, @err)
    end
  end
end
