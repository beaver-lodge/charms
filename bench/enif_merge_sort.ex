defmodule ENIFMergeSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm do_sort(arr :: Pointer.t(Term.t()), l :: i32(), r :: i32()) do
    if l < r do
      two = const 2 :: i32()
      m = value arith.divsi(l + r, two) :: i32()
      do_sort(arr, l, m)
      do_sort(arr, m + 1, r)
      SortUtil.merge(arr, l, m, r)
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
      zero = const 0 :: i32()
      do_sort(arr, zero, len - 1)
      defer free! arr
      enif_make_list_from_array(env, arr, len)
    else
      enif_raise_exception(env, @err)
    end
  end
end
