defmodule ENIFMergeSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term}

  defm merge(arr :: Pointer.t(), l :: i32(), m :: i32(), r :: i32()) do
    n1 = m - l + 1
    n2 = r - m

    left_temp = Pointer.allocate(Term.t(), n1)
    right_temp = Pointer.allocate(Term.t(), n2)

    for_loop {element, i} <- {Term.t(), Pointer.element_ptr(Term.t(), arr, l), n1} do
      i = op index.casts(i) :: i32()
      i = result_at(i, 0)
      Pointer.store(element, Pointer.element_ptr(Term.t(), left_temp, i))
    end

    for_loop {element, j} <- {Term.t(), Pointer.element_ptr(Term.t(), arr, m + 1), n2} do
      j = op index.casts(j) :: i32()
      j = result_at(j, 0)
      Pointer.store(element, Pointer.element_ptr(Term.t(), right_temp, j))
    end

    i_ptr = Pointer.allocate(i32())
    j_ptr = Pointer.allocate(i32())
    k_ptr = Pointer.allocate(i32())

    zero = const 0 :: i32()
    Pointer.store(zero, i_ptr)
    Pointer.store(zero, j_ptr)
    Pointer.store(l, k_ptr)

    while_loop(Pointer.load(i32(), i_ptr) < n1 && Pointer.load(i32(), j_ptr) < n2) do
      i = Pointer.load(i32(), i_ptr)
      j = Pointer.load(i32(), j_ptr)
      k = Pointer.load(i32(), k_ptr)

      left_term = Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), left_temp, i))
      right_term = Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), right_temp, j))

      if enif_compare(left_term, right_term) <= 0 do
        Pointer.store(
          Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), left_temp, i)),
          Pointer.element_ptr(Term.t(), arr, k)
        )

        Pointer.store(i + 1, i_ptr)
      else
        Pointer.store(
          Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), right_temp, j)),
          Pointer.element_ptr(Term.t(), arr, k)
        )

        Pointer.store(j + 1, j_ptr)
      end

      Pointer.store(k + 1, k_ptr)
    end

    while_loop(Pointer.load(i32(), i_ptr) < n1) do
      i = Pointer.load(i32(), i_ptr)
      k = Pointer.load(i32(), k_ptr)

      Pointer.store(
        Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), left_temp, i)),
        Pointer.element_ptr(Term.t(), arr, k)
      )

      Pointer.store(i + 1, i_ptr)
      Pointer.store(k + 1, k_ptr)
    end

    while_loop(Pointer.load(i32(), j_ptr) < n2) do
      j = Pointer.load(i32(), j_ptr)
      k = Pointer.load(i32(), k_ptr)

      Pointer.store(
        Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), right_temp, j)),
        Pointer.element_ptr(Term.t(), arr, k)
      )

      Pointer.store(j + 1, j_ptr)
      Pointer.store(k + 1, k_ptr)
    end

    func.return
  end

  defm do_sort(arr :: Pointer.t(), l :: i32(), r :: i32()) do
    if l < r do
      two = const 2 :: i32()
      m = op arith.divsi(l + r, two) :: i32()
      m = result_at(m, 0)
      do_sort(arr, l, m)
      do_sort(arr, m + 1, r)
      merge(arr, l, m, r)
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
      call ENIFTimSort.copy_terms(env, movable_list_ptr, arr)
      zero = const 0 :: i32()
      do_sort(arr, zero, len - 1)
      enif_make_list_from_array(env, arr, len)
    else
      enif_raise_exception(env, @err)
    end
  end
end
