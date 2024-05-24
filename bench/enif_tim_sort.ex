defmodule ENIFTimSort do
  @moduledoc false
  use Charms
  alias Charms.{Pointer, Term, Env}

  defm insertion_sort(arr :: Pointer.t(), left :: i32(), right :: i32()) do
    start_i = left + 1
    start = Pointer.element_ptr(Term.t(), arr, start_i)
    n = right - start_i + 1

    for_loop {temp, i} <- {Term.t(), start, n} do
      i = value index.casts(i) :: i32()
      i = i + start_i
      j_ptr = Pointer.allocate(i32())
      Pointer.store(i - 1, j_ptr)

      while_loop(
        Pointer.load(i32(), j_ptr) >= left &&
          Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), arr, Pointer.load(i32(), j_ptr))) >
            temp
      ) do
        j = Pointer.load(i32(), j_ptr)

        Pointer.store(
          Pointer.load(Term.t(), Pointer.element_ptr(Term.t(), arr, j)),
          Pointer.element_ptr(Term.t(), arr, j + 1)
        )

        Pointer.store(j - 1, j_ptr)
      end

      j = Pointer.load(i32(), j_ptr)
      Pointer.store(temp, Pointer.element_ptr(Term.t(), arr, j + 1))
    end

    func.return()
  end

  defm tim_sort(arr :: Pointer.t(), n :: i32()) do
    run_const = op arith.constant(value: Attribute.integer(i32(), 32)) :: i32()
    run = result_at(run_const, 0)
    i_ptr = Pointer.allocate(i32())
    zero_const = op arith.constant(value: Attribute.integer(i32(), 0)) :: i32()
    zero = result_at(zero_const, 0)
    Pointer.store(zero, i_ptr)

    while_loop(Pointer.load(i32(), i_ptr) < n) do
      i = Pointer.load(i32(), i_ptr)
      min = value arith.minsi(i + run - 1, n - 1) :: i32()
      insertion_sort(arr, i, min)
      Pointer.store(i + run, i_ptr)
    end

    size_ptr = Pointer.allocate(i32())
    Pointer.store(run, size_ptr)

    while_loop(Pointer.load(i32(), size_ptr) < n) do
      size = Pointer.load(i32(), size_ptr)

      left_ptr = Pointer.allocate(i32())
      Pointer.store(zero, left_ptr)

      while_loop(Pointer.load(i32(), left_ptr) < n) do
        left = Pointer.load(i32(), left_ptr)
        mid = left + size - 1
        right = op arith.minsi(left + 2 * size - 1, n - 1) :: i32()
        right = result_at(right, 0)

        struct_if(mid < right) do
          call ENIFMergeSort, merge(arr, left, mid, right) :: []
        end

        Pointer.store(left + 2 * size, left_ptr)
      end

      Pointer.store(size * 2, size_ptr)
    end

    func.return()
  end

  defm copy_terms(env :: Env.t(), movable_list_ptr :: Pointer.t(), arr :: Pointer.t()) do
    head = Pointer.allocate(Term.t())
    zero_const = op arith.constant(value: Attribute.integer(i32(), 0)) :: i32()
    zero = result_at(zero_const, 0)
    i_ptr = Pointer.allocate(i32())
    Pointer.store(zero, i_ptr)

    while_loop(
      enif_get_list_cell(
        env,
        Pointer.load(Term.t(), movable_list_ptr),
        head,
        movable_list_ptr
      ) > 0
    ) do
      head_val = Pointer.load(Term.t(), head)
      i = Pointer.load(i32(), i_ptr)
      ith_term_ptr = Pointer.element_ptr(Term.t(), arr, i)
      Pointer.store(head_val, ith_term_ptr)
      Pointer.store(i + 1, i_ptr)
    end

    func.return()
  end

  defm sort(env, list, err) :: Term.t() do
    len_ptr = Pointer.allocate(i32())

    cond_br(enif_get_list_length(env, list, len_ptr) != 0) do
      movable_list_ptr = Pointer.allocate(Term.t())
      Pointer.store(list, movable_list_ptr)
      len = Pointer.load(i32(), len_ptr)
      arr = Pointer.allocate(Term.t(), len)
      copy_terms(env, movable_list_ptr, arr)
      tim_sort(arr, len)
      ret = enif_make_list_from_array(env, arr, len)
      func.return(ret)
    else
      func.return(err)
    end
  end
end
