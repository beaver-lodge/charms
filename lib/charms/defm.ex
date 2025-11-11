defmodule Charms.Defm do
  @moduledoc """
  Charms.Defm is a DSL for defining functions that can be JIT-compiled.

  This module defines the `defm` DSL syntax as macros and special forms.

  ## Extending the `defm`
  There are two kinds of functions in `Charms`:
  - Intrinsic: use `beaver`'s DSL to define, which can be called in the function body of a `defm`
  - `defm`: use `defm` to define, can be JIT-compiled
  Although they are different in terms of underline implementation, they share the same syntax to invoke.
  """

  @doc """
  create an MLIR operation
  """
  defmacro op(_), do: raise("should only use inside 'defm'")

  @doc """
  create an MLIR operation and return the result value(s)
  """
  defmacro value(_expr), do: raise("should only use inside 'defm'")

  @doc """
  for loop
  """
  defmacro for_loop(_expr, do: _body), do: raise("should only use inside 'defm'")

  @doc """
  while loop
  """
  defmacro while(_expr, do: _body), do: raise("should only use inside 'defm'")

  @doc """
  `cond` expression requires identical types for both branches
  """
  defmacro cond_br(_condition, _clauses), do: raise("should only use inside 'defm'")

  @doc """
  setting value with a index expression
  """
  defmacro set!(_index_expression, _value), do: raise("should only use inside 'defm'")

  @doc """
  launch a GPU kernel and wait for its completion
  """
  defmacro launch!(_kernel_call, _blocks, _threads), do: raise("should only use inside 'defm'")

  @doc """
  Executes an expression unconditionally at do-block exit.
  """
  defmacro defer(_expression), do: raise("should only use inside 'defm'")

  @doc false
  def mangling(mod, func) do
    Module.concat(mod, func) |> to_string() |> String.replace(".", "$")
  end

  @doc false
  def extract_mangled_mod("@" <> name) do
    name
    |> String.split("$")
    |> then(&Enum.take(&1, length(&1) - 1))
    |> Enum.join(".")
    |> String.to_atom()
  end
end
