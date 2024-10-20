defmodule Charms.Defm do
  @moduledoc """
  Charms.Defm is a DSL for defining functions that can be JIT-compiled.

  This module defines the `defm` DSL syntax as macros and special forms.

  ## Extending the `defm`
  - use `beaver`'s DSL to define intrinsics which can be called in the function body of a `defm`
  - use `defm` to define functions that can be JIT-compiled
  """

  @doc """
  create an MLIR operation
  """
  defmacro op(_), do: :implemented_in_expander

  @doc """
  create an MLIR operation and return the result value(s)
  """
  defmacro value(_expr), do: :implemented_in_expander

  @doc """
  syntax sugar to create an MLIR value from an Elixir value
  """
  defmacro const(_), do: :implemented_in_expander

  @doc """
  call a local function with return
  """
  defmacro call({:"::", _, [_call, _types]}), do: :implemented_in_expander

  @doc """
  call a function defined in another `Charms` module with return
  """
  defmacro call(_mod, {:"::", _, [_call, _types]}), do: :implemented_in_expander

  @doc """
  for loop
  """
  defmacro for_loop(_expr, do: _body), do: :implemented_in_expander

  @doc """
  while loop
  """
  defmacro while(_expr, do: _body), do: :implemented_in_expander

  @doc """
  `cond` expression requires identical types for both branches
  """
  defmacro cond_br(_condition, _clauses), do: :implemented_in_expander

  @doc false
  def mangling(mod, func) do
    Module.concat(mod, func)
  end
end
