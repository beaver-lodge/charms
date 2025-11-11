defmodule Charms.Definition.Call do
  @moduledoc """
  Normalize and infer the call part of a `defm` function.
  """

  defp normalize_arg({:env, _, nil} = a, 0) do
    quote do
      unquote(a) :: Charms.Env.t()
    end
  end

  defp normalize_arg({name, _, context} = a, _) when is_atom(name) and is_atom(context) do
    quote do
      unquote(a) :: Charms.Term.t()
    end
  end

  defp normalize_arg({:"::", _, [_variable, _type]} = specified, _) do
    specified
  end

  @doc """
  normalize the call signature by adding type annotations
  - if an argument is `env`, it is annotated with `Charms.Env.t()`
  - if an argument is a bare term, it is annotated with `Charms.Term.t()`
  - if an argument is already typed, it is left unchanged
  """
  def normalize(call) do
    {name, args} = Macro.decompose_call(call)

    args =
      for {a, i} <- Enum.with_index(args) do
        normalize_arg(a, i)
      end

    quote do
      unquote(name)(unquote_splicing(args))
    end
  end

  @doc """
  Decomposes a call into the call part and return type.
  """
  def decompose_return_signature({:"::", _, [call, ret_type]}) do
    {call, [ret_type]}
  end

  def decompose_return_signature(call) do
    {call, []}
  end

  def decompose_call(call) do
    {call, ret_types} = decompose_return_signature(call)
    call = normalize(call)
    {name, args} = Macro.decompose_call(call)
    {name, args, ret_types}
  end
end
