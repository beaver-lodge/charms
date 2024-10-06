defmodule Charms.Term do
  @moduledoc """
  Intrinsic module for SIMD type.
  """
  use Charms.Intrinsic

  @impl true
  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.term(opts)
  end

  defintrinsic [:t]
end
