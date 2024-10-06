defmodule Charms.Env do
  @moduledoc """
  Intrinsic module for BEAM's types.
  """
  use Charms.Intrinsic

  @impl true
  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.env(opts)
  end
end
