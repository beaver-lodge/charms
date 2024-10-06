defmodule Charms.Env do
  use Charms.Intrinsic

  @impl true
  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.env(opts)
  end
end
