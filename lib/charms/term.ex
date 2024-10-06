defmodule Charms.Term do
  use Charms.Intrinsic

  @impl true
  def handle_intrinsic(:t, [], opts) do
    Beaver.ENIF.Type.term(opts)
  end
end
