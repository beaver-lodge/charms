defmodule Charms.Term do
  @moduledoc """
  Intrinsic module for Erlang term type.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts

  @doc """
  Return the Erlang term type.
  """
  defintrinsic t(), %Opts{ctx: ctx} do
    Beaver.ENIF.Type.term(ctx: ctx)
  end
end
