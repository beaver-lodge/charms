defmodule Charms.Env do
  @moduledoc """
  Intrinsic module for BEAM environment's type.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts

  defintrinsic t(), %Opts{ctx: ctx} do
    Beaver.ENIF.Type.env(ctx: ctx)
  end
end
