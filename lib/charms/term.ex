defmodule Charms.Term do
  @moduledoc """
  Intrinsic module for Erlang term type.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts

  @doc """
  Return the Erlang term type.
  """
  defintr t() do
    %Opts{ctx: ctx} = __IR__
    Beaver.ENIF.Type.term(ctx: ctx)
  end

  defintr to_i32!(env, term) do
    {quote do
       ptr = ptr! i32()
       enif_get_int!(env, term, ptr)
       ptr[0]
     end, env: env, term: term}
  end

  defintr to_i64!(env, term) do
    {quote do
       ptr = ptr! i64()
       enif_get_int64!(env, term, ptr)
       ptr[0]
     end, env: env, term: term}
  end

  defintr to_f64!(env, term) do
    {quote do
       ptr = ptr! f64()
       enif_get_double!(env, term, ptr)
       ptr[0]
     end, env: env, term: term}
  end

  defintr to_pid!(env, term) do
    {quote do
       ptr = ptr! i64()
       enif_get_local_pid!(env, term, ptr)
       ptr
     end, env: env, term: term}
  end
end
