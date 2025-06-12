defmodule Charms.Defmstruct.Definition do
  @moduledoc false
  defstruct [:env, :fields]

  def new(env, fields) do
    %__MODULE__{env: env, fields: fields}
  end
end
