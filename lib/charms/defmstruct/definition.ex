defmodule Charms.Defmstruct.Definition do
  @moduledoc false
  defstruct [:env, :fields]

  def new(env, fields) do
    %__MODULE__{env: env, fields: fields}
  end

  def expand(module, fields, state, env) do
    {field_names, field_types} = Enum.unzip(fields)
    {field_types, state, _env} = Charms.Defm.Expander.expand(field_types, state, env)

    Charms.Struct.llvm_struct_type_identified!(module, field_types, ctx: state.mlir.ctx)
    |> then(&{&1, field_names})
  end
end
