defmodule Charms.Defm.Expander.Resolver do
  @moduledoc """
  Infer the return type of a function call by looking up the symbol table
  or expanding the definition if available.
  """
  alias Beaver.MLIR, as: MLIR
  alias MLIR.Dialect.Func
  require Logger
  require Func
  import Charms.Diagnostic, only: :macros

  def resolve_return_type!(infer, {mod, name, arity}, state, env) do
    case infer.({name, arity}, state) do
      nil ->
        raise_compile_error(
          env,
          "function or intrinsic #{Exception.format_mfa(mod, name, arity)} is undefined"
        )

      [%MLIR.Type{} = t] ->
        [t]

      [] ->
        []
    end
  end

  def resolve_argument_type!(infer, {mod, name, arity}, index, argument, state, env) do
    expanded = infer.({name, arity}, index, state)

    case expanded do
      %MLIR.Type{} ->
        expanded

      nil ->
        raise_compile_error(
          env,
          "function or intrinsic #{Exception.format_mfa(mod, name, arity)} is undefined"
        )
    end
    |> then(fn
      %MLIR.Type{} = t ->
        loc = MLIR.Location.from_env(env)

        casted =
          Charms.Coercion.cast_argument(argument, t, state.mlir.ctx, state.mlir.blk, loc)

        if not MLIR.equal?(MLIR.Value.type(casted), t) do
          raise_compile_error(
            env,
            "Incompatible type for argument ##{index} in call to #{Exception.format_mfa(mod, name, arity)}: expected #{MLIR.to_string(t)}, but got #{MLIR.to_string(MLIR.Value.type(argument))}"
          )
        end

        casted

      _ ->
        argument
    end)
  end
end
