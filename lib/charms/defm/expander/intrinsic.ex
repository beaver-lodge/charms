defmodule Charms.Defm.Expander.Intrinsic do
  @moduledoc """
  Expand intrinsic function calls in Charms defm.
  """
  use Beaver
  alias Charms.Defm.Expander
  import Charms.Diagnostic, only: :macros

  def expand(loc, module, intrinsic_impl, args, state, env) do
    try do
      {args, state, env} = Expander.expand(args, state, env)
      state = update_in(state.mlir.required_intrinsic_modules, &MapSet.put(&1, module))

      v =
        apply(module, intrinsic_impl, args).(%Charms.Intrinsic.Opts{
          ctx: state.mlir.ctx,
          blk: state.mlir.blk,
          loc: loc
        })

      case v do
        %m{} when m in [MLIR.Value, MLIR.Type, MLIR.Operation] ->
          {v, state, env}

        {ast = {_, _, _}, bindings} when is_list(bindings) ->
          case Macro.validate(ast) do
            :ok ->
              :ok

            {:error, reason} ->
              raise_compile_error(
                env,
                "Intrinsic #{module}.#{intrinsic_impl} returned invalid AST: #{inspect(reason)}"
              )
          end

          Expander.expand_with_bindings(ast, state, env, bindings)

        other ->
          raise_compile_error(
            env,
            "Unexpected return type from intrinsic #{module}.#{intrinsic_impl}: #{inspect(other)}"
          )
      end
    rescue
      e ->
        raise_compile_error(
          env,
          "Failed to compile intrinsic\n#{Exception.message(e)}\n#{Exception.format_stacktrace(__STACKTRACE__)}"
        )
    end
  end
end
