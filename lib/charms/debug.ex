defmodule Charms.Debug do
  @moduledoc false
  alias Beaver.MLIR

  def print_ir_pass(op) do
    if System.get_env("DEFM_PRINT_IR") && !step_print?() do
      case op do
        %MLIR.Operation{} ->
          MLIR.dump!(op)

        _ ->
          MLIR.Transform.print_ir(op)
      end
    else
      op
    end
  end

  @default_debug_types ~w[pass-manager dialect-conversion]
  defp global_debug_types() do
    case System.get_env("DEFM_GLOBAL_DEBUG") do
      nil ->
        nil

      value ->
        normalized =
          value
          |> String.trim()
          |> String.downcase()

        if normalized in ["1", "true", "yes", "on"] do
          @default_debug_types
        else
          # it is a string flag like 'shape-inference,dataflow'
          value |> String.split(",") |> Enum.map(&String.trim/1)
        end
    end
  end

  def enable() do
    debug_types = global_debug_types()

    if debug_types do
      MLIR.Debug.enable_global_debug(true)
      MLIR.Debug.set_debug_type(debug_types)
    end
  end

  def step_print?() do
    System.get_env("DEFM_PRINT_IR") == "step"
  end
end
