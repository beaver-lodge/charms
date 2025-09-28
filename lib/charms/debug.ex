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

  def global_debug_enabled?() do
    System.get_env("DEFM_GLOBAL_DEBUG")
  end

  def step_print?() do
    System.get_env("DEFM_PRINT_IR") == "step"
  end
end
