defmodule Charms.Intrinsic do
  @moduledoc """
  Behaviour to define intrinsic functions.
  """
  alias Beaver
  @type opt :: {:ctx, MLIR.Context.t()} | {:block, MLIR.Block.t()}
  @type opts :: [opt | {atom(), term()}]
  @callback handle_intrinsic(atom(), [term()], opts()) :: term()
  Module.register_attribute(__MODULE__, :defintrinsic, accumulate: true)

  @doc false
  def collect_intrinsics(nil) do
    raise ArgumentError, "no intrinsic functions defined"
  end

  def collect_intrinsics(attr_list) when length(attr_list) > 0 do
    attr_list |> Enum.reverse() |> List.flatten() |> Enum.uniq()
  end

  defmacro __using__(_) do
    quote do
      @behaviour Charms.Intrinsic
      use Beaver
      @before_compile Charms.Intrinsic
      import Charms.Intrinsic, only: :macros
    end
  end

  defmacro defintrinsic(intrinsic_list) do
    quote do
      @defintrinsic unquote(intrinsic_list)
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @defintrinsic_list @defintrinsic |> Charms.Intrinsic.collect_intrinsics()
      def __intrinsics__() do
        @defintrinsic_list
      end
    end
  end
end
