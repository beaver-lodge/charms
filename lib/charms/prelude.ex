defmodule Charms.Prelude do
  @moduledoc """
  Intrinsic module to define essential functions provided by Charms.
  """
  use Charms.Intrinsic
  alias Charms.Intrinsic.Opts
  alias Beaver.MLIR.Dialect.{Arith, Func}
  @enif_functions Beaver.ENIF.functions()

  defp wrap_arg({i, t}, %Opts{ctx: ctx, block: block}) when is_integer(i) do
    mlir ctx: ctx, block: block do
      case i do
        %MLIR.Value{} ->
          i

        i when is_integer(i) ->
          if MLIR.CAPI.mlirTypeIsAInteger(t) |> Beaver.Native.to_term() do
            Arith.constant(value: Attribute.integer(t, i)) >>> t
          else
            raise ArgumentError, "Not an integer type, #{to_string(t)}"
          end
      end
    end
  end

  defp wrap_arg({v, _}, _) do
    v
  end

  defintrinsic result_at(%MLIR.Operation{} = op, index) do
    num_results = MLIR.CAPI.mlirOperationGetNumResults(op)

    if index < num_results do
      MLIR.CAPI.mlirOperationGetResult(op, index)
    else
      raise ArgumentError,
            "Index #{index} is out of bounds for operation results, num results: #{num_results}"
    end
  end

  @doc """
  Get the MLIR type of the given value.
  """
  defintrinsic type_of(value) do
    MLIR.Value.type(value)
  end

  @doc """
  Dump the MLIR entity at compile time with `IO.puts/1`
  """
  defintrinsic dump(entity) do
    entity |> tap(&IO.puts(MLIR.to_string(&1)))
  end

  signature_ctx = MLIR.Context.create()

  for name <- @enif_functions do
    {arg_types, _} = Beaver.ENIF.signature(signature_ctx, name)
    args = Macro.generate_arguments(length(arg_types), __MODULE__)

    defintrinsic unquote(name)(unquote_splicing(args)) do
      opts = %Opts{ctx: ctx, block: block, loc: loc} = __IR__
      {arg_types, ret_types} = Beaver.ENIF.signature(ctx, unquote(name))
      args = [unquote_splicing(args)] |> Enum.zip(arg_types) |> Enum.map(&wrap_arg(&1, opts))

      mlir ctx: ctx, block: block do
        Func.call(args, callee: Attribute.flat_symbol_ref("#{unquote(name)}"), loc: loc) >>>
          case ret_types do
            [ret] ->
              ret

            [] ->
              []
          end
      end
    end
  end

  MLIR.Context.destroy(signature_ctx)

  @doc false
  def intrinsics() do
    @enif_functions ++ [:result_at]
  end
end
