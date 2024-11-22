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

  defintrinsic result_at(_entity, _index),
               %Opts{args: [%MLIR.Operation{} = op, i]} do
    MLIR.CAPI.mlirOperationGetResult(op, i)
  end

  defintrinsic result_at(_entity, _index),
               %Opts{args: [l, i]} when is_list(l) do
    l |> Enum.at(i)
  end

  defintrinsic type_of(_value), %Opts{args: [v]} do
    MLIR.Value.type(v)
  end

  ctx = MLIR.Context.create()

  for name <- @enif_functions do
    {arg_types, _} = Beaver.ENIF.signature(ctx, name)
    args = Macro.generate_arguments(length(arg_types), __MODULE__)

    defintrinsic unquote(name)(unquote_splicing(args)),
                 opts = %Opts{args: args, ctx: ctx, block: block, loc: loc} do
      {arg_types, ret_types} = Beaver.ENIF.signature(ctx, unquote(name))
      args = args |> Enum.zip(arg_types) |> Enum.map(&wrap_arg(&1, opts))

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

  MLIR.Context.destroy(ctx)

  @doc false
  def intrinsics() do
    @enif_functions ++ [:result_at]
  end
end
