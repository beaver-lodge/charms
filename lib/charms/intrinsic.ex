defmodule Charms.Intrinsic do
  defmodule Opts do
    @moduledoc """
    Options for intrinsic functions.
    """
    defstruct [:ctx, :blk, :loc]
  end

  @moduledoc """
  Behaviour to define intrinsic functions.
  """
  alias Beaver
  @type ir_return :: MLIR.Value.t() | MLIR.Operation.t()
  @type intrinsic_return :: ir_return() | (any() -> ir_return())
  Module.register_attribute(__MODULE__, :defintrinsic, accumulate: true)

  defmacro __using__(_) do
    quote do
      use Beaver
      @before_compile Charms.Intrinsic
      import Charms.Intrinsic, only: :macros
      Module.register_attribute(__MODULE__, :intrinsic, accumulate: true)
    end
  end

  defp unwrap_unquote(name) do
    case name do
      {:unquote, _, [name]} ->
        name

      _ ->
        name
    end
  end

  defp recompose_when_clauses(name, args) do
    intrinsic_name_ast =
      {:unquote, [], [quote(do: :"__defintrinsic_#{unquote(unwrap_unquote(name))}__")]}

    opts =
      quote do
        %Charms.Intrinsic.Opts{} = var!(charms_intrinsic_internal_opts)
      end

    case opts do
      {:when, when_meta, [opts | clauses]} ->
        {:when, when_meta,
         [
           quote do
             unquote(intrinsic_name_ast)(unquote(args), unquote(opts))
           end
           | clauses
         ]}

      _ ->
        quote do
          unquote(intrinsic_name_ast)(unquote(args), unquote(opts))
        end
    end
  end

  defp normalize_arg_names(args) do
    for arg <- args do
      case arg do
        {arg_name, meta, nil} ->
          arg_name
          |> to_string()
          |> String.trim_leading("_")
          |> String.to_atom()
          |> then(&{&1, meta, nil})

        _ ->
          arg
      end
    end
  end

  @doc """
  To implement an intrinsic function
  """
  defmacro defintrinsic(call, do: body) do
    {name, _meta, args} = call
    call = recompose_when_clauses(name, args)
    placeholder_args = normalize_arg_names(args)

    # can't get the arity from length(args), because it might be an unquote_splicing, whose length is 1
    placeholder =
      quote generated: true do
        def unquote(name)(unquote_splicing(placeholder_args)) do
          arity = length([unquote_splicing(placeholder_args)])

          raise "Intrinsic #{Exception.format_mfa(__MODULE__, unquote(name), arity)} cannot be called outside of a defm body"
        end
      end

    body =
      Macro.postwalk(body, fn
        {:__IR__, _, args} when args == [] or args == nil ->
          quote do
            var!(charms_intrinsic_internal_opts)
          end

        ast ->
          ast
      end)

    quote do
      unquote(placeholder)
      @doc false
      def unquote(call) do
        %Charms.Intrinsic.Opts{ctx: ctx} = var!(charms_intrinsic_internal_opts)
        unquote(body)
      end

      @intrinsic {unquote(unwrap_unquote(name)),
                  :"__defintrinsic_#{unquote(unwrap_unquote(name))}__"}
    end
  end

  defmacro __before_compile__(_env) do
    quote bind_quoted: [] do
      @all_intrinsics @intrinsic |> Enum.uniq()

      for {name, intrinsic_name} <- @all_intrinsics do
        def __intrinsics__(unquote(name), arity) do
          if function_exported?(__MODULE__, unquote(name), arity) do
            unquote(intrinsic_name)
          end
        end
      end

      def __intrinsics__(_, _), do: nil
    end
  end
end
