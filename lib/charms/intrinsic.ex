defmodule Charms.Intrinsic do
  defmodule Opts do
    @moduledoc """
    Options for intrinsic functions.
    """
    defstruct [:ctx, :blk, :loc]
  end

  @moduledoc """
  Define intrinsic functions.
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
      def __use_intrinsic__, do: nil
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

  defp recompose_when_clauses({:when, meta, [call, guards]}) do
    {call, name} = recompose_when_clauses(call)
    {{:when, meta, [call, guards]}, name}
  end

  defp recompose_when_clauses({name, _meta, args}) do
    intrinsic_name_ast =
      {:unquote, [], [quote(do: :"__defintrinsic_#{unquote(unwrap_unquote(name))}__")]}

    {quote do
       unquote(intrinsic_name_ast)(unquote_splicing(args))
     end, name}
  end

  defp mark_generated(ast) do
    Macro.postwalk(ast, fn
      {tag, meta, args} ->
        {tag, Keyword.put(meta, :generated, true), args}

      other ->
        other
    end)
  end

  @doc """
  To implement an intrinsic function
  """
  defmacro defintr(call, do: body) do
    # can't get the arity from length(args), because it might be an unquote_splicing, whose length is 1
    placeholder =
      quote generated: true do
        def unquote(mark_generated(call)) do
          {name, arity} = __ENV__.function

          raise "Intrinsic #{Exception.format_mfa(__MODULE__, name, arity)} cannot be called outside of a defm body"
        end
      end

    {call, name} = recompose_when_clauses(call)

    body =
      Macro.postwalk(body, fn
        {:__IR__, _, args} when args == [] or is_atom(args) ->
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
        fn var!(charms_intrinsic_internal_opts) ->
          %Charms.Intrinsic.Opts{ctx: ctx} = var!(charms_intrinsic_internal_opts)
          unquote(body)
        end
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
