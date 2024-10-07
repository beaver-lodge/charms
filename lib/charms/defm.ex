defmodule Charms.Defm do
  @moduledoc """
  Charms.Defm provides a macro for defining functions that can be JIT compiled

  ## Extending the `defm`
  - use `beaver`'s DSL to define intrinsics which can be called in the function body of a `defm`
  - use `defm` to define functions that can be JIT-compiled
  """
  require Beaver.Env
  use Beaver
  alias MLIR.Dialect.Func
  require Func

  @doc """
  create an MLIR operation
  """
  defmacro op(_), do: :implemented_in_expander

  @doc """
  create an MLIR operation and return the result value(s)
  """
  defmacro value(_expr), do: :implemented_in_expander

  @doc """
  syntax sugar to create an MLIR value from an Elixir value
  """
  defmacro const(_), do: :implemented_in_expander

  @doc """
  call a local function with return
  """
  defmacro call({:"::", _, [_call, _types]}), do: :implemented_in_expander

  @doc """
  call a function defined in another `Charms` module with return
  """
  defmacro call(_mod, {:"::", _, [_call, _types]}), do: :implemented_in_expander

  @doc """
  for loop
  """
  defmacro for_loop(_expr, do: _body), do: :implemented_in_expander

  @doc """
  while loop
  """
  defmacro while_loop(_expr, do: _body), do: :implemented_in_expander

  @doc """
  `cond` expression requires identical types for both branches
  """
  defmacro cond_br(_condition, _clauses), do: :implemented_in_expander

  @doc false
  def decompose_call_with_return_type({:"::", _, [call, ret_type]}) do
    {call, [ret_type]}
  end

  def decompose_call_with_return_type(call) do
    {call, []}
  end

  @doc false
  def normalize_call(call) do
    {name, args} = Macro.decompose_call(call)

    args =
      for i <- Enum.with_index(args) do
        case i do
          # env
          {a = {:env, _, nil}, 0} ->
            quote do
              unquote(a) :: Charms.Env.t()
            end

          # term
          {a = {name, _, context}, version}
          when is_atom(name) and is_atom(context) and is_integer(version) ->
            quote do
              unquote(a) :: Charms.Term.t()
            end

          # typed
          {at = {:"::", _, [_a, _t]}, _} ->
            at
        end
      end

    quote do
      unquote(name)(unquote_splicing(args))
    end
  end

  defp check_poison!(op) do
    Beaver.Walker.postwalk(op, fn
      %MLIR.Operation{} = op ->
        if MLIR.Operation.name(op) == "ub.poison" do
          if msg = Beaver.Walker.attributes(op)["msg"] do
            msg = MLIR.CAPI.mlirStringAttrGetValue(msg) |> MLIR.StringRef.to_string()
            msg <> ", " <> to_string(MLIR.Operation.location(op))
          else
            "Poison operation detected in the IR. #{to_string(op)}"
          end
          |> raise
        else
          op
        end

      ir ->
        ir
    end)

    :ok
  end

  defp referenced_modules(module) do
    Beaver.Walker.postwalk(module, MapSet.new(), fn
      %MLIR.Operation{} = op, acc ->
        with "func.call" <- MLIR.Operation.name(op),
             callee when not is_nil(callee) <- Beaver.Walker.attributes(op)["callee"] do
          case callee |> to_string do
            "@Elixir." <> _ = name ->
              acc |> MapSet.put(extract_mangled_mod(name))

            _ ->
              acc
          end
          |> then(&{op, &1})
        else
          _ ->
            {op, acc}
        end

      ir, acc ->
        {ir, acc}
    end)
    |> then(fn {_, acc} -> MapSet.to_list(acc) end)
  end

  @doc false
  def compile_definitions(definitions) do
    import MLIR.Transforms
    ctx = MLIR.Context.create()
    m = MLIR.Module.create(ctx, "")

    mlir ctx: ctx, block: MLIR.Module.body(m) do
      mlir = %Charms.Defm.Expander{
        ctx: ctx,
        blk: Beaver.Env.block(),
        available_ops: MapSet.new(MLIR.Dialect.Registry.ops(:all, ctx: ctx)),
        vars: Map.new(),
        region: nil,
        enif_env: nil,
        mod: m
      }

      for {env, d} <- definitions do
        {call, ret_types, body} = d

        quote do
          def(unquote(call) :: unquote(ret_types), unquote(body))
        end
        |> Charms.Defm.Expander.expand_with(env, mlir)
      end
    end

    m
    |> Charms.Debug.print_ir_pass()
    |> MLIR.Pass.Composer.nested("func.func", Charms.Defm.Pass.CreateAbsentFunc)
    |> MLIR.Pass.Composer.append({"check-poison", "builtin.module", &check_poison!/1})
    |> canonicalize
    |> MLIR.Pass.Composer.run!(print: Charms.Debug.step_print?())
    |> then(&{MLIR.to_string(&1, bytecode: true), referenced_modules(&1)})
    |> tap(fn _ -> MLIR.Context.destroy(ctx) end)
  end

  @doc false
  def mangling(mod, func) do
    Module.concat(mod, func)
  end

  defp extract_mangled_mod("@" <> name) do
    name
    |> String.split(".")
    |> then(&Enum.take(&1, length(&1) - 1))
    |> Enum.join(".")
    |> String.to_atom()
  end
end
