defmodule Charms.Defm.Pass.CreateAbsentFunc do
  @moduledoc false
  use Beaver
  use MLIR.Pass, on: "func.func"
  alias MLIR.Dialect.Func
  require Func
  import MLIR.CAPI

  defp decompose(call) do
    arg_types =
      Beaver.Walker.operands(call)
      |> Enum.map(&mlirValueGetType/1)

    ret_types =
      Beaver.Walker.results(call)
      |> Enum.map(&mlirValueGetType/1)

    name =
      mlirOperationGetAttributeByName(
        call,
        MLIR.StringRef.create("callee")
      )
      |> mlirSymbolRefAttrGetRootReference()

    {name, arg_types, ret_types}
  end

  def run(func) do
    ctx = mlirOperationGetContext(func)
    block = mlirOperationGetBlock(func)
    symbol_table = mlirSymbolTableCreate(mlirOperationGetParentOperation(func))

    try do
      Beaver.Walker.postwalk(
        func,
        MapSet.new(),
        fn ir, created ->
          created =
            with op = %MLIR.Operation{} <- ir,
                 "func.call" <- MLIR.Operation.name(op),
                 {name, arg_types, ret_types} <- decompose(op),
                 true <- MLIR.is_null(mlirSymbolTableLookup(symbol_table, name)),
                 name_str <- MLIR.StringRef.to_string(name),
                 false <- MapSet.member?(created, name_str) do
              mlir ctx: ctx, block: block do
                {arg_types, ret_types} =
                  if s = Beaver.ENIF.signature(ctx, String.to_atom(name_str)) do
                    s
                  else
                    {arg_types, ret_types}
                  end

                Func.func _(
                            sym_name: MLIR.Attribute.string(name_str),
                            sym_visibility: MLIR.Attribute.string("private"),
                            function_type: Type.function(arg_types, ret_types)
                          ) do
                  region do
                  end
                end
              end

              MapSet.put(created, name_str)
            else
              _ ->
                created
            end

          {ir, created}
        end
      )
    after
      mlirSymbolTableDestroy(symbol_table)
    end

    :ok
  end
end
