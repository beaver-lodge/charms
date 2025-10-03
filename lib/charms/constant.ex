defmodule Charms.Constant do
  @moduledoc false
  use Beaver
  alias Beaver.MLIR.Dialect.{Arith, Index}

  def from_literal(literal, %MLIR.Value{} = v, ctx, blk, loc) do
    t = MLIR.Value.type(v)
    from_literal(literal, t, ctx, blk, loc)
  end

  def from_literal(literal, %MLIR.Type{} = t, ctx, blk, loc) do
    mlir ctx: ctx, blk: blk do
      cond do
        MLIR.Type.integer?(t) ->
          Arith.constant(value: Attribute.integer(t, literal), loc: loc) >>> t

        MLIR.Type.float?(t) ->
          Arith.constant(value: Attribute.float(t, literal), loc: loc) >>> t

        MLIR.Type.index?(t) ->
          Index.constant(value: Attribute.index(literal), loc: loc) >>> t

        true ->
          loc = Beaver.Deferred.create(loc, ctx)

          raise CompileError,
                Charms.Diagnostic.meta_from_loc(loc) ++
                  [description: "Not a supported type for constant, #{to_string(t)}"]
      end
    end
  end
end
