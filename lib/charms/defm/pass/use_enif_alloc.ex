defmodule Charms.Defm.Pass.UseENIFAlloc do
  @moduledoc false
  use Beaver
  use MLIR.Pass, on: "builtin.module"
  alias MLIR.Dialect.LLVM
  import Beaver.Pattern

  def initialize(ctx, nil) do
    patterns = [
      replace_alloc(),
      replace_free()
    ]

    frozen_pat_set = Beaver.Pattern.compile_patterns(ctx, patterns)
    {:ok, %{patterns: frozen_pat_set}}
  end

  def destruct(nil) do
    :ok
  end

  def destruct(%{patterns: frozen_pat_set}) do
    MLIR.CAPI.mlirFrozenRewritePatternSetDestroy(frozen_pat_set)
  end

  defpat replace_alloc(benefit: 10) do
    size = value()
    ptr_t = type()
    {op, _} = LLVM.call(size, callee: Attribute.flat_symbol_ref("malloc")) >>> {:op, [ptr_t]}

    rewrite op do
      r =
        LLVM.call(size,
          callee: Attribute.flat_symbol_ref("enif_alloc"),
          operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([1, 0]),
          op_bundle_sizes: ~a{array<i32>}
        ) >>> ptr_t

      replace(op, with: r)
    end
  end

  defpat replace_free(benefit: 10) do
    ptr = value()
    {op, _} = LLVM.call(ptr, callee: Attribute.flat_symbol_ref("free")) >>> {:op, []}

    rewrite op do
      {enif_free, _} =
        LLVM.call(ptr,
          callee: Attribute.flat_symbol_ref("enif_free"),
          operand_segment_sizes: Beaver.MLIR.ODS.operand_segment_sizes([1, 0]),
          op_bundle_sizes: ~a{array<i32>}
        ) >>> {:op, []}

      replace(op, with: enif_free)
    end
  end

  def run(op, %{patterns: patterns} = state) do
    module = MLIR.Module.from_operation(op)
    MLIR.apply!(module, patterns)
    state
  end
end
