module attributes {transform.with_named_sequence} {
  transform.named_sequence @__transform_main(%toplevel_module: !transform.any_op {transform.readonly}) {
    %gpu_module = transform.structured.match ops{["gpu.module"]} in %toplevel_module
      : (!transform.any_op) -> !transform.any_op

    transform.apply_patterns to %gpu_module {
      transform.apply_patterns.gpu.gpu_rewrite_patterns
    } : !transform.any_op

    transform.apply_conversion_patterns to %gpu_module {
      transform.apply_conversion_patterns.dialect_to_llvm "arith"
      transform.apply_conversion_patterns.dialect_to_llvm "cf"
      transform.apply_conversion_patterns.vector.vector_to_llvm
      transform.apply_conversion_patterns.func.func_to_llvm
      transform.apply_conversion_patterns.dialect_to_llvm "memref"
      transform.apply_conversion_patterns.gpu.gpu_to_nvvm {benefit = 10 : i16}
      transform.apply_conversion_patterns.gpu.gpu_wmma_to_nvvm
      transform.apply_conversion_patterns.gpu.gpu_subgroup_reduce_to_nvvm
      transform.apply_conversion_patterns.nvgpu.nvgpu_to_nvvm
    } with type_converter {
      transform.apply_conversion_patterns.memref.memref_to_llvm_type_converter
        {index_bitwidth = 64,
        use_bare_ptr_call_conv = false}
    } {
      legal_dialects = ["llvm", "memref", "nvvm", "test"],
      legal_ops = ["func.func", "gpu.module", "gpu.yield"],
      illegal_dialects = ["gpu"],
      illegal_ops = ["llvm.copysign", "llvm.cos", "llvm.exp", "llvm.exp2", "llvm.fabs", "llvm.fceil",
                    "llvm.ffloor", "llvm.frem", "llvm.log", "llvm.log10", "llvm.log2", "llvm.pow",
                    "llvm.roundeven", "llvm.round", "llvm.sin", "llvm.sqrt"],
      partial_conversion
    } : !transform.any_op
    transform.yield
  }
}
