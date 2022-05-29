// This file is a part of Julia. License is MIT: https://julialang.org/license

// This file provides a fallback implementation of the codegen plugin interface,
// used when libjulia-codegen is not available.

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

JL_DLLEXPORT void jl_dump_native_fallback(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname,
        const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
JL_DLLEXPORT int32_t jl_get_llvm_gv_fallback(void *native_code, jl_value_t *p) UNAVAILABLE

JL_DLLEXPORT void jl_extern_c_fallback(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_method_asm_fallback(jl_method_instance_t *linfo, size_t world,
        char raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_function_ir_fallback(jl_llvmf_dump_t *dump, char strip_ir_metadata, char dump_module, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT void jl_get_llvmf_defn_fallback(jl_llvmf_dump_t *dump, jl_method_instance_t *linfo, size_t world, char getwrapper, char optimize, const jl_cgparams_t params) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm_fallback(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction_fallback(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

JL_DLLEXPORT void jl_init_codegen_fallback(void) { }

JL_DLLEXPORT int jl_getFunctionInfo_fallback(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline)
{
    return 0;
}

JL_DLLEXPORT void jl_register_fptrs_fallback(uint64_t sysimage_base, const struct _jl_sysimg_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n)
{
    (void)sysimage_base; (void)fptrs; (void)linfos; (void)n;
}

JL_DLLEXPORT jl_code_instance_t *jl_generate_fptr_fallback(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world)
{
    return NULL;
}

JL_DLLEXPORT void jl_generate_fptr_for_unspecialized_fallback(jl_code_instance_t *unspec)
{
    jl_atomic_store_release(&unspec->invoke, &jl_fptr_interpret_call);
}

JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION_fallback(void)
{
    return 0;
}

JL_DLLEXPORT int jl_compile_extern_c_fallback(LLVMOrcThreadSafeModuleRef llvmmod, void *params, void *sysimg, jl_value_t *declrt, jl_value_t *sigt)
{
    return 0;
}

JL_DLLEXPORT void jl_teardown_codegen_fallback(void)
{
}

JL_DLLEXPORT size_t jl_jit_total_bytes_fallback(void)
{
    return 0;
}

JL_DLLEXPORT void jl_lock_profile_fallback(void)
{
}

JL_DLLEXPORT void jl_unlock_profile_fallback(void)
{
}

JL_DLLEXPORT void *jl_create_native_fallback(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmctxt, const jl_cgparams_t *cgparams, int _policy) UNAVAILABLE

JL_DLLEXPORT void jl_dump_compiles_fallback(void *s)
{
}

JL_DLLEXPORT void jl_dump_emitted_mi_name_fallback(void *s)
{
}

JL_DLLEXPORT void jl_dump_llvm_opt_fallback(void *s)
{
}

JL_DLLEXPORT jl_value_t *jl_dump_fptr_asm_fallback(uint64_t fptr, char raw_mc, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_dump_function_asm_fallback(jl_llvmf_dump_t* dump, char raw_mc, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE

JL_DLLEXPORT void jl_get_function_id_fallback(void *native_code, jl_code_instance_t *ncode,
        int32_t *func_idx, int32_t *specfunc_idx) UNAVAILABLE


JL_DLLEXPORT void *jl_get_llvm_function_fallback(void *native_code, uint32_t idx) UNAVAILABLE

JL_DLLEXPORT LLVMOrcThreadSafeModuleRef jl_get_llvm_module_fallback(void *native_code) UNAVAILABLE

JL_DLLEXPORT void *jl_type_to_llvm_fallback(jl_value_t *jt, LLVMContextRef llvmctxt, bool_t *isboxed) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_get_libllvm_fallback(void) JL_NOTSAFEPOINT
{
    return jl_nothing;
}

JL_DLLEXPORT uint64_t jl_getUnwindInfo_fallback(uint64_t dwAddr)
{
    return 0;
}

JL_DLLEXPORT void jl_add_optimization_passes_fallback(void *PM, int opt_level, int lower_intrinsics) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddLowerSimdLoopPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddFinalLowerGCPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddPropagateJuliaAddrspaces_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddRemoveJuliaAddrspacesPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddCombineMulAddPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddMultiVersioningPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddLowerExcHandlersPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddLateLowerGCFramePass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraJuliaLICMPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddAllocOptPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddLowerPTLSPass_fallback(void *PM, bool_t imaging_mode) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddRemoveNIPass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddGCInvariantVerifierPass_fallback(void *PM, bool_t Strong) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddDemoteFloat16Pass_fallback(void *PM) UNAVAILABLE

JL_DLLEXPORT void LLVMExtraAddCPUFeaturesPass_fallback(void *PM) UNAVAILABLE
