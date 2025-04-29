// This file is a part of Julia. License is MIT: https://julialang.org/license

// This file provides a fallback implementation of the codegen plugin interface,
// used when libjulia-codegen is not available.

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

JL_DLLEXPORT void jl_dump_native_fallback(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname,
        ios_t *z, ios_t *s) UNAVAILABLE
JL_DLLEXPORT void jl_get_llvm_gvs_fallback(void *native_code, arraylist_t *gvs) UNAVAILABLE
JL_DLLEXPORT void jl_get_llvm_external_fns_fallback(void *native_code, arraylist_t *gvs) UNAVAILABLE
JL_DLLEXPORT void jl_get_llvm_mis_fallback(void *native_code, arraylist_t* MIs) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_dump_method_asm_fallback(jl_method_instance_t *linfo, size_t world,
        char emit_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_function_ir_fallback(jl_llvmf_dump_t *dump, char strip_ir_metadata, char dump_module, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT void jl_get_llvmf_defn_fallback(jl_llvmf_dump_t *dump, jl_method_instance_t *linfo, jl_code_info_t *src, char getwrapper, char optimize, const jl_cgparams_t params) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm_fallback(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction_fallback(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

JL_DLLEXPORT void jl_init_codegen_fallback(void) { }

JL_DLLEXPORT int jl_getFunctionInfo_fallback(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline)
{
    return 0;
}

JL_DLLEXPORT void jl_register_fptrs_fallback(uint64_t image_base, const struct _jl_image_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n)
{
    (void)image_base; (void)fptrs; (void)linfos; (void)n;
}

JL_DLLEXPORT void jl_generate_fptr_for_unspecialized_fallback(jl_code_instance_t *unspec)
{
    jl_atomic_store_release(&unspec->invoke, &jl_fptr_interpret_call);
}

JL_DLLEXPORT int jl_compile_codeinst_fallback(jl_code_instance_t *unspec)
{
    // Do nothing. The caller will notice that we failed to provide an ->invoke and trigger
    // appropriate fallbacks.
    return 0;
}

JL_DLLEXPORT void jl_emit_codeinst_to_jit_fallback(jl_code_instance_t *codeinst, jl_code_info_t *src)
{
    jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
    if (jl_is_code_info(inferred))
        return;
    if (jl_is_svec(src->edges)) {
        jl_atomic_store_release(&codeinst->inferred, (jl_value_t*)src->edges);
        jl_gc_wb(codeinst, src->edges);
    }
    jl_atomic_store_release(&codeinst->debuginfo, src->debuginfo);
    jl_gc_wb(codeinst, src->debuginfo);
    jl_atomic_store_release(&codeinst->inferred, (jl_value_t*)src);
    jl_gc_wb(codeinst, src);
}

JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION_fallback(void)
{
    return 0;
}

JL_DLLEXPORT int jl_compile_extern_c_fallback(LLVMOrcThreadSafeModuleRef llvmmod, void *params, void *sysimg, jl_value_t *name, jl_value_t *declrt, jl_value_t *sigt)
{
    // Assume we were able to register the ccallable with the JIT. The
    // fact that we didn't is not observable since we cannot compile
    // anything else.
    return 1;
}

JL_DLLEXPORT void jl_teardown_codegen_fallback(void) JL_NOTSAFEPOINT
{
}

JL_DLLEXPORT size_t jl_jit_total_bytes_fallback(void)
{
    return 0;
}

JL_DLLEXPORT void *jl_create_native_fallback(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmmod, int _trim, int _external_linkage, size_t _world) UNAVAILABLE
JL_DLLEXPORT void *jl_emit_native_fallback(jl_array_t *codeinfos, LLVMOrcThreadSafeModuleRef llvmmod, const jl_cgparams_t *cgparams, int _external_linkage) UNAVAILABLE

JL_DLLEXPORT void jl_dump_compiles_fallback(void *s)
{
}

JL_DLLEXPORT void jl_dump_emitted_mi_name_fallback(void *s)
{
}

JL_DLLEXPORT void jl_dump_llvm_opt_fallback(void *s)
{
}

JL_DLLEXPORT jl_value_t *jl_dump_fptr_asm_fallback(uint64_t fptr, char emit_mc, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_dump_function_asm_fallback(jl_llvmf_dump_t* dump, char emit_mc, const char* asm_variant, const char *debuginfo, char binary, char raw) UNAVAILABLE

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

JL_DLLEXPORT void jl_register_passbuilder_callbacks_fallback(void *PB) { }

//LLVM C api to the julia JIT
JL_DLLEXPORT void* JLJITGetLLVMOrcExecutionSession_fallback(void* JIT) UNAVAILABLE

JL_DLLEXPORT void* JLJITGetJuliaOJIT_fallback(void) UNAVAILABLE

JL_DLLEXPORT void* JLJITGetExternalJITDylib_fallback(void* JIT) UNAVAILABLE

JL_DLLEXPORT void* JLJITAddObjectFile_fallback(void* JIT, void* JD, void* ObjBuffer) UNAVAILABLE

JL_DLLEXPORT void* JLJITAddLLVMIRModule_fallback(void* JIT, void* JD, void* TSM) UNAVAILABLE

JL_DLLEXPORT void* JLJITLookup_fallback(void* JIT, void* Result, const char *Name) UNAVAILABLE

JL_DLLEXPORT void* JLJITMangleAndIntern_fallback(void* JIT, const char *Name) UNAVAILABLE

JL_DLLEXPORT const char *JLJITGetTripleString_fallback(void* JIT) UNAVAILABLE

JL_DLLEXPORT const char JLJITGetGlobalPrefix_fallback(void* JIT) UNAVAILABLE

JL_DLLEXPORT const char *JLJITGetDataLayoutString_fallback(void* JIT) UNAVAILABLE

JL_DLLEXPORT void* JLJITGetIRCompileLayer_fallback(void* JIT) UNAVAILABLE
