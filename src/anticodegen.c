// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

void jl_dump_native(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname,
        const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
int32_t jl_get_llvm_gv(void *native_code, jl_value_t *p) UNAVAILABLE
void jl_write_malloc_log(void) UNAVAILABLE
void jl_write_coverage_data(const char *output) UNAVAILABLE

JL_DLLEXPORT void jl_clear_malloc_data(void) UNAVAILABLE
JL_DLLEXPORT int jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
JL_DLLEXPORT void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_method_asm(jl_method_instance_t *linfo, size_t world,
        char raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_function_ir(void *f, char strip_ir_metadata, char dump_module, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT void *jl_get_llvmf_defn(jl_method_instance_t *linfo, size_t world, char getwrapper, char optimize, const jl_cgparams_t params) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

int32_t jl_assign_functionID(const char *fname) UNAVAILABLE

void jl_init_codegen(void) { }

int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline)
{
    return 0;
}

void jl_register_fptrs(uint64_t sysimage_base, const struct _jl_sysimg_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n)
{
    (void)sysimage_base; (void)fptrs; (void)linfos; (void)n;
}

jl_code_instance_t *jl_generate_fptr(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world)
{
    return NULL;
}

void jl_generate_fptr_for_unspecialized(jl_code_instance_t *unspec)
{
}

JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION(void)
{
    return 0;
}

int jl_compile_extern_c(void *llvmmod, void *params, void *sysimg, jl_value_t *declrt, jl_value_t *sigt)
{
    return 0;
}

void jl_teardown_codegen(void)
{
}

void jl_lock_profile(void)
{
}

void jl_unlock_profile(void)
{
}

JL_DLLEXPORT void *jl_create_native(jl_array_t *methods, const jl_cgparams_t *cgparams, int _policy) UNAVAILABLE

JL_DLLEXPORT void jl_dump_compiles(void *s)
{
}

JL_DLLEXPORT jl_value_t *jl_dump_fptr_asm(uint64_t fptr, char raw_mc, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_dump_function_asm(void *F, char raw_mc, const char* asm_variant, const char *debuginfo, char binary) UNAVAILABLE

JL_DLLEXPORT void jl_get_function_id(void *native_code, jl_code_instance_t *ncode,
        int32_t *func_idx, int32_t *specfunc_idx) UNAVAILABLE

JL_DLLEXPORT void *jl_get_llvm_context(void *native_code) UNAVAILABLE

JL_DLLEXPORT void *jl_get_llvm_function(void *native_code, uint32_t idx) UNAVAILABLE

JL_DLLEXPORT void *jl_get_llvm_module(void *native_code) UNAVAILABLE

JL_DLLEXPORT void *jl_type_to_llvm(jl_value_t *jt, bool_t *isboxed) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_get_libllvm(void) JL_NOTSAFEPOINT
{
    return jl_nothing;
}
