// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

void JL_NORETURN nort_errorf(const char *fmt, const char *fname) {
    fprintf(stderr, fmt, fname);
    abort();
}

#define UNAVAILABLE { nort_errorf("%s: not available in this build of Julia\n", __func__); }

jl_cgparams_t jl_default_cgparams;

JL_DLLEXPORT void jl_dump_native(void *native_code, const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
JL_DLLEXPORT int32_t jl_get_llvm_gv(void *native_code, jl_value_t *p) UNAVAILABLE
JL_DLLEXPORT void jl_write_malloc_log(void) UNAVAILABLE
JL_DLLEXPORT void jl_write_coverage_data(const char *data) UNAVAILABLE

JL_DLLEXPORT void jl_clear_malloc_data(void) UNAVAILABLE
JL_DLLEXPORT void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
JL_DLLEXPORT void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_method_asm(jl_method_instance_t *linfo, size_t world, int raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT jl_value_t *jl_dump_function_ir(void *f, char strip_ir_metadata, char dump_module, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT void *jl_get_llvmf_defn(jl_method_instance_t *linfo, size_t world, char getwrapper, char optimize, const jl_cgparams_t params) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

int32_t jl_assign_functionID(const char *fname) UNAVAILABLE

JL_DLLEXPORT void jl_init_codegen(void) UNAVAILABLE //{ }

JL_DLLEXPORT int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline) UNAVAILABLE
/*
{
    return 0;
}
*/

JL_DLLEXPORT void jl_register_fptrs(uint64_t sysimage_base, const struct _jl_sysimg_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n) UNAVAILABLE
/*
{
    (void)sysimage_base; (void)fptrs; (void)linfos; (void)n;
}
*/

JL_DLLEXPORT jl_code_instance_t *jl_generate_fptr(jl_method_instance_t *mi, size_t world) UNAVAILABLE
JL_DLLEXPORT void jl_generate_fptr_for_unspecialized(jl_code_instance_t *unspec) UNAVAILABLE


JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION(void) UNAVAILABLE
/*
{
    return 0;
}
*/

JL_DLLEXPORT void jl_teardown_codegen(void) UNAVAILABLE //{ }
JL_DLLEXPORT void jl_lock_profile(void) UNAVAILABLE //{ }
JL_DLLEXPORT void jl_unlock_profile(void) UNAVAILABLE //{ }

// FIXME: Generate in build system
#ifndef JL_LLVM_VERSION
#define JL_LLVM_VERSION 120000
#endif // JL_LLVM_VERSION
