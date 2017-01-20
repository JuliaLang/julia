// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

int globalUnique = 0;

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

void jl_dump_native(const char *bc_fname, const char *obj_fname, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
void jl_dump_objfile(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
int32_t jl_get_llvm_gv(jl_value_t *p) UNAVAILABLE
void jl_write_malloc_log(void) UNAVAILABLE
void jl_write_coverage_data(void) UNAVAILABLE

JL_DLLEXPORT void jl_clear_malloc_data(void) UNAVAILABLE
JL_DLLEXPORT void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
JL_DLLEXPORT void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt) UNAVAILABLE
JL_DLLEXPORT const jl_value_t *jl_dump_function_asm(void *f, int raw_mc) UNAVAILABLE
JL_DLLEXPORT const jl_value_t *jl_dump_function_ir(void *f, uint8_t strip_ir_metadata, uint8_t dump_module) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

void jl_init_codegen(void) { }
void jl_fptr_to_llvm(jl_fptr_t fptr, jl_method_instance_t *lam, int specsig)
{
    if (!specsig)
        lam->fptr = fptr;
}

int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline)
{
    return 0;
}

void jl_register_fptrs(uint64_t sysimage_base, void **fptrs, jl_method_instance_t **linfos, size_t n)
{
    (void)sysimage_base; (void)fptrs; (void)linfos; (void)n;
}

void jl_compile_linfo(jl_method_instance_t *li) { }

jl_value_t *jl_interpret_call(jl_method_instance_t *lam, jl_value_t **args, uint32_t nargs);
void jl_generate_fptr(jl_method_instance_t *li)
{
    li->fptr = (jl_fptr_t)&jl_interpret_call;
    li->jlcall_api = 4;
}

JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION(void)
{
    return 0;
}
