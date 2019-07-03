// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

int globalUnique = 0;
jl_cgparams_t jl_default_cgparams = {1, 1, 1, 1, 0, NULL, NULL, NULL, NULL, NULL};

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

void jl_dump_native(const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
int32_t jl_get_llvm_gv(jl_value_t *p) UNAVAILABLE
void jl_write_malloc_log(void) UNAVAILABLE
void jl_write_coverage_data(void) UNAVAILABLE

JL_DLLEXPORT void jl_clear_malloc_data(void) UNAVAILABLE
JL_DLLEXPORT void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
JL_DLLEXPORT void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt) UNAVAILABLE
JL_DLLEXPORT const jl_value_t *jl_dump_function_asm(void *f, int raw_mc, const char* asm_variant, const char *debuginfo) UNAVAILABLE
JL_DLLEXPORT const jl_value_t *jl_dump_function_ir(void *f, uint8_t strip_ir_metadata, uint8_t dump_module, const char *debuginfo) UNAVAILABLE

JL_DLLEXPORT void *jl_LLVMCreateDisasm(const char *TripleName, void *DisInfo, int TagType, void *GetOpInfo, void *SymbolLookUp) UNAVAILABLE
JL_DLLEXPORT size_t jl_LLVMDisasmInstruction(void *DC, uint8_t *Bytes, uint64_t BytesSize, uint64_t PC, char *OutString, size_t OutStringSize) UNAVAILABLE

int32_t jl_assign_functionID(const char *fname) UNAVAILABLE

void jl_init_codegen(void) { }
void jl_fptr_to_llvm(void *fptr, jl_code_instance_t *codeinst, int spec_abi) { }

int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline)
{
    return 0;
}

void jl_register_fptrs(uint64_t sysimage_base, const struct _jl_sysimg_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n)
{
    (void)sysimage_base; (void)fptrs; (void)linfos; (void)n;
}

jl_code_instance_t *jl_compile_linfo(
        jl_method_instance_t *li JL_PROPAGATES_ROOT,
        jl_code_info_t *src JL_MAYBE_UNROOTED,
        size_t world,
        const jl_cgparams_t *params)
{
    if (jl_is_method(li->def.method)) {
        jl_printf(JL_STDERR, "code missing for ");
        jl_static_show(JL_STDERR, (jl_value_t*)li);
        jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
    }
    else {
        jl_printf(JL_STDERR, "top level expression cannot be compiled in this build of Julia");
    }
    return NULL;
}

void jl_generate_fptr(jl_code_instance_t *codeinst)
{
    return;
}

JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION(void)
{
    return 0;
}

jl_array_t *jl_cfunction_list;
