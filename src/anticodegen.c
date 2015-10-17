#include "julia.h"
#include "julia_internal.h"

#include "intrinsics.h"

int globalUnique = 0;

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

void jl_dump_bitcode(char *fname, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
void jl_dump_objfile(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len) UNAVAILABLE
int32_t jl_get_llvm_gv(jl_value_t *p) UNAVAILABLE
void jl_write_malloc_log(void) UNAVAILABLE
void jl_write_coverage_data(void) UNAVAILABLE
void jl_generate_fptr(jl_function_t *f) {
    jl_lambda_info_t *li = f->linfo;
    if (li->fptr == &jl_trampoline) UNAVAILABLE
    f->fptr = li->fptr;
}

DLLEXPORT void jl_clear_malloc_data(void) UNAVAILABLE
DLLEXPORT void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name) UNAVAILABLE
DLLEXPORT void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt) UNAVAILABLE
DLLEXPORT const jl_value_t *jl_dump_function_asm(void *f, int raw_mc) UNAVAILABLE
DLLEXPORT const jl_value_t *jl_dump_function_ir(void *f, uint8_t strip_ir_metadata, uint8_t dump_module) UNAVAILABLE

void jl_init_codegen(void) { }
void jl_compile(jl_function_t *f) { }
void jl_fptr_to_llvm(void *fptr, jl_lambda_info_t *lam, int specsig)
{
    if (!specsig)
        lam->fptr = (jl_fptr_t)fptr;
}
void jl_getFunctionInfo(char **name, char **filename, size_t *line,
                        char **inlinedat_file, size_t *inlinedat_line,
                        size_t pointer, int *fromC, int skipC, int skipInline)
{
    *name = NULL;
    *line = -1;
    *filename = NULL;
    *inlinedat_file = NULL;
    *inlinedat_line = -1;
    *fromC = 0;
}

jl_value_t *jl_static_eval(jl_value_t *ex, void *ctx_, jl_module_t *mod,
                           jl_value_t *sp, jl_expr_t *ast, int sparams, int allow_alloc)
{
    return NULL;
}
