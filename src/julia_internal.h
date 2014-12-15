#ifndef JULIA_INTERNAL_H
#define JULIA_INTERNAL_H

#include "options.h"
#include "uv.h"

#ifdef __cplusplus
extern "C" {
#endif

STATIC_INLINE jl_value_t *newobj(jl_value_t *type, size_t nfields)
{
    jl_value_t *jv = (jl_value_t*)allocobj((1+nfields) * sizeof(void*));
    jv->type = type;
    return jv;
}

STATIC_INLINE jl_value_t *newstruct(jl_datatype_t *type)
{
    jl_value_t *jv = (jl_value_t*)allocobj(sizeof(void*) + type->size);
    jv->type = (jl_value_t*)type;
    return jv;
}

int jl_assign_type_uid(void);
jl_value_t *jl_cache_type_(jl_datatype_t *type);
int  jl_get_t_uid_ctr(void);
void jl_set_t_uid_ctr(int i);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);


#define JL_CALLABLE(name) \
    DLLEXPORT jl_value_t *name(jl_value_t *F, jl_value_t **args, uint32_t nargs)

JL_CALLABLE(jl_trampoline);
JL_CALLABLE(jl_apply_generic);
JL_CALLABLE(jl_unprotect_stack);
JL_CALLABLE(jl_f_no_function);
JL_CALLABLE(jl_f_tuple);
extern jl_function_t *jl_unprotect_stack_func;
extern jl_function_t *jl_bottom_func;

extern jl_datatype_t *jl_box_type;
extern jl_value_t *jl_box_any_type;
extern jl_typename_t *jl_box_typename;

STATIC_INLINE int jl_is_box(void *v)
{
    jl_value_t *t = jl_typeof(v);
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_box_typename);
}


extern uv_loop_t *jl_io_loop;

DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle, jl_value_t *data);
DLLEXPORT int jl_uv_fs_result(uv_fs_t *f);


int jl_tuple_subtype(jl_value_t **child, size_t cl,
                     jl_value_t **parent, size_t pl, int ta);

int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta);
jl_value_t *jl_type_match(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_type_match_morespecific(jl_value_t *a, jl_value_t *b);
int jl_types_equal_generic(jl_value_t *a, jl_value_t *b, int useenv);

void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super);
void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name);
void jl_add_constructors(jl_datatype_t *t);

void jl_compute_field_offsets(jl_datatype_t *st);
jl_array_t *jl_new_array_for_deserialization(jl_value_t *atype, uint32_t ndims, size_t *dims,
                                             int isunboxed, int elsz);
#ifdef JL_USE_INTEL_JITEVENTS
extern char jl_using_intel_jitevents;
#endif
extern size_t jl_arr_xtralloc_limit;

void jl_init_types(void);
void jl_init_box_caches(void);
void jl_init_frontend(void);
void jl_init_primitives(void);
void jl_init_codegen(void);
void jl_init_intrinsic_functions(void);
void jl_init_tasks(void *stack, size_t ssize);
void jl_init_serializer(void);
void _julia_init(JL_IMAGE_SEARCH rel);
#ifdef COPY_STACKS
extern void *jl_stackbase;
#endif

void jl_dump_bitcode(char *fname);
void jl_dump_objfile(char *fname, int jit_model);
int32_t jl_get_llvm_gv(jl_value_t *p);

#ifdef _OS_LINUX_
DLLEXPORT void jl_read_sonames(void);
#endif

jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp);
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);
jl_function_t *jl_module_get_initializer(jl_module_t *m);
void jl_generate_fptr(jl_function_t *f);
void jl_fptr_to_llvm(void *fptr, jl_lambda_info_t *lam, int specsig);

// backtraces
#ifdef _OS_WINDOWS_
extern volatile HANDLE hMainThread;
typedef CONTEXT *bt_context_t;
DWORD64 jl_getUnwindInfo(ULONG64 dwBase);
extern volatile int jl_in_stackwalk;
#else
#define UNW_LOCAL_ONLY
#include <libunwind.h>
typedef unw_context_t *bt_context_t;
#endif
#define MAX_BT_SIZE 80000
extern ptrint_t bt_data[MAX_BT_SIZE+1];
extern size_t bt_size;
DLLEXPORT size_t rec_backtrace(ptrint_t *data, size_t maxsize);
DLLEXPORT size_t rec_backtrace_ctx(ptrint_t *data, size_t maxsize, bt_context_t ctx);
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(ptrint_t *data, size_t maxsize, bt_context_t ctx);
#endif

#ifndef _OS_WINDOWS_
DLLEXPORT void jl_raise_debugger(void);
#endif

// timers
// Returns time in nanosec
DLLEXPORT uint64_t jl_hrtime(void);

// libuv stuff:
DLLEXPORT extern uv_lib_t *jl_dl_handle;
DLLEXPORT extern uv_lib_t *jl_RTLD_DEFAULT_handle;
#if defined(_OS_WINDOWS_)
DLLEXPORT extern uv_lib_t *jl_exe_handle;
extern uv_lib_t *jl_ntdll_handle;
extern uv_lib_t *jl_kernel32_handle;
extern uv_lib_t *jl_crtdll_handle;
extern uv_lib_t *jl_winsock_handle;
#endif

DLLEXPORT void jl_atexit_hook();

#ifdef __cplusplus
}
#endif

#endif
