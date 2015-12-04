// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef JULIA_INTERNAL_H
#define JULIA_INTERNAL_H

#include <options.h>
#include <uv.h>

#ifdef __cplusplus
extern "C" {
#endif

extern size_t jl_page_size;
extern char *jl_stack_lo;
extern char *jl_stack_hi;
extern jl_function_t *jl_typeinf_func;
#if defined(JL_USE_INTEL_JITEVENTS)
extern unsigned sig_stack_size;
#endif

JL_DLLEXPORT extern int jl_lineno;
JL_DLLEXPORT extern const char *jl_filename;

STATIC_INLINE jl_value_t *newobj(jl_value_t *type, size_t nfields)
{
    jl_value_t *jv = NULL;
    switch (nfields) {
    case 0:
        jv = (jl_value_t*)jl_gc_alloc_0w(); break;
    case 1:
        jv = (jl_value_t*)jl_gc_alloc_1w(); break;
    case 2:
        jv = (jl_value_t*)jl_gc_alloc_2w(); break;
    case 3:
        jv = (jl_value_t*)jl_gc_alloc_3w(); break;
    default:
        jv = (jl_value_t*)jl_gc_allocobj(nfields * sizeof(void*));
    }
    jl_set_typeof(jv, type);
    return jv;
}

STATIC_INLINE jl_value_t *newstruct(jl_datatype_t *type)
{
    jl_value_t *jv = (jl_value_t*)jl_gc_allocobj(type->size);
    jl_set_typeof(jv, type);
    return jv;
}

#define GC_MAX_SZCLASS (2032-sizeof(void*))
// MSVC miscalculates sizeof(jl_taggedvalue_t) because
// empty structs are a GNU extension
#define sizeof_jl_taggedvalue_t (sizeof(void*))
void jl_gc_inhibit_finalizers(int state);
void jl_gc_setmark(jl_value_t *v);
void jl_gc_sync_total_bytes(void);
void jl_gc_track_malloced_array(jl_array_t *a);
void jl_gc_count_allocd(size_t sz);
void jl_gc_run_all_finalizers(void);
void *allocb(size_t sz);

void gc_queue_binding(jl_binding_t *bnd);
void gc_setmark_buf(void *buf, int);

static inline void jl_gc_wb_binding(jl_binding_t *bnd, void *val) // val isa jl_value_t*
{
    if (__unlikely((jl_astaggedvalue(bnd)->gc_bits & 1) == 1 &&
                   (jl_astaggedvalue(val)->gc_bits & 1) == 0))
        gc_queue_binding(bnd);
}

static inline void jl_gc_wb_buf(void *parent, void *bufptr) // parent isa jl_value_t*
{
    // if parent is marked and buf is not
    if (__unlikely((jl_astaggedvalue(parent)->gc_bits & 1) == 1))
        //            (jl_astaggedvalue(bufptr)->gc_bits) != 1))
        gc_setmark_buf(bufptr, jl_astaggedvalue(parent)->gc_bits);
}

#ifdef GC_DEBUG_ENV
void gc_debug_print_status(void);
#else
#define gc_debug_print_status()
#endif
#if defined(GC_FINAL_STATS)
void jl_print_gc_stats(JL_STREAM *s);
#else
#define jl_print_gc_stats(s) ((void)s)
#endif
int jl_assign_type_uid(void);
jl_value_t *jl_cache_type_(jl_datatype_t *type);
int  jl_get_t_uid_ctr(void);
void jl_set_t_uid_ctr(int i);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

void JL_NORETURN jl_no_method_error_bare(jl_function_t *f, jl_value_t *args);
void JL_NORETURN jl_no_method_error(jl_function_t *f, jl_value_t **args,
                                    size_t na);
JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t);

#define JL_CALLABLE(name)                                               \
    JL_DLLEXPORT jl_value_t *name(jl_value_t *F, jl_value_t **args, uint32_t nargs)

JL_CALLABLE(jl_trampoline);
JL_CALLABLE(jl_apply_generic);
JL_CALLABLE(jl_unprotect_stack);
JL_CALLABLE(jl_f_no_function);
JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_intrinsic_call);
extern jl_function_t *jl_unprotect_stack_func;
extern jl_function_t *jl_bottom_func;
void jl_install_default_signal_handlers(void);
void restore_signals(void);
void *jl_install_thread_signal_handler(void);

extern jl_datatype_t *jl_box_type;
extern jl_value_t *jl_box_any_type;
extern jl_typename_t *jl_box_typename;

STATIC_INLINE int jl_is_box(void *v)
{
    jl_value_t *t = jl_typeof(v);
    return (jl_is_datatype(t) &&
            ((jl_datatype_t*)(t))->name == jl_box_typename);
}

ssize_t jl_max_jlgensym_in(jl_value_t *v);

extern uv_loop_t *jl_io_loop;

JL_DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle,
                                            jl_value_t *data);
JL_DLLEXPORT int jl_uv_fs_result(uv_fs_t *f);


int jl_tuple_subtype(jl_value_t **child, size_t cl, jl_datatype_t *pdt, int ta);

int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta);
jl_value_t *jl_type_match(jl_value_t *a, jl_value_t *b);
extern int type_match_invariance_mask;
jl_value_t *jl_type_match_morespecific(jl_value_t *a, jl_value_t *b);
int jl_types_equal_generic(jl_value_t *a, jl_value_t *b, int useenv);
jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np);
jl_datatype_t *jl_inst_concrete_tupletype(jl_svec_t *p);
jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_function_t *method);
jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_function_t *method, jl_svec_t *tvars,
                                      int8_t isstaged);
int jl_is_type(jl_value_t *v);
jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b,
                                          jl_svec_t **penv, jl_svec_t *tvars);
jl_typector_t *jl_new_type_ctor(jl_svec_t *params, jl_value_t *body);
jl_value_t *jl_apply_type_(jl_value_t *tc, jl_value_t **params, size_t n);
jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n);
jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super,
                                   jl_svec_t *parameters);
jl_datatype_t *jl_wrap_Type(jl_value_t *t);  // x -> Type{x}
jl_datatype_t *jl_wrap_vararg(jl_value_t *t);
void jl_assign_bits(void *dest, jl_value_t *bits);
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module);
void jl_add_method(jl_function_t *gf, jl_tupletype_t *types, jl_function_t *meth,
                   jl_svec_t *tvars, int8_t isstaged);
jl_function_t *jl_module_call_func(jl_module_t *m);
int jl_is_submodule(jl_module_t *child, jl_module_t *parent);
int jl_start_parsing_file(const char *fname);
void jl_stop_parsing(void);
jl_value_t *jl_parse_next(void);
jl_lambda_info_t *jl_wrap_expr(jl_value_t *expr);
void jl_compile_linfo(jl_lambda_info_t *li);
jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e);
jl_value_t *jl_parse_eval_all(const char *fname, size_t len);
jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam);
jl_value_t *jl_interpret_toplevel_thunk_with(jl_lambda_info_t *lam,
                                             jl_value_t **loc, size_t nl);
jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e);
jl_value_t *jl_static_eval(jl_value_t *ex, void *ctx_, jl_module_t *mod,
                           jl_value_t *sp, jl_expr_t *ast, int sparams,
                           int allow_alloc);
int jl_is_toplevel_only_expr(jl_value_t *e);
void jl_type_infer(jl_lambda_info_t *li, jl_tupletype_t *argtypes,
                   jl_lambda_info_t *def);

jl_function_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tupletype_t *types,
                                        int cache, int inexact);
jl_function_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache);
jl_value_t *jl_gf_invoke(jl_function_t *gf, jl_tupletype_t *types,
                         jl_value_t **args, size_t nargs);
jl_sym_t *jl_decl_var(jl_value_t *ex);

jl_array_t *jl_lam_args(jl_expr_t *l);
jl_array_t *jl_lam_vinfo(jl_expr_t *l);
jl_array_t *jl_lam_capt(jl_expr_t *l);
jl_value_t *jl_lam_gensyms(jl_expr_t *l);
jl_array_t *jl_lam_staticparams(jl_expr_t *l);
jl_sym_t *jl_lam_argname(jl_lambda_info_t *li, int i);
int jl_lam_vars_captured(jl_expr_t *ast);
jl_expr_t *jl_lam_body(jl_expr_t *l);
int jl_in_vinfo_array(jl_array_t *a, jl_sym_t *v);
int jl_local_in_ast(jl_expr_t *ast, jl_sym_t *sym);

void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super);
void jl_add_constructors(jl_datatype_t *t);

jl_value_t *jl_nth_slot_type(jl_tupletype_t *sig, size_t i);
void jl_compute_field_offsets(jl_datatype_t *st);
jl_array_t *jl_new_array_for_deserialization(jl_value_t *atype, uint32_t ndims, size_t *dims,
                                             int isunboxed, int elsz);
JL_DLLEXPORT jl_value_t *jl_new_box(jl_value_t *v);
jl_lambda_info_t *jl_copy_lambda_info(jl_lambda_info_t *linfo);
extern jl_array_t *jl_module_init_order;

#ifdef JL_USE_INTEL_JITEVENTS
extern char jl_using_intel_jitevents;
#endif
#ifdef JL_USE_OPROFILE_JITEVENTS
extern char jl_using_oprofile_jitevents;
#endif
extern size_t jl_arr_xtralloc_limit;

void jl_init_types(void);
void jl_init_box_caches(void);
void jl_init_frontend(void);
void jl_init_primitives(void);
void jl_init_codegen(void);
void jl_init_intrinsic_functions(void);
void jl_init_tasks(void);
void jl_init_root_task(void *stack, size_t ssize);
void jl_init_serializer(void);
void jl_gc_init(void);
void jl_init_restored_modules(jl_array_t *init_order);

void _julia_init(JL_IMAGE_SEARCH rel);
#ifdef COPY_STACKS
#define jl_stackbase (jl_get_ptls_states()->stackbase)
#endif

void jl_set_stackbase(char *__stk);
void jl_set_base_ctx(char *__stk);

void jl_init_threading(void);
void jl_start_threads(void);
void jl_shutdown_threading(void);
#ifdef JULIA_ENABLE_THREADING
jl_get_ptls_states_func jl_get_ptls_states_getter(void);
#endif

void jl_dump_bitcode(char *fname, const char *sysimg_data, size_t sysimg_len);
void jl_dump_objfile(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len);
int32_t jl_get_llvm_gv(jl_value_t *p);
void jl_idtable_rehash(jl_array_t **pa, size_t newsz);

jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_svec_t *sp, jl_tupletype_t *types);
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tupletype_t *types);
jl_function_t *jl_module_get_initializer(jl_module_t *m);
void jl_generate_fptr(jl_function_t *f);
void jl_fptr_to_llvm(void *fptr, jl_lambda_info_t *lam, int specsig);
jl_tupletype_t *arg_type_tuple(jl_value_t **args, size_t nargs);

jl_value_t* skip_meta(jl_array_t *body);
int has_meta(jl_array_t *body, jl_sym_t *sym);

// backtraces
#ifdef _OS_WINDOWS_
extern HANDLE hMainThread;
typedef CONTEXT *bt_context_t;
DWORD64 jl_getUnwindInfo(ULONG64 dwBase);
extern volatile int jl_in_stackwalk;
#else
#define UNW_LOCAL_ONLY
#include <libunwind.h>
typedef unw_context_t *bt_context_t;
#endif
#define jl_bt_data (jl_get_ptls_states()->bt_data)
#define jl_bt_size (jl_get_ptls_states()->bt_size)
JL_DLLEXPORT size_t rec_backtrace(ptrint_t *data, size_t maxsize);
JL_DLLEXPORT size_t rec_backtrace_ctx(ptrint_t *data, size_t maxsize, bt_context_t ctx);
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(ptrint_t *data, size_t maxsize, bt_context_t ctx);
#endif
JL_DLLEXPORT void jl_raise_debugger(void);
#ifdef _OS_DARWIN_
JL_DLLEXPORT void attach_exception_port(void);
#endif
// Set *name and *filename to either NULL or malloc'd string
void jl_getFunctionInfo(char **name, char **filename, size_t *line,
                        char **inlinedat_file, size_t *inlinedat_line,
                        uintptr_t pointer, int *fromC, int skipC, int skipInline);
JL_DLLEXPORT void gdblookup(ptrint_t ip);

// *to is NULL or malloc'd pointer, from is allowed to be NULL
static inline char *jl_copy_str(char **to, const char *from)
{
    if (!from) {
        free(*to);
        *to = NULL;
        return NULL;
    }
    size_t len = strlen(from) + 1;
    *to = (char*)realloc(*to, len);
    memcpy(*to, from, len);
    return *to;
}

// timers
// Returns time in nanosec
JL_DLLEXPORT uint64_t jl_hrtime(void);

// libuv stuff:
JL_DLLEXPORT extern void *jl_dl_handle;
JL_DLLEXPORT extern void *jl_RTLD_DEFAULT_handle;
#if defined(_OS_WINDOWS_)
JL_DLLEXPORT extern void *jl_exe_handle;
extern void *jl_ntdll_handle;
extern void *jl_kernel32_handle;
extern void *jl_crtdll_handle;
extern void *jl_winsock_handle;
#endif

void *jl_get_library(const char *f_lib);
JL_DLLEXPORT void *jl_load_and_lookup(const char *f_lib, const char *f_name,
                                   void **hnd);
const char *jl_dlfind_win32(const char *name);


// libuv wrappers:
JL_DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path);

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
#define HAVE_CPUID
#endif

#ifdef SEGV_EXCEPTION
extern JL_DLLEXPORT jl_value_t *jl_segv_exception;
#endif

// Runtime intrinsics //
const char* jl_intrinsic_name(int f);

JL_DLLEXPORT jl_value_t *jl_reinterpret(jl_value_t *ty, jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i);
JL_DLLEXPORT jl_value_t *jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *i);

JL_DLLEXPORT jl_value_t *jl_neg_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_add_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_mul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sdiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_udiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_srem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_urem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_smod_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_neg_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_add_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_mul_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_div_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_rem_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_fma_float(jl_value_t *a, jl_value_t *b, jl_value_t *c);
JL_DLLEXPORT jl_value_t *jl_muladd_float(jl_value_t *a, jl_value_t *b, jl_value_t *c);

JL_DLLEXPORT jl_value_t *jl_eq_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ne_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_slt_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ult_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sle_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ule_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_eq_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ne_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_lt_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_le_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_fpiseq(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_fpislt(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_not_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_and_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_or_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_xor_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_shl_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_lshr_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ashr_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_bswap_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_ctpop_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_ctlz_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_cttz_int(jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_sext_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_zext_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_trunc_int(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sitofp(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_uitofp(jl_value_t *ty, jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_fptoui(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptosi(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptrunc(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fpext(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptoui_auto(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_fptosi_auto(jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_checked_fptoui(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_checked_fptosi(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_checked_trunc_sint(jl_value_t *ty, jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_checked_trunc_uint(jl_value_t *ty, jl_value_t *a);

JL_DLLEXPORT jl_value_t *jl_check_top_bit(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_checked_sadd(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_uadd(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_ssub(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_usub(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_smul(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_umul(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_sdiv(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_udiv(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_srem(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_urem(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_nan_dom_err(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_ceil_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_floor_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_trunc_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_rint_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sqrt_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_powi_llvm(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_abs_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_copysign_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_flipsign_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_select_value(jl_value_t *isfalse, jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_arraylen(jl_value_t *a);
int jl_array_store_unboxed(jl_value_t *el_type);
int jl_array_isdefined(jl_value_t **args, int nargs);

JL_DEFINE_MUTEX_EXT(codegen)

#if defined(_OS_WINDOWS_)
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
    return _aligned_malloc(sz ? sz : 1, align);
}
STATIC_INLINE void *jl_realloc_aligned(void *p, size_t sz, size_t oldsz,
                                       size_t align)
{
    (void)oldsz;
    return _aligned_realloc(p, sz ? sz : 1, align);
}
STATIC_INLINE void jl_free_aligned(void *p)
{
    _aligned_free(p);
}
#else
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return malloc(sz);
#endif
    void *ptr;
    if (posix_memalign(&ptr, align, sz))
        return NULL;
    return ptr;
}
STATIC_INLINE void *jl_realloc_aligned(void *d, size_t sz, size_t oldsz,
                                       size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return realloc(d, sz);
#endif
    void *b = jl_malloc_aligned(sz, align);
    if (b != NULL) {
        memcpy(b, d, oldsz > sz ? sz : oldsz);
        free(d);
    }
    return b;
}
STATIC_INLINE void jl_free_aligned(void *p)
{
    free(p);
}
#endif


#ifdef __cplusplus
}
#endif

#endif
