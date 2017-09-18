// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_INTERNAL_H
#define JL_INTERNAL_H

#include "options.h"
#include <uv.h>
#if !defined(_MSC_VER) && !defined(__MINGW32__)
#include <unistd.h>
#include <sched.h>
#else
#define sleep(x) Sleep(1000*x)
#endif

#ifdef __has_builtin
#  define jl_has_builtin(x) __has_builtin(x)
#else
#  define jl_has_builtin(x) 0
#endif

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define JL_ASAN_ENABLED     // Clang flavor
#endif
#elif defined(__SANITIZE_ADDRESS__)
#define JL_ASAN_ENABLED     // GCC flavor
#endif

#if defined(__has_feature)
#if __has_feature(memory_sanitizer)
#define JL_MSAN_ENABLED
#endif
#endif

// Remove when C11 is required for C code.
#ifndef static_assert
#  ifndef __cplusplus
// C11 should already have `static_assert` from `<assert.h>` so there's no need
// to check C version.
#    ifdef __GNUC__
#      define static_assert _Static_assert
#    else
#      define static_assert(...)
#    endif
#  endif
// For C++, C++11 or MSVC is required. Both provide `static_assert`.
#endif

#if jl_has_builtin(__builtin_assume)
#define jl_assume(cond) (__extension__ ({               \
                __typeof__(cond) cond_ = (cond);        \
                __builtin_assume(!!(cond_));            \
                cond_;                                  \
            }))
#elif defined(_COMPILER_MICROSOFT_) && defined(__cplusplus)
template<typename T>
static inline T
jl_assume(T v)
{
    __assume(!!v);
    return v;
}
#elif defined(_COMPILER_INTEL_)
#define jl_assume(cond) (__extension__ ({               \
                __typeof__(cond) cond_ = (cond);        \
                __assume(!!(cond_));                    \
                cond_;                                  \
            }))
#elif defined(__GNUC__)
static inline void jl_assume_(int cond)
{
    if (!cond) {
        __builtin_unreachable();
    }
}
#define jl_assume(cond) (__extension__ ({               \
                __typeof__(cond) cond_ = (cond);        \
                jl_assume_(!!(cond_));                  \
                cond_;                                  \
            }))
#else
#define jl_assume(cond) (cond)
#endif

#if defined(__GLIBC__) && defined(JULIA_HAS_IFUNC_SUPPORT)
// Make sure both the compiler and the glibc supports it.
// Only enable this on known working glibc versions.
#  if (defined(_CPU_X86_) || defined(_CPU_X86_64_)) && __GLIBC_PREREQ(2, 12)
#    define JL_USE_IFUNC 1
#  elif (defined(_CPU_ARM_) || defined(_CPU_AARCH64_)) && __GLIBC_PREREQ(2, 18)
// This is the oldest tested version that supports ifunc.
#    define JL_USE_IFUNC 1
#  endif
// TODO: PPC probably supports ifunc on some glibc versions too
#endif
// Make sure JL_USE_IFUNC is always defined to catch include errors.
#ifndef JL_USE_IFUNC
#  define JL_USE_IFUNC 0
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "timing.h"

#ifdef _COMPILER_MICROSOFT_
#  define jl_return_address() ((uintptr_t)_ReturnAddress())
#else
#  define jl_return_address() ((uintptr_t)__builtin_return_address(0))
#endif

STATIC_INLINE uint32_t jl_int32hash_fast(uint32_t a)
{
//    a = (a+0x7ed55d16) + (a<<12);
//    a = (a^0xc761c23c) ^ (a>>19);
//    a = (a+0x165667b1) + (a<<5);
//    a = (a+0xd3a2646c) ^ (a<<9);
//    a = (a+0xfd7046c5) + (a<<3);
//    a = (a^0xb55a4f09) ^ (a>>16);
    return a;  // identity hashing seems to work well enough here
}

#define GC_CLEAN  0 // freshly allocated
#define GC_MARKED 1 // reachable and young
#define GC_OLD    2 // if it is reachable it will be marked as old
#define GC_OLD_MARKED (GC_OLD | GC_MARKED) // reachable and old

// useful constants
extern jl_methtable_t *jl_type_type_mt;
JL_DLLEXPORT extern size_t jl_world_counter;

typedef void (*tracer_cb)(jl_value_t *tracee);
void jl_call_tracer(tracer_cb callback, jl_value_t *tracee);

extern size_t jl_page_size;
extern jl_function_t *jl_typeinf_func;
extern size_t jl_typeinf_world;

JL_DLLEXPORT extern int jl_lineno;
JL_DLLEXPORT extern const char *jl_filename;

JL_DLLEXPORT jl_value_t *jl_gc_pool_alloc(jl_ptls_t ptls, int pool_offset,
                                          int osize);
JL_DLLEXPORT jl_value_t *jl_gc_big_alloc(jl_ptls_t ptls, size_t allocsz);
int jl_gc_classify_pools(size_t sz, int *osize);
extern jl_mutex_t gc_perm_lock;
void *jl_gc_perm_alloc_nolock(size_t sz, int zero, unsigned align, unsigned offset);
void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align, unsigned offset);
void jl_gc_force_mark_old(jl_ptls_t ptls, jl_value_t *v);
void gc_sweep_sysimg(void);


// pools are 16376 bytes large (GC_POOL_SZ - GC_PAGE_OFFSET)
static const int jl_gc_sizeclasses[JL_GC_N_POOLS] = {
#ifdef _P64
    8,
#elif defined(_CPU_ARM_) || defined(_CPU_PPC_)
    // ARM and PowerPC have max alignment of 8,
    // make sure allocation of size 8 has that alignment.
    4, 8,
#else
    4, 8, 12,
#endif

    // 16 pools at 16-byte spacing
    16, 32, 48, 64, 80, 96, 112, 128,
    144, 160, 176, 192, 208, 224, 240, 256,

    // the following tables are computed for maximum packing efficiency via the formula:
    // sz=(div(2^14-8,rng)÷16)*16; hcat(sz, (2^14-8)÷sz, 2^14-(2^14-8)÷sz.*sz)'

    // rng = 60:-4:32 (8 pools)
    272, 288, 304, 336, 368, 400, 448, 496,
//   60,  56,  53,  48,  44,  40,  36,  33, /pool
//   64, 256, 272, 256, 192, 384, 256,  16, bytes lost

    // rng = 30:-2:16 (8 pools)
    544, 576, 624, 672, 736, 816, 896, 1008,
//   30,  28,  26,  24,  22,  20,  18,  16, /pool
//   64, 256, 160, 256, 192,  64, 256, 256, bytes lost

    // rng = 15:-1:8 (8 pools)
    1088, 1168, 1248, 1360, 1488, 1632, 1808, 2032
//    15,   14,   13,   12,   11,   10,    9,    8, /pool
//    64,   32,  160,   64,   16,   64,  112,  128, bytes lost
};

STATIC_INLINE int jl_gc_alignment(size_t sz)
{
    if (sz == 0)
        return sizeof(void*);
#ifdef _P64
    (void)sz;
    return 16;
#elif defined(_CPU_ARM_) || defined(_CPU_PPC_)
    return sz <= 4 ? 8 : 16;
#else
    // szclass 8
    if (sz <= 4)
        return 8;
    // szclass 12
    if (sz <= 8)
        return 4;
    // szclass 16+
    return 16;
#endif
}
JL_DLLEXPORT int jl_alignment(size_t sz);

STATIC_INLINE int JL_CONST_FUNC jl_gc_szclass(size_t sz)
{
#ifdef _P64
    if (sz <=    8)
        return 0;
    const int N = 0;
#elif defined(_CPU_ARM_) || defined(_CPU_PPC_)
    if (sz <=    8)
        return (sz + 3) / 4 - 1;
    const int N = 1;
#else
    if (sz <=   12)
        return (sz + 3) / 4 - 1;
    const int N = 2;
#endif
    if (sz <=  256)
        return (sz + 15) / 16 + N;
    if (sz <=  496)
        return 16 - 16376 / 4 / LLT_ALIGN(sz, 16 * 4) + 16 + N;
    if (sz <= 1008)
        return 16 - 16376 / 2 / LLT_ALIGN(sz, 16 * 2) + 24 + N;
    return     16 - 16376 / 1 / LLT_ALIGN(sz, 16 * 1) + 32 + N;
}

#ifdef __GNUC__
#  define jl_is_constexpr(e) __builtin_constant_p(e)
#else
#  define jl_is_constexpr(e) (0)
#endif
#define JL_SMALL_BYTE_ALIGNMENT 16
#define JL_CACHE_BYTE_ALIGNMENT 64
// JL_HEAP_ALIGNMENT is the maximum alignment that the GC can provide
#define JL_HEAP_ALIGNMENT JL_SMALL_BYTE_ALIGNMENT
#define GC_MAX_SZCLASS (2032-sizeof(void*))

STATIC_INLINE jl_value_t *jl_gc_alloc_(jl_ptls_t ptls, size_t sz, void *ty)
{
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    if (allocsz < sz) // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    jl_value_t *v;
    if (allocsz <= GC_MAX_SZCLASS + sizeof(jl_taggedvalue_t)) {
        int pool_id = jl_gc_szclass(allocsz);
        jl_gc_pool_t *p = &ptls->heap.norm_pools[pool_id];
        int osize;
        if (jl_is_constexpr(allocsz)) {
            osize = jl_gc_sizeclasses[pool_id];
        }
        else {
            osize = p->osize;
        }
        v = jl_gc_pool_alloc(ptls, (char*)p - (char*)ptls, osize);
    }
    else {
        v = jl_gc_big_alloc(ptls, allocsz);
    }
    jl_set_typeof(v, ty);
    return v;
}
JL_DLLEXPORT jl_value_t *jl_gc_alloc(jl_ptls_t ptls, size_t sz, void *ty);
// On GCC, only inline when sz is constant
#ifdef __GNUC__
#  define jl_gc_alloc(ptls, sz, ty)                             \
    (__builtin_constant_p(sz) ? jl_gc_alloc_(ptls, sz, ty) :    \
     (jl_gc_alloc)(ptls, sz, ty))
#else
#  define jl_gc_alloc(ptls, sz, ty) jl_gc_alloc_(ptls, sz, ty)
#endif

#define jl_buff_tag ((uintptr_t)0x4eade800)
STATIC_INLINE void *jl_gc_alloc_buf(jl_ptls_t ptls, size_t sz)
{
    return jl_gc_alloc(ptls, sz, (void*)jl_buff_tag);
}

STATIC_INLINE jl_value_t *jl_gc_permobj(size_t sz, void *ty)
{
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    unsigned align = (sz == 0 ? sizeof(void*) : (allocsz <= sizeof(void*) * 2 ?
                                                 sizeof(void*) * 2 : 16));
    jl_taggedvalue_t *o = (jl_taggedvalue_t*)jl_gc_perm_alloc(allocsz, 0, align,
                                                              sizeof(void*) % align);
    uintptr_t tag = (uintptr_t)ty;
    o->header = tag | GC_OLD_MARKED;
    return jl_valueof(o);
}
jl_value_t *jl_permbox8(jl_datatype_t *t, int8_t x);
jl_value_t *jl_permbox16(jl_datatype_t *t, int16_t x);
jl_value_t *jl_permbox32(jl_datatype_t *t, int32_t x);
jl_value_t *jl_permbox64(jl_datatype_t *t, int64_t x);
jl_svec_t *jl_perm_symsvec(size_t n, ...);

// Returns a int32 where the high 16 bits are a lower bound of the number of non-pointer fields
// at the beginning of the type and the low 16 bits are a lower bound on the number of non-pointer
// fields at the end of the type. This field only exists for a layout that has at least one
// pointer fields.
#define jl_datatype_layout_n_nonptr(layout) ((uint32_t*)(layout))[-1]

jl_value_t *jl_gc_realloc_string(jl_value_t *s, size_t sz);

jl_code_info_t *jl_type_infer(jl_method_instance_t **li, size_t world, int force);
jl_generic_fptr_t jl_generate_fptr(jl_method_instance_t *li, const char *F, size_t world);
jl_llvm_functions_t jl_compile_linfo(
        jl_method_instance_t **pli,
        jl_code_info_t *src,
        size_t world,
        const jl_cgparams_t *params);
jl_llvm_functions_t jl_compile_for_dispatch(jl_method_instance_t **li, size_t world);
JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types);
jl_code_info_t *jl_new_code_info_from_ast(jl_expr_t *ast);

STATIC_INLINE jl_value_t *jl_compile_method_internal(jl_generic_fptr_t *fptr,
                                                     jl_method_instance_t *meth)
{
    if (meth->jlcall_api == 2)
        return jl_assume(meth->inferred_const);
    fptr->fptr = meth->fptr;
    fptr->jlcall_api = meth->jlcall_api;
    if (__unlikely(fptr->fptr == NULL || fptr->jlcall_api == 0)) {
        size_t world = jl_get_ptls_states()->world_age;
        // first see if it likely needs to be compiled
        const char *F = meth->functionObjectsDecls.functionObject;
        if (!F) // ask codegen to try to turn it into llvm code
            F = jl_compile_for_dispatch(&meth, world).functionObject;
        if (meth->jlcall_api == 2)
            return jl_assume(meth->inferred_const);
        // if it hasn't been inferred, try using the unspecialized meth cache instead
        if (!meth->inferred) {
            fptr->fptr = meth->unspecialized_ducttape;
            fptr->jlcall_api = 1;
            if (!fptr->fptr) {
                if (jl_is_method(meth->def.method) && meth->def.method->unspecialized) {
                    fptr->fptr = meth->def.method->unspecialized->fptr;
                    fptr->jlcall_api = meth->def.method->unspecialized->jlcall_api;
                    if (fptr->jlcall_api == 2) {
                        return jl_assume(meth->def.method->unspecialized->inferred_const);
                    }
                }
            }
        }
        if (!fptr->fptr || fptr->jlcall_api == 0) {
            // ask codegen to make the fptr
            *fptr = jl_generate_fptr(meth, F, world);
            if (fptr->jlcall_api == 2)
                return jl_assume(meth->inferred_const);
        }
    }
    return NULL;
}

STATIC_INLINE jl_value_t *jl_call_fptr_internal(const jl_generic_fptr_t *fptr,
                                                jl_method_instance_t *meth,
                                                jl_value_t **args, uint32_t nargs)
{
    if (fptr->jlcall_api == 1)
        return fptr->fptr1(args[0], &args[1], nargs-1);
    else if (fptr->jlcall_api == 2)
        return meth->inferred;
    else if (fptr->jlcall_api == 3)
        return fptr->fptr3(meth->sparam_vals, args[0], &args[1], nargs-1);
    else if (fptr->jlcall_api == 4)
        return fptr->fptr4(meth, &args[0], nargs, meth->sparam_vals);
    else
        abort();
}

// invoke (compiling if necessary) the jlcall function pointer for a method
STATIC_INLINE jl_value_t *jl_call_method_internal(jl_method_instance_t *meth, jl_value_t **args, uint32_t nargs)
{
    jl_generic_fptr_t fptr;
    jl_value_t *v = jl_compile_method_internal(&fptr, meth);
    if (v)
        return v;
    (void)jl_assume(fptr.jlcall_api != 2);
    return jl_call_fptr_internal(&fptr, meth, args, nargs);
}

jl_tupletype_t *jl_argtype_with_function(jl_function_t *f, jl_tupletype_t *types);

JL_DLLEXPORT jl_value_t *jl_apply_2va(jl_value_t *f, jl_value_t **args, uint32_t nargs);

void jl_gc_sync_total_bytes(void);
void jl_gc_track_malloced_array(jl_ptls_t ptls, jl_array_t *a);
void jl_gc_count_allocd(size_t sz);
void jl_gc_run_all_finalizers(jl_ptls_t ptls);

void gc_queue_binding(jl_binding_t *bnd);
void gc_setmark_buf(jl_ptls_t ptls, void *buf, uint8_t, size_t);

STATIC_INLINE void jl_gc_wb_binding(jl_binding_t *bnd, void *val) // val isa jl_value_t*
{
    if (__unlikely(jl_astaggedvalue(bnd)->bits.gc == 3 &&
                   (jl_astaggedvalue(val)->bits.gc & 1) == 0))
        gc_queue_binding(bnd);
}

STATIC_INLINE void jl_gc_wb_buf(void *parent, void *bufptr, size_t minsz) // parent isa jl_value_t*
{
    // if parent is marked and buf is not
    if (__unlikely(jl_astaggedvalue(parent)->bits.gc & 1)) {
        jl_ptls_t ptls = jl_get_ptls_states();
        gc_setmark_buf(ptls, bufptr, 3, minsz);
    }
}

void gc_debug_print_status(void);
void gc_debug_critical_error(void);
void jl_print_gc_stats(JL_STREAM *s);
void jl_gc_reset_alloc_count(void);
int jl_assign_type_uid(void);
jl_value_t *jl_cache_type_(jl_datatype_t *type);
void jl_resort_type_cache(jl_svec_t *c);
int  jl_get_t_uid_ctr(void);
void jl_set_t_uid_ctr(int i);
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

void JL_NORETURN jl_method_error_bare(jl_function_t *f, jl_value_t *args, size_t world);
void JL_NORETURN jl_method_error(jl_function_t *f, jl_value_t **args, size_t na, size_t world);
jl_value_t *jl_get_exceptionf(jl_datatype_t *exception_type, const char *fmt, ...);

JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t);

#define JL_CALLABLE(name)                                               \
    JL_DLLEXPORT jl_value_t *name(jl_value_t *F, jl_value_t **args, uint32_t nargs)

JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_intrinsic_call);
extern jl_function_t *jl_unprotect_stack_func;
void jl_install_default_signal_handlers(void);
void restore_signals(void);
void jl_install_thread_signal_handler(jl_ptls_t ptls);

jl_fptr_t jl_get_builtin_fptr(jl_value_t *b);

extern uv_loop_t *jl_io_loop;

JL_DLLEXPORT void jl_uv_associate_julia_struct(uv_handle_t *handle,
                                               jl_value_t *data);
JL_DLLEXPORT int jl_uv_fs_result(uv_fs_t *f);

typedef struct _typeenv {
    jl_tvar_t *var;
    jl_value_t *val;
    struct _typeenv *prev;
} jl_typeenv_t;

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt);

int jl_has_intersect_type_not_kind(jl_value_t *t);
int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta);
int jl_has_concrete_subtype(jl_value_t *typ);
jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np);
jl_datatype_t *jl_inst_concrete_tupletype(jl_svec_t *p);
JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype);
void jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_t fptr);
jl_value_t *jl_type_intersection_env_s(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, int *issubty);
jl_value_t *jl_type_intersection_env(jl_value_t *a, jl_value_t *b, jl_svec_t **penv);
int jl_subtype_matching(jl_value_t *a, jl_value_t *b, jl_svec_t **penv);
// specificity comparison assuming !(a <: b) and !(b <: a)
JL_DLLEXPORT int jl_type_morespecific_no_subtype(jl_value_t *a, jl_value_t *b);
jl_value_t *jl_instantiate_type_with(jl_value_t *t, jl_value_t **env, size_t n);
JL_DLLEXPORT jl_value_t *jl_instantiate_type_in_env(jl_value_t *ty, jl_unionall_t *env, jl_value_t **vals);
jl_value_t *jl_substitute_var(jl_value_t *t, jl_tvar_t *var, jl_value_t *val);
jl_svec_t *jl_outer_unionall_vars(jl_value_t *u);
int jl_count_union_components(jl_value_t *v);
jl_value_t *jl_nth_union_component(jl_value_t *v, int i);
int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth);
jl_datatype_t *jl_new_uninitialized_datatype(void);
void jl_precompute_memoized_dt(jl_datatype_t *dt);
jl_datatype_t *jl_wrap_Type(jl_value_t *t);  // x -> Type{x}
jl_value_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n);
void jl_assign_bits(void *dest, jl_value_t *bits);
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module);
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, int iskw);
int jl_is_submodule(jl_module_t *child, jl_module_t *parent);
jl_array_t *jl_get_loaded_modules(void);

jl_value_t *jl_toplevel_eval_flex(jl_module_t *m, jl_value_t *e, int fast, int expanded);

jl_code_info_t *jl_wrap_expr(jl_value_t *expr);
jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e);
jl_value_t *jl_parse_eval_all(const char *fname,
                              const char *content, size_t contentlen,
                              jl_module_t *inmodule);
jl_value_t *jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src);
jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_code_info_t *src,
                                          jl_svec_t *sparam_vals);
int jl_is_toplevel_only_expr(jl_value_t *e);
jl_value_t *jl_call_scm_on_ast(const char *funcname, jl_value_t *expr, jl_module_t *inmodule);

jl_method_instance_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache, size_t world);
jl_value_t *jl_gf_invoke(jl_tupletype_t *types, jl_value_t **args, size_t nargs);
jl_method_instance_t *jl_lookup_generic(jl_value_t **args, uint32_t nargs, uint32_t callsite, size_t world);
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid);

JL_DLLEXPORT jl_datatype_t *jl_first_argument_datatype(jl_value_t *argtypes);
JL_DLLEXPORT jl_value_t *jl_argument_datatype(jl_value_t *argt);

jl_value_t *jl_nth_slot_type(jl_value_t *sig, size_t i);
void jl_compute_field_offsets(jl_datatype_t *st);
jl_array_t *jl_new_array_for_deserialization(jl_value_t *atype, uint32_t ndims, size_t *dims,
                                             int isunboxed, int elsz);
void jl_module_run_initializer(jl_module_t *m);
extern jl_array_t *jl_module_init_order;
extern union jl_typemap_t jl_cfunction_list;

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
void *jl_init_llvm(void);
void jl_init_codegen(void);
void jl_init_intrinsic_functions(void);
void jl_init_intrinsic_properties(void);
void jl_init_tasks(void);
void jl_init_stack_limits(int ismaster);
void jl_init_root_task(void *stack, size_t ssize);
void jl_init_serializer(void);
void jl_gc_init(void);
void jl_init_signal_async(void);
void jl_init_debuginfo(void);
void jl_init_runtime_ccall(void);
void jl_init_thread_heap(jl_ptls_t ptls);

void _julia_init(JL_IMAGE_SEARCH rel);

void jl_set_base_ctx(char *__stk);

extern ssize_t jl_tls_offset;
extern const int jl_tls_elf_support;
void jl_init_threading(void);
void jl_start_threads(void);
void jl_shutdown_threading(void);

// Whether the GC is running
extern char *jl_safepoint_pages;
STATIC_INLINE int jl_addr_is_safepoint(uintptr_t addr)
{
    uintptr_t safepoint_addr = (uintptr_t)jl_safepoint_pages;
    return addr >= safepoint_addr && addr < safepoint_addr + jl_page_size * 3;
}
extern volatile uint32_t jl_gc_running;
// All the functions are safe to be called from within a signal handler
// provided that the thread will not be interrupted by another asynchronous
// signal.
// Initialize the safepoint
void jl_safepoint_init(void);
// Start the GC, return `1` if the thread should be running the GC.
// Otherwise, the thread will wait in this function until the GC finishes on
// another thread and return `0`.
// The caller should have saved the `gc_state` and set it to `WAITING`
// before calling this function. If the calling thread is to run the GC,
// it should also wait for the mutator threads to hit a safepoint **AFTER**
// this function returns
int jl_safepoint_start_gc(void);
// Can only be called by the thread that have got a `1` return value from
// `jl_safepoint_start_gc()`. This disables the safepoint (for GC,
// the `mprotect` may not be removed if there's pending SIGINT) and wake
// up waiting threads if there's any.
// The caller should restore `gc_state` **AFTER** calling this function.
void jl_safepoint_end_gc(void);
// Wait for the GC to finish
// This function does **NOT** modify the `gc_state` to inform the GC thread
// The caller should set it **BEFORE** calling this function.
void jl_safepoint_wait_gc(void);

// Set pending sigint and enable the mechanisms to deliver the sigint.
void jl_safepoint_enable_sigint(void);
// If the safepoint is enabled to deliver sigint, disable it
// so that the thread won't repeatedly trigger it in a sigatomic region
// while not being able to actually throw the exception.
void jl_safepoint_defer_sigint(void);
// Clear the sigint pending flag and disable the mechanism to deliver sigint.
// Return `1` if the sigint should be delivered and `0` if there's no sigint
// to be delivered.
int jl_safepoint_consume_sigint(void);
void jl_wake_libuv(void);

#ifdef JULIA_ENABLE_THREADING
jl_get_ptls_states_func jl_get_ptls_states_getter(void);
static inline void jl_set_gc_and_wait(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // reading own gc state doesn't need atomic ops since no one else
    // should store to it.
    int8_t state = jl_gc_state(ptls);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    jl_safepoint_wait_gc();
    jl_atomic_store_release(&ptls->gc_state, state);
}
#endif

JL_DLLEXPORT jl_value_t *jl_dump_fptr_asm(uint64_t fptr, int raw_mc, const char* asm_variant);

void jl_dump_native(const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *sysimg_data, size_t sysimg_len);
int32_t jl_get_llvm_gv(jl_value_t *p);
int32_t jl_assign_functionID(const char *fname);
int32_t jl_jlcall_api(const char *fname);
// the first argument to jl_idtable_rehash is used to return a value
// make sure it is rooted if it is used after the function returns
JL_DLLEXPORT jl_array_t *jl_idtable_rehash(jl_array_t *a, size_t newsz);

JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module);
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types, size_t world);
JL_DLLEXPORT int jl_has_call_ambiguities(jl_tupletype_t *types, jl_method_t *m);
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_value_t *types, jl_svec_t *sp);
int jl_is_rettype_inferred(jl_method_instance_t *li);
JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt, jl_tupletype_t *type, size_t world);
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(jl_method_t *m, jl_value_t *type, jl_svec_t *sparams, size_t world);
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_method_instance_t *caller);
JL_DLLEXPORT void jl_method_table_add_backedge(jl_methtable_t *mt, jl_value_t *typ, jl_value_t *caller);

uint32_t jl_module_next_counter(jl_module_t *m);
void jl_fptr_to_llvm(jl_fptr_t fptr, jl_method_instance_t *lam, int specsig);
jl_tupletype_t *arg_type_tuple(jl_value_t **args, size_t nargs);

int jl_has_meta(jl_array_t *body, jl_sym_t *sym);

// backtraces
typedef struct {
    char *func_name;
    char *file_name;
    int line;
    jl_method_instance_t *linfo;
    int fromC;
    int inlined;
} jl_frame_t;

// Might be called from unmanaged thread
uint64_t jl_getUnwindInfo(uint64_t dwBase);
#ifdef _OS_WINDOWS_
#include <dbghelp.h>
JL_DLLEXPORT EXCEPTION_DISPOSITION __julia_personality(
        PEXCEPTION_RECORD ExceptionRecord, void *EstablisherFrame, PCONTEXT ContextRecord, void *DispatcherContext);
extern HANDLE hMainThread;
typedef CONTEXT bt_context_t;
#if defined(_CPU_X86_64_)
typedef CONTEXT bt_cursor_t;
#else
typedef struct {
    STACKFRAME64 stackframe;
    CONTEXT context;
} bt_cursor_t;
#endif
extern volatile int jl_in_stackwalk;
#elif !defined(JL_DISABLE_LIBUNWIND)
// This gives unwind only local unwinding options ==> faster code
#  define UNW_LOCAL_ONLY
#  include <libunwind.h>
typedef unw_context_t bt_context_t;
typedef unw_cursor_t bt_cursor_t;
#  if (!defined(SYSTEM_LIBUNWIND) || UNW_VERSION_MAJOR > 1 ||   \
       (UNW_VERSION_MAJOR == 1 && UNW_VERSION_MINOR > 1))
// Enable our memory manager only for libunwind with our patch or
// on a newer release
#    define JL_UNW_HAS_FORMAT_IP 1
#  endif
#else
// Unwinding is disabled
typedef int bt_context_t;
typedef int bt_cursor_t;
#endif
size_t rec_backtrace(uintptr_t *data, size_t maxsize);
size_t rec_backtrace_ctx(uintptr_t *data, size_t maxsize, bt_context_t *ctx);
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(uintptr_t *data, size_t maxsize, bt_context_t *ctx);
#endif
JL_DLLEXPORT jl_value_t *jl_get_backtrace(void);
JL_DLLEXPORT jl_value_t *jl_apply_with_saved_exception_state(jl_value_t **args, uint32_t nargs, int drop_exceptions);
void jl_critical_error(int sig, bt_context_t *context, uintptr_t *bt_data, size_t *bt_size);
JL_DLLEXPORT void jl_raise_debugger(void);
int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline);
JL_DLLEXPORT void jl_gdblookup(uintptr_t ip);
// *to is NULL or malloc'd pointer, from is allowed to be NULL
STATIC_INLINE char *jl_copy_str(char **to, const char *from)
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
// Windows only
#define JL_EXE_LIBNAME ((const char*)1)
#define JL_DL_LIBNAME ((const char*)2)
const char *jl_dlfind_win32(const char *name);
void *jl_dlopen_soname(const char *pfx, size_t n, unsigned flags);

// libuv wrappers:
JL_DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path);

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
#define HAVE_CPUID
#endif

#ifdef SEGV_EXCEPTION
extern JL_DLLEXPORT jl_value_t *jl_segv_exception;
#endif

// -- Runtime intrinsics -- //
JL_DLLEXPORT const char *jl_intrinsic_name(int f);
unsigned jl_intrinsic_nargs(int f);

JL_DLLEXPORT jl_value_t *jl_bitcast(jl_value_t *ty, jl_value_t *v);
JL_DLLEXPORT jl_value_t *jl_pointerref(jl_value_t *p, jl_value_t *i, jl_value_t *align);
JL_DLLEXPORT jl_value_t *jl_pointerset(jl_value_t *p, jl_value_t *x, jl_value_t *align, jl_value_t *i);
JL_DLLEXPORT jl_value_t *jl_cglobal(jl_value_t *v, jl_value_t *ty);
JL_DLLEXPORT jl_value_t *jl_cglobal_auto(jl_value_t *v);

JL_DLLEXPORT jl_value_t *jl_neg_int(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_add_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_mul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sdiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_udiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_srem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_urem_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_add_ptr(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_sub_ptr(jl_value_t *a, jl_value_t *b);

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

JL_DLLEXPORT jl_value_t *jl_checked_sadd_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_uadd_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_ssub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_usub_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_smul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_umul_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_sdiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_udiv_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_srem_int(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_checked_urem_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_ceil_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_floor_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_trunc_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_rint_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_sqrt_llvm(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_abs_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_copysign_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_flipsign_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_select_value(jl_value_t *isfalse, jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_arraylen(jl_value_t *a);
int jl_array_store_unboxed(jl_value_t *el_type);
int jl_array_isdefined(jl_value_t **args, int nargs);
JL_DLLEXPORT jl_value_t *(jl_array_data_owner)(jl_array_t *a);
JL_DLLEXPORT int jl_array_isassigned(jl_array_t *a, size_t i);

// -- synchronization utilities -- //

extern jl_mutex_t typecache_lock;
extern jl_mutex_t codegen_lock;
extern jl_mutex_t safepoint_lock;

// -- gc.c -- //

#if defined(__APPLE__) && defined(JULIA_ENABLE_THREADING)
void jl_mach_gc_end(void);
#endif

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

// -- typemap.c -- //

// a descriptor of a jl_typemap_t that gets
// passed around as self-documentation of the parameters of the type
struct jl_typemap_info {
    int8_t unsorted; // whether this should be unsorted
    jl_datatype_t **jl_contains; // the type that is being put in this
};

jl_typemap_entry_t *jl_typemap_insert(union jl_typemap_t *cache, jl_value_t *parent,
                                      jl_tupletype_t *type,
                                      jl_tupletype_t *simpletype, jl_svec_t *guardsigs,
                                      jl_value_t *newvalue, int8_t offs,
                                      const struct jl_typemap_info *tparams,
                                      size_t min_world, size_t max_world,
                                      jl_value_t **overwritten);

jl_typemap_entry_t *jl_typemap_assoc_by_type(
        union jl_typemap_t ml_or_cache, jl_tupletype_t *types, jl_svec_t **penv,
        int8_t subtype, int8_t offs, size_t world);
jl_typemap_entry_t *jl_typemap_level_assoc_exact(jl_typemap_level_t *cache, jl_value_t **args, size_t n, int8_t offs, size_t world);
jl_typemap_entry_t *jl_typemap_entry_assoc_exact(jl_typemap_entry_t *mn, jl_value_t **args, size_t n, size_t world);
STATIC_INLINE jl_typemap_entry_t *jl_typemap_assoc_exact(union jl_typemap_t ml_or_cache, jl_value_t **args, size_t n, int8_t offs, size_t world)
{
    // NOTE: This function is a huge performance hot spot!!
    if (jl_typeof(ml_or_cache.unknown) == (jl_value_t*)jl_typemap_entry_type) {
        return jl_typemap_entry_assoc_exact(ml_or_cache.leaf, args, n, world);
    }
    else if (jl_typeof(ml_or_cache.unknown) == (jl_value_t*)jl_typemap_level_type) {
        return jl_typemap_level_assoc_exact(ml_or_cache.node, args, n, offs, world);
    }
    return NULL;
}

typedef int (*jl_typemap_visitor_fptr)(jl_typemap_entry_t *l, void *closure);
int jl_typemap_visitor(union jl_typemap_t a, jl_typemap_visitor_fptr fptr, void *closure);

struct typemap_intersection_env;
typedef int (*jl_typemap_intersection_visitor_fptr)(jl_typemap_entry_t *l, struct typemap_intersection_env *closure);
struct typemap_intersection_env {
    // input values
    jl_typemap_intersection_visitor_fptr fptr; // fptr to call on a match
    jl_value_t *type; // type to match
    jl_value_t *va; // the tparam0 for the vararg in type, if applicable (or NULL)
    // output values
    jl_value_t *ti; // intersection type
    jl_svec_t *env; // intersection env (initialize to null to perform intersection without an environment)
    int issubty;    // if `a <: b` is true in `intersect(a,b)`
};
int jl_typemap_intersection_visitor(union jl_typemap_t a, int offs, struct typemap_intersection_env *closure);

unsigned jl_special_vector_alignment(size_t nfields, jl_value_t *field_type);

void register_eh_frames(uint8_t *Addr, size_t Size);
void deregister_eh_frames(uint8_t *Addr, size_t Size);

STATIC_INLINE void *jl_get_frame_addr(void)
{
#ifdef __GNUC__
    return __builtin_frame_address(0);
#else
    void *dummy = NULL;
    // The mask is to suppress the compiler warning about returning
    // address of local variable
    return (void*)((uintptr_t)&dummy & ~(uintptr_t)15);
#endif
}

JL_DLLEXPORT jl_array_t *jl_array_cconvert_cstring(jl_array_t *a);
JL_DLLEXPORT void jl_depwarn_partial_indexing(size_t n);
void jl_depwarn(const char *msg, jl_value_t *sym);

int isabspath(const char *in);

extern jl_sym_t *call_sym;    extern jl_sym_t *invoke_sym;
extern jl_sym_t *empty_sym;   extern jl_sym_t *body_sym;
extern jl_sym_t *dots_sym;    extern jl_sym_t *vararg_sym;
extern jl_sym_t *quote_sym;   extern jl_sym_t *newvar_sym;
extern jl_sym_t *top_sym;     extern jl_sym_t *dot_sym;
extern jl_sym_t *line_sym;    extern jl_sym_t *toplevel_sym;
extern jl_sym_t *core_sym;    extern jl_sym_t *globalref_sym;
extern jl_sym_t *error_sym;   extern jl_sym_t *amp_sym;
extern jl_sym_t *module_sym;  extern jl_sym_t *colons_sym;
extern jl_sym_t *export_sym;  extern jl_sym_t *import_sym;
extern jl_sym_t *importall_sym; extern jl_sym_t *using_sym;
extern jl_sym_t *goto_sym;    extern jl_sym_t *goto_ifnot_sym;
extern jl_sym_t *label_sym;   extern jl_sym_t *return_sym;
extern jl_sym_t *lambda_sym;  extern jl_sym_t *assign_sym;
extern jl_sym_t *method_sym;  extern jl_sym_t *slot_sym;
extern jl_sym_t *enter_sym;   extern jl_sym_t *leave_sym;
extern jl_sym_t *exc_sym;     extern jl_sym_t *new_sym;
extern jl_sym_t *compiler_temp_sym; extern jl_sym_t *foreigncall_sym;
extern jl_sym_t *const_sym;   extern jl_sym_t *thunk_sym;
extern jl_sym_t *anonymous_sym;  extern jl_sym_t *underscore_sym;
extern jl_sym_t *abstracttype_sym; extern jl_sym_t *primtype_sym;
extern jl_sym_t *structtype_sym;
extern jl_sym_t *global_sym; extern jl_sym_t *unused_sym;
extern jl_sym_t *boundscheck_sym; extern jl_sym_t *inbounds_sym;
extern jl_sym_t *copyast_sym; extern jl_sym_t *fastmath_sym;
extern jl_sym_t *pure_sym; extern jl_sym_t *simdloop_sym;
extern jl_sym_t *meta_sym; extern jl_sym_t *list_sym;
extern jl_sym_t *inert_sym; extern jl_sym_t *static_parameter_sym;
extern jl_sym_t *polly_sym; extern jl_sym_t *inline_sym;
extern jl_sym_t *propagate_inbounds_sym;
extern jl_sym_t *isdefined_sym;
extern jl_sym_t *nospecialize_sym;
extern jl_sym_t *boundscheck_sym;
extern jl_sym_t *gc_preserve_begin_sym; extern jl_sym_t *gc_preserve_end_sym;

void jl_register_fptrs(uint64_t sysimage_base, const char *base, const int32_t *offsets,
                       jl_method_instance_t **linfos, size_t n);

extern arraylist_t partial_inst;

#ifdef __cplusplus
}
#endif

#endif
