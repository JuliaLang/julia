// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_INTERNAL_H
#define JL_INTERNAL_H

#include "options.h"
#include "locks.h"
#include <uv.h>
#if !defined(_WIN32)
#include <unistd.h>
#else
#define sleep(x) Sleep(1000*x)
#endif

#ifdef __cplusplus
extern "C" {
#endif
#ifdef JL_ASAN_ENABLED
void __sanitizer_start_switch_fiber(void**, const void*, size_t);
void __sanitizer_finish_switch_fiber(void*, const void**, size_t*);
#endif
#ifdef JL_TSAN_ENABLED
void *__tsan_create_fiber(unsigned flags);
// void __tsan_destroy_fiber(void *fiber);
void __tsan_switch_to_fiber(void *fiber, unsigned flags);
#endif
#ifdef __cplusplus
}
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

#ifndef alignof
#  ifndef __cplusplus
#    ifdef __GNUC__
#      define alignof _Alignof
#    else
#      define alignof(...) 1
#    endif
#  endif
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

// If this is detected in a backtrace of segfault, it means the functions
// that use this value must be reworked into their async form with cb arg
// provided and with JL_UV_LOCK used around the calls
static uv_loop_t *const unused_uv_loop_arg = (uv_loop_t *)0xBAD10;

extern jl_mutex_t jl_uv_mutex;
extern int jl_uv_n_waiters;
void JL_UV_LOCK(void);
#define JL_UV_UNLOCK() JL_UNLOCK(&jl_uv_mutex)

#ifdef __cplusplus
extern "C" {
#endif

//--------------------------------------------------
// timers
// Returns time in nanosec
JL_DLLEXPORT uint64_t jl_hrtime(void);

// number of cycles since power-on
static inline uint64_t cycleclock(void)
{
#if defined(_CPU_X86_64_)
    uint64_t low, high;
    __asm__ volatile("rdtsc" : "=a"(low), "=d"(high));
    return (high << 32) | low;
#elif defined(_CPU_X86_)
    int64_t ret;
    __asm__ volatile("rdtsc" : "=A"(ret));
    return ret;
#elif defined(_CPU_AARCH64_)
    // System timer of ARMv8 runs at a different frequency than the CPU's.
    // The frequency is fixed, typically in the range 1-50MHz.  It can be
    // read at CNTFRQ special register.  We assume the OS has set up
    // the virtual timer properly.
    int64_t virtual_timer_value;
    __asm__ volatile("mrs %0, cntvct_el0" : "=r"(virtual_timer_value));
    return virtual_timer_value;
#else
    #warning No cycleclock() definition for your platform
    // copy from https://github.com/google/benchmark/blob/v1.5.0/src/cycleclock.h
    return 0;
#endif
}

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


// -- gc.c -- //

#define GC_CLEAN  0 // freshly allocated
#define GC_MARKED 1 // reachable and young
#define GC_OLD    2 // if it is reachable it will be marked as old
#define GC_OLD_MARKED (GC_OLD | GC_MARKED) // reachable and old

// useful constants
extern jl_methtable_t *jl_type_type_mt JL_GLOBALLY_ROOTED;
extern jl_methtable_t *jl_nonfunction_mt JL_GLOBALLY_ROOTED;
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
void *jl_gc_perm_alloc_nolock(size_t sz, int zero,
    unsigned align, unsigned offset) JL_NOTSAFEPOINT;
void *jl_gc_perm_alloc(size_t sz, int zero,
    unsigned align, unsigned offset) JL_NOTSAFEPOINT;
void jl_gc_force_mark_old(jl_ptls_t ptls, jl_value_t *v);
void gc_sweep_sysimg(void);


// pools are 16376 bytes large (GC_POOL_SZ - GC_PAGE_OFFSET)
static const int jl_gc_sizeclasses[] = {
#ifdef _P64
    8,
#elif MAX_ALIGN > 4
    // ARM and PowerPC have max alignment larger than pointer,
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
static_assert(sizeof(jl_gc_sizeclasses) / sizeof(jl_gc_sizeclasses[0]) == JL_GC_N_POOLS, "");

STATIC_INLINE int jl_gc_alignment(size_t sz)
{
    if (sz == 0)
        return sizeof(void*);
#ifdef _P64
    (void)sz;
    return 16;
#elif MAX_ALIGN == 8
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

// the following table is computed from jl_gc_sizeclasses via the formula:
// [searchsortedfirst(TABLE, i) for i = 0:16:table[end]]
static const uint8_t szclass_table[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 20, 21, 21, 22, 22, 23, 23, 23, 24, 24, 24, 25, 25, 25, 26, 26, 27, 27, 27, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, 37, 37, 37, 37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40};
static_assert(sizeof(szclass_table) == 128, "");

STATIC_INLINE uint8_t JL_CONST_FUNC jl_gc_szclass(unsigned sz)
{
    assert(sz <= 2032);
    uint8_t klass = szclass_table[(sz + 15) / 16];
#ifdef _P64
    if (sz <= 8)
        return 0;
    const int N = 0;
#elif MAX_ALIGN == 8
    if (sz <= 8)
        return (sz >= 4 ? 1 : 0);
    const int N = 1;
#else
    if (sz <= 12)
        return (sz >= 8 ? 2 : (sz >= 4 ? 1 : 0));
    const int N = 2;
#endif
    return klass + N;
}

#define JL_SMALL_BYTE_ALIGNMENT 16
#define JL_CACHE_BYTE_ALIGNMENT 64
// JL_HEAP_ALIGNMENT is the maximum alignment that the GC can provide
#define JL_HEAP_ALIGNMENT JL_SMALL_BYTE_ALIGNMENT
#define GC_MAX_SZCLASS (2032-sizeof(void*))

STATIC_INLINE jl_value_t *jl_gc_alloc_(jl_ptls_t ptls, size_t sz, void *ty)
{
    jl_value_t *v;
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    if (sz <= GC_MAX_SZCLASS) {
        int pool_id = jl_gc_szclass(allocsz);
        jl_gc_pool_t *p = &ptls->heap.norm_pools[pool_id];
        int osize = jl_gc_sizeclasses[pool_id];
        v = jl_gc_pool_alloc(ptls, (char*)p - (char*)ptls, osize);
    }
    else {
        if (allocsz < sz) // overflow in adding offs, size was "negative"
            jl_throw(jl_memory_exception);
        v = jl_gc_big_alloc(ptls, allocsz);
    }
    jl_set_typeof(v, ty);
    return v;
}
JL_DLLEXPORT jl_value_t *jl_gc_alloc(jl_ptls_t ptls, size_t sz, void *ty);
// On GCC, only inline when sz is constant
#ifdef __GNUC__
#  define jl_gc_alloc(ptls, sz, ty)  \
    (__builtin_constant_p(sz) ?      \
     jl_gc_alloc_(ptls, sz, ty) :    \
     (jl_gc_alloc)(ptls, sz, ty))
#else
#  define jl_gc_alloc(ptls, sz, ty) jl_gc_alloc_(ptls, sz, ty)
#endif

// jl_buff_tag must be a multiple of GC_PAGE_SZ so that it can't be
// confused for an actual type reference.
#define jl_buff_tag ((uintptr_t)0x4eadc000)
typedef void jl_gc_tracked_buffer_t; // For the benefit of the static analyzer
STATIC_INLINE jl_gc_tracked_buffer_t *jl_gc_alloc_buf(jl_ptls_t ptls, size_t sz)
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

// this sizeof(__VA_ARGS__) trick can't be computed until C11, but that only matters to Clang in some situations
#if !defined(__clang_analyzer__) && !(defined(JL_ASAN_ENABLED) || defined(JL_TSAN_ENABLED))
#ifdef __GNUC__
#define jl_perm_symsvec(n, ...) \
    (jl_perm_symsvec)(__extension__({                                         \
            static_assert(                                                    \
                n == sizeof((char *[]){ __VA_ARGS__ })/sizeof(char *),        \
                "Number of passed arguments does not match expected number"); \
            n;                                                                \
        }), __VA_ARGS__)
#define jl_svec(n, ...) \
    (jl_svec)(__extension__({                                                 \
            static_assert(                                                    \
                n == sizeof((void *[]){ __VA_ARGS__ })/sizeof(void *),        \
                "Number of passed arguments does not match expected number"); \
            n;                                                                \
        }), __VA_ARGS__)
#endif
#endif

jl_value_t *jl_gc_realloc_string(jl_value_t *s, size_t sz);
JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz);

JL_DLLEXPORT void JL_NORETURN jl_throw_out_of_memory_error(void);


JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void);
JL_DLLEXPORT int64_t jl_gc_sync_total_bytes(int64_t offset);
void jl_gc_track_malloced_array(jl_ptls_t ptls, jl_array_t *a) JL_NOTSAFEPOINT;
void jl_gc_count_allocd(size_t sz) JL_NOTSAFEPOINT;
void jl_gc_run_all_finalizers(jl_ptls_t ptls);

void gc_queue_binding(jl_binding_t *bnd) JL_NOTSAFEPOINT;
void gc_setmark_buf(jl_ptls_t ptls, void *buf, uint8_t, size_t) JL_NOTSAFEPOINT;

STATIC_INLINE void jl_gc_wb_binding(jl_binding_t *bnd, void *val) JL_NOTSAFEPOINT // val isa jl_value_t*
{
    if (__unlikely(jl_astaggedvalue(bnd)->bits.gc == 3 &&
                   (jl_astaggedvalue(val)->bits.gc & 1) == 0))
        gc_queue_binding(bnd);
}

STATIC_INLINE void jl_gc_wb_buf(void *parent, void *bufptr, size_t minsz) JL_NOTSAFEPOINT // parent isa jl_value_t*
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
uint32_t jl_get_gs_ctr(void);
void jl_set_gs_ctr(uint32_t ctr);

STATIC_INLINE jl_value_t *undefref_check(jl_datatype_t *dt, jl_value_t *v) JL_NOTSAFEPOINT
{
     if (dt->layout->first_ptr >= 0) {
        jl_value_t *nullp = ((jl_value_t**)v)[dt->layout->first_ptr];
        if (__unlikely(nullp == NULL))
            jl_throw(jl_undefref_exception);
    }
    return v;
}


// -- functions -- //

jl_code_info_t *jl_type_infer(jl_method_instance_t *li, size_t world, int force);
jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *meth JL_PROPAGATES_ROOT, size_t world);
jl_code_instance_t *jl_generate_fptr(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world);
void jl_generate_fptr_for_unspecialized(jl_code_instance_t *unspec);
JL_DLLEXPORT jl_code_instance_t *jl_get_method_inferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
        size_t min_world, size_t max_world);
jl_method_instance_t *jl_get_unspecialized(jl_method_instance_t *method JL_PROPAGATES_ROOT);

JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types);
jl_code_info_t *jl_code_for_interpreter(jl_method_instance_t *lam JL_PROPAGATES_ROOT);
int jl_code_requires_compiler(jl_code_info_t *src);
jl_code_info_t *jl_new_code_info_from_ir(jl_expr_t *ast);
JL_DLLEXPORT jl_code_info_t *jl_new_code_info_uninit(void);

jl_value_t *jl_argtype_with_function(jl_function_t *f, jl_value_t *types);

JL_DLLEXPORT jl_value_t *jl_apply_2va(jl_value_t *f, jl_value_t **args, uint32_t nargs);

void JL_NORETURN jl_method_error(jl_function_t *f, jl_value_t **args, size_t na, size_t world);
jl_value_t *jl_get_exceptionf(jl_datatype_t *exception_type, const char *fmt, ...);

JL_DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t);

#define JL_CALLABLE(name)                                               \
    JL_DLLEXPORT jl_value_t *name(jl_value_t *F, jl_value_t **args, uint32_t nargs)

JL_CALLABLE(jl_f_tuple);
JL_CALLABLE(jl_f_intrinsic_call);
void jl_install_default_signal_handlers(void);
void restore_signals(void);
void jl_install_thread_signal_handler(jl_ptls_t ptls);

jl_fptr_args_t jl_get_builtin_fptr(jl_value_t *b);

extern uv_loop_t *jl_io_loop;
void jl_uv_flush(uv_stream_t *stream);

typedef struct jl_typeenv_t {
    jl_tvar_t *var;
    jl_value_t *val;
    struct jl_typeenv_t *prev;
} jl_typeenv_t;

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt);
int jl_tuple1_isa(jl_value_t *child1, jl_value_t **child, size_t cl, jl_datatype_t *pdt);

int jl_has_intersect_type_not_kind(jl_value_t *t);
int jl_subtype_invariant(jl_value_t *a, jl_value_t *b, int ta);
int jl_has_concrete_subtype(jl_value_t *typ);
jl_datatype_t *jl_inst_concrete_tupletype_v(jl_value_t **p, size_t np) JL_ALWAYS_LEAFTYPE;
jl_datatype_t *jl_inst_concrete_tupletype(jl_svec_t *p) JL_ALWAYS_LEAFTYPE;
jl_tupletype_t *jl_inst_arg_tuple_type(jl_value_t *arg1, jl_value_t **args, size_t nargs, int leaf);
jl_tupletype_t *jl_lookup_arg_tuple_type(jl_value_t *arg1 JL_PROPAGATES_ROOT, jl_value_t **args, size_t nargs, int leaf);
JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype);
jl_datatype_t *jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_args_t fptr) JL_GC_DISABLED;
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
jl_value_t *jl_nth_union_component(jl_value_t *v JL_PROPAGATES_ROOT, int i) JL_NOTSAFEPOINT;
int jl_find_union_component(jl_value_t *haystack, jl_value_t *needle, unsigned *nth) JL_NOTSAFEPOINT;
jl_datatype_t *jl_new_uninitialized_datatype(void);
void jl_precompute_memoized_dt(jl_datatype_t *dt, int cacheable);
jl_datatype_t *jl_wrap_Type(jl_value_t *t);  // x -> Type{x}
jl_value_t *jl_wrap_vararg(jl_value_t *t, jl_value_t *n);
jl_datatype_t *jl_lookup_cache_type_(jl_datatype_t *type);
void jl_cache_type_(jl_datatype_t *type);
void jl_assign_bits(void *dest, jl_value_t *bits) JL_NOTSAFEPOINT;
void set_nth_field(jl_datatype_t *st, void *v, size_t i, jl_value_t *rhs) JL_NOTSAFEPOINT;
jl_expr_t *jl_exprn(jl_sym_t *head, size_t n);
jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module);
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st);
void jl_init_main_module(void);
int jl_is_submodule(jl_module_t *child, jl_module_t *parent);
jl_array_t *jl_get_loaded_modules(void);

jl_value_t *jl_toplevel_eval_flex(jl_module_t *m, jl_value_t *e, int fast, int expanded);

jl_value_t *jl_eval_global_var(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *e);
jl_value_t *jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src);
jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_code_info_t *src,
                                          jl_svec_t *sparam_vals);
int jl_is_toplevel_only_expr(jl_value_t *e) JL_NOTSAFEPOINT;
jl_value_t *jl_call_scm_on_ast(const char *funcname, jl_value_t *expr, jl_module_t *inmodule);
void jl_linenumber_to_lineinfo(jl_code_info_t *ci, jl_value_t *name);

jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world);
jl_value_t *jl_gf_invoke(jl_value_t *types, jl_value_t *f, jl_value_t **args, size_t nargs);
jl_method_instance_t *jl_lookup_generic(jl_value_t **args, uint32_t nargs, uint32_t callsite, size_t world) JL_ALWAYS_LEAFTYPE;
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid, int *ambig);

JL_DLLEXPORT jl_datatype_t *jl_first_argument_datatype(jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_value_t *jl_argument_datatype(jl_value_t *argt JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
JL_DLLEXPORT jl_methtable_t *jl_method_table_for(
    jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT;
jl_methtable_t *jl_argument_method_table(jl_value_t *argt JL_PROPAGATES_ROOT);

int jl_pointer_egal(jl_value_t *t);
jl_value_t *jl_nth_slot_type(jl_value_t *sig JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;
void jl_compute_field_offsets(jl_datatype_t *st);
jl_array_t *jl_new_array_for_deserialization(jl_value_t *atype, uint32_t ndims, size_t *dims,
                                             int isunboxed, int hasptr, int isunion, int elsz);
void jl_module_run_initializer(jl_module_t *m);
jl_binding_t *jl_get_module_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT;
extern jl_array_t *jl_module_init_order JL_GLOBALLY_ROOTED;
extern htable_t jl_current_modules JL_GLOBALLY_ROOTED;
JL_DLLEXPORT void jl_compile_extern_c(void *llvmmod, void *params, void *sysimg, jl_value_t *declrt, jl_value_t *sigt);

#ifdef JL_USE_INTEL_JITEVENTS
extern char jl_using_intel_jitevents;
#endif
#ifdef JL_USE_OPROFILE_JITEVENTS
extern char jl_using_oprofile_jitevents;
#endif
#ifdef JL_USE_PERF_JITEVENTS
extern char jl_using_perf_jitevents;
#endif
extern size_t jl_arr_xtralloc_limit;

// -- init.c -- //

void jl_init_types(void) JL_GC_DISABLED;
void jl_init_box_caches(void);
void jl_init_flisp(void);
void jl_init_common_symbols(void);
void jl_init_primitives(void) JL_GC_DISABLED;
void jl_init_llvm(void);
void jl_init_codegen(void);
void jl_init_intrinsic_functions(void);
void jl_init_intrinsic_properties(void);
void jl_init_tasks(void) JL_GC_DISABLED;
void jl_init_stack_limits(int ismaster, void **stack_hi, void **stack_lo);
void jl_init_root_task(void *stack_lo, void *stack_hi);
void jl_init_serializer(void);
void jl_gc_init(void);
void jl_init_uv(void);
void jl_init_debuginfo(void);
void jl_init_thread_heap(jl_ptls_t ptls);

void jl_teardown_codegen(void);

void _julia_init(JL_IMAGE_SEARCH rel);

void jl_set_base_ctx(char *__stk);

extern ssize_t jl_tls_offset;
extern const int jl_tls_elf_support;
void jl_init_threading(void);
void jl_start_threads(void);

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

#if !defined(__clang_analyzer__)
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

JL_DLLEXPORT jl_value_t *jl_dump_method_asm(jl_method_instance_t *linfo, size_t world,
        int raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo);
JL_DLLEXPORT void *jl_get_llvmf_defn(jl_method_instance_t *linfo, size_t world, char getwrapper, char optimize, const jl_cgparams_t params);
JL_DLLEXPORT jl_value_t *jl_dump_fptr_asm(uint64_t fptr, int raw_mc, const char* asm_variant, const char *debuginfo);
JL_DLLEXPORT jl_value_t *jl_dump_llvm_asm(void *F, const char* asm_variant, const char *debuginfo);
JL_DLLEXPORT jl_value_t *jl_dump_function_ir(void *f, char strip_ir_metadata, char dump_module, const char *debuginfo);

void *jl_create_native(jl_array_t *methods, const jl_cgparams_t cgparams, int policy);
void jl_dump_native(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *asm_fname,
        const char *sysimg_data, size_t sysimg_len);
int32_t jl_get_llvm_gv(void *native_code, jl_value_t *p) JL_NOTSAFEPOINT;
void jl_get_function_id(void *native_code, jl_code_instance_t *ncode,
        int32_t *func_idx, int32_t *specfunc_idx);

// the first argument to jl_idtable_rehash is used to return a value
// make sure it is rooted if it is used after the function returns
JL_DLLEXPORT jl_array_t *jl_idtable_rehash(jl_array_t *a, size_t newsz);

JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module);
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types, size_t world, size_t *min_valid, size_t *max_valid, int mt_cache);
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_value_t *types, jl_svec_t *sp);
JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_method_instance_t *li JL_PROPAGATES_ROOT, size_t min_world, size_t max_world);
jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world);
JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt, jl_value_t *type, size_t world);
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(
    jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams);
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_method_instance_t *caller);
JL_DLLEXPORT void jl_method_table_add_backedge(jl_methtable_t *mt, jl_value_t *typ, jl_value_t *caller);

uint32_t jl_module_next_counter(jl_module_t *m);
jl_tupletype_t *arg_type_tuple(jl_value_t *arg1, jl_value_t **args, size_t nargs);

int jl_has_meta(jl_array_t *body, jl_sym_t *sym);

jl_value_t *jl_parse(const char *text, size_t text_len, jl_value_t *filename,
                     size_t offset, jl_value_t *options);

//--------------------------------------------------
// Backtraces

// Backtrace buffers:
//
// A backtrace buffer conceptually contains a stack of instruction pointers
// ordered from the inner-most frame to the outermost. We store them in a
// special raw format for two reasons:
//
//   * Efficiency: Every `throw()` must populate the trace so it must be as
//     efficient as possible.
//   * Signal safety: For signal-based exceptions such as StackOverflowError
//     the trace buffer needs to be filled from a signal handler where most
//     operations are not allowed (including malloc) so we choose a flat
//     preallocated buffer.
//
// The raw buffer layout contains "frame entries" composed of one or several
// values of type `jl_bt_element_t`. From the point of view of the GC, an entry
// is either:
//
// 1. A single instruction pointer to native code, not GC-managed.
// 2. An "extended entry": a mixture of raw data and pointers to julia objects
//    which must be treated as GC roots.
//
// A single extended entry is seralized using multiple elements from the raw
// buffer; if `e` is the pointer to the first slot we have:
//
//   e[0]  JL_BT_NON_PTR_ENTRY  - Special marker to distinguish extended entries
//   e[1]  descriptor           - A bit packed uintptr_t containing a tag and
//                                the number of GC- managed and non-managed values
//   e[2+j]                     - GC managed data
//   e[2+ngc+i]                 - Non-GC-managed data
//
// The format of `descriptor` is, from LSB to MSB:
//   0:2     ngc     Number of GC-managed pointers for this frame entry
//   3:5     nptr    Number of non-GC-managed buffer elements
//   6:9     tag     Entry type
//   10:...  header  Entry-specific header data
typedef struct _jl_bt_element_t {
    union {
        uintptr_t   uintptr; // Metadata or native instruction ptr
        jl_value_t* jlvalue; // Pointer to GC-managed value
    };
} jl_bt_element_t;

#define JL_BT_NON_PTR_ENTRY (((uintptr_t)0)-1)
// Maximum size for an extended backtrace entry (likely significantly larger
// than the actual size of 3-4 for an interpreter frame)
#define JL_BT_MAX_ENTRY_SIZE 16

STATIC_INLINE int jl_bt_is_native(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    return bt_entry[0].uintptr != JL_BT_NON_PTR_ENTRY;
}

// Extended backtrace entry header packing; the bit packing is done manually
// for precise layout control for interop with julia side.
STATIC_INLINE uintptr_t jl_bt_entry_descriptor(int ngc, int nptr,
                                               int tag, uintptr_t header) JL_NOTSAFEPOINT
{
    assert(((ngc & 0x7) == ngc) && ((nptr & 0x7) == nptr) && ((tag & 0xf) == tag));
    return (ngc & 0x7) | (nptr & 0x7) << 3 | (tag & 0xf) << 6 | header << 10;
}

// Unpacking of extended backtrace entry data
STATIC_INLINE size_t jl_bt_num_jlvals(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return bt_entry[1].uintptr & 0x7;
}
STATIC_INLINE size_t jl_bt_num_uintvals(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return (bt_entry[1].uintptr >> 3) & 0x7;
}
STATIC_INLINE int jl_bt_entry_tag(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return (bt_entry[1].uintptr >> 6) & 0xf;
}
STATIC_INLINE uintptr_t jl_bt_entry_header(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    assert(!jl_bt_is_native(bt_entry));
    return bt_entry[1].uintptr >> 10;
}

// Return `i`th GC-managed pointer for extended backtrace entry
// The returned value is rooted for the lifetime of the parent exception stack.
STATIC_INLINE jl_value_t *jl_bt_entry_jlvalue(jl_bt_element_t *bt_entry, size_t i) JL_NOTSAFEPOINT
{
    return bt_entry[2 + i].jlvalue;
}

#define JL_BT_INTERP_FRAME_TAG    1  // An interpreter frame

// Number of bt elements in frame.
STATIC_INLINE size_t jl_bt_entry_size(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    return jl_bt_is_native(bt_entry) ?
        1 : 2 + jl_bt_num_jlvals(bt_entry) + jl_bt_num_uintvals(bt_entry);
}

//------------------------------
// Stack walking and debug info lookup

// Function metadata arising from debug info lookup of instruction pointer
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
uint64_t jl_trygetUnwindInfo(uint64_t dwBase);
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
       (UNW_VERSION_MAJOR == 1 && UNW_VERSION_MINOR != 0 && UNW_VERSION_MINOR != 1))
// Enable our memory manager only for libunwind with our patch or
// on a newer release
#    define JL_UNW_HAS_FORMAT_IP 1
#  endif
#else
// Unwinding is disabled
typedef int bt_context_t;
typedef int bt_cursor_t;
#endif
size_t rec_backtrace(jl_bt_element_t *bt_data, size_t maxsize, int skip) JL_NOTSAFEPOINT;
// Record backtrace from a signal handler. `ctx` is the context of the code
// which was asynchronously interrupted.
size_t rec_backtrace_ctx(jl_bt_element_t *bt_data, size_t maxsize, bt_context_t *ctx,
                         jl_gcframe_t *pgcstack, int lockless) JL_NOTSAFEPOINT;
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(jl_bt_element_t *bt_data, size_t maxsize, bt_context_t *ctx, jl_gcframe_t *pgcstack) JL_NOTSAFEPOINT;
#endif
JL_DLLEXPORT jl_value_t *jl_get_backtrace(void);
void jl_critical_error(int sig, bt_context_t *context, jl_bt_element_t *bt_data, size_t *bt_size);
JL_DLLEXPORT void jl_raise_debugger(void);
int jl_getFunctionInfo(jl_frame_t **frames, uintptr_t pointer, int skipC, int noInline) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_gdblookup(void* ip) JL_NOTSAFEPOINT;
void jl_print_native_codeloc(uintptr_t ip) JL_NOTSAFEPOINT;
void jl_print_bt_entry_codeloc(jl_bt_element_t *bt_data) JL_NOTSAFEPOINT;
// *to is NULL or malloc'd pointer, from is allowed to be NULL
STATIC_INLINE char *jl_copy_str(char **to, const char *from) JL_NOTSAFEPOINT
{
    if (!from) {
        free(*to);
        *to = NULL;
        return NULL;
    }
    size_t len = strlen(from) + 1;
    *to = (char*)realloc_s(*to, len);
    memcpy(*to, from, len);
    return *to;
}

JL_DLLEXPORT size_t jl_capture_interp_frame(jl_bt_element_t *bt_data,
        void *frameend, size_t space_remaining) JL_NOTSAFEPOINT;

//--------------------------------------------------
// Exception stack access and manipulation

// Exception stack: a stack of pairs of (exception,raw_backtrace).
// The stack may be traversed and accessed with the functions below.
struct _jl_excstack_t { // typedef in julia.h
    size_t top;
    size_t reserved_size;
    // Pack all stack entries into a growable buffer to amortize allocation
    // across repeated exception handling.
    // Layout: [bt_data1... bt_size1 exc1  bt_data2... bt_size2 exc2  ..]
    // jl_bt_element_t data[]; // Access with jl_excstack_raw
};

STATIC_INLINE jl_bt_element_t *jl_excstack_raw(jl_excstack_t *stack) JL_NOTSAFEPOINT
{
    return (jl_bt_element_t*)(stack + 1);
}

// Exception stack access
STATIC_INLINE jl_value_t *jl_excstack_exception(jl_excstack_t *stack JL_PROPAGATES_ROOT,
                                                size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack)[itr-1].jlvalue;
}
STATIC_INLINE size_t jl_excstack_bt_size(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack)[itr-2].uintptr;
}
STATIC_INLINE jl_bt_element_t *jl_excstack_bt_data(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return jl_excstack_raw(stack) + itr-2 - jl_excstack_bt_size(stack, itr);
}
// Exception stack iteration (start at itr=stack->top, stop at itr=0)
STATIC_INLINE size_t jl_excstack_next(jl_excstack_t *stack, size_t itr) JL_NOTSAFEPOINT
{
    return itr-2 - jl_excstack_bt_size(stack, itr);
}
// Exception stack manipulation
void jl_reserve_excstack(jl_excstack_t **stack JL_REQUIRE_ROOTED_SLOT,
                         size_t reserved_size);
void jl_push_excstack(jl_excstack_t **stack JL_REQUIRE_ROOTED_SLOT JL_ROOTING_ARGUMENT,
                      jl_value_t *exception JL_ROOTED_ARGUMENT,
                      jl_bt_element_t *bt_data, size_t bt_size);
void jl_copy_excstack(jl_excstack_t *dest, jl_excstack_t *src) JL_NOTSAFEPOINT;

//--------------------------------------------------
// congruential random number generator
// for a small amount of thread-local randomness
// we could just use libc:`rand()`, but we want to ensure this is fast
STATIC_INLINE void seed_cong(uint64_t *seed)
{
    *seed = rand();
}
STATIC_INLINE void unbias_cong(uint64_t max, uint64_t *unbias)
{
    *unbias = UINT64_MAX - ((UINT64_MAX % max) + 1);
}
STATIC_INLINE uint64_t cong(uint64_t max, uint64_t unbias, uint64_t *seed)
{
    while ((*seed = 69069 * (*seed) + 362437) > unbias)
        ;
    return *seed % max;
}

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

void *jl_get_library_(const char *f_lib, int throw_err);
#define jl_get_library(f_lib) jl_get_library_(f_lib, 1)
JL_DLLEXPORT void *jl_load_and_lookup(const char *f_lib, const char *f_name,
                                      void **hnd);
JL_DLLEXPORT jl_value_t *jl_get_cfunction_trampoline(
    jl_value_t *fobj, jl_datatype_t *result, htable_t *cache, jl_svec_t *fill,
    void *(*init_trampoline)(void *tramp, void **nval),
    jl_unionall_t *env, jl_value_t **vals);


// Windows only
#define JL_EXE_LIBNAME ((const char*)1)
#define JL_DL_LIBNAME ((const char*)2)
const char *jl_dlfind_win32(const char *name);

// libuv wrappers:
JL_DLLEXPORT int jl_fs_rename(const char *src_path, const char *dst_path);

#ifdef SEGV_EXCEPTION
extern JL_DLLEXPORT jl_value_t *jl_segv_exception;
#endif

// -- Runtime intrinsics -- //
JL_DLLEXPORT const char *jl_intrinsic_name(int f) JL_NOTSAFEPOINT;
unsigned jl_intrinsic_nargs(int f) JL_NOTSAFEPOINT;

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
JL_DLLEXPORT jl_value_t *jl_sqrt_llvm_fast(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_abs_float(jl_value_t *a);
JL_DLLEXPORT jl_value_t *jl_copysign_float(jl_value_t *a, jl_value_t *b);
JL_DLLEXPORT jl_value_t *jl_flipsign_int(jl_value_t *a, jl_value_t *b);

JL_DLLEXPORT jl_value_t *jl_arraylen(jl_value_t *a);
JL_DLLEXPORT int jl_stored_inline(jl_value_t *el_type);
JL_DLLEXPORT jl_value_t *(jl_array_data_owner)(jl_array_t *a);
JL_DLLEXPORT int jl_array_isassigned(jl_array_t *a, size_t i);

JL_DLLEXPORT uintptr_t jl_object_id_(jl_value_t *tv, jl_value_t *v) JL_NOTSAFEPOINT;
JL_DLLEXPORT void jl_set_next_task(jl_task_t *task);

// -- synchronization utilities -- //

extern jl_mutex_t typecache_lock;
extern jl_mutex_t codegen_lock;
extern jl_mutex_t safepoint_lock;

#if defined(__APPLE__)
void jl_mach_gc_end(void);
#endif

// -- smallintset.c -- //

typedef uint_t (*smallintset_hash)(size_t val, jl_svec_t *data);
typedef int (*smallintset_eq)(size_t val, const void *key, jl_svec_t *data, uint_t hv);
ssize_t jl_smallintset_lookup(jl_array_t *cache, smallintset_eq eq, const void *key, jl_svec_t *data, uint_t hv);
void jl_smallintset_insert(jl_array_t **pcache, jl_value_t *parent, smallintset_hash hash, size_t val, jl_svec_t *data);

// -- typemap.c -- //

// a descriptor of a jl_typemap_t that gets
// passed around as self-documentation of the parameters of the type
struct jl_typemap_info {
    int8_t unsorted; // whether this should be unsorted
    jl_datatype_t **jl_contains; // the type that is being put in this
};

void jl_typemap_insert(jl_typemap_t **cache, jl_value_t *parent,
        jl_typemap_entry_t *newrec, int8_t offs,
        const struct jl_typemap_info *tparams);
jl_typemap_entry_t *jl_typemap_alloc(
        jl_tupletype_t *type, jl_tupletype_t *simpletype, jl_svec_t *guardsigs,
        jl_value_t *newvalue, size_t min_world, size_t max_world);

struct jl_typemap_assoc {
    // inputs
    jl_value_t *const types;
    size_t const world;
    // outputs
    jl_svec_t *env; // subtype env (initialize to null to perform intersection without an environment)
    size_t min_valid;
    size_t max_valid;
};

jl_typemap_entry_t *jl_typemap_assoc_by_type(
        jl_typemap_t *ml_or_cache JL_PROPAGATES_ROOT,
        struct jl_typemap_assoc *search,
        int8_t offs, uint8_t subtype);

jl_typemap_entry_t *jl_typemap_level_assoc_exact(jl_typemap_level_t *cache, jl_value_t *arg1, jl_value_t **args, size_t n, int8_t offs, size_t world);
jl_typemap_entry_t *jl_typemap_entry_assoc_exact(jl_typemap_entry_t *mn, jl_value_t *arg1, jl_value_t **args, size_t n, size_t world);
STATIC_INLINE jl_typemap_entry_t *jl_typemap_assoc_exact(
    jl_typemap_t *ml_or_cache JL_PROPAGATES_ROOT,
    jl_value_t *arg1, jl_value_t **args, size_t n, int8_t offs, size_t world)
{
    // NOTE: This function is a huge performance hot spot!!
    if (jl_typeof(ml_or_cache) == (jl_value_t *)jl_typemap_entry_type) {
        return jl_typemap_entry_assoc_exact(
            (jl_typemap_entry_t *)ml_or_cache, arg1, args, n, world);
    }
    else if (jl_typeof(ml_or_cache) == (jl_value_t*)jl_typemap_level_type) {
        return jl_typemap_level_assoc_exact(
            (jl_typemap_level_t *)ml_or_cache, arg1, args, n, offs, world);
    }
    return NULL;
}
typedef int (*jl_typemap_visitor_fptr)(jl_typemap_entry_t *l, void *closure);
int jl_typemap_visitor(jl_typemap_t *a, jl_typemap_visitor_fptr fptr, void *closure);

struct typemap_intersection_env;
typedef int (*jl_typemap_intersection_visitor_fptr)(jl_typemap_entry_t *l, struct typemap_intersection_env *closure);
struct typemap_intersection_env {
    // input values
    jl_typemap_intersection_visitor_fptr const fptr; // fptr to call on a match
    jl_value_t *const type; // type to match
    jl_value_t *const va; // the tparam0 for the vararg in type, if applicable (or NULL)
    // output values
    jl_value_t *ti; // intersection type
    jl_svec_t *env; // intersection env (initialize to null to perform intersection without an environment)
    int issubty;    // if `a <: b` is true in `intersect(a,b)`
};
int jl_typemap_intersection_visitor(jl_typemap_t *a, int offs, struct typemap_intersection_env *closure);

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
void jl_depwarn(const char *msg, jl_value_t *sym);

// Log `msg` to the current logger by calling CoreLogging.logmsg_shim() on the
// julia side. If any of module, group, id, file or line are NULL, these will
// be passed to the julia side as `nothing`.  If `kwargs` is NULL an empty set
// of keyword arguments will be passed.
void jl_log(int level, jl_value_t *module, jl_value_t *group, jl_value_t *id,
            jl_value_t *file, jl_value_t *line, jl_value_t *kwargs,
            jl_value_t *msg);

int isabspath(const char *in);

extern jl_sym_t *call_sym;    extern jl_sym_t *invoke_sym;
extern jl_sym_t *empty_sym;   extern jl_sym_t *top_sym;
extern jl_sym_t *module_sym;  extern jl_sym_t *slot_sym;
extern jl_sym_t *export_sym;  extern jl_sym_t *import_sym;
extern jl_sym_t *toplevel_sym; extern jl_sym_t *quote_sym;
extern jl_sym_t *line_sym;    extern jl_sym_t *jl_incomplete_sym;
extern jl_sym_t *goto_sym;    extern jl_sym_t *goto_ifnot_sym;
extern jl_sym_t *return_sym;
extern jl_sym_t *lambda_sym;  extern jl_sym_t *assign_sym;
extern jl_sym_t *globalref_sym; extern jl_sym_t *do_sym;
extern jl_sym_t *method_sym;  extern jl_sym_t *core_sym;
extern jl_sym_t *enter_sym;   extern jl_sym_t *leave_sym;
extern jl_sym_t *exc_sym;     extern jl_sym_t *error_sym;
extern jl_sym_t *new_sym;     extern jl_sym_t *using_sym;
extern jl_sym_t *splatnew_sym;
extern jl_sym_t *pop_exception_sym;
extern jl_sym_t *const_sym;   extern jl_sym_t *thunk_sym;
extern jl_sym_t *foreigncall_sym;
extern jl_sym_t *global_sym; extern jl_sym_t *list_sym;
extern jl_sym_t *dot_sym;    extern jl_sym_t *newvar_sym;
extern jl_sym_t *boundscheck_sym; extern jl_sym_t *inbounds_sym;
extern jl_sym_t *aliasscope_sym; extern jl_sym_t *popaliasscope_sym;
extern jl_sym_t *copyast_sym; extern jl_sym_t *cfunction_sym;
extern jl_sym_t *pure_sym; extern jl_sym_t *loopinfo_sym;
extern jl_sym_t *meta_sym; extern jl_sym_t *inert_sym;
extern jl_sym_t *polly_sym; extern jl_sym_t *unused_sym;
extern jl_sym_t *static_parameter_sym; extern jl_sym_t *inline_sym;
extern jl_sym_t *noinline_sym; extern jl_sym_t *generated_sym;
extern jl_sym_t *generated_only_sym; extern jl_sym_t *isdefined_sym;
extern jl_sym_t *propagate_inbounds_sym; extern jl_sym_t *specialize_sym;
extern jl_sym_t *nospecialize_sym; extern jl_sym_t *macrocall_sym;
extern jl_sym_t *colon_sym; extern jl_sym_t *hygienicscope_sym;
extern jl_sym_t *throw_undef_if_not_sym; extern jl_sym_t *getfield_undefref_sym;
extern jl_sym_t *gc_preserve_begin_sym; extern jl_sym_t *gc_preserve_end_sym;
extern jl_sym_t *failed_sym; extern jl_sym_t *done_sym; extern jl_sym_t *runnable_sym;
extern jl_sym_t *coverageeffect_sym; extern jl_sym_t *escape_sym;
extern jl_sym_t *optlevel_sym;
extern jl_sym_t *atom_sym; extern jl_sym_t *statement_sym; extern jl_sym_t *all_sym;

struct _jl_sysimg_fptrs_t;

void jl_register_fptrs(uint64_t sysimage_base, const struct _jl_sysimg_fptrs_t *fptrs,
                       jl_method_instance_t **linfos, size_t n);

#if jl_has_builtin(__builtin_unreachable) || defined(_COMPILER_GCC_) || defined(_COMPILER_INTEL_)
#  define jl_unreachable() __builtin_unreachable()
#else
#  define jl_unreachable() ((void)jl_assume(0))
#endif

// Tools for locally disabling spurious compiler warnings
//
// Particular calls which are used elsewhere in the code include:
//
// * JL_GCC_IGNORE_START(-Wclobbered) - gcc misidentifies some variables which
//   are used inside a JL_TRY as being "clobbered" if JL_CATCH is entered. This
//   warning is spurious if the variable is not modified inside the JL_TRY.
//   See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=65041
#ifdef _COMPILER_GCC_
#define JL_DO_PRAGMA(s) _Pragma(#s)
#define JL_GCC_IGNORE_START(warning) \
    JL_DO_PRAGMA(GCC diagnostic push) \
    JL_DO_PRAGMA(GCC diagnostic ignored warning)
#define JL_GCC_IGNORE_STOP \
    JL_DO_PRAGMA(GCC diagnostic pop)
#else
#define JL_GCC_IGNORE_START(w)
#define JL_GCC_IGNORE_STOP
#endif // _COMPILER_GCC_

#ifdef __clang_analyzer__
  // Not a safepoint (so it dosn't free other values), but an artificial use.
  // Usually this is unnecessary because the analyzer can see all real uses,
  // but sometimes real uses are harder for the analyzer to see, or it may
  // give up before it sees it, so this can be helpful to be explicit.
  void JL_GC_ASSERT_LIVE(jl_value_t *v) JL_NOTSAFEPOINT;
#else
  #define JL_GC_ASSERT_LIVE(x) (void)(x)
#endif

#ifdef __cplusplus
}
#endif

#endif
