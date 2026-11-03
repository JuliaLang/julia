// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_plt_resolver (src/arch/<arch>/plt_resolver.S), the shared resolver
// for lazily bound calls (src/plt.h). Calls go through an entry's slot, which
// starts out pointing at its per-entry trampoline: the first call must reach
// the target through the resolver with all arguments, the return value and the
// stack alignment as in a direct call, while the resolution can unwind back
// into the caller (or be left by longjmp, the way Julia throws when a symbol
// cannot be found), and later calls must go straight to the target. The
// trampolines and slot jump thunks are those codegen emits (see
// plt_trampolines.S).

#include <setjmp.h>
#include <string.h>
#include <stdarg.h>
#include "archtest.h"

// These tests link the assembly directly into the executable.
#define JL_LIBRARY_STATIC
#include "../../plt.h"

#define NENTRIES 8
// The entries, their slots, and the variants of the trampolines and slot jump
// thunks (see plt_trampolines.S)
extern jl_plt_entry_t test_plt_entries[NENTRIES];
extern void *test_plt_slots[NENTRIES];
typedef int64_t thunk_t(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                        int64_t, int64_t);
extern const struct { const char *name; void (*entries[NENTRIES])(void); } test_plt_trampolines[];
extern const struct { const char *name; thunk_t *thunk; } test_plt_thunks[];
extern thunk_t test_plt_thunk_0;
static void (*const *trampolines)(void); // those of the variant under test
static void *targets[NENTRIES];
static int resolutions[NENTRIES];

// What the resolution does besides resolving, for the test of one entry.
static enum { RESOLVE, RESOLVE_WITH_BACKTRACE, FAIL, CLOBBER_AVX, CLOBBER_AVX512 } resolve_mode;
static backtrace_t resolve_bt, resolve_fp_bt;
static uintptr_t resolver_ip;
static jmp_buf resolve_failed;

extern void test_plt_clobber_args(void);
#if (defined(__x86_64__) && !defined(_WIN32)) || defined(__i386__)
extern void test_plt_clobber_avx(void), test_plt_clobber_avx512(void);
#endif

#ifdef __i386__
static __attribute__((noinline, target("no-sse,no-sse2"))) void check_resolver_x87(void)
{
    volatile double x = 1.25;
    CHECK(x + 0.5 == 1.75);
}
#endif

// The entries' resolve function: bind the slot to the target and return it.
static void *resolve(jl_plt_entry_t *entry)
{
    int i = (int)(entry - test_plt_entries);
#ifdef __i386__
    // MMX arguments must not leave the C resolver with a full x87 stack.
    check_resolver_x87();
#endif
    resolutions[i]++;
    if (resolve_mode == RESOLVE_WITH_BACKTRACE) {
        resolver_ip = RETURN_IP();
        get_backtrace(&resolve_bt);
#ifdef HAVE_FP_BACKTRACE
        get_fp_backtrace(&resolve_fp_bt);
#endif
    }
    else if (resolve_mode == FAIL)
        longjmp(resolve_failed, 1);
    test_plt_clobber_args();
#if (defined(__x86_64__) && !defined(_WIN32)) || defined(__i386__)
    if (resolve_mode == CLOBBER_AVX)
        test_plt_clobber_avx();
    else if (resolve_mode == CLOBBER_AVX512)
        test_plt_clobber_avx512();
#endif
    *entry->slot = targets[i];
    return targets[i];
}

static void reset_entry(int i, void *target)
{
    test_plt_entries[i].resolve = resolve;
    test_plt_entries[i].slot = &test_plt_slots[i];
    test_plt_slots[i] = (void*)trampolines[i];
    targets[i] = target;
    resolutions[i] = 0;
}

// The targets. They take more arguments of each class than any architecture
// passes in registers, so that every argument register carries one.
typedef struct { int64_t a, b, c, d; } big_t;
typedef double v2d __attribute__((vector_size(16)));
static volatile int aligned;
static uintptr_t target_return_ip;

static __attribute__((noinline)) int64_t ints(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e,
                                              int64_t f, int64_t g, int64_t h, int64_t i, int64_t j)
{
    aligned = STACK_ALIGNED_AT_ENTRY();
    target_return_ip = RETURN_IP();
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j;
}

static __attribute__((noinline)) double floats(double a, double b, double c, double d, double e,
                                               double f, double g, double h, double i, double j,
                                               double k, double l, double m, double n, float o,
                                               int32_t p)
{
    return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l + 13*m +
        14*n + 15*o + 16*p;
}

static __attribute__((noinline)) double vectors(v2d a, v2d b, v2d c, v2d d, v2d e, v2d f, v2d g,
                                                v2d h, v2d i, v2d j, v2d k, v2d l, v2d m)
{
    v2d s = a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i + 10*j + 11*k + 12*l + 13*m;
    return s[0] + 1000*s[1];
}

static __attribute__((noinline)) big_t structs(big_t x, int8_t y, int16_t z)
{
    big_t r = {x.d + y, x.c + z, x.b, x.a};
    return r;
}

static __attribute__((noinline)) int varargs(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int i = va_arg(ap, int);
    double d = va_arg(ap, double);
    long l = va_arg(ap, long);
    double e = va_arg(ap, double);
    va_end(ap);
    return strcmp(fmt, "%d %f %ld %f") == 0 && i == 7 && d == 2.5 && l == 1L << 30 && e == -0.5;
}

#if defined(__i386__)
// Conventions that pass arguments in registers (ecx, edx)
static __attribute__((noinline, fastcall)) int fastcall_target(int a, int b, int c)
{
    return a * 100 + b * 10 + c;
}
static __attribute__((noinline, thiscall)) int thiscall_target(int a, int b)
{
    return a * 10 + b;
}
#endif

typedef int64_t (*ints_t)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                          int64_t, int64_t);
typedef double (*floats_t)(double, double, double, double, double, double, double, double, double,
                           double, double, double, double, double, float, int32_t);
typedef double (*vectors_t)(v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d, v2d);
typedef big_t (*structs_t)(big_t, int8_t, int16_t);
typedef int (*varargs_t)(const char *, ...);

// Calls through the slot, as a stub does.
static uintptr_t caller_return_ip;
static backtrace_t after_fp_bt;
static __attribute__((noinline)) int64_t call_ints(int i)
{
    caller_return_ip = RETURN_IP();
    int64_t r = ((ints_t)test_plt_slots[i])(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
#ifdef HAVE_FP_BACKTRACE
    get_fp_backtrace(&after_fp_bt);
#endif
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
    return r;
}

// The same from a caller without a frame pointer, which unwinding must find by
// the stack pointer
#if defined(__GNUC__) && !defined(__clang__)
#define OMIT_FRAME_POINTER __attribute__((optimize("omit-frame-pointer")))
#else
#define OMIT_FRAME_POINTER
#endif
static __attribute__((noinline)) OMIT_FRAME_POINTER int64_t call_ints_without_fp(int i)
{
    caller_return_ip = RETURN_IP();
    int64_t r = ((ints_t)test_plt_slots[i])(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    __asm__ volatile ("" ::: "memory"); // keep the call out of tail position
    return r;
}

#ifdef _WIN64
// Unwind synthetic contexts at entry, in the body, and on either instruction
// of the epilogue. A backtrace from resolve() only exercises the body.
static void test_windows_unwind(void)
{
    DWORD64 image_base;
    PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry((DWORD64)(uintptr_t)jl_plt_resolver,
                                                 &image_base, NULL);
    CHECK(fn != NULL);
    if (!fn)
        return;
    uintptr_t begin = image_base + fn->BeginAddress;
    uintptr_t end = image_base + fn->EndAddress;
    const unsigned char *unwind_info = (const unsigned char*)(image_base + fn->UnwindData);
    _Alignas(16) uint64_t stack[128];
    memset(stack, 0x5a, sizeof(stack));
    uintptr_t user_sp = (uintptr_t)&stack[65]; // 8 mod 16, as after a call
    uintptr_t return_ip = (uintptr_t)call_ints;
    *(uintptr_t*)user_sp = return_ip;
    const unsigned char restore_sp[] = {0x48, 0x81, 0xc4, 0x98, 0, 0, 0};
    uintptr_t epilogue = 0;
    for (uintptr_t pc = begin; pc + sizeof(restore_sp) <= end; pc++) {
        if (!memcmp((const void*)pc, restore_sp, sizeof(restore_sp))) {
            epilogue = pc;
            break;
        }
    }
    CHECK(epilogue != 0);
    if (!epilogue)
        return;
    struct { uintptr_t ip; size_t frame_size; } positions[] = {
        {begin, 16},
        {begin + unwind_info[1], 0x98}, // first instruction after the prologue
        {epilogue, 0x98},
        {epilogue + sizeof(restore_sp), 0}, // tail jump, after restoring rsp
    };
    for (size_t i = 0; i < sizeof(positions)/sizeof(positions[0]); i++) {
        CONTEXT ctx = {0};
        void *handler_data;
        DWORD64 establisher;
        ctx.ContextFlags = CONTEXT_FULL;
        ctx.Rip = positions[i].ip;
        ctx.Rsp = user_sp - positions[i].frame_size;
        DWORD64 frame_base;
        PRUNTIME_FUNCTION frame_fn = RtlLookupFunctionEntry(ctx.Rip, &frame_base, NULL);
        CHECK(frame_fn != NULL);
        if (!frame_fn)
            continue;
        RtlVirtualUnwind(UNW_FLAG_NHANDLER, frame_base, ctx.Rip, frame_fn, &ctx,
                         &handler_data, &establisher, NULL);
        CHECK(ctx.Rip == return_ip);
        CHECK(ctx.Rsp == user_sp + sizeof(uintptr_t));
    }
}
#endif

#if (defined(__x86_64__) && !defined(_WIN32)) || defined(__i386__)
// Exercise every vector argument register and a stack argument, on cold and
// bound calls, with different values in every register and lane.
#define WIDE_VECTOR_TEST(name, feature, lanes, mode) \
    typedef double name##_t __attribute__((vector_size(lanes * sizeof(double)))); \
    static __attribute__((noinline, target(feature))) name##_t name##_target( \
        name##_t a, name##_t b, name##_t c, name##_t d, name##_t e, \
        name##_t f, name##_t g, name##_t h, name##_t i) \
    { \
        return a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i; \
    } \
    static __attribute__((noinline, target(feature))) void test_##name(void) \
    { \
        reset_entry(7, (void*)name##_target); \
        resolve_mode = mode; \
        name##_t args[9]; \
        for (int i = 0; i < 9; i++) \
            for (int j = 0; j < lanes; j++) \
                args[i][j] = 10*(i + 1) + j; \
        for (int n = 0; n < 2; n++) { \
            name##_t r = ((name##_t (*)(name##_t, name##_t, name##_t, name##_t, \
                name##_t, name##_t, name##_t, name##_t, name##_t))test_plt_slots[7])( \
                args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]); \
            for (int j = 0; j < lanes; j++) \
                CHECK(r[j] == 2850 + 45*j); \
        } \
        CHECK(resolutions[7] == 1); \
    }

#ifdef __i386__
WIDE_VECTOR_TEST(sse, "sse2", 2, RESOLVE)
#endif
WIDE_VECTOR_TEST(avx, "avx", 4, CLOBBER_AVX)
WIDE_VECTOR_TEST(avx512, "avx512f", 8, CLOBBER_AVX512)
#undef WIDE_VECTOR_TEST
#endif

#ifdef __i386__
typedef int32_t v2i __attribute__((vector_size(8)));
static __attribute__((noinline, target("mmx"))) v2i mmx_target(v2i a, v2i b, v2i c, v2i d)
{
    return a + 2*b + 3*c + 4*d;
}

static __attribute__((noinline, target("mmx"))) void test_mmx(void)
{
    reset_entry(7, (void*)mmx_target);
    resolve_mode = RESOLVE;
    for (int n = 0; n < 2; n++) {
        v2i r = ((v2i (*)(v2i, v2i, v2i, v2i))test_plt_slots[7])(
            (v2i){1, 2}, (v2i){3, 4}, (v2i){5, 6}, (v2i){7, 8});
        CHECK(r[0] == 50 && r[1] == 60);
    }
    __asm__ volatile ("emms" ::: "mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7");
    CHECK(resolutions[7] == 1);
}
#endif

// A call through a slot jump thunk, as callers call stubs
static void test_thunk(thunk_t *thunk)
{
    reset_entry(0, (void*)ints);
    for (int n = 0; n < 2; n++) {
        aligned = 0;
        CHECK(thunk(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385);
        CHECK(aligned);
    }
    CHECK(resolutions[0] == 1);
    // and a direct call, which on PPC64LE goes through the linker's stub that
    // saves the TOC, as the thunk needs
    reset_entry(0, (void*)ints);
    CHECK(test_plt_thunk_0(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) == 385);
    CHECK(resolutions[0] == 1);
}

static void test_trampolines(void)
{
    // The first call resolves, later calls go straight to the target.
    for (int i = 0; i < NENTRIES; i++)
        reset_entry(i, NULL);
    resolve_mode = RESOLVE;
    reset_entry(0, (void*)ints);
    for (int n = 0; n < 2; n++) {
        aligned = 0;
        CHECK(call_ints(0) == 385);
        CHECK(aligned);
    }
    CHECK(resolutions[0] == 1);
    CHECK(test_plt_slots[0] == (void*)ints);

    reset_entry(1, (void*)floats);
    CHECK(((floats_t)test_plt_slots[1])(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0.5f, -1) ==
          1015 + 7.5 - 16);
    reset_entry(2, (void*)vectors);
    v2d v[13];
    for (int i = 0; i < 13; i++)
        v[i] = (v2d){i + 1, (i + 1)*(i + 1)};
    CHECK(((vectors_t)test_plt_slots[2])(v[0], v[1], v[2], v[3], v[4], v[5], v[6],
                               v[7], v[8], v[9], v[10], v[11], v[12]) == 819 + 1000*8281);
    reset_entry(3, (void*)structs);
    big_t x = {1, 2, 3, 4};
    big_t r = ((structs_t)test_plt_slots[3])(x, -3, 300);
    CHECK(r.a == 1 && r.b == 303 && r.c == 2 && r.d == 1);
    reset_entry(4, (void*)varargs);
    CHECK(((varargs_t)test_plt_slots[4])("%d %f %ld %f", 7, 2.5, 1L << 30, -0.5));
#if defined(__i386__)
    reset_entry(5, (void*)fastcall_target);
    CHECK(((int (__attribute__((fastcall)) *)(int, int, int))test_plt_slots[5])(1, 2, 3) == 123);
    reset_entry(6, (void*)thiscall_target);
    CHECK(((int (__attribute__((thiscall)) *)(int, int))test_plt_slots[6])(4, 5) == 45);
#endif
    for (int i = 1; i < NENTRIES; i++)
        CHECK(targets[i] == NULL || resolutions[i] == 1);

    // The resolution unwinds through the resolver straight into the caller and
    // on to its caller, both by unwind info and along the frame records, and
    // the caller's frame records are intact after the call.
    reset_entry(7, (void*)ints);
    resolve_mode = RESOLVE_WITH_BACKTRACE;
    CHECK(call_ints(7) == 385);
    int in_caller = backtrace_index(&resolve_bt, target_return_ip);
    CHECK(in_caller >= 1 && resolve_bt.ips[in_caller - 1] == resolver_ip);
    CHECK(backtrace_index(&resolve_bt, caller_return_ip) > in_caller);
#ifdef HAVE_FP_BACKTRACE
    in_caller = backtrace_index(&resolve_fp_bt, target_return_ip);
    CHECK(in_caller >= 1 && resolve_fp_bt.ips[in_caller - 1] == resolver_ip);
    CHECK(backtrace_index(&resolve_fp_bt, caller_return_ip) > in_caller);
    CHECK(backtrace_index(&after_fp_bt, caller_return_ip) >= 0);
#endif
    reset_entry(7, (void*)ints);
    CHECK(call_ints_without_fp(7) == 385);
    in_caller = backtrace_index(&resolve_bt, target_return_ip);
    CHECK(in_caller >= 1 && resolve_bt.ips[in_caller - 1] == resolver_ip);
    CHECK(backtrace_index(&resolve_bt, caller_return_ip) > in_caller);

    // A resolution that fails leaves the resolver by longjmp, with the slot
    // still unbound, so that it resolves on the next call.
    reset_entry(7, (void*)ints);
    resolve_mode = FAIL;
    if (!setjmp(resolve_failed))
        call_ints(7);
    CHECK(resolutions[7] == 1);
    CHECK(test_plt_slots[7] == (void*)trampolines[7]);
    resolve_mode = RESOLVE;
    CHECK(call_ints(7) == 385);
    CHECK(resolutions[7] == 2);
#if (defined(__x86_64__) && !defined(_WIN32)) || defined(__i386__)
#ifdef __i386__
    test_mmx();
    test_sse();
#endif
    if (__builtin_cpu_supports("avx"))
        test_avx();
    if (__builtin_cpu_supports("avx512f"))
        test_avx512();
#endif
}

int main(void)
{
    for (size_t v = 0; test_plt_trampolines[v].name; v++) {
        printf("  %s\n", test_plt_trampolines[v].name);
        trampolines = test_plt_trampolines[v].entries;
        test_trampolines();
    }
    for (size_t v = 0; test_plt_thunks[v].name; v++) {
        printf("  %s\n", test_plt_thunks[v].name);
        test_thunk(test_plt_thunks[v].thunk);
    }
#ifdef _WIN64
    test_windows_unwind();
#endif
    ARCHTEST_DONE("plt_resolver");
}
