// This file is a part of Julia. License is MIT: https://julialang.org/license

// Helpers for the tests of the assembly in src/arch and cli/arch.

#ifndef JL_ARCHTEST_H
#define JL_ARCHTEST_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#endif
#ifndef _WIN64
#include <unwind.h>
#endif

static int archtest_failures = 0;

#define CHECK(cond) do { \
        if (!(cond)) { \
            fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, #cond); \
            archtest_failures++; \
        } \
    } while (0)

#define ARCHTEST_DONE(name) do { \
        if (archtest_failures) { \
            fprintf(stderr, "%s: %d check(s) failed\n", name, archtest_failures); \
            return 1; \
        } \
        printf("%s: ok\n", name); \
        return 0; \
    } while (0)

// Whether the function that uses this was entered with the stack aligned as
// the ABI requires right after a call. With frame pointers (the tests are
// built with -fno-omit-frame-pointer) the frame address is a fixed offset from
// the stack pointer at entry; elsewhere, fall back to an over-aligned local,
// which compilers place assuming the ABI alignment.
#if defined(__x86_64__) && !defined(_WIN32)
// rbp = entry rsp - 8, and entry rsp is 8 mod 16
#define STACK_ALIGNED_AT_ENTRY() ((((uintptr_t)__builtin_frame_address(0)) & 15) == 0)
#elif defined(__i386__) && !defined(_WIN32)
// ebp = entry esp - 4, and entry esp is 12 mod 16
#define STACK_ALIGNED_AT_ENTRY() ((((uintptr_t)__builtin_frame_address(0) + 8) & 15) == 0)
#elif defined(__aarch64__)
// the frame record is at the bottom of a frame that is a multiple of 16 bytes
#define STACK_ALIGNED_AT_ENTRY() ((((uintptr_t)__builtin_frame_address(0)) & 15) == 0)
#else
#define STACK_ALIGNED_AT_ENTRY() stack_is_aligned()
#endif
// The stack alignment the ABI requires at a call
#if defined(__arm__)
#define ABI_STACK_ALIGN 8
#elif defined(_WIN32) && defined(__i386__)
#define ABI_STACK_ALIGN 4
#else
#define ABI_STACK_ALIGN 16
#endif
// A local aligned as the ABI guarantees is placed relative to the incoming
// stack pointer, so it is misaligned if the stack was. The address is read
// through a volatile, so that the compiler cannot fold the check to true.
static __attribute__((noinline, unused)) int stack_is_aligned(void)
{
    _Alignas(ABI_STACK_ALIGN) volatile char probe[ABI_STACK_ALIGN];
    probe[0] = 0;
    volatile uintptr_t addr = (uintptr_t)probe;
    return (addr & (ABI_STACK_ALIGN - 1)) == 0;
}

// The return address of the function this is used in, as unwinders report it
// (on ARM, without the low bit that marks a return into Thumb code).
#if defined(__arm__)
#define RETURN_IP() ((uintptr_t)__builtin_extract_return_addr(__builtin_return_address(0)) & ~(uintptr_t)1)
#else
#define RETURN_IP() ((uintptr_t)__builtin_extract_return_addr(__builtin_return_address(0)))
#endif

// The return addresses of the frames above the caller, found by walking the
// stack in one of two ways, since unwinders use either:
//  - get_backtrace follows the unwind info of the code (DWARF CFI, ARM EHABI
//    tables, or the unwind tables of Windows x64);
//  - get_fp_backtrace follows the chain of frame records (saved frame pointer
//    and return address), where the platform has a convention for them
//    (HAVE_FP_BACKTRACE): not Windows x64, which always unwinds by its tables,
//    and on ARM only for GCC's ARM-mode code, as the layout of frame records
//    differs between compilers there and GCC's Thumb code keeps none.
typedef struct {
    uintptr_t ips[64];
    int n;
    uintptr_t caller_ip;
} backtrace_t;

#ifdef _WIN64
static __attribute__((noinline)) void get_backtrace(backtrace_t *bt)
{
    bt->caller_ip = RETURN_IP();
    void *frames[sizeof(bt->ips) / sizeof(bt->ips[0])];
    bt->n = RtlCaptureStackBackTrace(0, sizeof(frames) / sizeof(frames[0]), frames, NULL);
    for (int i = 0; i < bt->n; i++)
        bt->ips[i] = (uintptr_t)frames[i];
}
#else
// (on Windows i686, GCC and its unwinder use DWARF CFI too)
static _Unwind_Reason_Code backtrace_frame(struct _Unwind_Context *ctx, void *arg)
{
    backtrace_t *bt = (backtrace_t*)arg;
    if (bt->n == (int)(sizeof(bt->ips) / sizeof(bt->ips[0])))
        return _URC_END_OF_STACK;
#if defined(__arm__)
    // ARM's unwinder (EHABI) only has _Unwind_GetIP, as a macro in GCC's header
    bt->ips[bt->n++] = _Unwind_GetIP(ctx);
#else
    int ip_before_insn = 0;
    bt->ips[bt->n++] = _Unwind_GetIPInfo(ctx, &ip_before_insn);
#endif
    return _URC_NO_REASON;
}

static __attribute__((noinline)) void get_backtrace(backtrace_t *bt)
{
    bt->caller_ip = RETURN_IP();
    bt->n = 0;
    _Unwind_Backtrace(backtrace_frame, bt);
}
#endif

#if !defined(_WIN64) && !(defined(__arm__) && (defined(__thumb__) || defined(__clang__)))
#define HAVE_FP_BACKTRACE 1
// The walk stops at the first frame that does not look like part of the chain
// (one that is not above the previous one, misaligned, or far from it), as code
// without frame pointers (such as a C library's) leaves anything in the frame
// pointer register.
static __attribute__((noinline)) void get_fp_backtrace(backtrace_t *bt)
{
    bt->caller_ip = RETURN_IP();
    bt->n = 0;
    uintptr_t fp = (uintptr_t)__builtin_frame_address(0);
    while (fp != 0 && bt->n < (int)(sizeof(bt->ips) / sizeof(bt->ips[0]))) {
        uintptr_t *f = (uintptr_t*)fp, next, ra;
#if defined(__riscv)
        // the frame pointer is the frame's CFA, with the record just below it
        next = f[-2];
        ra = f[-1];
#elif defined(__arm__)
        // GCC's ARM-mode frame pointer points at the saved return address
        next = f[-1];
        ra = f[0];
#elif defined(__powerpc64__)
        // the back chain, and the return address in the caller's frame
        next = f[0];
        if (next == 0)
            break;
        ra = ((uintptr_t*)next)[2];
#else
        next = f[0];
        ra = f[1];
#endif
        if (ra == 0)
            break;
#if defined(__arm__)
        ra &= ~(uintptr_t)1;
#endif
        bt->ips[bt->n++] = ra;
        if (next <= fp || next - fp > (1 << 20) || next % sizeof(void*) != 0)
            break;
        fp = next;
    }
}
#endif

// Find an exact return address, without assuming a function's size or layout.
static int backtrace_index(const backtrace_t *bt, uintptr_t ip)
{
    for (int i = 0; i < bt->n; i++)
        if (bt->ips[i] == ip)
            return i;
    return -1;
}

#endif
