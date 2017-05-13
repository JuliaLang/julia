// This file is a part of Julia. License is MIT: https://julialang.org/license

// Processor feature detection

#include "julia.h"
#include "julia_internal.h"

extern "C" {

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)

// CPUID

JL_DLLEXPORT void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType)
{
#if defined _MSC_VER
    __cpuid(CPUInfo, InfoType);
#else
    __asm__ __volatile__ (
#if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;" :
        "=S" (CPUInfo[1]),
#else
        "cpuid" :
        "=b" (CPUInfo[1]),
#endif
        "=a" (CPUInfo[0]),
        "=c" (CPUInfo[2]),
        "=d" (CPUInfo[3]) :
        "a" (InfoType)
        );
#endif
}

JL_DLLEXPORT void jl_cpuidex(int32_t CPUInfo[4], int32_t InfoType, int32_t subInfoType)
{
#if defined _MSC_VER
    __cpuidex(CPUInfo, InfoType, subInfoType);
#else
    __asm__ __volatile__ (
#if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;" :
        "=S" (CPUInfo[1]),
#else
        "cpuid" :
        "=b" (CPUInfo[1]),
#endif
        "=a" (CPUInfo[0]),
        "=c" (CPUInfo[2]),
        "=d" (CPUInfo[3]) :
        "a" (InfoType),
        "c" (subInfoType)
        );
#endif
}

// -- set/clear the FZ/DAZ flags on x86 & x86-64 --
static uint32_t get_subnormal_flags(void)
{
    // CPU capabilities not yet inspected.
    int32_t info[4];
    jl_cpuid(info, 0);
    if (info[0] >= 1) {
        jl_cpuid(info, 1);
        if (info[3] & (1 << 26)) {
            // SSE2 supports both FZ and DAZ
            return 0x00008040;
        }
        else if (info[3] & (1 << 25)) {
            // SSE supports only the FZ flag
            return 0x00008000;
        }
    }
    return 0;
}

// Cache of information recovered from `cpuid` since executing `cpuid` it at runtime is slow.
static uint32_t subnormal_flags = get_subnormal_flags();

// Returns non-zero if subnormals go to 0; zero otherwise.
JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return _mm_getcsr() & subnormal_flags;
}

// Return zero on success, non-zero on failure.
JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t flags = subnormal_flags;
    if (flags) {
        uint32_t state = _mm_getcsr();
        if (isZero)
            state |= flags;
        else
            state &= ~flags;
        _mm_setcsr(state);
        return 0;
    }
    else {
        // Report a failure only if user is trying to enable FTZ/DAZ.
        return isZero;
    }
}

#elif defined(_CPU_AARCH64_)

// FZ, bit [24]
static const uint32_t fpcr_fz_mask = 1 << 24;

static inline uint32_t get_fpcr_aarch64(void)
{
    uint32_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    return fpcr;
}

static inline void set_fpcr_aarch64(uint32_t fpcr)
{
    asm volatile("msr fpcr, %0" :: "r"(fpcr));
}

JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return (get_fpcr_aarch64() & fpcr_fz_mask) != 0;
}

JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    uint32_t fpcr = get_fpcr_aarch64();
    fpcr = isZero ? (fpcr | fpcr_fz_mask) : (fpcr & ~fpcr_fz_mask);
    set_fpcr_aarch64(fpcr);
    return 0;
}

#else

JL_DLLEXPORT int32_t jl_get_zero_subnormals(void)
{
    return 0;
}

JL_DLLEXPORT int32_t jl_set_zero_subnormals(int8_t isZero)
{
    return isZero;
}

#endif

}
