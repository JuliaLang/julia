// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef IA_MISC_H
#define IA_MISC_H

#include <stdint.h>
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif
#include "support/dtypes.h"

#if defined(__i386__) && defined(__GNUC__) && !defined(__SSE2__)
#error Julia can only be built for architectures above Pentium 4. Pass -march=pentium4, or set MARCH=pentium4 and ensure that -march is not passed separately with an older architecture.
#endif

#ifdef __MIC__

STATIC_INLINE void cpu_pause(void)
{
    _mm_delay_64(100);
}

STATIC_INLINE void cpu_mfence(void)
{
    __asm__ __volatile__ ("":::"memory");
}

STATIC_INLINE void cpu_sfence(void)
{
    __asm__ __volatile__ ("":::"memory");
}

STATIC_INLINE void cpu_lfence(void)
{
    __asm__ __volatile__ ("":::"memory");
}

#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */

STATIC_INLINE void cpu_pause(void)
{
    _mm_pause();
}

STATIC_INLINE void cpu_mfence(void)
{
    _mm_mfence();
}

STATIC_INLINE void cpu_sfence(void)
{
    _mm_sfence();
}

STATIC_INLINE void cpu_lfence(void)
{
    _mm_lfence();
}

#elif defined(_CPU_AARCH64_)

STATIC_INLINE void cpu_pause(void)
{
    __asm__ volatile ("wfe" ::: "memory");
}

STATIC_INLINE void cpu_mfence(void)
{
    __asm__ volatile ("dmb ish" ::: "memory");
}

STATIC_INLINE void cpu_sfence(void)
{
    __asm__ volatile ("dmb ishst" ::: "memory");
}

STATIC_INLINE void cpu_lfence(void)
{
    __asm__ volatile ("dmb ishld" ::: "memory");
}

#elif defined(_CPU_ARM_) && __ARM_ARCH >= 7

STATIC_INLINE void cpu_pause(void)
{
    __asm__ volatile ("wfe" ::: "memory");
}

STATIC_INLINE void cpu_mfence(void)
{
    __asm__ volatile ("dmb" ::: "memory");
}

STATIC_INLINE void cpu_sfence(void)
{
    cpu_mfence();
}

STATIC_INLINE void cpu_lfence(void)
{
    cpu_mfence();
}

#else

STATIC_INLINE void cpu_pause(void)
{
}

STATIC_INLINE void cpu_mfence(void)
{
    // GCC intrinsic
    __atomic_thread_fence(__ATOMIC_SEQ_CST);
}

STATIC_INLINE void cpu_sfence(void)
{
    cpu_mfence();
}

STATIC_INLINE void cpu_lfence(void)
{
    cpu_mfence();
}

#endif /* __MIC__ */


#endif  /* IA_MISC_H */
