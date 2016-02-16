// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef IA_MISC_H
#define IA_MISC_H

#include <stdint.h>
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif
#include "support/dtypes.h"

#ifdef __MIC__
STATIC_INLINE void cpu_sfence(void)
{
    __asm__ __volatile__ ("":::"memory");
}

STATIC_INLINE void cpu_lfence(void)
{
    __asm__ __volatile__ ("":::"memory");
}

#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
STATIC_INLINE void cpu_sfence(void)
{
    _mm_sfence();
}

STATIC_INLINE void cpu_lfence(void)
{
    _mm_lfence();
}

#elif defined(_CPU_AARCH64_)
STATIC_INLINE void cpu_sfence(void)
{
    __asm__ volatile ("dmb ishst" ::: "memory");
}

STATIC_INLINE void cpu_lfence(void)
{
    __asm__ volatile ("dmb ishld" ::: "memory");
}

#elif defined(_CPU_ARM_) && __ARM_ARCH >= 7

STATIC_INLINE void cpu_sfence(void)
{
    __asm__ volatile ("dmb" ::: "memory");
}

STATIC_INLINE void cpu_lfence(void)
{
    __asm__ volatile ("dmb" ::: "memory");
}

#else

STATIC_INLINE void cpu_sfence(void)
{
    // GCC intrinsic
    __atomic_thread_fence(__ATOMIC_SEQ_CST);
}

STATIC_INLINE void cpu_lfence(void)
{
    // GCC intrinsic
    __atomic_thread_fence(__ATOMIC_SEQ_CST);
}

#endif /* __MIC__ */


#endif  /* IA_MISC_H */
