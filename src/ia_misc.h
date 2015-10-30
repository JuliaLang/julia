// This file is a part of Julia. License is MIT: http://julialang.org/license

#ifndef IA_MISC_H
#define IA_MISC_H

#include <stdint.h>
#include <immintrin.h>
#include "support/dtypes.h"

#if defined(__i386__)

STATIC_INLINE unsigned long long rdtsc(void)
{
      unsigned long long int x;
           __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
	        return x;
}

#elif defined(__x86_64__)

STATIC_INLINE uint64_t rdtsc()
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ((uint64_t)lo) | (((uint64_t)hi) << 32);
}

#elif defined(_COMPILER_MICROSOFT_)

#include <intrin.h>

STATIC_INLINE uint64_t rdtsc()
{
    return __rdtsc();
}

#endif  /* __i386__ */

#if (__MIC__)

STATIC_INLINE void cpu_pause()
{
    _mm_delay_64(100);
}

STATIC_INLINE void cpu_delay(int64_t cycles)
{
    _mm_delay_64(cycles);
}

STATIC_INLINE void cpu_mfence()
{
    __asm__ __volatile__ ("":::"memory");
}

STATIC_INLINE void cpu_sfence()
{
    __asm__ __volatile__ ("":::"memory");
}

STATIC_INLINE void cpu_lfence()
{
    __asm__ __volatile__ ("":::"memory");
}

#else  /* !__MIC__ */

STATIC_INLINE void cpu_pause()
{
    _mm_pause();
}

STATIC_INLINE void cpu_delay(int64_t cycles)
{
    uint64_t s = rdtsc();
    while ((rdtsc() - s) < cycles)
        _mm_pause();
}

STATIC_INLINE void cpu_mfence()
{
    _mm_mfence();
}

STATIC_INLINE void cpu_sfence()
{
    _mm_sfence();
}

STATIC_INLINE void cpu_lfence()
{
    _mm_lfence();
}

#endif /* __MIC__ */


#endif  /* IA_MISC_H */

