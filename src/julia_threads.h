// This file is a part of Julia. License is MIT: http://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JULIA_THREADS_H
#define JULIA_THREADS_H

#if defined(__i386__) && defined(__GNUC__) && !defined(__SSE2__)
#  error Julia can only be built for architectures above Pentium 4. Pass -march=pentium4, or set MARCH=pentium4 and ensure that -march is not passed separately with an older architecture.
#endif

#ifdef _COMPILER_MICROSOFT_
#  include <intrin.h>
#  define jl_signal_fence() _ReadWriteBarrier()
#else
#  define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#endif

#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#  include <immintrin.h>
#endif

#ifdef __MIC__
#  define jl_cpu_pause() _mm_delay_64(100)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_X86_64_) || defined(_CPU_X86_)  /* !__MIC__ */
#  define jl_cpu_pause() _mm_pause()
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#elif defined(_CPU_AARCH64_) || (defined(_CPU_ARM_) && __ARM_ARCH >= 7)
#  define jl_cpu_pause() __asm__ volatile ("wfe" ::: "memory")
#  define jl_cpu_wake() __asm__ volatile ("sev" ::: "memory")
#  define JL_CPU_WAKE_NOOP 0
#else
#  define jl_cpu_pause() ((void)0)
#  define jl_cpu_wake() ((void)0)
#  define JL_CPU_WAKE_NOOP 1
#endif

#endif
