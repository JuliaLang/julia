// This file is a part of Julia. License is MIT: http://julialang.org/license

// Meant to be included in <julia.h>
#ifndef JULIA_THREADS_H
#define JULIA_THREADS_H

#ifdef _COMPILER_MICROSOFT_
#  include <intrin.h>
#  define jl_signal_fence() _ReadWriteBarrier()
#else
#  define jl_signal_fence() __atomic_signal_fence(__ATOMIC_SEQ_CST)
#endif

#endif
