// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_TLS_H
#define JL_TLS_H

// Thread-local storage access

// Copied from libuv. Add `JL_CONST_FUNC` so that the compiler
// can optimize this better.
static inline unsigned long JL_CONST_FUNC jl_thread_self(void)
{
#ifdef _OS_WINDOWS_
    return (unsigned long)GetCurrentThreadId();
#else
    return (unsigned long)pthread_self();
#endif
}

typedef struct _jl_tls_states_t jl_tls_states_t;

typedef jl_tls_states_t *jl_ptls_t;

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT int16_t jl_threadid(void);
JL_DLLEXPORT void jl_threading_profile(void);

JL_DLLEXPORT JL_CONST_FUNC jl_ptls_t (jl_get_ptls_states)(void) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;

typedef jl_ptls_t (*jl_get_ptls_states_func)(void);
#if !defined(_OS_DARWIN_) && !defined(_OS_WINDOWS_)
JL_DLLEXPORT void jl_set_ptls_states_getter(jl_get_ptls_states_func f);
#endif

#ifdef __cplusplus
}
#endif

#endif
