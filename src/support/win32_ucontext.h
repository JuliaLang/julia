// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_WINUCONTEXT_H
#define JL_WINUCONTEXT_H

#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

#include <setjmp.h>
typedef struct {
    struct stack_t {
        void *ss_sp;
        size_t ss_size;
    } uc_stack;
    jmp_buf uc_mcontext;
} win32_ucontext_t;
void jl_makecontext(win32_ucontext_t *ucp, void (*func)(void));
void jl_swapcontext(win32_ucontext_t *oucp, const win32_ucontext_t *ucp);
void jl_setcontext(const win32_ucontext_t *ucp);

#ifdef __cplusplus
}
#endif
#endif
