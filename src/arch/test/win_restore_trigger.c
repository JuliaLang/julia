// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tests jl_win_restore_trigger (src/arch/<arch>/win_restore_trigger.S) the
// way src/signals-win.c uses it: a suspended thread is made to call a function
// on a frame built like jl_win_call_in_context builds it, the function returns
// into the trigger, and a vectored exception handler that restores the saved
// CONTEXT like jl_win_restore_veh resumes the thread where it was suspended.

#include "archtest.h"

#if defined(_WIN32) && (defined(__x86_64__) || defined(__i386__))

#include <string.h>
#include <windows.h>

extern void jl_win_restore_trigger(void);
void *jl_win_restore_page = NULL;

static volatile LONG64 spins;
static volatile LONG stop;
static volatile uintptr_t arg_in_fptr;
static volatile int aligned_in_fptr;
static volatile LONG faults;
static volatile uintptr_t fault_sp, saved_sp;

static DWORD WINAPI spinner(void *arg)
{
    (void)arg;
    while (!stop)
        InterlockedIncrement64(&spins);
    return 0;
}

static void fptr(uintptr_t arg)
{
    arg_in_fptr = arg;
    aligned_in_fptr = STACK_ALIGNED_AT_ENTRY();
}

// Same frame as jl_win_call_in_context
static void call_in_context(CONTEXT *ctx, void (*f)(uintptr_t), uintptr_t arg0)
{
#if defined(__x86_64__)
    uintptr_t sp = (uintptr_t)ctx->Rsp;
    sp = (sp - sizeof(CONTEXT)) & ~(uintptr_t)15;
    memcpy((void*)sp, ctx, sizeof(CONTEXT));
    saved_sp = sp;
    sp -= 32;
    sp -= sizeof(void*);
    *(uintptr_t*)sp = (uintptr_t)&jl_win_restore_trigger;
    ctx->Rsp = sp;
    ctx->Rip = (uintptr_t)f;
    ctx->Rcx = arg0;
#else
    uintptr_t sp = (uintptr_t)ctx->Esp;
    sp = (sp - sizeof(CONTEXT)) & ~(uintptr_t)15;
    memcpy((void*)sp, ctx, sizeof(CONTEXT));
    saved_sp = sp;
    sp -= sizeof(void*);
    *(uintptr_t*)sp = arg0;
    sp -= sizeof(void*);
    *(uintptr_t*)sp = (uintptr_t)&jl_win_restore_trigger;
    ctx->Esp = sp;
    ctx->Eip = (uintptr_t)f;
#endif
}

// Same restore as jl_win_restore_veh
static LONG WINAPI restore_veh(EXCEPTION_POINTERS *info)
{
    uintptr_t addr = info->ExceptionRecord->ExceptionInformation[1];
    uintptr_t page = (uintptr_t)jl_win_restore_page;
    if (info->ExceptionRecord->ExceptionCode != EXCEPTION_ACCESS_VIOLATION ||
        addr < page || addr >= page + 4096)
        return EXCEPTION_CONTINUE_SEARCH;
    InterlockedIncrement(&faults);
#if defined(__x86_64__)
    fault_sp = info->ContextRecord->Rsp + 32;
#else
    fault_sp = info->ContextRecord->Esp + sizeof(void*);
#endif
    memcpy(info->ContextRecord, (void*)fault_sp, sizeof(CONTEXT));
    return EXCEPTION_CONTINUE_EXECUTION;
}

int main(void)
{
    jl_win_restore_page = VirtualAlloc(NULL, 4096, MEM_RESERVE | MEM_COMMIT, PAGE_NOACCESS);
    CHECK(jl_win_restore_page != NULL);
    void *veh = AddVectoredExceptionHandler(1, restore_veh);
    HANDLE thread = CreateThread(NULL, 0, spinner, NULL, 0, NULL);
    while (spins == 0)
        Sleep(1);

    CHECK(SuspendThread(thread) != (DWORD)-1);
    CONTEXT ctx;
    memset(&ctx, 0, sizeof(ctx));
    ctx.ContextFlags = CONTEXT_FULL;
    CHECK(GetThreadContext(thread, &ctx));
    call_in_context(&ctx, fptr, 0x1234);
    CHECK(SetThreadContext(thread, &ctx));
    CHECK(ResumeThread(thread) != (DWORD)-1);

    while (faults == 0)
        Sleep(1);
    // the thread is back in its loop
    LONG64 resumed_at = spins;
    while (spins == resumed_at)
        Sleep(1);
    stop = 1;
    CHECK(WaitForSingleObject(thread, 10000) == WAIT_OBJECT_0);

    CHECK(arg_in_fptr == 0x1234);
    CHECK(aligned_in_fptr);
    CHECK(faults == 1);
    // the fault happens with the stack pointer just below the saved CONTEXT
    CHECK(fault_sp == saved_sp);
    RemoveVectoredExceptionHandler(veh);
    ARCHTEST_DONE("win_restore_trigger");
}

#else

int main(void)
{
    printf("win_restore_trigger: skipped on this platform\n");
    return 0;
}

#endif
