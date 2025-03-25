// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  stackwalk.c
  utilities for walking the stack and looking up information about code addresses
*/
#include <inttypes.h>
#include "gc-common.h"
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"
#include "julia_assert.h"

// define `jl_unw_get` as a macro, since (like setjmp)
// returning from the callee function will invalidate the context
#ifdef _OS_WINDOWS_
uv_mutex_t jl_in_stackwalk;
#define jl_unw_get(context) (RtlCaptureContext(context), 0)
#elif !defined(JL_DISABLE_LIBUNWIND)
#define jl_unw_get(context) unw_getcontext(context)
#else
int jl_unw_get(void *context) { return -1; }
#endif

#ifdef __cplusplus
extern "C" {
#endif

static int jl_unw_init(bt_cursor_t *cursor, bt_context_t *context) JL_NOTSAFEPOINT;
static int jl_unw_step(bt_cursor_t *cursor, int from_signal_handler, uintptr_t *ip, uintptr_t *sp) JL_NOTSAFEPOINT;

static jl_gcframe_t *is_enter_interpreter_frame(jl_gcframe_t **ppgcstack, uintptr_t sp) JL_NOTSAFEPOINT
{
    jl_gcframe_t *pgcstack = *ppgcstack;
    while (pgcstack != NULL) {
        jl_gcframe_t *prev = pgcstack->prev;
        if (pgcstack->nroots & 2) { // tagged frame
            uintptr_t frame_fp = ((uintptr_t*)pgcstack)[-1];
            if (frame_fp != 0) { // check that frame was fully initialized
                if (frame_fp >= sp)
                    break; // stack grows down, so frame pointer is monotonically increasing
                *ppgcstack = prev;
                return pgcstack;
            }
        }
        *ppgcstack = pgcstack = prev;
    }
    return NULL;
}


// Record backtrace entries into bt_data by stepping cursor with jl_unw_step
// until the outermost frame is encountered or the buffer bt_data is (close to)
// full. Returned instruction pointers are adjusted to point to the address of
// the call instruction. The first `skip` frames are not included in `bt_data`.
//
// `maxsize` is the size of the buffer `bt_data` (and `sp` if non-NULL). It
// must be at least `JL_BT_MAX_ENTRY_SIZE + 1` to accommodate extended backtrace
// entries.  If `sp != NULL`, the stack pointer corresponding `bt_data[i]` is
// stored in `sp[i]`.
//
// `*ppgcstack` should be given if you want to record extended backtrace
// entries in `bt_data` for each julia interpreter frame.
//
// Flag `from_signal_handler==1` should be set if the cursor was obtained by
// asynchronously interrupting the code.
//
// jl_unw_stepn will return 1 if there are more frames to come. The number of
// elements written to bt_data (and sp if non-NULL) are returned in bt_size.
static int jl_unw_stepn(bt_cursor_t *cursor, jl_bt_element_t *bt_data, size_t *bt_size,
                        uintptr_t *sp, size_t maxsize, int skip, jl_gcframe_t **ppgcstack,
                        int from_signal_handler) JL_NOTSAFEPOINT
{
    volatile size_t n = 0;
    volatile int need_more_space = 0;
    uintptr_t return_ip = 0;
    uintptr_t thesp = 0;
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    uv_mutex_lock(&jl_in_stackwalk);
    if (!from_signal_handler) {
        // Workaround 32-bit windows bug missing top frame
        // See for example https://bugs.chromium.org/p/crashpad/issues/detail?id=53
        skip--;
    }
#endif
#if !defined(_OS_WINDOWS_) // no point on windows, since RtlVirtualUnwind won't give us a second chance if the segfault happens in ntdll
    jl_jmp_buf *old_buf = jl_get_safe_restore();
    jl_jmp_buf buf;
    jl_set_safe_restore(&buf);
    if (!jl_setjmp(buf, 0)) {
#endif
        int have_more_frames = 1;
        while (have_more_frames) {
            if (n + JL_BT_MAX_ENTRY_SIZE + 1 > maxsize) {
                // Postpone advancing the cursor: may need more space
                need_more_space = 1;
                break;
            }
            uintptr_t oldsp = thesp;
            have_more_frames = jl_unw_step(cursor, from_signal_handler, &return_ip, &thesp);
            if ((n < 2 ? oldsp > thesp : oldsp >= thesp) && !jl_running_under_rr(0)) {
                // The stack pointer is clearly bad, as it must grow downwards,
                // But sometimes the external unwinder doesn't check that.
                // Except for n==0 when there is no oldsp and n==1 on all platforms but i686/x86_64.
                // (on x86, the platform first pushes the new stack frame, then does the
                // call, on almost all other platforms, the platform first does the call,
                // then the user pushes the link register to the frame).
                have_more_frames = 0;
            }
            if (return_ip == 0) {
                // The return address is clearly wrong, and while the unwinder
                // might try to continue (by popping another stack frame), that
                // likely won't work well, and it'll confuse the stack frame
                // separator detection logic (double-NULL).
                have_more_frames = 0;
            }
            if (skip > 0) {
                skip--;
                from_signal_handler = 0;
                continue;
            }
            // For the purposes of looking up debug info for functions, we want
            // to harvest addresses for the *call* instruction `call_ip` during
            // stack walking.  However, this information isn't directly
            // available. Instead, the stack walk discovers the address
            // `return_ip` which would be *returned to* as the stack is
            // unwound.
            //
            // To infer `call_ip` in full generality we need to understand each
            // platform ABI instruction pointer encoding and calling
            // conventions, noting that the latter may vary per stack frame.
            //
            // See also:
            // * The LLVM unwinder functions step() and setInfoBasedOnIPRegister()
            //   https://github.com/llvm/llvm-project/blob/master/libunwind/src/UnwindCursor.hpp
            // * The way that libunwind handles it in `unw_get_proc_name`:
            //   https://lists.nongnu.org/archive/html/libunwind-devel/2014-06/msg00025.html
            uintptr_t call_ip = return_ip;
            #if defined(_CPU_ARM_)
            // ARM instruction pointer encoding uses the low bit as a flag for
            // thumb mode, which must be cleared before further use. (Note not
            // needed for ARM AArch64.) See
            // https://github.com/libunwind/libunwind/pull/131
            call_ip &= ~(uintptr_t)0x1;
            #endif
            // Now there's two main cases to adjust for:
            // * Normal stack frames where compilers emit a `call` instruction
            //   which we can get from the return address via `call_ip = return_ip - 1`.
            // * Code which was interrupted asynchronously (eg, via a signal)
            //   is expected to have `call_ip == return_ip`.
            if (!from_signal_handler)
                call_ip -= 1; // normal frame
            from_signal_handler = 0;
            if (call_ip == JL_BT_NON_PTR_ENTRY || call_ip == 0) {
                // Never leave special marker in the bt data as it can corrupt the GC.
                have_more_frames = 0;
                call_ip = 0;
            }
            jl_bt_element_t *bt_entry = bt_data + n;
            jl_gcframe_t *pgcstack;
            if ((pgcstack = is_enter_interpreter_frame(ppgcstack, thesp))) {
                size_t add = jl_capture_interp_frame(bt_entry, (void*)((char*)pgcstack - sizeof(void*)), maxsize - n);
                n += add;
                bt_entry += add;
                while ((pgcstack = is_enter_interpreter_frame(ppgcstack, thesp))) {
                    // If the compiler got inlining-happy, or the user tried to
                    // push multiple frames (or the unwinder got very
                    // confused), we could end up here. That doesn't happen
                    // now, so just ignore this possibility. If we want this,
                    // we can work on adding support for it later.
                }
            }
            bt_entry->uintptr = call_ip;
            if (sp)
                sp[n] = thesp;
            n++;
        }
        // NOTE: if we have some pgcstack entries remaining (because the
        // unwinder failed and returned !have_more_frames early), we could
        // consider still appending those frames here
#if !defined(_OS_WINDOWS_)
    }
    else {
        // The unwinding fails likely because a invalid memory read.
        // Back off one frame since it is likely invalid.
        // This seems to be good enough on x86 to make the LLVM debug info
        // reader happy.
        if (n > 0) n -= 1;
    }
    jl_set_safe_restore(old_buf);
#endif
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    uv_mutex_unlock(&jl_in_stackwalk);
#endif
    *bt_size = n;
    return need_more_space;
}

NOINLINE size_t rec_backtrace_ctx(jl_bt_element_t *bt_data, size_t maxsize,
                                  bt_context_t *context, jl_gcframe_t *pgcstack) JL_NOTSAFEPOINT
{
    bt_cursor_t cursor;
    if (!jl_unw_init(&cursor, context))
        return 0;
    size_t bt_size = 0;
    jl_unw_stepn(&cursor, bt_data, &bt_size, NULL, maxsize, 0, &pgcstack, 1);
    return bt_size;
}

// Record backtrace into buffer `bt_data`, using a maximum of `maxsize`
// elements, and returning the number of elements written.
//
// The first `skip` frames are omitted, in addition to omitting the frame from
// `rec_backtrace` itself.
NOINLINE size_t rec_backtrace(jl_bt_element_t *bt_data, size_t maxsize, int skip) JL_NOTSAFEPOINT
{
    bt_context_t context;
    memset(&context, 0, sizeof(context));
    int r = jl_unw_get(&context);
    if (r < 0)
        return 0;
    bt_cursor_t cursor;
    if (!jl_unw_init(&cursor, &context) || maxsize == 0)
        return 0;
    jl_gcframe_t *pgcstack = jl_pgcstack;
    size_t bt_size = 0;
    jl_unw_stepn(&cursor, bt_data, &bt_size, NULL, maxsize, skip + 1, &pgcstack, 0);
    return bt_size;
}

NOINLINE int failed_to_sample_task_fun(jl_bt_element_t *bt_data, size_t maxsize, int skip) JL_NOTSAFEPOINT
{
    if (maxsize < 1) {
        return 0;
    }
    bt_data[0].uintptr = (uintptr_t) &failed_to_sample_task_fun;
    return 1;
}

NOINLINE int failed_to_stop_thread_fun(jl_bt_element_t *bt_data, size_t maxsize, int skip) JL_NOTSAFEPOINT
{
    if (maxsize < 1) {
        return 0;
    }
    bt_data[0].uintptr = (uintptr_t) &failed_to_stop_thread_fun;
    return 1;
}

static jl_value_t *array_ptr_void_type JL_ALWAYS_LEAFTYPE = NULL;
// Return backtrace information as an svec of (bt1, bt2, [sp])
//
// The stack pointers `sp` are returned only when `returnsp` evaluates to true.
// bt1 contains raw backtrace entries, while bt2 exists to root any julia
// objects associated with the entries in bt1.
//
// The frame from jl_backtrace_from_here will be skipped; set `skip > 0` to
// skip additional native frames from the start of the backtrace.
JL_DLLEXPORT jl_value_t *jl_backtrace_from_here(int returnsp, int skip)
{
    jl_array_t *ip = NULL;
    jl_array_t *sp = NULL;
    jl_array_t *bt2 = NULL;
    JL_GC_PUSH3(&ip, &sp, &bt2);
    if (array_ptr_void_type == NULL) {
        array_ptr_void_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_voidpointer_type, jl_box_long(1));
    }
    ip = jl_alloc_array_1d(array_ptr_void_type, 0);
    sp = returnsp ? jl_alloc_array_1d(array_ptr_void_type, 0) : NULL;
    bt2 = jl_alloc_array_1d(jl_array_any_type, 0);
    const size_t maxincr = 1000;
    bt_context_t context;
    bt_cursor_t cursor;
    memset(&context, 0, sizeof(context));
    int r = jl_unw_get(&context);
    jl_gcframe_t *pgcstack = jl_pgcstack;
    if (r == 0 && jl_unw_init(&cursor, &context)) {
        // Skip frame for jl_backtrace_from_here itself
        skip += 1;
        size_t offset = 0;
        int have_more_frames = 1;
        while (have_more_frames) {
            jl_array_grow_end(ip, maxincr);
            uintptr_t *sp_ptr = NULL;
            if (returnsp) {
                jl_array_grow_end(sp, maxincr);
                sp_ptr = jl_array_data(sp, uintptr_t) + offset;
            }
            size_t size_incr = 0;
            have_more_frames = jl_unw_stepn(&cursor, jl_array_data(ip, jl_bt_element_t) + offset,
                                            &size_incr, sp_ptr, maxincr, skip, &pgcstack, 0);
            skip = 0;
            offset += size_incr;
        }
        jl_array_del_end(ip, jl_array_nrows(ip) - offset);
        if (returnsp)
            jl_array_del_end(sp, jl_array_nrows(sp) - offset);

        size_t n = 0;
        jl_bt_element_t *bt_data = jl_array_data(ip, jl_bt_element_t);
        while (n < jl_array_nrows(ip)) {
            jl_bt_element_t *bt_entry = bt_data + n;
            if (!jl_bt_is_native(bt_entry)) {
                size_t njlvals = jl_bt_num_jlvals(bt_entry);
                for (size_t j = 0; j < njlvals; j++) {
                    jl_value_t *v = jl_bt_entry_jlvalue(bt_entry, j);
                    JL_GC_PROMISE_ROOTED(v);
                    jl_array_ptr_1d_push(bt2, v);
                }
            }
            n += jl_bt_entry_size(bt_entry);
        }
    }
    jl_value_t *bt = returnsp ? (jl_value_t*)jl_svec(3, ip, bt2, sp) : (jl_value_t*)jl_svec(2, ip, bt2);
    JL_GC_POP();
    return bt;
}

static void decode_backtrace(jl_bt_element_t *bt_data, size_t bt_size,
                             jl_array_t **btout JL_REQUIRE_ROOTED_SLOT,
                             jl_array_t **bt2out JL_REQUIRE_ROOTED_SLOT)
{
    jl_array_t *bt, *bt2;
    if (array_ptr_void_type == NULL) {
        array_ptr_void_type = jl_apply_type2((jl_value_t*)jl_array_type, (jl_value_t*)jl_voidpointer_type, jl_box_long(1));
    }
    bt = *btout = jl_alloc_array_1d(array_ptr_void_type, bt_size);
    static_assert(sizeof(jl_bt_element_t) == sizeof(void*),
                  "jl_bt_element_t is presented as Ptr{Cvoid} on julia side");
    memcpy(jl_array_data(bt, jl_bt_element_t), bt_data, bt_size * sizeof(jl_bt_element_t));
    bt2 = *bt2out = jl_alloc_array_1d(jl_array_any_type, 0);
    // Scan the backtrace buffer for any gc-managed values
    for (size_t i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_bt_element_t* bt_entry = bt_data + i;
        if (jl_bt_is_native(bt_entry))
            continue;
        size_t njlvals = jl_bt_num_jlvals(bt_entry);
        for (size_t j = 0; j < njlvals; j++) {
            jl_value_t *v = jl_bt_entry_jlvalue(bt_entry, j);
            JL_GC_PROMISE_ROOTED(v);
            jl_array_ptr_1d_push(bt2, v);
        }
    }
}

JL_DLLEXPORT jl_value_t *jl_get_backtrace(void)
{
    JL_TIMING(STACKWALK, STACKWALK_Backtrace);
    jl_excstack_t *s = jl_current_task->excstack;
    jl_bt_element_t *bt_data = NULL;
    size_t bt_size = 0;
    if (s && s->top) {
        bt_data = jl_excstack_bt_data(s, s->top);
        bt_size = jl_excstack_bt_size(s, s->top);
    }
    jl_array_t *bt = NULL, *bt2 = NULL;
    JL_GC_PUSH2(&bt, &bt2);
    decode_backtrace(bt_data, bt_size, &bt, &bt2);
    jl_svec_t *pair = jl_svec2(bt, bt2);
    JL_GC_POP();
    return (jl_value_t*)pair;
}

// Return data from the exception stack for `task` as an array of Any, starting
// with the top of the stack and returning up to `max_entries`. If requested by
// setting the `include_bt` flag, backtrace data in bt,bt2 format is
// interleaved.
JL_DLLEXPORT jl_value_t *jl_get_excstack(jl_task_t* task, int include_bt, int max_entries)
{
    JL_TYPECHK(current_exceptions, task, (jl_value_t*)task);
    JL_TIMING(STACKWALK, STACKWALK_Excstack);
    jl_task_t *ct = jl_current_task;
    if (task != ct && jl_atomic_load_relaxed(&task->_state) == JL_TASK_STATE_RUNNABLE) {
        jl_error("Inspecting the exception stack of a task which might "
                 "be running concurrently isn't allowed.");
    }
    jl_array_t *stack = NULL;
    jl_array_t *bt = NULL;
    jl_array_t *bt2 = NULL;
    JL_GC_PUSH3(&stack, &bt, &bt2);
    stack = jl_alloc_array_1d(jl_array_any_type, 0);
    jl_excstack_t *excstack = task->excstack;
    size_t itr = excstack ? excstack->top : 0;
    int i = 0;
    while (itr > 0 && i < max_entries) {
        jl_array_ptr_1d_push(stack, jl_excstack_exception(excstack, itr));
        if (include_bt) {
            decode_backtrace(jl_excstack_bt_data(excstack, itr),
                             jl_excstack_bt_size(excstack, itr),
                             &bt, &bt2);
            jl_array_ptr_1d_push(stack, (jl_value_t*)bt);
            jl_array_ptr_1d_push(stack, (jl_value_t*)bt2);
        }
        itr = jl_excstack_next(excstack, itr);
        i++;
    }
    JL_GC_POP();
    return (jl_value_t*)stack;
}

#if defined(_OS_WINDOWS_)
// XXX: these caches should be per-thread
#ifdef _CPU_X86_64_
static UNWIND_HISTORY_TABLE HistoryTable;
#else
static struct {
    DWORD64 dwAddr;
    DWORD64 ImageBase;
} HistoryTable;
#endif
static PVOID CALLBACK JuliaFunctionTableAccess64(
        _In_  HANDLE hProcess,
        _In_  DWORD64 AddrBase)
{
    //jl_printf(JL_STDOUT, "lookup %d\n", AddrBase);
#ifdef _CPU_X86_64_
    DWORD64 ImageBase;
    PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry(AddrBase, &ImageBase, &HistoryTable);
    if (fn)
        return fn;
    uv_mutex_lock(&jl_in_stackwalk);
    PVOID ftable = SymFunctionTableAccess64(hProcess, AddrBase);
    uv_mutex_unlock(&jl_in_stackwalk);
    return ftable;
#else
    return SymFunctionTableAccess64(hProcess, AddrBase);
#endif
}
static DWORD64 WINAPI JuliaGetModuleBase64(
        _In_  HANDLE hProcess,
        _In_  DWORD64 dwAddr)
{
    //jl_printf(JL_STDOUT, "lookup base %d\n", dwAddr);
#ifdef _CPU_X86_64_
    DWORD64 ImageBase;
    PRUNTIME_FUNCTION fn = RtlLookupFunctionEntry(dwAddr, &ImageBase, &HistoryTable);
    if (fn)
        return ImageBase;
    uv_mutex_lock(&jl_in_stackwalk);
    DWORD64 fbase = SymGetModuleBase64(hProcess, dwAddr);
    uv_mutex_unlock(&jl_in_stackwalk);
    return fbase;
#else
    if (dwAddr == HistoryTable.dwAddr)
        return HistoryTable.ImageBase;
    DWORD64 ImageBase = jl_getUnwindInfo(dwAddr);
    if (ImageBase) {
        HistoryTable.dwAddr = dwAddr;
        HistoryTable.ImageBase = ImageBase;
        return ImageBase;
    }
    return SymGetModuleBase64(hProcess, dwAddr);
#endif
}

// Might be called from unmanaged thread.
volatile int needsSymRefreshModuleList;
BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);

JL_DLLEXPORT void jl_refresh_dbg_module_list(void)
{
    if (needsSymRefreshModuleList && hSymRefreshModuleList != NULL) {
        hSymRefreshModuleList(GetCurrentProcess());
        needsSymRefreshModuleList = 0;
    }
}
static int jl_unw_init(bt_cursor_t *cursor, bt_context_t *Context)
{
    int result;
    uv_mutex_lock(&jl_in_stackwalk);
    jl_refresh_dbg_module_list();
#if !defined(_CPU_X86_64_)
    memset(&cursor->stackframe, 0, sizeof(cursor->stackframe));
    cursor->stackframe.AddrPC.Offset = Context->Eip;
    cursor->stackframe.AddrStack.Offset = Context->Esp;
    cursor->stackframe.AddrFrame.Offset = Context->Ebp;
    cursor->stackframe.AddrPC.Mode = AddrModeFlat;
    cursor->stackframe.AddrStack.Mode = AddrModeFlat;
    cursor->stackframe.AddrFrame.Mode = AddrModeFlat;
    cursor->context = *Context;
    result = StackWalk64(IMAGE_FILE_MACHINE_I386, GetCurrentProcess(), hMainThread,
            &cursor->stackframe, &cursor->context, NULL, JuliaFunctionTableAccess64,
            JuliaGetModuleBase64, NULL);
#else
    *cursor = *Context;
    result = 1;
#endif
    uv_mutex_unlock(&jl_in_stackwalk);
    return result;
}

static int readable_pointer(LPCVOID pointer)
{
    // Check whether the pointer is valid and executable before dereferencing
    // to avoid segfault while recording. See #10638.
    MEMORY_BASIC_INFORMATION mInfo;
    if (VirtualQuery(pointer, &mInfo, sizeof(MEMORY_BASIC_INFORMATION)) == 0)
        return 0;
    DWORD X = mInfo.AllocationProtect;
    if (!((X&PAGE_READONLY) || (X&PAGE_READWRITE) || (X&PAGE_WRITECOPY) || (X&PAGE_EXECUTE_READ)) ||
          (X&PAGE_GUARD) || (X&PAGE_NOACCESS))
        return 0;
    return 1;
}

static int jl_unw_step(bt_cursor_t *cursor, int from_signal_handler, uintptr_t *ip, uintptr_t *sp)
{
    // Might be called from unmanaged thread.
#ifndef _CPU_X86_64_
    *ip = (uintptr_t)cursor->stackframe.AddrPC.Offset;
    *sp = (uintptr_t)cursor->stackframe.AddrStack.Offset;
    if (*ip == 0) {
        if (!readable_pointer((LPCVOID)*sp))
            return 0;
        cursor->stackframe.AddrPC.Offset = *(DWORD32*)*sp;      // POP EIP (aka RET)
        cursor->stackframe.AddrStack.Offset += sizeof(void*);
        return cursor->stackframe.AddrPC.Offset != 0;
    }

    BOOL result = StackWalk64(IMAGE_FILE_MACHINE_I386, GetCurrentProcess(), hMainThread,
        &cursor->stackframe, &cursor->context, NULL, JuliaFunctionTableAccess64, JuliaGetModuleBase64, NULL);
    return result;
#else
    *ip = (uintptr_t)cursor->Rip;
    *sp = (uintptr_t)cursor->Rsp;
    if (*ip == 0 && from_signal_handler) {
        if (!readable_pointer((LPCVOID)*sp))
            return 0;
        cursor->Rip = *(DWORD64*)*sp;      // POP RIP (aka RET)
        cursor->Rsp += sizeof(void*);
        return cursor->Rip != 0;
    }

    DWORD64 ImageBase = JuliaGetModuleBase64(GetCurrentProcess(), cursor->Rip - !from_signal_handler);
    if (!ImageBase)
        return 0;

    PRUNTIME_FUNCTION FunctionEntry = (PRUNTIME_FUNCTION)JuliaFunctionTableAccess64(
        GetCurrentProcess(), cursor->Rip - !from_signal_handler);
    if (!FunctionEntry) {
        // Not code or bad unwind?
        return 0;
    }
    else {
        PVOID HandlerData;
        DWORD64 EstablisherFrame;
        (void)RtlVirtualUnwind(
                0 /*UNW_FLAG_NHANDLER*/,
                ImageBase,
                cursor->Rip,
                FunctionEntry,
                cursor,
                &HandlerData,
                &EstablisherFrame,
                NULL);
    }
    return cursor->Rip != 0;
#endif
}

#elif !defined(JL_DISABLE_LIBUNWIND)
// stacktrace using libunwind

static int jl_unw_init(bt_cursor_t *cursor, bt_context_t *context)
{
    return unw_init_local(cursor, context) == 0;
}

static int jl_unw_step(bt_cursor_t *cursor, int from_signal_handler, uintptr_t *ip, uintptr_t *sp)
{
    (void)from_signal_handler; // libunwind also tracks this
    unw_word_t reg;
    if (unw_get_reg(cursor, UNW_REG_IP, &reg) < 0)
        return 0;
    *ip = reg;
    if (unw_get_reg(cursor, UNW_REG_SP, &reg) < 0)
        return 0;
    *sp = reg;
    return unw_step(cursor) > 0;
}

#ifdef LLVMLIBUNWIND
NOINLINE size_t rec_backtrace_ctx_dwarf(jl_bt_element_t *bt_data, size_t maxsize,
                                        bt_context_t *context, jl_gcframe_t *pgcstack)
{
    size_t bt_size = 0;
    bt_cursor_t cursor;
    if (unw_init_local_dwarf(&cursor, context) != UNW_ESUCCESS)
        return 0;
    jl_unw_stepn(&cursor, bt_data, &bt_size, NULL, maxsize, 0, &pgcstack, 1);
    return bt_size;
}
#endif

#else
// stacktraces are disabled
static int jl_unw_init(bt_cursor_t *cursor, bt_context_t *context)
{
    return 0;
}

static int jl_unw_step(bt_cursor_t *cursor, int from_signal_handler, uintptr_t *ip, uintptr_t *sp)
{
    return 0;
}
#endif

JL_DLLEXPORT jl_value_t *jl_lookup_code_address(void *ip, int skipC)
{
    jl_task_t *ct = jl_current_task;
    jl_frame_t *frames = NULL;
    int8_t gc_state = jl_gc_safe_enter(ct->ptls);
    int n = jl_getFunctionInfo(&frames, (uintptr_t)ip, skipC, 0);
    jl_gc_safe_leave(ct->ptls, gc_state);
    jl_value_t *rs = (jl_value_t*)jl_alloc_svec(n);
    JL_GC_PUSH1(&rs);
    for (int i = 0; i < n; i++) {
        jl_frame_t frame = frames[i];
        jl_value_t *r = (jl_value_t*)jl_alloc_svec(6);
        jl_svecset(rs, i, r);
        if (frame.func_name)
            jl_svecset(r, 0, jl_symbol(frame.func_name));
        else
            jl_svecset(r, 0, jl_empty_sym);
        free(frame.func_name);
        if (frame.file_name)
            jl_svecset(r, 1, jl_symbol(frame.file_name));
        else
            jl_svecset(r, 1, jl_empty_sym);
        free(frame.file_name);
        jl_svecset(r, 2, jl_box_long(frame.line));
        jl_svecset(r, 3, frame.ci != NULL ? (jl_value_t*)frame.ci : jl_nothing);
        jl_svecset(r, 4, jl_box_bool(frame.fromC));
        jl_svecset(r, 5, jl_box_bool(frame.inlined));
    }
    free(frames);
    JL_GC_POP();
    return rs;
}

static void jl_safe_print_codeloc(const char* func_name, const char* file_name,
                                  int line, int inlined) JL_NOTSAFEPOINT
{
    const char *inlined_str = inlined ? " [inlined]" : "";
    if (line != -1) {
        jl_safe_printf("%s at %s:%d%s\n", func_name, file_name, line, inlined_str);
    }
    else {
        jl_safe_printf("%s at %s (unknown line)%s\n", func_name, file_name, inlined_str);
    }
}

// Print function, file and line containing native instruction pointer `ip` by
// looking up debug info. Prints multiple such frames when `ip` points to
// inlined code.
void jl_print_native_codeloc(uintptr_t ip) JL_NOTSAFEPOINT
{
    // This function is not allowed to reference any TLS variables since
    // it can be called from an unmanaged thread on OSX.
    // it means calling getFunctionInfo with noInline = 1
    jl_frame_t *frames = NULL;
    int n = jl_getFunctionInfo(&frames, ip, 0, 0);
    int i;

    for (i = 0; i < n; i++) {
        jl_frame_t frame = frames[i];
        if (!frame.func_name) {
            jl_safe_printf("unknown function (ip: %p) at %s\n", (void*)ip, frame.file_name ? frame.file_name : "(unknown file)");
        }
        else {
            jl_safe_print_codeloc(frame.func_name, frame.file_name, frame.line, frame.inlined);
            free(frame.func_name);
        }
        free(frame.file_name);
    }
    free(frames);
}

const char *jl_debuginfo_file1(jl_debuginfo_t *debuginfo)
{
    jl_value_t *def = debuginfo->def;
    if (jl_is_method_instance(def))
        def = ((jl_method_instance_t*)def)->def.value;
    if (jl_is_method(def))
        def = (jl_value_t*)((jl_method_t*)def)->file;
    if (jl_is_symbol(def))
        return jl_symbol_name((jl_sym_t*)def);
    return "<unknown>";
}

const char *jl_debuginfo_file(jl_debuginfo_t *debuginfo)
{
    jl_debuginfo_t *linetable = debuginfo->linetable;
    while ((jl_value_t*)linetable != jl_nothing) {
        debuginfo = linetable;
        linetable = debuginfo->linetable;
    }
    return jl_debuginfo_file1(debuginfo);
}

jl_module_t *jl_debuginfo_module1(jl_value_t *debuginfo_def)
{
    if (jl_is_method_instance(debuginfo_def))
        debuginfo_def = ((jl_method_instance_t*)debuginfo_def)->def.value;
    if (jl_is_method(debuginfo_def))
        debuginfo_def = (jl_value_t*)((jl_method_t*)debuginfo_def)->module;
    if (jl_is_module(debuginfo_def))
        return (jl_module_t*)debuginfo_def;
    return NULL;
}

const char *jl_debuginfo_name(jl_value_t *func)
{
    if (func == NULL)
        return "macro expansion";
    if (jl_is_method_instance(func))
        func = ((jl_method_instance_t*)func)->def.value;
    if (jl_is_method(func))
        func = (jl_value_t*)((jl_method_t*)func)->name;
    if (jl_is_symbol(func))
        return jl_symbol_name((jl_sym_t*)func);
    if (jl_is_module(func))
        return "top-level scope";
    return "<unknown>";
}

// func == module : top-level
// func == NULL : macro expansion
static void jl_print_debugloc(jl_debuginfo_t *debuginfo, jl_value_t *func, size_t ip, int inlined) JL_NOTSAFEPOINT
{
    if (!jl_is_symbol(debuginfo->def)) // this is a path or
        func = debuginfo->def; // this is inlined code
    struct jl_codeloc_t stmt = jl_uncompress1_codeloc(debuginfo->codelocs, ip);
    intptr_t edges_idx = stmt.to;
    if (edges_idx) {
        jl_debuginfo_t *edge = (jl_debuginfo_t*)jl_svecref(debuginfo->edges, edges_idx - 1);
        assert(jl_typetagis(edge, jl_debuginfo_type));
        jl_print_debugloc(edge, NULL, stmt.pc, 1);
    }
    intptr_t ip2 = stmt.line;
    if (ip2 >= 0 && ip > 0 && (jl_value_t*)debuginfo->linetable != jl_nothing) {
        jl_print_debugloc(debuginfo->linetable, func, ip2, 0);
    }
    else {
        if (ip2 < 0) // set broken debug info to ignored
            ip2 = 0;
        const char *func_name = jl_debuginfo_name(func);
        const char *file = jl_debuginfo_file(debuginfo);
        jl_safe_print_codeloc(func_name, file, ip2, inlined);
    }
}

// Print code location for backtrace buffer entry at *bt_entry
void jl_print_bt_entry_codeloc(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    if (jl_bt_is_native(bt_entry)) {
        jl_print_native_codeloc(bt_entry[0].uintptr);
    }
    else if (jl_bt_entry_tag(bt_entry) == JL_BT_INTERP_FRAME_TAG) {
        size_t ip = jl_bt_entry_header(bt_entry); // zero-indexed
        jl_value_t *code = jl_bt_entry_jlvalue(bt_entry, 0);
        jl_value_t *def = (jl_value_t*)jl_core_module; // just used as a token here that isa Module
        if (jl_is_code_instance(code)) {
            jl_code_instance_t *ci = (jl_code_instance_t*)code;
            def = (jl_value_t*)ci->def;
            code = jl_atomic_load_relaxed(&ci->inferred);
        } else if (jl_is_method_instance(code)) {
            jl_method_instance_t *mi = (jl_method_instance_t*)code;
            def = code;
            // When interpreting a method instance, need to unwrap to find the code info
            code = mi->def.method->source;
        }
        if (jl_is_code_info(code)) {
            jl_code_info_t *src = (jl_code_info_t*)code;
            // See also the debug info handling in codegen.cpp.
            jl_print_debugloc(src->debuginfo, def, ip + 1, 0);
        }
        else {
            // If we're using this function something bad has already happened;
            // be a bit defensive to avoid crashing while reporting the crash.
            jl_safe_printf("No code info - unknown interpreter state!\n");
        }
    }
    else {
        jl_safe_printf("Non-native bt entry with tag and header bits 0x%" PRIxPTR "\n",
                       bt_entry[1].uintptr);
    }
}


#ifdef _OS_LINUX_
#if defined(__GLIBC__) && defined(_CPU_AARCH64_)
#define LONG_JMP_SP_ENV_SLOT 13
static uintptr_t julia_longjmp_xor_key;
// GLIBC mangles the function pointers in jmp_buf (used in {set,long}*jmp
// functions) by XORing them with a random key.  For AArch64 it is a global
// variable rather than a TCB one (as for x86_64/powerpc).  We obtain the key by
// issuing a setjmp and XORing the SP pointer values to derive the key.
static void JuliaInitializeLongjmpXorKey(void)
{
    // 1. Call REAL(setjmp), which stores the mangled SP in env.
    jmp_buf env;
    _setjmp(env);

    // 2. Retrieve vanilla/mangled SP.
    uintptr_t sp;
    asm("mov  %0, sp" : "=r" (sp));
    uintptr_t mangled_sp = ((uintptr_t*)&env)[LONG_JMP_SP_ENV_SLOT];

    // 3. xor SPs to obtain key.
    julia_longjmp_xor_key = mangled_sp ^ sp;
}
#endif

JL_UNUSED static uintptr_t ptr_demangle(uintptr_t p) JL_NOTSAFEPOINT
{
#if defined(__GLIBC__)
#if defined(_CPU_X86_)
// from https://github.com/bminor/glibc/blame/master/sysdeps/unix/sysv/linux/i386/sysdep.h
// last changed for GLIBC_2.6 on 2007-02-01
    asm(" rorl $9, %0\n"
        " xorl %%gs:0x18, %0"
        : "=r"(p) : "0"(p) : );
#elif defined(_CPU_X86_64_)
// from https://github.com/bminor/glibc/blame/master/sysdeps/unix/sysv/linux/i386/sysdep.h
    asm(" rorq $17, %0\n"
        " xorq %%fs:0x30, %0"
        : "=r"(p) : "0"(p) : );
#elif defined(_CPU_AARCH64_)
// from https://github.com/bminor/glibc/blame/master/sysdeps/unix/sysv/linux/aarch64/sysdep.h
// We need to use a trick like this (from GCC/LLVM TSAN) to get access to it:
// https://github.com/llvm/llvm-project/commit/daa3ebce283a753f280c549cdb103fbb2972f08e
    static pthread_once_t once = PTHREAD_ONCE_INIT;
    pthread_once(&once, &JuliaInitializeLongjmpXorKey);
    p ^= julia_longjmp_xor_key;
#elif defined(_CPU_ARM_)
// from https://github.com/bminor/glibc/blame/master/sysdeps/unix/sysv/linux/arm/sysdep.h
    ; // nothing to do
#endif
#endif
    return p;
}
#endif

// n.b. musl does not mangle pointers, but intentionally makes that impossible
// to determine (https://www.openwall.com/lists/musl/2013/03/29/13) so we do
// not support musl here.

// n.b. We have not looked at other libc (e.g. ulibc), though they are probably
// often compatible with glibc (perhaps with or without pointer mangling).


#ifdef _OS_DARWIN_
// from https://github.com/apple/darwin-xnu/blame/main/libsyscall/os/tsd.h
#define __TSD_PTR_MUNGE 7

#if defined(__i386__) || defined(__x86_64__)

#if defined(__has_attribute)
#if __has_attribute(address_space)
#define OS_GS_RELATIVE  __attribute__((address_space(256)))
#endif
#endif

#ifdef OS_GS_RELATIVE
#define _os_tsd_get_base() ((void * OS_GS_RELATIVE *)0)
#else
__attribute__((always_inline))
static __inline__ void*
_os_tsd_get_direct(unsigned long slot)
{
    void *ret;
    __asm__("mov %%gs:%1, %0" : "=r" (ret) : "m" (*(void **)(slot * sizeof(void *))));
    return ret;
}
#endif

#elif defined(__arm__) || defined(__arm64__)
// Unconditionally defined ptrauth_strip (instead of using the ptrauth.h header)
// since libsystem will likely be compiled with -mbranch-protection, and we currently are not.
// code from https://github.com/llvm/llvm-project/blob/7714e0317520207572168388f22012dd9e152e9e/compiler-rt/lib/sanitizer_common/sanitizer_ptrauth.h
static inline uint64_t ptrauth_strip(uint64_t __value, unsigned int __key) JL_NOTSAFEPOINT {
  // On the stack the link register is protected with Pointer
  // Authentication Code when compiled with -mbranch-protection.
  // Let's strip the PAC unconditionally because xpaclri is in the NOP space,
  // so will do nothing when it is not enabled or not available.
  uint64_t ret;
  asm volatile(
      "mov x30, %1\n\t"
      "hint #7\n\t"  // xpaclri
      "mov %0, x30\n\t"
      : "=r"(ret)
      : "r"(__value)
      : "x30");
  return ret;
}

__attribute__((always_inline, pure))
static __inline__ void**
_os_tsd_get_base(void) JL_NOTSAFEPOINT
{
#if defined(__arm__)
    uintptr_t tsd;
    __asm__("mrc p15, 0, %0, c13, c0, 3\n"
            "bic %0, %0, #0x3\n" : "=r" (tsd));
    /* lower 2-bits contain CPU number */
#elif defined(__arm64__)
    uint64_t tsd;
    __asm__("mrs %0, TPIDRRO_EL0\n"
            "bic %0, %0, #0x7\n" : "=r" (tsd));
    /* lower 3-bits contain CPU number */
#endif

    return (void**)(uintptr_t)tsd;
}
#define _os_tsd_get_base()  _os_tsd_get_base()
#endif

#ifdef _os_tsd_get_base
__attribute__((always_inline))
static __inline__ void*
_os_tsd_get_direct(unsigned long slot) JL_NOTSAFEPOINT
{
    return _os_tsd_get_base()[slot];
}
#endif

__attribute__((always_inline, pure))
static __inline__ uintptr_t
_os_ptr_munge_token(void) JL_NOTSAFEPOINT
{
    return (uintptr_t)_os_tsd_get_direct(__TSD_PTR_MUNGE);
}

__attribute__((always_inline, pure))
JL_UNUSED static __inline__ uintptr_t
_os_ptr_munge(uintptr_t ptr) JL_NOTSAFEPOINT
{
    return ptr ^ _os_ptr_munge_token();
}
#define _OS_PTR_UNMUNGE(_ptr) _os_ptr_munge((uintptr_t)(_ptr))
#endif


extern bt_context_t *jl_to_bt_context(void *sigctx) JL_NOTSAFEPOINT;

// Some notes: this simulates a longjmp call occurring in context `c`, as if the
// user was to set the PC in `c` to call longjmp and the PC in the longjmp to
// return here. This helps work around many cases where siglongjmp out of a
// signal handler is not supported (e.g. missing a _sigunaltstack call).
// Additionally note that this doesn't restore the MXCSR or FP control word
// (which some, but not most longjmp implementations do).  It also doesn't
// support shadow stacks, so if those are in use, you might need to use a direct
// jl_longjmp instead to leave the signal frame instead of relying on simulating
// it and attempting to return normally.
int jl_simulate_longjmp(jl_jmp_buf mctx, bt_context_t *c) JL_NOTSAFEPOINT
{
#if (defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_))
    // https://github.com/llvm/llvm-project/blob/main/compiler-rt/lib/hwasan/hwasan_interceptors.cpp
    return 0;
#elif defined(_OS_WINDOWS_)
    _JUMP_BUFFER* _ctx = (_JUMP_BUFFER*)mctx;
    #if defined(_CPU_X86_64_)
    c->Rbx = _ctx->Rbx;
    c->Rsp = _ctx->Rsp;
    c->Rbp = _ctx->Rbp;
    c->Rsi = _ctx->Rsi;
    c->Rdi = _ctx->Rdi;
    c->R12 = _ctx->R12;
    c->R13 = _ctx->R13;
    c->R14 = _ctx->R14;
    c->R15 = _ctx->R15;
    c->Rip = _ctx->Rip;
    memcpy(&c->Xmm6, &_ctx->Xmm6, 10 * sizeof(_ctx->Xmm6)); // Xmm6-Xmm15
    // c->MxCsr = _ctx->MxCsr;
    // c->FloatSave.ControlWord = _ctx->FpCsr;
    // c->SegGS[0] = _ctx->Frame;
    c->Rax = 1;
    c->Rsp += sizeof(void*);
    assert(c->Rsp % 16 == 0);
    return 1;
    #elif defined(_CPU_X86_)
    c->Ebp = _ctx->Ebp;
    c->Ebx = _ctx->Ebx;
    c->Edi = _ctx->Edi;
    c->Esi = _ctx->Esi;
    c->Esp = _ctx->Esp;
    c->Eip = _ctx->Eip;
    // c->SegFS[0] = _ctx->Registration;
    // c->FloatSave.ControlWord = _ctx->FpCsr;
    c->Eax = 1;
    c->Esp += sizeof(void*);
    assert(c->Esp % 16 == 0);
    return 1;
    #else
    #error Windows is currently only supported on x86 and x86_64
    #endif
#elif defined(_OS_LINUX_) && defined(__GLIBC__)
    __jmp_buf *_ctx = &mctx->__jmpbuf;
    #if defined(_CPU_AARCH64_)
    // Only on aarch64-linux libunwind uses a different struct than system's one:
    // <https://github.com/libunwind/libunwind/blob/e63e024b72d35d4404018fde1a546fde976da5c5/include/libunwind-aarch64.h#L193-L205>.
    struct unw_sigcontext *mc = &c->uc_mcontext;
    #else
    mcontext_t *mc = &c->uc_mcontext;
    #endif
    #if defined(_CPU_X86_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/i386/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/i386/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/i386/longjmp.s
    mc->gregs[REG_EBX] = (*_ctx)[0];
    mc->gregs[REG_ESI] = (*_ctx)[1];
    mc->gregs[REG_EDI] = (*_ctx)[2];
    mc->gregs[REG_EBP] = (*_ctx)[3];
    mc->gregs[REG_ESP] = (*_ctx)[4];
    mc->gregs[REG_EIP] = (*_ctx)[5];
    // ifdef PTR_DEMANGLE ?
    mc->gregs[REG_ESP] = ptr_demangle(mc->gregs[REG_ESP]);
    mc->gregs[REG_EIP] = ptr_demangle(mc->gregs[REG_EIP]);
    mc->gregs[REG_EAX] = 1;
    assert(mc->gregs[REG_ESP] % 16 == 0);
    return 1;
    #elif defined(_CPU_X86_64_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/x86_64/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/x86_64/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/x86_64/setjmp.s
    mc->gregs[REG_RBX] = (*_ctx)[0];
    mc->gregs[REG_RBP] = (*_ctx)[1];
    mc->gregs[REG_R12] = (*_ctx)[2];
    mc->gregs[REG_R13] = (*_ctx)[3];
    mc->gregs[REG_R14] = (*_ctx)[4];
    mc->gregs[REG_R15] = (*_ctx)[5];
    mc->gregs[REG_RSP] = (*_ctx)[6];
    mc->gregs[REG_RIP] = (*_ctx)[7];
    // ifdef PTR_DEMANGLE ?
    mc->gregs[REG_RBP] = ptr_demangle(mc->gregs[REG_RBP]);
    mc->gregs[REG_RSP] = ptr_demangle(mc->gregs[REG_RSP]);
    mc->gregs[REG_RIP] = ptr_demangle(mc->gregs[REG_RIP]);
    mc->gregs[REG_RAX] = 1;
    assert(mc->gregs[REG_RSP] % 16 == 0);
    return 1;
    #elif defined(_CPU_ARM_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/arm/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/arm/include/bits/setjmp.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/arm/longjmp.S
    mc->arm_sp = (*_ctx)[0];
    mc->arm_lr = (*_ctx)[1];
    mc->arm_r4 = (*_ctx)[2]; // aka v1
    mc->arm_r5 = (*_ctx)[3]; // aka v2
    mc->arm_r6 = (*_ctx)[4]; // aka v3
    mc->arm_r7 = (*_ctx)[5]; // aka v4
    mc->arm_r8 = (*_ctx)[6]; // aka v5
    mc->arm_r9 = (*_ctx)[7]; // aka v6 aka sb
    mc->arm_r10 = (*_ctx)[8]; // aka v7 aka sl
    mc->arm_fp = (*_ctx)[10]; // aka v8 aka r11
    // ifdef PTR_DEMANGLE ?
    mc->arm_sp = ptr_demangle(mc->arm_sp);
    mc->arm_lr = ptr_demangle(mc->arm_lr);
    mc->arm_pc = mc->arm_lr;
    mc->arm_r0 = 1;
    assert(mc->arm_sp % 16 == 0);
    return 1;
    #elif defined(_CPU_AARCH64_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/aarch64/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/aarch64/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/aarch64/longjmp.s
    // https://github.com/libunwind/libunwind/blob/ec171c9ba7ea3abb2a1383cee2988a7abd483a1f/src/aarch64/unwind_i.h#L62
    unw_fpsimd_context_t *mcfp = (unw_fpsimd_context_t*)&mc->__reserved;
    mc->regs[19] = (*_ctx)[0];
    mc->regs[20] = (*_ctx)[1];
    mc->regs[21] = (*_ctx)[2];
    mc->regs[22] = (*_ctx)[3];
    mc->regs[23] = (*_ctx)[4];
    mc->regs[24] = (*_ctx)[5];
    mc->regs[25] = (*_ctx)[6];
    mc->regs[26] = (*_ctx)[7];
    mc->regs[27] = (*_ctx)[8];
    mc->regs[28] = (*_ctx)[9];
    mc->regs[29] = (*_ctx)[10]; // aka fp
    mc->regs[30] = (*_ctx)[11]; // aka lr
    // Yes, they did skip 12 when writing the code originally; and, no, I do not know why.
    mc->sp = (*_ctx)[13];
    mcfp->vregs[7] = (*_ctx)[14]; // aka d8
    mcfp->vregs[8] = (*_ctx)[15]; // aka d9
    mcfp->vregs[9] = (*_ctx)[16]; // aka d10
    mcfp->vregs[10] = (*_ctx)[17]; // aka d11
    mcfp->vregs[11] = (*_ctx)[18]; // aka d12
    mcfp->vregs[12] = (*_ctx)[19]; // aka d13
    mcfp->vregs[13] = (*_ctx)[20]; // aka d14
    mcfp->vregs[14] = (*_ctx)[21]; // aka d15
    // ifdef PTR_DEMANGLE ?
    mc->sp = ptr_demangle(mc->sp);
    mc->regs[30] = ptr_demangle(mc->regs[30]);
    mc->pc = mc->regs[30];
    mc->regs[0] = 1;
    assert(mc->sp % 16 == 0);
    return 1;
    #elif defined(_CPU_RISCV64_)
    // https://github.com/bminor/glibc/blob/master/sysdeps/riscv/bits/setjmp.h
    // https://github.com/llvm/llvm-project/blob/7714e0317520207572168388f22012dd9e152e9e/libunwind/src/Registers.hpp -> Registers_riscv
    mc->__gregs[1] = (*_ctx)->__pc;        // ra
    mc->__gregs[8] = (*_ctx)->__regs[0];   // s0
    mc->__gregs[9] = (*_ctx)->__regs[1];   // s1
    mc->__gregs[18] = (*_ctx)->__regs[2];  // s2
    mc->__gregs[19] = (*_ctx)->__regs[3];  // s3
    mc->__gregs[20] = (*_ctx)->__regs[4];  // s4
    mc->__gregs[21] = (*_ctx)->__regs[5];  // s5
    mc->__gregs[22] = (*_ctx)->__regs[6];  // s6
    mc->__gregs[23] = (*_ctx)->__regs[7];  // s7
    mc->__gregs[24] = (*_ctx)->__regs[8];  // s8
    mc->__gregs[25] = (*_ctx)->__regs[9];  // s9
    mc->__gregs[26] = (*_ctx)->__regs[10]; // s10
    mc->__gregs[27] = (*_ctx)->__regs[11]; // s11
    mc->__gregs[2] = (*_ctx)->__sp;        // sp
    #ifndef __riscv_float_abi_soft
    mc->__fpregs.__d.__f[8] = (unsigned long long) (*_ctx)->__fpregs[0];   // fs0
    mc->__fpregs.__d.__f[9] = (unsigned long long) (*_ctx)->__fpregs[1];   // fs1
    mc->__fpregs.__d.__f[18] = (unsigned long long) (*_ctx)->__fpregs[2];  // fs2
    mc->__fpregs.__d.__f[19] = (unsigned long long) (*_ctx)->__fpregs[3];  // fs3
    mc->__fpregs.__d.__f[20] = (unsigned long long) (*_ctx)->__fpregs[4];  // fs4
    mc->__fpregs.__d.__f[21] = (unsigned long long) (*_ctx)->__fpregs[5];  // fs5
    mc->__fpregs.__d.__f[22] = (unsigned long long) (*_ctx)->__fpregs[6];  // fs6
    mc->__fpregs.__d.__f[23] = (unsigned long long) (*_ctx)->__fpregs[7];  // fs7
    mc->__fpregs.__d.__f[24] = (unsigned long long) (*_ctx)->__fpregs[8];  // fs8
    mc->__fpregs.__d.__f[25] = (unsigned long long) (*_ctx)->__fpregs[9];  // fs9
    mc->__fpregs.__d.__f[26] = (unsigned long long) (*_ctx)->__fpregs[10]; // fs10
    mc->__fpregs.__d.__f[27] = (unsigned long long) (*_ctx)->__fpregs[11]; // fs11
    #endif
    // ifdef PTR_DEMANGLE ?
    mc->__gregs[REG_SP] = ptr_demangle(mc->__gregs[REG_SP]);
    mc->__gregs[REG_RA] = ptr_demangle(mc->__gregs[REG_RA]);
    mc->__gregs[REG_PC] = mc->__gregs[REG_RA];
    mc->__gregs[REG_A0] = 1;
    assert(mc->__gregs[REG_SP] % 16 == 0);
    return 1;
    #else
    #pragma message("jl_record_backtrace not defined for ASM/SETJMP on unknown linux")
    (void)mc;
    (void)mctx;
    return 0;
    #endif
#elif defined(_OS_DARWIN_)
    #if defined(_CPU_X86_64_)
    // from https://github.com/apple/darwin-libplatform/blob/main/src/setjmp/x86_64/_setjmp.s
    x86_thread_state64_t *mc = (x86_thread_state64_t*)c;
    mc->__rbx = ((uint64_t*)mctx)[0];
    mc->__rbp = ((uint64_t*)mctx)[1];
    mc->__rsp = ((uint64_t*)mctx)[2];
    mc->__r12 = ((uint64_t*)mctx)[3];
    mc->__r13 = ((uint64_t*)mctx)[4];
    mc->__r14 = ((uint64_t*)mctx)[5];
    mc->__r15 = ((uint64_t*)mctx)[6];
    mc->__rip = ((uint64_t*)mctx)[7];
    // added in libsystem_platform 177.200.16 (macOS Mojave 10.14.3)
    // prior to that _os_ptr_munge_token was (hopefully) typically 0,
    // so x ^ 0 == x and this is a no-op
    mc->__rbp = _OS_PTR_UNMUNGE(mc->__rbp);
    mc->__rsp = _OS_PTR_UNMUNGE(mc->__rsp);
    mc->__rip = _OS_PTR_UNMUNGE(mc->__rip);
    mc->__rax = 1;
    assert(mc->__rsp % 16 == 0);
    return 1;
    #elif defined(_CPU_AARCH64_)
    // from https://github.com/apple/darwin-libplatform/blob/main/src/setjmp/arm64/setjmp.s
    // https://github.com/apple/darwin-xnu/blob/main/osfmk/mach/arm/_structs.h
    // https://github.com/llvm/llvm-project/blob/7714e0317520207572168388f22012dd9e152e9e/libunwind/src/Registers.hpp -> Registers_arm64
    arm_thread_state64_t *mc = (arm_thread_state64_t*)c;
    mc->__x[19] = ((uint64_t*)mctx)[0];
    mc->__x[20] = ((uint64_t*)mctx)[1];
    mc->__x[21] = ((uint64_t*)mctx)[2];
    mc->__x[22] = ((uint64_t*)mctx)[3];
    mc->__x[23] = ((uint64_t*)mctx)[4];
    mc->__x[24] = ((uint64_t*)mctx)[5];
    mc->__x[25] = ((uint64_t*)mctx)[6];
    mc->__x[26] = ((uint64_t*)mctx)[7];
    mc->__x[27] = ((uint64_t*)mctx)[8];
    mc->__x[28] = ((uint64_t*)mctx)[9];
    mc->__x[10] = ((uint64_t*)mctx)[10];
    mc->__x[11] = ((uint64_t*)mctx)[11];
    mc->__x[12] = ((uint64_t*)mctx)[12];
    // 13 is reserved/unused
    double *mcfp = (double*)&mc[1];
    mcfp[7] = ((uint64_t*)mctx)[14]; // aka d8
    mcfp[8] = ((uint64_t*)mctx)[15]; // aka d9
    mcfp[9] = ((uint64_t*)mctx)[16]; // aka d10
    mcfp[10] = ((uint64_t*)mctx)[17]; // aka d11
    mcfp[11] = ((uint64_t*)mctx)[18]; // aka d12
    mcfp[12] = ((uint64_t*)mctx)[19]; // aka d13
    mcfp[13] = ((uint64_t*)mctx)[20]; // aka d14
    mcfp[14] = ((uint64_t*)mctx)[21]; // aka d15
    mc->__fp = _OS_PTR_UNMUNGE(mc->__x[10]);
    mc->__lr = _OS_PTR_UNMUNGE(mc->__x[11]);
    mc->__x[12] = _OS_PTR_UNMUNGE(mc->__x[12]);
    mc->__sp = mc->__x[12];
    // libunwind is broken for signed-pointers, but perhaps best not to leave the signed pointer lying around either
    mc->__pc = ptrauth_strip(mc->__lr, 0);
    mc->__pad = 0; // aka __ra_sign_state = not signed
    mc->__x[0] = 1;
    assert(mc->__sp % 16 == 0);
    return 1;
    #else
    #pragma message("jl_record_backtrace not defined for ASM/SETJMP on unknown darwin")
    (void)mctx;
    return 0;
#endif
#elif defined(_OS_FREEBSD_)
    mcontext_t *mc = &c->uc_mcontext;
    #if defined(_CPU_X86_64_)
    // https://github.com/freebsd/freebsd-src/blob/releng/13.1/lib/libc/amd64/gen/_setjmp.S
    mc->mc_rip = ((long*)mctx)[0];
    mc->mc_rbx = ((long*)mctx)[1];
    mc->mc_rsp = ((long*)mctx)[2];
    mc->mc_rbp = ((long*)mctx)[3];
    mc->mc_r12 = ((long*)mctx)[4];
    mc->mc_r13 = ((long*)mctx)[5];
    mc->mc_r14 = ((long*)mctx)[6];
    mc->mc_r15 = ((long*)mctx)[7];
    mc->mc_rax = 1;
    mc->mc_rsp += sizeof(void*);
    assert(mc->mc_rsp % 16 == 0);
    return 1;
    #elif defined(_CPU_AARCH64_)
    mc->mc_gpregs.gp_x[19] = ((long*)mctx)[0];
    mc->mc_gpregs.gp_x[20] = ((long*)mctx)[1];
    mc->mc_gpregs.gp_x[21] = ((long*)mctx)[2];
    mc->mc_gpregs.gp_x[22] = ((long*)mctx)[3];
    mc->mc_gpregs.gp_x[23] = ((long*)mctx)[4];
    mc->mc_gpregs.gp_x[24] = ((long*)mctx)[5];
    mc->mc_gpregs.gp_x[25] = ((long*)mctx)[6];
    mc->mc_gpregs.gp_x[26] = ((long*)mctx)[7];
    mc->mc_gpregs.gp_x[27] = ((long*)mctx)[8];
    mc->mc_gpregs.gp_x[28] = ((long*)mctx)[9];
    mc->mc_gpregs.gp_x[29] = ((long*)mctx)[10];
    mc->mc_gpregs.gp_lr = ((long*)mctx)[11];
    mc->mc_gpregs.gp_sp = ((long*)mctx)[12];
    mc->mc_fpregs.fp_q[7] = ((long*)mctx)[13];
    mc->mc_fpregs.fp_q[8] = ((long*)mctx)[14];
    mc->mc_fpregs.fp_q[9] = ((long*)mctx)[15];
    mc->mc_fpregs.fp_q[10] = ((long*)mctx)[16];
    mc->mc_fpregs.fp_q[11] = ((long*)mctx)[17];
    mc->mc_fpregs.fp_q[12] = ((long*)mctx)[18];
    mc->mc_fpregs.fp_q[13] = ((long*)mctx)[19];
    mc->mc_fpregs.fp_q[14] = ((long*)mctx)[20];
    mc->mc_gpregs.gp_x[0] = 1;
    assert(mc->mc_gpregs.gp_sp % 16 == 0);
    return 1;
    #else
    #pragma message("jl_record_backtrace not defined for ASM/SETJMP on unknown freebsd")
    (void)mctx;
    return 0;
    #endif
#else
return 0;
#endif
}

typedef struct {
    int16_t old;
    bt_context_t *c;
    int success;
} suspend_t;
static void suspend(void *ctx)
{
    suspend_t *suspenddata = (suspend_t*)ctx;
    suspenddata->success = jl_thread_suspend_and_get_state(suspenddata->old, 1, suspenddata->c);
}

JL_DLLEXPORT size_t jl_try_record_thread_backtrace(jl_ptls_t ptls2, jl_bt_element_t *bt_data, size_t max_bt_size) JL_NOTSAFEPOINT
{
    int16_t tid = ptls2->tid;
    jl_task_t *t = NULL;
    bt_context_t *context = NULL;
    bt_context_t c;
    suspend_t suspenddata = {tid, &c};
    jl_with_stackwalk_lock(suspend, &suspenddata);
    if (!suspenddata.success) {
        return 0;
    }
    // thread is stopped, safe to read the task it was running before we stopped it
    t = jl_atomic_load_relaxed(&ptls2->current_task);
    context = &c;
    size_t bt_size = rec_backtrace_ctx(bt_data, max_bt_size, context, ptls2->previous_task ? NULL : t->gcstack);
    jl_thread_resume(tid);
    return bt_size;
}

JL_DLLEXPORT jl_record_backtrace_result_t jl_record_backtrace(jl_task_t *t, jl_bt_element_t *bt_data, size_t max_bt_size, int all_tasks_profiler) JL_NOTSAFEPOINT
{
    int16_t tid = INT16_MAX;
    jl_record_backtrace_result_t result = {0, tid};
    jl_task_t *ct = NULL;
    jl_ptls_t ptls = NULL;
    if (!all_tasks_profiler) {
        ct = jl_current_task;
        ptls = ct->ptls;
        ptls->bt_size = 0;
        tid = ptls->tid;
    }
    if (t == ct) {
        result.bt_size = rec_backtrace(bt_data, max_bt_size, 0);
        result.tid = tid;
        return result;
    }
    bt_context_t *context = NULL;
    bt_context_t c;
    int16_t old;
    for (old = -1; !jl_atomic_cmpswap(&t->tid, &old, tid) && old != tid; old = -1) {
        // if this task is already running somewhere, we need to stop the thread it is running on and query its state
        suspend_t suspenddata = {old, &c};
        jl_with_stackwalk_lock(suspend, &suspenddata);
        if (!suspenddata.success) {
            if (jl_atomic_load_relaxed(&t->tid) != old)
                continue;
            return result;
        }
        if (jl_atomic_load_relaxed(&t->tid) == old) {
            jl_ptls_t ptls2 = jl_atomic_load_relaxed(&jl_all_tls_states)[old];
            if (ptls2->previous_task == t || // we might print the wrong stack here, since we can't know whether we executed the swapcontext yet or not, but it at least avoids trying to access the state inside uc_mcontext which might not be set yet
                (ptls2->previous_task == NULL && jl_atomic_load_relaxed(&ptls2->current_task) == t)) { // this case should be always accurate
                // use the thread context for the unwind state
                context = &c;
            }
            break;
        }
        // got the wrong thread stopped, try again
        jl_thread_resume(old);
    }
    if (context == NULL && (!t->ctx.copy_stack && t->ctx.started && t->ctx.ctx != NULL)) {
        // need to read the context from the task stored state
        jl_jmp_buf *mctx = &t->ctx.ctx->uc_mcontext;
#if defined(_OS_WINDOWS_)
        memset(&c, 0, sizeof(c));
        if (jl_simulate_longjmp(*mctx, &c))
            context = &c;
#elif defined(JL_HAVE_UNW_CONTEXT)
        context = t->ctx.ctx;
#elif defined(JL_HAVE_UCONTEXT)
        context = jl_to_bt_context(t->ctx.ctx);
#elif defined(JL_HAVE_ASM)
        memset(&c, 0, sizeof(c));
        if (jl_simulate_longjmp(*mctx, &c))
            context = &c;
#else
     #pragma message("jl_record_backtrace not defined for unknown task system")
#endif
    }
    size_t bt_size = 0;
    if (context) {
        bt_size = rec_backtrace_ctx(bt_data, max_bt_size, context, all_tasks_profiler ? NULL : t->gcstack);
    }
    if (old == -1)
        jl_atomic_store_relaxed(&t->tid, old);
    else if (old != tid)
        jl_thread_resume(old);
    result.bt_size = bt_size;
    result.tid = old;
    return result;
}

//--------------------------------------------------
// Tools for interactive debugging in gdb

JL_DLLEXPORT void jl_gdblookup(void* ip)
{
    jl_print_native_codeloc((uintptr_t)ip);
}

// Print backtrace for current exception in catch block
JL_DLLEXPORT void jlbacktrace(void) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    if (ct->ptls == NULL)
        return;
    jl_excstack_t *s = ct->excstack;
    if (!s)
        return;
    size_t i, bt_size = jl_excstack_bt_size(s, s->top);
    jl_bt_element_t *bt_data = jl_excstack_bt_data(s, s->top);
    for (i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_print_bt_entry_codeloc(bt_data + i);
    }
}

// Print backtrace for specified task to jl_safe_printf stderr
JL_DLLEXPORT void jlbacktracet(jl_task_t *t) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    ptls->bt_size = 0;
    jl_bt_element_t *bt_data = ptls->bt_data;
    jl_record_backtrace_result_t r = jl_record_backtrace(t, bt_data, JL_MAX_BT_SIZE, 0);
    size_t bt_size = r.bt_size;
    size_t i;
    for (i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_print_bt_entry_codeloc(bt_data + i);
    }
    if (bt_size == 0)
        jl_safe_printf("      no backtrace recorded\n");
}

JL_DLLEXPORT void jl_print_backtrace(void) JL_NOTSAFEPOINT
{
    jlbacktrace();
}

// Print backtraces for all live tasks, for all threads, to jl_safe_printf stderr
JL_DLLEXPORT void jl_print_task_backtraces(int show_done) JL_NOTSAFEPOINT
{
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    jl_ptls_t *allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (size_t i = 0; i < nthreads; i++) {
        jl_ptls_t ptls2 = allstates[i];
        if (gc_is_collector_thread(i)) {
            jl_safe_printf("==== Skipping backtrace for parallel/concurrent GC thread %zu\n", i + 1);
            continue;
        }
        if (ptls2 == NULL) {
            continue;
        }
        small_arraylist_t *live_tasks = &ptls2->gc_tls_common.heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        int t_state = JL_TASK_STATE_DONE;
        jl_task_t *t = ptls2->root_task;
        if (t != NULL)
            t_state = jl_atomic_load_relaxed(&t->_state);
        jl_safe_printf("==== Thread %d created %zu live tasks\n",
                ptls2->tid + 1, n + (t_state != JL_TASK_STATE_DONE));
        if (show_done || t_state != JL_TASK_STATE_DONE) {
            jl_safe_printf("     ---- Root task (%p)\n", ptls2->root_task);
            if (t != NULL) {
                jl_safe_printf("          (sticky: %d, started: %d, state: %d, tid: %d)\n",
                        t->sticky, t->ctx.started, t_state,
                        jl_atomic_load_relaxed(&t->tid) + 1);
                jlbacktracet(t);
            }
            jl_safe_printf("     ---- End root task\n");
        }

        for (size_t j = 0; j < n; j++) {
            jl_task_t *t = (jl_task_t*)mtarraylist_get(live_tasks, j);
            if (t == NULL)
                continue;
            int t_state = jl_atomic_load_relaxed(&t->_state);
            if (!show_done && t_state == JL_TASK_STATE_DONE)
                continue;
            jl_safe_printf("     ---- Task %zu (%p)\n", j + 1, t);
            // n.b. this information might not be consistent with the stack printing after it, since it could start running or change tid, etc.
            jl_safe_printf("          (sticky: %d, started: %d, state: %d, tid: %d)\n",
                    t->sticky, t->ctx.started, t_state,
                    jl_atomic_load_relaxed(&t->tid) + 1);
            jlbacktracet(t);
            jl_safe_printf("     ---- End task %zu\n", j + 1);
        }
        jl_safe_printf("==== End thread %d\n", ptls2->tid + 1);
    }
    jl_safe_printf("==== Done\n");
}

#ifdef __cplusplus
}
#endif
