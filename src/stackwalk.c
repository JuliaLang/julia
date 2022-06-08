// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  stackwalk.c
  utilities for walking the stack and looking up information about code addresses
*/
#include <inttypes.h>
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
#if !defined(_OS_WINDOWS_)
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
            if (oldsp >= thesp && !jl_running_under_rr(0)) {
                // The stack pointer is clearly bad, as it must grow downwards.
                // But sometimes the external unwinder doesn't check that.
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
            // ARM instruction pointer encoding uses the low bit as a flag for
            // thumb mode, which must be cleared before further use. (Note not
            // needed for ARM AArch64.) See
            // https://github.com/libunwind/libunwind/pull/131
            #ifdef _CPU_ARM_
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
NOINLINE size_t rec_backtrace(jl_bt_element_t *bt_data, size_t maxsize, int skip)
{
    bt_context_t context;
    memset(&context, 0, sizeof(context));
    int r = jl_unw_get(&context);
    if (r < 0)
        return 0;
    jl_gcframe_t *pgcstack = jl_pgcstack;
    bt_cursor_t cursor;
    if (!jl_unw_init(&cursor, &context))
        return 0;
    size_t bt_size = 0;
    jl_unw_stepn(&cursor, bt_data, &bt_size, NULL, maxsize, skip + 1, &pgcstack, 0);
    return bt_size;
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
                sp_ptr = (uintptr_t*)jl_array_data(sp) + offset;
            }
            size_t size_incr = 0;
            have_more_frames = jl_unw_stepn(&cursor, (jl_bt_element_t*)jl_array_data(ip) + offset,
                                            &size_incr, sp_ptr, maxincr, skip, &pgcstack, 0);
            skip = 0;
            offset += size_incr;
        }
        jl_array_del_end(ip, jl_array_len(ip) - offset);
        if (returnsp)
            jl_array_del_end(sp, jl_array_len(sp) - offset);

        size_t n = 0;
        jl_bt_element_t *bt_data = (jl_bt_element_t*)jl_array_data(ip);
        while (n < jl_array_len(ip)) {
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
    memcpy(bt->data, bt_data, bt_size * sizeof(jl_bt_element_t));
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
        jl_svecset(r, 3, frame.linfo != NULL ? (jl_value_t*)frame.linfo : jl_nothing);
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
            jl_safe_printf("unknown function (ip: %p)\n", (void*)ip);
        }
        else {
            jl_safe_print_codeloc(frame.func_name, frame.file_name, frame.line, frame.inlined);
            free(frame.func_name);
            free(frame.file_name);
        }
    }
    free(frames);
}

// Print code location for backtrace buffer entry at *bt_entry
void jl_print_bt_entry_codeloc(jl_bt_element_t *bt_entry) JL_NOTSAFEPOINT
{
    if (jl_bt_is_native(bt_entry)) {
        jl_print_native_codeloc(bt_entry[0].uintptr);
    }
    else if (jl_bt_entry_tag(bt_entry) == JL_BT_INTERP_FRAME_TAG) {
        size_t ip = jl_bt_entry_header(bt_entry);
        jl_value_t *code = jl_bt_entry_jlvalue(bt_entry, 0);
        if (jl_is_method_instance(code)) {
            // When interpreting a method instance, need to unwrap to find the code info
            code = ((jl_method_instance_t*)code)->uninferred;
        }
        if (jl_is_code_info(code)) {
            jl_code_info_t *src = (jl_code_info_t*)code;
            // See also the debug info handling in codegen.cpp.
            // NB: debuginfoloc is 1-based!
            intptr_t debuginfoloc = ((int32_t*)jl_array_data(src->codelocs))[ip];
            while (debuginfoloc != 0) {
                jl_line_info_node_t *locinfo = (jl_line_info_node_t*)
                    jl_array_ptr_ref(src->linetable, debuginfoloc - 1);
                assert(jl_typeis(locinfo, jl_lineinfonode_type));
                const char *func_name = "Unknown";
                jl_value_t *method = locinfo->method;
                if (jl_is_method_instance(method))
                    method = ((jl_method_instance_t*)method)->def.value;
                if (jl_is_method(method))
                    method = (jl_value_t*)((jl_method_t*)method)->name;
                if (jl_is_symbol(method))
                    func_name = jl_symbol_name((jl_sym_t*)method);
                jl_safe_print_codeloc(func_name, jl_symbol_name(locinfo->file),
                                      locinfo->line, locinfo->inlined_at);
                debuginfoloc = locinfo->inlined_at;
            }
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

JL_UNUSED static uintptr_t ptr_demangle(uintptr_t p)
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
static inline uint64_t ptrauth_strip(uint64_t __value, unsigned int __key) {
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
_os_tsd_get_base(void)
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
_os_tsd_get_direct(unsigned long slot)
{
    return _os_tsd_get_base()[slot];
}
#endif

__attribute__((always_inline, pure))
static __inline__ uintptr_t
_os_ptr_munge_token(void)
{
    return (uintptr_t)_os_tsd_get_direct(__TSD_PTR_MUNGE);
}

__attribute__((always_inline, pure))
JL_UNUSED static __inline__ uintptr_t
_os_ptr_munge(uintptr_t ptr)
{
    return ptr ^ _os_ptr_munge_token();
}
#define _OS_PTR_UNMUNGE(_ptr) _os_ptr_munge((uintptr_t)(_ptr))
#endif


extern bt_context_t *jl_to_bt_context(void *sigctx);

void jl_rec_backtrace(jl_task_t *t)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    ptls->bt_size = 0;
    if (t == ct) {
        ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE, 0);
        return;
    }
    if (t->copy_stack || !t->started || t->stkbuf == NULL)
        return;
    int16_t old = -1;
    if (!jl_atomic_cmpswap(&t->tid, &old, ptls->tid) && old != ptls->tid)
        return;
    bt_context_t *context = NULL;
#if defined(_OS_WINDOWS_)
    bt_context_t c;
    memset(&c, 0, sizeof(c));
    _JUMP_BUFFER *mctx = (_JUMP_BUFFER*)&t->ctx.ctx.uc_mcontext;
#if defined(_CPU_X86_64_)
    c.Rbx = mctx->Rbx;
    c.Rsp = mctx->Rsp;
    c.Rbp = mctx->Rbp;
    c.Rsi = mctx->Rsi;
    c.Rdi = mctx->Rdi;
    c.R12 = mctx->R12;
    c.R13 = mctx->R13;
    c.R14 = mctx->R14;
    c.R15 = mctx->R15;
    c.Rip = mctx->Rip;
    memcpy(&c.Xmm6, &mctx->Xmm6, 10 * sizeof(mctx->Xmm6)); // Xmm6-Xmm15
#else
    c.Eip = mctx->Eip;
    c.Esp = mctx->Esp;
    c.Ebp = mctx->Ebp;
#endif
    context = &c;
#elif defined(JL_HAVE_UNW_CONTEXT)
    context = &t->ctx.ctx;
#elif defined(JL_HAVE_UCONTEXT)
    context = jl_to_bt_context(&t->ctx.ctx);
#elif defined(JL_HAVE_ASM)
    bt_context_t c;
    memset(&c, 0, sizeof(c));
 #if defined(_OS_LINUX_) && defined(__GLIBC__)
    __jmp_buf *mctx = &t->ctx.ctx.uc_mcontext->__jmpbuf;
    mcontext_t *mc = &c.uc_mcontext;
  #if defined(_CPU_X86_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/i386/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/i386/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/i386/longjmp.s
    mc->gregs[REG_EBX] = (*mctx)[0];
    mc->gregs[REG_ESI] = (*mctx)[1];
    mc->gregs[REG_EDI] = (*mctx)[2];
    mc->gregs[REG_EBP] = (*mctx)[3];
    mc->gregs[REG_ESP] = (*mctx)[4];
    mc->gregs[REG_EIP] = (*mctx)[5];
    // ifdef PTR_DEMANGLE ?
    mc->gregs[REG_ESP] = ptr_demangle(mc->gregs[REG_ESP]);
    mc->gregs[REG_EIP] = ptr_demangle(mc->gregs[REG_EIP]);
    context = &c;
  #elif defined(_CPU_X86_64_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/x86_64/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/x86_64/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/x86_64/setjmp.s
    mc->gregs[REG_RBX] = (*mctx)[0];
    mc->gregs[REG_RBP] = (*mctx)[1];
    mc->gregs[REG_R12] = (*mctx)[2];
    mc->gregs[REG_R13] = (*mctx)[3];
    mc->gregs[REG_R14] = (*mctx)[4];
    mc->gregs[REG_R15] = (*mctx)[5];
    mc->gregs[REG_RSP] = (*mctx)[6];
    mc->gregs[REG_RIP] = (*mctx)[7];
    // ifdef PTR_DEMANGLE ?
    mc->gregs[REG_RBP] = ptr_demangle(mc->gregs[REG_RBP]);
    mc->gregs[REG_RSP] = ptr_demangle(mc->gregs[REG_RSP]);
    mc->gregs[REG_RIP] = ptr_demangle(mc->gregs[REG_RIP]);
    context = &c;
  #elif defined(_CPU_ARM_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/arm/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/arm/include/bits/setjmp.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/arm/longjmp.S
    mc->arm_sp = (*mctx)[0];
    mc->arm_lr = (*mctx)[1];
    mc->arm_r4 = (*mctx)[2]; // aka v1
    mc->arm_r5 = (*mctx)[3]; // aka v2
    mc->arm_r6 = (*mctx)[4]; // aka v3
    mc->arm_r7 = (*mctx)[5]; // aka v4
    mc->arm_r8 = (*mctx)[6]; // aka v5
    mc->arm_r9 = (*mctx)[7]; // aka v6 aka sb
    mc->arm_r10 = (*mctx)[8]; // aka v7 aka sl
    mc->arm_fp = (*mctx)[10]; // aka v8 aka r11
    // ifdef PTR_DEMANGLE ?
    mc->arm_sp = ptr_demangle(mc->arm_sp);
    mc->arm_lr = ptr_demangle(mc->arm_lr);
    mc->arm_pc = mc->arm_lr;
    context = &c;
  #elif defined(_CPU_AARCH64_)
    // https://github.com/bminor/glibc/blame/master/sysdeps/aarch64/__longjmp.S
    // https://github.com/bminor/glibc/blame/master/sysdeps/aarch64/jmpbuf-offsets.h
    // https://github.com/bminor/musl/blame/master/src/setjmp/aarch64/longjmp.s
    // https://github.com/libunwind/libunwind/blob/ec171c9ba7ea3abb2a1383cee2988a7abd483a1f/src/aarch64/unwind_i.h#L62
    unw_fpsimd_context_t *mcfp = (unw_fpsimd_context_t*)&mc->__reserved;
    mc->regs[19] = (*mctx)[0];
    mc->regs[20] = (*mctx)[1];
    mc->regs[21] = (*mctx)[2];
    mc->regs[22] = (*mctx)[3];
    mc->regs[23] = (*mctx)[4];
    mc->regs[24] = (*mctx)[5];
    mc->regs[25] = (*mctx)[6];
    mc->regs[26] = (*mctx)[7];
    mc->regs[27] = (*mctx)[8];
    mc->regs[28] = (*mctx)[9];
    mc->regs[29] = (*mctx)[10]; // aka fp
    mc->regs[30] = (*mctx)[11]; // aka lr
    // Yes, they did skip 12 why writing the code originally; and, no, I do not know why.
    mc->sp = (*mctx)[13];
    mcfp->vregs[7] = (*mctx)[14]; // aka d8
    mcfp->vregs[8] = (*mctx)[15]; // aka d9
    mcfp->vregs[9] = (*mctx)[16]; // aka d10
    mcfp->vregs[10] = (*mctx)[17]; // aka d11
    mcfp->vregs[11] = (*mctx)[18]; // aka d12
    mcfp->vregs[12] = (*mctx)[19]; // aka d13
    mcfp->vregs[13] = (*mctx)[20]; // aka d14
    mcfp->vregs[14] = (*mctx)[21]; // aka d15
    // ifdef PTR_DEMANGLE ?
    mc->sp = ptr_demangle(mc->sp);
    mc->regs[30] = ptr_demangle(mc->regs[30]);
    mc->pc = mc->regs[30];
    context = &c;
  #else
   #pragma message("jl_rec_backtrace not defined for ASM/SETJMP on unknown linux")
   (void)mc;
   (void)c;
  #endif
 #elif defined(_OS_DARWIN_)
    sigjmp_buf *mctx = &t->ctx.ctx.uc_mcontext;
  #if defined(_CPU_X86_64_)
    // from https://github.com/apple/darwin-libplatform/blob/main/src/setjmp/x86_64/_setjmp.s
    x86_thread_state64_t *mc = (x86_thread_state64_t*)&c;
    mc->__rbx = ((uint64_t*)mctx)[0];
    mc->__rbp = ((uint64_t*)mctx)[1];
    mc->__rsp = ((uint64_t*)mctx)[2];
    mc->__r12 = ((uint64_t*)mctx)[3];
    mc->__r13 = ((uint64_t*)mctx)[4];
    mc->__r14 = ((uint64_t*)mctx)[5];
    mc->__r15 = ((uint64_t*)mctx)[6];
    mc->__rip = ((uint64_t*)mctx)[7];
    // added in libsystem_plaform 177.200.16 (macOS Mojave 10.14.3)
    // prior to that _os_ptr_munge_token was (hopefully) typically 0,
    // so x ^ 0 == x and this is a no-op
    mc->__rbp = _OS_PTR_UNMUNGE(mc->__rbp);
    mc->__rsp = _OS_PTR_UNMUNGE(mc->__rsp);
    mc->__rip = _OS_PTR_UNMUNGE(mc->__rip);
    context = &c;
  #elif defined(_CPU_AARCH64_)
    // from https://github.com/apple/darwin-libplatform/blob/main/src/setjmp/arm64/setjmp.s
    // https://github.com/apple/darwin-xnu/blob/main/osfmk/mach/arm/_structs.h
    // https://github.com/llvm/llvm-project/blob/7714e0317520207572168388f22012dd9e152e9e/libunwind/src/Registers.hpp -> Registers_arm64
    arm_thread_state64_t *mc = (arm_thread_state64_t*)&c;
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
    context = &c;
  #else
   #pragma message("jl_rec_backtrace not defined for ASM/SETJMP on unknown darwin")
    (void)mctx;
    (void)c;
  #endif
 #else
  #pragma message("jl_rec_backtrace not defined for ASM/SETJMP on unknown system")
  (void)c;
 #endif
#elif defined(JL_HAVE_ASYNCIFY)
 #pragma message("jl_rec_backtrace not defined for ASYNCIFY")
#elif defined(JL_HAVE_SIGALTSTACK)
 #pragma message("jl_rec_backtrace not defined for SIGALTSTACK")
#else
 #pragma message("jl_rec_backtrace not defined for unknown task system")
#endif
    if (context)
        ptls->bt_size = rec_backtrace_ctx(ptls->bt_data, JL_MAX_BT_SIZE, context, t->gcstack);
    if (old == -1)
        jl_atomic_store_relaxed(&t->tid, old);
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
JL_DLLEXPORT void jlbacktracet(jl_task_t *t)
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    jl_rec_backtrace(t);
    size_t i, bt_size = ptls->bt_size;
    jl_bt_element_t *bt_data = ptls->bt_data;
    for (i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_print_bt_entry_codeloc(bt_data + i);
    }
}

JL_DLLEXPORT void jl_print_backtrace(void) JL_NOTSAFEPOINT
{
    jlbacktrace();
}

#ifdef __cplusplus
}
#endif
