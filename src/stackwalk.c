// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  task.c
  lightweight processes (symmetric coroutines)
*/
#include "julia.h"
#include "julia_internal.h"
#include "threading.h"


// Always Set *func_name and *file_name to malloc'd pointers (non-NULL)
static int frame_info_from_ip(char **func_name,
                              char **file_name, size_t *line_num,
                              char **inlinedat_file, size_t *inlinedat_line,
                              jl_lambda_info_t **outer_linfo,
                              size_t ip, int skipC, int skipInline)
{
    // This function is not allowed to reference any TLS variables since
    // it can be called from an unmanaged thread on OSX.
    static const char *name_unknown = "???";
    int fromC = 0;

    jl_getFunctionInfo(func_name, file_name, line_num, inlinedat_file, inlinedat_line, outer_linfo,
            ip, &fromC, skipC, skipInline);
    if (!*func_name) {
        *func_name = strdup(name_unknown);
        *line_num = ip;
    }
    if (!*file_name) {
        *file_name = strdup(name_unknown);
    }
    return fromC;
}

#if defined(_OS_WINDOWS_)
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
    if (fn) return fn;
    if (jl_in_stackwalk) {
        return 0;
    }
    jl_in_stackwalk = 1;
    PVOID ftable = SymFunctionTableAccess64(hProcess, AddrBase);
    jl_in_stackwalk = 0;
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
    if (fn) return ImageBase;
    if (jl_in_stackwalk) {
        return 0;
    }
    jl_in_stackwalk = 1;
    DWORD64 fbase = SymGetModuleBase64(hProcess, dwAddr);
    jl_in_stackwalk = 0;
    return fbase;
#else
    if (dwAddr == HistoryTable.dwAddr) return HistoryTable.ImageBase;
    DWORD64 ImageBase = jl_getUnwindInfo(dwAddr);
    if (ImageBase) {
        HistoryTable.dwAddr = dwAddr;
        HistoryTable.ImageBase = ImageBase;
        return ImageBase;
    }
    return SymGetModuleBase64(hProcess, dwAddr);
#endif
}

int needsSymRefreshModuleList;
BOOL (WINAPI *hSymRefreshModuleList)(HANDLE);
JL_DLLEXPORT size_t rec_backtrace(intptr_t *data, size_t maxsize)
{
    CONTEXT Context;
    memset(&Context, 0, sizeof(Context));
    RtlCaptureContext(&Context);
    return rec_backtrace_ctx(data, maxsize, &Context);
}
size_t rec_backtrace_ctx(intptr_t *data, size_t maxsize,
                                      CONTEXT *Context)
{
    if (needsSymRefreshModuleList && hSymRefreshModuleList != 0 && !jl_in_stackwalk) {
        jl_in_stackwalk = 1;
        hSymRefreshModuleList(GetCurrentProcess());
        jl_in_stackwalk = 0;
        needsSymRefreshModuleList = 0;
    }
#if !defined(_CPU_X86_64_)
    if (jl_in_stackwalk) {
        return 0;
    }
    DWORD MachineType = IMAGE_FILE_MACHINE_I386;
    STACKFRAME64 stk;
    memset(&stk, 0, sizeof(stk));
    stk.AddrPC.Offset = Context->Eip;
    stk.AddrStack.Offset = Context->Esp;
    stk.AddrFrame.Offset = Context->Ebp;
    stk.AddrPC.Mode = AddrModeFlat;
    stk.AddrStack.Mode = AddrModeFlat;
    stk.AddrFrame.Mode = AddrModeFlat;
    jl_in_stackwalk = 1;
#endif

    size_t n = 0;
    while (n < maxsize) {
#ifndef _CPU_X86_64_
        data[n++] = (intptr_t)stk.AddrPC.Offset;
        BOOL result = StackWalk64(MachineType, GetCurrentProcess(), hMainThread,
            &stk, Context, NULL, JuliaFunctionTableAccess64, JuliaGetModuleBase64, NULL);
        if (!result)
            break;
#else
        data[n++] = (intptr_t)Context->Rip;
        DWORD64 ImageBase = JuliaGetModuleBase64(GetCurrentProcess(), Context->Rip);
        if (!ImageBase)
            break;

        MEMORY_BASIC_INFORMATION mInfo;

        PRUNTIME_FUNCTION FunctionEntry = (PRUNTIME_FUNCTION)JuliaFunctionTableAccess64(GetCurrentProcess(), Context->Rip);
        if (!FunctionEntry) { // assume this is a NO_FPO RBP-based function
            Context->Rsp = Context->Rbp;                 // MOV RSP, RBP

            // Check whether the pointer is valid and executable before dereferencing
            // to avoid segfault while recording. See #10638.
            if (VirtualQuery((LPCVOID)Context->Rsp, &mInfo, sizeof(MEMORY_BASIC_INFORMATION)) == 0)
                break;
            DWORD X = mInfo.AllocationProtect;
            if (!((X&PAGE_READONLY) || (X&PAGE_READWRITE) || (X&PAGE_WRITECOPY) || (X&PAGE_EXECUTE_READ)) ||
                  (X&PAGE_GUARD) || (X&PAGE_NOACCESS))
                break;

            Context->Rbp = *(DWORD64*)Context->Rsp;      // POP RBP
            Context->Rsp = Context->Rsp + sizeof(void*);
            Context->Rip = *(DWORD64*)Context->Rsp;      // POP RIP (aka RET)
            Context->Rsp = Context->Rsp + sizeof(void*);
        }
        else {
            PVOID HandlerData;
            DWORD64 EstablisherFrame;
            (void)RtlVirtualUnwind(
                    0 /*UNW_FLAG_NHANDLER*/,
                    ImageBase,
                    Context->Rip,
                    FunctionEntry,
                    Context,
                    &HandlerData,
                    &EstablisherFrame,
                    NULL);
        }
        if (!Context->Rip)
            break;
#endif
    }
#if !defined(_CPU_X86_64_)
    jl_in_stackwalk = 0;
#endif
    return n;
}
#else
// stacktrace using libunwind
JL_DLLEXPORT size_t rec_backtrace(intptr_t *data, size_t maxsize)
{
    unw_context_t uc;
    unw_getcontext(&uc);
    return rec_backtrace_ctx(data, maxsize, &uc);
}
size_t rec_backtrace_ctx(intptr_t *data, size_t maxsize,
                                      unw_context_t *uc)
{
#if !defined(_CPU_ARM_) && !defined(_CPU_PPC64_)
    volatile size_t n = 0;
    jl_jmp_buf *old_buf = jl_safe_restore;
    jl_jmp_buf buf;
    jl_safe_restore = &buf;
    if (!jl_setjmp(buf, 0)) {
        unw_cursor_t cursor;
        unw_init_local(&cursor, uc);
        do {
            unw_word_t ip;
            if (n >= maxsize)
                break;
            if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0)
                break;
            data[n++] = ip;
        } while (unw_step(&cursor) > 0);
    }
    else {
        // The unwinding fails likely because a invalid memory read.
        // Back off one frame since it is likely invalid.
        // This seems to be good enough on x86 to make the LLVM debug info
        // reader happy.
        n = n > 0 ? n - 1 : n;
    }
    jl_safe_restore = old_buf;
    return n;
#else
    return 0;
#endif
}
#ifdef LIBOSXUNWIND
size_t rec_backtrace_ctx_dwarf(intptr_t *data, size_t maxsize, unw_context_t *uc)
{
    unw_cursor_t cursor;
    unw_word_t ip;
    size_t n=0;

    unw_init_local_dwarf(&cursor, uc);
    do {
        if (n >= maxsize)
            break;
        if (unw_get_reg(&cursor, UNW_REG_IP, &ip) < 0)
            break;
        data[n++] = ip;
    } while (unw_step(&cursor) > 0);
    return n;
}
#endif
#endif

static jl_value_t *array_ptr_void_type = NULL;
JL_DLLEXPORT jl_value_t *jl_backtrace_from_here(void)
{
    jl_svec_t *tp = NULL;
    jl_array_t *bt = NULL;
    JL_GC_PUSH2(&tp, &bt);
    if (array_ptr_void_type == NULL) {
        tp = jl_svec2(jl_voidpointer_type, jl_box_long(1));
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type, tp);
    }
    bt = jl_alloc_array_1d(array_ptr_void_type, JL_MAX_BT_SIZE);
    size_t n = rec_backtrace((intptr_t*)jl_array_data(bt), JL_MAX_BT_SIZE);
    if (n < JL_MAX_BT_SIZE)
        jl_array_del_end(bt, JL_MAX_BT_SIZE-n);
    JL_GC_POP();
    return (jl_value_t*)bt;
}

JL_DLLEXPORT jl_value_t *jl_lookup_code_address(void *ip, int skipC)
{
    char *func_name;
    size_t line_num;
    char *file_name;
    size_t inlinedat_line;
    char *inlinedat_file;
    jl_lambda_info_t *outer_linfo;
    int fromC = frame_info_from_ip(&func_name, &file_name, &line_num,
                                   &inlinedat_file, &inlinedat_line, &outer_linfo,
                                   (size_t)ip, skipC, 0);
    jl_value_t *r = (jl_value_t*)jl_alloc_svec(8);
    JL_GC_PUSH1(&r);
    jl_svecset(r, 0, jl_symbol(func_name));
    jl_svecset(r, 1, jl_symbol(file_name));
    jl_svecset(r, 2, jl_box_long(line_num));
    jl_svecset(r, 3, jl_symbol(inlinedat_file ? inlinedat_file : ""));
    jl_svecset(r, 4, jl_box_long(inlinedat_file ? inlinedat_line : -1));
    jl_svecset(r, 5, outer_linfo != NULL ? (jl_value_t*)outer_linfo : jl_nothing);
    jl_svecset(r, 6, jl_box_bool(fromC));
    jl_svecset(r, 7, jl_box_long((intptr_t)ip));
    free(func_name);
    free(file_name);
    free(inlinedat_file);
    JL_GC_POP();
    return r;
}

JL_DLLEXPORT jl_value_t *jl_get_backtrace(void)
{
    jl_svec_t *tp = NULL;
    jl_array_t *bt = NULL;
    JL_GC_PUSH2(&tp, &bt);
    if (array_ptr_void_type == NULL) {
        tp = jl_svec2(jl_voidpointer_type, jl_box_long(1));
        array_ptr_void_type = jl_apply_type((jl_value_t*)jl_array_type, tp);
    }
    bt = jl_alloc_array_1d(array_ptr_void_type, jl_bt_size);
    memcpy(bt->data, jl_bt_data, jl_bt_size*sizeof(void*));
    JL_GC_POP();
    return (jl_value_t*)bt;
}

//for looking up functions from gdb:
JL_DLLEXPORT void jl_gdblookup(intptr_t ip)
{
    // This function is not allowed to reference any TLS variables since
    // it can be called from an unmanaged thread on OSX.
    char *func_name;
    size_t line_num;
    char *file_name;
    size_t inlinedat_line;
    char *inlinedat_file;
    jl_lambda_info_t *outer_linfo;
    frame_info_from_ip(&func_name, &file_name, &line_num,
            &inlinedat_file, &inlinedat_line, &outer_linfo, ip,
            /* skipC */ 0, /* skipInline */ 0);
    if (line_num == ip) {
        jl_safe_printf("unknown function (ip: %p)\n", (void*)ip);
    }
    else {
        if (line_num != -1) {
            jl_safe_printf("%s at %s:%" PRIuPTR "\n", inlinedat_file ? "[inline]" : func_name, file_name,
                           (uintptr_t)line_num);
        }
        else {
            jl_safe_printf("%s at %s (unknown line)\n", inlinedat_file ? "[inline]" : func_name, file_name);
        }
        if (inlinedat_file) {
            if (inlinedat_line != -1) {
                jl_safe_printf("%s at %s:%" PRIuPTR "\n", func_name, inlinedat_file,
                               (uintptr_t)inlinedat_line);
            }
            else {
                jl_safe_printf("%s at %s (unknown line)\n", func_name, inlinedat_file);
            }
        }
    }
    free(func_name);
    free(file_name);
    free(inlinedat_file);
}

JL_DLLEXPORT void jlbacktrace(void)
{
    size_t n = jl_bt_size; // jl_bt_size > 40 ? 40 : jl_bt_size;
    for (size_t i=0; i < n; i++)
        jl_gdblookup(jl_bt_data[i]);
}
