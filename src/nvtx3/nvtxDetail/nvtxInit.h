/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#ifndef NVTX_IMPL_GUARD
#error Never include this file directly -- it is automatically included by nvToolsExt.h (except when NVTX_NO_IMPL is defined).
#endif

/* ---- Platform-independent helper definitions and functions ---- */

/* Prefer macros over inline functions to reduce symbol resolution at link time */

#if defined(_WIN32) 
#define NVTX_PATHCHAR   wchar_t
#define NVTX_STR(x)     L##x
#define NVTX_GETENV     _wgetenv
#define NVTX_BUFSIZE    MAX_PATH
#define NVTX_DLLHANDLE  HMODULE
#define NVTX_DLLOPEN(x) LoadLibraryW(x)
#define NVTX_DLLFUNC    GetProcAddress
#define NVTX_DLLCLOSE   FreeLibrary
#define NVTX_YIELD()    SwitchToThread()
#define NVTX_MEMBAR()   MemoryBarrier()
#define NVTX_ATOMIC_WRITE_32(address, value)                        InterlockedExchange((volatile LONG*)address, value)
#define NVTX_ATOMIC_CAS_32(old, address, exchange, comparand) old = InterlockedCompareExchange((volatile LONG*)address, exchange, comparand)
#elif defined(__GNUC__)
#define NVTX_PATHCHAR   char
#define NVTX_STR(x)     x
#define NVTX_GETENV     getenv
#define NVTX_BUFSIZE    PATH_MAX
#define NVTX_DLLHANDLE  void*
#define NVTX_DLLOPEN(x) dlopen(x, RTLD_LAZY)
#define NVTX_DLLFUNC    dlsym
#define NVTX_DLLCLOSE   dlclose
#define NVTX_YIELD()    sched_yield()
#define NVTX_MEMBAR()   __sync_synchronize()
/* Ensure full memory barrier for atomics, to match Windows functions */
#define NVTX_ATOMIC_WRITE_32(address, value)                  __sync_synchronize();       __sync_lock_test_and_set(address, value)
#define NVTX_ATOMIC_CAS_32(old, address, exchange, comparand) __sync_synchronize(); old = __sync_val_compare_and_swap(address, exchange, comparand)
#else
#error The library does not support your configuration!
#endif

/* Define this to 1 for platforms that where pre-injected libraries can be discovered. */
#if defined(_WIN32)
/* TODO */
#define NVTX_SUPPORT_ALREADY_INJECTED_LIBRARY 0
#else
#define NVTX_SUPPORT_ALREADY_INJECTED_LIBRARY 0
#endif

/* Define this to 1 for platforms that support environment variables */
/* TODO: Detect UWP, a.k.a. Windows Store app, and set this to 0. */
/* Try:  #if defined(WINAPI_FAMILY_PARTITION) && WINAPI_FAMILY_PARTITION(WINAPI_PARTITION_APP) */
#define NVTX_SUPPORT_ENV_VARS 1

/* Define this to 1 for platforms that support dynamic/shared libraries */
#define NVTX_SUPPORT_DYNAMIC_INJECTION_LIBRARY 1

/* Injection libraries implementing InitializeInjectionNvtx2 may be statically linked,
*  and this will override any dynamic injection.  Useful for platforms where dynamic
*  injection is not available.  Since weak symbols not explicitly marked extern are
*  guaranteed to be initialized to zero if no definitions are found by the linker, the
*  dynamic injection process proceeds normally if pfnInitializeInjectionNvtx2 is 0. */
#if defined(__GNUC__) && !defined(_WIN32) && !defined(__CYGWIN__)
#define NVTX_SUPPORT_STATIC_INJECTION_LIBRARY 1
/* To statically inject an NVTX library, define InitializeInjectionNvtx2_fnptr as a normal
*  symbol (not weak) pointing to the implementation of InitializeInjectionNvtx2 (which
*  does not need to be named "InitializeInjectionNvtx2" as is necessary in a dynamic
*  injection library. */
__attribute__((weak)) NvtxInitializeInjectionNvtxFunc_t InitializeInjectionNvtx2_fnptr;
#else
#define NVTX_SUPPORT_STATIC_INJECTION_LIBRARY 0
#endif

/* This function tries to find or load an NVTX injection library and get the
*  address of its InitializeInjection2 function.  If such a function pointer
*  is found, it is called, and passed the address of this NVTX instance's
*  nvtxGetExportTable function, so the injection can attach to this instance.
*  If the initialization fails for any reason, any dynamic library loaded will
*  be freed, and all NVTX implementation functions will be set to no-ops.  If
*  initialization succeeds, NVTX functions not attached to the tool will be set
*  to no-ops.  This is implemented as one function instead of several small
*  functions to minimize the number of weak symbols the linker must resolve.
*  Order of search is:
*  - Pre-injected library exporting InitializeInjectionNvtx2
*  - Loadable library exporting InitializeInjectionNvtx2
*      - Path specified by env var NVTX_INJECTION??_PATH (?? is 32 or 64)
*      - On Android, libNvtxInjection??.so within the package (?? is 32 or 64)
*  - Statically-linked injection library defining InitializeInjectionNvtx2_fnptr
*/
NVTX_LINKONCE_FWDDECL_FUNCTION int NVTX_VERSIONED_IDENTIFIER(nvtxInitializeInjectionLibrary)(void);
NVTX_LINKONCE_DEFINE_FUNCTION int NVTX_VERSIONED_IDENTIFIER(nvtxInitializeInjectionLibrary)(void)
{
    const char* const initFuncName = "InitializeInjectionNvtx2";
    NvtxInitializeInjectionNvtxFunc_t init_fnptr = (NvtxInitializeInjectionNvtxFunc_t)0;
    NVTX_DLLHANDLE injectionLibraryHandle = (NVTX_DLLHANDLE)0;
    int entryPointStatus = 0;

#if NVTX_SUPPORT_ALREADY_INJECTED_LIBRARY
    /* Use POSIX global symbol chain to query for init function from any module */
    init_fnptr = (NvtxInitializeInjectionNvtxFunc_t)NVTX_DLLFUNC(0, initFuncName);
#endif

#if NVTX_SUPPORT_DYNAMIC_INJECTION_LIBRARY
    /* Try discovering dynamic injection library to load */
    if (!init_fnptr)
    {
#if NVTX_SUPPORT_ENV_VARS
        /* If env var NVTX_INJECTION64_PATH is set, it should contain the path
        *  to a 64-bit dynamic NVTX injection library (and similar for 32-bit). */
        const NVTX_PATHCHAR* const nvtxEnvVarName = (sizeof(void*) == 4)
            ? NVTX_STR("NVTX_INJECTION32_PATH")
            : NVTX_STR("NVTX_INJECTION64_PATH");
#endif /* NVTX_SUPPORT_ENV_VARS */
        NVTX_PATHCHAR injectionLibraryPathBuf[NVTX_BUFSIZE];
        const NVTX_PATHCHAR* injectionLibraryPath = (const NVTX_PATHCHAR*)0;

        /* Refer to this variable explicitly in case all references to it are #if'ed out */
        (void)injectionLibraryPathBuf;

#if NVTX_SUPPORT_ENV_VARS
        /* Disable the warning for getenv & _wgetenv -- this usage is safe because
        *  these functions are not called again before using the returned value. */
#if defined(_MSC_VER)
#pragma warning( push )
#pragma warning( disable : 4996 )
#endif
        injectionLibraryPath = NVTX_GETENV(nvtxEnvVarName);
#if defined(_MSC_VER)
#pragma warning( pop )
#endif
#endif

#if defined(__ANDROID__)
        if (!injectionLibraryPath)
        {
            const char *bits = (sizeof(void*) == 4) ? "32" : "64";
            char cmdlineBuf[32];
            char pkgName[PATH_MAX];
            int count;
            int pid;
            FILE *fp;
            size_t bytesRead;
            size_t pos;

            pid = (int)getpid();
            count = snprintf(cmdlineBuf, sizeof(cmdlineBuf), "/proc/%d/cmdline", pid);
            if (count <= 0 || count >= (int)sizeof(cmdlineBuf))
            {
                NVTX_ERR("Path buffer too small for: /proc/%d/cmdline\n", pid);
                return NVTX_ERR_INIT_ACCESS_LIBRARY;
            }

            fp = fopen(cmdlineBuf, "r");
            if (!fp)
            {
                NVTX_ERR("File couldn't be opened: %s\n", cmdlineBuf);
                return NVTX_ERR_INIT_ACCESS_LIBRARY;
            }

            bytesRead = fread(pkgName, 1, sizeof(pkgName) - 1, fp);
            fclose(fp);
            if (bytesRead == 0)
            {
                NVTX_ERR("Package name couldn't be read from file: %s\n", cmdlineBuf);
                return NVTX_ERR_INIT_ACCESS_LIBRARY;
            }

            pkgName[bytesRead] = 0;

            /* String can contain colon as a process separator. In this case the package name is before the colon. */
            pos = 0;
            while (pos < bytesRead && pkgName[pos] != ':' && pkgName[pos] != '\0')
            {
                ++pos;
            }
            pkgName[pos] = 0;

            count = snprintf(injectionLibraryPathBuf, NVTX_BUFSIZE, "/data/data/%s/files/libNvtxInjection%s.so", pkgName, bits);
            if (count <= 0 || count >= NVTX_BUFSIZE)
            {
                NVTX_ERR("Path buffer too small for: /data/data/%s/files/libNvtxInjection%s.so\n", pkgName, bits);
                return NVTX_ERR_INIT_ACCESS_LIBRARY;
            }

            /* On Android, verify path is accessible due to aggressive file access restrictions. */
            /* For dlopen, if the filename contains a leading slash, then it is interpreted as a */
            /* relative or absolute pathname; otherwise it will follow the rules in ld.so. */
            if (injectionLibraryPathBuf[0] == '/')
            {
#if (__ANDROID_API__ < 21)
                int access_err = access(injectionLibraryPathBuf, F_OK | R_OK);
#else
                int access_err = faccessat(AT_FDCWD, injectionLibraryPathBuf, F_OK | R_OK, 0);
#endif
                if (access_err != 0)
                {
                    NVTX_ERR("Injection library path wasn't accessible [code=%s] [path=%s]\n", strerror(errno), injectionLibraryPathBuf);
                    return NVTX_ERR_INIT_ACCESS_LIBRARY;
                }
            }
            injectionLibraryPath = injectionLibraryPathBuf;
        }
#endif

        /* At this point, injectionLibraryPath is specified if a dynamic
        *  injection library was specified by a tool. */
        if (injectionLibraryPath)
        {
            /* Load the injection library */
            injectionLibraryHandle = NVTX_DLLOPEN(injectionLibraryPath);
            if (!injectionLibraryHandle)
            {
                NVTX_ERR("Failed to load injection library\n");
                return NVTX_ERR_INIT_LOAD_LIBRARY;
            }
            else
            {
                /* Attempt to get the injection library's entry-point */
                init_fnptr = (NvtxInitializeInjectionNvtxFunc_t)NVTX_DLLFUNC(injectionLibraryHandle, initFuncName);
                if (!init_fnptr)
                {
                    NVTX_DLLCLOSE(injectionLibraryHandle);
                    NVTX_ERR("Failed to get address of function InitializeInjectionNvtx2 from injection library\n");
                    return NVTX_ERR_INIT_MISSING_LIBRARY_ENTRY_POINT;
                }
            }
        }
    }
#endif

#if NVTX_SUPPORT_STATIC_INJECTION_LIBRARY
    if (!init_fnptr)
    {
        /* Check weakly-defined function pointer.  A statically-linked injection can define this as
        *  a normal symbol and it will take precedence over a dynamic injection. */
        if (InitializeInjectionNvtx2_fnptr)
        {
            init_fnptr = InitializeInjectionNvtx2_fnptr;
        }
    }
#endif

    /* At this point, if init_fnptr is not set, then no tool has specified
    *  an NVTX injection library -- return non-success result so all NVTX
    *  API functions will be set to no-ops. */
    if (!init_fnptr)
    {
        return NVTX_ERR_NO_INJECTION_LIBRARY_AVAILABLE;
    }

    /* Invoke injection library's initialization function.  If it returns
    *  0 (failure) and a dynamic injection was loaded, unload it. */
    entryPointStatus = init_fnptr(NVTX_VERSIONED_IDENTIFIER(nvtxGetExportTable));
    if (entryPointStatus == 0)
    {
        NVTX_ERR("Failed to initialize injection library -- initialization function returned 0\n");
        if (injectionLibraryHandle)
        {
            NVTX_DLLCLOSE(injectionLibraryHandle);
        }
        return NVTX_ERR_INIT_FAILED_LIBRARY_ENTRY_POINT;
    }

    return NVTX_SUCCESS;
}

NVTX_LINKONCE_DEFINE_FUNCTION void NVTX_VERSIONED_IDENTIFIER(nvtxInitOnce)(void)
{
    unsigned int old;
    if (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).initState == NVTX_INIT_STATE_COMPLETE)
    {
        return;
    }

    NVTX_ATOMIC_CAS_32(
        old,
        &NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).initState,
        NVTX_INIT_STATE_STARTED,
        NVTX_INIT_STATE_FRESH);
    if (old == NVTX_INIT_STATE_FRESH)
    {
        int result;
        int forceAllToNoops;

        /* Load & initialize injection library -- it will assign the function pointers */
        result = NVTX_VERSIONED_IDENTIFIER(nvtxInitializeInjectionLibrary)();

        /* Set all pointers not assigned by the injection to null */
        forceAllToNoops = result != NVTX_SUCCESS; /* Set all to null if injection init failed */
        NVTX_VERSIONED_IDENTIFIER(nvtxSetInitFunctionsToNoops)(forceAllToNoops);

        /* Signal that initialization has finished, so now the assigned function pointers will be used */
        NVTX_ATOMIC_WRITE_32(
            &NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).initState,
            NVTX_INIT_STATE_COMPLETE);
    }
    else /* Spin-wait until initialization has finished */
    {
        NVTX_MEMBAR();
        while (NVTX_VERSIONED_IDENTIFIER(nvtxGlobals).initState != NVTX_INIT_STATE_COMPLETE)
        {
            NVTX_YIELD();
            NVTX_MEMBAR();
        }
    }
}
