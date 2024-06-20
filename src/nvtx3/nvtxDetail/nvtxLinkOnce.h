/*
* Copyright 2009-2022  NVIDIA Corporation.  All rights reserved.
*
* Licensed under the Apache License v2.0 with LLVM Exceptions.
* See https://llvm.org/LICENSE.txt for license information.
* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*/

#ifndef __NVTX_LINKONCE_H__
#define __NVTX_LINKONCE_H__

/* This header defines macros to permit making definitions of global variables
 * and functions in C/C++ header files which may be included multiple times in
 * a translation unit or linkage unit.  It allows authoring header-only libraries
 * which can be used by multiple other header-only libraries (either as the same
 * copy or multiple copies), and does not require any build changes, such as
 * adding another .c file, linking a static library, or deploying a dynamic
 * library.  Globals defined with these macros have the property that they have
 * the same address, pointing to a single instance, for the entire linkage unit.
 * It is expected but not guaranteed that each linkage unit will have a separate
 * instance.
 *
 * In some situations it is desirable to declare a variable without initializing
 * it, refer to it in code or other variables' initializers, and then initialize
 * it later.  Similarly, functions can be prototyped, have their address taken,
 * and then have their body defined later.  In such cases, use the FWDDECL macros 
 * when forward-declaring LINKONCE global variables without initializers and
 * function prototypes, and then use the DEFINE macros when later defining them.
 * Although in many cases the FWDDECL macro is equivalent to the DEFINE macro,
 * following this pattern makes code maximally portable.
 */

#if defined(__MINGW32__) /* MinGW */
    #define NVTX_LINKONCE_WEAK __attribute__((section(".gnu.linkonce.0.")))
    #if defined(__cplusplus)
        #define NVTX_LINKONCE_DEFINE_GLOBAL   __declspec(selectany)
        #define NVTX_LINKONCE_DEFINE_FUNCTION extern "C" inline NVTX_LINKONCE_WEAK
    #else
        #define NVTX_LINKONCE_DEFINE_GLOBAL   __declspec(selectany)
        #define NVTX_LINKONCE_DEFINE_FUNCTION NVTX_LINKONCE_WEAK
    #endif
#elif defined(_MSC_VER) /* MSVC */
    #if defined(__cplusplus)
        #define NVTX_LINKONCE_DEFINE_GLOBAL   extern "C" __declspec(selectany)
        #define NVTX_LINKONCE_DEFINE_FUNCTION extern "C" inline
    #else
        #define NVTX_LINKONCE_DEFINE_GLOBAL   __declspec(selectany)
        #define NVTX_LINKONCE_DEFINE_FUNCTION __inline
    #endif
#elif defined(__CYGWIN__) && defined(__clang__) /* Clang on Cygwin */
    #define NVTX_LINKONCE_WEAK __attribute__((section(".gnu.linkonce.0.")))
    #if defined(__cplusplus)
        #define NVTX_LINKONCE_DEFINE_GLOBAL   NVTX_LINKONCE_WEAK
        #define NVTX_LINKONCE_DEFINE_FUNCTION extern "C" NVTX_LINKONCE_WEAK
    #else
        #define NVTX_LINKONCE_DEFINE_GLOBAL   NVTX_LINKONCE_WEAK
        #define NVTX_LINKONCE_DEFINE_FUNCTION NVTX_LINKONCE_WEAK
    #endif
#elif defined(__CYGWIN__) /* Assume GCC or compatible */
    #define NVTX_LINKONCE_WEAK __attribute__((weak))
    #if defined(__cplusplus)
        #define NVTX_LINKONCE_DEFINE_GLOBAL   __declspec(selectany)
        #define NVTX_LINKONCE_DEFINE_FUNCTION extern "C" inline
    #else
        #define NVTX_LINKONCE_DEFINE_GLOBAL   NVTX_LINKONCE_WEAK
        #define NVTX_LINKONCE_DEFINE_FUNCTION NVTX_LINKONCE_WEAK
    #endif
#else /* All others: Assume GCC, clang, or compatible */
    #define NVTX_LINKONCE_WEAK   __attribute__((weak))
    #define NVTX_LINKONCE_HIDDEN __attribute__((visibility("hidden")))
    #if defined(__cplusplus)
        #define NVTX_LINKONCE_DEFINE_GLOBAL   NVTX_LINKONCE_HIDDEN NVTX_LINKONCE_WEAK
        #define NVTX_LINKONCE_DEFINE_FUNCTION extern "C" NVTX_LINKONCE_HIDDEN inline
    #else
        #define NVTX_LINKONCE_DEFINE_GLOBAL   NVTX_LINKONCE_HIDDEN NVTX_LINKONCE_WEAK
        #define NVTX_LINKONCE_DEFINE_FUNCTION NVTX_LINKONCE_HIDDEN NVTX_LINKONCE_WEAK
    #endif
#endif

#define NVTX_LINKONCE_FWDDECL_GLOBAL   NVTX_LINKONCE_DEFINE_GLOBAL   extern
#define NVTX_LINKONCE_FWDDECL_FUNCTION NVTX_LINKONCE_DEFINE_FUNCTION

#endif /* __NVTX_LINKONCE_H__ */
