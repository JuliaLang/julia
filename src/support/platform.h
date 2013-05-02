#ifndef PLATFORM_H
#define PLATFORM_H

/*
 * This file provides convenient macros to be used to identify the platform
 * based of compiler-specific pre-defined macros. It is based on the
 * information that can be found at the following address:
 *
 *     http://sourceforge.net/p/predef/wiki/Home/
 *
 * Possible values include:
 *      Compiler:
 *          _COMPILER_CLANG_
 *          _COMPILER_GCC_
 *          _COMPILER_INTEL_
 *          _COMPILER_MICROSOFT_
 *          _COMPILER_MINGW_
 *      OS:
 *          _OS_FREEBSD_
 *          _OS_LINUX_
 *          _OS_WINDOWS_
 *          _OS_DARWIN_
 *
 *      CPU/Architecture:
 *          _CPU_X86_
 *          _CPU_X86_64_
 *          _CPU_ARM_
 */

/*******************************************************************************
*                               Compiler                                       *
*******************************************************************************/

/*
 * Notes:
 *
 *  1. Checking for Intel's compiler should be done before checking for
 * Microsoft's. On Windows Intel's compiler also defines _MSC_VER as the
 * acknoledgement of the fact that it is integrated with Visual Studio.
 *
 *  2. Checking for MinGW should be done before checking for GCC as MinGW
 * pretends to be GCC.
 */
#if defined(__clang__)
#define _COMPILER_CLANG_
#elif defined(__INTEL_COMPILER) || defined(__ICC)
#define _COMPILER_INTEL_
#elif defined(__MINGW32__)
#define _COMPILER_MINGW_
#elif defined(_MSC_VER)
#define _COMPILER_MICROSOFT_
#elif defined(__GNUC__)
#define _COMPILER_GCC_
#endif

/*******************************************************************************
*                               OS                                             *
*******************************************************************************/

#if defined(__FreeBSD__)
#define _OS_FREEBSD__
#elif defined(__linux__)
#define _OS_LINUX_
#elif defined(_WIN32) || defined(_WIN64)
#define _OS_WINDOWS_
#elif defined(__APPLE__) && defined(__MACH__)
#define _OS_DARWIN_
#endif

/*******************************************************************************
*                               Architecture                                   *
*******************************************************************************/

#if defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64) || defined(_M_X64) || defined(_M_AMD64)
#define _CPU_X86_64_
#elif defined(i386) || defined(__i386) || defined(__i386__) || defined(_M_IX86) || defined(_X86_)
#define _CPU_X86_
#elif defined(__arm__) || defined(_M_ARM)
#define _CPU_ARM_
#endif

#endif /* !PLATFORM_H */

