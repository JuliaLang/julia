/* crc32c.c -- compute CRC-32C using software table or available hardware instructions
 * Copyright (C) 2013 Mark Adler
 * Version 1.1  1 Aug 2013  Mark Adler
 *
 * Code retrieved in August 2016 from August 2013 post by Mark Adler on
 *    http://stackoverflow.com/questions/17645167/implementing-sse-4-2s-crc32c-in-software
 * Modified for use in libjulia:
 *    - exported function renamed to jl_crc32c, DLL exports added.
 *    - removed main() function
 *    - architecture and compiler detection
 *    - precompute crc32c tables and store in a generated .c file
 *    - ARMv8 support
 */

/*
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Mark Adler
  madler@alumni.caltech.edu
*/

/* This computes a CRC-32C, *not* the CRC-32 used by Ethernet and zip, gzip, etc.
 * A software version is provided as a fall-back, as well as for speed comparisons. */

/* Version history:
   1.0  10 Feb 2013  First version
   1.1   1 Aug 2013  Correct comments on why three crc instructions in parallel
*/

#include "julia.h"
#include "julia_internal.h"
#include "processor.h"

#ifdef _CPU_AARCH64_
#  include <sys/auxv.h>
#endif

/* CRC-32C (iSCSI) polynomial in reversed bit order. */
#define POLY 0x82f63b78

/* Block sizes for three-way parallel crc computation.  LONG and SHORT must
   both be powers of two.  The associated string constants must be set
   accordingly, for use in constructing the assembler instructions. */
#define LONG 8192
#define LONGx1 "8192"
#define LONGx2 "16384"
#define SHORT 256
#define SHORTx1 "256"
#define SHORTx2 "512"

#ifndef GEN_CRC32C_TABLES
#include "crc32c-tables.c"

#if JL_USE_IFUNC
// Archs that can't use ifunc to do feature detection (e.g. ARM) should undef this below.
#  define JL_CRC32C_USE_IFUNC
#endif

JL_DLLEXPORT uint32_t jl_crc32c_sw(uint32_t crci, const char *buf, size_t len);
typedef uint32_t (*crc32c_func_t)(uint32_t crc, const char *buf, size_t len);

/* Apply the zeros operator table to crc. */
JL_UNUSED static inline uint32_t crc32c_shift(const uint32_t zeros[][256], uint32_t crc)
{
    return zeros[0][crc & 0xff] ^ zeros[1][(crc >> 8) & 0xff] ^
        zeros[2][(crc >> 16) & 0xff] ^ zeros[3][crc >> 24];
}

#if (defined(_CPU_X86_64_) || defined(_CPU_X86_)) && !defined(_COMPILER_MICROSOFT_)
#  ifdef _CPU_X86_64_
#    define CRC32_PTR "crc32q"
#  else
#    define CRC32_PTR "crc32l"
#  endif

/* Compute CRC-32C using the SSE4.2 hardware instruction. */
static uint32_t crc32c_sse42(uint32_t crc, const char *buf, size_t len)
{
    /* need to be 64 bits for crc32q */
    /* pre-process the crc */
    uintptr_t crc0 = crc ^ 0xffffffff;

    /* compute the crc for up to seven leading bytes to bring the data pointer
       to an eight-byte boundary */
    while (len && ((uintptr_t)buf & 7) != 0) {
        __asm__("crc32b\t" "(%1), %0"
                : "=r"(crc0)
                : "r"(buf), "0"(crc0));
        buf++;
        len--;
    }

    /* compute the crc on sets of LONG*3 bytes, executing three independent crc
       instructions, each on LONG bytes -- this is optimized for the Nehalem,
       Westmere, Sandy Bridge, and Ivy Bridge architectures, which have a
       throughput of one crc per cycle, but a latency of three cycles */
    while (len >= LONG * 3) {
        uintptr_t crc1 = 0;
        uintptr_t crc2 = 0;
        const char *end = buf + LONG;
        do {
            __asm__(CRC32_PTR "\t" "(%3), %0\n\t"
                    CRC32_PTR "\t" LONGx1 "(%3), %1\n\t"
                    CRC32_PTR "\t" LONGx2 "(%3), %2"
                    : "=r"(crc0), "=r"(crc1), "=r"(crc2)
                    : "r"(buf), "0"(crc0), "1"(crc1), "2"(crc2));
            buf += sizeof(void*);
        } while (buf < end);
        crc0 = crc32c_shift(crc32c_long, crc0) ^ crc1;
        crc0 = crc32c_shift(crc32c_long, crc0) ^ crc2;
        buf += LONG * 2;
        len -= LONG * 3;
    }

    /* do the same thing, but now on SHORT*3 blocks for the remaining data less
       than a LONG*3 block */
    while (len >= SHORT * 3) {
        uintptr_t crc1 = 0;
        uintptr_t crc2 = 0;
        const char *end = buf + SHORT;
        do {
            __asm__(CRC32_PTR "\t" "(%3), %0\n\t"
                    CRC32_PTR "\t" SHORTx1 "(%3), %1\n\t"
                    CRC32_PTR "\t" SHORTx2 "(%3), %2"
                    : "=r"(crc0), "=r"(crc1), "=r"(crc2)
                    : "r"(buf), "0"(crc0), "1"(crc1), "2"(crc2));
            buf += sizeof(void*);
        } while (buf < end);
        crc0 = crc32c_shift(crc32c_short, crc0) ^ crc1;
        crc0 = crc32c_shift(crc32c_short, crc0) ^ crc2;
        buf += SHORT * 2;
        len -= SHORT * 3;
    }

    /* compute the crc on the remaining eight-byte units less than a SHORT*3
       block */
    const char *end = buf + (len - (len & 7));
    while (buf < end) {
        __asm__(CRC32_PTR "\t" "(%1), %0"
                : "=r"(crc0)
                : "r"(buf), "0"(crc0));
        buf += sizeof(void*);
    }
    len &= 7;

    /* compute the crc for up to seven trailing bytes */
    while (len) {
        __asm__("crc32b\t" "(%1), %0"
                : "=r"(crc0)
                : "r"(buf), "0"(crc0));
        buf++;
        len--;
    }

    /* return a post-processed crc */
    return (uint32_t)crc0 ^ 0xffffffff;
}

// HW feature detection
#  ifdef __SSE4_2__
// The C code is compiled with SSE42 being required. Skip runtime dispatch.
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len)
{
    return crc32c_sse42(crc, buf, len);
}
#  else
static crc32c_func_t crc32c_dispatch(void)
{
    // When used in ifunc, we cannot call external functions (i.e. jl_cpuid)
    uint32_t eax = 1, ebx, ecx, edx;
    asm (
#if defined(__i386__) && defined(__PIC__)
        "xchg %%ebx, %%esi;"
        "cpuid;"
        "xchg %%esi, %%ebx;":
        "=S" (ebx) ,
#else
        "cpuid":
        "=b" (ebx),
#endif
        "+a" (eax),
        "=c" (ecx),
        "=d" (edx));
    if ((ecx >> 20) & 1)
        return crc32c_sse42;
    return jl_crc32c_sw;
}
// For ifdef detection below
#    define crc32c_dispatch crc32c_dispatch
#    define crc32c_dispatch_ifunc "crc32c_dispatch"
#  endif
#elif defined(_CPU_AARCH64_) || defined(_CPU_ARM_)
/* Compute CRC-32C using the ARMv8 CRC32 extension. */
#  ifdef _CPU_AARCH64_
#  define CRC_TARGET __attribute__((target("+crc")))
CRC_TARGET static inline uint32_t crc32cx(uint32_t crc, uint64_t val)
{
    uint32_t res;
    asm("crc32cx %w0, %w1, %2" : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
CRC_TARGET static inline uint32_t crc32cw(uint32_t crc, uint32_t val)
{
    uint32_t res;
    asm("crc32cw %w0, %w1, %w2" : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
CRC_TARGET static inline uint32_t crc32ch(uint32_t crc, uint32_t val)
{
    uint32_t res;
    asm("crc32ch %w0, %w1, %w2" : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
CRC_TARGET static inline uint32_t crc32cb(uint32_t crc, uint32_t val)
{
    uint32_t res;
    asm("crc32cb %w0, %w1, %w2" : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
#    define crc32c_ptr crc32cx
#    define load_unaligned_intptr jl_load_unaligned_i64
#  else
/* #    ifdef _COMPILER_CLANG_ */
/* #      define CRC_TARGET __attribute__((target("armv8-a,crc"))) */
/* #    else */
/* #      define CRC_TARGET __attribute__((target("armv8-a+crc"))) */
/* #    endif */
// Workaround GCC bug https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82641
// Clang is slightly better and can generate the correct binary but wrong textual assembly.
#    define CRC_TARGET
asm("\t.set jl_arm_reg_r0, 0\n"
    "\t.set jl_arm_reg_r1, 1\n"
    "\t.set jl_arm_reg_r2, 2\n"
    "\t.set jl_arm_reg_r3, 3\n"
    "\t.set jl_arm_reg_r4, 4\n"
    "\t.set jl_arm_reg_r5, 5\n"
    "\t.set jl_arm_reg_r6, 6\n"
    "\t.set jl_arm_reg_r7, 7\n"
    "\t.set jl_arm_reg_r8, 8\n"
    "\t.set jl_arm_reg_r9, 9\n"
    "\t.set jl_arm_reg_r10, 10\n"
    "\t.set jl_arm_reg_sl, 10\n"
    "\t.set jl_arm_reg_r11, 11\n"
    "\t.set jl_arm_reg_fp, 11\n"
    "\t.set jl_arm_reg_r12, 12\n"
    "\t.set jl_arm_reg_ip, 12\n"
    "\t.set jl_arm_reg_r13, 13\n"
    "\t.set jl_arm_reg_sp, 13\n"
    "\t.set jl_arm_reg_r14, 14\n"
    "\t.set jl_arm_reg_lr, 14\n"
    "\t.set jl_arm_reg_r15, 15\n"
    "\t.set jl_arm_reg_pc, 15\n");
CRC_TARGET static inline uint32_t crc32cw(uint32_t crc, uint32_t val)
{
    uint32_t res;
    // asm("crc32cw %0, %1, %2" : "=r"(res) : "r"(crc), "r"(val));
    asm(".inst 0xe1400240 | (jl_arm_reg_%0 << 16) | (jl_arm_reg_%1 << 12) | (jl_arm_reg_%2)"
        : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
CRC_TARGET static inline uint32_t crc32ch(uint32_t crc, uint32_t val)
{
    uint32_t res;
    // asm("crc32ch %0, %1, %2" : "=r"(res) : "r"(crc), "r"(val));
    asm(".inst 0xe1200240 | (jl_arm_reg_%0 << 16) | (jl_arm_reg_%1 << 12) | (jl_arm_reg_%2)"
        : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
CRC_TARGET static inline uint32_t crc32cb(uint32_t crc, uint32_t val)
{
    uint32_t res;
    // asm("crc32cb %0, %1, %2" : "=r"(res) : "r"(crc), "r"(val));
    asm(".inst 0xe1000240 | (jl_arm_reg_%0 << 16) | (jl_arm_reg_%1 << 12) | (jl_arm_reg_%2)"
        : "=r"(res) : "r"(crc), "r"(val));
    return res;
}
#    define crc32c_ptr crc32cw
#    define load_unaligned_intptr jl_load_unaligned_i32
#endif

// Modified from the SSE4.2 version.
CRC_TARGET static uint32_t crc32c_armv8(uint32_t crc, const char *buf, size_t len)
{
    /* pre-process the crc */
    crc = ~crc;

    // Misaligned access doesn't seem to have any measurable performance overhead
    // on Cortex-A57

    // crc32c has a latency of 3 and throughput of 1 on Cortex-A57
    // The latency and throughput are 2 and 1 on Cortex-A72
    // In either case, the 3 wide parallel processing shouldn't hurt since the block size
    // should be big enough.
    /* compute the crc on sets of LONG*3 bytes, executing three independent crc
     * instructions, each on LONG bytes. */
    while (len >= LONG * 3) {
        uint32_t crc1 = 0;
        uint32_t crc2 = 0;
        const char *end = buf + LONG;
        const char *buf2 = end;
        const char *buf3 = end + LONG;
        do {
            crc = crc32c_ptr(crc, load_unaligned_intptr(buf));
            buf += sizeof(void*);
            crc1 = crc32c_ptr(crc1, load_unaligned_intptr(buf2));
            buf2 += sizeof(void*);
            crc2 = crc32c_ptr(crc2, load_unaligned_intptr(buf3));
            buf3 += sizeof(void*);
        } while (buf < end);
        crc = crc32c_shift(crc32c_long, crc) ^ crc1;
        crc = crc32c_shift(crc32c_long, crc) ^ crc2;
        buf += LONG * 2;
        len -= LONG * 3;
    }

    /* do the same thing, but now on SHORT*3 blocks for the remaining data less
     * than a LONG*3 block */
    while (len >= SHORT * 3) {
        uint32_t crc1 = 0;
        uint32_t crc2 = 0;
        const char *end = buf + SHORT;
        const char *buf2 = end;
        const char *buf3 = end + SHORT;
        do {
            crc = crc32c_ptr(crc, load_unaligned_intptr(buf));
            buf += sizeof(void*);
            crc1 = crc32c_ptr(crc1, load_unaligned_intptr(buf2));
            buf2 += sizeof(void*);
            crc2 = crc32c_ptr(crc2, load_unaligned_intptr(buf3));
            buf3 += sizeof(void*);
        } while (buf < end);
        crc = crc32c_shift(crc32c_short, crc) ^ crc1;
        crc = crc32c_shift(crc32c_short, crc) ^ crc2;
        buf += SHORT * 2;
        len -= SHORT * 3;
    }
    // The same shift table can be used to compute two SHORT blocks simultaneously
    if (len >= SHORT * 2) {
        uint32_t crc1 = 0;
        const char *end = buf + SHORT;
        const char *buf2 = end;
        do {
            crc = crc32c_ptr(crc, load_unaligned_intptr(buf));
            buf += sizeof(void*);
            crc1 = crc32c_ptr(crc1, load_unaligned_intptr(buf2));
            buf2 += sizeof(void*);
        } while (buf < end);
        crc = crc32c_shift(crc32c_short, crc) ^ crc1;
        buf += SHORT;
        len -= SHORT * 2;
    }

    /* compute the crc on the remaining eight-byte units less than a SHORT*2
       block */
    const char *end = buf + len - sizeof(void*);
    while (buf <= end) {
        crc = crc32c_ptr(crc, load_unaligned_intptr(buf));
        buf += sizeof(void*);
    }
    if (sizeof(void*) == 8 && len & 4) {
        crc = crc32cw(crc, jl_load_unaligned_i32(buf));
        buf += 4;
    }
    if (len & 2) {
        crc = crc32ch(crc, jl_load_unaligned_i16(buf));
        buf += 2;
    }
    if (len & 1)
        crc = crc32cb(crc, *buf);
    /* return a post-processed crc */
    return ~crc;
}

// HW feature detection
#  ifdef __ARM_FEATURE_CRC32
// The C code is compiled with CRC32 being required. Skip runtime dispatch.
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len)
{
    return crc32c_armv8(crc, buf, len);
}
#  elif defined(_CPU_AARCH64_)
static crc32c_func_t crc32c_dispatch(unsigned long hwcap)
{
    if (hwcap & (1 << JL_AArch64_crc))
        return crc32c_armv8;
    return jl_crc32c_sw;
}
// For ifdef detection below
#    define crc32c_dispatch() crc32c_dispatch(getauxval(AT_HWCAP))
#    define crc32c_dispatch_ifunc "crc32c_dispatch"
#  else
static crc32c_func_t crc32c_dispatch(void)
{
    if (jl_test_cpu_feature(JL_AArch32_crc))
        return crc32c_armv8;
    return jl_crc32c_sw;
}
// For ifdef detection below
#    define crc32c_dispatch crc32c_dispatch
// It's not really supported currently to access HWCAP2 in an ifunc.
// Since the CRC32 bit is in HWCAP2 on ARM, we can't use ifunc.
#    ifdef JL_CRC32C_USE_IFUNC
#      undef JL_CRC32C_USE_IFUNC
#    endif
#  endif
#else
// If we don't have any accelerated version to define, just make the _sw version define
// the real version and then define a _sw version as test wrapper.
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len);
JL_DLLEXPORT uint32_t jl_crc32c_sw(uint32_t crc, const char *buf, size_t len)
{
    return jl_crc32c(crc, buf, len);
}
#define jl_crc32c_sw jl_crc32c
#endif

#ifdef crc32c_dispatch
#  ifdef JL_CRC32C_USE_IFUNC
// ifunc dispatch
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len)
    __attribute__((ifunc (crc32c_dispatch_ifunc)));
#  else
// lazy wrapper dispatch
static uint32_t crc32c_lazy(uint32_t crc, const char *buf, size_t len);
static crc32c_func_t crc32c_func = crc32c_lazy;

static uint32_t crc32c_lazy(uint32_t crc, const char *buf, size_t len)
{
    crc32c_func = crc32c_dispatch();
    return crc32c_func(crc, buf, len);
}

/* Compute a CRC-32C. Do a lazy dispatch based on hardware features */
JL_DLLEXPORT uint32_t jl_crc32c(uint32_t crc, const char *buf, size_t len)
{
    return crc32c_func(crc, buf, len);
}
#  endif
#endif

/* Table-driven software version as a fall-back.  This is about 15 times slower
   than using the hardware instructions.  This computes a little-endian
   CRC32c, equivalent to the little-endian CRC of the SSE4.2 or ARMv8 instructions,
   regardless of the endianness of the machine this is running on.  */
JL_DLLEXPORT uint32_t jl_crc32c_sw(uint32_t crci, const char *buf, size_t len)
{
    uintptr_t crc = crci ^ 0xffffffff;
    while (len && ((uintptr_t)buf & 7) != 0) {
        crc = crc32c_table[0][(crc ^ *buf++) & 0xff] ^ (crc >> 8);
        len--;
    }
    while (len >= 8) {
#ifdef _P64
        crc ^= *(uint64_t*)buf;
        crc = crc32c_table[7][crc & 0xff] ^
            crc32c_table[6][(crc >> 8) & 0xff] ^
            crc32c_table[5][(crc >> 16) & 0xff] ^
            crc32c_table[4][(crc >> 24) & 0xff] ^
            crc32c_table[3][(crc >> 32) & 0xff] ^
            crc32c_table[2][(crc >> 40) & 0xff] ^
            crc32c_table[1][(crc >> 48) & 0xff] ^
            crc32c_table[0][crc >> 56];
#else
        uint32_t *p = (uint32_t*)buf;
        crc ^= p[0];
        uint32_t hi = p[1];
        crc = crc32c_table[7][crc & 0xff] ^
            crc32c_table[6][(crc >> 8) & 0xff] ^
            crc32c_table[5][(crc >> 16) & 0xff] ^
            crc32c_table[4][(crc >> 24) & 0xff] ^
            crc32c_table[3][hi & 0xff] ^
            crc32c_table[2][(hi >> 8) & 0xff] ^
            crc32c_table[1][(hi >> 16) & 0xff] ^
            crc32c_table[0][hi >> 24];
#endif
        buf += 8;
        len -= 8;
    }
    while (len) {
        crc = crc32c_table[0][(crc ^ *buf++) & 0xff] ^ (crc >> 8);
        len--;
    }
    return (uint32_t)crc ^ 0xffffffff;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/

/* Compile with -DGEN_CRC32C_TABLES to generate header file containing
   precomputed hardware and software tables */
// Example command line, run from top level directory:
// $ gcc src/crc32c.c -o main -DGEN_CRC32C_TABLES -I src -I src/support
// $ ./main > src/crc32c-tables.c

#else /* ifdef GEN_CRC32C_TABLES */

/* Table for a quadword-at-a-time software crc. */
static uint32_t crc32c_table[8][256];

/* Construct table for software CRC-32C calculation. */
static void crc32c_init_sw(void)
{
    uint32_t n, crc, k;

    for (n = 0; n < 256; n++) {
        crc = n;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc = crc & 1 ? (crc >> 1) ^ POLY : crc >> 1;
        crc32c_table[0][n] = crc;
    }
    for (n = 0; n < 256; n++) {
        crc = crc32c_table[0][n];
        for (k = 1; k < 8; k++) {
            crc = crc32c_table[0][crc & 0xff] ^ (crc >> 8);
            crc32c_table[k][n] = crc;
        }
    }
}

/* Tables for hardware crc that shift a crc by LONG and SHORT zeros. */
static uint32_t crc32c_long[4][256];
static uint32_t crc32c_short[4][256];

/* Multiply a matrix times a vector over the Galois field of two elements,
   GF(2).  Each element is a bit in an unsigned integer.  mat must have at
   least as many entries as the power of two for most significant one bit in
   vec. */
static inline uint32_t gf2_matrix_times(uint32_t *mat, uint32_t vec)
{
    uint32_t sum;

    sum = 0;
    while (vec) {
        if (vec & 1)
            sum ^= *mat;
        vec >>= 1;
        mat++;
    }
    return sum;
}

/* Multiply a matrix by itself over GF(2).  Both mat and square must have 32
   rows. */
static inline void gf2_matrix_square(uint32_t *square, uint32_t *mat)
{
    int n;

    for (n = 0; n < 32; n++)
        square[n] = gf2_matrix_times(mat, mat[n]);
}

/* Construct an operator to apply len zeros to a crc.  len must be a power of
   two.  If len is not a power of two, then the result is the same as for the
   largest power of two less than len.  The result for len == 0 is the same as
   for len == 1.  A version of this routine could be easily written for any
   len, but that is not needed for this application. */
static void crc32c_zeros_op(uint32_t *even, size_t len)
{
    int n;
    uint32_t row;
    uint32_t odd[32];       /* odd-power-of-two zeros operator */

    /* put operator for one zero bit in odd */
    odd[0] = POLY;              /* CRC-32C polynomial */
    row = 1;
    for (n = 1; n < 32; n++) {
        odd[n] = row;
        row <<= 1;
    }

    /* put operator for two zero bits in even */
    gf2_matrix_square(even, odd);

    /* put operator for four zero bits in odd */
    gf2_matrix_square(odd, even);

    /* first square will put the operator for one zero byte (eight zero bits),
       in even -- next square puts operator for two zero bytes in odd, and so
       on, until len has been rotated down to zero */
    do {
        gf2_matrix_square(even, odd);
        len >>= 1;
        if (len == 0)
            return;
        gf2_matrix_square(odd, even);
        len >>= 1;
    } while (len);

    /* answer ended up in odd -- copy to even */
    for (n = 0; n < 32; n++)
        even[n] = odd[n];
}

/* Take a length and build four lookup tables for applying the zeros operator
   for that length, byte-by-byte on the operand. */
static void crc32c_zeros(uint32_t zeros[][256], size_t len)
{
    uint32_t n;
    uint32_t op[32];

    crc32c_zeros_op(op, len);
    for (n = 0; n < 256; n++) {
        zeros[0][n] = gf2_matrix_times(op, n);
        zeros[1][n] = gf2_matrix_times(op, n << 8);
        zeros[2][n] = gf2_matrix_times(op, n << 16);
        zeros[3][n] = gf2_matrix_times(op, n << 24);
    }
}

/* Initialize tables for shifting crcs. */
static void crc32c_init_hw(void)
{
    crc32c_zeros(crc32c_long, LONG);
    crc32c_zeros(crc32c_short, SHORT);
}

#include <stdio.h>

static void print_array(const char *name, int m, int n, const uint32_t *a)
{
    int i, j;
    printf("JL_UNUSED static const uint32_t %s[%d][%d] = {\n", name, m, n);
    for (i = 0; i < m; ++i) {
        printf("    { %u", a[i*n+0]);
        for (j = 1; j < n; ++j) printf(",%u", a[i*n+j]);
        printf(" }%s", i == m-1 ? "\n" : ",\n");
    }
    printf("};\n");
}

int main(void)
{
    printf("// This file is a part of Julia. License is MIT: https://julialang.org/license\n\n");
    printf("/* Pregenerated tables for crc32c.c, produced by compiling with -DGEN_CRC32C_TABLES. */\n"
           "#if POLY != 0x%x\n#  error \"tables generated for different polynomial\"\n#endif\n\n", POLY);
    crc32c_init_sw();
    print_array("crc32c_table", 8, 256, &crc32c_table[0][0]);
    crc32c_init_hw();
    printf("\n");
    print_array("crc32c_long", 4, 256, &crc32c_long[0][0]);
    print_array("crc32c_short", 4, 256, &crc32c_short[0][0]);
    return 0;
}

#endif /* GEN_CRC32C_TABLES */

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
