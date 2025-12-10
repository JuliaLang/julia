/* Support for reading /etc/ld.so.cache files written by Linux ldconfig.
   Copyright (C) 1999-2019 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#include <stdint.h>

#define FLAG_ANY                        -1
#define FLAG_TYPE_MASK                  0x00ff
#define FLAG_LIBC4                      0x0000
#define FLAG_ELF                        0x0001
#define FLAG_ELF_LIBC5                  0x0002
#define FLAG_ELF_LIBC6                  0x0003
#define FLAG_REQUIRED_MASK              0xff00
#define FLAG_SPARC_LIB64                0x0100
#define FLAG_IA64_LIB64                 0x0200
#define FLAG_X8664_LIB64                0x0300
#define FLAG_S390_LIB64                 0x0400
#define FLAG_POWERPC_LIB64              0x0500
#define FLAG_MIPS64_LIBN32              0x0600
#define FLAG_MIPS64_LIBN64              0x0700
#define FLAG_X8664_LIBX32               0x0800
#define FLAG_ARM_LIBHF                  0x0900
#define FLAG_AARCH64_LIB64              0x0a00
#define FLAG_ARM_LIBSF                  0x0b00
#define FLAG_MIPS_LIB32_NAN2008         0x0c00
#define FLAG_MIPS64_LIBN32_NAN2008      0x0d00
#define FLAG_MIPS64_LIBN64_NAN2008      0x0e00
#define FLAG_RISCV_FLOAT_ABI_SOFT       0x0f00
#define FLAG_RISCV_FLOAT_ABI_DOUBLE     0x1000

#if defined(_CPU_X86_64_)

#define _DL_CACHE_DEFAULT_ID    0x303
#define _dl_cache_check_flags(flags)    ((flags) == _DL_CACHE_DEFAULT_ID)

#elif defined(_CPU_AARCH64_)

#ifdef __LP64__
# define _DL_CACHE_DEFAULT_ID    (FLAG_AARCH64_LIB64 | FLAG_ELF_LIBC6)
#else
# define _DL_CACHE_DEFAULT_ID    (FLAG_AARCH64_LIB32 | FLAG_ELF_LIBC6)
#endif

#define _dl_cache_check_flags(flags)    ((flags) == _DL_CACHE_DEFAULT_ID)

#elif defined(_CPU_RISCV64_)

/* For now we only support the natural XLEN ABI length on all targets, so the
   only bits that need to go into ld.so.cache are the flags for ABI length.  */
#if defined __riscv_float_abi_double
# define _DL_CACHE_DEFAULT_ID    (FLAG_RISCV_FLOAT_ABI_DOUBLE | FLAG_ELF_LIBC6)
#else
# define _DL_CACHE_DEFAULT_ID    (FLAG_RISCV_FLOAT_ABI_SOFT | FLAG_ELF_LIBC6)
#endif

#define _dl_cache_check_flags(flags)    ((flags) == _DL_CACHE_DEFAULT_ID)

#elif defined(_CPU_ARM_)

/* In order to support the transition from unmarked objects
   to marked objects we must treat unmarked objects as
   compatible with either FLAG_ARM_LIBHF or FLAG_ARM_LIBSF.  */
#ifdef __ARM_PCS_VFP
# define _dl_cache_check_flags(flags) \
  ((flags) == (FLAG_ARM_LIBHF | FLAG_ELF_LIBC6) \
   || (flags) == FLAG_ELF_LIBC6)
#else
# define _dl_cache_check_flags(flags) \
  ((flags) == (FLAG_ARM_LIBSF | FLAG_ELF_LIBC6) \
   || (flags) == FLAG_ELF_LIBC6)
#endif

#elif defined(_CPU_X86_)

/* Defined as (FLAG_ELF_LIBC6 | FLAG_X8664_LIBX32).  */
#undef _DL_CACHE_DEFAULT_ID
#define _DL_CACHE_DEFAULT_ID    0x803

#elif defined(_CPU_PPC64_)

#define _DL_CACHE_DEFAULT_ID    0x503

#define _dl_cache_check_flags(flags)                    \
  ((flags) == _DL_CACHE_DEFAULT_ID)

#else

#error "Missing CPU arch-specific definitions in dl-cache.h"

#endif

#ifndef _DL_CACHE_DEFAULT_ID
# define _DL_CACHE_DEFAULT_ID   3
#endif

#ifndef _dl_cache_check_flags
# define _dl_cache_check_flags(flags)                   \
  ((flags) == 1 || (flags) == _DL_CACHE_DEFAULT_ID)
#endif

#ifndef LD_SO_CACHE
# define LD_SO_CACHE SYSCONFDIR "/ld.so.cache"
#endif

#define CACHEMAGIC "ld.so-1.7.0"

/* libc5 and glibc 2.0/2.1 use the same format.  For glibc 2.2 another
   format has been added in a compatible way:
   The beginning of the string table is used for the new table:
        old_magic
        nlibs
        libs[0]
        ...
        libs[nlibs-1]
        pad, new magic needs to be aligned
             - this is string[0] for the old format
        new magic - this is string[0] for the new format
        newnlibs
        ...
        newlibs[0]
        ...
        newlibs[newnlibs-1]
        string 1
        string 2
        ...
*/
struct file_entry
{
  int flags;            /* This is 1 for an ELF library.  */
  unsigned int key, value; /* String table indices.  */
};

struct cache_file
{
  char magic[sizeof CACHEMAGIC - 1];
  unsigned int nlibs;
  struct file_entry libs[0];
};

#define CACHEMAGIC_NEW "glibc-ld.so.cache"
#define CACHE_VERSION "1.1"
#define CACHEMAGIC_VERSION_NEW CACHEMAGIC_NEW CACHE_VERSION


struct file_entry_new
{
  int32_t flags;                /* This is 1 for an ELF library.  */
  uint32_t key, value;          /* String table indices.  */
  uint32_t osversion;           /* Required OS version.  */
  uint64_t hwcap;               /* Hwcap entry.  */
};

struct cache_file_new
{
  char magic[sizeof CACHEMAGIC_NEW - 1];
  char version[sizeof CACHE_VERSION - 1];
  uint32_t nlibs;               /* Number of entries.  */
  uint32_t len_strings;         /* Size of string table. */
  uint32_t unused[5];           /* Leave space for future extensions
                                   and align to 8 byte boundary.  */
  struct file_entry_new libs[0]; /* Entries describing libraries.  */
  /* After this the string table of size len_strings is found.  */
};

/* Used to align cache_file_new.  */
#define ALIGN_CACHE(addr)                               \
(((addr) + __alignof__ (struct cache_file_new) -1)      \
 & (~(__alignof__ (struct cache_file_new) - 1)))

// extern int _dl_cache_libcmp (const char *p1, const char *p2) attribute_hidden;
