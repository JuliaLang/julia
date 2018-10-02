/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      from: @(#)DEFS.h        5.1 (Berkeley) 4/23/90
 * $FreeBSD: src/sys/amd64/include/asm.h,v 1.18 2007/08/22 04:26:07 jkoshy Exp $
 */

#define _START_ENTRY .p2align 4,0x90
#define STR(csym)           #csym
#define XSTR(csym)          STR(csym)
#if defined(__linux__) || defined(__FreeBSD__) || defined(__ELF__)
#ifndef __APPLE__
#define EXT_(csym)          csym
#define EXT(csym)           EXT_(csym)
#endif
#define HIDENAME(asmsym)    .asmsym
.text
_START_ENTRY
.globl EXT(CNAME)
.type EXT(CNAME),@function
EXT(CNAME):

#elif defined(_WIN32)
#define EXT_(csym)          csym
#define EXT(csym)           EXT_(csym)
#define HIDENAME(asmsym)    .asmsym

#ifndef _MSC_VER
.intel_syntax noprefix
.text
_START_ENTRY
.globl EXT(CNAME)
.section .drectve
.ascii " -export:"
.ascii XSTR(CNAME)
.section .text
.def EXT(CNAME)
.scl 2
.type 32
.endef
.seh_proc EXT(CNAME)
EXT(CNAME):
.seh_endprologue
#else
.code
CNAME proc
#endif

#endif
