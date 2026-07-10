/*
 * Copyright (c) 2006 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_OSREFERENCE_LICENSE_HEADER_START@
 *
 * This file contains Original Code and/or Modifications of Original Code
 * as defined in and that are subject to the Apple Public Source License
 * Version 2.0 (the 'License'). You may not use this file except in
 * compliance with the License. The rights granted to you under the License
 * may not be used to create, or enable the creation or redistribution of,
 * unlawful or unlicensed copies of an Apple operating system, or to
 * circumvent, violate, or enable the circumvention or violation of, any
 * terms of an Apple operating system software license agreement.
 *
 * Please obtain a copy of the License at
 * https://www.opensource.apple.com/apsl/ and read it before using this file.
 *
 * The Original Code and all software distributed under the License are
 * distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR NON-INFRINGEMENT.
 * Please see the License for the specific language governing rights and
 * limitations under the License.
 *
 * @APPLE_OSREFERENCE_LICENSE_HEADER_END@
 */
/*
 * @OSF_COPYRIGHT@
 */
/*
 * Mach Operating System
 * Copyright (c) 1991,1990,1989,1988,1987 Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 *
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
#ifndef _mach_exc_user_
#define _mach_exc_user_

/* Module mach_exc */

#include <string.h>
#include <mach/ndr.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/notify.h>
#include <mach/mach_types.h>
#include <mach/message.h>
#include <mach/mig_errors.h>
#include <mach/port.h>

/* BEGIN VOUCHER CODE */

#ifndef KERNEL
#if defined(__has_include)
#if __has_include(<mach/mig_voucher_support.h>)
#ifndef USING_VOUCHERS
#define USING_VOUCHERS
#endif
#ifndef __VOUCHER_FORWARD_TYPE_DECLS__
#define __VOUCHER_FORWARD_TYPE_DECLS__
#ifdef __cplusplus
extern "C" {
#endif
#ifndef __VOUCHER_FOWARD_TYPE_DECLS_SINGLE_ATTR
#define __VOUCHER_FOWARD_TYPE_DECLS_SINGLE_ATTR __unsafe_indexable
#endif
        extern boolean_t voucher_mach_msg_set(mach_msg_header_t * msg) __attribute__((weak_import));
#ifdef __cplusplus
}
#endif
#endif // __VOUCHER_FORWARD_TYPE_DECLS__
#endif // __has_include(<mach/mach_voucher_types.h>)
#endif // __has_include
#endif // !KERNEL

/* END VOUCHER CODE */


/* BEGIN MIG_STRNCPY_ZEROFILL CODE */

#if defined(__has_include)
#if __has_include(<mach/mig_strncpy_zerofill_support.h>)
#ifndef USING_MIG_STRNCPY_ZEROFILL
#define USING_MIG_STRNCPY_ZEROFILL
#endif
#ifndef __MIG_STRNCPY_ZEROFILL_FORWARD_TYPE_DECLS__
#define __MIG_STRNCPY_ZEROFILL_FORWARD_TYPE_DECLS__
#ifdef __cplusplus
extern "C" {
#endif
#ifndef __MIG_STRNCPY_ZEROFILL_FORWARD_TYPE_DECLS_CSTRING_ATTR
#define __MIG_STRNCPY_ZEROFILL_FORWARD_TYPE_DECLS_CSTRING_COUNTEDBY_ATTR(C) __unsafe_indexable
#endif
        extern int mig_strncpy_zerofill(char * dest, const char * src, int len) __attribute__((weak_import));
#ifdef __cplusplus
}
#endif
#endif /* __MIG_STRNCPY_ZEROFILL_FORWARD_TYPE_DECLS__ */
#endif /* __has_include(<mach/mig_strncpy_zerofill_support.h>) */
#endif /* __has_include */

/* END MIG_STRNCPY_ZEROFILL CODE */


#ifdef AUTOTEST
#ifndef FUNCTION_PTR_T
#define FUNCTION_PTR_T
typedef void (*function_ptr_t)(mach_port_t, char *, mach_msg_type_number_t);
typedef struct {
        char            * name;
        function_ptr_t  function;
} function_table_entry;
typedef function_table_entry   *function_table_t;
#endif /* FUNCTION_PTR_T */
#endif /* AUTOTEST */

#ifndef mach_exc_MSG_COUNT
#define mach_exc_MSG_COUNT      6
#endif  /* mach_exc_MSG_COUNT */

#include <Availability.h>
#include <mach/std_types.h>
#include <mach/mig.h>
#include <mach/mig.h>
#include <mach/mach_types.h>

#ifdef __BeforeMigUserHeader
__BeforeMigUserHeader
#endif /* __BeforeMigUserHeader */

#include <sys/cdefs.h>
__BEGIN_DECLS


/* Routine mach_exception_raise */
#ifdef  mig_external
mig_external
#else
extern
#endif  /* mig_external */
kern_return_t mach_exception_raise
(
        mach_port_t exception_port,
        mach_port_t thread,
        mach_port_t task,
        exception_type_t exception,
        mach_exception_data_t code,
        mach_msg_type_number_t codeCnt
);

/* Routine mach_exception_raise_state */
#ifdef  mig_external
mig_external
#else
extern
#endif  /* mig_external */
kern_return_t mach_exception_raise_state
(
        mach_port_t exception_port,
        exception_type_t exception,
        const mach_exception_data_t code,
        mach_msg_type_number_t codeCnt,
        int *flavor,
        const thread_state_t old_state,
        mach_msg_type_number_t old_stateCnt,
        thread_state_t new_state,
        mach_msg_type_number_t *new_stateCnt
);

/* Routine mach_exception_raise_state_identity */
#ifdef  mig_external
mig_external
#else
extern
#endif  /* mig_external */
kern_return_t mach_exception_raise_state_identity
(
        mach_port_t exception_port,
        mach_port_t thread,
        mach_port_t task,
        exception_type_t exception,
        mach_exception_data_t code,
        mach_msg_type_number_t codeCnt,
        int *flavor,
        thread_state_t old_state,
        mach_msg_type_number_t old_stateCnt,
        thread_state_t new_state,
        mach_msg_type_number_t *new_stateCnt
);

__END_DECLS

/********************** Caution **************************/
/* The following data types should be used to calculate  */
/* maximum message sizes only. The actual message may be */
/* smaller, and the position of the arguments within the */
/* message layout may vary from what is presented here.  */
/* For example, if any of the arguments are variable-    */
/* sized, and less than the maximum is sent, the data    */
/* will be packed tight in the actual message to reduce  */
/* the presence of holes.                                */
/********************** Caution **************************/

/* typedefs for all requests */

#ifndef __Request__mach_exc_subsystem__defined
#define __Request__mach_exc_subsystem__defined

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                /* start of the kernel processed data */
                mach_msg_body_t msgh_body;
                mach_msg_port_descriptor_t thread;
                mach_msg_port_descriptor_t task;
                /* end of the kernel processed data */
                NDR_record_t NDR;
                exception_type_t exception;
                mach_msg_type_number_t codeCnt;
                int64_t code[2];
        } __Request__mach_exception_raise_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                NDR_record_t NDR;
                exception_type_t exception;
                mach_msg_type_number_t codeCnt;
                int64_t code[2];
                int flavor;
                mach_msg_type_number_t old_stateCnt;
                natural_t old_state[1296];
        } __Request__mach_exception_raise_state_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                /* start of the kernel processed data */
                mach_msg_body_t msgh_body;
                mach_msg_port_descriptor_t thread;
                mach_msg_port_descriptor_t task;
                /* end of the kernel processed data */
                NDR_record_t NDR;
                exception_type_t exception;
                mach_msg_type_number_t codeCnt;
                int64_t code[2];
                int flavor;
                mach_msg_type_number_t old_stateCnt;
                natural_t old_state[1296];
        } __Request__mach_exception_raise_state_identity_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif
#endif /* !__Request__mach_exc_subsystem__defined */

/* union of all requests */

#ifndef __RequestUnion__mach_exc_subsystem__defined
#define __RequestUnion__mach_exc_subsystem__defined
union __RequestUnion__mach_exc_subsystem {
        __Request__mach_exception_raise_t Request_mach_exception_raise;
        __Request__mach_exception_raise_state_t Request_mach_exception_raise_state;
        __Request__mach_exception_raise_state_identity_t Request_mach_exception_raise_state_identity;
};
#endif /* !__RequestUnion__mach_exc_subsystem__defined */
/* typedefs for all replies */

#ifndef __Reply__mach_exc_subsystem__defined
#define __Reply__mach_exc_subsystem__defined

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                NDR_record_t NDR;
                kern_return_t RetCode;
        } __Reply__mach_exception_raise_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                NDR_record_t NDR;
                kern_return_t RetCode;
                int flavor;
                mach_msg_type_number_t new_stateCnt;
                natural_t new_state[1296];
        } __Reply__mach_exception_raise_state_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif

#ifdef  __MigPackStructs
#pragma pack(push, 4)
#endif
        typedef struct {
                mach_msg_header_t Head;
                NDR_record_t NDR;
                kern_return_t RetCode;
                int flavor;
                mach_msg_type_number_t new_stateCnt;
                natural_t new_state[1296];
        } __Reply__mach_exception_raise_state_identity_t __attribute__((unused));
#ifdef  __MigPackStructs
#pragma pack(pop)
#endif
#endif /* !__Reply__mach_exc_subsystem__defined */

/* union of all replies */

#ifndef __ReplyUnion__mach_exc_subsystem__defined
#define __ReplyUnion__mach_exc_subsystem__defined
union __ReplyUnion__mach_exc_subsystem {
        __Reply__mach_exception_raise_t Reply_mach_exception_raise;
        __Reply__mach_exception_raise_state_t Reply_mach_exception_raise_state;
        __Reply__mach_exception_raise_state_identity_t Reply_mach_exception_raise_state_identity;
};
#endif /* !__RequestUnion__mach_exc_subsystem__defined */

#ifndef subsystem_to_name_map_mach_exc
#define subsystem_to_name_map_mach_exc \
    { "mach_exception_raise", 2405 },\
    { "mach_exception_raise_state", 2406 },\
    { "mach_exception_raise_state_identity", 2407 }
#endif

#ifdef __AfterMigUserHeader
__AfterMigUserHeader
#endif /* __AfterMigUserHeader */

#endif   /* _mach_exc_user_ */
