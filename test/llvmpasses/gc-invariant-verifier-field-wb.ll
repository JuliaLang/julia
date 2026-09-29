; This file is a part of Julia. License is MIT: https://julialang.org/license

; Reject malformed field barriers, including invalid variadic operands.
; RUN: split-file %s %t
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(GCInvariantVerifier)' -S %t/null-slot.ll 2>&1 | FileCheck %s --check-prefix=NULL
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(GCInvariantVerifier)' -S %t/arity.ll 2>&1 | FileCheck %s --check-prefix=ARITY
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(GCInvariantVerifier)' -S %t/slot.ll 2>&1 | FileCheck %s --check-prefix=SLOT
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(GCInvariantVerifier)' -S %t/child.ll 2>&1 | FileCheck %s --check-prefix=CHILD

;--- null-slot.ll
declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; NULL: Field write barrier requires non-null slots
define void @field_write_barrier_null_slot(ptr addrspace(10) %parent, ptr addrspace(10) %child) {
top:
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) null, ptr addrspace(10) %child)
  ret void
}

;--- arity.ll
declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; ARITY: Field write barrier must be (parent, slot, child) plus (slot, child) pairs
define void @field_write_barrier_incomplete_pair(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child) {
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child, ptr addrspace(11) %slot)
  ret void
}

;--- slot.ll
declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; SLOT: Field write barrier slot address space must match its declaration
define void @field_write_barrier_wrong_slot(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(13) %loaded, ptr addrspace(10) %child) {
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child, ptr addrspace(13) %loaded, ptr addrspace(10) %child)
  ret void
}

;--- child.ll
declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; CHILD: Field write barrier requires tracked children
define void @field_write_barrier_wrong_child(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child) {
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child, ptr addrspace(11) %slot, i64 0)
  ret void
}
