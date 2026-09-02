; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(GCInvariantVerifier)' -S %s 2>&1 | FileCheck %s

declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; COM: a null slot is not a degraded field barrier; demotion must emit
; COM: julia.object_write_barrier explicitly
; CHECK: Field write barrier requires non-null slots
define void @field_write_barrier_null_slot(ptr addrspace(10) %parent, ptr addrspace(10) %child) {
top:
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) null, ptr addrspace(10) %child)
  ret void
}
