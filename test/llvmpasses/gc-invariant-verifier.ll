; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(GCInvariantVerifier)' -S %s | FileCheck %s

; CHECK-LABEL: @vectorized_addrspacecast
define ptr addrspace(10) @vectorized_addrspacecast() {
top:
  ret ptr addrspace(10) null

vector.ph:
  %0 = addrspacecast <4 x ptr addrspace(10)> zeroinitializer to <4 x ptr addrspace(11)>
  unreachable
}

declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)

; CHECK-LABEL: @field_write_barrier_ok
define void @field_write_barrier_ok(ptr addrspace(10) %parent, ptr addrspace(10) %child) {
top:
  %slot = addrspacecast ptr addrspace(10) %parent to ptr addrspace(11)
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child)
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %parent, ptr addrspace(11) %slot, ptr addrspace(10) %child, ptr addrspace(11) %slot, ptr addrspace(10) %child)
  ret void
}
