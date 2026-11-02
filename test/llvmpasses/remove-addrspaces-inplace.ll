; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='RemoveJuliaAddrspaces' -S %s | FileCheck %s

; No globals in Julia address spaces, so this module is rewritten in place.

declare void @llvm.memcpy.p10.p11.i64(ptr addrspace(10), ptr addrspace(11), i64, i1)

; CHECK-LABEL: define ptr @callee(ptr %x, ptr %y)
define ptr addrspace(10) @callee(ptr addrspace(10) %x, ptr addrspace(11) %y) {
top:
; CHECK-NOT: addrspacecast
; CHECK: %c = icmp eq ptr %x, null
  %c = icmp eq ptr addrspace(10) %x, null
; CHECK: %s = select i1 %c, ptr null, ptr %x
  %s = select i1 %c, ptr addrspace(10) null, ptr addrspace(10) %x
; CHECK: call void @llvm.memcpy.p0.p0.i64(ptr %s, ptr %y, i64 8, i1 false)
  call void @llvm.memcpy.p10.p11.i64(ptr addrspace(10) %s, ptr addrspace(11) %y, i64 8, i1 false)
; CHECK: ret ptr %s
  ret ptr addrspace(10) %s
}

; CHECK-LABEL: define void @caller(ptr %p)
define void @caller(ptr addrspace(10) %p) {
top:
; CHECK: %a = alloca [2 x ptr], align 8
  %a = alloca [2 x ptr addrspace(10)], align 8
; CHECK: %g = getelementptr inbounds [2 x ptr], ptr %a, i64 0, i64 1
  %g = getelementptr inbounds [2 x ptr addrspace(10)], ptr %a, i64 0, i64 1
; CHECK: store ptr %p, ptr %g
  store ptr addrspace(10) %p, ptr %g, align 8
  br label %def

use:
; the cast's operand is defined in a later block (in layout order)
; CHECK-NOT: addrspacecast
  %d = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: %r = call ptr @callee(ptr %p, ptr %v)
  %r = call ptr addrspace(10) @callee(ptr addrspace(10) %p, ptr addrspace(11) %d)
  ret void

def:
; CHECK: %v = load ptr, ptr %g
  %v = load ptr addrspace(10), ptr %g, align 8
  br label %use
}
