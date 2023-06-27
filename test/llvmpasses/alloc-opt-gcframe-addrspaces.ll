; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt -enable-new-pm=0 --opaque-pointers=0 -load libjulia-codegen%shlibext -AllocOpt -S %s | FileCheck %s --check-prefixes=CHECK,TYPED
; RUN: opt -enable-new-pm=1 --opaque-pointers=0 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,TYPED

; RUN: opt -enable-new-pm=0 --opaque-pointers=1 -load libjulia-codegen%shlibext -AllocOpt -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE
; RUN: opt -enable-new-pm=1 --opaque-pointers=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

target triple = "amdgcn-amd-amdhsa"
target datalayout = "e-p:64:64-p1:64:64-p2:32:32-p3:32:32-p4:64:64-p5:32:32-p6:32:32-i64:64-v16:16-v24:32-v32:32-v48:64-v96:128-v192:256-v256:256-v512:512-v1024:1024-v2048:2048-n32:64-S32-A5-G1-ni:7-ni:10:11:12:13"

@tag = external addrspace(10) global {}

declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()
declare noalias {} addrspace(10)* @julia.gc_alloc_obj(i8*, i64, {} addrspace(10)*)
declare {}* @julia.pointer_from_objref({} addrspace(11)*)

; Test that non-0 addrspace allocas are properly emitted and handled

; CHECK-LABEL: @non_zero_addrspace
; CHECK: %1 = alloca i32, align 8, addrspace(5)

; TYPED: %2 = bitcast i32 addrspace(5)* %1 to i8 addrspace(5)*
; TYPED: %3 = bitcast i8 addrspace(5)* %2 to {} addrspace(5)*
; TYPED: %var1 = addrspacecast {} addrspace(5)* %3 to {} addrspace(10)*
; TYPED: call void @llvm.lifetime.start.p5i8(i64 4, i8 addrspace(5)* %2)

; OPAQUE: %var1 = addrspacecast ptr addrspace(5) %1 to ptr addrspace(10)
; OPAQUE: call void @llvm.lifetime.start.p5(i64 4, ptr addrspace(5) %1)

; CHECK: ret void
define void @non_zero_addrspace() {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %var1 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 4, {} addrspace(10)* @tag)
  %var2 = addrspacecast {} addrspace(10)* %var1 to {} addrspace(11)*
  %var3 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var2)
  ret void
}
; CHECK-LABEL: }{{$}}
