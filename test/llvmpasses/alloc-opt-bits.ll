; This file is a part of Julia. License is MIT: https://julialang.org/license


; RUN: opt -enable-new-pm=0 --opaque-pointers=0 -load libjulia-codegen%shlibext -AllocOpt -S %s | FileCheck %s --check-prefixes=CHECK,TYPED
; RUN: opt -enable-new-pm=1 --opaque-pointers=0 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,TYPED

; RUN: opt -enable-new-pm=0 --opaque-pointers=1 -load libjulia-codegen%shlibext -AllocOpt -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE
; RUN: opt -enable-new-pm=1 --opaque-pointers=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE


@tag = external addrspace(10) global {}

@glob = external addrspace(10) global {}

; Test that the gc_preserve intrinsics are deleted directly.

; CHECK-LABEL: @ptr_and_bits
; CHECK-NOT: alloca 
; OPAQUE: call noalias ptr addrspace(10) @julia.gc_alloc_obj
; TYPED: call noalias {} addrspace(10)* @julia.gc_alloc_obj

define void @ptr_and_bits(i8* %fptr, i1 %b, i1 %b2, i32 %idx) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 16, {} addrspace(10)* @tag)
  %v2 = bitcast {} addrspace(10)* %v to { i64, {} addrspace(10)* } addrspace(10)*
  %g0 = getelementptr { i64, {} addrspace(10)* }, { i64, {} addrspace(10)* } addrspace(10)* %v2, i32 %idx, i32 1
  store {} addrspace(10)* @glob, {} addrspace(10)* addrspace(10)* %g0
  
  %g1 = getelementptr { i64, {} addrspace(10)* }, { i64, {} addrspace(10)* } addrspace(10)* %v2, i32 %idx, i32 0
  store i64 7, i64 addrspace(10)* %g1

  %res = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %g0
  %res2 = load i64, i64 addrspace(10)* %g1
  ret void
}

declare noalias {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)

declare {}*** @julia.get_pgcstack()
