; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaLICM' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

; COM: This file contains functions that currently do not trigger allocations to be hoisted out of loops
; COM: i.e. they are missed optimizations
; COM: Better optimization could potentially enable allocations to be hoisted out of these loops

@tag = external addrspace(10) global {}, align 16

; COM: Currently we don't hoist allocations that have references stored into them out of loops
; COM: This is because we need to insert write barriers for the stores when the storee does not
; COM: dominate the allocation after it has been moved out of the loop
; CHECK-LABEL: @julia_refstore
define void @julia_refstore({} addrspace(10)* %obj, i1 %ret) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
; CHECK: br label %preheader
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NOT: julia.gc_alloc_obj
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; OPAQUE-NEXT: %alloc = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_obj(ptr nonnull %current_task, i64 8, ptr addrspace(10) @tag)
  %alloc = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task, i64 8, {} addrspace(10)* @tag)
; OPAQUE-NEXT: %derived = addrspacecast ptr addrspace(10) %alloc to ptr addrspace(11)
  %derived = addrspacecast {} addrspace(10)* %alloc to {} addrspace(11)*
; OPAQUE-NEXT: %ptr = bitcast ptr addrspace(11) %derived to ptr addrspace(11)
  %ptr = bitcast {} addrspace(11)* %derived to {} addrspace(10)* addrspace(11)*
; OPAQUE-NEXT: store ptr addrspace(10) %obj, ptr addrspace(11) %ptr, align 8
  store {} addrspace(10)* %obj, {} addrspace(10)* addrspace(11)* %ptr, align 8
  br i1 %ret, label %return, label %loop
return:
  ret void
}

; COM: Currently our LLVM-level escape analysis doesn't handle phi nodes at all
; COM: so this allocation is counted as 'escaping' despite the fact that it's
; COM: clearly dead
; CHECK-LABEL: @julia_phi
define void @julia_phi({} addrspace(10)* %obj, i1 %ret) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
; CHECK: br label %preheader
  br label %preheader
; CHECK: preheader:
preheader:
; CHECK-NOT: julia.gc_alloc_obj
; CHECK-NEXT: br label %loop
  br label %loop
; CHECK: loop:
loop:
; OPAQUE-NEXT: %alloc = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_obj(ptr nonnull %current_task, i64 8, ptr addrspace(10) @tag)
  %alloc = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task, i64 8, {} addrspace(10)* @tag)
  br label %other
; CHECK: other:
other:
; OPAQUE-NEXT: %phi = phi ptr addrspace(10) [ %alloc, %loop ]
  %phi = phi {} addrspace(10)* [ %alloc, %loop ]
  br i1 %ret, label %return, label %loop
return:
  ret void
}



declare void @julia.write_barrier({}*, ...)

declare {}*** @julia.get_pgcstack()

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*) #1

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: inaccessiblemem_or_argmemonly
declare void @ijl_gc_queue_root({} addrspace(10)*) #3

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_small_alloc(i8*, i32, i32, i8*) #1

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_big_alloc(i8*, i64) #1

; COM: escape to make it easy to find
declare nonnull {} addrspace(10)* @escape({} addrspace(10)*)

; COM: addrescape function
declare nonnull {}* @julia.pointer_from_objref({} addrspace(11)*)

attributes #0 = { "probe-stack"="inline-asm" }
attributes #1 = { allocsize(1) }
attributes #2 = { argmemonly nofree nosync nounwind willreturn }
attributes #3 = { inaccessiblemem_or_argmemonly }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
