; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

; Extracts of pointer vectors at a dynamic index (e.g. formed by SROA or the
; vectorizers from a tuple of tracked pointers) used to abort with
; `cast<ConstantInt>` in FindBaseValue (issue #62899).

declare ptr @julia.get_pgcstack()
declare void @jl_safepoint()
declare ptr addrspace(10) @jl_box_int64(i64)
declare void @sink(ptr addrspace(10))

; A dynamic extract of a tracked vector is a root of its own and must be
; preserved across the safepoint.
define ptr addrspace(10) @dyn_extractelement(i64 %a, i64 %b, i64 %i) {
top:
; CHECK-LABEL: @dyn_extractelement
; CHECK: %e = extractelement <2 x ptr addrspace(10)> %v1, i64 %i
; CHECK: store ptr addrspace(10) %e, ptr %gc_slot_addr_{{[0-9]+}}
; CHECK: call void @jl_safepoint()
  %pgcstack = call ptr @julia.get_pgcstack()
  %aboxed = call ptr addrspace(10) @jl_box_int64(i64 %a)
  %bboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
  %v0 = insertelement <2 x ptr addrspace(10)> undef, ptr addrspace(10) %aboxed, i32 0
  %v1 = insertelement <2 x ptr addrspace(10)> %v0, ptr addrspace(10) %bboxed, i32 1
  %e = extractelement <2 x ptr addrspace(10)> %v1, i64 %i
  call void @jl_safepoint()
  call void @sink(ptr addrspace(10) %e)
  ret ptr addrspace(10) %e
}

; The same shape reached through phi refinement (the crash in #62899).
define ptr addrspace(10) @dyn_extractelement_phi(i64 %a, i64 %b, i64 %i, i1 %cond) {
top:
; CHECK-LABEL: @dyn_extractelement_phi
; CHECK: %e = extractelement <2 x ptr addrspace(10)> %v1, i64 %i
  %pgcstack = call ptr @julia.get_pgcstack()
  %aboxed = call ptr addrspace(10) @jl_box_int64(i64 %a)
  %bboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
  %v0 = insertelement <2 x ptr addrspace(10)> undef, ptr addrspace(10) %aboxed, i32 0
  %v1 = insertelement <2 x ptr addrspace(10)> %v0, ptr addrspace(10) %bboxed, i32 1
  br i1 %cond, label %left, label %right

left:
  %e = extractelement <2 x ptr addrspace(10)> %v1, i64 %i
  br label %join

right:
  br label %join

join:
; CHECK: %p = phi ptr addrspace(10)
; CHECK: store ptr addrspace(10) %p, ptr %gc_slot_addr_{{[0-9]+}}
; CHECK: call void @jl_safepoint()
  %p = phi ptr addrspace(10) [ %e, %left ], [ %aboxed, %right ]
  call void @jl_safepoint()
  call void @sink(ptr addrspace(10) %p)
  ret ptr addrspace(10) %p
}

; A dynamic extract of a vector of derived pointers is lifted to the matching
; lane of the vector of base pointers.
define double @dyn_extractelement_derived(i64 %a, i64 %b, i64 %i) {
top:
; CHECK-LABEL: @dyn_extractelement_derived
; CHECK: %gclift = extractelement <2 x ptr addrspace(10)> %{{.*}}, i64 %i
; CHECK: store ptr addrspace(10) %gclift, ptr %gc_slot_addr_{{[0-9]+}}
; CHECK: call void @jl_safepoint()
  %pgcstack = call ptr @julia.get_pgcstack()
  %aboxed = call ptr addrspace(10) @jl_box_int64(i64 %a)
  %bboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
  %v0 = insertelement <2 x ptr addrspace(10)> undef, ptr addrspace(10) %aboxed, i32 0
  %v1 = insertelement <2 x ptr addrspace(10)> %v0, ptr addrspace(10) %bboxed, i32 1
  %dv = addrspacecast <2 x ptr addrspace(10)> %v1 to <2 x ptr addrspace(11)>
  %e = extractelement <2 x ptr addrspace(11)> %dv, i64 %i
  call void @jl_safepoint()
  %x = load double, ptr addrspace(11) %e, align 8
  ret double %x
}
