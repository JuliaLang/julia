; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt,LateLowerGCFrame,FinalLowerGC)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"

@tag = external addrspace(10) global {}

; CHECK-LABEL: @return_obj
; CHECK-NOT: @julia.gc_alloc_obj

; OPAQUE: %current_task = getelementptr inbounds ptr, ptr %gcstack, i64 -12
; OPAQUE: [[ptls_field:%.*]] = getelementptr inbounds i8, ptr %current_task,
; OPAQUE-NEXT: [[ptls_load:%.*]] = load ptr, ptr [[ptls_field]], align 8, !tbaa !0
; OPAQUE-NEXT: %v = call noalias nonnull align {{[0-9]+}} dereferenceable({{[0-9]+}}) ptr addrspace(10) @ijl_gc_small_alloc(ptr [[ptls_load]], i32 [[SIZE_T:[0-9]+]], i32 16, i64 {{.*}} @tag {{.*}})
; OPAQUE: store atomic ptr addrspace(10) @tag, ptr addrspace(10) {{.*}} unordered, align 8, !tbaa !4

define {} addrspace(10)* @return_obj() {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  ret {} addrspace(10)* %v
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @return_load
; CHECK: alloca i64
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK-NOT: @jl_gc_small_alloc
; OPAQUE: call void @llvm.lifetime.start{{.*}}(i64 8, ptr
; CHECK-NOT: @tag
; CHECK-NOT: @llvm.lifetime.end
define i64 @return_load(i64 %i) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %v64 = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
  %v64a11 = addrspacecast i64 addrspace(10)* %v64 to i64 addrspace(11)*
  store i64 %i, i64 addrspace(11)* %v64a11, align 16, !tbaa !4
  call void @external_function()
  %l = load i64, i64 addrspace(11)* %v64a11, align 16, !tbaa !4
  ret i64 %l
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @ccall_obj
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK: @ijl_gc_small_alloc
; OPAQUE: store atomic ptr addrspace(10) @tag, ptr addrspace(10) {{.*}} unordered, align 8, !tbaa !4
define void @ccall_obj(i8* %fptr) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %f = bitcast i8* %fptr to void ({} addrspace(10)*)*
  call void %f({} addrspace(10)* %v)
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @ccall_ptr
; CHECK: alloca i64
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK-NOT: @jl_gc_small_alloc
; OPAQUE: call void @llvm.lifetime.start{{.*}}(i64 8, ptr
; OPAQUE: %f = bitcast ptr %fptr to ptr
; Currently the GC frame lowering pass strips away all operand bundles
; OPAQUE-NEXT: call void %f(ptr
; CHECK-NEXT: ret void
define void @ccall_ptr(i8* %fptr) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %va = addrspacecast {} addrspace(10)* %v to {} addrspace(11)*
  %ptrj = call {}* @julia.pointer_from_objref({} addrspace(11)* %va)
  %ptr = bitcast {}* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_roots"({} addrspace(10)* %v), "unknown_bundle"(i8* %ptr) ]
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @ccall_unknown_bundle
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK: @ijl_gc_small_alloc
; OPAQUE: store atomic ptr addrspace(10) @tag, ptr addrspace(10) {{.*}} unordered, align 8, !tbaa !4
define void @ccall_unknown_bundle(i8* %fptr) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %va = addrspacecast {} addrspace(10)* %v to {} addrspace(11)*
  %ptrj = call {}* @julia.pointer_from_objref({} addrspace(11)* %va)
  %ptr = bitcast {}* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_not_jl_roots"({} addrspace(10)* %v) ]
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @lifetime_branches
; CHECK: alloca i64
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK: L1:
; CHECK-NEXT: call void @llvm.lifetime.start{{.*}}(i64 8,


; OPAQUE: %f = bitcast ptr %fptr to ptr
; OPAQUE-NEXT: call void %f(ptr

; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; OPAQUE-NEXT: %f2 = bitcast ptr %fptr to ptr
; CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
; OPAQUE-NEXT: call void %f2(ptr null)

; CHECK: L3:
; CHECK-NEXT: call void @llvm.lifetime.end{{.*}}(i64 8,
define void @lifetime_branches(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %va = addrspacecast {} addrspace(10)* %v to {} addrspace(11)*
  %ptrj = call {}* @julia.pointer_from_objref({} addrspace(11)* %va)
  %ptr = bitcast {}* %ptrj to i8*
  %f = bitcast i8* %fptr to void (i8*)*
  call void %f(i8* %ptr) [ "jl_roots"({} addrspace(10)* %v) ]
  br i1 %b2, label %L2, label %L3

L2:
  %f2 = bitcast i8* %fptr to void ({}*)*
  call void %f2({}* null)
  br label %L3

L3:
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @object_field
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK-NOT: @jl_gc_small_alloc
; CHECK-NOT: store {} addrspace(10)* @tag, {} addrspace(10)* addrspace(10)* {{.*}}, align 8, !tbaa !4
define void @object_field({} addrspace(10)* %field) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %va = addrspacecast {} addrspace(10)* %v to {} addrspace(11)*
  %vab = bitcast {} addrspace(11)* %va to {} addrspace(10)* addrspace(11)*
  store {} addrspace(10)* %field, {} addrspace(10)* addrspace(11)* %vab, align 8
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @memcpy_opt
; CHECK: alloca [16 x i8], align 16
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK-NOT: @jl_gc_small_alloc
; OPAQUE: call void @llvm.memcpy.p0.p0.i64
define void @memcpy_opt(i8* %v22) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v19 = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 16, {} addrspace(10)* @tag)
  %v20 = bitcast {} addrspace(10)* %v19 to i8 addrspace(10)*
  %v21 = addrspacecast i8 addrspace(10)* %v20 to i8 addrspace(11)*
  call void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* %v21, i8* %v22, i64 16, i32 8, i1 false)
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @preserve_opt
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK-NOT: @julia.gc_alloc_obj
; CHECK-NOT: @jl_gc_small_alloc
; CHECK-NOT: @llvm.lifetime.end
; CHECK: @external_function
define void @preserve_opt(i8* %v22) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  %v19 = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 16, {} addrspace(10)* @tag)
  %v20 = bitcast {} addrspace(10)* %v19 to i8 addrspace(10)*
  %v21 = addrspacecast i8 addrspace(10)* %v20 to i8 addrspace(11)*
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v19)
  call void @external_function()
  call void @llvm.julia.gc_preserve_end(token %tok)
  call void @external_function()
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @preserve_branches
; OPAQUE: call ptr @julia.get_pgcstack()
; CHECK: L1:
; CHECK-NEXT: @external_function()
; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; CHECK: @external_function()
; CHECK-NEXT: br label %L3

; CHECK: L3:
define void @preserve_branches(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gcstack = bitcast {}*** %pgcstack to {}**
  %current_task = getelementptr inbounds {}*, {}** %gcstack, i64 -12
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
; CHECK-LABEL: }{{$}}

; OPAQUE: declare noalias nonnull ptr addrspace(10) @ijl_gc_small_alloc(ptr,
; OPAQUE: declare noalias nonnull ptr addrspace(10) @ijl_gc_big_alloc(ptr,
declare void @external_function()
declare {}*** @julia.get_pgcstack()
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)
declare {}* @julia.pointer_from_objref({} addrspace(11)*)
declare void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)

!0 = !{!1, !1, i64 0}
!1 = !{!"jtbaa_tag", !2, i64 0}
!2 = !{!"jtbaa_data", !3, i64 0}
!3 = !{!"jtbaa"}
!4 = !{!5, !5, i64 0}
!5 = !{!"jtbaa_mutab", !6, i64 0}
!6 = !{!"jtbaa_value", !2, i64 0}
