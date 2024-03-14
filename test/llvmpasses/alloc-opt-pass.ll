; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

@tag = external addrspace(10) global {}

; Test that the gc_preserve intrinsics are deleted directly.

; CHECK-LABEL: @preserve_branches
; TYPED: call {}*** @julia.ptls_states()
; OPAQUE: call ptr @julia.ptls_states()
; CHECK: L1:
; CHECK-NOT: @llvm.julia.gc_preserve_begin
; CHECK-NEXT: @external_function()
; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; CHECK: @external_function()
; CHECK-NEXT: br label %L3

; CHECK: L3:
define void @preserve_branches(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 8, {} addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* nonnull %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @preserve_branches2
; TYPED: call {}*** @julia.ptls_states()
; OPAQUE: call ptr @julia.ptls_states()
; CHECK: L1:
; TYPED-NEXT: @llvm.julia.gc_preserve_begin{{.*}}{} addrspace(10)* %v2
; OPAQUE-NEXT: @llvm.julia.gc_preserve_begin{{.*}}ptr addrspace(10) %v2
; CHECK-NEXT: @external_function()
; CHECK-NEXT: br i1 %b2, label %L2, label %L3

; CHECK: L2:
; CHECK: @external_function()
; CHECK-NEXT: br label %L3

; CHECK: L3:
define void @preserve_branches2(i8* %fptr, i1 %b, i1 %b2) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %v2 = call {} addrspace(10)* @external_function2()
  br i1 %b, label %L1, label %L3

L1:
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 8, {} addrspace(10)* @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v, {} addrspace(10)* nonnull %v2)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:
  call void @external_function()
  br label %L3

L3:
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @legal_int_types
; CHECK: alloca [12 x i8]
; CHECK-NOT: alloca i96
; CHECK: store [12 x i8] zeroinitializer,
; CHECK: ret void
define void @legal_int_types() {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %var1 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 12, {} addrspace(10)* @tag)
  %var2 = addrspacecast {} addrspace(10)* %var1 to {} addrspace(11)*
  %var3 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var2)
  ret void
}
; CHECK-LABEL: }{{$}}


declare void @external_function()
declare {} addrspace(10)* @external_function2()
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()
declare noalias {} addrspace(10)* @julia.gc_alloc_obj(i8*, i64, {} addrspace(10)*)
declare {}* @julia.pointer_from_objref({} addrspace(11)*)
declare void @llvm.memcpy.p11i8.p0i8.i64(i8 addrspace(11)* nocapture writeonly, i8* nocapture readonly, i64, i32, i1)
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)

; CHECK-LABEL: @memref_collision
; TYPED: call {}*** @julia.ptls_states()
; OPAQUE: call ptr @julia.ptls_states()
; TYPED-NOT: store {}
; OPAQUE-NOT: store ptr
; CHECK: store i
; TYPED-NOT: store {}
; OPAQUE-NOT: store ptr
; CHECK: L1:
; TYPED: load {}
; OPAQUE: load ptr
; CHECK: L2:
; CHECK: load i
define void @memref_collision(i64 %x) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 8, {} addrspace(10)* @tag)
  %v_p = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
  store i64 %x, i64 addrspace(10)* %v_p
  br i1 0, label %L1, label %L2

L1:
  %v1 = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
  %v1_x = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %v1
  ret void

L2:
  %v2 = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
  %v2_x = load i64, i64 addrspace(10)* %v2
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @lifetime_no_preserve_end
; CHECK: alloca
; CHECK-NOT: call token(...) @llvm.julia.gc_preserve_begin
; CHECK: call void @llvm.lifetime.start
; CHECK: store [8 x i8] zeroinitializer,
; CHECK-NOT: call void @llvm.lifetime.end
define void @lifetime_no_preserve_end({}* noalias nocapture noundef nonnull sret({}) %0) {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
  %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 8, {} addrspace(10)* @tag)
  %token = call token (...) @llvm.julia.gc_preserve_begin({} addrspace(10)* %v)
  %v_derived = addrspacecast {} addrspace(10)* %v to {} addrspace(11)*
  %ptr = call nonnull {}* @julia.pointer_from_objref({} addrspace(11)* %v_derived)
  %ptr_raw = bitcast {}* %ptr to i8*
  call void @external_function() ; safepoint
  %ret_raw = bitcast {}* %0 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %ret_raw, i8 * align 8 %ptr_raw, i64 0, i1 false)
  %ret_raw2 = bitcast {}* %0 to i8*
  ret void
}
; CHECK-LABEL: }{{$}}


; CHECK-LABEL: @initializers
; CHECK: alloca [1 x i8]
; CHECK-DAG: alloca [2 x i8]
; CHECK-DAG: alloca [3 x i8]
; CHECK-DAG: freeze [1 x i8] undef
; CHECK-DAG: store [1 x i8] %
; CHECK-DAG: store [3 x i8] zeroinitializer,
; CHECK-NOT: store
; CHECK-NOT: zeroinitializer
; CHECK: ret void
define void @initializers() {
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*

  %var1 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 1, {} addrspace(10)* @tag) #0
  %var2 = addrspacecast {} addrspace(10)* %var1 to {} addrspace(11)*
  %var3 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var2)

  %var4 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 2, {} addrspace(10)* @tag) #1
  %var5 = addrspacecast {} addrspace(10)* %var4 to {} addrspace(11)*
  %var6 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var5)

  %var7 = call {} addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 3, {} addrspace(10)* @tag) #2
  %var8 = addrspacecast {} addrspace(10)* %var7 to {} addrspace(11)*
  %var9 = call {}* @julia.pointer_from_objref({} addrspace(11)* %var8)

  ret void
}
; CHECK-LABEL: }{{$}}

attributes #0 = { allockind("alloc") }
attributes #1 = { allockind("alloc,uninitialized") }
attributes #2 = { allockind("alloc,zeroed") }
