; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt -enable-new-pm=1 --opaque-pointers=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE

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
define void @preserve_branches(ptr %fptr, i1 %b, i1 %b2) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  br i1 %b, label %L1, label %L3

L1:                                               ; preds = %0
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) nonnull %v)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:                                               ; preds = %L1
  call void @external_function()
  br label %L3

L3:                                               ; preds = %L2, %L1, %0
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
define void @preserve_branches2(ptr %fptr, i1 %b, i1 %b2) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v2 = call ptr addrspace(10) @external_function2()
  br i1 %b, label %L1, label %L3

L1:                                               ; preds = %0
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %tok = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %v, ptr addrspace(10) nonnull %v2)
  call void @external_function()
  br i1 %b2, label %L2, label %L3

L2:                                               ; preds = %L1
  call void @external_function()
  br label %L3

L3:                                               ; preds = %L2, %L1, %0
  ret void
}
; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @legal_int_types
; CHECK: alloca [12 x i8]
; CHECK-NOT: alloca i96
; CHECK: store [12 x i8] zeroinitializer,
; CHECK: ret void
define void @legal_int_types() {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %var1 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 12, ptr addrspace(10) @tag)
  %var2 = addrspacecast ptr addrspace(10) %var1 to ptr addrspace(11)
  %var3 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var2)
  ret void
}
; CHECK-LABEL: }{{$}}

declare void @external_function()

declare ptr addrspace(10) @external_function2()

declare ptr @julia.ptls_states()

declare ptr @julia.get_pgcstack()

declare noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr, i64, ptr addrspace(10))

declare ptr @julia.pointer_from_objref(ptr addrspace(11))

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
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %v_p = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  store i64 %x, ptr addrspace(10) %v_p, align 4
  br i1 false, label %L1, label %L2

L1:                                               ; preds = %0
  %v1 = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  %v1_x = load ptr addrspace(10), ptr addrspace(10) %v1, align 8
  ret void

L2:                                               ; preds = %0
  %v2 = bitcast ptr addrspace(10) %v to ptr addrspace(10)
  %v2_x = load i64, ptr addrspace(10) %v2, align 4
  ret void
}

; CHECK-LABEL: }{{$}}

; CHECK-LABEL: @lifetime_no_preserve_end
; CHECK: alloca
; CHECK-NOT: call token(...) @llvm.julia.gc_preserve_begin
; CHECK: call void @llvm.lifetime.start
; CHECK: store [8 x i8] zeroinitializer,
; CHECK-NOT: call void @llvm.lifetime.end
define void @lifetime_no_preserve_end(ptr noalias nocapture noundef nonnull sret({}) %0) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 8, ptr addrspace(10) @tag)
  %token = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %v)
  %v_derived = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
  %ptr = call nonnull ptr @julia.pointer_from_objref(ptr addrspace(11) %v_derived)
  %ptr_raw = bitcast ptr %ptr to ptr
  call void @external_function()
  %ret_raw = bitcast ptr %0 to ptr
  call void @llvm.memcpy.p0.p0.i64(ptr align 8 %ret_raw, ptr align 8 %ptr_raw, i64 0, i1 false)
  %ret_raw2 = bitcast ptr %0 to ptr
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
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %var1 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 1, ptr addrspace(10) @tag) #1
  %var2 = addrspacecast ptr addrspace(10) %var1 to ptr addrspace(11)
  %var3 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var2)
  %var4 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 2, ptr addrspace(10) @tag) #2
  %var5 = addrspacecast ptr addrspace(10) %var4 to ptr addrspace(11)
  %var6 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var5)
  %var7 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 3, ptr addrspace(10) @tag) #3
  %var8 = addrspacecast ptr addrspace(10) %var7 to ptr addrspace(11)
  %var9 = call ptr @julia.pointer_from_objref(ptr addrspace(11) %var8)
  ret void
}
; CHECK-LABEL: }{{$}}

; Test that the pass handles dead basic blocks with references to the allocation
; CHECK-LABEL: @nopreds
; CHECK: alloca i8, i64 0, align 1
; CHECK: call void @llvm.lifetime.start
define swiftcc { ptr addrspace(10), i8 } @nopreds() {
top:
  %0 = call ptr addrspace(10) @julia.gc_alloc_obj(ptr null, i64 0, ptr addrspace(10) null)
  %1 = addrspacecast ptr addrspace(10) %0 to ptr addrspace(11)
  br label %common.ret

common.ret:                                       ; preds = %union_move9, %top
  ret { ptr addrspace(10), i8 } zeroinitializer

union_move9:                                      ; No predecessors!
  call void @llvm.memcpy.p0.p11.i64(ptr null, ptr addrspace(11) %1, i64 0, i1 false)
  br label %common.ret
}
; CHECK-LABEL: }{{$}}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p11.p0.i64(ptr addrspace(11) noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0
; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p11.i64(ptr noalias nocapture writeonly, ptr addrspace(11) noalias nocapture readonly, i64, i1 immarg) #0
; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #1 = { allockind("alloc") }
attributes #2 = { allockind("alloc,uninitialized") }
attributes #3 = { allockind("alloc,zeroed") }
