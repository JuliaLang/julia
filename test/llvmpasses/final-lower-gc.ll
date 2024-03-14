; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='FinalLowerGC' -S %s | FileCheck %s --check-prefixes=CHECK,OPAQUE


@tag = external addrspace(10) global {}

declare void @boxed_simple({} addrspace(10)*, {} addrspace(10)*)
declare {} addrspace(10)* @ijl_box_int64(i64)
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()

declare noalias nonnull {} addrspace(10)** @julia.new_gc_frame(i32)
declare void @julia.push_gc_frame({} addrspace(10)**, i32)
declare {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)**, i32)
declare void @julia.pop_gc_frame({} addrspace(10)**)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_bytes(i8*, i64, i64) #0

attributes #0 = { allocsize(1) }

define void @gc_frame_lowering(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @gc_frame_lowering
; TYPED: %gcframe = alloca {} addrspace(10)*, i32 4
; OPAQUE: %gcframe = alloca ptr addrspace(10), i32 4
  %gcframe = call {} addrspace(10)** @julia.new_gc_frame(i32 2)
; TYPED:  [[GCFRAME_SLOT:%.*]] = call {}*** @julia.get_pgcstack()
; OPAQUE: [[GCFRAME_SLOT:%.*]] = call ptr @julia.get_pgcstack()
  %pgcstack = call {}*** @julia.get_pgcstack()
; TYPED-DAG: [[GCFRAME_SIZE_PTR:%.*]] = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %gcframe, i32 0
; TYPED-DAG: [[GCFRAME_SIZE_PTR2:%.*]] = bitcast {} addrspace(10)** [[GCFRAME_SIZE_PTR]] to i64*
; TYPED-DAG: store i64 8, i64* [[GCFRAME_SIZE_PTR2]], align 8, !tbaa !0
; TYPED-DAG: [[PREV_GCFRAME_PTR:%.*]] = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %gcframe, i32 1
; TYPED-DAG: [[PREV_GCFRAME_PTR2:%.*]] = bitcast {} addrspace(10)** [[PREV_GCFRAME_PTR]] to {}***
; TYPED-DAG: [[PREV_GCFRAME:%.*]] = load {}**, {}*** [[GCFRAME_SLOT]], align 8
; TYPED-DAG: store {}** [[PREV_GCFRAME]], {}*** [[PREV_GCFRAME_PTR2]], align 8, !tbaa !0
; TYPED-DAG: [[GCFRAME_SLOT2:%.*]] = bitcast {}*** [[GCFRAME_SLOT]] to {} addrspace(10)***
; TYPED-NEXT: store {} addrspace(10)** %gcframe, {} addrspace(10)*** [[GCFRAME_SLOT2]], align 8

; OPAQUE-DAG: [[GCFRAME_SIZE_PTR:%.*]] = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 0
; OPAQUE-DAG: store i64 8, ptr [[GCFRAME_SIZE_PTR]], align 8, !tbaa !0
; OPAQUE-DAG: [[PREV_GCFRAME_PTR:%.*]] = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 1
; OPAQUE-DAG: [[PREV_GCFRAME:%.*]] = load ptr, ptr [[GCFRAME_SLOT]], align 8
; OPAQUE-DAG: store ptr [[PREV_GCFRAME]], ptr [[PREV_GCFRAME_PTR]], align 8, !tbaa !0
; OPAQUE-NEXT: store ptr %gcframe, ptr [[GCFRAME_SLOT]], align 8
  call void @julia.push_gc_frame({} addrspace(10)** %gcframe, i32 2)
  %aboxed = call {} addrspace(10)* @ijl_box_int64(i64 signext %a)
; TYPED: %frame_slot_1 = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %gcframe, i32 3
; OPAQUE: %frame_slot_1 = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 3
  %frame_slot_1 = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 1)
  store {} addrspace(10)* %aboxed, {} addrspace(10)** %frame_slot_1, align 8
  %bboxed = call {} addrspace(10)* @ijl_box_int64(i64 signext %b)
; TYPED: %frame_slot_2 = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %gcframe, i32 2
; OPAQUE: %frame_slot_2 = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 2
  %frame_slot_2 = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 0)
  store {} addrspace(10)* %bboxed, {} addrspace(10)** %frame_slot_2, align 8
; TYPED: call void @boxed_simple({} addrspace(10)* %aboxed, {} addrspace(10)* %bboxed)
; OPAQUE: call void @boxed_simple(ptr addrspace(10) %aboxed, ptr addrspace(10) %bboxed)
  call void @boxed_simple({} addrspace(10)* %aboxed, {} addrspace(10)* %bboxed)
; TYPED-NEXT: [[PREV_GCFRAME_PTR3:%.*]] = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)** %gcframe, i32 1
; TYPED-NEXT: [[PREV_GCFRAME_PTR4:%.*]] = load {} addrspace(10)*, {} addrspace(10)** [[PREV_GCFRAME_PTR3]], align 8, !tbaa !0
; TYPED-NEXT: [[GCFRAME_SLOT4:%.*]] = bitcast {}*** [[GCFRAME_SLOT]] to {} addrspace(10)**
; TYPED-NEXT: store {} addrspace(10)* [[PREV_GCFRAME_PTR4]], {} addrspace(10)** [[GCFRAME_SLOT4]], align 8, !tbaa !0

; OPAQUE-NEXT: [[PREV_GCFRAME_PTR3:%.*]] = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 1
; OPAQUE-NEXT: [[PREV_GCFRAME_PTR4:%.*]] = load ptr addrspace(10), ptr [[PREV_GCFRAME_PTR3]], align 8, !tbaa !0
; OPAQUE-NEXT: store ptr addrspace(10) [[PREV_GCFRAME_PTR4]], ptr [[GCFRAME_SLOT]], align 8, !tbaa !0
  call void @julia.pop_gc_frame({} addrspace(10)** %gcframe)
; CHECK-NEXT: ret void
  ret void
}

define {} addrspace(10)* @gc_alloc_lowering() {
top:
; CHECK-LABEL: @gc_alloc_lowering
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
; TYPED: %v = call noalias nonnull align {{[0-9]+}} dereferenceable({{[0-9]+}}) {} addrspace(10)* @ijl_gc_pool_alloc_instrumented
; OPAQUE: %v = call noalias nonnull align {{[0-9]+}} dereferenceable({{[0-9]+}}) ptr addrspace(10) @ijl_gc_pool_alloc_instrumented
  %v = call {} addrspace(10)* @julia.gc_alloc_bytes(i8* %ptls_i8, i64 8, i64 12341234)
  %0 = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
  %1 = getelementptr {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %0, i64 -1
  store {} addrspace(10)* @tag, {} addrspace(10)* addrspace(10)* %1, align 8, !tbaa !0
  ret {} addrspace(10)* %v
}

define {} addrspace(10)* @gc_alloc_lowering_var(i64 %size) {
top:
; CHECK-LABEL: @gc_alloc_lowering_var
  %pgcstack = call {}*** @julia.get_pgcstack()
  %ptls = call {}*** @julia.ptls_states()
  %ptls_i8 = bitcast {}*** %ptls to i8*
; CHECK: %0 = add i64 %size, 8
; TYPED: %v = call noalias nonnull align {{[0-9]+}} dereferenceable(8) {} addrspace(10)* @ijl_gc_alloc_typed(i8* %ptls_i8, i64 %0, i64 12341234)
; OPAQUE: %v = call noalias nonnull align {{[0-9]+}} dereferenceable(8) ptr addrspace(10) @ijl_gc_alloc_typed(ptr %ptls_i8, i64 %0, i64 12341234)
  %v = call {} addrspace(10)* @julia.gc_alloc_bytes(i8* %ptls_i8, i64 %size, i64 12341234)
  %0 = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
  %1 = getelementptr {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %0, i64 -1
  store {} addrspace(10)* @tag, {} addrspace(10)* addrspace(10)* %1, align 8, !tbaa !0
  ret {} addrspace(10)* %v
}

!0 = !{!1, !1, i64 0}
!1 = !{!"jtbaa_gcframe", !2, i64 0}
!2 = !{!"jtbaa"}
