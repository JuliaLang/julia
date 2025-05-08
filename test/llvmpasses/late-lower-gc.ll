; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

@tag = external addrspace(10) global {}, align 16

declare void @boxed_simple({} addrspace(10)*, {} addrspace(10)*)
declare {} addrspace(10)* @jl_box_int64(i64)
declare {}*** @julia.get_pgcstack()
declare void @jl_safepoint()
declare {} addrspace(10)* @jl_apply_generic({} addrspace(10)*, {} addrspace(10)**, i32)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)
declare i32 @rooting_callee({} addrspace(12)*, {} addrspace(12)*)

define void @gc_frame_lowering(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @gc_frame_lowering

; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 2)
; CHECK:  %pgcstack = call ptr @julia.get_pgcstack()
    %pgcstack = call {}*** @julia.get_pgcstack()

; CHECK-NEXT: call void @julia.push_gc_frame(ptr %gcframe, i32 2)
; CHECK-NEXT: call ptr addrspace(10) @jl_box_int64
    %aboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %a)

; CHECK: [[GEP0:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 [[GEPSLOT0:[0-9]+]])
; CHECK-NEXT: store ptr addrspace(10) %aboxed, ptr [[GEP0]]
    %bboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %b)
; CHECK-NEXT: %bboxed =
; Make sure the same gc slot isn't re-used

; CHECK-NOT: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 [[GEPSLOT0]])
; CHECK: [[GEP1:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 [[GEPSLOT1:[0-9]+]])
; CHECK-NEXT: store ptr addrspace(10) %bboxed, ptr [[GEP1]]

; CHECK-NEXT: call void @boxed_simple
    call void @boxed_simple({} addrspace(10)* %aboxed,
                            {} addrspace(10)* %bboxed)
; CHECK-NEXT: call void @julia.pop_gc_frame(ptr %gcframe)
    ret void
}

define {} addrspace(10)* @gc_alloc_lowering() {
top:
; CHECK-LABEL: @gc_alloc_lowering
    %pgcstack = call {}*** @julia.get_pgcstack()
    %0 = bitcast {}*** %pgcstack to {}**
    %current_task = getelementptr inbounds {}*, {}** %0, i64 -12

; CHECK: %current_task = getelementptr inbounds ptr, ptr %0, i64 -12
; CHECK-NEXT: [[ptls_field:%.*]] = getelementptr inbounds i8, ptr %current_task,
; CHECK-NEXT: [[ptls_load:%.*]] = load ptr, ptr [[ptls_field]], align 8, !tbaa !0
; CHECK-NEXT: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes(ptr [[ptls_load]], [[SIZE_T:i.[0-9]+]] 8, i64 {{.*}} @tag {{.*}})
; CHECK-NEXT: [[V_HEADROOM:%.*]] = getelementptr inbounds ptr addrspace(10), ptr addrspace(10) %v, i64 -1
; CHECK-NEXT: store atomic ptr addrspace(10) @tag, ptr addrspace(10) [[V_HEADROOM]] unordered, align 8, !tbaa !4
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
; CHECK-NEXT: ret ptr addrspace(10) %v
    ret {} addrspace(10)* %v
}

; Confirm that loadedval instruction does not contain invariant.load metadata
; after the gc placement pass, but still contains the range metadata.
; Since loadedval is marked invariant, passes are allowed to move the use.
; But after the placement pass, must ensure it won't be relocated after our
; last gc-root use
define void @gc_drop_aliasing() {
top:
; CHECK-LABEL: @gc_drop_aliasing
    %pgcstack = call {}*** @julia.get_pgcstack()
    %0 = bitcast {}*** %pgcstack to {}**
    %current_task = getelementptr inbounds {}*, {}** %0, i64 -12

; CHECK: %current_task = getelementptr inbounds ptr, ptr %0, i64 -12
; CHECK-NEXT: [[ptls_field:%.*]] = getelementptr inbounds i8, ptr %current_task,
; CHECK-NEXT: [[ptls_load:%.*]] = load ptr, ptr [[ptls_field]], align 8, !tbaa !0
; CHECK-NEXT: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes(ptr [[ptls_load]], [[SIZE_T:i.[0-9]+]] 8, i64 {{.*}} @tag {{.*}})
; CHECK-NEXT: [[V_HEADROOM:%.*]] = getelementptr inbounds ptr addrspace(10), ptr addrspace(10) %v, i64 -1
; CHECK-NEXT: store atomic ptr addrspace(10) @tag, ptr addrspace(10) [[V_HEADROOM]] unordered, align 8, !tbaa !4
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
; CHECK-NEXT: %v64 = bitcast ptr addrspace(10) %v to ptr addrspace(10)
    %v64 = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
; CHECK-NEXT: %loadedval = load i64, ptr addrspace(10) %v64, align 8, !range !7
    %loadedval = load i64, i64 addrspace(10)* %v64, align 8, !range !0, !invariant.load !1
; CHECK-NEXT: store i64 %loadedval, ptr addrspace(10) %v64, align 8, !noalias !8
    store i64 %loadedval, i64 addrspace(10)* %v64, align 8, !noalias !2
; CHECK-NEXT: %lv2 = load i64, ptr addrspace(10) %v64, align 8, !tbaa !11, !range !7
    %lv2 = load i64, i64 addrspace(10)* %v64, align 8, !range !0, !tbaa !4
; CHECK-NEXT: ret void
    ret void
}

; Confirm that `invariant.load` on other loads survive
define void @gc_keep_invariant(float addrspace(1)* %0) {
top:
; CHECK-LABEL: @gc_keep_invariant
    %pgcstack = call {}*** @julia.get_pgcstack()
    %1 = bitcast {}*** %pgcstack to {}**
    %current_task = getelementptr inbounds {}*, {}** %1, i64 -12

; CHECK: %current_task = getelementptr inbounds ptr, ptr %1, i64 -12
    %2 = load float, ptr addrspace(1) %0, align 4, !invariant.load !1
; CHECK-NEXT: %2 = load float, ptr addrspace(1) %0, align 4, !invariant.load
    ret void
}

define i32 @callee_root({} addrspace(10)* %v0, {} addrspace(10)* %v1) {
top:
; CHECK-LABEL: @callee_root
; CHECK-NOT: @julia.new_gc_frame
  %v2 = call {}*** @julia.get_pgcstack()
  %v3 = bitcast {} addrspace(10)* %v0 to {} addrspace(10)* addrspace(10)*
  %v4 = addrspacecast {} addrspace(10)* addrspace(10)* %v3 to {} addrspace(10)* addrspace(11)*
  %v5 = load atomic {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %v4 unordered, align 8
  %v6 = bitcast {} addrspace(10)* %v1 to {} addrspace(10)* addrspace(10)*
  %v7 = addrspacecast {} addrspace(10)* addrspace(10)* %v6 to {} addrspace(10)* addrspace(11)*
  %v8 = load atomic {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %v7 unordered, align 8
  %v9 = addrspacecast {} addrspace(10)* %v5 to {} addrspace(12)*
  %v10 = addrspacecast {} addrspace(10)* %v8 to {} addrspace(12)*
  %v11 = call i32 @rooting_callee({} addrspace(12)* %v9, {} addrspace(12)* %v10)
  ret i32 %v11
; CHECK: ret i32
}

define i32 @freeze({} addrspace(10)* %v0, {} addrspace(10)* %v1) {
top:
; CHECK-LABEL: @freeze
; CHECK-NOT: @julia.new_gc_frame
  %v2 = call {}*** @julia.get_pgcstack()
  %v3 = bitcast {} addrspace(10)* %v0 to {} addrspace(10)* addrspace(10)*
  %v4 = addrspacecast {} addrspace(10)* addrspace(10)* %v3 to {} addrspace(10)* addrspace(11)*
  %v5 = load atomic {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %v4 unordered, align 8
  %v6 = bitcast {} addrspace(10)* %v1 to {} addrspace(10)* addrspace(10)*
  %v7 = addrspacecast {} addrspace(10)* addrspace(10)* %v6 to {} addrspace(10)* addrspace(11)*
  %v8 = load atomic {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %v7 unordered, align 8
  %fv8 = freeze {} addrspace(10)* %v8
  %v9 = addrspacecast {} addrspace(10)* %v5 to {} addrspace(12)*
  %v10 = addrspacecast {} addrspace(10)* %fv8 to {} addrspace(12)*
  %v11 = call i32 @rooting_callee({} addrspace(12)* %v9, {} addrspace(12)* %v10)
  ret i32 %v11
; CHECK: ret i32
}

; COM: the bugs here may be caught by death-by-verify-assertion
define {} addrspace(10)* @gclift_switch({} addrspace(13)* addrspace(10)* %input, i1 %unpredictable) {
  top:
  %0 = call {}*** @julia.get_pgcstack()
  br i1 %unpredictable, label %mid1, label %mid2
  mid1:
  br label %mid2
  mid2:
  %root = phi {} addrspace(13)* addrspace(10)* [ %input, %top ], [ %input, %mid1 ]
  %unrelated = phi i1 [ %unpredictable, %top ], [ %unpredictable, %mid1 ]
  %1 = addrspacecast {} addrspace(13)* addrspace(10)* %root to {} addrspace(13)* addrspace(11)*
  %2 = bitcast {} addrspace(13)* addrspace(11)* %1 to {} addrspace(11)*
  switch i1 %unpredictable, label %end [
    i1 1, label %end
    i1 0, label %end
  ]
  end:
  %phi = phi {} addrspace(11)* [ %2, %mid2 ], [ %2, %mid2 ], [ %2, %mid2 ]
  %ret = bitcast {} addrspace(13)* addrspace(10)* %input to {} addrspace(10)*
  ; CHECK: %gclift
  ret {} addrspace(10)* %ret
}

; Shouldn't hang
define void @vector_insert(<4 x {} addrspace(10)* > %0, <2 x {} addrspace(10)* > %1) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %2 = call <4 x {} addrspace(10)*> @llvm.vector.insert.v4p10.v2p10(<4 x {} addrspace(10)*> %0, <2 x {} addrspace(10)*> %1, i64 2)
  ret void
}

define void @vector_extract(<4 x {} addrspace(10)* > %0, <2 x {} addrspace(10)* > %1) {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %2 = call <2 x {} addrspace(10)*> @llvm.vector.extract.v2p10.v4p10(<4 x {} addrspace(10)* > %0, i64 2)
  ret void
}

define void @decayar([2 x {} addrspace(10)* addrspace(11)*] %ar) {
  %v2 = call {}*** @julia.get_pgcstack()
  %e0 = extractvalue [2 x {} addrspace(10)* addrspace(11)*] %ar, 0
  %l0 = load {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %e0
  %e1 = extractvalue [2 x {} addrspace(10)* addrspace(11)*] %ar, 1
  %l1 = load {} addrspace(10)*, {} addrspace(10)* addrspace(11)* %e1
  %r = call i32 @callee_root({} addrspace(10)* %l0, {} addrspace(10)* %l1)
  ret void
}

; CHECK-LABEL: @decayar

; CHECK:  %gcframe = call ptr @julia.new_gc_frame(i32 2)
; CHECK: [[gc_slot_addr_:%.*]]1 = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 1)
; CHECK:  store ptr addrspace(10) %l0, ptr [[gc_slot_addr_:%.*]], align 8
; CHECK:  [[gc_slot_addr_:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 0)
; CHECK: store ptr addrspace(10) %l1, ptr [[gc_slot_addr_:%.*]], align 8
; CHECK: %r = call i32 @callee_root(ptr addrspace(10) %l0, ptr addrspace(10) %l1)
; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)

!0 = !{i64 0, i64 23}
!1 = !{!1}
!2 = !{!7} ; scope list
!3 = !{!4, !4, i64 0, i64 1}
!4 = !{!"jtbaa_const", !5}
!5 = !{!"jtbaa"}
!6 = distinct !{!6} ; alias domain
!7 = distinct !{!7, !6} ; alias scope


; CHECK:      !0 = !{!1, !1, i64 0}
; CHECK-NEXT: !1 = !{!"jtbaa_gcframe", !2, i64 0}
; CHECK-NEXT: !2 = !{!"jtbaa", !3, i64 0}
; CHECK-NEXT: !3 = !{!"jtbaa"}
; CHECK-NEXT: !4 = !{!5, !5, i64 0}
; CHECK-NEXT: !5 = !{!"jtbaa_tag", !6, i64 0}
; CHECK-NEXT: !6 = !{!"jtbaa_data", !2, i64 0}
; CHECK-NEXT: !7 = !{i64 0, i64 23}
; CHECK-NEXT: !8 = !{!9}
; CHECK-NEXT: !9 = distinct !{!9, !10}
; CHECK-NEXT: !10 = distinct !{!10}
; CHECK-NEXT: !11 = !{!12, !12, i64 0}
; CHECK-NEXT: !12 = !{!"jtbaa_const", !3}

