; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -LateLowerGCFrame -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

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
; CHECK: %gcframe = call {} addrspace(10)** @julia.new_gc_frame(i32 2)
; CHECK:  %pgcstack = call {}*** @julia.get_pgcstack()
    %pgcstack = call {}*** @julia.get_pgcstack()
; CHECK-NEXT: call void @julia.push_gc_frame({} addrspace(10)** %gcframe, i32 2)
; CHECK-NEXT: call {} addrspace(10)* @jl_box_int64
    %aboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: [[GEP0:%.*]] = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]])
; CHECK-NEXT: store {} addrspace(10)* %aboxed, {} addrspace(10)** [[GEP0]]
    %bboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %b)
; CHECK-NEXT: %bboxed =
; Make sure the same gc slot isn't re-used
; CHECK-NOT: call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 [[GEPSLOT0]])
; CHECK: [[GEP1:%.*]] = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 [[GEPSLOT1:[0-9]+]])
; CHECK-NEXT: store {} addrspace(10)* %bboxed, {} addrspace(10)** [[GEP1]]
; CHECK-NEXT: call void @boxed_simple
    call void @boxed_simple({} addrspace(10)* %aboxed,
                            {} addrspace(10)* %bboxed)
; CHECK-NEXT: call void @julia.pop_gc_frame({} addrspace(10)** %gcframe)
    ret void
}

define {} addrspace(10)* @gc_alloc_lowering() {
top:
; CHECK-LABEL: @gc_alloc_lowering
    %pgcstack = call {}*** @julia.get_pgcstack()
    %0 = bitcast {}*** %pgcstack to {}**
    %current_task = getelementptr inbounds {}*, {}** %0, i64 -12
; CHECK: %current_task = getelementptr inbounds {}*, {}** %0, i64 -12
; CHECK-NEXT: [[ptls_field:%.*]] = getelementptr inbounds {}*, {}** %current_task, i64 15
; CHECK-NEXT: [[ptls_load:%.*]] = load {}*, {}** [[ptls_field]], align 8, !tbaa !0
; CHECK-NEXT: [[ppjl_ptls:%.*]] = bitcast {}* [[ptls_load]] to {}**
; CHECK-NEXT: [[ptls_i8:%.*]] = bitcast {}** [[ppjl_ptls]] to i8*
; CHECK-NEXT: %v = call {} addrspace(10)* @julia.gc_alloc_bytes(i8* [[ptls_i8]], [[SIZE_T:i.[0-9]+]] 8)
; CHECK-NEXT: [[V2:%.*]] = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
; CHECK-NEXT: [[V_HEADROOM:%.*]] = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)* addrspace(10)* [[V2]], i64 -1
; CHECK-NEXT: store atomic {} addrspace(10)* @tag, {} addrspace(10)* addrspace(10)* [[V_HEADROOM]] unordered, align 8, !tbaa !4
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
; CHECK-NEXT: ret {} addrspace(10)* %v
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
; CHECK: %current_task = getelementptr inbounds {}*, {}** %0, i64 -12
; CHECK-NEXT: [[ptls_field:%.*]] = getelementptr inbounds {}*, {}** %current_task, i64 15
; CHECK-NEXT: [[ptls_load:%.*]] = load {}*, {}** [[ptls_field]], align 8, !tbaa !0
; CHECK-NEXT: [[ppjl_ptls:%.*]] = bitcast {}* [[ptls_load]] to {}**
; CHECK-NEXT: [[ptls_i8:%.*]] = bitcast {}** [[ppjl_ptls]] to i8*
; CHECK-NEXT: %v = call {} addrspace(10)* @julia.gc_alloc_bytes(i8* [[ptls_i8]], [[SIZE_T:i.[0-9]+]] 8)
; CHECK-NEXT: [[V2:%.*]] = bitcast {} addrspace(10)* %v to {} addrspace(10)* addrspace(10)*
; CHECK-NEXT: [[V_HEADROOM:%.*]] = getelementptr inbounds {} addrspace(10)*, {} addrspace(10)* addrspace(10)* [[V2]], i64 -1
; CHECK-NEXT: store atomic {} addrspace(10)* @tag, {} addrspace(10)* addrspace(10)* [[V_HEADROOM]] unordered, align 8, !tbaa !4
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
; CHECK-NEXT: %v64 = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
    %v64 = bitcast {} addrspace(10)* %v to i64 addrspace(10)*
; CHECK-NEXT: %loadedval = load i64, i64 addrspace(10)* %v64, align 8, !range !7
    %loadedval = load i64, i64 addrspace(10)* %v64, align 8, !range !0, !invariant.load !1
; CHECK-NEXT: store i64 %loadedval, i64 addrspace(10)* %v64, align 8, !noalias !8
    store i64 %loadedval, i64 addrspace(10)* %v64, align 8, !noalias !2
; CHECK-NEXT: %lv2 = load i64, i64 addrspace(10)* %v64, align 8, !tbaa !9, !range !7
    %lv2 = load i64, i64 addrspace(10)* %v64, align 8, !range !0, !tbaa !4
; CHECK-NEXT: ret void
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

!0 = !{i64 0, i64 23}
!1 = !{}
!2 = distinct !{!2}
!3 = !{!4, !4, i64 0, i64 1}
!4 = !{!"jtbaa_const", !5}
!5 = !{!"jtbaa"}

; CHECK:      !0 = !{!1, !1, i64 0}
; CHECK-NEXT: !1 = !{!"jtbaa_gcframe", !2, i64 0}
; CHECK-NEXT: !2 = !{!"jtbaa", !3, i64 0}
; CHECK-NEXT: !3 = !{!"jtbaa"}
; CHECK-NEXT: !4 = !{!5, !5, i64 0}
; CHECK-NEXT: !5 = !{!"jtbaa_tag", !6, i64 0}
; CHECK-NEXT: !6 = !{!"jtbaa_data", !2, i64 0}
; CHECK-NEXT: !7 = !{i64 0, i64 23}
; CHECK-NEXT: !8 = distinct !{!8}
