; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -S %s | FileCheck %s

%jl_value_t = type opaque
@tag = external addrspace(10) global %jl_value_t

declare void @boxed_simple(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)
declare %jl_value_t*** @julia.ptls_states()
declare void @jl_safepoint()
declare %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32)
declare noalias nonnull %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8*, i64, %jl_value_t addrspace(10)*)

define void @gc_frame_lowering(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @gc_frame_lowering
; CHECK: %gcframe = call %jl_value_t addrspace(10)** @julia.new_gc_frame(i32 2)
    %ptls = call %jl_value_t*** @julia.ptls_states()
; CHECK: %ptls = call %jl_value_t*** @julia.ptls_states()
; CHECK-NEXT: call void @julia.push_gc_frame(%jl_value_t addrspace(10)** %gcframe, i32 2)
; CHECK-NEXT: call %jl_value_t addrspace(10)* @jl_box_int64
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: [[GEP0:%.*]] = call %jl_value_t addrspace(10)** @julia.get_gc_frame_slot(%jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]])
; CHECK-NEXT: store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** [[GEP0]]
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
; CHECK-NEXT: %bboxed =
; Make sure the same gc slot isn't re-used
; CHECK-NOT: call %jl_value_t addrspace(10)** @julia.get_gc_frame_slot(%jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0]])
; CHECK: [[GEP1:%.*]] = call %jl_value_t addrspace(10)** @julia.get_gc_frame_slot(%jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT1:[0-9]+]])
; CHECK-NEXT: store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** [[GEP1]]
; CHECK-NEXT: call void @boxed_simple
    call void @boxed_simple(%jl_value_t addrspace(10)* %aboxed,
                            %jl_value_t addrspace(10)* %bboxed)
; CHECK-NEXT: call void @julia.pop_gc_frame(%jl_value_t addrspace(10)** %gcframe)
    ret void
}

define %jl_value_t addrspace(10)* @gc_alloc_lowering() {
top:
; CHECK-LABEL: @gc_alloc_lowering
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %ptls_i8 = bitcast %jl_value_t*** %ptls to i8*
; CHECK: %v = call %jl_value_t addrspace(10)* @julia.gc_alloc_bytes(i8* %ptls_i8, [[SIZE_T:i.[0-9]+]] 8)
; CHECK-NEXT: [[V2:%.*]] = bitcast %jl_value_t addrspace(10)* %v to %jl_value_t addrspace(10)* addrspace(10)*
; CHECK-NEXT: [[V_HEADROOM:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)* addrspace(10)* [[V2]], i64 -1
; CHECK-NEXT: store %jl_value_t addrspace(10)* @tag, %jl_value_t addrspace(10)* addrspace(10)* [[V_HEADROOM]], !tbaa !0
    %v = call noalias %jl_value_t addrspace(10)* @julia.gc_alloc_obj(i8* %ptls_i8, i64 8, %jl_value_t addrspace(10)* @tag)
; CHECK-NEXT: ret %jl_value_t addrspace(10)* %v
    ret %jl_value_t addrspace(10)* %v
}
