; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -S %s | FileCheck %s

%jl_value_t = type opaque

declare void @boxed_simple(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)
declare %jl_value_t*** @julia.ptls_states()
declare void @jl_safepoint()
declare %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32)

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
