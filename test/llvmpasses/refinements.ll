; RUN: opt -load libjulia.so -LateLowerGCFrame -S %s | FileCheck %s

%jl_value_t = type opaque

declare %jl_value_t*** @jl_get_ptls_states()
declare void @jl_safepoint()
declare void @one_arg_boxed(%jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)

define void @argument_refinement(%jl_value_t addrspace(10)* %a) {
; CHECK-LABEL: @argument_refinement
; CHECK-NOT: %gcframe
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %casted1 = bitcast %jl_value_t addrspace(10)* %a to %jl_value_t addrspace(10)* addrspace(10)*
    %loaded1 = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)* addrspace(10)* %casted1, !tbaa !1
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    ret void
}

; Check that we reuse the gc slot from the box
define void @heap_refinement1(i64 %a) {
; CHECK-LABEL: @heap_refinement1
; CHECK:   %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast %jl_value_t addrspace(10)* %aboxed to %jl_value_t addrspace(10)* addrspace(10)*
    %loaded1 = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)* addrspace(10)* %casted1, !tbaa !1
; CHECK: store %jl_value_t addrspace(10)* %aboxed
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %aboxed)
    ret void
}

; Check that we don't root the allocated value here, just the derived value
define void @heap_refinement2(i64 %a) {
; CHECK-LABEL: @heap_refinement2
; CHECK:   %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast %jl_value_t addrspace(10)* %aboxed to %jl_value_t addrspace(10)* addrspace(10)*
    %loaded1 = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)* addrspace(10)* %casted1, !tbaa !1
; CHECK: store %jl_value_t addrspace(10)* %loaded1
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    ret void
}

!0 = !{!"jtbaa"}
!1 = !{!2, !2, i64 0}
!2 = !{!"jtbaa_immut", !0, i64 0}
