; RUN: opt -load libjulia.so -LateLowerGCFrame -S %s | FileCheck %s

%jl_value_t = type opaque

declare void @boxed_simple(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)
declare %jl_value_t*** @jl_get_ptls_states()
declare %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32)

define void @simple(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @simple
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
; CHECK: call %jl_value_t addrspace(10)* @jl_box_int64
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]]
; CHECK-NEXT: store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** [[GEP0]]
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
; CHECK-NEXT: %bboxed =
; Make sure the same gc slot isn't re-used
; CHECK-NOT: getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0]]
; CHECK: [[GEP1:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT1:[0-9]+]]
; CHECK-NEXT: store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** [[GEP1]]
; CHECK-NEXT: call void @boxed_simple
    call void @boxed_simple(%jl_value_t addrspace(10)* %aboxed,
                            %jl_value_t addrspace(10)* %bboxed)
    ret void
}

define void @leftover_alloca(%jl_value_t addrspace(10)*%a) {
; If this pass encounters an alloca, it'll just sink it into the gcframe,
; relying on mem2reg to catch simple cases such as this earlier
; CHECK-LABEL: @leftover_alloca
; CHECK: %var = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %var = alloca %jl_value_t addrspace(10)*
    store %jl_value_t addrspace(10)* %a, %jl_value_t addrspace(10)** %var
    %b = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %var
    call void @boxed_simple(%jl_value_t addrspace(10)* %a,
                            %jl_value_t addrspace(10)* %b)
    ret void
}

declare {%jl_value_t addrspace(10)*, i8} @union_ret()
declare void @union_arg({%jl_value_t addrspace(10)*, i8})

define void @simple_union() {
; CHECK-LABEL: @simple_union
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
; CHECK: %a = call { %jl_value_t addrspace(10)*, i8 } @union_ret()
    %a = call { %jl_value_t addrspace(10)*, i8 } @union_ret()
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]]
; CHECK-NEXT: [[EXTRACT:%.*]] = extractvalue { %jl_value_t addrspace(10)*, i8 } %a, 0
; CHECK-NEXT: store %jl_value_t addrspace(10)* [[EXTRACT]], %jl_value_t addrspace(10)** [[GEP0]]
    call void @union_arg({%jl_value_t addrspace(10)*, i8} %a)
    ret void
}

declare void @one_arg_boxed(%jl_value_t addrspace(10)*)

define void @select_simple(i64 %a, i64 %b) {
; CHECK-LABEL: @select_simple
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    %cmp = icmp eq i64 %a, %b
    %selectb = select i1 %cmp, %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)* %bboxed
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %selectb)
    ret void
}

define void @phi_simple(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @phi_simple
; CHECK:   %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %cmp = icmp eq i64 %a, %b
    br i1 %cmp, label %alabel, label %blabel
alabel:
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    br label %common
blabel:
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    br label %common
common:
    %phi = phi %jl_value_t addrspace(10)* [ %aboxed, %alabel ], [ %bboxed, %blabel ]
; CHECK:  [[GEP:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 2
; CHECK:  store %jl_value_t addrspace(10)* %phi, %jl_value_t addrspace(10)** [[GEP]]
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %phi)
    ret void
}

declare void @one_arg_decayed(i64 addrspace(12)*)

define void @select_lift(i64 %a, i64 %b) {
; CHECK-LABEL: @select_lift
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %adecayed = addrspacecast %jl_value_t addrspace(10)* %aboxed to i64 addrspace(12)*
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    %bdecayed = addrspacecast %jl_value_t addrspace(10)* %bboxed to i64 addrspace(12)*
    %cmp = icmp eq i64 %a, %b
; CHECK: %gclift = select i1 %cmp, %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)* %bboxed
    %selectb = select i1 %cmp, i64 addrspace(12)* %adecayed, i64 addrspace(12)* %bdecayed
    call void @one_arg_decayed(i64 addrspace(12)* %selectb)
    ret void
}

define void @phi_lift(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @phi_lift
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %cmp = icmp eq i64 %a, %b
    br i1 %cmp, label %alabel, label %blabel
alabel:
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %adecayed = addrspacecast %jl_value_t addrspace(10)* %aboxed to i64 addrspace(12)*
    br label %common
blabel:
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    %bdecayed = addrspacecast %jl_value_t addrspace(10)* %bboxed to i64 addrspace(12)*
    br label %common
common:
    %phi = phi i64 addrspace(12)* [ %adecayed, %alabel ], [ %bdecayed, %blabel ]
    call void @one_arg_decayed(i64 addrspace(12)* %phi)
    ret void
}

define void @live_if_live_out(i64 %a, i64 %b) {
; CHECK-LABEL: @live_if_live_out
top:
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
; The failure case is failing to realize that `aboxed` is live across the first
; one_arg_boxed safepoint and putting bboxed in the same root slot
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %bboxed)
    br label %succ
succ:
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %aboxed)
    ret void
}

; A ret is a use - make sure the value is kept alive for any intervening
; safepoint
define %jl_value_t addrspace(10)* @ret_use(i64 %a, i64 %b) {
; CHECK-LABEL: @ret_use
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    ret %jl_value_t addrspace(10)* %aboxed
}
