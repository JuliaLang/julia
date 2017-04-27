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

; Checks for ugly phi corner case
define void @phi_corner_case(i64 %a, i64 %b, i64 %c, i64 %d) {
top:
; CHECK-LABEL: @phi_corner_case
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 6
; Currently each value gets stored twice, once to the gc frame, once to the
; jlcall frame. Future optimizations may change that, which would require appropriate
; adjustments to this test.
; CHECK: store %jl_value_t addrspace(10)* %aboxed
; CHECK: store %jl_value_t addrspace(10)* %aboxed
; CHECK: store %jl_value_t addrspace(10)* %bboxed
; CHECK: store %jl_value_t addrspace(10)* %bboxed
; CHECK: store %jl_value_t addrspace(10)* %cboxed
; CHECK: store %jl_value_t addrspace(10)* %cboxed
; CHECK: store %jl_value_t addrspace(10)* %dboxed
; CHECK: store %jl_value_t addrspace(10)* %dboxed
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %frame1 = alloca %jl_value_t addrspace(10)*, i32 2
    %frame11 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %frame1, i32 1
    %frame2 = alloca %jl_value_t addrspace(10)*, i32 2
    %frame21 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %frame2, i32 1
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** %frame1
    store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** %frame11
    %cboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %c)
    %dboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %d)
    store %jl_value_t addrspace(10)* %cboxed, %jl_value_t addrspace(10)** %frame2
    store %jl_value_t addrspace(10)* %dboxed, %jl_value_t addrspace(10)** %frame21
    %cmp = icmp eq i64 %a, %b
    br i1 %cmp, label %alabel, label %blabel
alabel:
    br label %common
blabel:
    br label %common
common:
    %callframe = phi %jl_value_t addrspace(10)** [ %frame1, %alabel], [ %frame2, %blabel ]
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %callframe, i32 2)
    ret void
}

define void @phi_corner_case2(i64 %a, i64 %b, i64 %c, i64 %d) {
; CHECK-LABEL: @phi_corner_case2
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 6
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %frame1 = alloca %jl_value_t addrspace(10)*, i32 2
    %frame11 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %frame1, i32 1
    %frame2 = alloca %jl_value_t addrspace(10)*, i32 2
    %frame21 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %frame2, i32 1
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** %frame1
    store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** %frame11
    %cmp = icmp eq i64 %a, %b
    br i1 %cmp, label %alabel, label %blabel
alabel:
    br label %common
blabel:
    %cboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %c)
; cboxed gets two stores (one to gcroot it for the boxing of d)
; CHECK: store %jl_value_t addrspace(10)* %cboxed
; CHECK: store %jl_value_t addrspace(10)* %cboxed
; CHECK: store %jl_value_t addrspace(10)* %dboxed
    %dboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %d)
    store %jl_value_t addrspace(10)* %cboxed, %jl_value_t addrspace(10)** %frame2
    store %jl_value_t addrspace(10)* %dboxed, %jl_value_t addrspace(10)** %frame21
    br label %common
common:
; CHECK-DAG: [[LIFT1:%.*]] = phi %jl_value_t addrspace(10)* [ null, %alabel ], [ %cboxed, %blabel ]
; CHECK-DAG: [[LIFT2:%.*]] = phi %jl_value_t addrspace(10)* [ null, %alabel ], [ %dboxed, %blabel ]
; CHECK-DAG: store %jl_value_t addrspace(10)* [[LIFT2]]
; CHECK-DAG: store %jl_value_t addrspace(10)* [[LIFT1]]
    %callframe = phi %jl_value_t addrspace(10)** [ %frame1, %alabel], [ %frame2, %blabel ]
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %callframe, i32 2)
    ret void
}

define void @underlying_object(i64 %a, i64 %b) {
; CHECK-LABEL: @underlying_object
; We need to root both values here. The observed failure case was that we'd
; only consider stores to the gep passed to the call rather than all stores
; to the alloca.
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %jlcall = alloca [2 x %jl_value_t addrspace(10)*], align 8
    %jlcall.sub = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 0
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** %jlcall.sub
    %bslot = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 1
    store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** %bslot
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall.sub, i32 2)
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

declare void @llvm.lifetime.start.p0p10s_jl_value_ts(i64, %jl_value_t addrspace(10)**)
declare void @llvm.lifetime.end.p0p10s_jl_value_ts(i64, %jl_value_t addrspace(10)**)

; The system shouldn't really be generating things like this, but we can get unlucky
define void @weird_alloca(i1 %cnd, i64 %a, i64 %b) {
; CHECK-LABEL: weird_alloca
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %jlcall = alloca [2 x %jl_value_t addrspace(10)*], align 8
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %jlcall.sub = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 0
    %jlcall1 = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 1
    store %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall.sub
    store %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall1
    br i1 %cnd, label %if, label %done

if:
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** %jlcall.sub
    store %jl_value_t addrspace(10)* %bboxed, %jl_value_t addrspace(10)** %jlcall1
    br label %done

done:
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall.sub, i32 2)
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

define void @vector_ops(%jl_value_t addrspace(10)* %a, %jl_value_t addrspace(10)** %b, i32 %c) {
; CHECK-LABEL: @vector_ops
; N.B.: This should be 4, but it's currently not realizing that %a doesn't need a root
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 5
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %jlcall = alloca [3 x %jl_value_t addrspace(10)*], align 8
    %jlcall.sub = getelementptr inbounds [3 x %jl_value_t addrspace(10)*], [3 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 0
    %casted = bitcast %jl_value_t addrspace(10)** %b to <2 x %jl_value_t addrspace(10)*>*
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*>* %casted
    %jlcall.casted = bitcast [3 x %jl_value_t addrspace(10)*]* %jlcall to <2 x %jl_value_t addrspace(10)*>*
    store <2 x %jl_value_t addrspace(10)*> %loaded, <2 x %jl_value_t addrspace(10)*>* %jlcall.casted
    %third = getelementptr inbounds [3 x %jl_value_t addrspace(10)*], [3 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 2
    store %jl_value_t addrspace(10)* %a, %jl_value_t addrspace(10)** %third
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* %a, %jl_value_t addrspace(10)** %jlcall.sub, i32 3)
    ret void
}
