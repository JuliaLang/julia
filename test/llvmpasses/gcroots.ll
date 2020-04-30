; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -FinalLowerGC -S %s | FileCheck %s

%jl_value_t = type opaque

declare void @boxed_simple(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)
declare %jl_value_t*** @julia.ptls_states()
declare void @jl_safepoint()
declare %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32)

define void @simple(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @simple
    %ptls = call %jl_value_t*** @julia.ptls_states()
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

define void @leftover_alloca(%jl_value_t addrspace(10)* %a) {
; If this pass encounters an alloca, it'll just sink it into the gcframe,
; relying on mem2reg to catch simple cases such as this earlier
; CHECK-LABEL: @leftover_alloca
; CHECK: %var = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
; CHECK: %gclift = phi %jl_value_t addrspace(10)* [ %aboxed, %alabel ], [ %bboxed, %blabel ], [ %gclift, %common ]
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %phi = phi i64 addrspace(12)* [ %adecayed, %alabel ], [ %bdecayed, %blabel ], [ %phi, %common ]
    call void @one_arg_decayed(i64 addrspace(12)* %phi)
    br label %common
}


define void @phi_lift_union(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @phi_lift_union
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %cmp = icmp eq i64 %a, %b
    br i1 %cmp, label %alabel, label %blabel
alabel:
    %u = call { %jl_value_t addrspace(10)*, i8 } @union_ret()
; CHECK: %aboxed = extractvalue { %jl_value_t addrspace(10)*, i8 } %u, 0
    %aboxed = extractvalue { %jl_value_t addrspace(10)*, i8 } %u, 0
    %adecayed = addrspacecast %jl_value_t addrspace(10)* %aboxed to i64 addrspace(12)*
; CHECK: br label %common
    br label %common
blabel:
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    %bdecayed = addrspacecast %jl_value_t addrspace(10)* %bboxed to i64 addrspace(12)*
    br label %common
common:
; CHECK: %gclift = phi %jl_value_t addrspace(10)* [ %aboxed, %alabel ], [ %bboxed, %blabel ]
    %phi = phi i64 addrspace(12)* [ %adecayed, %alabel ], [ %bdecayed, %blabel ]
    call void @one_arg_decayed(i64 addrspace(12)* %phi)
    ret void
}

define void @live_if_live_out(i64 %a, i64 %b) {
; CHECK-LABEL: @live_if_live_out
top:
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
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
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    ret %jl_value_t addrspace(10)* %aboxed
}

define {%jl_value_t addrspace(10)*, i8} @ret_use_struct() {
; CHECK-LABEL: @ret_use_struct
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @julia.ptls_states()
; CHECK: %aunion = call { %jl_value_t addrspace(10)*, i8 } @union_ret()
    %aunion = call { %jl_value_t addrspace(10)*, i8 } @union_ret()
; CHECK-DAG: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]]
; CHECK-DAG: [[EXTRACT:%.*]] = extractvalue { %jl_value_t addrspace(10)*, i8 } %aunion, 0
; CHECK-NEXT: store %jl_value_t addrspace(10)* [[EXTRACT]], %jl_value_t addrspace(10)** [[GEP0]]
; CHECK-NEXT: call void @jl_safepoint()
    call void @jl_safepoint()
    ret {%jl_value_t addrspace(10)*, i8} %aunion
}


define i8 @nosafepoint(%jl_value_t addrspace(10)* dereferenceable(16)) {
; CHECK-LABEL: @nosafepoint
; CHECK-NOT: %gcframe
top:
  %1 = call %jl_value_t*** @julia.ptls_states()
  %2 = bitcast %jl_value_t*** %1 to %jl_value_t addrspace(10)**
  %3 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %2, i64 3
  %4 = bitcast %jl_value_t addrspace(10)** %3 to i64**
  %5 = load i64*, i64** %4
  %6 = bitcast %jl_value_t addrspace(10)* %0 to i8 addrspace(10)*
  %7 = addrspacecast i8 addrspace(10)* %6 to i8 addrspace(11)*
  %8 = getelementptr i8, i8 addrspace(11)* %7, i64 0
  %9 = load i8, i8 addrspace(11)* %8
  %10 = trunc i8 %9 to i1
  %11 = zext i1 %10 to i8
  %12 = xor i8 %11, 1
  ret i8 %12
}

define void @global_ref() {
; CHECK-LABEL: @global_ref
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %loaded = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** getelementptr (%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** inttoptr (i64 140540744325952 to %jl_value_t addrspace(10)**), i64 1)
; CHECK: store %jl_value_t addrspace(10)* %loaded, %jl_value_t addrspace(10)**
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %loaded)
    ret void
}

define %jl_value_t addrspace(10)* @no_redundant_rerooting(i64 %a, i1 %cond) {
; CHECK-LABEL: @no_redundant_rerooting
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed
; CHECK-NEXT: call void @jl_safepoint()
    call void @jl_safepoint()
    br i1 %cond, label %blocka, label %blockb
blocka:
; CHECK-NOT: call void @jl_safepoint()
; CHECK: call void @jl_safepoint()
    call void @jl_safepoint()
    ret %jl_value_t addrspace(10)* %aboxed
blockb:
; CHECK-NOT: call void @jl_safepoint()
; CHECK: call void @jl_safepoint()
    call void @jl_safepoint()
    ret %jl_value_t addrspace(10)* %aboxed
}

declare void @llvm.memcpy.p064.p10i8.i64(i64*, i8 addrspace(10)*, i64, i32, i1)

define void @memcpy_use(i64 %a, i64 *%aptr) {
; CHECK-LABEL: @memcpy_use
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed
    call void @jl_safepoint()
    %acast = bitcast %jl_value_t addrspace(10)* %aboxed to i8 addrspace(10)*
    call void @llvm.memcpy.p064.p10i8.i64(i64* %aptr, i8 addrspace(10)* %acast, i64 8, i32 1, i1 false)
    ret void
}

declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)

define void @gc_preserve(i64 %a) {
; CHECK-LABEL: @gc_preserve
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed
    call void @jl_safepoint()
    %tok = call token (...) @llvm.julia.gc_preserve_begin(%jl_value_t addrspace(10)* %aboxed)
    %aboxed2 = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed2
    call void @jl_safepoint()
    call void @llvm.julia.gc_preserve_end(token %tok)
    %aboxed3 = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
; CHECK: store %jl_value_t addrspace(10)* %aboxed3
    call void @jl_safepoint()
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %aboxed2)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %aboxed3)
    ret void
}

define void @gc_preserve_vec([2 x <2 x %jl_value_t addrspace(10)*>] addrspace(11)* nocapture nonnull readonly dereferenceable(16)) {
; CHECK-LABEL: @gc_preserve_vec
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 6
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %v = load [2 x <2 x %jl_value_t addrspace(10)*>], [2 x <2 x %jl_value_t addrspace(10)*>] addrspace(11)* %0, align 8
; CHECK-DAG: [[EXTRACT11:%.*]] = extractvalue [2 x <2 x %jl_value_t addrspace(10)*>] %v, 0
; CHECK-DAG: [[EXTRACT12:%.*]] = extractvalue [2 x <2 x %jl_value_t addrspace(10)*>] %v, 0
; CHECK-DAG: [[EXTRACT21:%.*]] = extractvalue [2 x <2 x %jl_value_t addrspace(10)*>] %v, 1
; CHECK-DAG: [[EXTRACT22:%.*]] = extractvalue [2 x <2 x %jl_value_t addrspace(10)*>] %v, 1
; CHECK-DAG: [[V11:%.*]] = extractelement <2 x %jl_value_t addrspace(10)*> [[EXTRACT11]], i32 0
; CHECK-DAG: [[V12:%.*]] = extractelement <2 x %jl_value_t addrspace(10)*> [[EXTRACT12]], i32 1
; CHECK-DAG: [[V21:%.*]] = extractelement <2 x %jl_value_t addrspace(10)*> [[EXTRACT21]], i32 0
; CHECK-DAG: [[V22:%.*]] = extractelement <2 x %jl_value_t addrspace(10)*> [[EXTRACT22]], i32 1
; CHECK-DAG: store %jl_value_t addrspace(10)* [[V11]]
; CHECK-DAG: store %jl_value_t addrspace(10)* [[V12]]
; CHECK-DAG: store %jl_value_t addrspace(10)* [[V21]]
; CHECK-DAG: store %jl_value_t addrspace(10)* [[V22]]
    %tok = call token (...) @llvm.julia.gc_preserve_begin([2 x <2 x %jl_value_t addrspace(10)*>] %v, i64 addrspace(10)* null, %jl_value_t*** %ptls)
    call void @jl_safepoint()
    ret void
}


@gv1 = external global %jl_value_t*
@gv2 = external global %jl_value_t addrspace(10)*

define %jl_value_t addrspace(10)* @gv_const() {
; CHECK-LABEL: @gv_const
; CHECK-NOT: %gcframe
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %v10 = load %jl_value_t*, %jl_value_t** @gv1, !tbaa !2
    %v1 = addrspacecast %jl_value_t* %v10 to %jl_value_t addrspace(10)*
    %v2 = load %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** @gv2, !tbaa !2
    call void @jl_safepoint()
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %v1)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %v2)
    ret %jl_value_t addrspace(10)* %v1
}

define %jl_value_t addrspace(10)* @vec_jlcallarg(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32) {
; CHECK-LABEL: @vec_jlcallarg
; CHECK-NOT: %gcframe
  %v4 = call %jl_value_t*** @julia.ptls_states()
  %v5 = bitcast %jl_value_t addrspace(10)** %1 to <2 x %jl_value_t addrspace(10)*>*
  %v6 = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*>* %v5, align 8
  %v7 = extractelement <2 x %jl_value_t addrspace(10)*> %v6, i32 0
  ret %jl_value_t addrspace(10)* %v7
}

declare %jl_value_t addrspace(10) *@alloc()

define %jl_value_t addrspace(10)* @vec_loadobj() {
; CHECK-LABEL: @vec_loadobj
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
  %v4 = call %jl_value_t*** @julia.ptls_states()
  %obj = call %jl_value_t addrspace(10) *@alloc()
  %v1 = bitcast %jl_value_t addrspace(10) * %obj to %jl_value_t addrspace(10)* addrspace(10)*
  %v5 = bitcast %jl_value_t addrspace(10)* addrspace(10)* %v1 to <2 x %jl_value_t addrspace(10)*> addrspace(10)*
  %v6 = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> addrspace(10)* %v5, align 8
  %obj2 = call %jl_value_t addrspace(10) *@alloc()
  %v7 = extractelement <2 x %jl_value_t addrspace(10)*> %v6, i32 0
  ret %jl_value_t addrspace(10)* %v7
}

define %jl_value_t addrspace(10)* @vec_gep() {
; CHECK-LABEL: @vec_gep
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
  %v4 = call %jl_value_t*** @julia.ptls_states()
  %obj = call %jl_value_t addrspace(10) *@alloc()
  %obj1 = bitcast %jl_value_t addrspace(10) * %obj to %jl_value_t addrspace(10)* addrspace(10)*
  %v1 = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)* addrspace(10)* %obj1, <2 x i32> < i32 0, i32 0 >
  call void @jl_safepoint()
  %obj2 = extractelement <2 x %jl_value_t addrspace(10)* addrspace(10)*> %v1, i32 0
  %obj3 = bitcast %jl_value_t addrspace(10)* addrspace(10)* %obj2 to %jl_value_t addrspace(10)*
  ret %jl_value_t addrspace(10)* %obj3
}

declare i1 @check_property(%jl_value_t addrspace(10)* %val)
define void @loopyness(i1 %cond1, %jl_value_t addrspace(10) *%arg) {
; CHECK-LABEL: @loopyness
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    br label %header

header:
    %phi = phi %jl_value_t addrspace(10)* [null, %top], [%obj, %latch]
    br i1 %cond1, label %a, label %latch

a:
; This needs a store
; CHECK-LABEL: a:
; CHECK:  [[GEP1:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0:[0-9]+]]
; CHECK:  store %jl_value_t addrspace(10)* %phi, %jl_value_t addrspace(10)** [[GEP1]]
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %phi)
    br label %latch

latch:
; This as well in case we went the other path
; CHECK:  [[GEP2:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 [[GEPSLOT0]]
; CHECK:  store %jl_value_t addrspace(10)* %phi, %jl_value_t addrspace(10)** [[GEP2]]
    %obj = call %jl_value_t addrspace(10)* @alloc()
    %cond = call i1 @check_property(%jl_value_t addrspace(10)* %phi)
    br i1 %cond, label %exit, label %header

exit:
    ret void
}

define %jl_value_t addrspace(10)* @phi_union(i1 %cond) {
; CHECK-LABEL: @phi_union
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  br i1 %cond, label %a, label %b

a:
  %obj = call %jl_value_t addrspace(10) *@alloc()
  %aobj = insertvalue {%jl_value_t addrspace(10)*, i8} undef, %jl_value_t addrspace(10)* %obj, 0
  %aunion = insertvalue {%jl_value_t addrspace(10)*, i8} undef, i8 -126, 1
  br label %join

b:
  %bunion = call {%jl_value_t addrspace(10)*, i8} @union_ret()
  br label %join

join:
  %phi = phi {%jl_value_t addrspace(10)*, i8} [%aunion, %a], [%bunion, %b]
  call void @jl_safepoint()
  %rval = extractvalue { %jl_value_t addrspace(10)*, i8 } %phi, 0
  ret %jl_value_t addrspace(10)* %rval
}

define %jl_value_t addrspace(10)* @select_union(i1 %cond) {
; CHECK-LABEL: @select_union
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %obj = call %jl_value_t addrspace(10) *@alloc()
  %aobj = insertvalue {%jl_value_t addrspace(10)*, i8} undef, %jl_value_t addrspace(10)* %obj, 0
  %aunion = insertvalue {%jl_value_t addrspace(10)*, i8} undef, i8 -126, 1
  %bunion = call {%jl_value_t addrspace(10)*, i8} @union_ret()
  %select = select i1 %cond, {%jl_value_t addrspace(10)*, i8} %aunion, {%jl_value_t addrspace(10)*, i8} %bunion
  call void @jl_safepoint()
  %rval = extractvalue { %jl_value_t addrspace(10)*, i8 } %select, 0
  ret %jl_value_t addrspace(10)* %rval
}

define i8 @simple_arrayptr() {
; CHECK-LABEL: @simple_arrayptr
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
   %ptls = call %jl_value_t*** @julia.ptls_states()
   %obj1 = call %jl_value_t addrspace(10) *@alloc()
   %obj2 = call %jl_value_t addrspace(10) *@alloc()
   %decayed = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11) *
   %arrayptrptr = bitcast %jl_value_t addrspace(11) *%decayed to i8 addrspace(13)* addrspace(11)*
   %arrayptr = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %arrayptrptr
   call void @jl_safepoint()
   call void @one_arg_boxed(%jl_value_t addrspace(10) *%obj2)
   %val = load i8, i8 addrspace(13)* %arrayptr
   ret i8 %val
}

define %jl_value_t addrspace(10)* @vecstoreload(<2 x %jl_value_t addrspace(10)*> *%arg) {
; CHECK-LABEL: @vecstoreload
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> *%arg
    call void @jl_safepoint()
    %obj = call %jl_value_t addrspace(10) *@alloc()
    %casted = bitcast %jl_value_t addrspace(10)* %obj to <2 x %jl_value_t addrspace(10)*> addrspace(10)*
    store <2 x %jl_value_t addrspace(10)*> %loaded, <2 x %jl_value_t addrspace(10)*> addrspace(10)* %casted
    ret %jl_value_t addrspace(10)* %obj
}

define void @vecphi(i1 %cond, <2 x %jl_value_t addrspace(10)*> *%arg) {
; CHECK-LABEL: @vecphi
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    br i1 %cond, label %A, label %B

A:
    br label %common

B:
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> *%arg
    call void @jl_safepoint()
    br label %common

common:
    %phi = phi <2 x %jl_value_t addrspace(10)*> [ zeroinitializer, %A ], [ %loaded, %B ]
    call void @jl_safepoint()
    %el1 = extractelement <2 x %jl_value_t addrspace(10)*> %phi, i32 0
    %el2 = extractelement <2 x %jl_value_t addrspace(10)*> %phi, i32 1
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %el1)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %el2)
    unreachable
}

define i8 @phi_arrayptr(i1 %cond) {
; CHECK-LABEL: @phi_arrayptr
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    br i1 %cond, label %A, label %B

A:
    %obj1 = call %jl_value_t addrspace(10) *@alloc()
    %obj2 = call %jl_value_t addrspace(10) *@alloc()
    %decayed1 = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11) *
    %arrayptrptr1 = bitcast %jl_value_t addrspace(11) *%decayed1 to i8 addrspace(13)* addrspace(11)*
    %arrayptr1 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %arrayptrptr1
    %decayed2 = addrspacecast %jl_value_t addrspace(10) *%obj2 to %jl_value_t addrspace(11) *
    %arrayptrptr2 = bitcast %jl_value_t addrspace(11) *%decayed2 to i8 addrspace(13)* addrspace(11)*
    %arrayptr2 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %arrayptrptr2
    %insert1 = insertelement <2 x i8 addrspace(13)*> undef, i8 addrspace(13)* %arrayptr1, i32 0
    %insert2 = insertelement <2 x i8 addrspace(13)*> %insert1, i8 addrspace(13)* %arrayptr2, i32 1
    call void @jl_safepoint()
    br label %common

B:
    br label %common

common:
; CHECK: %gclift
; CHECK: %gclift1
; CHECK-NOT: %gclift2
    %phi = phi <2 x i8 addrspace(13)*> [ %insert2, %A ], [ zeroinitializer, %B ]
    call void @jl_safepoint()
    %el1 = extractelement <2 x i8 addrspace(13)*> %phi, i32 0
    %el2 = extractelement <2 x i8 addrspace(13)*> %phi, i32 1
    %l1 = load i8, i8 addrspace(13)* %el1
    %l2 = load i8, i8 addrspace(13)* %el2
    %add = add i8 %l1, %l2
    ret i8 %add
}

define void @vecselect(i1 %cond, <2 x %jl_value_t addrspace(10)*> *%arg) {
; CHECK-LABEL: @vecselect
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> *%arg
    call void @jl_safepoint()
    %select = select i1 %cond, <2 x %jl_value_t addrspace(10)*> zeroinitializer, <2 x %jl_value_t addrspace(10)*> %loaded
    call void @jl_safepoint()
    %el1 = extractelement <2 x %jl_value_t addrspace(10)*> %select, i32 0
    %el2 = extractelement <2 x %jl_value_t addrspace(10)*> %select, i32 1
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %el1)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %el2)
    unreachable
}

define void @vecselect_lift(i1 %cond, <2 x %jl_value_t addrspace(10)*> *%arg) {
; CHECK-LABEL: @vecselect_lift
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> *%arg
    %decayed = addrspacecast <2 x %jl_value_t addrspace(10)*> %loaded to <2 x i64 addrspace(12)*>
    call void @jl_safepoint()
; CHECK: %gclift = select i1 %cond, %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)* %{{[0-9]+}}
    %select = select i1 %cond, <2 x i64 addrspace(12)*> zeroinitializer, <2 x i64 addrspace(12)*> %decayed
    call void @jl_safepoint()
    %el1 = extractelement <2 x i64 addrspace(12)*> %select, i32 0
    %el2 = extractelement <2 x i64 addrspace(12)*> %select, i32 1
    call void @one_arg_decayed(i64 addrspace(12)* %el1)
    call void @one_arg_decayed(i64 addrspace(12)* %el2)
    unreachable
}

define void @vecvecselect_lift(<2 x i1> %cond, <2 x %jl_value_t addrspace(10)*> *%arg) {
; CHECK-LABEL: @vecvecselect_lift
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %loaded = load <2 x %jl_value_t addrspace(10)*>, <2 x %jl_value_t addrspace(10)*> *%arg
    %decayed = addrspacecast <2 x %jl_value_t addrspace(10)*> %loaded to <2 x i64 addrspace(12)*>
    call void @jl_safepoint()
; CHECK: %gclift = select i1 %{{[0-9]+}}, %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)* %{{[0-9]+}}
    %select = select <2 x i1> %cond, <2 x i64 addrspace(12)*> zeroinitializer, <2 x i64 addrspace(12)*> %decayed
    call void @jl_safepoint()
    %el1 = extractelement <2 x i64 addrspace(12)*> %select, i32 0
    %el2 = extractelement <2 x i64 addrspace(12)*> %select, i32 1
    call void @one_arg_decayed(i64 addrspace(12)* %el1)
    call void @one_arg_decayed(i64 addrspace(12)* %el2)
    unreachable
}

define void @vecscalarselect_lift(<2 x i1> %cond, i64 %a) {
; CHECK-LABEL: @vecscalarselect_lift
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %adecayed = addrspacecast %jl_value_t addrspace(10)* %aboxed to i64 addrspace(12)*
    %avec = getelementptr i64, i64 addrspace(12)*  %adecayed, <2 x i32> zeroinitializer
    call void @jl_safepoint()
; CHECK: %gclift = select i1 %{{[0-9]+}}, %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)* %aboxed
    %select = select <2 x i1> %cond, <2 x i64 addrspace(12)*> zeroinitializer, <2 x i64 addrspace(12)*> %avec
    call void @jl_safepoint()
    %el1 = extractelement <2 x i64 addrspace(12)*> %select, i32 0
    %el2 = extractelement <2 x i64 addrspace(12)*> %select, i32 1
    call void @one_arg_decayed(i64 addrspace(12)* %el1)
    call void @one_arg_decayed(i64 addrspace(12)* %el2)
    unreachable
}

define void @scalarvecselect_lift(i1 %cond, i64 %a) {
; CHECK-LABEL: @scalarvecselect_lift
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %adecayed = addrspacecast %jl_value_t addrspace(10)* %aboxed to i64 addrspace(12)*
    %avec = getelementptr i64, i64 addrspace(12)*  %adecayed, <2 x i32> zeroinitializer
    call void @jl_safepoint()
; CHECK: %gclift = select i1 %cond, %jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)* %aboxed
    %select = select i1 %cond, <2 x i64 addrspace(12)*> zeroinitializer, <2 x i64 addrspace(12)*> %avec
    call void @jl_safepoint()
    %el1 = extractelement <2 x i64 addrspace(12)*> %select, i32 0
    %el2 = extractelement <2 x i64 addrspace(12)*> %select, i32 1
    call void @one_arg_decayed(i64 addrspace(12)* %el1)
    call void @one_arg_decayed(i64 addrspace(12)* %el2)
    unreachable
}

define i8 @select_arrayptr(i1 %cond) {
; CHECK-LABEL: @select_arrayptr
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %obj1 = call %jl_value_t addrspace(10) *@alloc()
    %obj2 = call %jl_value_t addrspace(10) *@alloc()
    %decayed1 = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11) *
    %arrayptrptr1 = bitcast %jl_value_t addrspace(11) *%decayed1 to i8 addrspace(13)* addrspace(11)*
    %arrayptr1 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %arrayptrptr1
    %decayed2 = addrspacecast %jl_value_t addrspace(10) *%obj2 to %jl_value_t addrspace(11) *
    %arrayptrptr2 = bitcast %jl_value_t addrspace(11) *%decayed2 to i8 addrspace(13)* addrspace(11)*
    %arrayptr2 = load i8 addrspace(13)*, i8 addrspace(13)* addrspace(11)* %arrayptrptr2
    %insert1 = insertelement <2 x i8 addrspace(13)*> undef, i8 addrspace(13)* %arrayptr1, i32 0
    %insert2 = insertelement <2 x i8 addrspace(13)*> %insert1, i8 addrspace(13)* %arrayptr2, i32 1
    call void @jl_safepoint()
    %select = select i1 %cond, <2 x i8 addrspace(13)*> %insert2, <2 x i8 addrspace(13)*> zeroinitializer
    call void @jl_safepoint()
    %el1 = extractelement <2 x i8 addrspace(13)*> %select, i32 0
    %el2 = extractelement <2 x i8 addrspace(13)*> %select, i32 1
    %l1 = load i8, i8 addrspace(13)* %el1
    %l2 = load i8, i8 addrspace(13)* %el2
    %add = add i8 %l1, %l2
    ret i8 %add
}

define i8 @vector_arrayptrs() {
; CHECK-LABEL: @vector_arrayptrs
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 2
; CHECK: store %jl_value_t addrspace(10)* %obj1, %jl_value_t addrspace(10)** [[GEP0]]
;
top:
   %ptls = call %jl_value_t*** @julia.ptls_states()
   %obj1 = call %jl_value_t addrspace(10) *@alloc()
   %decayed = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11) *
   %arrayptrptr = bitcast %jl_value_t addrspace(11) *%decayed to <2 x i8 addrspace(13)*> addrspace(11)*
   %arrayptrs = load <2 x i8 addrspace(13)*>, <2 x i8 addrspace(13)*> addrspace(11)* %arrayptrptr, align 16
   %arrayptr = extractelement <2 x i8 addrspace(13)*> %arrayptrs, i32 0
   call void @jl_safepoint()
   %val = load i8, i8 addrspace(13)* %arrayptr
   ret i8 %val
}

declare <2 x i8 addrspace(13)*> @llvm.masked.load.v2p13i8.p11v2p13i8 (<2 x i8 addrspace(13)*> addrspace(11)*, i32, <2 x i1>, <2 x i8 addrspace(13)*>)

define i8 @masked_arrayptrs() {
; CHECK-LABEL: @masked_arrayptrs
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
; CHECK: %arrayptrs = call <2 x i8 addrspace(13)*> @llvm.masked.load.v2p13i8.p11v2p13i8(<2 x i8 addrspace(13)*> addrspace(11)* %arrayptrptr, i32 16, <2 x i1> <i1 true, i1 false>, <2 x i8 addrspace(13)*> zeroinitializer)
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 2
; CHECK: store %jl_value_t addrspace(10)* %obj1, %jl_value_t addrspace(10)** [[GEP0]]
;
top:
   %ptls = call %jl_value_t*** @julia.ptls_states()
   %obj1 = call %jl_value_t addrspace(10) *@alloc()
   %decayed = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11) *
   %arrayptrptr = bitcast %jl_value_t addrspace(11) *%decayed to <2 x i8 addrspace(13)*> addrspace(11)*
   %arrayptrs = call <2 x i8 addrspace(13)*> @llvm.masked.load.v2p13i8.p11v2p13i8(<2 x i8 addrspace(13)*> addrspace(11)* %arrayptrptr, i32 16, <2 x i1> <i1 true, i1 false>, <2 x i8 addrspace(13)*> undef)
   %arrayptr = extractelement <2 x i8 addrspace(13)*> %arrayptrs, i32 0
   call void @jl_safepoint()
   %val = load i8, i8 addrspace(13)* %arrayptr
   ret i8 %val
}

declare <2 x i8 addrspace(13)*> @llvm.masked.gather.v2p13i8.v2p11p13i8 (<2 x i8 addrspace(13)* addrspace(11)*>, i32, <2 x i1>, <2 x i8 addrspace(13)*>)

define i8 @gather_arrayptrs() {
; CHECK-LABEL: @gather_arrayptrs
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
; CHECK: %arrayptrs = call <2 x i8 addrspace(13)*> @llvm.masked.gather.v2p13i8.v2p11p13i8(<2 x i8 addrspace(13)* addrspace(11)*> %arrayptrptrs, i32 16, <2 x i1> <i1 true, i1 false>, <2 x i8 addrspace(13)*> zeroinitializer)
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 2
; CHECK: store %jl_value_t addrspace(10)* %obj1, %jl_value_t addrspace(10)** [[GEP0]]
;
top:
   %ptls = call %jl_value_t*** @julia.ptls_states()
   %obj1 = call %jl_value_t addrspace(10) *@alloc()
   %decayed = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11)*
   %arrayptrptr = bitcast %jl_value_t addrspace(11) *%decayed to i8 addrspace(13)* addrspace(11)*
   %arrayptrptrs = insertelement <2 x i8 addrspace(13)* addrspace(11)*> zeroinitializer, i8 addrspace(13)* addrspace(11)* %arrayptrptr, i32 0
   %arrayptrs = call <2 x i8 addrspace(13)*> @llvm.masked.gather.v2p13i8.v2p11p13i8(<2 x i8 addrspace(13)* addrspace(11)*> %arrayptrptrs, i32 16, <2 x i1> <i1 true, i1 false>, <2 x i8 addrspace(13)*> undef)
   %arrayptr = extractelement <2 x i8 addrspace(13)*> %arrayptrs, i32 0
   call void @jl_safepoint()
   %val = load i8, i8 addrspace(13)* %arrayptr
   ret i8 %val
}

define i8 @lost_select_decayed(i1 %arg1) {
; CHECK-LABEL: @lost_select_decayed
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 3
; CHECK: [[GEP0:%.*]] = getelementptr %jl_value_t addrspace(10)*, %jl_value_t addrspace(10)** %gcframe, i32 2
; CHECK: store %jl_value_t addrspace(10)* [[SOMETHING:%.*]], %jl_value_t addrspace(10)** [[GEP0]]
top:
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %obj1 = call %jl_value_t addrspace(10) *@alloc()
    %decayed = addrspacecast %jl_value_t addrspace(10) *%obj1 to %jl_value_t addrspace(11)*
    %selected = select i1 %arg1, %jl_value_t addrspace(11)* null, %jl_value_t addrspace(11)* %decayed
    %casted = bitcast %jl_value_t addrspace(11)* %selected to i8 addrspace(11)*
    call void @jl_safepoint()
    %val = load i8, i8 addrspace(11)* %casted
    ret i8 %val
}

!0 = !{!"jtbaa"}
!1 = !{!"jtbaa_const", !0, i64 0}
!2 = !{!1, !1, i64 0, i64 1}
