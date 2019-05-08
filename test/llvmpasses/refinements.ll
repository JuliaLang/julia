; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -FinalLowerGC -S %s | FileCheck %s

%jl_value_t = type opaque

declare %jl_value_t*** @julia.ptls_states()
declare void @jl_safepoint()
declare void @one_arg_boxed(%jl_value_t addrspace(100)*)
declare %jl_value_t addrspace(100)* @jl_box_int64(i64)

define void @argument_refinement(%jl_value_t addrspace(100)* %a) {
; CHECK-LABEL: @argument_refinement
; CHECK-NOT: %gcframe
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %casted1 = bitcast %jl_value_t addrspace(100)* %a to %jl_value_t addrspace(100)* addrspace(100)*
    %loaded1 = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %casted1, !tbaa !1
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(100)* %loaded1 to i64 addrspace(100)*
    %loaded2 = load i64, i64 addrspace(100)* %casted2
    ret void
}

; Check that we reuse the gc slot from the box
define void @heap_refinement1(i64 %a) {
; CHECK-LABEL: @heap_refinement1
; CHECK:   %gcframe = alloca %jl_value_t addrspace(100)*, i32 3
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(100)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast %jl_value_t addrspace(100)* %aboxed to %jl_value_t addrspace(100)* addrspace(100)*
    %loaded1 = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %casted1, !tbaa !1
; CHECK: store %jl_value_t addrspace(100)* %aboxed
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(100)* %loaded1 to i64 addrspace(100)*
    %loaded2 = load i64, i64 addrspace(100)* %casted2
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %aboxed)
    ret void
}

; Check that we don't root the allocated value here, just the derived value
define void @heap_refinement2(i64 %a) {
; CHECK-LABEL: @heap_refinement2
; CHECK:   %gcframe = alloca %jl_value_t addrspace(100)*, i32 3
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(100)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast %jl_value_t addrspace(100)* %aboxed to %jl_value_t addrspace(100)* addrspace(100)*
    %loaded1 = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %casted1, !tbaa !1
; CHECK: store %jl_value_t addrspace(100)* %loaded1
    call void @jl_safepoint()
    %casted2 = bitcast %jl_value_t addrspace(100)* %loaded1 to i64 addrspace(100)*
    %loaded2 = load i64, i64 addrspace(100)* %casted2
    ret void
}

declare %jl_value_t addrspace(100)* @allocate_some_value()

; Check that the way we compute rooting is compatible with refinements
define void @issue22770() {
; CHECK-LABEL: @issue22770
; CHECK: %gcframe = alloca %jl_value_t addrspace(100)*, i32 4
    %ptls = call %jl_value_t*** @julia.ptls_states()
    %y = call %jl_value_t addrspace(100)* @allocate_some_value()
    %casted1 = bitcast %jl_value_t addrspace(100)* %y to %jl_value_t addrspace(100)* addrspace(100)*
    %x = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %casted1, !tbaa !1
; CHECK: store %jl_value_t addrspace(100)* %y,
    %a = call %jl_value_t addrspace(100)* @allocate_some_value()
; CHECK: store %jl_value_t addrspace(100)* %a
; CHECK: call void @one_arg_boxed(%jl_value_t addrspace(100)* %x)
; CHECK: call void @one_arg_boxed(%jl_value_t addrspace(100)* %a)
; CHECK: call void @one_arg_boxed(%jl_value_t addrspace(100)* %y)
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %x)
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %a)
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %y)
; CHECK: store %jl_value_t addrspace(100)* %x
    %c = call %jl_value_t addrspace(100)* @allocate_some_value()
; CHECK: store %jl_value_t addrspace(100)* %c
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %x)
    call void @one_arg_boxed(%jl_value_t addrspace(100)* %c)
    ret void
}

define void @refine_select_phi(%jl_value_t addrspace(100)* %x, %jl_value_t addrspace(100)* %y, i1 %b) {
; CHECK-LABEL: @refine_select_phi
; CHECK-NOT: %gcframe
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %s = select i1 %b, %jl_value_t addrspace(100)* %x, %jl_value_t addrspace(100)* %y
  br i1 %b, label %L1, label %L2

L1:
  br label %L3

L2:
  br label %L3

L3:
  %p = phi %jl_value_t addrspace(100)* [ %x, %L1 ], [ %y, %L2 ]
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %s)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %p)
  ret void
}

define void @dont_refine_loop(%jl_value_t addrspace(100)* %x) {
; CHECK-LABEL: @dont_refine_loop
; CHECK: %gcframe = alloca %jl_value_t addrspace(100)*, i32 4
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
  %p = phi %jl_value_t addrspace(100)* [ %x, %top ], [ %v, %L1 ]
  %v = call %jl_value_t addrspace(100)* @allocate_some_value()
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %v)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

@gv1 = external global %jl_value_t*

define void @refine_loop_const(%jl_value_t addrspace(100)* %x) {
; CHECK-LABEL: @refine_loop_const
; CHECK-NOT: %gcframe
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
  %p = phi %jl_value_t addrspace(100)* [ %x, %top ], [ %v, %L1 ]
  %v0 = load %jl_value_t*, %jl_value_t** @gv1, !tbaa !4
  %v = addrspacecast %jl_value_t* %v0 to %jl_value_t addrspace(100)*
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %v)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

define void @refine_loop_indirect(%jl_value_t addrspace(100)* %x) {
; CHECK-LABEL: @refine_loop_indirect
; CHECK: %gcframe = alloca %jl_value_t addrspace(100)*, i32 3
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %a = call %jl_value_t addrspace(100)* @allocate_some_value()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
; `%v` is not a valid refinement incoming value of the phi node `%p`,
; however, `%v` can be refined to `%a` and `%a` is a valid refinement
; of the phi node. Therefore, we need only one gc slot for `%a`.
  %p = phi %jl_value_t addrspace(100)* [ %x, %top ], [ %v, %L1 ]
  %ca = bitcast %jl_value_t addrspace(100)* %a to %jl_value_t addrspace(100)* addrspace(100)*
  %v = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %ca, !tbaa !1
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %v)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

define void @refine_loop_indirect2(%jl_value_t addrspace(100)* %x) {
; CHECK-LABEL: @refine_loop_indirect2
; CHECK: %gcframe = alloca %jl_value_t addrspace(100)*, i32 3
top:
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %a = call %jl_value_t addrspace(100)* @allocate_some_value()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
; `%p` has circular dependency but it can only be derived from `%a` which dominate `%p`.
  %p = phi %jl_value_t addrspace(100)* [ %a, %top ], [ %v, %L1 ]
  %ca = bitcast %jl_value_t addrspace(100)* %p to %jl_value_t addrspace(100)* addrspace(100)*
  %v = load %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(100)* %ca, !tbaa !1
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %v)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

declare %jl_value_t addrspace(100)* @julia.typeof(%jl_value_t addrspace(100)*) #0

define %jl_value_t addrspace(100)* @typeof(%jl_value_t addrspace(100)* %x) {
; CHECK-LABEL: @typeof(
; CHECK-NOT: %gcframe
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %v = call %jl_value_t addrspace(100)* @julia.typeof(%jl_value_t addrspace(100)* %x)
  call void @one_arg_boxed(%jl_value_t addrspace(100)* %v)
  ret %jl_value_t addrspace(100)* %v
}

declare void @julia.write_barrier(%jl_value_t addrspace(100)*, %jl_value_t addrspace(100)*) #1

define %jl_value_t addrspace(100)* @setfield(%jl_value_t addrspace(100)* %p) {
; CHECK-LABEL: @setfield(
; CHECK-NOT: %gcframe
; CHECK: call void @jl_gc_queue_root
  %ptls = call %jl_value_t*** @julia.ptls_states()
  %c = call %jl_value_t addrspace(100)* @allocate_some_value()
  %fp = bitcast %jl_value_t addrspace(100)* %p to %jl_value_t addrspace(100)* addrspace(100)*
  store %jl_value_t addrspace(100)* %c, %jl_value_t addrspace(100)* addrspace(100)* %fp
  call void @julia.write_barrier(%jl_value_t addrspace(100)* %p, %jl_value_t addrspace(100)* %c)
  ret %jl_value_t addrspace(100)* %c
}

attributes #0 = { argmemonly norecurse nounwind readonly }
attributes #1 = { inaccessiblememonly norecurse nounwind }

!0 = !{!"jtbaa"}
!1 = !{!2, !2, i64 0}
!2 = !{!"jtbaa_immut", !0, i64 0}
!3 = !{!"jtbaa_const", !0, i64 0}
!4 = !{!3, !3, i64 0, i64 1}
