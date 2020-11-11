; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -FinalLowerGC -S %s | FileCheck %s


declare {}*** @julia.ptls_states()
declare void @jl_safepoint()
declare void @one_arg_boxed({} addrspace(10)*)
declare {} addrspace(10)* @jl_box_int64(i64)

define void @argument_refinement({} addrspace(10)* %a) {
; CHECK-LABEL: @argument_refinement
; CHECK-NOT: %gcframe
    %ptls = call {}*** @julia.ptls_states()
    %casted1 = bitcast {} addrspace(10)* %a to {} addrspace(10)* addrspace(10)*
    %loaded1 = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %casted1, !tbaa !1
    call void @jl_safepoint()
    %casted2 = bitcast {} addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    ret void
}

; Check that we reuse the gc slot from the box
define void @heap_refinement1(i64 %a) {
; CHECK-LABEL: @heap_refinement1
; CHECK:   %gcframe = alloca {} addrspace(10)*, i32 3
    %ptls = call {}*** @julia.ptls_states()
    %aboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast {} addrspace(10)* %aboxed to {} addrspace(10)* addrspace(10)*
    %loaded1 = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %casted1, !tbaa !1
; CHECK: store {} addrspace(10)* %aboxed
    call void @jl_safepoint()
    %casted2 = bitcast {} addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    call void @one_arg_boxed({} addrspace(10)* %aboxed)
    ret void
}

; Check that we don't root the allocated value here, just the derived value
define void @heap_refinement2(i64 %a) {
; CHECK-LABEL: @heap_refinement2
; CHECK:   %gcframe = alloca {} addrspace(10)*, i32 3
    %ptls = call {}*** @julia.ptls_states()
    %aboxed = call {} addrspace(10)* @jl_box_int64(i64 signext %a)
    %casted1 = bitcast {} addrspace(10)* %aboxed to {} addrspace(10)* addrspace(10)*
    %loaded1 = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %casted1, !tbaa !1
; CHECK: store {} addrspace(10)* %loaded1
    call void @jl_safepoint()
    %casted2 = bitcast {} addrspace(10)* %loaded1 to i64 addrspace(10)*
    %loaded2 = load i64, i64 addrspace(10)* %casted2
    ret void
}

declare {} addrspace(10)* @allocate_some_value()

; Check that the way we compute rooting is compatible with refinements
define void @issue22770() {
; CHECK-LABEL: @issue22770
; CHECK: %gcframe = alloca {} addrspace(10)*, i32 4
    %ptls = call {}*** @julia.ptls_states()
    %y = call {} addrspace(10)* @allocate_some_value()
    %casted1 = bitcast {} addrspace(10)* %y to {} addrspace(10)* addrspace(10)*
    %x = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %casted1, !tbaa !1
; CHECK: store {} addrspace(10)* %y,
    %a = call {} addrspace(10)* @allocate_some_value()
; CHECK: store {} addrspace(10)* %a
; CHECK: call void @one_arg_boxed({} addrspace(10)* %x)
; CHECK: call void @one_arg_boxed({} addrspace(10)* %a)
; CHECK: call void @one_arg_boxed({} addrspace(10)* %y)
    call void @one_arg_boxed({} addrspace(10)* %x)
    call void @one_arg_boxed({} addrspace(10)* %a)
    call void @one_arg_boxed({} addrspace(10)* %y)
; CHECK: store {} addrspace(10)* %x
    %c = call {} addrspace(10)* @allocate_some_value()
; CHECK: store {} addrspace(10)* %c
    call void @one_arg_boxed({} addrspace(10)* %x)
    call void @one_arg_boxed({} addrspace(10)* %c)
    ret void
}

define void @refine_select_phi({} addrspace(10)* %x, {} addrspace(10)* %y, i1 %b) {
; CHECK-LABEL: @refine_select_phi
; CHECK-NOT: %gcframe
top:
  %ptls = call {}*** @julia.ptls_states()
  %s = select i1 %b, {} addrspace(10)* %x, {} addrspace(10)* %y
  br i1 %b, label %L1, label %L2

L1:
  br label %L3

L2:
  br label %L3

L3:
  %p = phi {} addrspace(10)* [ %x, %L1 ], [ %y, %L2 ]
  call void @one_arg_boxed({} addrspace(10)* %s)
  call void @one_arg_boxed({} addrspace(10)* %p)
  ret void
}

define void @dont_refine_loop({} addrspace(10)* %x) {
; CHECK-LABEL: @dont_refine_loop
; CHECK: %gcframe = alloca {} addrspace(10)*, i32 4
top:
  %ptls = call {}*** @julia.ptls_states()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
  %p = phi {} addrspace(10)* [ %x, %top ], [ %v, %L1 ]
  %v = call {} addrspace(10)* @allocate_some_value()
  call void @one_arg_boxed({} addrspace(10)* %v)
  call void @one_arg_boxed({} addrspace(10)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

@gv1 = external global {}*

define void @refine_loop_const({} addrspace(10)* %x) {
; CHECK-LABEL: @refine_loop_const
; CHECK-NOT: %gcframe
top:
  %ptls = call {}*** @julia.ptls_states()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
  %p = phi {} addrspace(10)* [ %x, %top ], [ %v, %L1 ]
  %v0 = load {}*, {}** @gv1, !tbaa !4
  %v = addrspacecast {}* %v0 to {} addrspace(10)*
  call void @one_arg_boxed({} addrspace(10)* %v)
  call void @one_arg_boxed({} addrspace(10)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

define void @refine_loop_indirect({} addrspace(10)* %x) {
; CHECK-LABEL: @refine_loop_indirect
; CHECK: %gcframe = alloca {} addrspace(10)*, i32 3
top:
  %ptls = call {}*** @julia.ptls_states()
  %a = call {} addrspace(10)* @allocate_some_value()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
; `%v` is not a valid refinement incoming value of the phi node `%p`,
; however, `%v` can be refined to `%a` and `%a` is a valid refinement
; of the phi node. Therefore, we need only one gc slot for `%a`.
  %p = phi {} addrspace(10)* [ %x, %top ], [ %v, %L1 ]
  %ca = bitcast {} addrspace(10)* %a to {} addrspace(10)* addrspace(10)*
  %v = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %ca, !tbaa !1
  call void @one_arg_boxed({} addrspace(10)* %v)
  call void @one_arg_boxed({} addrspace(10)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

define void @refine_loop_indirect2({} addrspace(10)* %x) {
; CHECK-LABEL: @refine_loop_indirect2
; CHECK: %gcframe = alloca {} addrspace(10)*, i32 3
top:
  %ptls = call {}*** @julia.ptls_states()
  %a = call {} addrspace(10)* @allocate_some_value()
  br label %L1

L1:
  %continue = phi i1 [ true, %top ], [ false, %L1 ]
; `%p` has circular dependency but it can only be derived from `%a` which dominate `%p`.
  %p = phi {} addrspace(10)* [ %a, %top ], [ %v, %L1 ]
  %ca = bitcast {} addrspace(10)* %p to {} addrspace(10)* addrspace(10)*
  %v = load {} addrspace(10)*, {} addrspace(10)* addrspace(10)* %ca, !tbaa !1
  call void @one_arg_boxed({} addrspace(10)* %v)
  call void @one_arg_boxed({} addrspace(10)* %p)
  br i1 %continue, label %L1, label %L2

L2:
  ret void
}

declare {} addrspace(10)* @julia.typeof({} addrspace(10)*) #0

define {} addrspace(10)* @typeof({} addrspace(10)* %x) {
; CHECK-LABEL: @typeof(
; CHECK-NOT: %gcframe
  %ptls = call {}*** @julia.ptls_states()
  %v = call {} addrspace(10)* @julia.typeof({} addrspace(10)* %x)
  call void @one_arg_boxed({} addrspace(10)* %v)
  ret {} addrspace(10)* %v
}

declare void @julia.write_barrier({} addrspace(10)*, {} addrspace(10)*) #1

define {} addrspace(10)* @setfield({} addrspace(10)* %p) {
; CHECK-LABEL: @setfield(
; CHECK-NOT: %gcframe
; CHECK: call void @jl_gc_queue_root
  %ptls = call {}*** @julia.ptls_states()
  %c = call {} addrspace(10)* @allocate_some_value()
  %fp = bitcast {} addrspace(10)* %p to {} addrspace(10)* addrspace(10)*
  store {} addrspace(10)* %c, {} addrspace(10)* addrspace(10)* %fp
  call void @julia.write_barrier({} addrspace(10)* %p, {} addrspace(10)* %c)
  ret {} addrspace(10)* %c
}

attributes #0 = { argmemonly norecurse nounwind readonly }
attributes #1 = { inaccessiblememonly norecurse nounwind }

!0 = !{!"jtbaa"}
!1 = !{!2, !2, i64 0}
!2 = !{!"jtbaa_immut", !0, i64 0}
!3 = !{!"jtbaa_const", !0, i64 0}
!4 = !{!3, !3, i64 0, i64 1}
