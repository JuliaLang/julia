; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -JuliaMultiVersioning -S %s | FileCheck %s --allow-unused-prefixes=false
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning' -S %s | FileCheck %s --allow-unused-prefixes=false

; CHECK: @jl_fvar_idxs = hidden constant [1 x i32] zeroinitializer
; CHECK: @jl_gvar_idxs = hidden constant [0 x i32] zeroinitializer
; CHECK: @subtarget_cloned_gv = hidden global i64* null
; CHECK: @subtarget_cloned.reloc_slot = hidden global i32 (i32)* null
; CHECK: @jl_fvar_offsets = hidden constant [2 x i32] [i32 1, i32 0]
; CHECK: @jl_gvar_base = hidden constant i64 0
; CHECK: @jl_gvar_offsets = hidden constant [1 x i32] zeroinitializer
; CHECK: @jl_clone_slots = hidden constant [5 x i32]
; CHECK-SAME: i32 2, i32 0, {{.*}} sub {{.*}}@subtarget_cloned.reloc_slot{{.*}}@jl_gvar_base
; CHECK: @jl_clone_idxs = hidden constant [13 x i32]
; COM: TODO actually check the clone idxs maybe?
; CHECK: @jl_clone_offsets = hidden constant [4 x i32]
; CHECK-SAME: sub
; CHECK-SAME: @subtarget_cloned.1
; CHECK-SAME: @subtarget_cloned
; CHECK-SAME: sub
; CHECK-SAME: @subtarget_cloned.2
; CHECK-SAME: @subtarget_cloned
; CHECK-SAME: sub

@jl_fvars = global [1 x i64*] [i64* bitcast (i32 (i32)* @subtarget_cloned to i64*)], align 16
@jl_gvars = global [0 x i64*] zeroinitializer, align 16
@jl_fvar_idxs = hidden constant [1 x i32] [i32 0], align 16
@jl_gvar_idxs = hidden constant [0 x i32] zeroinitializer, align 16
@subtarget_cloned_gv = hidden global i64* bitcast (i32 (i32)* @subtarget_cloned to i64*), align 16

@subtarget_cloned_aliased = alias i32 (i32), i32 (i32)* @subtarget_cloned

; CHECK: define{{.*}}@boring({{.*}}#[[BORING_DEFAULT_ATTRS:[0-9]+]]
; CHECK-NEXT: ret i32 %0
define noundef i32 @boring(i32 noundef %0) #0 {
    ret i32 %0
}

; CHECK: declare{{.*}}@declaration({{.*}}#[[DECLARATION_DEFAULT_ATTRS:[0-9]+]]
declare i32 @declaration(i32 %0) #1

; CHECK: define{{.*}}@call_boring({{.*}}#[[BORING_DEFAULT_ATTRS]]
; CHECK-NEXT: %2 = call noundef i32 @boring(i32 noundef %0)
define noundef i32 @call_boring(i32 noundef %0) #0 {
    %2 = call noundef i32 @boring(i32 noundef %0)
    ret i32 %2
}

; CHECK: define{{.*}}@call_declaration({{.*}}#[[DECLARATION_DEFAULT_ATTRS]]
; CHECK-NEXT: %2 = call noundef i32 @declaration(i32 noundef %0)
define noundef i32 @call_declaration(i32 noundef %0) #1 {
    %2 = call noundef i32 @declaration(i32 noundef %0)
    ret i32 %2
}

; CHECK: define{{.*}}@subtarget_cloned({{.*}}#[[SUBTARGET_CLONED_DEFAULT_ATTRS:[0-9]+]]
; CHECK-NEXT: ret i32 0
define noundef i32 @subtarget_cloned(i32 noundef %0) #2 {
    ret i32 0
}

; COM: should fixup this callsite since 2 is cloned for a subtarget
; CHECK: define{{.*}}@call_subtarget_cloned({{.*}}#[[CALL_SUBTARGET_CLONED_DEFAULT_ATTRS:[0-9]+]]
; CHECK-NEXT: [[FUNC_PTR:%[0-9]+]] = load{{.*}}@subtarget_cloned.reloc_slot{{.*}}!tbaa ![[TBAA_CONST_METADATA:[0-9]+]], !invariant.load
; CHECK-NEXT: call{{.*}}[[FUNC_PTR]]
; CHECK: ret i32
define noundef i32 @call_subtarget_cloned(i32 noundef %0) #3 {
    %2 = call noundef i32 @subtarget_cloned(i32 noundef %0)
    ret i32 %2
}

; CHECK: define{{.*}}@call_subtarget_cloned_but_not_cloned({{.*}}#[[BORING_DEFAULT_ATTRS]]
; CHECK-NEXT: [[FUNC_PTR:%[0-9]+]] = load{{.*}}@subtarget_cloned.reloc_slot{{.*}}!tbaa ![[TBAA_CONST_METADATA]], !invariant.load
; CHECK-NEXT: call{{.*}}[[FUNC_PTR]]
; CHECK: ret i32
define noundef i32 @call_subtarget_cloned_but_not_cloned(i32 noundef %0) #0 {
    %2 = call noundef i32 @subtarget_cloned(i32 noundef %0)
    ret i32 %2
}

; CHECK: define{{.*}}@boring.1({{.*}}#[[BORING_CLONEALL_ATTRS:[0-9]+]]
; CHECK-NEXT: ret i32 %0

; CHECK: declare{{.*}}@declaration.1({{.*}}#[[DECLARATION_CLONEALL_ATTRS:[0-9]+]]

; COM: should not fixup this callsite since boring is not cloned for a subtarget
; COM: also should call boring.1 instead of boring
; CHECK: define{{.*}}@call_boring.1({{.*}}#[[BORING_CLONEALL_ATTRS]]
; CHECK-NEXT: %2 = call noundef i32 @boring.1(i32 noundef %0)

; CHECK: define{{.*}}@call_declaration.1({{.*}}#[[DECLARATION_CLONEALL_ATTRS]]
; CHECK-NEXT: %2 = call noundef i32 @declaration.1(i32 noundef %0)

; CHECK: define{{.*}}@subtarget_cloned.1({{.*}}#[[SUBTARGET_CLONED_CLONEALL_ATTRS:[0-9]+]]
; CHECK-NEXT: ret i32 0

; CHECK: define{{.*}}@subtarget_cloned.2({{.*}}#[[SUBTARGET_CLONED_FASTMATH_ATTRS:[0-9]+]]
; CHECK-NEXT: ret i32 0

; COM: should *NOT* fixup this callsite since subtarget_cloned is not cloned for a subtarget of the cloneall
; CHECK: define{{.*}}@call_subtarget_cloned.1({{.*}}#[[CALL_SUBTARGET_CLONED_CLONEALL_ATTRS:[0-9]+]]
; CHECK-NEXT: %2 = call noundef i32 @subtarget_cloned.1(i32 noundef %0)

; CHECK: define {{.*}}@call_subtarget_cloned.2({{.*}}#[[CALL_SUBTARGET_CLONED_FASTMATH_ATTRS:[0-9]+]]
; CHECK-NEXT: %2 = call noundef i32 @subtarget_cloned.2(i32 noundef %0)

; CHECK: define{{.*}}@call_subtarget_cloned_but_not_cloned.1({{.*}}#[[BORING_CLONEALL_ATTRS]]
; CHECK-NEXT: %2 = call noundef i32 @subtarget_cloned.1(i32 noundef %0)

; COM: should not have cloned for fastmath
; CHECK-NOT: @subtarget_cloned_but_not_cloned.2

; COM: check for alias being rewritten to a function trampoline
; CHECK: define{{.*}}@subtarget_cloned_aliased{{.*}}#[[SUBTARGET_ALIASED_ATTRS:[0-9]+]]
; CHECK-NOT: }
; CHECK: [[FUNC_PTR:%[0-9]+]] = load{{.*}}@subtarget_cloned.reloc_slot{{.*}}!tbaa ![[TBAA_CONST_METADATA]], !invariant.load
; CHECK-NEXT: call{{.*}}[[FUNC_PTR]]
; CHECK: ret i32

; CHECK: attributes #[[BORING_DEFAULT_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="0"
; CHECK-DAG: "target-cpu"="cpubase"
; CHECK-DAG: "target-features"="nofeatures"
; CHECK-SAME: }
; CHECK: attributes #[[DECLARATION_DEFAULT_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="0"
; CHECK-DAG: "target-cpu"="cpubase"
; CHECK-DAG: "target-features"="nofeatures"
; CHECK-SAME: }
; CHECK: attributes #[[SUBTARGET_CLONED_DEFAULT_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="0"
; CHECK-DAG: "target-cpu"="cpubase"
; CHECK-DAG: "target-features"="nofeatures"
; CHECK-DAG: "julia.mv.reloc"
; CHECK-SAME: }
; CHECK: attributes #[[CALL_SUBTARGET_CLONED_DEFAULT_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="0"
; CHECK-DAG: "target-cpu"="cpubase"
; CHECK-DAG: "target-features"="nofeatures"
; CHECK-SAME: }
; CHECK: attributes #[[BORING_CLONEALL_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="1"
; CHECK-DAG: "target-cpu"="cpucloneall"
; CHECK-DAG: "target-features"="cloneall"
; CHECK-SAME: }
; CHECK: attributes #[[DECLARATION_CLONEALL_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="1"
; CHECK-DAG: "target-cpu"="cpucloneall"
; CHECK-DAG: "target-features"="cloneall"
; CHECK-SAME: }
; CHECK: attributes #[[SUBTARGET_CLONED_CLONEALL_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="1"
; CHECK-DAG: "target-cpu"="cpucloneall"
; CHECK-DAG: "target-features"="cloneall"
; CHECK-DAG: "julia.mv.reloc"
; CHECK-SAME: }
; CHECK: attributes #[[SUBTARGET_CLONED_FASTMATH_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="2"
; CHECK-DAG: "target-cpu"="cpufastmath"
; CHECK-DAG: "target-features"="fastmathclone"
; CHECK-DAG: "julia.mv.reloc"
; CHECK-SAME: }
; CHECK: attributes #[[CALL_SUBTARGET_CLONED_CLONEALL_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="1"
; CHECK-DAG: "target-cpu"="cpucloneall"
; CHECK-DAG: "target-features"="cloneall"
; CHECK-SAME: }
; CHECK: attributes #[[CALL_SUBTARGET_CLONED_FASTMATH_ATTRS]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="6"
; CHECK-DAG: "julia.mv.clone"="2"
; CHECK-DAG: "target-cpu"="cpufastmath"
; CHECK-DAG: "target-features"="fastmathclone"
; CHECK-SAME: }
; CHECK: attributes #[[SUBTARGET_ALIASED_ATTRS]]
; CHECK-SAME: {
; CHECK-SAME: "julia.mv.alias"
; CHECK-SAME: }
attributes #0 = {"julia.mv.clones"="2"}
attributes #1 = {"julia.mv.clones"="2" "test.unique"="1"}
attributes #2 = {"julia.mv.clones"="6" "julia.mv.reloc"}
attributes #3 = {"julia.mv.clones"="6"}

!llvm.module.flags = !{!0, !1, !2}

!0 = !{i32 1, !"julia.mv.enable", i32 1}
!1 = !{i32 1, !"julia.mv.annotated", i32 1}
!2 = !{i32 1, !"julia.mv.specs", !3}
!3 = !{!4, !5, !6, !7, !8}
!4 = !{!"cpubase", !"nofeatures", i32 0, i32 2}
!5 = !{!"cpucloneall", !"cloneall", i32 0, i32 2}
!6 = !{!"cpufastmath", !"fastmathclone", i32 0, i32 4}
!7 = !{!"cpuloop", !"loopclone", i32 0, i32 8}
!8 = !{!"cpusimd", !"simdclone", i32 0, i32 16}
; CHECK-DAG: ![[TBAA_CONST_METADATA]] = !{![[JTBAA_CONST_METADATA:[0-9]+]], ![[JTBAA_CONST_METADATA]]
; CHECK-DAG: ![[JTBAA_CONST_METADATA]] = !{!"jtbaa_const"
