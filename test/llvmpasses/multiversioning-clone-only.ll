; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -JuliaMultiVersioning -S %s | FileCheck %s --allow-unused-prefixes=false
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning' -S %s | FileCheck %s --allow-unused-prefixes=false

@jl_fvars = global [0 x i64] zeroinitializer, align 16
@jl_gvars = global [0 x i64] zeroinitializer, align 16
@jl_fvar_idxs = global [0 x i32] zeroinitializer, align 16
@jl_gvar_idxs = global [0 x i32] zeroinitializer, align 16

; CHECK-DAG: define{{.*}}@boring({{.*}}#[[BORING_DEFAULT_ATTRS:[0-9]+]]
; CHECK-DAG-NEXT: ret i32 %0
; CHECK-DAG: define{{.*}}@boring.1({{.*}}#[[BORING_CLONEALL_ATTRS:[0-9]+]]
; CHECK-DAG-NEXT: ret i32 %0
define noundef i32 @boring(i32 noundef %0) #0 {
    ret i32 %0
}

; CHECK-DAG: declare{{.*}}@declaration({{.*}}#[[DECLARATION_DEFAULT_ATTRS:[0-9]+]]
; CHECK-DAG: declare{{.*}}@declaration.1({{.*}}#[[DECLARATION_CLONEALL_ATTRS:[0-9]+]]
declare i32 @declaration(i32 %0) #1

; CHECK: }

; CHECK-DAG: attributes #[[BORING_DEFAULT_ATTRS:[0-9]+]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="0"
; CHECK-DAG: "target-cpu"="cpubase"
; CHECK-DAG: "target-features"="nofeatures"
; CHECK-SAME: }
; CHECK-DAG: attributes #[[BORING_CLONEALL_ATTRS:[0-9]+]]
; CHECK-SAME: {
; CHECK-DAG: "julia.mv.clones"="2"
; CHECK-DAG: "julia.mv.clone"="1"
; CHECK-DAG: "target-cpu"="cpucloneall"
; CHECK-DAG: "target-features"="cloneall"
; CHECK-SAME: }
attributes #0 = {"julia.mv.clones"="2"}
attributes #1 = {"julia.mv.clones"="2" "test.unique"="1"}

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