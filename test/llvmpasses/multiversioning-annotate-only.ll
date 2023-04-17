; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -JuliaMultiVersioning -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning' -S %s | FileCheck %s

; COM: This test checks that multiversioning correctly picks up on features that should trigger cloning
; COM: Note that for annotations alone, we don't need jl_fvars or jl_gvars

; COM: Copied from src/processor.h
; COM:    JL_TARGET_VEC_CALL = 1 << 0,
; COM:    // Clone all functions
; COM:    JL_TARGET_CLONE_ALL = 1 << 1,
; COM:    // Clone when there's scalar math operations that can benefit from target-specific
; COM:    // optimizations. This includes `muladd`, `fma`, `fast`/`contract` flags.
; COM:    JL_TARGET_CLONE_MATH = 1 << 2,
; COM:    // Clone when the function has a loop
; COM:    JL_TARGET_CLONE_LOOP = 1 << 3,
; COM:    // Clone when the function uses any vectors
; COM:    // When this is specified, the cloning pass should also record if any of the cloned functions
; COM:    // used this in any function call (including the signature of the function itself)
; COM:    JL_TARGET_CLONE_SIMD = 1 << 4,
; COM:    // The CPU name is unknown
; COM:    JL_TARGET_UNKNOWN_NAME = 1 << 5,
; COM:    // Optimize for size for this target
; COM:    JL_TARGET_OPTSIZE = 1 << 6,
; COM:    // Only optimize for size for this target
; COM:    JL_TARGET_MINSIZE = 1 << 7,
; COM:    // Clone when the function queries CPU features
; COM:    JL_TARGET_CLONE_CPU = 1 << 8,
; COM:    // Clone when the function uses fp16
; COM:    JL_TARGET_CLONE_FLOAT16 = 1 << 9,

; COM: start with the basics, just one feature per function

; COM: boring should only be cloned if clone_all is enabled on the target
; CHECK: @boring{{.*}}#[[BORING_ATTRS:[0-9]+]]
define noundef i32 @boring(i32 noundef %0) {
  ret i32 %0
}

; CHECK: @fastmath_test{{.*}}#[[FASTMATH_TEST_ATTRS:[0-9]+]]
define noundef float @fastmath_test(float noundef %0, float noundef %1) {
  %3 = fadd fast float %0, %1
  ret float %3
}

; CHECK: @loop_test{{.*}}#[[LOOP_TEST_ATTRS:[0-9]+]]
define noundef i32 @loop_test(i32 noundef %0) {
  %2 = icmp sgt i32 %0, 0
  br i1 %2, label %5, label %3

3:                                                ; preds = %5, %1
  %4 = phi i32 [ 0, %1 ], [ %9, %5 ]
  ret i32 %4

5:                                                ; preds = %1, %5
  %6 = phi i32 [ %10, %5 ], [ 0, %1 ]
  %7 = phi i32 [ %9, %5 ], [ 0, %1 ]
  %8 = lshr i32 %6, 1
  %9 = add nuw nsw i32 %8, %7
  %10 = add nuw nsw i32 %6, 1
  %11 = icmp eq i32 %10, %0
  br i1 %11, label %3, label %5, !llvm.loop !9
}

; CHECK: @simd_test{{.*}}#[[SIMD_TEST_ATTRS:[0-9]+]]
define noundef i32 @simd_test(<4 x i32> noundef %0) {
  %2 = extractelement <4 x i32> %0, i64 0
  ret i32 %2
}

; COM: now check all the combinations

; CHECK: @simd_fastmath_test{{.*}}#[[SIMD_FASTMATH_TEST_ATTRS:[0-9]+]]
define noundef float @simd_fastmath_test(<4 x float> noundef %0) {
  %2 = extractelement <4 x float> %0, i64 0
  %3 = extractelement <4 x float> %0, i64 1
  %4 = fadd fast float %2, %3
  ret float %4
}

; CHECK: @loop_fastmath_test{{.*}}#[[LOOP_FASTMATH_TEST_ATTRS:[0-9]+]]
define noundef i32 @loop_fastmath_test(i32 noundef %0) {
  %2 = icmp sgt i32 %0, 0
  br i1 %2, label %7, label %5

3:                                                ; preds = %7
  %4 = fptosi float %12 to i32
  br label %5

5:                                                ; preds = %3, %1
  %6 = phi i32 [ 0, %1 ], [ %4, %3 ]
  ret i32 %6

7:                                                ; preds = %1, %7
  %8 = phi i32 [ %13, %7 ], [ 0, %1 ]
  %9 = phi float [ %12, %7 ], [ 0.000000e+00, %1 ]
  %10 = lshr i32 %8, 1
  %11 = sitofp i32 %10 to float
  %12 = fadd fast float %9, %11
  %13 = add nuw nsw i32 %8, 1
  %14 = icmp eq i32 %13, %0
  br i1 %14, label %3, label %7, !llvm.loop !9
}

; CHECK: @simd_loop_test{{.*}}#[[SIMD_LOOP_TEST_ATTRS:[0-9]+]]
define dso_local noundef i32 @simd_loop_test(<4 x i32> noundef %0) {
  %2 = extractelement <4 x i32> %0, i64 0
  %3 = icmp sgt i32 %2, 0
  br i1 %3, label %6, label %4

4:                                                ; preds = %6, %1
  %5 = phi i32 [ 0, %1 ], [ %10, %6 ]
  ret i32 %5

6:                                                ; preds = %1, %6
  %7 = phi i32 [ %11, %6 ], [ 0, %1 ]
  %8 = phi i32 [ %10, %6 ], [ 0, %1 ]
  %9 = lshr i32 %7, 1
  %10 = add nuw nsw i32 %9, %8
  %11 = add nuw nsw i32 %7, 1
  %12 = icmp eq i32 %11, %2
  br i1 %12, label %4, label %6, !llvm.loop !9
}

; CHECK: @simd_loop_fastmath_test{{.*}}#[[SIMD_LOOP_FASTMATH_TEST_ATTRS:[0-9]+]]
define noundef i32 @simd_loop_fastmath_test(<4 x i32> noundef %0) {
  %2 = extractelement <4 x i32> %0, i64 0
  %3 = icmp sgt i32 %2, 0
  br i1 %3, label %8, label %6

4:                                                ; preds = %8
  %5 = fptosi float %13 to i32
  br label %6

6:                                                ; preds = %4, %1
  %7 = phi i32 [ 0, %1 ], [ %5, %4 ]
  ret i32 %7

8:                                                ; preds = %1, %8
  %9 = phi i32 [ %14, %8 ], [ 0, %1 ]
  %10 = phi float [ %13, %8 ], [ 0.000000e+00, %1 ]
  %11 = lshr i32 %9, 1
  %12 = sitofp i32 %11 to float
  %13 = fadd fast float %10, %12
  %14 = add nuw nsw i32 %9, 1
  %15 = icmp eq i32 %14, %2
  br i1 %15, label %4, label %8, !llvm.loop !9
}

; COM: check for fvar and reloc annotations on functions used by other globals

@func_gv = global i32 (i32)* @func_in_gv, align 8

; CHECK: @func_in_gv{{.*}}#[[FUNC_IN_GV_ATTRS:[0-9]+]]
define noundef i32 @func_in_gv(i32 noundef returned %0) {
  ret i32 %0
}

@aliaser = alias i32 (i32)*, bitcast (i32 (i32)* @aliasee to i32 (i32)**)

; CHECK: @aliasee{{.*}}#[[ALIASEE_ATTRS:[0-9]+]]
define i32 @aliasee(i32 noundef returned %0) {
  ret i32 %0
}

; COM: check for reloc annotations on functions used by other functions
; CHECK: @cloned{{.*}}#[[CLONED_RELOC_ATTRS:[0-9]+]]
define noundef float @cloned(float noundef %0, float noundef %1) {
  %3 = fadd fast float %0, %1
  ret float %3
}

define noundef i32 @uncloned(i32 noundef %0) {
  %2 = sitofp i32 %0 to float
  %3 = call noundef float @cloned(float noundef %2, float noundef %2)
  %4 = fptosi float %3 to i32
  ret i32 %4
}

; COM: Note that these strings are hex-encoded bits of the target indices that will be cloned
; CHECK-DAG: attributes #[[BORING_ATTRS]] = { "julia.mv.clones"="2" }
; CHECK-DAG: attributes #[[FASTMATH_TEST_ATTRS]] = { "julia.mv.clones"="6" }
; CHECK-DAG: attributes #[[LOOP_TEST_ATTRS]] = { "julia.mv.clones"="A" }
; CHECK-DAG: attributes #[[SIMD_TEST_ATTRS]] = { "julia.mv.clones"="12" }
; CHECK-DAG: attributes #[[SIMD_FASTMATH_TEST_ATTRS]] = { "julia.mv.clones"="16" }
; CHECK-DAG: attributes #[[LOOP_FASTMATH_TEST_ATTRS]] = { "julia.mv.clones"="E" }
; CHECK-DAG: attributes #[[SIMD_LOOP_TEST_ATTRS]] = { "julia.mv.clones"="1A" }
; CHECK-DAG: attributes #[[SIMD_LOOP_FASTMATH_TEST_ATTRS]] = { "julia.mv.clones"="1E" }
; CHECK-DAG: attributes #[[FUNC_IN_GV_ATTRS]]
; CHECK-SAME: "julia.mv.clones"="2"
; CHECK-SAME: "julia.mv.fvar"
; CHECK-DAG: attributes #[[ALIASEE_ATTRS]]
; CHECK-SAME: "julia.mv.clones"="2"
; CHECK-SAME: "julia.mv.reloc"
; CHECK-DAG: attributes #[[CLONED_RELOC_ATTRS]]
; CHECK-SAME: "julia.mv.clones"="6"
; CHECK-SAME: "julia.mv.reloc"

; CHECK-LABEL: !llvm.module.flags

!llvm.module.flags = !{!0, !1, !2}

; CHECK-DAG: julia.mv.enable
; CHECK-DAG: julia.mv.skipcloning
; CHECK-DAG: julia.mv.specs
; CHECK-DAG: julia.mv.annotated
; CHECK-DAG: julia.mv.veccall

!0 = !{i32 1, !"julia.mv.enable", i32 1}
!1 = !{i32 1, !"julia.mv.skipcloning", i32 1}
!2 = !{i32 1, !"julia.mv.specs", !3}
!3 = !{!4, !5, !6, !7, !8}
!4 = !{!"cpubase", !"nofeatures", i32 0, i32 2}
!5 = !{!"cpucloneall", !"cloneall", i32 0, i32 2}
!6 = !{!"cpufastmath", !"fastmathclone", i32 0, i32 4}
!7 = !{!"cpuloop", !"loopclone", i32 0, i32 8}
!8 = !{!"cpusimd", !"simdclone", i32 0, i32 16}
!9 = !{!9}
