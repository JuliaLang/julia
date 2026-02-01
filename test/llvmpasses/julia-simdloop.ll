; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='loop(LowerSIMDLoop)' -S %s | FileCheck %s

; CHECK-LABEL: @simd_test(
define void @simd_test(ptr %a, ptr %b) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %aptr = getelementptr double, ptr %a, i64 %i
  %bptr = getelementptr double, ptr %b, i64 %i
; CHECK: llvm.mem.parallel_loop_access
  %aval = load double, ptr %aptr
  %bval = load double, ptr %aptr
  %cval = fadd double %aval, %bval
  store double %cval, ptr %bptr
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !1
loopdone:
  ret void
}

; CHECK-LABEL: @simd_test_sub(
define double @simd_test_sub(ptr %a) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %v = phi double [0.000000e+00, %top], [%nextv, %loop]
  %aptr = getelementptr double, ptr %a, i64 %i
; CHECK: llvm.mem.parallel_loop_access
  %aval = load double, ptr %aptr
  %nextv = fsub double %v, %aval
; CHECK: fsub reassoc contract double %v, %aval
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !1
loopdone:
  ret double %nextv
}

; CHECK-LABEL: @simd_test_sub2(
define double @simd_test_sub2(ptr %a) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %v = phi double [0.000000e+00, %top], [%nextv, %loop]
  %aptr = getelementptr double, ptr %a, i64 %i
  %aval = load double, ptr %aptr
  %nextv = fsub double %v, %aval
; CHECK: fsub reassoc contract double %v, %aval
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !0
loopdone:
  ret double %nextv
}

; CHECK-LABEL: @simd_test_sub4(
define double @simd_test_sub4(ptr %a) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %v = phi double [0.000000e+00, %top], [%nextv, %loop]
  %aptr = getelementptr double, double *%a, i64 %i
  %aval = load double, double *%aptr
  %nextv2 = fmul double %aval, %aval
  ; CHECK: fmul contract double %aval, %aval
  %nextv = fsub double %v, %nextv2
; CHECK: fsub reassoc contract double %v, %nextv2
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !0
loopdone:
  ret double %nextv
}

; Tests if we correctly pass through other metadata
; CHECK-LABEL: @disabled(
define i32 @disabled(i32* noalias nocapture %a, i32* noalias nocapture readonly %b, i32 %N) {
entry:
  br label %for.body

for.body:                                         ; preds = %for.body, %entry
  %indvars.iv = phi i64 [ 0, %entry ], [ %indvars.iv.next, %for.body ]
  %arrayidx = getelementptr inbounds i32, i32* %b, i64 %indvars.iv
  %0 = load i32, i32* %arrayidx, align 4
  %add = add nsw i32 %0, %N
  %arrayidx2 = getelementptr inbounds i32, i32* %a, i64 %indvars.iv
  store i32 %add, i32* %arrayidx2, align 4
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  %exitcond = icmp eq i64 %indvars.iv.next, 48
; CHECK: br {{.*}} !llvm.loop [[LOOP:![0-9]+]]
  br i1 %exitcond, label %for.end, label %for.body, !llvm.loop !2

for.end:                                          ; preds = %for.body
  %1 = load i32, i32* %a, align 4
  ret i32 %1
}

; Check that we don't add contract to non loop things
; CHECK-LABEL: @dont_add_no_loop(
define double @dont_add_no_loop(ptr nocapture noundef nonnull readonly align 8 dereferenceable(72) %"a::Tuple", ptr nocapture noundef nonnull readonly align 8 dereferenceable(24) %"b::Tuple") #0 {
top:
   %"a::Tuple[9]_ptr" = getelementptr inbounds i8, ptr %"a::Tuple", i64 64
   %"b::Tuple[3]_ptr" = getelementptr inbounds i8, ptr %"b::Tuple", i64 16
   %"a::Tuple[6]_ptr" = getelementptr inbounds i8, ptr %"a::Tuple", i64 40
   %"b::Tuple[2]_ptr" = getelementptr inbounds i8, ptr %"b::Tuple", i64 8
   %"a::Tuple[3]_ptr" = getelementptr inbounds i8, ptr %"a::Tuple", i64 16
   %"a::Tuple[3]_ptr.unbox" = load double, ptr %"a::Tuple[3]_ptr", align 8
   %"b::Tuple.unbox" = load double, ptr %"b::Tuple", align 8
   %0 = fmul double %"a::Tuple[3]_ptr.unbox", %"b::Tuple.unbox"
; CHECK: fmul double %
   %"a::Tuple[6]_ptr.unbox" = load double, ptr %"a::Tuple[6]_ptr", align 8
   %"b::Tuple[2]_ptr.unbox" = load double, ptr %"b::Tuple[2]_ptr", align 8
   %1 = fmul contract double %"a::Tuple[6]_ptr.unbox", %"b::Tuple[2]_ptr.unbox"
   %2 = fadd contract double %0, %1
   %"a::Tuple[9]_ptr.unbox" = load double, ptr %"a::Tuple[9]_ptr", align 8
   %"b::Tuple[3]_ptr.unbox" = load double, ptr %"b::Tuple[3]_ptr", align 8
   %3 = fmul contract double %"a::Tuple[9]_ptr.unbox", %"b::Tuple[3]_ptr.unbox"
   %4 = fadd contract double %2, %3
   ret double %4
}


!0 = distinct !{!0, !"julia.simdloop"}
!1 = distinct !{!1, !"julia.simdloop", !"julia.ivdep"}
!2 = distinct !{!2, !"julia.simdloop", !"julia.ivdep", !3}
!3 = !{!"llvm.loop.vectorize.disable", i1 0}
