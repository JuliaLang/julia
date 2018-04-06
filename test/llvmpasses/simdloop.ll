; RUN: opt -load libjulia%shlibext -LowerSIMDLoop -S %s | FileCheck %s

define void @simd_test(double *%a, double *%b) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %aptr = getelementptr double, double *%a, i64 %i
  %bptr = getelementptr double, double *%b, i64 %i
; CHECK: llvm.mem.parallel_loop_access
  %aval = load double, double *%aptr
  %bval = load double, double *%aptr
  %cval = fadd double %aval, %bval
  store double %cval, double *%bptr
  %nexti = add i64 %i, 1, !simd_loop !1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop
loopdone:
  ret void
}

define double @simd_test_sub(double *%a) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %v = phi double [0.000000e+00, %top], [%nextv, %loop]
  %aptr = getelementptr double, double *%a, i64 %i
; CHECK: llvm.mem.parallel_loop_access
  %aval = load double, double *%aptr
  %nextv = fsub double %v, %aval
; CHECK: fsub fast double %v, %aval
  %nexti = add i64 %i, 1, !simd_loop !1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop
loopdone:
  ret double %nextv
}

!1 = !{}
