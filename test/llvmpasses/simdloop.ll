; RUN: opt -load libjulia.so -LowerSIMDLoop -S %s | FileCheck %s

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

!1 = !{}
