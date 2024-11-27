; COM: NewPM-only test, tests that memoryssa is preserved correctly

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(loop-mssa(LowerSIMDLoop),print<memoryssa>)' -S -o /dev/null %s 2>&1 | FileCheck %s --check-prefixes=CHECK

; CHECK-LABEL: MemorySSA for function: simd_test
; CHECK-LABEL: @simd_test(
define void @simd_test(double *%a, double *%b) {
; CHECK: top:
top:
  br label %loop
; CHECK: loop:
loop:
; CHECK-NEXT: [[MPHI:[0-9]+]] = MemoryPhi({top,liveOnEntry},{loop,[[MSSA_USE:[0-9]+]]})
  %i = phi i64 [0, %top], [%nexti, %loop]
  %aptr = getelementptr double, double *%a, i64 %i
  %bptr = getelementptr double, double *%b, i64 %i
; CHECK: MemoryUse([[MPHI]])
; CHECK: llvm.mem.parallel_loop_access
  %aval = load double, double *%aptr
; CHECK: MemoryUse([[MPHI]])
  %bval = load double, double *%aptr
  %cval = fadd double %aval, %bval
; CHECK: [[MSSA_USE]] = MemoryDef([[MPHI]])
  store double %cval, double *%bptr
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !1
loopdone:
  ret void
}

; CHECK-LABEL: MemorySSA for function: simd_test_sub2
; CHECK-LABEL: @simd_test_sub2(
define double @simd_test_sub2(double *%a) {
top:
  br label %loop
loop:
  %i = phi i64 [0, %top], [%nexti, %loop]
  %v = phi double [0.000000e+00, %top], [%nextv, %loop]
  %aptr = getelementptr double, double *%a, i64 %i
; CHECK: MemoryUse(liveOnEntry) 
  %aval = load double, double *%aptr
  %nextv = fsub double %v, %aval
; CHECK: fsub reassoc contract double %v, %aval
  %nexti = add i64 %i, 1
  %done = icmp sgt i64 %nexti, 500
  br i1 %done, label %loopdone, label %loop, !llvm.loop !0
loopdone:
  ret double %nextv
}

!0 = distinct !{!0, !"julia.simdloop"}
!1 = distinct !{!1, !"julia.simdloop", !"julia.ivdep"}