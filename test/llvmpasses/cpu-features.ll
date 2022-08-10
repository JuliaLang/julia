; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -CPUFeatures -simplifycfg -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='CPUFeatures,simplifycfg' -S %s | FileCheck %s

declare i1 @julia.cpu.have_fma.f64()
declare double @with_fma(double %0, double %1, double %2)
declare double @without_fma(double %0, double %1, double %2)

; CHECK: @fma1
define double @fma1(double %0, double %1, double %2) #0 {
top:
  %3 = call i1 @julia.cpu.have_fma.f64()
  br i1 %3, label %L1, label %L2

; CHECK-NOT: @julia.cpu.have_fma
; CHECK: @with_fma
L1:                                               ; preds = %top
  %4 = call double @with_fma(double %0, double %1, double %2)
  ret double %4

L2:                                               ; preds = %top
  %5 = call double @without_fma(double %0, double %1, double %2)
  ret double %5
}

; CHECK: @fma2
define double @fma2(double %0, double %1, double %2) #1 {
top:
  %3 = call i1 @julia.cpu.have_fma.f64()
  br i1 %3, label %L1, label %L2

; CHECK-NOT: @julia.cpu.have_fma
; CHECK: @without_fma
L1:                                               ; preds = %top
  %4 = call double @with_fma(double %0, double %1, double %2)
  ret double %4

L2:                                               ; preds = %top
  %5 = call double @without_fma(double %0, double %1, double %2)
  ret double %5
}

attributes #0 = { "target-features"="+fma" }
attributes #1 = { "target-features"="-fma" }
