; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='CPUFeatures,simplifycfg' -S %s | FileCheck %s
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-linux-gnu"

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
