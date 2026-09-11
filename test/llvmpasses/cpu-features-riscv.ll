; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='CPUFeatures,simplifycfg' -S %s | FileCheck %s
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64-unknown-linux-gnu"

declare i1 @julia.cpu.have_fma.f64()
declare i1 @julia.cpu.have_fma.f32()
declare double @with_fma(double %0, double %1, double %2)
declare double @without_fma(double %0, double %1, double %2)
declare float @with_fma_f32(float %0, float %1, float %2)
declare float @without_fma_f32(float %0, float %1, float %2)

; the D extension provides fmadd.d

; CHECK: @fma_f64_d
define double @fma_f64_d(double %0, double %1, double %2) #0 {
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

; F alone provides fmadd.s but not fmadd.d

; CHECK: @fma_f64_f_only
define double @fma_f64_f_only(double %0, double %1, double %2) #1 {
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

; CHECK: @fma_f32_f_only
define float @fma_f32_f_only(float %0, float %1, float %2) #1 {
top:
  %3 = call i1 @julia.cpu.have_fma.f32()
  br i1 %3, label %L1, label %L2

; CHECK-NOT: @julia.cpu.have_fma
; CHECK: @with_fma_f32
L1:                                               ; preds = %top
  %4 = call float @with_fma_f32(float %0, float %1, float %2)
  ret float %4

L2:                                               ; preds = %top
  %5 = call float @without_fma_f32(float %0, float %1, float %2)
  ret float %5
}

; a soft-float target has no fused multiply-add at all

; CHECK: @fma_f64_soft
define double @fma_f64_soft(double %0, double %1, double %2) #2 {
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

attributes #0 = { "target-features"="+m,+a,+f,+d,+c" }
attributes #1 = { "target-features"="+m,+a,+f,+c" }
attributes #2 = { "target-features"="+m,+a,+c" }
