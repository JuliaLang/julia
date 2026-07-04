; This file is a part of Julia. License is MIT: https://julialang.org/license

; Same coverage as cpu-features-supports.ll, but targeting AArch64.

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -mtriple=aarch64-linux-gnu -passes='CPUFeatures,simplifycfg' -S %s | FileCheck %s
; REQUIRES: aarch64

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128"
target triple = "aarch64-linux-gnu"

declare i1 @julia.cpu.supports.neon()
declare i1 @julia.cpu.supports.sve()
declare i1 @julia.cpu.supports.sve2()
declare i1 @julia.cpu.supports.dotprod()
declare i1 @julia.cpu.supports.fullfp16()

declare i32 @sink_true()
declare i32 @sink_false()

; CHECK-LABEL: @explicit_neon
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @explicit_neon() #0 {
  %v = call i1 @julia.cpu.supports.neon()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; +sve2 implies +sve via the subtarget.
; CHECK-LABEL: @sve2_implies_sve
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @sve2_implies_sve() #1 {
  %v = call i1 @julia.cpu.supports.sve()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; A target-cpu that has the feature folds to true.
; CHECK-LABEL: @cortex_a78_has_dotprod
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @cortex_a78_has_dotprod() #2 {
  %v = call i1 @julia.cpu.supports.dotprod()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; Multiversioning shape: same body, different per-clone targets.
; CHECK-LABEL: @clone_sve2
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @clone_sve2() #1 {
  %v = call i1 @julia.cpu.supports.sve2()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @clone_a78
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_false
define i32 @clone_a78() #2 {
  %v = call i1 @julia.cpu.supports.sve2()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; Unknown feature names fold to false (no vacuous match in checkFeatures).
; CHECK-LABEL: @unknown_feature
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_false
define i32 @unknown_feature() #1 {
  %v = call i1 @julia.cpu.supports.totally_made_up_feature()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

declare i1 @julia.cpu.supports.totally_made_up_feature()

attributes #0 = { "target-features"="+neon" }
attributes #1 = { "target-features"="+sve2" }
attributes #2 = { "target-cpu"="cortex-a78" }
