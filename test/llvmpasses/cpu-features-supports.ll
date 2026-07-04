; This file is a part of Julia. License is MIT: https://julialang.org/license

; Verify the CPUFeatures pass folds julia.cpu.supports.* against each
; function's per-attribute target-features / target-cpu. Covers:
;   - feature implications (e.g. +avx512f implies +fma);
;   - per-clone correctness — same body, different target-features per clone;
;   - unknown feature names fold to false (no vacuous match).

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -mtriple=x86_64-linux-gnu -passes='CPUFeatures,simplifycfg' -S %s | FileCheck %s
; REQUIRES: x86_64

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-linux-gnu"

declare i1 @julia.cpu.supports.fma()
declare i1 @julia.cpu.supports.avx2()
declare i1 @julia.cpu.supports.avx512f()
declare i1 @julia.cpu.supports.sse4.2()

declare i32 @sink_true()
declare i32 @sink_false()

; CHECK-LABEL: @explicit_fma
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @explicit_fma() #0 {
  %v = call i1 @julia.cpu.supports.fma()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; +avx512f implies +fma via the subtarget.
; CHECK-LABEL: @avx512f_implies_fma
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @avx512f_implies_fma() #1 {
  %v = call i1 @julia.cpu.supports.fma()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @avx512f_implies_avx2
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @avx512f_implies_avx2() #1 {
  %v = call i1 @julia.cpu.supports.avx2()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @no_fma
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_false
define i32 @no_fma() #2 {
  %v = call i1 @julia.cpu.supports.fma()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @sse42
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @sse42() #3 {
  %v = call i1 @julia.cpu.supports.sse4.2()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; Multiversioning shape: same body, three different per-clone targets.
; CHECK-LABEL: @clone_avx512
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_true
define i32 @clone_avx512() #1 {
  %v = call i1 @julia.cpu.supports.avx512f()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @clone_haswell
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_false
define i32 @clone_haswell() #4 {
  %v = call i1 @julia.cpu.supports.avx512f()
  br i1 %v, label %T, label %F
T:
  %r1 = call i32 @sink_true()
  ret i32 %r1
F:
  %r2 = call i32 @sink_false()
  ret i32 %r2
}

; CHECK-LABEL: @clone_generic
; CHECK-NOT: julia.cpu.supports
; CHECK: call i32 @sink_false
define i32 @clone_generic() #5 {
  %v = call i1 @julia.cpu.supports.avx2()
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

attributes #0 = { "target-features"="+fma" }
attributes #1 = { "target-features"="+avx512f" }
attributes #2 = { "target-features"="-fma" }
attributes #3 = { "target-features"="+sse4.2" }
attributes #4 = { "target-cpu"="haswell" }
attributes #5 = { "target-cpu"="x86-64" }
