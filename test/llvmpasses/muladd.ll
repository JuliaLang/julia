; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='CombineMulAdd' -S %s | FileCheck %s


; CHECK-LABEL: @fast_muladd1
define double @fast_muladd1(double %a, double %b, double %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul double %a, %b
  %v2 = fadd fast double %v1, %c
; CHECK: ret double
  ret double %v2
}

; CHECK-LABEL: @fast_mulsub1
define double @fast_mulsub1(double %a, double %b, double %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul double %a, %b
  %v2 = fsub fast double %v1, %c
; CHECK: ret double
  ret double %v2
}

; CHECK-LABEL: @fast_mulsub_vec1
define <2 x double> @fast_mulsub_vec1(<2 x double> %a, <2 x double> %b, <2 x double> %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul <2 x double> %a, %b
  %v2 = fsub fast <2 x double> %c, %v1
; CHECK: ret <2 x double>
  ret <2 x double> %v2
}

; COM: Should not mark fmul as contract when multiple uses of fmul exist
; CHECK-LABEL: @slow_muladd1
define double @slow_muladd1(double %a, double %b, double %c) {
top:
; CHECK: %v1 = fmul double %a, %b
  %v1 = fmul double %a, %b
; CHECK: %v2 = fadd fast double %v1, %c
  %v2 = fadd fast double %v1, %c
; CHECK: %v3 = fadd fast double %v1, %b
  %v3 = fadd fast double %v1, %b
; CHECK: %v4 = fadd fast double %v3, %v2
  %v4 = fadd fast double %v3, %v2
; CHECK: ret double %v4
  ret double %v4
}

; COM: Should not mark fadd->fadd fast as contract
; CHECK-LABEL: @slow_addadd1
define double @slow_addadd1(double %a, double %b, double %c) {
top:
; CHECK: %v1 = fadd double %a, %b
  %v1 = fadd double %a, %b
; CHECK: %v2 = fadd fast double %v1, %c
  %v2 = fadd fast double %v1, %c
; CHECK: ret double %v2
  ret double %v2
}
