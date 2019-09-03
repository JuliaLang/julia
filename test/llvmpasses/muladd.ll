; RUN: opt -load libjulia%shlibext -CombineMulAdd -S %s | FileCheck %s

define double @fast_muladd1(double %a, double %b, double %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul double %a, %b
  %v2 = fadd fast double %v1, %c
; CHECK: ret double
  ret double %v2
}

define double @fast_mulsub1(double %a, double %b, double %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul double %a, %b
  %v2 = fsub fast double %v1, %c
; CHECK: ret double
  ret double %v2
}

define <2 x double> @fast_mulsub_vec1(<2 x double> %a, <2 x double> %b, <2 x double> %c) {
top:
; CHECK: {{contract|fmuladd}}
  %v1 = fmul <2 x double> %a, %b
  %v2 = fsub fast <2 x double> %c, %v1
; CHECK: ret <2 x double>
  ret <2 x double> %v2
}
