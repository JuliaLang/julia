; RUNx: opt --mtriple=`llvm-config --host-target` -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='require<machine-module>,CPUFeatures' -S %s | FileCheck %s --check-prefixes=CHECK,CHECK-any
; RUNx: opt --mtriple=x86_64-unknown-linux-gnu -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='require<machine-module>,CPUFeatures' -S %s | FileCheck %s --check-prefixes=CHECK,CHECK-generic
; RUNx: opt --mtriple=aarch64-unknown-linux-gnu -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='require<machine-module>,CPUFeatures' -S %s | FileCheck %s --check-prefixes=CHECK,CHECK-aarch64
; RUNx: opt --mtriple=x86_64-unknown-linux-gnu --march=avx512 -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='require<machine-module>,CPUFeatures' -S %s | FileCheck %s --check-prefixes=CHECK,CHECK-avx512
; RUN: true

declare i1 @julia.cpu.have_fma.f32()
declare i1 @julia.cpu.have_fma.f64()

; CHECK-LABEL: @havefma_test(
; CHECK-LABEL: top:
; CHECK-any-NEXT: %0 = and i1
; CHECK-generic-NEXT: %0 = and i1 false, false
; CHECK-avx512-NEXT: %0 = and i1 false, false
; CHECK-aarch64-NEXT: %0 = and i1 true, true
; CHECK-NEXT: ret i1 %0

define i1 @havefma_test() {
top:
  %0 = call i1 @julia.cpu.have_fma.f32()
  %1 = call i1 @julia.cpu.have_fma.f64()
  %2 = and i1 %0, %1
  ret i1 %2
}
