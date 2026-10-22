; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='VersionUnalignedLoads' -S %s | FileCheck %s
; REQUIRES: riscv64
target datalayout = "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128"
target triple = "riscv64-unknown-linux-gnu"

; CHECK-LABEL: @slow
; CHECK: %is_aligned = icmp
; CHECK: load i64, ptr %p, align 8
; CHECK: load i64, ptr %p, align 1
define i64 @slow(ptr %p) #0 {
  %x = load i64, ptr %p, align 1
  ret i64 %x
}

; CHECK-LABEL: @fast
; CHECK-NOT: is_aligned
; CHECK: load i64, ptr %p, align 1
define i64 @fast(ptr %p) #1 {
  %x = load i64, ptr %p, align 1
  ret i64 %x
}

attributes #0 = { "target-cpu"="generic-rv64" "target-features"="+64bit,+a,+c,+d,+f,+m,-unaligned-scalar-mem" }
attributes #1 = { "target-cpu"="generic-rv64" "target-features"="+64bit,+a,+c,+d,+f,+m,+unaligned-scalar-mem" }
