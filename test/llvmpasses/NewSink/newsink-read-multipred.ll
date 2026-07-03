; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that memory reads are not sunk to multi-predecessor targets.
;
; When a load is sunk from block A to a merge block M that also has
; predecessor B, writes on the A→B→M path can modify the loaded location.
; Since we only check for clobbers in the source block, sinking to a
; multi-predecessor target is unsafe.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare void @modify(ptr)

; The load of %val must NOT be sunk to %merge because the modify→merge
; path writes to %ptr (via @modify), changing what the load would read.
; This is the heappop! pattern: x = xs[1] is sunk past percolate_down!
; which overwrites xs[1].
; CHECK-LABEL: @test_no_sink_read_to_multipred
; CHECK: entry:
; CHECK: %val = load i64, ptr %ptr
; CHECK: br i1 %cond
define i64 @test_no_sink_read_to_multipred(ptr %ptr, i1 %cond) {
entry:
  %val = load i64, ptr %ptr, align 8
  br i1 %cond, label %merge, label %modify_path

modify_path:
  call void @modify(ptr %ptr)
  br label %merge

merge:
  ret i64 %val
}

; Negative test: single-predecessor target is fine for read sinking.
; CHECK-LABEL: @test_sink_read_to_single_pred
; CHECK: entry:
; CHECK-NOT: load i64, ptr %ptr
; CHECK: br i1 %cond
; CHECK: single_pred:
; CHECK: %val = load i64, ptr %ptr
define i64 @test_sink_read_to_single_pred(ptr %ptr, i1 %cond) {
entry:
  %val = load i64, ptr %ptr, align 8
  br i1 %cond, label %other, label %single_pred

single_pred:
  ret i64 %val

other:
  ret i64 0
}
