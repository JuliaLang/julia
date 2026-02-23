; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test that loads are not incorrectly sunk past clobbering writes in
; intermediate blocks. This is a regression test for a bug where load
; sinking didn't check for writes in blocks between source and target.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

declare void @use(i64)

; CHECK-LABEL: @test_load_not_sunk_past_clobber
define void @test_load_not_sunk_past_clobber(i1 %cond) {
entry:
  %p = alloca i64, align 8
  store i64 0, ptr %p, align 8
; The load must stay here - it must not be sunk past the store in %then
; CHECK: entry:
; CHECK: %v = load i64, ptr %p
; CHECK: br i1 %cond
  %v = load i64, ptr %p, align 8
  br i1 %cond, label %then, label %error

then:
  store i64 42, ptr %p, align 8
  br label %use_block

use_block:
; CHECK: use_block:
; CHECK-NOT: load
; CHECK: call void @use(i64 %v)
  call void @use(i64 %v)
  ret void

error:
  unreachable
}
