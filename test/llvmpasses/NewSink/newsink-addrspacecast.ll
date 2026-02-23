; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test that stores are not incorrectly sunk past calls that read from the
; same memory. This is a regression test for a bug where MSSA walk didn't
; follow through MemoryDefs in the source block.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

declare i64 @reader(ptr nocapture readonly) nounwind

; CHECK-LABEL: @test_store_before_call
define void @test_store_before_call(i1 %cond, ptr %val) {
entry:
  %alloca = alloca ptr, align 8
  br label %loop

loop:
  br i1 %cond, label %body, label %exit

body:
; The store must stay before the call since the call reads from %alloca
; CHECK: body:
; CHECK-NEXT: store ptr %val, ptr %alloca
; CHECK-NEXT: %result = call i64 @reader
  store ptr %val, ptr %alloca, align 8
  %result = call i64 @reader(ptr %alloca)
  br i1 %cond, label %loop, label %check

check:
  br i1 %cond, label %exit, label %error

exit:
  ret void

error:
  unreachable
}
