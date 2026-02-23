; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test: stores in loop headers must not be sunk to successors
; A loop header has multiple predecessors (entry + back-edge). Sinking stores
; to a successor means they only execute on one path, not on subsequent iterations.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

declare void @use_state(ptr)
declare i1 @get_cond()

; CHECK-LABEL: @test_loop_header_store
define void @test_loop_header_store() {
entry:
  %state = alloca [2 x i64], align 8
  br label %loop_header

; The stores MUST stay in the loop header, not be sunk to exit_path
; CHECK-LABEL: loop_header:
; CHECK: phi i64
; CHECK: store i64 %i, ptr %state
; CHECK: store i64 42, ptr %ptr2
loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  ; These stores update state based on loop iteration
  store i64 %i, ptr %state, align 8
  %ptr2 = getelementptr i8, ptr %state, i64 8
  store i64 42, ptr %ptr2, align 8
  %cond = call i1 @get_cond()
  br i1 %cond, label %loop_latch, label %exit_path

loop_latch:
  %next = add i64 %i, 1
  br label %loop_header

exit_path:
  ; State is used here - if stores were sunk here, they'd only execute once
  ; but loop iterations need them to execute each time through the header
  call void @use_state(ptr %state)
  ret void
}

; Similar test with memcpy in loop header
; CHECK-LABEL: @test_loop_header_memcpy
define void @test_loop_header_memcpy(ptr %src) {
entry:
  %state = alloca [16 x i8], align 8
  br label %loop_header

; CHECK-LABEL: loop_header:
; CHECK: phi i64
; CHECK: call void @llvm.memcpy
loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  call void @llvm.memcpy.p0.p0.i64(ptr %state, ptr %src, i64 16, i1 false)
  %cond = call i1 @get_cond()
  br i1 %cond, label %loop_latch, label %exit_path

loop_latch:
  %next = add i64 %i, 1
  br label %loop_header

exit_path:
  call void @use_state(ptr %state)
  ret void
}

declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
