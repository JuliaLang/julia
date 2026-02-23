; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test: stores must not be sunk when target doesn't post-dominate source
;
; In this loop, sinking stores from loop_header to use_state_path would be
; wrong because iterations taking skip_state_path would miss the stores,
; causing stale values when the loop continues.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

declare void @use_state(ptr addrspace(11) nocapture readonly)
declare i1 @get_flag()
declare i64 @get_value()

; CHECK-LABEL: @test_loop_backedge_sink
; CHECK: loop_header:
; CHECK-NEXT: phi i64
; CHECK-NEXT: phi i64
; CHECK: store i64 %iter_val
; CHECK: store i64 %state_val
define void @test_loop_backedge_sink() {
entry:
  %state = alloca [7 x i64], align 8
  %flag = call i1 @get_flag()
  br i1 %flag, label %setup_path, label %direct_path

setup_path:
  %v1 = call i64 @get_value()
  %v2 = call i64 @get_value()
  br label %loop_header

direct_path:
  br label %loop_header

loop_header:
  %iter_val = phi i64 [ %v1, %setup_path ], [ 0, %direct_path ], [ %next_val, %loop_latch ]
  %state_val = phi i64 [ %v2, %setup_path ], [ 0, %direct_path ], [ %new_state, %loop_latch ]
  store i64 %iter_val, ptr %state, align 8
  %ptr2 = getelementptr i8, ptr %state, i64 8
  store i64 %state_val, ptr %ptr2, align 8
  br i1 %flag, label %use_state_path, label %skip_state_path

use_state_path:
  %as11 = addrspacecast ptr %state to ptr addrspace(11)
  call void @use_state(ptr addrspace(11) %as11)
  br label %loop_latch

skip_state_path:
  br label %loop_latch

loop_latch:
  %next_val = add i64 %iter_val, 1
  %new_state = add i64 %state_val, 1
  %done = icmp eq i64 %next_val, 10
  br i1 %done, label %exit, label %loop_header

exit:
  ret void
}
