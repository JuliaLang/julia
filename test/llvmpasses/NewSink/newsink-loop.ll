; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that we correctly handle error blocks inside loops
; Adapted from D136218 multiblock-sink.ll patterns

define i64 @test_loop_with_error_block(ptr %array, i64 %n) {
; CHECK-LABEL: @test_loop_with_error_block
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  ; The store should be sunk to error block
  ; CHECK: loop:
  ; CHECK-NOT: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.latch, label %error

loop.latch:
  %ptr = getelementptr i64, ptr %array, i64 %i
  %val = load i64, ptr %ptr, align 8
  %i.next = add i64 %i, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

error:
  ; CHECK: error:
  ; CHECK: store i64 %i, ptr %tuple
  call void @throw_bounds_error(ptr %tuple)
  unreachable

exit:
  ret i64 0
}

; Test: Loop where error block is the only exit but still ends in unreachable
define void @test_infinite_loop_with_error(ptr %array, i64 %n) {
; CHECK-LABEL: @test_infinite_loop_with_error
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  ; Store should be sunk - only used in error block
  ; CHECK: loop:
  ; CHECK-NOT: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  %i.next = add i64 %i, 1
  br i1 %cmp, label %loop, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %i, ptr %tuple
  call void @throw_bounds_error(ptr %tuple)
  unreachable
}

; Test: Value used in loop latch should NOT be sunk
define i64 @test_loop_value_used_in_latch(ptr %array, i64 %n) {
; CHECK-LABEL: @test_loop_value_used_in_latch
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  ; This store should NOT be sunk - value is loaded in loop.latch
  ; CHECK: loop:
  ; CHECK: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.latch, label %error

loop.latch:
  ; Load prevents sinking
  %loaded = load i64, ptr %tuple, align 8
  %i.next = add i64 %loaded, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

error:
  call void @throw_bounds_error(ptr %tuple)
  unreachable

exit:
  ret i64 0
}

declare void @throw_bounds_error(ptr)

; ============================================================================
; Tests to ensure we don't sink INTO loops
; (More comprehensive nested loop tests are in newsink-loop-complex.ll)
; ============================================================================

; Test: Store defined before loop, read only in loop body
; Should NOT be sunk because it would execute on every iteration
define i64 @test_no_sink_store_into_loop(i64 %a, i64 %n) {
; CHECK-LABEL: @test_no_sink_store_into_loop
entry:
  %p = alloca i64, align 8
  ; Store should NOT be sunk into the loop
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %p
  store i64 %a, ptr %p, align 8
  br label %loop.header

loop.header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.body, label %exit

loop.body:
  %v = load i64, ptr %p, align 8
  %result = add i64 %v, %i
  br label %loop.latch

loop.latch:
  %i.next = add i64 %i, 1
  br label %loop.header

exit:
  %final = load i64, ptr %p, align 8
  ret i64 %final
}
