; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s
; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -debug-only=new-sink -S %s 2>&1 | FileCheck %s --check-prefix=CHECK-ITER
; REQUIRES: asserts

; Tests for deep sinking - instructions should sink multiple blocks in one step
; The iteration check verifies we sink in minimal iterations (not one block at a time)

declare void @use(i64)
declare void @throw_error()

; Test: Deep sinking to error path - instruction should sink directly to deepest use block
; Without deep sinking this would need 4 iterations (entry->block1->block2->block3->done)
; With deep sinking we need only 2 iterations (entry->block3->done)
; CHECK-ITER: NEWSINK ITERATION #1 on test_deep_sink_error
; CHECK-ITER: NEWSINK ITERATION #2 on test_deep_sink_error
; CHECK-ITER-NOT: NEWSINK ITERATION #3 on test_deep_sink_error
define i64 @test_deep_sink_error(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_deep_sink_error
; CHECK: entry:
; CHECK-NEXT: %cmp = icmp
; CHECK-NEXT: br i1 %cmp
; CHECK: block3:
; CHECK-NEXT: %result = add i64 %val, 1
; CHECK-NEXT: call void @use
entry:
  %result = add i64 %val, 1
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %block1, label %ok

block1:
  br label %block2

block2:
  br label %block3

block3:
  call void @use(i64 %result)
  call void @throw_error()
  unreachable

ok:
  ret i64 %val
}

; Test: Deep sinking to general path - instruction should sink directly to deepest use block
; CHECK-ITER: NEWSINK ITERATION #1 on test_deep_sink_general
; CHECK-ITER: NEWSINK ITERATION #2 on test_deep_sink_general
; CHECK-ITER-NOT: NEWSINK ITERATION #3 on test_deep_sink_general
define i64 @test_deep_sink_general(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_deep_sink_general
; CHECK: entry:
; CHECK-NOT: %result = add
; CHECK: block3:
; CHECK-NEXT: %result = add i64 %val, 1
; CHECK-NEXT: call void @use
entry:
  %result = add i64 %val, 1
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %block1, label %other

block1:
  br label %block2

block2:
  br label %block3

block3:
  call void @use(i64 %result)
  ret i64 %result

other:
  ret i64 %val
}

; Test: Chain of instructions should all sink together
; CHECK-ITER: NEWSINK ITERATION #1 on test_deep_sink_chain
; CHECK-ITER: NEWSINK ITERATION #2 on test_deep_sink_chain
; CHECK-ITER-NOT: NEWSINK ITERATION #3 on test_deep_sink_chain
define i64 @test_deep_sink_chain(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_deep_sink_chain
; CHECK: entry:
; CHECK-NOT: %a = add
; CHECK-NOT: %b = mul
; CHECK: block2:
; CHECK: %a = add i64 %val, 1
; CHECK: %b = mul i64 %a, 2
; CHECK: call void @use(i64 %b)
entry:
  %a = add i64 %val, 1
  %b = mul i64 %a, 2
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %block1, label %ok

block1:
  br label %block2

block2:
  call void @use(i64 %b)
  call void @throw_error()
  unreachable

ok:
  ret i64 %val
}
