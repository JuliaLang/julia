; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test basic store sinking to error path
; The stores to %tuple should be sunk to the error block since they're only used there

define double @test_sink_stores(ptr %array, i64 %i, i64 %j, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_sink_stores
entry:
  %tuple = alloca [2 x i64], align 8
  ; These stores should be sunk to the error block
  ; CHECK-NOT: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8
  %ptr1 = getelementptr inbounds i8, ptr %tuple, i64 8
  ; CHECK-NOT: store i64 %j, ptr %ptr1
  store i64 %j, ptr %ptr1, align 8

  ; Bounds checks
  %cmp1 = icmp ult i64 %i, %bound1
  %cmp2 = icmp ult i64 %j, %bound2
  %inbounds = and i1 %cmp1, %cmp2
  br i1 %inbounds, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %i, ptr %tuple
  ; CHECK: store i64 %j, ptr %ptr1
  call void @throw_error(ptr %tuple)
  unreachable

ok:
  ; Load from array (unrelated to tuple)
  %val = load double, ptr %array, align 8
  ret double %val
}

declare void @throw_error(ptr)

; Test sinking stores to noalias argument (function-local object)
; The noalias attribute means the pointer doesn't alias anything else,
; so we can sink writes to it like we do for allocas.

define i64 @test_sink_noalias_arg(ptr noalias %buf, i64 %val, i64 %bound) {
; CHECK-LABEL: @test_sink_noalias_arg
entry:
  ; This store should be sunk since %buf is noalias (function-local)
  ; CHECK: entry:
  ; CHECK-NEXT: %cmp = icmp ult i64 %val, %bound
  ; CHECK-NEXT: br i1 %cmp
  store i64 %val, ptr %buf, align 8
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

ok:
  ret i64 %val

error:
  ; CHECK: error:
  ; CHECK-NEXT: store i64 %val, ptr %buf
  ; CHECK-NEXT: call void @throw_error
  call void @throw_error(ptr %buf)
  unreachable
}
