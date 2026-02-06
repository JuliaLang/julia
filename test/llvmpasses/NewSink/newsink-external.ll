; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that stores to external/global pointers are NOT sunk
; because they have observable side effects

@global = external global i64

define i64 @test_no_sink_global_store(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_no_sink_global_store
entry:
  ; Store to global should NOT be sunk - it's observable
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr @global
  store i64 %a, ptr @global, align 8

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr @global, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

define i64 @test_no_sink_external_ptr_store(ptr %p, i64 %a, i64 %bound) {
; CHECK-LABEL: @test_no_sink_external_ptr_store
entry:
  ; Store to external pointer should NOT be sunk - could be observed
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %p
  store i64 %a, ptr %p, align 8

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr %p, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

; But stores to local allocas that are only read in error paths SHOULD be sunk

define i64 @test_sink_local_alloca(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_sink_local_alloca
; CHECK: entry:
; CHECK-NEXT: %p = alloca
; CHECK-NEXT: %cmp = icmp
; CHECK-NEXT: br i1 %cmp
entry:
  %p = alloca i64, align 8
  store i64 %a, ptr %p, align 8

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %a, ptr %p
  %v = load i64, ptr %p, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

declare void @use(i64)

; Test: Store should sink when pointer is captured in a dominated block
; The capture in 'cleanup' is fine because it's dominated by 'error' (where store will go)
define i64 @test_sink_capture_in_dominated(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_sink_capture_in_dominated
; CHECK: entry:
; CHECK-NEXT: %p = alloca
; CHECK-NEXT: %cmp = icmp
; CHECK-NEXT: br i1 %cmp
entry:
  %p = alloca i64, align 8
  store i64 %a, ptr %p, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %a, ptr %p
  ; CHECK: call void @throw
  call void @throw(ptr %p)
  br label %cleanup

cleanup:
  ; Capture happens here, but it's dominated by 'error' where store lands
  call void @escape(ptr %p)
  unreachable

ok:
  ret i64 %a
}

; Test: Store should NOT sink when pointer is captured in a non-dominated block
define i64 @test_no_sink_capture_not_dominated(i64 %a, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_no_sink_capture_not_dominated
; CHECK: entry:
; CHECK: store i64 %a, ptr %p
entry:
  %p = alloca i64, align 8
  store i64 %a, ptr %p, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error_dispatch

error_dispatch:
  br i1 %cond, label %error1, label %error2

error1:
  ; We'd want to sink to error1
  call void @throw(ptr %p)
  unreachable

error2:
  ; But capture here is not dominated by error1 - blocks sinking
  call void @escape(ptr %p)
  unreachable

ok:
  ret i64 %a
}

declare void @escape(ptr)
declare void @throw(ptr nocapture readonly)
