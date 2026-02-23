; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that pure computations only used in error path are sunk

define i64 @test_sink_pure(i64 %a, i64 %b, i64 %bound) {
; CHECK-LABEL: @test_sink_pure
entry:
  ; These computations are only used in the error path
  ; CHECK-NOT: %sum = add i64 %a, %b
  %sum = add i64 %a, %b
  ; CHECK-NOT: %prod = mul i64 %sum, 42
  %prod = mul i64 %sum, 42

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: %sum = add i64 %a, %b
  ; CHECK: %prod = mul i64 %sum, 42
  call void @report_error(i64 %prod)
  unreachable

ok:
  ; Return something unrelated
  ret i64 %b
}

declare void @report_error(i64)

; Pure function call (readnone) should be sunk to error path
; CHECK-LABEL: @test_sink_pure_call
; CHECK: entry:
; CHECK-NOT: %result = call i64 @pure_compute
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: %result = call i64 @pure_compute
; CHECK-NEXT: call void @report_error
define i64 @test_sink_pure_call(i64 %a, i64 %b, i64 %bound) {
entry:
  %result = call i64 @pure_compute(i64 %a, i64 %b)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @report_error(i64 %result)
  unreachable

ok:
  ret i64 %b
}

declare i64 @pure_compute(i64, i64) readnone nounwind willreturn

; Readonly function call CAN be sunk (reads memory but no side effects)
; The key is mayHaveSideEffects() returns false for readonly calls.
; CHECK-LABEL: @test_sink_readonly_call
; CHECK: entry:
; CHECK-NOT: %result = call i64 @readonly_compute
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: %result = call i64 @readonly_compute
; CHECK-NEXT: call void @report_error
define i64 @test_sink_readonly_call(i64 %a, ptr %p, i64 %bound) {
entry:
  %result = call i64 @readonly_compute(ptr %p)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @report_error(i64 %result)
  unreachable

ok:
  ret i64 %a
}

declare i64 @readonly_compute(ptr) readonly nounwind willreturn

; Function that only reads from its arguments (argmem: read)
; Should be sinkable since it has no global side effects
; CHECK-LABEL: @test_sink_argmem_read
; CHECK: entry:
; CHECK-NOT: %result = call i64 @argmem_read_fn
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: %result = call i64 @argmem_read_fn
; CHECK-NEXT: call void @report_error
define i64 @test_sink_argmem_read(i64 %a, ptr %p, i64 %bound) {
entry:
  %result = call i64 @argmem_read_fn(ptr %p)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @report_error(i64 %result)
  unreachable

ok:
  ret i64 %a
}

declare i64 @argmem_read_fn(ptr) nounwind willreturn memory(argmem: read)

; Function that writes to its arguments only - should NOT be sunk
; via value path (it has a return value used), but the write effect
; means mayHaveSideEffects() returns true
; CHECK-LABEL: @test_no_sink_argmem_write_with_retval
; CHECK: entry:
; CHECK-NEXT: %result = call i64 @argmem_write_fn
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_no_sink_argmem_write_with_retval(i64 %a, ptr %p, i64 %bound) {
entry:
  %result = call i64 @argmem_write_fn(ptr %p, i64 %a)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @report_error(i64 %result)
  unreachable

ok:
  ret i64 %a
}

declare i64 @argmem_write_fn(ptr, i64) nounwind willreturn memory(argmem: write)

; Function that writes to its arguments with no return value
; NOT sunk because we can't identify a specific destination location.
; MemoryLocation::getForDest only works for known intrinsics (memset, memcpy)
; and TLI-recognized library functions (strcpy), not arbitrary functions.
; CHECK-LABEL: @test_no_sink_argmem_write_void
; CHECK: entry:
; CHECK:   %buf = alloca i64
; CHECK-NEXT: call void @argmem_write_void_fn
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_no_sink_argmem_write_void(i64 %a, i64 %bound) {
entry:
  %buf = alloca i64, align 8
  call void @argmem_write_void_fn(ptr %buf, i64 %a)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use_buffer(ptr %buf)
  unreachable

ok:
  ret i64 %a
}

declare void @argmem_write_void_fn(ptr, i64) nounwind willreturn memory(argmem: write)
declare void @use_buffer(ptr)
