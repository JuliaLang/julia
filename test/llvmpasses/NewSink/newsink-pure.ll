; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test sinking of calls with various memory attributes.

declare void @report_error(i64)
declare void @use_buffer(ptr)

; Readnone call sinks (doesNotAccessMemory — no clobber check needed).
; CHECK-LABEL: @test_sink_pure_call
; CHECK: entry:
; CHECK-NOT: call i64 @pure_compute
; CHECK: error:
; CHECK-NEXT: %result = call i64 @pure_compute
declare i64 @pure_compute(i64, i64) readnone nounwind willreturn
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

; Readonly call sinks (canSinkMemoryRead verifies no intervening clobber).
; CHECK-LABEL: @test_sink_readonly_call
; CHECK: entry:
; CHECK-NOT: call i64 @readonly_compute
; CHECK: error:
; CHECK-NEXT: %result = call i64 @readonly_compute
declare i64 @readonly_compute(ptr) readonly nounwind willreturn
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

; Writing call with return value — not sinkable (mayHaveSideEffects).
; CHECK-LABEL: @test_no_sink_argmem_write_with_retval
; CHECK: entry:
; CHECK-NEXT: %result = call i64 @argmem_write_fn
declare i64 @argmem_write_fn(ptr, i64) nounwind willreturn memory(argmem: write)
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

; Writing call with no return value sinks via the write path.
; CHECK-LABEL: @test_sink_argmem_write_void
; CHECK: entry:
; CHECK-NOT: call void @argmem_write_void_fn
; CHECK: error:
; CHECK-NEXT: call void @argmem_write_void_fn
declare void @argmem_write_void_fn(ptr, i64) nounwind willreturn memory(argmem: write)
define i64 @test_sink_argmem_write_void(i64 %a, i64 %bound) {
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
