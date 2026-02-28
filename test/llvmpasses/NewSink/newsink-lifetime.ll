; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test sinking with lifetime intrinsics - stores should sink to paths that
; actually read the memory, ignoring paths with lifetime.end

declare void @llvm.lifetime.end.p0(i64, ptr)
declare void @use(ptr)

; Store doesn't sink: the MSSA walk finds readers on both paths
; (load in slow, lifetime.end in fast), so neither is a unique target.
; CHECK-LABEL: @test_sink_past_lifetime_end
; CHECK: entry:
; CHECK-NEXT: %buf = alloca i64
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_sink_past_lifetime_end(i64 %a, i64 %bound) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %fast, label %slow

fast:
  call void @llvm.lifetime.end.p0(i64 8, ptr %buf)
  ret i64 %a

slow:
  %v = load i64, ptr %buf, align 8
  ret i64 %v
}

; Both paths access the alloca, so no unique target.
; CHECK-LABEL: @test_sink_needs_lifetime_end_both
; CHECK: entry:
; CHECK:   %buf = alloca i64
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_sink_needs_lifetime_end_both(i64 %a, i64 %bound) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %fast, label %slow

fast:
  call void @llvm.lifetime.end.p0(i64 8, ptr %buf)
  ret i64 %a

slow:
  call void @use(ptr %buf)
  ret i64 0
}
