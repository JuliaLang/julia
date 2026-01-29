; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test sinking with lifetime intrinsics - stores should sink to paths that
; actually read the memory, ignoring paths with lifetime.end

declare void @llvm.lifetime.end.p0(i64, ptr)
declare void @llvm.lifetime.start.p0(i64, ptr)
declare void @use(ptr)

; Store should sink to slow path (fast path has lifetime.end)
; CHECK-LABEL: @test_sink_past_lifetime_end
; CHECK: entry:
; CHECK-NEXT: %buf = alloca i64
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %fast, label %slow
; CHECK: fast:
; CHECK-NEXT: call void @llvm.lifetime.end
; CHECK: slow:
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: %v = load i64, ptr %buf
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

; Store should NOT sink if both paths read the memory
; CHECK-LABEL: @test_no_sink_both_paths_read
; CHECK: entry:
; CHECK-NEXT: %buf = alloca i64
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_no_sink_both_paths_read(i64 %a, i64 %bound) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %path1, label %path2

path1:
  %v1 = load i64, ptr %buf, align 8
  ret i64 %v1

path2:
  %v2 = load i64, ptr %buf, align 8
  ret i64 %v2
}

; Store should sink when one path has no accesses at all
; CHECK-LABEL: @test_sink_one_path_no_access
; CHECK: entry:
; CHECK-NEXT: %buf = alloca i64
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %fast, label %slow
; CHECK: slow:
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: call void @use
define i64 @test_sink_one_path_no_access(i64 %a, i64 %bound) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %fast, label %slow

fast:
  ret i64 %a

slow:
  call void @use(ptr %buf)
  ret i64 0
}

; Both paths need lifetime.end for store to sink
; Without lifetime.end on fast path, we conservatively keep the store
; CHECK-LABEL: @test_sink_needs_lifetime_end_both
; CHECK: entry:
; CHECK:   %buf = alloca i64
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %fast, label %slow
; CHECK: fast:
; CHECK-NEXT: call void @llvm.lifetime.end
; CHECK: slow:
; CHECK-NEXT: store i64 %a, ptr %buf
; CHECK-NEXT: call void @use
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
