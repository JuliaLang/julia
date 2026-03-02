; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test sinking of memory intrinsics (memset, memcpy, memmove).

declare void @throw(ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.lifetime.end.p0(i64, ptr)
declare ptr @strcpy(ptr, ptr) nounwind willreturn

; Basic memset sinks to error path.
; CHECK-LABEL: @test_memset_sink_to_error
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp
; CHECK: error:
; CHECK-NEXT: call void @llvm.memset
define i64 @test_memset_sink_to_error(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %buf)
  unreachable
}

; Volatile memset should NOT be sunk.
; CHECK-LABEL: @test_volatile_memset_no_sink
; CHECK: entry:
; CHECK-NEXT: %buf = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memset{{.*}}i1 true
define i64 @test_volatile_memset_no_sink(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 64, i1 true)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %buf)
  unreachable
}

; strcpy with return value used — not sinkable (use_empty check).
; CHECK-LABEL: @test_strcpy_with_uses_no_sink
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %p = call ptr @strcpy
define i64 @test_strcpy_with_uses_no_sink(i64 %a, i64 %bound, ptr %src) {
entry:
  %buf = alloca [64 x i8], align 8
  %p = call ptr @strcpy(ptr %buf, ptr %src)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %p)
  unreachable
}

; canSinkMemTransfer: source modified by volatile store blocks memcpy.
; CHECK-LABEL: @test_memcpy_source_volatile_store_no_sink
; CHECK: entry:
; CHECK:   call void @llvm.memcpy
; CHECK-NEXT: store volatile i8 99, ptr %src
define i64 @test_memcpy_source_volatile_store_no_sink(i64 %a, i64 %bound) {
entry:
  %src = alloca [64 x i8], align 8
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  store volatile i8 99, ptr %src, align 1
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; canSinkMemTransfer: source lifetime.end blocks memcpy.
; CHECK-LABEL: @test_memcpy_source_lifetime_end_no_sink
; CHECK: entry:
; CHECK:   call void @llvm.memcpy
; CHECK-NEXT: call void @llvm.lifetime.end
define i64 @test_memcpy_source_lifetime_end_no_sink(i64 %a, i64 %bound) {
entry:
  %src = alloca [64 x i8], align 8
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  call void @llvm.lifetime.end.p0(i64 64, ptr %src)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; Dead store blocks memcpy (store modifies source between memcpy and terminator).
; CHECK-LABEL: @test_memcpy_dead_store_blocks_both
; CHECK: entry:
; CHECK:   call void @llvm.memcpy
; CHECK-NEXT: store i8 99, ptr %src
define i64 @test_memcpy_dead_store_blocks_both(i64 %a, i64 %bound) {
entry:
  %src = alloca [64 x i8], align 8
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  store i8 99, ptr %src, align 1
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; Memcpy and unrelated store/load sink to different paths.
; CHECK-LABEL: @test_memcpy_store_load_all_sink
; CHECK: entry:
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp
; CHECK: ok:
; CHECK-NEXT: store i8 99, ptr %src
; CHECK-NEXT: %v = load i8, ptr %src
; CHECK: error:
; CHECK-NEXT: call void @llvm.memcpy
define i64 @test_memcpy_store_load_all_sink(i64 %a, i64 %bound) {
entry:
  %src = alloca [64 x i8], align 8
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  store i8 99, ptr %src, align 1
  %v = load i8, ptr %src, align 1
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  %ext = zext i8 %v to i64
  ret i64 %ext
error:
  call void @throw(ptr %dest)
  unreachable
}
