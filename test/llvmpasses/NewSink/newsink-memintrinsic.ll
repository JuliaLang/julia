; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test sinking of memory intrinsics (memset, memcpy, memmove) to error paths.

declare void @throw(ptr)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)

; CHECK-LABEL: @test_memset_sink_to_error
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: call void @llvm.memset
; CHECK-NEXT: call void @throw
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

; CHECK-LABEL: @test_memcpy_sink_to_error
; CHECK: entry:
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: call void @throw
define i64 @test_memcpy_sink_to_error(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; CHECK-LABEL: @test_memmove_sink_to_error
; CHECK: entry:
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK-NEXT: call void @llvm.memmove
; CHECK-NEXT: call void @throw
define i64 @test_memmove_sink_to_error(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  call void @llvm.memmove.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; Volatile memset should NOT be sunk
; CHECK-LABEL: @test_volatile_memset_no_sink
; CHECK: entry:
; CHECK-NEXT: %buf = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memset{{.*}}i1 true
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
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

; memset followed by store - both sink together
; CHECK-LABEL: @test_memset_store_chain_sink
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: error:
; CHECK:   call void @llvm.memset
; CHECK:   store i64 %a, ptr %buf
; CHECK:   call void @throw
define i64 @test_memset_store_chain_sink(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 64, i1 false)
  store i64 %a, ptr %buf, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %buf)
  unreachable
}

; Destination read on normal path blocks sinking
; CHECK-LABEL: @test_memcpy_read_on_normal_no_sink
; CHECK: entry:
; CHECK-NEXT: %dest = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_memcpy_read_on_normal_no_sink(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  %v = load i64, ptr %dest, align 8
  ret i64 %v
error:
  call void @throw(ptr %dest)
  unreachable
}

; External pointer writes are observable - don't sink
; CHECK-LABEL: @test_memset_external_no_sink
; CHECK: entry:
; CHECK-NEXT: call void @llvm.memset
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_memset_external_no_sink(i64 %a, i64 %bound, ptr %external) {
entry:
  call void @llvm.memset.p0.i64(ptr %external, i8 42, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %external)
  unreachable
}

; General sinking to single-pred successor (non-error path)
; CHECK-LABEL: @test_memset_general_sink
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %fast_path, label %slow_path
; CHECK: slow_path:
; CHECK-NEXT: call void @llvm.memset
; CHECK-NEXT: call void @use_buffer
define i64 @test_memset_general_sink(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %fast_path, label %slow_path
fast_path:
  ret i64 %a
slow_path:
  call void @use_buffer(ptr %buf)
  ret i64 0
}

; memcpy and load both sink to their respective paths
; CHECK-LABEL: @test_memcpy_and_load_both_sink
; CHECK: entry:
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
; CHECK-NEXT: br i1 %cmp, label %ok, label %error
; CHECK: ok:
; CHECK-NEXT: %early = load i8, ptr %src
; CHECK: error:
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: call void @throw
define i64 @test_memcpy_and_load_both_sink(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  %early = load i8, ptr %src, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  %ext = zext i8 %early to i64
  ret i64 %ext
error:
  call void @throw(ptr %dest)
  unreachable
}

declare void @use_buffer(ptr)
declare ptr @strcpy(ptr, ptr) nounwind willreturn

; strcpy with return value used - not sinkable
; CHECK-LABEL: @test_strcpy_with_uses_no_sink
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %p = call ptr @strcpy
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
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

; Regression: do not sink source-reading call past source mutation.
; CHECK-LABEL: @test_strcpy_unused_ret_source_mutated_no_sink
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %src.local = alloca [8 x i8]
; CHECK-NEXT: call ptr @strcpy
; CHECK-NEXT: %src.byte = getelementptr [8 x i8], ptr %src.local, i64 0, i64 0
; CHECK-NEXT: store volatile i8 0, ptr %src.byte
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_strcpy_unused_ret_source_mutated_no_sink(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  %src.local = alloca [8 x i8], align 1
  call ptr @strcpy(ptr %buf, ptr %src.local)
  %src.byte = getelementptr [8 x i8], ptr %src.local, i64 0, i64 0
  store volatile i8 0, ptr %src.byte, align 1
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %buf)
  unreachable
}

; Same regression with non-throwing attributes.
; CHECK-LABEL: @test_strcpy_unused_ret_source_mutated_no_sink_attr
; CHECK: entry:
; CHECK:   %buf = alloca [64 x i8]
; CHECK-NEXT: %src.local = alloca [8 x i8]
; CHECK-NEXT: call ptr @strcpy
; CHECK-NEXT: %src.byte = getelementptr [8 x i8], ptr %src.local, i64 0, i64 0
; CHECK-NEXT: store volatile i8 0, ptr %src.byte
; CHECK-NEXT: %cmp = icmp ult i64 %a, %bound
define i64 @test_strcpy_unused_ret_source_mutated_no_sink_attr(i64 %a, i64 %bound) {
entry:
  %buf = alloca [64 x i8], align 8
  %src.local = alloca [8 x i8], align 1
  call ptr @strcpy(ptr %buf, ptr %src.local)
  %src.byte = getelementptr [8 x i8], ptr %src.local, i64 0, i64 0
  store volatile i8 0, ptr %src.byte, align 1
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %buf)
  unreachable
}

declare void @llvm.lifetime.end.p0(i64, ptr)

; Regression test: memcpy past source lifetime.end causes use-after-free
; CHECK-LABEL: @test_memcpy_source_lifetime_end_no_sink
; CHECK: entry:
; CHECK:   %src = alloca [64 x i8]
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: call void @llvm.lifetime.end
; CHECK-NEXT: %cmp = icmp
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

; memcpy past destination lifetime.end is UB
; CHECK-LABEL: @test_memcpy_dest_lifetime_end_no_sink
; CHECK: entry:
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: call void @llvm.lifetime.end{{.*}}%dest
; CHECK-NEXT: %cmp = icmp
define i64 @test_memcpy_dest_lifetime_end_no_sink(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  call void @llvm.lifetime.end.p0(i64 64, ptr %dest)
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}

; The store to %src is dead (ambiguous target) so it stays. That blocks
; the memcpy too (store modifies %src between the memcpy and terminator).
; CHECK-LABEL: @test_memcpy_dead_store_blocks_both
; CHECK: entry:
; CHECK:   call void @llvm.memcpy
; CHECK-NEXT: store i8 99, ptr %src
; CHECK-NEXT: %cmp = icmp
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

; Volatile store can't sink, so memcpy must stay to preserve order
; CHECK-LABEL: @test_memcpy_source_volatile_store_no_sink
; CHECK: entry:
; CHECK:   %src = alloca [64 x i8]
; CHECK:   %dest = alloca [64 x i8]
; CHECK-NEXT: call void @llvm.memcpy
; CHECK-NEXT: store volatile i8 99, ptr %src
; CHECK-NEXT: %cmp = icmp
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

; Instructions sink to different paths based on their uses
; CHECK-LABEL: @test_memcpy_store_load_all_sink
; CHECK: entry:
; CHECK:   %src = alloca [64 x i8]
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

; Memcpy sinks to error (read by @throw), but the dead store to %unrelated
; stays (ambiguous — valid for both successors, neither reads it).
; CHECK-LABEL: @test_memcpy_unrelated_store_both_sink
; CHECK: entry:
; CHECK:   %unrelated = alloca i64
; CHECK-NEXT: store i64 42, ptr %unrelated
; CHECK-NEXT: %cmp = icmp
; CHECK: error:
; CHECK-NEXT: call void @llvm.memcpy
define i64 @test_memcpy_unrelated_store_both_sink(i64 %a, i64 %bound, ptr %src) {
entry:
  %dest = alloca [64 x i8], align 8
  %unrelated = alloca i64, align 8
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src, i64 64, i1 false)
  store i64 42, ptr %unrelated, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error
ok:
  ret i64 %a
error:
  call void @throw(ptr %dest)
  unreachable
}
