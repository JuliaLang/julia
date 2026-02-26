; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that stores to escaping noalias allocations are not sunk.
;
; When a noalias-allocated object escapes the function (via return, store to
; global, or call), sinking a field store to one branch makes the field
; uninitialized on the other branch. Inter-procedural reads through the
; escaped pointer would see the wrong value, but the intra-function MSSA
; walk cannot detect those reads.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare noalias nonnull ptr @alloc(ptr)
declare void @use_val(i64)

; The object escapes via return — stores must not be sunk because the
; caller will read all fields of the returned object.
; CHECK-LABEL: @test_escape_via_return
; CHECK: entry:
; CHECK: store i64 %size, ptr %size_ptr
; CHECK: br i1 %is_empty
define ptr @test_escape_via_return(ptr %ptls, i1 %is_empty, i64 %size, ptr %data_buf) {
entry:
  %obj = call noalias nonnull ptr @alloc(ptr %ptls)
  store ptr %data_buf, ptr %obj, align 8
  %size_ptr = getelementptr inbounds i8, ptr %obj, i64 16
  store i64 %size, ptr %size_ptr, align 8
  br i1 %is_empty, label %done, label %fill

fill:
  call void @llvm.memset.p0.i64(ptr %data_buf, i8 0, i64 %size, i1 false)
  br label %done

done:
  ret ptr %obj
}

; The object escapes via store to a global — same issue.
; CHECK-LABEL: @test_escape_via_store
; CHECK: entry:
; CHECK: store i64 %size, ptr %size_ptr
; CHECK: br i1 %is_empty
@global_slot = external global ptr
define void @test_escape_via_store(ptr %ptls, i1 %is_empty, i64 %size, ptr %data_buf) {
entry:
  %obj = call noalias nonnull ptr @alloc(ptr %ptls)
  store ptr %data_buf, ptr %obj, align 8
  %size_ptr = getelementptr inbounds i8, ptr %obj, i64 16
  store i64 %size, ptr %size_ptr, align 8
  br i1 %is_empty, label %done, label %fill

fill:
  call void @llvm.memset.p0.i64(ptr %data_buf, i8 0, i64 %size, i1 false)
  br label %done

done:
  store ptr %obj, ptr @global_slot, align 8
  ret void
}

; The object escapes via call — same issue.
; CHECK-LABEL: @test_escape_via_call
; CHECK: entry:
; CHECK: store i64 %size, ptr %size_ptr
; CHECK: br i1 %is_empty
declare void @consume(ptr)
define void @test_escape_via_call(ptr %ptls, i1 %is_empty, i64 %size, ptr %data_buf) {
entry:
  %obj = call noalias nonnull ptr @alloc(ptr %ptls)
  store ptr %data_buf, ptr %obj, align 8
  %size_ptr = getelementptr inbounds i8, ptr %obj, i64 16
  store i64 %size, ptr %size_ptr, align 8
  br i1 %is_empty, label %done, label %fill

fill:
  call void @llvm.memset.p0.i64(ptr %data_buf, i8 0, i64 %size, i1 false)
  br label %done

done:
  call void @consume(ptr %obj)
  ret void
}

; Negative test: object does NOT escape — store CAN be sunk to the
; noreturn error path where the tuple is consumed.
; CHECK-LABEL: @test_no_escape
; CHECK: entry:
; CHECK-NOT: store i64 %size
; CHECK: br i1 %cmp
declare void @throw_error(ptr) noreturn
define void @test_no_escape(ptr %ptls, i64 %size, i64 %bound) {
entry:
  %obj = call noalias nonnull ptr @alloc(ptr %ptls)
  %size_ptr = getelementptr inbounds i8, ptr %obj, i64 16
  store i64 %size, ptr %size_ptr, align 8
  %cmp = icmp ult i64 %size, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @throw_error(ptr %obj)
  unreachable

ok:
  ret void
}

declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
