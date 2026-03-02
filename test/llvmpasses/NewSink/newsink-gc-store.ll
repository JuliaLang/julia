; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that stores to noalias-allocated objects are not incorrectly sunk
; through critical edge splits to multi-predecessor targets.
;
; Bug pattern: When the MSSA walk evaluates a multi-predecessor target, it
; skips reads "in the target" assuming they'll see the sunk write. But after
; splitting the critical edge, the write lands on only one incoming path,
; so reads arriving from the other predecessor see uninitialized data.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

declare noalias nonnull ptr @alloc(ptr)
declare void @use(ptr)
declare void @use_val(i64)

; CHECK-LABEL: @test_no_sink_to_critical_edge
; The size store must stay in entry, not be sunk to a critical edge split block.
; CHECK: entry:
; CHECK: store i64 %size, ptr %size_ptr
; CHECK: br i1 %is_empty, label %merge, label %fill
; CHECK-NOT: entry.merge_crit_edge:
define void @test_no_sink_to_critical_edge(ptr %ptls, ptr noalias %other_arr, i1 %is_empty, i64 %size, ptr %data_buf) {
entry:
  %obj = call noalias nonnull ptr @alloc(ptr %ptls)
  store ptr %data_buf, ptr %obj, align 8, !tbaa !3
  %size_ptr = getelementptr inbounds i8, ptr %obj, i64 16
  store i64 %size, ptr %size_ptr, align 8, !tbaa !5
  br i1 %is_empty, label %merge, label %fill

fill:
  call void @llvm.memset.p0.i64(ptr %data_buf, i8 0, i64 %size, i1 false), !tbaa !7
  br label %merge

merge:
  %other_size_ptr = getelementptr inbounds i8, ptr %other_arr, i64 16
  %other_size = load i64, ptr %other_size_ptr, align 8, !tbaa !5
  call void @use_val(i64 %other_size)
  call void @use(ptr %obj)
  ret void
}

declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)

!0 = !{!"root"}
!1 = !{!"jtbaa", !0, i64 0}
!2 = !{!"jtbaa_arrayptr", !1, i64 0}
!3 = !{!2, !2, i64 0}
!4 = !{!"jtbaa_arraysize", !1, i64 0}
!5 = !{!4, !4, i64 0}
!6 = !{!"jtbaa_arraybuf", !1, i64 0}
!7 = !{!6, !6, i64 0}
