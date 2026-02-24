; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Bug: The pass sinks stores from a loop body to the loop exit block.
; The loop exit post-dominates the loop body, but the stores execute on
; each iteration with different destination offsets. Sinking them to the
; exit means they only execute once (with the final iteration's offset),
; losing all but the last iteration's data.
;
; This models the StaticArrays._solve pattern where a 2-iteration loop
; computes Cramer's rule per column, writing results via memcpy from a
; reused sret buffer into an output array at iteration-dependent offsets.
; The pass sinks the memcpys to the exit, so only the last column is written.

declare void @compute_result(ptr noalias sret([16 x i8]), i64)
declare void @use_buffer(ptr)

; Test: memcpy in loop body must not be sunk to loop exit
;
; The loop writes to buf[i*16] each iteration via memcpy from a reused
; tmp buffer. Sinking to exit would execute memcpy once with i=1, leaving
; buf[0:16] as zeros.
define void @test_no_sink_memcpy_out_of_loop() {
; CHECK-LABEL: @test_no_sink_memcpy_out_of_loop
entry:
  %buf = alloca [32 x i8], align 16
  %tmp = alloca [16 x i8], align 16
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 32, i1 false)
  br label %loop

; The memcpy MUST stay in the loop body
; CHECK: loop:
; CHECK: call void @compute_result
; CHECK: call void @llvm.memcpy
; CHECK: br i1
loop:
  %i = phi i64 [ 0, %entry ], [ %next, %loop ]
  call void @compute_result(ptr sret([16 x i8]) %tmp, i64 %i)
  %offset = shl i64 %i, 4
  %dest = getelementptr inbounds i8, ptr %buf, i64 %offset
  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %tmp, i64 16, i1 false)
  %next = add nuw nsw i64 %i, 1
  %done = icmp eq i64 %next, 2
  br i1 %done, label %exit, label %loop

; CHECK: exit:
; CHECK-NOT: call void @llvm.memcpy
; CHECK: call void @use_buffer
exit:
  call void @use_buffer(ptr %buf)
  ret void
}

; Test: store in loop body must not be sunk to loop exit
;
; Same pattern but with plain stores instead of memcpy.
; Each iteration stores the loop index to a different offset.
define void @test_no_sink_store_out_of_loop() {
; CHECK-LABEL: @test_no_sink_store_out_of_loop
entry:
  %buf = alloca [2 x i64], align 8
  br label %loop

; Stores must stay in the loop body
; CHECK: loop:
; CHECK: store i64 %i
; CHECK: br i1
loop:
  %i = phi i64 [ 0, %entry ], [ %next, %loop ]
  %dest = getelementptr inbounds i64, ptr %buf, i64 %i
  store i64 %i, ptr %dest, align 8
  %next = add nuw nsw i64 %i, 1
  %done = icmp eq i64 %next, 2
  br i1 %done, label %exit, label %loop

; CHECK: exit:
; CHECK-NOT: store i64
; CHECK: call void @use_buffer
exit:
  call void @use_buffer(ptr %buf)
  ret void
}

declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
