; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; ============================================================================
; Sinking TO error blocks inside loops
; ============================================================================

; Store to loop-invariant alloca sinks to error exit.
define i64 @test_loop_with_error_block(ptr %array, i64 %n) {
; CHECK-LABEL: @test_loop_with_error_block
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  ; CHECK: loop:
  ; CHECK-NOT: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.latch, label %error

loop.latch:
  %ptr = getelementptr i64, ptr %array, i64 %i
  %val = load i64, ptr %ptr, align 8
  %i.next = add i64 %i, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

error:
  ; CHECK: error:
  ; CHECK: store i64 %i, ptr %tuple
  call void @throw_bounds_error(ptr %tuple)
  unreachable

exit:
  ret i64 0
}

; Self-loop: store sinks to error exit.
define void @test_infinite_loop_with_error(ptr %array, i64 %n) {
; CHECK-LABEL: @test_infinite_loop_with_error
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  ; CHECK: loop:
  ; CHECK-NOT: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  %i.next = add i64 %i, 1
  br i1 %cmp, label %loop, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %i, ptr %tuple
  call void @throw_bounds_error(ptr %tuple)
  unreachable
}

; Store must NOT sink: the latch reads the stored value.
define i64 @test_loop_value_used_in_latch(ptr %array, i64 %n) {
; CHECK-LABEL: @test_loop_value_used_in_latch
entry:
  %tuple = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  ; CHECK: loop:
  ; CHECK: store i64 %i, ptr %tuple
  store i64 %i, ptr %tuple, align 8

  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.latch, label %error

loop.latch:
  %loaded = load i64, ptr %tuple, align 8
  %i.next = add i64 %loaded, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

error:
  call void @throw_bounds_error(ptr %tuple)
  unreachable

exit:
  ret i64 0
}

; ============================================================================
; Cross-loop sinking: loop-invariant vs loop-variant destinations
; ============================================================================

; Loop-invariant destination: CAN sink out of loop (all iterations write
; to the same address, only the last write matters).
; CHECK-LABEL: @test_sink_loop_invariant_dest_to_exit
; CHECK: loop:
; CHECK-NOT: store i64 %i, ptr %buf
; CHECK: br i1
; CHECK: exit:
; CHECK: store i64 %i, ptr %buf
; CHECK: call void @use_value
define void @test_sink_loop_invariant_dest_to_exit(i64 %n) {
entry:
  %buf = alloca i64, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %next, %latch ]
  store i64 %i, ptr %buf, align 8
  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %latch, label %exit

latch:
  %next = add nuw nsw i64 %i, 1
  br label %loop

exit:
  call void @use_value(ptr %buf)
  ret void
}

; Loop-variant destination: must NOT sink (earlier iterations' writes lost).
; CHECK-LABEL: @test_no_sink_loop_variant_dest
; CHECK: loop:
; CHECK: store i64 42, ptr %dest
; CHECK: br i1
define void @test_no_sink_loop_variant_dest(i64 %n) {
entry:
  %buf = alloca [8 x i64], align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %next, %latch ]
  %dest = getelementptr inbounds i64, ptr %buf, i64 %i
  store i64 42, ptr %dest, align 8
  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %latch, label %exit

latch:
  %next = add nuw nsw i64 %i, 1
  br label %loop

exit:
  call void @use_value(ptr %buf)
  ret void
}

; Loop-variant memcpy destination: must NOT sink.
define void @test_no_sink_memcpy_out_of_loop() {
; CHECK-LABEL: @test_no_sink_memcpy_out_of_loop
entry:
  %buf = alloca [32 x i8], align 16
  %tmp = alloca [16 x i8], align 16
  call void @llvm.memset.p0.i64(ptr %buf, i8 0, i64 32, i1 false)
  br label %loop

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

; Loop-invariant destination with a load on the backedge.
; The load stays in latch — sinking to loop would be a backedge.
; CHECK-LABEL: @test_no_sink_loop_invariant_with_backedge_reader
; CHECK: loop:
; CHECK: store i64 %i, ptr %buf
; CHECK: latch:
; CHECK: %val = load i64, ptr %buf
define i64 @test_no_sink_loop_invariant_with_backedge_reader(i64 %n) {
entry:
  %buf = alloca i64, align 8
  store i64 0, ptr %buf, align 8
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %next, %latch ]
  store i64 %i, ptr %buf, align 8
  %val = load i64, ptr %buf, align 8
  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %latch, label %exit

latch:
  %next = add nuw nsw i64 %val, 1
  br label %loop

exit:
  %result = load i64, ptr %buf, align 8
  ret i64 %result
}

; ============================================================================
; Must NOT sink INTO loops
; ============================================================================

; Store before loop, read in loop body — must not sink.
define i64 @test_no_sink_store_into_loop(i64 %a, i64 %n) {
; CHECK-LABEL: @test_no_sink_store_into_loop
entry:
  %p = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %p
  store i64 %a, ptr %p, align 8
  br label %loop.header

loop.header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.latch ]
  %cmp = icmp ult i64 %i, %n
  br i1 %cmp, label %loop.body, label %exit

loop.body:
  %v = load i64, ptr %p, align 8
  %result = add i64 %v, %i
  br label %loop.latch

loop.latch:
  %i.next = add i64 %i, 1
  br label %loop.header

exit:
  %final = load i64, ptr %p, align 8
  ret i64 %final
}

; ============================================================================
; Post-dominance and backedge safety
; ============================================================================

; Stores must not sink when target doesn't post-dominate source in a loop.
; Sinking to use_state_path would skip stores on iterations taking skip_state_path.

; CHECK-LABEL: @test_loop_backedge_sink
; CHECK: loop_header:
; CHECK-NEXT: phi i64
; CHECK-NEXT: phi i64
; CHECK: store i64 %iter_val
; CHECK: store i64 %state_val
define void @test_loop_backedge_sink() {
entry:
  %state = alloca [7 x i64], align 8
  %flag = call i1 @get_flag()
  br i1 %flag, label %setup_path, label %direct_path

setup_path:
  %v1 = call i64 @get_value()
  %v2 = call i64 @get_value()
  br label %loop_header

direct_path:
  br label %loop_header

loop_header:
  %iter_val = phi i64 [ %v1, %setup_path ], [ 0, %direct_path ], [ %next_val, %loop_latch ]
  %state_val = phi i64 [ %v2, %setup_path ], [ 0, %direct_path ], [ %new_state, %loop_latch ]
  store i64 %iter_val, ptr %state, align 8
  %ptr2 = getelementptr i8, ptr %state, i64 8
  store i64 %state_val, ptr %ptr2, align 8
  br i1 %flag, label %use_state_path, label %skip_state_path

use_state_path:
  %as11 = addrspacecast ptr %state to ptr addrspace(11)
  call void @use_state(ptr addrspace(11) %as11)
  br label %loop_latch

skip_state_path:
  br label %loop_latch

loop_latch:
  %next_val = add i64 %iter_val, 1
  %new_state = add i64 %state_val, 1
  %done = icmp eq i64 %next_val, 10
  br i1 %done, label %exit, label %loop_header

exit:
  ret void
}

; Stores in loop header must not sink to a successor (the successor
; doesn't post-dominate the header due to the backedge).
; CHECK-LABEL: @test_loop_header_store
define void @test_loop_header_store() {
entry:
  %state = alloca [2 x i64], align 8
  br label %loop_header

; CHECK-LABEL: loop_header:
; CHECK: phi i64
; CHECK: store i64 %i, ptr %state
; CHECK: store i64 42, ptr %ptr2
loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  store i64 %i, ptr %state, align 8
  %ptr2 = getelementptr i8, ptr %state, i64 8
  store i64 42, ptr %ptr2, align 8
  %cond = call i1 @get_cond()
  br i1 %cond, label %loop_latch, label %exit_path

loop_latch:
  %next = add i64 %i, 1
  br label %loop_header

exit_path:
  call void @use_state_p(ptr %state)
  ret void
}

; Same with memcpy in loop header.
; CHECK-LABEL: @test_loop_header_memcpy
define void @test_loop_header_memcpy(ptr %src) {
entry:
  %state = alloca [16 x i8], align 8
  br label %loop_header

; CHECK-LABEL: loop_header:
; CHECK: phi i64
; CHECK: call void @llvm.memcpy
loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  call void @llvm.memcpy.p0.p0.i64(ptr %state, ptr %src, i64 16, i1 false)
  %cond = call i1 @get_cond()
  br i1 %cond, label %loop_latch, label %exit_path

loop_latch:
  %next = add i64 %i, 1
  br label %loop_header

exit_path:
  call void @use_state_p(ptr %state)
  ret void
}

declare void @throw_bounds_error(ptr)
declare void @use_value(ptr)
declare void @use_buffer(ptr)
declare void @compute_result(ptr noalias sret([16 x i8]), i64)
declare void @use_state(ptr addrspace(11) nocapture readonly)
declare void @use_state_p(ptr)
declare i1 @get_flag()
declare i1 @get_cond()
declare i64 @get_value()
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
