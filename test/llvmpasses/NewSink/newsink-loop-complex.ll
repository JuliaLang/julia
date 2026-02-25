; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Complex loop tests: multiple exits, nested loops, post-dominance cases
; Note: We avoid calls between stores and branches because unknown calls
; are conservatively assumed to read memory, which blocks sinking.


declare void @use(ptr)
declare void @throw(ptr)

; =============================================================================
; Multiple loop exits
; =============================================================================

; Test: Loop with two normal exits - store used on one exit path
; Store should NOT be sunk because we're in a loop and target doesn't post-dominate
; CHECK-LABEL: @test_loop_multiple_exits
; CHECK: loop_header:
; CHECK: store i64 %i, ptr %buf
define void @test_loop_multiple_exits(i64 %n, i1 %c1, i1 %c2) {
entry:
  %buf = alloca i64, align 8
  br label %loop_header

loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  store i64 %i, ptr %buf, align 8
  br i1 %c1, label %exit_a, label %check_b

check_b:
  br i1 %c2, label %exit_b, label %loop_latch

loop_latch:
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit_c, label %loop_header

exit_a:
  call void @use(ptr %buf)
  ret void

exit_b:
  ret void

exit_c:
  ret void
}

; Test: Loop with error exit (unreachable) - CAN sink to error path
; CHECK-LABEL: @test_loop_error_exit
; CHECK: loop_header:
; CHECK-NOT: store i64 %i, ptr %buf
; CHECK: error_exit:
; CHECK: store i64 %i, ptr %buf
define void @test_loop_error_exit(i64 %n, i1 %c1) {
entry:
  %buf = alloca i64, align 8
  br label %loop_header

loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  store i64 %i, ptr %buf, align 8
  br i1 %c1, label %error_exit, label %loop_latch

loop_latch:
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %normal_exit, label %loop_header

error_exit:
  call void @throw(ptr %buf)
  unreachable

normal_exit:
  ret void
}

; =============================================================================
; Nested loops - various sinking scenarios
; =============================================================================

; Test: Store in outer loop header, read in inner loop
; Should NOT sink - inner loop iterations need the store
; CHECK-LABEL: @test_nested_outer_to_inner
; CHECK: outer_header:
; CHECK: store i64 %i, ptr %buf
define void @test_nested_outer_to_inner(i64 %n, i64 %m, i1 %c) {
entry:
  %buf = alloca i64, align 8
  br label %outer_header

outer_header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %outer_latch ]
  store i64 %i, ptr %buf, align 8
  br label %inner_header

inner_header:
  %j = phi i64 [ 0, %outer_header ], [ %j.next, %inner_latch ]
  %v = load i64, ptr %buf, align 8
  br i1 %c, label %inner_latch, label %outer_latch

inner_latch:
  %j.next = add i64 %j, 1
  %inner_done = icmp eq i64 %j.next, %m
  br i1 %inner_done, label %outer_latch, label %inner_header

outer_latch:
  %i.next = add i64 %i, 1
  %outer_done = icmp eq i64 %i.next, %n
  br i1 %outer_done, label %exit, label %outer_header

exit:
  ret void
}

; Test: Store in inner loop header, read only on one path
; Should NOT sink - other inner iterations need the store
; CHECK-LABEL: @test_nested_inner_header_store
; CHECK: inner_header:
; CHECK: store i64 %j, ptr %buf
define void @test_nested_inner_header_store(i64 %n, i64 %m, i1 %c) {
entry:
  %buf = alloca i64, align 8
  br label %outer_header

outer_header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %outer_latch ]
  br label %inner_header

inner_header:
  %j = phi i64 [ 0, %outer_header ], [ %j.next, %inner_latch ]
  store i64 %j, ptr %buf, align 8
  br i1 %c, label %use_path, label %skip_path

use_path:
  call void @use(ptr %buf)
  br label %inner_latch

skip_path:
  br label %inner_latch

inner_latch:
  %j.next = add i64 %j, 1
  %inner_done = icmp eq i64 %j.next, %m
  br i1 %inner_done, label %outer_latch, label %inner_header

outer_latch:
  %i.next = add i64 %i, 1
  %outer_done = icmp eq i64 %i.next, %n
  br i1 %outer_done, label %exit, label %outer_header

exit:
  ret void
}

; Test: Store in inner loop, sink to inner error exit (unreachable)
; CAN sink because error path is noreturn
; CHECK-LABEL: @test_nested_inner_error_exit
; CHECK: inner_header:
; CHECK-NOT: store i64 %j, ptr %buf
; CHECK: inner_error:
; CHECK: store i64 %j, ptr %buf
define void @test_nested_inner_error_exit(i64 %n, i64 %m, i1 %c) {
entry:
  %buf = alloca i64, align 8
  br label %outer_header

outer_header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %outer_latch ]
  br label %inner_header

inner_header:
  %j = phi i64 [ 0, %outer_header ], [ %j.next, %inner_latch ]
  store i64 %j, ptr %buf, align 8
  br i1 %c, label %inner_error, label %inner_latch

inner_latch:
  %j.next = add i64 %j, 1
  %inner_done = icmp eq i64 %j.next, %m
  br i1 %inner_done, label %outer_latch, label %inner_header

inner_error:
  call void @throw(ptr %buf)
  unreachable

outer_latch:
  %i.next = add i64 %i, 1
  %outer_done = icmp eq i64 %i.next, %n
  br i1 %outer_done, label %exit, label %outer_header

exit:
  ret void
}

; Test: Early exit from nested loop to outer error
; Store in outer, error exit from inner - CANNOT sink
; (outer_error is not a direct successor of outer_header, only inner_header is)
; CHECK-LABEL: @test_nested_early_exit_error
; CHECK: outer_header:
; CHECK: store i64 %i, ptr %buf
define void @test_nested_early_exit_error(i64 %n, i64 %m, i1 %c) {
entry:
  %buf = alloca i64, align 8
  br label %outer_header

outer_header:
  %i = phi i64 [ 0, %entry ], [ %i.next, %outer_latch ]
  store i64 %i, ptr %buf, align 8
  br label %inner_header

inner_header:
  %j = phi i64 [ 0, %outer_header ], [ %j.next, %inner_latch ]
  br i1 %c, label %outer_error, label %inner_latch

inner_latch:
  %j.next = add i64 %j, 1
  %inner_done = icmp eq i64 %j.next, %m
  br i1 %inner_done, label %outer_latch, label %inner_header

outer_error:
  call void @throw(ptr %buf)
  unreachable

outer_latch:
  %i.next = add i64 %i, 1
  %outer_done = icmp eq i64 %i.next, %n
  br i1 %outer_done, label %exit, label %outer_header

exit:
  ret void
}

; =============================================================================
; Don't sink INTO a different loop
; =============================================================================

; Test: Store before loop, target is inside loop - should NOT sink
; CHECK-LABEL: @test_no_sink_into_loop
; CHECK: entry:
; CHECK: store i64 %a, ptr %buf
define void @test_no_sink_into_loop(i64 %a, i64 %n, i1 %enter) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  br i1 %enter, label %loop_header, label %skip

loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_latch ]
  call void @use(ptr %buf)
  br label %loop_latch

loop_latch:
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit, label %loop_header

skip:
  ret void

exit:
  ret void
}

; =============================================================================
; Post-dominance cases - when sinking IS allowed in loops
; =============================================================================

; Test: Linear loop body - single successor, no benefit to sinking
; (We skip sinking when there's only one successor since it doesn't reduce work)
; CHECK-LABEL: @test_loop_linear_body
; CHECK: loop_header:
; CHECK: store i64 %i, ptr %buf
define void @test_loop_linear_body(i64 %n) {
entry:
  %buf = alloca i64, align 8
  br label %loop_header

loop_header:
  %i = phi i64 [ 0, %entry ], [ %next, %loop_body ]
  store i64 %i, ptr %buf, align 8
  br label %loop_body

loop_body:
  call void @use(ptr %buf)
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit, label %loop_header

exit:
  ret void
}

; =============================================================================
; Loop with multiple back-edges
; =============================================================================

; Test: Loop with two back-edges (continue from two places)
; Store should NOT be sunk (neither path post-dominates)
; CHECK-LABEL: @test_loop_multiple_backedges
; CHECK: loop_header:
; CHECK: store i64 %i, ptr %buf
define void @test_loop_multiple_backedges(i64 %n, i1 %c1, i1 %c2) {
entry:
  %buf = alloca i64, align 8
  br label %loop_header

loop_header:
  %i = phi i64 [ 0, %entry ], [ %next1, %continue1 ], [ %next2, %continue2 ]
  store i64 %i, ptr %buf, align 8
  br i1 %c1, label %path_a, label %path_b

path_a:
  br i1 %c2, label %use_and_exit, label %continue1

path_b:
  br label %continue2

use_and_exit:
  call void @use(ptr %buf)
  ret void

continue1:
  %next1 = add i64 %i, 1
  br label %loop_header

continue2:
  %next2 = add i64 %i, 2
  br label %loop_header
}

; Store must not be sunk into a different loop.
; CHECK-LABEL: @test_no_sink_into_different_loop
; CHECK: between_loops:
; CHECK: store i64 42, ptr %buf
define void @test_no_sink_into_different_loop(i64 %n) {
entry:
  %buf = alloca i64, align 8
  br label %loop1

loop1:
  %i = phi i64 [ 0, %entry ], [ %next1, %loop1 ]
  %next1 = add nuw nsw i64 %i, 1
  %done1 = icmp eq i64 %next1, %n
  br i1 %done1, label %between_loops, label %loop1

between_loops:
  store i64 42, ptr %buf, align 8
  br i1 true, label %loop2, label %exit

loop2:
  %j = phi i64 [ 0, %between_loops ], [ %next2, %loop2 ]
  %v = load i64, ptr %buf, align 8
  %next2 = add nuw nsw i64 %j, 1
  %done2 = icmp eq i64 %next2, %n
  br i1 %done2, label %exit, label %loop2

exit:
  call void @use(ptr %buf)
  ret void
}
