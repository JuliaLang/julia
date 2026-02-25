; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test cases with multiple predecessors to error blocks
; Adapted from D136218 multiblock-sink.ll patterns

; Test: Error block has multiple predecessors, stores from different blocks.
; Both stores should be sunk via edge splitting — each gets its own block
; on the critical edge from its source to the multi-pred error block.
define i64 @test_multi_pred_error_block(i64 %a, i64 %b, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_multi_pred_error_block
entry:
  %tuple = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK-NOT: store
  ; CHECK: br i1 %cmp1
  store i64 %a, ptr %tuple, align 8
  %cmp1 = icmp ult i64 %a, %bound1
  br i1 %cmp1, label %check2, label %error

; CHECK: entry.error_crit_edge:
; CHECK-NEXT: store i64 %a, ptr %tuple

check2:
  ; CHECK: check2:
  ; CHECK-NOT: store
  ; CHECK: br i1 %cmp2
  store i64 %b, ptr %tuple, align 8
  %cmp2 = icmp ult i64 %b, %bound2
  br i1 %cmp2, label %ok, label %error

; CHECK: check2.error_crit_edge:
; CHECK-NEXT: store i64 %b, ptr %tuple

error:
  call void @throw_bounds_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

; Test: Same store location from two different predecessor blocks
; Both paths are already in error-only region, so stores stay in place
; (no benefit from sinking further - they're already off the hot path)
define i64 @test_diamond_to_error(i64 %a, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_diamond_to_error
entry:
  %tuple = alloca i64, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %fail

fail:
  ; fail is error-only (all successors lead to unreachable)
  ; CHECK: fail:
  ; CHECK-NEXT: br i1 %cond
  br i1 %cond, label %fail.path1, label %fail.path2

fail.path1:
  ; fail.path1 is error-only, stores stay here
  ; CHECK: fail.path1:
  ; CHECK-NEXT: store i64 %a
  store i64 %a, ptr %tuple, align 8
  br label %error

fail.path2:
  ; fail.path2 is error-only, stores stay here
  ; CHECK: fail.path2:
  ; CHECK-NEXT: %a2 = add
  ; CHECK-NEXT: store i64 %a2
  %a2 = add i64 %a, 1
  store i64 %a2, ptr %tuple, align 8
  br label %error

error:
  ; CHECK: error:
  call void @throw_bounds_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

; Test: Nested conditionals leading to multiple error blocks.
; Mark throw callees readonly to avoid conservative AA blocking sinks.
define i64 @test_nested_errors(i64 %a, i64 %b, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_nested_errors
entry:
  %tuple1 = alloca i64, align 8
  %tuple2 = alloca i64, align 8
  ; Store for first error
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %tuple1
  store i64 %a, ptr %tuple1, align 8

  %cmp1 = icmp ult i64 %a, %bound1
  br i1 %cmp1, label %check2, label %error1

check2:
  ; Store for second error
  ; CHECK: check2:
  ; CHECK-NOT: store i64 %b, ptr %tuple2
  store i64 %b, ptr %tuple2, align 8
  %cmp2 = icmp ult i64 %b, %bound2
  br i1 %cmp2, label %ok, label %error2

error1:
  ; CHECK: error1:
  ; CHECK: store i64 %a, ptr %tuple1
  call void @throw_error1(ptr nocapture readonly %tuple1)
  unreachable

error2:
  ; CHECK: error2:
  ; CHECK: store i64 %b, ptr %tuple2
  call void @throw_error2(ptr nocapture readonly %tuple2)
  unreachable

ok:
  ret i64 %a
}

declare void @throw_bounds_error(ptr)
declare void @throw_error1(ptr nocapture readonly)
declare void @throw_error2(ptr nocapture readonly)
declare void @use(i64)

;; ============================================================================
;; PHI node handling
;; ============================================================================

; Test: PHI nodes in error blocks - values used by PHI should sink to incoming blocks
define i64 @test_phi_in_error_block(i64 %a, i64 %b, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_phi_in_error_block
entry:
  ; These should be sunk since PHI uses are in error block
  ; CHECK-NOT: %sum1 = add i64 %a, 1
  ; CHECK-NOT: %sum2 = add i64 %b, 2
  %sum1 = add i64 %a, 1
  %sum2 = add i64 %b, 2

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error_entry

error_entry:
  br i1 %cond, label %error1, label %error2

error1:
  ; CHECK: error1:
  ; CHECK: %sum1 = add i64 %a, 1
  br label %error_merge

error2:
  ; CHECK: error2:
  ; CHECK: %sum2 = add i64 %b, 2
  br label %error_merge

error_merge:
  %phi = phi i64 [ %sum1, %error1 ], [ %sum2, %error2 ]
  call void @use(i64 %phi)
  unreachable

ok:
  ret i64 %a
}

; Test: PHI node with use from non-error block - should NOT sink
define i64 @test_phi_mixed_sources(i64 %a, i64 %b, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_phi_mixed_sources
entry:
  ; This should NOT be sunk because PHI has incoming from non-error block
  ; CHECK: entry:
  ; CHECK: %sum = add i64 %a, %b
  %sum = add i64 %a, %b

  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %maybe_ok, label %error

maybe_ok:
  br i1 %cond, label %ok, label %also_error

also_error:
  br label %merge

error:
  br label %merge

merge:
  ; PHI has incoming from maybe_ok (not error-only) and error
  %phi = phi i64 [ %sum, %also_error ], [ %sum, %error ]
  call void @use(i64 %phi)
  unreachable

ok:
  ret i64 %b
}

;; ============================================================================
;; Common error dominator patterns
;; ============================================================================

; Test: Multiple error blocks share a common error-only dominator
define i64 @test_sink_common_error_dominator(i64 %a, i64 %b, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_sink_common_error_dominator
; CHECK: entry:
; CHECK-NEXT: %cmp = icmp
; CHECK-NEXT: br i1 %cmp
entry:
  ; This should be sunk to error_dispatch (common dominator of error1 and error2)
  %sum = add i64 %a, %b
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error_dispatch

error_dispatch:
  ; CHECK: error_dispatch:
  ; CHECK: %sum = add i64 %a, %b
  br i1 %cond, label %error1, label %error2

error1:
  call void @use(i64 %sum)
  unreachable

error2:
  call void @use(i64 %sum)
  unreachable

ok:
  ret i64 %b
}

; Test: Error blocks without common error-only dominator - should NOT sink
define i64 @test_no_sink_no_common_error_dominator(i64 %a, i64 %b, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_no_sink_no_common_error_dominator
entry:
  ; This should NOT be sunk because common dominator of error1 and error2
  ; is entry, which is not an error-only block
  ; CHECK: entry:
  ; CHECK: %sum = add i64 %a, %b
  %sum = add i64 %a, %b
  %cmp1 = icmp ult i64 %a, %bound1
  br i1 %cmp1, label %check2, label %error1

check2:
  %cmp2 = icmp ult i64 %b, %bound2
  br i1 %cmp2, label %ok, label %error2

error1:
  call void @use(i64 %sum)
  unreachable

error2:
  call void @use(i64 %sum)
  unreachable

ok:
  ret i64 %b
}
