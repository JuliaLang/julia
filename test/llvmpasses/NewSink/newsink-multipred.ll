; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test cases with multiple predecessors to error blocks
; Adapted from D136218 multiblock-sink.ll patterns

; Test: Error block has multiple predecessors, stores from different blocks.
; Stores are NOT sunk to multi-predecessor targets because the MSSA walk
; cannot guarantee reads in the target see correct values from all incoming
; edges after a critical edge split. This is a conservative restriction
; that prevents miscompilation when an object escapes and is read later.
define i64 @test_multi_pred_error_block(i64 %a, i64 %b, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_multi_pred_error_block
entry:
  %tuple = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %tuple
  ; CHECK: br i1 %cmp1
  store i64 %a, ptr %tuple, align 8
  %cmp1 = icmp ult i64 %a, %bound1
  br i1 %cmp1, label %check2, label %error

check2:
  ; CHECK: check2:
  ; CHECK: store i64 %b, ptr %tuple
  ; CHECK: br i1 %cmp2
  store i64 %b, ptr %tuple, align 8
  %cmp2 = icmp ult i64 %b, %bound2
  br i1 %cmp2, label %ok, label %error

error:
  call void @throw_bounds_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

declare void @throw_bounds_error(ptr)
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
  %phi = phi i64 [ %sum, %also_error ], [ %sum, %error ]
  call void @use(i64 %phi)
  unreachable

ok:
  ret i64 %b
}

;; ============================================================================
;; Common error dominator patterns
;; ============================================================================

; Value sinks to common dominator of all use blocks.
define i64 @test_sink_common_error_dominator(i64 %a, i64 %b, i64 %bound, i1 %cond) {
; CHECK-LABEL: @test_sink_common_error_dominator
; CHECK: entry:
; CHECK-NEXT: %cmp = icmp
; CHECK-NEXT: br i1 %cmp
entry:
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

; NCD of use blocks is entry (CurrentBB) — stays put.
define i64 @test_no_sink_no_common_error_dominator(i64 %a, i64 %b, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_no_sink_no_common_error_dominator
entry:
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
