; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests for sinking stores to error/noreturn paths
; Covers patterns like bounds check error handling where stores should be
; moved to the error path to avoid unnecessary work on the fast path

declare void @use(ptr)
declare void @throw(ptr)

;; ============================================================================
;; Basic sinking to error paths
;; ============================================================================

; Test: Sink store to single-pred error block
; CHECK-LABEL: @sink_to_single_pred_error
; CHECK: entry:
; CHECK-NEXT: %P = alloca i32
; CHECK-NEXT: br i1 %cond
; CHECK: then:
; CHECK-NEXT: store i32 %val, ptr %P
; CHECK-NEXT: call void @use
define void @sink_to_single_pred_error(i32 %val, i1 %cond) {
entry:
  %P = alloca i32, align 4
  store i32 %val, ptr %P
  br i1 %cond, label %then, label %else

then:
  call void @use(ptr %P)
  unreachable

else:
  ret void
}

; Test: Sink store to noreturn block that dominates multiple error exits
; CHECK-LABEL: @sink_to_error_dominator
; CHECK: entry:
; CHECK-NEXT: %P = alloca i64
; CHECK-NEXT: br i1 %c.1
; CHECK: if.then:
; CHECK-NEXT: store i64 1, ptr %P
; CHECK-NEXT: br i1 %c.2
define void @sink_to_error_dominator(i1 %c.1, i1 %c.2) {
entry:
  %P = alloca i64, align 8
  store i64 1, ptr %P
  br i1 %c.1, label %if.then, label %if.end

if.then:
  br i1 %c.2, label %e.0, label %e.1

e.0:
  call void @throw(ptr %P)
  unreachable

e.1:
  call void @throw(ptr %P)
  unreachable

if.end:
  ret void
}

;; ============================================================================
;; Cases that should NOT sink
;; ============================================================================

; Test: Don't sink when entry has only one successor
; CHECK-LABEL: @no_sink_single_succ
; CHECK: entry:
; CHECK-NEXT: %P = alloca i32
; CHECK-NEXT: store i32 0, ptr %P
; CHECK-NEXT: br label %bb0
define void @no_sink_single_succ() {
entry:
  %P = alloca i32, align 4
  store i32 0, ptr %P
  br label %bb0

bb0:
  br i1 true, label %bb1, label %bb2

bb1:
  br label %bb3

bb2:
  call void @use(ptr %P)
  unreachable

bb3:
  ret void
}

; Test: Don't sink to block with multiple predecessors
; CHECK-LABEL: @no_sink_multi_pred
; CHECK: entry:
; CHECK-NEXT: %P = alloca i32
; CHECK-NEXT: store i32 0, ptr %P
; CHECK-NEXT: br i1 %cond
define void @no_sink_multi_pred(i1 %cond) {
entry:
  %P = alloca i32, align 4
  store i32 0, ptr %P
  br i1 %cond, label %bb1, label %merge

bb1:
  store i32 1, ptr %P
  br label %merge

merge:
  ; Multiple predecessors - can't sink here
  call void @use(ptr %P)
  unreachable
}

; Test: Sink to non-error block when it's a valid single-pred successor
; (Unlike DSE which only sinks to noreturn paths, we sink to any valid target)
; CHECK-LABEL: @sink_to_normal_block
; CHECK: entry:
; CHECK-NEXT: %P = alloca i32
; CHECK-NEXT: br i1 %cond
; CHECK: bb2:
; CHECK-NEXT: store i32 0, ptr %P
define void @sink_to_normal_block(i1 %cond) {
entry:
  %P = alloca i32, align 4
  store i32 0, ptr %P
  br i1 %cond, label %bb1, label %bb2

bb1:
  store i32 1, ptr %P
  ret void

bb2:
  ; Normal return block - we still sink here since it's a valid target
  %v = load i32, ptr %P
  ret void
}

