; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests adapted from LLVM Sink and DSE pass test suites
; These cover interesting edge cases from the official LLVM test infrastructure

;; ============================================================================
;; From LLVM Sink/dead-user.ll - Dead blocks with no predecessors
;; ============================================================================

; Test: Dead block (unreachable infinite loop) should not crash the pass
; The pass should complete without error and preserve the CFG structure
define void @test_dead_block_no_predecessors(i16 %p1, i1 %arg) {
; CHECK-LABEL: @test_dead_block_no_predecessors
; CHECK: bb.0:
; CHECK: %conv = sext i16 %p1 to i32
; CHECK: br i1 %arg, label %bb.1, label %bb.3
bb.0:
  %conv = sext i16 %p1 to i32
  br i1 %arg, label %bb.1, label %bb.3

bb.1:
  br label %bb.2

bb.2:
  %and.2 = and i32 undef, %conv
  br label %bb.2

bb.3:
  %and.3 = and i32 undef, %conv
  br label %bb.3

dead:
  ; No predecessors - this block is unreachable
  %and.dead = and i32 undef, %conv
  br label %dead
}

; Test: Dead block referenced in PHI node (from dead_from_phi)
; The pass should complete without error and preserve the CFG structure
define i32 @test_dead_block_phi(i32 %a) {
; CHECK-LABEL: @test_dead_block_phi
; CHECK: entry:
; CHECK: br i1 %.not, label %if.end, label %if.then
; CHECK: if.end:
; CHECK: phi i32
; CHECK: ret i32
entry:
  %.not = icmp eq i32 %a, 0
  br i1 %.not, label %if.end, label %if.then

if.then:
  %b = and i32 undef, 65535
  br label %if.end

dead:
  ; No predecessors but referenced in PHI
  br label %if.end

if.end:
  %.0 = phi i32 [ %a, %entry ], [ %b, %if.then ], [ %b, %dead ]
  ret i32 %.0
}

;; ============================================================================
;; From LLVM DSE/multiblock-unreachable.ll - Unreachable exits
;; ============================================================================

; Test: Unreachable exit with no call - store before unreachable should be sinkable
define void @unreachable_exit_with_no_call(ptr %ptr, i1 %c.1) {
; CHECK-LABEL: @unreachable_exit_with_no_call
; The store should NOT be sunk to unreachable block since it's also used in if.end
; (DSE removes the first store because it's overwritten on all paths)
entry:
  %loc = alloca i64, align 8
  store i64 1, ptr %loc, align 8
  br i1 %c.1, label %if.then, label %if.end

if.then:
  call void @throw(ptr %loc)
  unreachable

if.end:
  store i64 0, ptr %loc, align 8
  ret void
}

; Test: Store only used in unreachable block CAN be sunk
define void @store_only_used_in_unreachable(i64 %val, i1 %c.1) {
; CHECK-LABEL: @store_only_used_in_unreachable
; CHECK: entry:
; CHECK-NEXT: %loc = alloca
; CHECK-NEXT: br i1
entry:
  %loc = alloca i64, align 8
  store i64 %val, ptr %loc, align 8
  br i1 %c.1, label %if.then, label %if.end

if.then:
  ; CHECK: if.then:
  ; CHECK-NEXT: store i64 %val, ptr %loc
  call void @throw(ptr %loc)
  unreachable

if.end:
  ret void
}

; Test: Call in unreachable block that may read memory
; Store should NOT be sunk because the call in error path may observe it
define void @unreachable_with_may_read_call(ptr %ptr, i1 %c.1) {
; CHECK-LABEL: @unreachable_with_may_read_call
; Store to external ptr is not sunk anyway
; CHECK: entry:
; CHECK: store i64 1, ptr %ptr
entry:
  store i64 1, ptr %ptr, align 8
  br i1 %c.1, label %if.then, label %if.end

if.then:
  ; @exit may read memory
  tail call void @exit()
  unreachable

if.end:
  store i64 0, ptr %ptr, align 8
  ret void
}

;; ============================================================================
;; From LLVM Sink/basic.ll - Diamond patterns
;; ============================================================================

; Test: Diamond CFG where result is used in return (not an error path)
; Our pass only sinks to error paths (unreachable blocks), so this mul is NOT sunk
; (contrast with LLVM's general Sink pass which would sink it)
define i32 @test_diamond_not_sunk_to_non_error(i32 %a, i32 %b, i32 %c) {
; CHECK-LABEL: @test_diamond_not_sunk_to_non_error
entry:
  ; This mul is used only in X, but X is not an error-only block
  ; Our pass does NOT sink to non-error blocks
  ; CHECK: entry:
  ; CHECK: mul nsw
  %1 = mul nsw i32 %c, %b
  %2 = icmp sgt i32 %a, 0
  br i1 %2, label %B0, label %B1

B0:
  br label %X

B1:
  br label %X

X:
  ; X is a normal block (returns), not error-only
  %.01 = phi i32 [ %c, %B0 ], [ %a, %B1 ]
  %R = sub i32 %1, %.01
  ret i32 %R
}

; Test: Diamond CFG where one branch goes to error
; Computation used only in error path SHOULD be sunk
define i32 @test_diamond_sink_to_error(i32 %a, i32 %b, i32 %c) {
; CHECK-LABEL: @test_diamond_sink_to_error
entry:
  ; This mul is used only in error block
  ; CHECK: entry:
  ; CHECK-NOT: mul nsw i32 %c, %b
  %1 = mul nsw i32 %c, %b
  %2 = icmp sgt i32 %a, 0
  br i1 %2, label %ok, label %error

ok:
  ret i32 %a

error:
  ; CHECK: error:
  ; CHECK: mul nsw
  call void @use_i32(i32 %1)
  unreachable
}

; Test: Constant-sized allocas should NOT be sunk from entry
define i64 @test_alloca_not_sunk_from_entry(i64 %a, i64 %b, i64 %bound) {
; CHECK-LABEL: @test_alloca_not_sunk_from_entry
; CHECK: entry:
; CHECK-NEXT: alloca
entry:
  %tuple = alloca i64, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  store i64 %b, ptr %tuple, align 8
  call void @throw(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

;; ============================================================================
;; Complex CFG patterns
;; ============================================================================

; Test: Complex CFG with unreachable blocks and no predecessors
; From DSE multiblock-unreachable.ll @test
; The pass should complete without error on this complex CFG
define void @test_complex_cfg_with_unreachable(ptr %ptr, i1 %c.1, i1 %c.2, i1 %c.3) {
; CHECK-LABEL: @test_complex_cfg_with_unreachable
; CHECK: bb:
; CHECK: br i1 %c.1
; CHECK: bb27:
; CHECK: br i1 %c.3
bb:
  br i1 %c.1, label %bb27, label %bb53

bb10:
  ; No predecessors - dead block
  br label %bb43

bb22:
  ; Self-loop
  br i1 %c.2, label %bb22, label %bb53

bb27:
  br i1 %c.3, label %bb38, label %bb39

bb38:
  ; Infinite loop
  store float 0.000000e+00, ptr %ptr, align 4
  br label %bb38

bb39:
  br i1 %c.2, label %bb43, label %bb38

bb43:
  store float 0.000000e+00, ptr %ptr, align 4
  br label %bb50

bb50:
  br i1 %c.3, label %bb27, label %bb53

bb53:
  ; Unreachable infinite loop
  br label %bb53
}

; Test: Multiple paths to same unreachable block
define i64 @test_multi_path_to_unreachable(i64 %a, i64 %bound1, i64 %bound2) {
; CHECK-LABEL: @test_multi_path_to_unreachable
entry:
  %tuple = alloca i64, align 8
  ; Store should be sunk since only used in error paths
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %tuple
  store i64 %a, ptr %tuple, align 8
  %cmp1 = icmp ult i64 %a, %bound1
  br i1 %cmp1, label %check2, label %error

check2:
  %cmp2 = icmp ult i64 %a, %bound2
  br i1 %cmp2, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %a, ptr %tuple
  call void @throw(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

declare void @throw(ptr)
declare void @exit()
declare void @use_i32(i32)
