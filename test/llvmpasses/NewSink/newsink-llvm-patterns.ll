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

; Store sinks to if.then (only read there). The store in if.end writes a
; different value and doesn't depend on the first store.
define void @unreachable_exit_with_no_call(ptr %ptr, i1 %c.1) {
; CHECK-LABEL: @unreachable_exit_with_no_call
; CHECK: entry:
; CHECK-NOT: store
; CHECK: br i1
; CHECK: if.then:
; CHECK-NEXT: store i64 1, ptr %loc
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

declare void @throw(ptr)
