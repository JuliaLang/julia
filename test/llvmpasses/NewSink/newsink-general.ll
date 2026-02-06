; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests for general sinking (D136218-style) to non-error single-predecessor successors

; Test: Sink computation to single-predecessor successor where it's only used
define i64 @test_general_sink_to_single_pred(i64 %a, i64 %b, i1 %cond) {
; CHECK-LABEL: @test_general_sink_to_single_pred
entry:
  ; This computation is only used in the 'then' block
  ; 'then' has single predecessor (entry), so we can sink
  ; CHECK: entry:
  ; CHECK-NOT: %sum = add i64 %a, %b
  %sum = add i64 %a, %b
  br i1 %cond, label %then, label %else

then:
  ; CHECK: then:
  ; CHECK: %sum = add i64 %a, %b
  %result = mul i64 %sum, 2
  ret i64 %result

else:
  ret i64 %a
}

; Test: Don't sink if successor has multiple predecessors
define i64 @test_no_sink_multi_pred(i64 %a, i64 %b, i1 %cond1, i1 %cond2) {
; CHECK-LABEL: @test_no_sink_multi_pred
entry:
  ; This computation is used in 'merge' which has multiple predecessors
  ; CHECK: entry:
  ; CHECK: %sum = add i64 %a, %b
  %sum = add i64 %a, %b
  br i1 %cond1, label %path1, label %path2

path1:
  br label %merge

path2:
  br label %merge

merge:
  ; Multiple predecessors - can't sink here
  %result = mul i64 %sum, 2
  ret i64 %result
}

; Test: Sink store to single-predecessor successor (D136218 pattern)
define i64 @test_general_sink_store(i64 %a, i64 %b, i1 %cond) {
; CHECK-LABEL: @test_general_sink_store
entry:
  %loc = alloca i64, align 8
  ; Store only loaded in 'then' block, which has single predecessor
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %loc
  store i64 %a, ptr %loc, align 8
  br i1 %cond, label %then, label %else

then:
  ; CHECK: then:
  ; CHECK: store i64 %a, ptr %loc
  %v = load i64, ptr %loc, align 8
  ret i64 %v

else:
  ret i64 %b
}

; Test: Read before store CAN be sunk independently
; The early_read doesn't depend on the store (it reads a previous value),
; so both can be sunk to their respective use paths.
define i64 @test_sink_read_before_store(i64 %a, i64 %b, i1 %cond) {
; CHECK-LABEL: @test_sink_read_before_store
entry:
  %loc = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK-NOT: load
  ; CHECK-NOT: store
  ; CHECK: br i1 %cond
  %early_read = load i64, ptr %loc, align 8
  store i64 %a, ptr %loc, align 8
  br i1 %cond, label %then, label %else

then:
  ; CHECK: then:
  ; CHECK: store i64 %a, ptr %loc
  %v = load i64, ptr %loc, align 8
  ret i64 %v

else:
  ; CHECK: else:
  ; CHECK: %early_read = load i64, ptr %loc
  ret i64 %early_read
}

; Test: Sink chain of dependent instructions to single-pred successor
define i64 @test_general_sink_chain(i64 %a, i64 %b, i1 %cond) {
; CHECK-LABEL: @test_general_sink_chain
entry:
  ; Entire chain only used in 'then'
  ; CHECK: entry:
  ; CHECK-NOT: %step1 = add i64 %a, %b
  ; CHECK-NOT: %step2 = mul i64 %step1, 2
  %step1 = add i64 %a, %b
  %step2 = mul i64 %step1, 2
  br i1 %cond, label %then, label %else

then:
  ; CHECK: then:
  ; CHECK: %step1 = add i64 %a, %b
  ; CHECK: %step2 = mul i64 %step1, 2
  ; CHECK: ret i64 %step2
  ret i64 %step2

else:
  ret i64 %a
}

; Test: Don't sink if used in both successors
define i64 @test_no_sink_used_in_both(i64 %a, i64 %b, i1 %cond) {
; CHECK-LABEL: @test_no_sink_used_in_both
entry:
  ; Used in both successors - can't sink
  ; CHECK: entry:
  ; CHECK: %sum = add i64 %a, %b
  %sum = add i64 %a, %b
  br i1 %cond, label %then, label %else

then:
  %r1 = mul i64 %sum, 2
  ret i64 %r1

else:
  %r2 = mul i64 %sum, 3
  ret i64 %r2
}

; Test: Sink to single-pred even if not an error block
; This is the key D136218 behavior
define i64 @test_sink_to_normal_single_pred(i64 %a, i64 %bound, i1 %extra_cond) {
; CHECK-LABEL: @test_sink_to_normal_single_pred
entry:
  %tuple = alloca i64, align 8
  ; Store only used in 'slow_path' which has single predecessor and returns normally
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %tuple
  store i64 %a, ptr %tuple, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %fast_path, label %slow_path

fast_path:
  ret i64 %a

slow_path:
  ; Single predecessor, normal return (not error)
  ; CHECK: slow_path:
  ; CHECK: store i64 %a, ptr %tuple
  %v = load i64, ptr %tuple, align 8
  %result = add i64 %v, 1
  ret i64 %result
}
