; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests for read clobber scenarios
; Adapted from D136218 multiblock-sink.ll patterns

; Test: Read from location happens in same block before branch
; Store should NOT be sunk
define i64 @test_read_before_branch(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_read_before_branch
entry:
  %loc = alloca i64, align 8
  ; Store happens
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %loc
  store i64 %a, ptr %loc, align 8
  ; Read happens before branch - store cannot be sunk
  %v = load i64, ptr %loc, align 8
  %cmp = icmp ult i64 %v, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %v
}

; Test: Read is only in the other (non-error) successor
; Store CAN be sunk since the read is only on the non-error path
define i64 @test_read_in_ok_path_only(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_read_in_ok_path_only
entry:
  %loc = alloca i64, align 8
  ; Store can be sunk - read is only in ok path, not error path
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %loc
  store i64 %a, ptr %loc, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %a, ptr %loc
  %errv = load i64, ptr %loc, align 8
  call void @use(i64 %errv)
  unreachable

ok:
  ; No read from %loc on this path.
  ret i64 %a
}

; Test: Callable that might read memory before branch
; Store should NOT be sunk
declare void @readonly_fn() readonly

define i64 @test_call_might_read_before_branch(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_call_might_read_before_branch
entry:
  %loc = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %loc
  store i64 %a, ptr %loc, align 8
  ; This call might read from loc
  call void @readonly_fn()
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr %loc, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

; Test: Store then store - second store kills first, so first can potentially be removed
; but our pass just sinks, so both stores would be sunk (second kills first)
define i64 @test_store_after_store(i64 %a, i64 %b, i64 %bound) {
; CHECK-LABEL: @test_store_after_store
entry:
  %loc = alloca i64, align 8
  ; Both stores should be sunk, second overwrites first
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %loc
  ; CHECK-NOT: store i64 %b, ptr %loc
  store i64 %a, ptr %loc, align 8
  store i64 %b, ptr %loc, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %a, ptr %loc
  ; CHECK: store i64 %b, ptr %loc
  %v = load i64, ptr %loc, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

; Test: Specific case - store to different offsets of same alloca
; Both stores should be sunk independently
define i64 @test_stores_different_offsets(i64 %a, i64 %b, i64 %bound) {
; CHECK-LABEL: @test_stores_different_offsets
entry:
  %tuple = alloca [2 x i64], align 8
  %ptr0 = getelementptr [2 x i64], ptr %tuple, i64 0, i64 0
  %ptr1 = getelementptr [2 x i64], ptr %tuple, i64 0, i64 1
  ; Both stores should be sunk
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %a, ptr %ptr0
  ; CHECK-NOT: store i64 %b, ptr %ptr1
  store i64 %a, ptr %ptr0, align 8
  store i64 %b, ptr %ptr1, align 8
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK-DAG: store i64 %a, ptr %ptr0
  ; CHECK-DAG: store i64 %b, ptr %ptr1
  ; CHECK: call void @throw_error
  call void @throw_error(ptr %tuple)
  unreachable

ok:
  ret i64 %a
}

; Intervening non-aliasing write followed by a read from %a.
define i64 @test_intervening_nonaliasing_write(i64 %x, i64 %y, i64 %bound) {
; CHECK-LABEL: @test_intervening_nonaliasing_write
entry:
  %a = alloca i64, align 8
  %b = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK: store i64 %x, ptr %a
  store i64 %x, ptr %a, align 8       ; Our write to %a
  store i64 %y, ptr %b, align 8       ; Non-aliasing write to %b
  %v = load i64, ptr %a, align 8      ; Read from %a - blocks sinking!
  %cmp = icmp ult i64 %v, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %v
}

; Test: Same scenario but read is only on error path - CAN sink
define i64 @test_intervening_nonaliasing_write_error_only(i64 %x, i64 %y, i64 %bound) {
; CHECK-LABEL: @test_intervening_nonaliasing_write_error_only
entry:
  %a = alloca i64, align 8
  %b = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK-NOT: store i64 %x, ptr %a
  store i64 %x, ptr %a, align 8       ; Our write to %a
  store i64 %y, ptr %b, align 8       ; Non-aliasing write to %b
  %cmp = icmp ult i64 %x, %bound
  br i1 %cmp, label %ok, label %error

error:
  ; CHECK: error:
  ; CHECK: store i64 %x, ptr %a
  %v = load i64, ptr %a, align 8      ; Read only on error path
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %x
}

; Regression: clobbering source-block write blocks sinking.
define i64 @test_intervening_volatile_clobber_blocks_sink(i64 %x, i64 %y, i64 %bound) {
; CHECK-LABEL: @test_intervening_volatile_clobber_blocks_sink
entry:
  %a = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK-NEXT: %a = alloca i64, align 8
  ; CHECK-NEXT: store i64 %x, ptr %a, align 8
  ; CHECK-NEXT: store volatile i64 %y, ptr %a, align 8
  store i64 %x, ptr %a, align 8
  store volatile i64 %y, ptr %a, align 8
  %cmp = icmp ult i64 %x, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr %a, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %x
}

; Regression: same pattern with ordered atomic clobber.
define i64 @test_intervening_atomic_clobber_blocks_sink(i64 %x, i64 %y, i64 %bound) {
; CHECK-LABEL: @test_intervening_atomic_clobber_blocks_sink
entry:
  %a = alloca i64, align 8
  ; CHECK: entry:
  ; CHECK-NEXT: %a = alloca i64, align 8
  ; CHECK-NEXT: store i64 %x, ptr %a, align 8
  ; CHECK-NEXT: store atomic i64 %y, ptr %a seq_cst, align 8
  store i64 %x, ptr %a, align 8
  store atomic i64 %y, ptr %a seq_cst, align 8
  %cmp = icmp ult i64 %x, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr %a, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %x
}

declare void @use(i64)
declare void @throw_error(ptr)

; Load and store to the same alloca sink independently to different paths.
; CHECK-LABEL: @test_sink_read_before_store
; CHECK: entry:
; CHECK-NOT: load
; CHECK-NOT: store
; CHECK: br i1 %cond
; CHECK: then:
; CHECK: store i64 %a, ptr %loc
; CHECK: else:
; CHECK: %early_read = load i64, ptr %loc
define i64 @test_sink_read_before_store(i64 %a, i64 %b, i1 %cond) {
entry:
  %loc = alloca i64, align 8
  %early_read = load i64, ptr %loc, align 8
  store i64 %a, ptr %loc, align 8
  br i1 %cond, label %then, label %else

then:
  %v = load i64, ptr %loc, align 8
  ret i64 %v

else:
  ret i64 %early_read
}

; Both the initial store and load sink to %then. The load executes before
; the clobbering store so it still reads the correct value (0).
; CHECK-LABEL: @test_load_sunk_before_clobber
define void @test_load_sunk_before_clobber(i1 %cond) {
entry:
  %p = alloca i64, align 8
  store i64 0, ptr %p, align 8
; CHECK: entry:
; CHECK-NOT: store
; CHECK-NOT: load
; CHECK: br i1 %cond
  %v = load i64, ptr %p, align 8
  br i1 %cond, label %then, label %error

then:
; CHECK: then:
; CHECK: store i64 0, ptr %p
; CHECK-NEXT: %v = load i64, ptr %p
; CHECK-NEXT: store i64 42, ptr %p
  store i64 42, ptr %p, align 8
  br label %use_block

use_block:
; CHECK: use_block:
; CHECK-NOT: load
; CHECK: call void @use_val(i64 %v)
  call void @use_val(i64 %v)
  ret void

error:
  unreachable
}

declare void @use_val(i64)
