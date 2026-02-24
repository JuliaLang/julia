; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Bug: The NewSink pass sinks memory(read) calls past returns_twice functions.
; In Julia's exception handling, this causes ijl_excstack_state() to be sunk
; from before __sigsetjmp() into the catch block, capturing the wrong exception
; stack state and breaking exception cleanup.
;
; Root cause: findSinkTargetForValue only applies canSinkLoad (alias-based
; clobber checking) to LoadInst. A CallInst with memory(read) is functionally
; equivalent to a load but bypasses all clobber checks. Additionally, the pass
; does not treat returns_twice as a sinking barrier.

; --- Function declarations with real Julia attributes ---

; ijl_excstack_state: captures exception stack state (pure read)
declare i64 @ijl_excstack_state(ptr) #0

; ijl_enter_handler: sets up exception handler (readwrite, no special attrs)
declare void @ijl_enter_handler(ptr, ptr)

; __sigsetjmp: returns 0 normally, non-zero on longjmp (returns_twice)
declare i32 @__sigsetjmp(ptr, i32) #1

; ijl_pop_handler / ijl_restore_excstack: exception cleanup
declare void @ijl_pop_handler(ptr, i32) #2
declare void @ijl_restore_excstack(ptr, i64)

; Generic helpers
declare void @try_body()
declare void @use(i64)

; Attribute groups matching real Julia LLVM IR
attributes #0 = { mustprogress nofree nounwind willreturn memory(read) }
attributes #1 = { returns_twice }
attributes #2 = { mustprogress nounwind willreturn }

; ============================================================================
; Test 1: Julia exception handling pattern
;
; ijl_excstack_state captures the exception stack BEFORE sigsetjmp.
; If sunk to catch_enter, it captures state AFTER the exception is thrown,
; so ijl_restore_excstack restores the wrong state and the exception leaks.
; ============================================================================

define void @test_julia_eh_pattern(ptr %task, ptr %handler) {
; CHECK-LABEL: @test_julia_eh_pattern
; excstack_state must stay in entry, before sigsetjmp
; CHECK: entry:
; CHECK: %state = call i64 @ijl_excstack_state(ptr %task)
; CHECK: call void @ijl_enter_handler(ptr %task, ptr %handler)
; CHECK: call i32 @__sigsetjmp(ptr %handler, i32 0)
entry:
  %state = call i64 @ijl_excstack_state(ptr %task)
  call void @ijl_enter_handler(ptr %task, ptr %handler)
  %r = call i32 @__sigsetjmp(ptr %handler, i32 0) #1
  %is_try = icmp eq i32 %r, 0
  br i1 %is_try, label %try, label %catch

try:
  call void @try_body()
  call void @ijl_pop_handler(ptr %task, i32 1)
  br label %done

catch:
  call void @ijl_pop_handler(ptr %task, i32 1)
  call void @ijl_restore_excstack(ptr %task, i64 %state)
  br label %done

done:
  ret void
}

; ============================================================================
; Test 2: Minimal returns_twice barrier test
;
; A memory(read) call used only after the second return of a returns_twice
; function must NOT be sunk past it, even when the returns_twice function
; only writes to its argument (so alias analysis alone wouldn't block it).
; ============================================================================

declare i64 @read_global_state() #0
declare i32 @pure_setjmp(ptr) #3

; returns_twice + only writes to its argument
attributes #3 = { returns_twice nounwind memory(argmem: readwrite) }

define i64 @test_no_sink_past_returns_twice(ptr %buf) {
; CHECK-LABEL: @test_no_sink_past_returns_twice
; read_global_state must stay before pure_setjmp
; CHECK: entry:
; CHECK: %state = call i64 @read_global_state()
; CHECK: call i32 @pure_setjmp(ptr %buf)
entry:
  %state = call i64 @read_global_state()
  %r = call i32 @pure_setjmp(ptr %buf) #3
  %cmp = icmp eq i32 %r, 0
  br i1 %cmp, label %normal, label %handler

normal:
  ret i64 0

handler:
  call void @use(i64 %state)
  ret i64 %state
}

; ============================================================================
; Test 3: memory(read) call should not be sunk past a readwrite call
;
; Even without returns_twice, a memory(read) call should not be sunk past
; an intervening call that may write to the same memory. This is analogous
; to canSinkLoad's clobber check, but for call instructions.
; ============================================================================

declare void @modify_state(ptr)

define i64 @test_no_sink_readonly_call_past_write(ptr %p) {
; CHECK-LABEL: @test_no_sink_readonly_call_past_write
; read_global_state must not be sunk past modify_state
; CHECK: entry:
; CHECK: %state = call i64 @read_global_state()
; CHECK: call void @modify_state(ptr %p)
entry:
  %state = call i64 @read_global_state()
  call void @modify_state(ptr %p)
  %cmp = icmp eq i64 %state, 0
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %state)
  unreachable

ok:
  ret i64 0
}
