; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Tests for instructions that should NOT be sunk

declare void @throw_error(ptr)
declare void @use(i64)
declare void @use_ptr(ptr)
declare token @llvm.coro.id(i32, ptr, ptr, ptr)
declare i1 @llvm.coro.alloc(token)
declare i64 @may_not_return(i64) memory(none)

; Test: Convergent calls should not be sunk
define i64 @test_convergent(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_convergent
; CHECK: entry:
; CHECK-NEXT: %result = call i64 @convergent_op(i64 %val)
; CHECK-NEXT: %cmp = icmp
entry:
  %result = call i64 @convergent_op(i64 %val)
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %result)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

declare i64 @convergent_op(i64) convergent

; Test: Inline asm should not be sunk
define i64 @test_inline_asm(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_inline_asm
; CHECK: entry:
; CHECK-NEXT: %result = call i64 asm
; CHECK-NEXT: %cmp = icmp
entry:
  %result = call i64 asm "mov $1, $0", "=r,r"(i64 %val)
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %result)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

; Test: nomerge calls should not be sunk
define i64 @test_nomerge(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_nomerge
; CHECK: entry:
; CHECK-NEXT: %result = call i64 @nomerge_op(i64 %val)
; CHECK-NEXT: %cmp = icmp
entry:
  %result = call i64 @nomerge_op(i64 %val) nomerge
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %result)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

declare i64 @nomerge_op(i64)

; Test: Volatile loads should not be sunk
define i64 @test_volatile_load(ptr %p, i64 %bound) {
; CHECK-LABEL: @test_volatile_load
; CHECK: entry:
; CHECK-NEXT: %val = load volatile i64, ptr %p
; CHECK-NEXT: %cmp = icmp
entry:
  %val = load volatile i64, ptr %p
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %val)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 0
}

; Test: Atomic loads with ordering should not be sunk
define i64 @test_atomic_load(ptr %p, i64 %bound) {
; CHECK-LABEL: @test_atomic_load
; CHECK: entry:
; CHECK-NEXT: %val = load atomic i64, ptr %p seq_cst
; CHECK-NEXT: %cmp = icmp
entry:
  %val = load atomic i64, ptr %p seq_cst, align 8
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %val)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 0
}

; Test: Volatile stores should not be sunk
define i64 @test_volatile_store(ptr %p, i64 %val, i64 %bound) {
; CHECK-LABEL: @test_volatile_store
; CHECK: entry:
; CHECK-NEXT: store volatile i64 %val, ptr %p
; CHECK-NEXT: %cmp = icmp
entry:
  store volatile i64 %val, ptr %p
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @throw_error(ptr %p)
  unreachable

ok:
  ret i64 %val
}

; Test: Fence should not be sunk
define i64 @test_fence(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_fence
; CHECK: entry:
; CHECK-NEXT: fence seq_cst
; CHECK-NEXT: %cmp = icmp
entry:
  fence seq_cst
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

; Test: AtomicRMW should not be sunk
define i64 @test_atomicrmw(ptr %p, i64 %val, i64 %bound) {
; CHECK-LABEL: @test_atomicrmw
; CHECK: entry:
; CHECK-NEXT: %old = atomicrmw add ptr %p, i64 %val seq_cst
; CHECK-NEXT: %cmp = icmp
entry:
  %old = atomicrmw add ptr %p, i64 %val seq_cst
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %old)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

; Test: AtomicCmpXchg should not be sunk
define i64 @test_cmpxchg(ptr %p, i64 %expected, i64 %new, i64 %bound) {
; CHECK-LABEL: @test_cmpxchg
; CHECK: entry:
; CHECK-NEXT: %result = cmpxchg ptr %p, i64 %expected, i64 %new seq_cst seq_cst
; CHECK-NEXT: %val = extractvalue
; CHECK-NEXT: %cmp = icmp
entry:
  %result = cmpxchg ptr %p, i64 %expected, i64 %new seq_cst seq_cst
  %val = extractvalue { i64, i1 } %result, 0
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %val)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

; Test: Calls that may throw should not be sunk
define i64 @test_maythrow(i64 %val, i64 %bound) {
; CHECK-LABEL: @test_maythrow
; CHECK: entry:
; CHECK-NEXT: %result = call i64 @may_throw_op(i64 %val)
; CHECK-NEXT: %cmp = icmp
entry:
  %result = call i64 @may_throw_op(i64 %val)
  %cmp = icmp ult i64 %val, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @use(i64 %result)
  call void @throw_error(ptr null)
  unreachable

ok:
  ret i64 %val
}

declare i64 @may_throw_op(i64)


; Test: Store to local alloca NOT sunk past fence (conservative)
define i64 @test_no_sink_store_past_fence(i64 %a, i64 %bound) {
; CHECK-LABEL: @test_no_sink_store_past_fence
entry:
  %loc = alloca i64, align 8
  ; Store is NOT sunk past fence
  ; CHECK: entry:
  ; CHECK: store i64 %a, ptr %loc
  ; CHECK: fence seq_cst
  store i64 %a, ptr %loc, align 8
  fence seq_cst
  %cmp = icmp ult i64 %a, %bound
  br i1 %cmp, label %ok, label %error

error:
  %v = load i64, ptr %loc, align 8
  call void @use(i64 %v)
  unreachable

ok:
  ret i64 %a
}

; Test: Store used on both paths should NOT be sunk
define double @test_store_used_both_paths(ptr %array, i64 %i, i64 %bound) {
; CHECK-LABEL: @test_store_used_both_paths
entry:
  %shared = alloca i64, align 8
  ; This store should NOT be sunk because %shared is loaded on the ok path
  ; CHECK: entry:
  ; CHECK: store i64 %i, ptr %shared
  store i64 %i, ptr %shared, align 8

  %cmp = icmp ult i64 %i, %bound
  br i1 %cmp, label %ok, label %error

error:
  call void @throw_error(ptr %shared)
  unreachable

ok:
  ; Also used here - prevents sinking
  %loaded = load i64, ptr %shared, align 8
  %arrayptr = getelementptr double, ptr %array, i64 %loaded
  %val = load double, ptr %arrayptr, align 8
  ret double %val
}

; Test: Calls without willreturn should not be sunk
; CHECK-LABEL: @test_no_willreturn
; CHECK: entry:
; CHECK: call i64 @may_not_return
define i64 @test_no_willreturn(i64 %a, i1 %cond) {
entry:
  %v = call i64 @may_not_return(i64 %a)
  br i1 %cond, label %use_it, label %skip

use_it:
  call void @use(i64 %v)
  unreachable

skip:
  ret i64 0
}

; Test: Token-typed instructions should not be sunk
; CHECK-LABEL: @test_token_type
; CHECK: entry:
; CHECK: call token @llvm.coro.id
define void @test_token_type(i1 %cond) {
entry:
  %tok = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null)
  %alloc = call i1 @llvm.coro.alloc(token %tok)
  br i1 %cond, label %use_it, label %skip

use_it:
  br label %skip

skip:
  ret void
}

; Test: Store in a block with one successor stays (no benefit from sinking)
; CHECK-LABEL: @test_single_successor_store
; CHECK: entry:
; CHECK: store i64 42, ptr %p
define void @test_single_successor_store() {
entry:
  %p = alloca i64, align 8
  store i64 42, ptr %p, align 8
  br label %next

next:
  call void @use_ptr(ptr %p)
  ret void
}
