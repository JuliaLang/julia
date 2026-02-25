; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -S %s | FileCheck %s

; Test that the MSSA walk prevents incorrect sinking when both paths need
; the store, and that ambiguity (both targets valid) only arises for dead writes.

declare void @use(ptr)
declare void @throw(ptr)

; Both successors read the store's location. The MSSA walk finds the reader
; on the OTHER path and blocks sinking to either. Store must stay in entry.
; CHECK-LABEL: @test_both_paths_read
; CHECK: entry:
; CHECK: store i64 %a, ptr %buf
; CHECK: br i1
define void @test_both_paths_read(i64 %a, i1 %cond) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  br i1 %cond, label %left, label %right

left:
  call void @use(ptr %buf)
  ret void

right:
  call void @use(ptr %buf)
  ret void
}

; Only one successor reads the store. The MSSA walk blocks sinking to the
; non-reading path, so only one target is valid — no ambiguity.
; CHECK-LABEL: @test_one_path_reads
; CHECK: entry:
; CHECK-NOT: store i64 %a, ptr %buf
; CHECK: br i1
; CHECK: left:
; CHECK: store i64 %a, ptr %buf
define void @test_one_path_reads(i64 %a, i1 %cond) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  br i1 %cond, label %left, label %right

left:
  call void @use(ptr %buf)
  ret void

right:
  ret void
}

; Neither successor reads the store (dead write). Both targets pass the MSSA
; walk. When both are non-noreturn, this is ambiguous and the store stays put.
; DSE can clean up dead stores later.
; CHECK-LABEL: @test_dead_write_ambiguous
; CHECK: entry:
; CHECK: store i64 %a, ptr %buf
; CHECK: br i1
define void @test_dead_write_ambiguous(i64 %a, i1 %cond) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  br i1 %cond, label %left, label %right

left:
  ret void

right:
  ret void
}

; Dead write with one noreturn successor: noreturn wins the tiebreak.
; CHECK-LABEL: @test_dead_write_noreturn_tiebreak
; CHECK: entry:
; CHECK-NOT: store i64 %a, ptr %buf
; CHECK: br i1
; CHECK: error:
; CHECK: store i64 %a, ptr %buf
define void @test_dead_write_noreturn_tiebreak(i64 %a, i1 %cond) {
entry:
  %buf = alloca i64, align 8
  store i64 %a, ptr %buf, align 8
  br i1 %cond, label %ok, label %error

ok:
  ret void

error:
  call void @throw(ptr %buf)
  unreachable
}

; Memory read with two successors that both dominate all uses: ambiguous,
; load stays put.
; CHECK-LABEL: @test_load_ambiguous_successor
; CHECK: entry:
; CHECK: %v = load i64, ptr %p
define i64 @test_load_ambiguous_successor(ptr %p, i1 %cond) {
entry:
  %v = load i64, ptr %p, align 8
  br i1 %cond, label %left, label %right

left:
  br label %merge

right:
  br label %merge

merge:
  call void @use(i64 %v)
  ret i64 %v
}

; The store sinks to right; %val stays in entry (used on both paths).
; CHECK-LABEL: @test_operands_dont_dominate
; CHECK: entry:
; CHECK: %val = add i64 %a, %b
; CHECK-NOT: store
; CHECK: br i1
; CHECK: right:
; CHECK: store i64 %val, ptr %p
declare void @use_i64(i64) nounwind willreturn memory(none)
define void @test_operands_dont_dominate(i64 %a, i64 %b, i1 %cond) {
entry:
  %p = alloca i64, align 8
  %val = add i64 %a, %b
  store i64 %val, ptr %p, align 8
  br i1 %cond, label %left, label %right

left:
  call void @use_i64(i64 %val)
  ret void

right:
  call void @throw(ptr %p)
  unreachable
}
