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
