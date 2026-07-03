; RUN: opt -load-pass-plugin=libjulia-codegen%shlibext -passes='NewSink' -newsink-memoryssa-scanlimit=2 -S %s | FileCheck %s --check-prefix=SCANLIMIT

declare void @throw(ptr)

; MSSA scan limit: with scanlimit=2, the walk aborts and conservatively
; keeps the store. With default limit it would sink.
; SCANLIMIT-LABEL: @test_mssa_scan_limit
; SCANLIMIT: entry:
; SCANLIMIT: store i64 %a, ptr %p
define void @test_mssa_scan_limit(i64 %a, i1 %c1, i1 %c2, i1 %c3) {
entry:
  %p = alloca i64, align 8
  store i64 %a, ptr %p, align 8
  br i1 %c1, label %b1, label %error

b1:
  br i1 %c2, label %b2, label %exit1

b2:
  br i1 %c3, label %b3, label %exit2

b3:
  br label %exit3

exit1:
  ret void

exit2:
  ret void

exit3:
  ret void

error:
  call void @throw(ptr %p)
  unreachable
}

