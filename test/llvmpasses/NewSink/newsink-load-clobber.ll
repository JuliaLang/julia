; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test that loads are not incorrectly sunk past clobbering writes in
; intermediate blocks. This is a regression test for a bug where load
; sinking didn't check for writes in blocks between source and target.


declare void @use(i64)

; Both the initial store and load sink to %then. The load executes before the
; clobbering store so it still reads the correct value (0).
; CHECK-LABEL: @test_load_not_sunk_past_clobber
define void @test_load_not_sunk_past_clobber(i1 %cond) {
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
; CHECK: call void @use(i64 %v)
  call void @use(i64 %v)
  ret void

error:
  unreachable
}
