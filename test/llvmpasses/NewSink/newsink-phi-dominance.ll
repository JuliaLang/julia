; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test for bug: store using PHI sunk to block where PHI doesn't dominate
;
; CFG:
;   entry -> left -> merge (with PHI) -> noreturn
;         -> right -> noreturn (bypassing merge)
;
; The noreturn block has two predecessors: merge and right
; So merge does NOT dominate noreturn
; The PHI is only valid in merge and blocks dominated by merge
; The store must NOT be sunk to noreturn!

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

declare void @use(ptr)

; CHECK-LABEL: define void @test_phi_multipath
define void @test_phi_multipath(i1 %cond1, i1 %cond2, i1 %cond3) {
entry:
  %p = alloca i64, align 8
  br i1 %cond1, label %left, label %right

left:
  br label %merge

right:
  br i1 %cond2, label %merge, label %noreturn

merge:
  %phi = phi i64 [ 1, %left ], [ 2, %right ]
; CHECK: %phi = phi i64
; CHECK-NEXT: store i64 %phi, ptr %p
  store i64 %phi, ptr %p, align 8
  br i1 %cond3, label %ok, label %noreturn

ok:
  ret void

noreturn:
; CHECK-LABEL: noreturn:
; CHECK-NOT: store i64 %phi
  call void @use(ptr %p)
  unreachable
}
