; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='ColdUnreachableBranches' -S %s | FileCheck %s

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-linux-gnu"

declare void @error_helper() noreturn
declare i64 @use(i64)

; Throwing successor is the second target: weights should be (2000, 1).
; CHECK-LABEL: @branch_throw_on_false
; CHECK: br i1 %cond, label %ok, label %err, !prof ![[#W1:]]
define i64 @branch_throw_on_false(i1 %cond, i64 %x) {
top:
  br i1 %cond, label %ok, label %err

ok:
  ret i64 %x

err:
  call void @error_helper()
  unreachable
}

; Throwing successor is the first target: weights should be (1, 2000).
; CHECK-LABEL: @branch_throw_on_true
; CHECK: br i1 %cond, label %err, label %ok, !prof ![[#W2:]]
define i64 @branch_throw_on_true(i1 %cond, i64 %x) {
top:
  br i1 %cond, label %err, label %ok

err:
  call void @error_helper()
  unreachable

ok:
  ret i64 %x
}

; The "always-unreachable" successor branches internally before reaching
; `unreachable`. The structurally-sensitive predecessor of the original
; implementation would miss this; the reachability-based analysis catches it.
; CHECK-LABEL: @branch_throw_indirect
; CHECK: br i1 %cond, label %ok, label %err1, !prof ![[#W1]]
define i64 @branch_throw_indirect(i1 %cond, i1 %inner, i64 %x) {
top:
  br i1 %cond, label %ok, label %err1

ok:
  ret i64 %x

err1:
  br i1 %inner, label %err2, label %err3

err2:
  call void @error_helper()
  unreachable

err3:
  call void @error_helper()
  unreachable
}

; Both successors reach a return, so no annotation should be added.
; CHECK-LABEL: @no_unreachable_side
; CHECK: br i1 %cond, label %a, label %b{{$}}
define i64 @no_unreachable_side(i1 %cond, i64 %x) {
top:
  br i1 %cond, label %a, label %b

a:
  ret i64 %x

b:
  %y = call i64 @use(i64 %x)
  ret i64 %y
}

; An existing !prof annotation must be preserved (we should not overwrite it).
; CHECK-LABEL: @existing_prof_preserved
; CHECK: br i1 %cond, label %ok, label %err, !prof ![[#EXISTING:]]
define i64 @existing_prof_preserved(i1 %cond, i64 %x) {
top:
  br i1 %cond, label %ok, label %err, !prof !0

ok:
  ret i64 %x

err:
  call void @error_helper()
  unreachable
}

; CHECK-DAG: ![[#W1]] = !{!"branch_weights", i32 2000, i32 1}
; CHECK-DAG: ![[#W2]] = !{!"branch_weights", i32 1, i32 2000}
; CHECK-DAG: ![[#EXISTING]] = !{!"branch_weights", i32 100, i32 100}

!0 = !{!"branch_weights", i32 100, i32 100}
