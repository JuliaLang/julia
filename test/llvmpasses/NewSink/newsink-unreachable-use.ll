; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(NewSink)' -S %s | FileCheck %s

; Test that values used in unreachable blocks (blocks with no predecessors)
; don't cause the pass to crash. These blocks are not in the dominator tree.

target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-unknown-linux-gnu"

; CHECK-LABEL: @test_unreachable_use_blocks
; CHECK: entry:
; CHECK: %gep = getelementptr i8, ptr %p, i64 -152
; CHECK: ret void
define swiftcc void @test_unreachable_use_blocks(ptr %p) {
entry:
  %gep = getelementptr i8, ptr %p, i64 -152
  ret void

unreachable1:                                     ; No predecessors!
  %call1 = call ptr addrspace(10) null(ptr %gep, i64 0, ptr addrspace(10) null)
  unreachable

unreachable2:                                     ; No predecessors!
  %call2 = call ptr addrspace(10) null(ptr %gep, i64 0, ptr addrspace(10) null)
  unreachable
}
