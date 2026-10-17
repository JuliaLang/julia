; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

; Write barriers on a fresh allocation are removed when no safepoint lies in between.

@tag = external addrspace(10) global {}, align 16

declare ptr @julia.get_pgcstack()
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_obj(ptr, i64, ptr addrspace(10))
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_memory(ptr, i64, ptr addrspace(10), i64, i1)
declare void @julia.object_write_barrier(ptr addrspace(10), ...)
declare void @julia.field_write_barrier.p11(ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...)
declare void @safepoint()
declare i64 @readonly_helper(i64) memory(read)
declare i32 @memcmp(ptr, ptr, i64)

; CHECK-LABEL: @fresh
; CHECK-NOT: julia.object_write_barrier
; CHECK: ret
define ptr addrspace(10) @fresh(ptr addrspace(10) %child, i64 %x) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  %y = call i64 @readonly_helper(i64 %x)
  %slot = addrspacecast ptr addrspace(10) %obj to ptr addrspace(11)
  store ptr addrspace(10) %child, ptr addrspace(11) %slot, align 8
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; CHECK-LABEL: @fresh_memory
; CHECK-NOT: julia.field_write_barrier
; CHECK: ret
define ptr addrspace(10) @fresh_memory(ptr addrspace(10) %child) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call ptr addrspace(10) @julia.gc_alloc_memory(ptr %pgcstack, i64 8, ptr addrspace(10) @tag, i64 1, i1 true)
  %slot = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  call void (ptr addrspace(10), ptr addrspace(11), ptr addrspace(10), ...) @julia.field_write_barrier.p11(ptr addrspace(10) %mem, ptr addrspace(11) %slot, ptr addrspace(10) %child)
  ret ptr addrspace(10) %mem
}

; CHECK-LABEL: @after_safepoint
; CHECK: call void @safepoint()
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @after_safepoint(ptr addrspace(10) %child) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  call void @safepoint()
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; A safepoint on either path keeps the barrier.
; CHECK-LABEL: @safepoint_on_one_path
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @safepoint_on_one_path(ptr addrspace(10) %child, i1 %c) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  br i1 %c, label %a, label %b
a:
  call void @safepoint()
  br label %join
b:
  br label %join
join:
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; CHECK-LABEL: @no_safepoint_on_any_path
; CHECK-NOT: julia.object_write_barrier
; CHECK: ret
define ptr addrspace(10) @no_safepoint_on_any_path(ptr addrspace(10) %child, i1 %c) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  br i1 %c, label %a, label %b
a:
  br label %join
b:
  br label %join
join:
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; The safepoint after the barrier precedes it in the next iteration.
; CHECK-LABEL: @safepoint_in_loop
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @safepoint_in_loop(ptr addrspace(10) %child, i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  br label %loop
loop:
  %i = phi i64 [ 0, %top ], [ %next, %loop ]
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  call void @safepoint()
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit, label %loop
exit:
  ret ptr addrspace(10) %obj
}

; An allocation in the loop is fresh again in every iteration.
; CHECK-LABEL: @allocation_in_loop
; CHECK-NOT: julia.object_write_barrier
; CHECK: ret
define void @allocation_in_loop(ptr addrspace(10) %child, i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  br label %loop
loop:
  %i = phi i64 [ 0, %top ], [ %next, %loop ]
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  call void @safepoint()
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit, label %loop
exit:
  ret void
}

; The phi may select an existing object, so the barrier stays.
; CHECK-LABEL: @phi_parent
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @phi_parent(ptr addrspace(10) %old, ptr addrspace(10) %child, i1 %c) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  br i1 %c, label %new, label %join
new:
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  br label %join
join:
  %parent = phi ptr addrspace(10) [ %obj, %new ], [ %old, %top ]
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %parent, ptr addrspace(10) %child)
  ret ptr addrspace(10) %parent
}

; GC transitions add safepoints, even around calls that only read memory.
; CHECK-LABEL: @gc_transition
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @gc_transition(ptr addrspace(10) %child, ptr %ptls) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  %y = call i64 @readonly_helper(i64 1) [ "gc-transition"(ptr %ptls) ]
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; CHECK-LABEL: @gc_transition_memcmp
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define ptr addrspace(10) @gc_transition_memcmp(ptr addrspace(10) %child, ptr %ptls, ptr %a, ptr %b) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  %c = call i32 @memcmp(ptr %a, ptr %b, i64 8) [ "gc-transition"(ptr %ptls) ]
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  ret ptr addrspace(10) %obj
}

; In unreachable code the barrier may come before the allocation.
; CHECK-LABEL: @unreachable
; CHECK: dead:
; CHECK: call void (ptr addrspace(10), ...) @julia.object_write_barrier(
define void @unreachable(ptr addrspace(10) %child) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  ret void
dead:
  call void (ptr addrspace(10), ...) @julia.object_write_barrier(ptr addrspace(10) %obj, ptr addrspace(10) %child)
  %obj = call ptr addrspace(10) @julia.gc_alloc_obj(ptr %pgcstack, i64 8, ptr addrspace(10) @tag)
  ret void
}
