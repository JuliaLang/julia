; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame,FinalLowerGC,gvn)' -S %s | FileCheck %s

; Test for issue #59547: Ensure write barrier GC tag loads are volatile
; This test verifies that the LateLowerGCFrame pass marks GC tag loads as volatile
; to prevent GVN from incorrectly constant-folding them, which would eliminate
; necessary write barrier checks.

@tag = external addrspace(10) global {}, align 16

declare void @julia.write_barrier({} addrspace(10)*, {} addrspace(10)*)
declare {}*** @julia.get_pgcstack()
declare {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)

; Test that write barrier expansion produces volatile GC tag loads
; CHECK-LABEL: @test_writebarrier_volatile_tags
define {} addrspace(10)* @test_writebarrier_volatile_tags() {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %current_task = bitcast {}*** %pgcstack to {}**
  %parent = call {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  %child = call {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 8, {} addrspace(10)* @tag)
  call void @julia.write_barrier({} addrspace(10)* %parent, {} addrspace(10)* %child)
  ret {} addrspace(10)* %parent

; The critical test: GC tag loads must be volatile to prevent constant folding
; CHECK: load atomic volatile i64, ptr {{.*}} unordered, align 8, {{.*}}!tbaa
; CHECK: and i64 {{.*}}, 3
; CHECK: icmp eq i64 {{.*}}, 3
; CHECK: br i1 {{.*}}, label %may_trigger_wb, label

; CHECK: may_trigger_wb:
; CHECK: load atomic volatile i64, ptr {{.*}} unordered, align 8, {{.*}}!tbaa
; CHECK: and i64 {{.*}}, 1
; CHECK: icmp eq i64 {{.*}}, 0
; CHECK: br i1 {{.*}}, label %trigger_wb, label

; CHECK: trigger_wb:
; CHECK: call void @ijl_gc_queue_root(ptr {{.*}})
}
