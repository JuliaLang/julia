; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(AllocOpt)' -S %s | FileCheck %s

; `julia.gc_alloc_memory` with a constant size whose data fits a GC pool is optimized like any
; other allocation. When moved to the stack, its header is initialized like the allocation would.

@tag = external global {}

; CHECK-LABEL: @stack_memory
; CHECK-NOT: @julia.gc_alloc_memory
; CHECK: call void @llvm.memset.p0.i64(ptr align 16 %mem, i8 0, i64 40, i1 false)
; CHECK: [[LEN:%.*]] = getelementptr inbounds i8, ptr %mem, i64 0
; CHECK: store i64 3, ptr [[LEN]]
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr %mem, i64 {{4|8}}
; CHECK: [[DATA:%.*]] = getelementptr inbounds i8, ptr %mem, i64 16
; CHECK: store ptr [[DATA]], ptr [[PTR]]
; CHECK-NOT: @julia.gc_alloc_memory
; CHECK: ret double
define double @stack_memory(double %x) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %data = load ptr, ptr addrspace(11) %ptr_field, align 8
  %loaded = call ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) %mem, ptr %data)
  %elt = getelementptr inbounds i8, ptr addrspace(13) %loaded, i64 8
  store double %x, ptr addrspace(13) %elt, align 8
  %val = load double, ptr addrspace(13) %elt, align 8
  ret double %val
}

; Data that needs zeroing is cleared every time the allocation would have executed.
; CHECK-LABEL: @stack_memory_loop
; CHECK: loop:
; CHECK: call void @llvm.memset.p0.i64(ptr align 16 %mem, i8 0, i64 40, i1 false)
; CHECK: store i64 3,
; CHECK: ret
define i8 @stack_memory_loop(i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  br label %loop
loop:
  %i = phi i64 [ 0, %top ], [ %next, %loop ]
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 true)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %data = load ptr, ptr addrspace(11) %ptr_field, align 8
  %selector = getelementptr inbounds i8, ptr %data, i64 %i
  %sel = load i8, ptr %selector, align 1
  store i8 1, ptr %selector, align 1
  %next = add i64 %i, 1
  %done = icmp eq i64 %next, %n
  br i1 %done, label %exit, label %loop
exit:
  ret i8 %sel
}

; Without loads nothing can observe the allocation.
; CHECK-LABEL: @dead_memory
; CHECK-NOT: @julia.gc_alloc_memory
; CHECK-NOT: alloca
; CHECK: ret void
define void @dead_memory() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  store i64 3, ptr addrspace(11) %mem_derived, align 8
  ret void
}

; Allocations that are not moved to the stack stay as they are, for LateLowerGCFrame to expand.
; CHECK-LABEL: @escaping_memory
; CHECK: %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @escaping_memory() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  ret ptr addrspace(10) %mem
}

; Elements that are GC references would not be rooted on the stack.
; CHECK-LABEL: @pointerful_memory
; CHECK: @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 true)
; CHECK: ret
define ptr addrspace(10) @pointerful_memory(ptr addrspace(10) %x) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 true)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %data = load ptr, ptr addrspace(11) %ptr_field, align 8
  %loaded = call ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) %mem, ptr %data)
  store ptr addrspace(10) %x, ptr addrspace(13) %loaded, align 8
  %val = load ptr addrspace(10), ptr addrspace(13) %loaded, align 8
  ret ptr addrspace(10) %val
}

; Data that doesn't fit a GC pool, or of unknown size, is not allocated inline.
; CHECK-LABEL: @too_large
; CHECK: @julia.gc_alloc_memory(ptr %task, i64 4096,
; CHECK: ret
define i8 @too_large() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 4096, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 4096, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %data = load ptr, ptr addrspace(11) %ptr_field, align 8
  %val = load i8, ptr %data, align 1
  ret i8 %val
}

; CHECK-LABEL: @unknown_size
; CHECK: @julia.gc_alloc_memory(ptr %task, i64 %n,
; CHECK: ret
define i8 @unknown_size(i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 %n, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 %n, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %data = load ptr, ptr addrspace(11) %ptr_field, align 8
  %val = load i8, ptr %data, align 1
  ret i8 %val
}

declare ptr @julia.get_pgcstack()
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_memory(ptr, i64, ptr addrspace(10), i64, i1) #0
declare ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) nocapture readnone, ptr readnone) #1

attributes #0 = { allockind("alloc") memory(argmem: read, inaccessiblemem: readwrite) nounwind willreturn }
attributes #1 = { nounwind readnone speculatable willreturn }
