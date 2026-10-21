; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(AllocOpt)' -S %s | FileCheck %s

; AllocOpt can remove `julia.gc_alloc_memory` or move it to the stack when its size is a
; constant that fits a GC pool. On the stack, its length is stored like the allocation would,
; loads of its data pointer are forwarded when nothing else may write it, and the data pointer
; is only stored if it may be read.

@tag = external global {}

; CHECK-LABEL: @stack_memory
; CHECK-NOT: @julia.gc_alloc_memory
; CHECK: call void @llvm.memset.p0.i64(ptr align 16 %mem, i8 0, i64 40, i1 false)
; CHECK: [[LEN:%.*]] = getelementptr inbounds i8, ptr %mem, i64 0
; CHECK: store i64 3, ptr [[LEN]]
; CHECK: [[DATA:%.*]] = getelementptr inbounds i8, ptr %mem, i64 16
; CHECK-NOT: store ptr
; CHECK: [[LOADED:%.*]] = addrspacecast ptr [[DATA]] to ptr addrspace(13)
; CHECK: getelementptr inbounds i8, ptr addrspace(13) [[LOADED]], i64 8
; CHECK-NOT: @julia.gc_alloc_memory
; CHECK-NOT: @julia.gc_loaded
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

; Codegen's store of the data pointer is removed too.
; CHECK-LABEL: @stack_memory_codegen
; CHECK: [[DATA:%.*]] = getelementptr inbounds i8, ptr %mem, i64 16
; CHECK-NOT: store ptr
; CHECK-NOT: @julia.pointer_from_objref
; CHECK: store double %x, ptr {{%.*}}
; CHECK-NOT: store ptr
; CHECK: ret double
define double @stack_memory_codegen(double %x) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %objref = call ptr @julia.pointer_from_objref(ptr addrspace(11) %mem_derived)
  %data = getelementptr inbounds i8, ptr %objref, i64 16
  store ptr %data, ptr addrspace(11) %ptr_field, align 8
  %elt = getelementptr inbounds i8, ptr %data, i64 8
  store double %x, ptr %elt, align 8
  %reload = load ptr, ptr addrspace(11) %ptr_field, align 8
  %elt2 = getelementptr inbounds i8, ptr %reload, i64 8
  %val = load double, ptr %elt2, align 8
  ret double %val
}

; When something else may read the header, the data pointer is stored.
; CHECK-LABEL: @stack_memory_header_read
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr %mem, i64 {{4|8}}
; CHECK: store ptr {{%.*}}, ptr [[PTR]]
; CHECK: call void @use(ptr
; CHECK: ret
define void @stack_memory_header_read() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %objref = call ptr @julia.pointer_from_objref(ptr addrspace(11) %mem_derived)
  %token = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %mem)
  call void @use(ptr %objref)
  call void @llvm.julia.gc_preserve_end(token %token)
  ret void
}

; Nothing is forwarded when a call may write the field, or a memset of unknown size, or a
; volatile store.
; CHECK-LABEL: @stack_memory_clobbered
; CHECK: store ptr {{%.*}}, ptr
; CHECK: call void @use(ptr
; CHECK: [[P:%.*]] = load ptr, ptr
; CHECK: icmp eq ptr [[P]], null
define i1 @stack_memory_clobbered() {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %objref = call ptr @julia.pointer_from_objref(ptr addrspace(11) %mem_derived)
  %token = call token (...) @llvm.julia.gc_preserve_begin(ptr addrspace(10) %mem)
  call void @use(ptr %objref)
  call void @llvm.julia.gc_preserve_end(token %token)
  %p = load ptr, ptr addrspace(11) %ptr_field, align 8
  %isnull = icmp eq ptr %p, null
  ret i1 %isnull
}

; CHECK-LABEL: @stack_memory_memset_unknown
; CHECK: store ptr {{%.*}}, ptr
; CHECK: call void @llvm.memset
; CHECK: [[P:%.*]] = load ptr, ptr
; CHECK: icmp eq ptr [[P]], null
define i1 @stack_memory_memset_unknown(i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  call void @llvm.memset.p11.i64(ptr addrspace(11) %ptr_field, i8 0, i64 %n, i1 false)
  %p = load ptr, ptr addrspace(11) %ptr_field, align 8
  %isnull = icmp eq ptr %p, null
  ret i1 %isnull
}

; CHECK-LABEL: @stack_memory_volatile
; CHECK: store volatile ptr
; CHECK: load ptr, ptr
; CHECK: ret
define double @stack_memory_volatile(double %x) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %task = getelementptr inbounds i8, ptr %pgcstack, i64 -152
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 24, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 3, i1 false)
  %mem_derived = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
  %ptr_field = getelementptr inbounds i8, ptr addrspace(11) %mem_derived, i64 8
  %objref = call ptr @julia.pointer_from_objref(ptr addrspace(11) %mem_derived)
  %data = getelementptr inbounds i8, ptr %objref, i64 16
  store volatile ptr %data, ptr addrspace(11) %ptr_field, align 8
  %reload = load ptr, ptr addrspace(11) %ptr_field, align 8
  %val = load double, ptr %reload, align 8
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

; An allocation that is only stored to is dead.
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

; Escaping allocations stay as they are, for LateLowerGCFrame to expand.
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

; Elements that are GC references would not be rooted on the stack, so this stays on the heap.
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

; Allocations that are too large for a GC pool, or of non-constant size, stay on the heap.
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
declare ptr @julia.pointer_from_objref(ptr addrspace(11)) #1
declare void @use(ptr)
declare void @llvm.memset.p11.i64(ptr addrspace(11), i8, i64, i1)
declare token @llvm.julia.gc_preserve_begin(...)
declare void @llvm.julia.gc_preserve_end(token)
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_memory(ptr, i64, ptr addrspace(10), i64, i1) #0
declare ptr addrspace(13) @julia.gc_loaded(ptr addrspace(10) nocapture readnone, ptr readnone) #1

attributes #0 = { allockind("alloc") memory(argmem: read, inaccessiblemem: readwrite) nounwind willreturn }
attributes #1 = { nounwind readnone speculatable willreturn }
