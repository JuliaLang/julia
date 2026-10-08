; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

; `julia.gc_alloc_memory` is expanded into a single object with inline data if that fits a GC
; pool, and into a call to the runtime otherwise. Either way the header is initialized, and the
; data zeroed if requested, before anything else can happen.

@tag = external global {}

; CHECK-LABEL: @inline_zeroed
; CHECK: %mem = call noalias nonnull dereferenceable(48) ptr addrspace(10) @julia.gc_alloc_bytes(ptr %ptls_load, i64 48,
; CHECK: store atomic ptr {{.*}}@tag
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
; CHECK: [[OBJ:%.*]] = addrspacecast ptr addrspace(11) [[DERIVED]] to ptr
; CHECK: [[DATA:%.*]] = getelementptr inbounds i8, ptr [[OBJ]], i64 16
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 {{4|8}}
; CHECK: store ptr [[DATA]], ptr addrspace(11) [[PTR]]
; CHECK: call void @llvm.memset.p0.i64(ptr align {{4|8}} [[DATA]], i8 0, i64 32, i1 false)
; CHECK: [[LEN:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 0
; CHECK: store i64 4, ptr addrspace(11) [[LEN]]
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @inline_zeroed(ptr %task) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 32, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 4, i1 true)
  ret ptr addrspace(10) %mem
}

; CHECK-LABEL: @inline
; CHECK: %mem = call noalias nonnull dereferenceable(48) ptr addrspace(10) @julia.gc_alloc_bytes(ptr %ptls_load, i64 48,
; CHECK-NOT: memset
; CHECK: store i64 4,
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @inline(ptr %task) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 32, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 4, i1 false)
  ret ptr addrspace(10) %mem
}

; Zero-sized elements have no data to zero.
; CHECK-LABEL: @inline_empty
; CHECK: %mem = call noalias nonnull dereferenceable(16) ptr addrspace(10) @julia.gc_alloc_bytes(ptr %ptls_load, i64 16,
; CHECK-NOT: memset
; CHECK: store i64 4,
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @inline_empty(ptr %task) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 0, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 4, i1 true)
  ret ptr addrspace(10) %mem
}

; CHECK-LABEL: @runtime_zeroed
; CHECK: %mem = call noalias nonnull align 16 dereferenceable(16) ptr addrspace(10) @{{i?}}jl_alloc_genericmemory_unchecked(ptr %ptls_load, i64 %nbytes, ptr @tag)
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %mem to ptr addrspace(11)
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 {{4|8}}
; CHECK: [[DATA:%.*]] = load ptr, ptr addrspace(11) [[PTR]]
; CHECK: call void @llvm.memset.p0.i64(ptr align {{4|8}} [[DATA]], i8 0, i64 %nbytes, i1 false)
; CHECK: store i64 %n,
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @runtime_zeroed(ptr %task, i64 %nbytes, i64 %n) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 %nbytes, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 %n, i1 true)
  ret ptr addrspace(10) %mem
}

; CHECK-LABEL: @runtime_large
; CHECK: %mem = call {{.*}} @{{i?}}jl_alloc_genericmemory_unchecked(ptr %ptls_load, i64 4096, ptr @tag)
; CHECK-NOT: memset
; CHECK: store i64 512,
; CHECK: ret ptr addrspace(10) %mem
define ptr addrspace(10) @runtime_large(ptr %task) {
top:
  %pgcstack = call ptr @julia.get_pgcstack()
  %mem = call noalias nonnull align 16 ptr addrspace(10) @julia.gc_alloc_memory(ptr %task, i64 4096, ptr addrspace(10) addrspacecast (ptr @tag to ptr addrspace(10)), i64 512, i1 false)
  ret ptr addrspace(10) %mem
}

declare ptr @julia.get_pgcstack()
declare noalias nonnull ptr addrspace(10) @julia.gc_alloc_memory(ptr, i64, ptr addrspace(10), i64, i1) #0

attributes #0 = { allockind("alloc") memory(argmem: read, inaccessiblemem: readwrite) nounwind willreturn }
