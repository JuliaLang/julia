; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

@tag = external addrspace(10) global {}, align 16

declare {}*** @julia.get_pgcstack()
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)
declare noalias nonnull {} addrspace(10)* @jl_alloc_genericmemory_unchecked({}*, i64, {} addrspace(10)*)
declare i64 @external_call({} addrspace(10)*)

; Test that julia.gc_alloc_ptr_offsets bundle causes null stores to be emitted
define {} addrspace(10)* @gc_alloc_ptr_offsets_single({}** %current_task) {
; CHECK-LABEL: @gc_alloc_ptr_offsets_single
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 8
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR]], align 8
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 24, {} addrspace(10)* @tag) [ "julia.gc_alloc_ptr_offsets"(i64 8) ]
    ret {} addrspace(10)* %v
}

; Test multiple pointer offsets
define {} addrspace(10)* @gc_alloc_ptr_offsets_multiple({}** %current_task) {
; CHECK-LABEL: @gc_alloc_ptr_offsets_multiple
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR0:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 0
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR0]], align 8
; CHECK: [[PTR8:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 8
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR8]], align 8
; CHECK: [[PTR16:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 16
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR16]], align 8
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 24, {} addrspace(10)* @tag) [ "julia.gc_alloc_ptr_offsets"(i64 0, i64 8, i64 16) ]
    ret {} addrspace(10)* %v
}

; Test julia.gc_alloc_zeroinit bundle causes memset to be emitted
define {} addrspace(10)* @gc_alloc_zeroinit({}** %current_task) {
; CHECK-LABEL: @gc_alloc_zeroinit
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 16
; CHECK: call void @llvm.memset.p11.i64(ptr addrspace(11) align 8 [[PTR]], i8 0, i64 32, i1 false)
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 48, {} addrspace(10)* @tag) [ "julia.gc_alloc_zeroinit"(i64 16, i64 32) ]
    ret {} addrspace(10)* %v
}

; Test that zeroing happens before any potential safepoint
define {} addrspace(10)* @gc_alloc_zeroing_before_safepoint({}** %current_task, {} addrspace(10)* %input) {
; CHECK-LABEL: @gc_alloc_zeroing_before_safepoint
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 8
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR]], align 8
; CHECK: call i64 @external_call
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 24, {} addrspace(10)* @tag) [ "julia.gc_alloc_ptr_offsets"(i64 8) ]
    ; This call could be a safepoint - the null store must happen before it
    %len = call i64 @external_call({} addrspace(10)* %input)
    ret {} addrspace(10)* %v
}

; Test zero-size zeroinit region is handled (no memset emitted)
define {} addrspace(10)* @gc_alloc_zeroinit_zero_size({}** %current_task) {
; CHECK-LABEL: @gc_alloc_zeroinit_zero_size
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK-NOT: call void @llvm.memset
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 16, {} addrspace(10)* @tag) [ "julia.gc_alloc_zeroinit"(i64 16, i64 0) ]
    ret {} addrspace(10)* %v
}

; Test allocation without bundles still works
define {} addrspace(10)* @gc_alloc_no_bundles({}** %current_task) {
; CHECK-LABEL: @gc_alloc_no_bundles
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; CHECK-NOT: store ptr addrspace(10) null
; CHECK-NOT: call void @llvm.memset
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 24, {} addrspace(10)* @tag)
    ret {} addrspace(10)* %v
}

; Test julia.gc_alloc_zeroinit_indirect bundle on jl_alloc_genericmemory_unchecked
; This loads the data pointer from offset 8 and zeros via that pointer
define {} addrspace(10)* @gc_alloc_zeroinit_indirect({}* %ptls, i64 %nbytes) {
; CHECK-LABEL: @gc_alloc_zeroinit_indirect
; CHECK: %v = call noalias ptr addrspace(10) @jl_alloc_genericmemory_unchecked
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR_FIELD:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 8
; CHECK: [[DATA_PTR:%.*]] = load ptr, ptr addrspace(11) [[PTR_FIELD]], align 8
; CHECK: call void @llvm.memset.p0.i64(ptr align 8 [[DATA_PTR]], i8 0, i64 %nbytes, i1 false)
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @jl_alloc_genericmemory_unchecked({}* %ptls, i64 %nbytes, {} addrspace(10)* @tag) [ "julia.gc_alloc_zeroinit_indirect"(i64 8, i64 %nbytes) ]
    ret {} addrspace(10)* %v
}

; Test zeroinit_indirect with constant size
define {} addrspace(10)* @gc_alloc_zeroinit_indirect_const({}* %ptls) {
; CHECK-LABEL: @gc_alloc_zeroinit_indirect_const
; CHECK: %v = call noalias ptr addrspace(10) @jl_alloc_genericmemory_unchecked
; CHECK: [[DERIVED:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR_FIELD:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED]], i64 8
; CHECK: [[DATA_PTR:%.*]] = load ptr, ptr addrspace(11) [[PTR_FIELD]], align 8
; CHECK: call void @llvm.memset.p0.i64(ptr align 8 [[DATA_PTR]], i8 0, i64 32, i1 false)
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @jl_alloc_genericmemory_unchecked({}* %ptls, i64 32, {} addrspace(10)* @tag) [ "julia.gc_alloc_zeroinit_indirect"(i64 8, i64 32) ]
    ret {} addrspace(10)* %v
}

; Test zeroinit_indirect with zero size (should not emit memset)
define {} addrspace(10)* @gc_alloc_zeroinit_indirect_zero_size({}* %ptls) {
; CHECK-LABEL: @gc_alloc_zeroinit_indirect_zero_size
; CHECK: %v = call noalias ptr addrspace(10) @jl_alloc_genericmemory_unchecked
; CHECK-NOT: call void @llvm.memset
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @jl_alloc_genericmemory_unchecked({}* %ptls, i64 0, {} addrspace(10)* @tag) [ "julia.gc_alloc_zeroinit_indirect"(i64 8, i64 0) ]
    ret {} addrspace(10)* %v
}
