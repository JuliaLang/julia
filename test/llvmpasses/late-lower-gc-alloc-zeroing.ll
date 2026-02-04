; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

@tag = external addrspace(10) global {}, align 16

declare {}*** @julia.get_pgcstack()
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*)
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

; Test both bundles together
define {} addrspace(10)* @gc_alloc_both_bundles({}** %current_task) {
; CHECK-LABEL: @gc_alloc_both_bundles
; CHECK: %v = call noalias nonnull ptr addrspace(10) @julia.gc_alloc_bytes
; CHECK: store atomic ptr addrspace(10) @tag
; ptr_offsets bundle: null store at offset 8
; CHECK: [[DERIVED1:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR8:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED1]], i64 8
; CHECK: store ptr addrspace(10) null, ptr addrspace(11) [[PTR8]], align 8
; zeroinit bundle: memset at offset 24 for 16 bytes
; CHECK: [[DERIVED2:%.*]] = addrspacecast ptr addrspace(10) %v to ptr addrspace(11)
; CHECK: [[PTR24:%.*]] = getelementptr inbounds i8, ptr addrspace(11) [[DERIVED2]], i64 24
; CHECK: call void @llvm.memset.p11.i64(ptr addrspace(11) align 8 [[PTR24]], i8 0, i64 16, i1 false)
; CHECK: ret ptr addrspace(10) %v
    %v = call noalias {} addrspace(10)* @julia.gc_alloc_obj({}** %current_task, i64 48, {} addrspace(10)* @tag) [ "julia.gc_alloc_ptr_offsets"(i64 8), "julia.gc_alloc_zeroinit"(i64 24, i64 16) ]
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
