; RUN: opt -load libjulia.so -LateLowerGCFrame -S %s | FileCheck %s

; This file has test cases for IR only valid on LLVM 5.0

%jl_value_t = type opaque

declare void @llvm.lifetime.start.p0p10s_jl_value_ts(i64, %jl_value_t addrspace(10)**)
declare void @llvm.lifetime.end.p0p10s_jl_value_ts(i64, %jl_value_t addrspace(10)**)

declare %jl_value_t*** @jl_get_ptls_states()
declare %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)**, i32)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)

; Check that LLVM duplicating some blocks doesn't confuse the GC placement pass
define void @bb_duplication(i1 %cnd, i64 %a, i64 %b) {
; CHECK-LABEL: @bb_duplication
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %jlcall = alloca [2 x %jl_value_t addrspace(10)*], align 8
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %jlcall.sub = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 0
    %jlcall1 = getelementptr inbounds [2 x %jl_value_t addrspace(10)*], [2 x %jl_value_t addrspace(10)*]* %jlcall, i64 0, i64 1
    br label %loop
loop:
    br i1 %cnd, label %A, label %B
A:
    %Aaboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %Abboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    br i1 undef, label %loop, label %Abody

Abody:
    call void @llvm.lifetime.start.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall.sub)
    store %jl_value_t addrspace(10)* %Aaboxed, %jl_value_t addrspace(10)** %jlcall.sub
    store %jl_value_t addrspace(10)* %Abboxed, %jl_value_t addrspace(10)** %jlcall1
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall.sub, i32 2)
    call void @llvm.lifetime.end.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall.sub)
    br i1 undef, label %loop, label %out
B:
    %Baboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    %Bbboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %b)
    call void @llvm.lifetime.start.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall.sub)
    store %jl_value_t addrspace(10)* %Baboxed, %jl_value_t addrspace(10)** %jlcall.sub
    store %jl_value_t addrspace(10)* %Bbboxed, %jl_value_t addrspace(10)** %jlcall1
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall.sub, i32 2)
    call void @llvm.lifetime.end.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall.sub)
    br i1 undef, label %loop, label %out
out:
    ret void
}

; We can trivially sink single allocas into the gc frame, but we need to delete
; any lifetime markers
define void @single_alloca_lifetime(i64 %a) {
; CHECK-LABEL: @single_alloca_lifetime
; CHECK-NOT: call void @llvm.lifetime.start
    %ptls = call %jl_value_t*** @jl_get_ptls_states()
    %jlcall = alloca %jl_value_t addrspace(10)*
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 signext %a)
    call void @llvm.lifetime.start.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall)
    store %jl_value_t addrspace(10)* %aboxed, %jl_value_t addrspace(10)** %jlcall
    call %jl_value_t addrspace(10)* @jl_apply_generic(%jl_value_t addrspace(10)* null, %jl_value_t addrspace(10)** %jlcall, i32 1)
    call void @llvm.lifetime.end.p0p10s_jl_value_ts(i64 16, %jl_value_t addrspace(10)** %jlcall)
    ret void
}
