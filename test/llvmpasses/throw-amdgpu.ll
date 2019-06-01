; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt -load libjulia%shlibext -mtriple=amdgcn-amd-amdhsa -mcpu=gfx900 -AllocOpt -LateLowerGCFrame -S %s | FileCheck %s

%jl_value_t = type opaque
declare void @roc_report_exception(i64)
declare %jl_value_t*** @julia.ptls_states()
declare %jl_value_t addrspace(100)* @julia.gc_alloc_obj(i8*, i64, %jl_value_t addrspace(100)*)
declare void @llvm.trap()
@exception = external global [10 x i8]

; CHECK-LABEL: @throw_func
define void @throw_func(i8) {
top:
  %1 = call %jl_value_t*** @julia.ptls_states()
  %2 = bitcast %jl_value_t*** %1 to %jl_value_t addrspace(100)**
  %3 = getelementptr inbounds %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)** %2, i64 4
  %4 = bitcast %jl_value_t addrspace(100)** %3 to i64**
  %5 = load i64*, i64** %4
  %6 = trunc i8 %0 to i1
  %7 = xor i1 %6, true
  br i1 %7, label %L5, label %L2

L2:                                               ; preds = %top
  %8 = bitcast %jl_value_t*** %1 to i8*
  %9 = call noalias nonnull %jl_value_t addrspace(100)* @julia.gc_alloc_obj(i8* %8, i64 8, %jl_value_t addrspace(100)* addrspacecast (%jl_value_t* inttoptr (i64 140281676247392 to %jl_value_t*) to %jl_value_t addrspace(100)*)) #0
  %10 = addrspacecast %jl_value_t addrspace(100)* %9 to %jl_value_t addrspace(101)*
  %11 = bitcast %jl_value_t addrspace(101)* %10 to %jl_value_t addrspace(100)* addrspace(101)*
  %12 = getelementptr inbounds %jl_value_t addrspace(100)*, %jl_value_t addrspace(100)* addrspace(101)* %11, i64 0
  store %jl_value_t addrspace(100)* null, %jl_value_t addrspace(100)* addrspace(101)* %12
  %13 = bitcast %jl_value_t addrspace(100)* %9 to %jl_value_t addrspace(100)* addrspace(100)*
  store %jl_value_t addrspace(100)* addrspacecast (%jl_value_t* inttoptr (i64 140281609886928 to %jl_value_t*) to %jl_value_t addrspace(100)*), %jl_value_t addrspace(100)* addrspace(100)* %13
  call void @roc_report_exception(i64 ptrtoint ([10 x i8]* @exception to i64))
  call void @llvm.trap()
  unreachable

L5:                                               ; preds = %top
  ret void

after_throw:                                      ; No predecessors!
  call void @llvm.trap()
  unreachable

after_noret:                                      ; No predecessors!
  unreachable
}
