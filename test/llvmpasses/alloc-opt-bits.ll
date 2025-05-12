; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(AllocOpt)' -S %s | FileCheck %s 


@tag = external addrspace(10) global {}

@glob = external addrspace(10) global {}

; Test that the gc_preserve intrinsics are deleted directly.

; CHECK-LABEL: @ptr_and_bits
; CHECK-NOT: alloca 
; CHECK: call noalias ptr addrspace(10) @julia.gc_alloc_obj

define void @ptr_and_bits(ptr %fptr, i1 %b, i1 %b2, i32 %idx) {
  %pgcstack = call ptr @julia.get_pgcstack()
  %ptls = call ptr @julia.ptls_states()
  %ptls_i8 = bitcast ptr %ptls to ptr
  %v = call noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr %ptls_i8, i64 16, ptr addrspace(10) @tag)
  
  %g0 = getelementptr { i64, ptr addrspace(10) }, ptr addrspace(10) %v, i32 %idx, i32 1
  store ptr addrspace(10) @glob, ptr addrspace(10) %g0
  
  %g1 = getelementptr { i64, ptr addrspace(10) }, ptr addrspace(10) %v, i32 %idx, i32 0
  store i64 7, ptr addrspace(10) %g1

  %res = load ptr addrspace(10), ptr addrspace(10) %g0
  %res2 = load i64, ptr addrspace(10) %g1
  ret void
}

declare noalias ptr addrspace(10) @julia.gc_alloc_obj(ptr, i64, ptr addrspace(10))

declare ptr @julia.ptls_states()

declare ptr @julia.get_pgcstack()
