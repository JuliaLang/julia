; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

; Test that stores of GC-tracked values through select/phi of alloca pointers
; are properly rooted. This is a regression test for
; https://github.com/JuliaLang/julia/issues/60985

declare ptr @julia.get_pgcstack()

declare void @safepoint() "julia.safepoint"

; Store through a select of two allocas
define void @store_select(ptr addrspace(10) %val, i1 %cond) {
  ; CHECK-LABEL: @store_select
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 4)
  %pgcstack = call ptr @julia.get_pgcstack()
  %alloca1 = alloca [2 x ptr addrspace(10)], align 8
  %alloca2 = alloca [2 x ptr addrspace(10)], align 8
  %sel = select i1 %cond, ptr %alloca1, ptr %alloca2
  store ptr addrspace(10) %val, ptr %sel, align 8
  call void @safepoint()
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}

; Store through a phi of two allocas
define void @store_phi(ptr addrspace(10) %val, i1 %cond) {
top:
  ; CHECK-LABEL: @store_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 4)
  %pgcstack = call ptr @julia.get_pgcstack()
  %alloca1 = alloca [2 x ptr addrspace(10)], align 8
  %alloca2 = alloca [2 x ptr addrspace(10)], align 8
  br i1 %cond, label %left, label %right

left:
  br label %merge

right:
  br label %merge

merge:
  %phi = phi ptr [ %alloca1, %left ], [ %alloca2, %right ]
  store ptr addrspace(10) %val, ptr %phi, align 8
  call void @safepoint()
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}
