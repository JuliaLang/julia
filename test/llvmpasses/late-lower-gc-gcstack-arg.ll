; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame,FinalLowerGC),LowerPTLSPass,verify' -S %s | FileCheck %s --check-prefix=FINAL

; A function that receives its pgcstack as the "gcstack" argument can still end up with a
; call to julia.get_pgcstack in its entry block, e.g. when an llvmcall body is inlined into it.
; The GC frame must be pushed at function entry and not after that call, or the safepoints
; before it run without the roots of the function.

declare ptr @julia.get_pgcstack()
declare ptr addrspace(10) @allocate(ptr)
declare void @safepoint(ptr)
declare void @use(ptr, ptr addrspace(10))

define swiftcc ptr addrspace(10) @inlined_getter(ptr nonnull swiftself "gcstack" %pgcstack_arg) {
; CHECK-LABEL: @inlined_getter
; CHECK: %gcframe = call ptr @julia.new_gc_frame
; CHECK-NEXT: call void @julia.push_gc_frame(ptr %gcframe
; CHECK: call ptr addrspace(10) @allocate
; CHECK: call void @safepoint
; FINAL-LABEL: @inlined_getter
; FINAL: store ptr %gcframe, ptr %{{.*}}
; FINAL: call ptr addrspace(10) @allocate
; FINAL: call void @safepoint
; FINAL-NOT: julia.get_pgcstack
top:
  %v = call ptr addrspace(10) @allocate(ptr %pgcstack_arg)
  call void @safepoint(ptr %pgcstack_arg)
  %inlined_pgcstack = call ptr @julia.get_pgcstack()
  call void @use(ptr %inlined_pgcstack, ptr addrspace(10) %v)
  ret ptr addrspace(10) %v
}
