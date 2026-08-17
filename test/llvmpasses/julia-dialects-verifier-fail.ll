; This file is a part of Julia. License is MIT: https://julialang.org/license

; Checks that the llvm-dialects verifier rejects ill-formed uses of the
; Julia dialect ops: julia.gc_loaded must return a Loaded (addrspace 13)
; pointer and take a Tracked (addrspace 10) base.
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaDialectsVerifier' -S %s -o /dev/null 2>&1 | FileCheck %s

declare ptr @julia.gc_loaded(ptr, ptr)

; CHECK: Verifier error
define void @bad(ptr %base, ptr %p) {
top:
  %loaded = call ptr @julia.gc_loaded(ptr %base, ptr %p)
  ret void
}
