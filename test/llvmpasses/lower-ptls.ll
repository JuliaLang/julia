; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='LowerPTLSPass<no-imaging>' -S %s | FileCheck %s --check-prefixes=CHECK,JIT
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='LowerPTLSPass<imaging>' -S %s | FileCheck %s --check-prefixes=CHECK,AOT

; JIT and AOT code is lowered the same way, except that an image defines its own slots
; while JIT code links to the runtime's.

target triple = "x86_64-unknown-linux-gnu"

; JIT: @jl_pgcstack_func_slot = external global ptr
; JIT: @jl_pgcstack_key_slot = external global i64
; JIT: @jl_image_tls_offset = external global i64
; AOT: @jl_pgcstack_func_slot = external hidden global ptr
; AOT: @jl_pgcstack_key_slot = external hidden global i64
; AOT: @jl_image_tls_offset = external hidden global i64

declare ptr @julia.get_pgcstack()

define ptr @getter() {
top:
; CHECK-LABEL: @getter
; CHECK: load ptr, ptr @jl_pgcstack_func_slot
  %pgcstack = call ptr @julia.get_pgcstack()
  ret ptr %pgcstack
}

; CHECK-NOT: julia.get_pgcstack
; CHECK-NOT: jl_get_pgcstack
