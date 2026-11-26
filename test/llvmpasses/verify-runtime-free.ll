; This file is a part of Julia. License is MIT: https://julialang.org/license

; A function that codegen compiled as a runtime-free callback (see `@cfunction`), which has
; the attribute "julia.runtime_free", must not need the runtime once it is lowered.
; RUN: split-file %s %t
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/ok.ll | FileCheck %s --check-prefix=OK
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/alloc.ll 2>&1 | FileCheck %s --check-prefix=ALLOC
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/throw.ll 2>&1 | FileCheck %s --check-prefix=THROW
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/task.ll 2>&1 | FileCheck %s --check-prefix=TASK
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/pointer.ll 2>&1 | FileCheck %s --check-prefix=POINTER
; RUN: not --crash opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(VerifyRuntimeFree)' -S %t/other.ll 2>&1 | FileCheck %s --check-prefix=OTHER

;--- ok.ll
declare i32 @memcmp(ptr, ptr, i64)
declare i64 @llvm.ctpop.i64(i64)
declare void @ijl_throw(ptr)

; plain code, and calls of functions that are plain code as well
; OK-LABEL: @callback
define i32 @callback(ptr %p, i64 %n) #0 {
top:
  %c = call i32 @memcmp(ptr %p, ptr %p, i64 %n)
  %b = call i64 @llvm.ctpop.i64(i64 %n)
  %t = trunc i64 %b to i32
  %r = call i32 @helper(i32 %t)
  %s = add i32 %r, %c
  ret i32 %s
}

define internal i32 @helper(i32 %x) #0 {
top:
  %y = add i32 %x, 1
  ret i32 %y
}

; a function without the attribute is not checked
; OK-LABEL: @other
define void @other(ptr %e) {
top:
  call void @ijl_throw(ptr %e)
  unreachable
}

attributes #0 = { "julia.runtime_free" }

;--- alloc.ll
declare ptr @ijl_gc_small_alloc(ptr, i32, i32, i64)

; ALLOC: the runtime-free function `callback` needs the runtime once compiled: it allocates: it calls `ijl_gc_small_alloc`
define ptr @callback(ptr %ptls) #0 {
top:
  %v = call ptr @ijl_gc_small_alloc(ptr %ptls, i32 552, i32 16, i64 0)
  ret ptr %v
}

attributes #0 = { "julia.runtime_free" }

;--- throw.ll
declare void @ijl_throw(ptr)

; THROW: the runtime-free function `callback` needs the runtime once compiled: it can throw: it calls `ijl_throw`
define void @callback(ptr %e) #0 {
top:
  call void @ijl_throw(ptr %e)
  unreachable
}

attributes #0 = { "julia.runtime_free" }

;--- task.ll
; TASK: the runtime-free function `callback` needs the runtime once compiled: it uses the current task
define ptr @callback(ptr "gcstack" %pgcstack) #0 {
top:
  %ptls_field = getelementptr inbounds i8, ptr %pgcstack, i64 24
  %ptls = load ptr, ptr %ptls_field, align 8
  ret ptr %ptls
}

attributes #0 = { "julia.runtime_free" }

;--- pointer.ll
; POINTER: the runtime-free function `callback` needs the runtime once compiled: it calls a function pointer
define void @callback(ptr %f) #0 {
top:
  call void %f()
  ret void
}

attributes #0 = { "julia.runtime_free" }

;--- other.ll
; a function that is not known to be runtime-free
declare i32 @other(i32)

; OTHER: the runtime-free function `callback` needs the runtime once compiled: it calls `other`
define i32 @callback(i32 %x) #0 {
top:
  %y = call i32 @other(i32 %x)
  ret i32 %y
}

attributes #0 = { "julia.runtime_free" }
