; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes=SoftTicks -S %s | FileCheck %s -DTICK='call void asm sideeffect' --check-prefixes=CHECK,X64,JIT
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='SoftTicks<imaging>' -S %s | FileCheck %s -DTICK='call void asm sideeffect' --check-prefixes=CHECK,X64,IMAGING,IMAGING-X64
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -mtriple=i686-unknown-linux-gnu -passes='SoftTicks<imaging>' -S %s | FileCheck %s -DTICK='call void asm sideeffect' --check-prefixes=CHECK,X86,IMAGING,IMAGING-X86
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -mtriple=aarch64-unknown-linux-gnu -passes='SoftTicks<imaging>' -S %s | FileCheck %s -DTICK='call void asm sideeffect' --check-prefixes=CHECK,ARM64,IMAGING,IMAGING-ARM64
; COM: a module is instrumented once
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='SoftTicks,SoftTicks<imaging>' -S %s | FileCheck %s -DTICK='call void asm sideeffect' --check-prefixes=CHECK,X64,JIT
; COM: targets that rr's software-ticks ABI does not define are left alone
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -mtriple=x86_64-apple-darwin -passes='SoftTicks<imaging>' -S %s | FileCheck %s --check-prefix=NONE
; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -mtriple=riscv64-unknown-linux-gnu -passes='SoftTicks<imaging>' -S %s | FileCheck %s --check-prefix=NONE

; NONE-NOT: 0x7000
; NONE-NOT: module asm
; NONE-NOT: rr.softticks

; COM: in imaging mode, the initializer that maps the countdown's page and the note, in one
; COM: COMDAT group
; JIT-NOT: module asm
; IMAGING: {{^}}module asm
; IMAGING-X64-NEXT: "\09.pushsection .text.__rr_softticks_init,\22axG\22,@progbits,__rr_softticks_init,comdat"
; IMAGING-X86-NEXT: "\09.pushsection .text.__rr_softticks_init,\22axG\22,@progbits,__rr_softticks_init,comdat"
; IMAGING-ARM64-NEXT: "\09.pushsection .text.__rr_softticks_init,\22axG\22,%progbits,__rr_softticks_init,comdat"
; IMAGING: "__rr_softticks_init:"
; IMAGING-X64: "\09movl $0x70001000, %edi"
; IMAGING-X64: "\09syscall"
; IMAGING-X86: "\09movl $0x70001000, %ebx"
; IMAGING-X86: "\09int $0x80"
; IMAGING-ARM64: "\09movk x0, #0x7001, lsl #16"
; IMAGING-ARM64: "\09svc #0"
; IMAGING: "\09.section .init_array.00001,\22awG\22,{{[@%]}}init_array,__rr_softticks_init,comdat"
; IMAGING-X64: "\09.quad __rr_softticks_init"
; IMAGING-X86: "\09.long __rr_softticks_init"
; IMAGING-ARM64: "\09.xword __rr_softticks_init"
; IMAGING: "\09.section .note.rrsoftticks,\22aG\22,{{[@%]}}note,__rr_softticks_init,comdat"
; IMAGING: "\09.asciz \22rr\22"
; IMAGING: "\09.popsection"
; IMAGING-NOT: __rr_softticks_init:

target triple = "x86_64-unknown-linux-gnu"

declare void @use(i64)
declare i32 @sigsetjmp(ptr, i32) returns_twice
declare i32 @returns_twice_unary(ptr) returns_twice
declare i32 @plain_unary(ptr)
declare i32 @__gxx_personality_v0(...)

; COM: the ABI's sequences, exactly
define void @sequence() {
; CHECK-LABEL: @sequence(
; X64-NEXT: call void asm sideeffect "testb $$0x80, 0x70001807\0A\09jnz 1f\0A\09decq 0x70001800\0A\09jnz 1f\0A\09int3\0A1:", "~{cc},~{dirflag},~{fpsr},~{flags}"() #[[NOUNWIND:[0-9]+]]
; X86-NEXT: call void asm sideeffect "testb $$0x80, 0x70001807\0A\09jnz 1f\0A\09subl $$1, 0x70001800\0A\09jnz 1f\0A\09subl $$1, 0x70001804\0A\09jae 1f\0A\09int3\0A1:", "~{cc},~{dirflag},~{fpsr},~{flags}"() #[[NOUNWIND:[0-9]+]]
; ARM64-NEXT: call void asm sideeffect "movz x16, #0x7001, lsl #16\0A\09ldr x17, [x16, #0x800]\0A\09tbnz x17, #63, 1f\0A\09sub x17, x17, #1\0A\09str x17, [x16, #0x800]\0A\09cbnz x17, 1f\0A\09brk #0x5354\0A1:\0A\09mov x17, xzr", "~{x16},~{x17}"() #[[NOUNWIND:[0-9]+]]
; CHECK-NEXT: ret void
  ret void
}

; COM: rule 1: a tick in the entry block, after its leading allocas; a block that is not on
; COM: a cycle does not tick
define void @straight(i64 %x, i1 %c) {
; CHECK-LABEL: @straight(
; CHECK-NEXT: top:
; CHECK-NEXT: %a = alloca
; CHECK-NEXT: %b = alloca
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: store i64 %x, ptr %a
; CHECK-NOT: [[TICK]]
; CHECK: ret void
top:
  %a = alloca i64
  %b = alloca i64
  store i64 %x, ptr %a
  br i1 %c, label %l, label %r
l:
  call void @use(i64 1)
  br label %join
r:
  call void @use(i64 2)
  br label %join
join:
  ret void
}

; COM: rule 2: the header of every loop, after its phis, once
define i64 @nested(i64 %n) {
; CHECK-LABEL: @nested(
; CHECK-NEXT: top:
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: br label %outer
; CHECK: outer:
; CHECK-NEXT: %i = phi
; CHECK-NEXT: %s = phi
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: br label %inner
; CHECK: inner:
; CHECK-NEXT: %j = phi
; CHECK-NEXT: %t = phi
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: %t1 = add
; CHECK-NOT: [[TICK]]
; CHECK: ret i64
top:
  br label %outer
outer:
  %i = phi i64 [0, %top], [%i1, %latch]
  %s = phi i64 [0, %top], [%t1, %latch]
  br label %inner
inner:
  %j = phi i64 [0, %outer], [%j1, %inner], [%j1, %inner2]
  %t = phi i64 [%s, %outer], [%t1, %inner], [%t1, %inner2]
  %t1 = add i64 %t, %j
  %j1 = add i64 %j, 1
  %ci = icmp slt i64 %j1, %n
  br i1 %ci, label %inner, label %inner2
inner2:
  %ci2 = icmp slt i64 %j1, 100
  br i1 %ci2, label %inner, label %latch
latch:
  %i1 = add i64 %i, 1
  %co = icmp slt i64 %i1, %n
  br i1 %co, label %outer, label %exit
exit:
  ret i64 %t1
}

; COM: an irreducible cycle (a <-> b, entered at both) ticks at the target of the walk's
; COM: retreating edge
define void @irreducible(i1 %c, i1 %d) {
; CHECK-LABEL: @irreducible(
; CHECK-NEXT: top:
; CHECK-NEXT: [[TICK]]
; CHECK: a:
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: call void @use(i64 1)
; CHECK: b:
; CHECK-NEXT: call void @use(i64 2)
; CHECK-NOT: [[TICK]]
; CHECK: ret void
top:
  br i1 %c, label %a, label %b
a:
  call void @use(i64 1)
  br label %b
b:
  call void @use(i64 2)
  br i1 %d, label %a, label %exit
exit:
  ret void
}

; COM: rule 3: right after a returns_twice call (the callee's or the call site's attribute)
define i32 @setjmp(ptr %buf) {
; CHECK-LABEL: @setjmp(
; CHECK-NEXT: top:
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: %r = call i32 @sigsetjmp(ptr %buf, i32 0)
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: %r2 = call i32 @plain_unary(ptr %buf) #[[RT:[0-9]+]]
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: %r3 = call i32 @plain_unary(ptr %buf)
; CHECK-NEXT: add
top:
  %r = call i32 @sigsetjmp(ptr %buf, i32 0)
  %r2 = call i32 @plain_unary(ptr %buf) returns_twice
  %r3 = call i32 @plain_unary(ptr %buf)
  %s = add i32 %r, %r2
  %s2 = add i32 %s, %r3
  ret i32 %s2
}

; COM: nothing may follow a musttail call
define i32 @musttail(ptr %buf) {
; CHECK-LABEL: @musttail(
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: %r = musttail call i32 @returns_twice_unary(ptr %buf)
; CHECK-NEXT: ret i32 %r
  %r = musttail call i32 @returns_twice_unary(ptr %buf)
  ret i32 %r
}

; COM: a returns_twice invoke ticks at its normal destination, split off if it has other
; COM: predecessors
define i32 @invoke(ptr %buf, i1 %c) personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: @invoke(
; CHECK-NEXT: top:
; CHECK-NEXT: [[TICK]]
; CHECK: invoke i32 @sigsetjmp(ptr %buf, i32 0)
; CHECK-NEXT: to label %[[SPLIT:.*]] unwind label %lp
; CHECK: [[SPLIT]]:
; CHECK-NEXT: [[TICK]]
; CHECK-NEXT: br label %join
; CHECK: join:
; CHECK-NOT: [[TICK]]
; CHECK: ret i32
top:
  br i1 %c, label %do, label %join
do:
  %r = invoke i32 @sigsetjmp(ptr %buf, i32 0)
          to label %join unwind label %lp
join:
  %v = phi i32 [0, %top], [%r, %do]
  ret i32 %v
lp:
  %l = landingpad { ptr, i32 } cleanup
  resume { ptr, i32 } %l
}

; COM: not instrumented: naked functions, disable_sanitizer_instrumentation, IFUNC resolvers
; COM: (which run before the initializer), the ABI's reserved sections, declarations and
; COM: available_externally functions
define void @naked() naked {
; CHECK-LABEL: @naked(
; CHECK-NEXT: call void asm sideeffect "ret", ""()
; CHECK-NEXT: unreachable
  call void asm sideeffect "ret", ""()
  unreachable
}

define void @nosan() disable_sanitizer_instrumentation {
; CHECK-LABEL: @nosan(
; CHECK-NEXT: ret void
  ret void
}

@ifunc = ifunc void (), ptr @resolver

define ptr @resolver() {
; CHECK-LABEL: @resolver(
; CHECK-NEXT: ret ptr @nosan
  ret ptr @nosan
}

define void @reserved() section ".text.__rr_softticks_stub" {
; CHECK-LABEL: @reserved(
; CHECK-NEXT: ret void
  ret void
}

define available_externally void @ae() {
; CHECK-LABEL: @ae(
; CHECK-NEXT: ret void
  ret void
}

; CHECK: attributes #[[NOUNWIND]] = { nounwind }
; CHECK: !{i32 7, !"rr.softticks", i32 1}
