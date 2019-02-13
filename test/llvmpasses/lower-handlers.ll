; RUN: opt -load libjulia%shlibext -LowerExcHandlers -S %s | FileCheck %s

attributes #1 = { returns_twice }
declare i32 @julia.except_enter() #1
declare void @jl_pop_handler(i32)
declare i8**** @julia.ptls_states()

define void @simple() {
top:
    %ptls = call i8**** @julia.ptls_states()
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @jl_enter_handler
; CHECK: setjmp
    %r = call i32 @julia.except_enter()
    %cmp = icmp eq i32 %r, 0
    br i1 %cmp, label %try, label %catch
try:
    br label %after
catch:
    br label %after
after:
    call void @jl_pop_handler(i32 1)
; CHECK: llvm.lifetime.end
    ret void
}
