; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -LowerExcHandlers -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LowerExcHandlers)' -S %s | FileCheck %s

attributes #1 = { returns_twice }
declare i32 @julia.except_enter() #1
declare void @ijl_pop_handler(i32)
declare i8**** @julia.ptls_states()
declare i8**** @julia.get_pgcstack()

define void @simple() {
top:
    %pgcstack = call i8**** @julia.get_pgcstack()
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @ijl_enter_handler
; CHECK: setjmp
    %r = call i32 @julia.except_enter()
    %cmp = icmp eq i32 %r, 0
    br i1 %cmp, label %try, label %catch
try:
    br label %after
catch:
    br label %after
after:
    call void @ijl_pop_handler(i32 1)
; CHECK: llvm.lifetime.end
    ret void
}
