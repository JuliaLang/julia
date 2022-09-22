; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -LowerExcHandlers -print-before-all -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LowerExcHandlers)' -S %s | FileCheck %s

target triple = "amdgcn-amd-amdhsa"
target datalayout = "e-p:64:64-p1:64:64-p2:32:32-p3:32:32-p4:64:64-p5:32:32-p6:32:32-i64:64-v16:16-v24:32-v32:32-v48:64-v96:128-v192:256-v256:256-v512:512-v1024:1024-v2048:2048-n32:64-S32-A5-G1-ni:7-ni:10:11:12:13"

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
