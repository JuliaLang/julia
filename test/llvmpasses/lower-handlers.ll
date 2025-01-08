; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LowerExcHandlers)' -S %s | FileCheck %s

attributes #1 = { returns_twice }
declare {i32, i8*} @julia.except_enter({}*) #1
declare void @ijl_pop_handler({}*, i32)
declare i8**** @julia.ptls_states()
declare i8**** @julia.get_pgcstack()

define void @simple() {
top:
    %pgcstack = call i8**** @julia.get_pgcstack()
; CHECK: call void @llvm.lifetime.start
; CHECK: call void @ijl_enter_handler
; CHECK: setjmp
    %rb = call {i32, i8*} @julia.except_enter({}* null)
    %r = extractvalue {i32, i8*} %rb, 0
    %b = extractvalue {i32, i8*} %rb, 1
    %cmp = icmp eq i32 %r, 0
    br i1 %cmp, label %try, label %catch
try:
    %lcssa = phi {i32, i8*} [ %rb, %top ]
    br label %after
catch:
    br label %after
after:
    call void @ijl_pop_handler({}* null, i32 1)
; CHECK: llvm.lifetime.end
    ret void
}
