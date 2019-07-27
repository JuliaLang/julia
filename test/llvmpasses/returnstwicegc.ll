; RUN: opt -load libjulia%shlibext -LateLowerGCFrame -FinalLowerGC -S %s | FileCheck %s

%jl_value_t = type opaque

declare void @boxed_simple(%jl_value_t addrspace(10)*, %jl_value_t addrspace(10)*)
declare %jl_value_t addrspace(10)* @jl_box_int64(i64)
declare %jl_value_t*** @julia.ptls_states()
declare i32 @sigsetjmp(i8*, i32) returns_twice
declare void @one_arg_boxed(%jl_value_t addrspace(10)*)

define void @try_catch(i64 %a, i64 %b)
{
; Because of the returns_twice function, we need to keep aboxed live everywhere
; CHECK: %gcframe = alloca %jl_value_t addrspace(10)*, i32 4
top:
    %sigframe = alloca [208 x i8], align 16
    %sigframe.sub = getelementptr inbounds [208 x i8], [208 x i8]* %sigframe, i64 0, i64 0
    call %jl_value_t*** @julia.ptls_states()
    %aboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 %a)
    %val = call i32 @sigsetjmp(i8 *%sigframe.sub, i32 0) returns_twice
    %cmp = icmp eq i32 %val, 0
    br i1 %cmp, label %zero, label %not
zero:
    %bboxed = call %jl_value_t addrspace(10)* @jl_box_int64(i64 %b)
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %bboxed)
    unreachable
not:
    call void @one_arg_boxed(%jl_value_t addrspace(10)* %aboxed)
    ret void
}
