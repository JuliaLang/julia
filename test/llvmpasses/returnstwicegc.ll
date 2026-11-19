; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame,FinalLowerGC)' -S %s | FileCheck %s


declare void @boxed_simple({} addrspace(10)*, {} addrspace(10)*)
declare {} addrspace(10)* @jl_box_int64(i64)
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()
declare i32 @sigsetjmp(i8*, i32) returns_twice
declare void @one_arg_boxed({} addrspace(10)*)

define void @try_catch(i64 %a, i64 %b)
{
; Because of the returns_twice function, we need to keep aboxed live everywhere
; CHECK: %gcframe = alloca ptr addrspace(10), i32 4
; CHECK:  store ptr addrspace(10) %aboxed, ptr [[slot_0:%.*]],
; CHECK-NOT:  store {{.*}} ptr [[slot_0]]
; CHECK:  store ptr addrspace(10) %bboxed, ptr {{%.*}}
; CHECK-NOT:  store {{.*}} ptr [[slot_0]]

top:
    %sigframe = alloca [208 x i8], align 16
    %sigframe.sub = getelementptr inbounds [208 x i8], [208 x i8]* %sigframe, i64 0, i64 0
    call {}*** @julia.get_pgcstack()
    call {}*** @julia.ptls_states()
    %aboxed = call {} addrspace(10)* @jl_box_int64(i64 %a)
    %val = call i32 @sigsetjmp(i8 *%sigframe.sub, i32 0) returns_twice
    %cmp = icmp eq i32 %val, 0
    br i1 %cmp, label %zero, label %not
zero:
    %bboxed = call {} addrspace(10)* @jl_box_int64(i64 %b)
    call void @one_arg_boxed({} addrspace(10)* %bboxed)
    unreachable
not:
    call void @one_arg_boxed({} addrspace(10)* %aboxed)
    ret void
}

declare void @llvm.lifetime.start.p0(ptr)
declare void @llvm.lifetime.end.p0(ptr)

define void @try_region(i64 %a, i64 %b) {
; CHECK-LABEL: @try_region
; CHECK: [[A:%gc_slot_addr_[0-9]+]] = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 [[ASLOT:[0-9]+]]
; CHECK-NEXT: store ptr addrspace(10) %aboxed, ptr [[A]]
; CHECK-NEXT: call i32 @sigsetjmp
; The catch reads aboxed, so no other value takes its slot until the jmp_buf dies
; CHECK: try:
; CHECK-NOT: ptr %gcframe, i32 [[ASLOT]]
; CHECK: call void @llvm.lifetime.end.p0(ptr %buf)
; and the slot is reset there, as aboxed is dead on this path
; CHECK-NEXT: getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 [[ASLOT]]
; CHECK-NEXT: store ptr addrspace(10) null
; CHECK: catch:
; CHECK-NEXT: call void @llvm.lifetime.end.p0(ptr %buf)
; CHECK-NEXT: call void @one_arg_boxed(ptr addrspace(10) %aboxed)
top:
    %buf = alloca [208 x i8], align 16
    %pgcstack = call ptr @julia.get_pgcstack()
    %aboxed = call ptr addrspace(10) @jl_box_int64(i64 %a)
    call void @llvm.lifetime.start.p0(ptr %buf)
    %val = call i32 @sigsetjmp(ptr %buf, i32 0) returns_twice
    %cmp = icmp eq i32 %val, 0
    br i1 %cmp, label %try, label %catch
try:
    %bboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
    call void @one_arg_boxed(ptr addrspace(10) %bboxed)
    call void @llvm.lifetime.end.p0(ptr %buf)
    br label %done
catch:
    call void @llvm.lifetime.end.p0(ptr %buf)
    call void @one_arg_boxed(ptr addrspace(10) %aboxed)
    br label %done
done:
    %cboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
    call void @one_arg_boxed(ptr addrspace(10) %cboxed)
    ret void
}

define void @nested_try_region(i64 %a, i64 %b) {
; CHECK-LABEL: @nested_try_region
; CHECK: [[A:%gc_slot_addr_[0-9]+]] = getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 [[ASLOT:[0-9]+]]
; CHECK-NEXT: store ptr addrspace(10) %aboxed, ptr [[A]]
; CHECK-NEXT: call i32 @sigsetjmp(ptr %outer
; The outer catch reads aboxed, so the end of the inner region keeps its slot
; CHECK: inner_try:
; CHECK-NOT: ptr %gcframe, i32 [[ASLOT]]
; CHECK: call void @llvm.lifetime.end.p0(ptr %outer)
; CHECK-NEXT: getelementptr inbounds ptr addrspace(10), ptr %gcframe, i32 [[ASLOT]]
; CHECK-NEXT: store ptr addrspace(10) null
; CHECK: outer_catch:
; CHECK-NEXT: call void @llvm.lifetime.end.p0(ptr %outer)
; CHECK-NEXT: call void @one_arg_boxed(ptr addrspace(10) %aboxed)
top:
    %outer = alloca [208 x i8], align 16
    %inner = alloca [208 x i8], align 16
    %pgcstack = call ptr @julia.get_pgcstack()
    %aboxed = call ptr addrspace(10) @jl_box_int64(i64 %a)
    call void @llvm.lifetime.start.p0(ptr %outer)
    %val = call i32 @sigsetjmp(ptr %outer, i32 0) returns_twice
    %cmp = icmp eq i32 %val, 0
    br i1 %cmp, label %outer_try, label %outer_catch
outer_try:
    call void @llvm.lifetime.start.p0(ptr %inner)
    %ival = call i32 @sigsetjmp(ptr %inner, i32 0) returns_twice
    %icmp = icmp eq i32 %ival, 0
    br i1 %icmp, label %inner_try, label %inner_catch
inner_try:
    %bboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
    call void @one_arg_boxed(ptr addrspace(10) %bboxed)
    call void @llvm.lifetime.end.p0(ptr %inner)
    %cboxed = call ptr addrspace(10) @jl_box_int64(i64 %b)
    call void @one_arg_boxed(ptr addrspace(10) %cboxed)
    call void @llvm.lifetime.end.p0(ptr %outer)
    br label %done
inner_catch:
    call void @llvm.lifetime.end.p0(ptr %inner)
    call void @one_arg_boxed(ptr addrspace(10) %aboxed)
    call void @llvm.lifetime.end.p0(ptr %outer)
    br label %done
outer_catch:
    call void @llvm.lifetime.end.p0(ptr %outer)
    call void @one_arg_boxed(ptr addrspace(10) %aboxed)
    br label %done
done:
    ret void
}
