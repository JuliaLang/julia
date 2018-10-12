; RUN: opt -load libjulia%shlibext -PropagateJuliaAddrspaces -dce -S %s | FileCheck %s

define i64 @simple() {
; CHECK-LABEL: @simple
; CHECK-NOT: addrspace(11)
    %stack = alloca i64
    %casted = addrspacecast i64 *%stack to i64 addrspace(11)*
    %loaded = load i64, i64 addrspace(11)* %casted
    ret i64 %loaded
}

define i64 @twogeps() {
; CHECK-LABEL: @twogeps
; CHECK-NOT: addrspace(11)
    %stack = alloca i64
    %casted = addrspacecast i64 *%stack to i64 addrspace(11)*
    %gep1 = getelementptr i64, i64 addrspace(11)* %casted, i64 1
    %gep2 = getelementptr i64, i64 addrspace(11)* %gep1, i64 1
    %loaded = load i64, i64 addrspace(11)* %gep2
    ret i64 %loaded
}

define i64 @phi(i1 %cond) {
; CHECK-LABEL: @phi
; CHECK-NOT: addrspace(11)
top:
    %stack1 = alloca i64
    %stack2 = alloca i64
    %stack1_casted = addrspacecast i64 *%stack1 to i64 addrspace(11)*
    %stack2_casted = addrspacecast i64 *%stack2 to i64 addrspace(11)*
    br i1 %cond, label %A, label %B
A:
    br label %B
B:
    %phi = phi i64 addrspace(11)* [ %stack1_casted, %top ], [ %stack2_casted, %A ]
    %load = load i64, i64 addrspace(11)* %phi
    ret i64 %load
}


define i64 @select(i1 %cond) {
; CHECK-LABEL: @select
; CHECK-NOT: addrspace(11)
top:
    %stack1 = alloca i64
    %stack2 = alloca i64
    %stack1_casted = addrspacecast i64 *%stack1 to i64 addrspace(11)*
    %stack2_casted = addrspacecast i64 *%stack2 to i64 addrspace(11)*
    %select = select i1 %cond, i64 addrspace(11)* %stack1_casted, i64 addrspace(11)* %stack2_casted
    %load = load i64, i64 addrspace(11)* %select
    ret i64 %load
}

define i64 @nullptr() {
; CHECK-LABEL: @nullptr
; CHECK-NOT: addrspace(11)
    %casted = addrspacecast i64 *null to i64 addrspace(11)*
    %load = load i64, i64 addrspace(11)* %casted
    ret i64 %load
}
