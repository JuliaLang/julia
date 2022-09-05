; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -FinalLowerGC -S %s | FileCheck %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='FinalLowerGC' -S %s | FileCheck %s

target triple = "amdgcn-amd-amdhsa"
target datalayout = "e-p:64:64-p1:64:64-p2:32:32-p3:32:32-p4:64:64-p5:32:32-p6:32:32-i64:64-v16:16-v24:32-v32:32-v48:64-v96:128-v192:256-v256:256-v512:512-v1024:1024-v2048:2048-n32:64-S32-A5-G1-ni:7-ni:10:11:12:13"

@tag = external addrspace(10) global {}

declare void @boxed_simple({} addrspace(10)*, {} addrspace(10)*)
declare {} addrspace(10)* @ijl_box_int64(i64)
declare {}*** @julia.ptls_states()
declare {}*** @julia.get_pgcstack()

declare noalias nonnull {} addrspace(10)** @julia.new_gc_frame(i32)
declare void @julia.push_gc_frame({} addrspace(10)**, i32)
declare {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)**, i32)
declare void @julia.pop_gc_frame({} addrspace(10)**)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_bytes(i8*, i64) #0

attributes #0 = { allocsize(1) }

define void @gc_frame_addrspace(i64 %a, i64 %b) {
top:
; CHECK-LABEL: @gc_frame_addrspace
; CHECK: %0 = alloca {} addrspace(10)*, i32 4, align 16, addrspace(5)
; CHECK: %gcframe = addrspacecast {} addrspace(10)* addrspace(5)* %0 to {} addrspace(10)**
; CHECK: %1 = bitcast {} addrspace(10)** %gcframe to i8*
  %gcframe = call {} addrspace(10)** @julia.new_gc_frame(i32 2)
  %pgcstack = call {}*** @julia.get_pgcstack()
  call void @julia.push_gc_frame({} addrspace(10)** %gcframe, i32 2)
  %aboxed = call {} addrspace(10)* @ijl_box_int64(i64 signext %a)
  %frame_slot_1 = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 1)
  store {} addrspace(10)* %aboxed, {} addrspace(10)** %frame_slot_1, align 8
  %bboxed = call {} addrspace(10)* @ijl_box_int64(i64 signext %b)
  %frame_slot_2 = call {} addrspace(10)** @julia.get_gc_frame_slot({} addrspace(10)** %gcframe, i32 0)
  store {} addrspace(10)* %bboxed, {} addrspace(10)** %frame_slot_2, align 8
  call void @boxed_simple({} addrspace(10)* %aboxed, {} addrspace(10)* %bboxed)
  call void @julia.pop_gc_frame({} addrspace(10)** %gcframe)
; CHECK: ret void
  ret void
}
