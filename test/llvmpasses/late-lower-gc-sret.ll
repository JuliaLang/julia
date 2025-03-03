; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

declare ptr @julia.get_pgcstack()

declare swiftcc void @sret_call(ptr noalias nocapture noundef nonnull sret([3 x ptr addrspace(10)]), ptr nonnull swiftself, ptr addrspace(10) nonnull)

define hidden swiftcc nonnull ptr addrspace(10) @sret_select(ptr nonnull swiftself %0, ptr addrspace(10) noundef nonnull align 8 dereferenceable(88) %1, i1 %unpredictable) {
  ; CHECK-LABEL: @sret_select
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 6)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 3)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 0)
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 6)
  %pgcstack = call ptr @julia.get_pgcstack()
  %3 = alloca [3 x i64], align 8
  %4 = alloca [3 x i64], align 8
  %5 = select i1 %unpredictable, ptr %3, ptr %4
  call swiftcc void @sret_call(ptr noalias nocapture noundef nonnull sret([3 x ptr addrspace(10)]) %5, ptr nonnull swiftself %0, ptr addrspace(10) nonnull %1)
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret ptr addrspace(10) %1
}

define hidden swiftcc nonnull ptr addrspace(10) @sret_phi(ptr nonnull swiftself %0, ptr addrspace(10) noundef nonnull align 8 dereferenceable(88) %1, i1 %unpredictable) {
top:
  ; CHECK-LABEL: @sret_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 6)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 3)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 0)
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 6)
  %pgcstack = call ptr @julia.get_pgcstack()
  %2 = alloca [3 x i64], align 8
  %3 = alloca [3 x i64], align 8
  br i1 %unpredictable, label %true, label %false

true:                                             ; preds = %top
  br label %ret

false:                                            ; preds = %top
  br label %ret

ret:                                              ; preds = %false, %true
  %4 = phi ptr [ %2, %true ], [ %3, %false ]
  call swiftcc void @sret_call(ptr noalias nocapture noundef nonnull sret([3 x ptr addrspace(10)]) %4, ptr nonnull swiftself %0, ptr addrspace(10) nonnull %1)
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret ptr addrspace(10) %1
}

declare swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }), ptr noalias nocapture noundef, ptr nonnull swiftself)

define hidden swiftcc void @sret_gc_root_phi(ptr nonnull swiftself %0, i1 %unpredictable) {
top:
  ; CHECK-LABEL: @sret_gc_root_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 2)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 1)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 0)
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 2)
  ; CHECK: alloca [3 x i64], align 8
  %pgcstack = call ptr @julia.get_pgcstack()
  %1 = alloca [3 x i64], align 8
  %2 = alloca ptr addrspace(10), align 8
  %3 = alloca ptr addrspace(10), align 8
  store i64 0, ptr %2, align 8
  store i64 0, ptr %3, align 8
  br i1 %unpredictable, label %true, label %false

true:                                             ; preds = %top
  br label %ret

false:                                            ; preds = %top
  br label %ret

ret:                                              ; preds = %false, %true
  %4 = phi ptr [ %2, %true ], [ %3, %false ]
  call swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }) %1, ptr noalias nocapture noundef %4, ptr nonnull swiftself %0)
   ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}
