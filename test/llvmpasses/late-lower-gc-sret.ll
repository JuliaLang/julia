; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%{shlibext} -passes='function(LateLowerGCFrame)' -S %s | FileCheck %s

declare ptr @julia.get_pgcstack()

declare swiftcc void @sret_call(ptr noalias nocapture noundef nonnull "julia.return_roots"="3" sret([3 x ptr addrspace(10)]), ptr nonnull swiftself, ptr addrspace(10) nonnull)

define hidden swiftcc nonnull ptr addrspace(10) @sret_select(ptr nonnull swiftself "gcstack" %0, ptr addrspace(10) noundef nonnull align 8 dereferenceable(88) %1, i1 %unpredictable) {
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
  call swiftcc void @sret_call(ptr noalias nocapture noundef nonnull "julia.return_roots"="3" sret([3 x ptr addrspace(10)]) %5, ptr nonnull swiftself "gcstack" %0, ptr addrspace(10) nonnull %1)
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret ptr addrspace(10) %1
}

define hidden swiftcc nonnull ptr addrspace(10) @sret_phi(ptr nonnull swiftself "gcstack" %0, ptr addrspace(10) noundef nonnull align 8 dereferenceable(88) %1, i1 %unpredictable) {
top:
  ; CHECK-LABEL: @sret_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 6)
  ; CHECK-NOT: @julia.get_gc_frame_slot
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 6)
  %pgcstack = call ptr @julia.get_pgcstack()
  %2 = alloca [3 x i64], align 8
  %3 = alloca [3 x i64], align 8
  br i1 %unpredictable, label %true, label %false

true:                                             ; preds = %top
  ; CHECK: true:
  ; CHECK-NEXT: [[SRET_PHI_TRUE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[03]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

false:                                            ; preds = %top
  ; CHECK: false:
  ; CHECK-NEXT: [[SRET_PHI_FALSE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[03]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

ret:                                              ; preds = %false, %true
  ; CHECK: ret:
  ; CHECK-NEXT: phi ptr [ [[SRET_PHI_TRUE]], %true ], [ [[SRET_PHI_FALSE]], %false ]
  %4 = phi ptr [ %2, %true ], [ %3, %false ]
  call swiftcc void @sret_call(ptr noalias nocapture noundef nonnull "julia.return_roots"="3" sret([3 x ptr addrspace(10)]) %4, ptr nonnull swiftself "gcstack" %0, ptr addrspace(10) nonnull %1)
  ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret ptr addrspace(10) %1
}

declare swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }), ptr noalias nocapture noundef "julia.return_roots"="1", ptr nonnull swiftself)

define hidden swiftcc void @sret_gc_root_phi(ptr nonnull swiftself "gcstack" %0, i1 %unpredictable) {
top:
  ; CHECK-LABEL: @sret_gc_root_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 2)
  ; CHECK-NOT: @julia.get_gc_frame_slot
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
  ; CHECK: true:
  ; CHECK-NEXT: [[ROOT_PHI_TRUE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[01]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

false:                                            ; preds = %top
  ; CHECK: false:
  ; CHECK-NEXT: [[ROOT_PHI_FALSE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[01]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

ret:                                              ; preds = %false, %true
  ; CHECK: ret:
  ; CHECK-NEXT: phi ptr [ [[ROOT_PHI_TRUE]], %true ], [ [[ROOT_PHI_FALSE]], %false ]
  %4 = phi ptr [ %2, %true ], [ %3, %false ]
  call swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }) %1, ptr noalias nocapture noundef "julia.return_roots"="1" %4, ptr nonnull swiftself "gcstack" %0)
   ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}


define hidden swiftcc void @sret_gc_root_phi_select(ptr nonnull swiftself "gcstack" %0, i1 %unpredictable, i1 %unpredictable2) {
top:
  ; CHECK-LABEL: @sret_gc_root_phi_select
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 3)
  ; CHECK-NOT: @julia.get_gc_frame_slot
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 3)
  ; CHECK: alloca [3 x i64], align 8
  %pgcstack = call ptr @julia.get_pgcstack()
  %1 = alloca [3 x i64], align 8
  %2 = alloca ptr addrspace(10), align 8
  %3 = alloca ptr addrspace(10), align 8
  %4 = alloca ptr addrspace(10), align 8
  store i64 0, ptr %2, align 8
  store i64 0, ptr %3, align 8
  store i64 0, ptr %4, align 8
  br i1 %unpredictable, label %true, label %false

true:                                             ; preds = %top
  ; CHECK: true:
  ; CHECK-NEXT: [[PHI_SEL_TRUE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

false:                                            ; preds = %top
  ; CHECK: false:
  ; CHECK-NEXT: [[PHI_SEL_FALSE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

ret:                                              ; preds = %false, %true
  ; CHECK: ret:
  ; CHECK-NEXT: [[PHI_SEL_PHI:%.*]] = phi ptr [ [[PHI_SEL_TRUE]], %true ], [ [[PHI_SEL_FALSE]], %false ]
  ; CHECK-NEXT: [[SRET_SEL_SLOT:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK-NEXT: select i1 %unpredictable2, ptr [[SRET_SEL_SLOT]], ptr [[PHI_SEL_PHI]]
  %5 = phi ptr [ %2, %true ], [ %3, %false ]
  %6 = select i1 %unpredictable2, ptr %4, ptr %5
  call swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }) %1, ptr noalias nocapture noundef "julia.return_roots"="1" %6, ptr nonnull swiftself "gcstack" %0)
   ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}

define hidden swiftcc void @sret_gc_root_select_phi(ptr nonnull swiftself "gcstack" %0, i1 %unpredictable, i1 %unpredictable2) {
top:
  ; CHECK-LABEL: @sret_gc_root_select_phi
  ; CHECK: %gcframe = call ptr @julia.new_gc_frame(i32 3)
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK: call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK-NOT: @julia.get_gc_frame_slot
  ; CHECK: %pgcstack = call ptr @julia.get_pgcstack()
  ; CHECK: call void @julia.push_gc_frame(ptr %gcframe, i32 3)
  ; CHECK: alloca [3 x i64], align 8
  %pgcstack = call ptr @julia.get_pgcstack()
  %1 = alloca [3 x i64], align 8
  %2 = alloca ptr addrspace(10), align 8
  %3 = alloca ptr addrspace(10), align 8
  %4 = alloca ptr addrspace(10), align 8
  store i64 0, ptr %2, align 8
  store i64 0, ptr %3, align 8
  store i64 0, ptr %4, align 8
  ; CHECK: [[SEL_PHI_SEL:%.*]] = select i1 %unpredictable2
  %5 = select i1 %unpredictable2, ptr %3, ptr %4
  br i1 %unpredictable, label %true, label %false

true:                                             ; preds = %top
  ; CHECK: true:
  ; CHECK-NEXT: [[SEL_PHI_TRUE:%.*]] = call ptr @julia.get_gc_frame_slot(ptr %gcframe, i32 {{[012]}})
  ; CHECK-NEXT: br label %ret
  br label %ret

false:                                            ; preds = %top
  ; CHECK: false:
  ; CHECK-NEXT: br label %ret
  br label %ret

ret:                                              ; preds = %false, %true
  ; CHECK: ret:
  ; CHECK-NEXT: phi ptr [ [[SEL_PHI_TRUE]], %true ], [ [[SEL_PHI_SEL]], %false ]
  %6 = phi ptr [ %2, %true ], [ %5, %false ]

  call swiftcc void @sret_call_gc(ptr noalias nocapture noundef sret({ ptr addrspace(10), i64, i64 }) %1, ptr noalias nocapture noundef "julia.return_roots"="1" %6, ptr nonnull swiftself "gcstack" %0)
   ; CHECK: call void @julia.pop_gc_frame(ptr %gcframe)
  ret void
}
