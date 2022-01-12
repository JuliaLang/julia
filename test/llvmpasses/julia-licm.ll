; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -JuliaLICM -S %s | FileCheck %s

@tag = external addrspace(10) global {}, align 16

; COM: This isn't an actual intrinsic, but it suffices for the test
declare {} addrspace(10)* @julia.alloc_array_1d({} addrspace(10)*, i64)

declare void @julia.write_barrier({}*, ...)

declare {}*** @julia.get_pgcstack()

define nonnull {} addrspace(10)* @julia_allocation_hoist(i64 signext %n) #0 {
top:
  %pgcstack = call {}*** @julia.get_pgcstack()
  %gt0 = icmp sgt i64 %n, 0
  br i1 %gt0, label %L4, label %L3

L3.loopexit:                                      ; preds = %L22
  %.lcssa = phi {} addrspace(10)* [ %alloc_obj, %L22 ]
  %.lcssa_array = phi {} addrspace(10)* [ %alloc_arr, %L22 ]
  br label %L3

L3:                                               ; preds = %L3.loopexit, %top
  %merge = phi {} addrspace(10)* [ addrspacecast ({}* inttoptr (i64 139952239804424 to {}*) to {} addrspace(10)*), %top ], [ %.lcssa, %L3.loopexit ]
  ret {} addrspace(10)* %merge

L4:                                               ; preds = %top
  %current_task112 = getelementptr inbounds {}**, {}*** %pgcstack, i64 -12
  %current_task1 = bitcast {}*** %current_task112 to {}**
  ; CHECK: %alloc_obj = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task1, i64 8, {} addrspace(10)* @tag)
  ; CHECK-NEXT: %alloc_arr = call noalias nonnull {} addrspace(10)* @julia.alloc_array_1d({} addrspace(10)* @tag, i64 1), !julia.array
  ; CHECK-NEXT: br label %L22
  br label %L22

L22:                                              ; preds = %L4, %L22
  %value_phi5 = phi i64 [ 1, %L4 ], [ %indvar, %L22 ]
  ; CHECK: %value_phi5 = phi i64 [ 1, %L4 ], [ %indvar, %L22 ]
  ; CHECK-NEXT: %bitcast_obj = bitcast {} addrspace(10)* %alloc_obj to i64 addrspace(10)*
  %alloc_obj = call noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}** nonnull %current_task1, i64 8, {} addrspace(10)* @tag) #2
  %bitcast_obj = bitcast {} addrspace(10)* %alloc_obj to i64 addrspace(10)*
  store i64 %value_phi5, i64 addrspace(10)* %bitcast_obj, align 8, !tbaa !2
  ; CHECK: store i64 %value_phi5, i64 addrspace(10)* %bitcast_obj, align 8
  ; CHECK-NEXT: %.not = icmp eq i64 %value_phi5, %n
  %alloc_arr = call noalias nonnull {} addrspace(10)* @julia.alloc_array_1d({} addrspace(10)* @tag, i64 1), !julia.array !8
  %.not = icmp eq i64 %value_phi5, %n
  %indvar = add i64 %value_phi5, 1
  br i1 %.not, label %L3.loopexit, label %L22
}

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @julia.gc_alloc_obj({}**, i64, {} addrspace(10)*) #2

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #3

; Function Attrs: inaccessiblemem_or_argmemonly
declare void @ijl_gc_queue_root({} addrspace(10)*) #4

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_pool_alloc(i8*, i32, i32) #2

; Function Attrs: allocsize(1)
declare noalias nonnull {} addrspace(10)* @ijl_gc_big_alloc(i8*, i64) #2

attributes #0 = { "probe-stack"="inline-asm" }
attributes #1 = { "probe-stack"="inline-asm" "thunk" }
attributes #2 = { allocsize(1) }
attributes #3 = { argmemonly nofree nosync nounwind willreturn }
attributes #4 = { inaccessiblemem_or_argmemonly }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 2, !"Dwarf Version", i32 4}
!1 = !{i32 2, !"Debug Info Version", i32 3}
!2 = !{!3, !3, i64 0}
!3 = !{!"jtbaa_mutab", !4, i64 0}
!4 = !{!"jtbaa_value", !5, i64 0}
!5 = !{!"jtbaa_data", !6, i64 0}
!6 = !{!"jtbaa", !7, i64 0}
!7 = !{!"jtbaa"}
!8 = !{!"allocation"}