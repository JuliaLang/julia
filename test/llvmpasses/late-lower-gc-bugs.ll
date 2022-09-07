; RUN: opt -enable-new-pm=0 -load libjulia-codegen%shlibext -LateLowerGCFrame -S %s
; RUN: opt -enable-new-pm=1 --load-pass-plugin=libjulia-codegen%shlibext -passes='function(LateLowerGCFrame)' -S %s

; Old bugs (should be caught by assertions and death)

; This one tests that PHI nodes don't have duplicated bitcasts for the same BB
define {} addrspace(10)* @"julia__generic_matmatmul!_927"() {
top:
  %0 = call {}*** @julia.get_pgcstack()
  ret {} addrspace(10)* null

L3044.preheader:                                  ; No predecessors!
  %1 = addrspacecast i64 addrspace(13)* addrspace(10)* null to i64 addrspace(13)* addrspace(11)*
  ret {} addrspace(10)* null

L3292.preheader:                                  ; No predecessors!
  %2 = load i64 addrspace(13)*, i64 addrspace(13)* addrspace(11)* %1, align 8
  ret {} addrspace(10)* null

idxend704.us:                                     ; No predecessors!
  br label %L3350.us

idxend671.us:                                     ; No predecessors!
  %3 = getelementptr inbounds i64, i64 addrspace(13)* %2, i64 undef
  switch i32 0, label %L3332 [
    i32 1, label %L3350.us
    i32 0, label %L3350.us
  ]

L3350.us:                                         ; preds = %idxend671.us, %idxend671.us, %idxend704.us
  %value_phi672.us.in = phi i64 addrspace(13)* [ null, %idxend704.us ], [ %2, %idxend671.us ], [ %2, %idxend671.us ]
  ret {} addrspace(10)* null

L3332:                                            ; preds = %idxend671.us
  unreachable
}

declare {}*** @julia.get_pgcstack()

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #0

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smin.i64(i64, i64) #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.umin.i64(i64, i64) #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.smax.i64(i64, i64) #1

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare i64 @llvm.umax.i64(i64, i64) #1

; Function Attrs: argmemonly nofree nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #2

attributes #0 = { argmemonly nofree nounwind willreturn }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { argmemonly nofree nounwind willreturn writeonly }
