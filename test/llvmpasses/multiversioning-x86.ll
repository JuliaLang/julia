; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning,CPUFeatures' -S %s | FileCheck %s --allow-unused-prefixes=false --check-prefixes=CHECK,OPAQUE


; COM: This test checks that multiversioning actually happens from start to finish
; COM: We need the fvars for a proper test



; OPAQUE: @jl_gvar_ptrs = global [0 x ptr] zeroinitializer, align 8
; CHECK: @jl_fvar_idxs = hidden constant [5 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4], align 8
; CHECK: @jl_gvar_idxs = hidden constant [0 x i32] zeroinitializer, align 8
; OPAQUE: @simd_test.reloc_slot = hidden global ptr null
; OPAQUE: @jl_fvar_ptrs = hidden global [5 x ptr] [ptr @boring, ptr @fastmath_test, ptr @loop_test, ptr @simd_test, ptr @simd_test_call]
; OPAQUE: @jl_clone_slots = hidden constant [3 x i32] [i32 1, i32 3, i32 trunc (i64 sub (i64 ptrtoint (ptr @simd_test.reloc_slot to i64), i64 ptrtoint (ptr @jl_clone_slots to i64)) to i32)]
; CHECK: @jl_clone_idxs = hidden constant [10 x i32] [i32 -2147483647, i32 3, i32 -2147483647, i32 3, i32 4, i32 1, i32 1, i32 2, i32 -2147483645, i32 4]
; OPAQUE: @jl_clone_ptrs = hidden constant [9 x ptr] [ptr @boring.1, ptr @fastmath_test.1, ptr @loop_test.1, ptr @simd_test.1, ptr @simd_test_call.1, ptr @fastmath_test.2, ptr @loop_test.2, ptr @simd_test.2, ptr @simd_test_call.2]


target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128-ni:10:11:12:13"
target triple = "x86_64-linux-gnu"

@jl_fvars = global [5 x i64*] [i64* bitcast (i32 (i32)* @boring to i64*),
                               i64* bitcast (float (float, float)* @fastmath_test to i64*),
                               i64* bitcast (i32 (i32)* @loop_test to i64*),
                               i64* bitcast (i32 (<4 x i32>)* @simd_test to i64*),
                               i64* bitcast (i32 (<4 x i32>)* @simd_test_call to i64*)
                              ], align 8
@jl_gvar_ptrs = global [0 x i64*] zeroinitializer, align 8
@jl_fvar_idxs = hidden constant [5 x i32] [i32 0, i32 1, i32 2, i32 3, i32 4], align 8
@jl_gvar_idxs = hidden constant [0 x i32] zeroinitializer, align 8

declare i1 @julia.cpu.have_fma.f32()

; CHECK: @boring{{.*}}#[[BORING_BASE:[0-9]+]]
define noundef i32 @boring(i32 noundef %0) {
  ret i32 %0
}

; CHECK: @fastmath_test{{.*}}#[[NOT_BORING_BASE:[0-9]+]]
; CHECK: %3 = sitofp i1 false to float
define noundef float @fastmath_test(float noundef %0, float noundef %1) {
  %3 = call i1 @julia.cpu.have_fma.f32()
  %4 = sitofp i1 %3 to float
  %5 = fadd fast float %0, %4
  ret float %5
}

; CHECK: @loop_test{{.*}}#[[NOT_BORING_BASE:[0-9]+]]
define noundef i32 @loop_test(i32 noundef %0) {
  %2 = icmp sgt i32 %0, 0
  br i1 %2, label %5, label %3

3:                                                ; preds = %5, %1
  %4 = phi i32 [ 0, %1 ], [ %9, %5 ]
  ret i32 %4

5:                                                ; preds = %1, %5
  %6 = phi i32 [ %10, %5 ], [ 0, %1 ]
  %7 = phi i32 [ %9, %5 ], [ 0, %1 ]
  %8 = lshr i32 %6, 1
  %9 = add nuw nsw i32 %8, %7
  %10 = add nuw nsw i32 %6, 1
  %11 = icmp eq i32 %10, %0
  br i1 %11, label %3, label %5;, !llvm.loop -
}

; CHECK: @simd_test{{.*}}#[[SIMD_BASE_RELOC:[0-9]+]]
define noundef i32 @simd_test(<4 x i32> noundef %0) {
  %2 = extractelement <4 x i32> %0, i64 0
  ret i32 %2
}

; CHECK: @simd_test_call{{.*}}#[[NOT_BORING_BASE:[0-9]+]]
define noundef i32 @simd_test_call(<4 x i32> noundef %0) {
  %2 = call noundef i32 @simd_test(<4 x i32> noundef %0)
  ret i32 %2
}

; CHECK: @boring{{.*}}#[[BORING_CLONE:[0-9]+]]

; CHECK: @fastmath_test{{.*}}#[[NOT_BORING_CLONE1:[0-9]+]]
; CHECK: %3 = sitofp i1 false to float

; CHECK: @fastmath_test{{.*}}#[[NOT_BORING_CLONE2:[0-9]+]]
; CHECK: %3 = sitofp i1 true to float

; CHECK: @loop_test{{.*}}#[[NOT_BORING_CLONE1:[0-9]+]]

; CHECK: @loop_test{{.*}}#[[NOT_BORING_CLONE2:[0-9]+]]

; CHECK: @simd_test{{.*}}#[[SIMD_CLONE1:[0-9]+]]

; CHECK: @simd_test{{.*}}#[[SIMD_CLONE2:[0-9]+]]

; CHECK: @simd_test_call{{.*}}#[[NOT_BORING_CLONE1:[0-9]+]]
; OPAQUE: %2 = load ptr, ptr @simd_test.reloc_slot, align 8, !tbaa !8, !invariant.load !12
; CHECK: %3 = call noundef i32 %2(<4 x i32> noundef %0)

; CHECK: @simd_test_call{{.*}}#[[NOT_BORING_CLONE2:[0-9]+]]
; CHECK: %2 = call noundef i32 @simd_test.2(<4 x i32> noundef %0)

; CHECK-DAG: attributes #[[BORING_BASE]] = { "julia.mv.clone"="0" "julia.mv.clones"="2" "julia.mv.fvar" "target-cpu"="x86-64" "target-features"="+cx16,-sse3,-pclmul,-ssse3,-fma,-sse4.1,-sse4.2,-movbe,-popcnt,-aes,-xsave,-avx,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sahf,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[NOT_BORING_BASE]] = { "julia.mv.clone"="0" "julia.mv.clones"="6" "julia.mv.fvar" "target-cpu"="x86-64" "target-features"="+cx16,-sse3,-pclmul,-ssse3,-fma,-sse4.1,-sse4.2,-movbe,-popcnt,-aes,-xsave,-avx,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sahf,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[SIMD_BASE_RELOC]] = { "julia.mv.clone"="0" "julia.mv.clones"="6" "julia.mv.reloc" "target-cpu"="x86-64" "target-features"="+cx16,-sse3,-pclmul,-ssse3,-fma,-sse4.1,-sse4.2,-movbe,-popcnt,-aes,-xsave,-avx,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sahf,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[BORING_CLONE]] = { "julia.mv.clone"="1" "julia.mv.clones"="2" "julia.mv.fvar" "target-cpu"="sandybridge" "target-features"="+sahf,+avx,+xsave,+popcnt,+sse4.2,+sse4.1,+cx16,+ssse3,+pclmul,+sse3,-fma,-movbe,-aes,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[NOT_BORING_CLONE1]] = { "julia.mv.clone"="1" "julia.mv.clones"="6" "julia.mv.fvar" "target-cpu"="sandybridge" "target-features"="+sahf,+avx,+xsave,+popcnt,+sse4.2,+sse4.1,+cx16,+ssse3,+pclmul,+sse3,-fma,-movbe,-aes,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[NOT_BORING_CLONE2]] =  { "julia.mv.clone"="2" "julia.mv.clones"="6" "julia.mv.fvar" "target-cpu"="haswell" "target-features"="+lzcnt,+sahf,+bmi2,+avx2,+bmi,+fsgsbase,+f16c,+avx,+xsave,+popcnt,+movbe,+sse4.2,+sse4.1,+cx16,+fma,+ssse3,+pclmul,+sse3,-aes,-rdrnd,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[SIMD_CLONE1]] = { "julia.mv.clone"="1" "julia.mv.clones"="6" "julia.mv.reloc" "target-cpu"="sandybridge" "target-features"="+sahf,+avx,+xsave,+popcnt,+sse4.2,+sse4.1,+cx16,+ssse3,+pclmul,+sse3,-fma,-movbe,-aes,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }
; CHECK-DAG: attributes #[[SIMD_CLONE2]] = { "julia.mv.clone"="2" "julia.mv.clones"="6" "julia.mv.reloc" "target-cpu"="haswell" "target-features"="+lzcnt,+sahf,+bmi2,+avx2,+bmi,+fsgsbase,+f16c,+avx,+xsave,+popcnt,+movbe,+sse4.2,+sse4.1,+cx16,+fma,+ssse3,+pclmul,+sse3,-aes,-rdrnd,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8" }


!llvm.module.flags = !{!0, !2}


!0 = !{i32 1, !"julia.mv.enable", i32 1}
!1 = !{!1}
!2 = !{i32 1, !"julia.mv.specs", !3}
!3 = !{!4, !5, !6}
!4 = !{!"x86-64", !"+cx16,-sse3,-pclmul,-ssse3,-fma,-sse4.1,-sse4.2,-movbe,-popcnt,-aes,-xsave,-avx,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sahf,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8", i32 0, i32 0}
!5 = !{!"sandybridge", !"+sahf,+avx,+xsave,+popcnt,+sse4.2,+sse4.1,+cx16,+ssse3,+pclmul,+sse3,-fma,-movbe,-aes,-f16c,-rdrnd,-fsgsbase,-bmi,-avx2,-bmi2,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-lzcnt,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8", i32 0, i32 2}
!6 = !{!"haswell", !"+lzcnt,+sahf,+bmi2,+avx2,+bmi,+fsgsbase,+f16c,+avx,+xsave,+popcnt,+movbe,+sse4.2,+sse4.1,+cx16,+fma,+ssse3,+pclmul,+sse3,-aes,-rdrnd,-rtm,-avx512f,-avx512dq,-rdseed,-adx,-avx512ifma,-clflushopt,-clwb,-avx512cd,-sha,-avx512bw,-avx512vl,-avx512vbmi,-pku,-waitpkg,-avx512vbmi2,-shstk,-gfni,-vaes,-vpclmulqdq,-avx512vnni,-avx512bitalg,-avx512vpopcntdq,-rdpid,-cldemote,-movdiri,-movdir64b,-enqcmd,-uintr,-avx512vp2intersect,-serialize,-tsxldtrk,-pconfig,-amx-bf16,-avx512fp16,-amx-tile,-amx-int8,-sse4a,-prfchw,-xop,-fma4,-tbm,-mwaitx,-xsaveopt,-xsavec,-xsaves,-clzero,-wbnoinvd,-avxvnni,-avx512bf16,-ptwrite,+sse2,+mmx,+fxsr,+64bit,+cx8", i32 1, i32 284}
