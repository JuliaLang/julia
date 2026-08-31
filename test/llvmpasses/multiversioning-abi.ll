; This file is a part of Julia. License is MIT: https://julialang.org/license

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning' -S %s | FileCheck %s
; COM: the trampolines must also survive gc lowering, which aborts on a
; COM: `julia.return_roots` buffer it cannot trace back to a gc frame
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='JuliaMultiVersioning,function(LateLowerGCFrame)' -S %s -o /dev/null

; COM: `@ccallable` entry points are emitted as aliases, which multiversioning
; COM: rewrites into trampolines that forward to the reloc slot. The forwarded
; COM: call must keep the ABI-relevant parameter attributes, or arguments end
; COM: up in the wrong registers/stack slots (e.g. a byval struct handed over
; COM: as a plain pointer).

%struct.Large = type { double, double, double, double, double, double, double, double, double, double, double, double, [9 x double] }

@jl_fvars = global [3 x ptr] [ptr @takes_byval, ptr @returns_sret, ptr @returns_roots], align 8
@jl_gvar_base = hidden constant i64 zeroinitializer, align 8
@jl_gvar_offsets = hidden constant [0 x i32] zeroinitializer, align 8
@jl_fvar_idxs = hidden constant [3 x i32] [i32 0, i32 1, i32 2], align 8
@jl_gvar_idxs = hidden constant [0 x i32] zeroinitializer, align 8

@takes_byval_alias = alias void (i32, ptr, ptr), ptr @takes_byval
@returns_sret_alias = alias void (ptr, i32), ptr @returns_sret
@returns_roots_alias = alias void (ptr, ptr), ptr @returns_roots

define void @takes_byval(i32 signext %0, ptr byval(%struct.Large) align 8 %1, ptr %2) #0 {
    ret void
}

define void @returns_sret(ptr sret(%struct.Large) align 8 %0, i32 zeroext %1) #0 {
    ret void
}

define swiftcc void @returns_roots(ptr noalias nonnull sret([2 x ptr addrspace(10)]) align 8 "julia.return_roots"="2" %0, ptr nonnull swiftself "gcstack" %1) #0 {
    ret void
}

declare ptr @ijl_autoinit_and_adopt_thread()

; COM: the autoinit trampolines the reloc slots are initialized with forward
; COM: every argument unchanged
; CHECK: define hidden void @takes_byval.autoinit_trampoline(i32 signext %0, ptr byval(%struct.Large) align 8 %1, ptr %2)
; CHECK: call ptr @ijl_autoinit_and_adopt_thread()
; CHECK-NEXT: [[BYVAL_AUTOINIT_SLOT:%[0-9]+]] = load ptr, ptr @takes_byval.reloc_slot
; CHECK-NEXT: call void [[BYVAL_AUTOINIT_SLOT]](i32 signext %0, ptr byval(%struct.Large) align 8 %1, ptr %2)

; CHECK: define hidden void @returns_sret.autoinit_trampoline(ptr sret(%struct.Large) align 8 %0, i32 zeroext %1)
; CHECK: call ptr @ijl_autoinit_and_adopt_thread()
; CHECK-NEXT: [[SRET_AUTOINIT_SLOT:%[0-9]+]] = load ptr, ptr @returns_sret.reloc_slot
; CHECK-NEXT: call void [[SRET_AUTOINIT_SLOT]](ptr sret(%struct.Large) align 8 %0, i32 zeroext %1)

; COM: `julia.return_roots` is the caller's obligation towards its own gc frame,
; COM: not part of the ABI, so a trampoline passes the buffer on without it
; CHECK: define hidden swiftcc void @returns_roots.autoinit_trampoline(ptr noalias nonnull sret([2 x ptr addrspace(10)]) align 8 "julia.return_roots"="2" %0, ptr nonnull swiftself "gcstack" %1)
; CHECK: call ptr @ijl_autoinit_and_adopt_thread()
; CHECK-NEXT: [[ROOTS_AUTOINIT_SLOT:%[0-9]+]] = load ptr, ptr @returns_roots.reloc_slot
; CHECK-NEXT: call swiftcc void [[ROOTS_AUTOINIT_SLOT]](ptr noalias nonnull sret([2 x ptr addrspace(10)]) align 8 %0, ptr nonnull swiftself "gcstack" %1)

; COM: and so do the trampolines the aliases are rewritten into
; CHECK: define void @takes_byval_alias(i32 signext %0, ptr byval(%struct.Large) align 8 %1, ptr %2)
; CHECK-NEXT: top:
; CHECK-NEXT: [[BYVAL_SLOT:%[0-9]+]] = load ptr, ptr @takes_byval.reloc_slot
; CHECK-NEXT: call void [[BYVAL_SLOT]](i32 signext %0, ptr byval(%struct.Large) align 8 %1, ptr %2)

; CHECK: define void @returns_sret_alias(ptr sret(%struct.Large) align 8 %0, i32 zeroext %1)
; CHECK-NEXT: top:
; CHECK-NEXT: [[SRET_SLOT:%[0-9]+]] = load ptr, ptr @returns_sret.reloc_slot
; CHECK-NEXT: call void [[SRET_SLOT]](ptr sret(%struct.Large) align 8 %0, i32 zeroext %1)

; CHECK: define swiftcc void @returns_roots_alias(ptr noalias nonnull sret([2 x ptr addrspace(10)]) align 8 "julia.return_roots"="2" %0, ptr nonnull swiftself "gcstack" %1)
; CHECK-NEXT: top:
; CHECK-NEXT: [[ROOTS_SLOT:%[0-9]+]] = load ptr, ptr @returns_roots.reloc_slot
; CHECK-NEXT: call swiftcc void [[ROOTS_SLOT]](ptr noalias nonnull sret([2 x ptr addrspace(10)]) align 8 %0, ptr nonnull swiftself "gcstack" %1)

attributes #0 = {"julia.mv.clones"="2" "julia.mv.reloc"}

!llvm.module.flags = !{!0, !1, !2}

!0 = !{i32 1, !"julia.mv.enable", i32 1}
!1 = !{i32 1, !"julia.mv.annotated", i32 1}
!2 = !{i32 1, !"julia.mv.specs", !3}
!3 = !{!4, !5}
!4 = !{!"cpubase", !"-nofeatures", i32 0, i32 0}
!5 = !{!"cpucloneall", !"-cloneall", i32 0, i32 1}
