; COM: NewPM-only test, tests for ability to parse Julia passes

; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes='module(CPUFeatures,RemoveNI,JuliaMultiVersioning,RemoveJuliaAddrspaces,LowerPTLSPass,function(DemoteFloat16,LateLowerGCFrame,FinalLowerGC,AllocOpt,PropagateJuliaAddrspaces,GCInvariantVerifier,loop(LowerSIMDLoop,JuliaLICM),GCInvariantVerifier<strong>,GCInvariantVerifier<no-strong>),LowerPTLSPass<imaging>,LowerPTLSPass<no-imaging>,JuliaMultiVersioning<external>,JuliaMultiVersioning<no-external>)' -S %s -o /dev/null
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes="julia<level=3;llvm_only>" -S %s -o /dev/null
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes="julia<level=3;no_llvm_only>" -S %s -o /dev/null
; RUN: opt --load-pass-plugin=libjulia-codegen%shlibext -passes="julia<level=3;no_enable_vector_pipeline>" -S %s -o /dev/null

define void @test() {
    ret void
}
