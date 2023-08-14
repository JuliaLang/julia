; COM: NewPM-only test, tests for ability to parse Julia passes

; RUN: opt --opaque-pointers=0 --load-pass-plugin=libjulia-codegen%shlibext -passes='module(CPUFeatures,RemoveNI,FinalLowerGC,JuliaMultiVersioning,RemoveJuliaAddrspaces,LowerPTLSPass,function(DemoteFloat16,CombineMulAdd,LateLowerGCFrame,AllocOpt,PropagateJuliaAddrspaces,LowerExcHandlers,GCInvariantVerifier,loop(LowerSIMDLoop,JuliaLICM),GCInvariantVerifier<strong>,GCInvariantVerifier<no-strong>),LowerPTLSPass<imaging>,LowerPTLSPass<no-imaging>,JuliaMultiVersioning<external>,JuliaMultiVersioning<no-external>)' -S %s -o /dev/null

define void @test() {
    ret void
}
