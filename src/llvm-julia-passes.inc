//Module passes
#ifdef MODULE_PASS
MODULE_PASS("CPUFeatures", CPUFeaturesPass, CPUFeaturesPass())
MODULE_PASS("RemoveNI", RemoveNIPass, RemoveNIPass())
MODULE_PASS("JuliaMultiVersioning", MultiVersioningPass, MultiVersioningPass())
MODULE_PASS("RemoveJuliaAddrspaces", RemoveJuliaAddrspacesPass, RemoveJuliaAddrspacesPass())
MODULE_PASS("RemoveAddrspaces", RemoveAddrspacesPass, RemoveAddrspacesPass())
MODULE_PASS("LowerPTLSPass", LowerPTLSPass, LowerPTLSPass())
#endif

//Function passes
#ifdef FUNCTION_PASS
FUNCTION_PASS("DemoteFloat16", DemoteFloat16Pass, DemoteFloat16Pass())
FUNCTION_PASS("CombineMulAdd", CombineMulAddPass, CombineMulAddPass())
FUNCTION_PASS("LateLowerGCFrame", LateLowerGCPass, LateLowerGCPass())
FUNCTION_PASS("AllocOpt", AllocOptPass, AllocOptPass())
FUNCTION_PASS("PropagateJuliaAddrspaces", PropagateJuliaAddrspacesPass, PropagateJuliaAddrspacesPass())
FUNCTION_PASS("LowerExcHandlers", LowerExcHandlersPass, LowerExcHandlersPass())
FUNCTION_PASS("GCInvariantVerifier", GCInvariantVerifierPass, GCInvariantVerifierPass())
FUNCTION_PASS("FinalLowerGC", FinalLowerGCPass, FinalLowerGCPass())
#endif

//Loop passes
#ifdef LOOP_PASS
LOOP_PASS("JuliaLICM", JuliaLICMPass, JuliaLICMPass())
LOOP_PASS("LowerSIMDLoop", LowerSIMDLoopPass, LowerSIMDLoopPass())
#endif
