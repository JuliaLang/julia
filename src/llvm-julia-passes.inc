//Module passes
#ifdef MODULE_PASS
MODULE_PASS("CPUFeatures", CPUFeaturesPass())
MODULE_PASS("RemoveNI", RemoveNIPass())
MODULE_PASS("LowerSIMDLoop", LowerSIMDLoopPass())
MODULE_PASS("FinalLowerGC", FinalLowerGCPass())
MODULE_PASS("JuliaMultiVersioning", MultiVersioningPass())
MODULE_PASS("RemoveJuliaAddrspaces", RemoveJuliaAddrspacesPass())
MODULE_PASS("RemoveAddrspaces", RemoveAddrspacesPass())
MODULE_PASS("LowerPTLSPass", LowerPTLSPass())
#endif

//Function passes
#ifdef FUNCTION_PASS
FUNCTION_PASS("DemoteFloat16", DemoteFloat16Pass())
FUNCTION_PASS("CombineMulAdd", CombineMulAddPass())
FUNCTION_PASS("LateLowerGCFrame", LateLowerGCPass())
FUNCTION_PASS("AllocOpt", AllocOptPass())
FUNCTION_PASS("PropagateJuliaAddrspaces", PropagateJuliaAddrspacesPass())
FUNCTION_PASS("LowerExcHandlers", LowerExcHandlersPass())
FUNCTION_PASS("GCInvariantVerifier", GCInvariantVerifierPass())
#endif

//Loop passes
#ifdef LOOP_PASS
LOOP_PASS("JuliaLICM", JuliaLICMPass())
#endif
