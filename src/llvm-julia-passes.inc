//Module passes
#ifdef MODULE_PASS
MODULE_PASS("CPUFeatures", CPUFeaturesPass())
MODULE_PASS("RemoveNI", RemoveNIPass())
MODULE_PASS("JuliaMultiVersioning", MultiVersioningPass())
MODULE_PASS("RemoveJuliaAddrspaces", RemoveJuliaAddrspacesPass())
MODULE_PASS("RemoveAddrspaces", RemoveAddrspacesPass())
MODULE_PASS("LowerPTLSPass", LowerPTLSPass())
#endif

//Function passes
#ifdef FUNCTION_PASS
FUNCTION_PASS("DemoteFloat16", DemoteFloat16Pass())
FUNCTION_PASS("LateLowerGCFrame", LateLowerGCPass())
FUNCTION_PASS("AllocOpt", AllocOptPass())
FUNCTION_PASS("PropagateJuliaAddrspaces", PropagateJuliaAddrspacesPass())
FUNCTION_PASS("GCInvariantVerifier", GCInvariantVerifierPass())
FUNCTION_PASS("FinalLowerGC", FinalLowerGCPass())
#endif

//Loop passes
#ifdef LOOP_PASS
LOOP_PASS("JuliaLICM", JuliaLICMPass())
LOOP_PASS("LowerSIMDLoop", LowerSIMDLoopPass())
#endif
