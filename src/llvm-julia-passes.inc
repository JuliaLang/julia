//Module passes
#ifdef MODULE_PASS
MODULE_PASS("CPUFeatures", CPUFeatures())
MODULE_PASS("RemoveNI", RemoveNI())
MODULE_PASS("LowerSIMDLoop", LowerSIMDLoop())
MODULE_PASS("FinalLowerGC", FinalLowerGCPass())
MODULE_PASS("JuliaMultiVersioning", MultiVersioning())
MODULE_PASS("RemoveJuliaAddrspaces", RemoveJuliaAddrspacesPass())
MODULE_PASS("RemoveAddrspaces", RemoveAddrspacesPass())
MODULE_PASS("LowerPTLSPass", LowerPTLSPass())
#endif

//Function passes
#ifdef FUNCTION_PASS
FUNCTION_PASS("DemoteFloat16", DemoteFloat16())
FUNCTION_PASS("CombineMulAdd", CombineMulAdd())
FUNCTION_PASS("LateLowerGCFrame", LateLowerGC())
FUNCTION_PASS("AllocOpt", AllocOptPass())
FUNCTION_PASS("PropagateJuliaAddrspaces", PropagateJuliaAddrspacesPass())
FUNCTION_PASS("LowerExcHandlers", LowerExcHandlers())
FUNCTION_PASS("GCInvariantVerifier", GCInvariantVerifierPass())
#endif

//Loop passes
#ifdef LOOP_PASS
LOOP_PASS("JuliaLICM", JuliaLICMPass())
#endif
