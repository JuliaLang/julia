//TODO clobber files when this changes in Makefile

//Module passes

#ifndef MODULE_PASS
#define MODULE_PASS(NAME, CREATE_PASS)
#endif

MODULE_PASS("CPUFeatures", CPUFeatures())
MODULE_PASS("RemoveNI", RemoveNI())
MODULE_PASS("LowerSIMDLoop", LowerSIMDLoop())
MODULE_PASS("FinalLowerGC", FinalLowerGCPass())
MODULE_PASS("JuliaMultiVersioning", MultiVersioning())
MODULE_PASS("RemoveJuliaAddrspaces", RemoveJuliaAddrspacesPass())
MODULE_PASS("RemoveAddrspaces", RemoveAddrspacesPass())
MODULE_PASS("LowerPTLSPass", LowerPTLSPass())

#ifndef FUNCTION_PASS
#define FUNCTION_PASS(NAME, CREATE_PASS)
#endif
//Function passes
FUNCTION_PASS("DemoteFloat16", DemoteFloat16())
FUNCTION_PASS("CombineMulAdd", CombineMulAdd())
FUNCTION_PASS("LateLowerGCFrame", LateLowerGC())
FUNCTION_PASS("AllocOpt", AllocOptPass())
FUNCTION_PASS("PropagateJuliaAddrspaces", PropagateJuliaAddrspacesPass())
FUNCTION_PASS("LowerExcHandlers", LowerExcHandlers())
FUNCTION_PASS("GCInvariantVerifier", GCInvariantVerifierPass())

#ifndef LOOP_PASS
#define LOOP_PASS(NAME, CREATE_PASS)
#endif
//Loop passes
LOOP_PASS("JuliaLICM", JuliaLICMPass())