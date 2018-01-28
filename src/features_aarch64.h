// This file is a part of Julia. License is MIT: https://julialang.org/license

// AArch64 features definition
// hwcap
JL_FEATURE_DEF(crypto, 3, 0)
JL_FEATURE_DEF(crc, 7, 0)
JL_FEATURE_DEF(lse, 8, 40000) // ARMv8.1-Atomics
JL_FEATURE_DEF(fullfp16, 9, 0)
JL_FEATURE_DEF(rdm, 12, 50000) // ARMv8.1-SIMD
JL_FEATURE_DEF(jscvt, 13, UINT32_MAX) // Linux Kernel HWCAP name
JL_FEATURE_DEF(fcma, 14, UINT32_MAX) // Linux Kernel HWCAP name
JL_FEATURE_DEF(rcpc, 15, 60000)
JL_FEATURE_DEF(dcpop, 16, UINT32_MAX) // Linux Kernel HWCAP name
// JL_FEATURE_DEF(dotprod, ???, 60000) // ARMv8.2-DotProd
// JL_FEATURE_DEF(ras, ???, 0)
// JL_FEATURE_DEF(sve, ???, UINT32_MAX)

// hwcap2
// JL_FEATURE_DEF(?, 32 + ?, 0)

// custom bits to match llvm model
JL_FEATURE_DEF(v8_1a, 32 * 2 + 0, 0)
JL_FEATURE_DEF(v8_2a, 32 * 2 + 1, 0)
JL_FEATURE_DEF(v8_3a, 32 * 2 + 2, 60000)
// JL_FEATURE_DEF(v8_4a, 32 * 2 + 3, ???)
