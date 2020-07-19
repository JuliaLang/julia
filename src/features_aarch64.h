// This file is a part of Julia. License is MIT: https://julialang.org/license

// Copy values `from arch/arm64/include/uapi/asm/hwcap.h` from linux kernel source tree
// and match LLVM names.
// See also https://www.kernel.org/doc/html/latest/arm64/elf_hwcaps.html

// LLVM features in `llvm/lib/Target/AArch64/AArch64.td`

// AArch64 features definition
// hwcap
// JL_FEATURE_DEF(fp, 0, 0) // HWCAP_HP. Required
// JL_FEATURE_DEF(asimd, 1, 0) // HWCAP_ASIMD. Required
// JL_FEATURE_DEF(evtstrm, 2, 0) // HWCAP_EVTSTRM. Not needed
// JL_FEATURE_DEF(aes, 3, 0) // HWCAP_AES. Implied by `aes`
JL_FEATURE_DEF(aes, 4, 0) // HWCAP_PMULL, ID_AA64ISAR0_EL1.AES == 2
// JL_FEATURE_DEF(sha1, 5, UINT32_MAX) // HWCAP_SHA1. Implied by `sha2`
JL_FEATURE_DEF(sha2, 6, 0) // HWCAP_SHA2
JL_FEATURE_DEF(crc, 7, 0) // HWCAP_CRC32. Required in ARMv8.1
JL_FEATURE_DEF(lse, 8, 0) // HWCAP_ATOMICS, ARMv8.1-Atomics. Required in ARMv8.1
JL_FEATURE_DEF(fullfp16, 9, 0) // HWCAP_FPHP
// JL_FEATURE_DEF(asimdhp, 10, 0) // HWCAP_ASIMDHP. Same as `fullfp16`
// JL_FEATURE_DEF(cpuid, 11, 0) // HWCAP_CPUID. Not needed
JL_FEATURE_DEF(rdm, 12, 0) // HWCAP_ASIMDRDM, ARMv8.1-SIMD. Required in ARMv8.1
JL_FEATURE_DEF(jsconv, 13, 0) // HWCAP_JSCVT. Required in ARMv8.3
JL_FEATURE_DEF(complxnum, 14, 0) // HWCAP_FCMA. Required in ARMv8.3
JL_FEATURE_DEF(rcpc, 15, 0) // HWCAP_LRCPC, ARMv8.3-RCPC. Required in ARMv8.3
JL_FEATURE_DEF(ccpp, 16, 0) // HWCAP_DCPOP, ARMv8.2-DCPoP. Required in ARMv8.2
JL_FEATURE_DEF(sha3, 17, 0) // HWCAP_SHA3. ARMv8.2-SHA
// JL_FEATURE_DEF(sm3, 18, 0) // HWCAP_SM3. Same as `sm4`
JL_FEATURE_DEF(sm4, 19, 0) // HWCAP_SM4, ARMv8.2-SM
JL_FEATURE_DEF(dotprod, 20, 0) // HWCAP_ASIMDDP, ARMv8.2-DotProd
// JL_FEATURE_DEF(sha512, 21, UINT32_MAX) // HWCAP_SHA512. Not implement in LLVM yet
JL_FEATURE_DEF(sve, 22, 0) // HWCAP_SVE
JL_FEATURE_DEF(fp16fml, 23, 0) // HWCAP_ASIMDFHM, ARMv8.2-FHM
JL_FEATURE_DEF(dit, 24, 0) // HWCAP_DIT, ARMv8.4-DIT. Required in ARMv8.4
// JL_FEATURE_DEF(uscat, 25, UINT32_MAX) // HWCAP_USCAT, ARMv8.4-LSE
JL_FEATURE_DEF_NAME(rcpc_immo, 26, 0, "rcpc-immo") // HWCAP_ILRCPC, ARMv8.4-RCPC. Required in ARMv8.4
JL_FEATURE_DEF(fmi, 27, 0) // HWCAP_FLAGM, ARMv8.4-CondM. Requird in ARMv8.4
JL_FEATURE_DEF(ssbs, 28, 0) // HWCAP_SSBS
JL_FEATURE_DEF(sb, 29, 0) // HWCAP_SB. Required in ARMv8.5
JL_FEATURE_DEF(pa, 30, 0) // HWCAP_PACA
// JL_FEATURE_DEF(pa, 31, 0) // HWCAP_PACG. Merged with `pa`.

// hwcap2
JL_FEATURE_DEF(ccdp, 32 + 0, 0) // HWCAP2_DCPODP, ARMv8.2-DCCVADP. Required in ARMv8.5
JL_FEATURE_DEF(sve2, 32 + 1, 90000) // HWCAP2_SVE2
// JL_FEATURE_DEF_NAME(sve2_aes, 32 + 2, 90000, "sve2-aes") // HWCAP2_SVEAES, Implied by `sve2-aes`
JL_FEATURE_DEF_NAME(sve2_aes, 32 + 3, 90000, "sve2-aes") // HWCAP2_SVEPMULL, ID_AA64ZFR0_EL1.AES == 2
JL_FEATURE_DEF_NAME(sve2_bitperm, 32 + 4, 100000, "sve2-bitperm") // HWCAP2_SVEBITPERM
JL_FEATURE_DEF_NAME(sve2_sha3, 32 + 5, 90000, "sve2-sha3") // HWCAP2_SVESHA3
JL_FEATURE_DEF_NAME(sve2_sm4, 32 + 6, 90000, "sve2-sm4") // HWCAP2_SM4
JL_FEATURE_DEF(altnzcv, 32 + 7, 0) // HWCAP2_FLAGM2, ARMv8.5-CondM. Required in ARMv8.5
JL_FEATURE_DEF(fptoint, 32 + 8, 0) // HWCAP2_FRINT. Required in ARMv8.5
// JL_FEATURE_DEF(svei8mm, 32 + 9, UINT32_MAX) // HWCAP2_SVEI8MM, ARMv8.2-I8MM. Same as `i8mm`
JL_FEATURE_DEF(f32mm, 32 + 10, 110000) // HWCAP2_SVEF32MM, ARMv8.2-F32MM
JL_FEATURE_DEF(f64mm, 32 + 11, 110000) // HWCAP2_SVEF64MM, ARMv8.2-F64MM
// JL_FEATURE_DEF(svebf16, 32 + 12, UINT32_MAX) // HWCAP2_SVEBF16, ARMv8.2-BF16. Same as `bf16`
JL_FEATURE_DEF(i8mm, 32 + 13, 110000) // HWCAP2_I8MM, ARMv8.2-I8MM. Required in ARMv8.6
JL_FEATURE_DEF(bf16, 32 + 14, 110000) // HWCAP2_BF16, ARMv8.2-BF16. Required in ARMv8.6
// JL_FEATURE_DEF(dgh, 32 + 15, UINT32_MAX) // HWCAP2_DGH, ARMv8.0-DGH. Not implement in LLVM yet
JL_FEATURE_DEF(rand, 32 + 16, 0) // HWCAP2_RNG, ARMv8.5-RNG
JL_FEATURE_DEF(bti, 32 + 17, 0) // HWCAP2_BTI

// custom bits to match llvm model
JL_FEATURE_DEF(v8_1a, 32 * 2 + 0, 0)
JL_FEATURE_DEF(v8_2a, 32 * 2 + 1, 0)
JL_FEATURE_DEF(v8_3a, 32 * 2 + 2, 0)
JL_FEATURE_DEF(v8_4a, 32 * 2 + 3, 0)
JL_FEATURE_DEF(v8_5a, 32 * 2 + 4, 0)
JL_FEATURE_DEF(v8_6a, 32 * 2 + 5, 110000)

// Missing LLVM features available at EL0:
//     tme: ID_AA64ISAR0_EL1.TME (0b1) (LLVM 10)
//     am: ID_AA64PFR0_EL1.AMU (0b1, 0b10)
//     specrestrict: ID_AA64PFR0_EL1.CSV2 (0b10)
//     predres: ID_AA64PFR0_EL1.CSV3 (0b1)
//     mte: ID_AA64PFR1_EL1.MTE (0b1, 0b10)
//     ecv: ID_AA64MMFR0_EL1.ECV (0b1, 0b10) (LLVM 11)
//     lor: ID_AA64MMFR1_EL1.LO (0b1)
//     perfmon: ID_AA64DFR0_EL1.PMUVer (0b1, 0b100, 0b101, 0b110)
//     spe: ID_AA64DFR0_EL1.PMSVer (0b1 or 0b10)
//     tracev8.4: ID_AA64DFR0_EL1.TraceFilt (0b1)
//     ete: ???
