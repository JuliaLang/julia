// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifdef _CPU_X86_
#define JL_X86_64ONLY_VER(x) UINT32_MAX
#else
#define JL_X86_64ONLY_VER(x) x
#endif

// X86 features definition
// EAX=1: ECX
JL_FEATURE_DEF(sse3, 0, 0)
JL_FEATURE_DEF(pclmul, 1, 0)
JL_FEATURE_DEF(ssse3, 9, 0)
JL_FEATURE_DEF(fma, 12, 0)
JL_FEATURE_DEF(cx16, 13, JL_X86_64ONLY_VER(0)) // cx16 requires 64bit
JL_FEATURE_DEF_NAME(sse41, 19, 0, "sse4.1")
JL_FEATURE_DEF_NAME(sse42, 20, 0, "sse4.2")
JL_FEATURE_DEF(movbe, 22, 0)
JL_FEATURE_DEF(popcnt, 23, 0)
JL_FEATURE_DEF(aes, 25, 0)
JL_FEATURE_DEF(xsave, 26, 0)
JL_FEATURE_DEF(avx, 28, 0)
JL_FEATURE_DEF(f16c, 29, 0)
JL_FEATURE_DEF(rdrnd, 30, 0)

// EAX=1: EDX
// JL_FEATURE_DEF(, 32 + ?, ????)

// EAX=7,ECX=0: EBX
JL_FEATURE_DEF(fsgsbase, 32 * 2 + 0, 0)
// JL_FEATURE_DEF(sgx, 32 * 2 + 2, 0) // Disable for now since it's very hard to detect
JL_FEATURE_DEF(bmi, 32 * 2 + 3, 0)
// JL_FEATURE_DEF(hle, 32 * 2 + 4, 0) // Not used and gone in LLVM 5.0
JL_FEATURE_DEF(avx2, 32 * 2 + 5, 0)
JL_FEATURE_DEF(bmi2, 32 * 2 + 8, 0)
// JL_FEATURE_DEF(invpcid, 32 * 2 + 10, 0) // Not used and gone in LLVM 5.0
JL_FEATURE_DEF(rtm, 32 * 2 + 11, 0)
JL_FEATURE_DEF(mpx, 32 * 2 + 14, 0)
// Disable avx512 pre-5.0 since it can't handle address space
JL_FEATURE_DEF(avx512f, 32 * 2 + 16, 0)
JL_FEATURE_DEF(avx512dq, 32 * 2 + 17, 0)
JL_FEATURE_DEF(rdseed, 32 * 2 + 18, 0)
JL_FEATURE_DEF(adx, 32 * 2 + 19, 0)
// JL_FEATURE_DEF(smap, 32 * 2 + 20, 0) // Not used and gone in LLVM 5.0
JL_FEATURE_DEF(avx512ifma, 32 * 2 + 21, 0)
// JL_FEATURE_DEF(pcommit, 32 * 2 + 22, 0) // Deprecated
JL_FEATURE_DEF(clflushopt, 32 * 2 + 23, 0)
JL_FEATURE_DEF(clwb, 32 * 2 + 24, 0)
JL_FEATURE_DEF(avx512pf, 32 * 2 + 26, 0)
JL_FEATURE_DEF(avx512er, 32 * 2 + 27, 0)
JL_FEATURE_DEF(avx512cd, 32 * 2 + 28, 0)
JL_FEATURE_DEF(sha, 32 * 2 + 29, 0)
JL_FEATURE_DEF(avx512bw, 32 * 2 + 30, 0)
JL_FEATURE_DEF(avx512vl, 32 * 2 + 31, 0)

// EAX=7,ECX=0: ECX
JL_FEATURE_DEF(prefetchwt1, 32 * 3 + 0, 0)
JL_FEATURE_DEF(avx512vbmi, 32 * 3 + 1, 0)
JL_FEATURE_DEF(pku, 32 * 3 + 4, 0) // ospke
JL_FEATURE_DEF(avx512vpopcntdq, 32 * 3 + 14, 0)

// EAX=7,ECX=0: EDX
// JL_FEATURE_DEF(avx512_4vnniw, 32 * 4 + 2, ?????)
// JL_FEATURE_DEF(avx512_4fmaps, 32 * 4 + 3, ?????)

// EAX=0x80000001: ECX
// ignore sahf on 32bit x86 since it is required
JL_FEATURE_DEF(sahf, 32 * 5 + 0, JL_X86_64ONLY_VER(0))
JL_FEATURE_DEF(lzcnt, 32 * 5 + 5, 0)
JL_FEATURE_DEF(sse4a, 32 * 5 + 6, 0)
JL_FEATURE_DEF(prfchw, 32 * 5 + 8, 0)
JL_FEATURE_DEF(xop, 32 * 5 + 11, 0)
JL_FEATURE_DEF(lwp, 32 * 5 + 15, 0)
JL_FEATURE_DEF(fma4, 32 * 5 + 16, 0)
JL_FEATURE_DEF(tbm, 32 * 5 + 21, 0)
JL_FEATURE_DEF(mwaitx, 32 * 5 + 29, 0)

// EAX=0x80000001: EDX
// 3dnow is here but we don't care...
// JL_FEATURE_DEF(, 32 * 6 + ?, ?????)

// EAX=0xd: EAX
JL_FEATURE_DEF(xsaveopt, 32 * 7 + 0, 0)
JL_FEATURE_DEF(xsavec, 32 * 7 + 1, 0)
JL_FEATURE_DEF(xsaves, 32 * 7 + 3, 0)

// EAX=0x80000008: EBX
JL_FEATURE_DEF(clzero, 32 * 8 + 0, 0)

#undef JL_X86_64ONLY_VER
