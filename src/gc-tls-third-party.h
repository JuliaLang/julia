// This file is a part of Julia. License is MIT: https://julialang.org/license

// Pick the appropriate third-party implementation
#ifdef WITH_THIRD_PARTY_HEAP
#if WITH_THIRD_PARTY_HEAP == 1
#include "gc-tls-mmtk.h"
#endif
// To be extended by other GC implementations
// #if WITH_THIRD_PARTY_HEAP == X
//      #include gc-tls-XXXX.h
// #endif
#endif
