// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_JINATIVE_PRELINK_H
#define JL_JINATIVE_PRELINK_H

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/MemoryBufferRef.h>
#include <llvm/TargetParser/Triple.h>

#include <string>

// Link `Objects` (object files of the triple `TT`) into the native code section
// of a .ji (see jinative.h). Returns false, with the reason in `Err`, if they
// cannot be represented.
bool jl_prelink_native_image(llvm::ArrayRef<llvm::MemoryBufferRef> Objects, const llvm::Triple &TT,
                             uint64_t PageSize, llvm::SmallVectorImpl<char> &Out, std::string &Err,
                             std::string *Stats);

#endif
