// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_DEBUGINFO_H
#define JL_DEBUGINFO_H

#include <llvm/DebugInfo/DIContext.h>
#include <llvm/Object/ObjectFile.h>

#include "dtypes.h"

// Declarations for debuginfo.cpp
int jl_DI_for_fptr(uint64_t fptr, uint64_t *symsize, uint64_t *slide,
        llvm::object::SectionRef *Section, llvm::DIContext **context) JL_NOTSAFEPOINT;

bool jl_dylib_DI_for_fptr(size_t pointer, llvm::object::SectionRef *Section, uint64_t *slide, llvm::DIContext **context,
    bool onlyImage, bool *isImage, uint64_t* fbase, void **saddr, char **name, char **filename) JL_NOTSAFEPOINT;

static llvm::object::SectionedAddress makeAddress(
        llvm::object::SectionRef Section, uint64_t address) JL_NOTSAFEPOINT
{
    return llvm::object::SectionedAddress{address, Section.getIndex()};
}

#endif
