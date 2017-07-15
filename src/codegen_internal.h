// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "codegen_shared.h"

// Declarations for disasm.cpp
extern "C"
void jl_dump_asm_internal(uintptr_t Fptr, size_t Fsize, int64_t slide,
                          const object::ObjectFile *object,
                          llvm::DIContext *context,
                          raw_ostream &rstream,
                          const char* asm_variant="att"
                          );

// Declarations for debuginfo.cpp
extern int jl_DI_for_fptr(uint64_t fptr, uint64_t *symsize, int64_t *slide, int64_t *section_slide,
                      const object::ObjectFile **object,
                      llvm::DIContext **context
                      );

extern bool jl_dylib_DI_for_fptr(size_t pointer, const object::ObjectFile **object, llvm::DIContext **context, int64_t *slide, int64_t *section_slide,
        bool onlySysImg, bool *isSysImg, void **saddr, char **name, char **filename);

#ifdef _OS_WINDOWS_
void *lookupWriteAddressFor(RTDyldMemoryManager *memmgr, void *rt_addr);
#endif

RTDyldMemoryManager* createRTDyldMemoryManager(void);
