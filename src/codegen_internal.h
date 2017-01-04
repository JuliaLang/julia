// This file is a part of Julia. License is MIT: http://julialang.org/license

#if defined(USE_ORCJIT) && JL_LLVM_VERSION <= 30800
#  include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
void notifyObjectLoaded(RTDyldMemoryManager *memmgr,
                        llvm::orc::ObjectLinkingLayerBase::ObjSetHandleT H);
#endif

// Declarations for disasm.cpp
extern "C"
void jl_dump_asm_internal(uintptr_t Fptr, size_t Fsize, int64_t slide,
#ifndef USE_MCJIT
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#endif
                          const object::ObjectFile *object,
                          llvm::DIContext *context,
#if JL_LLVM_VERSION >= 30700
                          raw_ostream &rstream,
#else
                          formatted_raw_ostream &stream,
#endif
                          const char* asm_variant="att"
                          );

// Declarations for debuginfo.cpp
extern int jl_DI_for_fptr(uint64_t fptr, uint64_t *symsize, int64_t *slide, int64_t *section_slide,
                      const object::ObjectFile **object,
#ifdef USE_MCJIT
                      llvm::DIContext **context
#else
                      std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines
#endif
                      );

extern bool jl_dylib_DI_for_fptr(size_t pointer, const object::ObjectFile **object, llvm::DIContext **context, int64_t *slide, int64_t *section_slide,
        bool onlySysImg, bool *isSysImg, void **saddr, char **name, char **filename);

#ifdef USE_ORCJIT
#ifdef _OS_WINDOWS_
void *lookupWriteAddressFor(RTDyldMemoryManager *memmgr, void *rt_addr);
#endif
#endif

#ifdef USE_MCJIT
RTDyldMemoryManager* createRTDyldMemoryManager(void);
#endif
