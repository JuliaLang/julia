// Pre-declaration. Definition in disasm.cpp
extern "C"
void jl_dump_asm_internal(uintptr_t Fptr, size_t Fsize, int64_t slide,
#ifdef USE_MCJIT
                          llvm::DIContext *context,
#else
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#endif
#ifdef LLVM37
                          raw_ostream &rstream
#else
                          formatted_raw_ostream &stream
#endif
                          );

extern int jl_DI_for_fptr(uint64_t fptr, uint64_t *symsize, int64_t *slide, int64_t *SectionSlide,
                      const object::ObjectFile **object,
#ifdef USE_MCJIT
                      llvm::DIContext **context
#else
                      std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines
#endif
                      );
extern bool jl_dylib_DI_for_fptr(size_t pointer, const object::ObjectFile **object, llvm::DIContext **context, int64_t *slide, bool onlySysImg,
    bool *isSysImg, void **saddr, char **name, char **filename);
#ifdef USE_MCJIT
extern void jl_cleanup_DI(llvm::DIContext *context);
#endif
