// This file is part of Julia.
// Parts of this file are copied from LLVM, under the UIUC license.

template <class T>
static void addOptimizationPasses(T *PM)
{
#ifdef __has_feature
#   if __has_feature(address_sanitizer)
#   if defined(LLVM37) && !defined(LLVM38)
    // LLVM 3.7 BUG: ASAN pass doesn't properly initialize its dependencies
    initializeTargetLibraryInfoWrapperPassPass(*PassRegistry::getPassRegistry());
#   endif
    FPM->add(createAddressSanitizerFunctionPass());
#   endif
#   if __has_feature(memory_sanitizer)
    FPM->add(llvm::createMemorySanitizerPass(true));
#   endif
#endif
#ifdef LLVM37
    PM->add(createTargetTransformInfoWrapperPass(jl_TargetMachine->getTargetIRAnalysis()));
#else
    jl_TargetMachine->addAnalysisPasses(*PM);
#endif
#ifdef LLVM38
    PM->add(createTypeBasedAAWrapperPass());
#else
    PM->add(createTypeBasedAliasAnalysisPass());
#endif
    if (jl_options.opt_level>=1) {
#ifdef LLVM38
        PM->add(createBasicAAWrapperPass());
#else
        PM->add(createBasicAliasAnalysisPass());
#endif
    }
    // list of passes from vmkit
    PM->add(createCFGSimplificationPass()); // Clean up disgusting code
    PM->add(createPromoteMemoryToRegisterPass());// Kill useless allocas

#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
#endif
    PM->add(createSROAPass());                 // Break up aggregate allocas
#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
#endif
    PM->add(createJumpThreadingPass());        // Thread jumps.
    // NOTE: CFG simp passes after this point seem to hurt native codegen.
    // See issue #6112. Should be re-evaluated when we switch to MCJIT.
    //FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Combine silly seq's
#endif

    //FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
    PM->add(createReassociatePass());          // Reassociate expressions

    // this has the potential to make some things a bit slower
    //FPM->add(createBBVectorizePass());

    PM->add(createEarlyCSEPass()); //// ****

    PM->add(createLoopIdiomPass()); //// ****
    PM->add(createLoopRotatePass());           // Rotate loops.
    // LoopRotate strips metadata from terminator, so run LowerSIMD afterwards
    PM->add(createLowerSimdLoopPass());        // Annotate loop marked with "simdloop" as LLVM parallel loop
    PM->add(createLICMPass());                 // Hoist loop invariants
    PM->add(createLoopUnswitchPass());         // Unswitch loops.
    // Subsequent passes not stripping metadata from terminator
#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass());
#endif
    PM->add(createIndVarSimplifyPass());       // Canonicalize indvars
    PM->add(createLoopDeletionPass());         // Delete dead loops
#if defined(LLVM35)
    PM->add(createSimpleLoopUnrollPass());     // Unroll small loops
#else
    PM->add(createLoopUnrollPass());           // Unroll small loops
#endif
#if !defined(LLVM35) && !defined(INSTCOMBINE_BUG)
    PM->add(createLoopVectorizePass());        // Vectorize loops
#endif
    //FPM->add(createLoopStrengthReducePass());   // (jwb added)

#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Clean up after the unroller
#endif
    PM->add(createGVNPass());                  // Remove redundancies
    //FPM->add(createMemCpyOptPass());            // Remove memcpy / form memset
    PM->add(createSCCPPass());                 // Constant prop with SCCP

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    PM->add(createSinkingPass()); ////////////// ****
    PM->add(createInstructionSimplifierPass());///////// ****
#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass());
#endif
    PM->add(createJumpThreadingPass());         // Thread jumps
    PM->add(createDeadStoreEliminationPass());  // Delete dead stores
#if !defined(INSTCOMBINE_BUG)
    if (jl_options.opt_level>=1)
        PM->add(createSLPVectorizerPass());     // Vectorize straight-line code
#endif

    PM->add(createAggressiveDCEPass());         // Delete dead instructions
#if !defined(INSTCOMBINE_BUG)
    if (jl_options.opt_level>=1)
        PM->add(createInstructionCombiningPass());   // Clean up after SLP loop vectorizer
#endif
#if defined(LLVM35)
    PM->add(createLoopVectorizePass());         // Vectorize loops
    PM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer
#endif
    //FPM->add(createCFGSimplificationPass());     // Merge & remove BBs
}

#ifdef USE_ORCJIT

// ------------------------ TEMPORARILY COPIED FROM LLVM -----------------
// This must be kept in sync with gdb/gdb/jit.h .
extern "C" {

  typedef enum {
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN
  } jit_actions_t;

  struct jit_code_entry {
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
  };

  struct jit_descriptor {
    uint32_t version;
    // This should be jit_actions_t, but we want to be specific about the
    // bit-width.
    uint32_t action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
  };

  // We put information about the JITed function in this global, which the
  // debugger reads.  Make sure to specify the version statically, because the
  // debugger checks the version before we can set it during runtime.
  extern struct jit_descriptor __jit_debug_descriptor;

  LLVM_ATTRIBUTE_NOINLINE extern void __jit_debug_register_code();
}

extern JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                      const object::ObjectFile &obj,
                                      const object::ObjectFile &debugObj,
                                      const RuntimeDyld::LoadedObjectInfo &L);

namespace {

using namespace llvm;
using namespace llvm::object;
using namespace llvm::orc;

/// Do the registration.
void NotifyDebugger(jit_code_entry* JITCodeEntry) {
  __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;

  // Insert this entry at the head of the list.
  JITCodeEntry->prev_entry = nullptr;
  jit_code_entry* NextEntry = __jit_debug_descriptor.first_entry;
  JITCodeEntry->next_entry = NextEntry;
  if (NextEntry) {
    NextEntry->prev_entry = JITCodeEntry;
  }
  __jit_debug_descriptor.first_entry = JITCodeEntry;
  __jit_debug_descriptor.relevant_entry = JITCodeEntry;
  __jit_debug_register_code();
}

// --------------------------------------------------------------------------

class DebugObjectRegistrar {
private:
    void NotifyGDB(OwningBinary<ObjectFile> &DebugObj) {
      const char *Buffer = DebugObj.getBinary()->getMemoryBufferRef().getBufferStart();
      size_t      Size = DebugObj.getBinary()->getMemoryBufferRef().getBufferSize();

      assert(Buffer && "Attempt to register a null object with a debugger.");
      jit_code_entry* JITCodeEntry = new jit_code_entry();

      if (!JITCodeEntry) {
        jl_printf(JL_STDERR, "WARNING: Allocation failed when registering a JIT entry!\n");
      } else {
        JITCodeEntry->symfile_addr = Buffer;
        JITCodeEntry->symfile_size = Size;

        NotifyDebugger(JITCodeEntry);
      }
    }

    std::vector<OwningBinary<ObjectFile>> SavedObjects;
    std::unique_ptr<JITEventListener> JuliaListener;

public:
    DebugObjectRegistrar() : JuliaListener(CreateJuliaJITEventListener()) {}

    template <typename ObjSetT, typename LoadResult>
    void operator()(ObjectLinkingLayerBase::ObjSetHandleT, const ObjSetT &Objects,
                  const LoadResult &LOS) {
        auto oit = Objects.begin();
        auto lit = LOS.begin();
        while (oit != Objects.end()) {
            auto &Object = *oit;
            auto &LO = *lit;

            OwningBinary<ObjectFile> SavedObject = LO->getObjectForDebug(*Object);

            // If the debug object is unavailable, save (a copy of) the original object
            // for our backtraces
            if (!SavedObject.getBinary()) {
                // This is unfortunate, but there doesn't seem to be a way to take
                // ownership of the original buffer
                auto NewBuffer = MemoryBuffer::getMemBufferCopy(Object->getData(), Object->getFileName());
                auto NewObj = ObjectFile::createObjectFile(NewBuffer->getMemBufferRef());
                SavedObject = OwningBinary<ObjectFile>(std::move(*NewObj),std::move(NewBuffer));
            }
            else {
                NotifyGDB(SavedObject);
            }

            SavedObjects.push_back(std::move(SavedObject));
            ORCNotifyObjectEmitted(JuliaListener.get(),*Object,*SavedObjects.back().getBinary(),*LO);

            ++oit;
            ++lit;
        }
    }
};

}

#if defined(_OS_DARWIN_) && defined(LLVM37) && defined(LLVM_SHLIB)
#define CUSTOM_MEMORY_MANAGER 1
extern RTDyldMemoryManager* createRTDyldMemoryManagerOSX();
#endif

class JuliaOJIT {
public:
    typedef orc::ObjectLinkingLayer<DebugObjectRegistrar> ObjLayerT;
    typedef orc::IRCompileLayer<ObjLayerT> CompileLayerT;
    typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;
    typedef StringMap<void*> GlobalSymbolTableT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;

    JuliaOJIT(TargetMachine &TM)
      : TM(TM),
        DL(TM.createDataLayout()),
        ObjStream(ObjBufferSV),
        MemMgr(
#ifdef CUSTOM_MEMORY_MANAGER
            createRTDyldMemoryManagerOSX()
#else
            new SectionMemoryManager
#endif
            ) {
#ifdef JL_DEBUG_BUILD
            PM.add(createVerifierPass());
#endif
            // In imaging mode, we run the pass manager on creation
            // to make sure it ends up optimized in the shadow module
            if (!imaging_mode) {
                addOptimizationPasses(&PM);
#ifdef JL_DEBUG_BUILD
                PM.add(createVerifierPass());
#endif
            }
            if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
                llvm_unreachable("Target does not support MC emission.");

            CompileLayer = std::unique_ptr<CompileLayerT>{new CompileLayerT(ObjectLayer,
                [&](Module &M) {
                    PM.run(M);
                    std::unique_ptr<MemoryBuffer> ObjBuffer(
                        new ObjectMemoryBuffer(std::move(ObjBufferSV)));
                    ErrorOr<std::unique_ptr<object::ObjectFile>> Obj =
                        object::ObjectFile::createObjectFile(ObjBuffer->getMemBufferRef());

                    if (!Obj) {
                        M.dump();
                        llvm::report_fatal_error("FATAL: Unable to compile LLVM Module.\n"
                            "The module's content was printed above. Please file a bug report");
                    }

                    return OwningObj(std::move(*Obj), std::move(ObjBuffer));
                }
            )};
            // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
            // symbols in the program as well. The nullptr argument to the function
            // tells DynamicLibrary to load the program, not a library.

            std::string *ErrorStr = nullptr;
            if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, ErrorStr))
                report_fatal_error("FATAL: unable to dlopen self\n" + *ErrorStr);
        }

    std::string mangle(const std::string &Name) {
        std::string MangledName;
        {
            raw_string_ostream MangledNameStream(MangledName);
            Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
        }
        return MangledName;
    }

    void addGlobalMapping(StringRef Name, void *Addr) {
       GlobalSymbolTable[mangle(Name)] = Addr;
    }

    ModuleHandleT addModule(Module *M) {
        // We need a memory manager to allocate memory and resolve symbols for this
        // new module. Create one that resolves symbols by looking back into the
        // JIT.
        auto Resolver = orc::createLambdaResolver(
                          [&](const std::string &Name) {
                            // TODO: consider moving the FunctionMover resolver here
                            // Step 0: ObjectLinkingLayer has checked whether it is in the current module
                            // Step 1: Check against list of known external globals
                            GlobalSymbolTableT::const_iterator pos = GlobalSymbolTable.find(Name);
                            if (pos != GlobalSymbolTable.end())
                                return RuntimeDyld::SymbolInfo((intptr_t)pos->second, JITSymbolFlags::Exported);
                            // Step 2: Search all previously emitted symbols
                            if (auto Sym = findSymbol(Name))
                              return RuntimeDyld::SymbolInfo(Sym.getAddress(),
                                                             Sym.getFlags());
                            // Step 2: Search the program symbols
                            if (uint64_t addr = SectionMemoryManager::getSymbolAddressInProcess(Name))
                                return RuntimeDyld::SymbolInfo(addr, JITSymbolFlags::Exported);
                            // Return failure code
                            return RuntimeDyld::SymbolInfo(nullptr);
                          },
                          [](const std::string &S) { return nullptr; }
                        );
        SmallVector<std::unique_ptr<Module>,1> Ms;
        Ms.push_back(std::unique_ptr<Module>{M});
        return CompileLayer->addModuleSet(std::move(Ms),
                                         MemMgr,
                                         std::move(Resolver));
    }

    void removeModule(ModuleHandleT H) { CompileLayer->removeModuleSet(H); }

    orc::JITSymbol findSymbol(const std::string &Name) {
        return CompileLayer->findSymbol(Name, true);
    }

    orc::JITSymbol findUnmangledSymbol(const std::string Name) {
        return findSymbol(mangle(Name));
    }

    uint64_t getGlobalValueAddress(const std::string &Name) {
        return CompileLayer->findSymbol(mangle(Name), false).getAddress();
    }

    uint64_t getFunctionAddress(const std::string &Name) {
        return CompileLayer->findSymbol(mangle(Name), false).getAddress();
    }

    uint64_t FindFunctionNamed(const std::string &Name) {
        return 0; // Functions are not kept around
    }

    void RegisterJITEventListener(JITEventListener *L) {
        // TODO
    }

    const DataLayout& getDataLayout() const {
        return DL;
    }

    const Triple& getTargetTriple() const {
        return TM.getTargetTriple();
    }

private:
    TargetMachine &TM;
    const DataLayout DL;
    // Should be big enough that in the common case, The
    // object fits in its entirety
    SmallVector<char, 4096> ObjBufferSV;
    raw_svector_ostream ObjStream;
    legacy::PassManager PM;
    MCContext *Ctx;
    RTDyldMemoryManager *MemMgr;
    ObjLayerT ObjectLayer;
    std::unique_ptr<CompileLayerT> CompileLayer;
    GlobalSymbolTableT GlobalSymbolTable;
};
#endif
