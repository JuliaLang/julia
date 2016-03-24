// This file is a part of Julia. License is MIT: http://julialang.org/license

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

// this defines the set of optimization passes defined for Julia at various optimization levels
template <class T>
static void addOptimizationPasses(T *PM)
{
#ifdef __has_feature
#   if __has_feature(address_sanitizer)
#   if defined(LLVM37) && !defined(LLVM38)
    // LLVM 3.7 BUG: ASAN pass doesn't properly initialize its dependencies
    initializeTargetLibraryInfoWrapperPassPass(*PassRegistry::getPassRegistry());
#   endif
    PM->add(createAddressSanitizerFunctionPass());
#   endif
#   if __has_feature(memory_sanitizer)
    PM->add(llvm::createMemorySanitizerPass(true));
#   endif
#endif
    if (jl_options.opt_level <= 1) {
        return;
    }
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
    if (jl_options.opt_level >= 3) {
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
    //PM->add(createCFGSimplificationPass());    // Merge & remove BBs
#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Combine silly seq's
#endif

    //PM->add(createCFGSimplificationPass());    // Merge & remove BBs
    PM->add(createReassociatePass());          // Reassociate expressions

    // this has the potential to make some things a bit slower
    //PM->add(createBBVectorizePass());

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
    //PM->add(createLoopStrengthReducePass());   // (jwb added)

#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Clean up after the unroller
#endif
    PM->add(createGVNPass());                  // Remove redundancies
    //PM->add(createMemCpyOptPass());            // Remove memcpy / form memset
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
    if (jl_options.opt_level >= 3) {
#ifdef LLVM39
        initializeDemandedBitsPass(*PassRegistry::getPassRegistry());
#endif
        PM->add(createSLPVectorizerPass());     // Vectorize straight-line code
    }
#endif

    PM->add(createAggressiveDCEPass());         // Delete dead instructions
#if !defined(INSTCOMBINE_BUG)
    if (jl_options.opt_level >= 3)
        PM->add(createInstructionCombiningPass());   // Clean up after SLP loop vectorizer
#endif
#if defined(LLVM35)
    PM->add(createLoopVectorizePass());         // Vectorize loops
    PM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer
#endif
    //PM->add(createCFGSimplificationPass());     // Merge & remove BBs
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

#if defined(_OS_DARWIN_) && defined(LLVM37) && defined(LLVM_SHLIB)
#define CUSTOM_MEMORY_MANAGER createRTDyldMemoryManagerOSX
extern RTDyldMemoryManager *createRTDyldMemoryManagerOSX();
#elif defined(_OS_LINUX_) && defined(LLVM37) && defined(JL_UNW_HAS_FORMAT_IP)
#define CUSTOM_MEMORY_MANAGER createRTDyldMemoryManagerUnix
extern RTDyldMemoryManager *createRTDyldMemoryManagerUnix();
#endif

namespace {

using namespace llvm;
using namespace llvm::object;
using namespace llvm::orc;

/// Do the registration.
void NotifyDebugger(jit_code_entry *JITCodeEntry)
{
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;

    // Insert this entry at the head of the list.
    JITCodeEntry->prev_entry = nullptr;
    jit_code_entry *NextEntry = __jit_debug_descriptor.first_entry;
    JITCodeEntry->next_entry = NextEntry;
    if (NextEntry) {
        NextEntry->prev_entry = JITCodeEntry;
    }
    __jit_debug_descriptor.first_entry = JITCodeEntry;
    __jit_debug_descriptor.relevant_entry = JITCodeEntry;
    __jit_debug_register_code();
}
// ------------------------ END OF TEMPORARY COPY FROM LLVM -----------------

// Custom object emission notification handler for the JuliaOJIT
// TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
class DebugObjectRegistrar {
private:
    void NotifyGDB(OwningBinary<ObjectFile> &DebugObj)
    {
      const char *Buffer = DebugObj.getBinary()->getMemoryBufferRef().getBufferStart();
      size_t      Size = DebugObj.getBinary()->getMemoryBufferRef().getBufferSize();

      assert(Buffer && "Attempt to register a null object with a debugger.");
      jit_code_entry *JITCodeEntry = new jit_code_entry();

      if (!JITCodeEntry) {
          jl_printf(JL_STDERR, "WARNING: Allocation failed when registering a JIT entry!\n");
      }
      else {
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
                    const LoadResult &LOS)
    {
        auto oit = Objects.begin();
        auto lit = LOS.begin();
        while (oit != Objects.end()) {
#ifdef LLVM39
            const auto &Object = (*oit)->getBinary();
#else
            auto &Object = *oit;
#endif
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

            ORCNotifyObjectEmitted(JuliaListener.get(),
                    *Object,
                    *SavedObjects.back().getBinary(),
                    *LO);

            ++oit;
            ++lit;
        }
    }
};

// A simplified model of the LLVM ExecutionEngine that implements only the methods that Julia needs
// but tries to roughly match the API anyways so that compatibility is easier
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
            CUSTOM_MEMORY_MANAGER()
#else
            new SectionMemoryManager
#endif
            ) {
#ifdef JL_DEBUG_BUILD
            PM.add(createVerifierPass());
#endif
            addOptimizationPasses(&PM);
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

    std::string getMangledName(const std::string &Name)
    {
        SmallString<128> FullName;
        Mangler::getNameWithPrefix(FullName, Name, DL);
        return FullName.str();
    }

    std::string getMangledName(const GlobalValue *GV)
    {
        return getMangledName(GV->getName());
    }

    void addGlobalMapping(StringRef Name, uint64_t Addr)
    {
       bool successful = GlobalSymbolTable.insert(make_pair(getMangledName(Name), (void*)Addr)).second;
       (void)successful;
       assert(successful);
    }

    void *getPointerToGlobalIfAvailable(StringRef S) {
        GlobalSymbolTableT::const_iterator pos = GlobalSymbolTable.find(S);
        if (pos != GlobalSymbolTable.end())
            return pos->second;
        return nullptr;
    }

    ModuleHandleT addModule(std::unique_ptr<Module> M)
    {
#ifndef NDEBUG
        // validate the relocations for M
        for (Module::iterator I = M->begin(), E = M->end(); I != E; ) {
            Function *F = &*I;
            ++I;
            if (F->isDeclaration()) {
                if (F->use_empty())
                    F->eraseFromParent();
                else
                    assert(F->isIntrinsic() || findUnmangledSymbol(F->getName()) ||
                            SectionMemoryManager::getSymbolAddressInProcess(F->getName()));
            }
        }
#endif
        // We need a memory manager to allocate memory and resolve symbols for this
        // new module. Create one that resolves symbols by looking back into the JIT.
        auto Resolver = orc::createLambdaResolver(
                          [&](const std::string &Name) {
                            // TODO: consider moving the FunctionMover resolver here
                            // Step 0: ObjectLinkingLayer has checked whether it is in the current module
                            // Step 1: See if it's something known to the ExecutionEngine
                            if (auto Sym = findSymbol(Name, true))
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
        Ms.push_back(std::move(M));
        return CompileLayer->addModuleSet(std::move(Ms),
                                          MemMgr,
                                          std::move(Resolver));
    }

    void removeModule(ModuleHandleT H) { CompileLayer->removeModuleSet(H); }

    orc::JITSymbol findSymbol(const std::string &Name, bool ExportedSymbolsOnly)
    {
        if (ExportedSymbolsOnly) {
            // Step 1: Check against list of known external globals
            void *Addr = getPointerToGlobalIfAvailable(Name);
            if (Addr != nullptr)
                return orc::JITSymbol((uintptr_t)Addr, JITSymbolFlags::Exported);
        }
        // Step 2: Search all previously emitted symbols
        return CompileLayer->findSymbol(Name, ExportedSymbolsOnly);
    }

    orc::JITSymbol findUnmangledSymbol(const std::string Name)
    {
        return findSymbol(getMangledName(Name), true);
    }

    uint64_t getGlobalValueAddress(const std::string &Name)
    {
        return findSymbol(getMangledName(Name), false).getAddress();
    }

    uint64_t getFunctionAddress(const std::string &Name)
    {
        return findSymbol(getMangledName(Name), false).getAddress();
    }

    Function *FindFunctionNamed(const std::string &Name)
    {
        return 0; // Functions are not kept around
    }

    void RegisterJITEventListener(JITEventListener *L)
    {
        // TODO
    }

    const DataLayout& getDataLayout() const
    {
        return DL;
    }

    const Triple& getTargetTriple() const
    {
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

}
#endif

#ifdef USE_ORCJIT
JuliaOJIT *jl_ExecutionEngine;
#else
ExecutionEngine *jl_ExecutionEngine;
#endif

// destructively move the contents of src into dest
// this assumes that the targets of the two modules are the same
// including the DataLayout and ModuleFlags (for example)
// and that there is no module-level assembly
static void jl_merge_module(Module *dest, std::unique_ptr<Module> src)
{
    assert(dest != src.get());
    for (Module::global_iterator I = src->global_begin(), E = src->global_end(); I != E;) {
        GlobalVariable *sG = &*I;
        GlobalValue *dG = dest->getNamedValue(sG->getName());
        ++I;
        if (dG) {
            if (sG->isDeclaration()) {
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        sG->removeFromParent();
        dest->getGlobalList().push_back(sG);
    }

    for (Module::iterator I = src->begin(), E = src->end(); I != E;) {
        Function *sG = &*I;
        GlobalValue *dG = dest->getNamedValue(sG->getName());
        ++I;
        if (dG) {
            if (sG->isDeclaration()) {
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        sG->removeFromParent();
        dest->getFunctionList().push_back(sG);
    }

    for (Module::alias_iterator I = src->alias_begin(), E = src->alias_end(); I != E;) {
        GlobalAlias *sG = &*I;
        GlobalValue *dG = dest->getNamedValue(sG->getName());
        ++I;
        if (dG) {
            if (!dG->isDeclaration()) { // aliases are always definitions, so this test is reversed from the above two
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        sG->removeFromParent();
        dest->getAliasList().push_back(sG);
    }

    // metadata nodes need to be explicitly merged not just copied
    // so there are special passes here for each known type of metadata
    NamedMDNode *sNMD = src->getNamedMetadata("llvm.dbg.cu");
    if (sNMD) {
        NamedMDNode *dNMD = dest->getOrInsertNamedMetadata("llvm.dbg.cu");
#ifdef LLVM35
        for (NamedMDNode::op_iterator I = sNMD->op_begin(), E = sNMD->op_end(); I != E; ++I) {
            dNMD->addOperand(*I);
        }
#else
        for (unsigned i = 0, l = sNMD->getNumOperands(); i < l; i++) {
            dNMD->addOperand(sNMD->getOperand(i));
        }
#endif
    }
}

// to finalizing a function, look up its name in the `module_for_fname` map of unfinalized functions
// and merge it, plus any other modules it depends upon, into `collector`
// then add `collector` to the execution engine
//
// in the old JIT, functions are finalized by adding them to the shadow module
// (which aliases the engine module), so this is unneeded
#ifdef USE_MCJIT
static StringMap<Module*> module_for_fname;
static void jl_finalize_function(StringRef F, Module *collector = NULL)
{
    std::unique_ptr<Module> m(module_for_fname.lookup(F));
    if (m) {
        // probably not many unresolved declarations, but be sure iterate over their Names,
        // since the declarations may get destroyed by the jl_merge_module call
        SmallVector<StringRef, 8> to_finalize;
        for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
            Function *F = &*I;
            if (!F->isDeclaration()) {
                module_for_fname.erase(F->getName());
            }
            else if (!F->isIntrinsic()) {
                to_finalize.push_back(F->getName());
            }
        }

        for (auto F : to_finalize) {
            jl_finalize_function(F, collector ? collector : m.get());
        }

        if (collector) {
            jl_merge_module(collector, std::move(m));
        }
        else {
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_) && defined(LLVM35)
            // Add special values used by debuginfo to build the UnwindData table registration for Win64
            ArrayType *atype = ArrayType::get(T_uint32, 3); // want 4-byte alignment of 12-bytes of data
            (new GlobalVariable(*m, atype,
                false, GlobalVariable::InternalLinkage,
                ConstantAggregateZero::get(atype), "__UnwindData"))->setSection(".text");
            (new GlobalVariable(*m, atype,
                false, GlobalVariable::InternalLinkage,
                ConstantAggregateZero::get(atype), "__catchjmp"))->setSection(".text");
#endif
            assert(jl_ExecutionEngine);
#if defined(LLVM36)
            jl_ExecutionEngine->addModule(std::move(m));
#else
            jl_ExecutionEngine->addModule(m.release());
#endif
        }
    }
}
static void jl_finalize_function(Function *F, Module *collector = NULL)
{
    jl_finalize_function(F->getName(), collector);
}
#endif

// MSVC's link.exe requires each function declaration to have a Comdat section
// rather than litter the code with conditionals,
// all global values that get emitted call this function
// and it decides whether the definition needs a Comdat section and adds the appropriate declation
// TODO: consider moving this into jl_add_to_shadow or jl_dump_shadow? the JIT doesn't care, so most calls are now no-ops
template<class T> // for GlobalObject's
static T *addComdat(T *G)
{
#if defined(_OS_WINDOWS_)
    if (imaging_mode && !G->isDeclaration()) {
#ifdef LLVM35
        // Add comdat information to make MSVC link.exe happy
        Comdat *jl_Comdat = G->getParent()->getOrInsertComdat(G->getName());
        jl_Comdat->setSelectionKind(Comdat::NoDuplicates);
        G->setComdat(jl_Comdat);
        // add __declspec(dllexport) to everything marked for export
        if (G->getLinkage() == GlobalValue::ExternalLinkage)
            G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
#endif
    }
#endif
    return G;
}

// Connect Modules via prototypes, each owned by module `M`
static GlobalVariable *global_proto(GlobalVariable *G, Module *M = NULL)
{
    // Copy the GlobalVariable, but without the initializer, so it becomes a declaration
    GlobalVariable *proto = new GlobalVariable(G->getType()->getElementType(),
            G->isConstant(), GlobalVariable::ExternalLinkage,
            NULL, G->getName(),  G->getThreadLocalMode());
    if (M)
        M->getGlobalList().push_back(proto);
    return proto;
}

static Function *function_proto(Function *F, Module *M = NULL)
{
    // Copy the declaration characteristics of the Function (not the body)
    Function *NewF = Function::Create(F->getFunctionType(),
                                      Function::ExternalLinkage,
                                      F->getName(), M);
    NewF->setAttributes(AttributeSet());

    // FunctionType does not include any attributes. Copy them over manually
    // as codegen may make decisions based on the presence of certain attributes
    NewF->copyAttributesFrom(F);

#ifdef LLVM37
    // Declarations are not allowed to have personality routines, but
    // copyAttributesFrom sets them anyway, so clear them again manually
    NewF->setPersonalityFn(nullptr);
#endif

    return NewF;
}

// helper function for adding a DLLImport (dlsym) address to the execution engine
// (for values created locally or in the sysimage, jl_emit_and_add_to_shadow is generally preferable)
template<typename T>
#ifdef LLVM35
static inline void add_named_global(GlobalObject *gv, T *_addr, bool dllimport = true)
#else
static inline void add_named_global(GlobalValue *gv, T *_addr, bool dllimport = true)
#endif
{
    // cast through integer to avoid c++ pedantic warning about casting between
    // data and code pointers
    void *addr = (void*)(uintptr_t)_addr;
#ifdef LLVM34
    StringRef name = gv->getName();
#ifdef _OS_WINDOWS_
    std::string imp_name;
#endif
#endif

#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
#ifdef LLVM35
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
        // this will cause llvm to rename it, so we do the same
        imp_name = Twine("__imp_", name).str();
        name = StringRef(imp_name);
#else
        gv->setLinkage(GlobalValue::DLLImportLinkage);
#endif
#if defined(_P64) || defined(LLVM35)
        // __imp_ variables are indirection pointers, so use malloc to simulate that
        void **imp_addr = (void**)malloc(sizeof(void**));
        *imp_addr = addr;
        addr = (void*)imp_addr;
#endif
    }
#endif // _OS_WINDOWS_

#ifdef USE_ORCJIT
    addComdat(gv);
    jl_ExecutionEngine->addGlobalMapping(name, (uintptr_t)addr);
#elif defined(USE_MCJIT)
    addComdat(gv);
    sys::DynamicLibrary::AddSymbol(name, addr);
#else // USE_MCJIT
    jl_ExecutionEngine->addGlobalMapping(gv, addr);
#endif // USE_MCJIT
}

static std::vector<Constant*> jl_sysimg_gvars;
static std::vector<Constant*> jl_sysimg_fvars;
typedef struct {Value *gv; int32_t index;} jl_value_llvm; // uses 1-based indexing
static std::map<void*, jl_value_llvm> jl_value_to_llvm;

// global variables to pointers are pretty common,
// so this method is available as a convenience for emitting them.
// for other types, the formula for implementation is straightforward:
// (see stringConstPtr, for an alternative example to the code below)
//
// if in imaging_mode, emit a GlobalVariable with the same name and an initializer to the shadow_module
// making it valid for emission and reloading in the sysimage
//
// then add a global mapping to the current value (usually from calloc'd space)
// to the execution engine to make it valid for the current session (with the current value)
static void* jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit = NULL)
{
    PointerType *T = cast<PointerType>(gv->getType()->getElementType()); // pointer is the only supported type here

    GlobalVariable *shadowvar = NULL;
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    if (imaging_mode)
#endif
        shadowvar = global_proto(gv, shadow_output);

    if (shadowvar) {
        shadowvar->setInitializer(ConstantPointerNull::get(T));
        shadowvar->setLinkage(GlobalVariable::InternalLinkage);
        addComdat(shadowvar);
        if (imaging_mode && gvarinit) {
            // make the pointer valid for future sessions
            jl_sysimg_gvars.push_back(ConstantExpr::getBitCast(shadowvar, T_psize));
            jl_value_llvm gv_struct;
            gv_struct.gv = global_proto(gv);
            gv_struct.index = jl_sysimg_gvars.size();
            jl_value_to_llvm[gvarinit] = gv_struct;
        }
    }

    // make the pointer valid for this session
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    void *slot = calloc(1, sizeof(void*));
    jl_ExecutionEngine->addGlobalMapping(gv->getName(), (uintptr_t)slot);
    return slot;
#else
    return jl_ExecutionEngine->getPointerToGlobal(shadowvar);
#endif
}

static void* jl_get_global(GlobalVariable *gv)
{
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    void *p = (void*)(intptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(
            jl_ExecutionEngine->getMangledName(gv));
#else
    void *p = jl_ExecutionEngine->getPointerToGlobal(
            shadow_output->getNamedValue(gv->getName()));
#endif
    assert(p);
    return p;
}

// clones the contents of the module `m` to the shadow_output collector
// in the old JIT, this is equivalent to also adding it to the execution engine
static void jl_add_to_shadow(Module *m)
{
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    if (!imaging_mode)
        return;
    ValueToValueMapTy VMap;
    std::unique_ptr<Module> clone(CloneModule(m, VMap));
    for (Module::iterator I = clone->begin(), E = clone->end(); I != E; ++I) {
        Function *F = &*I;
        if (!F->isDeclaration()) {
            F->setLinkage(Function::InternalLinkage);
            addComdat(F);
        }
    }
#else
    // on the old jit, the shadow_module is the same as the execution engine_module
    std::unique_ptr<Module> clone(m);
#endif
    jl_merge_module(shadow_output, std::move(clone));
}

#ifdef HAVE_CPUID
extern "C" {
    extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
}
#endif

static void jl_gen_llvm_globaldata(llvm::Module *mod, ValueToValueMapTy &VMap,
                                   const char *sysimg_data, size_t sysimg_len)
{
    ArrayType *gvars_type = ArrayType::get(T_psize, jl_sysimg_gvars.size());
    addComdat(new GlobalVariable(*mod,
                                 gvars_type,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 MapValue(ConstantArray::get(gvars_type, ArrayRef<Constant*>(jl_sysimg_gvars)), VMap),
                                 "jl_sysimg_gvars"));
    ArrayType *fvars_type = ArrayType::get(T_pvoidfunc, jl_sysimg_fvars.size());
    addComdat(new GlobalVariable(*mod,
                                 fvars_type,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 MapValue(ConstantArray::get(fvars_type, ArrayRef<Constant*>(jl_sysimg_fvars)), VMap),
                                 "jl_sysimg_fvars"));
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size, globalUnique+1),
                                 "jl_globalUnique"));
#ifdef JULIA_ENABLE_THREADING
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size, jltls_states_func_idx),
                                 "jl_ptls_states_getter_idx"));
#endif

    Constant *feature_string = ConstantDataArray::getString(jl_LLVMContext, jl_options.cpu_target);
    addComdat(new GlobalVariable(*mod,
                                 feature_string->getType(),
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 feature_string,
                                 "jl_sysimg_cpu_target"));

#ifdef HAVE_CPUID
    // For native also store the cpuid
    if (strcmp(jl_options.cpu_target,"native") == 0) {
        uint32_t info[4];

        jl_cpuid((int32_t*)info, 1);
        addComdat(new GlobalVariable(*mod,
                                     T_int64,
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     ConstantInt::get(T_int64,((uint64_t)info[2])|(((uint64_t)info[3])<<32)),
                                     "jl_sysimg_cpu_cpuid"));
    }
#endif

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(jl_LLVMContext, ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
        addComdat(new GlobalVariable(*mod, data->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data"));
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*mod, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"));
    }
}

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
static void jl_dump_shadow(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len,
                           bool dump_as_bc)
{
#ifdef JL_DEBUG_BUILD
    verifyModule(*shadow_output);
#endif

#ifdef LLVM36
    std::error_code err;
    StringRef fname_ref = StringRef(fname);
    raw_fd_ostream OS(fname_ref, err, sys::fs::F_None);
#elif defined(LLVM35)
    std::string err;
    raw_fd_ostream OS(fname, err, sys::fs::F_None);
#else
    std::string err;
    raw_fd_ostream OS(fname, err);
#endif
#ifdef LLVM37 // 3.7 simplified formatted output; just use the raw stream alone
    raw_fd_ostream& FOS(OS);
#else
    formatted_raw_ostream FOS(OS);
#endif

    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
#if defined(_OS_WINDOWS_) && defined(FORCE_ELF)
#ifdef LLVM35
    TheTriple.setObjectFormat(Triple::COFF);
#else
    TheTriple.setEnvironment(Triple::UnknownEnvironment);
#endif
#elif defined(_OS_DARWIN_) && defined(FORCE_ELF)
#ifdef LLVM35
    TheTriple.setObjectFormat(Triple::MachO);
#else
    TheTriple.setEnvironment(Triple::MachO);
#endif
#endif
#ifdef LLVM35
    std::unique_ptr<TargetMachine>
#else
    OwningPtr<TargetMachine>
#endif
    TM(jl_TargetMachine->getTarget().createTargetMachine(
        TheTriple.getTriple(),
        jl_TargetMachine->getTargetCPU(),
        jl_TargetMachine->getTargetFeatureString(),
        jl_TargetMachine->Options,
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        Reloc::PIC_,
#else
        jit_model ? Reloc::PIC_ : Reloc::Default,
#endif
        jit_model ? CodeModel::JITDefault : CodeModel::Default,
        CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
        ));

#ifdef LLVM38
    legacy::PassManager PM;
#else
    PassManager PM;
#endif
#ifndef LLVM37
    PM.add(new TargetLibraryInfo(Triple(TM->getTargetTriple())));
#else
    PM.add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
#endif


    // set up optimization passes
#ifdef LLVM37
    // No DataLayout pass needed anymore.
#elif defined(LLVM36)
    PM.add(new DataLayoutPass());
#elif defined(LLVM35)
    PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
    PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
#endif

    addOptimizationPasses(&PM);
    if (!dump_as_bc) {
        if (TM->addPassesToEmitFile(PM, FOS, TargetMachine::CGFT_ObjectFile, false)) {
            jl_error("Could not generate obj file for this target");
        }
    }

    // now copy the module, since PM.run may modify it
    ValueToValueMapTy VMap;
#ifdef LLVM38
    Module *clone = CloneModule(shadow_output, VMap).release();
#else
    Module *clone = CloneModule(shadow_output, VMap);
#endif
#ifdef LLVM37
    // Reset the target triple to make sure it matches the new target machine
    clone->setTargetTriple(TM->getTargetTriple().str());
#ifdef LLVM38
    clone->setDataLayout(TM->createDataLayout());
#else
    clone->setDataLayout(TM->getDataLayout()->getStringRepresentation());
#endif
#endif

    // add metadata information
    jl_gen_llvm_globaldata(clone, VMap, sysimg_data, sysimg_len);

    // do the actual work
    PM.run(*clone);
    if (dump_as_bc)
        WriteBitcodeToFile(clone, FOS);
    delete clone;
}

static int32_t jl_assign_functionID(Function *functionObject, int specsig)
{
    // give the function an index in the constant lookup table
    if (!imaging_mode)
        return 0;
    jl_sysimg_fvars.push_back(ConstantExpr::getBitCast(
                shadow_output->getNamedValue(functionObject->getName()),
                T_pvoidfunc));
    return jl_sysimg_fvars.size();
}

extern "C" int32_t jl_get_llvm_gv(jl_value_t *p)
{
    // map a jl_value_t memory location to a GlobalVariable
    std::map<void*, jl_value_llvm>::iterator it;
    it = jl_value_to_llvm.find(p);
    if (it == jl_value_to_llvm.end())
        return 0;
    return it->second.index;
}
