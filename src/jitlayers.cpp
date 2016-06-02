// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <iostream>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

// this defines the set of optimization passes defined for Julia at various optimization levels
template <class T>
static void addOptimizationPasses(T *PM)
{
#ifdef JL_DEBUG_BUILD
    PM->add(createVerifierPass());
#endif

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
    if (jl_options.opt_level == 0) {
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

#ifdef USE_POLLY
    polly::registerPollyPasses(*PM);
#endif

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
    PM->add(createMemCpyOptPass());            // Remove memcpy / form memset
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

#ifndef LLVM38
void notifyObjectLoaded(RTDyldMemoryManager *memmgr,
                        llvm::orc::ObjectLinkingLayerBase::ObjSetHandleT H);
#endif

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
}
// ------------------------ END OF TEMPORARY COPY FROM LLVM -----------------

#ifdef _OS_LINUX_
// Resolve non-lock free atomic functions in the libatomic library.
// This is the library that provides support for c11/c++11 atomic operations.
static uint64_t resolve_atomic(const char *name)
{
    static void *atomic_hdl = jl_load_dynamic_library_e("libatomic",
                                                        JL_RTLD_LOCAL);
    static const char *const atomic_prefix = "__atomic_";
    if (!atomic_hdl)
        return 0;
    if (strncmp(name, atomic_prefix, strlen(atomic_prefix)) != 0)
        return 0;
    return (uintptr_t)jl_dlsym_e(atomic_hdl, name);
}
#endif

// A simplified model of the LLVM ExecutionEngine that implements only the methods that Julia needs
// but tries to roughly match the API anyways so that compatibility is easier
class JuliaOJIT {
    // Custom object emission notification handler for the JuliaOJIT
    // TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
    class DebugObjectRegistrar {
    public:
        DebugObjectRegistrar(JuliaOJIT &JIT)
            : JuliaListener(CreateJuliaJITEventListener()),
              JIT(JIT) {}

        template <typename ObjSetT, typename LoadResult>
        void operator()(ObjectLinkingLayerBase::ObjSetHandleT H, const ObjSetT &Objects,
                        const LoadResult &LOS)
        {
#ifndef LLVM38
            notifyObjectLoaded(JIT.MemMgr, H);
#endif
            auto oit = Objects.begin();
            auto lit = LOS.begin();
            for (; oit != Objects.end(); ++oit, ++lit) {
#ifdef LLVM39
                const auto &Object = (*oit)->getBinary();
#else
                auto &Object = *oit;
#endif
                auto &LO = *lit;

                OwningBinary<object::ObjectFile> SavedObject = LO->getObjectForDebug(*Object);

                // If the debug object is unavailable, save (a copy of) the original object
                // for our backtraces
                if (!SavedObject.getBinary()) {
                    // This is unfortunate, but there doesn't seem to be a way to take
                    // ownership of the original buffer
                    auto NewBuffer = MemoryBuffer::getMemBufferCopy(Object->getData(), Object->getFileName());
                    auto NewObj = ObjectFile::createObjectFile(NewBuffer->getMemBufferRef());
                    assert(NewObj);
                    SavedObject = OwningBinary<object::ObjectFile>(std::move(*NewObj),std::move(NewBuffer));
                }
                else {
                    NotifyGDB(SavedObject);
                }

                SavedObjects.push_back(std::move(SavedObject));

                ORCNotifyObjectEmitted(JuliaListener.get(),
                        *Object,
                        *SavedObjects.back().getBinary(),
                        *LO, JIT.MemMgr);

                // record all of the exported symbols defined in this object
                // in the primary hash table for the enclosing JIT
                for (auto &Symbol : Object->symbols()) {
                    auto Flags = Symbol.getFlags();
                    if (Flags & object::BasicSymbolRef::SF_Undefined)
                        continue;
                    if (!(Flags & object::BasicSymbolRef::SF_Exported))
                        continue;
                    auto NameOrError = Symbol.getName();
                    assert(NameOrError);
                    auto Name = NameOrError.get();
                    orc::JITSymbol Sym = JIT.CompileLayer.findSymbolIn(H, Name, true);
                    assert(Sym);
                    // note: calling getAddress here eagerly finalizes H
                    // as an alternative, we could store the JITSymbol instead
                    // (which would present a lazy-initializer functor interface instead)
                    JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)Sym.getAddress();
                }
            }
        }

    private:
        void NotifyGDB(OwningBinary<object::ObjectFile> &DebugObj)
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

        std::vector<OwningBinary<object::ObjectFile>> SavedObjects;
        std::unique_ptr<JITEventListener> JuliaListener;
        JuliaOJIT &JIT;
    };

public:
    typedef orc::ObjectLinkingLayer<DebugObjectRegistrar> ObjLayerT;
    typedef orc::IRCompileLayer<ObjLayerT> CompileLayerT;
    typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;
    typedef StringMap<void*> SymbolTableT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;

    JuliaOJIT(TargetMachine &TM)
      : TM(TM),
        DL(TM.createDataLayout()),
        ObjStream(ObjBufferSV),
        MemMgr(createRTDyldMemoryManager()),
        ObjectLayer(DebugObjectRegistrar(*this)),
        CompileLayer(
                ObjectLayer,
                [this](Module &M) {
                    JL_TIMING(LLVM_OPT);
                    PM.run(M);
                    std::unique_ptr<MemoryBuffer> ObjBuffer(
                        new ObjectMemoryBuffer(std::move(ObjBufferSV)));
                    auto Obj = object::ObjectFile::createObjectFile(ObjBuffer->getMemBufferRef());

                    if (!Obj) {
                        M.dump();
#ifdef LLVM39
                        std::string Buf;
                        raw_string_ostream OS(Buf);
                        logAllUnhandledErrors(Obj.takeError(), OS, "");
                        OS.flush();
                        llvm::report_fatal_error("FATAL: Unable to compile LLVM Module: '" + Buf + "'\n"
                            "The module's content was printed above. Please file a bug report");
#else
                        llvm::report_fatal_error("FATAL: Unable to compile LLVM Module.\n"
                            "The module's content was printed above. Please file a bug report");
#endif
                    }

                    return OwningObj(std::move(*Obj), std::move(ObjBuffer));
                }
            )
        {
            addOptimizationPasses(&PM);
            if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
                llvm_unreachable("Target does not support MC emission.");

            // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
            // symbols in the program as well. The nullptr argument to the function
            // tells DynamicLibrary to load the program, not a library.
            std::string *ErrorStr = nullptr;
            if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, ErrorStr))
                report_fatal_error("FATAL: unable to dlopen self\n" + *ErrorStr);
        }

    void addGlobalMapping(StringRef Name, uint64_t Addr)
    {
        bool successful = GlobalSymbolTable.insert(std::make_pair(Name, (void*)Addr)).second;
        (void)successful;
        assert(successful);
    }

    void addGlobalMapping(const GlobalValue *GV, void *Addr)
    {
        addGlobalMapping(getMangledName(GV), (uintptr_t)Addr);
    }

    void *getPointerToGlobalIfAvailable(StringRef S)
    {
        SymbolTableT::const_iterator pos = GlobalSymbolTable.find(S);
        if (pos != GlobalSymbolTable.end())
            return pos->second;
        return nullptr;
    }

    void *getPointerToGlobalIfAvailable(const GlobalValue *GV)
    {
        return getPointerToGlobalIfAvailable(getMangledName(GV));
    }


    void addModule(std::unique_ptr<Module> M)
    {
#ifndef NDEBUG
        // validate the relocations for M
        for (Module::iterator I = M->begin(), E = M->end(); I != E; ) {
            Function *F = &*I;
            ++I;
            if (F->isDeclaration()) {
                if (F->use_empty())
                    F->eraseFromParent();
                else if (!(F->isIntrinsic() ||
                           findUnmangledSymbol(F->getName()) ||
                           SectionMemoryManager::getSymbolAddressInProcess(
                               F->getName()))) {
                    std::cerr << "FATAL ERROR: "
                              << "Symbol \"" << F->getName().str() << "\""
                              << "not found";
                    abort();
                }
            }
        }
#endif
        JL_TIMING(LLVM_MODULE_FINISH);
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
#ifdef _OS_LINUX_
                            if (uint64_t addr = resolve_atomic(Name.c_str()))
                                return RuntimeDyld::SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
                            // Return failure code
                            return RuntimeDyld::SymbolInfo(nullptr);
                          },
                          [](const std::string &S) { return nullptr; }
                        );
        SmallVector<std::unique_ptr<Module>,1> Ms;
        Ms.push_back(std::move(M));
        auto modset = CompileLayer.addModuleSet(std::move(Ms), MemMgr,
                                                std::move(Resolver));
        // Force LLVM to emit the module so that we can register the symbols
        // in our lookup table.
        CompileLayer.emitAndFinalize(modset);
    }

    void removeModule(ModuleHandleT H)
    {
        CompileLayer.removeModuleSet(H);
    }

    orc::JITSymbol findSymbol(const std::string &Name, bool ExportedSymbolsOnly)
    {
        void *Addr = nullptr;
        if (ExportedSymbolsOnly) {
            // Step 1: Check against list of known external globals
            Addr = getPointerToGlobalIfAvailable(Name);
        }
        // Step 2: Search all previously emitted symbols
        if (Addr == nullptr)
            Addr = LocalSymbolTable[Name];
        return orc::JITSymbol((uintptr_t)Addr, JITSymbolFlags::Exported);
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
        return shadow_output->getFunction(Name);
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
    CompileLayerT CompileLayer;
    SymbolTableT GlobalSymbolTable;
    SymbolTableT LocalSymbolTable;
};

#endif

#ifdef USE_ORCJIT
JuliaOJIT *jl_ExecutionEngine;
#else
ExecutionEngine *jl_ExecutionEngine;
#endif

// MSVC's link.exe requires each function declaration to have a Comdat section
// So rather than litter the code with conditionals,
// all global values that get emitted call this function
// and it decides whether the definition needs a Comdat section and adds the appropriate declaration
// TODO: consider moving this into jl_add_to_shadow or jl_dump_shadow? the JIT doesn't care, so most calls are now no-ops
template<class T> // for GlobalObject's
static T *addComdat(T *G)
{
#if defined(_OS_WINDOWS_) && defined(LLVM35)
    if (imaging_mode && !G->isDeclaration()) {
        // Add comdat information to make MSVC link.exe happy
        // it's valid to emit this for ld.exe too,
        // but makes it very slow to link for no benefit
        if (G->getParent() == shadow_output) {
#if defined(_COMPILER_MICROSOFT_)
            Comdat *jl_Comdat = G->getParent()->getOrInsertComdat(G->getName());
            // ELF only supports Comdat::Any
            jl_Comdat->setSelectionKind(Comdat::NoDuplicates);
            G->setComdat(jl_Comdat);
#endif
#if defined(_CPU_X86_64_)
            // Add unwind exception personalities to functions to handle async exceptions
            assert(!juliapersonality_func || juliapersonality_func->getParent() == shadow_output);
            if (Function *F = dyn_cast<Function>(G))
                F->setPersonalityFn(juliapersonality_func);
#endif
        }
        // add __declspec(dllexport) to everything marked for export
        if (G->getLinkage() == GlobalValue::ExternalLinkage)
            G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
        else
            G->setDLLStorageClass(GlobalValue::DefaultStorageClass);
    }
#endif
    return G;
}

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
        // Replace a declaration with the definition:
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
        // Reparent the global variable:
        sG->removeFromParent();
        dest->getGlobalList().push_back(sG);
        // Comdat is owned by the Module, recreate it in the new parent:
        addComdat(sG);
    }

    for (Module::iterator I = src->begin(), E = src->end(); I != E;) {
        Function *sG = &*I;
        GlobalValue *dG = dest->getNamedValue(sG->getName());
        ++I;
        // Replace a declaration with the definition:
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
        // Reparent the global variable:
        sG->removeFromParent();
        dest->getFunctionList().push_back(sG);
        // Comdat is owned by the Module, recreate it in the new parent:
        addComdat(sG);
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
static void jl_finalize_function(const std::string &F, Module *collector = NULL)
{
    std::unique_ptr<Module> m(module_for_fname.lookup(F));
    if (m) {
        // probably not many unresolved declarations, but be sure iterate over their Names,
        // since the declarations may get destroyed by the jl_merge_module call.
        // this is also why we copy the Name string, rather than save a StringRef
        SmallVector<std::string, 8> to_finalize;
        for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
            Function *F = &*I;
            if (!F->isDeclaration()) {
                module_for_fname.erase(F->getName());
            }
            else if (!F->isIntrinsic()) {
                to_finalize.push_back(F->getName().str());
            }
        }

        for (const auto F : to_finalize) {
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
    jl_finalize_function(F->getName().str(), collector);
}
#endif

// Connect Modules via prototypes, each owned by module `M`
static GlobalVariable *global_proto(GlobalVariable *G, Module *M = NULL)
{
    // Copy the GlobalVariable, but without the initializer, so it becomes a declaration
    GlobalVariable *proto = new GlobalVariable(G->getType()->getElementType(),
            G->isConstant(), GlobalVariable::ExternalLinkage,
            NULL, G->getName(),  G->getThreadLocalMode());
    proto->copyAttributesFrom(G);
#ifdef LLVM35
    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    proto->setDLLStorageClass(GlobalValue::DefaultStorageClass);
#endif
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
    // FunctionType does not include any attributes. Copy them over manually
    // as codegen may make decisions based on the presence of certain attributes
    NewF->copyAttributesFrom(F);

#ifdef LLVM37
    // Declarations are not allowed to have personality routines, but
    // copyAttributesFrom sets them anyway, so clear them again manually
    NewF->setPersonalityFn(nullptr);
#endif

#ifdef LLVM35
    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    NewF->setDLLStorageClass(GlobalValue::DefaultStorageClass);
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

#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
#ifdef LLVM35
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
#else
        gv->setLinkage(GlobalValue::DLLImportLinkage);
#if defined(_P64)
        // __imp_ variables are indirection pointers, so use malloc to simulate that
        void **imp_addr = (void**)malloc(sizeof(void*));
        *imp_addr = addr;
        addr = (void*)imp_addr;
#endif
#endif
    }
#endif // _OS_WINDOWS_

    jl_ExecutionEngine->addGlobalMapping(gv, addr);
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
    jl_ExecutionEngine->addGlobalMapping(gv, slot);
    return slot;
#else
    return jl_ExecutionEngine->getPointerToGlobal(shadowvar);
#endif
}

#ifdef JULIA_ENABLE_THREADING
// Emit a slot in the system image to be filled at sysimg init time.
// Returns the global var. Fill `idx` with 1-base index in the sysimg gv.
// Use as an optimization for runtime constant addresses to have one less
// load. (Used only by threading).
static GlobalVariable *jl_emit_sysimg_slot(Module *m, Type *typ, const char *name,
                                           uintptr_t init, size_t &idx)
{
    assert(imaging_mode);
    // This is **NOT** a external variable or a normal global variable
    // This is a special internal global slot with a special index
    // in the global variable table.
    GlobalVariable *gv = new GlobalVariable(*m, typ, false,
                                            GlobalVariable::InternalLinkage,
                                            ConstantPointerNull::get((PointerType*)typ), name);
    addComdat(gv);
    // make the pointer valid for this session
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    auto p = new uintptr_t(init);
    jl_ExecutionEngine->addGlobalMapping(gv, (void*)p);
#else
    uintptr_t *p = (uintptr_t*)jl_ExecutionEngine->getPointerToGlobal(gv);
    *p = init;
#endif
    jl_sysimg_gvars.push_back(ConstantExpr::getBitCast(gv, T_psize));
    idx = jl_sysimg_gvars.size();
    return gv;
}
#endif

static void* jl_get_global(GlobalVariable *gv)
{
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    void *p = (void*)(intptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(gv);
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
#ifndef KEEP_BODIES
    if (!imaging_mode)
        return;
#endif
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
extern "C"
void jl_dump_native(const char *bc_fname, const char *obj_fname, const char *sysimg_data, size_t sysimg_len)
{
    assert(imaging_mode);
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
#elif defined(LLVM39)
        Optional<Reloc::Model>(),
#else
        Reloc::Default,
#endif
        CodeModel::Default,
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

    std::unique_ptr<raw_fd_ostream> bc_OS;
    std::unique_ptr<raw_fd_ostream> obj_OS;
#ifdef LLVM37 // 3.7 simplified formatted output; just use the raw stream alone
    std::unique_ptr<raw_fd_ostream> &bc_FOS = bc_OS;
    std::unique_ptr<raw_fd_ostream> &obj_FOS = obj_OS;
#else
    std::unique_ptr<formatted_raw_ostream> bc_FOS;
    std::unique_ptr<formatted_raw_ostream> obj_FOS;
#endif

    if (bc_fname) {
#if defined(LLVM35)
        // call output handler directly to avoid special case handling of `-` filename
        int FD;
        std::error_code EC = sys::fs::openFileForWrite(bc_fname, FD, sys::fs::F_None);
        bc_OS.reset(new raw_fd_ostream(FD, true));
        std::string err;
        if (EC)
            err = "ERROR: failed to open --output-bc file '" + std::string(bc_fname) + "': " + EC.message();
#else
        std::string err;
        bc_OS.reset(new raw_fd_ostream(bc_fname, err, raw_fd_ostream::F_Binary));
#endif
        if (!err.empty())
            jl_safe_printf("%s\n", err.c_str());
        else {
#ifndef LLVM37
            bc_FOS.reset(new formatted_raw_ostream(*bc_OS.get()));
#endif
            PM.add(createBitcodeWriterPass(*bc_FOS.get()));     // Unroll small loops
        }
    }

    if (obj_fname) {
#if defined(LLVM35)
        // call output handler directly to avoid special case handling of `-` filename
        int FD;
        std::error_code EC = sys::fs::openFileForWrite(obj_fname, FD, sys::fs::F_None);
        obj_OS.reset(new raw_fd_ostream(FD, true));
        std::string err;
        if (EC)
            err = "ERROR: failed to open --output-o file '" + std::string(obj_fname) + "': " + EC.message();
#else
        std::string err;
        obj_OS.reset(new raw_fd_ostream(obj_fname, err, raw_fd_ostream::F_Binary));
#endif
        if (!err.empty())
            jl_safe_printf("%s\n", err.c_str());
        else {
#ifndef LLVM37
            obj_FOS.reset(new formatted_raw_ostream(*obj_OS.get()));
#endif
            if (TM->addPassesToEmitFile(PM, *obj_FOS.get(), TargetMachine::CGFT_ObjectFile, false)) {
                jl_safe_printf("ERROR: target does not support generation of object files\n");
            }
        }
    }

    ValueToValueMapTy VMap;
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    // now copy the module (if using the old JIT), since PM.run may modify it
    Module *clone = shadow_output;
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
#if !defined(USE_MCJIT) && !defined(USE_ORCJIT)
    delete clone;
#endif
    imaging_mode = false;
}

extern "C" int32_t jl_assign_functionID(void *function)
{
    // give the function an index in the constant lookup table
    assert(imaging_mode);
    if (function == NULL)
        return 0;
    jl_sysimg_fvars.push_back(ConstantExpr::getBitCast(
                shadow_output->getNamedValue(((Function*)function)->getName()),
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
