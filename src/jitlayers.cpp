// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#include <iostream>
#include <sstream>

// analysis passes
#include <llvm/Analysis/Passes.h>
#if JL_LLVM_VERSION >= 30800
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#endif
#if JL_LLVM_VERSION >= 30700
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#endif
#if JL_LLVM_VERSION >= 30500
#include <llvm/IR/Verifier.h>
#else
#include <llvm/Analysis/Verifier.h>
#endif
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/LinkAllPasses.h>
#include <polly/CodeGen/CodegenCleanup.h>
#endif

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Vectorize.h>
#if JL_LLVM_VERSION >= 30900
#include <llvm/Transforms/Scalar/GVN.h>
#endif
#if JL_LLVM_VERSION >= 40000
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#endif

namespace llvm {
    extern Pass *createLowerSimdLoopPass();
}

#if JL_LLVM_VERSION >= 40000
#  include <llvm/Bitcode/BitcodeWriter.h>
#else
#  include <llvm/Bitcode/ReaderWriter.h>
#endif
#if JL_LLVM_VERSION >= 30500
#include <llvm/Bitcode/BitcodeWriterPass.h>
#endif

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/ExecutionEngine/JITEventListener.h>

// target support
#include <llvm/ADT/Triple.h>
#include <llvm/Support/TargetRegistry.h>
#if JL_LLVM_VERSION < 30700
#include <llvm/Target/TargetLibraryInfo.h>
#endif
#include <llvm/IR/DataLayout.h>
#include <llvm/Support/DynamicLibrary.h>


#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/ADT/SmallSet.h>

using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#ifdef USE_MCJIT
RTDyldMemoryManager* createRTDyldMemoryManager(void);
#endif

static Type *T_void;
static IntegerType *T_uint32;
static IntegerType *T_uint64;
static IntegerType *T_size;
static Type *T_psize;
static Type *T_pvoidfunc;
static Type *T_pjlvalue;
void jl_init_jit(Type *T_pjlvalue_)
{
    T_void = Type::getVoidTy(jl_LLVMContext);
    T_uint32 = Type::getInt32Ty(jl_LLVMContext);
    T_uint64 = Type::getInt64Ty(jl_LLVMContext);
    if (sizeof(size_t) == 8)
        T_size = T_uint64;
    else
        T_size = T_uint32;
    T_psize = PointerType::get(T_size, 0);
    T_pvoidfunc = FunctionType::get(T_void, /*isVarArg*/false)->getPointerTo();
    T_pjlvalue = T_pjlvalue_;
}

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

// this defines the set of optimization passes defined for Julia at various optimization levels
#if JL_LLVM_VERSION >= 30700
void addOptimizationPasses(legacy::PassManager *PM)
#else
void addOptimizationPasses(PassManager *PM)
#endif
{
    PM->add(createLowerGCFramePass());
#ifdef JL_DEBUG_BUILD
    PM->add(createVerifierPass());
#endif

#if defined(JL_ASAN_ENABLED)
#   if JL_LLVM_VERSION >= 30700 && JL_LLVM_VERSION < 30800
    // LLVM 3.7 BUG: ASAN pass doesn't properly initialize its dependencies
    initializeTargetLibraryInfoWrapperPassPass(*PassRegistry::getPassRegistry());
#   endif
    PM->add(createAddressSanitizerFunctionPass());
#endif
#if defined(JL_MSAN_ENABLED)
    PM->add(llvm::createMemorySanitizerPass(true));
#endif
    if (jl_options.opt_level == 0) {
        PM->add(createLowerPTLSPass(imaging_mode));
        return;
    }
#if JL_LLVM_VERSION >= 30700
    PM->add(createTargetTransformInfoWrapperPass(jl_TargetMachine->getTargetIRAnalysis()));
#else
    jl_TargetMachine->addAnalysisPasses(*PM);
#endif
#if JL_LLVM_VERSION >= 30800
    PM->add(createTypeBasedAAWrapperPass());
#else
    PM->add(createTypeBasedAliasAnalysisPass());
#endif
    if (jl_options.opt_level >= 3) {
#if JL_LLVM_VERSION >= 30800
        PM->add(createBasicAAWrapperPass());
#else
        PM->add(createBasicAliasAnalysisPass());
#endif
    }
    // list of passes from vmkit
    PM->add(createCFGSimplificationPass()); // Clean up disgusting code
    PM->add(createPromoteMemoryToRegisterPass());// Kill useless allocas
#if JL_LLVM_VERSION >= 40000
    PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline
#else
    PM->add(createAlwaysInlinerPass()); // Respect always_inline
#endif

#ifndef INSTCOMBINE_BUG
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
#endif
    // Let the InstCombine pass remove the unnecessary load of
    // safepoint address first
    PM->add(createLowerPTLSPass(imaging_mode));
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
#ifdef USE_POLLY
    // LCSSA (which has already run at this point due to the dependencies of the
    // above passes) introduces redundant phis that hinder Polly. Therefore we
    // run InstCombine here to remove them.
    PM->add(createInstructionCombiningPass());
    PM->add(polly::createCodePreparationPass());
    polly::registerPollyPasses(*PM);
    PM->add(polly::createCodegenCleanupPass());
#endif
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
#if JL_LLVM_VERSION >= 30500
    PM->add(createSimpleLoopUnrollPass());     // Unroll small loops
#else
    PM->add(createLoopUnrollPass());           // Unroll small loops
#endif
#if JL_LLVM_VERSION < 30500 && !defined(INSTCOMBINE_BUG)
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
#if JL_LLVM_VERSION >= 30500
    if (jl_options.opt_level >= 3) {
        PM->add(createSLPVectorizerPass());     // Vectorize straight-line code
    }
#endif

    PM->add(createAggressiveDCEPass());         // Delete dead instructions
#if JL_LLVM_VERSION >= 30500
    if (jl_options.opt_level >= 3)
        PM->add(createInstructionCombiningPass());   // Clean up after SLP loop vectorizer
    PM->add(createLoopVectorizePass());         // Vectorize loops
    PM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer
#endif
    //PM->add(createCFGSimplificationPass());     // Merge & remove BBs
}

#ifdef USE_ORCJIT

#if JL_LLVM_VERSION < 30800
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

#if defined(_OS_LINUX_)
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

// Custom object emission notification handler for the JuliaOJIT
extern JITEventListener *CreateJuliaJITEventListener();
JuliaOJIT::DebugObjectRegistrar::DebugObjectRegistrar(JuliaOJIT &JIT)
    : JuliaListener(CreateJuliaJITEventListener()),
      JIT(JIT) {}

JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const object::ObjectFile &debugObj,
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr);

// TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
template <typename ObjSetT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::operator()(ObjectLinkingLayerBase::ObjSetHandleT H,
                const ObjSetT &Objects, const LoadResult &LOS)
{
#if JL_LLVM_VERSION < 30800
    notifyObjectLoaded(JIT.MemMgr, H);
#endif
    auto oit = Objects.begin();
    auto lit = LOS.begin();
    for (; oit != Objects.end(); ++oit, ++lit) {
#if JL_LLVM_VERSION >= 30900
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
            auto NewBuffer = MemoryBuffer::getMemBufferCopy(Object->getData(),
                                                            Object->getFileName());
            auto NewObj = ObjectFile::createObjectFile(NewBuffer->getMemBufferRef());
            assert(NewObj);
            SavedObject = OwningBinary<object::ObjectFile>(std::move(*NewObj),
                                                           std::move(NewBuffer));
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
            auto Sym = JIT.CompileLayer.findSymbolIn(H, Name, true);
            assert(Sym);
            // note: calling getAddress here eagerly finalizes H
            // as an alternative, we could store the JITSymbol instead
            // (which would present a lazy-initializer functor interface instead)
            JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)Sym.getAddress();
        }
    }
}

void JuliaOJIT::DebugObjectRegistrar::NotifyGDB(OwningBinary<object::ObjectFile> &DebugObj)
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

JuliaOJIT::JuliaOJIT(TargetMachine &TM)
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
#if JL_LLVM_VERSION >= 30900
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
    if (!jl_generating_output()) {
        addOptimizationPasses(&PM);
    }
    else {
        PM.add(createLowerGCFramePass());
        PM.add(createLowerPTLSPass(imaging_mode));
    }
    if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
        llvm_unreachable("Target does not support MC emission.");

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    std::string *ErrorStr = nullptr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, ErrorStr))
        report_fatal_error("FATAL: unable to dlopen self\n" + *ErrorStr);
}

void JuliaOJIT::addGlobalMapping(StringRef Name, uint64_t Addr)
{
    bool successful = GlobalSymbolTable.insert(std::make_pair(Name, (void*)Addr)).second;
    (void)successful;
    assert(successful);
}

void JuliaOJIT::addGlobalMapping(const GlobalValue *GV, void *Addr)
{
    addGlobalMapping(getMangledName(GV), (uintptr_t)Addr);
}

void *JuliaOJIT::getPointerToGlobalIfAvailable(StringRef S)
{
    SymbolTableT::const_iterator pos = GlobalSymbolTable.find(S);
    if (pos != GlobalSymbolTable.end())
        return pos->second;
    return nullptr;
}

void *JuliaOJIT::getPointerToGlobalIfAvailable(const GlobalValue *GV)
{
    return getPointerToGlobalIfAvailable(getMangledName(GV));
}


void JuliaOJIT::addModule(std::unique_ptr<Module> M)
{
#ifndef NDEBUG
    // validate the relocations for M
    for (Module::iterator I = M->begin(), E = M->end(); I != E; ) {
        Function *F = &*I;
        ++I;
        if (F->isDeclaration()) {
            if (F->use_empty())
                F->eraseFromParent();
            else if (!(isIntrinsicFunction(F) ||
                       findUnmangledSymbol(F->getName()) ||
                       SectionMemoryManager::getSymbolAddressInProcess(
                           getMangledName(F->getName())))) {
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
                        if (auto Sym = findSymbol(Name, true)) {
#if JL_LLVM_VERSION >= 40000
                            // `findSymbol` already eagerly resolved the address
                            // return it directly.
                            return Sym;
#else
                            return RuntimeDyld::SymbolInfo(Sym.getAddress(),
                                                           Sym.getFlags());
#endif
                        }
                        // Step 2: Search the program symbols
                        if (uint64_t addr = SectionMemoryManager::getSymbolAddressInProcess(Name))
                            return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#if defined(_OS_LINUX_)
                        if (uint64_t addr = resolve_atomic(Name.c_str()))
                            return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
                        // Return failure code
                        return JL_SymbolInfo(nullptr);
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

void JuliaOJIT::removeModule(ModuleHandleT H)
{
    CompileLayer.removeModuleSet(H);
}

JL_JITSymbol JuliaOJIT::findSymbol(const std::string &Name, bool ExportedSymbolsOnly)
{
    void *Addr = nullptr;
    if (ExportedSymbolsOnly) {
        // Step 1: Check against list of known external globals
        Addr = getPointerToGlobalIfAvailable(Name);
    }
    // Step 2: Search all previously emitted symbols
    if (Addr == nullptr)
        Addr = LocalSymbolTable[Name];
    return JL_JITSymbol((uintptr_t)Addr, JITSymbolFlags::Exported);
}

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(const std::string Name)
{
    return findSymbol(getMangledName(Name), true);
}

uint64_t JuliaOJIT::getGlobalValueAddress(const std::string &Name)
{
    return findSymbol(getMangledName(Name), false).getAddress();
}

uint64_t JuliaOJIT::getFunctionAddress(const std::string &Name)
{
    return findSymbol(getMangledName(Name), false).getAddress();
}

Function *JuliaOJIT::FindFunctionNamed(const std::string &Name)
{
    return shadow_output->getFunction(Name);
}

void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    // TODO
}

const DataLayout& JuliaOJIT::getDataLayout() const
{
    return DL;
}

const Triple& JuliaOJIT::getTargetTriple() const
{
    return TM.getTargetTriple();
}

std::string JuliaOJIT::getMangledName(const std::string &Name)
{
    SmallString<128> FullName;
    Mangler::getNameWithPrefix(FullName, Name, DL);
    return FullName.str();
}

std::string JuliaOJIT::getMangledName(const GlobalValue *GV)
{
    return getMangledName(GV->getName());
}

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
#if defined(_OS_WINDOWS_) && JL_LLVM_VERSION >= 30500
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
#if JL_LLVM_VERSION >= 30500
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

// to finalize a function, look up its name in the `module_for_fname` map of
// unfinalized functions and merge it, plus any other modules it depends upon,
// into `collector` then add `collector` to the execution engine
static StringMap<Module*> module_for_fname;
static void jl_merge_recursive(Module *m, Module *collector);

#if defined(USE_MCJIT) || defined(USE_ORCJIT)
static void jl_add_to_ee(std::unique_ptr<Module> m)
{
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_) && JL_LLVM_VERSION >= 30500
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
#if JL_LLVM_VERSION >= 30600
    jl_ExecutionEngine->addModule(std::move(m));
#else
    jl_ExecutionEngine->addModule(m.release());
#endif
}

void jl_finalize_function(Function *F)
{
    std::unique_ptr<Module> m(module_for_fname.lookup(F->getName()));
    if (m) {
        jl_merge_recursive(m.get(), m.get());
        jl_add_to_ee(std::move(m));
    }
}
#else
static bool jl_try_finalize(Module *m)
{
    for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
        Function *F = &*I;
        if (F->isDeclaration() && !isIntrinsicFunction(F)) {
            if (!jl_can_finalize_function(F))
                return false;
        }
    }
    jl_merge_recursive(m, shadow_output);
    jl_merge_module(shadow_output, std::unique_ptr<Module>(m));
    return true;
}
#endif

static void jl_finalize_function(const std::string &F, Module *collector)
{
    std::unique_ptr<Module> m(module_for_fname.lookup(F));
    if (m) {
        jl_merge_recursive(m.get(), collector);
        jl_merge_module(collector, std::move(m));
    }
}

static void jl_merge_recursive(Module *m, Module *collector)
{
    // probably not many unresolved declarations, but be sure to iterate over their Names,
    // since the declarations may get destroyed by the jl_merge_module call.
    // this is also why we copy the Name string, rather than save a StringRef
    SmallVector<std::string, 8> to_finalize;
    for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
        Function *F = &*I;
        if (!F->isDeclaration()) {
            module_for_fname.erase(F->getName());
        }
        else if (!isIntrinsicFunction(F)) {
            to_finalize.push_back(F->getName().str());
        }
    }

    for (const auto F : to_finalize) {
        jl_finalize_function(F, collector);
    }
}

// see if any of the functions needed by F are still WIP
static StringSet<> incomplete_fname;
static bool jl_can_finalize_function(StringRef F, SmallSet<Module*, 16> &known)
{
    if (incomplete_fname.find(F) != incomplete_fname.end())
        return false;
    Module *M = module_for_fname.lookup(F);
#if JL_LLVM_VERSION >= 30500
    if (M && known.insert(M).second)
#else
    if (M && known.insert(M))
#endif
    {
        for (Module::iterator I = M->begin(), E = M->end(); I != E; ++I) {
            Function *F = &*I;
            if (F->isDeclaration() && !isIntrinsicFunction(F)) {
                if (!jl_can_finalize_function(F->getName(), known))
                    return false;
            }
        }
    }
    return true;
}
bool jl_can_finalize_function(Function *F)
{
    SmallSet<Module*, 16> known;
    return jl_can_finalize_function(F->getName(), known);
}

// let the JIT know this function is a WIP
void jl_init_function(Function *F)
{
    incomplete_fname.insert(F->getName());
}

// this takes ownership of a module after code emission is complete
// and will add it to the execution engine when required (by jl_finalize_function)
void jl_finalize_module(Module *m, bool shadow)
{
#if !defined(USE_ORCJIT)
    jl_globalPM->run(*m);
#endif
    // record the function names that are part of this Module
    // so it can be added to the JIT when needed
    for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
        Function *F = &*I;
        if (!F->isDeclaration()) {
            bool known = incomplete_fname.erase(F->getName());
            (void)known; // TODO: assert(known); // llvmcall gets this wrong
            module_for_fname[F->getName()] = m;
        }
    }
#if defined(USE_ORCJIT) || defined(USE_MCJIT)
    // in the newer JITs, the shadow module is separate from the execution module
    if (shadow)
        jl_add_to_shadow(m);
#else
    bool changes = jl_try_finalize(m);
    while (changes) {
        // this definitely isn't the most efficient, but it's only for the old LLVM 3.3 JIT
        changes = false;
        for (StringMap<Module*>::iterator MI = module_for_fname.begin(), ME = module_for_fname.end(); MI != ME; ++MI) {
            changes |= jl_try_finalize(MI->second);
        }
    }
#endif
}

// helper function for adding a DLLImport (dlsym) address to the execution engine
// (for values created locally or in the sysimage, jl_emit_and_add_to_shadow is generally preferable)
#if JL_LLVM_VERSION >= 30500
void add_named_global(GlobalObject *gv, void *addr, bool dllimport)
#else
void add_named_global(GlobalValue *gv, void *addr, bool dllimport)
#endif
{
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
#if JL_LLVM_VERSION >= 30500
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
void* jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit)
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

// Emit a slot in the system image to be filled at sysimg init time.
// Returns the global var. Fill `idx` with 1-base index in the sysimg gv.
// Use as an optimization for runtime constant addresses to have one less
// load. (Used only by threading).
GlobalVariable *jl_emit_sysimg_slot(Module *m, Type *typ, const char *name,
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

void* jl_get_global(GlobalVariable *gv)
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
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
void jl_add_to_shadow(Module *m)
{
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
    jl_merge_module(shadow_output, std::move(clone));
}
#endif

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
                                     T_uint64,
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     ConstantInt::get(T_uint64,((uint64_t)info[2])|(((uint64_t)info[3])<<32)),
                                     "jl_sysimg_cpu_cpuid"));
    }
#endif

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(jl_LLVMContext,
            ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
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
    JL_TIMING(NATIVE_DUMP);
    assert(imaging_mode);
    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
    // make sure to emit the native object format, even if FORCE_ELF was set in codegen
#if defined(_OS_WINDOWS_)
#if JL_LLVM_VERSION >= 30500
    TheTriple.setObjectFormat(Triple::COFF);
#else
    TheTriple.setEnvironment(Triple::UnknownEnvironment);
#endif
#elif defined(_OS_DARWIN_)
#if JL_LLVM_VERSION >= 30500
    TheTriple.setObjectFormat(Triple::MachO);
#else
    TheTriple.setEnvironment(Triple::MachO);
#endif
#endif
#if JL_LLVM_VERSION >= 30500
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
#elif JL_LLVM_VERSION >= 30900
        Optional<Reloc::Model>(),
#else
        Reloc::Default,
#endif
        CodeModel::Default,
        CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
        ));

#if JL_LLVM_VERSION >= 30700
    legacy::PassManager PM;
#else
    PassManager PM;
#endif
#if JL_LLVM_VERSION < 30700
    PM.add(new TargetLibraryInfo(Triple(TM->getTargetTriple())));
#else
    PM.add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
#endif


    // set up optimization passes
#if JL_LLVM_VERSION >= 30700
    // No DataLayout pass needed anymore.
#elif JL_LLVM_VERSION >= 30600
    PM.add(new DataLayoutPass());
#elif JL_LLVM_VERSION >= 30500
    PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
    PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
#endif

    addOptimizationPasses(&PM);

    std::unique_ptr<raw_fd_ostream> bc_OS;
    std::unique_ptr<raw_fd_ostream> obj_OS;
#if JL_LLVM_VERSION >= 30700 // 3.7 simplified formatted output; just use the raw stream alone
    std::unique_ptr<raw_fd_ostream> &bc_FOS = bc_OS;
    std::unique_ptr<raw_fd_ostream> &obj_FOS = obj_OS;
#else
    std::unique_ptr<formatted_raw_ostream> bc_FOS;
    std::unique_ptr<formatted_raw_ostream> obj_FOS;
#endif

    if (bc_fname) {
#if JL_LLVM_VERSION >= 30500
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
#if JL_LLVM_VERSION < 30700
            bc_FOS.reset(new formatted_raw_ostream(*bc_OS.get()));
#endif
            PM.add(createBitcodeWriterPass(*bc_FOS.get()));     // Unroll small loops
        }
    }

    if (obj_fname) {
#if JL_LLVM_VERSION >= 30500
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
#if JL_LLVM_VERSION < 30700
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

#if JL_LLVM_VERSION >= 30700
    // Reset the target triple to make sure it matches the new target machine
    clone->setTargetTriple(TM->getTargetTriple().str());
#if JL_LLVM_VERSION >= 30800
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

GlobalVariable *jl_get_global_for(const char *cname, void *addr, Module *M)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    std::map<void*, jl_value_llvm>::iterator it;
    // first see if there already is a GlobalVariable for this address
    it = jl_value_to_llvm.find(addr);
    if (it != jl_value_to_llvm.end())
        return prepare_global((llvm::GlobalVariable*)it->second.gv, M);

    std::stringstream gvname;
    gvname << cname << globalUnique++;
    // no existing GlobalVariable, create one and store it
    GlobalVariable *gv = new GlobalVariable(*M, T_pjlvalue,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, gvname.str());
    *(void**)jl_emit_and_add_to_shadow(gv, addr) = addr;
    return gv;
}
