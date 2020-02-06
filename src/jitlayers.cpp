// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#if defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
#  define JL_DISABLE_FPO
#endif

#include <iostream>
#include <sstream>

// analysis passes
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/Verifier.h>
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/LinkAllPasses.h>
#include <polly/CodeGen/CodegenCleanup.h>
#if defined(USE_POLLY_ACC)
#include <polly/Support/LinkGPURuntime.h>
#endif
#endif

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Support/SmallVectorMemoryBuffer.h>

#if JL_LLVM_VERSION >= 100000
#include <llvm/Support/CodeGen.h>
#endif

namespace llvm {
    extern Pass *createLowerSimdLoopPass();
}

#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>

#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include "llvm/Object/ArchiveWriter.h"

// target support
#include <llvm/ADT/Triple.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/Support/DynamicLibrary.h>


#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/ADT/SmallSet.h>
#include "codegen_shared.h"

using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "julia_assert.h"

#if JL_LLVM_VERSION < 100000
static const TargetMachine::CodeGenFileType CGFT_ObjectFile = TargetMachine::CGFT_ObjectFile;
#endif

RTDyldMemoryManager* createRTDyldMemoryManager(void);

static IntegerType *T_uint32;
static IntegerType *T_uint64;
static IntegerType *T_size;
static Type *T_psize;
static Type *T_pjlvalue;
void jl_init_jit(Type *T_pjlvalue_)
{
    T_uint32 = Type::getInt32Ty(jl_LLVMContext);
    T_uint64 = Type::getInt64Ty(jl_LLVMContext);
    if (sizeof(size_t) == 8)
        T_size = T_uint64;
    else
        T_size = T_uint32;
    T_psize = PointerType::get(T_size, 0);
    T_pjlvalue = T_pjlvalue_;
}

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

void addTargetPasses(legacy::PassManagerBase *PM, TargetMachine *TM)
{
    PM->add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
    PM->add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
}

// this defines the set of optimization passes defined for Julia at various optimization levels.
// it assumes that the TLI and TTI wrapper passes have already been added.
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level,
                           bool lower_intrinsics, bool dump_native)
{
#ifdef JL_DEBUG_BUILD
    PM->add(createGCInvariantVerifierPass(true));
    PM->add(createVerifierPass());
#endif

#if defined(JL_ASAN_ENABLED)
    PM->add(createAddressSanitizerFunctionPass());
#endif
#if defined(JL_MSAN_ENABLED)
    PM->add(llvm::createMemorySanitizerPass(true));
#endif
    if (opt_level < 2) {
        PM->add(createCFGSimplificationPass()); // Clean up disgusting code
        if (opt_level == 1) {
            PM->add(createSROAPass());                 // Break up aggregate allocas
            PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
            PM->add(createEarlyCSEPass());
        }
        PM->add(createMemCpyOptPass()); // Remove memcpy / form memset
        PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline
        if (lower_intrinsics) {
            PM->add(createBarrierNoopPass());
            PM->add(createLowerExcHandlersPass());
            PM->add(createGCInvariantVerifierPass(false));
            PM->add(createLateLowerGCFramePass());
            PM->add(createFinalLowerGCPass());
            PM->add(createLowerPTLSPass(dump_native));
        }
        PM->add(createLowerSimdLoopPass());        // Annotate loop marked with "loopinfo" as LLVM parallel loop
        if (dump_native)
            PM->add(createMultiVersioningPass());
        return;
    }
    PM->add(createPropagateJuliaAddrspaces());
    PM->add(createScopedNoAliasAAWrapperPass());
    PM->add(createTypeBasedAAWrapperPass());
    if (opt_level >= 3) {
        PM->add(createBasicAAWrapperPass());
    }
    // list of passes from vmkit
    PM->add(createCFGSimplificationPass()); // Clean up disgusting code
    PM->add(createDeadCodeEliminationPass());
    PM->add(createSROAPass()); // Kill useless allocas

    PM->add(createMemCpyOptPass());

    PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline

    // Running `memcpyopt` between this and `sroa` seems to give `sroa` a hard time
    // merging the `alloca` for the unboxed data and the `alloca` created by the `alloc_opt`
    // pass.
    PM->add(createAllocOptPass());
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    // Now that SROA has cleaned up for front-end mess, a lot of control flow should
    // be more evident - try to clean it up.
    PM->add(createCFGSimplificationPass());    // Merge & remove BBs
    if (dump_native)
        PM->add(createMultiVersioningPass());
    PM->add(createSROAPass());                 // Break up aggregate allocas
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    PM->add(createJumpThreadingPass());        // Thread jumps.
    PM->add(createInstructionCombiningPass()); // Combine silly seq's

    //PM->add(createCFGSimplificationPass());    // Merge & remove BBs
    PM->add(createReassociatePass());          // Reassociate expressions

    // this has the potential to make some things a bit slower
    //PM->add(createBBVectorizePass());

    PM->add(createEarlyCSEPass()); //// ****

    // Load forwarding above can expose allocations that aren't actually used
    // remove those before optimizing loops.
    PM->add(createAllocOptPass());
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
    PM->add(createLowerSimdLoopPass());        // Annotate loop marked with "loopinfo" as LLVM parallel loop
    PM->add(createLICMPass());                 // Hoist loop invariants
    PM->add(createLoopUnswitchPass());         // Unswitch loops.
    // Subsequent passes not stripping metadata from terminator
    PM->add(createInstructionCombiningPass());
    PM->add(createIndVarSimplifyPass());       // Canonicalize indvars
    PM->add(createLoopDeletionPass());         // Delete dead loops
    PM->add(createSimpleLoopUnrollPass());     // Unroll small loops
    //PM->add(createLoopStrengthReducePass());   // (jwb added)

    // Run our own SROA on heap objects before LLVM's
    PM->add(createAllocOptPass());
    // Re-run SROA after loop-unrolling (useful for small loops that operate,
    // over the structure of an aggregate)
    PM->add(createSROAPass());                 // Break up aggregate allocas
    PM->add(createInstructionCombiningPass()); // Clean up after the unroller
    PM->add(createGVNPass());                  // Remove redundancies
    PM->add(createMemCpyOptPass());            // Remove memcpy / form memset
    PM->add(createSCCPPass());                 // Constant prop with SCCP

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    PM->add(createInstructionCombiningPass());
    PM->add(createJumpThreadingPass());         // Thread jumps
    PM->add(createDeadStoreEliminationPass());  // Delete dead stores

    // More dead allocation (store) deletion before loop optimization
    PM->add(createAllocOptPass());
    // see if all of the constant folding has exposed more loops
    // to simplification and deletion
    // this helps significantly with cleaning up iteration
    PM->add(createCFGSimplificationPass());     // Merge & remove BBs
    PM->add(createLoopIdiomPass());
    PM->add(createLoopDeletionPass());          // Delete dead loops
    PM->add(createJumpThreadingPass());         // Thread jumps
    PM->add(createSLPVectorizerPass());         // Vectorize straight-line code
    PM->add(createAggressiveDCEPass());         // Delete dead instructions
    PM->add(createInstructionCombiningPass());  // Clean up after SLP loop vectorizer
    PM->add(createLoopVectorizePass());         // Vectorize loops
    PM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer

    if (lower_intrinsics) {
        // LowerPTLS removes an indirect call. As a result, it is likely to trigger
        // LLVM's devirtualization heuristics, which would result in the entire
        // pass pipeline being re-exectuted. Prevent this by inserting a barrier.
        PM->add(createBarrierNoopPass());
        PM->add(createLowerExcHandlersPass());
        PM->add(createGCInvariantVerifierPass(false));
        PM->add(createLateLowerGCFramePass());
        PM->add(createFinalLowerGCPass());
        // Remove dead use of ptls
        PM->add(createDeadCodeEliminationPass());
        PM->add(createLowerPTLSPass(dump_native));
        // Clean up write barrier and ptls lowering
        PM->add(createCFGSimplificationPass());
    }
    PM->add(createCombineMulAddPass());
}

extern "C" JL_DLLEXPORT
void jl_add_optimization_passes(LLVMPassManagerRef PM, int opt_level, int lower_intrinsics) {
    addOptimizationPasses(unwrap(PM), opt_level, lower_intrinsics);
}

#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
// Resolve non-lock free atomic functions in the libatomic1 library.
// This is the library that provides support for c11/c++11 atomic operations.
static uint64_t resolve_atomic(const char *name)
{
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
    static const char *const libatomic = "libatomic.so.1";
#elif defined(_OS_WINDOWS_)
    static const char *const libatomic = "libatomic-1.dll";
#endif
    static void *atomic_hdl = jl_load_dynamic_library(libatomic,
                                                      JL_RTLD_LOCAL, 0);
    static const char *const atomic_prefix = "__atomic_";
    if (!atomic_hdl)
        return 0;
    if (strncmp(name, atomic_prefix, strlen(atomic_prefix)) != 0)
        return 0;
    uintptr_t value;
    jl_dlsym(atomic_hdl, name, (void **)&value, 0);
    return value;
}
#endif

// Custom object emission notification handler for the JuliaOJIT
extern JITEventListener *CreateJuliaJITEventListener();
JuliaOJIT::DebugObjectRegistrar::DebugObjectRegistrar(JuliaOJIT &JIT)
    : JuliaListener(CreateJuliaJITEventListener()),
      JIT(JIT) {}

JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr);

template <typename ObjT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::registerObject(RTDyldObjHandleT H, const ObjT &Obj,
                                                     const LoadResult &LO)
{
    const ObjT* Object = &Obj;

    JIT.NotifyFinalizer(H, *Object, *LO);
    ORCNotifyObjectEmitted(JuliaListener.get(), *Object,
                           *LO, JIT.MemMgr.get());

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
        auto Sym = JIT.CompileLayer.findSymbolIn(H, Name.str(), true);
        assert(Sym);
        // note: calling getAddress here eagerly finalizes H
        // as an alternative, we could store the JITSymbol instead
        // (which would present a lazy-initializer functor interface instead)
        JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)cantFail(Sym.getAddress());
    }
}

template <typename ObjSetT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::operator()(RTDyldObjHandleT H,
                const ObjSetT &Object, const LoadResult &LOS)
{
    registerObject(H, Object,
                   static_cast<const RuntimeDyld::LoadedObjectInfo*>(&LOS));
}


CompilerResultT JuliaOJIT::CompilerT::operator()(Module &M)
{
    JL_TIMING(LLVM_OPT);
    jit.PM.run(M);
    std::unique_ptr<MemoryBuffer> ObjBuffer(
        new SmallVectorMemoryBuffer(std::move(jit.ObjBufferSV)));
    auto Obj = object::ObjectFile::createObjectFile(ObjBuffer->getMemBufferRef());

    if (!Obj) {
        llvm_dump(&M);
        std::string Buf;
        raw_string_ostream OS(Buf);
        logAllUnhandledErrors(Obj.takeError(), OS, "");
        OS.flush();
        llvm::report_fatal_error("FATAL: Unable to compile LLVM Module: '" + Buf + "'\n"
                                 "The module's content was printed above. Please file a bug report");
    }

    return CompilerResultT(std::move(ObjBuffer));
}

JuliaOJIT::JuliaOJIT(TargetMachine &TM)
  : TM(TM),
    DL(TM.createDataLayout()),
    ObjStream(ObjBufferSV),
    MemMgr(createRTDyldMemoryManager()),
    registrar(*this),
    ES(),
    SymbolResolver(llvm::orc::createLegacyLookupResolver(
          ES,
          [this](StringRef name) -> llvm::JITSymbol {
            return this->resolveSymbol(name);
          },
          [](llvm::Error Err) {
            cantFail(std::move(Err), "resolveSymbol failed");
          })),
    ObjectLayer(
        ES,
        [this](RTDyldObjHandleT) {
                      ObjLayerT::Resources result;
                      result.MemMgr = MemMgr;
                      result.Resolver = SymbolResolver;
                      return result;
                    },
        std::ref(registrar)
        ),
    CompileLayer(
            ObjectLayer,
            CompilerT(this)
        )
{
    addTargetPasses(&PM, &TM);
    addOptimizationPasses(&PM, jl_generating_output() ? 0 : jl_options.opt_level);
    if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
        llvm_unreachable("Target does not support MC emission.");

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    std::string ErrorStr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, &ErrorStr))
        report_fatal_error("FATAL: unable to dlopen self\n" + ErrorStr);
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
#ifndef JL_NDEBUG
    // validate the relocations for M
    for (Module::global_object_iterator I = M->global_objects().begin(), E = M->global_objects().end(); I != E; ) {
        GlobalObject *F = &*I;
        ++I;
        if (F->isDeclaration()) {
            if (F->use_empty())
                F->eraseFromParent();
            else if (!((isa<Function>(F) && isIntrinsicFunction(cast<Function>(F))) ||
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

    auto key = ES.allocateVModule();
    cantFail(CompileLayer.addModule(key, std::move(M)));
    // Force LLVM to emit the module so that we can register the symbols
    // in our lookup table.
    Error Err = CompileLayer.emitAndFinalize(key);
    // Check for errors to prevent LLVM from crashing the program.
    if (Err)
        report_fatal_error(std::move(Err));
}

void JuliaOJIT::removeModule(ModuleHandleT H)
{
    (void)CompileLayer.removeModule(H);
}

JL_JITSymbol JuliaOJIT::findSymbol(StringRef Name, bool ExportedSymbolsOnly)
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

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(StringRef Name)
{
    return findSymbol(getMangledName(Name), true);
}

JL_JITSymbol JuliaOJIT::resolveSymbol(StringRef Name)
{
    // Step 0: ObjectLinkingLayer has checked whether it is in the current module
    // Step 1: See if it's something known to the ExecutionEngine
    if (auto Sym = findSymbol(Name, true)) {
        // `findSymbol` already eagerly resolved the address
        // return it directly.
        return Sym;
    }
    // Step 2: Search the program symbols
    if (uint64_t addr = SectionMemoryManager::getSymbolAddressInProcess(Name.str()))
        return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
    if (uint64_t addr = resolve_atomic(Name.str().c_str()))
        return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
    // Return failure code
    return JL_SymbolInfo(nullptr);
}

uint64_t JuliaOJIT::getGlobalValueAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
}

Function *JuliaOJIT::FindFunctionNamed(StringRef Name)
{
    return shadow_output->getFunction(Name);
}

void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    if (!L)
        return;
    EventListeners.push_back(L);
}

void JuliaOJIT::NotifyFinalizer(RTDyldObjHandleT Key,
                                const object::ObjectFile &Obj,
                                const RuntimeDyld::LoadedObjectInfo &LoadedObjectInfo)
{
    for (auto &Listener : EventListeners)
        Listener->notifyObjectLoaded(Key, Obj, LoadedObjectInfo);
}

const DataLayout& JuliaOJIT::getDataLayout() const
{
    return DL;
}

const Triple& JuliaOJIT::getTargetTriple() const
{
    return TM.getTargetTriple();
}

std::string JuliaOJIT::getMangledName(StringRef Name)
{
    SmallString<128> FullName;
    Mangler::getNameWithPrefix(FullName, Name, DL);
    return FullName.str().str();
}

std::string JuliaOJIT::getMangledName(const GlobalValue *GV)
{
    return getMangledName(GV->getName());
}

JuliaOJIT *jl_ExecutionEngine;

// MSVC's link.exe requires each function declaration to have a Comdat section
// So rather than litter the code with conditionals,
// all global values that get emitted call this function
// and it decides whether the definition needs a Comdat section and adds the appropriate declaration
// TODO: consider moving this into jl_add_to_shadow or jl_dump_shadow? the JIT doesn't care, so most calls are now no-ops
template<class T> // for GlobalObject's
static T *addComdat(T *G)
{
#if defined(_OS_WINDOWS_)
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
        for (NamedMDNode::op_iterator I = sNMD->op_begin(), E = sNMD->op_end(); I != E; ++I) {
            dNMD->addOperand(*I);
        }
    }
}

// to finalize a function, look up its name in the `module_for_fname` map of
// unfinalized functions and merge it, plus any other modules it depends upon,
// into `collector` then add `collector` to the execution engine
static StringMap<Module*> module_for_fname;
static void jl_merge_recursive(Module *m, Module *collector);

static void jl_add_to_ee(std::unique_ptr<Module> m)
{
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_)
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
    jl_ExecutionEngine->addModule(std::move(m));
}

void jl_finalize_function(StringRef F)
{
    std::unique_ptr<Module> m(module_for_fname.lookup(F));
    if (m) {
        jl_merge_recursive(m.get(), m.get());
        jl_add_to_ee(std::move(m));
    }
}

static void jl_finalize_function(StringRef F, Module *collector)
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
    for (Module::global_object_iterator I = m->global_objects().begin(), E = m->global_objects().end(); I != E; ++I) {
        GlobalObject *F = &*I;
        if (!F->isDeclaration()) {
            module_for_fname.erase(F->getName());
        }
        else if (isa<Function>(F) && !isIntrinsicFunction(cast<Function>(F))) {
            to_finalize.push_back(F->getName().str());
        }
        else if (isa<GlobalValue>(F) && module_for_fname.count(F->getName())) {
            to_finalize.push_back(F->getName().str());
        }
    }

    for (const auto F : to_finalize) {
        jl_finalize_function(F, collector);
    }
}

// see if any of the functions needed by F are still WIP
static StringSet<> incomplete_fname;
static bool can_finalize_function(StringRef F, SmallSet<Module*, 16> &known)
{
    if (incomplete_fname.find(F) != incomplete_fname.end())
        return false;
    Module *M = module_for_fname.lookup(F);
    if (M && known.insert(M).second) {
        for (Module::iterator I = M->begin(), E = M->end(); I != E; ++I) {
            Function *F = &*I;
            if (F->isDeclaration() && !isIntrinsicFunction(F)) {
                if (!can_finalize_function(F->getName(), known))
                    return false;
            }
        }
    }
    return true;
}
bool jl_can_finalize_function(StringRef F)
{
    SmallSet<Module*, 16> known;
    return can_finalize_function(F, known);
}

// let the JIT know this function is a WIP
void jl_init_function(Function *F)
{
    // set any attributes that *must* be set on all functions
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to realign the stack to the next 16-byte boundary
    // upon entry to any function. This achieves compatibility
    // with both MinGW-GCC (which assumes an 16-byte-aligned stack) and
    // i686 Windows (which uses a 4-byte-aligned stack)
    AttrBuilder attr;
    attr.addStackAlignmentAttr(16);
    F->addAttributes(AttributeList::FunctionIndex, attr);
#endif
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    F->setHasUWTable(); // force NeedsWinEH
#endif
#ifdef JL_DISABLE_FPO
#if LLVM_VERSION_MAJOR >= 8
    F->addFnAttr("frame-pointer", "all");
#else
    F->addFnAttr("no-frame-pointer-elim", "true");
#endif
#endif
    // record the WIP name
    incomplete_fname.insert(F->getName());
}

// this takes ownership of a module after code emission is complete
// and will add it to the execution engine when required (by jl_finalize_function)
void jl_finalize_module(Module *m, bool shadow)
{
    // record the function names that are part of this Module
    // so it can be added to the JIT when needed
    for (Module::global_object_iterator I = m->global_objects().begin(), E = m->global_objects().end(); I != E; ++I) {
        GlobalObject *F = &*I;
        if (!F->isDeclaration()) {
            if (isa<Function>(F)) {
                bool known = incomplete_fname.erase(F->getName());
                (void)known; // TODO: assert(known); // llvmcall gets this wrong
            }
            module_for_fname[F->getName()] = m;
        }
    }
    // in the newer JITs, the shadow module is separate from the execution module
    if (shadow)
        jl_add_to_shadow(m);
}

// helper function for adding a DLLImport (dlsym) address to the execution engine
// (for values created locally or in the sysimage, jl_emit_and_add_to_shadow is generally preferable)
void add_named_global(GlobalObject *gv, void *addr, bool dllimport)
{
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
    }
#endif // _OS_WINDOWS_

    jl_ExecutionEngine->addGlobalMapping(gv, addr);
}

static std::vector<GlobalValue*> jl_sysimg_gvars;
static std::vector<GlobalValue*> jl_sysimg_fvars;
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
void** jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit)
{
    PointerType *T = cast<PointerType>(gv->getType()->getElementType()); // pointer is the only supported type here

    GlobalVariable *shadowvar = NULL;
    if (imaging_mode)
        shadowvar = global_proto(gv, shadow_output);

    if (shadowvar) {
        shadowvar->setInitializer(ConstantPointerNull::get(T));
        shadowvar->setLinkage(GlobalVariable::InternalLinkage);
        addComdat(shadowvar);
        if (imaging_mode && gvarinit) {
            // make the pointer valid for future sessions
            jl_sysimg_gvars.push_back(shadowvar);
            jl_value_llvm gv_struct;
            gv_struct.gv = global_proto(gv);
            gv_struct.index = jl_sysimg_gvars.size();
            jl_value_to_llvm[gvarinit] = gv_struct;
        }
    }

    // make the pointer valid for this session
    void **slot = (void**)calloc(1, sizeof(void*));
    jl_ExecutionEngine->addGlobalMapping(gv, slot);
    return slot;
}

void* jl_get_globalvar(GlobalVariable *gv)
{
    void *p = (void*)(intptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(gv);
    assert(p);
    return p;
}

// clones the contents of the module `m` to the shadow_output collector
void jl_add_to_shadow(Module *m)
{
#ifndef KEEP_BODIES
    if (!imaging_mode && !jl_options.outputjitbc)
        return;
#endif
    ValueToValueMapTy VMap;
    std::unique_ptr<Module> clone(CloneModule(*m, VMap));
    for (Module::iterator I = clone->begin(), E = clone->end(); I != E; ++I) {
        Function *F = &*I;
        if (!F->isDeclaration()) {
            F->setLinkage(Function::InternalLinkage);
            addComdat(F);
        }
    }
    jl_merge_module(shadow_output, std::move(clone));
}

static void emit_offset_table(Module *mod, const std::vector<GlobalValue*> &vars, StringRef name)
{
    // Emit a global variable with all the variable addresses.
    // The cloning pass will convert them into offsets.
    assert(!vars.empty());
    size_t nvars = vars.size();
    std::vector<Constant*> addrs(nvars);
    for (size_t i = 0; i < nvars; i++)
        addrs[i] = ConstantExpr::getBitCast(vars[i], T_psize);
    ArrayType *vars_type = ArrayType::get(T_psize, nvars);
    new GlobalVariable(*mod, vars_type, true,
                       GlobalVariable::ExternalLinkage,
                       ConstantArray::get(vars_type, addrs),
                       name);
}

static void emit_result(std::vector<NewArchiveMember> &Archive, SmallVectorImpl<char> &OS,
        StringRef Name, std::vector<std::string> &outputs)
{
    outputs.push_back({ OS.data(), OS.size() });
    Archive.push_back(NewArchiveMember(MemoryBufferRef(outputs.back(), Name)));
    OS.clear();
}

static object::Archive::Kind getDefaultForHost(Triple &triple) {
      if (triple.isOSDarwin())
          return object::Archive::K_DARWIN;
      return object::Archive::K_GNU;
}

typedef Error ArchiveWriterError;
static void reportWriterError(const ErrorInfoBase &E) {
    std::string err = E.message();
    jl_safe_printf("ERROR: failed to emit output file %s\n", err.c_str());
}

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
extern "C"
void jl_dump_native(const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname, const char *sysimg_data, size_t sysimg_len)
{
    JL_TIMING(NATIVE_DUMP);
    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
    // make sure to emit the native object format, even if FORCE_ELF was set in codegen
#if defined(_OS_WINDOWS_)
    TheTriple.setObjectFormat(Triple::COFF);
#elif defined(_OS_DARWIN_)
    TheTriple.setObjectFormat(Triple::MachO);
    TheTriple.setOS(llvm::Triple::MacOSX);
#endif
    std::unique_ptr<TargetMachine>
    TM(jl_TargetMachine->getTarget().createTargetMachine(
        TheTriple.getTriple(),
        jl_TargetMachine->getTargetCPU(),
        jl_TargetMachine->getTargetFeatureString(),
        jl_TargetMachine->Options,
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        Reloc::PIC_,
#else
        Optional<Reloc::Model>(),
#endif
#if defined(_CPU_PPC_) || defined(_CPU_PPC64_)
        // On PPC the small model is limited to 16bit offsets
        CodeModel::Medium,
#else
        // Use small model so that we can use signed 32bits offset in the function and GV tables
        CodeModel::Small,
#endif
        CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
        ));

    legacy::PassManager PM;
    addTargetPasses(&PM, TM.get());

    // set up optimization passes
    SmallVector<char, 128> bc_Buffer;
    SmallVector<char, 128> obj_Buffer;
    SmallVector<char, 128> unopt_bc_Buffer;
    raw_svector_ostream bc_OS(bc_Buffer);
    raw_svector_ostream obj_OS(obj_Buffer);
    raw_svector_ostream unopt_bc_OS(unopt_bc_Buffer);
    std::vector<NewArchiveMember> bc_Archive;
    std::vector<NewArchiveMember> obj_Archive;
    std::vector<NewArchiveMember> unopt_bc_Archive;
    std::vector<std::string> outputs;

    if (unopt_bc_fname)
        PM.add(createBitcodeWriterPass(unopt_bc_OS));
    if (bc_fname || obj_fname)
        addOptimizationPasses(&PM, jl_options.opt_level, true, true);
    if (bc_fname)
        PM.add(createBitcodeWriterPass(bc_OS));
    if (obj_fname)
        if (TM->addPassesToEmitFile(PM, obj_OS, nullptr, CGFT_ObjectFile, false))
            jl_safe_printf("ERROR: target does not support generation of object files\n");

    // Reset the target triple to make sure it matches the new target machine
    shadow_output->setTargetTriple(TM->getTargetTriple().str());
    DataLayout DL = TM->createDataLayout();
    DL.reset(DL.getStringRepresentation() + "-ni:10:11:12:13");
    shadow_output->setDataLayout(DL);

    // add metadata information
    if (imaging_mode) {
        emit_offset_table(shadow_output, jl_sysimg_gvars, "jl_sysimg_gvars");
        emit_offset_table(shadow_output, jl_sysimg_fvars, "jl_sysimg_fvars");

        // reflect the address of the jl_RTLD_DEFAULT_handle variable
        // back to the caller, so that we can check for consistency issues
        GlobalValue *jlRTLD_DEFAULT_var = shadow_output->getNamedValue("jl_RTLD_DEFAULT_handle");
        addComdat(new GlobalVariable(*shadow_output,
                                     jlRTLD_DEFAULT_var->getType(),
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     jlRTLD_DEFAULT_var,
                                     "jl_RTLD_DEFAULT_handle_pointer"));
    }

    // do the actual work
    auto add_output = [&] (Module &M, StringRef unopt_bc_Name, StringRef bc_Name, StringRef obj_Name) {
        PM.run(M);
        if (unopt_bc_fname)
            emit_result(unopt_bc_Archive, unopt_bc_Buffer, unopt_bc_Name, outputs);
        if (bc_fname)
            emit_result(bc_Archive, bc_Buffer, bc_Name, outputs);
        if (obj_fname)
            emit_result(obj_Archive, obj_Buffer, obj_Name, outputs);
    };

    add_output(*shadow_output, "unopt.bc", "text.bc", "text.o");
    // save some memory, by deleting all of the function bodies
    for (auto &F : shadow_output->functions()) {
        if (!F.isDeclaration())
            F.deleteBody();
    }

    LLVMContext &Context = shadow_output->getContext();
    std::unique_ptr<Module> sysimage(new Module("sysimage", Context));
    sysimage->setTargetTriple(shadow_output->getTargetTriple());
    sysimage->setDataLayout(shadow_output->getDataLayout());

    addComdat(new GlobalVariable(*sysimage,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size, globalUnique + 1),
                                 "jl_globalUnique"));

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(Context,
            ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
        addComdat(new GlobalVariable(*sysimage, data->getType(), false,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data"))->setAlignment(Align(64));
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*sysimage, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"));
    }
    add_output(*sysimage, "data.bc", "data.bc", "data.o");

    object::Archive::Kind Kind = getDefaultForHost(TheTriple);
    if (unopt_bc_fname)
        handleAllErrors(writeArchive(unopt_bc_fname, unopt_bc_Archive, true,
                    Kind, true, false), reportWriterError);
    if (bc_fname)
        handleAllErrors(writeArchive(bc_fname, bc_Archive, true,
                    Kind, true, false), reportWriterError);
    if (obj_fname)
        handleAllErrors(writeArchive(obj_fname, obj_Archive, true,
                    Kind, true, false), reportWriterError);

    imaging_mode = false;
}

extern "C" int32_t jl_assign_functionID(const char *fname)
{
    // give the function an index in the constant lookup table
    assert(imaging_mode);
    if (fname == NULL)
        return 0;
    jl_sysimg_fvars.push_back(shadow_output->getNamedValue(fname));
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
        return prepare_global_in(M, (llvm::GlobalVariable*)it->second.gv);

    std::stringstream gvname;
    gvname << cname << globalUnique++;
    // no existing GlobalVariable, create one and store it
    GlobalVariable *gv = new GlobalVariable(*M, T_pjlvalue,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, gvname.str());
    *jl_emit_and_add_to_shadow(gv, addr) = addr;
    return gv;
}

// An LLVM module pass that just runs all julia passes in order. Useful for
// debugging
extern "C" void jl_init_codegen(void);
template <int OptLevel>
class JuliaPipeline : public Pass {
public:
    static char ID;
    // A bit of a hack, but works
    struct TPMAdapter : public PassManagerBase {
        PMTopLevelManager *TPM;
        TPMAdapter(PMTopLevelManager *TPM) : TPM(TPM) {}
        void add(Pass *P) { TPM->schedulePass(P); }
    };
    void preparePassManager(PMStack &Stack) override {
        (void)jl_init_llvm();
        PMTopLevelManager *TPM = Stack.top()->getTopLevelManager();
        TPMAdapter Adapter(TPM);
        addTargetPasses(&Adapter, jl_TargetMachine);
        addOptimizationPasses(&Adapter, OptLevel);
    }
    JuliaPipeline() : Pass(PT_PassManager, ID) {}
    Pass *createPrinterPass(raw_ostream &O, const std::string &Banner) const override {
        return createPrintModulePass(O, Banner);
    }
};
template<> char JuliaPipeline<0>::ID = 0;
template<> char JuliaPipeline<2>::ID = 0;
template<> char JuliaPipeline<3>::ID = 0;
static RegisterPass<JuliaPipeline<0>> X("juliaO0", "Runs the entire julia pipeline (at -O0)", false, false);
static RegisterPass<JuliaPipeline<2>> Y("julia", "Runs the entire julia pipeline (at -O2)", false, false);
static RegisterPass<JuliaPipeline<3>> Z("juliaO3", "Runs the entire julia pipeline (at -O3)", false, false);
