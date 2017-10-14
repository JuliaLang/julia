// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#include <iostream>
#include <sstream>

// analysis passes
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
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
#include <llvm/Bitcode/BitcodeWriterPass.h>

#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/Transforms/Utils/Cloning.h>

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
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level)
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
    if (opt_level == 0) {
        PM->add(createCFGSimplificationPass()); // Clean up disgusting code
#if JL_LLVM_VERSION < 50000
        PM->add(createBarrierNoopPass());
        PM->add(createLowerExcHandlersPass());
        PM->add(createGCInvariantVerifierPass(false));
        PM->add(createLateLowerGCFramePass());
        PM->add(createLowerPTLSPass(imaging_mode));
        PM->add(createBarrierNoopPass());
#endif
        PM->add(createMemCpyOptPass()); // Remove memcpy / form memset
#if JL_LLVM_VERSION >= 40000
        PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline
#else
        PM->add(createAlwaysInlinerPass()); // Respect always_inline
#endif
#if JL_LLVM_VERSION >= 50000
        PM->add(createBarrierNoopPass());
        PM->add(createLowerExcHandlersPass());
        PM->add(createGCInvariantVerifierPass(false));
        PM->add(createLateLowerGCFramePass());
        PM->add(createLowerPTLSPass(imaging_mode));
#endif
        return;
    }
    PM->add(createPropagateJuliaAddrspaces());
    PM->add(createTypeBasedAAWrapperPass());
    if (jl_options.opt_level >= 3) {
        PM->add(createBasicAAWrapperPass());
    }
    // list of passes from vmkit
    PM->add(createCFGSimplificationPass()); // Clean up disgusting code
    PM->add(createDeadInstEliminationPass());
    PM->add(createPromoteMemoryToRegisterPass()); // Kill useless allocas

    // Due to bugs and missing features LLVM < 5.0, does not properly propagate
    // our invariants. We need to do GC rooting here. This reduces the
    // effectiveness of the optimization, but should retain correctness.
#if JL_LLVM_VERSION < 50000
    PM->add(createLowerExcHandlersPass());
    PM->add(createAllocOptPass());
    PM->add(createLateLowerGCFramePass());
    // Remove dead use of ptls
    PM->add(createDeadCodeEliminationPass());
    PM->add(createLowerPTLSPass(imaging_mode));
#endif

    PM->add(createMemCpyOptPass());

#if JL_LLVM_VERSION >= 40000
    PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline
#else
    PM->add(createAlwaysInlinerPass()); // Respect always_inline
#endif

#if JL_LLVM_VERSION >= 50000
    // Running `memcpyopt` between this and `sroa` seems to give `sroa` a hard time
    // merging the `alloca` for the unboxed data and the `alloca` created by the `alloc_opt`
    // pass.
    PM->add(createAllocOptPass());
#endif
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    PM->add(createSROAPass());                 // Break up aggregate allocas
    PM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    PM->add(createJumpThreadingPass());        // Thread jumps.
    // NOTE: CFG simp passes after this point seem to hurt native codegen.
    // See issue #6112. Should be re-evaluated when we switch to MCJIT.
    //PM->add(createCFGSimplificationPass());    // Merge & remove BBs
    PM->add(createInstructionCombiningPass()); // Combine silly seq's

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
    PM->add(createInstructionCombiningPass());
    PM->add(createIndVarSimplifyPass());       // Canonicalize indvars
    PM->add(createLoopDeletionPass());         // Delete dead loops
    PM->add(createSimpleLoopUnrollPass());     // Unroll small loops
    //PM->add(createLoopStrengthReducePass());   // (jwb added)

    // Re-run SROA after loop-unrolling (useful for small loops that operate,
    // over the structure of an aggregate)
    PM->add(createSROAPass());                 // Break up aggregate allocas
    PM->add(createInstructionCombiningPass()); // Clean up after the unroller
    PM->add(createGVNPass());                  // Remove redundancies
    PM->add(createMemCpyOptPass());            // Remove memcpy / form memset
    PM->add(createSCCPPass());                 // Constant prop with SCCP

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    PM->add(createSinkingPass()); ////////////// ****
    PM->add(createInstructionSimplifierPass());///////// ****
    PM->add(createInstructionCombiningPass());
    PM->add(createJumpThreadingPass());         // Thread jumps
    PM->add(createDeadStoreEliminationPass());  // Delete dead stores

    // see if all of the constant folding has exposed more loops
    // to simplification and deletion
    // this helps significantly with cleaning up iteration
    PM->add(createCFGSimplificationPass());     // Merge & remove BBs
    PM->add(createLoopIdiomPass());
    PM->add(createLoopDeletionPass());          // Delete dead loops
    PM->add(createJumpThreadingPass());         // Thread jumps

    if (jl_options.opt_level >= 3) {
        PM->add(createSLPVectorizerPass());     // Vectorize straight-line code
    }

    PM->add(createAggressiveDCEPass());         // Delete dead instructions
    if (jl_options.opt_level >= 3)
        PM->add(createInstructionCombiningPass());   // Clean up after SLP loop vectorizer
    PM->add(createLoopVectorizePass());         // Vectorize loops
    PM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer
    // LowerPTLS removes an indirect call. As a result, it is likely to trigger
    // LLVM's devirtualization heuristics, which would result in the entire
    // pass pipeline being re-exectuted. Prevent this by inserting a barrier.
#if JL_LLVM_VERSION >= 50000
    PM->add(createBarrierNoopPass());
    PM->add(createLowerExcHandlersPass());
    PM->add(createGCInvariantVerifierPass(false));
    PM->add(createLateLowerGCFramePass());
    // Remove dead use of ptls
    PM->add(createDeadCodeEliminationPass());
    PM->add(createLowerPTLSPass(imaging_mode));
#endif
    PM->add(createCombineMulAddPass());
}

extern "C" JL_DLLEXPORT
void jl_add_optimization_passes(LLVMPassManagerRef PM, int opt_level) {
    addOptimizationPasses(unwrap(PM), opt_level);
}

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

// Use a local variable to hold the addresses to avoid generating a PLT
// on the function call.
// It messes up the GDB lookup logic with dynamically linked LLVM.
// (Ref https://sourceware.org/bugzilla/show_bug.cgi?id=20633)
// Use `volatile` to make sure the call always loads this slot.
void (*volatile jit_debug_register_code)() = __jit_debug_register_code;

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
    jit_debug_register_code();
}
}
// ------------------------ END OF TEMPORARY COPY FROM LLVM -----------------

#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_)
// Resolve non-lock free atomic functions in the libatomic library.
// This is the library that provides support for c11/c++11 atomic operations.
static uint64_t resolve_atomic(const char *name)
{
#if defined(_OS_LINUX_)
    static const char *const libatomic = "libatomic";
#elif defined(_OS_WINDOWS_)
    static const char *const libatomic = "libatomic-1";
#endif
    static void *atomic_hdl = jl_load_dynamic_library_e(libatomic,
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

template <typename ObjT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::registerObject(RTDyldObjHandleT H, const ObjT &Object,
                                                     const LoadResult &LO)
{
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

    ORCNotifyObjectEmitted(JuliaListener.get(), *Object,
                           *SavedObjects.back().getBinary(),
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
        auto Sym = JIT.CompileLayer.findSymbolIn(H, Name, true);
        assert(Sym);
        // note: calling getAddress here eagerly finalizes H
        // as an alternative, we could store the JITSymbol instead
        // (which would present a lazy-initializer functor interface instead)
#if JL_LLVM_VERSION >= 50000
        JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)cantFail(Sym.getAddress());
#else
        JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)Sym.getAddress();
#endif
    }
}

// TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
template <typename ObjSetT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::operator()(RTDyldObjHandleT H,
                const ObjSetT &Objects, const LoadResult &LOS)
{
#if JL_LLVM_VERSION >= 50000
    registerObject(H, Objects->getBinary(),
                   static_cast<const RuntimeDyld::LoadedObjectInfo*>(&LOS));
#else
    auto oit = Objects.begin();
    auto lit = LOS.begin();
    for (; oit != Objects.end(); ++oit, ++lit) {
        const auto &Object = (*oit)->getBinary();
        auto &LO = *lit;

        registerObject(H, Object, LO);
    }
#endif
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

object::OwningBinary<object::ObjectFile> JuliaOJIT::CompilerT::operator()(Module &M)
{
    JL_TIMING(LLVM_OPT);
    jit.PM.run(M);
    std::unique_ptr<MemoryBuffer> ObjBuffer(
        new ObjectMemoryBuffer(std::move(jit.ObjBufferSV)));
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

    return OwningObj(std::move(*Obj), std::move(ObjBuffer));
}

JuliaOJIT::JuliaOJIT(TargetMachine &TM)
  : TM(TM),
    DL(TM.createDataLayout()),
    ObjStream(ObjBufferSV),
    MemMgr(createRTDyldMemoryManager()),
    registrar(*this),
    ObjectLayer(
#if JL_LLVM_VERSION >= 50000
        [&] { return MemMgr; },
#endif
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
#ifndef JL_NDEBUG
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
#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_)
                        if (uint64_t addr = resolve_atomic(Name.c_str()))
                            return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
                        // Return failure code
                        return JL_SymbolInfo(nullptr);
                      },
                      [](const std::string &S) { return nullptr; }
                    );
#if JL_LLVM_VERSION >= 50000
    auto modset = cantFail(CompileLayer.addModule(std::move(M), std::move(Resolver)));
#else
    SmallVector<std::unique_ptr<Module>,1> Ms;
    Ms.push_back(std::move(M));
    auto modset = CompileLayer.addModuleSet(std::move(Ms), MemMgr.get(),
                                            std::move(Resolver));
#endif
    // Force LLVM to emit the module so that we can register the symbols
    // in our lookup table.
#if JL_LLVM_VERSION >= 50000
    auto Err = CompileLayer.emitAndFinalize(modset);
    // Check for errors to prevent LLVM from crashing the program.
    assert(!Err);
#else
    CompileLayer.emitAndFinalize(modset);
#endif
}

void JuliaOJIT::removeModule(ModuleHandleT H)
{
#if JL_LLVM_VERSION >= 50000
    CompileLayer.removeModule(H);
#else
    CompileLayer.removeModuleSet(H);
#endif
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
#if JL_LLVM_VERSION >= 50000
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
#else
    return findSymbol(getMangledName(Name), false).getAddress();
#endif
}

uint64_t JuliaOJIT::getFunctionAddress(const std::string &Name)
{
#if JL_LLVM_VERSION >= 50000
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
#else
    return findSymbol(getMangledName(Name), false).getAddress();
#endif
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
    incomplete_fname.insert(F->getName());
}

// this takes ownership of a module after code emission is complete
// and will add it to the execution engine when required (by jl_finalize_function)
void jl_finalize_module(Module *m, bool shadow)
{
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
void* jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit)
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
    void *slot = calloc(1, sizeof(void*));
    jl_ExecutionEngine->addGlobalMapping(gv, slot);
    return slot;
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
                                            Constant::getNullValue(typ), name);
    addComdat(gv);
    // make the pointer valid for this session
    auto p = new uintptr_t(init);
    jl_ExecutionEngine->addGlobalMapping(gv, (void*)p);
    jl_sysimg_gvars.push_back(gv);
    idx = jl_sysimg_gvars.size();
    return gv;
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

#ifdef HAVE_CPUID
extern "C" {
    extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
}
#endif

static void emit_offset_table(Module *mod, const std::vector<GlobalValue*> &vars, StringRef name)
{
    assert(!vars.empty());
    addComdat(GlobalAlias::create(GlobalVariable::ExternalLinkage, name + "_base", vars[0]));
    auto vbase = ConstantExpr::getPtrToInt(vars[0], T_size);
    size_t nvars = vars.size();
    std::vector<Constant*> offsets(nvars);
    for (size_t i = 0; i < nvars; i++) {
        auto ptrdiff = ConstantExpr::getSub(ConstantExpr::getPtrToInt(vars[i], T_size), vbase);
        offsets[i] = sizeof(void*) == 8 ? ConstantExpr::getTrunc(ptrdiff, T_uint32) : ptrdiff;
    }
    ArrayType *vars_type = ArrayType::get(T_uint32, nvars);
    addComdat(new GlobalVariable(*mod, vars_type, true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantArray::get(vars_type, ArrayRef<Constant*>(offsets)),
                                 name + "_offsets"));
}


static void jl_gen_llvm_globaldata(Module *mod, const char *sysimg_data, size_t sysimg_len)
{
    emit_offset_table(mod, jl_sysimg_gvars, "jl_sysimg_gvars");
    emit_offset_table(mod, jl_sysimg_fvars, "jl_sysimg_fvars");
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
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size, jltls_offset_idx),
                                 "jl_tls_offset_idx"));
#endif

    Constant *feature_string = ConstantDataArray::getString(jl_LLVMContext, jl_options.cpu_target);
    addComdat(new GlobalVariable(*mod,
                                 feature_string->getType(),
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 feature_string,
                                 "jl_sysimg_cpu_target"));

    // reflect the address of the jl_RTLD_DEFAULT_handle variable
    // back to the caller, so that we can check for consistency issues
    GlobalValue *jlRTLD_DEFAULT_var = mod->getNamedValue("jl_RTLD_DEFAULT_handle");
    addComdat(new GlobalVariable(*mod,
                                 jlRTLD_DEFAULT_var->getType(),
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 jlRTLD_DEFAULT_var,
                                 "jl_RTLD_DEFAULT_handle_pointer"));

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
        addComdat(new GlobalVariable(*mod, data->getType(), false,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data"))->setAlignment(64);
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*mod, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"));
    }
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
        // Use small model so that we can use signed 32bits offset in the function and GV tables
        CodeModel::Small,
        CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
        ));

    legacy::PassManager PM;
    addTargetPasses(&PM, TM.get());

    // set up optimization passes
    std::unique_ptr<raw_fd_ostream> unopt_bc_OS;
    std::unique_ptr<raw_fd_ostream> bc_OS;
    std::unique_ptr<raw_fd_ostream> obj_OS;

    if (unopt_bc_fname) {
        // call output handler directly to avoid special case handling of `-` filename
        int FD;
        std::error_code EC = sys::fs::openFileForWrite(unopt_bc_fname, FD, sys::fs::F_None);
        unopt_bc_OS.reset(new raw_fd_ostream(FD, true));
        std::string err;
        if (EC)
            err = "ERROR: failed to open --output-unopt-bc file '" + std::string(unopt_bc_fname) + "': " + EC.message();
        if (!err.empty())
            jl_safe_printf("%s\n", err.c_str());
        else {
            PM.add(createBitcodeWriterPass(*unopt_bc_OS.get()));
        }
    }

    if (bc_fname || obj_fname)
        addOptimizationPasses(&PM, jl_options.opt_level);

    if (bc_fname) {
        // call output handler directly to avoid special case handling of `-` filename
        int FD;
        std::error_code EC = sys::fs::openFileForWrite(bc_fname, FD, sys::fs::F_None);
        bc_OS.reset(new raw_fd_ostream(FD, true));
        std::string err;
        if (EC)
            err = "ERROR: failed to open --output-bc file '" + std::string(bc_fname) + "': " + EC.message();
        if (!err.empty())
            jl_safe_printf("%s\n", err.c_str());
        else {
            PM.add(createBitcodeWriterPass(*bc_OS.get()));
        }
    }

    if (obj_fname) {
        // call output handler directly to avoid special case handling of `-` filename
        int FD;
        std::error_code EC = sys::fs::openFileForWrite(obj_fname, FD, sys::fs::F_None);
        obj_OS.reset(new raw_fd_ostream(FD, true));
        std::string err;
        if (EC)
            err = "ERROR: failed to open --output-o file '" + std::string(obj_fname) + "': " + EC.message();
        if (!err.empty())
            jl_safe_printf("%s\n", err.c_str());
        else {
            if (TM->addPassesToEmitFile(PM, *obj_OS.get(), TargetMachine::CGFT_ObjectFile, false)) {
                jl_safe_printf("ERROR: target does not support generation of object files\n");
            }
        }
    }

    // Reset the target triple to make sure it matches the new target machine
    shadow_output->setTargetTriple(TM->getTargetTriple().str());
#if JL_LLVM_VERSION >= 40000
    DataLayout DL = TM->createDataLayout();
    DL.reset(DL.getStringRepresentation() + "-ni:10:11:12");
    shadow_output->setDataLayout(DL);
#else
    shadow_output->setDataLayout(TM->createDataLayout());
#endif

    // add metadata information
    if (imaging_mode)
        jl_gen_llvm_globaldata(shadow_output, sysimg_data, sysimg_len);

    // do the actual work
    PM.run(*shadow_output);
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
    *(void**)jl_emit_and_add_to_shadow(gv, addr) = addr;
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
