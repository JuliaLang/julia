#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO/ConstantMerge.h>
#include <llvm/Transforms/Scalar/EarlyCSE.h>
#include <llvm/Transforms/Scalar/DCE.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include "reoptimization.h"

#include "jitlayers.h"

#define DEBUG_TYPE "reopt"

STATISTIC(NumReoptimized, "Number of functions reoptimized");
STATISTIC(NumProfileReoptimized, "Number of functions reoptimized by profiling");
STATISTIC(NumPreoptimized, "Number of functions already optimized to requested level");
STATISTIC(NumSpeculated, "Number of functions speculated");
STATISTIC(NumSpeculationInferred, "Number of functions speculated based on block frequencies");
STATISTIC(NumRaced, "Number of raced optimizations");
STATISTIC(NumPartitioned, "Number of partitioned functions");

using namespace llvm;

//Profiling the profiling (heh) reveals this is actually really bad for unoptimized functions,
//which is the common case, so we turn this off by default
cl::opt<bool> EnablePGOProfiling("julia-enable-pgo-profiling", cl::init(false),
                                  cl::desc("Enable PGO profiling"), cl::Hidden);
cl::opt<bool> EnableReoptimization("julia-enable-reoptimization", cl::init(true),
                                   cl::desc("Enable reoptimization"), cl::Hidden);
cl::opt<bool> EnableSpeculation("julia-enable-speculation", cl::init(true),
                                cl::desc("Enable speculation"), cl::Hidden);
cl::opt<bool> EnablePartitionSimplification("julia-enable-partition-simplification", cl::init(true),
                                cl::desc("Enable pre-partition simplification"), cl::Hidden);
cl::opt<bool> EnablePartitioning("julia-enable-partitioning", cl::init(true),
                                 cl::desc("Enable partitioning"), cl::Hidden);

static constexpr auto OptlevelMDName = "julia.optlevel";
static constexpr auto ReoptimizeMDName = "julia.reoptimize";
static constexpr auto ProfMDName = "julia.prof";

size_t getModuleOptLevel(Module &M, bool DetermineOptLevel) {
    if (auto OptMD = M.getModuleFlag(OptlevelMDName)) {
        return cast<ConstantInt>(cast<ConstantAsMetadata>(OptMD)->getValue())->getZExtValue();
    }
    assert(DetermineOptLevel && "Module should have optlevel metadata!");
    size_t optlevel = SIZE_MAX;
    if (jl_generating_output()) {
        optlevel = std::max(static_cast<int>(jl_options.opt_level_min), 0);
    }
    else {
        optlevel = std::max(static_cast<int>(jl_options.opt_level), 0);
        size_t optlevel_min = std::max(static_cast<int>(jl_options.opt_level_min), 0);
        for (auto &F : M.functions()) {
            if (!F.getBasicBlockList().empty()) {
                Attribute attr = F.getFnAttribute("julia-optimization-level");
                StringRef val = attr.getValueAsString();
                if (val != "") {
                    size_t ol = (size_t)val[0] - '0';
                    if (ol < optlevel)
                        optlevel = ol;
                }
            }
        }
        optlevel = std::min(std::max(optlevel, optlevel_min), static_cast<size_t>(3));
    }
    M.addModuleFlag(Module::Error, OptlevelMDName, optlevel);
    return optlevel;
}

static std::unique_ptr<Module> partitionFunction(Module &M, Function &F) {
    assert(!F.isDeclaration() && "Cannot partition a declaration");
    ValueToValueMapTy VMap;
    auto Mod = CloneModule(M, VMap, [F = &F](const GlobalValue *GV) { return GV == F; });
    F.deleteBody();
    return Mod;
}

static uint64_t computeOptLevelLimit(uint32_t OptLevel) {
    //This is essentially 16^(n+1), so this unrolls to:
    // { 16, 256, 4096 }
    // for upgrading 0->1, 1->2, 2->3
    return 16 << (OptLevel * 4);
}

extern "C" JITTargetAddress jl_cc_entrypoint(const char *Name, uint32_t Length, void *JD, int32_t Version, uint32_t OptLevel, void *Impl) {
    auto RJD = reinterpret_cast<orc::JITDylib*>(JD);
    return reinterpret_cast<ReoptimizationManager*>(Impl)->optimize(RJD->getExecutionSession().intern(StringRef(Name, Length)), *RJD, Version, OptLevel).Address;
}

//This looks extremely similar to the jl_cc_entrypoint function (and it is, since they have the same arguments)
//but they serve different purposes. jl_cc_entrypoint's job is to get a valid function pointer ASAP since it's
//about to be called, while this function's job is to notify the JIT that this function could be reoptimized
//since it's being called a lot. If the JIT is saturated with compilation already, going through optimize
//directly would involve a long wait vs queueing the function and letting a background thread do the update
//and recompilation, which allows this thread to get a move on the actual work the user wants to do.
extern "C" void jl_reoptimize_entrypoint(const char *Name, uint32_t Length, void *JD, int32_t Version, uint32_t OptLevel, void *Impl) {
    auto RJD = reinterpret_cast<orc::JITDylib*>(JD);
    reinterpret_cast<ReoptimizationManager*>(Impl)->profileReentry(RJD->getExecutionSession().intern(StringRef(Name, Length)), RJD, Version, OptLevel);
}

static Function &createCallback(Module &M, Function &F, const orc::SymbolStringPtr &Name, orc::JITDylib *JD, int32_t Version, uint32_t OptLevel, ReoptimizationManager *Impl) {
    static_assert(sizeof(JITTargetAddress) == sizeof(void*), "JITTargetAddress and void* must be the same size");
    static_assert(sizeof(&jl_cc_entrypoint) == sizeof(void*), "Must be able to convert function pointer to void*");
    static_assert(alignof(decltype(&jl_cc_entrypoint)) == alignof(void*), "Must be able to convert function pointer to void*");
    assert(F.isDeclaration() && "Cannot create callback for a definition");
    auto CCF = Function::Create(F.getFunctionType(), GlobalValue::PrivateLinkage, "__jcc." + F.getName(), M);
    auto T_int32 = Type::getInt32Ty(CCF->getContext());
    auto T_pint8 = Type::getInt8PtrTy(CCF->getContext());
    CCF->setAttributes(F.getAttributes());
    //Compilation callback is usually called just once, and then never seen again, so mark it cold
    CCF->addFnAttr(Attribute::Cold);
    CCF->addFnAttr(Attribute::MinSize);
    auto CCFT = FunctionType::get(Type::getInt64Ty(F.getContext()), {T_pint8, T_int32, T_pint8, T_int32, T_int32, T_pint8}, false);
    auto CCFP = literal_static_pointer_val(reinterpret_cast<void*>(&jl_cc_entrypoint), CCFT->getPointerTo());
    auto EntryBB = BasicBlock::Create(CCF->getContext(), "top", CCF);
    IRBuilder<> Builder(EntryBB);
    auto RF = Builder.CreateCall(CCFT, CCFP, {
        literal_static_pointer_val((*Name).data(), T_pint8),
        ConstantInt::get(T_int32, (*Name).size()),
        literal_static_pointer_val(JD, T_pint8),
        ConstantInt::get(T_int32, Version),
        ConstantInt::get(T_int32, OptLevel),
        literal_static_pointer_val(Impl, T_pint8)
    });
    auto RFP = Builder.CreateIntToPtr(RF, F.getFunctionType()->getPointerTo());
    SmallVector<Value *> Args;
    for (auto &Arg : CCF->args())
        Args.push_back(&Arg);
    auto Result = Builder.CreateCall(F.getFunctionType(), RFP, Args);
    Result->setAttributes(F.getAttributes());
    Result->setTailCall();
    if (F.getReturnType()->isVoidTy())
        Builder.CreateRetVoid();
    else
        Builder.CreateRet(Result);
    return *CCF;
}

static constexpr auto StubPrefix = "__jstub.";

static std::string getStubGVName(StringRef Name) {
    return (StubPrefix + Name).str();
}

static GlobalVariable &createStub(Module &M, Function &F, Function &CC) {
    assert(F.isDeclaration() && "Cannot create a stub for a definition");
    auto IntPtrT = M.getDataLayout().getIntPtrType(Type::getInt8PtrTy(F.getContext()));
    auto StubGV = new GlobalVariable(M, IntPtrT, false, GlobalValue::ExternalLinkage, ConstantExpr::getPtrToInt(&CC, IntPtrT), getStubGVName(F.getName()));
    auto EntryBB = BasicBlock::Create(F.getContext(), "top", &F);
    IRBuilder<> Builder(EntryBB);
    auto RFP = Builder.CreateLoad(IntPtrT, StubGV);
    RFP->setAtomic(AtomicOrdering::Monotonic);
    auto RF = Builder.CreateIntToPtr(RFP, F.getFunctionType()->getPointerTo());
    SmallVector<Value *> Args;
    for (auto &Arg : F.args())
        Args.push_back(&Arg);
    auto Result = Builder.CreateCall(F.getFunctionType(), RF, Args);
    Result->setAttributes(F.getAttributes());
    Result->setTailCall();
    if (F.getReturnType()->isVoidTy())
        Builder.CreateRetVoid();
    else
        Builder.CreateRet(Result);
    return *StubGV;
}

static auto serializeModule(Module &M) {
    assert(!verifyModule(M, &errs()));
    SmallVector<char, 0> ClonedModuleBuffer;
    BitcodeWriter BCWriter(ClonedModuleBuffer);
    BCWriter.writeModule(M);
    BCWriter.writeSymtab();
    BCWriter.writeStrtab();
    return ClonedModuleBuffer;
}

static std::unique_ptr<Module> deserializeModule(const SmallVector<char, 0> &ClonedModuleBuffer, LLVMContext &Ctx) {
    MemoryBufferRef ClonedModuleBufferRef(
        StringRef(ClonedModuleBuffer.data(), ClonedModuleBuffer.size()),
        "cloned module buffer");
    auto M = cantFail(parseBitcodeFile(ClonedModuleBufferRef, Ctx));
    assert(!verifyModule(*M, &errs()));
    return M;
}

static void simplifyModule(Module &M) {
    //We just want to drop off all of the annoying conditions that are
    //super easy to eliminate, so that we only instrument the parts
    //we actually care about
    ModulePassManager MPM;
    MPM.addPass(ConstantMergePass());
    FunctionPassManager FPM;
    FPM.addPass(SimplifyCFGPass());
    FPM.addPass(EarlyCSEPass());
    FPM.addPass(DCEPass());
    FPM.addPass(SimplifyCFGPass());
    PassBuilder PB;
    AnalysisManagers AM(PB);
    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    MPM.run(M, AM.MAM);
}

Expected<orc::ThreadSafeModule> FunctionPartitioner::operator()(orc::ThreadSafeModule TSM,
                                                        orc::MaterializationResponsibility &R) {
    if (!EnablePartitioning) {
        return std::move(TSM);
    }
    auto Promoted = TSM.withModuleDo([this, JD=&R.getTargetJITDylib()](Module &M) {
        orc::SymbolFlagsMap NewSymbols;
        if (EnablePartitionSimplification)
            simplifyModule(M);
        auto PromotedGlobals = Promoter->run(M);
        if (!PromotedGlobals.empty()) {
            orc::IRSymbolMapper::add(ES, ManglingOptions, PromotedGlobals, NewSymbols);
        }
        SmallVector<Function *> ToPartition;
        for (auto &F : M) {
            if (F.isDeclaration())
                continue;
            if (!ShouldPartition(F))
                continue;
            ToPartition.push_back(&F);
        }
        NumPartitioned += ToPartition.size();
        for (auto Func : ToPartition) {
            auto &F = *Func;
            auto Name = ES.intern(F.getName());
            auto Partitioned = partitionFunction(M, F);
            if (EnablePGOProfiling) {
                preannotateBranches(*Partitioned->getFunction(F.getName()));
            }
            Cache.store(Name, std::move(Partitioned));
            auto &CC = createCallback(M, F, Name, JD, 0, ReoptMgr.getOptLevelRange().first, &ReoptMgr);
            auto &GV = createStub(M, F, CC);
            NewSymbols[ES.intern(GV.getName())] = JITSymbolFlags::fromGlobalValue(GV);
        }
        return NewSymbols;
    });
    //Outside the context lock since it takes the session lock
    if (!Promoted.empty())
        cantFail(R.defineMaterializing(Promoted));
    return std::move(TSM);
}

static constexpr auto ManglePrefix = "__jreopt.";

//version is unsigned here to avoid ugly negative numbers
static std::string mangle(StringRef Name, uint32_t version) {
    return (Twine(ManglePrefix) + std::to_string(version) + "." + Name).str();
}

static StringRef demangle(StringRef Name) {
    if (Name.consume_front(ManglePrefix)) {
        int32_t version;
        bool valid = !Name.consumeInteger(10, version);
        assert(valid && "Invalid version number");
        bool dot = Name.consume_front(".");
        assert(dot && "Invalid mangled name");
        (void) version;
        (void) valid;
        (void) dot;
    }
    return Name;
}

StringRef getBaseName(StringRef Name) {
    return demangle(Name);
}

llvm::Expected<llvm::orc::ThreadSafeModule> FunctionMangler(llvm::orc::ThreadSafeModule TSM,
                                                           llvm::orc::MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        if (auto verMD = M.getModuleFlag(ReoptimizeMDName)) {
            int32_t version = cast<ConstantInt>(cast<ConstantAsMetadata>(verMD)->getValue())->getSExtValue();
            for (auto &F : M) {
                if (F.isDeclaration())
                    continue;
                F.setName(mangle(F.getName(), version));
            }
        }
    });
    return std::move(TSM);
}

void StubDisassemblerPlugin::modifyPassConfig(orc::MaterializationResponsibility &MR,
                                jitlink::LinkGraph &G,
                                jitlink::PassConfiguration &Config) {
    Config.PostAllocationPasses.push_back([this, &MR](jitlink::LinkGraph &G) {
        SmallVector<std::pair<JITTargetAddress, orc::SymbolStringPtr>, 4> Stubs;
        StringMap<JITTargetAddress> StubFunctions;
        size_t PendingStubs = 0;
        for (auto Sym : G.defined_symbols()) {
            auto Name = Sym->getName();
            //anonymous symbols are not stubs
            if (Name.empty()) {
                continue;
            }
            if (Name.consume_front(StubPrefix)) {
                PendingStubs++;
                auto It = StubFunctions.find(Name);
                if (It == StubFunctions.end()) {
                    StubFunctions[Name] = 0; // placeholder until we find the function
                } else {
                    assert(It->second);
                    Stubs.push_back({It->second, ES.intern(Name)}); // it's the real address stored there
                }
            } else {
                auto It = StubFunctions.find(Name);
                if (It != StubFunctions.end()) {
                    assert(!It->second);
                    Stubs.push_back({Sym->getAddress().getValue(), ES.intern(Name)});
                } else {
                    StubFunctions[Name] = Sym->getAddress().getValue();
                }
            }
        }
        assert(PendingStubs == Stubs.size() && "Found a stub without a function");
        if (!Stubs.empty()) {
            cantFail(MR.withResourceKeyDo([&](orc::ResourceKey K) {
                std::lock_guard<std::mutex> Lock(ReverseStubsMutex);
                auto &Owned = OwnedStubs[K];
                for (auto &Stub : Stubs) {
                    ReverseStubs[Stub.first] = Stub.second;
                    Owned.push_back(Stub.first);
                }
            }));
        }
        return Error::success();
    });
}

//This should only have an impact if we fail post-allocation
Error StubDisassemblerPlugin::notifyFailed(orc::MaterializationResponsibility &MR) {
    return MR.withResourceKeyDo([this](orc::ResourceKey K) { cantFail(notifyRemovingResources(K)); });
}
Error StubDisassemblerPlugin::notifyRemovingResources(orc::ResourceKey K) {
    auto Stubs = OwnedStubs.find(K);
    if (Stubs != OwnedStubs.end()) {
        for (auto &Stub : Stubs->second) {
            ReverseStubs.erase(Stub);
        }
        OwnedStubs.erase(Stubs);
    }
    return Error::success();
}
void StubDisassemblerPlugin::notifyTransferringResources(orc::ResourceKey DstKey,
                                            orc::ResourceKey SrcKey) {
    std::lock_guard<std::mutex> Lock(ReverseStubsMutex);
    auto Src = OwnedStubs.find(SrcKey);
    if (Src == OwnedStubs.end())
        return;
    auto Dst = OwnedStubs.find(DstKey);
    if (Dst == OwnedStubs.end()) {
        OwnedStubs[DstKey] = std::move(Src->second);
    } else {
        Dst->second.append(Src->second.begin(), Src->second.end());
    }
    OwnedStubs.erase(Src);
}

JITTargetAddress StubDisassemblerPlugin::launderStub(JITTargetAddress MaybeStub) {
    orc::SymbolStringPtr Name;
    {
        std::lock_guard<std::mutex> Lock(ReverseStubsMutex);
        auto It = ReverseStubs.find(MaybeStub);
        if (It == ReverseStubs.end()) {
            return MaybeStub;
        }
        Name = It->second;
    }
    //This thing has an early exit anyways, just let it handle the address lookup by name
    return Manager.optimize(std::move(Name), JD, 0, 0).Address;
}

void FunctionCache::store(orc::SymbolStringPtr Name, std::unique_ptr<Module> M) {
    auto Lock = *Functions;
    auto &Entry = (*Lock)[Name];
    assert(!Entry && "Storing function module twice");
    Entry = std::make_unique<FunctionInfo>();
    Entry->Stub = nullptr;
    Entry->Module = serializeModule(*M);
    Entry->Version = -1;
    Entry->NextVersion = 0;
    Entry->OptLevel = 0;
}

FunctionCache::FunctionInfo *FunctionCache::lookup(orc::SymbolStringPtr Name) {
    auto Lock = *Functions;
    auto It = Lock->find(Name);
    if (It == Lock->end()) {
        return nullptr;
    }
    return It->second.get();
}

namespace {
class ReoptimizedMaterializationUnit : public orc::MaterializationUnit {
public:

    ReoptimizedMaterializationUnit(orc::ThreadSafeModule TSM, orc::SymbolStringPtr Name, orc::IRLayer &Layer)
    : orc::MaterializationUnit(getInterface(Layer.getExecutionSession(), Name, TSM)), TSM(std::move(TSM)), Name(std::move(Name)), Layer(Layer) {}

    StringRef getName() const override {
        return *Name;
    }

private:

    static Interface getInterface(orc::ExecutionSession &ES, const orc::SymbolStringPtr &Name, orc::ThreadSafeModule &TSM) {
        return TSM.withModuleDo([&](Module &M) {
            auto F = M.getFunction(demangle(*Name));
            assert(F);
            orc::SymbolFlagsMap Symbols;
            Symbols[Name] = JITSymbolFlags::fromGlobalValue(*F);
            for (auto &alias : M.aliases()) {
                Symbols[ES.intern(alias.getName())] = JITSymbolFlags::fromGlobalValue(alias);
            }
            return Interface(Symbols, nullptr);
        });
    }

    orc::ThreadSafeModule TSM;
    orc::SymbolStringPtr Name;
    orc::IRLayer &Layer;

    void materialize(std::unique_ptr<orc::MaterializationResponsibility> R) override {
        Layer.emit(std::move(R), std::move(TSM));
    }

    void discard(const orc::JITDylib &JD, const orc::SymbolStringPtr &Name) override {
        assert(TSM);
        TSM.withModuleDo([&Name](Module &M) {
            auto GV = M.getNamedValue(demangle(*Name));
            assert(GV && !GV->isDeclaration());
            GV->setLinkage(GlobalValue::AvailableExternallyLinkage);
        });
    }
};
}

ReoptimizationManager::OptimizationResult ReoptimizationManager::optimize(orc::SymbolStringPtr Name, orc::JITDylib &JD, int32_t Version, uint32_t OptLevel) {
    ++NumReoptimized;
    OptLevel = std::min(std::max(OptLevel, MinOptLevel), MaxOptLevel);
    assert(OptLevel <= MaxOptLevel && OptLevel >= MinOptLevel && "Invalid optimization level");
    assert(PostPartitioningLayer && "ReoptimizationManager PostPartitioningLayer not set");
    auto *Entry = Cache.lookup(Name);
    if (!Entry) {
        return {0, 0, 0};
    }
    auto &ES = PostPartitioningLayer->getExecutionSession();
    assert(Version >= 0 && "Version must be nonnegative");
    auto ComputedVersion = Version * int32_t(MaxOptLevel) + int32_t(OptLevel);
    {
        //Early exit if we already have the version we want
        auto Current = Entry->Version.load(std::memory_order::memory_order_seq_cst);
        if (Current >= ComputedVersion) {
            ++NumPreoptimized;
            std::lock_guard<std::mutex> Lock(Entry->StubMutex);
            return {*Entry->Stub, Entry->Version.load(std::memory_order_relaxed), Entry->OptLevel};
        }
    }
    //We need to reoptimize, but we know that the stub pointer is definitely valid
    auto TSCtx = ContextPool.get();
    orc::ThreadSafeModule TSM;
    {
        auto Lock = TSCtx->getLock();
        auto M = deserializeModule(Entry->Module, *TSCtx->getContext());
        //Annotate module with requested optlevel
        M->addModuleFlag(Module::ModFlagBehavior::Error, OptlevelMDName, OptLevel);
        //Annotate module with requested version
        M->addModuleFlag(Module::ModFlagBehavior::Error, ReoptimizeMDName, ComputedVersion);

        //Add profiling flags
        ProfilingFlags Profiling;
        Profiling.ApplyPGO = OptLevel == MaxOptLevel && EnablePGOProfiling && EnableReoptimization;
        Profiling.ProfileAllocations = false;
        //Branch profiling is heavy, so don't do it when we enable vectorization
        Profiling.ProfileBranches = OptLevel <= 1 && EnablePGOProfiling && EnableReoptimization;
        Profiling.ProfileCalls = OptLevel != MaxOptLevel && EnableReoptimization;

        auto F = M->getFunction(*Name);
        assert(F && "Function not found in module");
        F->setMetadata(ProfMDName, Profiling.toMDNode(M->getContext()));

        if (OptLevel != MaxOptLevel && EnableReoptimization) {
            //We want to trigger recompilation later, so we add a callback when we get called enough times
            //let's just start by saying 'enough times' is 10 * (optlevel + 1) ^ 2
            //this unrolls to {10, 40, 90}
            F->setMetadata("julia.prof.reporter", MDTuple::get(F->getContext(), {
                //Limit
                ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(F->getContext()), computeOptLevelLimit(OptLevel))),
                //Function
                ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(F->getContext()), reinterpret_cast<uint64_t>(&jl_reoptimize_entrypoint))),
                //Args...
                ConstantAsMetadata::get(literal_static_pointer_val((*Name).data(), Type::getInt8PtrTy(F->getContext()))),
                ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(F->getContext()), (*Name).size())),
                ConstantAsMetadata::get(literal_static_pointer_val(&JD, Type::getInt8PtrTy(F->getContext()))),
                ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(F->getContext()), Version)),
                ConstantAsMetadata::get(ConstantInt::get(Type::getInt32Ty(F->getContext()), OptLevel)),
                ConstantAsMetadata::get(literal_static_pointer_val(this, Type::getInt8PtrTy(F->getContext()))),
            }));
        }
        TSM = orc::ThreadSafeModule(std::move(M), *TSCtx);
    }

    //Do the actual optimization
    auto Mangled = ES.intern(mangle(*Name, ComputedVersion));
    auto GetBetterVersion = [&]() -> Optional<OptimizationResult> {
        auto Current = Entry->Version.load(std::memory_order::memory_order_relaxed);
        if (Current >= ComputedVersion) {
            //backcompute the version number from the registered best version
            return OptimizationResult{*Entry->Stub, (Current - int32_t(Entry->OptLevel)) / int32_t(MaxOptLevel), Entry->OptLevel};
        }
        return None;
    };
    //This might fail due to a race condition when two threads both attempt to reoptimize the same method. Only one of their
    //define calls will succeed, and the other will fail, which is ok as long as a registration succeeds. The failing
    //thread will then just wait for the stub to be updated and return that.
    if (auto Err = JD.define(std::make_unique<ReoptimizedMaterializationUnit>(std::move(TSM), Mangled, *PostPartitioningLayer))) {
        consumeError(std::move(Err));
        std::unique_lock<std::mutex> Lock(Entry->StubMutex);
        Entry->StubCV.wait(Lock, [&]() { return !!GetBetterVersion(); });
        ++NumRaced;
        return *GetBetterVersion();
    }
    auto Optimized = cantFail(ES.lookup({{&JD, orc::JITDylibLookupFlags::MatchAllSymbols}}, Mangled));
    {
        if (!Entry->Stub) {
            //The lookup needs to happen outside the lock, but the pointer is stable so the write doesn't need
            //to be inside the lock
            Entry->Stub = reinterpret_cast<JITTargetAddress*>(cantFail(ES.lookup({{&JD, orc::JITDylibLookupFlags::MatchAllSymbols}}, ES.intern(getStubGVName(*Name)))).getAddress());
        }
        std::unique_lock<std::mutex> Lock(Entry->StubMutex);
        if (auto Better = GetBetterVersion()) {
            ++NumRaced;
            return *Better;
        }
        *Entry->Stub = Optimized.getAddress();
        Entry->OptLevel = OptLevel;
        Entry->Version.store(ComputedVersion, std::memory_order::memory_order_seq_cst);
        Entry->StubCV.notify_all(); //all the waiters just got a better version that what was there before
        return {Optimized.getAddress(), Version, OptLevel};
    }
}

ReoptimizationManager::ReoptimizationManager(JITFunctionProfiler &Profiler, FunctionCache &Cache, uint32_t MinOptLevel, uint32_t MaxOptLevel)
: Profiler(Profiler), Cache(Cache), PostPartitioningLayer(nullptr), ContextPool([](){ return std::make_unique<LLVMContext>(); }), MinOptLevel(MinOptLevel), MaxOptLevel(MaxOptLevel) {
}

bool ReoptimizationQueue::enqueue(orc::SymbolStringPtr Name, orc::JITDylib *JD, int32_t Version, uint16_t Priority) {
    std::lock_guard<std::mutex> Lock(QueueMutex);
    if (Dead) {
        return false;
    }
    //We care more about getting the latest version in, since that's going to clobber any old level
    int64_t ComputedPriority = static_cast<int64_t>(Version) << sizeof(Priority) * CHAR_BIT | Priority;
    Queue.push({Name, JD, ComputedPriority, Version, -1, Idx++});
    QueueCV.notify_one();
    return true;
}

bool ReoptimizationQueue::speculate(llvm::orc::SymbolStringPtr Name, llvm::orc::JITDylib *JD, int32_t Version, uint32_t OptLevel, llvm::BranchProbability Probability) {
    if (!EnableSpeculation) {
        return false;
    }
    std::lock_guard<std::mutex> Lock(QueueMutex);
    if (Dead) {
        return false;
    }
    //We care more about getting higher-probability branches than about getting the latest version in,
    //since a versioning conflict will cause an early exit, but higher probability branches are more
    //likely to occur more often
    int64_t ComputedPriority = std::max(static_cast<int32_t>(Probability.getNumerator()), std::numeric_limits<int32_t>::max());
    ComputedPriority = ComputedPriority << sizeof(Version) * CHAR_BIT | Version;
    Queue.push({Name, JD, ComputedPriority, Version, static_cast<int32_t>(OptLevel), Idx++});
    QueueCV.notify_one();
    return true;
}

Optional<ReoptimizationQueue::Entry> ReoptimizationQueue::try_dequeue() {
    std::unique_lock<std::mutex> Lock(QueueMutex);
    if (Queue.empty() || Dead) {
        return None;
    }
    auto Entry = Queue.top();
    Queue.pop();
    return Entry;
}

Optional<ReoptimizationQueue::Entry> ReoptimizationQueue::wait_dequeue() {
    std::unique_lock<std::mutex> Lock(QueueMutex);
    if (Dead) {
        return None;
    }
    if (Queue.empty()) {
        ++Waiters;
        QueueCV.wait(Lock, [this](){ return !Queue.empty() || Dead; });
        if (!--Waiters) {
            DeadCV.notify_one();
        }
        if (Dead) {
            return None;
        }
    }
    auto Entry = Queue.top();
    Queue.pop();
    return Entry;
}

ReoptimizationQueue::~ReoptimizationQueue() {
    std::unique_lock<std::mutex> Lock(QueueMutex);
    Dead = true;
    QueueCV.notify_all();
    //Delay destruction and deallocation until all of our other threads have exited
    if (Waiters) {
        DeadCV.wait(Lock, [this](){ return !Waiters; });
    }
}

bool ReoptimizationManager::profileReentry(orc::SymbolStringPtr Name, orc::JITDylib *JD, int32_t Version, uint32_t OptLevel) {
    ++NumProfileReoptimized;
    //Just directly translate the optlevel to a priority so that bigger optlevels get higher priority
    return Queue.enqueue(Name, JD, Version, OptLevel);
}

bool ReoptimizationManager::blockingRecompileNext() {
    auto Entry = Queue.wait_dequeue();
    if (!Entry) {
        return false;
    }
    auto ReloadOptLevel = [&]() {
        auto Profile = Profiler.getProfile(*Entry->Name);
        assert(Profile && "Forgot to profile this function?");
        auto CallCount = Profile->CallCount.load(std::memory_order::memory_order_relaxed);
        //The compiler may be able to pull some magic to invert the optlevel to call count mapping,
        //but in the general case this is more robust. Again, not a bottleneck on a background thread.
        for (uint32_t i = MinOptLevel; i <= MaxOptLevel; i++) {
            if (CallCount < computeOptLevelLimit(i)) {
                return i;
            }
        }
        return MaxOptLevel;
    };
    uint32_t OptLevel = Entry->speculative() ? Entry->OptLevel : ReloadOptLevel();
    NumSpeculated += Entry->speculative();
    auto Optimized = optimize(Entry->Name, *Entry->JD, Entry->Version, OptLevel);
    //This function wasn't optimizable (happens if we speculate on a function but we rejected
    //reoptimizing it because of function size or name)
    if (!Optimized.Address) {
        return true; // Although we failed to optimize, we still want to keep going for the next entry
    }
    assert((Optimized.Version > Entry->Version || (Optimized.Version == Entry->Version && Optimized.OptLevel >= OptLevel)) && "Optimization failed to improve the version");
    if (Optimized.Version == Entry->Version) {
        //If we didn't deoptimize the function, then we might hit a race condition between
        //updating call count and updating the stub. By now we've updated the stub, so as
        //long as the optlevel didn't change we won't have to recompile again. If it did,
        //then it's our responsibility to requeue the function.
        uint32_t Current = ReloadOptLevel();
        if (OptLevel != Current) {
            //OptLevel changed while we were optimizing, reoptimize because we don't know
            //if we passed the threshold before or after the stub was updated
            //worst case, we pop it off the queue and reoptimize it again, but that's
            //ideally on a background thread and thus doesn't compromise the application
            profileReentry(std::move(Entry->Name), Entry->JD, Entry->Version, Current);
        }
    }
    return true;
}

PreservedAnalyses SpeculationPass::run(Function &F, FunctionAnalysisManager &FAM) {
    if (!ReoptMgr || !JD) {
        return PreservedAnalyses::all();
    }
    if (!EnableSpeculation) {
        return PreservedAnalyses::all();
    }
    auto ReoptMD = F.getMetadata(ReoptimizeMDName);
    if (!ReoptMD) {
        return PreservedAnalyses::all(); // Don't speculate on non-reoptimized functions, it's not worth it
    }
    auto OptLevel = cast<ConstantInt>(cast<ConstantAsMetadata>(F.getMetadata(OptlevelMDName))->getValue())->getZExtValue();
    auto &BFI = FAM.getResult<BlockFrequencyAnalysis>(F);
    auto Entry = BFI.getEntryFreq();
    for (auto &BB : F) {
        auto BF = BFI.getBlockFreq(&BB);
        if (!BF.getFrequency()) {
            continue; // Ignore blocks with no frequency
        }
        if (/*Entry / BF.getFrequency() < 2*/true) {
            // This block is more likely than not to be taken,
            // so let's compile any functions in it at the same
            // optlevel as F
            for (auto &I : BB) {
                if (auto *CI = dyn_cast<CallInst>(&I)) {
                    if (auto *Callee = CI->getCalledFunction()) {
                        auto Name = JD->getExecutionSession().intern(Callee->getName());
                        if (auto Info = ReoptMgr->getCache().lookup(Name)) {
                            ++NumSpeculationInferred;
                            ReoptMgr->speculate(std::move(Name), JD, Info->Version.load(std::memory_order::memory_order_seq_cst), OptLevel, BranchProbability(std::min(BF.getFrequency(), Entry), Entry));
                        }
                    }
                }
            }
        }
    }
    //We didn't touch the function, we just adjusted state
    //outside the function to try to improve compile perf
    return PreservedAnalyses::all();
}
