#include <llvm/ADT/SmallSet.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/Cloning.h>

#include "reoptimization.h"

#include "jitlayers.h"

using namespace llvm;

cl::opt<bool> EnablePGOProfiling("julia-enable-pgo-profiling", cl::init(true),
                                  cl::desc("Enable PGO profiling"));

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
    SmallSet<GlobalAlias*, 2> MovedAliases;
    auto Mod = CloneModule(M, VMap, [&MovedAliases, F = &F](const GlobalValue *GV) {
        if (auto GA = dyn_cast<GlobalAlias>(GV)) {
            if (GA->getAliaseeObject() == F) {
                MovedAliases.insert(const_cast<GlobalAlias*>(GA));
                return true;
            }
            return false;
        } else {
            return GV == F;
        }
    });
    // Aliases cannot refer to a declaration, so pretend they're actual function declarations
    for (auto GA : MovedAliases) {
        Function *NewF = Function::Create(cast<FunctionType>(F.getValueType()), F.getLinkage(), "", M);
        NewF->copyAttributesFrom(&F);
        NewF->takeName(GA);
        GA->eraseFromParent();
    }
    F.deleteBody();
    return Mod;
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

Expected<orc::ThreadSafeModule> FunctionPartitioner::operator()(orc::ThreadSafeModule TSM,
                                                        orc::MaterializationResponsibility &R) {
    orc::SymbolFlagsMap Stubs;
    TSM.withModuleDo([this, &Stubs, &R](Module &M) {
        auto PromotedGlobals = Promoter->run(M);
        if (!PromotedGlobals.empty()) {
            orc::SymbolFlagsMap SymbolFlags;
            orc::IRSymbolMapper::add(ES, ManglingOptions, PromotedGlobals, SymbolFlags);

            cantFail(R.defineMaterializing(SymbolFlags));
        }
        SmallVector<Function *> ToPartition;
        for (auto &F : M) {
            if (F.isDeclaration())
                continue;
            if (!ShouldPartition(F))
                continue;
            ToPartition.push_back(&F);
        }
        for (auto Func : ToPartition) {
            auto &F = *Func;
            auto Name = ES.intern(F.getName());
            Cache.store(Name, partitionFunction(M, F));
            auto &CC = createCallback(M, F, Name, &R.getTargetJITDylib(), 0, ReoptMgr.getOptLevelRange().first, &ReoptMgr);
            auto &GV = createStub(M, F, CC);
            Stubs[ES.intern(GV.getName())] = JITSymbolFlags::fromGlobalValue(GV);
        }
    });
    cantFail(R.defineMaterializing(Stubs));
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
        assert(!Name.consumeInteger(10, version) && "Invalid version number");
        assert(Name.consume_front(".") && "Invalid mangled name");
    }
    return Name;
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
    Entry->NextVersion = 1;
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
        TSM.withModuleDo([&JD, &Name](Module &M) {
            auto GV = M.getNamedValue(demangle(*Name));
            assert(GV && !GV->isDeclaration());
            GV->setLinkage(GlobalValue::AvailableExternallyLinkage);
        });
    }
};
}

ReoptimizationManager::OptimizationResult ReoptimizationManager::optimize(orc::SymbolStringPtr Name, orc::JITDylib &JD, int32_t Version, uint32_t OptLevel) {
    assert(OptLevel <= MaxOptLevel && OptLevel >= MinOptLevel && "Invalid optimization level");
    assert(PostPartitioningLayer && "ReoptimizationManager PostPartitioningLayer not set");
    auto *Entry = Cache.lookup(Name);
    if (!Entry) {
        return {0, 0, 0};
    }
    auto &ES = PostPartitioningLayer->getExecutionSession();
    assert(Version >= 0 && "Version must be positive");
    {
        //Early exit if we already have the version we want
        auto Current = Entry->Version.load(std::memory_order::memory_order_seq_cst);
        if (Current >= Version) {
            std::lock_guard<std::mutex> Lock(Entry->StubMutex);
            return {*Entry->Stub, Entry->Version.load(std::memory_order_relaxed), Entry->OptLevel};
        }
        //Don't synchronize here because repeated lookups should result in the same address
        if (!Entry->Stub) {
            Entry->Stub = reinterpret_cast<JITTargetAddress*>(cantFail(ES.lookup({{&JD, orc::JITDylibLookupFlags::MatchAllSymbols}}, ES.intern(getStubGVName(*Name)))).getAddress());
        }
    }
    //We need to reoptimize, but we know that the stub pointer is definitely valid
    auto TSCtx = ContextPool.get();
    auto M = deserializeModule(Entry->Module, *TSCtx->getContext());
    //Annotate module with requested optlevel
    M->addModuleFlag(Module::ModFlagBehavior::Error, OptlevelMDName, OptLevel);
    //Annotate module with requested version
    M->addModuleFlag(Module::ModFlagBehavior::Error, ReoptimizeMDName, Version);

    //Add profiling flags
    ProfilingFlags Profiling;
    Profiling.ApplyPGO = OptLevel == MaxOptLevel && EnablePGOProfiling;
    Profiling.ProfileAllocations = false;
    //Branch profiling is heavy, so don't do it when we enable vectorization
    Profiling.ProfileBranches = OptLevel <= 1 && EnablePGOProfiling;
    Profiling.ProfileCalls = OptLevel != MaxOptLevel;

    auto F = M->getFunction(*Name);
    assert(F && "Function not found in module");
    F->setMetadata(ProfMDName, Profiling.toMDNode(M->getContext()));

    if (OptLevel != MaxOptLevel) {
        //We want to trigger recompilation later, so we add a callback when we get called enough times
        //let's just start by saying 'enough times' is 10 * (optlevel + 1) ^ 2
        //this unrolls to {10, 40, 90}
        F->setMetadata("julia.prof.reporter", MDTuple::get(F->getContext(), {
            //Limit
            ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(F->getContext()), 10 * (OptLevel + 1) * (OptLevel + 1))),
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

    //Do the actual optimization
    auto &OptJD = *OptJDs[OptLevel - MinOptLevel];
    auto TSM = orc::ThreadSafeModule(std::move(M), *TSCtx);
    auto Mangled = ES.intern(mangle(*Name, Version));
    cantFail(OptJD.define(std::make_unique<ReoptimizedMaterializationUnit>(std::move(TSM), Mangled, *PostPartitioningLayer)));
    auto Optimized = cantFail(ES.lookup({{&OptJD, orc::JITDylibLookupFlags::MatchAllSymbols}}, Mangled));
    {
        std::lock_guard<std::mutex> Lock(Entry->StubMutex);
        auto Current = Entry->Version.load(std::memory_order::memory_order_relaxed);
        if (Current >= Version) {
            return {*Entry->Stub, Current, Entry->OptLevel};
        }
        *Entry->Stub = Optimized.getAddress();
        Entry->Version.store(Version, std::memory_order::memory_order_relaxed);
        Entry->OptLevel = OptLevel;
        return {Optimized.getAddress(), Version, OptLevel};
    }
}

ReoptimizationManager::ReoptimizationManager(FunctionCache &Cache, orc::JITDylib &JD, uint32_t MinOptLevel, uint32_t MaxOptLevel)
: Cache(Cache), PostPartitioningLayer(nullptr), ContextPool([](){ return std::make_unique<LLVMContext>(); }), MinOptLevel(MinOptLevel), MaxOptLevel(MaxOptLevel) {
    OptJDs.resize(MaxOptLevel - MinOptLevel + 1);
    for (size_t i = 0; i < OptJDs.size(); i++) {
        OptJDs[i] = &cantFail(JD.getExecutionSession().createJITDylib((Twine("__jtier.") + std::to_string(i) + "." + JD.getName()).str()));
    }
}

void ReoptimizationManager::addToLinkOrder(orc::JITDylib &JD) {
    auto SubJDs = cantFail(JD.getDFSLinkOrder());
    for (auto OptJD : OptJDs) {
        for (auto SubJD : SubJDs) {
            OptJD->addToLinkOrder(*SubJD, orc::JITDylibLookupFlags::MatchAllSymbols);
        }
    }
}

bool ReoptimizationQueue::enqueue(orc::SymbolStringPtr Name, orc::JITDylib *JD, int32_t Version, uint16_t Priority) {
    std::lock_guard<std::mutex> Lock(QueueMutex);
    if (Dead) {
        return false;
    }
    Queue.push({Name, JD, Version, Idx++, static_cast<int64_t>(Version) << sizeof(Priority) * CHAR_BIT | Priority});
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
    //Just directly translate the optlevel to a priority so that bigger optlevels get higher priority
    return Queue.enqueue(Name, JD, Version, OptLevel);
}
