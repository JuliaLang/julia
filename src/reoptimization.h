#ifndef JL_REOPTIMIZATION_H
#define JL_REOPTIMIZATION_H

#include <llvm/ExecutionEngine/Orc/IndirectionUtils.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#include <llvm/Passes/PassBuilder.h>

#include "concurrent-utils.h"
#include "jitprof.h"

class FunctionCache {
public:

    struct FunctionInfo {
        //The stub must be written at the same time the version and optlevel are written,
        //so that future optimize operations can update the stub safely
        std::mutex StubMutex;
        llvm::JITTargetAddress *Stub;
        std::atomic<int32_t> Version;
        uint32_t OptLevel;
        llvm::SmallVector<char, 0> Module;
        std::atomic<int32_t> NextVersion;
    };

    void store(llvm::orc::SymbolStringPtr Name, std::unique_ptr<llvm::Module> M);

    FunctionInfo *lookup(llvm::orc::SymbolStringPtr Name);

private:
    Locked<llvm::DenseMap<llvm::orc::SymbolStringPtr, std::unique_ptr<FunctionInfo>>> Functions;
};

struct SymbolPromoter {
    Locked<llvm::orc::SymbolLinkagePromoter> Promoter;

    auto run(llvm::Module &M) {
        return (**Promoter)(M);
    }
};

size_t getModuleOptLevel(llvm::Module &M, bool DetermineOptLevel = false);

llvm::StringRef getBaseName(llvm::StringRef Name);

class ReoptimizationManager;

class FunctionPartitioner {
public:

    typedef llvm::unique_function<bool(const llvm::Function &)> PartitionFunction;

    FunctionPartitioner(llvm::orc::ExecutionSession &ES, FunctionCache &Cache,
        ReoptimizationManager &Manager, const llvm::orc::IRSymbolMapper::ManglingOptions &ManglingOptions, PartitionFunction ShouldPartition)
    : ES(ES), Cache(Cache), ReoptMgr(Manager), ManglingOptions(ManglingOptions),
      ShouldPartition(std::move(ShouldPartition)), Promoter(std::make_unique<SymbolPromoter>()) {}

    llvm::Expected<llvm::orc::ThreadSafeModule> operator()(llvm::orc::ThreadSafeModule TSM,
                                                           llvm::orc::MaterializationResponsibility &R);
private:
    llvm::orc::ExecutionSession &ES;
    FunctionCache &Cache;
    ReoptimizationManager &ReoptMgr;
    const llvm::orc::IRSymbolMapper::ManglingOptions &ManglingOptions;
    PartitionFunction ShouldPartition;
    std::unique_ptr<SymbolPromoter> Promoter;
};

llvm::Expected<llvm::orc::ThreadSafeModule> FunctionMangler(llvm::orc::ThreadSafeModule TSM,
                                                           llvm::orc::MaterializationResponsibility &R);

class StubDisassemblerPlugin : public llvm::orc::ObjectLinkingLayer::Plugin {
public:
    StubDisassemblerPlugin(llvm::orc::ExecutionSession &ES, llvm::orc::JITDylib &JD, ReoptimizationManager &Manager)
        : ES(ES), JD(JD), Manager(Manager) {}

    void modifyPassConfig(llvm::orc::MaterializationResponsibility &MR,
                                  llvm::jitlink::LinkGraph &G,
                                  llvm::jitlink::PassConfiguration &Config) override;

    //This should only have an impact if we fail post-allocation
    llvm::Error notifyFailed(llvm::orc::MaterializationResponsibility &MR) override;
    llvm::Error notifyRemovingResources(llvm::orc::ResourceKey K) override;
    void notifyTransferringResources(llvm::orc::ResourceKey DstKey,
                                             llvm::orc::ResourceKey SrcKey) override;

    llvm::JITTargetAddress launderStub(llvm::JITTargetAddress MaybeStub);
private:
    llvm::orc::ExecutionSession &ES;
    llvm::orc::JITDylib &JD;
    ReoptimizationManager &Manager;
    //We don't need to keep a pending stubs map, because if we fail before we emit nobody can access the stubs,
    //and if somebody does access the stub then we definitely emitted.
    std::mutex ReverseStubsMutex;
    llvm::DenseMap<llvm::JITTargetAddress, llvm::orc::SymbolStringPtr> ReverseStubs;
    llvm::DenseMap<llvm::orc::ResourceKey, llvm::SmallVector<llvm::JITTargetAddress, 0>> OwnedStubs;
};

class ReoptimizationQueue {
public:
    struct Entry {
        llvm::orc::SymbolStringPtr Name;
        llvm::orc::JITDylib *JD;
        int32_t Version;
        uint32_t Idx;
        int64_t Priority;

        bool operator<(const Entry &Other) const {
            return Priority < Other.Priority || (Priority == Other.Priority && Idx < Other.Idx);
        }
    };

    bool enqueue(llvm::orc::SymbolStringPtr Name, llvm::orc::JITDylib *JD, int32_t Version, uint16_t Priority);
    llvm::Optional<Entry> try_dequeue();
    llvm::Optional<Entry> wait_dequeue();

    ~ReoptimizationQueue();
private:
    std::mutex QueueMutex;
    std::condition_variable QueueCV;
    std::condition_variable DeadCV;
    std::priority_queue<Entry> Queue;
    uint32_t Idx = 0;
    uint32_t Waiters = 0;
    bool Dead = false;
};

class ReoptimizationManager {
public:
    ReoptimizationManager(JITFunctionProfiler &Profiler, FunctionCache &Cache, uint32_t MinOptLevel = 0, uint32_t MaxOptLevel = 3);

    struct OptimizationResult {
        llvm::JITTargetAddress Address;
        int32_t Version;
        uint32_t OptLevel;
    };

    OptimizationResult optimize(llvm::orc::SymbolStringPtr Name, llvm::orc::JITDylib &JD, int32_t Version, uint32_t OptLevel);

    bool profileReentry(llvm::orc::SymbolStringPtr Name, llvm::orc::JITDylib *JD, int32_t Version, uint32_t OptLevel);

    std::pair<uint32_t, uint32_t> getOptLevelRange() const {
        return std::make_pair(MinOptLevel, MaxOptLevel);
    }

    void setPostPartitioningLayer(llvm::orc::IRLayer *Layer) {
        assert(!PostPartitioningLayer && "PostPartitioningLayer already set");
        PostPartitioningLayer = Layer;
    }

    bool blockingRecompileNext();

    void continuousRecompile() {
        while (blockingRecompileNext());
    }

private:

    JITFunctionProfiler &Profiler;
    FunctionCache &Cache;
    llvm::orc::IRLayer *PostPartitioningLayer;
    ResourcePool<llvm::orc::ThreadSafeContext, 0, std::queue<llvm::orc::ThreadSafeContext>> ContextPool;
    ReoptimizationQueue Queue;
    uint32_t MinOptLevel;
    uint32_t MaxOptLevel;
};

#endif
