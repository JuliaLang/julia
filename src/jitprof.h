#ifndef JL_JITPROF_H
#define JL_JITPROF_H

#include "llvm-version.h"

#include <llvm/Passes/PassBuilder.h>

struct ProfilingFlags {
    bool ProfileAllocations;
    bool ProfileBranches;
    bool ProfileCalls;
    bool ApplyPGO;

    llvm::MDNode *toMDNode(llvm::LLVMContext &Ctx);
    static ProfilingFlags fromMDNode(llvm::MDNode *MDN);
};

void preannotateBranches(llvm::Function &F);

struct FunctionProfile {

    struct BranchInfo {
        std::atomic<uint64_t> Taken;
        std::atomic<uint64_t> Total;
    };

    struct AllocInfo {
        std::atomic<uint64_t> Size;
        std::atomic<uint64_t> Count;
    };

    std::atomic<uint64_t> CallCount;
    std::mutex BranchesMutex;
    //This is a std::vector to avoid an indirection through a unique_ptr
    std::vector<BranchInfo> BranchProfiles;
    AllocInfo Preopt;
    AllocInfo Postopt;

    unsigned Loops;
    unsigned BBs;
    unsigned Insts;
};

class JITFunctionProfiler;

class JITPreoptimizationProfiler : public llvm::PassInfoMixin<JITPreoptimizationProfiler> {
public:
    JITPreoptimizationProfiler() : JITProf(nullptr) {}
    explicit JITPreoptimizationProfiler(JITFunctionProfiler &JFP) : JITProf(&JFP) {}

    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
    JITFunctionProfiler *JITProf;
};

class JITPostoptimizationProfiler : public llvm::PassInfoMixin<JITPostoptimizationProfiler> {
public:
    JITPostoptimizationProfiler() : JITProf(nullptr) {}
    explicit JITPostoptimizationProfiler(JITFunctionProfiler &JFP) : JITProf(&JFP) {}

    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
    JITFunctionProfiler *JITProf;
};

class JITFunctionProfiler {
public:

    FunctionProfile &getOrCreateProfile(llvm::StringRef Name, llvm::unique_function<std::unique_ptr<FunctionProfile>()> CreateProfile);
    FunctionProfile *getProfile(llvm::StringRef Name);

    void dump(llvm::raw_ostream &OS) const;

    JITPreoptimizationProfiler createPreoptimizationProfiler() {
        return JITPreoptimizationProfiler(*this);
    }

    JITPostoptimizationProfiler createPostoptimizationProfiler() {
        return JITPostoptimizationProfiler(*this);
    }

private:
    friend class JITPreoptimizationProfiler;
    friend class JITPostoptimizationProfiler;

    mutable std::mutex FunctionProfilesMutex;
    llvm::StringMap<std::unique_ptr<FunctionProfile>> FunctionProfiles;
};

#endif
