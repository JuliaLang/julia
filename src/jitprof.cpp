#include "jitprof.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include "codegen_shared.h"
#include "julia_internal.h"

cl::opt<bool> ForceProfileAllocations("julia-force-profile-allocations", cl::init(false), cl::Hidden);
cl::opt<bool> ForceProfileCalls("julia-force-profile-calls", cl::init(false), cl::Hidden);
cl::opt<bool> ForceProfileBranches("julia-force-profile-branches", cl::init(false), cl::Hidden);
using namespace llvm;

static auto extractProfNum(MDTuple *MD, unsigned Idx) {
    auto *MDV = cast<ConstantAsMetadata>(MD->getOperand(Idx))->getValue();
    return cast<ConstantInt>(MDV);
}

static ProfilingFlags fromFunction(Function &F) {

    ProfilingFlags Flags = {false, false, false, false};
    if (auto ProfMD = F.getMetadata("julia.prof")) {
        Flags = ProfilingFlags::fromMDNode(ProfMD);
    }
    Flags.ProfileAllocations |= ForceProfileAllocations;
    Flags.ProfileBranches |= ForceProfileBranches;
    Flags.ProfileCalls |= ForceProfileCalls;
    return Flags;
}

ProfilingFlags ProfilingFlags::fromMDNode(MDNode *MDN) {
    auto *MD = cast<MDTuple>(MDN);
    assert(MD->getNumOperands() == 4);
    return ProfilingFlags{
        !!extractProfNum(MD, 0)->getZExtValue(),
        !!extractProfNum(MD, 1)->getZExtValue(),
        !!extractProfNum(MD, 2)->getZExtValue(),
        !!extractProfNum(MD, 3)->getZExtValue(),
    };
}

MDNode *ProfilingFlags::toMDNode(LLVMContext &Ctx) {
    return MDTuple::get(Ctx, {
        ConstantAsMetadata::get(ConstantInt::getBool(Ctx, ProfileAllocations)),
        ConstantAsMetadata::get(ConstantInt::getBool(Ctx, ProfileBranches)),
        ConstantAsMetadata::get(ConstantInt::getBool(Ctx, ProfileCalls)),
        ConstantAsMetadata::get(ConstantInt::getBool(Ctx, ApplyPGO)),
    });
}

static void addCallInstrumentation(FunctionProfile &Prof, Function &F) {
    IRBuilder<> Builder(&*F.getEntryBlock().getFirstInsertionPt());
    auto CallPtr = ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(F.getContext()), (uint64_t)&Prof.CallCount), Type::getInt64PtrTy(F.getContext()));
    //Increment call count by one
    auto CallCount = Builder.CreateAtomicRMW(AtomicRMWInst::Add, CallPtr, ConstantInt::get(Type::getInt64Ty(F.getContext()), 1), Align(alignof(decltype(Prof.CallCount))), AtomicOrdering::Monotonic);

    //We might also care about reporting when we hit a certain threshold of calls, so we add an additional piece of metadata
    //for threshold, function, and arguments
    auto ReporterMDRaw = F.getMetadata("julia.prof.reporter");
    if (ReporterMDRaw) {
        auto ReporterMD = cast<MDTuple>(ReporterMDRaw);
        assert(ReporterMD->getNumOperands() >= 2);
        auto Limit = extractProfNum(ReporterMD, 0);
        auto Report = Builder.CreateICmpEQ(CallCount, Limit);
        Builder.SetInsertPoint(SplitBlockAndInsertIfThen(Report, &*Builder.GetInsertPoint(), false, MDBuilder(F.getContext()).createBranchWeights(1, Limit->getSExtValue() - Prof.CallCount.load(std::memory_order::memory_order_relaxed))));
        SmallVector<Value *, 8> Args;
        SmallVector<Type *, 8> Types;
        //Collect all the arguments and types
        for (unsigned i = 2; i < ReporterMD->getNumOperands(); i++) {
            auto *Arg = cast<ConstantAsMetadata>(ReporterMD->getOperand(i))->getValue();
            Args.push_back(Arg);
            Types.push_back(Arg->getType());
        }
        auto ReporterFT = FunctionType::get(Type::getVoidTy(F.getContext()), Types, false);
        auto ReporterF = ConstantExpr::getIntToPtr(extractProfNum(ReporterMD, 1), ReporterFT->getPointerTo());
        Builder.CreateCall(ReporterFT, ReporterF, Args);
    }
}

//Annotate all of the branches so that after serialization we can recover branch->branch
//mappings even across different deserializations
void preannotateBranches(Function &F) {
    //We care only about conditional non-constant branches, everything else is either
    //easily handled by the optimizer or annoying to store information for (indirect switch jumps)
    size_t Count = 0;
    for (auto &BB : F) {
        auto *Term = BB.getTerminator();
        if (!isa<BranchInst>(Term))
            continue;
        auto *BI = cast<BranchInst>(Term);
        if (BI->isUnconditional())
            continue;
        if (isa<Constant>(BI->getCondition()))
            continue;
        BI->setMetadata("julia.prof.branch", MDTuple::get(F.getContext(), {ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(F.getContext()), Count++))}));
    }
    if (Count == 0)
        return; //No branches to profile, might as well pretend we didn't ask to profile this function
    F.setMetadata("julia.prof.branch", MDTuple::get(F.getContext(), {ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(F.getContext()), Count))}));
}

static void addBranchInstrumentation(FunctionProfile &Prof, Function &F) {
    std::lock_guard<std::mutex> Lock(Prof.BranchesMutex);
    if (Prof.BranchProfiles.empty()) {
        auto BranchMD = F.getMetadata("julia.prof.branch");
        if (!BranchMD)
            return;
        auto BranchCount = extractProfNum(cast<MDTuple>(BranchMD), 0);
        //Neat trick to avoid an extra unique pointer
        decltype(Prof.BranchProfiles) BranchProfiles(BranchCount->getZExtValue());
        std::swap(Prof.BranchProfiles, BranchProfiles);
        for (auto &BI : Prof.BranchProfiles) {
            BI.Taken = 0;
            BI.Total = 0;
        }
    }
    for (auto &BB : F) {
        auto Term = BB.getTerminator();
        auto BranchMD = Term->getMetadata("julia.prof.branch");
        if (!BranchMD)
            continue;
        assert(isa<BranchInst>(Term) && "Branch metadata can only be attached to branches");
        assert(cast<BranchInst>(Term)->isConditional() && "Branch metadata can only be attached to conditional branches");
        assert(!isa<Constant>(cast<BranchInst>(Term)->getCondition()) && "Branch metadata can only be attached to branches with non-constant condition");
        auto &BI = Prof.BranchProfiles[extractProfNum(cast<MDTuple>(BranchMD), 0)->getZExtValue()];
        auto TakenPtr = ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(F.getContext()), (uint64_t)&BI.Taken), Type::getInt64PtrTy(F.getContext()));
        auto TotalPtr = ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(F.getContext()), (uint64_t)&BI.Total), Type::getInt64PtrTy(F.getContext()));
        IRBuilder<> Builder(Term);
        Builder.CreateAtomicRMW(AtomicRMWInst::Add, TotalPtr, ConstantInt::get(Type::getInt64Ty(F.getContext()), 1), Align(alignof(decltype(BI.Total))), AtomicOrdering::Monotonic);
        Builder.CreateAtomicRMW(AtomicRMWInst::Add, TakenPtr, Builder.CreateZExt(cast<BranchInst>(Term)->getCondition(), Type::getInt64Ty(F.getContext())), Align(alignof(decltype(BI.Taken))), AtomicOrdering::Monotonic);
    }
}

static void applyPGOInstrumentation(FunctionProfile &Prof, Function &F) {
    auto Calls = Prof.CallCount.load(std::memory_order::memory_order_relaxed);
    auto MDB = MDBuilder(F.getContext());
    F.setMetadata(LLVMContext::MD_prof, MDB.createFunctionEntryCount(Calls, false, nullptr));
    std::lock_guard<std::mutex> (Prof.BranchesMutex);
    if (Prof.BranchProfiles.empty())
        return;
    for (auto &BB : F) {
        auto Term = BB.getTerminator();
        auto BranchMD = Term->getMetadata("julia.prof.branch");
        if (!BranchMD)
            continue;
        assert(isa<BranchInst>(Term) && "Branch metadata can only be attached to branches");
        assert(cast<BranchInst>(Term)->isConditional() && "Branch metadata can only be attached to conditional branches");
        assert(!isa<Constant>(cast<BranchInst>(Term)->getCondition()) && "Branch metadata can only be attached to branches with non-constant condition");
        auto &Branch = Prof.BranchProfiles[extractProfNum(cast<MDTuple>(BranchMD), 0)->getZExtValue()];
        assert(isa<BranchInst>(BB.getTerminator()) && "BB ordering changed!");
        assert(cast<BranchInst>(BB.getTerminator())->isConditional() && "Branch is not conditional!");
        auto Taken = Branch.Taken.load(std::memory_order::memory_order_relaxed);
        auto Total = Branch.Total.load(std::memory_order::memory_order_relaxed);
        if (Total > 0) {
            auto MD = MDB.createBranchWeights(Taken, Total - Taken);
            BB.getTerminator()->setMetadata(LLVMContext::MD_prof, MD);
        }
    }
}

static void addAllocInstrumentation(FunctionProfile::AllocInfo &Prof, Function &F, bool Preopt) {
    auto Size = ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(F.getContext()), (uint64_t)&Prof.Size), Type::getInt64PtrTy(F.getContext()));
    auto Count = ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(F.getContext()), (uint64_t)&Prof.Count), Type::getInt64PtrTy(F.getContext()));
    for (auto &BB : F) {
        for (auto &I : BB) {
            if (auto CI = dyn_cast<CallInst>(&I)) {
                if (CI->getCalledFunction()) {
                    Value *AllocSize = nullptr;
                    auto T_int64 = Type::getInt64Ty(F.getContext());
                    IRBuilder<> Builder(CI);
                    if (Preopt) {
                        if (CI->getCalledFunction()->getName() == "julia.gc_alloc_obj") {
                            //Duplicates the pool/big sizing logic from final-lowering, since
                            //that can significantly change the size of the allocation
                            auto sz = cast<ConstantInt>(CI->getArgOperand(1))->getZExtValue();
                            int osize;
                            int offset = jl_gc_classify_pools(sz, &osize);
                            if (offset < 0) {
                                AllocSize = ConstantInt::get(T_int64, sz + sizeof(void*));
                            } else {
                                AllocSize = ConstantInt::get(T_int64, osize);
                            }
                        }
                    }
                    else {
                        auto Name = CI->getCalledFunction()->getName();
                        if (Name.contains("jl_gc_pool_alloc")) {
                            //size is argument 2 of gc_pool_alloc
                            AllocSize = Builder.CreateIntCast(CI->getArgOperand(2), T_int64, false);
                        } else if (Name.contains("jl_gc_big_alloc")) {
                            //size is argument 1 of gc_big_alloc
                            AllocSize = Builder.CreateIntCast(CI->getArgOperand(1), T_int64, false);
                        }
                    }
                    if (AllocSize) {
                        //Increment size by the size of the allocation
                        Builder.CreateAtomicRMW(AtomicRMWInst::Add, Size, Builder.CreateIntCast(AllocSize, T_int64, false), Align(alignof(decltype(Prof.Size))), AtomicOrdering::Monotonic);
                        //Increment count by one
                        Builder.CreateAtomicRMW(AtomicRMWInst::Add, Count, ConstantInt::get(T_int64, 1), Align(alignof(decltype(Prof.Count))), AtomicOrdering::Monotonic);
                    }
                }
            }
        }
    }
}

PreservedAnalyses JITPostoptimizationProfiler::run(Function &F, FunctionAnalysisManager &FAM) {
    static_assert(sizeof(void*) == sizeof(uint64_t), "This code assumes 64-bit pointers");
    if (!JITProf)
        return PreservedAnalyses::all();

    auto Flags = fromFunction(F);

    if (!Flags.ProfileAllocations && !Flags.ProfileCalls)
        return PreservedAnalyses::all();

    auto Prof = JITProf->getProfile(F.getName());
    if (!Prof)
        return PreservedAnalyses::all();

    if (Flags.ProfileCalls) {
        addCallInstrumentation(*Prof, F);
    }

    if (Flags.ProfileAllocations) {
        addAllocInstrumentation(Prof->Postopt, F, false);
    }

    //Call instrumentation could add a branch, invalidating the CFG
    return PreservedAnalyses();
}

PreservedAnalyses JITPreoptimizationProfiler::run(Function &F, FunctionAnalysisManager &FAM) {
    static_assert(sizeof(void*) == sizeof(uint64_t), "This code assumes 64-bit pointers");
    if (!JITProf)
        return PreservedAnalyses::all();

    auto Flags = fromFunction(F);

    if (!Flags.ProfileBranches && !Flags.ProfileAllocations && !Flags.ApplyPGO)
        return PreservedAnalyses::all();

    auto &Prof = JITProf->getOrCreateProfile(F.getName(), [&]() {
        auto FP = std::make_unique<FunctionProfile>();
        FP->Loops = FAM.getResult<LoopAnalysis>(F).getLoopsInPreorder().size();
        FP->BBs = F.size();
        FP->Insts = std::accumulate(F.begin(), F.end(), 0u, [](unsigned Acc, const BasicBlock &BB) {
            return Acc + BB.size();
        });
        return FP;
    });

    if (Flags.ProfileBranches) {
        addBranchInstrumentation(Prof, F);
    }

    if (Flags.ProfileAllocations) {
        addAllocInstrumentation(Prof.Preopt, F, true);
    }

    if (Flags.ApplyPGO) {
        applyPGOInstrumentation(Prof, F);
    }

    //If we apply PGO we must clobber almost everything due to branch weights updating
    return PreservedAnalyses::none();
}

void JITFunctionProfiler::dump(raw_ostream &OS) const {
    std::lock_guard<std::mutex> Lock(FunctionProfilesMutex);
    OS << '[';
    size_t Count = FunctionProfiles.size();
    for (const auto &KV : FunctionProfiles) {
        OS <<
        '{' <<
            "\"Name\":\"" << KV.first() << "\",";
        if (KV.second->CallCount) {
            OS << "\"Calls\":" << KV.second->CallCount << ",";
        }
        if (KV.second->Preopt.Count || KV.second->Postopt.Count) {
            OS <<
            "\"Allocs\":["
                "{\"Size\":" << KV.second->Preopt.Size << ",\"Count\":" << KV.second->Preopt.Count << "},"
                "{\"Size\":" << KV.second->Postopt.Size << ",\"Count\":" << KV.second->Postopt.Count << "}"
            << "],";
        }
        {
            std::lock_guard<std::mutex> Lock(KV.second->BranchesMutex);
            if (!KV.second->BranchProfiles.empty()) {
                size_t Count = KV.second->BranchProfiles.size();
                bool Comma = false;
                for (size_t Idx = 0; Idx < Count; Idx++) {
                    const auto &Branch = KV.second->BranchProfiles[Idx];
                    if (Comma) {
                        OS << ',';
                    } else {
                        OS << "\"Branches\":[";
                    }
                    OS << "{\"Idx\":" << Idx << ",\"Taken\":" << Branch.Taken << ",\"Total\":" << Branch.Total << "}";
                    Comma = true;
                }
                if (Comma) {
                    OS << "],";
                }
            }
        }
        if (KV.second->Loops)
            OS << "\"Loops\":" << KV.second->Loops << ",";
        OS <<
            "\"BBs\":" << KV.second->BBs << "," <<
            "\"Insts\":" << KV.second->Insts << '}';
        if (--Count) {
            OS << ",";
        }
    }
    OS << ']' << '\n';
}

FunctionProfile *JITFunctionProfiler::getProfile(StringRef Name) {
    std::lock_guard<std::mutex> Lock(FunctionProfilesMutex);
    auto It = FunctionProfiles.find(Name);
    if (It == FunctionProfiles.end())
        return nullptr;
    return It->second.get();
}

FunctionProfile &JITFunctionProfiler::getOrCreateProfile(StringRef Name, unique_function<std::unique_ptr<FunctionProfile>()> Create) {
    std::lock_guard<std::mutex> Lock(FunctionProfilesMutex);
    auto It = FunctionProfiles.find(Name);
    if (It == FunctionProfiles.end()) {
        auto Prof = Create();
        It = FunctionProfiles.insert(std::make_pair(Name, std::move(Prof))).first;
    }
    return *It->second;
}
