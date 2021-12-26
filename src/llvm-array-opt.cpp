#define DEBUG_TYPE "arraylen_cmp"
#undef DEBUG
#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/ConstantRange.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/InitializePasses.h>

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"
#include "llvm-alloc-helpers.h"

#include <map>

using namespace llvm;

namespace {

    struct ArrayAllocation {
        CallInst *allocation;
        int dimcount;
    };

    struct ArrayOpt;

    struct Optimizer {
        ArrayOpt *pass;
        const DataLayout *DL;
        SmallVector<ArrayAllocation, 4> array_allocations;
        std::map<Value *, SmallSet<std::size_t, 2>> lengths;

        Optimizer(ArrayOpt *pass) : pass(pass) {}

        void initializeAllocations(Function &F);
        bool propagate1DArrayLengths();
        bool optimizeCmpInsts();

        void reset() {
            array_allocations.clear();
            lengths.clear();
        }
    };

    struct ArrayOpt : public FunctionPass, public JuliaPassContext {
        static char ID;
        Optimizer optimizer;
        ArrayOpt() : FunctionPass(ID), optimizer(this) {
            llvm::initializeDominatorTreeWrapperPassPass(*PassRegistry::getPassRegistry());
        }
        bool runOnFunction(Function &F) override {
            optimizer.initializeAllocations(F);
            bool propagation_changed = optimizer.propagate1DArrayLengths();
            //Propagating 1D array allocations may allow more cmp insts to be folded
            bool cmp_folded = optimizer.optimizeCmpInsts();
            return propagation_changed || cmp_folded;
        }
        void getAnalysisUsage(AnalysisUsage &AU) const override
        {
            FunctionPass::getAnalysisUsage(AU);
            AU.addRequired<DominatorTreeWrapperPass>();
            AU.addPreserved<DominatorTreeWrapperPass>();
            AU.setPreservesCFG();
        }
    };

    void Optimizer::initializeAllocations(Function &F) {
        reset();
        jl_alloc::AllocIdInfo info;
        DL = &F.getParent()->getDataLayout();
        for (auto &BB : F) {
            for (auto &I : BB) {
                if (auto call = dyn_cast<CallInst>(&I)) {
                    if (jl_alloc::getArrayAllocInfo(info, call) && info.array.dimcount) {
                        std::size_t idx = array_allocations.size();
                        array_allocations.push_back({call, info.array.dimcount});
                        for (int i = 0; i < info.array.dimcount; i++) {
                            lengths[call->getArgOperand(i + 1)].insert(idx);
                        }
                    }
                }
            }
        }
    }

    bool Optimizer::propagate1DArrayLengths() {
        bool changed = false;
        jl_alloc::AllocUseInfo use_info;
        jl_alloc::CheckInst::Stack check_stack;
        for (auto &allocation : array_allocations) {
            if (allocation.dimcount == 1) {
                jl_alloc::EscapeAnalysisRequiredArgs args{use_info, check_stack, *pass, *DL};
                jl_alloc::runEscapeAnalysis(allocation.allocation, args);
                if (use_info.escaped) {
                    continue;
                }
                //If we clobber any of the dimensions,
                //we must necessarily clobber the array length
                //This allows us to track all clobbers via the
                //array length field
                #ifdef STORE_ARRAY_LEN
                bool arraylen_clobbered = false;
                for (auto &field : use_info.memops) {
                    for (auto &memop : field.second.accesses) {
                        if (memop.offset == offsetof(jl_array_t, length) && !isa<LoadInst>(memop.inst)) {
                            arraylen_clobbered = true;
                            break;
                        }
                    }
                    if (arraylen_clobbered) {
                        break;
                    }
                }
                if (!arraylen_clobbered) {
                    //The size of the array doesn't change, we can change
                    //length/dim loads directly to the allocated amount
                    for (auto &field : use_info.memops) {
                        for (auto &memop : field.second.accesses) {
                            if (memop.offset == offsetof(jl_array_t, length)) {
                                changed = true;
                                IRBuilder<> builder(memop.inst);
                                memop.inst->replaceAllUsesWith(builder.CreateIntCast(allocation.allocation->getArgOperand(1), memop.inst->getType(), false, "arraylen"));
                                memop.inst->eraseFromParent();
                            }
                        }
                    }
                }
                #endif
            }
        }
        return changed;
    }

    bool Optimizer::optimizeCmpInsts() {
        bool changed = false;
        auto &dtree = pass->getAnalysis<DominatorTreeWrapperPass>().getDomTree();
        for (auto &dim : lengths) {
            for (auto user : dim.first->users()) {
                if (auto cmp = dyn_cast<CmpInst>(user)) {
                    for (auto idx : dim.second) {
                        if (dtree.dominates(array_allocations[idx].allocation, cmp)) {
                            //gets operand 1 if 0 is dim.first, otherwise operand 0
                            auto value = cmp->getOperand(cmp->getOperand(0) == dim.first);
                            if (auto ci = dyn_cast<ConstantInt>(value)) {
                                if (ci->isNegative()) {
                                    changed = true;
                                    switch (cmp->getPredicate()) {
                                        case CmpInst::Predicate::ICMP_SLT:
                                        case CmpInst::Predicate::ICMP_SLE:
                                        case CmpInst::Predicate::ICMP_EQ:
                                            return ConstantInt::getFalse(cmp->getContext());
                                        case CmpInst::Predicate::ICMP_SGE:
                                        case CmpInst::Predicate::ICMP_SGT:
                                        case CmpInst::Predicate::ICMP_NE:
                                            return ConstantInt::getTrue(cmp->getContext());
                                        default:
                                            assert(false && "Unsigned compare on negative value!");
                                            break;
                                    }
                                } else if (cmp->isSigned()) {
                                    changed = true;
                                    cmp->setPredicate(cmp->getUnsignedPredicate());
                                }
                            } else if (cmp->isSigned()) {
                                bool positive = false;
                                if (auto inst = dyn_cast<Instruction>(value)) {
                                    if (auto range = inst->getMetadata(LLVMContext::MD_range)) {
                                        auto cr = getConstantRangeFromMetadata(*range);
                                        if (cr.getLower().sge(0)) {
                                            positive = true;
                                        }
                                    }
                                }
                                if (!positive) {
                                    auto dim2 = lengths.find(value);
                                    if (dim2 != lengths.end()) {
                                        for (auto idx2 : dim2->second) {
                                            if (dtree.dominates(array_allocations[idx2].allocation, cmp)) {
                                                positive = true;
                                            }
                                        }
                                    }
                                }
                                if (positive) {
                                    changed = true;
                                    cmp->setPredicate(cmp->getUnsignedPredicate());
                                    break;
                                }
                            }
                            break;
                        }
                    }
                }
            }
        }
        return changed;
    }

    char ArrayOpt::ID = 0;
    static RegisterPass<ArrayOpt> X("ArrayOpt", "1D array length propagation and array length unsignedness",
                                    false /* Only looks at CFG */,
                                    false /* Analysis Pass */);
}



Pass *createArrayOptPass()
{
    return new ArrayOpt();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddArrayOptPass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createArrayOptPass());
}