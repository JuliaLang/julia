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
                            if (memop.offset == offsetof(jl_array_t, length) || memop.offset == offsetof(jl_array_t, nrows) || memop.offset == offsetof(jl_array_t, maxsize)) {
                                changed = true;
                                IRBuilder<> builder(memop.inst);
                                memop.inst->replaceAllUsesWith(builder.CreateIntCast(allocation.allocation->getArgOperand(1), memop.inst->getType(), false, "arraylen"));
                                memop.inst->eraseFromParent();
                            } else if (memop.offset == offsetof(jl_array_t, offset)) {
                                changed = true;
                                memop.inst->replaceAllUsesWith(ConstantInt::get(memop.inst->getType(), 0));
                                memop.inst->eraseFromParent();
                            }
                        }
                    }
                }
            }
        }
        return changed;
    }

    bool Optimizer::optimizeCmpInsts() {
        bool changed = false;
        auto &dtree = pass->getAnalysis<DominatorTreeWrapperPass>().getDomTree();
        std::map<Value *, SmallSet<std::size_t, 2>> checked, next_check;
        while (!lengths.empty()) {
            for (auto &dim : lengths) {
                auto get_dominators = [&](Instruction *inst) {
                    llvm::SmallSet<std::size_t, 2> dominators;
                    for (auto idx : dim.second) {
                        if (dtree.dominates(array_allocations[idx].allocation, inst)) {
                            dominators.insert(idx);
                        }
                    }
                    return dominators;
                };
                for (auto user : dim.first->users()) {
                    if (checked.find(user) == checked.end() && next_check.find(user) == next_check.end()) {
                        if (auto inst = dyn_cast<Instruction>(user)) {
                            auto dominators = get_dominators(inst);
                            auto adjust_dominators = [&](decltype(checked) &check, Value *val){
                                auto it = check.find(val);
                                if (it != check.end()) {
                                    llvm::SmallSet<std::size_t, 2> actual_dominators;
                                    for (auto dominator : dominators) {
                                        if (it->second.contains(dominator)) {
                                            actual_dominators.insert(dominator);
                                        }
                                    }
                                    std::swap(actual_dominators, dominators);
                                    return true;
                                }
                                return false;
                            };
                            switch (inst->getOpcode()) {
                                case Instruction::Mul: {
                                    std::size_t idx = inst->getOperand(0) == dim.first; // 1 if equal, 0 if not
                                    auto val = inst->getOperand(idx);
                                    if (inst->hasNoSignedWrap() || adjust_dominators(checked, val) || adjust_dominators(next_check, val) || adjust_dominators(lengths, val)) {
                                        if (!inst->hasNoSignedWrap() && dominators.empty()) {
                                            //We don't have an allocation that uses both
                                            //operands of the mul instruction AND dominates
                                            //the mul instruction; therefore this mul could
                                            //overflow and become negative, invalidating
                                            //our comparison check.
                                            continue;
                                        }
                                        //This mul is probably part of a length instruction;
                                        //We'll push it to be checked for cmp optimization
                                        next_check[inst] = std::move(dominators);
                                    }
                                    break;
                                }
                                case Instruction::ICmp: {
                                    auto cmp = cast<ICmpInst>(inst);
                                    if (dominators.empty()) {
                                        if (auto ci = dyn_cast<ConstantInt>(dim.first)) {
                                            if (ci->isNegative()) {
                                                //We can't optimize this comparison instruction
                                                //to be unsigned if it's a negative length!
                                                continue;
                                            }
                                        } else if (auto dinst = dyn_cast<Instruction>(dim.first)) {
                                            if (!dinst->hasNoSignedWrap()) {
                                                //This might have overflowed, can't make
                                                //comparisons unsigned
                                                continue;
                                            }
                                        } else {
                                            // We don't know what this value is,
                                            // can't make the comparison unsigned
                                            continue;
                                        }
                                    }
                                    std::size_t idx = inst->getOperand(0) == dim.first;
                                    auto val = inst->getOperand(idx);
                                    if (auto ci = dyn_cast<ConstantInt>(val)) {
                                        if (!ci->isNegative()) {
                                            //Should automagically take care of major constant cases (0 and 1)
                                            if (cmp->isSigned()) {
                                                changed = true;
                                                cmp->setPredicate(cmp->getUnsignedPredicate());
                                            }
                                        } else {
                                            // Negative constant == fully predictable
                                            switch (cmp->getPredicate()) {
                                                case CmpInst::Predicate::ICMP_SLE:
                                                case CmpInst::Predicate::ICMP_SLT:
                                                case CmpInst::Predicate::ICMP_EQ:
                                                    changed = true;
                                                    cmp->replaceAllUsesWith(ConstantInt::getFalse(cmp->getContext()));
                                                    break;
                                                case CmpInst::Predicate::ICMP_SGE:
                                                case CmpInst::Predicate::ICMP_SGT:
                                                case CmpInst::Predicate::ICMP_NE:
                                                    changed = true;
                                                    cmp->replaceAllUsesWith(ConstantInt::getTrue(cmp->getContext()));
                                                    break;
                                                default:
                                                    break;
                                            }
                                        }
                                    } else {
                                        if (cmp->isSigned() && (adjust_dominators(checked, val) || adjust_dominators(next_check, val) || adjust_dominators(lengths, val))) {
                                            if (dominators.empty()) {
                                                continue;
                                            }
                                            //We know that both operands to this cmp are nonnegative,
                                            //because both were used as nonoverflowing array dimensions
                                            //and this compare is definitely after an allocation that
                                            //guarantees the above property. Therefore, we can make this
                                            //an unsigned compare, regardless of what kind of comparison
                                            //was being made (because the operands are nonnegative)
                                            changed = true;
                                            cmp->setPredicate(cmp->getUnsignedPredicate());
                                        }
                                    }
                                    break;
                                }
                                default:
                                    break;
                            }
                        }
                    }
                }
                checked.emplace(std::move(dim));
            }
            std::swap(lengths, next_check);
            next_check.clear();
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