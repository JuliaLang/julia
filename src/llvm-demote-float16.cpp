// This file is a part of Julia. License is MIT: https://julialang.org/license

// This pass finds floating-point operations on 16-bit (half precision) values, and replaces
// them by equivalent operations on 32-bit (single precision) values surrounded by a fpext
// and fptrunc. This ensures that the exact semantics of IEEE floating-point are preserved.
//
// Without this pass, back-ends that do not natively support half-precision (e.g. x86_64)
// similarly pattern-match half-precision operations with single-precision equivalents, but
// without truncating after every operation. Doing so breaks floating-point operations that
// assume precise semantics, such as Dekker arithmetic (as used in twiceprecision.jl).
//
// This pass is intended to run late in the pipeline, and should not be followed by
// instcombine. A run of GVN is recommended to clean-up identical conversions.

#include "llvm-version.h"

#define DEBUG_TYPE "demote_float16"

#include "support/dtypes.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Debug.h>

using namespace llvm;

namespace {

struct DemoteFloat16Pass : public FunctionPass {
    static char ID;
    DemoteFloat16Pass() : FunctionPass(ID){};

private:
    bool runOnFunction(Function &F) override;
};

bool DemoteFloat16Pass::runOnFunction(Function &F)
{
    auto &ctx = F.getContext();
    auto T_float16 = Type::getHalfTy(ctx);
    auto T_float32 = Type::getFloatTy(ctx);

    SmallVector<Instruction *, 0> erase;
    for (auto &BB : F) {
        for (auto &I : BB) {
            switch (I.getOpcode()) {
            case Instruction::FNeg:
            case Instruction::FAdd:
            case Instruction::FSub:
            case Instruction::FMul:
            case Instruction::FDiv:
            case Instruction::FRem:
            case Instruction::FCmp:
                break;
            default:
                continue;
            }

            // skip @fastmath operations
            // TODO: more fine-grained check (afn?)
            if (I.isFast())
                continue;

            IRBuilder<> builder(&I);

            // extend Float16 operands to Float32
            bool OperandsChanged = false;
            SmallVector<Value *, 2> Operands(I.getNumOperands());
            for (size_t i = 0; i < I.getNumOperands(); i++) {
                Value *Op = I.getOperand(i);
                if (Op->getType() == T_float16) {
                    Op = builder.CreateFPExt(Op, T_float32);
                    OperandsChanged = true;
                }
                Operands[i] = (Op);
            }

            // recreate the instruction if any operands changed,
            // truncating the result back to Float16
            if (OperandsChanged) {
                Value *NewI;
                switch (I.getOpcode()) {
                case Instruction::FNeg:
                    assert(Operands.size() == 1);
                    NewI = builder.CreateFNeg(Operands[0]);
                    break;
                case Instruction::FAdd:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFAdd(Operands[0], Operands[1]);
                    break;
                case Instruction::FSub:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFSub(Operands[0], Operands[1]);
                    break;
                case Instruction::FMul:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFMul(Operands[0], Operands[1]);
                    break;
                case Instruction::FDiv:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFDiv(Operands[0], Operands[1]);
                    break;
                case Instruction::FRem:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFRem(Operands[0], Operands[1]);
                    break;
                case Instruction::FCmp:
                    assert(Operands.size() == 2);
                    NewI = builder.CreateFCmp(cast<FCmpInst>(&I)->getPredicate(),
                                              Operands[0], Operands[1]);
                    break;
                default:
                    abort();
                }
                cast<Instruction>(NewI)->copyMetadata(I);
                cast<Instruction>(NewI)->copyFastMathFlags(&I);
                if (NewI->getType() != I.getType())
                    NewI = builder.CreateFPTrunc(NewI, I.getType());
                I.replaceAllUsesWith(NewI);
                erase.push_back(&I);
            }
        }
    }

    if (erase.size() > 0) {
        for (auto V : erase)
            V->eraseFromParent();
        return true;
    }
    else
        return false;
}

char DemoteFloat16Pass::ID = 0;
static RegisterPass<DemoteFloat16Pass>
        Y("DemoteFloat16",
          "Demote Float16 operations to Float32 equivalents.",
          false,
          false);
}

Pass *createDemoteFloat16Pass()
{
    return new DemoteFloat16Pass();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddDemoteFloat16Pass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createDemoteFloat16Pass());
}
