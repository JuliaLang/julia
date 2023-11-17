// This file is a part of Julia. License is MIT: https://julialang.org/license

// This pass finds floating-point operations on 16-bit values (half precision and bfloat),
// and replaces them by equivalent operations on 32-bit (single precision) values surrounded
// by a fpext and fptrunc. This ensures that the exact semantics of IEEE floating-point are
// preserved.
//
// Without this pass, back-ends that do not natively support half-precision (e.g. x86_64)
// similarly pattern-match half-precision operations with single-precision equivalents, but
// without truncating after every operation. Doing so breaks floating-point operations that
// assume precise semantics, such as Dekker arithmetic (as used in twiceprecision.jl).
//
// This pass is intended to run late in the pipeline, and should not be followed by
// instcombine. A run of GVN is recommended to clean-up identical conversions.

#include "llvm-version.h"

#include "support/dtypes.h"
#include "passes.h"

#include <llvm/Pass.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>
#include "julia.h"
#include "jitlayers.h"

#define DEBUG_TYPE "demote_float16"

using namespace llvm;

STATISTIC(TotalChanged, "Total number of instructions changed");
STATISTIC(TotalExt, "Total number of FPExt instructions inserted");
STATISTIC(TotalTrunc, "Total number of FPTrunc instructions inserted");
#define INST_STATISTIC(Opcode) STATISTIC(Opcode##Changed, "Number of " #Opcode " instructions changed")
INST_STATISTIC(FNeg);
INST_STATISTIC(FAdd);
INST_STATISTIC(FSub);
INST_STATISTIC(FMul);
INST_STATISTIC(FDiv);
INST_STATISTIC(FRem);
INST_STATISTIC(FCmp);
#undef INST_STATISTIC

extern JuliaOJIT *jl_ExecutionEngine;

namespace {

static bool have_fp16(Function &caller, const Triple &TT) {
    Attribute FSAttr = caller.getFnAttribute("target-features");
    StringRef FS = "";
    if (FSAttr.isValid())
        FS = FSAttr.getValueAsString();
    else if (jl_ExecutionEngine)
        FS = jl_ExecutionEngine->getTargetFeatureString();
    // else probably called from opt, just do nothing
    if (TT.isAArch64()) {
        if (FS.find("+fp16fml") != llvm::StringRef::npos || FS.find("+fullfp16") != llvm::StringRef::npos){
            return true;
        }
    } else if (TT.getArch() == Triple::x86_64) {
        if (FS.find("+avx512fp16") != llvm::StringRef::npos){
            return true;
        }
    }
    if (caller.hasFnAttribute("julia.hasfp16")) {
        return true;
    }
    return false;
}

static bool have_bf16(Function &caller, const Triple &TT) {
    if (caller.hasFnAttribute("julia.hasbf16")) {
        return true;
    }

    // there's no targets that fully support bfloat yet;,
    // AVX512BF16 only provides conversion and dot product instructions.
    return false;
}

static bool demoteFloat16(Function &F)
{
    auto TT = Triple(F.getParent()->getTargetTriple());
    auto has_fp16 = have_fp16(F, TT);
    auto has_bf16 = have_bf16(F, TT);
    if (has_fp16 && has_bf16)
        return false;

    auto &ctx = F.getContext();
    auto T_float32 = Type::getFloatTy(ctx);
    SmallVector<Instruction *, 0> erase;
    for (auto &BB : F) {
        for (auto &I : BB) {
            // check whether there's any 16-bit floating point operands to extend
            bool Float16 = I.getType()->getScalarType()->isHalfTy();
            bool BFloat16 = I.getType()->getScalarType()->isBFloatTy();
            for (size_t i = 0; !BFloat16 && !Float16 && i < I.getNumOperands(); i++) {
                Value *Op = I.getOperand(i);
                if (!has_fp16 && Op->getType()->getScalarType()->isHalfTy())
                    Float16 = true;
                else if (!has_bf16 && Op->getType()->getScalarType()->isBFloatTy())
                    BFloat16 = true;
            }
            if (!Float16 && !BFloat16)
                continue;

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
                // TODO: Do calls to llvm.fma.f16 may need to go to f64 to be correct?
                continue;
            }

            // skip @fastmath operations
            // TODO: more fine-grained check (afn?)
            if (I.isFast())
                continue;

            IRBuilder<> builder(&I);

            // extend 16-bit floating point operands
            SmallVector<Value *, 2> Operands(I.getNumOperands());
            for (size_t i = 0; i < I.getNumOperands(); i++) {
                Value *Op = I.getOperand(i);
                if (!has_fp16 && Op->getType()->getScalarType()->isHalfTy()) {
                    // extend Float16 to Float32
                    ++TotalExt;
                    Op = builder.CreateFPExt(Op, Op->getType()->getWithNewType(T_float32));
                } else if (!has_bf16 && Op->getType()->getScalarType()->isBFloatTy()) {
                    // extend BFloat16 to Float32
                    ++TotalExt;
                    Op = builder.CreateFPExt(Op, Op->getType()->getWithNewType(T_float32));
                }
                Operands[i] = Op;
            }

            // recreate the instruction if any operands changed,
            // truncating the result back to the original type
            Value *NewI;
            ++TotalChanged;
            switch (I.getOpcode()) {
            case Instruction::FNeg:
                assert(Operands.size() == 1);
                ++FNegChanged;
                NewI = builder.CreateFNeg(Operands[0]);
                break;
            case Instruction::FAdd:
                assert(Operands.size() == 2);
                ++FAddChanged;
                NewI = builder.CreateFAdd(Operands[0], Operands[1]);
                break;
            case Instruction::FSub:
                assert(Operands.size() == 2);
                ++FSubChanged;
                NewI = builder.CreateFSub(Operands[0], Operands[1]);
                break;
            case Instruction::FMul:
                assert(Operands.size() == 2);
                ++FMulChanged;
                NewI = builder.CreateFMul(Operands[0], Operands[1]);
                break;
            case Instruction::FDiv:
                assert(Operands.size() == 2);
                ++FDivChanged;
                NewI = builder.CreateFDiv(Operands[0], Operands[1]);
                break;
            case Instruction::FRem:
                assert(Operands.size() == 2);
                ++FRemChanged;
                NewI = builder.CreateFRem(Operands[0], Operands[1]);
                break;
            case Instruction::FCmp:
                assert(Operands.size() == 2);
                ++FCmpChanged;
                NewI = builder.CreateFCmp(cast<FCmpInst>(&I)->getPredicate(),
                                          Operands[0], Operands[1]);
                break;
            default:
                abort();
            }
            cast<Instruction>(NewI)->copyMetadata(I);
            cast<Instruction>(NewI)->copyFastMathFlags(&I);
            if (NewI->getType() != I.getType()) {
                ++TotalTrunc;
                NewI = builder.CreateFPTrunc(NewI, I.getType());
            }
            I.replaceAllUsesWith(NewI);
            erase.push_back(&I);
        }
    }

    if (erase.size() > 0) {
        for (auto V : erase)
            V->eraseFromParent();
#ifdef JL_VERIFY_PASSES
        assert(!verifyLLVMIR(F));
#endif
        return true;
    }
    else
        return false;
}

} // end anonymous namespace

PreservedAnalyses DemoteFloat16Pass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (demoteFloat16(F)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}
