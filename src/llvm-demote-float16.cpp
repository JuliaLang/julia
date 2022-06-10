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

#include "support/dtypes.h"
#include "passes.h"

#include <llvm/Pass.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Debug.h>

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

namespace {

inline AttributeSet getFnAttrs(const AttributeList &Attrs)
{
#if JL_LLVM_VERSION >= 140000
    return Attrs.getFnAttrs();
#else
    return Attrs.getFnAttributes();
#endif
}

inline AttributeSet getRetAttrs(const AttributeList &Attrs)
{
#if JL_LLVM_VERSION >= 140000
    return Attrs.getRetAttrs();
#else
    return Attrs.getRetAttributes();
#endif
}

static Instruction *replaceIntrinsicWith(IntrinsicInst *call, Type *RetTy, ArrayRef<Value*> args)
{
    Intrinsic::ID ID = call->getIntrinsicID();
    assert(ID);
    auto oldfType = call->getFunctionType();
    auto nargs = oldfType->getNumParams();
    assert(args.size() > nargs);
    SmallVector<Type*, 8> argTys(nargs);
    for (unsigned i = 0; i < nargs; i++)
        argTys[i] = args[i]->getType();
    auto newfType = FunctionType::get(RetTy, argTys, oldfType->isVarArg());

    // Accumulate an array of overloaded types for the given intrinsic
    // and compute the new name mangling schema
    SmallVector<Type*, 4> overloadTys;
    {
        SmallVector<Intrinsic::IITDescriptor, 8> Table;
        getIntrinsicInfoTableEntries(ID, Table);
        ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
        auto res = Intrinsic::matchIntrinsicSignature(newfType, TableRef, overloadTys);
        assert(res == Intrinsic::MatchIntrinsicTypes_Match);
        (void)res;
        bool matchvararg = !Intrinsic::matchIntrinsicVarArg(newfType->isVarArg(), TableRef);
        assert(matchvararg);
        (void)matchvararg;
    }
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
    assert(newF->getFunctionType() == newfType);
    newF->setCallingConv(call->getCallingConv());
    assert(args.back() == call->getCalledFunction());
    auto newCall = CallInst::Create(newF, args.drop_back(), "", call);
    newCall->setTailCallKind(call->getTailCallKind());
    auto old_attrs = call->getAttributes();
    newCall->setAttributes(AttributeList::get(call->getContext(), getFnAttrs(old_attrs),
                                              getRetAttrs(old_attrs), {})); // drop parameter attributes
    return newCall;
}


static Value* CreateFPCast(Instruction::CastOps opcode, Value *V, Type *DestTy, IRBuilder<> &builder)
{
    Type *SrcTy = V->getType();
    Type *RetTy = DestTy;
    if (auto *VC = dyn_cast<Constant>(V)) {
        // The input IR often has things of the form
        //   fcmp olt half %0, 0xH7C00
        // and we would like to avoid turning that constant into a call here
        // if we can simply constant fold it to the new type.
        VC = ConstantExpr::getCast(opcode, VC, DestTy, true);
        if (VC)
            return VC;
    }
    assert(SrcTy->isVectorTy() == DestTy->isVectorTy());
    if (SrcTy->isVectorTy()) {
        unsigned NumElems = cast<FixedVectorType>(SrcTy)->getNumElements();
        assert(cast<FixedVectorType>(DestTy)->getNumElements() == NumElems && "Mismatched cast");
        Value *NewV = UndefValue::get(DestTy);
        RetTy = RetTy->getScalarType();
        for (unsigned i = 0; i < NumElems; ++i) {
            Value *I = builder.getInt32(i);
            Value *Vi = builder.CreateExtractElement(V, I);
            Vi = CreateFPCast(opcode, Vi, RetTy, builder);
            NewV = builder.CreateInsertElement(NewV, Vi, I);
        }
        return NewV;
    }
    auto &M = *builder.GetInsertBlock()->getModule();
    auto &ctx = M.getContext();
    // Pick the Function to call in the Julia runtime
    StringRef Name;
    switch (opcode) {
    case Instruction::FPExt:
        // this is exact, so we only need one conversion
        assert(SrcTy->isHalfTy());
        Name = "julia__gnu_h2f_ieee";
        RetTy = Type::getFloatTy(ctx);
        break;
    case Instruction::FPTrunc:
        assert(DestTy->isHalfTy());
        if (SrcTy->isFloatTy())
            Name = "julia__gnu_f2h_ieee";
        else if (SrcTy->isDoubleTy())
            Name = "julia__truncdfhf2";
        break;
    // All F16 fit exactly in Int32 (-65504 to 65504)
    case Instruction::FPToSI: JL_FALLTHROUGH;
    case Instruction::FPToUI:
        assert(SrcTy->isHalfTy());
        Name = "julia__gnu_h2f_ieee";
        RetTy = Type::getFloatTy(ctx);
        break;
    case Instruction::SIToFP: JL_FALLTHROUGH;
    case Instruction::UIToFP:
        assert(DestTy->isHalfTy());
        Name = "julia__gnu_f2h_ieee";
        SrcTy = Type::getFloatTy(ctx);
        break;
    default:
        errs() << Instruction::getOpcodeName(opcode) << ' ';
        V->getType()->print(errs());
        errs() << " to ";
        DestTy->print(errs());
        errs() << " is an ";
        llvm_unreachable("invalid cast");
    }
    if (Name.empty()) {
        errs() << Instruction::getOpcodeName(opcode) << ' ';
        V->getType()->print(errs());
        errs() << " to ";
        DestTy->print(errs());
        errs() << " is an ";
        llvm_unreachable("illegal cast");
    }
    // Coerce the source to the required size and type
    auto T_int16 = Type::getInt16Ty(ctx);
    if (SrcTy->isHalfTy())
        SrcTy = T_int16;
    if (opcode == Instruction::SIToFP)
        V = builder.CreateSIToFP(V, SrcTy);
    else if (opcode == Instruction::UIToFP)
        V = builder.CreateUIToFP(V, SrcTy);
    else
        V = builder.CreateBitCast(V, SrcTy);
    // Call our intrinsic
    if (RetTy->isHalfTy())
        RetTy = T_int16;
    auto FT = FunctionType::get(RetTy, {SrcTy}, false);
    FunctionCallee F = M.getOrInsertFunction(Name, FT);
    Value *I = builder.CreateCall(F, {V});
    // Coerce the result to the expected type
    if (opcode == Instruction::FPToSI)
        I = builder.CreateFPToSI(I, DestTy);
    else if (opcode == Instruction::FPToUI)
        I = builder.CreateFPToUI(I, DestTy);
    else if (opcode == Instruction::FPExt)
        I = builder.CreateFPCast(I, DestTy);
    else
        I = builder.CreateBitCast(I, DestTy);
    return I;
}

static bool demoteFloat16(Function &F)
{
    auto &ctx = F.getContext();
    auto T_float32 = Type::getFloatTy(ctx);

    SmallVector<Instruction *, 0> erase;
    for (auto &BB : F) {
        for (auto &I : BB) {
            // extend Float16 operands to Float32
            bool Float16 = I.getType()->getScalarType()->isHalfTy();
            for (size_t i = 0; !Float16 && i < I.getNumOperands(); i++) {
                Value *Op = I.getOperand(i);
                if (Op->getType()->getScalarType()->isHalfTy())
                    Float16 = true;
            }
            if (!Float16)
                continue;

            if (auto CI = dyn_cast<CastInst>(&I)) {
                if (CI->getOpcode() != Instruction::BitCast) { // aka !CI->isNoopCast(DL)
                    ++TotalChanged;
                    IRBuilder<> builder(&I);
                    Value *NewI = CreateFPCast(CI->getOpcode(), I.getOperand(0), I.getType(), builder);
                    I.replaceAllUsesWith(NewI);
                    erase.push_back(&I);
                }
                continue;
            }

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
                if (auto intrinsic = dyn_cast<IntrinsicInst>(&I))
                    if (intrinsic->getIntrinsicID())
                        break;
                continue;
            }

            // skip @fastmath operations
            // TODO: more fine-grained check (afn?)
            if (I.isFast())
                continue;

            IRBuilder<> builder(&I);

            // extend Float16 operands to Float32
            // XXX: Calls to llvm.fma.f16 may need to go to f64 to be correct?
            SmallVector<Value *, 2> Operands(I.getNumOperands());
            for (size_t i = 0; i < I.getNumOperands(); i++) {
                Value *Op = I.getOperand(i);
                if (Op->getType()->getScalarType()->isHalfTy()) {
                    ++TotalExt;
                    Op = CreateFPCast(Instruction::FPExt, Op, Op->getType()->getWithNewType(T_float32), builder);
                }
                Operands[i] = (Op);
            }

            // recreate the instruction if any operands changed,
            // truncating the result back to Float16
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
                if (auto intrinsic = dyn_cast<IntrinsicInst>(&I)) {
                    // XXX: this is not correct in general
                    // some obvious failures include llvm.convert.to.fp16.*, llvm.vp.*to*, llvm.experimental.constrained.*to*, llvm.masked.*
                    Type *RetTy = I.getType();
                    if (RetTy->getScalarType()->isHalfTy())
                        RetTy = RetTy->getWithNewType(T_float32);
                    NewI = replaceIntrinsicWith(intrinsic, RetTy, Operands);
                    break;
                }
                abort();
            }
            cast<Instruction>(NewI)->copyMetadata(I);
            cast<Instruction>(NewI)->copyFastMathFlags(&I);
            if (NewI->getType() != I.getType()) {
                ++TotalTrunc;
                NewI = CreateFPCast(Instruction::FPTrunc, NewI, I.getType(), builder);
            }
            I.replaceAllUsesWith(NewI);
            erase.push_back(&I);
        }
    }

    if (erase.size() > 0) {
        for (auto V : erase)
            V->eraseFromParent();
        assert(!verifyFunction(F));
        return true;
    }
    else
        return false;
}

} // end anonymous namespace

PreservedAnalyses DemoteFloat16::run(Function &F, FunctionAnalysisManager &AM)
{
    if (demoteFloat16(F)) {
        return PreservedAnalyses::allInSet<CFGAnalyses>();
    }
    return PreservedAnalyses::all();
}

namespace {

struct DemoteFloat16Legacy : public FunctionPass {
    static char ID;
    DemoteFloat16Legacy() : FunctionPass(ID){};

private:
    bool runOnFunction(Function &F) override {
        return demoteFloat16(F);
    }
};

char DemoteFloat16Legacy::ID = 0;
static RegisterPass<DemoteFloat16Legacy>
        Y("DemoteFloat16",
          "Demote Float16 operations to Float32 equivalents.",
          false,
          false);
} // end anonymous namespac

Pass *createDemoteFloat16Pass()
{
    return new DemoteFloat16Legacy();
}

extern "C" JL_DLLEXPORT void LLVMExtraAddDemoteFloat16Pass_impl(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createDemoteFloat16Pass());
}
