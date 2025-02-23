// This file is a part of Julia. License is MIT: https://julialang.org/license

// TODO: move this feature into AtomicExpandImpl

#include "llvm-version.h"
#include "passes.h"

#include <variant>

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/Analysis/InstSimplifyFolder.h>
#include <llvm/CodeGen/AtomicExpandUtils.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/LowerAtomic.h>

#include "julia.h"
#include "julia_assert.h"

#define DEBUG_TYPE "expand-atomic-modify"
#undef DEBUG

using namespace llvm;

// This pass takes fake call instructions that look like this which were emitted by the front end:
//   (oldval, newval) = call atomicmodify.iN(ptr %op, ptr align(N) %ptr, i8 immarg %SSID, i8 immarg %Ordering, ...) !rmwattributes
//   where op is a function with a prototype of `iN (iN arg, ...)`
// Then rewrite that to
//   oldval = atomicrmw op ptr, val ordering syncscope
//   newval = op oldval, val
// Or to an equivalent RMWCmpXchgLoop if `op` isn't valid for atomicrmw


// from AtomicExpandImpl, with modification of failure order and added Attributes
using CreateWeakCmpXchgInstFun =
   std::function<void(IRBuilderBase &, Value *, Value *, Value *, Align,
                     AtomicOrdering, SyncScope::ID, Instruction &Attributes,
                     Value *&, Value *&)>;

static void createWeakCmpXchgInstFun(IRBuilderBase &Builder, Value *Addr,
                                 Value *Loaded, Value *NewVal, Align AddrAlign,
                                 AtomicOrdering MemOpOrder, SyncScope::ID SSID, Instruction &Attributes,
                                 Value *&Success, Value *&NewLoaded) {
  Type *OrigTy = NewVal->getType();

  // This code can go away when cmpxchg supports FP types.
  assert(!OrigTy->isPointerTy());
  bool NeedBitcast = OrigTy->isFloatingPointTy();
  if (NeedBitcast) {
    IntegerType *IntTy = Builder.getIntNTy(OrigTy->getPrimitiveSizeInBits());
    NewVal = Builder.CreateBitCast(NewVal, IntTy);
    Loaded = Builder.CreateBitCast(Loaded, IntTy);
  }

  AtomicCmpXchgInst *Pair = Builder.CreateAtomicCmpXchg(
      Addr, Loaded, NewVal, AddrAlign, MemOpOrder,
      AtomicOrdering::Monotonic, // why does LLVM use AtomicCmpXchgInst::getStrongestFailureOrdering(MemOpOrder) here
      SSID);
  Pair->copyMetadata(Attributes);
  Success = Builder.CreateExtractValue(Pair, 1, "success");
  NewLoaded = Builder.CreateExtractValue(Pair, 0, "newloaded");

  if (NeedBitcast)
    NewLoaded = Builder.CreateBitCast(NewLoaded, OrigTy);
}

// from AtomicExpandImpl, with modification of values returned
std::pair<Value *, Value *> insertRMWCmpXchgLoop(
    IRBuilderBase &Builder, Type *ResultTy, Value *Addr, Align AddrAlign,
    AtomicOrdering MemOpOrder, SyncScope::ID SSID, Instruction &Attributes,
    const std::function<Value *(IRBuilderBase &, Value *)> &PerformOp,
    const CreateWeakCmpXchgInstFun &CreateWeakCmpXchg) {
  LLVMContext &Ctx = Builder.getContext();
  BasicBlock *BB = Builder.GetInsertBlock();
  Function *F = BB->getParent();

  // Given: atomicrmw some_op iN* %addr, iN %incr ordering
  //
  // The standard expansion we produce is:
  //     [...]
  //     %init_loaded = load atomic iN* %addr
  //     br label %loop
  // loop:
  //     %loaded = phi iN [ %init_loaded, %entry ], [ %new_loaded, %loop ]
  //     %new = some_op iN %loaded, %incr
  //     %pair = cmpxchg iN* %addr, iN %loaded, iN %new
  //     %new_loaded = extractvalue { iN, i1 } %pair, 0
  //     %success = extractvalue { iN, i1 } %pair, 1
  //     br i1 %success, label %atomicrmw.end, label %loop
  // atomicrmw.end:
  //     [...]
  BasicBlock *ExitBB =
      BB->splitBasicBlock(Builder.GetInsertPoint(), "atomicrmw.end");
  BasicBlock *LoopBB = BasicBlock::Create(Ctx, "atomicrmw.start", F, ExitBB);

  // The split call above "helpfully" added a branch at the end of BB (to the
  // wrong place), but we want a load. It's easiest to just remove
  // the branch entirely.
  std::prev(BB->end())->eraseFromParent();
  Builder.SetInsertPoint(BB);
  LoadInst *InitLoaded = Builder.CreateAlignedLoad(ResultTy, Addr, AddrAlign);
  InitLoaded->setOrdering(AtomicOrdering::Unordered); // n.b. the original LLVM pass is missing this call so is actually mildly UB
  Builder.CreateBr(LoopBB);

  // Start the main loop block now that we've taken care of the preliminaries.
  Builder.SetInsertPoint(LoopBB);
  PHINode *Loaded = Builder.CreatePHI(ResultTy, 2, "loaded");
  Loaded->addIncoming(InitLoaded, BB);

  Value *NewVal = PerformOp(Builder, Loaded);

  Value *NewLoaded = nullptr;
  Value *Success = nullptr;

  CreateWeakCmpXchg(Builder, Addr, Loaded, NewVal, AddrAlign,
                MemOpOrder == AtomicOrdering::Unordered
                    ? AtomicOrdering::Monotonic
                    : MemOpOrder,
                SSID, Attributes, Success, NewLoaded);
  assert(Success && NewLoaded);

  Loaded->addIncoming(NewLoaded, LoopBB);

  Builder.CreateCondBr(Success, ExitBB, LoopBB);

  Builder.SetInsertPoint(ExitBB, ExitBB->begin());
  return {NewLoaded, NewVal};
}

// from AtomicExpandImpl
struct ReplacementIRBuilder : IRBuilder<InstSimplifyFolder> {
  // Preserves the DebugLoc from I, and preserves still valid metadata.
  explicit ReplacementIRBuilder(Instruction *I, const DataLayout &DL)
      : IRBuilder(I->getContext(), DL) {
    SetInsertPoint(I);
    this->CollectMetadataToCopy(I, {LLVMContext::MD_pcsections});
  }
};

// Must check that either Target cannot observe or mutate global state
// or that no trailing instructions does so either.
// Depending on the choice, it can also decide whether it is better to move Target after RMW
// or to move RMW before Target (or meet somewhere in the middle).
// Currently conservatively implemented as there being no instruction in the
// function which writes memory (which includes any atomics).
// Excluding the Target itself, unless some other instruction might read memory to observe it.
static bool canReorderWithRMW(Instruction &Target, bool verifyop)
{
  if (!verifyop)
    return true;
  Function &Op = *Target.getFunction();
  // quick check: if Op is nosync and Target doesn't access any memory, then reordering is trivially valid
  bool nosync = Op.hasNoSync();
  if (nosync && !Target.mayReadOrWriteMemory())
    return true;
  // otherwise, scan the whole function to see if any function accesses memory
  // in a way that would conflict with reordering the atomic read and write
  bool mayRead = false;
  for (auto &BB : Op) {
    for (auto &I : BB) {
      if (&I == &Target)
        continue;
      if (I.mayWriteToMemory())
        return false;
      if (!mayRead) {
        mayRead = I.mayReadFromMemory();
        if (!nosync && mayRead)
          return false;
      }
    }
  }
  // if any other instruction read memory, then the ordering of any writes by the target instruction might be observed
  return !(mayRead && Target.mayWriteToMemory());
}

static std::variant<AtomicRMWInst::BinOp,bool> patternMatchAtomicRMWOp(Value *Old, Use **ValOp, Value *RetVal)
{
  bool verifyop = RetVal == nullptr;
  assert(verifyop ? isa<Argument>(Old) : isa<AtomicRMWInst>(Old));
  Function *Op = verifyop ? cast<Argument>(Old)->getParent() : nullptr;
  if (verifyop && (Op->isDeclaration() || Op->isInterposable() || Op->isIntrinsic()))
    return false;
   // TODO: peek forward from Old through any trivial casts which don't affect the instruction (e.g. i64 to f64 and back)
  if (RetVal == nullptr) {
    if (Old->use_empty()) {
      if (ValOp) *ValOp = nullptr;
      return AtomicRMWInst::Xchg;
    }
    if (!Old->hasOneUse())
      return false;
    ReturnInst *Ret = nullptr;
    for (auto &BB : *Op) {
      if (isa<ReturnInst>(BB.getTerminator())) {
        if (Ret != nullptr)
          return false;
        Ret = cast<ReturnInst>(BB.getTerminator());
      }
    }
    if (Ret == nullptr)
      return false;
    // Now examine the instruction list
    RetVal = Ret->getReturnValue();
    if (!RetVal->hasOneUse())
      return false;
  }
  if (RetVal == Old) {
    // special token indicating to convert to an atomic fence
    if (ValOp) *ValOp = nullptr;
    return AtomicRMWInst::Or;
  }
  if (Old->use_empty()) {
    if (ValOp) *ValOp = nullptr;
    return AtomicRMWInst::Xchg;
  }
  if (auto BinOp = dyn_cast<BinaryOperator>(RetVal)) {
    if ((BinOp->getOperand(0) == Old || (BinOp->isCommutative() && BinOp->getOperand(1) == Old)) && canReorderWithRMW(*BinOp, verifyop)) {
      if (ValOp) *ValOp = &BinOp->getOperandUse(BinOp->getOperand(0) == Old ? 1 : 0);
      switch (BinOp->getOpcode()) {
        case Instruction::Add:
          return AtomicRMWInst::Add;
        case Instruction::Sub:
          return AtomicRMWInst::Sub;
        case Instruction::And:
          return AtomicRMWInst::And;
        case Instruction::Or:
          return AtomicRMWInst::Or;
        case Instruction::Xor:
          return AtomicRMWInst::Xor;
        case Instruction::FAdd:
          return AtomicRMWInst::FAdd;
        case Instruction::FSub:
          return AtomicRMWInst::FSub;
        default:
          break;
      }
    }
    if (BinOp->getOpcode() == Instruction::Xor) {
      if (auto CI = dyn_cast<ConstantInt>(BinOp->getOperand(1))) {
        if (CI->isAllOnesValue()) {
          BinOp = dyn_cast<BinaryOperator>(BinOp->getOperand(0));
          if (BinOp && BinOp->hasOneUse() && BinOp->getOpcode() == Instruction::And) {
            if ((BinOp->getOperand(0) == Old || (BinOp->isCommutative() && BinOp->getOperand(1) == Old)) && canReorderWithRMW(*BinOp, verifyop)) {
              if (ValOp) *ValOp = &BinOp->getOperandUse(BinOp->getOperand(0) == Old ? 1 : 0);
              return AtomicRMWInst::Nand;
            }
          }
        }
      }
    }
    return false;
  } else if (auto Intr = dyn_cast<IntrinsicInst>(RetVal)) {
    if (Intr->arg_size() == 2) {
      if ((Intr->getOperand(0) == Old || (Intr->isCommutative() && Intr->getOperand(1) == Old)) && canReorderWithRMW(*Intr, verifyop)) {
        if (ValOp) *ValOp = &Intr->getOperandUse(Intr->getOperand(0) == Old ? 1 : 0);
        switch (Intr->getIntrinsicID()) {
          case Intrinsic::minnum:
            return AtomicRMWInst::FMin;
          case Intrinsic::maxnum:
            return AtomicRMWInst::FMax;
          case Intrinsic::smax:
            return AtomicRMWInst::Max;
          case Intrinsic::umax:
            return AtomicRMWInst::UMax;
          case Intrinsic::smin:
            return AtomicRMWInst::Min;
          case Intrinsic::umin:
            return AtomicRMWInst::UMin;
          // for LLVM 19:
          // case Intrinsic::usub_sat:
          //  return AtomicRMWInst::USubSat;
        }
      }
    }
    return false;
  }
  else if (auto Intr = dyn_cast<CallInst>(RetVal)) {
    // TODO: decide inlining cost of Op, or check alwaysinline/inlinehint, before this?
    for (auto &Arg : Intr->args()) {
      if (Arg == Old) {
        if (canReorderWithRMW(*Intr, verifyop)) {
          if (ValOp) *ValOp = &Arg;
          return true;
        }
        return false;
      }
    }
  }
  // TODO: does this need to deal with F->hasFnAttribute(Attribute::StrictFP)?
  // TODO: does Fneg and Neg have expansions?
  // TODO: be able to ignore some simple bitcasts (particularly f64 to i64)
  // TODO: handle longer sequences (UIncWrap, UDecWrap, USubCond, and target-specific ones for CUDA)
  return false;
}

void expandAtomicModifyToCmpXchg(CallInst &Modify,
                                 const CreateWeakCmpXchgInstFun &CreateWeakCmpXchg) {
  Value *Ptr = Modify.getOperand(0);
  Function *Op = dyn_cast<Function>(Modify.getOperand(1));
  if (!Op) {
      Modify.getParent()->getParent()->print(errs());
      llvm_unreachable("expected immarg for function argument");
  }
  AtomicOrdering Ordering = (AtomicOrdering)cast<ConstantInt>(Modify.getOperand(2))->getZExtValue();
  SyncScope::ID SSID = (SyncScope::ID)cast<ConstantInt>(Modify.getOperand(3))->getZExtValue();
  MaybeAlign Alignment = Modify.getParamAlign(0);
  unsigned user_arg_start = Modify.getFunctionType()->getNumParams();
  Type *Ty = Modify.getFunctionType()->getReturnType()->getStructElementType(0);

  ReplacementIRBuilder Builder(&Modify, Modify.getModule()->getDataLayout());
  Builder.setIsFPConstrained(Modify.hasFnAttr(Attribute::StrictFP));

  CallInst *ModifyOp;
  {
    SmallVector<Value*> Args(1 + Modify.arg_size() - user_arg_start);
    Args[0] = UndefValue::get(Ty); // Undef used as placeholder for Loaded / RMW;
    for (size_t argi = 0; argi < Modify.arg_size() - user_arg_start; ++argi) {
      Args[argi + 1] = Modify.getArgOperand(argi + user_arg_start);
    }
    SmallVector<OperandBundleDef> Defs;
    Modify.getOperandBundlesAsDefs(Defs);
    ModifyOp = Builder.CreateCall(Op, Args, Defs);
    ModifyOp->setCallingConv(Op->getCallingConv());
  }
  Use *LoadedOp = &ModifyOp->getOperandUse(0);

  Value *OldVal = nullptr;
  Value *NewVal = nullptr;
  auto BinOp = patternMatchAtomicRMWOp(Op->getArg(0), nullptr, nullptr);
  if (BinOp != decltype(BinOp)(false)) {
    Builder.SetInsertPoint(ModifyOp);
    AtomicRMWInst *RMW = Builder.CreateAtomicRMW(AtomicRMWInst::Xchg, Ptr, UndefValue::get(Ty), Alignment, Ordering, SSID); // Undef used as placeholder
    RMW->copyMetadata(Modify);
    Builder.SetInsertPoint(&Modify);
    LoadedOp->set(RMW);
    for (int attempts = 0; ; ) {
      FreezeInst *TrackReturn = Builder.Insert(new FreezeInst(ModifyOp)); // Create a temporary TrackingVH so we can recover the NewVal after inlining
      InlineFunctionInfo IFI;
      if (!InlineFunction(*ModifyOp, IFI).isSuccess()) {
        // Undo the attempt, since inlining failed
        BinOp = false;
        TrackReturn->eraseFromParent();
        break;
      }
      ModifyOp = nullptr;
      NewVal = TrackReturn->getOperand(0);
      TrackReturn->eraseFromParent();
      // NewVal might have been folded away by inlining so redo patternMatchAtomicRMWOp here
      // tracing from RMW to NewVal, in case instsimplify folded something
      Use *ValOp;
      BinOp = patternMatchAtomicRMWOp(RMW, &ValOp, NewVal);
      if (BinOp == decltype(BinOp)(true)) {
        ModifyOp = cast<CallInst>(ValOp->getUser());
        LoadedOp = ValOp;
        assert(LoadedOp->get() == RMW);
        RMW->moveBefore(ModifyOp); // NewValInst is a user of RMW, and RMW has no other dependants (per patternMatchAtomicRMWOp)
        BinOp = false;
        if (++attempts > 3)
          break;
        if (auto FOp = ModifyOp->getCalledFunction())
          BinOp = patternMatchAtomicRMWOp(FOp->getArg(LoadedOp->getOperandNo()), nullptr, nullptr);
        else
          break;
        if (BinOp == decltype(BinOp)(false))
          break;
      } else {
        assert(BinOp != decltype(BinOp)(true));
        auto RMWOp = std::get<AtomicRMWInst::BinOp>(BinOp);
        assert(RMWOp != AtomicRMWInst::BAD_BINOP);
        assert(isa<UndefValue>(RMW->getOperand(1))); // RMW was previously being used as the placeholder for Val
        Value *Val;
        if (ValOp != nullptr) {
          RMW->moveBefore(cast<Instruction>(ValOp->getUser())); // ValOp is a user of RMW, and RMW has no other dependants (per patternMatchAtomicRMWOp)
          Val = ValOp->get();
        } else if (RMWOp == AtomicRMWInst::Xchg) {
          Val = NewVal;
        } else {
          // convert to an atomic fence of the form: atomicrmw or %ptr, 0
          assert(RMWOp == AtomicRMWInst::Or);
          Val = ConstantInt::getNullValue(Ty);
        }
        RMW->setOperation(RMWOp);
        RMW->setOperand(1, Val);
        OldVal = RMW;
        break;
      }
    }
    if (BinOp == decltype(BinOp)(false)) {
      LoadedOp->set(UndefValue::get(Ty));
      RMW->eraseFromParent();
    }
  }

  if (BinOp == decltype(BinOp)(false)) {
    // FIXME: If FP exceptions are observable, we should force them off for the
    // loop for the FP atomics.
    std::tie(OldVal, NewVal) = insertRMWCmpXchgLoop(
      Builder, Ty,  Ptr, *Alignment, Ordering, SSID, Modify,
      [&](IRBuilderBase &Builder, Value *Loaded) JL_NOTSAFEPOINT {
        LoadedOp->set(Loaded);
        ModifyOp->moveBefore(*Builder.GetInsertBlock(), Builder.GetInsertPoint());
        return ModifyOp;
      },
      CreateWeakCmpXchg);
  }

  for (auto user : make_early_inc_range(Modify.users())) {
    if (auto EV = dyn_cast<ExtractValueInst>(user)) {
      if (EV->getNumIndices() == 1) {
        if (EV->use_empty()) {
          EV->eraseFromParent();
          continue;
        }
        else if (EV->getIndices()[0] == 0) {
          EV->replaceAllUsesWith(OldVal);
          EV->eraseFromParent();
          continue;
        } else if (EV->getIndices()[0] == 1) {
          EV->replaceAllUsesWith(NewVal);
          EV->eraseFromParent();
          continue;
        }
      }
    }
  }
  if (!Modify.use_empty()) {
    auto OldNewVal = Builder.CreateInsertValue(UndefValue::get(Modify.getType()), OldVal, 0);
    OldNewVal = Builder.CreateInsertValue(OldNewVal, NewVal, 1);
    Modify.replaceAllUsesWith(OldNewVal);
  }
  Modify.eraseFromParent();
}

static bool expandAtomicModify(Function &F) {
  SmallVector<CallInst*> AtomicInsts;

  // Changing control-flow while iterating through it is a bad idea, so gather a
  // list of all atomic instructions before we start.
  for (Instruction &I : instructions(F))
    if (auto CI = dyn_cast<CallInst>(&I)) {
      auto callee = dyn_cast_or_null<Function>(CI->getCalledOperand());
      if (callee && callee->getName().starts_with("julia.atomicmodify.")) {
        assert(CI->getFunctionType() == callee->getFunctionType());
        AtomicInsts.push_back(CI);
      }
    }

  bool MadeChange = !AtomicInsts.empty();
  for (auto *I : AtomicInsts)
    expandAtomicModifyToCmpXchg(*I, createWeakCmpXchgInstFun);
  return MadeChange;
}

PreservedAnalyses ExpandAtomicModifyPass::run(Function &F, FunctionAnalysisManager &AM)
{
    if (expandAtomicModify(F)) {
        return PreservedAnalyses::none();
    }
    return PreservedAnalyses::all();
}
