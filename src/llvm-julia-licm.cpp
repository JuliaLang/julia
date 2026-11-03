// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "passes.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/LoopPass.h>
#include <llvm/Analysis/LoopIterator.h>
#include <llvm/Analysis/MemorySSA.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/Utils/LoopUtils.h>

#include "llvm-pass-helpers.h"
#include "julia.h"
#include "llvm-alloc-helpers.h"
#include "llvm-codegen-shared.h"

#define DEBUG_TYPE "julia-licm"

using namespace llvm;

STATISTIC(HoistedPreserveBegin, "Number of gc_preserve_begin instructions hoisted out of a loop");
STATISTIC(SunkPreserveEnd, "Number of gc_preserve_end instructions sunk out of a loop");
STATISTIC(ErasedPreserveEnd, "Number of gc_preserve_end instructions removed from nonterminating loops");
STATISTIC(HoistedWriteBarrier, "Number of write barriers hoisted out of a loop");
STATISTIC(HoistedAllocation, "Number of allocations hoisted out of a loop");

/*
 * Julia LICM pass.
 * This takes care of some julia intrinsics that are safe to move around/out of loops but
 * can't be handled by LLVM's LICM. These intrinsics can be moved outside of
 * loop context as well but it is inside a loop where they matter the most.
 */

#ifndef __clang_gcanalyzer__
#define REMARK(remark) ORE.emit(remark)
#else
#define REMARK(remark) (void) 0;
#endif

namespace {

//Stolen and modified from LICM.cpp
static void eraseInstruction(Instruction &I,
                             MemorySSAUpdater &MSSAU) {
  if (MSSAU.getMemorySSA())
    MSSAU.removeMemoryAccess(&I);
  I.eraseFromParent();
}

//Stolen and modified from LICM.cpp
static void moveInstructionBefore(Instruction &I, Instruction &Dest,
                                  MemorySSAUpdater &MSSAU,
                                  ScalarEvolution *SE,
                                  MemorySSA::InsertionPlace Place = MemorySSA::BeforeTerminator) {
  I.moveBefore(Dest.getIterator());
  if (MSSAU.getMemorySSA())
    if (MemoryUseOrDef *OldMemAcc = cast_or_null<MemoryUseOrDef>(
            MSSAU.getMemorySSA()->getMemoryAccess(&I)))
      MSSAU.moveToPlace(OldMemAcc, Dest.getParent(), Place);
  if (SE)
    SE->forgetValue(&I);
}

static void createNewInstruction(Instruction *New, Instruction *Ref, MemorySSAUpdater &MSSAU,
                                 MemorySSA::InsertionPlace Point = MemorySSA::Beginning) {
  if (MSSAU.getMemorySSA() && MSSAU.getMemorySSA()->getMemoryAccess(Ref)) {
    // Create a new MemoryAccess and let MemorySSA set its defining access.
    MemoryAccess *NewMemAcc = MSSAU.createMemoryAccessInBB(
        New, nullptr, New->getParent(), Point);
    if (NewMemAcc) {
      if (auto *MemDef = dyn_cast<MemoryDef>(NewMemAcc))
        MSSAU.insertDef(MemDef, /*RenameUses=*/true);
      else {
        auto *MemUse = cast<MemoryUse>(NewMemAcc);
        MSSAU.insertUse(MemUse, /*RenameUses=*/true);
      }
    }
  }
}

//Stolen and modified to update SE from LoopInfo.cpp
static bool makeLoopInvariant(Loop *L, Value *V, bool &Changed, Instruction *InsertPt, MemorySSAUpdater &MSSAU, ScalarEvolution *SE);

static bool makeLoopInvariant(Loop *L, Instruction *I, bool &Changed, Instruction *InsertPt, MemorySSAUpdater &MSSAU, ScalarEvolution *SE) {
  // Test if the value is already loop-invariant.
  if (L->isLoopInvariant(I))
    return true;
  if (!isSafeToSpeculativelyExecute(I))
    return false;
  if (I->mayReadFromMemory())
    return false;
  // EH block instructions are immobile.
  if (I->isEHPad())
    return false;
  // Don't hoist instructions with loop-variant operands.
  for (Value *Operand : I->operands())
    if (!makeLoopInvariant(L, Operand, Changed, InsertPt, MSSAU, SE))
      return false;

  // Hoist.
  moveInstructionBefore(*I, *InsertPt, MSSAU, SE);

  // There is possibility of hoisting this instruction above some arbitrary
  // condition. Any metadata defined on it can be control dependent on this
  // condition. Conservatively strip it here so that we don't give any wrong
  // information to the optimizer.
  I->dropUnknownNonDebugMetadata();

  Changed = true;
  return true;
}

static bool makeLoopInvariant(Loop *L, Value *V, bool &Changed, Instruction *InsertPt, MemorySSAUpdater &MSSAU, ScalarEvolution *SE) {
  if (Instruction *I = dyn_cast<Instruction>(V))
    return makeLoopInvariant(L, I, Changed, InsertPt, MSSAU, SE);
  return true; // All non-instructions are loop-invariant.
}

struct JuliaLICM : public JuliaPassContext {
    function_ref<DominatorTree &()> GetDT;
    function_ref<LoopInfo &()> GetLI;
    function_ref<MemorySSA *()> GetMSSA;
    function_ref<ScalarEvolution *()> GetSE;
    JuliaLICM(function_ref<DominatorTree &()> GetDT,
              function_ref<LoopInfo &()> GetLI,
              function_ref<MemorySSA *()> GetMSSA,
              function_ref<ScalarEvolution *()> GetSE) :
                GetDT(GetDT),
                GetLI(GetLI),
                GetMSSA(GetMSSA),
                GetSE(GetSE) {}

    // An allocation of an immutable object is never written after its initializing
    // stores, so it can be hoisted together with them even if it escapes, provided
    // it runs on every iteration, the stores are in its block, precede every other
    // use and store loop-invariant values, and nothing else may write to it.
    bool hoistImmutableAlloc(Loop *L, CallInst *call, BasicBlock *preheader,
                             BasicBlock::iterator &II, DominatorTree &DT,
                             MemorySSAUpdater &MSSAU, ScalarEvolution *SE)
    {
        if (!call->getMetadata("julia.immutable"))
            return false;
        BasicBlock *BB = call->getParent();
        // hoisting a conditional allocation would allocate where we did not before
        SmallVector<BasicBlock*, 4> exiting;
        L->getExitingBlocks(exiting);
        for (BasicBlock *E : exiting)
            if (!DT.dominates(BB, E))
                return false;
        if (BasicBlock *latch = L->getLoopLatch(); !latch || !DT.dominates(BB, latch))
            return false;
        SmallVector<StoreInst*, 4> inits;
        SmallVector<Instruction*, 8> worklist{call};
        SmallPtrSet<Instruction*, 8> visited{call};
        while (!worklist.empty()) {
            Instruction *ptr = worklist.pop_back_val();
            for (Use &U : ptr->uses()) {
                auto *I = cast<Instruction>(U.getUser());
                if (auto *SI = dyn_cast<StoreInst>(I)) {
                    if (SI->getPointerOperand() != ptr)
                        continue; // the object itself is stored somewhere
                    if (SI->getValueOperand() == ptr || SI->isVolatile() || SI->getParent() != BB ||
                        !L->isLoopInvariant(SI->getValueOperand()))
                        return false;
#if defined(GC_BARRIER_SNAPSHOT)
                    // the store would need to be seen by the marker
                    if (SI->getValueOperand()->getType()->isPointerTy())
                        return false;
#endif
                    inits.push_back(SI);
                }
                else if (isa<AddrSpaceCastInst>(I) || isa<BitCastInst>(I) ||
                         (isa<GetElementPtrInst>(I) && I->getOperand(0) == ptr)) {
                    for (Value *Op : I->operands())
                        if (Op != ptr && !L->isLoopInvariant(Op))
                            return false;
                    if (visited.insert(I).second)
                        worklist.push_back(I);
                }
                else if (auto *CB = dyn_cast<CallBase>(I)) {
                    // Passing the object itself (a tracked reference) passes an immutable
                    // Julia value, which the callee cannot modify. A derived pointer could
                    // be written through (e.g. an sret destination), unless it is read-only.
                    if (CB->isBundleOperand(&U))
                        continue;
                    if (!CB->isArgOperand(&U))
                        return false;
                    if (U.get()->getType()->getPointerAddressSpace() != AddressSpace::Tracked &&
                        !CB->onlyReadsMemory(CB->getArgOperandNo(&U)))
                        return false;
                }
                else if (!isa<LoadInst>(I) && !isa<ICmpInst>(I) && !isa<PtrToIntInst>(I)) {
                    return false;
                }
            }
        }
        if (inits.empty())
            return false;
        // every initializing store must happen before anything else uses the object
        size_t seen = 0;
        for (auto it = std::next(call->getIterator()); it != BB->end() && seen < inits.size(); ++it) {
            Instruction &I = *it;
            if (visited.count(&I))
                continue;
            if (auto *SI = dyn_cast<StoreInst>(&I); SI && is_contained(inits, SI)) {
                seen++;
                continue;
            }
            for (Value *Op : I.operands())
                if (auto *OI = dyn_cast<Instruction>(Op); OI && visited.count(OI))
                    return false;
        }
        if (seen != inits.size())
            return false;
        // all checks passed: the IR has not been changed before this point
        Instruction *InsertPt = preheader->getTerminator();
        bool changed = false;
        moveInstructionBefore(*call, *InsertPt, MSSAU, SE);
        // keep the initializing stores in their original order
        for (Instruction &I : make_early_inc_range(*BB)) {
            auto *SI = dyn_cast<StoreInst>(&I);
            if (!SI || !is_contained(inits, SI))
                continue;
            bool ok = makeLoopInvariant(L, SI->getPointerOperand(), changed, InsertPt, MSSAU, SE);
            assert(ok); (void)ok;
            moveInstructionBefore(*SI, *InsertPt, MSSAU, SE);
        }
        // the instruction after `call` may have moved too: rescan the block
        II = BB->begin();
        return true;
    }

#if !defined(GC_BARRIER_SNAPSHOT) && !defined(GC_BARRIER_FIELD_PRECISE)
    // Replace a field barrier with an object barrier in the preheader.
    CallInst *hoistAsObjectBarrier(CallInst *call, BasicBlock *preheader,
                                   MemorySSAUpdater &MSSAU)
    {
        assert(isFieldWriteBarrier(call->getCalledOperand()));
        object_write_barrier_func = getOrDeclare(jl_intrinsics::objectWriteBarrier);
        SmallVector<Value*, 4> args{call->getArgOperand(0)};
        for (Value *child : writeBarrierChildren(call))
            args.push_back(child);

        IRBuilder<> builder(preheader->getTerminator());
        builder.SetCurrentDebugLocation(call->getDebugLoc());
        auto *replacement = builder.CreateCall(object_write_barrier_func, args);
        if (auto *MD = call->getMetadata("julia.reset_region"))
            replacement->setMetadata("julia.reset_region", MD);
        createNewInstruction(replacement, call, MSSAU, MemorySSA::BeforeTerminator);
        eraseInstruction(*call, MSSAU);
        return replacement;
    }
#endif

    bool runOnLoop(Loop *L, OptimizationRemarkEmitter &ORE)
    {
        // Get the preheader block to move instructions into,
        // required to run this pass.
        BasicBlock *preheader = L->getLoopPreheader();
        if (!preheader)
            return false;
        BasicBlock *header = L->getHeader();
        const llvm::DataLayout &DL = header->getModule()->getDataLayout();
        initFunctions(*header->getModule());
        // Also require `gc_preserve_begin_func` whereas
        // `gc_preserve_end_func` is optional since the input to
        // `gc_preserve_end_func` must be from `gc_preserve_begin_func`.
        // We also hoist write barriers here, so we don't exit if a write barrier func exists
        if (!gc_preserve_begin_func && !object_write_barrier_func && !field_write_barrier_p11_func &&
            !field_write_barrier_p13_func && !alloc_obj_func) {
            LLVM_DEBUG(dbgs() << "No gc_preserve_begin_func or write barrier or alloc_obj_func found, skipping JuliaLICM\n");
            return false;
        }
        auto LI = &GetLI();
        auto DT = &GetDT();
        auto MSSA = GetMSSA();
        auto SE = GetSE();
        MemorySSAUpdater MSSAU(MSSA);

        // Lazily collect exit blocks; insertion points depend on the begin's loop.
        bool exit_bbs_init = false;
        SmallVector<BasicBlock*, 8> exit_bbs;
        auto get_exit_pts = [&] (Loop *begin_loop) {
            if (!exit_bbs_init) {
                exit_bbs_init = true;
                L->getUniqueExitBlocks(exit_bbs);
            }
            SmallVector<Instruction*, 8> exit_pts;
            for (BasicBlock *bb: exit_bbs) {
                // Keep the token inside its defining loop: LCSSA cannot insert
                // token PHIs, so later loop cloning can otherwise break dominance.
                // Omitting an end conservatively extends the preserve region.
                if (begin_loop && !begin_loop->contains(bb))
                    continue;
                exit_pts.push_back(&*bb->getFirstInsertionPt());
            }
            return exit_pts;
        };

        bool changed = false;
        // Scan in the right order so that we'll hoist the `begin`
        // before we consider sinking `end`.
        LoopBlocksRPO worklist(L);
        worklist.perform(LI);
        for (auto *bb : worklist) {
            for (BasicBlock::iterator II = bb->begin(), E = bb->end(); II != E;) {
                auto call = dyn_cast<CallInst>(&*II++);
                if (!call)
                    continue;
                Value *callee = call->getCalledOperand();
                assert(callee != nullptr);
                // It is always legal to extend the preserve period
                // so we only need to make sure it is legal to move/clone
                // the calls.
                // If all the input arguments dominates the whole loop we can
                // hoist the `begin` and if a `begin` dominates the loop the
                // corresponding `end` can be moved to the loop exit.
                if (callee == gc_preserve_begin_func) {
                    bool canhoist = true;
                    for (Use &U : call->args()) {
                        // Check if all arguments are generated outside the loop
                        auto origin = dyn_cast<Instruction>(U.get());
                        if (!origin)
                            continue;
                        if (!DT->properlyDominates(origin->getParent(), header)) {
                            canhoist = false;
                            break;
                        }
                    }
                    if (!canhoist)
                        continue;
                    ++HoistedPreserveBegin;
                    moveInstructionBefore(*call, *preheader->getTerminator(), MSSAU, SE);
                    LLVM_DEBUG(dbgs() << "Hoisted gc_preserve_begin: " << *call << "\n");
                    REMARK([&](){
                        return OptimizationRemark(DEBUG_TYPE, "Hoisted", call)
                            << "hoisting preserve begin " << ore::NV("PreserveBegin", call);
                    });
                    changed = true;
                }
                else if (callee == gc_preserve_end_func) {
                    auto begin = cast<Instruction>(call->getArgOperand(0));
                    if (!DT->properlyDominates(begin->getParent(), header))
                        continue;
                    changed = true;
                    auto exit_pts = get_exit_pts(LI->getLoopFor(begin->getParent()));
                    if (exit_pts.empty()) {
                        ++ErasedPreserveEnd;
                        eraseInstruction(*call, MSSAU);
                        continue;
                    }
                    ++SunkPreserveEnd;
                    moveInstructionBefore(*call, *exit_pts[0], MSSAU, SE, MemorySSA::Beginning);
                    LLVM_DEBUG(dbgs() << "Sunk gc_preserve_end: " << *call << "\n");
                    REMARK([&](){
                        return OptimizationRemark(DEBUG_TYPE, "Sunk", call)
                            << "sinking preserve end " << ore::NV("PreserveEnd", call);
                    });
                    for (unsigned i = 1; i < exit_pts.size(); i++) {
                        // Clone exit
                        auto CI = CallInst::Create(call, {}, exit_pts[i]->getIterator());
                        createNewInstruction(CI, call, MSSAU);
                        LLVM_DEBUG(dbgs() << "Cloned and sunk gc_preserve_end: " << *CI << "\n");
                        REMARK([&](){
                            return OptimizationRemark(DEBUG_TYPE, "Sunk", call)
                                << "cloning and sinking preserve end" << ore::NV("PreserveEnd", call);
                        });
                    }
                }
                else if (isWriteBarrierFunc(callee)) {
                    // Loop stores keep invariant new values live across safepoints, allowing
                    // generational barrier hoisting. Snapshotting barriers must be notified
                    // of the store though, so the hoisted liveness is insufficient (we would
                    // have to also ensure we do not hoist across any safepoint).
#ifndef GC_BARRIER_SNAPSHOT
                    auto make_invariant = [&](Value *value) {
                        return makeLoopInvariant(L, value, changed,
                                                 preheader->getTerminator(), MSSAU, SE);
                    };
                    // Make the parent and new values available in the preheader,
                    // hoisting their computations where safe.
                    if (!make_invariant(call->getArgOperand(0)) ||
                        !llvm::all_of(writeBarrierChildren(call), make_invariant)) {
                        LLVM_DEBUG(dbgs() << "Failed to hoist write barrier: " << *call << "\n");
                        continue;
                    }
                    // Retaining a field barrier also requires its slot addresses.
                    bool slots_invariant = true;
                    if (isFieldWriteBarrier(callee)) {
                        for (unsigned i = field_wb_slot_arg; i < call->arg_size(); i += 2)
                            slots_invariant &= make_invariant(call->getArgOperand(i));
                    }
                    if (slots_invariant) {
                        moveInstructionBefore(*call, *preheader->getTerminator(), MSSAU, SE);
                    }
                    else {
#ifdef GC_BARRIER_FIELD_PRECISE
                        // For a field-precise GC, we assume that demotion to an object barrier
                        // is too imprecise / costly to be worth hoisting.
                        continue;
#else
                        call = hoistAsObjectBarrier(call, preheader, MSSAU);
#endif
                    }
                    ++HoistedWriteBarrier;
                    changed = true;
                    REMARK([&](){
                        return OptimizationRemark(DEBUG_TYPE, "Hoist", call)
                            << "hoisting write barrier " << ore::NV("GC Write Barrier", call);
                    });
#endif
                }
                else if (callee == alloc_obj_func) {
                    bool valid = true;
                    for (std::size_t i = 0; i < call->arg_size(); i++) {
                        if (!makeLoopInvariant(L, call->getArgOperand(i), changed,
                            preheader->getTerminator(), MSSAU, SE)) {
                            valid = false;
                            LLVM_DEBUG(dbgs() << "Failed to hoist alloc_obj argument: " << *call->getArgOperand(i) << "\n");
                            break;
                        }
                    }
                    if (!valid) {
                        LLVM_DEBUG(dbgs() << "Failed to hoist alloc_obj: " << *call << "\n");
                        continue;
                    }
                    if (hoistImmutableAlloc(L, call, preheader, II, *DT, MSSAU, SE)) {
                        changed = true;
                        ++HoistedAllocation;
                        REMARK([&](){
                            return OptimizationRemark(DEBUG_TYPE, "Hoist", call)
                                << "hoisting immutable gc allocation " << ore::NV("GC Allocation", call);
                        });
                        continue;
                    }
                    LLVM_DEBUG(dbgs() << "Running escape analysis for " << *call << "\n");
                    jl_alloc::AllocUseInfo use_info;
                    jl_alloc::CheckInst::Stack check_stack;
                    jl_alloc::EscapeAnalysisRequiredArgs required{use_info, check_stack, *this, DL};
                    jl_alloc::runEscapeAnalysis(call, required, jl_alloc::EscapeAnalysisOptionalArgs().with_valid_loop(L).with_optimization_remark_emitter(&ORE));
                    REMARK([&](){
                        std::string suse_info;
                        llvm::raw_string_ostream osuse_info(suse_info);
                        use_info.dump(osuse_info);
                        return OptimizationRemarkAnalysis(DEBUG_TYPE, "EscapeAnalysis", call) << "escape analysis for " << ore::NV("GC Allocation", call) << "\n" << ore::NV("UseInfo", osuse_info.str());
                    });
                    if (use_info.escaped) {
                        REMARK([&](){
                            return OptimizationRemarkMissed(DEBUG_TYPE, "Escape", call)
                                << "not hoisting gc allocation " << ore::NV("GC Allocation", call)
                                << " because it may escape";
                        });
                        continue;
                    }
                    if (use_info.addrescaped) {
                        REMARK([&](){
                            return OptimizationRemarkMissed(DEBUG_TYPE, "Escape", call)
                                << "not hoisting gc allocation " << ore::NV("GC Allocation", call)
                                << " because its address may escape";
                        });
                        continue;
                    }
                    if (use_info.refstore) {
                        // We need to add write barriers to any stores
                        // that may start crossing generations
                        REMARK([&](){
                            return OptimizationRemarkMissed(DEBUG_TYPE, "Escape", call)
                                << "not hoisting gc allocation " << ore::NV("GC Allocation", call)
                                << " because it may have an object stored to it";
                        });
                        continue;
                    }
                    REMARK([&](){
                        return OptimizationRemark(DEBUG_TYPE, "Hoist", call)
                            << "hoisting gc allocation " << ore::NV("GC Allocation", call);
                    });
                    ++HoistedAllocation;
                    moveInstructionBefore(*call, *preheader->getTerminator(), MSSAU, SE);
                    IRBuilder<> builder(preheader->getTerminator());
                    builder.SetCurrentDebugLocation(call->getDebugLoc());
                    // Note that this alignment is assuming the GC allocates at least pointer-aligned memory
                    auto align = Align(DL.getPointerSize(0));
                    auto clear_obj = builder.CreateMemSet(call, ConstantInt::get(Type::getInt8Ty(call->getContext()), 0), call->getArgOperand(1), align);
                    if (MSSAU.getMemorySSA()) {
                        auto clear_mdef = MSSAU.createMemoryAccessInBB(clear_obj, nullptr, clear_obj->getParent(), MemorySSA::BeforeTerminator);
                        MSSAU.insertDef(cast<MemoryDef>(clear_mdef), true);
                    }
                    changed = true;
                }
            }
        }
        if (changed && SE) {
            SE->forgetLoopDispositions();
        }
#ifdef JL_VERIFY_PASSES
        assert(!verifyLLVMIR(*L));
#endif
        return changed;
    }
};

} //namespace

PreservedAnalyses JuliaLICMPass::run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U)
{
    OptimizationRemarkEmitter ORE(L.getHeader()->getParent());
    auto GetDT = [&AR]() -> DominatorTree & {
        return AR.DT;
    };
    auto GetLI = [&AR]() -> LoopInfo & {
        return AR.LI;
    };
    auto GetMSSA = [&AR]() {
        return AR.MSSA;
    };
    auto GetSE = [&AR]() {
        return &AR.SE;
    };
    auto juliaLICM = JuliaLICM(GetDT, GetLI, GetMSSA, GetSE);
    if (juliaLICM.runOnLoop(&L, ORE)) {
#ifdef JL_DEBUG_BUILD
        if (AR.MSSA)
            AR.MSSA->verifyMemorySSA();
#endif
        auto preserved = getLoopPassPreservedAnalyses();
        preserved.preserveSet<CFGAnalyses>();
        preserved.preserve<MemorySSAAnalysis>();
        return preserved;
    }
    return PreservedAnalyses::all();
}
