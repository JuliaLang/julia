// This file is a part of Julia. License is MIT: https://julialang.org/license

#undef DEBUG
#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/OptimizationRemarkEmitter.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/PromoteMemToReg.h>

#include <llvm/InitializePasses.h>

#include "passes.h"
#include "llvm-codegen-shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"
#include "llvm-alloc-helpers.h"

#include <map>
#include <set>

#define DEBUG_TYPE "alloc-opt"
#include "julia_assert.h"

using namespace llvm;
using namespace jl_alloc;

STATISTIC(RemovedAllocs, "Total number of heap allocations elided");
STATISTIC(DeletedAllocs, "Total number of heap allocations fully deleted");
STATISTIC(SplitAllocs, "Total number of allocations split into registers");
STATISTIC(StackAllocs, "Total number of allocations moved to the stack");
STATISTIC(RemovedTypeofs, "Total number of typeofs removed");
STATISTIC(RemovedWriteBarriers, "Total number of write barriers removed");
STATISTIC(RemovedGCPreserve, "Total number of GC preserve instructions removed");

namespace {

static void removeGCPreserve(CallInst *call, Instruction *val)
{
    ++RemovedGCPreserve;
    auto replace = Constant::getNullValue(val->getType());
    call->replaceUsesOfWith(val, replace);
    call->setAttributes(AttributeList());
    for (auto &arg: call->args()) {
        if (!isa<Constant>(arg.get())) {
            return;
        }
    }
    while (!call->use_empty()) {
        auto end = cast<Instruction>(*call->user_begin());
        // gc_preserve_end returns void.
        assert(end->use_empty());
        end->eraseFromParent();
    }
    call->eraseFromParent();
}

/**
 * Promote `julia.gc_alloc_obj` which do not have escaping root to a alloca.
 * Uses that are not considered to escape the object (i.e. heap address) includes,
 *
 * * load
 * * `pointer_from_objref`
 * * `gc_loaded`
 * * Any real llvm intrinsics
 * * gc preserve intrinsics
 * * `ccall` gcroot array (`jl_roots` operand bundle)
 * * store (as address)
 * * addrspacecast, bitcast, getelementptr
 *
 *     The results of these cast instructions will be scanned recursively.
 *
 * All other uses are considered to escape conservatively.
 */

/**
 * TODO:
 * * Return twice
 * * Handle phi node.
 * * Handle jl_box*
 */

#ifndef __clang_gcanalyzer__
#define REMARK(remark) ORE.emit(remark)
#else
#define REMARK(remark) (void) 0;
#endif
struct AllocOpt : public JuliaPassContext {

    const DataLayout *DL;

    Function *lifetime_start;
    Function *lifetime_end;

    bool doInitialization(Module &m);
    bool runOnFunction(Function &F, function_ref<DominatorTree&()> GetDT);
};

struct Optimizer {
    Optimizer(Function &F, AllocOpt &pass, function_ref<DominatorTree&()> GetDT)
        : F(F),
          ORE(&F),
          pass(pass),
          GetDT(std::move(GetDT))
    {}

    void initialize();
    void optimizeAll();
    bool finalize();
private:
    bool isSafepoint(Instruction *inst);
    Instruction *getFirstSafepoint(BasicBlock *bb);
    ssize_t getGCAllocSize(Instruction *I);
    void pushInstruction(Instruction *I);

    void insertLifetimeEnd(Value *ptr, Constant *sz, Instruction *insert);
    // insert llvm.lifetime.* calls for `ptr` with size `sz` based on the use of `orig`.
    void insertLifetime(Value *ptr, Constant *sz, Instruction *orig);

    void checkInst(CallInst *I);

    void replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID,
                                 Instruction *orig_i, Instruction *new_i);
    void removeAlloc(CallInst *orig_inst);
    void moveToStack(CallInst *orig_inst, size_t sz, bool has_ref, AllocFnKind allockind);
    void initializeAlloca(IRBuilder<> &prolog_builder, AllocaInst *buff, AllocFnKind allockind);
    void splitOnStack(CallInst *orig_inst);
    void optimizeTag(CallInst *orig_inst);

    Function &F;
    OptimizationRemarkEmitter ORE;
    AllocOpt &pass;
    DominatorTree *_DT = nullptr;
    function_ref<DominatorTree &()> GetDT;

    DominatorTree &getDomTree()
    {
        if (!_DT)
            _DT = &GetDT();
        return *_DT;
    }
    struct Lifetime {
        struct Frame {
            BasicBlock *bb;
            pred_iterator p_cur;
            pred_iterator p_end;
            Frame(BasicBlock *bb)
                : bb(bb),
                  p_cur(pred_begin(bb)),
                  p_end(pred_end(bb))
            {}
        };
        typedef SmallVector<Frame,4> Stack;
    };
    struct ReplaceUses {
        struct Frame {
            Instruction *orig_i;
            union {
                Instruction *new_i;
                uint32_t offset;
            };
            Frame(Instruction *orig_i, Instruction *new_i)
                : orig_i(orig_i),
                  new_i(new_i)
            {}
            Frame(Instruction *orig_i, uint32_t offset)
                : orig_i(orig_i),
                  offset(offset)
            {}
        };
        typedef SmallVector<Frame,4> Stack;
    };

    SetVector<std::pair<CallInst*,size_t>> worklist;
    SmallVector<CallInst*,6> removed;
    AllocUseInfo use_info;
    CheckInst::Stack check_stack;
    Lifetime::Stack lifetime_stack;
    ReplaceUses::Stack replace_stack;
    std::map<BasicBlock*, llvm::WeakVH> first_safepoint;
};

void Optimizer::pushInstruction(Instruction *I)
{
    ssize_t sz = getGCAllocSize(I);
    if (sz != -1) {
        worklist.insert(std::make_pair(cast<CallInst>(I), sz));
    }
}

void Optimizer::initialize()
{
    for (auto &bb: F) {
        for (auto &I: bb) {
            pushInstruction(&I);
        }
    }
}

void Optimizer::optimizeAll()
{
    while (!worklist.empty()) {
        auto item = worklist.pop_back_val();
        auto orig = item.first;
        size_t sz = item.second;
        checkInst(orig);
        if (use_info.escaped) {
            REMARK([&]() {
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC allocation escaped " << ore::NV("GC Allocation", StringRef(str));
            });
            if (use_info.hastypeof)
                optimizeTag(orig);
            continue;
        }
        if (use_info.haserror || use_info.returned) {
            REMARK([&]() {
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC allocation has error or was returned " << ore::NV("GC Allocation", StringRef(str));
            });
            if (use_info.hastypeof)
                optimizeTag(orig);
            continue;
        }
        if (!use_info.addrescaped && !use_info.hasload && (!use_info.haspreserve ||
                                                           !use_info.refstore)) {
            REMARK([&]() {
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemark(DEBUG_TYPE, "Dead Allocation", orig)
                    << "GC allocation removed " << ore::NV("GC Allocation", StringRef(str));
            });
            // No one took the address, no one reads anything and there's no meaningful
            // preserve of fields (either no preserve/ccall or no object reference fields)
            // We can just delete all the uses.
            removeAlloc(orig);
            continue;
        }
        bool has_unboxed = use_info.has_unknown_unboxed;
        bool has_ref = use_info.has_unknown_objref;
        bool has_refaggr = use_info.has_unknown_objrefaggr;
        for (auto memop: use_info.memops) {
            auto &field = memop.second;
            has_unboxed |= field.hasunboxed;
            if (field.hasobjref) {
                has_ref = true;
                // This can be relaxed a little based on hasload
                // TODO: add support for hasaggr load/store
                if (field.hasaggr || field.multiloc || field.size != sizeof(void*)) {
                    has_refaggr = true;
                    break;
                }
            }
        }
        if (has_refaggr) {
            REMARK([&]() {
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC allocation has unusual object reference, unable to move to stack " << ore::NV("GC Allocation", StringRef(str));
            });
            if (use_info.hastypeof)
                optimizeTag(orig);
            continue;
        }
        if (!use_info.hasunknownmem && !use_info.addrescaped) {
            REMARK([&](){
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemark(DEBUG_TYPE, "Stack Split Allocation", orig)
                    << "GC allocation split on stack " << ore::NV("GC Allocation", StringRef(str));
            });
            // No one actually care about the memory layout of this object, split it.
            splitOnStack(orig);
            continue;
        }
        // The move to stack code below, if has_ref is set, changes the allocation to an array of jlvalue_t's. This is fine
        // if all objects are jlvalue_t's. However, if part of the allocation is an unboxed value (e.g. it is a { float, jlvaluet }),
        // then moveToStack will create a [2 x jlvaluet] bitcast to { float, jlvaluet }.
        // This later causes the GC rooting pass, to miss-characterize the float as a pointer to a GC value
        if (has_unboxed && has_ref) {
            REMARK([&]() {
                std::string str;
                llvm::raw_string_ostream rso(str);
                orig->print(rso);
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC allocation could not be split since it contains both boxed and unboxed values, unable to move to stack " << ore::NV("GC Allocation", StringRef(str));
            });
            if (use_info.hastypeof)
                optimizeTag(orig);
            continue;
        }
        REMARK([&](){
            std::string str;
            llvm::raw_string_ostream rso(str);
            orig->print(rso);
            return OptimizationRemark(DEBUG_TYPE, "Stack Move Allocation", orig)
                << "GC allocation moved to stack " << ore::NV("GC Allocation", StringRef(str));
        });
        // The object has no fields with mix reference access
        moveToStack(orig, sz, has_ref, use_info.allockind);
    }
}

bool Optimizer::finalize()
{
    if (removed.empty())
        return false;
    for (auto inst: removed)
        inst->eraseFromParent();
    return true;
}

bool Optimizer::isSafepoint(Instruction *inst)
{
    auto call = dyn_cast<CallInst>(inst);
    if (!call)
        return false;
    if (isa<IntrinsicInst>(call))
        return false;
    if (auto callee = call->getCalledFunction()) {
        // Known functions emitted in codegen that are not safepoints
        if (callee == pass.pointer_from_objref_func
            || callee == pass.gc_loaded_func
            || callee->getName() == "memcmp") {
            return false;
        }
    }
    return true;
}

Instruction *Optimizer::getFirstSafepoint(BasicBlock *bb)
{
    auto it = first_safepoint.find(bb);
    if (it != first_safepoint.end()) {
        Value *Val = it->second;
        if (Val)
            return cast<Instruction>(Val);
    }
    Instruction *first = nullptr;
    for (auto &I: *bb) {
        if (isSafepoint(&I)) {
            first = &I;
            break;
        }
    }
    first_safepoint[bb] = first;
    return first;
}

ssize_t Optimizer::getGCAllocSize(Instruction *I)
{
    auto call = dyn_cast<CallInst>(I);
    if (!call)
        return -1;
    if (call->getCalledOperand() != pass.alloc_obj_func)
        return -1;
    assert(call->arg_size() == 3);
    auto CI = dyn_cast<ConstantInt>(call->getArgOperand(1));
    if (!CI)
        return -1;
    size_t sz = (size_t)CI->getZExtValue();
    if (sz < IntegerType::MAX_INT_BITS / 8 && sz < INT32_MAX)
        return sz;
    return -1;
}

void Optimizer::checkInst(CallInst *I)
{
    LLVM_DEBUG(dbgs() << "Running escape analysis on " << *I << "\n");
    jl_alloc::EscapeAnalysisRequiredArgs required{use_info, check_stack, pass, *pass.DL};
    jl_alloc::runEscapeAnalysis(I, required, jl_alloc::EscapeAnalysisOptionalArgs().with_optimization_remark_emitter(&ORE));
    REMARK([&](){
        std::string suse_info;
        llvm::raw_string_ostream osuse_info(suse_info);
        use_info.dump(osuse_info);
        std::string str;
        llvm::raw_string_ostream rso(str);
        I->print(rso);
        return OptimizationRemarkAnalysis(DEBUG_TYPE, "EscapeAnalysis", I) << "escape analysis for " << ore::NV("GC Allocation", StringRef(str)) << "\n" << ore::NV("UseInfo", osuse_info.str());
    });
}

void Optimizer::insertLifetimeEnd(Value *ptr, Constant *sz, Instruction *insert)
{
    BasicBlock::iterator it(insert);
    BasicBlock::iterator begin(insert->getParent()->begin());
    // Makes sure that the end is inserted before nearby start.
    // We insert start before the allocation call, if it is the first safepoint we find for
    // another instruction, it's better if we insert the end before the start instead of the
    // allocation so that the two allocations do not have overlapping lifetime.
    while (it != begin) {
        --it;
        if (auto II = dyn_cast<IntrinsicInst>(&*it)) {
            if (II->getIntrinsicID() == Intrinsic::lifetime_start ||
                II->getIntrinsicID() == Intrinsic::lifetime_end) {
                insert = II;
                continue;
            }
        }
        break;
    }
#if JL_LLVM_VERSION >= 200000
    CallInst::Create(pass.lifetime_end, {sz, ptr}, "", insert->getIterator());
#else
    CallInst::Create(pass.lifetime_end, {sz, ptr}, "", insert);
#endif
}

void Optimizer::insertLifetime(Value *ptr, Constant *sz, Instruction *orig)
{
#if JL_LLVM_VERSION >= 200000
    CallInst::Create(pass.lifetime_start, {sz, ptr}, "", orig->getIterator());
#else
    CallInst::Create(pass.lifetime_start, {sz, ptr}, "", orig);
#endif
    BasicBlock *def_bb = orig->getParent();
    std::set<BasicBlock*> bbs{def_bb};
    auto &DT = getDomTree();
    // Collect all BB where the allocation is live
    for (auto use: use_info.uses) {
        auto bb = use->getParent();
        if (!bbs.insert(bb).second)
            continue;
        if (pred_empty(bb))
            continue; // No predecessors so the block is dead
        assert(lifetime_stack.empty());
        Lifetime::Frame cur{bb};
        while (true) {
            assert(cur.p_cur != cur.p_end);
            auto pred = *cur.p_cur;
            ++cur.p_cur;
            if (bbs.insert(pred).second) {
                if (cur.p_cur != cur.p_end)
                    lifetime_stack.push_back(cur);
                cur = Lifetime::Frame(pred);
            }
            if (cur.p_cur == cur.p_end) {
                if (lifetime_stack.empty())
                    break;
                cur = lifetime_stack.back();
                lifetime_stack.pop_back();
            }
        }
    }
#ifndef JL_NDEBUG
    for (auto bb: bbs) {
        if (bb == def_bb)
            continue;
        if (DT.dominates(orig, bb))
            continue;
        auto F = bb->getParent();
        llvm_dump(F);
        llvm_dump(orig);
        jl_safe_printf("Does not dominate BB:\n");
        llvm_dump(bb);
        abort();
    }
#endif

    // Record extra BBs that contain invisible uses with gc_preserve_{begin,end}.
    // We traverse the dominator tree starting at each `gc_preserve_begin` and marking blocks
    // as users until a corresponding `gc_preserve_end` is found. Blocks containing
    // the `gc_preserve_end` have already been marked in the previous step.
    SmallSet<BasicBlock*, 8> extra_use;
    SmallVector<DomTreeNodeBase<BasicBlock>*, 8> dominated;
    for (auto preserve: use_info.preserves) {
        assert(dominated.empty());
        dominated.push_back(DT.getNode(preserve->getParent()));
        while (!dominated.empty()) {
            auto N = dominated.pop_back_val();
            if (!N) {
                dominated.clear();
                break;
            }
            auto bb = N->getBlock();
            if (extra_use.count(bb))
                continue;
            bool ended = false;
            for (auto end: preserve->users()) {
                auto end_bb = cast<Instruction>(end)->getParent();
                auto end_node = DT.getNode(end_bb);
                if (end_bb == bb || (end_node && DT.dominates(end_node, N))) {
                    ended = true;
                    break;
                }
            }
            if (ended)
                continue;
            bbs.insert(bb);
            extra_use.insert(bb);
            dominated.append(N->begin(), N->end());
        }
    }

    // For each BB, find the first instruction(s) where the allocation is possibly dead.
    // If all successors are live, then there isn't one.
    // If the BB has "invisible" uses, then there isn't one.
    // If all successors are dead, then it's the first instruction after the last use
    // within the BB.
    // If some successors are live and others are dead, it's the first instruction in
    // the successors that are dead.
    SmallVector<Instruction*, 0> first_dead;
    for (auto bb: bbs) {
        bool has_use = false;
        for (auto succ: successors(bb)) {
            // def_bb is the only bb in bbs that's not dominated by orig
            if (succ != def_bb && bbs.count(succ)) {
                has_use = true;
                break;
            }
        }
        if (has_use) {
            for (auto succ: successors(bb)) {
                if (!bbs.count(succ)) {
                    first_dead.push_back(&*succ->begin());
                }
            }
        }
        else if (extra_use.count(bb)) {
            first_dead.push_back(bb->getTerminator());
        }
        else {
            for (auto it = bb->rbegin(), end = bb->rend(); it != end; ++it) {
                if (use_info.uses.count(&*it)) {
                    --it;
                    first_dead.push_back(&*it);
                    break;
                }
            }
        }
    }
    bbs.clear();
    // There can/need only be one lifetime.end for each allocation in each bb, use bbs
    // to record that.
    // Iterate through the first dead and find the first safepoint following each of them.
    while (!first_dead.empty()) {
        auto I = first_dead.back();
        first_dead.pop_back();
        auto bb = I->getParent();
        if (!bbs.insert(bb).second)
            continue;
        if (I == &*bb->begin()) {
            // There's no use in or after this bb. If this bb is not dominated by
            // the def then it has to be dead on entering this bb.
            // Otherwise, there could be use that we don't track
            // before hitting the next safepoint.
            if (!DT.dominates(orig, bb)) {
                insertLifetimeEnd(ptr, sz, &*bb->getFirstInsertionPt());
                continue;
            }
            else if (auto insert = getFirstSafepoint(bb)) {
                insertLifetimeEnd(ptr, sz, insert);
                continue;
            }
        }
        else {
            assert(bb == def_bb || DT.dominates(orig, I));
            BasicBlock::iterator it(I);
            BasicBlock::iterator end = bb->end();
            bool safepoint_found = false;
            for (; it != end; ++it) {
                auto insert = &*it;
                if (isSafepoint(insert)) {
                    insertLifetimeEnd(ptr, sz, insert);
                    safepoint_found = true;
                    break;
                }
            }
            if (safepoint_found) {
                continue;
            }
        }
        for (auto succ: successors(bb)) {
            first_dead.push_back(&*succ->begin());
        }
    }
}

void Optimizer::replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID,
                                        Instruction *orig_i, Instruction *new_i)
{
    auto nargs = call->arg_size();
    SmallVector<Value*, 8> args(nargs);
    SmallVector<Type*, 8> argTys(nargs);
    for (unsigned i = 0; i < nargs; i++) {
        auto arg = call->getArgOperand(i);
        args[i] = arg == orig_i ? new_i : arg;
        argTys[i] = args[i]->getType();
    }
    auto oldfType = call->getFunctionType();
    auto newfType = FunctionType::get(
            oldfType->getReturnType(),
            ArrayRef<Type*>(argTys).slice(0, oldfType->getNumParams()),
            oldfType->isVarArg());

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
#if JL_LLVM_VERSION >= 200000
    auto newF = Intrinsic::getOrInsertDeclaration(call->getModule(), ID, overloadTys);
#else
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
#endif
    assert(newF->getFunctionType() == newfType);
    newF->setCallingConv(call->getCallingConv());
#if JL_LLVM_VERSION >= 200000
    auto newCall = CallInst::Create(newF, args, "", call->getIterator());
#else
    auto newCall = CallInst::Create(newF, args, "", call);
#endif
    newCall->setTailCallKind(call->getTailCallKind());
    auto old_attrs = call->getAttributes();
    newCall->setAttributes(AttributeList::get(pass.getLLVMContext(), getFnAttrs(old_attrs),
                                              getRetAttrs(old_attrs), {}));
    newCall->setDebugLoc(call->getDebugLoc());
    call->replaceAllUsesWith(newCall);
    call->eraseFromParent();
}

void Optimizer::initializeAlloca(IRBuilder<> &prolog_builder, AllocaInst *buff, AllocFnKind allockind)
{
    if ((allockind & AllocFnKind::Uninitialized) != AllocFnKind::Unknown)
        return;
    assert(!buff->isArrayAllocation());
    Type *T = buff->getAllocatedType();
    const DataLayout &DL = F.getParent()->getDataLayout();
    prolog_builder.CreateMemSet(buff, ConstantInt::get(Type::getInt8Ty(prolog_builder.getContext()), 0), DL.getTypeAllocSize(T), buff->getAlign());

}

// This function should not erase any safepoint so that the lifetime marker can find and cache
// all the original safepoints.
void Optimizer::moveToStack(CallInst *orig_inst, size_t sz, bool has_ref, AllocFnKind allockind)
{
    ++RemovedAllocs;
    ++StackAllocs;
    auto tag = orig_inst->getArgOperand(2);
    removed.push_back(orig_inst);
    // The allocation does not escape or get used in a phi node so none of the derived
    // SSA from it are live when we run the allocation again.
    // It is now safe to promote the allocation to an entry block alloca.
    size_t align = 1;
    // TODO: This is overly conservative. May want to instead pass this as a
    //       parameter to the allocation function directly.
    if (sz > 1)
        align = MinAlign(JL_SMALL_BYTE_ALIGNMENT, NextPowerOf2(sz));
    // No debug info for prolog instructions
    IRBuilder<> prolog_builder(&F.getEntryBlock().front());
    AllocaInst *buff;
    Instruction *ptr;
    if (sz == 0) {
        ptr = buff = prolog_builder.CreateAlloca(Type::getInt8Ty(prolog_builder.getContext()), ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), 0));
    }
    else if (has_ref) {
        // Allocate with the correct type so that the GC frame lowering pass will
        // treat this as a non-mem2reg'd alloca
        // The ccall root and GC preserve handling below makes sure that
        // the alloca isn't optimized out.
        const DataLayout &DL = F.getParent()->getDataLayout();
        auto asize = ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), sz / DL.getTypeAllocSize(pass.T_prjlvalue));
        buff = prolog_builder.CreateAlloca(pass.T_prjlvalue, asize);
        buff->setAlignment(Align(align));
        ptr = cast<Instruction>(buff);
    }
    else {
        Type *buffty;
        if (pass.DL->isLegalInteger(sz * 8))
            buffty = Type::getIntNTy(pass.getLLVMContext(), sz * 8);
        else
            buffty = ArrayType::get(Type::getInt8Ty(pass.getLLVMContext()), sz);
        buff = prolog_builder.CreateAlloca(buffty);
        buff->setAlignment(Align(align));
        ptr = cast<Instruction>(buff);
    }
    insertLifetime(ptr, ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), sz), orig_inst);
    if (sz != 0 && !has_ref) { // TODO: fix has_ref case too
        IRBuilder<> builder(orig_inst);
        initializeAlloca(builder, buff, allockind);
    }
    Instruction *new_inst = cast<Instruction>(ptr);
    new_inst->takeName(orig_inst);

    auto simple_replace = [&] (Instruction *orig_i, Instruction *new_i) {
        if (orig_i->user_empty()) {
            if (orig_i != orig_inst)
                orig_i->eraseFromParent();
            return true;
        }
        Type *orig_t = orig_i->getType();
        Type *new_t = new_i->getType();
        if (orig_t == new_t) {
            orig_i->replaceAllUsesWith(new_i);
            if (orig_i != orig_inst)
                orig_i->eraseFromParent();
            return true;
        }
        return false;
    };
    if (simple_replace(orig_inst, new_inst)) {
        LLVM_DEBUG(dbgs() << "Simple replace of allocation was successful in stack move\n");
        return;
    }
    assert(replace_stack.empty());
    ReplaceUses::Frame cur{orig_inst, new_inst};
    auto finish_cur = [&] () {
        assert(cur.orig_i->user_empty());
        if (cur.orig_i != orig_inst) {
            cur.orig_i->eraseFromParent();
        }
    };
    auto push_frame = [&] (Instruction *orig_i, Instruction *new_i) {
        if (simple_replace(orig_i, new_i))
            return;
        replace_stack.push_back(cur);
        cur = {orig_i, new_i};
    };
    // Both `orig_i` and `new_i` should be pointer of the same type
    // but possibly different address spaces. `new_i` is always in addrspace 0.
    auto replace_inst = [&] (Instruction *user) {
        Instruction *orig_i = cur.orig_i;
        Instruction *new_i = cur.new_i;
        if (isa<LoadInst>(user) || isa<StoreInst>(user) ||
            isa<AtomicCmpXchgInst>(user) || isa<AtomicRMWInst>(user)) {
            // TODO: these atomics are likely removable if the user is the first argument
            user->replaceUsesOfWith(orig_i, new_i);
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledOperand();
            if (pass.pointer_from_objref_func == callee) {
                call->replaceAllUsesWith(prolog_builder.CreateAddrSpaceCast(new_i, call->getCalledFunction()->getReturnType()));
                call->eraseFromParent();
                return;
            }
            if (pass.gc_loaded_func == callee) {
                // TODO: handle data pointer forwarding, length forwarding, and fence removal
                user->replaceUsesOfWith(orig_i, Constant::getNullValue(orig_i->getType()));
                return;
            }
            if (pass.typeof_func == callee) {
                ++RemovedTypeofs;
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            // Also remove the preserve intrinsics so that it can be better optimized.
            if (pass.gc_preserve_begin_func == callee) {
                if (has_ref) {
                    call->replaceUsesOfWith(orig_i, buff);
                }
                else {
                    removeGCPreserve(call, orig_i);
                }
                return;
            }
            if (pass.write_barrier_func == callee) {
                ++RemovedWriteBarriers;
                call->eraseFromParent();
                return;
            }
            if (auto intrinsic = dyn_cast<IntrinsicInst>(call)) {
                if (Intrinsic::ID ID = intrinsic->getIntrinsicID()) {
                    replaceIntrinsicUseWith(intrinsic, ID, orig_i, new_i);
                    return;
                }
            }
            // remove from operand bundle
            Value *replace = has_ref ? (Value*)buff : Constant::getNullValue(orig_i->getType());
            user->replaceUsesOfWith(orig_i, replace);
        }
        else if (isa<AddrSpaceCastInst>(user) || isa<BitCastInst>(user)) {
            push_frame(user, new_i);
        }
        else if (auto gep = dyn_cast<GetElementPtrInst>(user)) {
            SmallVector<Value *, 4> IdxOperands(gep->idx_begin(), gep->idx_end());
            auto new_gep = GetElementPtrInst::Create(gep->getSourceElementType(),
                                                     new_i, IdxOperands,
#if JL_LLVM_VERSION >= 200000
                                                     gep->getName(), gep->getIterator());
#else
                                                     gep->getName(), gep);
#endif
            new_gep->setIsInBounds(gep->isInBounds());
            new_gep->takeName(gep);
            new_gep->copyMetadata(*gep);
            push_frame(gep, new_gep);
        }
        else {
            abort();
        }
    };

    while (true) {
        replace_inst(cast<Instruction>(*cur.orig_i->user_begin()));
        while (cur.orig_i->use_empty()) {
            finish_cur();
            if (replace_stack.empty())
                return;
            cur = replace_stack.back();
            replace_stack.pop_back();
        }
    }
}

// This function should not erase any safepoint so that the lifetime marker can find and cache
// all the original safepoints.
void Optimizer::removeAlloc(CallInst *orig_inst)
{
    ++RemovedAllocs;
    ++DeletedAllocs;
    auto tag = orig_inst->getArgOperand(2);
    removed.push_back(orig_inst);
    auto simple_remove = [&] (Instruction *orig_i) {
        if (orig_i->user_empty()) {
            if (orig_i != orig_inst)
                orig_i->eraseFromParent();
            return true;
        }
        return false;
    };
    if (simple_remove(orig_inst)) {
        LLVM_DEBUG(dbgs() << "Simple remove of allocation was successful in removeAlloc\n");
        return;
    }
    assert(replace_stack.empty());
    ReplaceUses::Frame cur{orig_inst, nullptr};
    auto finish_cur = [&] () {
        assert(cur.orig_i->user_empty());
        if (cur.orig_i != orig_inst) {
            cur.orig_i->eraseFromParent();
        }
    };
    auto push_frame = [&] (Instruction *orig_i) {
        if (simple_remove(orig_i))
            return;
        replace_stack.push_back(cur);
        cur = {orig_i, nullptr};
    };
    auto remove_inst = [&] (Instruction *user) {
        Instruction *orig_i = cur.orig_i;
        if (auto store = dyn_cast<StoreInst>(user)) {
            // All stores are known to be dead.
            // The stored value might be an gc pointer in which case deleting the object
            // might open more optimization opportunities.
            if (auto stored_inst = dyn_cast<Instruction>(store->getValueOperand()))
                pushInstruction(stored_inst);
            user->eraseFromParent();
            return;
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledOperand();
            if (pass.gc_preserve_begin_func == callee) {
                removeGCPreserve(call, orig_i);
                return;
            }
            if (pass.typeof_func == callee) {
                ++RemovedTypeofs;
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            if (pass.write_barrier_func == callee) {
                ++RemovedWriteBarriers;
                call->eraseFromParent();
                return;
            }
            if (auto II = dyn_cast<IntrinsicInst>(call)) {
                auto id = II->getIntrinsicID();
                if (id == Intrinsic::memset || id == Intrinsic::lifetime_start ||
                    id == Intrinsic::lifetime_end || isa<DbgInfoIntrinsic>(II)) {
                    call->eraseFromParent();
                    return;
                }
            }
            // remove from operand bundle
            user->replaceUsesOfWith(orig_i, Constant::getNullValue(orig_i->getType()));
        }
        else if (isa<AddrSpaceCastInst>(user) || isa<BitCastInst>(user) ||
                 isa<GetElementPtrInst>(user)) {
            push_frame(user);
        }
        else {
            abort();
        }
    };

    while (true) {
        remove_inst(cast<Instruction>(*cur.orig_i->user_begin()));
        while (cur.orig_i->use_empty()) {
            finish_cur();
            if (replace_stack.empty())
                return;
            cur = replace_stack.back();
            replace_stack.pop_back();
        }
    }
}

// Unable to optimize out the allocation, do store to load forwarding on the tag instead.
void Optimizer::optimizeTag(CallInst *orig_inst)
{
    auto tag = orig_inst->getArgOperand(2);
    // `julia.typeof` is only legal on the original pointer, no need to scan recursively
    size_t last_deleted = removed.size();
    for (auto user: orig_inst->users()) {
        if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledOperand();
            if (pass.typeof_func == callee) {
                ++RemovedTypeofs;
                REMARK([&](){
                    std::string str;
                    llvm::raw_string_ostream rso(str);
                    orig_inst->print(rso);
                    return OptimizationRemark(DEBUG_TYPE, "typeof", call)
                        << "removed typeof call for GC allocation " << ore::NV("Alloc", StringRef(str));
                });
                call->replaceAllUsesWith(tag);
                // Push to the removed instructions to trigger `finalize` to
                // return the correct result.
                // Also so that we don't have to worry about iterator invalidation...
                removed.push_back(call);
            }
        }
    }
    while (last_deleted < removed.size())
        removed[last_deleted++]->replaceUsesOfWith(orig_inst, UndefValue::get(orig_inst->getType()));
}

void Optimizer::splitOnStack(CallInst *orig_inst)
{
    auto tag = orig_inst->getArgOperand(2);
    ++RemovedAllocs;
    ++SplitAllocs;
    removed.push_back(orig_inst);
    IRBuilder<> prolog_builder(&F.getEntryBlock().front());
    struct SplitSlot {
        AllocaInst *slot;
        bool isref;
        uint32_t offset;
        uint32_t size;
    };
    SmallVector<SplitSlot,8> slots;
    for (auto memop: use_info.memops) {
        auto offset = memop.first;
        auto &field = memop.second;
        // If the field has no reader and is not a object reference field that we
        // need to preserve at some point, there's no need to allocate the field.
        if (!field.hasload && (!field.hasobjref || !use_info.haspreserve))
            continue;
        SplitSlot slot{nullptr, field.hasobjref, offset, field.size};
        Type *allocty;
        if (field.hasobjref) {
            allocty = pass.T_prjlvalue;
        }
        else if (field.elty && !field.multiloc) {
            allocty = field.elty;
        }
        else if (pass.DL->isLegalInteger(field.size * 8)) {
            allocty = Type::getIntNTy(pass.getLLVMContext(), field.size * 8);
        } else {
            allocty = ArrayType::get(Type::getInt8Ty(pass.getLLVMContext()), field.size);
        }
        slot.slot = prolog_builder.CreateAlloca(allocty);
        IRBuilder<> builder(orig_inst);
        insertLifetime(slot.slot, ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), field.size), orig_inst);
        initializeAlloca(builder, slot.slot, use_info.allockind);
        slots.push_back(std::move(slot));
    }
    const auto nslots = slots.size();
    auto find_slot = [&] (uint32_t offset) {
        if (offset == 0)
            return 0u;
        unsigned lb = 0;
        unsigned ub = slots.size();
        while (lb + 1 < ub) {
            unsigned mid = (lb + ub) / 2;
            if (slots[mid].offset <= offset) {
                lb = mid;
            }
            else {
                ub = mid;
            }
        }
        return lb;
    };
    auto simple_replace = [&] (Instruction *orig_i) {
        if (orig_i->user_empty()) {
            if (orig_i != orig_inst)
                orig_i->eraseFromParent();
            return true;
        }
        return false;
    };
    if (simple_replace(orig_inst)) {
        LLVM_DEBUG(dbgs() << "Simple replace of allocation was successful in stack split\n");
        return;
    }
    assert(replace_stack.empty());
    ReplaceUses::Frame cur{orig_inst, uint32_t(0)};
    auto finish_cur = [&] () {
        assert(cur.orig_i->user_empty());
        if (cur.orig_i != orig_inst) {
            cur.orig_i->eraseFromParent();
        }
    };
    auto push_frame = [&] (Instruction *orig_i, uint32_t offset) {
        if (simple_replace(orig_i))
            return;
        replace_stack.push_back(cur);
        cur = {orig_i, offset};
    };
    auto slot_gep = [&] (SplitSlot &slot, uint32_t offset, Type *elty, IRBuilder<> &builder) {
        assert(slot.offset <= offset);
        offset -= slot.offset;
        auto size = pass.DL->getTypeAllocSize(elty);
        Value *addr;
        if (offset % size == 0) {
            addr = slot.slot;
            if (offset != 0) {
                addr = builder.CreateConstInBoundsGEP1_32(elty, addr, offset / size);
            }
        }
        else {
            addr = slot.slot;
            addr = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), addr, offset);
        }
        return addr;
    };
    auto replace_inst = [&] (Use *use) {
        Instruction *user = cast<Instruction>(use->getUser());
        Instruction *orig_i = cur.orig_i;
        uint32_t offset = cur.offset;
        if (auto load = dyn_cast<LoadInst>(user)) {
            auto slot_idx = find_slot(offset);
            auto &slot = slots[slot_idx];
            assert(slot.offset <= offset && slot.offset + slot.size >= offset);
            IRBuilder<> builder(load);
            Value *val;
            Type *load_ty = load->getType();
            LoadInst *newload;
            if (slot.isref) {
                assert(slot.offset == offset);
                newload = builder.CreateLoad(pass.T_prjlvalue, slot.slot);
                // Assume the addrspace is correct.
                val = newload;
            }
            else {
                newload = builder.CreateLoad(load_ty, slot_gep(slot, offset, load_ty, builder));
                val = newload;
            }
            // TODO: should we use `load->clone()`, or manually copy any other metadata?
            newload->setAlignment(load->getAlign());
            // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
            newload->setOrdering(AtomicOrdering::NotAtomic);
            load->replaceAllUsesWith(val);
            load->eraseFromParent();
            return;
        }
        else if (auto store = dyn_cast<StoreInst>(user)) {
            if (auto stored_inst = dyn_cast<Instruction>(store->getValueOperand()))
                pushInstruction(stored_inst);
            auto slot_idx = find_slot(offset);
            auto &slot = slots[slot_idx];
            if (slot.offset > offset || slot.offset + slot.size <= offset) {
                store->eraseFromParent();
                return;
            }
            IRBuilder<> builder(store);
            auto store_val = store->getValueOperand();
            auto store_ty = store_val->getType();
            StoreInst *newstore;
            if (slot.isref) {
                assert(slot.offset == offset);
                auto T_pjlvalue = JuliaType::get_pjlvalue_ty(builder.getContext());
                if (!isa<PointerType>(store_ty)) {
                    store_val = builder.CreateBitCast(store_val, pass.DL->getIntPtrType(builder.getContext(), T_pjlvalue->getAddressSpace()));
                    store_val = builder.CreateIntToPtr(store_val, T_pjlvalue);
                    store_ty = T_pjlvalue;
                }
                else {
                    store_ty = PointerType::get(T_pjlvalue->getContext(), store_ty->getPointerAddressSpace());
                }
                if (store_ty->getPointerAddressSpace() != AddressSpace::Tracked)
                    store_val = builder.CreateAddrSpaceCast(store_val, pass.T_prjlvalue);
                newstore = builder.CreateStore(store_val, slot.slot);
            }
            else {
                newstore = builder.CreateStore(store_val, slot_gep(slot, offset, store_ty, builder));
            }
            // TODO: should we use `store->clone()`, or manually copy any other metadata?
            newstore->setAlignment(store->getAlign());
            // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
            newstore->setOrdering(AtomicOrdering::NotAtomic);
            store->eraseFromParent();
            return;
        }
        else if (isa<AtomicCmpXchgInst>(user) || isa<AtomicRMWInst>(user)) {
            // TODO: Downgrade atomics here potentially
            auto slot_idx = find_slot(offset);
            auto &slot = slots[slot_idx];
            assert(slot.offset <= offset && slot.offset + slot.size >= offset);
            IRBuilder<> builder(user);
            Value *newptr;
            if (slot.isref) {
                assert(slot.offset == offset);
                newptr = slot.slot;
            }
            else {
                Value *Val = isa<AtomicCmpXchgInst>(user) ? cast<AtomicCmpXchgInst>(user)->getNewValOperand() : cast<AtomicRMWInst>(user)->getValOperand();
                newptr = slot_gep(slot, offset, Val->getType(), builder);
            }
            *use = newptr;
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledOperand();
            assert(callee); // makes it clear for clang analyser that `callee` is not NULL
            if (auto intrinsic = dyn_cast<IntrinsicInst>(call)) {
                if (Intrinsic::ID id = intrinsic->getIntrinsicID()) {
                    if (id == Intrinsic::memset) {
                        IRBuilder<> builder(call);
                        auto val_arg = cast<ConstantInt>(call->getArgOperand(1));
                        auto size_arg = cast<ConstantInt>(call->getArgOperand(2));
                        uint8_t val = val_arg->getLimitedValue();
                        uint32_t size = size_arg->getLimitedValue();
                        for (unsigned idx = find_slot(offset); idx < nslots; idx++) {
                            auto &slot = slots[idx];
                            if (slot.offset + slot.size <= offset || slot.offset >= offset + size)
                                break;
                            if (slot.isref) {
                                assert(slot.offset >= offset &&
                                       slot.offset + slot.size <= offset + size);
                                Constant *ptr;
                                if (val == 0) {
                                    ptr = Constant::getNullValue(pass.T_prjlvalue);
                                }
                                else {
                                    uint64_t intval;
                                    memset(&intval, val, 8);
                                    Constant *val = ConstantInt::get(pass.DL->getIntPtrType(builder.getContext(), pass.T_prjlvalue->getAddressSpace()), intval);
                                    val = ConstantExpr::getIntToPtr(val, JuliaType::get_pjlvalue_ty(builder.getContext()));
                                    ptr = ConstantExpr::getAddrSpaceCast(val, pass.T_prjlvalue);
                                }
                                StoreInst *store = builder.CreateAlignedStore(ptr, slot.slot, Align(sizeof(void*)));
                                store->setOrdering(AtomicOrdering::NotAtomic);
                                continue;
                            }
                            Value *ptr_slot = slot.slot;
                            if (offset > slot.offset)
                                ptr_slot = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), slot.slot,
                                                                          offset - slot.offset);
                            auto sub_size = std::min(slot.offset + slot.size, offset + size) -
                                std::max(offset, slot.offset);
                            // TODO: alignment computation
                            builder.CreateMemSet(ptr_slot, val_arg, sub_size, MaybeAlign(0));
                        }
                        call->eraseFromParent();
                        return;
                    }
                    call->eraseFromParent();
                    return;
                }
            }
            if (pass.typeof_func == callee) {
                ++RemovedTypeofs;
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            if (pass.write_barrier_func == callee) {
                ++RemovedWriteBarriers;
                call->eraseFromParent();
                return;
            }
            if (pass.gc_preserve_begin_func == callee) {
                SmallVector<Value*,8> operands;
                for (auto &arg: call->args()) {
                    if (arg.get() == orig_i || isa<Constant>(arg.get()))
                        continue;
                    operands.push_back(arg.get());
                }
                IRBuilder<> builder(call);
                for (auto &slot: slots) {
                    if (!slot.isref)
                        continue;
                    LoadInst *ref = builder.CreateAlignedLoad(pass.T_prjlvalue, slot.slot, Align(sizeof(void*)));
                    // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
                    ref->setOrdering(AtomicOrdering::NotAtomic);
                    operands.push_back(ref);
                }
#ifndef __clang_analyzer__
                // FIXME: SA finds "Called C++ object pointer is null" inside the LLVM code.
                auto new_call = builder.CreateCall(pass.gc_preserve_begin_func, operands);
                new_call->takeName(call);
                call->replaceAllUsesWith(new_call);
#endif
                call->eraseFromParent();
                return;
            }
            // remove from operand bundle
            assert(call->isBundleOperand(use->getOperandNo()));
            assert(call->getOperandBundleForOperand(use->getOperandNo()).getTagName() ==
                   "jl_roots");
            SmallVector<OperandBundleDef,2> bundles;
            call->getOperandBundlesAsDefs(bundles);
            for (auto &bundle: bundles) {
                if (bundle.getTag() != "jl_roots")
                    continue;
                SmallVector<Value*, 0> operands;
                for (auto op: bundle.inputs()) {
                    if (op == orig_i || isa<Constant>(op))
                        continue;
                    operands.push_back(op);
                }
                IRBuilder<> builder(call);
                for (auto &slot: slots) {
                    if (!slot.isref)
                        continue;
                    LoadInst *ref = builder.CreateAlignedLoad(pass.T_prjlvalue, slot.slot, Align(sizeof(void*)));
                    // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
                    ref->setOrdering(AtomicOrdering::NotAtomic);
                    operands.push_back(ref);
                }
                bundle = OperandBundleDef("jl_roots", std::move(operands));
                break;
            }
#if JL_LLVM_VERSION >= 200000
            auto new_call = CallInst::Create(call, bundles, call->getIterator());
#else
            auto new_call = CallInst::Create(call, bundles, call);
#endif
            new_call->takeName(call);
            call->replaceAllUsesWith(new_call);
            call->eraseFromParent();
            return;
        }
        else if (isa<AddrSpaceCastInst>(user) || isa<BitCastInst>(user)) {
            push_frame(user, offset);
        }
        else if (auto gep = dyn_cast<GetElementPtrInst>(user)) {
            APInt apoffset(sizeof(void*) * 8, offset, true);
            gep->accumulateConstantOffset(*pass.DL, apoffset);
            push_frame(gep, apoffset.getLimitedValue());
        }
        else {
            abort();
        }
    };

    while (true) {
        replace_inst(&*cur.orig_i->use_begin());
        while (cur.orig_i->use_empty()) {
            finish_cur();
            if (replace_stack.empty())
                goto cleanup;
            cur = replace_stack.back();
            replace_stack.pop_back();
        }
    }
cleanup:
    for (auto &slot: slots) {
        if (!slot.isref)
            continue;
        PromoteMemToReg({slot.slot}, getDomTree());
    }
}

bool AllocOpt::doInitialization(Module &M)
{
    initAll(M);
    if (!alloc_obj_func)
        return false;

    DL = &M.getDataLayout();

#if JL_LLVM_VERSION >= 200000
    lifetime_start = Intrinsic::getOrInsertDeclaration(&M, Intrinsic::lifetime_start, { PointerType::get(M.getContext(), DL->getAllocaAddrSpace()) });
#else
    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { PointerType::get(M.getContext(), DL->getAllocaAddrSpace()) });
#endif
#if JL_LLVM_VERSION >= 200000
    lifetime_end = Intrinsic::getOrInsertDeclaration(&M, Intrinsic::lifetime_end, { PointerType::get(M.getContext(), DL->getAllocaAddrSpace()) });
#else
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { PointerType::get(M.getContext(), DL->getAllocaAddrSpace()) });
#endif

    return true;
}

bool AllocOpt::runOnFunction(Function &F, function_ref<DominatorTree&()> GetDT)
{
    if (!alloc_obj_func) {
        LLVM_DEBUG(dbgs() << "AllocOpt: no alloc_obj function found, skipping pass\n");
        return false;
    }
    Optimizer optimizer(F, *this, std::move(GetDT));
    optimizer.initialize();
    optimizer.optimizeAll();
    bool modified = optimizer.finalize();
#ifdef JL_VERIFY_PASSES
    assert(!verifyLLVMIR(F));
#endif
    return modified;
}


} // anonymous namespace
PreservedAnalyses AllocOptPass::run(Function &F, FunctionAnalysisManager &AM) {
    AllocOpt opt;
    bool modified = opt.doInitialization(*F.getParent());
    if (opt.runOnFunction(F, [&]()->DominatorTree &{ return AM.getResult<DominatorTreeAnalysis>(F); })) {
        modified = true;
    }
    if (modified) {
        auto preserved = PreservedAnalyses::allInSet<CFGAnalyses>();
        preserved.preserve<DominatorTreeAnalysis>();
        return preserved;
    } else {
        return PreservedAnalyses::all();
    }
}
