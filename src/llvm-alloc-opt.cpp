// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "alloc_opt"
#undef DEBUG
#include "llvm-version.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"

#include <map>
#include <set>

#include "julia_assert.h"

using namespace llvm;

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);

namespace {

static void copyMetadata(Instruction *dest, const Instruction *src)
{
#if JL_LLVM_VERSION < 40000
    if (!src->hasMetadata())
        return;
    SmallVector<std::pair<unsigned,MDNode*>,4> TheMDs;
    src->getAllMetadataOtherThanDebugLoc(TheMDs);
    for (const auto &MD : TheMDs)
        dest->setMetadata(MD.first, MD.second);
    dest->setDebugLoc(src->getDebugLoc());
#else
    dest->copyMetadata(*src);
#endif
}

static bool isBundleOperand(CallInst *call, unsigned idx)
{
#if JL_LLVM_VERSION < 40000
    return call->hasOperandBundles() && idx >= call->getBundleOperandsStartIndex() &&
        idx < call->getBundleOperandsEndIndex();
#else
    return call->isBundleOperand(idx);
#endif
}

static void removeGCPreserve(CallInst *call, Instruction *val)
{
    auto replace = ConstantPointerNull::get(cast<PointerType>(val->getType()));
    call->replaceUsesOfWith(val, replace);
    for (auto &arg: call->arg_operands()) {
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

struct AllocOpt : public FunctionPass {
    static char ID;
    AllocOpt()
        : FunctionPass(ID)
    {
        llvm::initializeDominatorTreeWrapperPassPass(*PassRegistry::getPassRegistry());
    }

    LLVMContext *ctx;

    const DataLayout *DL;

    Function *alloc_obj;
    Function *ptr_from_objref;
    Function *lifetime_start;
    Function *lifetime_end;
    Function *gc_preserve_begin;
    Function *typeof_func;

    Type *T_int8;
    Type *T_int32;
    Type *T_int64;
    Type *T_size;
    Type *T_pint8;
    Type *T_prjlvalue;
    Type *T_pjlvalue;
    Type *T_pprjlvalue;

    MDNode *tbaa_tag;

private:
    bool doInitialization(Module &m) override;
    bool runOnFunction(Function &F) override;
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        FunctionPass::getAnalysisUsage(AU);
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addPreserved<DominatorTreeWrapperPass>();
        AU.setPreservesCFG();
    }
};

struct Optimizer {
    Optimizer(Function &F, AllocOpt &pass)
        : F(F),
          pass(pass)
    {}

    void initialize();
    void optimizeAll();
    bool finalize();
private:
    bool isSafepoint(Instruction *inst);
    Instruction *getFirstSafepoint(BasicBlock *bb);
    ssize_t getGCAllocSize(Instruction *I);

    void insertLifetimeEnd(Instruction *ptr, Constant *sz, Instruction *insert);
    // insert llvm.lifetime.* calls for `ptr` with size `sz`
    // based on the use of `orig` given in `alloc_uses`.
    void insertLifetime(Instruction *ptr, Constant *sz, Instruction *orig);

    bool checkInst(Instruction *I);

    void replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID,
                                 Instruction *orig_i, Instruction *new_i);
    void replaceUsesWith(Instruction *orig_inst, Instruction *new_inst, Value *tag);

    Function &F;
    AllocOpt &pass;
    DominatorTree *_DT = nullptr;

    DominatorTree &getDomTree()
    {
        if (!_DT)
            _DT = &pass.getAnalysis<DominatorTreeWrapperPass>().getDomTree();
        return *_DT;
    }

    struct CheckInst {
        struct Frame {
            Instruction *parent;
            uint64_t offset;
            Instruction::use_iterator use_it;
            Instruction::use_iterator use_end;
        };
        typedef SmallVector<Frame,4> Stack;
    };
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
            Instruction *new_i;
            Frame(Instruction *orig_i, Instruction *new_i)
                : orig_i(orig_i),
                  new_i(new_i)
            {}
        };
        typedef SmallVector<Frame,4> Stack;
    };

    SetVector<std::pair<CallInst*,size_t>> worklist;
    SmallVector<CallInst*,6> removed;
    SmallSet<Instruction*,16> alloc_uses;
    SmallSet<CallInst*,4> preserves;
    CheckInst::Stack check_stack;
    Lifetime::Stack lifetime_stack;
    ReplaceUses::Stack replace_stack;
    std::map<BasicBlock*,Instruction*> first_safepoint;
};

void Optimizer::initialize()
{
    for (auto &bb: F) {
        for (auto &I: bb) {
            ssize_t sz = getGCAllocSize(&I);
            if (sz != -1) {
                worklist.insert(std::make_pair(cast<CallInst>(&I), sz));
            }
        }
    }
}

void Optimizer::optimizeAll()
{
    while (!worklist.empty()) {
        auto item = worklist.pop_back_val();
        auto orig = item.first;
        size_t sz = item.second;
        if (!checkInst(orig))
            continue;
        removed.push_back(orig);
        // The allocation does not escape or get used in a phi node so none of the derived
        // SSA from it are live when we run the allocation again.
        // It is now safe to promote the allocation to an entry block alloca.
        size_t align = 1;
        // TODO make codegen handling of alignment consistent and pass that as a parameter
        // to the allocation function directly.
        if (sz > 1)
            align = MinAlign(JL_SMALL_BYTE_ALIGNMENT, NextPowerOf2(sz));
        // No debug info for prolog instructions
        IRBuilder<> prolog_builder(&F.getEntryBlock().front());
        AllocaInst *buff;
        Instruction *ptr;
        if (sz == 0) {
            buff = prolog_builder.CreateAlloca(pass.T_int8, ConstantInt::get(pass.T_int64, 0));
            ptr = buff;
        }
        else {
            buff = prolog_builder.CreateAlloca(Type::getIntNTy(*pass.ctx, sz * 8));
            buff->setAlignment(align);
            ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, pass.T_pint8));
        }
        insertLifetime(ptr, ConstantInt::get(pass.T_int64, sz), orig);
        auto tag = orig->getArgOperand(2);
        auto casti = cast<Instruction>(prolog_builder.CreateBitCast(ptr, pass.T_pjlvalue));
        casti->takeName(orig);
        replaceUsesWith(orig, casti, tag);
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
        if (callee == pass.ptr_from_objref || callee->getName() == "memcmp") {
            return false;
        }
    }
    return true;
}

Instruction *Optimizer::getFirstSafepoint(BasicBlock *bb)
{
    auto it = first_safepoint.find(bb);
    if (it != first_safepoint.end())
        return it->second;
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
    if (call->getCalledValue() != pass.alloc_obj)
        return -1;
    assert(call->getNumArgOperands() == 3);
    size_t sz = (size_t)cast<ConstantInt>(call->getArgOperand(1))->getZExtValue();
    if (sz < IntegerType::MAX_INT_BITS / 8 && sz < INT32_MAX)
        return sz;
    return -1;
}

bool Optimizer::checkInst(Instruction *I)
{
    preserves.clear();
    alloc_uses.clear();
    if (I->use_empty())
        return true;
    CheckInst::Frame cur{I, 0, I->use_begin(), I->use_end()};
    check_stack.clear();

    // Recursion
    auto push_inst = [&] (Instruction *inst) {
        if (cur.use_it != cur.use_end)
            check_stack.push_back(cur);
        cur.parent = inst;
        cur.use_it = inst->use_begin();
        cur.use_end = inst->use_end();
    };

    auto check_inst = [&] (Instruction *inst, Use *use) {
        if (isa<LoadInst>(inst))
            return true;
        if (auto call = dyn_cast<CallInst>(inst)) {
            // TODO handle `memcmp`
            // None of the intrinsics should care if the memory is stack or heap allocated.
            auto callee = call->getCalledValue();
            if (auto II = dyn_cast<IntrinsicInst>(call)) {
                if (II->getIntrinsicID()) {
                    return true;
                }
                if (pass.gc_preserve_begin == callee) {
                    for (auto user: call->users())
                        alloc_uses.insert(cast<Instruction>(user));
                    preserves.insert(call);
                    return true;
                }
            }
            if (pass.ptr_from_objref == callee || pass.typeof_func == callee)
                return true;
            auto opno = use->getOperandNo();
            // Uses in `jl_roots` operand bundle are not counted as escaping, everything else is.
            if (!isBundleOperand(call, opno))
                return false;
            return call->getOperandBundleForOperand(opno).getTagName() == "jl_roots";
        }
        if (auto store = dyn_cast<StoreInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != StoreInst::getPointerOperandIndex())
                return false;
            auto storev = store->getValueOperand();
            // There's GC root in this object.
            if (auto ptrtype = dyn_cast<PointerType>(storev->getType())) {
                if (ptrtype->getAddressSpace() == AddressSpace::Tracked) {
                    return false;
                }
            }
            return true;
        }
        if (isa<AddrSpaceCastInst>(inst) || isa<BitCastInst>(inst)) {
            push_inst(inst);
            return true;
        }
        if (auto gep = dyn_cast<GetElementPtrInst>(inst)) {
            APInt apoffset(sizeof(void*) * 8, cur.offset, true);
            uint64_t next_offset;
            if (!gep->accumulateConstantOffset(*pass.DL, apoffset) || apoffset.isNegative()) {
                next_offset = UINT64_MAX;
            }
            else {
                next_offset = apoffset.getLimitedValue();
            }
            push_inst(inst);
            cur.offset = next_offset;
            return true;
        }
        return false;
    };

    while (true) {
        assert(cur.use_it != cur.use_end);
        auto use = &*cur.use_it;
        auto inst = dyn_cast<Instruction>(use->getUser());
        ++cur.use_it;
        if (!inst)
            return false;
        if (!check_inst(inst, use))
            return false;
        alloc_uses.insert(inst);
        if (cur.use_it == cur.use_end) {
            if (check_stack.empty())
                return true;
            cur = check_stack.back();
            check_stack.pop_back();
        }
    }
}

void Optimizer::insertLifetimeEnd(Instruction *ptr, Constant *sz, Instruction *insert)
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
    CallInst::Create(pass.lifetime_end, {sz, ptr}, "", insert);
}

void Optimizer::insertLifetime(Instruction *ptr, Constant *sz, Instruction *orig)
{
    CallInst::Create(pass.lifetime_start, {sz, ptr}, "", orig);
    BasicBlock *def_bb = orig->getParent();
    std::set<BasicBlock*> bbs{def_bb};
    auto &DT = getDomTree();
    // Collect all BB where the allocation is live
    for (auto use: alloc_uses) {
        auto bb = use->getParent();
        if (!bbs.insert(bb).second)
            continue;
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
    // Record extra BBs that contain invisible uses.
    SmallSet<BasicBlock*, 8> extra_use;
    SmallVector<DomTreeNodeBase<BasicBlock>*, 8> dominated;
    for (auto preserve: preserves) {
        for (auto RN = DT.getNode(preserve->getParent()); RN;
             RN = dominated.empty() ? nullptr : dominated.pop_back_val()) {
            for (auto N: *RN) {
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
                dominated.push_back(N);
            }
        }
        assert(dominated.empty());
    }
    // For each BB, find the first instruction(s) where the allocation is possibly dead.
    // If all successors are live, then there isn't one.
    // If all successors are dead, then it's the first instruction after the last use
    // within the BB.
    // If some successors are live and others are dead, it's the first instruction in
    // the successors that are dead.
    std::vector<Instruction*> first_dead;
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
                if (alloc_uses.count(&*it)) {
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
    auto nargs = call->getNumArgOperands();
    SmallVector<Value*, 8> args(nargs);
    SmallVector<Type*, 8> argTys(nargs);
    for (unsigned i = 0; i < nargs; i++) {
        auto arg = call->getArgOperand(i);
        args[i] = arg == orig_i ? new_i : arg;
        argTys[i] = args[i]->getType();
    }

    // Accumulate an array of overloaded types for the given intrinsic
    SmallVector<Type*, 4> overloadTys;
    {
        SmallVector<Intrinsic::IITDescriptor, 8> Table;
        getIntrinsicInfoTableEntries(ID, Table);
        ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
        auto oldfType = call->getFunctionType();
        bool res = Intrinsic::matchIntrinsicType(oldfType->getReturnType(), TableRef, overloadTys);
        assert(!res);
        for (auto Ty : argTys) {
            res = Intrinsic::matchIntrinsicType(Ty, TableRef, overloadTys);
            assert(!res);
        }
        res = Intrinsic::matchIntrinsicVarArg(oldfType->isVarArg(), TableRef);
        assert(!res);
        (void)res;
    }
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
    newF->setCallingConv(call->getCallingConv());
    auto newCall = CallInst::Create(newF, args, "", call);
    newCall->setTailCallKind(call->getTailCallKind());
    auto old_attrs = call->getAttributes();
#if JL_LLVM_VERSION >= 50000
    newCall->setAttributes(AttributeList::get(*pass.ctx, old_attrs.getFnAttributes(),
                                              old_attrs.getRetAttributes(), {}));
#else
    AttributeSet attr;
    attr = attr.addAttributes(*pass.ctx, AttributeSet::ReturnIndex, old_attrs.getRetAttributes())
        .addAttributes(*pass.ctx, AttributeSet::FunctionIndex, old_attrs.getFnAttributes());
    newCall->setAttributes(attr);
#endif
    newCall->setDebugLoc(call->getDebugLoc());
    call->replaceAllUsesWith(newCall);
    call->eraseFromParent();
}

// This function needs to handle all cases `checkInst` can handle.
// This function should not erase any safepoint so that the lifetime marker can find and cache
// all the original safepoints.
void Optimizer::replaceUsesWith(Instruction *orig_inst, Instruction *new_inst, Value *tag)
{
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
    if (simple_replace(orig_inst, new_inst))
        return;
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
        if (isa<LoadInst>(user) || isa<StoreInst>(user)) {
            user->replaceUsesOfWith(orig_i, new_i);
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledValue();
            if (pass.ptr_from_objref == callee) {
                call->replaceAllUsesWith(new_i);
                call->eraseFromParent();
                return;
            }
            if (pass.typeof_func == callee) {
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            // Also remove the preserve intrinsics so that it can be better optimized.
            if (pass.gc_preserve_begin == callee) {
                removeGCPreserve(call, orig_i);
                return;
            }
            if (auto intrinsic = dyn_cast<IntrinsicInst>(call)) {
                if (Intrinsic::ID ID = intrinsic->getIntrinsicID()) {
                    replaceIntrinsicUseWith(intrinsic, ID, orig_i, new_i);
                    return;
                }
            }
            // remove from operand bundle
            Type *orig_t = orig_i->getType();
            user->replaceUsesOfWith(orig_i, ConstantPointerNull::get(cast<PointerType>(orig_t)));
        }
        else if (isa<AddrSpaceCastInst>(user) || isa<BitCastInst>(user)) {
            auto cast_t = PointerType::get(cast<PointerType>(user->getType())->getElementType(),
                                           0);
            auto replace_i = new_i;
            Type *new_t = new_i->getType();
            if (cast_t != new_t) {
                replace_i = new BitCastInst(replace_i, cast_t, "", user);
                replace_i->setDebugLoc(user->getDebugLoc());
                replace_i->takeName(user);
            }
            push_frame(user, replace_i);
        }
        else if (auto gep = dyn_cast<GetElementPtrInst>(user)) {
            SmallVector<Value *, 4> IdxOperands(gep->idx_begin(), gep->idx_end());
            auto new_gep = GetElementPtrInst::Create(gep->getSourceElementType(),
                                                     new_i, IdxOperands,
                                                     gep->getName(), gep);
            new_gep->setIsInBounds(gep->isInBounds());
            new_gep->takeName(gep);
            copyMetadata(new_gep, gep);
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

bool AllocOpt::doInitialization(Module &M)
{
    ctx = &M.getContext();
    DL = &M.getDataLayout();

    alloc_obj = M.getFunction("julia.gc_alloc_obj");
    if (!alloc_obj)
        return false;

    ptr_from_objref = M.getFunction("julia.pointer_from_objref");
    gc_preserve_begin = M.getFunction("llvm.julia.gc_preserve_begin");
    typeof_func = M.getFunction("julia.typeof");

    T_prjlvalue = alloc_obj->getReturnType();
    T_pjlvalue = PointerType::get(cast<PointerType>(T_prjlvalue)->getElementType(), 0);
    T_pprjlvalue = PointerType::get(T_prjlvalue, 0);

    T_int8 = Type::getInt8Ty(*ctx);
    T_int32 = Type::getInt32Ty(*ctx);
    T_int64 = Type::getInt64Ty(*ctx);
    T_size = sizeof(void*) == 8 ? T_int64 : T_int32;
    T_pint8 = PointerType::get(T_int8, 0);

#if JL_LLVM_VERSION >= 50000
    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { T_pint8 });
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { T_pint8 });
#else
    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start);
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end);
#endif

    MDNode *tbaa_data;
    MDNode *tbaa_data_scalar;
    std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child("jtbaa_data");
    tbaa_tag = tbaa_make_child("jtbaa_tag", tbaa_data_scalar).first;

    return true;
}

bool AllocOpt::runOnFunction(Function &F)
{
    if (!alloc_obj)
        return false;
    Optimizer optimizer(F, *this);
    optimizer.initialize();
    optimizer.optimizeAll();
    return optimizer.finalize();
}

char AllocOpt::ID = 0;
static RegisterPass<AllocOpt> X("AllocOpt", "Promote heap allocation to stack",
                                false /* Only looks at CFG */,
                                false /* Analysis Pass */);

}

Pass *createAllocOptPass()
{
    return new AllocOpt();
}
