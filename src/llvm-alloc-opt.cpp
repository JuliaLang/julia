// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "alloc_opt"
#undef DEBUG
#include "llvm-version.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
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

private:
    LLVMContext *ctx;

    const DataLayout *DL;

    Function *alloc_obj;
    Function *ptr_from_objref;
    Function *lifetime_start;
    Function *lifetime_end;
    Function *gc_preserve_begin;

    Type *T_int8;
    Type *T_int32;
    Type *T_int64;
    Type *T_size;
    Type *T_pint8;
    Type *T_prjlvalue;
    Type *T_pjlvalue;
    Type *T_pprjlvalue;

    MDNode *tbaa_tag;

    struct CheckInstFrame {
        Instruction *parent;
        uint64_t offset;
        Instruction::use_iterator use_it;
        Instruction::use_iterator use_end;
    };
    typedef SmallVector<CheckInstFrame, 4> CheckInstStack;
    struct ReplaceUsesFrame {
        Instruction *orig_i;
        Instruction *new_i;
        ReplaceUsesFrame(Instruction *orig_i, Instruction *new_i)
            : orig_i(orig_i),
              new_i(new_i)
        {}
    };
    typedef SmallVector<ReplaceUsesFrame,4> ReplaceUsesStack;

    struct LifetimeMarker {
        LifetimeMarker(AllocOpt &pass)
            : pass(pass),
              first_safepoint{},
              stack{}
        {}
        // insert llvm.lifetime.* calls for `ptr` with size `sz`
        // based on the use of `orig` given in `alloc_uses`.
        void insert(Function &F, Instruction *ptr, Constant *sz, Instruction *orig,
                    const std::set<Instruction*> &alloc_uses,
                    const std::set<CallInst*> &preserves);
    private:
        Instruction *getFirstSafepoint(BasicBlock *bb);
        void insertEnd(Instruction *ptr, Constant *sz, Instruction *insert);
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
        AllocOpt &pass;
        std::map<BasicBlock*,Instruction*> first_safepoint;
        SmallVector<Frame,4> stack;
    };

    bool doInitialization(Module &m) override;
    bool runOnFunction(Function &F) override;
    bool checkInst(Instruction *I, CheckInstStack &stack, std::set<Instruction*> &uses,
                   std::set<CallInst*> &preserves, bool &ignore_tag);
    void replaceUsesWith(Instruction *orig_i, Instruction *new_i, ReplaceUsesStack &stack);
    void replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID, Instruction *orig_i,
                                 Instruction *new_i);
    bool isSafepoint(Instruction *inst);
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        FunctionPass::getAnalysisUsage(AU);
        AU.addRequired<DominatorTreeWrapperPass>();
        AU.addPreserved<DominatorTreeWrapperPass>();
        AU.setPreservesCFG();
    }
};

Instruction *AllocOpt::LifetimeMarker::getFirstSafepoint(BasicBlock *bb)
{
    auto it = first_safepoint.find(bb);
    if (it != first_safepoint.end())
        return it->second;
    Instruction *first = nullptr;
    for (auto &I: *bb) {
        if (pass.isSafepoint(&I)) {
            first = &I;
            break;
        }
    }
    first_safepoint[bb] = first;
    return first;
}

void AllocOpt::LifetimeMarker::insertEnd(Instruction *ptr, Constant *sz, Instruction *insert)
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

void AllocOpt::LifetimeMarker::insert(Function &F, Instruction *ptr, Constant *sz,
                                      Instruction *orig, const std::set<Instruction*> &alloc_uses,
                                      const std::set<CallInst*> &preserves)
{
    CallInst::Create(pass.lifetime_start, {sz, ptr}, "", orig);
    BasicBlock *def_bb = orig->getParent();
    std::set<BasicBlock*> bbs{def_bb};
    auto &DT = pass.getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    // Collect all BB where the allocation is live
    for (auto use: alloc_uses) {
        auto bb = use->getParent();
        if (!bbs.insert(bb).second)
            continue;
        assert(stack.empty());
        Frame cur{bb};
        while (true) {
            assert(cur.p_cur != cur.p_end);
            auto pred = *cur.p_cur;
            ++cur.p_cur;
            if (bbs.insert(pred).second) {
                if (cur.p_cur != cur.p_end)
                    stack.push_back(cur);
                cur = Frame(pred);
            }
            if (cur.p_cur == cur.p_end) {
                if (stack.empty())
                    break;
                cur = stack.back();
                stack.pop_back();
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
                insertEnd(ptr, sz, &*bb->getFirstInsertionPt());
                continue;
            }
            else if (auto insert = getFirstSafepoint(bb)) {
                insertEnd(ptr, sz, insert);
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
                if (pass.isSafepoint(insert)) {
                    insertEnd(ptr, sz, insert);
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

bool AllocOpt::doInitialization(Module &M)
{
    ctx = &M.getContext();
    DL = &M.getDataLayout();

    alloc_obj = M.getFunction("julia.gc_alloc_obj");
    if (!alloc_obj)
        return false;

    ptr_from_objref = M.getFunction("julia.pointer_from_objref");
    gc_preserve_begin = M.getFunction("llvm.julia.gc_preserve_begin");

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

bool AllocOpt::checkInst(Instruction *I, CheckInstStack &stack, std::set<Instruction*> &uses,
                         std::set<CallInst*> &preserves, bool &ignore_tag)
{
    uses.clear();
    if (I->use_empty())
        return true;
    CheckInstFrame cur{I, 0, I->use_begin(), I->use_end()};
    stack.clear();

    // Recursion
    auto push_inst = [&] (Instruction *inst) {
        if (cur.use_it != cur.use_end)
            stack.push_back(cur);
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
            auto callee = call->getCalledFunction();
            if (auto II = dyn_cast<IntrinsicInst>(call)) {
                if (II->getIntrinsicID()) {
                    return true;
                }
                if (gc_preserve_begin && gc_preserve_begin == callee) {
                    for (auto user: call->users())
                        uses.insert(cast<Instruction>(user));
                    preserves.insert(call);
                    return true;
                }
            }
            if (ptr_from_objref && ptr_from_objref == callee)
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
            if (ignore_tag && (!gep->accumulateConstantOffset(*DL, apoffset) ||
                               apoffset.isNegative()))
                ignore_tag = false;
            push_inst(inst);
            cur.offset = apoffset.getLimitedValue();
            // Check overflow
            if (cur.offset == UINT64_MAX)
                ignore_tag = false;
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
        uses.insert(inst);
        if (cur.use_it == cur.use_end) {
            if (stack.empty())
                return true;
            cur = stack.back();
            stack.pop_back();
        }
    }
}

void AllocOpt::replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID,
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
    newCall->setAttributes(AttributeList::get(*ctx, old_attrs.getFnAttributes(),
                                              old_attrs.getRetAttributes(), {}));
#else
    AttributeSet attr;
    attr = attr.addAttributes(*ctx, AttributeSet::ReturnIndex, old_attrs.getRetAttributes())
        .addAttributes(*ctx, AttributeSet::FunctionIndex, old_attrs.getFnAttributes());
    newCall->setAttributes(attr);
#endif
    newCall->setDebugLoc(call->getDebugLoc());
    call->replaceAllUsesWith(newCall);
    call->eraseFromParent();
}

// This function needs to handle all cases `AllocOpt::checkInst` can handle.
// This function should not erase any safepoint so that the lifetime marker can find and cache
// all the original safepoints.
void AllocOpt::replaceUsesWith(Instruction *orig_inst, Instruction *new_inst,
                               ReplaceUsesStack &stack)
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
    assert(stack.empty());
    ReplaceUsesFrame cur{orig_inst, new_inst};
    auto finish_cur = [&] () {
        assert(cur.orig_i->user_empty());
        if (cur.orig_i != orig_inst) {
            cur.orig_i->eraseFromParent();
        }
    };
    auto push_frame = [&] (Instruction *orig_i, Instruction *new_i) {
        if (simple_replace(orig_i, new_i))
            return;
        stack.push_back(cur);
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
            if (ptr_from_objref && ptr_from_objref == call->getCalledFunction()) {
                call->replaceAllUsesWith(new_i);
                call->eraseFromParent();
                return;
            }
            if (auto intrinsic = dyn_cast<IntrinsicInst>(call)) {
                if (Intrinsic::ID ID = intrinsic->getIntrinsicID()) {
                    replaceIntrinsicUseWith(intrinsic, ID, orig_i, new_i);
                    return;
                }
            }
            // remove from operand bundle or arguments for gc_perserve_begin
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
            if (stack.empty())
                return;
            cur = stack.back();
            stack.pop_back();
        }
    }
}

bool AllocOpt::isSafepoint(Instruction *inst)
{
    auto call = dyn_cast<CallInst>(inst);
    if (!call)
        return false;
    if (isa<IntrinsicInst>(call))
        return false;
    if (auto callee = call->getCalledFunction()) {
        // Known functions emitted in codegen that are not safepoints
        if (callee == ptr_from_objref || callee->getName() == "memcmp") {
            return false;
        }
    }
    return true;
}

bool AllocOpt::runOnFunction(Function &F)
{
    if (!alloc_obj)
        return false;
    SmallVector<std::pair<CallInst*,size_t>,6> allocs;
    for (auto &bb: F) {
        for (auto &I: bb) {
            auto call = dyn_cast<CallInst>(&I);
            if (!call)
                continue;
            auto callee = call->getCalledFunction();
            if (!callee)
                continue;
            size_t sz;
            if (callee == alloc_obj) {
                assert(call->getNumArgOperands() == 3);
                sz = (size_t)cast<ConstantInt>(call->getArgOperand(1))->getZExtValue();
            }
            else {
                continue;
            }
            if (sz < IntegerType::MAX_INT_BITS / 8 && sz < INT32_MAX) {
                allocs.push_back(std::make_pair(call, sz));
            }
        }
    }

    auto &entry = F.getEntryBlock();
    CheckInstStack check_stack;
    ReplaceUsesStack replace_stack;
    std::set<Instruction*> alloc_uses;
    std::set<CallInst*> preserves;
    LifetimeMarker lifetime(*this);
    for (auto &it: allocs) {
        bool ignore_tag = true;
        auto orig = it.first;
        size_t &sz = it.second;
        preserves.clear();
        if (!checkInst(orig, check_stack, alloc_uses, preserves, ignore_tag)) {
            sz = UINT32_MAX;
            continue;
        }
        // The allocation does not escape or get used in a phi node so none of the derived
        // SSA from it are live when we run the allocation again.
        // It is now safe to promote the allocation to an entry block alloca.
        size_t align = 1;
        // TODO make codegen handling of alignment consistent and pass that as a parameter
        // to the allocation function directly.
        if (!ignore_tag) {
            align = sz <= 8 ? 8 : JL_SMALL_BYTE_ALIGNMENT;
            sz += align;
        }
        else if (sz > 1) {
            align = llvm::MinAlign(JL_SMALL_BYTE_ALIGNMENT, llvm::NextPowerOf2(sz));
        }
        // No debug info for prolog instructions
        IRBuilder<> prolog_builder(&entry.front());
        AllocaInst *buff;
        Instruction *ptr;
        if (sz == 0) {
            buff = prolog_builder.CreateAlloca(T_int8, ConstantInt::get(T_int64, 0));
            ptr = buff;
        }
        else {
            buff = prolog_builder.CreateAlloca(Type::getIntNTy(*ctx, sz * 8));
            buff->setAlignment(align);
            ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, T_pint8));
        }
        lifetime.insert(F, ptr, ConstantInt::get(T_int64, sz), orig, alloc_uses, preserves);
        // Someone might be reading the tag, initialize it.
        if (!ignore_tag) {
            ptr = cast<Instruction>(prolog_builder.CreateConstGEP1_32(T_int8, ptr, align));
            auto casti = prolog_builder.CreateBitCast(ptr, T_pprjlvalue);
            auto tagaddr = prolog_builder.CreateGEP(T_prjlvalue, casti,
                                                    ConstantInt::get(T_size, -1));
            // Store should be created at the callsite and not in the prolog
            auto store = new StoreInst(orig->getArgOperand(2), tagaddr, orig);
            store->setMetadata(LLVMContext::MD_tbaa, tbaa_tag);
            store->setDebugLoc(orig->getDebugLoc());
        }
        auto casti = cast<Instruction>(prolog_builder.CreateBitCast(ptr, T_pjlvalue));
        casti->takeName(orig);
        replaceUsesWith(orig, cast<Instruction>(casti), replace_stack);
    }
    for (auto it: allocs) {
        if (it.second == UINT32_MAX)
            continue;
        it.first->eraseFromParent();
    }
    return true;
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
