// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "alloc_opt"
#undef DEBUG
#include "llvm-version.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Pass.h>
#include <llvm/Support/Debug.h>
#include <llvm/Transforms/Utils/PromoteMemToReg.h>

#if JL_LLVM_VERSION >= 100000
#include <llvm/InitializePasses.h>
#endif

#include "codegen_shared.h"
#include "julia.h"
#include "julia_internal.h"
#include "llvm-pass-helpers.h"

#include <map>
#include <set>

#include "julia_assert.h"

using namespace llvm;

namespace {

static void removeGCPreserve(CallInst *call, Instruction *val)
{
    auto replace = Constant::getNullValue(val->getType());
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

static bool hasObjref(Type *ty)
{
    if (auto ptrty = dyn_cast<PointerType>(ty))
        return ptrty->getAddressSpace() == AddressSpace::Tracked;
#if JL_LLVM_VERSION >= 110000
    if (isa<ArrayType>(ty) || isa<VectorType>(ty))
        return GetElementPtrInst::getTypeAtIndex(ty, (uint64_t)0);
#else
    if (auto seqty = dyn_cast<SequentialType>(ty))
        return hasObjref(seqty->getElementType());
#endif
    if (auto structty = dyn_cast<StructType>(ty)) {
        for (auto elty: structty->elements()) {
            if (hasObjref(elty)) {
                return true;
            }
        }
    }
    return false;
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

/**
 * TODO:
 * * Return twice
 * * Handle phi node.
 * * Look through `pointer_from_objref`.
 * * Handle jl_box*
 */

struct AllocOpt : public FunctionPass, public JuliaPassContext {
    static char ID;
    AllocOpt()
        : FunctionPass(ID)
    {
        llvm::initializeDominatorTreeWrapperPassPass(*PassRegistry::getPassRegistry());
    }

    const DataLayout *DL;

    Function *lifetime_start;
    Function *lifetime_end;

    Type *T_int64;

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
    void pushInstruction(Instruction *I);

    void insertLifetimeEnd(Value *ptr, Constant *sz, Instruction *insert);
    // insert llvm.lifetime.* calls for `ptr` with size `sz` based on the use of `orig`.
    void insertLifetime(Value *ptr, Constant *sz, Instruction *orig);

    void checkInst(Instruction *I);

    void replaceIntrinsicUseWith(IntrinsicInst *call, Intrinsic::ID ID,
                                 Instruction *orig_i, Instruction *new_i);
    void removeAlloc(CallInst *orig_inst);
    void moveToStack(CallInst *orig_inst, size_t sz, bool has_ref);
    void splitOnStack(CallInst *orig_inst);

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
            uint32_t offset;
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

    struct MemOp {
        Instruction *inst;
        unsigned opno;
        uint32_t offset = 0;
        uint32_t size = 0;
        bool isobjref:1;
        bool isaggr:1;
        MemOp(Instruction *inst, unsigned opno)
            : inst(inst),
              opno(opno),
              isobjref(false),
              isaggr(false)
        {}
    };
    struct Field {
        uint32_t size;
        bool hasobjref:1;
        bool hasaggr:1;
        bool multiloc:1;
        bool hasload:1;
        Type *elty;
        SmallVector<MemOp,4> accesses;
        Field(uint32_t size, Type *elty)
            : size(size),
              hasobjref(false),
              hasaggr(false),
              multiloc(false),
              hasload(false),
              elty(elty)
        {
        }
    };
    struct AllocUseInfo {
        SmallSet<Instruction*,16> uses;
        SmallSet<CallInst*,4> preserves;
        std::map<uint32_t,Field> memops;
        // Completely unknown use
        bool escaped:1;
        // Address is leaked to functions that doesn't care where the object is allocated.
        bool addrescaped:1;
        // There are reader of the memory
        bool hasload:1;
        // There are uses in gc_preserve intrinsics or ccall roots
        bool haspreserve:1;
        // There are objects fields being loaded
        bool refload:1;
        // There are objects fields being stored
        bool refstore:1;
        // There are memset call
        bool hasmemset:1;
        // There are store/load/memset on this object with offset or size (or value for memset)
        // that cannot be statically computed.
        // This is a weaker form of `addrescaped` since `hasload` can still be used
        // to see if the memory is actually being used
        bool hasunknownmem:1;
        void reset()
        {
            escaped = false;
            addrescaped = false;
            hasload = false;
            haspreserve = false;
            refload = false;
            refstore = false;
            hasunknownmem = false;
            uses.clear();
            preserves.clear();
            memops.clear();
        }
        void dump();
        bool addMemOp(Instruction *inst, unsigned opno, uint32_t offset, Type *elty,
                      bool isstore, const DataLayout &DL);
        std::pair<const uint32_t,Field> &getField(uint32_t offset, uint32_t size, Type *elty);
        std::map<uint32_t,Field>::iterator findLowerField(uint32_t offset)
        {
            // Find the last field that starts no higher than `offset`.
            auto it = memops.upper_bound(offset);
            if (it != memops.begin())
                return --it;
            return memops.end();
        }
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
        if (use_info.escaped)
            continue;
        if (!use_info.addrescaped && !use_info.hasload && (!use_info.haspreserve ||
                                                           !use_info.refstore)) {
            // No one took the address, no one reads anything and there's no meaningful
            // preserve of fields (either no preserve/ccall or no object reference fields)
            // We can just delete all the uses.
            removeAlloc(orig);
            continue;
        }
        bool has_ref = false;
        bool has_refaggr = false;
        for (auto memop: use_info.memops) {
            auto &field = memop.second;
            if (field.hasobjref) {
                has_ref = true;
                // This can be relaxed a little based on hasload
                if (field.hasaggr || field.multiloc) {
                    has_refaggr = true;
                    break;
                }
            }
        }
        if (!use_info.hasunknownmem && !use_info.addrescaped && !has_refaggr) {
            // No one actually care about the memory layout of this object, split it.
            splitOnStack(orig);
            continue;
        }
        if (has_ref) {
            if (use_info.memops.size() != 1 || has_refaggr ||
                use_info.memops.begin()->second.size != sz) {
                continue;
            }
            // The object only has a single field that's a reference with only one kind of access.
        }
        moveToStack(orig, sz, has_ref);
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
        if (callee == pass.pointer_from_objref_func || callee->getName() == "memcmp") {
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
    if (call->getCalledValue() != pass.alloc_obj_func)
        return -1;
    assert(call->getNumArgOperands() == 3);
    size_t sz = (size_t)cast<ConstantInt>(call->getArgOperand(1))->getZExtValue();
    if (sz < IntegerType::MAX_INT_BITS / 8 && sz < INT32_MAX)
        return sz;
    return -1;
}

std::pair<const uint32_t,Optimizer::Field>&
Optimizer::AllocUseInfo::getField(uint32_t offset, uint32_t size, Type *elty)
{
    auto it = findLowerField(offset);
    auto end = memops.end();
    auto lb = end; // first overlap
    auto ub = end; // last overlap
    if (it != end) {
        // The slot found contains the current location
        if (it->first + it->second.size >= offset + size) {
            if (it->second.elty != elty)
                it->second.elty = nullptr;
            return *it;
        }
        if (it->first + it->second.size > offset) {
            lb = it;
            ub = it;
        }
    }
    else {
        it = memops.begin();
    }
    // Now fine the last slot that overlaps with the current memory location.
    // Also set `lb` if we didn't find any above.
    for (; it != end && it->first < offset + size; ++it) {
        if (lb == end)
            lb = it;
        ub = it;
    }
    // no overlap found just create a new one.
    if (lb == end)
        return *memops.emplace(offset, Field(size, elty)).first;
    // We find overlapping but not containing slot we need to merge slot/create new one
    uint32_t new_offset = std::min(offset, lb->first);
    uint32_t new_addrub = std::max(offset + uint32_t(size), ub->first + ub->second.size);
    uint32_t new_size = new_addrub - new_offset;
    Field field(new_size, nullptr);
    field.multiloc = true;
    ++ub;
    for (it = lb; it != ub; ++it) {
        field.hasobjref |= it->second.hasobjref;
        field.hasload |= it->second.hasload;
        field.hasaggr |= it->second.hasaggr;
        field.accesses.append(it->second.accesses.begin(), it->second.accesses.end());
    }
    memops.erase(lb, ub);
    return *memops.emplace(new_offset, std::move(field)).first;
}

bool Optimizer::AllocUseInfo::addMemOp(Instruction *inst, unsigned opno, uint32_t offset,
                                       Type *elty, bool isstore, const DataLayout &DL)
{
    MemOp memop(inst, opno);
    memop.offset = offset;
    uint64_t size = DL.getTypeStoreSize(elty);
    if (size >= UINT32_MAX - offset)
        return false;
    memop.size = size;
    memop.isaggr = isa<StructType>(elty) || isa<ArrayType>(elty) || isa<VectorType>(elty);
    memop.isobjref = hasObjref(elty);
    auto &field = getField(offset, size, elty);
    if (field.first != offset || field.second.size != size)
        field.second.multiloc = true;
    if (!isstore)
        field.second.hasload = true;
    if (memop.isobjref) {
        if (isstore) {
            refstore = true;
        }
        else {
            refload = true;
        }
        if (memop.isaggr)
            field.second.hasaggr = true;
        field.second.hasobjref = true;
    }
    else if (memop.isaggr) {
        field.second.hasaggr = true;
    }
    field.second.accesses.push_back(memop);
    return true;
}

JL_USED_FUNC void Optimizer::AllocUseInfo::dump()
{
    jl_safe_printf("escaped: %d\n", escaped);
    jl_safe_printf("addrescaped: %d\n", addrescaped);
    jl_safe_printf("hasload: %d\n", hasload);
    jl_safe_printf("haspreserve: %d\n", haspreserve);
    jl_safe_printf("refload: %d\n", refload);
    jl_safe_printf("refstore: %d\n", refstore);
    jl_safe_printf("hasunknownmem: %d\n", hasunknownmem);
    jl_safe_printf("Uses: %d\n", (unsigned)uses.size());
    for (auto inst: uses)
        llvm_dump(inst);
    if (!preserves.empty()) {
        jl_safe_printf("Preserves: %d\n", (unsigned)preserves.size());
        for (auto inst: preserves) {
            llvm_dump(inst);
        }
    }
    if (!memops.empty()) {
        jl_safe_printf("Memops: %d\n", (unsigned)memops.size());
        for (auto &field: memops) {
            jl_safe_printf("  Field %d @ %d\n", field.second.size, field.first);
            jl_safe_printf("    Accesses:\n");
            for (auto memop: field.second.accesses) {
                jl_safe_printf("    ");
                llvm_dump(memop.inst);
            }
        }
    }
}

void Optimizer::checkInst(Instruction *I)
{
    use_info.reset();
    if (I->use_empty())
        return;
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
        if (isa<LoadInst>(inst)) {
            use_info.hasload = true;
            if (cur.offset == UINT32_MAX || !use_info.addMemOp(inst, 0, cur.offset,
                                                               inst->getType(),
                                                               false, *pass.DL))
                use_info.hasunknownmem = true;
            return true;
        }
        if (auto call = dyn_cast<CallInst>(inst)) {
            // TODO handle `memcmp`
            // None of the intrinsics should care if the memory is stack or heap allocated.
            auto callee = call->getCalledValue();
            if (auto II = dyn_cast<IntrinsicInst>(call)) {
                if (auto id = II->getIntrinsicID()) {
                    if (id == Intrinsic::memset) {
                        assert(call->getNumArgOperands() == 4);
                        use_info.hasmemset = true;
                        if (cur.offset == UINT32_MAX ||
                            !isa<ConstantInt>(call->getArgOperand(2)) ||
                            !isa<ConstantInt>(call->getArgOperand(1)) ||
                            (cast<ConstantInt>(call->getArgOperand(2))->getLimitedValue() >=
                             UINT32_MAX - cur.offset))
                            use_info.hasunknownmem = true;
                        return true;
                    }
                    if (id == Intrinsic::lifetime_start || id == Intrinsic::lifetime_end ||
                        isa<DbgInfoIntrinsic>(II))
                        return true;
                    use_info.addrescaped = true;
                    return true;
                }
                if (pass.gc_preserve_begin_func == callee) {
                    for (auto user: call->users())
                        use_info.uses.insert(cast<Instruction>(user));
                    use_info.preserves.insert(call);
                    use_info.haspreserve = true;
                    return true;
                }
            }
            if (pass.pointer_from_objref_func == callee) {
                use_info.addrescaped = true;
                return true;
            }
            if (pass.typeof_func == callee || pass.write_barrier_func == callee)
                return true;
            auto opno = use->getOperandNo();
            // Uses in `jl_roots` operand bundle are not counted as escaping, everything else is.
            if (!call->isBundleOperand(opno) ||
                call->getOperandBundleForOperand(opno).getTagName() != "jl_roots") {
                use_info.escaped = true;
                return false;
            }
            use_info.haspreserve = true;
            return true;
        }
        if (auto store = dyn_cast<StoreInst>(inst)) {
            // Only store value count
            if (use->getOperandNo() != StoreInst::getPointerOperandIndex()) {
                use_info.escaped = true;
                return false;
            }
            auto storev = store->getValueOperand();
            if (cur.offset == UINT32_MAX || !use_info.addMemOp(inst, use->getOperandNo(),
                                                               cur.offset, storev->getType(),
                                                               true, *pass.DL))
                use_info.hasunknownmem = true;
            return true;
        }
        if (isa<AddrSpaceCastInst>(inst) || isa<BitCastInst>(inst)) {
            push_inst(inst);
            return true;
        }
        if (auto gep = dyn_cast<GetElementPtrInst>(inst)) {
            uint64_t next_offset = cur.offset;
            if (cur.offset != UINT32_MAX) {
                APInt apoffset(sizeof(void*) * 8, cur.offset, true);
                if (!gep->accumulateConstantOffset(*pass.DL, apoffset) || apoffset.isNegative()) {
                    next_offset = UINT32_MAX;
                }
                else {
                    next_offset = apoffset.getLimitedValue();
                    if (next_offset > UINT32_MAX) {
                        next_offset = UINT32_MAX;
                    }
                }
            }
            push_inst(inst);
            cur.offset = (uint32_t)next_offset;
            return true;
        }
        use_info.escaped = true;
        return false;
    };

    while (true) {
        assert(cur.use_it != cur.use_end);
        auto use = &*cur.use_it;
        auto inst = dyn_cast<Instruction>(use->getUser());
        ++cur.use_it;
        if (!inst) {
            use_info.escaped = true;
            return;
        }
        if (!check_inst(inst, use))
            return;
        use_info.uses.insert(inst);
        if (cur.use_it == cur.use_end) {
            if (check_stack.empty())
                return;
            cur = check_stack.back();
            check_stack.pop_back();
        }
    }
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
    CallInst::Create(pass.lifetime_end, {sz, ptr}, "", insert);
}

void Optimizer::insertLifetime(Value *ptr, Constant *sz, Instruction *orig)
{
    CallInst::Create(pass.lifetime_start, {sz, ptr}, "", orig);
    BasicBlock *def_bb = orig->getParent();
    std::set<BasicBlock*> bbs{def_bb};
    auto &DT = getDomTree();
    // Collect all BB where the allocation is live
    for (auto use: use_info.uses) {
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
    for (auto preserve: use_info.preserves) {
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
    auto nargs = call->getNumArgOperands();
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
            makeArrayRef(argTys).slice(0, oldfType->getNumParams()),
            oldfType->isVarArg());

    // Accumulate an array of overloaded types for the given intrinsic
    // and compute the new name mangling schema
    SmallVector<Type*, 4> overloadTys;
    {
        SmallVector<Intrinsic::IITDescriptor, 8> Table;
        getIntrinsicInfoTableEntries(ID, Table);
        ArrayRef<Intrinsic::IITDescriptor> TableRef = Table;
#if JL_LLVM_VERSION >= 90000
        auto res = Intrinsic::matchIntrinsicSignature(newfType, TableRef, overloadTys);
        assert(res == Intrinsic::MatchIntrinsicTypes_Match);
        (void)res;
#else
        bool res = Intrinsic::matchIntrinsicType(oldfType->getReturnType(), TableRef, overloadTys);
        assert(!res);
        for (auto Ty : newfType->params()) {
            res = Intrinsic::matchIntrinsicType(Ty, TableRef, overloadTys);
            assert(!res);
        }
        (void)res;
#endif
        bool matchvararg = Intrinsic::matchIntrinsicVarArg(newfType->isVarArg(), TableRef);
        assert(!matchvararg);
        (void)matchvararg;
    }
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
    assert(newF->getFunctionType() == newfType);
    newF->setCallingConv(call->getCallingConv());
    auto newCall = CallInst::Create(newF, args, "", call);
    newCall->setTailCallKind(call->getTailCallKind());
    auto old_attrs = call->getAttributes();
    newCall->setAttributes(AttributeList::get(pass.getLLVMContext(), old_attrs.getFnAttributes(),
                                              old_attrs.getRetAttributes(), {}));
    newCall->setDebugLoc(call->getDebugLoc());
    call->replaceAllUsesWith(newCall);
    call->eraseFromParent();
}

// This function should not erase any safepoint so that the lifetime marker can find and cache
// all the original safepoints.
void Optimizer::moveToStack(CallInst *orig_inst, size_t sz, bool has_ref)
{
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
        buff = prolog_builder.CreateAlloca(pass.T_int8, ConstantInt::get(pass.T_int64, 0));
        ptr = buff;
    }
    else if (has_ref) {
        // Allocate with the correct type so that the GC frame lowering pass will
        // treat this as a non-mem2reg'd alloca
        // The ccall root and GC preserve handling below makes sure that
        // the alloca isn't optimized out.
        buff = prolog_builder.CreateAlloca(pass.T_prjlvalue);
        buff->setAlignment(Align(align));
        ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, pass.T_pint8));
    }
    else {
        buff = prolog_builder.CreateAlloca(Type::getIntNTy(pass.getLLVMContext(), sz * 8));
        buff->setAlignment(Align(align));
        ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, pass.T_pint8));
    }
    insertLifetime(ptr, ConstantInt::get(pass.T_int64, sz), orig_inst);
    auto new_inst = cast<Instruction>(prolog_builder.CreateBitCast(ptr, pass.T_pjlvalue));
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
            if (pass.pointer_from_objref_func == callee) {
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
    if (simple_remove(orig_inst))
        return;
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
            auto callee = call->getCalledValue();
            if (pass.gc_preserve_begin_func == callee) {
                removeGCPreserve(call, orig_i);
                return;
            }
            if (pass.typeof_func == callee) {
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            if (pass.write_barrier_func == callee) {
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

void Optimizer::splitOnStack(CallInst *orig_inst)
{
    auto tag = orig_inst->getArgOperand(2);
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
        else {
            allocty = Type::getIntNTy(pass.getLLVMContext(), field.size * 8);
        }
        slot.slot = prolog_builder.CreateAlloca(allocty);
        insertLifetime(prolog_builder.CreateBitCast(slot.slot, pass.T_pint8),
                       ConstantInt::get(pass.T_int64, field.size), orig_inst);
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
    if (simple_replace(orig_inst))
        return;
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
            addr = builder.CreateBitCast(slot.slot, elty->getPointerTo());
            if (offset != 0) {
                addr = builder.CreateConstInBoundsGEP1_32(elty, addr, offset / size);
            }
        }
        else {
            addr = builder.CreateBitCast(slot.slot, pass.T_pint8);
            addr = builder.CreateConstInBoundsGEP1_32(pass.T_int8, addr, offset);
            addr = builder.CreateBitCast(addr, elty->getPointerTo());
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
                val = builder.CreateBitCast(newload, load_ty);
            }
            else {
                newload = builder.CreateLoad(load_ty, slot_gep(slot, offset, load_ty, builder));
                val = newload;
            }
            // TODO: should we use `load->clone()`, or manually copy any other metadata?
#if JL_LLVM_VERSION >= 100000
            newload->setAlignment(MaybeAlign(load->getAlignment()));
#else
            newload->setAlignment(load->getAlignment());
#endif
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
                if (!isa<PointerType>(store_ty)) {
                    store_val = builder.CreateBitCast(store_val, pass.T_size);
                    store_val = builder.CreateIntToPtr(store_val, pass.T_pjlvalue);
                    store_ty = pass.T_pjlvalue;
                }
                else {
                    store_ty = cast<PointerType>(pass.T_pjlvalue)->getElementType()
                        ->getPointerTo(cast<PointerType>(store_ty)->getAddressSpace());
                    store_val = builder.CreateBitCast(store_val, store_ty);
                }
                if (cast<PointerType>(store_ty)->getAddressSpace() != AddressSpace::Tracked)
                    store_val = builder.CreateAddrSpaceCast(store_val, pass.T_prjlvalue);
                newstore = builder.CreateStore(store_val, slot.slot);
            }
            else {
                newstore = builder.CreateStore(store_val, slot_gep(slot, offset, store_ty, builder));
            }
            // TODO: should we use `store->clone()`, or manually copy any other metadata?
#if JL_LLVM_VERSION >= 100000
            newstore->setAlignment(MaybeAlign(store->getAlignment()));
#else
            newstore->setAlignment(store->getAlignment());
#endif
            // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
            newstore->setOrdering(AtomicOrdering::NotAtomic);
            store->eraseFromParent();
            return;
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledValue();
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
                                    Constant *val = ConstantInt::get(pass.T_size, intval);
                                    val = ConstantExpr::getIntToPtr(val, pass.T_pjlvalue);
                                    ptr = ConstantExpr::getAddrSpaceCast(val, pass.T_prjlvalue);
                                }
                                StoreInst *store = builder.CreateAlignedStore(ptr, slot.slot, sizeof(void*));
                                store->setOrdering(AtomicOrdering::NotAtomic);
                                continue;
                            }
                            auto ptr8 = builder.CreateBitCast(slot.slot, pass.T_pint8);
                            if (offset > slot.offset)
                                ptr8 = builder.CreateConstInBoundsGEP1_32(pass.T_int8, ptr8,
                                                                          offset - slot.offset);
                            auto sub_size = std::min(slot.offset + slot.size, offset + size) -
                                std::max(offset, slot.offset);
                            // TODO: alignment computation
#if JL_LLVM_VERSION >= 100000
                            builder.CreateMemSet(ptr8, val_arg, sub_size, MaybeAlign(0));
#else
                            builder.CreateMemSet(ptr8, val_arg, sub_size, 0);
#endif
                        }
                        call->eraseFromParent();
                        return;
                    }
                    call->eraseFromParent();
                    return;
                }
            }
            if (pass.typeof_func == callee) {
                call->replaceAllUsesWith(tag);
                call->eraseFromParent();
                return;
            }
            if (pass.write_barrier_func == callee) {
                call->eraseFromParent();
                return;
            }
            if (pass.gc_preserve_begin_func == callee) {
                SmallVector<Value*,8> operands;
                for (auto &arg: call->arg_operands()) {
                    if (arg.get() == orig_i || isa<Constant>(arg.get()))
                        continue;
                    operands.push_back(arg.get());
                }
                IRBuilder<> builder(call);
                for (auto &slot: slots) {
                    if (!slot.isref)
                        continue;
                    LoadInst *ref = builder.CreateAlignedLoad(pass.T_prjlvalue, slot.slot, sizeof(void*));
                    // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
                    ref->setOrdering(AtomicOrdering::NotAtomic);
                    operands.push_back(ref);
                }
                auto new_call = builder.CreateCall(pass.gc_preserve_begin_func, operands);
                new_call->takeName(call);
                new_call->setAttributes(call->getAttributes());
                call->replaceAllUsesWith(new_call);
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
                std::vector<Value*> operands;
                for (auto op: bundle.inputs()) {
                    if (op == orig_i || isa<Constant>(op))
                        continue;
                    operands.push_back(op);
                }
                IRBuilder<> builder(call);
                for (auto &slot: slots) {
                    if (!slot.isref)
                        continue;
                    LoadInst *ref = builder.CreateAlignedLoad(pass.T_prjlvalue, slot.slot, sizeof(void*));
                    // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
                    ref->setOrdering(AtomicOrdering::NotAtomic);
                    operands.push_back(ref);
                }
                bundle = OperandBundleDef("jl_roots", std::move(operands));
                break;
            }
            auto new_call = CallInst::Create(call, bundles, call);
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

    T_int64 = Type::getInt64Ty(getLLVMContext());

    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { T_pint8 });
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { T_pint8 });

    return true;
}

bool AllocOpt::runOnFunction(Function &F)
{
    if (!alloc_obj_func)
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

extern "C" JL_DLLEXPORT void LLVMExtraAddAllocOptPass(LLVMPassManagerRef PM)
{
    unwrap(PM)->add(createAllocOptPass());
}
