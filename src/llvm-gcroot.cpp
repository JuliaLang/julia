// This file is a part of Julia. License is MIT: http://julialang.org/license

#define DEBUG_TYPE "lower_gcroot"
#undef DEBUG

#include "llvm-version.h"
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Intrinsics.h>
#if JL_LLVM_VERSION >= 30700
#include <llvm/IR/LegacyPassManager.h>
#else
#include <llvm/PassManager.h>
#endif
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#if JL_LLVM_VERSION >= 30600
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#endif
#include <llvm/IR/MDBuilder.h>

#include <vector>
#include <queue>
#include <set>
#include <functional>
#include <utility>

#include "julia.h"

#if JL_LLVM_VERSION >= 30700
#define LLVM37_param(x) (x),
#else
#define LLVM37_param(x)
#endif

using namespace llvm;

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);

namespace {

#ifndef NDEBUG
static struct {
    unsigned count;
    unsigned locals;
    unsigned temp;
} jl_gc_frame_stats = {0};
#endif

typedef std::pair<CallInst*, unsigned> frame_register;
struct liveness {
    typedef unsigned id;
    enum {
        // an assignment to a gcroot exists in the basic-block
        // (potentially no live-in from the predecessor basic-blocks)
        assign = 1<<0,
        // a use of a gcroot exists in the basic-block
        // (potentially a "kill" and no live-out to the successor basic-blocks)
        kill   = 1<<1,
        // the gcroot is live over the entire basic-block
        // (the assign/kill are not dominating of entry/exit)
        live   = 1<<2

        // live | kill | assign:
        //     a usage and assignment exist, but it is also live on exit,
        //     the entry liveness depends on whether a store or use is
        //     encountered first
        // live | kill:
        //     a usage exists,
        //     but the value must be live for the entire basic-block
        //     since it is not the terminal usage in the domination tree
        // kill | assign:
        //     a usage and definition exist in domination order,
        //     so the actual lifetime is only a subset of the basic-block
        // live | assign:
        //     impossible (this would be strange)
    };
};

static void tbaa_decorate_gcframe(Instruction *inst,
                                  std::set<Instruction*> &visited,
                                  MDNode *tbaa_gcframe)
{
    if (!visited.insert(inst).second)
        return;
#if JL_LLVM_VERSION >= 30500
    Value::user_iterator I = inst->user_begin(), E = inst->user_end();
#else
    Value::use_iterator I = inst->use_begin(), E = inst->use_end();
#endif
    for (;I != E;++I) {
        Instruction *user = dyn_cast<Instruction>(*I);
        if (!user) {
            continue;
        }
        else if (isa<GetElementPtrInst>(user)) {
            if (__likely(user->getOperand(0) == inst)) {
                tbaa_decorate_gcframe(user, visited, tbaa_gcframe);
            }
        }
        else if (isa<StoreInst>(user)) {
            if (user->getOperand(1) == inst) {
                user->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
            }
        }
        else if (isa<LoadInst>(user)) {
            user->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
        }
        else if (isa<BitCastInst>(user)) {
            tbaa_decorate_gcframe(user, visited, tbaa_gcframe);
        }
    }
}

static void tbaa_decorate_gcframe(Instruction *inst, MDNode *tbaa_gcframe)
{
    std::set<Instruction*> visited;
    tbaa_decorate_gcframe(inst, visited, tbaa_gcframe);
}

class JuliaGCAllocator {
public:
    JuliaGCAllocator(Function &F, CallInst *ptlsStates,
                     Type *T_pjlvalue, MDNode *tbaa) :
        F(F),
        M(*F.getParent()),
        T_int1(Type::getInt1Ty(F.getContext())),
        T_int8(Type::getInt8Ty(F.getContext())),
        T_int32(Type::getInt32Ty(F.getContext())),
        T_int64(Type::getInt64Ty(F.getContext())),
        V_null(T_pjlvalue ? Constant::getNullValue(T_pjlvalue) : nullptr),
        ptlsStates(ptlsStates),
        gcframe(ptlsStates ? new AllocaInst(T_pjlvalue, ConstantInt::get(T_int32, 0)) : nullptr),
        gcroot_func(M.getFunction("julia.gc_root_decl")),
        gckill_func(M.getFunction("julia.gc_root_kill")),
        jlcall_frame_func(M.getFunction("julia.jlcall_frame_decl")),
        gcroot_flush_func(M.getFunction("julia.gcroot_flush")),
        except_enter_func(M.getFunction("julia.except_enter")),
        jlleave_func(M.getFunction("jl_pop_handler")),
        tbaa_gcframe(tbaa)
    {
/* Algorithm sketch:
 *  Compute liveness for each basic block
 *    liveness computed at the basic-block level for <inst, arg-offset> pairs
 *  Propagate liveness from each basic block to its predecessors
 *  Allocate argument slot for each jlcall frame
 */
        if (gcframe) {
#ifdef JL_DEBUG_BUILD
            gcframe->setName("gcrootframe");
#endif
            gcframe->insertAfter(ptlsStates);
        }
    }

private:
    Function &F;
    Module &M;
    Type *const T_int1;
    Type *const T_int8;
    Type *const T_int32;
    Type *const T_int64;
    Value *const V_null;
    CallInst *const ptlsStates;
    AllocaInst *const gcframe;
    Function *const gcroot_func;
    Function *const gckill_func;
    Function *const jlcall_frame_func;
    Function *const gcroot_flush_func;
    Function *const except_enter_func;
    Function *const jlleave_func;
    MDNode *const tbaa_gcframe;

    Instruction *get_pgcstack(Instruction *ptlsStates);
    frame_register get_gcroot(Value *ptr);
    void collapseRedundantRoots();
    bool record_usage(CallInst *callInst,
        std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses,
        std::map<BasicBlock*, SmallBitVector> &regs_used,
        unsigned &offset, bool commit=true);
    unsigned find_space_for(CallInst *callInst,
        std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses,
        std::map<BasicBlock*, SmallBitVector> &regs_used);
    void rearrangeRoots();
    void lowerHandlers();
public:
    void allocate_frame();
};

struct HandlerData {
    // Pairs of <jl_pop_handle call inst>, number of pops left after popping
    // this frame.
    std::vector<std::pair<CallInst*,uint64_t>> leaves;
    // enters that are directly nested in this frame
    std::set<CallInst*> nested;
    std::unique_ptr<std::vector<CallInst*>> parent_vec;
    CallInst *parent{nullptr};
    bool processed{false};
#ifdef _COMPILER_MICROSOFT_
    // MSVC 2013 seems to call the copy version instead of the move verion
    // without this, which triggers compilation error since `std::unique_ptr`
    // is not copyable.
    HandlerData() = default;
    HandlerData(HandlerData &&other)
        : HandlerData()
    {
        operator=(std::move(other));
    }
    HandlerData(const HandlerData&) = delete;
    HandlerData &operator=(HandlerData &&other)
    {
        std::swap(leaves, other.leaves);
        std::swap(nested, other.nested);
        std::swap(parent_vec, other.parent_vec);
        std::swap(parent, other.parent);
        std::swap(processed, other.processed);
        return *this;
    }
    HandlerData &operator=(const HandlerData&) = delete;
#endif
};

void JuliaGCAllocator::lowerHandlers()
{
    if (!except_enter_func)
        return;
    auto jlenter_func = M.getNamedValue("jl_enter_handler");
    auto setjmp_func = M.getNamedValue(jl_setjmp_name);
    // Collect all exception enters
    std::map<CallInst*,HandlerData> handlers;
    for (auto &BB: F) {
        for (auto &I: BB) {
            auto call = dyn_cast<CallInst>(&I);
            if (call && call->getCalledValue() == except_enter_func) {
                handlers[call] = HandlerData();
            }
        }
    }

    if (handlers.empty())
        return;

    typedef std::map<CallInst*,HandlerData>::iterator hdlr_iter_t;
    // For each exception enter, compute the life time of the enter, find
    // the corresponding leaves and collect a list of nested exception frames.
    // This assumes the exception frames have simple structure. E.g.
    // there's no recursion and different frames do not share the same leave.
    std::function<void(hdlr_iter_t)> process_handler = [&] (hdlr_iter_t hdlr) {
        auto enter = hdlr->first;
        auto &data = hdlr->second;
        if (data.processed)
            return;
        data.processed = true;
        std::vector<BasicBlock::iterator> frontier = {
            ++BasicBlock::iterator(enter)
        };
        // Start from the enter, the enter is live until it hits a leave
        // or the control flow terminates.
        std::set<Instruction*> visited;
        auto add_inst = [&] (BasicBlock::iterator it) {
            if (visited.insert(&*it).second) {
                frontier.push_back(it);
            }
        };
        while (!frontier.empty()) {
            BasicBlock::iterator IT = frontier.back();
            Instruction *I = &*IT;
            ++IT;
            frontier.pop_back();
            if (I->isTerminator()) {
                BasicBlock *bb = I->getParent();
                for (auto S = succ_begin(bb), E = succ_end(bb); S != E; S++)
                    add_inst(S->getFirstInsertionPt());
                continue;
            }
            auto call = dyn_cast<CallInst>(I);
            if (!call) {
                add_inst(IT);
                continue;
            }
            auto callee = call->getCalledValue();
            if (callee == except_enter_func) {
                // Nested frame, make sure the frame is processed and skip to
                // its leaves.
                auto other_it = handlers.find(call);
                assert(other_it != handlers.end());
                process_handler(other_it);
                auto &other_data = other_it->second;
                data.nested.insert(call);
                // Record a reference to parent frame, there should be only
                // one parent in most cases although more than one parent is
                // also possible if LLVM split the enter call.
                if (__unlikely(other_data.parent_vec)) {
                    other_data.parent_vec->push_back(enter);
                }
                else if (__unlikely(other_data.parent)) {
                    other_data.parent_vec.reset(
                        new std::vector<CallInst*>{other_data.parent, enter});
                }
                else {
                    other_data.parent = enter;
                }
                for (auto leave: other_it->second.leaves) {
                    if (leave.second > 0) {
                        data.leaves.push_back(std::make_pair(leave.first,
                                                             leave.second - 1));
                    }
                    else {
                        add_inst(++BasicBlock::iterator(leave.first));
                    }
                }
            }
            else if (callee == jlleave_func) {
                // Leave, record how many pops are left on this leave.
                assert(call->getNumArgOperands() == 1);
                auto arg = cast<ConstantInt>(call->getArgOperand(0));
                uint64_t pop_count = arg->getLimitedValue();
                assert(pop_count >= 1);
                data.leaves.push_back(std::make_pair(call, pop_count - 1));
            }
            else {
                // Otherwise, go to the next instruction.
                add_inst(IT);
            }
        }
    };

    // Make sure all frames are processed.
    for (auto IT = handlers.begin(), E = handlers.end(); IT != E; ++IT)
        process_handler(IT);

    std::set<CallInst*> processing;
    Value *handler_sz = ConstantInt::get(T_int32, sizeof(jl_handler_t));
    // Now allocate the stack slots.
    // At each iteration, we allocate a new handler and assign all the remaining
    // frames that don't have a non-processed child to this handler.
    Instruction *firstInst = &F.getEntryBlock().front();
    while (!handlers.empty()) {
        processing.clear();
        auto buff = new AllocaInst(T_int8, handler_sz, "", firstInst);
        buff->setAlignment(16);
        // Collect the list of frames to process.
        for (auto &hdlr: handlers) {
            auto enter = hdlr.first;
            auto &nested = hdlr.second.nested;
            if (nested.empty()) {
                processing.insert(enter);
            }
        }
        // There shouldn't be loops so there has to be leafs
        assert(!processing.empty());
        for (auto enter: processing) {
            // Lower the enter
            auto new_enter = CallInst::Create(jlenter_func, buff, "", enter);
#ifndef _OS_WINDOWS_
            // For LLVM 3.3 compatibility
            Value *args[] = {buff, ConstantInt::get(T_int32,0)};
            auto sj = CallInst::Create(setjmp_func, args, "", enter);
#else
            auto sj = CallInst::Create(setjmp_func, buff, "", enter);
#endif
            // We need to mark this on the call site as well. See issue #6757
            sj->setCanReturnTwice();
#if JL_LLVM_VERSION >= 30600
            if (auto dbg = enter->getMetadata(LLVMContext::MD_dbg)) {
                new_enter->setMetadata(LLVMContext::MD_dbg, dbg);
                sj->setMetadata(LLVMContext::MD_dbg, dbg);
            }
#else
            (void)new_enter;
#endif
            // Remove it from all the parents.
            auto it = handlers.find(enter);
            if (__unlikely(it->second.parent_vec)) {
                for (auto parent: *it->second.parent_vec) {
                    handlers.find(parent)->second.nested.erase(enter);
                }
            }
            else if (it->second.parent) {
                handlers.find(it->second.parent)->second.nested.erase(enter);
            }
            // Delete it.
            handlers.erase(it);
            enter->replaceAllUsesWith(sj);
            enter->eraseFromParent();
        }
    }
}

Instruction *JuliaGCAllocator::get_pgcstack(Instruction *ptlsStates)
{
    Constant *offset = ConstantInt::getSigned(T_int32, offsetof(jl_tls_states_t, pgcstack) / sizeof(void*));
    return GetElementPtrInst::Create(LLVM37_param(NULL)
                                     ptlsStates,
                                     ArrayRef<Value*>(offset),
                                     "jl_pgcstack");
}

frame_register JuliaGCAllocator::get_gcroot(Value *ptr)
{
    frame_register frame;
    frame.first = dyn_cast<CallInst>(ptr);
    frame.second = 0;
    if (frame.first == NULL) {
        // also try to look through GEP for jlcall_frame_func
        if (GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(ptr)) {
            if (gepInst->getNumIndices() == 1) {
                frame.first = dyn_cast<CallInst>(gepInst->getPointerOperand());
                if (frame.first && frame.first->getCalledValue() == jlcall_frame_func)
                    frame.second = cast<ConstantInt>(gepInst->idx_begin()->get())->getZExtValue();
                else
                    frame.first = NULL;
            }
        }
    }
    return frame;
}

void JuliaGCAllocator::collapseRedundantRoots()
{
    for (BasicBlock::iterator I = gcframe->getParent()->begin(), E(gcframe); I != E; ) {
        CallInst* callInst = dyn_cast<CallInst>(&*I);
        ++I;
        if (callInst && callInst->getCalledValue() == gcroot_func) {
            // see if a root is only used briefly for `store -> load -> store other` pattern or `store, store other`
            // such that the first store can be trivially replaced with just "other" and delete the chain
            // or if is used for store, but the value is never needed
            StoreInst *theStore = NULL;
            unsigned n_stores = 0;
            bool variable_slot = true; // whether this gc-root is only used as a variable-slot; e.g. whether theLoad is theValue
            LoadInst *theLoad = NULL;
            for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ) {
#if JL_LLVM_VERSION >= 30500
                User *user = use->getUser();
#else
                User *user = use.getUse().getUser();
#endif
                ++use;
                if (StoreInst *storeInst = dyn_cast<StoreInst>(user)) {
                    if (n_stores == 0)
                        theStore = storeInst;
                    else
                        theStore = NULL;
                    Value *theValue = storeInst->getValueOperand();
                    if (!theValue->hasOneUse()) { // not just the store
                        variable_slot = false; // this gc-root is used as more than just a variable-slot (aka phi node)
                    }
                    n_stores++;
                }
                else if (LoadInst *loadInst = dyn_cast<LoadInst>(user)) {
                    if (loadInst->use_empty()) {
                        // dead load?
                        loadInst->eraseFromParent();
                    }
                    else {
                        if (theLoad) {
                            // multiple live loads, this is hard to optimize, so skip it
                            n_stores = 0;
                            break;
                        }
                        theLoad = loadInst;
                    }
                }
                else {
                    // what is this? oh, well. skip trying to optimize this gc-root
                    n_stores = 0;
                    break;
                }
            }

            if (n_stores == 0)
                continue;

            if (theLoad == NULL) {
                // this gc-root is never loaded from, so we don't need it as a variable location
                // delete any stores to this gc-root that would be keeping an otherwise-unused value alive
                for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ) {
#if JL_LLVM_VERSION >= 30500
                    User *user = use->getUser();
#else
                    User *user = use.getUse().getUser();
#endif
                    StoreInst *theStore = cast<StoreInst>(user);
                    ++use;
                    Value *theValue = theStore->getValueOperand();
                    if (theValue->hasOneUse()) { // just the store
                        if (&*I == theStore) ++I;
                        theStore->eraseFromParent();
                    }
                }
                if (callInst->use_empty()) {
                    callInst->eraseFromParent();
                    continue;
                }
                else if (callInst->hasOneUse()) {
                    User::use_iterator use = callInst->use_begin();
#if JL_LLVM_VERSION >= 30500
                    theStore = cast<StoreInst>(use->getUser());
#else
                    theStore = cast<StoreInst>(use.getUse().getUser());
#endif
                }
            }

            if ((theLoad != NULL && variable_slot) ||
                (theLoad == NULL && theStore != NULL)) {
                Value *theValue = theLoad ? theLoad : theStore->getValueOperand();
                if (theValue->hasNUses(theLoad ? 1 : 2)) { // only uses are theStore and theLoad and theOther
                    // check if this value is only used for a store to another gcroot
                    User::use_iterator value_use = theValue->use_begin();
                    if (theLoad && *value_use == theStore)
                        ++value_use;
#if JL_LLVM_VERSION >= 30500
                    StoreInst *theOther = dyn_cast<StoreInst>(value_use->getUser());
                    unsigned OperandNo = value_use->getOperandNo();
#else
                    StoreInst *theOther = dyn_cast<StoreInst>(value_use.getUse().getUser());
                    unsigned OperandNo = value_use.getOperandNo();
#endif
                    if (theOther && OperandNo != StoreInst::getPointerOperandIndex()) {
                        // test whether this store is valid as a gc-root
                        bool patternMatchSuccess = false;
                        frame_register gcroot_other_gep = get_gcroot(theOther->getPointerOperand());
                        CallInst *gcroot_other = gcroot_other_gep.first;
                        // it could be a gcroot...
                        if (gcroot_other && gcroot_other->getCalledValue() == gcroot_func && theStore != NULL) {
                            // need to make sure there aren't any other uses of gcroot_other (including gckill)
                            // between the initial store and the replacement store
                            // TODO: do this better once we have liveness information for locals?
                            BasicBlock *current = theStore->getParent();
                            BasicBlock::iterator bbi(theStore);
                            BasicBlock::iterator bbi_end = current->end();
                            patternMatchSuccess = true;
                            ++bbi;
                            while (patternMatchSuccess) {
                                Instruction *inst = &*bbi;
                                if (inst == theOther) {
                                    break; // success
                                }
                                for (Instruction::op_iterator op = inst->op_begin(), op_e = inst->op_end(); op != op_e; ++op) {
                                    if (op->get() == gcroot_other) {
                                        patternMatchSuccess = false;
                                        break; // fail: gcroot_other had another reference, can't make this replacement
                                    }
                                }
                                if (++bbi == bbi_end) {
                                    // iterate the basicblock forward, if it's a simple branch
#if JL_LLVM_VERSION >= 30500
                                    BasicBlock *next = current->getUniqueSuccessor();
#else
                                    succ_iterator SI = succ_begin(current), E = succ_end(current);
                                    BasicBlock *next = NULL;
                                    if (SI != E) {
                                        next = *SI;
                                        for (++SI; SI != E; ++SI) {
                                            if (*SI != next) {
                                                next = NULL;
                                                break;
                                            }
                                        }
                                    }
#endif
                                    if (next) {
                                        bbi = next->begin();
                                        bbi_end = next->end();
                                        current = next;
                                    }
                                    else {
                                        patternMatchSuccess = false;
                                    }
                                }
                            }
                        }
                        // ...or it could be a jlcall frame
                        else if (gcroot_other && gcroot_other->getCalledValue() == jlcall_frame_func) {
                            // jlcall_frame_func slots are effectively SSA,
                            // so it's always safe to merge an earlier store into it
                            // but do need to update liveness information for this slot
                            // TODO: do this better once we have liveness information for locals?
                            if (theStore != NULL && theOther->getParent() == theStore->getParent()) {
                                //unsigned arg_offset = gcroot_other_gep->second;
                                //frame_register def(gcroot_other, arg_offset);
                                //std::map<frame_register, liveness::id> &inuse_list = bb_uses[theOther->getParent()];
                                //std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                                patternMatchSuccess = true;
                            }
                        }
                        if (patternMatchSuccess) {
                            // do the gcroot merge -- replace gcroot with gcroot_other in all the store operations for this gcroot
                            // so that theOther, theLoad, and this gcroot are no longer needed
                            Value *gcroot_other = theOther->getPointerOperand();
                            if (&*I == theOther) ++I;
                            theOther->eraseFromParent();
                            if (theLoad) {
                                if (&*I == theLoad) ++I;
                                theLoad->eraseFromParent();
                            }
                            if (theStore) {
                                theStore->setOperand(StoreInst::getPointerOperandIndex(), gcroot_other);
                            }
                            else {
                                for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ) {
#if JL_LLVM_VERSION >= 30500
                                    User *user = use->getUser();
#else
                                    User *user = use.getUse().getUser();
#endif
                                    ++use;
                                    StoreInst *theStore = cast<StoreInst>(user);
                                    theStore->setOperand(StoreInst::getPointerOperandIndex(), gcroot_other);
                                }
                            }
                            callInst->eraseFromParent();
                        }
                    }
                }
            }
        }
    }
}

bool JuliaGCAllocator::record_usage(CallInst *callInst,
        std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses,
        std::map<BasicBlock*, SmallBitVector> &regs_used,
        unsigned &offset, bool commit)
{
/* record-usage(inst, bb-uses, regs-used, offset, commit=true)
 *     for (arg-offset, operand) in enumerate(arguments(inst))
 *         reg = <inst, arg-offset>
 *         for (bb, liveness) in bb-uses
 *             if not reg in liveness
 *                 continue
 *             # TODO: optimize better if liveness[reg] doesn't contain live
 *             conflict = regs-used[bb][offset + arg-offset]
 *             if commit
 *                 assert !conflict
 *                 regs-used[bb][offset + arg-offset] = true
 *             else if conflict
 *                 return false
 *     return true
 */
    unsigned arg_n = cast<ConstantInt>(callInst->getArgOperand(0))->getZExtValue();
#if 0 // suboptimal allocator that ignores computed liveness data
    {
        SmallBitVector &regs = regs_used[&callInst->getParent()->getParent()->getEntryBlock()];
        if (offset + arg_n > regs.size())
            regs.resize(offset + arg_n);
        for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
            frame_register def(callInst, arg_offset);
#else // }} better allocator that uses per-basicblock liveness
    for (std::map<BasicBlock*, std::map<frame_register, liveness::id> >::iterator
            live_reg = bb_uses.begin(), e = bb_uses.end(); live_reg != e; ++live_reg) {
        BasicBlock *bb = live_reg->first;
        SmallBitVector &regs = regs_used[bb];
        if (offset + arg_n > regs.size())
            regs.resize(offset + arg_n);
        for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
            frame_register def(callInst, arg_offset);
            std::map<frame_register, liveness::id>::iterator inuse_reg = live_reg->second.find(def);
            if (inuse_reg == live_reg->second.end())
                continue;
            // TODO: optimize here better when not live in inuse_reg->second, by ascertaining liveness at the instruction level for this bb
#endif
            unsigned index = offset + arg_offset;
            bool conflict = regs.test(index);
            if (commit) {
                assert(!conflict);
                regs.set(index);
            }
            else if (conflict) {
                // update the offset argument to point to the next open register beyond index
                // to help avoid unnecessary work and accelerate the search
                ++offset;
                while (offset + arg_offset < regs.size() && regs.test(offset + arg_offset))
                    ++offset;
                return false;
            }
        }
    }
    return true;
}

unsigned JuliaGCAllocator::find_space_for(CallInst *callInst,
        std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses,
        std::map<BasicBlock*, SmallBitVector> &regs_used)
{
/* find-space-for(inst, bb-uses, regs-used)
 *     n = 0
 *     while !record-usage(inst, bb-uses, regs-used, n, false)
 *         n++
 *     return n
 *
 */
    unsigned n = 0;
    while (!record_usage(callInst, bb_uses, regs_used, n, false)) { }
    return n;
}

void JuliaGCAllocator::rearrangeRoots()
{
    for (auto BB = F.begin(), E(F.end()); BB != E; BB++) {
        auto terminst = BB->getTerminator();
        if (!isa<ReturnInst>(terminst) && !isa<UnreachableInst>(terminst))
            continue;
        SmallVector<StoreInst*, 16> toRemove;
        for (auto I = BB->rbegin(), E(BB->rend()); I != E; ++I) {
            // Only handle the simplest case for now, give up if there's a call
            // or load from the GC frame.
            // (Assume we don't have loads that can alias GC frame
            // unless the source address is a `julia.gc_root_decl`)
            Instruction *inst = &*I;
            if (isa<CallInst>(inst))
                break;
            if (LoadInst *loadInst = dyn_cast<LoadInst>(inst)) {
                CallInst *loadAddr =
                    dyn_cast<CallInst>(loadInst->getPointerOperand());
                if (loadAddr && loadAddr->getCalledValue() == gcroot_func)
                    break;
                continue;
            }
            if (StoreInst *storeInst = dyn_cast<StoreInst>(inst)) {
                CallInst *storeAddr =
                    dyn_cast<CallInst>(storeInst->getPointerOperand());
                if (storeAddr && storeAddr->getCalledValue() == gcroot_func)
                    toRemove.push_back(storeInst);
                continue;
            }
        }
        for (auto inst: toRemove) {
            CallInst *decl = cast<CallInst>(inst->getPointerOperand());
            inst->eraseFromParent();
            // TODO removing unused slot should probably be handled later
            // when we allocate the frame
            if (decl->use_empty()) {
                decl->eraseFromParent();
            }
        }
    }
}

void JuliaGCAllocator::allocate_frame()
{
    lowerHandlers();
    if (!ptlsStates)
        return;
    Instruction *last_gcframe_inst = gcframe;
    collapseRedundantRoots();
    rearrangeRoots();

/* # initialize the kill BasicBlock of all jlcall-frames
 * bb-uses : map<BB, map< pair<inst, arg-offset>, assign|live|kill > >
 * for inst in gc-frame(f)
 *     if inst match "a call to make-jlcall-frame"
 *         kill-use = get-unique-use<isa-CallInst>(inst)
 *         bb-uses[bb][<def, 1:nargs>] = kill
 */
    std::map<BasicBlock*, std::map<frame_register, liveness::id> > bb_uses;
    std::priority_queue< std::pair<unsigned, CallInst*> > frames;
    for (BasicBlock::iterator I = gcframe->getParent()->begin(), E(gcframe); I != E; ) {
        CallInst* callInst = dyn_cast<CallInst>(&*I);
        ++I;
        if (callInst && callInst->getCalledValue() == jlcall_frame_func) {
            BasicBlock *bb = NULL;
            unsigned arg_n = cast<ConstantInt>(callInst->getArgOperand(0))->getZExtValue();
            frames.push(std::make_pair(arg_n, callInst));
            // the jlcall frame should have been passed to exactly one call (the jlcall) -- find its basic-block
            for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ++use) {
#if JL_LLVM_VERSION >= 30500
                User *user = use->getUser();
#else
                User *user = use.getUse().getUser();
#endif
                if (CallInst *callInst = dyn_cast<CallInst>(user)) {
                    assert(bb == NULL);
                    bb = callInst->getParent();
#ifdef NDEBUG
                    break;
#endif
                }
            }
            assert(bb != NULL);
            std::map<frame_register, liveness::id> &inuse_list = bb_uses[bb];
            for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
                inuse_list[frame_register(callInst, arg_offset)] = liveness::kill;
            }
        }
    }

/* # initialize the dataflow queue for tracking liveness
 * bb-queue : queue<BB>
 * for bb in iterator(f)
 *     inuse-list = &bb-uses[bb]
 *     for inst in reverse-iterator(f)
 *         if inst matches store-inst # todo: or inst matches "gc-store-inst" (for stores to non-stack-slots)
 *             if inst->operand(0) matches "a call to make-jlcall-frame" (or gep thereof)
 *                 def = <inst, arg-offset>
 *                 if inuse-list[def] is kill
 *                     inuse-list[def] = assign|kill
 *      if not has-live-out(bb)
 *          continue
 *      for pred in predecessors(bb)
 *          if not pred in bb-queue
 *              push-back(bb-queue, pred)
 */
    std::vector<BasicBlock*> bb_queue;
    for (std::map<BasicBlock*, std::map<frame_register, liveness::id> >::iterator
            live_reg = bb_uses.begin(), e = bb_uses.end(); live_reg != e; ++live_reg) {
        BasicBlock *bb = live_reg->first;
        std::map<frame_register, liveness::id> &inuse_list = live_reg->second;
        unsigned live_out = inuse_list.size();

        for (BasicBlock::iterator ri = bb->end(); ri != bb->begin(); ) {
            Instruction *i = &*--ri;
            if (StoreInst *storeInst = dyn_cast<StoreInst>(i)) {
                frame_register def = get_gcroot(storeInst->getPointerOperand());
                if (CallInst *callInst = def.first) {
                    if (callInst->getCalledValue() == jlcall_frame_func) {
                        std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                        if (inuse_reg != inuse_list.end() && inuse_reg->second == liveness::kill) {
                            inuse_reg->second |= liveness::assign;
                            --live_out;
                        }
                    }
                }
            }
        }
        if (live_out == 0)
            continue;
        assert(&*bb != &F.getEntryBlock()); // only undef variables should live-out from the entry bb
        for (pred_iterator PI = pred_begin(bb), PE = pred_end(bb); PI != PE; ++PI) {
            if (std::find(bb_queue.begin(), bb_queue.end(), *PI) == bb_queue.end())
                bb_queue.push_back(*PI);
        }
    }


/* # follow liveness information flow until termination
 * while not empty(bb-queue)
 *     bb = pop(bb-queue)
 *     inuse-list = &bb-uses[bb]
 *     changes = 0
 *     for succ in successors(bb)
 *         for <def, op> in bb-uses[succ]
 *             if (not assign in op) and not (inuse-list[def] contains live or assign)
 *                 # need to add live value from successor to current block, unless it was already marked
 *                 inuse-list[def] |= live
 *                 changes += 1
 *     for inst in iterator(bb)
 *         if inst matches store-inst # todo: or inst matches "gc-store-inst" (for stores to non-stack-slots)
 *             if inst->operand(0) matches "a call to make-jlcall-frame" (or gep thereof)
 *                 def = <inst, arg-offset>
 *                 if live in inuse-list[def]
 *                     inuse-list[def] |= assign
 *                     if not kill in inuse-list[def]
 *                          # found the assignment, def is no longer live
 *                          inuse-list[def] &= ~live
 *                     else
 *                          # not a true kill due to recursion -- the kill happened before this assign in this BB, so it is still live
 *                     changes -= 1
 *     # if the live list changed, make sure all predecessors are in the queue to be reanalyzed
 *     if changes == 0
 *         continue
 *     for pred in predecessors(bb)
 *         if not pred in bb-queue
 *             push-back(bb-queue, pred)
 */

    while (!bb_queue.empty()) {
        BasicBlock *bb = bb_queue.back();
        bb_queue.pop_back();
        std::map<frame_register, liveness::id> &inuse_list = bb_uses[bb];
        unsigned changes = 0;
        for (succ_iterator SI = succ_begin(bb), SE = succ_end(bb); SI != SE; ++SI) {
            std::map<frame_register, liveness::id> &succ_uses = bb_uses[*SI];
            for (std::map<frame_register, liveness::id>::iterator reg = succ_uses.begin(), rege = succ_uses.end(); reg != rege; ++reg) {
                if (!(reg->second & liveness::assign)) {
                    liveness::id &live = inuse_list[reg->first];
                    if (!(live & (liveness::live | liveness::assign))) {
                        live |= liveness::live;
                        ++changes;
                    }
                }
            }
        }
        if (!changes) // short-circuit
            continue;
        for (BasicBlock::iterator i = bb->begin(), ie = bb->end(); i != ie; ++i) {
            if (StoreInst *storeInst = dyn_cast<StoreInst>(&*i)) {
                frame_register def = get_gcroot(storeInst->getPointerOperand());
                if (CallInst *callInst = def.first) {
                    if (callInst->getCalledValue() == jlcall_frame_func) {
                        std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                        if (inuse_reg != inuse_list.end() && (inuse_reg->second & liveness::live)) {
                            inuse_reg->second |= liveness::assign;
                            if (!(inuse_reg->second & liveness::kill))
                                inuse_reg->second &= ~liveness::live;
                            --changes;
                        }
                    }
                }
            }
        }
        if (!changes)
            continue;
        assert(bb != &F.getEntryBlock()); // only undef variables should live-out from the entry bb
        for (pred_iterator PI = pred_begin(bb), PE = pred_end(bb); PI != PE; ++PI) {
            if (std::find(bb_queue.begin(), bb_queue.end(), *PI) == bb_queue.end())
                bb_queue.push_back(*PI);
        }
    }

/* # allocate space in locals for the variables
 * TBD
 */

/* # allocate space in temp-args for each jlcall frame
 * regs-used = zip(get-basic-blocks(), falses)
 * for <frame-size, inst> in frames
 *     frame-offset = find-space-for(inst, bb-uses, regs-used)
 *     record-usage(inst, bb-uses, regs-used, frame-offset)
 * # frame iterator allocates space in reverse size order
 * # so that the large frames get allocated first
 * # and the smaller frames just fill in the gaps
 * # I believe this is likely to give good results (compact gc-frames)
 */
    std::map<BasicBlock*, SmallBitVector> regs_used;
    std::map<CallInst*, unsigned> frame_offsets;
    unsigned maxDepth = 0;
    for (; !frames.empty(); frames.pop()) {
        std::pair<unsigned, CallInst*> frame = frames.top();
        unsigned arg_n = frame.first;
        if (arg_n == 0) continue;
        CallInst *callInst = frame.second;
        unsigned frame_offset = find_space_for(callInst, bb_uses, regs_used);
        record_usage(callInst, bb_uses, regs_used, frame_offset);
        frame_offsets[callInst] = frame_offset;
        if (frame_offset + arg_n > maxDepth)
            maxDepth = frame_offset + arg_n;
    }

/* # cleanup and finalize the IR */
    for (Function::iterator bb = F.begin(), be = F.end(); bb != be; ++bb) {
        for (BasicBlock::iterator i = bb->begin(), ie = bb->end(); i != ie; ) {
            Instruction *inst = &*i;
            ++i;
            // delete the now unused gckill information
            if (CallInst* callInst = dyn_cast<CallInst>(inst)) {
                Value *callee = callInst->getCalledValue();
                if (callee == gckill_func || callee == gcroot_flush_func) {
                    callInst->eraseFromParent();
                }
            }
            // delete any StoreInst to a gcframe slot that isn't live
            else if (StoreInst *storeInst = dyn_cast<StoreInst>(inst)) {
                frame_register def = get_gcroot(storeInst->getPointerOperand());
                if (CallInst *gcroot = def.first) {
                    if (gcroot->getCalledValue() == jlcall_frame_func) {
                        std::map<frame_register, liveness::id> &inuse_list = bb_uses[storeInst->getParent()];
                        std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                        if (inuse_reg == inuse_list.end())
                            storeInst->eraseFromParent();
                    }
                }
            }
        }
    }

    Instruction *tempSlot;
    if (frame_offsets.empty()) {
        tempSlot = NULL;
    }
    else {
        tempSlot = GetElementPtrInst::Create(LLVM37_param(NULL) gcframe, ArrayRef<Value*>(ConstantInt::get(T_int32, 2)));
#ifdef JL_DEBUG_BUILD
        tempSlot->setName("temproots");
#endif
        tempSlot->insertAfter(gcframe);
        if (last_gcframe_inst == gcframe)
            last_gcframe_inst = tempSlot;

        // finalize all of the jlcall frames by replacing all of the frames with the appropriate gep(tempslot)
        for (std::map<CallInst*, unsigned>::iterator frame = frame_offsets.begin(), framee = frame_offsets.end(); frame != framee; ++frame) {
            CallInst *gcroot = frame->first;
            tbaa_decorate_gcframe(gcroot, tbaa_gcframe);
            Value* offset[1] = {ConstantInt::get(T_int32, frame->second)};
            GetElementPtrInst *gep = GetElementPtrInst::Create(LLVM37_param(NULL) tempSlot, makeArrayRef(offset));
            gep->insertAfter(last_gcframe_inst);
            gcroot->replaceAllUsesWith(gep);
            gep->takeName(gcroot);
            gcroot->eraseFromParent();
            last_gcframe_inst = gep;
        }
    }

/* # replace all intermediate roots defs with the appropriate gep(gcroot)
 * for inst in entry-basic-block(function)
 *     if inst matches "gc-root"
 *         slot = get-argument(inst)
 *         newslot = CreateGEP(gc-frame) -> at InsertPoint(gc-frame)
 *         Replace(slot, newslot) -> at InsertPoint(gc-frame)
 *         CreateStore(NULL, newslot) -> at InsertPoint(gc-frame)
 */
#if JL_LLVM_VERSION >= 30600
    DIBuilder dbuilder(M, false);
#endif
    unsigned argSpaceSize = 0;
    for (BasicBlock::iterator I = gcframe->getParent()->begin(), E(gcframe); I != E; ) {
        Instruction* inst = &*I;
        ++I;
        if (CallInst* callInst = dyn_cast<CallInst>(inst)) {
            if (callInst->getCalledValue() == gcroot_func) {
                unsigned offset = 2 + argSpaceSize++;
                Instruction *argTempi = GetElementPtrInst::Create(LLVM37_param(NULL) gcframe, ArrayRef<Value*>(ConstantInt::get(T_int32, offset)));
                argTempi->insertAfter(last_gcframe_inst);
#if JL_LLVM_VERSION >= 30600
                Metadata *md = ValueAsMetadata::getIfExists(callInst);
                if (md) {
                    Value *mdValue = MetadataAsValue::get(M.getContext(), md);
                    for (User::use_iterator use = mdValue->use_begin(), usee = mdValue->use_end(); use != usee; ) {
                        // need to recreate the dbg_declare accordingly -- sadly llvm can't handle this in RAUW
                        User *user = use->getUser();
                        ++use;
                        if (CallInst* dbg = dyn_cast<CallInst>(user)) {
                            Function *called = dbg->getCalledFunction();
                            if (called && called->getIntrinsicID() == Intrinsic::dbg_declare) {
                                DILocalVariable *dinfo = cast<DILocalVariable>(cast<MetadataAsValue>(dbg->getOperand(1))->getMetadata());
                                DIExpression *expr = cast<DIExpression>(cast<MetadataAsValue>(dbg->getOperand(2))->getMetadata());
                                SmallVector<uint64_t, 8> addr;
                                addr.push_back(llvm::dwarf::DW_OP_plus);
                                addr.push_back(offset * sizeof(void*));
                                addr.append(expr->elements_begin(), expr->elements_end());
                                expr = dbuilder.createExpression(addr);
                                dbuilder.insertDeclare(gcframe, dinfo, expr,
#if JL_LLVM_VERSION >= 30700
                                                dbg->getDebugLoc(),
#endif
                                                dbg->getParent());
                                dbg->eraseFromParent();
                            }
                        }
                    }
                }
#endif
                tbaa_decorate_gcframe(callInst, tbaa_gcframe);
                callInst->replaceAllUsesWith(argTempi);
                argTempi->takeName(callInst);
                callInst->eraseFromParent();
                // Initialize the slots for function variables to NULL
                StoreInst *store = new StoreInst(V_null, argTempi);
                store->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
                store->insertAfter(argTempi);
                last_gcframe_inst = store;
            }
        }
        else if (AllocaInst *allocaInst = dyn_cast<AllocaInst>(inst)) {
            if (allocaInst->getAllocatedType() == V_null->getType()) {
                // TODO: this is overly aggressive at zeroing allocas that may not actually need to be zeroed
                StoreInst *store = new StoreInst(V_null, allocaInst);
                store->insertAfter(allocaInst);
            }
        }
    }
#if JL_LLVM_VERSION >= 30600
    dbuilder.finalize();
#endif

    if (argSpaceSize + maxDepth == 0) {
        // 0 roots; remove gc frame entirely
        gcframe->eraseFromParent();
    }
    else {
        // Initialize the slots for temporary variables to NULL
        if (maxDepth > 0) {
            BitCastInst *tempSlot_i8 = new BitCastInst(tempSlot, PointerType::get(T_int8, 0), "", last_gcframe_inst);
            Type *argsT[2] = {tempSlot_i8->getType(), T_int32};
            Function *memset = Intrinsic::getDeclaration(&M, Intrinsic::memset, makeArrayRef(argsT));
            Value *args[5] = {
                tempSlot_i8, // dest
                ConstantInt::get(T_int8, 0), // val
                ConstantInt::get(T_int32, sizeof(jl_value_t*)*maxDepth), // len
                ConstantInt::get(T_int32, 0), // align
                ConstantInt::get(T_int1, 0)}; // volatile
            CallInst *zeroing = CallInst::Create(memset, makeArrayRef(args));
            zeroing->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
            zeroing->insertAfter(tempSlot_i8);
            last_gcframe_inst = zeroing;
        }

        gcframe->setOperand(0, ConstantInt::get(T_int32, 2 + argSpaceSize + maxDepth)); // fix up the size of the gc frame
        if (tempSlot)
            tempSlot->setOperand(1, ConstantInt::get(T_int32, 2 + argSpaceSize)); // fix up the offset to the temp slot space

        IRBuilder<> builder(F.getContext());
        Type *T_ppjlvalue = V_null->getType()->getPointerTo();
#ifdef _P64
        Type *T_size = T_int64;
#else
        Type *T_size = T_int32;
#endif
        builder.SetInsertPoint(&*(++BasicBlock::iterator(last_gcframe_inst))); // set insert *before* point, e.g. after the gcframe
        DebugLoc noDbg;
        builder.SetCurrentDebugLocation(noDbg);

        Instruction *inst =
            builder.CreateStore(ConstantInt::get(T_size, (argSpaceSize + maxDepth) << 1),
                                builder.CreateBitCast(builder.CreateConstGEP1_32(gcframe, 0), T_size->getPointerTo()));
        inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
        inst = builder.CreateStore(builder.CreateLoad(builder.Insert(get_pgcstack(ptlsStates))),
                                   builder.CreatePointerCast(builder.CreateConstGEP1_32(gcframe, 1), PointerType::get(T_ppjlvalue,0)));
        inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
        builder.CreateStore(gcframe, builder.Insert(get_pgcstack(ptlsStates)));

        // Finish by emitting the gc pops before any return
        for(Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
            if (isa<ReturnInst>(I->getTerminator())) {
                builder.SetInsertPoint(I->getTerminator()); // set insert *before* Ret
                Instruction *gcpop =
                    (Instruction*)builder.CreateConstGEP1_32(gcframe, 1);
                inst = builder.CreateLoad(gcpop);
                inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
                inst = builder.CreateStore(builder.CreatePointerCast(inst, T_ppjlvalue),
                                           builder.Insert(get_pgcstack(ptlsStates)));
                inst->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_gcframe);
            }
        }
    }

#ifndef NDEBUG
    jl_gc_frame_stats.count++;
    jl_gc_frame_stats.locals += argSpaceSize;
    jl_gc_frame_stats.temp += maxDepth;
#endif
}

struct LowerGCFrame: public ModulePass {
    static char ID;
    LowerGCFrame() : ModulePass(ID)
    {}

private:
    void runOnFunction(Module *M, Function &F, Function *ptls_getter,
                       Type *T_pjlvalue, MDNode *tbaa_gcframe);
    bool runOnModule(Module &M) override;
};

static void eraseFunction(Module &M, const char *name)
{
    if (Function *f = M.getFunction(name)) {
        f->eraseFromParent();
    }
}

static void ensure_enter_function(Module &M)
{
    auto T_int8  = Type::getInt8Ty(M.getContext());
    auto T_pint8 = PointerType::get(T_int8, 0);
    auto T_void = Type::getVoidTy(M.getContext());
    auto T_int32 = Type::getInt32Ty(M.getContext());
    if (!M.getNamedValue("jl_enter_handler")) {
        std::vector<Type*> ehargs(0);
        ehargs.push_back(T_pint8);
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage, "jl_enter_handler", &M);
    }
    if (!M.getNamedValue(jl_setjmp_name)) {
        std::vector<Type*> args2(0);
        args2.push_back(T_pint8);
#ifndef _OS_WINDOWS_
        args2.push_back(T_int32);
#endif
        Function::Create(FunctionType::get(T_int32, args2, false),
                         Function::ExternalLinkage, jl_setjmp_name, &M)
            ->addFnAttr(Attribute::ReturnsTwice);
    }
}

bool LowerGCFrame::runOnModule(Module &M)
{
    MDNode *tbaa_gcframe = tbaa_make_child("jtbaa_gcframe").first;

    Function *ptls_getter = M.getFunction("jl_get_ptls_states");
    ensure_enter_function(M);
    FunctionType *functype = nullptr;
    Type *T_pjlvalue = nullptr;
    if (ptls_getter) {
        functype = ptls_getter->getFunctionType();
        auto T_ppjlvalue =
            cast<PointerType>(functype->getReturnType())->getElementType();
        T_pjlvalue = cast<PointerType>(T_ppjlvalue)->getElementType();
    }
    for (auto F = M.begin(), E = M.end(); F != E; ++F) {
        if (F->isDeclaration())
            continue;
        runOnFunction(&M, *F, ptls_getter, T_pjlvalue, tbaa_gcframe);
    }

    // Cleanup for GC frame lowering.
    eraseFunction(M, "julia.gc_root_decl");
    eraseFunction(M, "julia.gc_root_kill");
    eraseFunction(M, "julia.jlcall_frame_decl");
    eraseFunction(M, "julia.gcroot_flush");
    eraseFunction(M, "julia.except_enter");
    return true;
}

void LowerGCFrame::runOnFunction(Module *M, Function &F, Function *ptls_getter,
                                 Type *T_pjlvalue, MDNode *tbaa_gcframe)
{
    CallInst *ptlsStates = nullptr;
    for (auto I = F.getEntryBlock().begin(), E = F.getEntryBlock().end();
         ptls_getter && I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledValue() == ptls_getter) {
                ptlsStates = callInst;
                break;
            }
        }
    }
    JuliaGCAllocator allocator(F, ptlsStates, T_pjlvalue, tbaa_gcframe);
    allocator.allocate_frame();
}

char LowerGCFrame::ID = 0;

static RegisterPass<LowerGCFrame> X("LowerGCFrame", "Lower GCFrame Pass",
                                    false /* Only looks at CFG */,
                                    false /* Analysis Pass */);
}

#ifndef NDEBUG // llvm assertions build
// gdb debugging code for inspecting the bb_uses map
void jl_dump_bb_uses(std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses)
{
    for (std::map<BasicBlock*, std::map<frame_register, liveness::id> >::iterator
            live_reg = bb_uses.begin(), e = bb_uses.end(); live_reg != e; ++live_reg) {
        BasicBlock *bb = live_reg->first;
        errs() << '\n' << bb << '\n';
        for (std::map<frame_register, liveness::id>::iterator
                regs = live_reg->second.begin(), regse = live_reg->second.end(); regs != regse; ++regs) {
            errs() << regs->second << " #" << regs->first.second << ' ' << regs->first.first << '\n';
        }
    }
}
#endif

Pass *createLowerGCFramePass()
{
    return new LowerGCFrame();
}

extern "C" JL_DLLEXPORT
void LLVMAddLowerGCFramePass(LLVMPassManagerRef PM) {
    unwrap(PM)->add(createLowerGCFramePass());
}
