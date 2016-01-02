#include "llvm-version.h"
#include <vector>
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

using namespace llvm;
extern Function *gcroot_func;
extern Function *gckill_func;
extern Function *jlcall_frame_func;
extern Function *jlcall_root_func;

typedef std::pair<CallInst*, unsigned> frame_register;
class liveness {
public:
    typedef unsigned id;
    enum {
        assign = 1<<0,
        kill   = 1<<1,
        live   = 1<<2
    };
};

#ifdef DEBUG // llvm assertions build
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

static bool record_usage(CallInst *callInst,
        std::map<BasicBlock*, std::map<frame_register, liveness::id> > &bb_uses,
        std::map<BasicBlock*, SmallBitVector> &regs_used,
        unsigned offset, bool commit=true)
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
    SmallBitVector &regs = regs_used[&callInst->getParent()->getParent()->getEntryBlock()];
    if (regs.size() < offset + arg_n)
        regs.resize(offset + arg_n);
    for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
        frame_register def(callInst, arg_offset);
        unsigned index = offset + arg_offset;
        bool conflict = regs.test(index);
        if (commit) {
            assert(!conflict);
            regs.set(index);
        } else if (conflict) {
            return false;
        }
    }
#else // better allocator that uses per-basicblock liveness
    for (std::map<BasicBlock*, std::map<frame_register, liveness::id> >::iterator
            live_reg = bb_uses.begin(), e = bb_uses.end(); live_reg != e; ++live_reg) {
        BasicBlock *bb = live_reg->first;
        SmallBitVector &regs = regs_used[bb];
        if (regs.size() < offset + arg_n)
            regs.resize(offset + arg_n);
        for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
            frame_register def(callInst, arg_offset);
            std::map<frame_register, liveness::id>::iterator inuse_reg = live_reg->second.find(def);
            if (inuse_reg == live_reg->second.end())
                continue;
            // TODO: optimize here better when not live in inuse_reg->second, by ascertaining liveness at the instruction level for this bb
            unsigned index = offset + arg_offset;
            bool conflict = regs.test(index);
            if (commit) {
                assert(!conflict);
                regs.set(index);
            } else if (conflict) {
                // TODO: updating offset to point to the next open register > index may help accelerate this loop and avoid unnecessary work
                return false;
            }
        }
    }
#endif
    return true;
}

static unsigned find_space_for(CallInst *callInst,
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
    while (!record_usage(callInst, bb_uses, regs_used, n, false))
        ++n;
    return n;
}

void jl_codegen_finalize_temp_arg(AllocaInst *gcframe, Instruction *&last_gcframe_inst, unsigned &argSpaceSize, unsigned &maxDepth)
{
/* Algorithm sketch:
 *  Compute liveness for each basic block
 *    liveness computed at the basic-block level for <inst, arg-offset> pairs
 *  Propagate liveness from each basic block to its predecessors
 *  Allocate argument slot for each jlcall frame
 */
    Function &F = *gcframe->getParent()->getParent();
    Type *T_int32 = Type::getInt32Ty(F.getContext());
    Value *V_null = Constant::getNullValue(gcframe->getType()->getPointerElementType());
    last_gcframe_inst = gcframe;

/* bb-queue : queue<BB>
 * bb-uses : map<BB, map< pair<inst, arg-offset>, assign|live|kill > >
 * for bb in iterator(f)
 *     inuse-list ; map< pair<inst, arg-offset>, assign|live|kill >
 *     for inst in reverse-iterator(f)
 *         if inst matches "a call to make-jlcall-frame"
 *            # note that below this logic is rearranged slightly in the implementation so that failure to match an argument IR is "safe"
 *            for (arg-offset, operand) in enumerate(arguments(inst))
 *                if (not hasgcroot(operand)) or (users(GC-Root(operand)) is not only operand and store-instructions)
 *                    inuse-list[<inst, arg-offset>] = (assign|kill)
 *                else # e.g. this is the only use, replace gc-root with direct store to arg-slot
 *                    ReplaceOperand(GC-Root(operand), "a call to jlcall-root", pair<inst, arg-offset>)
 *                    inuse-list[<inst, arg-offset>] = kill
 *         else if inst matches store-inst with op(1) matching "a call to jlcall-root"
 *             def = <inst, arg-offset>
 *             if inuse-list[def] is kill
 *                 inuse-list[def] = assign|kill
 *      bb-uses[bb] = inuse-list
 *      if not has-live-out(bb)
 *          continue
 *      for pred in predecessors(bb)
 *          if not pred in bb-queue
 *              push-back(bb-queue, pred)
 * GC-Root(load-gcroot-inst) = get-argument(load-gcroot-inst)
 * hasgcroot(inst) = inst matches "CreateLoad(CallInst::Create(gcroot_func))"
 */

    std::vector<BasicBlock*> bb_queue;
    std::map<BasicBlock*, std::map<frame_register, liveness::id> > bb_uses;
    for (Function::iterator bb = F.begin(), be = F.end(); bb != be; ++bb) {
        // TODO: this assumes that the kill bb (aka the jlcall) is the same as the store and the frame allocation (aka jlcall_frame_func)
        // which is currently a safe assumption due to the specific behavior of `make_jlcall` and `emit_jlcall`, but may not be sufficiently general
        std::map<frame_register, liveness::id> &inuse_list = bb_uses[bb];
        unsigned live_out = 0;
        for (BasicBlock::iterator ri = bb->end(); ri != bb->begin(); ) {
            Instruction *i = &*--ri;
            if (CallInst* callInst = dyn_cast<CallInst>(i)) {
                if (callInst->getCalledFunction() == jlcall_frame_func) {
                    unsigned arg_n = cast<ConstantInt>(callInst->getArgOperand(0))->getZExtValue();
                    for (unsigned arg_offset = 0; arg_offset < arg_n; ++arg_offset) {
                        liveness::id &live = inuse_list[frame_register(callInst, arg_offset)];
                        if (!live)
                            live = liveness::kill | liveness::assign;
                    }
                }
                if (callInst->getCalledFunction() == gckill_func) {
                    // TODO: track variable liveness information starting from gckill, store-to-gcroot, and store-to-jlcall-frame
                }
            }
            else if (StoreInst *storeInst = dyn_cast<StoreInst>(i)) {
                CallInst *callInst = dyn_cast<CallInst>(storeInst->getPointerOperand());
                unsigned arg_offset = 0;
                if (!callInst) {
                    // also try to look through GEP
                    if (GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(storeInst->getPointerOperand())) {
                        if (gepInst->getNumIndices() == 1) {
                            callInst = dyn_cast<CallInst>(gepInst->getPointerOperand());
                            if (callInst && callInst->getCalledFunction() == jlcall_frame_func) {
                                arg_offset = cast<ConstantInt>(gepInst->idx_begin()->get())->getZExtValue();
                            }
                        }
                    }
                }
                if (callInst && callInst->getCalledFunction() == jlcall_frame_func) {
                    LoadInst *loadInst = dyn_cast<LoadInst>(storeInst->getValueOperand());
                    CallInst *gcroot = NULL;
                    if (loadInst && loadInst->hasOneUse()) {
                        gcroot = dyn_cast<CallInst>(loadInst->getPointerOperand());
                        if (gcroot && gcroot->getCalledFunction() != gcroot_func)
                            gcroot = NULL;
                    }
                    liveness::id live = liveness::kill;
                    if (gcroot == NULL) {
                        live |= liveness::assign;
                    }
                    else {
                        for (User::use_iterator use = gcroot->use_begin(), usee = gcroot->use_end(); use != usee; ++use) {
                            User *user = use.getUse().getUser();
                            if (user != loadInst && !(isa<StoreInst>(user) && use.getOperandNo() == StoreInst::getPointerOperandIndex())) {
                                gcroot = NULL;
                                live |= liveness::assign;
                                break;
                            }
                        }
                    }
                    inuse_list[frame_register(callInst, arg_offset)] = live;
                    if (gcroot) {
                        Value* args[2] = {callInst, ConstantInt::get(T_int32, arg_offset)};
                        ReplaceInstWithInst(gcroot, CallInst::Create(jlcall_root_func, makeArrayRef(args)));
                        ++ri;
                        storeInst->eraseFromParent();
                        loadInst->eraseFromParent();
                        ++live_out;
                    }
                }
                else if (callInst && callInst->getCalledFunction() == jlcall_root_func) {
                    assert(arg_offset == 0);
                    frame_register def(
                            cast<CallInst>(callInst->getArgOperand(0)),
                            cast<ConstantInt>(callInst->getArgOperand(1))->getZExtValue());
                    std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                    if (inuse_reg != inuse_list.end() && inuse_reg->second == liveness::kill) {
                        inuse_reg->second |= liveness::assign;
                        --live_out;
                    }
                }
            }
        }
        if (!live_out)
            continue;
        assert(&*bb != &F.getEntryBlock()); // nothing should live-out from the entry bb
        for (pred_iterator PI = pred_begin(bb), PE = pred_end(bb); PI != PE; ++PI) {
            if (std::find(bb_queue.begin(), bb_queue.end(), *PI) == bb_queue.end())
                bb_queue.push_back(*PI);
        }
    }

/* while not empty(bb-queue)
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
 *         if inst matches store-inst with op(1) matching "a call to jlcall-root"
 *             def = <inst, arg-offset>
 *             if live in inuse-list[def]
 *                 inuse-list[def] |= assign
 *                 if not kill in inuse-list[def]
 *                      # found the assignment, def is no longer live
 *                      inuse-list[def] &= ~live
 *                 else
 *                      # not a true kill due to recursion -- the kill happened before this assign in this BB, so it is still live
 *                 changes -= 1
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
                if (CallInst *callInst = dyn_cast<CallInst>(storeInst->getPointerOperand())) {
                    if (callInst->getCalledFunction() == jlcall_root_func) {
                        frame_register def(
                                cast<CallInst>(callInst->getArgOperand(0)),
                                cast<ConstantInt>(callInst->getArgOperand(1))->getZExtValue());
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
        assert(bb != &F.getEntryBlock()); // nothing should live-out from the entry bb
        for (pred_iterator PI = pred_begin(bb), PE = pred_end(bb); PI != PE; ++PI) {
            if (std::find(bb_queue.begin(), bb_queue.end(), *PI) == bb_queue.end())
                bb_queue.push_back(*PI);
        }
    }

    Instruction *tempSlot = GetElementPtrInst::Create(gcframe, ArrayRef<Value*>(ConstantInt::get(T_int32, 2)));
#ifdef JL_DEBUG_BUILD
    tempSlot->setName("temproots");
#endif
    tempSlot->insertAfter(gcframe);
    if (last_gcframe_inst == gcframe)
        last_gcframe_inst = tempSlot;

/* # allocate space in locals for the variables
 * TBD
 */
    for (BasicBlock::iterator I = gcframe->getParent()->begin(), E = gcframe; I != E; ) {
        CallInst* callInst = dyn_cast<CallInst>(&*I);
        ++I;
        if (callInst && callInst->getCalledFunction() == gcroot_func) {
            // see if a root is only used briefly for `store -> load -> store other` pattern or `store, store other`
            // such that the first store can be trivially replaced with just "other" and delete the chain
            // or if is used for store, but the value is never needed
            StoreInst *theStore = NULL;
            LoadInst *theLoad = NULL;
            for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ++use) {
                // see if the gcroot uses consists of only the load and the store
                User *user = use.getUse().getUser();
                if (StoreInst *storeInst = dyn_cast<StoreInst>(user)) {
                    if (theStore) {
                        theStore = NULL;
                        break;
                    }
                    theStore = storeInst;
                }
                else if (LoadInst *loadInst = dyn_cast<LoadInst>(user)) {
                    if (theLoad) {
                        theStore = NULL;
                        break;
                    }
                    theLoad = loadInst;
                }
                else {
                    theStore = NULL;
                    break;
                }
            }
            if (theStore) {
                Value *theValue = theLoad ? theLoad : theStore->getValueOperand();
                if (!theLoad && theValue->hasOneUse()) {
                    // this gcroot is unused (the only Use of theValue is theStore)
                    if (&*I == theStore) ++I;
                    theStore->eraseFromParent();
                    callInst->eraseFromParent();
                }
                else if (theValue->hasNUses(theLoad ? 1 : 2)) {
                    StoreInst *theOther = NULL;
                    bool patternMatchSuccess = false;
                    // check if this value is only used for a store to another gcroot
                    User::use_iterator value_use = theValue->use_begin();
                    if (theLoad && *value_use == theStore)
                        ++value_use;
                    theOther = dyn_cast<StoreInst>(value_use.getUse().getUser());
                    if (theOther && value_use.getOperandNo() != StoreInst::getPointerOperandIndex()) {
                        // test whether this store is valid as a gc-root
                        CallInst *gcroot_other = dyn_cast<CallInst>(theOther->getPointerOperand());
                        unsigned arg_offset = 0;
                        if (!gcroot_other) {
                            // also try to look through GEP for jlcall_frame_func
                            if (GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(theOther->getPointerOperand())) {
                                if (gepInst->getNumIndices() == 1) {
                                    gcroot_other = dyn_cast<CallInst>(gepInst->getPointerOperand());
                                    if (gcroot_other && gcroot_other->getCalledFunction() == jlcall_frame_func)
                                        arg_offset = cast<ConstantInt>(gepInst->idx_begin()->get())->getZExtValue();
                                    else
                                        gcroot_other = NULL;
                                }
                            }
                        }
                        // it could be a gcroot
                        if (gcroot_other && gcroot_other->getCalledFunction() == gcroot_func) {
                            // need to make sure there aren't any other uses of gcroot_other (including gckill)
                            // between the initial store and the replacement store
                            BasicBlock *current = theStore->getParent();
                            BasicBlock::iterator bbi = theStore, bbi_end = current->end();
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
#ifdef LLVM35
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
                        // or it could be a jlcall frame
                        else if (gcroot_other && gcroot_other->getCalledFunction() == jlcall_frame_func) {
                            // only one store to a jlcall_frame_func slot exists now,
                            // until the allocate space in temp-args for each jlcall frame
                            // but do need to update liveness information for this slot
                            // TODO: do this better once we have liveness information for locals
                            if (theOther->getParent() == theStore->getParent()) {
                                //frame_register def(gcroot_other, arg_offset);
                                //std::map<frame_register, liveness::id> &inuse_list = bb_uses[theOther->getParent()];
                                //std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                                patternMatchSuccess = true;
                            }
                        }
                    }
                    if (patternMatchSuccess) {
                        // do the gcroot merge
                        theStore->setOperand(StoreInst::getPointerOperandIndex(), theOther->getPointerOperand());
                        if (&*I == theOther) ++I;
                        theOther->eraseFromParent();
                        if (theLoad) {
                            if (&*I == theLoad) ++I;
                            theLoad->eraseFromParent();
                        }
                        callInst->eraseFromParent();
                    }
                }
            }
        }
    }

/* # allocate space in temp-args for each jlcall frame
 * regs-used = zip(get-basic-blocks(), falses)
 * for bb in iterator(f)
 *     for inst in iterator(bb)
 *         if inst matches "a call to make-jlcall-frame"
 *             frame-offset = find-space-for(inst, bb-uses, regs-used)
 *             record-usage(inst, bb-uses, regs-used, frame-offset)
 */
    std::map<BasicBlock*, SmallBitVector> regs_used;
    std::map<CallInst*, unsigned> frames;
    maxDepth = 0;
    // TODO: it is probably more optimal to allocate these from largest to smallest
    for (Function::iterator bb = F.begin(), be = F.end(); bb != be; ++bb) {
        for (BasicBlock::iterator i = bb->begin(), ie = bb->end(); i != ie; ++i) {
            if (CallInst* callInst = dyn_cast<CallInst>(&*i)) {
                if (callInst->getCalledFunction() == jlcall_frame_func) {
                    unsigned arg_n = cast<ConstantInt>(callInst->getArgOperand(0))->getZExtValue();
                    unsigned frame_offset = find_space_for(callInst, bb_uses, regs_used);
                    record_usage(callInst, bb_uses, regs_used, frame_offset);
                    frames[callInst] = frame_offset;
                    if (frame_offset + arg_n > maxDepth)
                        maxDepth = frame_offset + arg_n;
                }
            }
        }
    }

    // delete the now unused gckill information
    for (Function::iterator bb = F.begin(), be = F.end(); bb != be; ++bb) {
        for (BasicBlock::iterator i = bb->begin(), ie = bb->end(); i != ie; ) {
            Instruction *inst = &*i;
            ++i;
            if (CallInst* callInst = dyn_cast<CallInst>(inst)) {
                if (callInst->getCalledFunction() == gckill_func) {
                    callInst->eraseFromParent();
                }
            }
        }
    }

/* replace all intermediate roots defs with the appropriate gep(gcroot) */
    /* for inst in entry-basic-block(function)
     *     if inst matches "jlcall_root_func" or "gc-root"
     *         slot = get-argument(inst)
     *         newslot = CreateGEP(gc-frame) -> at InsertPoint(gc-frame)
     *         Replace(slot, newslot) -> at InsertPoint(gc-frame)
     *         CreateStore(NULL, newslot) -> at InsertPoint(gc-frame)
     */
    argSpaceSize = 0;
    Instruction *argSlot = NULL;
    for(BasicBlock::iterator I = gcframe->getParent()->begin(), E = gcframe; I != E; ) {
        CallInst* callInst = dyn_cast<CallInst>(&*I);
        ++I;
        if (callInst && callInst->getCalledFunction() == jlcall_root_func) {
            frame_register def(
                    cast<CallInst>(callInst->getArgOperand(0)),
                    cast<ConstantInt>(callInst->getArgOperand(1))->getZExtValue());
            unsigned frame_offset = frames.at(def.first);
            Value* offset[1] = {ConstantInt::get(T_int32, frame_offset + def.second)};
            GetElementPtrInst *frame = GetElementPtrInst::Create(tempSlot, makeArrayRef(offset));
            frame->insertAfter(last_gcframe_inst);
            callInst->replaceAllUsesWith(frame);
            callInst->eraseFromParent();
            last_gcframe_inst = frame;

            // delete any associated StoreInst that aren't live
            for (User::use_iterator use = callInst->use_begin(), usee = callInst->use_end(); use != usee; ++use) {
                User *user = use.getUse().getUser();
                if (StoreInst *storeInst = dyn_cast<StoreInst>(user)) {
                    std::map<frame_register, liveness::id> &inuse_list = bb_uses[storeInst->getParent()];
                    std::map<frame_register, liveness::id>::iterator inuse_reg = inuse_list.find(def);
                    if (inuse_reg == inuse_list.end())
                        storeInst->eraseFromParent();
                }
            }
        }

        if (callInst && callInst->getCalledFunction() == gcroot_func) {
            if (!argSlot) {
                argSlot = GetElementPtrInst::Create(gcframe, ArrayRef<Value*>(ConstantInt::get(T_int32, 2)));
#ifdef JL_DEBUG_BUILD
                argSlot->setName("locals");
#endif
                argSlot->insertAfter(gcframe);
                if (last_gcframe_inst == gcframe)
                    last_gcframe_inst = argSlot;
            }
            Instruction *argTempi = GetElementPtrInst::Create(argSlot, ArrayRef<Value*>(ConstantInt::get(T_int32, argSpaceSize++)));
            argTempi->insertAfter(last_gcframe_inst);
            callInst->replaceAllUsesWith(argTempi);
            callInst->eraseFromParent();
            // Initialize the slots for function variables to NULL
            StoreInst *store = new StoreInst(V_null, argTempi);
            store->insertAfter(argTempi);
            last_gcframe_inst = store;
        }
    }

    // Initialize the slots for temporary variables to NULL
    for (int i = 0; i < maxDepth; i++) {
        Instruction *argTempi = GetElementPtrInst::Create(tempSlot, ArrayRef<Value*>(ConstantInt::get(T_int32, i)));
        argTempi->insertAfter(last_gcframe_inst);
        StoreInst *store = new StoreInst(V_null, argTempi);
        store->insertAfter(argTempi);
        last_gcframe_inst = store;
    }
    gcframe->setOperand(0, ConstantInt::get(T_int32, 2 + argSpaceSize + maxDepth)); // fix up the size of the gc frame
    tempSlot->setOperand(1, ConstantInt::get(T_int32, 2 + argSpaceSize)); // fix up the offset to the temp slot space

/* finalize all of the frames by replacing them with the appropriate gep(tempslot) */
    for (std::map<CallInst*, unsigned>::iterator frame = frames.begin(), framee = frames.end(); frame != framee; ++frame) {
        Value* offset[1] = {ConstantInt::get(T_int32, frame->second)};
        GetElementPtrInst *gep = GetElementPtrInst::Create(tempSlot, makeArrayRef(offset));
        ReplaceInstWithInst(frame->first, gep);
    }

#if 0
    static struct {
        unsigned count;
        unsigned locals;
        unsigned temp;
    } gc_frame_stats = {0};
    gc_frame_stats.count++;
    gc_frame_stats.locals += argSpaceSize;
    gc_frame_stats.temp += maxDepth;
#endif
}
