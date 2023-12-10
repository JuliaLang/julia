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
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
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
    bool cfgChanged = false;

    bool doInitialization(Module &m);
    bool runOnFunction(Function &F, FunctionAnalysisManager &AM);
};

struct Optimizer {
    Optimizer(Function &F, AllocOpt &pass, FunctionAnalysisManager &AM)
        : F(F),
          ORE(&F),
          pass(pass),
          AM(AM)
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

    void optimizeObject(CallInst *orig_inst, size_t sz);
    void optimizeArray(CallInst *orig_inst, jl_genericmemory_info_t info);

    void moveSizedBitsArrayToStack(CallInst *orig, jl_genericmemory_info_t info);
    void moveUnsizedBitsArrayToStack(CallInst *orig, jl_genericmemory_info_t info);
    void replaceBitsArrayUses(CallInst *orig, Value *conditional, Value *root, function_ref<Instruction*()> shell, Instruction *data);


    // for deoptimizing array allocations in errors (usually BoundsErrors)
    struct SunkenArray {
        Instruction *root; // technically also shell at this point
        Instruction *data;
        Instruction *loaded;
        Instruction *err_insertpt;

        struct Frame {
            Instruction *orig_i;
            Value::use_iterator next;
            size_t offset_frame; // index of frame with offset data in stack
            // dynamic offset of orig_i from data
            MapVector<Value *, APInt> variables;
            APInt constant;
            bool loaded;
        };
    };
    bool canDeoptimizeErrorBlocks(CallInst *orig);
    void sinkArrayDataPointer(CallInst *orig, DenseMap<BasicBlock *, SunkenArray> &sunken, Value *root, Instruction *data, LoadInst *load);

    Function &F;
    OptimizationRemarkEmitter ORE;
    AllocOpt &pass;
    DominatorTree *_DT = nullptr;
    FunctionAnalysisManager &AM;

    DominatorTree &getDomTree()
    {
        if (!_DT)
            _DT = &AM.getResult<DominatorTreeAnalysis>(F);
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
    DenseMap<CallInst*, jl_genericmemory_info_t> arrays;
    SmallVector<CallInst*,6> removed;
    AllocUseInfo use_info;
    AllocUseInfo array_data_info;
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
        if (isa<UnreachableInst>(bb.getTerminator()))
            continue;
        for (auto &I: bb) {
            pushInstruction(&I);
        }
    }
}

void Optimizer::optimizeObject(CallInst *orig, size_t sz) {
    checkInst(orig);
    if (use_info.escaped) {
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC allocation escaped " << ore::NV("GC Allocation", orig);
        });
        if (use_info.hastypeof)
            optimizeTag(orig);
        return;
    }
    if (use_info.returned) {
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC allocation was returned " << ore::NV("GC Allocation", orig);
        });
        if (use_info.hastypeof)
            optimizeTag(orig);
        return;
    }
    if (!use_info.addrescaped && !use_info.hasload && use_info.errorbbs.empty()
        && (!use_info.haspreserve || !use_info.refstore)) {
        REMARK([&]() {
            return OptimizationRemark(DEBUG_TYPE, "Dead Allocation", orig)
                << "GC allocation removed " << ore::NV("GC Allocation", orig);
        });
        // No one took the address, no one reads anything and there's no meaningful
        // preserve of fields (either no preserve/ccall or no object reference fields)
        // We can just delete all the uses.
        removeAlloc(orig);
        return;
    }
    bool has_ref = use_info.has_unknown_objref;
    bool has_refaggr = use_info.has_unknown_objrefaggr;
    for (auto memop: use_info.memops) {
        auto &field = memop.second;
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
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC allocation has unusual object reference, unable to move to stack " << ore::NV("GC Allocation", orig);
        });
        if (use_info.hastypeof)
            optimizeTag(orig);
        return;
    }
    if (!use_info.hasunknownmem && !use_info.addrescaped) {
        REMARK([&](){
            return OptimizationRemark(DEBUG_TYPE, "Stack Split Allocation", orig)
                << "GC allocation split on stack " << ore::NV("GC Allocation", orig);
        });
        // No one actually care about the memory layout of this object, split it.
        splitOnStack(orig);
        return;
    }
    if (!use_info.errorbbs.empty()) {
        REMARK([&](){
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC allocation has error " << ore::NV("GC Allocation", orig);
        });
        if (use_info.hastypeof)
            optimizeTag(orig);
        return;
    }
    REMARK([&](){
        return OptimizationRemark(DEBUG_TYPE, "Stack Move Allocation", orig)
            << "GC allocation moved to stack " << ore::NV("GC Allocation", orig);
    });
    // The object has no fields with mix reference access
    moveToStack(orig, sz, has_ref, use_info.allockind);
}

bool Optimizer::canDeoptimizeErrorBlocks(CallInst *orig) {
    SmallSet<LoadInst *, 4> data_pointers;
    for (auto &field : use_info.memops) {
        for (auto &acc : field.second.accesses) {
            assert(isa<LoadInst>(acc.inst) && "Should only have loads of array length/data");
        }
        if (field.first == 0) {
            if (field.second.size > pass.DL->getPointerSize()) {
                dbgs() << "Can't deoptimize error blocks because of large field 0\n";
                return false;
            }
        } else {
            assert(field.first == pass.DL->getPointerSize() && "Got a nonzero/non-data load?");
            assert(field.second.size == pass.DL->getPointerSize() && "Got a load of array data for less than pointer size?");
            for (auto &acc : field.second.accesses) {
                data_pointers.insert(cast<LoadInst>(acc.inst));
            }
        }
    }
    jl_alloc::EscapeAnalysisRequiredArgs required{array_data_info, check_stack, pass, *pass.DL};
    for (auto I : data_pointers) {
        LLVM_DEBUG(dbgs() << "Running escape analysis on " << *I << "\n");
        jl_alloc::runEscapeAnalysis(I, required, jl_alloc::EscapeAnalysisOptionalArgs().with_optimization_remark_emitter(&ORE));
        REMARK([&](){
            std::string suse_info;
            llvm::raw_string_ostream osuse_info(suse_info);
            array_data_info.dump(osuse_info);
            return OptimizationRemarkAnalysis(DEBUG_TYPE, "EscapeAnalysis", I) << "escape analysis for " << ore::NV("GC Allocation", I) << "\n" << ore::NV("UseInfo", osuse_info.str());
        });
        // This implicitly relies on PHI nodes of the data pointer escaping; if escape analysis every changes
        // to not do that, we'll need to update GEP offset calculations (probably everywhere)
        if (array_data_info.escaped) {
            REMARK([&]() {
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC allocation escaped " << ore::NV("GC Allocation", orig);
            });
            return false;
        }
        assert(!array_data_info.returned);
        assert(!array_data_info.hastypeof);
    }
    return true;
}

void Optimizer::moveSizedBitsArrayToStack(CallInst *orig, jl_genericmemory_info_t info) {
    auto length = orig->getArgOperand(1);
    auto align = orig->getRetAlign().valueOrOne();
    IRBuilder<> builder(&*F.getEntryBlock().getFirstInsertionPt());
    auto T_size = pass.DL->getIntPtrType(builder.getContext());
    auto data = builder.CreateAlloca(Type::getInt8Ty(builder.getContext()), length);
    data->setAlignment(align);
    data->takeName(orig);
    auto root = Constant::getNullValue(orig->getType()); // technically valid to root this
    Instruction *shell0 = nullptr;

    auto shell = [&]() -> Instruction * {
        if (shell0)
            return shell0;
        auto shellData = builder.CreateAlloca(Type::getInt8PtrTy(builder.getContext()), ConstantInt::get(T_size, 2));
        shellData->setAlignment(align);
        shellData->setName(data->getName() + ".shell_data");
        auto lenptr = builder.CreateBitCast(shellData, length->getType()->getPointerTo(shellData->getType()->getPointerAddressSpace()));
        builder.CreateAlignedStore(length, lenptr, align);
        auto dataptr = builder.CreateConstGEP1_64(Type::getInt8PtrTy(builder.getContext()), shellData, 1);
        builder.CreateAlignedStore(data, dataptr, Align(std::min(align.value(), (uint64_t) pass.DL->getPointerSize())));
        auto asc = builder.CreateAddrSpaceCast(shellData, Type::getInt8PtrTy(builder.getContext())->getPointerTo(0)); // pointer_to_objref always returns addrspace 0
        shell0 = cast<Instruction>(builder.CreateBitCast(asc, JuliaType::get_pjlvalue_ty(builder.getContext())));
        return shell0;
    };

    replaceBitsArrayUses(orig, nullptr, root, shell, data);
    orig->eraseFromParent();
}

void Optimizer::moveUnsizedBitsArrayToStack(CallInst *orig, jl_genericmemory_info_t info) {
    size_t maxStackAlloc = 512; // TODO parameterize by module flag/ctor param
    IRBuilder<> builder(&*F.getEntryBlock().getFirstInsertionPt());
    StringRef origName = orig->getName();
    auto T_size = pass.DL->getIntPtrType(builder.getContext());
    auto data = builder.CreateAlloca(Type::getInt8Ty(builder.getContext()), ConstantInt::get(T_size, maxStackAlloc));
    auto align = orig->getRetAlign().valueOrOne();
    data->setAlignment(align);
    data->setName(origName + ".stack_data");
    // don't store the length pointer yet, since it might not be computed here

    // this makes sure we update domtree when splitting the bb, so we preserve the analysis
    _DT = AM.getCachedResult<DominatorTreeAnalysis>(F);
    auto origBB = orig->getParent();
    builder.SetInsertPoint(orig);
    auto length = orig->getArgOperand(1);
    auto maxElements = ConstantInt::get(length->getType(), maxStackAlloc / info.elsize);
    auto tooBig = builder.CreateICmpUGT(length, maxElements);
    auto fallback = SplitBlockAndInsertIfThen(tooBig, orig, false, nullptr, _DT);
    pass.cfgChanged = true;
    fallback->getParent()->setName("stack_alloc_fallback");
    builder.SetInsertPoint(orig);
    auto ownerPhi = builder.CreatePHI(orig->getType(), 2);
    auto dataPhi = builder.CreatePHI(Type::getInt8PtrTy(builder.getContext()), 2);
    PHINode *shellPhi = nullptr;

    auto shell = [&]() -> Instruction * {
        if (shellPhi)
            return shellPhi;
        builder.SetInsertPoint(origBB->getTerminator());
        auto shellData = builder.CreateAlloca(Type::getInt8PtrTy(builder.getContext()), ConstantInt::get(T_size, 2));
        shellData->setAlignment(align);
        shellData->setName(data->getName() + ".shell_data");
        auto lenptr = builder.CreateBitCast(shellData, length->getType()->getPointerTo(shellData->getType()->getPointerAddressSpace()));
        builder.CreateAlignedStore(length, lenptr, align);
        auto dataptr = builder.CreateConstGEP1_64(Type::getInt8PtrTy(builder.getContext()), shellData, 1);
        builder.CreateAlignedStore(data, dataptr, Align(std::min(align.value(), (uint64_t) pass.DL->getPointerSize())));
        auto asc = builder.CreateAddrSpaceCast(shellData, Type::getInt8PtrTy(builder.getContext())->getPointerTo(0)); // pointer_to_objref always returns addrspace 0
        auto bc = builder.CreateBitCast(asc, JuliaType::get_pjlvalue_ty(builder.getContext()));
        builder.SetInsertPoint(ownerPhi);
        shellPhi = builder.CreatePHI(bc->getType(), 2);
        shellPhi->addIncoming(bc, origBB);
        return shellPhi;
    };

    // Replace all the uses now, before we make the original instruction conditional on array size
    replaceBitsArrayUses(orig, tooBig, ownerPhi, shell, dataPhi);

    orig->moveBefore(fallback);
    builder.SetInsertPoint(fallback);
    auto casted = builder.CreateBitCast(orig, Type::getInt8PtrTy(builder.getContext())->getPointerTo(orig->getType()->getPointerAddressSpace()));
    auto fallbackDataPtr = builder.CreateConstGEP1_64(Type::getInt8PtrTy(builder.getContext()), casted, 1, origName + ".fallback_data_ptr");
    auto fallbackData = builder.CreateAlignedLoad(Type::getInt8PtrTy(builder.getContext()), fallbackDataPtr, Align(pass.DL->getPointerSize()), origName + ".fallback_data");
    auto datacast = builder.CreateBitCast(data, Type::getInt8PtrTy(builder.getContext()));
    ownerPhi->addIncoming(Constant::getNullValue(orig->getType()), origBB);
    ownerPhi->addIncoming(orig, fallback->getParent());
    dataPhi->addIncoming(datacast, origBB);
    dataPhi->addIncoming(fallbackData, fallback->getParent());
    if (shellPhi) {
        assert(pass.pointer_from_objref_func);
        auto shell = builder.CreateCall(pass.pointer_from_objref_func, {orig});
        shellPhi->addIncoming(shell, fallback->getParent());
    }

    dataPhi->setName(origName + ".data");
    ownerPhi->takeName(orig);
    ownerPhi->getParent()->setName("allocated_array");
}

void Optimizer::sinkArrayDataPointer(CallInst *orig, DenseMap<BasicBlock *, SunkenArray> &sunken, Value *root, Instruction *data, LoadInst *load) {
    SmallVector<SunkenArray::Frame, 4> stack;
    auto BitWidth = pass.DL->getPointerSizeInBits();
    IRBuilder<> builder(orig->getContext());
    auto replace_error_use = [&](Use *use, Instruction *user, SunkenArray &sunk, SunkenArray::Frame &frame) {
        auto src = frame.loaded ? sunk.loaded : sunk.data;
        if (frame.constant.isZero() && frame.variables.empty()) {
            use->set(src);
            return;
        }
        auto addrspace = frame.loaded ? AddressSpace::Loaded : AddressSpace::Generic;
        builder.SetInsertPoint(user);
        // 0 or 13, both are legal to gep in directly
        Value *offset = ConstantInt::get(Type::getIntNTy(builder.getContext(), BitWidth), frame.constant);
        for (auto &var : stack[frame.offset_frame].variables) {
            Value *idx = var.first;
            auto multiplier = var.second;
            if (!multiplier.isOne()) {
                idx = builder.CreateMul(idx, ConstantInt::get(idx->getType(), multiplier), "", true, true); // geps implicitly have nuw nsw
            }
            offset = builder.CreateAdd(offset, idx, "", true, true); // geps implicitly have nuw nsw
        }
        auto bc = builder.CreateBitCast(src, Type::getInt8PtrTy(builder.getContext())->getPointerTo(addrspace));
        auto gep = builder.CreateInBoundsGEP(Type::getInt8Ty(builder.getContext()), bc, {offset});
        auto cast = builder.CreateBitCast(gep, use->get()->getType());
        use->set(cast);
    };
    auto replace_use = [&](Use *use) {
        auto &cur = stack.back();
        auto inst = cast<Instruction>(use->getUser());
        auto it = sunken.find(inst->getParent());
        if (it != sunken.end()) {
            replace_error_use(use, inst, it->second, cur);
            return;
        }
        // we don't actually need to replace any other uses;
        // we just need to follow them to see if they end up
        // in an error block.
        SunkenArray::Frame frame;
        frame.orig_i = inst;
        frame.loaded = cur.loaded;
        switch (inst->getOpcode()) {
            case Instruction::BitCast:
            case Instruction::AddrSpaceCast:
            {
                frame.offset_frame = cur.offset_frame;
                break;
            }
            // Nothing to do for these
            case Instruction::Load:
            case Instruction::Store:
            case Instruction::AtomicCmpXchg:
            case Instruction::AtomicRMW:
            {
                return;
            }
            case Instruction::GetElementPtr:
            {
                auto gep = cast<GetElementPtrInst>(inst);
                // Copy the current offsets to the new frame
                frame.variables = stack[cur.offset_frame].variables;
                frame.constant = stack[cur.offset_frame].constant;
                bool success = gep->collectOffset(*pass.DL, BitWidth, frame.variables, frame.constant);
                assert(success); // TODO this may not work on ARM with scalable vectors,
                // but for now let's just start with this
                frame.offset_frame = stack.size();
                break;
            }
            case Instruction::Call:
            {
                auto call = cast<CallInst>(inst);
                auto callee = call->getCalledOperand();
                // Pretty much the only function we'd care about is
                // gc_loaded, since everything else only applies to the
                // gc-tracked pointer (except for write barrier, which
                // doesn't apply since we're not stack allocating
                // arrays with gc-tracked pointers)
                if (callee != pass.gc_loaded_func)
                    return;
                assert(!frame.loaded);
                frame.loaded = true;
                break;
            }
            default:
            {
                llvm_dump(inst);
                llvm_unreachable("Unexpected instruction");
            }
        }
        frame.next = inst->use_begin();
        stack.push_back(frame);
    };
    if (load->use_empty()) {
        load->eraseFromParent();
        return;
    }
    while (true) {
        auto &cur = stack.back();
        auto use = &*cur.next;
        ++cur.next;
        replace_use(use);
        while (cur.next == cur.orig_i->use_end()) {
            stack.pop_back();
            if (stack.empty())
                return;
        }
    }
    load->replaceAllUsesWith(data);
    load->eraseFromParent();
}

void Optimizer::replaceBitsArrayUses(CallInst *alloc, Value *conditional, Value *root, function_ref<Instruction *()>shell, Instruction *data) {
    IRBuilder<> builder(alloc->getContext());
    auto type = alloc->getArgOperand(0);
    auto length = alloc->getArgOperand(1);
    auto align = alloc->getRetAlign().valueOrOne();
    DenseMap<BasicBlock *, SunkenArray> sunken;
    if (!use_info.errorbbs.empty()) {
        if (!pass.gc_loaded_func) {
            auto decl = get_gc_loaded_decl(builder.getContext());
            auto FC = alloc->getModule()->getOrInsertFunction("julia.gc_loaded", decl.first, decl.second);
            pass.gc_loaded_func = cast<Function>(FC.getCallee());
        }
        IRBuilder<> builder(data->getNextNode());
        auto loaded = builder.CreateCall(pass.gc_loaded_func, {root, data});
        for (auto bb : use_info.errorbbs) {
            auto sunk = alloc->clone();
            auto insertpt = &*bb->getFirstInsertionPt();
            Instruction *rootsunk;
            Instruction *datasunk;
            Instruction *loadedsunk;
            if (!conditional) {
                sunk->insertBefore(insertpt);
                builder.SetInsertPoint(insertpt);
                auto asc = builder.CreateAddrSpaceCast(sunk, PointerType::getWithSamePointeeType(cast<PointerType>(sunk->getType()), AddressSpace::Derived));
                auto bc = builder.CreateBitCast(asc, Type::getInt8PtrTy(builder.getContext())->getPointerTo(AddressSpace::Derived));
                auto gep = builder.CreateConstGEP1_64(Type::getInt8PtrTy(builder.getContext()), bc, 1);
                datasunk = builder.CreateAlignedLoad(Type::getInt8PtrTy(builder.getContext()), gep, Align(MinAlign(align.value(), pass.DL->getPointerSize())));
                rootsunk = sunk;
                loadedsunk = builder.CreateCall(pass.gc_loaded_func, {rootsunk, datasunk});
                // length must dominate here, since the alloc is not in a phi node,
                // length must dominate the allocation for obvious reasons,
                // and the allocation must dominate its uses (including those in this bb)
                // also we know nuw nsw because original alloc would have thrown if not, and no exc handlers
                auto memcpy_length = builder.CreateMul(length, alloc->getArgOperand(2), "", true, true); // 2 is elsize
                // TODO can i get alignment guarantees for these?
                builder.CreateMemCpy(loadedsunk, MaybeAlign(), loaded, MaybeAlign(), memcpy_length);
                builder.SetInsertPoint(insertpt);
            } else {
                assert(pass.cfgChanged); // should have changed it above to get the runtime alloc branch anyways
                _DT = AM.getCachedResult<DominatorTreeAnalysis>(F);
                auto term = SplitBlockAndInsertIfThen(conditional, insertpt, false, nullptr, _DT);
                term->getParent()->setName("sunk_alloc");
                pass.cfgChanged = true; // should have already been set, but just being consistent
                sunk->insertBefore(term);
                builder.SetInsertPoint(term);
                auto asc = builder.CreateAddrSpaceCast(sunk, PointerType::getWithSamePointeeType(cast<PointerType>(sunk->getType()), AddressSpace::Derived));
                auto sunkdata = builder.CreateBitCast(asc, Type::getInt8PtrTy(builder.getContext())->getPointerTo(AddressSpace::Derived));
                sunkdata = builder.CreateConstGEP1_64(Type::getInt8PtrTy(builder.getContext()), sunkdata, 1);
                sunkdata = builder.CreateAlignedLoad(Type::getInt8PtrTy(builder.getContext()), sunkdata, Align(MinAlign(align.value(), pass.DL->getPointerSize())));
                // we have to do this a second time because we need to do the memcpy inside the conditional
                auto sunkloaded = builder.CreateCall(pass.gc_loaded_func, {sunk, sunkdata});
                // length must dominate here, since the alloc is not in a phi node,
                // length must dominate the allocation for obvious reasons,
                // and the allocation must dominate its uses (including those in this bb)
                auto memcpy_length = builder.CreateMul(length, alloc->getArgOperand(2)); // 2 is elsize
                // TODO can i get alignment guarantees for these?
                builder.CreateMemCpy(sunkloaded, MaybeAlign(), loaded, MaybeAlign(), memcpy_length);
                builder.SetInsertPoint(insertpt);
                auto rootphi = builder.CreatePHI(alloc->getType(), 2);
                auto dataphi = builder.CreatePHI(Type::getInt8PtrTy(builder.getContext()), 2);
                rootsunk = rootphi;
                datasunk = dataphi;
                rootphi->addIncoming(root, bb);
                rootphi->addIncoming(sunk, term->getParent());
                dataphi->addIncoming(data, bb);
                dataphi->addIncoming(sunkdata, term->getParent());
                loadedsunk = builder.CreateCall(pass.gc_loaded_func, {rootsunk, datasunk});
            }
            sunken[bb] = {rootsunk, datasunk, loadedsunk, insertpt};
        }
    }
    // we need to replace all of the length/data accesses upfront, because in the case of an unsized array alloc
    // it's not legal to derive it from the phi node (may be nullptr)
    for (auto &field : use_info.memops) {
        for (auto &access : field.second.accesses) {
            assert(isa<LoadInst>(access.inst) && "Should only have loads of array length/data");
            auto load = cast<LoadInst>(access.inst);
            auto offset = access.offset;
            assert((offset == 0 || offset == pass.DL->getPointerSize()) && "Should only have loads of array length/data");
            builder.SetInsertPoint(load);
            if (offset == 0) {
                assert(load->getType()->isIntegerTy() && "Should only have loads of array length from offset 0");
                assert(load->getType()->getIntegerBitWidth() <= length->getType()->getIntegerBitWidth() && "Should only have loads of array length from offset 0");
                auto len = builder.CreateTrunc(length, load->getType()); // llvm may load a smaller int, but hopefully shouldn't go larger
                if (len != length) {
                    len->takeName(length);
                    if (auto I = dyn_cast<Instruction>(len))
                        if (auto leni = dyn_cast<Instruction>(length))
                            I->copyMetadata(*leni);
                }
                load->replaceAllUsesWith(len);
                load->eraseFromParent();
            } else {
                if (load->getType()->isIntegerTy()) {
                    assert(cast<IntegerType>(load->getType())->getBitWidth() == pass.DL->getPointerSizeInBits() && "Should only have loads of array data from offset 8");
                    auto p2i = builder.CreatePtrToInt(data, load->getType());
                    load->replaceAllUsesWith(p2i);
                    load->eraseFromParent();
                } else {
                    assert(load->getType()->isPointerTy() && "Should only have loads of array data from offset 8");
                    if (!sunken.empty()) {
                        sinkArrayDataPointer(alloc, sunken, root, data, load);
                        continue;
                    }
                    auto atype = PointerType::getWithSamePointeeType(cast<PointerType>(data->getType()), load->getType()->getPointerAddressSpace());
                    auto acast = builder.CreateAddrSpaceCast(data, atype);
                    auto bcast = builder.CreateBitCast(acast, load->getType());
                    load->replaceAllUsesWith(bcast);
                    load->eraseFromParent();
                }
            }
        }
    }

    while (!alloc->use_empty()) {
        auto &use = *alloc->use_begin();
        auto user = cast<Instruction>(use.getUser());
        auto it = sunken.find(user->getParent());
        if (it != sunken.end()) {
            auto &sunk = it->second;
            use.set(sunk.root);
            continue;
        }
        if (auto CI = dyn_cast<CallInst>(user)) {
            auto callee = CI->getCalledFunction();
            if (callee == pass.pointer_from_objref_func) {
                use.set(shell());
                continue;
            } else if (callee == pass.typeof_func) {
                use.set(type);
                continue;
            }
            if (CI->isArgOperand(&use)) {
                auto arg = CI->getArgOperandNo(&use);
                CI->removeParamAttr(arg, Attribute::NonNull); // can actually be null now
            }
        }
        use.set(root);
    }
}

void Optimizer::optimizeArray(CallInst *orig, jl_genericmemory_info_t info) {
    checkInst(orig);
    dbgs() << "checking array allocation\n";
    if (use_info.escaped) {
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC genericmemory allocation escaped " << ore::NV("GC GenericMemory Allocation", orig);
        });
        return;
    }
    if (use_info.returned) {
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC allocation was returned " << ore::NV("GC GenericMemory Allocation", orig);
        });
        return;
    }
    if (use_info.hasunknownmem) {
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC genericmemory allocation has weird IR uses " << ore::NV("GC GenericMemory Allocation", orig);
        });
        return;
    }
    if (info.zeroinit || info.isunion || info.isboxed) {
        // This is a hack to detect arrays of possibly-pointers, which must always be zero initialized.
        // TODO actually support this maybe?
        // I think in the future we may be able to support arrays of pointers by hooking into
        // the gc roots alloca and adding our own "roots" (stack-allocated pointer arrays)
        // We can technically do exactly one pointer-isbits array as well since the flag
        // bits are at the end of the array (so we pretend the roots array is shorter than
        // it actually is in late-gc-lowering), but that won't scale to multiple of those.
        REMARK([&]() {
            return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                << "GC genericmemory allocation is probably a pointer array " << ore::NV("GC GenericMemory Allocation", orig);
        });
        return;
    }
    if (!use_info.errorbbs.empty()) {
        if (!canDeoptimizeErrorBlocks(orig)) {
            REMARK([&]() {
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC genericmemory allocation has error " << ore::NV("GC GenericMemory Allocation", orig);
            });
            return;
        }
    }
    // at this point the only valid real operations on orig are loading the length,
    // loading the data pointer, and getting a tracked pointer via gc_loaded.
    // we will assume that if the data pointer or a tracked pointer escapes, then the
    // original array must have also escaped, and therefore we would not have gotten here.

    // since we are here, we're free to turn the whole thing into a stack allocation
    // and remove the original allocation.
    dbgs() << "Moving array allocation to stack\n";
    if (isa<ConstantInt>(orig->getArgOperand(1))) {
        size_t maxSizedStackBytes = 4096; // TODO parameterize by module flag/ctor param
        auto length = orig->getArgOperand(1);
        auto ilen = cast<ConstantInt>(length)->getZExtValue();
        size_t bytes = jl_genericmemory_bytesize(&info, ilen);
        if (bytes > maxSizedStackBytes) {
            REMARK([&]() {
                return OptimizationRemarkMissed(DEBUG_TYPE, "Escaped", orig)
                    << "GC genericmemory allocation size is too large " << ore::NV("GC GenericMemory Allocation", orig);
            });
            return;
        }
        dbgs() << "allocation was sized\n";
        moveSizedBitsArrayToStack(orig, info);
    } else {
        dbgs() << "allocation was unsized\n";
        moveUnsizedBitsArrayToStack(orig, info);
    }
}

void Optimizer::optimizeAll()
{
    while (!worklist.empty()) {
        auto item = worklist.pop_back_val();
        auto orig = item.first;
        size_t sz = item.second;
        optimizeObject(orig, sz);
    }
    for (auto &item: arrays) {
        optimizeArray(item.first, item.second);
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
    if (!call->getCalledOperand())
        return -1;
    if (call->getCalledOperand() == pass.alloc_obj_func) {
        assert(call->arg_size() == 3);
        if (auto CI = dyn_cast<ConstantInt>(call->getArgOperand(1))) {
            size_t sz = (size_t)CI->getZExtValue();
            if (sz < IntegerType::MAX_INT_BITS / 8 && sz < INT32_MAX)
                return sz;
        }
    }
    if (call->getCalledOperand() == pass.alloc_genericmemory_func) {
        assert(call->arg_size() == 6);
        if (auto CI = dyn_cast<ConstantInt>(call->getArgOperand(2))) {
            size_t elsz = (size_t)CI->getZExtValue();
            if (elsz != 0) {
                auto isunion = dyn_cast<ConstantInt>(call->getArgOperand(3));
                auto zeroinit = dyn_cast<ConstantInt>(call->getArgOperand(4));
                auto isboxed = dyn_cast<ConstantInt>(call->getArgOperand(5));
                if (isunion && zeroinit && isboxed) {
                    jl_genericmemory_info_t info{elsz, (uint8_t)isunion->getZExtValue(),
                                                 (uint8_t)zeroinit->getZExtValue(),
                                                 (uint8_t)isboxed->getZExtValue()};
                    arrays[call] = info;
                }
            }
        }
    }
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
        return OptimizationRemarkAnalysis(DEBUG_TYPE, "EscapeAnalysis", I) << "escape analysis for " << ore::NV("GC Allocation", I) << "\n" << ore::NV("UseInfo", osuse_info.str());
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
    auto newF = Intrinsic::getDeclaration(call->getModule(), ID, overloadTys);
    assert(newF->getFunctionType() == newfType);
    newF->setCallingConv(call->getCallingConv());
    auto newCall = CallInst::Create(newF, args, "", call);
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
    Value *Init = UndefValue::get(T);
    if ((allockind & AllocFnKind::Zeroed) != AllocFnKind::Unknown)
        Init = Constant::getNullValue(T); // zero, as described
    else if (allockind == AllocFnKind::Unknown)
        Init = Constant::getNullValue(T); // assume zeroed since we didn't find the attribute
    else
        Init = prolog_builder.CreateFreeze(UndefValue::get(T)); // assume freeze, since LLVM does not natively support this case
    prolog_builder.CreateStore(Init, buff);
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
        ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, Type::getInt8PtrTy(prolog_builder.getContext())));
    }
    else {
        Type *buffty;
        if (pass.DL->isLegalInteger(sz * 8))
            buffty = Type::getIntNTy(pass.getLLVMContext(), sz * 8);
        else
            buffty = ArrayType::get(Type::getInt8Ty(pass.getLLVMContext()), sz);
        buff = prolog_builder.CreateAlloca(buffty);
        buff->setAlignment(Align(align));
        ptr = cast<Instruction>(prolog_builder.CreateBitCast(buff, Type::getInt8PtrTy(prolog_builder.getContext(), buff->getType()->getPointerAddressSpace())));
    }
    insertLifetime(ptr, ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), sz), orig_inst);
    if (sz != 0 && !has_ref) { // TODO: fix has_ref case too
        IRBuilder<> builder(orig_inst);
        initializeAlloca(builder, buff, allockind);
    }
    Instruction *new_inst = cast<Instruction>(prolog_builder.CreateBitCast(ptr, JuliaType::get_pjlvalue_ty(prolog_builder.getContext(), buff->getType()->getPointerAddressSpace())));
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
        if (isa<LoadInst>(user) || isa<StoreInst>(user)) {
            user->replaceUsesOfWith(orig_i, new_i);
        }
        else if (auto call = dyn_cast<CallInst>(user)) {
            auto callee = call->getCalledOperand();
            if (pass.pointer_from_objref_func == callee) {
                call->replaceAllUsesWith(prolog_builder.CreateAddrSpaceCast(new_i, call->getCalledFunction()->getReturnType()));
                call->eraseFromParent();
                return;
            }
            //if (pass.gc_loaded_func == callee) {
            //    call->replaceAllUsesWith(new_i);
            //    call->eraseFromParent();
            //    return;
            //}
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
            auto cast_t = PointerType::getWithSamePointeeType(cast<PointerType>(user->getType()), new_i->getType()->getPointerAddressSpace());
            auto replace_i = new_i;
            Type *new_t = new_i->getType();
            if (cast_t != new_t) {
                // Shouldn't get here when using opaque pointers, so the new BitCastInst is fine
                assert(cast_t->getContext().supportsTypedPointers());
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
                if (!isa<UnreachableInst>(stored_inst->getParent()->getTerminator()))
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
                    return OptimizationRemark(DEBUG_TYPE, "typeof", call)
                        << "removed typeof call for GC allocation " << ore::NV("Alloc", orig_inst);
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
    auto align = orig_inst->getRetAlign().valueOrOne();
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
        slot.slot->setAlignment(Align(MinAlign(align.value(), slot.offset)));
        IRBuilder<> builder(orig_inst);
        insertLifetime(prolog_builder.CreateBitCast(slot.slot, Type::getInt8PtrTy(prolog_builder.getContext())),
                       ConstantInt::get(Type::getInt64Ty(prolog_builder.getContext()), field.size), orig_inst);
        initializeAlloca(builder, slot.slot, use_info.allockind);
        slots.push_back(std::move(slot));
    }
    struct ErrorBBInfo {
        CallInst *sunk;
        Instruction *insertpt;
        Instruction *p11i8;
        DenseMap<uint32_t, Instruction *> p11i8_offsets;

        ErrorBBInfo(CallInst *sunk) : sunk(sunk), insertpt(sunk->getNextNonDebugInstruction()), p11i8(nullptr) {}

        Instruction *gep(uint32_t offset, Type *elty, IRBuilder<> &builder) {
            builder.SetInsertPoint(insertpt);
            if (!p11i8) {
                auto p11jlvalue = builder.CreateAddrSpaceCast(sunk, PointerType::getWithSamePointeeType(cast<PointerType>(sunk->getType()), AddressSpace::Derived));
                p11i8 = cast<Instruction>(builder.CreateBitCast(p11jlvalue, Type::getInt8PtrTy(builder.getContext(), AddressSpace::Derived)));
            }
            auto it = p11i8_offsets.find(offset);
            if (it == p11i8_offsets.end()) {
                auto gep = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), p11i8, offset);
                it = p11i8_offsets.insert(std::make_pair(offset, cast<Instruction>(gep))).first;
            }
            return cast<Instruction>(builder.CreateBitCast(it->second, elty->getPointerTo(AddressSpace::Derived)));
        }
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
            addr = builder.CreateBitCast(slot.slot, Type::getInt8PtrTy(builder.getContext()));
            addr = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), addr, offset);
            addr = builder.CreateBitCast(addr, elty->getPointerTo());
        }
        return addr;
    };
    DenseMap<BasicBlock*, ErrorBBInfo> partial_escapes;
    IRBuilder<> errbuilder(orig_inst->getContext());
    // sink allocation into error blocks, copy fields
    for (auto errbb : use_info.errorbbs) {
        auto sunk = cast<CallInst>(orig_inst->clone());
        sunk->insertBefore(&*errbb->getFirstInsertionPt());
        auto &info = partial_escapes.insert(std::make_pair(errbb, ErrorBBInfo(sunk))).first->second;
        errbuilder.SetInsertPoint(info.insertpt);
        // copy every slot into the final object
        for (auto &slot : slots) {
            auto psize = pass.DL->getPointerSize();
            if (slot.isref) {
                auto copyt = pass.T_prjlvalue;
                assert(slot.size % psize == 0);
                for (uint32_t offset = 0; offset < slot.size; offset += psize) {
                    auto dest = info.gep(slot.offset + offset, copyt, errbuilder);
                    auto src = slot_gep(slot, slot.offset + offset, copyt, errbuilder);
                    auto load = errbuilder.CreateAlignedLoad(copyt, src, Align(MinAlign(slot.slot->getAlign().value(), offset)));
                    errbuilder.CreateAlignedStore(load, dest, Align(MinAlign(align.value(), slot.offset + offset)));
                }
            } else {
                auto copyt = pass.DL->getIntPtrType(errbuilder.getContext());
                for (uint32_t offset = 0; offset < slot.size; offset += psize) {
                    auto dest = info.gep(slot.offset + offset, copyt, errbuilder);
                    auto src = slot_gep(slot, slot.offset + offset, copyt, errbuilder);
                    auto load = errbuilder.CreateAlignedLoad(copyt, src, Align(MinAlign(slot.slot->getAlign().value(), offset)));
                    errbuilder.CreateAlignedStore(load, dest, Align(MinAlign(align.value(), slot.offset + offset)));
                }
                auto remainder = slot.size % psize;
                if (remainder != 0) {
                    copyt = cast<IntegerType>(pass.DL->getSmallestLegalIntType(errbuilder.getContext(), 8));
                    assert(copyt->getBitWidth() % 8 == 0);
                    auto copysize = copyt->getBitWidth() / 8;
                    assert(remainder % copysize == 0);
                    for (size_t offset = slot.size - remainder; offset < slot.size; offset += copysize) {
                        auto dest = info.gep(slot.offset + offset, copyt, errbuilder);
                        auto src = slot_gep(slot, slot.offset + offset, copyt, errbuilder);
                        auto load = errbuilder.CreateAlignedLoad(copyt, src, Align(MinAlign(slot.slot->getAlign().value(), offset)));
                        errbuilder.CreateAlignedStore(load, dest, Align(MinAlign(align.value(), slot.offset + offset)));
                    }
                }
            }
        }
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
    auto replace_inst = [&] (Use *use) {
        Instruction *user = cast<Instruction>(use->getUser());
        Instruction *orig_i = cur.orig_i;
        uint32_t offset = cur.offset;
        auto errit = partial_escapes.find(user->getParent());
        ErrorBBInfo *errinfo = errit == partial_escapes.end() ? nullptr : &errit->second;
        if (auto load = dyn_cast<LoadInst>(user)) {
            if (errinfo) {
                auto gep = errinfo->gep(offset, load->getType(), errbuilder);
                use->set(gep);
                return;
            }
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
            newload->setAlignment(load->getAlign());
            // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
            newload->setOrdering(AtomicOrdering::NotAtomic);
            load->replaceAllUsesWith(val);
            load->eraseFromParent();
            return;
        }
        else if (auto store = dyn_cast<StoreInst>(user)) {
            if (auto stored_inst = dyn_cast<Instruction>(store->getValueOperand()))
                if (!isa<UnreachableInst>(stored_inst->getParent()->getTerminator()))
                    pushInstruction(stored_inst); // may be able to stack allocate this object too
            if (errinfo) {
                auto gep = errinfo->gep(offset, store->getValueOperand()->getType(), errbuilder);
                use->set(gep);
                return;
            }
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
                    store_ty = PointerType::getWithSamePointeeType(T_pjlvalue, cast<PointerType>(store_ty)->getAddressSpace());
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
            newstore->setAlignment(store->getAlign());
            // since we're moving heap-to-stack, it is safe to downgrade the atomic level to NotAtomic
            newstore->setOrdering(AtomicOrdering::NotAtomic);
            store->eraseFromParent();
            return;
        }
        else if (isa<AtomicCmpXchgInst>(user) || isa<AtomicRMWInst>(user)) {
            if (errinfo) {
                auto gep = errinfo->gep(offset, user->getType(), errbuilder);
                use->set(gep);
                return;
            }
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
            if (errinfo) {
                auto pt = cast<PointerType>(use->get()->getType());
                if (pt->getAddressSpace() == AddressSpace::Tracked) {
                    assert(offset == 0); // can't have tracked gep
                    auto bc = errbuilder.CreateBitCast(errinfo->sunk, pt);
                    use->set(bc);
                } else {
                    auto eltype = pt->isOpaque() ? Type::getInt8Ty(pt->getContext()) : pt->getNonOpaquePointerElementType();
                    auto gep = errinfo->gep(offset, eltype, errbuilder);
                    use->set(gep);
                }
                return;
            }
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
                            auto ptr8 = builder.CreateBitCast(slot.slot, Type::getInt8PtrTy(builder.getContext()));
                            if (offset > slot.offset)
                                ptr8 = builder.CreateConstInBoundsGEP1_32(Type::getInt8Ty(builder.getContext()), ptr8,
                                                                          offset - slot.offset);
                            auto sub_size = std::min(slot.offset + slot.size, offset + size) -
                                std::max(offset, slot.offset);
                            // TODO: alignment computation
                            builder.CreateMemSet(ptr8, val_arg, sub_size, MaybeAlign(0));
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
            APInt apoffset(pass.DL->getPointerSizeInBits(), offset, true);
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
    if (!alloc_obj_func && !alloc_genericmemory_func)
        return false;

    DL = &M.getDataLayout();

    lifetime_start = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_start, { Type::getInt8PtrTy(M.getContext(), DL->getAllocaAddrSpace()) });
    lifetime_end = Intrinsic::getDeclaration(&M, Intrinsic::lifetime_end, { Type::getInt8PtrTy(M.getContext(), DL->getAllocaAddrSpace()) });

    return true;
}

bool AllocOpt::runOnFunction(Function &F, FunctionAnalysisManager &AM)
{
    if (!alloc_obj_func && !alloc_genericmemory_func) {
        LLVM_DEBUG(dbgs() << "AllocOpt: no alloc_obj/alloc_genericmemory function found, skipping pass\n");
        return false;
    }
    Optimizer optimizer(F, *this, AM);
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
    if (opt.runOnFunction(F, AM)) {
        modified = true;
    }
    if (modified) {
        auto preserved = opt.cfgChanged ? PreservedAnalyses::none() : PreservedAnalyses::allInSet<CFGAnalyses>();
        preserved.preserve<DominatorTreeAnalysis>();
        return preserved;
    } else {
        return PreservedAnalyses::all();
    }
}
