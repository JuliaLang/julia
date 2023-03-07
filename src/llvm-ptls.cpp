// This file is a part of Julia. License is MIT: https://julialang.org/license
// LLVM pass to lower TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"
#include "passes.h"

#include <llvm-c/Core.h>
#include <llvm-c/Types.h>

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Verifier.h>

#include <llvm/IR/InlineAsm.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "julia.h"
#include "julia_internal.h"
#include "llvm-codegen-shared.h"
#include "julia_assert.h"

#define DEBUG_TYPE "lower_ptls"
#undef DEBUG

using namespace llvm;

typedef Instruction TerminatorInst;

namespace {

struct LowerPTLS {
    LowerPTLS(Module &M, bool imaging_mode=false)
        : imaging_mode(imaging_mode), M(&M)
    {}

    bool run(bool *CFGModified);
private:
    const bool imaging_mode;
    Module *M;
    MDNode *tbaa_const{nullptr};
    MDNode *tbaa_gcframe{nullptr};
    FunctionType *FT_pgcstack_getter{nullptr};
    PointerType *T_pgcstack_getter{nullptr};
    PointerType *T_pppjlvalue{nullptr};
    GlobalVariable *pgcstack_func_slot{nullptr};
    GlobalVariable *pgcstack_key_slot{nullptr};
    GlobalVariable *pgcstack_offset{nullptr};
    void set_pgcstack_attrs(CallInst *pgcstack) const;
    Instruction *emit_pgcstack_tp(Value *offset, Instruction *insertBefore) const;
    template<typename T> T *add_comdat(T *G) const;
    GlobalVariable *create_aliased_global(Type *T, StringRef name) const;
    void fix_pgcstack_use(CallInst *pgcstack, Function *pgcstack_getter, bool or_new, bool *CFGModified);
};

void LowerPTLS::set_pgcstack_attrs(CallInst *pgcstack) const
{
    addFnAttr(pgcstack, Attribute::ReadNone);
    addFnAttr(pgcstack, Attribute::NoUnwind);
}

Instruction *LowerPTLS::emit_pgcstack_tp(Value *offset, Instruction *insertBefore) const
{
    Value *tls;
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
    if (insertBefore->getFunction()->callsFunctionThatReturnsTwice()) {
        // Workaround LLVM bug by hiding the offset computation
        // (and therefore the optimization opportunity) from LLVM.
        // Ref https://github.com/JuliaLang/julia/issues/17288
        static const std::string const_asm_str = [&] () {
            std::string stm;
#  if defined(_CPU_X86_64_)
            raw_string_ostream(stm) << "movq %fs:0, $0;\naddq $$" << jl_tls_offset << ", $0";
#  else
            raw_string_ostream(stm) << "movl %gs:0, $0;\naddl $$" << jl_tls_offset << ", $0";
#  endif
            return stm;
        }();
#  if defined(_CPU_X86_64_)
        const char *dyn_asm_str = "movq %fs:0, $0;\naddq $1, $0";
#  else
        const char *dyn_asm_str = "movl %gs:0, $0;\naddl $1, $0";
#  endif

        // The add instruction clobbers flags
        if (offset) {
            std::vector<Type*> args(0);
            args.push_back(offset->getType());
            auto tp = InlineAsm::get(FunctionType::get(Type::getInt8PtrTy(insertBefore->getContext()), args, false),
                                     dyn_asm_str, "=&r,r,~{dirflag},~{fpsr},~{flags}", false);
            tls = CallInst::Create(tp, offset, "pgcstack_i8", insertBefore);
        }
        else {
            auto tp = InlineAsm::get(FunctionType::get(Type::getInt8PtrTy(insertBefore->getContext()), false),
                                     const_asm_str.c_str(), "=r,~{dirflag},~{fpsr},~{flags}",
                                     false);
            tls = CallInst::Create(tp, "pgcstack_i8", insertBefore);
        }
    }
    else
#endif
    {
        // AArch64/ARM doesn't seem to have this issue.
        // (Possibly because there are many more registers and the offset is
        // positive and small)
        // It's also harder to emit the offset in a generic way on ARM/AArch64
        // (need to generate one or two `add` with shift) so let llvm emit
        // the add for now.
#if defined(_CPU_AARCH64_)
        const char *asm_str = "mrs $0, tpidr_el0";
#elif defined(__ARM_ARCH) && __ARM_ARCH >= 7
        const char *asm_str = "mrc p15, 0, $0, c13, c0, 3";
#elif defined(_CPU_X86_64_)
        const char *asm_str = "movq %fs:0, $0";
#elif defined(_CPU_X86_)
        const char *asm_str = "movl %gs:0, $0";
#else
        const char *asm_str = nullptr;
        assert(0 && "Cannot emit thread pointer for this architecture.");
#endif
        if (!offset)
            offset = ConstantInt::getSigned(getSizeTy(insertBefore->getContext()), jl_tls_offset);
        auto tp = InlineAsm::get(FunctionType::get(Type::getInt8PtrTy(insertBefore->getContext()), false), asm_str, "=r", false);
        tls = CallInst::Create(tp, "thread_ptr", insertBefore);
        tls = GetElementPtrInst::Create(Type::getInt8Ty(insertBefore->getContext()), tls, {offset}, "ppgcstack_i8", insertBefore);
    }
    tls = new BitCastInst(tls, T_pppjlvalue->getPointerTo(), "ppgcstack", insertBefore);
    return new LoadInst(T_pppjlvalue, tls, "pgcstack", false, insertBefore);
}

GlobalVariable *LowerPTLS::create_aliased_global(Type *T, StringRef name) const
{
    // Create a static global variable and points a global alias to it so that
    // the address is visible externally but LLVM can still assume that the
    // address of this variable doesn't need dynamic relocation
    // (can be accessed with a single PC-rel load).
    auto GV = new GlobalVariable(*M, T, false, GlobalVariable::ExternalLinkage,
                                 nullptr, name);
    return GV;
}

void LowerPTLS::fix_pgcstack_use(CallInst *pgcstack, Function *pgcstack_getter, bool or_new, bool *CFGModified)
{
    if (pgcstack->use_empty()) {
        pgcstack->eraseFromParent();
        return;
    }
    if (or_new) {
        // pgcstack();
        // if (pgcstack != nullptr)
        //     last_gc_state = emit_gc_unsafe_enter(ctx);
        //     phi = pgcstack;        // fast
        // else
        //     last_gc_state = gc_safe;
        //     phi = adopt();         // slow
        // use phi;
        // if (!retboxed)
        //     foreach(retinst)
        //         emit_gc_unsafe_leave(ctx, last_gc_state);
        auto phi = PHINode::Create(pgcstack->getType(), 2, "");
        phi->insertAfter(pgcstack);
        pgcstack->replaceAllUsesWith(phi);
        MDBuilder MDB(pgcstack->getContext());
        SmallVector<uint32_t, 2> Weights{9, 1};
        TerminatorInst *fastTerm;
        TerminatorInst *slowTerm;
        auto cmp = new ICmpInst(phi, CmpInst::ICMP_NE, pgcstack, Constant::getNullValue(pgcstack->getType()));
        SplitBlockAndInsertIfThenElse(cmp, phi, &fastTerm, &slowTerm,
                                      MDB.createBranchWeights(Weights));
        if (CFGModified)
            *CFGModified = true;
        // emit slow branch code
        CallInst *adopt = cast<CallInst>(pgcstack->clone());
        Function *adoptFunc = M->getFunction(XSTR(jl_adopt_thread));
        if (adoptFunc == NULL) {
            adoptFunc = Function::Create(pgcstack_getter->getFunctionType(),
                pgcstack_getter->getLinkage(), pgcstack_getter->getAddressSpace(),
                XSTR(jl_adopt_thread), M);
            adoptFunc->copyAttributesFrom(pgcstack_getter);
            adoptFunc->copyMetadata(pgcstack_getter, 0);
        }
        adopt->setCalledFunction(adoptFunc);
        adopt->insertBefore(slowTerm);
        phi->addIncoming(adopt, slowTerm->getParent());
        // emit fast branch code
        IRBuilder<> builder(fastTerm->getParent());
        fastTerm->removeFromParent();
        MDNode *tbaa = tbaa_gcframe;
        Value *prior = emit_gc_unsafe_enter(builder, get_current_ptls_from_task(builder, get_current_task_from_pgcstack(builder, pgcstack), tbaa), true);
        builder.Insert(fastTerm);
        phi->addIncoming(pgcstack, fastTerm->getParent());
        // emit pre-return cleanup
        if (CountTrackedPointers(pgcstack->getParent()->getParent()->getReturnType()).count == 0) {
            auto last_gc_state = PHINode::Create(Type::getInt8Ty(pgcstack->getContext()), 2, "", phi);
            // if we called jl_adopt_thread, we must end this cfunction back in the safe-state
            last_gc_state->addIncoming(ConstantInt::get(Type::getInt8Ty(M->getContext()), JL_GC_STATE_SAFE), slowTerm->getParent());
            last_gc_state->addIncoming(prior, fastTerm->getParent());
            for (auto &BB : *pgcstack->getParent()->getParent()) {
                if (isa<ReturnInst>(BB.getTerminator())) {
                    IRBuilder<> builder(BB.getTerminator());
                    emit_gc_unsafe_leave(builder, get_current_ptls_from_task(builder, get_current_task_from_pgcstack(builder, phi), tbaa), last_gc_state, true);
                }
            }
        }
    }

    if (imaging_mode) {
        if (jl_tls_elf_support) {
            // if (offset != 0)
            //     pgcstack = tp + offset; // fast
            // else
            //     pgcstack = getter();    // slow
            auto offset = new LoadInst(getSizeTy(pgcstack->getContext()), pgcstack_offset, "", false, pgcstack);
            offset->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            offset->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(pgcstack->getContext(), None));
            auto cmp = new ICmpInst(pgcstack, CmpInst::ICMP_NE, offset,
                                    Constant::getNullValue(offset->getType()));
            MDBuilder MDB(pgcstack->getContext());
            SmallVector<uint32_t, 2> Weights{9, 1};
            TerminatorInst *fastTerm;
            TerminatorInst *slowTerm;
            SplitBlockAndInsertIfThenElse(cmp, pgcstack, &fastTerm, &slowTerm,
                                          MDB.createBranchWeights(Weights));
            if (CFGModified)
                *CFGModified = true;

            auto fastTLS = emit_pgcstack_tp(offset, fastTerm);
            auto phi = PHINode::Create(T_pppjlvalue, 2, "", pgcstack);
            pgcstack->replaceAllUsesWith(phi);
            pgcstack->moveBefore(slowTerm);
            auto getter = new LoadInst(T_pgcstack_getter, pgcstack_func_slot, "", false, pgcstack);
            getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            getter->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(pgcstack->getContext(), None));
            pgcstack->setCalledFunction(pgcstack->getFunctionType(), getter);
            set_pgcstack_attrs(pgcstack);

            phi->addIncoming(fastTLS, fastTLS->getParent());
            phi->addIncoming(pgcstack, pgcstack->getParent());

            return;
        }
        // In imaging mode, we emit the function address as a load of a static
        // variable to be filled (in `staticdata.c`) at initialization time of the sysimg.
        // This way we can bypass the extra indirection in `jl_get_pgcstack`
        // since we may not know which getter function to use ahead of time.
        auto getter = new LoadInst(T_pgcstack_getter, pgcstack_func_slot, "", false, pgcstack);
        getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        getter->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(pgcstack->getContext(), None));
#if defined(_OS_DARWIN_)
        auto key = new LoadInst(getSizeTy(pgcstack->getContext()), pgcstack_key_slot, "", false, pgcstack);
        key->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        key->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(pgcstack->getContext(), None));
        auto new_pgcstack = CallInst::Create(FT_pgcstack_getter, getter, {key}, "", pgcstack);
        new_pgcstack->takeName(pgcstack);
        pgcstack->replaceAllUsesWith(new_pgcstack);
        pgcstack->eraseFromParent();
        pgcstack = new_pgcstack;
#else
        pgcstack->setCalledFunction(pgcstack->getFunctionType(), getter);
#endif
        set_pgcstack_attrs(pgcstack);
    }
    else if (jl_tls_offset != -1) {
        pgcstack->replaceAllUsesWith(emit_pgcstack_tp(nullptr, pgcstack));
        pgcstack->eraseFromParent();
    }
    else {
        // use the address of the actual getter function directly
        jl_get_pgcstack_func *f;
        jl_pgcstack_key_t k;
        jl_pgcstack_getkey(&f, &k);
        Constant *val = ConstantInt::get(getSizeTy(pgcstack->getContext()), (uintptr_t)f);
        val = ConstantExpr::getIntToPtr(val, T_pgcstack_getter);
#if defined(_OS_DARWIN_)
        assert(sizeof(k) == sizeof(uintptr_t));
        Constant *key = ConstantInt::get(getSizeTy(pgcstack->getContext()), (uintptr_t)k);
        auto new_pgcstack = CallInst::Create(FT_pgcstack_getter, val, {key}, "", pgcstack);
        new_pgcstack->takeName(pgcstack);
        pgcstack->replaceAllUsesWith(new_pgcstack);
        pgcstack->eraseFromParent();
        pgcstack = new_pgcstack;
#else
        pgcstack->setCalledFunction(pgcstack->getFunctionType(), val);
#endif
        set_pgcstack_attrs(pgcstack);
    }
}

bool LowerPTLS::run(bool *CFGModified)
{
    bool need_init = true;
    auto runOnGetter = [&](bool or_new) {
        Function *pgcstack_getter = M->getFunction(or_new ? "julia.get_pgcstack_or_new" : "julia.get_pgcstack");
        if (!pgcstack_getter)
            return false;

        if (need_init) {
            tbaa_const = tbaa_make_child_with_context(M->getContext(), "jtbaa_const", nullptr, true).first;
            tbaa_gcframe = tbaa_make_child_with_context(M->getContext(), "jtbaa_gcframe").first;

            FT_pgcstack_getter = pgcstack_getter->getFunctionType();
#if defined(_OS_DARWIN_)
            assert(sizeof(jl_pgcstack_key_t) == sizeof(uintptr_t));
            FT_pgcstack_getter = FunctionType::get(FT_pgcstack_getter->getReturnType(), {getSizeTy(M->getContext())}, false);
#endif
            T_pgcstack_getter = FT_pgcstack_getter->getPointerTo();
            T_pppjlvalue = cast<PointerType>(FT_pgcstack_getter->getReturnType());
            if (imaging_mode) {
                pgcstack_func_slot = create_aliased_global(T_pgcstack_getter, "jl_pgcstack_func_slot");
                pgcstack_key_slot = create_aliased_global(getSizeTy(M->getContext()), "jl_pgcstack_key_slot"); // >= sizeof(jl_pgcstack_key_t)
                pgcstack_offset = create_aliased_global(getSizeTy(M->getContext()), "jl_tls_offset");
            }
            need_init = false;
        }

        for (auto it = pgcstack_getter->user_begin(); it != pgcstack_getter->user_end();) {
            auto call = cast<CallInst>(*it);
            ++it;
            assert(call->getCalledOperand() == pgcstack_getter);
            fix_pgcstack_use(call, pgcstack_getter, or_new, CFGModified);
        }
        assert(pgcstack_getter->use_empty());
        pgcstack_getter->eraseFromParent();
        return true;
    };
    return runOnGetter(false) + runOnGetter(true);
}

struct LowerPTLSLegacy: public ModulePass {
    static char ID;
    LowerPTLSLegacy(bool imaging_mode=false)
        : ModulePass(ID),
          imaging_mode(imaging_mode)
    {}

    bool imaging_mode;
    bool runOnModule(Module &M) override {
        LowerPTLS lower(M, imaging_mode);
        bool modified = lower.run(nullptr);
#ifdef JL_VERIFY_PASSES
        assert(!verifyModule(M, &errs()));
#endif
        return modified;
    }
};

char LowerPTLSLegacy::ID = 0;

static RegisterPass<LowerPTLSLegacy> X("LowerPTLS", "LowerPTLS Pass",
                                 false /* Only looks at CFG */,
                                 false /* Analysis Pass */);

} // anonymous namespace

PreservedAnalyses LowerPTLSPass::run(Module &M, ModuleAnalysisManager &AM) {
    LowerPTLS lower(M, imaging_mode);
    bool CFGModified = false;
    bool modified = lower.run(&CFGModified);
#ifdef JL_VERIFY_PASSES
    assert(!verifyModule(M, &errs()));
#endif
    if (modified) {
        if (CFGModified) {
            return PreservedAnalyses::none();
        } else {
            return PreservedAnalyses::allInSet<CFGAnalyses>();
        }
    }
    return PreservedAnalyses::all();
}

Pass *createLowerPTLSPass(bool imaging_mode)
{
    return new LowerPTLSLegacy(imaging_mode);
}

extern "C" JL_DLLEXPORT void LLVMExtraAddLowerPTLSPass_impl(LLVMPassManagerRef PM, LLVMBool imaging_mode)
{
    unwrap(PM)->add(createLowerPTLSPass(imaging_mode));
}
