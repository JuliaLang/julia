// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "lower_ptls"
#undef DEBUG

// LLVM pass to optimize TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"
#include <sstream>

#include <llvm/Pass.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/MDBuilder.h>

#if defined(JULIA_ENABLE_THREADING)
#  include <llvm/IR/InlineAsm.h>
#  include <llvm/Transforms/Utils/BasicBlockUtils.h>
#endif
#include "fix_llvm_assert.h"

#include "julia.h"
#include "julia_internal.h"

using namespace llvm;

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);

namespace {

struct LowerPTLS: public ModulePass {
    static char ID;
    LowerPTLS(bool _imaging_mode=false)
        : ModulePass(ID),
          imaging_mode(_imaging_mode)
    {}

private:
    bool imaging_mode;
    bool runOnModule(Module &M) override;
    void runOnFunction(LLVMContext &ctx, Module &M, Function *F,
                       Function *ptls_getter, Type *T_ppjlvalue, MDNode *tbaa_const);
};

static void ensure_global(const char *name, Type *t, Module &M,
                          bool dllimport=false)
{
    if (M.getNamedValue(name))
        return;
    GlobalVariable *proto = new GlobalVariable(M, t, false,
                                               GlobalVariable::ExternalLinkage,
                                               NULL, name);
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport) {
        // add the __declspec(dllimport) attribute
        proto->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
    }
#else // _OS_WINDOWS_
    (void)proto;
#endif // _OS_WINDOWS_
}

#ifdef JULIA_ENABLE_THREADING
static void setCallPtlsAttrs(CallInst *ptlsStates)
{
#if JL_LLVM_VERSION >= 50000
    ptlsStates->addAttribute(AttributeList::FunctionIndex, Attribute::ReadNone);
    ptlsStates->addAttribute(AttributeList::FunctionIndex, Attribute::NoUnwind);
#else
    ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::ReadNone);
    ptlsStates->addAttribute(AttributeSet::FunctionIndex, Attribute::NoUnwind);
#endif
}

static Instruction *emit_ptls_tp(LLVMContext &ctx, Value *offset, Type *T_ppjlvalue,
                                 Instruction *insertBefore)
{
    auto T_int8 = Type::getInt8Ty(ctx);
    auto T_pint8 = PointerType::get(T_int8, 0);
#  if defined(_CPU_X86_64_) || defined(_CPU_X86_)
    // Workaround LLVM bug by hiding the offset computation
    // (and therefore the optimization opportunity) from LLVM.
    // Ref https://github.com/JuliaLang/julia/issues/17288
    static const std::string const_asm_str = [&] () {
        std::stringstream stm;
#  if defined(_CPU_X86_64_)
        stm << "movq %fs:0, $0;\naddq $$" << jl_tls_offset << ", $0";
#  else
        stm << "movl %gs:0, $0;\naddl $$" << jl_tls_offset << ", $0";
#  endif
        return stm.str();
    }();
#  if defined(_CPU_X86_64_)
    const char *dyn_asm_str = "movq %fs:0, $0;\naddq $1, $0";
#  else
    const char *dyn_asm_str = "movl %gs:0, $0;\naddl $1, $0";
#  endif

    // The add instruction clobbers flags
    Value *tls;
    if (offset) {
        std::vector<Type*> args(0);
        args.push_back(offset->getType());
        auto tp = InlineAsm::get(FunctionType::get(T_pint8, args, false),
                                 dyn_asm_str, "=&r,r,~{dirflag},~{fpsr},~{flags}", false);
        tls = CallInst::Create(tp, offset, "ptls_i8", insertBefore);
    }
    else {
        auto tp = InlineAsm::get(FunctionType::get(T_pint8, false),
                                 const_asm_str.c_str(), "=r,~{dirflag},~{fpsr},~{flags}", false);
        tls = CallInst::Create(tp, "ptls_i8", insertBefore);
    }
    return new BitCastInst(tls, PointerType::get(T_ppjlvalue, 0), "ptls", insertBefore);
#  elif defined(_CPU_AARCH64_) || (defined(__ARM_ARCH) && __ARM_ARCH >= 7)
    // AArch64/ARM doesn't seem to have this issue.
    // (Possibly because there are many more registers and the offset is
    // positive and small)
    // It's also harder to emit the offset in a generic way on ARM/AArch64
    // (need to generate one or two `add` with shift) so let llvm emit
    // the add for now.
#if defined(_CPU_AARCH64_)
    const char *asm_str = "mrs $0, tpidr_el0";
#else
    const char *asm_str = "mrc p15, 0, $0, c13, c0, 3";
#endif
    if (!offset) {
        auto T_size = (sizeof(size_t) == 8 ? Type::getInt64Ty(ctx) : Type::getInt32Ty(ctx));
        offset = ConstantInt::getSigned(T_size, jl_tls_offset);
    }
    auto tp = InlineAsm::get(FunctionType::get(T_pint8, false), asm_str, "=r", false);
    Value *tls = CallInst::Create(tp, "thread_ptr", insertBefore);
    tls = GetElementPtrInst::Create(T_int8, tls, {offset}, "ptls_i8", insertBefore);
    return new BitCastInst(tls, PointerType::get(T_ppjlvalue, 0), "ptls", insertBefore);
#  else
    (void)T_pint8;
    assert(0 && "Cannot emit thread pointer for this architecture.");
    return nullptr;
#  endif
}

#endif

void LowerPTLS::runOnFunction(LLVMContext &ctx, Module &M, Function *F,
                              Function *ptls_getter, Type *T_ppjlvalue, MDNode *tbaa_const)
{
    CallInst *ptlsStates = NULL;
    for (auto I = F->getEntryBlock().begin(), E = F->getEntryBlock().end();
         I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledValue() == ptls_getter) {
                ptlsStates = callInst;
                break;
            }
        }
    }
    if (!ptlsStates)
        return;

    if (ptlsStates->use_empty()) {
        ptlsStates->eraseFromParent();
        return;
    }

#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode) {
        GlobalVariable *GV = cast<GlobalVariable>(
            M.getNamedValue("jl_get_ptls_states.ptr"));
        if (jl_tls_elf_support) {
            GlobalVariable *OffsetGV = cast<GlobalVariable>(
                M.getNamedValue("jl_tls_offset.val"));
            // if (offset != 0)
            //     ptls = tp + offset;
            // else
            //     ptls = getter();
            auto offset = new LoadInst(OffsetGV, "", ptlsStates);
            offset->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            auto cmp = new ICmpInst(ptlsStates, CmpInst::ICMP_NE, offset,
                                    Constant::getNullValue(offset->getType()));
            MDBuilder MDB(ctx);
            SmallVector<uint32_t, 2> Weights{9, 1};
            TerminatorInst *fastTerm;
            TerminatorInst *slowTerm;
            SplitBlockAndInsertIfThenElse(cmp, ptlsStates, &fastTerm, &slowTerm,
                                          MDB.createBranchWeights(Weights));

            auto fastTLS = emit_ptls_tp(ctx, offset, T_ppjlvalue, fastTerm);
            auto phi = PHINode::Create(PointerType::get(T_ppjlvalue, 0), 2, "", ptlsStates);
            ptlsStates->replaceAllUsesWith(phi);
            ptlsStates->moveBefore(slowTerm);
            auto getter = new LoadInst(GV, "", ptlsStates);
            getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            ptlsStates->setCalledFunction(getter);
            setCallPtlsAttrs(ptlsStates);

            phi->addIncoming(fastTLS, fastTLS->getParent());
            phi->addIncoming(ptlsStates, ptlsStates->getParent());

            return;
        }
        auto getter = new LoadInst(GV, "", ptlsStates);
        getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        ptlsStates->setCalledFunction(getter);
        setCallPtlsAttrs(ptlsStates);
    }
    else if (jl_tls_offset != -1) {
        ptlsStates->replaceAllUsesWith(emit_ptls_tp(ctx, nullptr, T_ppjlvalue, ptlsStates));
        ptlsStates->eraseFromParent();
    }
    else {
        setCallPtlsAttrs(ptlsStates);
    }
#else
    ptlsStates->replaceAllUsesWith(M.getNamedValue("jl_tls_states"));
    ptlsStates->eraseFromParent();
#endif
}

bool LowerPTLS::runOnModule(Module &M)
{
    MDNode *tbaa_const = tbaa_make_child("jtbaa_const", nullptr, true).first;

    Function *ptls_getter = M.getFunction("jl_get_ptls_states");
    if (!ptls_getter)
        return true;
    LLVMContext &ctx = M.getContext();
    FunctionType *functype = ptls_getter->getFunctionType();
    auto T_ppjlvalue =
        cast<PointerType>(functype->getReturnType())->getElementType();
#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode) {
        ensure_global("jl_get_ptls_states.ptr", functype->getPointerTo(), M);
        ensure_global("jl_tls_offset.val",
                      sizeof(size_t) == 8 ? Type::getInt64Ty(ctx) : Type::getInt32Ty(ctx), M);
    }
#else
    ensure_global("jl_tls_states", T_ppjlvalue, M, imaging_mode);
#endif
    for (auto F = M.begin(), E = M.end(); F != E; ++F) {
        if (F->isDeclaration())
            continue;
        runOnFunction(ctx, M, &*F, ptls_getter, T_ppjlvalue, tbaa_const);
    }
#ifndef JULIA_ENABLE_THREADING
    ptls_getter->eraseFromParent();
#endif
    return true;
}

char LowerPTLS::ID = 0;

static RegisterPass<LowerPTLS> X("LowerPTLS", "LowerPTLS Pass",
                                 false /* Only looks at CFG */,
                                 false /* Analysis Pass */);

} // anonymous namespace

Pass *createLowerPTLSPass(bool imaging_mode)
{
    return new LowerPTLS(imaging_mode);
}
