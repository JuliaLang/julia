// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "lower_ptls"
#undef DEBUG

// LLVM pass to lower TLS access and remove references to julia intrinsics

#include "llvm-version.h"
#include "support/dtypes.h"

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

#include <llvm/IR/InlineAsm.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>

#include "julia.h"
#include "julia_internal.h"
#include "codegen_shared.h"
#include "julia_assert.h"

using namespace llvm;

typedef Instruction TerminatorInst;

std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr,
                                           bool isConstant=false);

namespace {

struct LowerPTLS: public ModulePass {
    static char ID;
    LowerPTLS(bool imaging_mode=false)
        : ModulePass(ID),
          imaging_mode(imaging_mode)
    {}

private:
    const bool imaging_mode;
    Module *M;
    Function *pgcstack_getter;
    LLVMContext *ctx;
    MDNode *tbaa_const;
    FunctionType *FT_pgcstack_getter;
    PointerType *T_pgcstack_getter;
    PointerType *T_ppjlvalue;
    PointerType *T_pppjlvalue;
    Type *T_int8;
    Type *T_size;
    PointerType *T_pint8;
    GlobalVariable *pgcstack_func_slot{nullptr};
    GlobalVariable *pgcstack_key_slot{nullptr};
    GlobalVariable *pgcstack_offset{nullptr};
    void set_pgcstack_attrs(CallInst *pgcstack) const;
    Instruction *emit_pgcstack_tp(Value *offset, Instruction *insertBefore) const;
    template<typename T> T *add_comdat(T *G) const;
    GlobalVariable *create_aliased_global(Type *T, StringRef name) const;
    void fix_pgcstack_use(CallInst *pgcstack);
    bool runOnModule(Module &M) override;
};

void LowerPTLS::set_pgcstack_attrs(CallInst *pgcstack) const
{
    pgcstack->addAttribute(AttributeList::FunctionIndex, Attribute::ReadNone);
    pgcstack->addAttribute(AttributeList::FunctionIndex, Attribute::NoUnwind);
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
            auto tp = InlineAsm::get(FunctionType::get(T_pint8, args, false),
                                     dyn_asm_str, "=&r,r,~{dirflag},~{fpsr},~{flags}", false);
            tls = CallInst::Create(tp, offset, "pgcstack_i8", insertBefore);
        }
        else {
            auto tp = InlineAsm::get(FunctionType::get(T_pint8, false),
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
            offset = ConstantInt::getSigned(T_size, jl_tls_offset);
        auto tp = InlineAsm::get(FunctionType::get(T_pint8, false), asm_str, "=r", false);
        tls = CallInst::Create(tp, "thread_ptr", insertBefore);
        tls = GetElementPtrInst::Create(T_int8, tls, {offset}, "ppgcstack_i8", insertBefore);
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
    auto GV = new GlobalVariable(*M, T, false, GlobalVariable::InternalLinkage,
                                 Constant::getNullValue(T), name + ".real");
    add_comdat(GlobalAlias::create(T, 0, GlobalVariable::ExternalLinkage,
                                   name, GV, M));
    return GV;
}

template<typename T>
inline T *LowerPTLS::add_comdat(T *G) const
{
#if defined(_OS_WINDOWS_)
    // Add comdat information to make MSVC link.exe happy
    // it's valid to emit this for ld.exe too,
    // but makes it very slow to link for no benefit
#if defined(_COMPILER_MICROSOFT_)
    Comdat *jl_Comdat = G->getParent()->getOrInsertComdat(G->getName());
    // ELF only supports Comdat::Any
    jl_Comdat->setSelectionKind(Comdat::NoDuplicates);
    G->setComdat(jl_Comdat);
#endif
    // add __declspec(dllexport) to everything marked for export
    if (G->getLinkage() == GlobalValue::ExternalLinkage)
        G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
    else
        G->setDLLStorageClass(GlobalValue::DefaultStorageClass);
#endif
    return G;
}

void LowerPTLS::fix_pgcstack_use(CallInst *pgcstack)
{
    if (pgcstack->use_empty()) {
        pgcstack->eraseFromParent();
        return;
    }

    if (imaging_mode) {
        if (jl_tls_elf_support) {
            // if (offset != 0)
            //     pgcstack = tp + offset;
            // else
            //     pgcstack = getter();
            auto offset = new LoadInst(T_size, pgcstack_offset, "", false, pgcstack);
            offset->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            offset->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(*ctx, None));
            auto cmp = new ICmpInst(pgcstack, CmpInst::ICMP_NE, offset,
                                    Constant::getNullValue(offset->getType()));
            MDBuilder MDB(*ctx);
            SmallVector<uint32_t, 2> Weights{9, 1};
            TerminatorInst *fastTerm;
            TerminatorInst *slowTerm;
            SplitBlockAndInsertIfThenElse(cmp, pgcstack, &fastTerm, &slowTerm,
                                          MDB.createBranchWeights(Weights));

            auto fastTLS = emit_pgcstack_tp(offset, fastTerm);
            auto phi = PHINode::Create(T_pppjlvalue, 2, "", pgcstack);
            pgcstack->replaceAllUsesWith(phi);
            pgcstack->moveBefore(slowTerm);
            auto getter = new LoadInst(T_pgcstack_getter, pgcstack_func_slot, "", false, pgcstack);
            getter->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
            getter->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(*ctx, None));
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
        getter->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(*ctx, None));
#if defined(_OS_DARWIN_)
        auto key = new LoadInst(T_size, pgcstack_key_slot, "", false, pgcstack);
        key->setMetadata(llvm::LLVMContext::MD_tbaa, tbaa_const);
        key->setMetadata(llvm::LLVMContext::MD_invariant_load, MDNode::get(*ctx, None));
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
        Constant *val = ConstantInt::get(T_size, (uintptr_t)f);
        val = ConstantExpr::getIntToPtr(val, T_pgcstack_getter);
#if defined(_OS_DARWIN_)
        assert(sizeof(k) == sizeof(uintptr_t));
        Constant *key = ConstantInt::get(T_size, (uintptr_t)k);
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

bool LowerPTLS::runOnModule(Module &_M)
{
    M = &_M;
    pgcstack_getter = M->getFunction("julia.get_pgcstack");
    if (!pgcstack_getter)
        return false;

    ctx = &M->getContext();
    tbaa_const = tbaa_make_child("jtbaa_const", nullptr, true).first;

    T_int8 = Type::getInt8Ty(*ctx);
    T_size = sizeof(size_t) == 8 ? Type::getInt64Ty(*ctx) : Type::getInt32Ty(*ctx);
    T_pint8 = T_int8->getPointerTo();
    FT_pgcstack_getter = pgcstack_getter->getFunctionType();
#if defined(_OS_DARWIN_)
    assert(sizeof(jl_pgcstack_key_t) == sizeof(uintptr_t));
    FT_pgcstack_getter = FunctionType::get(FT_pgcstack_getter->getReturnType(), {T_size}, false);
#endif
    T_pgcstack_getter = FT_pgcstack_getter->getPointerTo();
    T_pppjlvalue = cast<PointerType>(FT_pgcstack_getter->getReturnType());
    T_ppjlvalue = cast<PointerType>(T_pppjlvalue->getElementType());
    if (imaging_mode) {
        pgcstack_func_slot = create_aliased_global(T_pgcstack_getter, "jl_pgcstack_func_slot");
        pgcstack_key_slot = create_aliased_global(T_size, "jl_pgcstack_key_slot"); // >= sizeof(jl_pgcstack_key_t)
        pgcstack_offset = create_aliased_global(T_size, "jl_tls_offset");
    }

    for (auto it = pgcstack_getter->user_begin(); it != pgcstack_getter->user_end();) {
        auto call = cast<CallInst>(*it);
        ++it;
        assert(call->getCalledOperand() == pgcstack_getter);
        fix_pgcstack_use(call);
    }
    assert(pgcstack_getter->use_empty());
    pgcstack_getter->eraseFromParent();
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

extern "C" JL_DLLEXPORT void LLVMExtraAddLowerPTLSPass(LLVMPassManagerRef PM, LLVMBool imaging_mode)
{
    unwrap(PM)->add(createLowerPTLSPass(imaging_mode));
}
