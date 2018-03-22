// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "eh_lowering"

// LLVM pass lowering the call to outlined exception handling function.

// The pass needs to be run after GC frame lowering since it can add addrspace casts
// that breaks the invariance needed by the GC frame lowering pass.

#include "llvm-version.h"
#include "support/dtypes.h"

#include <llvm/ADT/SetVector.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include "julia.h"
#include "julia_internal.h"
#include "codegen_shared.h"
#include "julia_assert.h"

using namespace llvm;

namespace {

// TODO: strip lifetime and invariant intrinsics with variable size.

struct EHLowering: public ModulePass {
    static char ID;
    EHLowering()
        : ModulePass(ID)
    {}

private:
    struct ArgInfo {
        Value *val;
        enum ArgClass {
            // Combine into a big alloca
            Alloca,
            // Can be passed by value directly
            PtrVal,
            // Can be zext to a pointer size integer and passed directly
            IntZext,
            // Must be passed on the stack
            Spill
        };
        ArgClass arg_cls;
        AllocaInst *base; // Alloca only
        uint64_t offset; // Alloca only
        uint32_t idx; // Field index for alloca/spill or argument index for ptrval/intzext
        uint32_t orig_opno;
        ArgInfo(Value *val)
            : val(val)
        {}
    };

    bool runOnModule(Module &M) override;
    // At least one of V and T must be a pointer,
    // The other one can be a pointer or a integer value that can fit into a pointer.
    Value *castPtrValue(IRBuilder<> &builder, Value *V, Type *T) const;
    SmallVector<ArgInfo,16> collectArgInfo(CallInst *call, ArrayRef<CallInst*> calls,
                                           uint32_t orig_numargs, uint32_t &max_val_arg) const;
    SmallVector<Type*,4> computeAllocaTypes(MutableArrayRef<ArgInfo> arg_infos,
                                            uint32_t max_val_arg) const;
    SmallVector<Value*,6> getEHArgs(CallInst *call, MutableArrayRef<ArgInfo> arg_infos,
                                    AllocaInst *eh_data) const;
    Function *rewriteEHBody(Function *old_eh, ArrayRef<ArgInfo> arg_infos,
                            ArrayRef<Value*> eh_args, StructType *alloca_ty) const;
    void lowerEH(CallInst *call);
    Function *getCatchWrapper(uint32_t nargs, bool isgeneric);

    constexpr static uint32_t catch_narg = 5;

    LLVMContext *ctx;
    const DataLayout *DL;
    uint32_t ptr_bits;
    IntegerType *T_int8;
    IntegerType *T_int32;
    IntegerType *T_size;
    PointerType *T_pint8;
    Function *catcher;
    Function *catch_1[catch_narg + 1];
    Function *catch_generic[catch_narg + 1];
};

Value *EHLowering::castPtrValue(IRBuilder<> &builder, Value *V, Type *T) const
{
    auto VT = V->getType();
    if (auto ptr_ty = dyn_cast<PointerType>(T)) {
        if (auto ptr_vt = dyn_cast<PointerType>(VT)) {
            auto to_space = ptr_ty->getAddressSpace();
            if (to_space != ptr_vt->getAddressSpace()) {
                auto elty = ptr_vt->getElementType();
                V = builder.CreateAddrSpaceCast(V, elty->getPointerTo(to_space));
            }
            return builder.CreateBitCast(V, T);
        }
        V = builder.CreateZExt(V, T_size);
#if JL_LLVM_VERSION >= 40000
        if (!DL->isNonIntegralPointerType(ptr_ty))
            return builder.CreateIntToPtr(V, ptr_ty);
#endif
        auto elty = ptr_ty->getElementType();
        V = builder.CreateIntToPtr(V, elty->getPointerTo());
        return builder.CreateBitCast(V, T);
    }
#if JL_LLVM_VERSION >= 40000
    auto ptr_vt = cast<PointerType>(VT);
    if (DL->isNonIntegralPointerType(ptr_vt)) {
        auto elty = ptr_vt->getElementType();
        V = builder.CreateAddrSpaceCast(V, elty->getPointerTo());
    }
#endif
    V = builder.CreatePtrToInt(V, T_size);
    return builder.CreateTrunc(V, T);
}

SmallVector<EHLowering::ArgInfo,16>
EHLowering::collectArgInfo(CallInst *call, ArrayRef<CallInst*> calls, uint32_t orig_numargs,
                           uint32_t &max_val_arg) const
{
    bool has_alloca_arg = false;
    SmallVector<ArgInfo,16> args;
    uint32_t num_val_args = 0;
    auto check_arg_sameval = [&] (uint32_t i, Value *V) {
        for (auto call: calls) {
            if (call->getOperand(i) != V) {
                return false;
            }
        }
        return true;
    };
    // Skip the first argument assuming there's at least one argument
    for (auto ai = call->arg_begin(), ae = call->arg_end(); ++ai != ae;) {
        args.emplace_back(ai->get());
        auto &info = args.back();
        info.orig_opno = ai->getOperandNo();
        auto ty = info.val->getType();
        if (auto ptr_ty = dyn_cast<PointerType>(ty)) {
            if (ptr_ty->getAddressSpace() == 0) {
                APInt offset(ptr_bits, 0);
                auto base = info.val->stripAndAccumulateInBoundsConstantOffsets(*DL, offset);
                if (auto alloca_base = dyn_cast<AllocaInst>(base)) {
                    auto align = alloca_base->getAlignment();
                    auto ty_align = DL->getABITypeAlignment(alloca_base->getAllocatedType());
                    if (alloca_base->isStaticAlloca() && (align == 0 || align <= ty_align) &&
                        check_arg_sameval(ai->getOperandNo(), info.val)) {
                        has_alloca_arg = true;
                        info.arg_cls = ArgInfo::Alloca;
                        info.base = alloca_base;
                        info.offset = offset.getLimitedValue();
                        continue;
                    }
                }
            }
            info.arg_cls = ArgInfo::PtrVal;
            num_val_args++;
            continue;
        }
        if (auto int_ty = dyn_cast<IntegerType>(ty)) {
            if (int_ty->getBitWidth() <= ptr_bits) {
                info.arg_cls = ArgInfo::IntZext;
                num_val_args++;
                continue;
            }
        }
        info.arg_cls = ArgInfo::Spill;
        has_alloca_arg = true;
    }

    if (has_alloca_arg || num_val_args > catch_narg) {
        max_val_arg = catch_narg - 1;
    }
    else {
        max_val_arg = catch_narg;
    }
    return args;
}

SmallVector<Type*,4>
EHLowering::computeAllocaTypes(MutableArrayRef<EHLowering::ArgInfo> arg_infos,
                               uint32_t max_val_arg) const
{
    uint32_t arg_idx = 0;
    SmallVector<Type*,4> alloca_field_ty;
    std::map<AllocaInst*,uint32_t> alloca_map;
    for (auto &info: arg_infos) {
        if (info.arg_cls == ArgInfo::PtrVal || info.arg_cls == ArgInfo::IntZext) {
            if (arg_idx < max_val_arg) {
                info.idx = arg_idx++;
                continue;
            }
            info.arg_cls = ArgInfo::Spill;
        }
        info.idx = alloca_field_ty.size();
        if (info.arg_cls == ArgInfo::Spill) {
            alloca_field_ty.push_back(info.val->getType());
        }
        else {
            assert(info.arg_cls == ArgInfo::Alloca);
            auto it = alloca_map.find(info.base);
            if (it != alloca_map.end()) {
                info.idx = it->second;
                continue;
            }
            alloca_map[info.base] = info.idx;
            if (info.base->isArrayAllocation()) {
                auto array_sz = cast<ConstantInt>(info.base->getArraySize())->getLimitedValue();
                alloca_field_ty.push_back(ArrayType::get(info.base->getAllocatedType(),
                                                         array_sz));
            }
            else {
                alloca_field_ty.push_back(info.base->getAllocatedType());
            }
        }
    }
    return alloca_field_ty;
}

SmallVector<Value*,6> EHLowering::getEHArgs(CallInst *call, MutableArrayRef<ArgInfo> arg_infos,
                                            AllocaInst *eh_data) const
{
    SmallVector<Value*,6> args;
    IRBuilder<> builder(call);
    SmallSet<Instruction*,16> toremove;
    for (auto &info: arg_infos) {
        if (info.arg_cls == ArgInfo::PtrVal || info.arg_cls == ArgInfo::IntZext) {
            assert(args.size() == info.idx);
            args.push_back(call->getOperand(info.orig_opno));
            continue;
        }
        auto alloca_ty = eh_data->getAllocatedType();
        if (info.arg_cls == ArgInfo::Spill) {
            auto field_addr = builder.CreateConstInBoundsGEP2_32(alloca_ty, eh_data, 0, info.idx);
            builder.CreateStore(call->getOperand(info.orig_opno), field_addr);
            continue;
        }

        assert(info.arg_cls == ArgInfo::Alloca);
        if (!info.base || toremove.count(info.base)) {
            info.base = nullptr;
            continue;
        }
        IRBuilder<> builder2(info.base);
        Value *field_addr = builder2.CreateConstInBoundsGEP2_32(alloca_ty, eh_data, 0, info.idx);
        field_addr = castPtrValue(builder2, field_addr, info.base->getType());
        info.base->replaceAllUsesWith(field_addr);
        field_addr->takeName(info.base);
        toremove.insert(info.base);
        info.base = nullptr;
    }
    for (auto val: toremove)
        val->eraseFromParent();
    if (eh_data)
        args.push_back(eh_data);
    return args;
}

Function *EHLowering::rewriteEHBody(Function *old_eh, ArrayRef<EHLowering::ArgInfo> arg_infos,
                                    ArrayRef<Value*> eh_args, StructType *alloca_ty) const
{
    SmallVector<Type*,catch_narg> new_sig;
    for (auto arg: eh_args) {
        auto arg_ty = arg->getType();
        new_sig.push_back(arg_ty->isPointerTy() ? arg_ty : T_size);
    }

    Type *ret_ty = old_eh->getReturnType();
    bool is_generic = !ret_ty->isVoidTy();
    if (is_generic)
        new_sig.insert(new_sig.begin(), T_int32);

    std::string name = old_eh->getName().str();
    // Rename the old one temporarily so that the new function can have the same name.
    old_eh->setName("tmp");

    Module *M = old_eh->getParent();
    FunctionType *func_ty = FunctionType::get(ret_ty, new_sig, false);
    Function *eh_func = Function::Create(func_ty, GlobalValue::InternalLinkage, name, M);
    eh_func->setDoesNotThrow();
    eh_func->setHasUWTable();
    eh_func->setSubprogram(old_eh->getSubprogram());
#if JL_LLVM_VERSION >= 50000
    AttrBuilder AB(old_eh->getAttributes().getFnAttributes());
    for (const auto &attr : AB.td_attrs())
        eh_func->addFnAttr(attr.first, attr.second);
#endif

    SmallVector<BasicBlock*,32> bbs;
    for (auto &bb: *old_eh)
        bbs.push_back(&bb);

    auto &old_bbs = old_eh->getBasicBlockList();
    auto &new_bbs = eh_func->getBasicBlockList();

    for (auto bb: bbs) {
        old_bbs.remove(bb);
        new_bbs.push_back(bb);
    }

    SmallVector<Argument*,6> new_args;
    for (auto &arg: eh_func->args())
        new_args.push_back(&arg);
    if (is_generic) {
        new_args.erase(new_args.begin());
        old_eh->arg_begin()->replaceAllUsesWith(&*eh_func->arg_begin());
    }

    uint32_t nargs = arg_infos.size();
    auto old_arg_it = old_eh->arg_begin();
    if (is_generic)
        ++old_arg_it;
    IRBuilder<> builder(&*eh_func->getEntryBlock().begin());
    for (uint32_t i = 0; i < nargs; i++) {
        auto &old_arg = *old_arg_it;
        ++old_arg_it;
        auto &info = arg_infos[i];
        if (info.arg_cls == ArgInfo::PtrVal || info.arg_cls == ArgInfo::IntZext) {
            old_arg.replaceAllUsesWith(castPtrValue(builder, new_args[info.idx],
                                                    old_arg.getType()));
            continue;
        }
        auto eh_data = new_args[eh_args.size() - 1];
        Value *addr = builder.CreateConstInBoundsGEP2_32(alloca_ty, eh_data, 0, info.idx);
        if (info.arg_cls == ArgInfo::Spill) {
            addr = builder.CreateBitCast(addr, old_arg.getType()->getPointerTo());
            old_arg.replaceAllUsesWith(builder.CreateLoad(old_arg.getType(), addr));
            continue;
        }
        assert(info.arg_cls == ArgInfo::Alloca);
        if (info.offset != 0) {
            addr = builder.CreateBitCast(addr, T_pint8);
            addr = builder.CreateConstInBoundsGEP1_32(T_int8, addr, info.offset);
        }
        old_arg.replaceAllUsesWith(builder.CreateBitCast(addr, old_arg.getType()));
    }

    old_eh->replaceAllUsesWith(UndefValue::get(old_eh->getType()));
    old_eh->eraseFromParent();
    return eh_func;
}

void EHLowering::lowerEH(CallInst *call)
{
    auto F = call->getFunction();
    auto eh_func = cast<Function>(call->getArgOperand(0));
    uint32_t orig_numargs = call->getNumArgOperands();
    SmallVector<CallInst*,4> calls;
    if (!eh_func->hasOneUse()) {
        auto orig_call = call;
        for (auto user: eh_func->users()) {
            auto call = cast<CallInst>(user);
            if (call == orig_call)
                continue;
            assert(call->getFunction() == F);
            assert(call->getNumArgOperands() == orig_numargs);
            calls.push_back(call);
        }
    }

    uint32_t max_val_arg;
    auto arg_infos = collectArgInfo(call, calls, orig_numargs, max_val_arg);
    auto alloca_field_ty = computeAllocaTypes(arg_infos, max_val_arg);

    StructType *alloca_ty = nullptr;
    AllocaInst *eh_data = nullptr;
    if (!alloca_field_ty.empty()) {
        IRBuilder<> prolog_builder(&F->getEntryBlock().front());
        alloca_ty = StructType::get(*ctx, alloca_field_ty);
        eh_data = prolog_builder.CreateAlloca(alloca_ty);
    }
    auto call_args = getEHArgs(call, arg_infos, eh_data);
    eh_func = rewriteEHBody(eh_func, arg_infos, call_args, alloca_ty);
    auto nargs = call_args.size();
    call_args.insert(call_args.begin(), eh_func);
    IRBuilder<> builder(call);
    for (auto &arg: call_args)
        arg = castPtrValue(builder, arg, T_pint8);
    auto catch_fn = getCatchWrapper(nargs, !eh_func->getReturnType()->isVoidTy());
    auto new_call = builder.CreateCall(catch_fn, call_args);
    call->replaceAllUsesWith(new_call);
    call->eraseFromParent();
    for (auto call: calls) {
        auto call_args = getEHArgs(call, arg_infos, eh_data);
        assert(call_args.size() == nargs);
        call_args.insert(call_args.begin(), eh_func);
        IRBuilder<> builder(call);
        for (auto &arg: call_args)
            arg = castPtrValue(builder, arg, T_pint8);
        auto new_call = builder.CreateCall(catch_fn, call_args);
        call->replaceAllUsesWith(new_call);
        call->eraseFromParent();
    }
}

Function *EHLowering::getCatchWrapper(uint32_t nargs, bool isgeneric)
{
    auto catch_funcs = isgeneric ? catch_generic : catch_1;
    if (!catch_funcs[nargs]) {
        SmallVector<Type*,6> args(nargs + 1, T_pint8);
        auto fty = FunctionType::get(T_int32, args, false);
        std::string suffix = std::to_string(nargs);
        const char *name = isgeneric ? "jl_catch_exception_generic" : "jl_catch_exception";
        auto M = catcher->getParent();
        catch_funcs[nargs] = cast<Function>(M->getOrInsertFunction(name + suffix, fty));
    }
    return catch_funcs[nargs];
}

bool EHLowering::runOnModule(Module &M)
{
    catcher = M.getFunction("julia.catch_exception");
    if (!catcher)
        return false;
    ctx = &M.getContext();
    DL = &M.getDataLayout();
    ptr_bits = DL->getPointerSizeInBits(0);
    T_int8 = Type::getInt8Ty(*ctx);
    T_int32 = Type::getInt32Ty(*ctx);
    T_size = IntegerType::get(*ctx, ptr_bits);
    T_pint8 = Type::getInt8PtrTy(*ctx);
    memset(catch_1, 0, sizeof(catch_1));
    memset(catch_generic, 0, sizeof(catch_generic));

    bool changed = false;
    while (!catcher->use_empty())
        lowerEH(cast<CallInst>(*catcher->user_begin()));
    catcher->eraseFromParent();
    return changed;
}

char EHLowering::ID = 0;

static RegisterPass<EHLowering> X("EHLowering", "EHLowering Pass",
                                  false /* Only looks at CFG */,
                                  false /* Analysis Pass */);

} // anonymous namespace

Pass *createEHLoweringPass()
{
    return new EHLowering();
}
