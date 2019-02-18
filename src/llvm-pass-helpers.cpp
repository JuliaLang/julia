// This file is a part of Julia. License is MIT: https://julialang.org/license
//
// This file implements common functionality that is useful for the late GC frame
// lowering and final GC intrinsic lowering passes. See the corresponding header
// for docs.

#include <llvm/IR/Function.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include <iostream>

#include "llvm-version.h"
#include "codegen_shared.h"
#include "julia_assert.h"
#include "llvm-pass-helpers.h"

using namespace llvm;

extern std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false);

JuliaPassContext::JuliaPassContext()
    : module(nullptr), T_prjlvalue(nullptr), T_ppjlvalue(nullptr), T_size(nullptr),
        T_int8(nullptr), T_int32(nullptr), T_pint8(nullptr), T_pjlvalue(nullptr),
        T_pjlvalue_der(nullptr), T_ppjlvalue_der(nullptr), ptls_getter(nullptr),
        gc_flush_func(nullptr), gc_preserve_begin_func(nullptr), gc_preserve_end_func(nullptr),
        pointer_from_objref_func(nullptr), alloc_obj_func(nullptr), typeof_func(nullptr),
        write_barrier_func(nullptr)
{
    tbaa_gcframe = tbaa_make_child("jtbaa_gcframe").first;
    MDNode *tbaa_data;
    MDNode *tbaa_data_scalar;
    std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child("jtbaa_data");
    tbaa_tag = tbaa_make_child("jtbaa_tag", tbaa_data_scalar).first;
}

void JuliaPassContext::initFunctions(Module &M)
{
    module = &M;

    ptls_getter = M.getFunction("julia.ptls_states");
    gc_flush_func = M.getFunction("julia.gcroot_flush");
    gc_preserve_begin_func = M.getFunction("llvm.julia.gc_preserve_begin");
    gc_preserve_end_func = M.getFunction("llvm.julia.gc_preserve_end");
    pointer_from_objref_func = M.getFunction("julia.pointer_from_objref");
    typeof_func = M.getFunction("julia.typeof");
    write_barrier_func = M.getFunction("julia.write_barrier");
    alloc_obj_func = M.getFunction("julia.gc_alloc_obj");
}

void JuliaPassContext::initAll(Module &M)
{
    // First initialize the functions.
    initFunctions(M);

    // Then initialize types and metadata nodes.
    auto &ctx = M.getContext();
    T_size = M.getDataLayout().getIntPtrType(ctx);
    T_int8 = Type::getInt8Ty(ctx);
    T_pint8 = PointerType::get(T_int8, 0);
    T_int32 = Type::getInt32Ty(ctx);
    if (write_barrier_func) {
        T_prjlvalue = write_barrier_func->getFunctionType()->getParamType(0);
    }
    if (alloc_obj_func) {
        T_prjlvalue = alloc_obj_func->getReturnType();
        auto T_jlvalue = cast<PointerType>(T_prjlvalue)->getElementType();
        T_pjlvalue = PointerType::get(T_jlvalue, 0);
        T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
        T_pjlvalue_der = PointerType::get(T_jlvalue, AddressSpace::Derived);
        T_ppjlvalue_der = PointerType::get(T_prjlvalue, AddressSpace::Derived);
    }
    else if (ptls_getter) {
        auto functype = ptls_getter->getFunctionType();
        T_ppjlvalue = cast<PointerType>(functype->getReturnType())->getElementType();
        T_pjlvalue = cast<PointerType>(T_ppjlvalue)->getElementType();
        auto T_jlvalue = cast<PointerType>(T_pjlvalue)->getElementType();
        T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        T_pjlvalue_der = PointerType::get(T_jlvalue, AddressSpace::Derived);
        T_ppjlvalue_der = PointerType::get(T_prjlvalue, AddressSpace::Derived);
    }
    else {
        T_ppjlvalue = nullptr;
        T_prjlvalue = nullptr;
        T_pjlvalue = nullptr;
        T_pjlvalue_der = nullptr;
        T_ppjlvalue_der = nullptr;
    }
}

llvm::CallInst *JuliaPassContext::getPtls(llvm::Function &F) const
{
    for (auto I = F.getEntryBlock().begin(), E = F.getEntryBlock().end();
         ptls_getter && I != E; ++I) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
            if (callInst->getCalledValue() == ptls_getter) {
                return callInst;
            }
        }
    }
    return nullptr;
}

llvm::Function *JuliaPassContext::getOrNull(
    const jl_intrinsics::IntrinsicDescription &desc) const
{
    return module->getFunction(desc.name);
}

llvm::Function *JuliaPassContext::getOrDefine(
    const jl_intrinsics::IntrinsicDescription &desc) const
{
    auto local = getOrNull(desc);
    if (local) {
        return local;
    }
    else {
        return desc.define(*module, *this);
    }
}

namespace jl_intrinsics {
    static const char* NEW_GC_FRAME_NAME = "julia.new_gc_frame";
    static const char* PUSH_GC_FRAME_NAME = "julia.push_gc_frame";
    static const char* POP_GC_FRAME_NAME = "julia.pop_gc_frame";

    const IntrinsicDescription newGCFrame(
        NEW_GC_FRAME_NAME,
        [](llvm::Module &M, const JuliaPassContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(PointerType::get(context.T_prjlvalue, 0), {context.T_size}, false),
                Function::ExternalLinkage,
                NEW_GC_FRAME_NAME,
                &M);
            intrinsic->addAttribute(AttributeList::ReturnIndex, Attribute::NoAlias);
            intrinsic->addAttribute(AttributeList::ReturnIndex, Attribute::NonNull);

            return intrinsic;
        });

    const IntrinsicDescription pushGCFrame(
        PUSH_GC_FRAME_NAME,
        [](llvm::Module &M, const JuliaPassContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(M.getContext()),
                    {PointerType::get(context.T_prjlvalue, 0), context.T_size},
                    false),
                Function::ExternalLinkage,
                PUSH_GC_FRAME_NAME,
                &M);

            return intrinsic;
        });

    const IntrinsicDescription popGCFrame(
        POP_GC_FRAME_NAME,
        [](llvm::Module &M, const JuliaPassContext &context) {
            auto intrinsic = Function::Create(
                FunctionType::get(
                    Type::getVoidTy(M.getContext()),
                    {PointerType::get(context.T_prjlvalue, 0)},
                    false),
                Function::ExternalLinkage,
                POP_GC_FRAME_NAME,
                &M);

            return intrinsic;
        });
}
