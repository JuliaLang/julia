// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <utility>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Debug.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>

#define STR(csym)           #csym
#define XSTR(csym)          STR(csym)
#include "julia.h"

enum AddressSpace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
    FirstSpecial = Tracked,
    LastSpecial = Loaded,
};

namespace JuliaType {
    static inline llvm::StructType* get_jlvalue_ty(llvm::LLVMContext &C) {
        return llvm::StructType::get(C);
    }

    static inline llvm::PointerType* get_pjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_jlvalue_ty(C), 0);
    }

    static inline llvm::PointerType* get_prjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_jlvalue_ty(C), AddressSpace::Tracked);
    }

    static inline llvm::PointerType* get_ppjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_pjlvalue_ty(C), 0);
    }
}

// JLCALL with API arguments ([extra], arg0, arg1, arg2, ...) has the following ABI calling conventions defined:
#define JLCALL_F_CC (CallingConv::ID)37     // (jl_value_t *arg0, jl_value_t **argv, uint32_t nargv)
#define JLCALL_F2_CC (CallingConv::ID)38    // (jl_value_t *arg0, jl_value_t **argv, uint32_t nargv, jl_value_t *extra)

// return how many Tracked pointers are in T (count > 0),
// and if there is anything else in T (all == false)
struct CountTrackedPointers {
    unsigned count = 0;
    bool all = true;
    bool derived = false;
    CountTrackedPointers(llvm::Type *T);
};

unsigned TrackWithShadow(llvm::Value *Src, llvm::Type *T, bool isptr, llvm::Value *Dst, llvm::IRBuilder<> &irbuilder);
std::vector<llvm::Value*> ExtractTrackedValues(llvm::Value *Src, llvm::Type *STy, bool isptr, llvm::IRBuilder<> &irbuilder, llvm::ArrayRef<unsigned> perm_offsets={});

static inline void llvm_dump(llvm::Value *v)
{
    v->print(llvm::dbgs(), true);
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::Type *v)
{
    v->print(llvm::dbgs(), true);
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::Function *f)
{
    f->print(llvm::dbgs(), nullptr, false, true);
}

static inline void llvm_dump(llvm::Module *m)
{
    m->print(llvm::dbgs(), nullptr);
}

static inline void llvm_dump(llvm::Metadata *m)
{
    m->print(llvm::dbgs());
    llvm::dbgs() << "\n";
}

static inline void llvm_dump(llvm::DebugLoc *dbg)
{
    dbg->print(llvm::dbgs());
    llvm::dbgs() << "\n";
}

static inline std::pair<llvm::MDNode*,llvm::MDNode*> tbaa_make_child_with_context(llvm::LLVMContext &ctxt, const char *name, llvm::MDNode *parent=nullptr, bool isConstant=false)
{
    llvm::MDBuilder mbuilder(ctxt);
    llvm::MDNode *jtbaa = mbuilder.createTBAARoot("jtbaa");
    llvm::MDNode *tbaa_root = mbuilder.createTBAAScalarTypeNode("jtbaa", jtbaa);
    llvm::MDNode *scalar = mbuilder.createTBAAScalarTypeNode(name, parent ? parent : tbaa_root);
    llvm::MDNode *n = mbuilder.createTBAAStructTagNode(scalar, scalar, 0, isConstant);
    return std::make_pair(n, scalar);
}

static inline llvm::MDNode *get_tbaa_gcframe(llvm::LLVMContext &ctxt) {
    return tbaa_make_child_with_context(ctxt, "jtbaa_gcframe").first;
}
static inline llvm::MDNode *get_tbaa_const(llvm::LLVMContext &ctxt) {
    return tbaa_make_child_with_context(ctxt, "jtbaa_const", nullptr, true).first;
}

static inline llvm::Instruction *tbaa_decorate(llvm::MDNode *md, llvm::Instruction *inst)
{
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, md);
    if (llvm::isa<llvm::LoadInst>(inst) && md == get_tbaa_const(md->getContext()))
        inst->setMetadata(llvm::LLVMContext::MD_invariant_load, llvm::MDNode::get(md->getContext(), llvm::None));
    return inst;
}

// bitcast a value, but preserve its address space when dealing with pointer types
static inline llvm::Value *emit_bitcast_with_builder(llvm::IRBuilder<> &builder, llvm::Value *v, llvm::Type *jl_value)
{
    using namespace llvm;
    if (isa<PointerType>(jl_value) &&
        v->getType()->getPointerAddressSpace() != jl_value->getPointerAddressSpace()) {
        // Cast to the proper address space
        Type *jl_value_addr =
                PointerType::get(cast<PointerType>(jl_value)->getElementType(),
                                 v->getType()->getPointerAddressSpace());
        return builder.CreateBitCast(v, jl_value_addr);
    }
    else {
        return builder.CreateBitCast(v, jl_value);
    }
}

// Get PTLS through current task.
static inline llvm::Value *get_current_ptls_from_task(llvm::IRBuilder<> &builder, llvm::Value *current_task)
{
    using namespace llvm;
    auto T_ppjlvalue = JuliaType::get_ppjlvalue_ty(builder.getContext());
    auto T_size = builder.GetInsertBlock()->getModule()->getDataLayout().getIntPtrType(builder.getContext());
    const int ptls_offset = offsetof(jl_task_t, ptls);
    llvm::Value *pptls = builder.CreateInBoundsGEP(
        JuliaType::get_pjlvalue_ty(builder.getContext()), current_task,
        ConstantInt::get(T_size, ptls_offset / sizeof(void *)),
        "ptls_field");
    LoadInst *ptls_load = builder.CreateAlignedLoad(
        emit_bitcast_with_builder(builder, pptls, T_ppjlvalue), Align(sizeof(void *)), "ptls_load");
    // Note: Corresponding store (`t->ptls = ptls`) happens in `ctx_switch` of tasks.c.
    tbaa_decorate(get_tbaa_gcframe(builder.getContext()), ptls_load);
    // Using `CastInst::Create` to get an `Instruction*` without explicit cast:
    auto ptls = CastInst::Create(Instruction::BitCast, ptls_load, T_ppjlvalue, "ptls");
    builder.Insert(ptls);
    return ptls;
}