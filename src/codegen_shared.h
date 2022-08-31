// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <utility>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/Debug.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include "julia.h"

#define STR(csym)           #csym
#define XSTR(csym)          STR(csym)

enum AddressSpace {
    Generic = 0,
    Tracked = 10,
    Derived = 11,
    CalleeRooted = 12,
    Loaded = 13,
    FirstSpecial = Tracked,
    LastSpecial = Loaded,
};

static inline auto getSizeTy(llvm::LLVMContext &ctxt) {
    if (sizeof(size_t) > sizeof(uint32_t)) {
        return llvm::Type::getInt64Ty(ctxt);
    } else {
        return llvm::Type::getInt32Ty(ctxt);
    }
}

namespace JuliaType {
    static inline llvm::StructType* get_jlvalue_ty(llvm::LLVMContext &C) {
        return llvm::StructType::get(C);
    }

    static inline llvm::PointerType* get_pjlvalue_ty(llvm::LLVMContext &C, unsigned addressSpace=0) {
        return llvm::PointerType::get(get_jlvalue_ty(C), addressSpace);
    }

    static inline llvm::PointerType* get_prjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_jlvalue_ty(C), AddressSpace::Tracked);
    }

    static inline llvm::PointerType* get_ppjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_pjlvalue_ty(C), 0);
    }

    static inline llvm::PointerType* get_pprjlvalue_ty(llvm::LLVMContext &C) {
        return llvm::PointerType::get(get_prjlvalue_ty(C), 0);
    }

    static inline auto get_jlfunc_ty(llvm::LLVMContext &C) {
        auto T_prjlvalue = get_prjlvalue_ty(C);
        auto T_pprjlvalue = llvm::PointerType::get(T_prjlvalue, 0);
        return llvm::FunctionType::get(T_prjlvalue, {
                T_prjlvalue,  // function
                T_pprjlvalue, // args[]
                llvm::Type::getInt32Ty(C)}, // nargs
            false);
    }

    static inline auto get_jlfunc2_ty(llvm::LLVMContext &C) {
        auto T_prjlvalue = get_prjlvalue_ty(C);
        auto T_pprjlvalue = llvm::PointerType::get(T_prjlvalue, 0);
        return llvm::FunctionType::get(T_prjlvalue, {
                T_prjlvalue,  // function
                T_pprjlvalue, // args[]
                llvm::Type::getInt32Ty(C),
                T_prjlvalue,  // linfo
                }, // nargs
            false);
    }

    static inline auto get_jlfuncparams_ty(llvm::LLVMContext &C) {
        auto T_prjlvalue = get_prjlvalue_ty(C);
        auto T_pprjlvalue = llvm::PointerType::get(T_prjlvalue, 0);
        return llvm::FunctionType::get(T_prjlvalue, {
                T_prjlvalue,  // function
                T_pprjlvalue, // args[]
                llvm::Type::getInt32Ty(C),
                T_pprjlvalue,  // linfo->sparam_vals
                }, // nargs
            false);
    }

    static inline auto get_voidfunc_ty(llvm::LLVMContext &C) {
        return llvm::FunctionType::get(llvm::Type::getVoidTy(C), /*isVarArg*/false);
    }

    static inline auto get_pvoidfunc_ty(llvm::LLVMContext &C) {
        return get_voidfunc_ty(C)->getPointerTo();
    }
}

// return how many Tracked pointers are in T (count > 0),
// and if there is anything else in T (all == false)
struct CountTrackedPointers {
    unsigned count = 0;
    bool all = true;
    bool derived = false;
    CountTrackedPointers(llvm::Type *T);
};

unsigned TrackWithShadow(llvm::Value *Src, llvm::Type *T, bool isptr, llvm::Value *Dst, llvm::Type *DTy, llvm::IRBuilder<> &irbuilder);
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

static inline llvm::MDNode *get_tbaa_const(llvm::LLVMContext &ctxt) {
    return tbaa_make_child_with_context(ctxt, "jtbaa_const", nullptr, true).first;
}

static inline llvm::Instruction *tbaa_decorate(llvm::MDNode *md, llvm::Instruction *inst)
{
    inst->setMetadata(llvm::LLVMContext::MD_tbaa, md);
    if (llvm::isa<llvm::LoadInst>(inst) && md && md == get_tbaa_const(md->getContext()))
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
        Type *jl_value_addr = PointerType::getWithSamePointeeType(cast<PointerType>(jl_value), v->getType()->getPointerAddressSpace());
        return builder.CreateBitCast(v, jl_value_addr);
    }
    else {
        return builder.CreateBitCast(v, jl_value);
    }
}

// Get PTLS through current task.
static inline llvm::Value *get_current_ptls_from_task(llvm::IRBuilder<> &builder, llvm::Value *current_task, llvm::MDNode *tbaa)
{
    using namespace llvm;
    auto T_ppjlvalue = JuliaType::get_ppjlvalue_ty(builder.getContext());
    auto T_pjlvalue = JuliaType::get_pjlvalue_ty(builder.getContext());
    auto T_size = builder.GetInsertBlock()->getModule()->getDataLayout().getIntPtrType(builder.getContext());
    const int ptls_offset = offsetof(jl_task_t, ptls);
    llvm::Value *pptls = builder.CreateInBoundsGEP(
        T_pjlvalue, current_task,
        ConstantInt::get(T_size, ptls_offset / sizeof(void *)),
        "ptls_field");
    LoadInst *ptls_load = builder.CreateAlignedLoad(T_pjlvalue,
        emit_bitcast_with_builder(builder, pptls, T_ppjlvalue), Align(sizeof(void *)), "ptls_load");
    // Note: Corresponding store (`t->ptls = ptls`) happens in `ctx_switch` of tasks.c.
    tbaa_decorate(tbaa, ptls_load);
    // Using `CastInst::Create` to get an `Instruction*` without explicit cast:
    auto ptls = CastInst::Create(Instruction::BitCast, ptls_load, T_ppjlvalue, "ptls");
    builder.Insert(ptls);
    return ptls;
}

// Compatibility shims for LLVM attribute APIs that were renamed in LLVM 14.
//
// Once we no longer support LLVM < 14, these can be mechanically removed by
// translating foo(Bar, …) into Bar->foo(…) resp. Bar.foo(…).
namespace {
using namespace llvm;

inline void addFnAttr(CallInst *Target, Attribute::AttrKind Attr)
{
#if JL_LLVM_VERSION >= 140000
    Target->addFnAttr(Attr);
#else
    Target->addAttribute(AttributeList::FunctionIndex, Attr);
#endif
}

template<class T, class A>
inline void addRetAttr(T *Target, A Attr)
{
#if JL_LLVM_VERSION >= 140000
    Target->addRetAttr(Attr);
#else
    Target->addAttribute(AttributeList::ReturnIndex, Attr);
#endif
}

inline void addAttributeAtIndex(Function *F, unsigned Index, Attribute Attr)
{
#if JL_LLVM_VERSION >= 140000
    F->addAttributeAtIndex(Index, Attr);
#else
    F->addAttribute(Index, Attr);
#endif
}

inline AttributeSet getFnAttrs(const AttributeList &Attrs)
{
#if JL_LLVM_VERSION >= 140000
    return Attrs.getFnAttrs();
#else
    return Attrs.getFnAttributes();
#endif
}

inline AttributeSet getRetAttrs(const AttributeList &Attrs)
{
#if JL_LLVM_VERSION >= 140000
    return Attrs.getRetAttrs();
#else
    return Attrs.getRetAttributes();
#endif
}

inline bool hasFnAttr(const AttributeList &L, Attribute::AttrKind Kind)
{
#if JL_LLVM_VERSION >= 140000
    return L.hasFnAttr(Kind);
#else
    return L.hasAttribute(AttributeList::FunctionIndex, Kind);
#endif
}

inline AttributeList addAttributeAtIndex(const AttributeList &L, LLVMContext &C,
                                         unsigned Index, Attribute::AttrKind Kind)
{
#if JL_LLVM_VERSION >= 140000
    return L.addAttributeAtIndex(C, Index, Kind);
#else
    return L.addAttribute(C, Index, Kind);
#endif
}

inline AttributeList addAttributeAtIndex(const AttributeList &L, LLVMContext &C,
                                         unsigned Index, Attribute Attr)
{
#if JL_LLVM_VERSION >= 140000
    return L.addAttributeAtIndex(C, Index, Attr);
#else
    return L.addAttribute(C, Index, Attr);
#endif
}

inline AttributeList addAttributesAtIndex(const AttributeList &L, LLVMContext &C,
                                          unsigned Index, const AttrBuilder &Builder)
{
#if JL_LLVM_VERSION >= 140000
    return L.addAttributesAtIndex(C, Index, Builder);
#else
    return L.addAttributes(C, Index, Builder);
#endif
}

inline AttributeList addFnAttribute(const AttributeList &L, LLVMContext &C,
                                    Attribute::AttrKind Kind)
{
#if JL_LLVM_VERSION >= 140000
    return L.addFnAttribute(C, Kind);
#else
    return L.addAttribute(C, AttributeList::FunctionIndex, Kind);
#endif
}

inline AttributeList addRetAttribute(const AttributeList &L, LLVMContext &C,
                                     Attribute::AttrKind Kind)
{
#if JL_LLVM_VERSION >= 140000
    return L.addRetAttribute(C, Kind);
#else
    return L.addAttribute(C, AttributeList::ReturnIndex, Kind);
#endif
}

inline bool hasAttributesAtIndex(const AttributeList &L, unsigned Index)
{
#if JL_LLVM_VERSION >= 140000
    return L.hasAttributesAtIndex(Index);
#else
    return L.hasAttributes(Index);
#endif
}

inline Attribute getAttributeAtIndex(const AttributeList &L, unsigned Index, Attribute::AttrKind Kind)
{
#if JL_LLVM_VERSION >= 140000
    return L.getAttributeAtIndex(Index, Kind);
#else
    return L.getAttribute(Index, Kind);
#endif
}

}
