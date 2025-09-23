// This file is a part of Julia. License is MIT: https://julialang.org/license

namespace JL_I {
#include "intrinsics.h"
}

#include <array>
#include <bitset>
#include <string>

#include "ccall.cpp"

//Mark our stats as being from intrinsics irgen
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_intrinsics"

STATISTIC(EmittedConstants, "Number of constants emitted");
STATISTIC(EmittedCoercedUnboxes, "Number of unbox coercions emitted");
STATISTIC(EmittedUnboxes, "Number of unboxes emitted");
STATISTIC(EmittedRuntimeCalls, "Number of runtime intrinsic calls emitted");
STATISTIC(EmittedIntrinsics, "Number of intrinsic calls emitted");
STATISTIC(Emitted_pointerref, "Number of pointerref calls emitted");
STATISTIC(Emitted_pointerset, "Number of pointerset calls emitted");
STATISTIC(Emitted_pointerarith, "Number of pointer arithmetic calls emitted");
STATISTIC(Emitted_atomic_fence, "Number of atomic_fence calls emitted");
STATISTIC(Emitted_atomic_pointerref, "Number of atomic_pointerref calls emitted");
STATISTIC(Emitted_atomic_pointerop, "Number of atomic_pointerop calls emitted");
STATISTIC(Emitted_bitcast, "Number of bitcast calls emitted");
STATISTIC(Emitted_trunc_int, "Number of trunc_int calls emitted");
STATISTIC(Emitted_sext_int, "Number of sext_int calls emitted");
STATISTIC(Emitted_zext_int, "Number of zext_int calls emitted");
STATISTIC(Emitted_uitofp, "Number of uitofp calls emitted");
STATISTIC(Emitted_sitofp, "Number of sitofp calls emitted");
STATISTIC(Emitted_fptoui, "Number of fptoui calls emitted");
STATISTIC(Emitted_fptosi, "Number of fptosi calls emitted");
STATISTIC(Emitted_fptrunc, "Number of fptrunc calls emitted");
STATISTIC(Emitted_fpext, "Number of fpext calls emitted");
STATISTIC(Emitted_not_int, "Number of not_int calls emitted");
STATISTIC(Emitted_have_fma, "Number of have_fma calls emitted");
STATISTIC(EmittedUntypedIntrinsics, "Number of untyped intrinsics emitted");

using namespace JL_I;

FunctionType *get_intr_args1(LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_prjlvalue_ty(C)}, false); }
FunctionType *get_intr_args2(LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C)}, false); }
FunctionType *get_intr_args3(LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C)}, false); }
FunctionType *get_intr_args4(LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C)}, false); }
FunctionType *get_intr_args5(LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C), JuliaType::get_prjlvalue_ty(C)}, false); }

const auto &runtime_func() {
    static struct runtime_funcs_t {
        std::array<JuliaFunction<> *, num_intrinsics> runtime_func;
        runtime_funcs_t() :
        runtime_func{
#define ADD_I(name, nargs) new JuliaFunction<>{XSTR(jl_##name), get_intr_args##nargs, nullptr},
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) nullptr,
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
        }
        {
#define ADD_I(name, nargs)
#define ADD_HIDDEN(name, nargs)
#define ALIAS(alias, base) runtime_func[alias] = runtime_func[base];
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
        }
    } runtime_funcs;
    return runtime_funcs.runtime_func;
}

const auto &float_func() {
    static struct float_funcs_t {
        std::bitset<num_intrinsics> float_func;
        float_funcs_t() {
            float_func[neg_float] = true;
            float_func[neg_float_fast] = true;
            float_func[add_float] = true;
            float_func[sub_float] = true;
            float_func[mul_float] = true;
            float_func[div_float] = true;
            float_func[min_float] = true;
            float_func[max_float] = true;
            float_func[add_float_fast] = true;
            float_func[sub_float_fast] = true;
            float_func[mul_float_fast] = true;
            float_func[div_float_fast] = true;
            float_func[min_float_fast] = true;
            float_func[max_float_fast] = true;
            float_func[fma_float] = true;
            float_func[muladd_float] = true;
            float_func[eq_float] = true;
            float_func[ne_float] = true;
            float_func[lt_float] = true;
            float_func[le_float] = true;
            float_func[eq_float_fast] = true;
            float_func[ne_float_fast] = true;
            float_func[lt_float_fast] = true;
            float_func[le_float_fast] = true;
            float_func[fpiseq] = true;
            float_func[abs_float] = true;
            float_func[copysign_float] = true;
            float_func[ceil_llvm] = true;
            float_func[floor_llvm] = true;
            float_func[trunc_llvm] = true;
            float_func[rint_llvm] = true;
            float_func[sqrt_llvm] = true;
            float_func[sqrt_llvm_fast] = true;
        }
    } float_funcs;

    return float_funcs.float_func;
}

extern "C" JL_DLLEXPORT_CODEGEN
uint32_t jl_get_LLVM_VERSION_impl(void)
{
    return 10000 * LLVM_VERSION_MAJOR + 100 * LLVM_VERSION_MINOR
#ifdef LLVM_VERSION_PATCH
        + LLVM_VERSION_PATCH
#endif
        ;
}

/*
  low-level intrinsics design:
  intrinsics only operate on bitstype values
  any composite type is expected to be handled via its constructor,
   so it is not permitted here
  functions like add_int expect unboxed values of matching types
  every operation that can return an unboxed value does so.
  this maximizes opportunities for composing functions without
    unnecessary boxing.
  the bitcast function does nothing except change the type tag
   of a value. At the user-level, it is perhaps better known as reinterpret.
  boxing is delayed until absolutely necessary, and handled at the point
    where the box is needed.
  all intrinsics have a non-compiled implementation, this file contains
    the optimizations for handling them unboxed
*/

// convert an llvm type to same-size float type
static Type *FLOATT(Type *t)
{
    if (t->isFloatingPointTy())
        return t;
    unsigned nb = (t->isPointerTy() ? sizeof(void*) * 8 : t->getPrimitiveSizeInBits());
    auto &ctxt = t->getContext();
    if (nb == 64)
        return getDoubleTy(ctxt);
    if (nb == 32)
        return getFloatTy(ctxt);
    if (nb == 16)
        return getHalfTy(ctxt);
    if (nb == 128)
        return getFP128Ty(ctxt);
    return NULL;
}

// convert an llvm type to same-size int type
static Type *INTT(Type *t, const DataLayout &DL)
{
    auto &ctxt = t->getContext();
    if (t->isIntegerTy())
        return t;
    if (t->isPointerTy())
        return DL.getIntPtrType(t);
    if (t == getDoubleTy(ctxt))
        return getInt64Ty(ctxt);
    if (t == getFloatTy(ctxt))
        return getInt32Ty(ctxt);
    if (t == getHalfTy(ctxt) || t == getBFloatTy(ctxt))
        return getInt16Ty(ctxt);
    unsigned nb = t->getPrimitiveSizeInBits();
    assert(t != getVoidTy(ctxt) && nb > 0);
    return IntegerType::get(ctxt, nb);
}

static Value *uint_cnvt(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateZExtOrTrunc(x, to);
}

static Constant *julia_const_to_llvm(jl_codectx_t &ctx, const void *ptr, jl_datatype_t *bt)
{
    // assumes `jl_is_pointerfree(bt)`.
    // `ptr` can point to a inline field, do not read the tag from it.
    // make sure to return exactly the type specified by
    // julia_type_to_llvm as this will be assumed by the callee.
    if (bt == jl_bool_type)
        return ConstantInt::get(getInt8Ty(ctx.builder.getContext()), (*(const uint8_t*)ptr) ? 1 : 0);

    Type *lt = julia_struct_to_llvm(ctx, (jl_value_t*)bt, NULL);

    if (jl_is_vecelement_type((jl_value_t*)bt) && !jl_is_uniontype(jl_tparam0(bt)))
        bt = (jl_datatype_t*)jl_tparam0(bt);

    if (type_is_ghost(lt))
        return UndefValue::get(lt);

    if (lt->isFloatTy()) {
        uint32_t data32 = *(const uint32_t*)ptr;
        return ConstantFP::get(ctx.builder.getContext(),
                APFloat(lt->getFltSemantics(), APInt(32, data32)));
    }
    if (lt->isDoubleTy()) {
        uint64_t data64 = *(const uint64_t*)ptr;
        return ConstantFP::get(ctx.builder.getContext(),
                APFloat(lt->getFltSemantics(), APInt(64, data64)));
    }
    if (lt->isFloatingPointTy() || lt->isIntegerTy() || lt->isPointerTy()) {
        int nb = jl_datatype_size(bt);
        APInt val(8 * nb, 0);
        void *bits = const_cast<uint64_t*>(val.getRawData());
        assert(sys::IsLittleEndianHost);
        memcpy(bits, ptr, nb);
        if (lt->isFloatingPointTy()) {
            return ConstantFP::get(ctx.builder.getContext(),
                    APFloat(lt->getFltSemantics(), val));
        }
        if (lt->isPointerTy()) {
            Type *Ty = IntegerType::get(ctx.builder.getContext(), 8 * nb);
            Constant *addr = ConstantInt::get(Ty, val);
            return ConstantExpr::getIntToPtr(addr, lt);
        }
        assert(cast<IntegerType>(lt)->getBitWidth() == 8u * nb);
        return ConstantInt::get(lt, val);
    }

    size_t nf = jl_datatype_nfields(bt);
    SmallVector<Constant*, 0> fields(0);
    for (size_t i = 0; i < nf; i++) {
        size_t offs = jl_field_offset(bt, i);
        jl_value_t *ft = jl_field_type(bt, i);
        Type *lft = julia_type_to_llvm(ctx, ft);
        if (type_is_ghost(lft))
            continue;
        assert(!jl_field_isptr(bt, i));
        unsigned llvm_idx = isa<StructType>(lt) ? convert_struct_offset(jl_Module->getDataLayout(), lt, offs) : i;
        while (fields.size() < llvm_idx)
            fields.push_back(
                UndefValue::get(GetElementPtrInst::getTypeAtIndex(lt, fields.size())));
        const uint8_t *ov = (const uint8_t*)ptr + offs;
        if (jl_is_uniontype(ft)) {
            // compute the same type layout as julia_struct_to_llvm
            size_t fsz = 0, al = 0;
            (void)jl_islayout_inline(ft, &fsz, &al); // compute al
            fsz = jl_field_size(bt, i); // get LLT_ALIGN(fsz+1,al)
            uint8_t sel = ((const uint8_t*)ptr)[offs + fsz - 1];
            jl_value_t *active_ty = jl_nth_union_component(ft, sel);
            size_t active_sz = jl_datatype_size(active_ty);
            Type *AlignmentType = IntegerType::get(ctx.builder.getContext(), 8 * al);
            unsigned NumATy = (fsz - 1) / al;
            unsigned remainder = (fsz - 1) % al;
            while (NumATy--) {
                Constant *fld;
                if (active_sz > 0) {
                    APInt Elem(8 * al, 0);
                    void *bits = const_cast<uint64_t*>(Elem.getRawData());
                    if (active_sz > al) {
                        memcpy(bits, ov, al);
                        active_sz -= al;
                    }
                    else {
                        memcpy(bits, ov, active_sz);
                        active_sz = 0;
                    }
                    fld = ConstantInt::get(AlignmentType, Elem);
                }
                else {
                    fld = UndefValue::get(AlignmentType);
                }
                ov += al;
                fields.push_back(fld);
            }
            while (remainder--) {
                Constant *fld;
                if (active_sz > 0) {
                    uint8_t byte = *ov;
                    APInt Elem(8, byte);
                    active_sz -= 1;
                    fld = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), Elem);
                }
                else {
                    fld = UndefValue::get(getInt8Ty(ctx.builder.getContext()));
                }
                ov += 1;
                fields.push_back(fld);
            }
            fields.push_back(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), sel));
        }
        else {
            Constant *val = julia_const_to_llvm(ctx, ov, (jl_datatype_t*)ft);
            fields.push_back(val);
        }
    }

    if (lt->isVectorTy())
        return ConstantVector::get(fields);
    if (StructType *st = dyn_cast<StructType>(lt))
        return ConstantStruct::get(st, fields);
    if (ArrayType *at = dyn_cast<ArrayType>(lt))
        return ConstantArray::get(at, fields);
    assert(false && "Unknown LLVM type");
    jl_unreachable();
}

static Constant *julia_const_to_llvm(jl_codectx_t &ctx, jl_value_t *e)
{
    if (e == jl_true)
        return ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1);
    if (e == jl_false)
        return ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0);
    jl_value_t *bt = jl_typeof(e);
    if (!jl_is_pointerfree(bt))
        return NULL;
    return julia_const_to_llvm(ctx, e, (jl_datatype_t*)bt);
}

static Constant *undef_value_for_type(Type *T) {
    auto tracked = CountTrackedPointers(T);
    Constant *undef;
    if (tracked.count)
        // make sure gc pointers (including ptr_phi of union-split) are initialized to NULL
        undef = Constant::getNullValue(T);
    else
        undef = UndefValue::get(T);
    return undef;
}

// rebuild a struct type with any i1 Bool (e.g. the llvmcall type) widened to i8 (the native size for memcpy)
static Type *zext_struct_type(Type *T)
{
    if (auto *AT = dyn_cast<ArrayType>(T)) {
        return ArrayType::get(AT->getElementType(), AT->getNumElements());
    }
    else if (auto *ST = dyn_cast<StructType>(T)) {
        SmallVector<Type*> Elements(ST->element_begin(), ST->element_end());
        for (size_t i = 0; i < Elements.size(); i++) {
            Elements[i] = zext_struct_type(Elements[i]);
        }
        return StructType::get(ST->getContext(), Elements, ST->isPacked());
    }
    else if (auto *VT = dyn_cast<VectorType>(T)) {
        return VectorType::get(zext_struct_type(VT->getElementType()), VT);
    }
    else if (auto *IT = dyn_cast<IntegerType>(T)) {
        unsigned BitWidth = IT->getBitWidth();
        if (alignTo(BitWidth, 8) != BitWidth)
            return IntegerType::get(IT->getContext(), alignTo(BitWidth, 8));
    }
    return T;
}

// rebuild a struct with any i1 Bool (e.g. the llvmcall type) widened to i8 (the native size for memcpy)
static Value *zext_struct_helper(jl_codectx_t &ctx, Value *V, Type *T2)
{
    Type *T = V->getType();
    if (T == T2)
        return V;
    if (auto *AT = dyn_cast<ArrayType>(T2)) {
        Value *V2 = undef_value_for_type(AT);
        for (size_t i = 0; i < AT->getNumElements(); i++) {
            Value *E = zext_struct_helper(ctx, ctx.builder.CreateExtractValue(V, i), AT->getElementType());
            V2 = ctx.builder.CreateInsertValue(V2, E, i);
        }
        return V2;
    }
    else if (auto *ST = dyn_cast<StructType>(T2)) {
        Value *V2 = undef_value_for_type(ST);
        for (size_t i = 0; i < ST->getNumElements(); i++) {
            Value *E = zext_struct_helper(ctx, ctx.builder.CreateExtractValue(V, i), ST->getElementType(i));
            V2 = ctx.builder.CreateInsertValue(V2, E, i);
        }
        return V2;
    }
    else if (T2->isIntegerTy() || T2->isVectorTy()) {
        return ctx.builder.CreateZExt(V, T2);
    }
    return V;
}

static Value *zext_struct(jl_codectx_t &ctx, Value *V)
{
    return zext_struct_helper(ctx, V, zext_struct_type(V->getType()));
}

static Value *emit_unboxed_coercion(jl_codectx_t &ctx, Type *to, Value *unboxed)
{
    if (unboxed->getType() == to)
        return unboxed;
    if (CastInst::castIsValid(Instruction::Trunc, unboxed, to))
        return ctx.builder.CreateTrunc(unboxed, to);
    unboxed = zext_struct(ctx, unboxed);
    Type *ty = unboxed->getType();
    if (ty == to)
        return unboxed;
    bool frompointer = ty->isPointerTy();
    bool topointer = to->isPointerTy();
    const DataLayout &DL = jl_Module->getDataLayout();
    if (ty->isVoidTy() || DL.getTypeSizeInBits(ty) != DL.getTypeSizeInBits(to)) {
        // this can happen in dead code
        CreateTrap(ctx.builder);
        return UndefValue::get(to);
    }
    else if (!ty->isIntOrPtrTy() && !ty->isFloatingPointTy()) {
        assert(DL.getTypeSizeInBits(ty) == DL.getTypeSizeInBits(to));
        Align align = std::max(DL.getPrefTypeAlign(ty), DL.getPrefTypeAlign(to));
        AllocaInst *cast = emit_static_alloca(ctx, ty, align);
        setName(ctx.emission_context, cast, "coercion");
        ctx.builder.CreateAlignedStore(unboxed, cast, align);
        unboxed = ctx.builder.CreateAlignedLoad(to, cast, align);
    }
    else if (frompointer) {
        Type *INTT_to = INTT(to, DL);
        unboxed = ctx.builder.CreatePtrToInt(unboxed, INTT_to);
        setName(ctx.emission_context, unboxed, "coercion");
        if (INTT_to != to) //TODO when is this true?
            unboxed = ctx.builder.CreateBitCast(unboxed, to);
    }
    else if (topointer) {
        Type *INTT_to = INTT(to, DL);
        if (to != INTT_to) //TODO when is this true?
            unboxed = ctx.builder.CreateBitCast(unboxed, INTT_to);
        unboxed = emit_inttoptr(ctx, unboxed, to);
        setName(ctx.emission_context, unboxed, "coercion");
    }
    else {
        unboxed = ctx.builder.CreateBitCast(unboxed, to);
    }
    return unboxed;
}

// emit code to unpack a raw value from a box into registers
static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x)
{
    assert(to != getVoidTy(ctx.builder.getContext()));
    if (x.isghost) {
        // this can happen when a branch yielding a different type ends
        // up being dead code, and type inference knows that the other
        // branch's type is the only one that matters.
        if (type_is_ghost(to)) {
            return nullptr;
        }
        CreateTrap(ctx.builder);
        return UndefValue::get(to); // type mismatch error
    }

    Constant *c = x.constant ? julia_const_to_llvm(ctx, x.constant) : nullptr;
    if ((x.inline_roots.empty() && !x.ispointer()) || c != nullptr) { // already unboxed, but sometimes need conversion
        Value *unboxed = c ? c : x.V;
        assert(unboxed); // clang-sa doesn't know that !x.ispointer() implies x.V does have a value
        return emit_unboxed_coercion(ctx, to, unboxed);
    }

    // bools stored as int8, so an extra Trunc is needed to get an int1
    Value *p = x.constant ? literal_pointer_val(ctx, x.constant) : x.V;

    if (x.typ == (jl_value_t*)jl_bool_type || to->isIntegerTy(1)) {
        assert(p && x.inline_roots.empty()); // clang-sa doesn't know that x.ispointer() implied these are true
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, x.tbaa);
        Instruction *unbox_load = ai.decorateInst(ctx.builder.CreateLoad(getInt8Ty(ctx.builder.getContext()), p));
        setName(ctx.emission_context, unbox_load, p->getName() + ".unbox");
        if (x.typ == (jl_value_t*)jl_bool_type)
            unbox_load->setMetadata(LLVMContext::MD_range, MDNode::get(ctx.builder.getContext(), {
                ConstantAsMetadata::get(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0)),
                ConstantAsMetadata::get(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 2)) }));
        Value *unboxed;
        if (to->isIntegerTy(1))
            unboxed = ctx.builder.CreateTrunc(unbox_load, to);
        else
            unboxed = unbox_load; // `to` must be Int8Ty
        return unboxed;
    }

    unsigned alignment = julia_alignment(x.typ);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, x.tbaa);
    if (!x.inline_roots.empty()) {
        AllocaInst *combined = emit_static_alloca(ctx, to, Align(alignment));
        auto combined_ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack);
        recombine_value(ctx, x, combined, combined_ai, Align(alignment), false);
        p = combined;
        ai = combined_ai;
    }
    assert(p); // clang-sa doesn't know that x.ispointer() implied this is true
    Instruction *load = ctx.builder.CreateAlignedLoad(to, p, Align(alignment));
    setName(ctx.emission_context, load, p->getName() + ".unbox");
    return ai.decorateInst(load);
}

// emit code to store a raw value into a destination
static void emit_unbox_store(jl_codectx_t &ctx, const jl_cgval_t &x, Value *dest, MDNode *tbaa_dest, MaybeAlign align_src, Align align_dst, bool isVolatile)
{
    if (x.isghost) {
        // this can happen when a branch yielding a different type ends
        // up being dead code, and type inference knows that the other
        // branch's type is the only one that matters.
        return;
    }

    auto dest_ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa_dest);

    if (!x.inline_roots.empty()) {
        recombine_value(ctx, x, dest, dest_ai, align_dst, isVolatile);
        return;
    }

    if (!x.ispointer()) { // already unboxed, but sometimes need conversion (e.g. f32 -> i32)
        assert(x.V);
        Value *unboxed = zext_struct(ctx, x.V);
        StoreInst *store = ctx.builder.CreateAlignedStore(unboxed, dest, align_dst);
        store->setVolatile(isVolatile);
        dest_ai.decorateInst(store);
        return;
    }

    Value *src = data_pointer(ctx, x);
    auto src_ai = jl_aliasinfo_t::fromTBAA(ctx, x.tbaa);
    emit_memcpy(ctx, dest, dest_ai, src, src_ai, jl_datatype_size(x.typ), Align(align_dst), align_src ? *align_src : Align(julia_alignment(x.typ)), isVolatile);
}

static jl_datatype_t *staticeval_bitstype(const jl_cgval_t &targ)
{
    // evaluate an argument at compile time to determine what type it is
    jl_value_t *unw = jl_unwrap_unionall(targ.typ);
    if (jl_is_type_type(unw)) {
        jl_value_t *bt = jl_tparam0(unw);
        if (jl_is_primitivetype(bt))
            return (jl_datatype_t*)bt;
    }
    return NULL;
}

static jl_cgval_t emit_runtime_call(jl_codectx_t &ctx, JL_I::intrinsic f, ArrayRef<jl_cgval_t> argv, size_t nargs)
{
    Function *func = prepare_call(runtime_func()[f]);
    SmallVector<Value *, 0> argvalues(nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argvalues[i] = boxed(ctx, argv[i]);
    }
    Value *r = ctx.builder.CreateCall(func, argvalues);
    return mark_julia_type(ctx, r, true, (jl_value_t*)jl_any_type);
}

// put a bits type tag on some value (despite the name, this doesn't necessarily actually change anything about the value however)
static jl_cgval_t generic_bitcast(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    // Give the arguments names //
    const jl_cgval_t &bt_value = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_datatype_t *bt = staticeval_bitstype(bt_value);

    // it's easier to throw a good error from C than llvm
    if (!bt)
        return emit_runtime_call(ctx, bitcast, argv, 2);

    Type *llvmt = bitstype_to_llvm((jl_value_t*)bt, ctx.builder.getContext(), true);
    uint32_t nb = jl_datatype_size(bt);

    Value *bt_value_rt = NULL;
    if (!jl_is_concrete_type((jl_value_t*)bt)) {
        bt_value_rt = boxed(ctx, bt_value);
        emit_concretecheck(ctx, bt_value_rt, "bitcast: target type not a leaf primitive type");
    }

    // Examine the second argument //
    bool isboxed;
    Type *vxt = julia_type_to_llvm(ctx, v.typ, &isboxed);
    if (!jl_is_primitivetype(v.typ) || jl_datatype_size(v.typ) != nb) {
        Value *typ = emit_typeof(ctx, v, false, false);
        if (!jl_is_primitivetype(v.typ)) {
            if (jl_is_datatype(v.typ) && !jl_is_abstracttype(v.typ)) {
                emit_error(ctx, "bitcast: value not a primitive type");
                return jl_cgval_t();
            }
            else {
                Value *isprimitive = emit_datatype_isprimitivetype(ctx, typ);
                error_unless(ctx, isprimitive, "bitcast: value not a primitive type");
            }
        }
        if (jl_is_datatype(v.typ) && !jl_is_abstracttype(v.typ)) {
            emit_error(ctx, "bitcast: argument size does not match size of target type");
            return jl_cgval_t();
        }
        else {
            Value *size = emit_datatype_size(ctx, typ);
            auto sizecheck = ctx.builder.CreateICmpEQ(size, ConstantInt::get(size->getType(), nb));
            setName(ctx.emission_context, sizecheck, "sizecheck");
            error_unless(ctx,
                    sizecheck,
                    "bitcast: argument size does not match size of target type");
        }
    }

    assert(!v.isghost);
    Value *vx = NULL;
    if (!v.ispointer())
        vx = v.V;
    else if (v.constant)
        vx = julia_const_to_llvm(ctx, v.constant);

    if (v.ispointer() && vx == NULL) {
        // try to load as original Type, to preserve llvm optimizations
        // but if the v.typ is not well known, use llvmt
        if (isboxed)
            vxt = llvmt;
        auto storage_type = vxt->isIntegerTy(1) ? getInt8Ty(ctx.builder.getContext()) : vxt;
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, v.tbaa);
        vx = ai.decorateInst(ctx.builder.CreateLoad(
            storage_type,
            data_pointer(ctx, v)));
        setName(ctx.emission_context, vx, "bitcast");
    }

    vxt = vx->getType();
    if (vxt != llvmt) {
        if (llvmt->isIntegerTy(1)) {
            vx = ctx.builder.CreateTrunc(vx, llvmt);
        } else if (vxt->isIntegerTy(1) && llvmt->isIntegerTy(8)) {
            vx = ctx.builder.CreateZExt(vx, llvmt);
        } else if (vxt->isPointerTy() && !llvmt->isPointerTy()) {
            vx = ctx.builder.CreatePtrToInt(vx, llvmt);
            if (isa<Instruction>(vx) && !vx->hasName())
                // CreatePtrToInt may undo an IntToPtr
                setName(ctx.emission_context, vx, "bitcast_coercion");
        } else if (!vxt->isPointerTy() && llvmt->isPointerTy()) {
            vx = emit_inttoptr(ctx, vx, llvmt);
            if (isa<Instruction>(vx) && !vx->hasName())
                // emit_inttoptr may undo an PtrToInt
                setName(ctx.emission_context, vx, "bitcast_coercion");
        } else if (vxt->isPointerTy() && llvmt->isPointerTy()) {
            // emit_bitcast preserves the origin address space, which we can't have here
            vx = ctx.builder.CreateAddrSpaceCast(vx, llvmt);
            if (isa<Instruction>(vx) && !vx->hasName())
                // cast may have been folded
                setName(ctx.emission_context, vx, "bitcast_coercion");
        } else {
            vx = emit_bitcast(ctx, vx, llvmt);
            if (isa<Instruction>(vx) && !vx->hasName())
                // emit_bitcast may undo another bitcast
                setName(ctx.emission_context, vx, "bitcast_coercion");
        }
    }

    if (jl_is_concrete_type((jl_value_t*)bt)) {
        return mark_julia_type(ctx, vx, false, bt);
    }
    else {
        unsigned align = sizeof(void*); // Allocations are at least pointer aligned
        Value *box = emit_allocobj(ctx, nb, bt_value_rt, true, align);
        setName(ctx.emission_context, box, "bitcast_box");
        init_bits_value(ctx, box, vx, ctx.tbaa().tbaa_immut);
        return mark_julia_type(ctx, box, true, bt->name->wrapper);
    }
}

static jl_cgval_t generic_cast(
        jl_codectx_t &ctx,
        intrinsic f, Instruction::CastOps Op,
        ArrayRef<jl_cgval_t> argv, bool toint, bool fromint)
{
    auto &TT = ctx.emission_context.TargetTriple;
    auto &DL = ctx.emission_context.DL;
    const jl_cgval_t &targ = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_datatype_t *jlto = staticeval_bitstype(targ);
    if (!jlto || !jl_is_primitivetype(v.typ))
        return emit_runtime_call(ctx, f, argv, 2);
    uint32_t nb = jl_datatype_size(jlto);
    Type *to = bitstype_to_llvm((jl_value_t*)jlto, ctx.builder.getContext(), true);
    Type *vt = bitstype_to_llvm(v.typ, ctx.builder.getContext(), true);

    // fptrunc and fpext depend on the specific floating point
    // format to work correctly, and so do not pun their argument types.
    if (!(f == fpext || f == fptrunc)) {
        // uitofp/sitofp require a specific float type argument
        if (!(f == uitofp || f == sitofp)){
            if (toint)
                to = INTT(to, DL);
            else
                to = FLOATT(to);
        }
        // fptoui/fptosi require a specific float value argument
        if (!(f == fptoui || f == fptosi)) {
            if (fromint)
                vt = INTT(vt, DL);
            else
                vt = FLOATT(vt);
        }
    }

    if (!to || !vt)
        return emit_runtime_call(ctx, f, argv, 2);

    Value *from = emit_unbox(ctx, vt, v);
    if (!CastInst::castIsValid(Op, from, to))
        return emit_runtime_call(ctx, f, argv, 2);
    if (Op == Instruction::FPExt) {
        if (jl_floattemp_var_needed(TT)) {
            // Target platform might carry extra precision.
            // Force rounding to single precision first. The reason is that it's
            // fine to keep working in extended precision as long as it's
            // understood that everything is implicitly rounded to 23 bits,
            // but if we start looking at more bits we need to actually do the
            // rounding first instead of carrying around incorrect low bits.
            Align align(julia_alignment((jl_value_t*)jlto));
            Value *jlfloattemp_var = emit_static_alloca(ctx, from->getType(), align);
            setName(ctx.emission_context, jlfloattemp_var, "rounding_slot");
            ctx.builder.CreateAlignedStore(from, jlfloattemp_var, align);
            from = ctx.builder.CreateAlignedLoad(from->getType(), jlfloattemp_var, align, /*force this to load from the stack*/true);
            setName(ctx.emission_context, from, "rounded");
        }
    }
    Value *ans = ctx.builder.CreateCast(Op, from, to);
    if (f == fptosi || f == fptoui)
        ans = ctx.builder.CreateFreeze(ans);
    if (jl_is_concrete_type((jl_value_t*)jlto)) {
        return mark_julia_type(ctx, ans, false, jlto);
    }
    else {
        Value *targ_rt = boxed(ctx, targ);
        emit_concretecheck(ctx, targ_rt, std::string(jl_intrinsic_name(f)) + ": target type not a leaf primitive type");
        unsigned align = sizeof(void*); // Allocations are at least pointer aligned
        Value *box = emit_allocobj(ctx, nb, targ_rt, true, align);
        setName(ctx.emission_context, box, "cast_box");
        init_bits_value(ctx, box, ans, ctx.tbaa().tbaa_immut);
        return mark_julia_type(ctx, box, true, jlto->name->wrapper);
    }
}

static jl_cgval_t emit_runtime_pointerref(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    return emit_runtime_call(ctx, pointerref, argv, 3);
}

static jl_cgval_t emit_pointerref(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    const jl_cgval_t &e = argv[0];
    const jl_cgval_t &i = argv[1];
    const jl_cgval_t &align = argv[2];

    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerref(ctx, argv);
    unsigned align_nb = jl_unbox_long(align.constant);

    if (i.typ != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerref(ctx, argv);
    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerref(ctx, argv);
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerref(ctx, argv);
    if (!is_valid_intrinsic_elptr(ety)) {
        emit_error(ctx, "pointerref: invalid pointer type");
        return jl_cgval_t();
    }

    Value *idx = emit_unbox(ctx, ctx.types().T_size, i);
    Value *im1 = ctx.builder.CreateSub(idx, ConstantInt::get(ctx.types().T_size, 1));
    setName(ctx.emission_context, im1, "pointerref_idx");

    if (ety == (jl_value_t*)jl_any_type) {
        Value *thePtr = emit_unbox(ctx, ctx.types().T_pprjlvalue, e);
        if (isa<Instruction>(thePtr) && !thePtr->hasName())
            setName(ctx.emission_context, thePtr, "unbox_any_ptr");
        LoadInst *load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, thePtr, im1), Align(align_nb));
        setName(ctx.emission_context, load, "any_unbox");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_data);
        ai.decorateInst(load);
        return mark_julia_type(ctx, load, true, ety);
    }
    else if (!deserves_stack(ety)) {
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(ctx, (jl_datatype_t*)ety, true);
        setName(ctx.emission_context, strct, "pointerref_box");
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(ctx.types().T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        setName(ctx.emission_context, im1, "pointerref_offset");
        Value *thePtr = emit_unbox(ctx, getPointerTy(ctx.builder.getContext()), e);
        thePtr = emit_ptrgep(ctx, thePtr, im1);
        setName(ctx.emission_context, thePtr, "pointerref_src");
        MDNode *tbaa = best_tbaa(ctx.tbaa(), ety);
        emit_memcpy(ctx, strct, jl_aliasinfo_t::fromTBAA(ctx, tbaa), thePtr, jl_aliasinfo_t::fromTBAA(ctx, nullptr), size, Align(sizeof(jl_value_t*)), Align(align_nb));
        return mark_julia_type(ctx, strct, true, ety);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ctx, ety, &isboxed);
        assert(!isboxed);
        if (!type_is_ghost(ptrty)) {
            Value *thePtr = emit_unbox(ctx, PointerType::getUnqual(ptrty->getContext()), e);
            thePtr = ctx.builder.CreateInBoundsGEP(ptrty, thePtr, im1);
            auto load = typed_load(ctx, thePtr, nullptr, ety, ctx.tbaa().tbaa_data, nullptr, isboxed, AtomicOrdering::NotAtomic, false, align_nb);
            setName(ctx.emission_context, load.V, "pointerref");
            return load;
        }
        else {
            return ghostValue(ctx, ety);
        }
    }
}

static jl_cgval_t emit_runtime_pointerset(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    return emit_runtime_call(ctx, pointerset, argv, 4);
}

// e[i] = x
static jl_cgval_t emit_pointerset(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    const jl_cgval_t &e = argv[0];
    jl_cgval_t x = argv[1];
    const jl_cgval_t &i = argv[2];
    const jl_cgval_t &align = argv[3];

    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerset(ctx, argv);
    unsigned align_nb = jl_unbox_long(align.constant);

    if (i.typ != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerset(ctx, argv);
    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerset(ctx, argv);
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerset(ctx, argv);
    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerset(ctx, argv);
    if (!is_valid_intrinsic_elptr(ety)) {
        emit_error(ctx, "pointerset: invalid pointer type");
        return jl_cgval_t();
    }
    emit_typecheck(ctx, x, ety, "pointerset");
    x = update_julia_type(ctx, x, ety);
    if (x.typ == jl_bottom_type)
        return jl_cgval_t();

    Value *idx = emit_unbox(ctx, ctx.types().T_size, i);
    Value *im1 = ctx.builder.CreateSub(idx, ConstantInt::get(ctx.types().T_size, 1));
    setName(ctx.emission_context, im1, "pointerset_idx");

    Value *thePtr = emit_unbox(ctx, getPointerTy(ctx.builder.getContext()), e);
    if (ety == (jl_value_t*)jl_any_type) {
        // unsafe_store to Ptr{Any} is allowed to implicitly drop GC roots.
        auto gep = ctx.builder.CreateInBoundsGEP(ctx.types().T_size, thePtr, im1);
        setName(ctx.emission_context, gep, "pointerset_ptr");
        auto val = ctx.builder.CreatePtrToInt(emit_pointer_from_objref(ctx, boxed(ctx, x)), ctx.types().T_size);
        setName(ctx.emission_context, val, "pointerset_val");
        Instruction *store = ctx.builder.CreateAlignedStore(val, gep, Align(align_nb));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_data);
        ai.decorateInst(store);
    }
    else if (!x.inline_roots.empty()) {
        recombine_value(ctx, e, thePtr, jl_aliasinfo_t(), Align(align_nb), false);
    }
    else if (x.ispointer()) {
        uint64_t size = jl_datatype_size(ety);
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(ctx.types().T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        setName(ctx.emission_context, im1, "pointerset_offset");
        auto gep = emit_ptrgep(ctx, thePtr, im1);
        setName(ctx.emission_context, gep, "pointerset_ptr");
        emit_memcpy(ctx, gep, jl_aliasinfo_t::fromTBAA(ctx, nullptr), x, size, Align(align_nb), Align(julia_alignment(ety)));
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ctx, ety, &isboxed);
        assert(!isboxed);
        if (!type_is_ghost(ptrty)) {
            thePtr = ctx.builder.CreateInBoundsGEP(ptrty, thePtr, im1);
            typed_store(ctx, thePtr, x, jl_cgval_t(), ety, ctx.tbaa().tbaa_data, nullptr, nullptr, isboxed,
                        AtomicOrdering::NotAtomic, AtomicOrdering::NotAtomic, align_nb, nullptr, true, false, false, false, false, false, nullptr, "atomic_pointerset", nullptr, nullptr);
        }
    }
    return e;
}

// ptr + offset
// ptr - offset
static jl_cgval_t emit_pointerarith(jl_codectx_t &ctx, intrinsic f,
                                    ArrayRef<jl_cgval_t> argv)
{
    jl_value_t *ptrtyp = argv[0].typ;
    jl_value_t *offtyp = argv[1].typ;
    if (!jl_is_cpointer_type(ptrtyp) || offtyp != (jl_value_t *)jl_ulong_type)
        return emit_runtime_call(ctx, f, argv, argv.size());
    assert(f == add_ptr || f == sub_ptr);

    Value *ptr = emit_unbox(ctx, ctx.types().T_ptr, argv[0]);
    Value *off = emit_unbox(ctx, ctx.types().T_size, argv[1]);
    if (f == sub_ptr)
        off = ctx.builder.CreateNeg(off);
    Value *ans = ctx.builder.CreateGEP(getInt8Ty(ctx.builder.getContext()), ptr, off);

    if (jl_is_concrete_type(ptrtyp)) {
        return mark_julia_type(ctx, ans, false, ptrtyp);
    }
    else {
        Value *box = emit_allocobj(ctx, (jl_datatype_t *)ptrtyp, true);
        setName(ctx.emission_context, box, "ptr_box");
        init_bits_value(ctx, box, ans, ctx.tbaa().tbaa_immut);
        return mark_julia_type(ctx, box, true, (jl_datatype_t *)ptrtyp);
    }
}

static jl_cgval_t emit_atomicfence(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    const jl_cgval_t &ord = argv[0];
    if (ord.constant && jl_is_symbol(ord.constant)) {
        enum jl_memory_order order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, true);
        if (order == jl_memory_order_invalid) {
            emit_atomic_error(ctx, "invalid atomic ordering");
            return jl_cgval_t(); // unreachable
        }
        if (order > jl_memory_order_monotonic)
            ctx.builder.CreateFence(get_llvm_atomic_order(order));
        return ghostValue(ctx, jl_nothing_type);
    }
    return emit_runtime_call(ctx, atomic_fence, argv, 1);
}

static jl_cgval_t emit_atomic_pointerref(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> argv)
{
    const jl_cgval_t &e = argv[0];
    const jl_cgval_t &ord = argv[1];
    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty) || !ord.constant || !jl_is_symbol(ord.constant))
        return emit_runtime_call(ctx, atomic_pointerref, argv, 2);
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_call(ctx, atomic_pointerref, argv, 2);
    enum jl_memory_order order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
    if (order == jl_memory_order_invalid) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        return jl_cgval_t(); // unreachable
    }
    AtomicOrdering llvm_order = get_llvm_atomic_order(order);

    if (ety == (jl_value_t*)jl_any_type) {
        Value *thePtr = emit_unbox(ctx, ctx.types().T_pprjlvalue, e);
        LoadInst *load = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, thePtr, Align(sizeof(jl_value_t*)));
        setName(ctx.emission_context, load, "atomic_pointerref");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_data);
        ai.decorateInst(load);
        load->setOrdering(llvm_order);
        return mark_julia_type(ctx, load, true, ety);
    }

    if (!is_valid_intrinsic_elptr(ety)) {
        emit_error(ctx, "atomic_pointerref: invalid pointer type");
        return jl_cgval_t();
    }

    size_t nb = jl_datatype_size(ety);
    if ((nb & (nb - 1)) != 0 || nb > MAX_POINTERATOMIC_SIZE) {
        emit_error(ctx, "atomic_pointerref: invalid pointer for atomic operation");
        return jl_cgval_t();
    }

    if (!deserves_stack(ety)) {
        assert(jl_is_datatype(ety));
        Value *strct = emit_allocobj(ctx, (jl_datatype_t*)ety, true);
        setName(ctx.emission_context, strct, "atomic_pointerref_box");
        Value *thePtr = emit_unbox(ctx, getPointerTy(ctx.builder.getContext()), e);
        Type *loadT = Type::getIntNTy(ctx.builder.getContext(), nb * 8);
        MDNode *tbaa = best_tbaa(ctx.tbaa(), ety);
        LoadInst *load = ctx.builder.CreateAlignedLoad(loadT, thePtr, Align(nb));
        setName(ctx.emission_context, load, "atomic_pointerref");
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        ai.decorateInst(load);
        load->setOrdering(llvm_order);
        thePtr = strct;
        StoreInst *store = ctx.builder.CreateAlignedStore(load, thePtr, Align(julia_alignment(ety)));
        ai.decorateInst(store);
        return mark_julia_type(ctx, strct, true, ety);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ctx, ety, &isboxed);
        assert(!isboxed);
        if (!type_is_ghost(ptrty)) {
            Value *thePtr = emit_unbox(ctx, PointerType::getUnqual(ptrty->getContext()), e);
            auto load = typed_load(ctx, thePtr, nullptr, ety, ctx.tbaa().tbaa_data, nullptr, isboxed, llvm_order, false, nb);
            setName(ctx.emission_context, load.V, "atomic_pointerref");
            return load;
        }
        else {
            if (order > jl_memory_order_monotonic)
                ctx.builder.CreateFence(llvm_order);
            return ghostValue(ctx, ety);
        }
    }
}

// e[i] = x (set)
// e[i] <= x (swap)
// e[i] y => x (replace)
// x(e[i], y) (modify)
static jl_cgval_t emit_atomic_pointerop(jl_codectx_t &ctx, intrinsic f, ArrayRef<jl_cgval_t> argv, int nargs, const jl_cgval_t *modifyop)
{
    bool issetfield = f == atomic_pointerset;
    bool isreplacefield = f == atomic_pointerreplace;
    bool isswapfield = f == atomic_pointerswap;
    bool ismodifyfield = f == atomic_pointermodify;
    const jl_cgval_t undefval;
    const jl_cgval_t &e = argv[0];
    jl_cgval_t x = isreplacefield || ismodifyfield ? argv[2] : argv[1];
    const jl_cgval_t &y = isreplacefield || ismodifyfield ? argv[1] : undefval;
    const jl_cgval_t &ord = isreplacefield || ismodifyfield ? argv[3] : argv[2];
    const jl_cgval_t &failord = isreplacefield ? argv[4] : undefval;

    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty) || !ord.constant || !jl_is_symbol(ord.constant))
        return emit_runtime_call(ctx, f, argv, nargs);
    if (isreplacefield) {
        if (!failord.constant || !jl_is_symbol(failord.constant))
            return emit_runtime_call(ctx, f, argv, nargs);
    }
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_call(ctx, f, argv, nargs);
    enum jl_memory_order order = jl_get_atomic_order((jl_sym_t*)ord.constant, !issetfield, true);
    enum jl_memory_order failorder = isreplacefield ? jl_get_atomic_order((jl_sym_t*)failord.constant, true, false) : order;
    if (order == jl_memory_order_invalid || failorder == jl_memory_order_invalid || failorder > order) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        return jl_cgval_t(); // unreachable
    }
    AtomicOrdering llvm_order = get_llvm_atomic_order(order);
    AtomicOrdering llvm_failorder = get_llvm_atomic_order(failorder);

    if (ety == (jl_value_t*)jl_any_type) {
        // unsafe_store to Ptr{Any} is allowed to implicitly drop GC roots.
        // n.b.: the expected value (y) must be rooted, but not the others
        Value *thePtr = emit_unbox(ctx, ctx.types().T_pprjlvalue, e);
        bool isboxed = true;
        jl_cgval_t ret = typed_store(ctx, thePtr, x, y, ety, ctx.tbaa().tbaa_data, nullptr, nullptr, isboxed,
                    llvm_order, llvm_failorder, sizeof(jl_value_t*), nullptr, issetfield, isreplacefield, isswapfield, ismodifyfield, false, false, modifyop, "atomic_pointermodify", nullptr, nullptr);
        if (issetfield)
            ret = e;
        return ret;
    }

    if (!is_valid_intrinsic_elptr(ety)) {
        std::string msg(StringRef(jl_intrinsic_name((int)f)));
        msg += ": invalid pointer type";
        emit_error(ctx, msg);
        return jl_cgval_t();
    }
    if (!ismodifyfield) {
        emit_typecheck(ctx, x, ety, std::string(jl_intrinsic_name((int)f)));
        x = update_julia_type(ctx, x, ety);
        if (x.typ == jl_bottom_type)
            return jl_cgval_t();
    }

    size_t nb = jl_datatype_size(ety);
    if ((nb & (nb - 1)) != 0 || nb > MAX_POINTERATOMIC_SIZE) {
        std::string msg(StringRef(jl_intrinsic_name((int)f)));
        msg += ": invalid pointer for atomic operation";
        emit_error(ctx, msg);
        return jl_cgval_t();
    }

    if (!jl_isbits(ety)) {
        //if (!deserves_stack(ety))
        //Value *thePtr = emit_unbox(ctx, getPointerTy(ctx.builder.getContext()), e);
        //uint64_t size = jl_datatype_size(ety);
        return emit_runtime_call(ctx, f, argv, nargs); // TODO: optimizations
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ctx, ety, &isboxed);
        assert(!isboxed);
        Value *thePtr;
        if (!type_is_ghost(ptrty))
            thePtr = emit_unbox(ctx, PointerType::getUnqual(ptrty->getContext()), e);
        else
            thePtr = nullptr; // could use any value here, since typed_store will not use it
        jl_cgval_t ret = typed_store(ctx, thePtr, x, y, ety, ctx.tbaa().tbaa_data, nullptr, nullptr, isboxed,
                    llvm_order, llvm_failorder, nb, nullptr, issetfield, isreplacefield, isswapfield, ismodifyfield, false, false, modifyop, "atomic_pointermodify", nullptr, nullptr);
        if (issetfield)
            ret = e;
        return ret;
    }
}

static Value *emit_checked_srem_int(jl_codectx_t &ctx, Value *x, Value *den)
{
    Type *t = den->getType();
    auto ndivby0 = ctx.builder.CreateICmpNE(den, ConstantInt::get(t, 0));
    setName(ctx.emission_context, ndivby0, "ndivby0");
    raise_exception_unless(ctx,
            ndivby0,
            literal_pointer_val(ctx, jl_diverror_exception));
    BasicBlock *m1BB = BasicBlock::Create(ctx.builder.getContext(), "minus1", ctx.f);
    BasicBlock *okBB = BasicBlock::Create(ctx.builder.getContext(), "oksrem", ctx.f);
    BasicBlock *cont = BasicBlock::Create(ctx.builder.getContext(), "after_srem", ctx.f);
    PHINode *ret = PHINode::Create(t, 2);
    auto divbym1 = ctx.builder.CreateICmpEQ(den, ConstantInt::get(t, -1, true));
    setName(ctx.emission_context, divbym1, "divbym1");
    ctx.builder.CreateCondBr(divbym1, m1BB, okBB);
    ctx.builder.SetInsertPoint(m1BB);
    ctx.builder.CreateBr(cont);
    ctx.builder.SetInsertPoint(okBB);
    Value *sremval = ctx.builder.CreateSRem(x, den);
    ctx.builder.CreateBr(cont);
    ctx.builder.SetInsertPoint(cont);
    ret->addIncoming(// rem(typemin, -1) is undefined
                     ConstantInt::get(t, 0), m1BB);
    ret->addIncoming(sremval, okBB);
    setName(ctx.emission_context, ret, "checked_srem");
    ctx.builder.Insert(ret);
    return ret;
}

// Temporarily switch the ctx.builder to fast-math mode if requested
struct math_builder {
    IRBuilder<> &ctxbuilder;
    FastMathFlags old_fmf;
    math_builder(jl_codectx_t &ctx, bool always_fast = false, bool contract = false)
      : ctxbuilder(ctx.builder),
        old_fmf(ctxbuilder.getFastMathFlags())
    {
        FastMathFlags fmf;
        if (jl_options.fast_math != JL_OPTIONS_FAST_MATH_OFF &&
            (always_fast ||
             jl_options.fast_math == JL_OPTIONS_FAST_MATH_ON)) {
            fmf.setFast();
        }
        if (contract)
            fmf.setAllowContract(true);
        ctxbuilder.setFastMathFlags(fmf);
    }
    IRBuilder<>& operator()() const { return ctxbuilder; }
    ~math_builder() {
        ctxbuilder.setFastMathFlags(old_fmf);
    }
};

static Value *emit_untyped_intrinsic(jl_codectx_t &ctx, intrinsic f, ArrayRef<Value*> argvalues, size_t nargs,
                                     jl_datatype_t **newtyp, jl_value_t *xtyp);


static jl_cgval_t emit_ifelse(jl_codectx_t &ctx, jl_cgval_t c, jl_cgval_t x, jl_cgval_t y, jl_value_t *rt_hint)
{
    Value *isfalse = emit_condition(ctx, c, "ifelse");
    setName(ctx.emission_context, isfalse, "ifelse_cond");
    jl_value_t *t1 = x.typ;
    jl_value_t *t2 = y.typ;
    // handle cases where the condition is irrelevant based on type info
    if (t1 == jl_bottom_type && t2 == jl_bottom_type)
        return jl_cgval_t(); // undefined
    if (t1 == jl_bottom_type)
        return y;
    if (t2 == jl_bottom_type)
        return x;

    if (t1 != t2) {
        // type inference may know something we don't, in which case it may
        // be illegal for us to convert to rt_hint. Check first if either
        // of the types have empty intersection with the result type,
        // in which case, we may use the other one.
        if (jl_type_intersection(t1, rt_hint) == jl_bottom_type)
            return y;
        else if (jl_type_intersection(t2, rt_hint) == jl_bottom_type)
            return x;
        // if they aren't the same type, consider using the expr type
        // to instantiate a union-split optimization
        x = convert_julia_type(ctx, x, rt_hint);
        y = convert_julia_type(ctx, y, rt_hint);
        t1 = x.typ;
        t2 = y.typ;
    }

    Value *ifelse_result;
    bool isboxed = t1 != t2 || !deserves_stack(t1);
    Type *llt1 = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, t1);
    if (!isboxed) {
        if (type_is_ghost(llt1))
            return x;
        ifelse_result = ctx.builder.CreateSelect(isfalse,
                emit_unbox(ctx, llt1, y),
                emit_unbox(ctx, llt1, x));
    }
    else {
        Value *x_tindex = x.TIndex;
        Value *y_tindex = y.TIndex;
        if (x_tindex || y_tindex) {
            if (!x.isghost)
                x = value_to_pointer(ctx, x);
            if (!y.isghost)
                y = value_to_pointer(ctx, y);
            Value *x_vboxed = x.Vboxed;
            Value *y_vboxed = y.Vboxed;
            Value *x_ptr = (x.isghost ? NULL : data_pointer(ctx, x));
            Value *y_ptr = (y.isghost ? NULL : data_pointer(ctx, y));
            MDNode *ifelse_tbaa;
            if (!x.isghost && x.constant)
                x_vboxed = boxed(ctx, x);
            if (!y.isghost && y.constant)
                y_vboxed = boxed(ctx, y);
            if (!x_ptr && !y_ptr) { // both ghost
                ifelse_result = NULL;
                ifelse_tbaa = ctx.tbaa().tbaa_stack;
            }
            else if (!x_ptr) {
                ifelse_result = y_ptr;
                ifelse_tbaa = y.tbaa;
            }
            else if (!y_ptr) {
                ifelse_result = x_ptr;
                ifelse_tbaa = x.tbaa;
            }
            else {
                x_ptr = decay_derived(ctx, x_ptr);
                y_ptr = decay_derived(ctx, y_ptr);
                ifelse_result = ctx.builder.CreateSelect(isfalse, y_ptr, x_ptr);
                setName(ctx.emission_context, ifelse_result, "ifelse_result");
                ifelse_tbaa = MDNode::getMostGenericTBAA(x.tbaa, y.tbaa);
                if (ifelse_tbaa == NULL) {
                    // LLVM won't return a TBAA result for the root, but mark_julia_struct requires it: make it now
                    auto *OffsetNode = ConstantAsMetadata::get(ConstantInt::get(getInt64Ty(ctx.builder.getContext()), 0));
                    Metadata *Ops[] = {ctx.tbaa().tbaa_root, ctx.tbaa().tbaa_root, OffsetNode};
                    ifelse_tbaa = MDNode::get(ctx.builder.getContext(), Ops);
                }
            }
            Value *tindex;
            if (!x_tindex && x.constant) {
                x_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80 | get_box_tindex((jl_datatype_t*)jl_typeof(x.constant), rt_hint));
            }
            if (!y_tindex && y.constant) {
                y_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80 | get_box_tindex((jl_datatype_t*)jl_typeof(y.constant), rt_hint));
            }
            if (x_tindex && y_tindex) {
                tindex = ctx.builder.CreateSelect(isfalse, y_tindex, x_tindex);
            }
            else {
                PHINode *ret = PHINode::Create(getInt8Ty(ctx.builder.getContext()), 2);
                BasicBlock *post = BasicBlock::Create(ctx.builder.getContext(), "post", ctx.f);
                BasicBlock *compute = BasicBlock::Create(ctx.builder.getContext(), "compute_tindex", ctx.f);
                // compute tindex if we select the previously-boxed value
                if (x_tindex) {
                    assert(y.isboxed && y.V);
                    ctx.builder.CreateCondBr(isfalse, compute, post);
                    ret->addIncoming(x_tindex, ctx.builder.GetInsertBlock());
                    ctx.builder.SetInsertPoint(compute);
                    tindex = compute_tindex_unboxed(ctx, y, rt_hint);
                }
                else {
                    assert(x.isboxed);
                    ctx.builder.CreateCondBr(isfalse, post, compute);
                    ret->addIncoming(y_tindex, ctx.builder.GetInsertBlock());
                    ctx.builder.SetInsertPoint(compute);
                    tindex = compute_tindex_unboxed(ctx, x, rt_hint);
                }
                tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
                compute = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.CreateBr(post);
                ret->addIncoming(tindex, compute);
                ctx.builder.SetInsertPoint(post);
                ctx.builder.Insert(ret);
                tindex = ret;
                setName(ctx.emission_context, tindex, "ifelse_tindex");
            }
            jl_cgval_t ret = mark_julia_slot(ifelse_result, rt_hint, tindex, ifelse_tbaa);
            if (x_vboxed || y_vboxed) {
                if (!x_vboxed)
                    x_vboxed = ConstantPointerNull::get(cast<PointerType>(y_vboxed->getType()));
                if (!y_vboxed)
                    y_vboxed = ConstantPointerNull::get(cast<PointerType>(x_vboxed->getType()));
                ret.Vboxed = ctx.builder.CreateSelect(isfalse, y_vboxed, x_vboxed);
                setName(ctx.emission_context, ret.Vboxed, "ifelse_vboxed");
                assert(ret.Vboxed->getType() == ctx.types().T_prjlvalue);
            }
            return ret;
        }
        ifelse_result = ctx.builder.CreateSelect(isfalse,
                boxed(ctx, y),
                boxed(ctx, x));
        setName(ctx.emission_context, ifelse_result, "ifelse_result");
    }
    jl_value_t *jt = (t1 == t2 ? t1 : rt_hint);
    return mark_julia_type(ctx, ifelse_result, isboxed, jt);
}

static jl_cgval_t emit_intrinsic(jl_codectx_t &ctx, intrinsic f, jl_value_t **args, size_t nargs)
{
    auto &DL = ctx.emission_context.DL;
    assert(f < num_intrinsics);
    if (f == cglobal && nargs == 1)
        f = cglobal_auto;
    unsigned expected_nargs = jl_intrinsic_nargs((int)f);
    if (expected_nargs && expected_nargs != nargs) {
        jl_errorf("intrinsic #%d %s: wrong number of arguments", f, jl_intrinsic_name((int)f));
    }

    if (f == llvmcall)
        return emit_llvmcall(ctx, args, nargs);
    if (f == cglobal_auto || f == cglobal)
        return emit_cglobal(ctx, args, nargs);

    SmallVector<jl_cgval_t, 0> argv(nargs);
    for (size_t i = 0; i < nargs; ++i) {
        jl_cgval_t arg = emit_expr(ctx, args[i + 1]);
        if (arg.typ == jl_bottom_type) {
            // intrinsics generally don't handle bottom values, so bail out early
            return jl_cgval_t();
        }
        argv[i] = arg;
    }

    // this forces everything to use runtime-intrinsics (e.g. for testing)
    // return emit_runtime_call(ctx, f, argv, nargs);

    switch (f) {
    case pointerref:
        ++Emitted_pointerref;
        assert(nargs == 3);
        return emit_pointerref(ctx, argv);
    case pointerset:
        ++Emitted_pointerset;
        assert(nargs == 4);
        return emit_pointerset(ctx, argv);

    case add_ptr:
    case sub_ptr:
        ++Emitted_pointerarith;
        assert(nargs == 2);
        return emit_pointerarith(ctx, f, argv);

    case atomic_fence:
        ++Emitted_atomic_fence;
        assert(nargs == 1);
        return emit_atomicfence(ctx, argv);
    case atomic_pointerref:
        ++Emitted_atomic_pointerref;
        assert(nargs == 2);
        return emit_atomic_pointerref(ctx, argv);
    case atomic_pointerset:
    case atomic_pointerswap:
    case atomic_pointermodify:
    case atomic_pointerreplace:
        ++Emitted_atomic_pointerop;
        return emit_atomic_pointerop(ctx, f, argv, nargs, nullptr);
    case bitcast:
        ++Emitted_bitcast;
        assert(nargs == 2);
        return generic_bitcast(ctx, argv);
    case trunc_int:
        ++Emitted_trunc_int;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::Trunc, argv, true, true);
    case sext_int:
        ++Emitted_sext_int;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::SExt, argv, true, true);
    case zext_int:
        ++Emitted_zext_int;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::ZExt, argv, true, true);
    case uitofp:
        ++Emitted_uitofp;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::UIToFP, argv, false, true);
    case sitofp:
        ++Emitted_sitofp;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::SIToFP, argv, false, true);
    case fptoui:
        ++Emitted_fptoui;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::FPToUI, argv, true, false);
    case fptosi:
        ++Emitted_fptosi;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::FPToSI, argv, true, false);
    case fptrunc:
        ++Emitted_fptrunc;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::FPTrunc, argv, false, false);
    case fpext:
        ++Emitted_fpext;
        assert(nargs == 2);
        return generic_cast(ctx, f, Instruction::FPExt, argv, false, false);

    case not_int: {
        ++Emitted_not_int;
        assert(nargs == 1);
        const jl_cgval_t &x = argv[0];
        if (!jl_is_primitivetype(x.typ))
            return emit_runtime_call(ctx, f, argv, nargs);
        Type *xt = INTT(bitstype_to_llvm(x.typ, ctx.builder.getContext(), true), DL);
        Value *from = emit_unbox(ctx, xt, x);
        Value *ans = ctx.builder.CreateNot(from);
        return mark_julia_type(ctx, ans, false, x.typ);
    }

    case have_fma: {
        ++Emitted_have_fma;
        assert(nargs == 1);
        const jl_cgval_t &x = argv[0];
        if (!x.constant || !jl_is_datatype(x.constant))
            return emit_runtime_call(ctx, f, argv, nargs);
        jl_datatype_t *dt = (jl_datatype_t*) x.constant;

        // select the appropriated overloaded intrinsic
        std::string intr_name = "julia.cpu.have_fma.";
        if (dt == jl_float32_type)
            intr_name += "f32";
        else if (dt == jl_float64_type)
            intr_name += "f64";
        else
            return emit_runtime_call(ctx, f, argv, nargs);

        FunctionCallee intr = jl_Module->getOrInsertFunction(intr_name, getInt1Ty(ctx.builder.getContext()));
        auto ret = ctx.builder.CreateCall(intr);
        return mark_julia_type(ctx, ret, false, jl_bool_type);
    }

    default: {
        assert(nargs >= 1 && "invalid nargs for intrinsic call");
        const jl_cgval_t &xinfo = argv[0];

        // verify argument types
        if (!jl_is_primitivetype(xinfo.typ))
            return emit_runtime_call(ctx, f, argv, nargs);
        Type *xtyp = bitstype_to_llvm(xinfo.typ, ctx.builder.getContext(), true);
        if (float_func()[f]) {
            if (!xtyp->isFloatingPointTy())
                return emit_runtime_call(ctx, f, argv, nargs);
        }
        else {
            xtyp = INTT(xtyp, DL);
        }
        if (!xtyp)
            return emit_runtime_call(ctx, f, argv, nargs);
        ////Bool are required to be in the range [0,1]
        ////so while they are represented as i8,
        ////the operations need to be done in mod 1
        ////we can either do that now, or truncate them
        ////later into mod 1.
        ////LLVM seems to emit better code if we do the latter,
        ////(more likely to fold away the cast) so that's what we'll do.
        //if (xtyp == (jl_value_t*)jl_bool_type)
        //    r = getInt1Ty(ctx.builder.getContext());

        SmallVector<Type *, 0> argt(nargs);
        argt[0] = xtyp;

        if (f == shl_int || f == lshr_int || f == ashr_int) {
            if (!jl_is_primitivetype(argv[1].typ))
                return emit_runtime_call(ctx, f, argv, nargs);
            argt[1] = INTT(bitstype_to_llvm(argv[1].typ, ctx.builder.getContext(), true), DL);
        }
        else {
            for (size_t i = 1; i < nargs; ++i) {
                if (xinfo.typ != argv[i].typ)
                    return emit_runtime_call(ctx, f, argv, nargs);
                argt[i] = xtyp;
            }
        }

        // unbox the arguments
        SmallVector<Value *, 0> argvalues(nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argvalues[i] = emit_unbox(ctx, argt[i], argv[i]);
        }

        // call the intrinsic
        jl_value_t *newtyp = xinfo.typ;
        Value *r = emit_untyped_intrinsic(ctx, f, argvalues, nargs, (jl_datatype_t**)&newtyp, xinfo.typ);
        // Turn Bool operations into mod 1 now, if needed
        if (newtyp == (jl_value_t*)jl_bool_type && !r->getType()->isIntegerTy(1))
            r = ctx.builder.CreateTrunc(r, getInt1Ty(ctx.builder.getContext()));
        return mark_julia_type(ctx, r, false, newtyp);
    }
    }
    assert(0 && "unreachable");
}

static Value *emit_untyped_intrinsic(jl_codectx_t &ctx, intrinsic f, ArrayRef<Value*> argvalues, size_t nargs,
                                     jl_datatype_t **newtyp, jl_value_t *xtyp)
{
    ++EmittedUntypedIntrinsics;
    Value *x = nargs > 0 ? argvalues[0] : NULL;
    Value *y = nargs > 1 ? argvalues[1] : NULL;
    Value *z = nargs > 2 ? argvalues[2] : NULL;
    Type *t = x->getType();

    switch (f) {
    case neg_int:
        return ctx.builder.CreateNeg(x);
    case add_int: return ctx.builder.CreateAdd(x, y);
    case sub_int: return ctx.builder.CreateSub(x, y);
    case mul_int: return ctx.builder.CreateMul(x, y);
    case sdiv_int: return ctx.builder.CreateSDiv(x, y);
    case udiv_int: return ctx.builder.CreateUDiv(x, y);
    case srem_int: return ctx.builder.CreateSRem(x, y);
    case urem_int: return ctx.builder.CreateURem(x, y);

    case neg_float: return math_builder(ctx)().CreateFNeg(x);
    case neg_float_fast: return math_builder(ctx, true)().CreateFNeg(x);
    case add_float: return math_builder(ctx)().CreateFAdd(x, y);
    case sub_float: return math_builder(ctx)().CreateFSub(x, y);
    case mul_float: return math_builder(ctx)().CreateFMul(x, y);
    case div_float: return math_builder(ctx)().CreateFDiv(x, y);
    case min_float: {
        assert(x->getType() == y->getType());
#if JL_LLVM_VERSION >= 200000
        FunctionCallee minintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::minimum, ArrayRef<Type*>(t));
#else
        FunctionCallee minintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::minimum, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(minintr, {x, y});
    }
    case max_float: {
        assert(x->getType() == y->getType());
#if JL_LLVM_VERSION >= 200000
        FunctionCallee maxintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::maximum, ArrayRef<Type*>(t));
#else
        FunctionCallee maxintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::maximum, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(maxintr, {x, y});
    }
    case min_float_fast: {
        assert(x->getType() == y->getType());
#if JL_LLVM_VERSION >= 200000
        FunctionCallee minintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::minimum, ArrayRef<Type*>(t));
#else
        FunctionCallee minintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::minimum, ArrayRef<Type*>(t));
#endif
        auto call = ctx.builder.CreateCall(minintr, {x, y});
        auto fmf = call->getFastMathFlags();
        fmf.setFast();
        call->copyFastMathFlags(fmf);
        return call;
    }
    case max_float_fast: {
        assert(x->getType() == y->getType());
#if JL_LLVM_VERSION >= 200000
        FunctionCallee maxintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::maximum, ArrayRef<Type*>(t));
#else
        FunctionCallee maxintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::maximum, ArrayRef<Type*>(t));
#endif
        auto call = ctx.builder.CreateCall(maxintr, {x, y});
        auto fmf = call->getFastMathFlags();
        fmf.setFast();
        call->copyFastMathFlags(fmf);
        return call;
    }
    case add_float_fast: return math_builder(ctx, true)().CreateFAdd(x, y);
    case sub_float_fast: return math_builder(ctx, true)().CreateFSub(x, y);
    case mul_float_fast: return math_builder(ctx, true)().CreateFMul(x, y);
    case div_float_fast: return math_builder(ctx, true)().CreateFDiv(x, y);
    case fma_float: {
        assert(y->getType() == x->getType());
        assert(z->getType() == y->getType());
#if JL_LLVM_VERSION >= 200000
        FunctionCallee fmaintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::fma, ArrayRef<Type*>(t));
#else
        FunctionCallee fmaintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fma, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(fmaintr, {x, y, z});
    }
    case muladd_float: {
        // LLVM 5.0 can create FMA in the backend for contractible fmul and fadd
        // Emitting fmul and fadd here since they are easier for other LLVM passes to
        // optimize.
        auto mathb = math_builder(ctx, false, true);
        return mathb().CreateFAdd(mathb().CreateFMul(x, y), z);
    }

    case checked_sadd_int:
    case checked_uadd_int:
    case checked_ssub_int:
    case checked_usub_int:
    case checked_smul_int:
    case checked_umul_int: {
        assert(x->getType() == y->getType());
        Intrinsic::ID intr_id =
            (f == checked_sadd_int ?
             Intrinsic::sadd_with_overflow :
             (f == checked_uadd_int ?
              Intrinsic::uadd_with_overflow :
              (f == checked_ssub_int ?
               Intrinsic::ssub_with_overflow :
               (f == checked_usub_int ?
                Intrinsic::usub_with_overflow :
                (f == checked_smul_int ?
                 Intrinsic::smul_with_overflow :
                 Intrinsic::umul_with_overflow)))));
#if JL_LLVM_VERSION >= 200000
        FunctionCallee intr = Intrinsic::getOrInsertDeclaration(jl_Module, intr_id, ArrayRef<Type*>(t));
#else
        FunctionCallee intr = Intrinsic::getDeclaration(jl_Module, intr_id, ArrayRef<Type*>(t));
#endif
        Value *tupval = ctx.builder.CreateCall(intr, {x, y});

        jl_value_t *params[2];
        params[0] = xtyp;
        params[1] = (jl_value_t*)jl_bool_type;
        jl_datatype_t *tuptyp = (jl_datatype_t*)jl_apply_tuple_type_v(params, 2);
        *newtyp = tuptyp;

        return tupval;
    }

    case checked_sdiv_int: {
        Value *typemin = ctx.builder.CreateShl(ConstantInt::get(t, 1), t->getPrimitiveSizeInBits() - 1);
        auto cond = ctx.builder.CreateAnd(
                        ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                        ctx.builder.CreateOr(
                            ctx.builder.CreateICmpNE(y, ConstantInt::get(t, -1, true)),
                            ctx.builder.CreateICmpNE(x, typemin)));
        setName(ctx.emission_context, cond, "divisor_valid");
        raise_exception_unless(ctx, cond, literal_pointer_val(ctx, jl_diverror_exception));

        return ctx.builder.CreateSDiv(x, y);
    }
    case checked_udiv_int: {
        auto cond = ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0));
        setName(ctx.emission_context, cond, "ndivby0");
        raise_exception_unless(ctx, cond, literal_pointer_val(ctx, jl_diverror_exception));
        return ctx.builder.CreateUDiv(x, y);
    }
    case checked_srem_int:
        return emit_checked_srem_int(ctx, x, y);

    case checked_urem_int: {
        auto cond = ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0));
        setName(ctx.emission_context, cond, "ndivby0");
        raise_exception_unless(ctx, cond, literal_pointer_val(ctx, jl_diverror_exception));
        return ctx.builder.CreateURem(x, y);
    }

    case eq_int:  *newtyp = jl_bool_type; return ctx.builder.CreateICmpEQ(x, y);
    case ne_int:  *newtyp = jl_bool_type; return ctx.builder.CreateICmpNE(x, y);
    case slt_int: *newtyp = jl_bool_type; return ctx.builder.CreateICmpSLT(x, y);
    case ult_int: *newtyp = jl_bool_type; return ctx.builder.CreateICmpULT(x, y);
    case sle_int: *newtyp = jl_bool_type; return ctx.builder.CreateICmpSLE(x, y);
    case ule_int: *newtyp = jl_bool_type; return ctx.builder.CreateICmpULE(x, y);

    case eq_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOEQ(x, y);
    case ne_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpUNE(x, y);
    case lt_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOLT(x, y);
    case le_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOLE(x, y);

    case eq_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOEQ(x, y);
    case ne_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpUNE(x, y);
    case lt_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOLT(x, y);
    case le_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOLE(x, y);

    case fpiseq: {
        *newtyp = jl_bool_type;
        Type *it = INTT(t, ctx.emission_context.DL);
        Value *xi = ctx.builder.CreateBitCast(x, it);
        Value *yi = ctx.builder.CreateBitCast(y, it);
        return ctx.builder.CreateOr(ctx.builder.CreateAnd(ctx.builder.CreateFCmpUNO(x, x),
                                                  ctx.builder.CreateFCmpUNO(y, y)),
                                ctx.builder.CreateICmpEQ(xi, yi));
    }

    case and_int: return ctx.builder.CreateAnd(x, y);
    case or_int:  return ctx.builder.CreateOr(x, y);
    case xor_int: return ctx.builder.CreateXor(x, y);

    case shl_int: {
        Value *the_shl = ctx.builder.CreateShl(x, uint_cnvt(ctx, t, y));
        if (ConstantInt::isValueValidForType(y->getType(), t->getPrimitiveSizeInBits())) {
            return ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                                  t->getPrimitiveSizeInBits())),
                    ConstantInt::get(t, 0),
                    the_shl);
        }
        else {
            return the_shl;
        }
    }
    case lshr_int: {
        Value *the_shr = ctx.builder.CreateLShr(x, uint_cnvt(ctx, t, y));
        if (ConstantInt::isValueValidForType(y->getType(), t->getPrimitiveSizeInBits())) {
            return ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                                  t->getPrimitiveSizeInBits())),
                    ConstantInt::get(t, 0),
                    the_shr);
        }
        else {
            return the_shr;
        }
    }
    case ashr_int: {
        Value *the_shr = ctx.builder.CreateAShr(x, uint_cnvt(ctx, t, y));
        if (ConstantInt::isValueValidForType(y->getType(), t->getPrimitiveSizeInBits())) {
            return ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                                  t->getPrimitiveSizeInBits())),
                    ctx.builder.CreateAShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits() - 1)),
                    the_shr);
        }
        else {
            return the_shr;
        }
    }
    case bswap_int: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee bswapintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::bswap, ArrayRef<Type*>(t)); //TODO: Move to deduction guides
#else
        FunctionCallee bswapintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap, ArrayRef<Type*>(t)); //TODO: Move to deduction guides
#endif
        return ctx.builder.CreateCall(bswapintr, x);                                                           // when we drop LLVM 15
    }
    case ctpop_int: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee ctpopintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::ctpop, ArrayRef<Type*>(t));
#else
        FunctionCallee ctpopintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(ctpopintr, x);
    }
    case ctlz_int: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee ctlz = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::ctlz, ArrayRef<Type*>(t));
#else
        FunctionCallee ctlz = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz, ArrayRef<Type*>(t));
#endif
        y = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
        return ctx.builder.CreateCall(ctlz, {x, y});
    }
    case cttz_int: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee cttz = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::cttz, ArrayRef<Type*>(t));
#else
        FunctionCallee cttz = Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, ArrayRef<Type*>(t));
#endif
        y = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
        return ctx.builder.CreateCall(cttz, {x, y});
    }

    case abs_float: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee absintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::fabs, ArrayRef<Type*>(t));
#else
        FunctionCallee absintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fabs, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(absintr, x);
    }
    case copysign_float: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee copyintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::copysign, ArrayRef<Type*>(t));
#else
        FunctionCallee copyintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::copysign, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(copyintr, {x, y});
    }
    case flipsign_int: {
        ConstantInt *cx = dyn_cast<ConstantInt>(x);
        ConstantInt *cy = dyn_cast<ConstantInt>(y);
        if (cx && cy) {
            APInt ix = cx->getValue();
            APInt iy = cy->getValue();
            return ConstantInt::get(t, iy.isNonNegative() ? ix : -ix);
        }
        if (cy) {
            APInt iy = cy->getValue();
            return iy.isNonNegative() ? x : ctx.builder.CreateSub(ConstantInt::get(t, 0), x);
        }
        Value *tmp = ctx.builder.CreateAShr(y, ConstantInt::get(t, cast<IntegerType>(t)->getBitWidth() - 1));
        return ctx.builder.CreateXor(ctx.builder.CreateAdd(x, tmp), tmp);
    }
    case ceil_llvm: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee ceilintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::ceil, ArrayRef<Type*>(t));
#else
        FunctionCallee ceilintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(ceilintr, x);
    }
    case floor_llvm: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee floorintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::floor, ArrayRef<Type*>(t));
#else
        FunctionCallee floorintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::floor, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(floorintr, x);
    }
    case trunc_llvm: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee truncintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::trunc, ArrayRef<Type*>(t));
#else
        FunctionCallee truncintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(truncintr, x);
    }
    case rint_llvm: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee rintintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::rint, ArrayRef<Type*>(t));
#else
        FunctionCallee rintintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::rint, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(rintintr, x);
    }
    case sqrt_llvm: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee sqrtintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::sqrt, ArrayRef<Type*>(t));
#else
        FunctionCallee sqrtintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt, ArrayRef<Type*>(t));
#endif
        return ctx.builder.CreateCall(sqrtintr, x);
    }
    case sqrt_llvm_fast: {
#if JL_LLVM_VERSION >= 200000
        FunctionCallee sqrtintr = Intrinsic::getOrInsertDeclaration(jl_Module, Intrinsic::sqrt, ArrayRef<Type*>(t));
#else
        FunctionCallee sqrtintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt, ArrayRef<Type*>(t));
#endif
        return math_builder(ctx, true)().CreateCall(sqrtintr, x);
    }

    default:
        assert(0 && "invalid intrinsic");
        abort();
    }
    assert(0 && "unreachable");
}

//Redefine us as being part of codegen
#undef DEBUG_TYPE
#define DEBUG_TYPE "julia_irgen_codegen"
