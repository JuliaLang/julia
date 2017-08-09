// This file is a part of Julia. License is MIT: https://julialang.org/license

namespace JL_I {
#include "intrinsics.h"
}

#include "ccall.cpp"

using namespace JL_I;
static Function *runtime_func[num_intrinsics];
static bool float_func[num_intrinsics];
static void jl_init_intrinsic_functions_codegen(Module *m)
{
    std::vector<Type *> args1(0); \
    args1.push_back(T_prjlvalue); \
    std::vector<Type *> args2(0); \
    args2.push_back(T_prjlvalue); \
    args2.push_back(T_prjlvalue); \
    std::vector<Type *> args3(0); \
    args3.push_back(T_prjlvalue); \
    args3.push_back(T_prjlvalue); \
    args3.push_back(T_prjlvalue); \
    std::vector<Type *> args4(0); \
    args4.push_back(T_prjlvalue); \
    args4.push_back(T_prjlvalue); \
    args4.push_back(T_prjlvalue); \
    args4.push_back(T_prjlvalue);

#define ADD_I(name, nargs) do { \
        Function *func = Function::Create(FunctionType::get(T_prjlvalue, args##nargs, false), \
                                          Function::ExternalLinkage, "jl_"#name, m); \
        runtime_func[name] = func; \
        add_named_global(func, &jl_##name); \
    } while (0);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) runtime_func[alias] = runtime_func[base];
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS

    float_func[neg_float] = true;
    float_func[neg_float_fast] = true;
    float_func[add_float] = true;
    float_func[sub_float] = true;
    float_func[mul_float] = true;
    float_func[div_float] = true;
    float_func[rem_float] = true;
    float_func[add_float_fast] = true;
    float_func[sub_float_fast] = true;
    float_func[mul_float_fast] = true;
    float_func[div_float_fast] = true;
    float_func[rem_float_fast] = true;
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
    float_func[fpislt] = true;
    float_func[abs_float] = true;
    //float_func[copysign_float] = false; // this is actually an integer operation
    float_func[ceil_llvm] = true;
    float_func[floor_llvm] = true;
    float_func[trunc_llvm] = true;
    float_func[rint_llvm] = true;
    float_func[sqrt_llvm] = true;
}

extern "C"
JL_DLLEXPORT uint32_t jl_get_LLVM_VERSION(void)
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
    if (nb == 64)
        return T_float64;
    if (nb == 32)
        return T_float32;
#ifndef DISABLE_FLOAT16
    if (nb == 16)
        return T_float16;
#endif
    if (nb == 128)
        return T_float128;
    return NULL;
}

// convert an llvm type to same-size int type
static Type *INTT(Type *t)
{
    if (t->isIntegerTy())
        return t;
    if (t->isPointerTy())
        return T_size;
    if (t == T_float64)
        return T_int64;
    if (t == T_float32)
        return T_int32;
    if (t == T_float16)
        return T_int16;
    unsigned nb = t->getPrimitiveSizeInBits();
    assert(t != T_void && nb > 0);
    return IntegerType::get(jl_LLVMContext, nb);
}

static Value *uint_cnvt(jl_codectx_t &ctx, Type *to, Value *x)
{
    Type *t = x->getType();
    if (t == to)
        return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return ctx.builder.CreateTrunc(x, to);
    return ctx.builder.CreateZExt(x, to);
}

#if JL_LLVM_VERSION >= 40000
#define LLVM_FP(a,b) APFloat(a(),b)
#else
#define LLVM_FP(a,b) APFloat(a,b)
#endif
static Constant *julia_const_to_llvm(void *ptr, jl_value_t *bt)
{
    // assumes `jl_isbits(bt)`.
    // `ptr` can point to a inline field, do not read the tag from it.
    // make sure to return exactly the type specified by
    // julia_type_to_llvm as this will be assumed by the callee.
    if (bt == (jl_value_t*)jl_bool_type)
        return ConstantInt::get(T_int8, (*(uint8_t*)ptr) ? 1 : 0);

    if (bt == (jl_value_t*)jl_ssavalue_type)
        return NULL;

    if (jl_is_vecelement_type(bt))
        bt = jl_tparam0(bt);

    if (jl_is_cpointer_type(bt))
        return ConstantExpr::getIntToPtr(ConstantInt::get(T_size, *(uintptr_t*)ptr), julia_type_to_llvm(bt));
    if (jl_is_primitivetype(bt)) {
        int nb = jl_datatype_size(bt);
        // TODO: non-power-of-2 size datatypes may not be interpreted correctly on big-endian systems
        switch (nb) {
        case 1: {
            uint8_t data8 = *(uint8_t*)ptr;
            return ConstantInt::get(T_int8, data8);
        }
        case 2: {
            uint16_t data16 = *(uint16_t*)ptr;
            return ConstantInt::get(T_int16, data16);
        }
        case 4: {
            uint32_t data32 = *(uint32_t*)ptr;
            if (bt == (jl_value_t*)jl_float32_type)
                return ConstantFP::get(jl_LLVMContext,
                        LLVM_FP(APFloat::IEEEsingle,
                            APInt(32, data32)));
            return ConstantInt::get(T_int32, data32);
        }
        case 8: {
            uint64_t data64 = *(uint64_t*)ptr;
            if (bt == (jl_value_t*)jl_float64_type)
                return ConstantFP::get(jl_LLVMContext,
                        LLVM_FP(APFloat::IEEEdouble,
                            APInt(64, data64)));
            return ConstantInt::get(T_int64, data64);
        }
        default:
            size_t nw = (nb+sizeof(uint64_t)-1)/sizeof(uint64_t);
            uint64_t *data = (uint64_t*)ptr;
            APInt val;
#if !defined(_P64)
            // malloc may not be 16-byte aligned on P32,
            // but we must ensure that llvm's uint64_t reads don't fall
            // off the end of a page
            // where 16-byte alignment requirement == (8-byte typetag) % (uint64_t ArrayRef access)
            if (nb % 16 != 0) {
                uint64_t *data_a64 = (uint64_t*)alloca(sizeof(uint64_t)*nw);
                memcpy(data_a64, data, nb);
                val = APInt(8*nb, ArrayRef<uint64_t>(data_a64, nw));
            }
            else
#endif
            val = APInt(8*nb, ArrayRef<uint64_t>(data, nw));
            return ConstantInt::get(IntegerType::get(jl_LLVMContext,8*nb),val);
        }
    }
    size_t nf = jl_datatype_nfields(bt);
    Constant **fields = (Constant**)alloca(nf * sizeof(Constant*));
    for (size_t i = 0; i < nf; i++) {
        size_t offs = jl_field_offset((jl_datatype_t*)bt, i);
        jl_value_t *ft = jl_field_type(bt, i);
        Constant *val = julia_const_to_llvm((char*)ptr + offs, ft);
        if (val == NULL)
            return NULL;
        fields[i] = val;
    }

    Type *t = julia_struct_to_llvm(bt, NULL, NULL);
    if (type_is_ghost(t))
        return UndefValue::get(NoopType);
    if (t->isVectorTy())
        return ConstantVector::get(ArrayRef<Constant*>(fields, nf));
    if (StructType *st = dyn_cast<StructType>(t)) {
        return ConstantStruct::get(st, ArrayRef<Constant*>(fields, nf));
    }
    else {
        ArrayType *at = cast<ArrayType>(t);
        return ConstantArray::get(at, ArrayRef<Constant*>(fields, nf));
    }
}

static Constant *julia_const_to_llvm(jl_value_t *e)
{
    if (e == jl_true)
        return ConstantInt::get(T_int8, 1);
    if (e == jl_false)
        return ConstantInt::get(T_int8, 0);
    jl_value_t *bt = jl_typeof(e);
    if (!jl_isbits(bt))
        return NULL;
    return julia_const_to_llvm(e, bt);
}

static jl_cgval_t ghostValue(jl_value_t *ty);

// emit code to unpack a raw value from a box into registers or a stack slot
static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt, Value *dest, bool volatile_store)
{
    assert(to != T_void);
    // TODO: fully validate that x.typ == jt?
    if (x.isghost) {
        // this can happen when a branch yielding a different type ends
        // up being dead code, and type inference knows that the other
        // branch's type is the only one that matters.
        if (type_is_ghost(to)) {
            return NULL;
        }
        //emit_error(ctx, "emit_unbox: a type mismatch error in occurred during codegen");
        return UndefValue::get(to); // type mismatch error
    }

    Constant *c = x.constant ? julia_const_to_llvm(x.constant) : NULL;
    if (!x.ispointer() || c) { // already unboxed, but sometimes need conversion
        Value *unboxed = c ? c : x.V;
        Type *ty = unboxed->getType();
        assert(ty != T_void);
        bool frompointer = ty->isPointerTy();
        bool topointer = to->isPointerTy();
        if (frompointer && topointer) {
            unboxed = emit_bitcast(ctx, unboxed, to);
        }
        else if (frompointer) {
            Type *INTT_to = INTT(to);
            unboxed = ctx.builder.CreatePtrToInt(unboxed, INTT_to);
            if (INTT_to != to)
                unboxed = ctx.builder.CreateBitCast(unboxed, to);
        }
        else if (topointer) {
            Type *INTT_to = INTT(to);
            if (to != INTT_to)
                unboxed = ctx.builder.CreateBitCast(unboxed, INTT_to);
            unboxed = ctx.builder.CreateIntToPtr(unboxed, to);
        }
        else if (ty == T_int1 && to == T_int8) {
            // bools may be stored internally as int8
            unboxed = ctx.builder.CreateZExt(unboxed, T_int8);
        }
        else if (ty != to) {
            unboxed = ctx.builder.CreateBitCast(unboxed, to);
        }
        if (!dest)
            return unboxed;
        ctx.builder.CreateStore(unboxed, dest, volatile_store);
        return NULL;
    }

    // bools stored as int8, so an extra Trunc is needed to get an int1
    Value *p = x.constant ? literal_pointer_val(ctx, x.constant) : x.V;
    Type *ptype = (to == T_int1 ? T_pint8 : to->getPointerTo());
    if (p->getType() != ptype)
        p = emit_bitcast(ctx, p, ptype);

    Value *unboxed = NULL;
    if (to == T_int1)
        unboxed = ctx.builder.CreateTrunc(tbaa_decorate(x.tbaa, ctx.builder.CreateLoad(p)), T_int1);
    else if (jt == (jl_value_t*)jl_bool_type)
        unboxed = ctx.builder.CreateZExt(ctx.builder.CreateTrunc(tbaa_decorate(x.tbaa, ctx.builder.CreateLoad(p)), T_int1), to);
    if (unboxed) {
        if (!dest)
            return unboxed;
        ctx.builder.CreateStore(unboxed, dest);
        return NULL;
    }

    int alignment;
    if (jt) {
        alignment = julia_alignment(p, jt, 0);
    }
    else {
        // stack has default alignment
        alignment = 0;
    }
    if (dest) {
        // callers using the dest argument only use it for a stack slot for now
        alignment = 0;
        MDNode *tbaa = x.tbaa;
        // the memcpy intrinsic does not allow to specify different alias tags
        // for the load part (x.tbaa) and the store part (tbaa_stack).
        // since the tbaa lattice has to be a tree we have unfortunately
        // x.tbaa ∪ tbaa_stack = tbaa_root if x.tbaa != tbaa_stack
        if (tbaa != tbaa_stack)
            tbaa = NULL;
        ctx.builder.CreateMemCpy(dest, p, jl_datatype_size(jt), alignment, volatile_store, tbaa);
        return NULL;
    }
    else {
        Instruction *load;
        if (alignment)
            load = ctx.builder.CreateAlignedLoad(p, alignment);
        else
            load = ctx.builder.CreateLoad(p);
        return tbaa_decorate(x.tbaa, load);
    }
}

static jl_value_t *staticeval_bitstype(const jl_cgval_t &targ)
{
    // evaluate an argument at compile time to determine what type it is
    if (jl_is_type_type(targ.typ)) {
        jl_value_t *bt = jl_tparam0(targ.typ);
        if (jl_is_primitivetype(bt))
            return bt;
    }
    return NULL;
}

static jl_cgval_t emit_runtime_call(jl_codectx_t &ctx, JL_I::intrinsic f, const jl_cgval_t *argv, size_t nargs)
{
    Value *func = prepare_call(runtime_func[f]);
    Value **argvalues = (Value**)alloca(sizeof(Value*) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argvalues[i] = boxed(ctx, argv[i]);
    }
    Value *r = ctx.builder.CreateCall(func, makeArrayRef(argvalues, nargs));
    return mark_julia_type(ctx, r, true, (jl_value_t*)jl_any_type);
}

// put a bits type tag on some value (despite the name, this doesn't necessarily actually change anything about the value however)
static jl_cgval_t generic_bitcast(jl_codectx_t &ctx, const jl_cgval_t *argv)
{
    // Give the arguments names //
    const jl_cgval_t &bt_value = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_value_t *bt = staticeval_bitstype(bt_value);

    // it's easier to throw a good error from C than llvm
    if (!bt)
        return emit_runtime_call(ctx, bitcast, argv, 2);

    Type *llvmt = bitstype_to_llvm(bt);
    int nb = jl_datatype_size(bt);

    // Examine the second argument //
    bool isboxed;
    Type *vxt = julia_type_to_llvm(v.typ, &isboxed);

    if (!jl_is_primitivetype(v.typ) || jl_datatype_size(v.typ) != nb) {
        Value *typ = emit_typeof_boxed(ctx, v);
        if (!jl_is_primitivetype(v.typ)) {
            if (isboxed) {
                Value *isbits = emit_datatype_isbitstype(ctx, typ);
                error_unless(ctx, isbits, "bitcast: expected primitive type value for second argument");
            }
            else {
                emit_error(ctx, "bitcast: expected primitive type value for second argument");
                return jl_cgval_t();
            }
        }
        if (jl_datatype_size(v.typ) != nb) {
            if (isboxed) {
                Value *size = emit_datatype_size(ctx, typ);
                error_unless(ctx,
                        ctx.builder.CreateICmpEQ(size, ConstantInt::get(T_int32, nb)),
                        "bitcast: argument size does not match size of target type");
            }
            else {
                emit_error(ctx, "bitcast: argument size does not match size of target type");
                return jl_cgval_t();
            }
        }
    }

    assert(!v.isghost);
    Value *vx = NULL;
    if (!v.ispointer())
        vx = v.V;
    else if (v.constant)
        vx = julia_const_to_llvm(v.constant);

    if (v.ispointer() && vx == NULL) {
        // try to load as original Type, to preserve llvm optimizations
        // but if the v.typ is not well known, use llvmt
        if (isboxed)
            vxt = llvmt;
        vx = tbaa_decorate(v.tbaa, ctx.builder.CreateLoad(
                    data_pointer(ctx, v, vxt == T_int1 ? T_pint8 : vxt->getPointerTo())));
    }

    vxt = vx->getType();
    if (vxt != llvmt) {
        if (llvmt == T_int1)
            vx = ctx.builder.CreateTrunc(vx, llvmt);
        else if (vxt == T_int1 && llvmt == T_int8)
            vx = ctx.builder.CreateZExt(vx, llvmt);
        else if (vxt->isPointerTy() && !llvmt->isPointerTy())
            vx = ctx.builder.CreatePtrToInt(vx, llvmt);
        else if (!vxt->isPointerTy() && llvmt->isPointerTy())
            vx = ctx.builder.CreateIntToPtr(vx, llvmt);
        else
            vx = emit_bitcast(ctx, vx, llvmt);
    }

    if (jl_is_leaf_type(bt)) {
        return mark_julia_type(ctx, vx, false, bt);
    }
    else {
        Value *box = emit_allocobj(ctx, nb, boxed(ctx, bt_value));
        init_bits_value(ctx, box, vx, tbaa_immut);
        return mark_julia_type(ctx, box, true, bt);
    }
}

static jl_cgval_t generic_cast(
        jl_codectx_t &ctx,
        intrinsic f, Value *(*generic)(jl_codectx_t&, Type*, Value*),
        const jl_cgval_t *argv, bool toint, bool fromint)
{
    const jl_cgval_t &targ = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_value_t *jlto = staticeval_bitstype(targ);
    if (!jlto || !jl_is_primitivetype(v.typ))
        return emit_runtime_call(ctx, f, argv, 2);
    Type *to = bitstype_to_llvm(jlto);
    Type *vt = bitstype_to_llvm(v.typ);
    if (toint)
        to = INTT(to);
    else
        to = FLOATT(to);
    if (fromint)
        vt = INTT(vt);
    else
        vt = FLOATT(vt);
    if (!to || !vt)
        return emit_runtime_call(ctx, f, argv, 2);
    Value *from = emit_unbox(ctx, vt, v, v.typ);
    Value *ans = generic(ctx, to, from);
    return mark_julia_type(ctx, ans, false, jlto);
}

static Value *generic_trunc(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateTrunc(x, to);
}

static Value *generic_sext(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateSExt(x, to);
}

static Value *generic_zext(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateZExt(x, to);
}

static Value *generic_uitofp(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateUIToFP(x, to);
}

static Value *generic_sitofp(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateSIToFP(x, to);
}

static Value *generic_fptoui(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateFPToUI(x, to);
}

static Value *generic_fptosi(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateFPToSI(x, to);
}

static Value *generic_fptrunc(jl_codectx_t &ctx, Type *to, Value *x)
{
    return ctx.builder.CreateFPTrunc(x, to);
}

static Value *generic_fpext(jl_codectx_t &ctx, Type *to, Value *x)
{
#ifdef JL_NEED_FLOATTEMP_VAR
    // Target platform might carry extra precision.
    // Force rounding to single precision first. The reason is that it's
    // fine to keep working in extended precision as long as it's
    // understood that everything is implicitly rounded to 23 bits,
    // but if we start looking at more bits we need to actually do the
    // rounding first instead of carrying around incorrect low bits.
    Value *jlfloattemp_var = emit_static_alloca(ctx, x->getType());
    ctx.builder.CreateStore(x, jlfloattemp_var);
    x  = ctx.builder.CreateLoad(jlfloattemp_var, true);
#endif
    return ctx.builder.CreateFPExt(x, to);
}

static jl_cgval_t emit_runtime_pointerref(jl_codectx_t &ctx, jl_cgval_t *argv)
{
    return emit_runtime_call(ctx, pointerref, argv, 3);
}

static jl_cgval_t emit_pointerref(jl_codectx_t &ctx, jl_cgval_t *argv)
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
    if (!jl_is_datatype(ety))
        ety = (jl_value_t*)jl_any_type;

    Value *idx = emit_unbox(ctx, T_size, i, (jl_value_t*)jl_long_type);
    Value *im1 = ctx.builder.CreateSub(idx, ConstantInt::get(T_size, 1));

    if (!jl_isbits(ety)) {
        if (ety == (jl_value_t*)jl_any_type) {
            Value *thePtr = emit_unbox(ctx, T_pprjlvalue, e, e.typ);
            return mark_julia_type(
                    ctx,
                    ctx.builder.CreateAlignedLoad(ctx.builder.CreateGEP(thePtr, im1), align_nb),
                    true,
                    ety);
        }
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error(ctx, "pointerref: invalid pointer type");
            return jl_cgval_t();
        }
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(ctx, size,
                                     literal_pointer_val(ctx, (jl_value_t*)ety));
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        Value *thePtr = emit_unbox(ctx, T_pint8, e, e.typ);
        thePtr = ctx.builder.CreateGEP(emit_bitcast(ctx, thePtr, T_pint8), im1);
        ctx.builder.CreateMemCpy(emit_bitcast(ctx, strct, T_pint8), thePtr, size, 1);
        return mark_julia_type(ctx, strct, true, ety);
    }

    bool isboxed;
    Type *ptrty = julia_type_to_llvm(e.typ, &isboxed);
    assert(!isboxed);
    Value *thePtr = emit_unbox(ctx, ptrty, e, e.typ);
    return typed_load(ctx, thePtr, im1, ety, tbaa_data, true, align_nb);
}

static jl_cgval_t emit_runtime_pointerset(jl_codectx_t &ctx, jl_cgval_t *argv)
{
    return emit_runtime_call(ctx, pointerset, argv, 4);
}

// e[i] = x
static jl_cgval_t emit_pointerset(jl_codectx_t &ctx, jl_cgval_t *argv)
{
    const jl_cgval_t &e = argv[0];
    const jl_cgval_t &x = argv[1];
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
    if (!jl_is_datatype(ety))
        ety = (jl_value_t*)jl_any_type;
    emit_typecheck(ctx, x, ety, "pointerset: type mismatch in assign");

    Value *idx = emit_unbox(ctx, T_size, i, (jl_value_t*)jl_long_type);
    Value *im1 = ctx.builder.CreateSub(idx, ConstantInt::get(T_size, 1));

    Value *thePtr;
    if (!jl_isbits(ety) && ety != (jl_value_t*)jl_any_type) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error(ctx, "pointerset: invalid pointer type");
            return jl_cgval_t();
        }
        thePtr = emit_unbox(ctx, T_pint8, e, e.typ);
        uint64_t size = jl_datatype_size(ety);
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        ctx.builder.CreateMemCpy(ctx.builder.CreateGEP(thePtr, im1),
                             data_pointer(ctx, x, T_pint8), size, align_nb);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(e.typ, &isboxed);
        assert(!isboxed);
        thePtr = emit_unbox(ctx, ptrty, e, e.typ);
        if (ety == (jl_value_t*)jl_any_type) {
            // unsafe_store to Ptr{Any} is allowed to implicitly drop GC roots.
            Instruction *store = ctx.builder.CreateAlignedStore(
              emit_pointer_from_objref(ctx, boxed(ctx, x, false)),
                ctx.builder.CreateGEP(thePtr, im1), align_nb);
            tbaa_decorate(tbaa_data, store);
        } else {
            typed_store(ctx, thePtr, im1, x, ety, tbaa_data, NULL, align_nb);
        }
    }
    return mark_julia_type(ctx, thePtr, false, aty);
}

static Value *emit_checked_srem_int(jl_codectx_t &ctx, Value *x, Value *den)
{
    Type *t = den->getType();
    raise_exception_unless(ctx,
            ctx.builder.CreateICmpNE(den, ConstantInt::get(t, 0)),
            literal_pointer_val(ctx, jl_diverror_exception));
    BasicBlock *m1BB = BasicBlock::Create(jl_LLVMContext, "minus1", ctx.f);
    BasicBlock *okBB = BasicBlock::Create(jl_LLVMContext, "oksrem", ctx.f);
    BasicBlock *cont = BasicBlock::Create(jl_LLVMContext, "after_srem", ctx.f);
    PHINode *ret = PHINode::Create(t, 2);
    ctx.builder.CreateCondBr(ctx.builder.CreateICmpEQ(den ,ConstantInt::get(t, -1, true)),
                         m1BB, okBB);
    ctx.builder.SetInsertPoint(m1BB);
    ctx.builder.CreateBr(cont);
    ctx.builder.SetInsertPoint(okBB);
    Value *sremval = ctx.builder.CreateSRem(x, den);
    ctx.builder.CreateBr(cont);
    ctx.builder.SetInsertPoint(cont);
    ret->addIncoming(// rem(typemin, -1) is undefined
                     ConstantInt::get(t, 0), m1BB);
    ret->addIncoming(sremval, okBB);
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
            fmf.setUnsafeAlgebra();
        }
#if JL_LLVM_VERSION >= 50000
        if (contract)
            fmf.setAllowContract(true);
#else
        assert(!contract);
#endif
        ctxbuilder.setFastMathFlags(fmf);
    }
    IRBuilder<>& operator()() const { return ctxbuilder; }
    ~math_builder() {
        ctxbuilder.setFastMathFlags(old_fmf);
    }
};

static Value *emit_untyped_intrinsic(jl_codectx_t &ctx, intrinsic f, Value **argvalues, size_t nargs,
                                     jl_datatype_t **newtyp, jl_value_t *xtyp);

static jl_cgval_t emit_intrinsic(jl_codectx_t &ctx, intrinsic f, jl_value_t **args, size_t nargs)
{
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

    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
    }

    // this forces everything to use runtime-intrinsics (e.g. for testing)
    // return emit_runtime_call(ctx, f, argv, nargs);

    switch (f) {
    case arraylen:
        return mark_julia_type(ctx, emit_arraylen(ctx, argv[0], args[1]), false, jl_long_type);
    case pointerref:
        return emit_pointerref(ctx, argv);
    case pointerset:
        return emit_pointerset(ctx, argv);
    case bitcast:
        return generic_bitcast(ctx, argv);
    case trunc_int:
        return generic_cast(ctx, f, generic_trunc, argv, true, true);
    case sext_int:
        return generic_cast(ctx, f, generic_sext, argv, true, true);
    case zext_int:
        return generic_cast(ctx, f, generic_zext, argv, true, true);
    case uitofp:
        return generic_cast(ctx, f, generic_uitofp, argv, false, true);
    case sitofp:
        return generic_cast(ctx, f, generic_sitofp, argv, false, true);
    case fptoui:
        return generic_cast(ctx, f, generic_fptoui, argv, true, false);
    case fptosi:
        return generic_cast(ctx, f, generic_fptosi, argv, true, false);
    case fptrunc:
        return generic_cast(ctx, f, generic_fptrunc, argv, false, false);
    case fpext:
        return generic_cast(ctx, f, generic_fpext, argv, false, false);

    case select_value: {
        Value *isfalse = emit_condition(ctx, argv[0], "select_value"); // emit the first argument
        // emit X and Y arguments
        const jl_cgval_t &x = argv[1];
        const jl_cgval_t &y = argv[2];
        jl_value_t *t1 = x.typ;
        jl_value_t *t2 = y.typ;
        // check the return value was valid
        if (t1 == jl_bottom_type && t2 == jl_bottom_type)
            return jl_cgval_t(); // undefined
        if (t1 == jl_bottom_type)
            return y;
        if (t2 == jl_bottom_type)
            return x;

        Value *ifelse_result;
        bool isboxed;
        Type *llt1 = julia_type_to_llvm(t1, &isboxed);
        if (t1 != t2)
            isboxed = true;
        if (!isboxed) {
            if (type_is_ghost(llt1))
                return x;
            ifelse_result = ctx.builder.CreateSelect(isfalse,
                    emit_unbox(ctx, llt1, y, t1),
                    emit_unbox(ctx, llt1, x, t1));
        }
        else {
            ifelse_result = ctx.builder.CreateSelect(isfalse,
                    boxed(ctx, y),
                    boxed(ctx, x));
        }
        jl_value_t *jt = (t1 == t2 ? t1 : (jl_value_t*)jl_any_type);
        return mark_julia_type(ctx, ifelse_result, isboxed, jt);
    }

    case not_int: {
        const jl_cgval_t &x = argv[0];
        if (!jl_is_primitivetype(x.typ))
            return emit_runtime_call(ctx, f, argv, nargs);
        Type *xt = INTT(bitstype_to_llvm(x.typ));
        Value *from = emit_unbox(ctx, xt, x, x.typ);
        Value *ans;
        if (x.typ == (jl_value_t*)jl_bool_type)
            ans = ctx.builder.CreateXor(from, ConstantInt::get(T_int8, 1, true));
        else
            ans = ctx.builder.CreateXor(from, ConstantInt::get(xt, -1, true));
        return mark_julia_type(ctx, ans, false, x.typ);
    }

    default: {
        assert(nargs >= 1 && "invalid nargs for intrinsic call");
        const jl_cgval_t &xinfo = argv[0];

        // verify argument types
        if (!jl_is_primitivetype(xinfo.typ))
            return emit_runtime_call(ctx, f, argv, nargs);
        Type *xtyp = bitstype_to_llvm(xinfo.typ);
        if (float_func[f])
            xtyp = FLOATT(xtyp);
        else
            xtyp = INTT(xtyp);
        if (!xtyp)
            return emit_runtime_call(ctx, f, argv, nargs);

        Type **argt = (Type**)alloca(sizeof(Type*) * nargs);
        argt[0] = xtyp;

        if (f == shl_int || f == lshr_int || f == ashr_int) {
            if (!jl_is_primitivetype(argv[1].typ))
                return emit_runtime_call(ctx, f, argv, nargs);
            argt[1] = INTT(bitstype_to_llvm(argv[1].typ));
        }
        else {
            for (size_t i = 1; i < nargs; ++i) {
                if (xinfo.typ != argv[i].typ)
                    return emit_runtime_call(ctx, f, argv, nargs);
                argt[i] = xtyp;
            }
        }

        // unbox the arguments
        Value **argvalues = (Value**)alloca(sizeof(Value*) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argvalues[i] = emit_unbox(ctx, argt[i], argv[i], argv[i].typ);
        }

        // call the intrinsic
        jl_value_t *newtyp = NULL;
        Value *r = emit_untyped_intrinsic(ctx, f, argvalues, nargs, (jl_datatype_t**)&newtyp, xinfo.typ);
        if (r->getType() == T_int1)
            r = ctx.builder.CreateZExt(r, T_int8);
        return mark_julia_type(ctx, r, false, newtyp ? newtyp : xinfo.typ);
    }
    }
    assert(0 && "unreachable");
}

static Value *emit_untyped_intrinsic(jl_codectx_t &ctx, intrinsic f, Value **argvalues, size_t nargs,
                                     jl_datatype_t **newtyp, jl_value_t *xtyp)
{
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

// Implements IEEE negate. See issue #7868
    case neg_float: return math_builder(ctx)().CreateFSub(ConstantFP::get(t, -0.0), x);
    case neg_float_fast: return math_builder(ctx, true)().CreateFNeg(x);
    case add_float: return math_builder(ctx)().CreateFAdd(x, y);
    case sub_float: return math_builder(ctx)().CreateFSub(x, y);
    case mul_float: return math_builder(ctx)().CreateFMul(x, y);
    case div_float: return math_builder(ctx)().CreateFDiv(x, y);
    case rem_float: return math_builder(ctx)().CreateFRem(x, y);
    case add_float_fast: return math_builder(ctx, true)().CreateFAdd(x, y);
    case sub_float_fast: return math_builder(ctx, true)().CreateFSub(x, y);
    case mul_float_fast: return math_builder(ctx, true)().CreateFMul(x, y);
    case div_float_fast: return math_builder(ctx, true)().CreateFDiv(x, y);
    case rem_float_fast: return math_builder(ctx, true)().CreateFRem(x, y);
    case fma_float: {
        assert(y->getType() == x->getType());
        assert(z->getType() == y->getType());
        Value *fmaintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fma, makeArrayRef(t));
        return ctx.builder.CreateCall(fmaintr, {x, y, z});
    }
    case muladd_float: {
#if JL_LLVM_VERSION >= 50000
        // LLVM 5.0 can create FMA in the backend for contractable fmul and fadd
        // Emitting fmul and fadd here since they are easier for other LLVM passes to
        // optimize.
        auto mathb = math_builder(ctx, false, true);
        return mathb().CreateFAdd(mathb().CreateFMul(x, y), z);
#else
        assert(y->getType() == x->getType());
        assert(z->getType() == y->getType());
        Value *muladdintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fmuladd, makeArrayRef(t));
        return ctx.builder.CreateCall(muladdintr, {x, y, z});
#endif
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
        Value *intr = Intrinsic::getDeclaration(jl_Module, intr_id, makeArrayRef(t));
        Value *res = ctx.builder.CreateCall(intr, {x, y});
        Value *val = ctx.builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
        Value *obit = ctx.builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        Value *obyte = ctx.builder.CreateZExt(obit, T_int8);

        jl_value_t *params[2];
        params[0] = xtyp;
        params[1] = (jl_value_t*)jl_bool_type;
        jl_datatype_t *tuptyp = jl_apply_tuple_type_v(params, 2);
        *newtyp = tuptyp;

        Value *tupval;
        tupval = UndefValue::get(julia_type_to_llvm((jl_value_t*)tuptyp));
        tupval = ctx.builder.CreateInsertValue(tupval, val, ArrayRef<unsigned>(0));
        tupval = ctx.builder.CreateInsertValue(tupval, obyte, ArrayRef<unsigned>(1));
        return tupval;
    }

    case checked_sdiv_int: {
        Value *typemin = ctx.builder.CreateShl(ConstantInt::get(t, 1), t->getPrimitiveSizeInBits() - 1);
        raise_exception_unless(ctx,
                ctx.builder.CreateAnd(
                    ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                    ctx.builder.CreateOr(
                        ctx.builder.CreateICmpNE(y, ConstantInt::get(t, -1, true)),
                        ctx.builder.CreateICmpNE(x, typemin))),
                literal_pointer_val(ctx, jl_diverror_exception));

        return ctx.builder.CreateSDiv(x, y);
    }
    case checked_udiv_int:
        raise_exception_unless(ctx,
                ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                literal_pointer_val(ctx, jl_diverror_exception));
        return ctx.builder.CreateUDiv(x, y);

    case checked_srem_int:
        return emit_checked_srem_int(ctx, x, y);

    case checked_urem_int:
        raise_exception_unless(ctx,
                ctx.builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                literal_pointer_val(ctx, jl_diverror_exception));
        return ctx.builder.CreateURem(x, y);

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
        Type *it = INTT(t);
        Value *xi = ctx.builder.CreateBitCast(x, it);
        Value *yi = ctx.builder.CreateBitCast(y, it);
        return ctx.builder.CreateOr(ctx.builder.CreateAnd(ctx.builder.CreateFCmpUNO(x, x),
                                                  ctx.builder.CreateFCmpUNO(y, y)),
                                ctx.builder.CreateICmpEQ(xi, yi));
    }

    case fpislt: {
        *newtyp = jl_bool_type;
        Type *it = INTT(t);
        Value *xi = ctx.builder.CreateBitCast(x, it);
        Value *yi = ctx.builder.CreateBitCast(y, it);
        return ctx.builder.CreateOr(
            ctx.builder.CreateAnd(
                ctx.builder.CreateFCmpORD(x, x),
                ctx.builder.CreateFCmpUNO(y, y)),
            ctx.builder.CreateAnd(
                ctx.builder.CreateFCmpORD(x, y),
                ctx.builder.CreateOr(
                    ctx.builder.CreateAnd(
                        ctx.builder.CreateICmpSGE(xi, ConstantInt::get(it, 0)),
                        ctx.builder.CreateICmpSLT(xi, yi)),
                    ctx.builder.CreateAnd(
                        ctx.builder.CreateICmpSLT(xi, ConstantInt::get(it, 0)),
                        ctx.builder.CreateICmpUGT(xi, yi)))));
    }

    case and_int: return ctx.builder.CreateAnd(x, y);
    case or_int:  return ctx.builder.CreateOr(x, y);
    case xor_int: return ctx.builder.CreateXor(x, y);

    case shl_int:
        return ctx.builder.CreateSelect(
                ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                ConstantInt::get(t, 0),
                ctx.builder.CreateShl(x, uint_cnvt(ctx, t, y)));
    case lshr_int:
        return ctx.builder.CreateSelect(
                ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                ConstantInt::get(t, 0),
                ctx.builder.CreateLShr(x, uint_cnvt(ctx, t, y)));
    case ashr_int:
        return ctx.builder.CreateSelect(
                ctx.builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                ctx.builder.CreateAShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits() - 1)),
                ctx.builder.CreateAShr(x, uint_cnvt(ctx, t, y)));

    case bswap_int: {
        Value *bswapintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap, makeArrayRef(t));
        return ctx.builder.CreateCall(bswapintr, x);
    }
    case ctpop_int: {
        Value *ctpopintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop, makeArrayRef(t));
        return ctx.builder.CreateCall(ctpopintr, x);
    }
    case ctlz_int: {
        Value *ctlz = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz, makeArrayRef(t));
        y = ConstantInt::get(T_int1, 0);
        return ctx.builder.CreateCall(ctlz, {x, y});
    }
    case cttz_int: {
        Value *cttz = Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, makeArrayRef(t));
        y = ConstantInt::get(T_int1, 0);
        return ctx.builder.CreateCall(cttz, {x, y});
    }

    case abs_float: {
        Value *absintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fabs, makeArrayRef(t));
        return ctx.builder.CreateCall(absintr, x);
    }
    case copysign_float: {
        Value *bits = ctx.builder.CreateBitCast(x, t);
        Value *sbits = ctx.builder.CreateBitCast(y, t);
        unsigned nb = cast<IntegerType>(t)->getBitWidth();
        APInt notsignbit = APInt::getSignedMaxValue(nb);
        APInt signbit0(nb, 0); signbit0.setBit(nb - 1);
        return ctx.builder.CreateOr(
                    ctx.builder.CreateAnd(bits, ConstantInt::get(t, notsignbit)),
                    ctx.builder.CreateAnd(sbits, ConstantInt::get(t, signbit0)));
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
        Value *ceilintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil, makeArrayRef(t));
        return ctx.builder.CreateCall(ceilintr, x);
    }
    case floor_llvm: {
        Value *floorintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::floor, makeArrayRef(t));
        return ctx.builder.CreateCall(floorintr, x);
    }
    case trunc_llvm: {
        Value *truncintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc, makeArrayRef(t));
        return ctx.builder.CreateCall(truncintr, x);
    }
    case rint_llvm: {
        Value *rintintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::rint, makeArrayRef(t));
        return ctx.builder.CreateCall(rintintr, x);
    }
    case sqrt_llvm: {
        Value *sqrtintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt, makeArrayRef(t));
        return ctx.builder.CreateCall(sqrtintr, x);
    }

    default:
        assert(0 && "invalid intrinsic");
        abort();
    }
    assert(0 && "unreachable");
}

#define BOX_F(ct,jl_ct)                                                  \
    box_##ct##_func = boxfunc_llvm(ft1arg(T_prjlvalue, T_##jl_ct),       \
                                   "jl_box_"#ct, &jl_box_##ct, m);

#define SBOX_F(ct,jl_ct) BOX_F(ct,jl_ct); box_##ct##_func->addAttribute(1, Attribute::SExt);
#define UBOX_F(ct,jl_ct) BOX_F(ct,jl_ct); box_##ct##_func->addAttribute(1, Attribute::ZExt);

template<typename T>
static Function *boxfunc_llvm(FunctionType *ft, const std::string &cname,
                              T *addr, Module *m)
{
    Function *f =
        Function::Create(ft, Function::ExternalLinkage, cname, m);
    add_named_global(f, addr);
    return f;
}

static FunctionType *ft1arg(Type *ret, Type *arg)
{
    std::vector<Type*> args1(0);
    args1.push_back(arg);
    return FunctionType::get(ret, args1, false);
}

static FunctionType *ft2arg(Type *ret, Type *arg1, Type *arg2)
{
    std::vector<Type*> args2(0);
    args2.push_back(arg1);
    args2.push_back(arg2);
    return FunctionType::get(ret, args2, false);
}
