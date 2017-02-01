// This file is a part of Julia. License is MIT: http://julialang.org/license

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
    args1.push_back(T_pjlvalue); \
    std::vector<Type *> args2(0); \
    args2.push_back(T_pjlvalue); \
    args2.push_back(T_pjlvalue); \
    std::vector<Type *> args3(0); \
    args3.push_back(T_pjlvalue); \
    args3.push_back(T_pjlvalue); \
    args3.push_back(T_pjlvalue); \
    std::vector<Type *> args4(0); \
    args4.push_back(T_pjlvalue); \
    args4.push_back(T_pjlvalue); \
    args4.push_back(T_pjlvalue); \
    args4.push_back(T_pjlvalue);

#define ADD_I(name, nargs) do { \
        Function *func = Function::Create(FunctionType::get(T_pjlvalue, args##nargs, false), \
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
    float_func[sqrt_llvm_fast] = true;
    float_func[powi_llvm] = true;
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

static Value *uint_cnvt(Type *to, Value *x)
{
    Type *t = x->getType();
    if (t == to)
        return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return builder.CreateTrunc(x, to);
    return builder.CreateZExt(x, to);
}

#if JL_LLVM_VERSION >= 40000
#define LLVM_FP(a,b) APFloat(a(),b)
#else
#define LLVM_FP(a,b) APFloat(a,b)
#endif
static Constant *julia_const_to_llvm(void *ptr, jl_value_t *bt)
{
    // assume `jl_isbits(bt)`.
    // `ptr` can point to a inline field, do not read the tag from it.
    if (bt == (jl_value_t*)jl_bool_type)
        return ConstantInt::get(T_int8, (*(uint8_t*)ptr) ? 1 : 0);

    if (bt == (jl_value_t*)jl_ssavalue_type)
        return NULL;

    if (jl_is_vecelement_type(bt))
        bt = jl_tparam0(bt);

    if (jl_is_cpointer_type(bt))
        return ConstantExpr::getIntToPtr(ConstantInt::get(T_size, *(uintptr_t*)ptr), julia_type_to_llvm(bt));
    if (jl_is_bitstype(bt)) {
        int nb = jl_datatype_size(bt);
        // TODO: non-power-of-2 size datatypes may not be interpreted correctly on big-endian systems
        switch (nb) {
        case 1: {
            uint8_t data8 = *(uint8_t*)ptr;
            return ConstantInt::get(T_int8, data8);
        }
        case 2: {
            uint16_t data16 = *(uint16_t*)ptr;
#ifndef DISABLE_FLOAT16
            if (jl_is_floattype(bt))
                return ConstantFP::get(jl_LLVMContext, LLVM_FP(APFloat::IEEEhalf,APInt(16,data16)));
#endif
            return ConstantInt::get(T_int16, data16);
        }
        case 4: {
            uint32_t data32 = *(uint32_t*)ptr;
            if (jl_is_floattype(bt))
                return ConstantFP::get(jl_LLVMContext, LLVM_FP(APFloat::IEEEsingle,APInt(32,data32)));
            return ConstantInt::get(T_int32, data32);
        }
        case 8: {
            uint64_t data64 = *(uint64_t*)ptr;
            if (jl_is_floattype(bt))
                return ConstantFP::get(jl_LLVMContext, LLVM_FP(APFloat::IEEEdouble,APInt(64,data64)));
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
            if (nb == 16 && jl_is_floattype(bt)) {
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEquad,val));
                // If we have a floating point type that's not hardware supported, just treat it like an integer for LLVM purposes
            }
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
static Value *emit_unbox(Type *to, const jl_cgval_t &x, jl_value_t *jt, Value *dest, bool volatile_store)
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
        //emit_error("emit_unbox: a type mismatch error in occurred during codegen", ctx);
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
            unboxed = emit_bitcast(unboxed, to);
        }
        else if (frompointer) {
            Type *INTT_to = INTT(to);
            unboxed = builder.CreatePtrToInt(unboxed, INTT_to);
            if (INTT_to != to)
                unboxed = builder.CreateBitCast(unboxed, to);
        }
        else if (topointer) {
            Type *INTT_to = INTT(to);
            if (to != INTT_to)
                unboxed = builder.CreateBitCast(unboxed, INTT_to);
            unboxed = builder.CreateIntToPtr(unboxed, to);
        }
        else if (ty == T_int1 && to == T_int8) {
            // bools may be stored internally as int8
            unboxed = builder.CreateZExt(unboxed, T_int8);
        }
        else {
            unboxed = builder.CreateBitCast(unboxed, to);
        }
        if (!dest)
            return unboxed;
        builder.CreateStore(unboxed, dest, volatile_store);
        return NULL;
    }

    // bools stored as int8, so an extra Trunc is needed to get an int1
    Value *p = x.constant ? literal_pointer_val(x.constant) : x.V;
    Type *ptype = (to == T_int1 ? T_pint8 : to->getPointerTo());
    if (p->getType() != ptype)
        p = emit_bitcast(p, ptype);

    Value *unboxed = NULL;
    if (to == T_int1)
        unboxed = builder.CreateTrunc(tbaa_decorate(x.tbaa, builder.CreateLoad(p)), T_int1);
    else if (jt == (jl_value_t*)jl_bool_type)
        unboxed = builder.CreateZExt(builder.CreateTrunc(tbaa_decorate(x.tbaa, builder.CreateLoad(p)), T_int1), to);
    if (unboxed) {
        if (!dest)
            return unboxed;
        builder.CreateStore(unboxed, dest);
        return NULL;
    }

    int alignment;
    if (x.isboxed) {
         // julia's gc gives 16-byte aligned addresses
        alignment = 16;
    }
    else if (jt) {
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
        builder.CreateMemCpy(dest, p, jl_datatype_size(jt), alignment, volatile_store, tbaa);
        return NULL;
    }
    else {
        Instruction *load;
        if (alignment)
            load = builder.CreateAlignedLoad(p, alignment);
        else
            load = builder.CreateLoad(p);
        return tbaa_decorate(x.tbaa, load);
    }
}

static jl_value_t *staticeval_bitstype(const jl_cgval_t &targ)
{
    // evaluate an argument at compile time to determine what type it is
    if (jl_is_type_type(targ.typ)) {
        jl_value_t *bt = jl_tparam0(targ.typ);
        if (jl_is_bitstype(bt))
            return bt;
    }
    return NULL;
}

static jl_cgval_t emit_runtime_call(JL_I::intrinsic f, const jl_cgval_t *argv, size_t nargs, jl_codectx_t *ctx)
{
    Value *func = prepare_call(runtime_func[f]);
    Value **argvalues = (Value**)alloca(sizeof(Value*) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argvalues[i] = boxed(argv[i], ctx);
    }
    Value *r = builder.CreateCall(func, makeArrayRef(argvalues, nargs));
    return mark_julia_type(r, true, (jl_value_t*)jl_any_type, ctx);
}

// put a bits type tag on some value (despite the name, this doesn't necessarily actually change anything about the value however)
static jl_cgval_t generic_bitcast(const jl_cgval_t *argv, jl_codectx_t *ctx)
{
    // Give the arguments names //
    const jl_cgval_t &bt_value = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_value_t *bt = staticeval_bitstype(bt_value);

    // it's easier to throw a good error from C than llvm
    if (!bt)
        return emit_runtime_call(bitcast, argv, 2, ctx);

    Type *llvmt = bitstype_to_llvm(bt);
    int nb = jl_datatype_size(bt);

    // Examine the second argument //
    bool isboxed;
    Type *vxt = julia_type_to_llvm(v.typ, &isboxed);

    if (!jl_is_bitstype(v.typ) || jl_datatype_size(v.typ) != nb) {
        Value *typ = emit_typeof_boxed(v, ctx);
        if (!jl_is_bitstype(v.typ)) {
            if (isboxed) {
                Value *isbits = emit_datatype_isbitstype(typ);
                error_unless(isbits, "bitcast: expected bitstype value for second argument", ctx);
            }
            else {
                emit_error("bitcast: expected bitstype value for second argument", ctx);
                return jl_cgval_t();
            }
        }
        if (jl_datatype_size(v.typ) != nb) {
            if (isboxed) {
                Value *size = emit_datatype_size(typ);
                error_unless(builder.CreateICmpEQ(size, ConstantInt::get(T_int32, nb)),
                            "bitcast: argument size does not match size of target type", ctx);
            }
            else {
                emit_error("bitcast: argument size does not match size of target type", ctx);
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
        vx = tbaa_decorate(v.tbaa, builder.CreateLoad(data_pointer(v, ctx,
                                                                   vxt == T_int1 ? T_pint8 : vxt->getPointerTo())));
    }

    vxt = vx->getType();
    if (vxt != llvmt) {
        if (llvmt == T_int1)
            vx = builder.CreateTrunc(vx, llvmt);
        else if (vxt == T_int1 && llvmt == T_int8)
            vx = builder.CreateZExt(vx, llvmt);
        else if (vxt->isPointerTy() && !llvmt->isPointerTy())
            vx = builder.CreatePtrToInt(vx, llvmt);
        else if (!vxt->isPointerTy() && llvmt->isPointerTy())
            vx = builder.CreateIntToPtr(vx, llvmt);
        else
            vx = emit_bitcast(vx, llvmt);
    }

    if (jl_is_leaf_type(bt))
        return mark_julia_type(vx, false, bt, ctx);
    else
        return mark_julia_type(
            init_bits_value(emit_allocobj(ctx, nb, boxed(bt_value, ctx)),
                            vx, tbaa_immut),
            true, bt, ctx);
}

static jl_cgval_t generic_cast(
        intrinsic f, Value *(*generic)(Type*, Value*, jl_codectx_t*),
        const jl_cgval_t *argv, jl_codectx_t *ctx, bool toint, bool fromint)
{
    const jl_cgval_t &targ = argv[0];
    const jl_cgval_t &v = argv[1];
    jl_value_t *jlto = staticeval_bitstype(targ);
    if (!jlto || !jl_is_bitstype(v.typ))
        return emit_runtime_call(f, argv, 2, ctx);
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
        return emit_runtime_call(f, argv, 2, ctx);
    Value *from = emit_unbox(vt, v, v.typ);
    Value *ans = generic(to, from, ctx);
    return mark_julia_type(ans, false, jlto, ctx);
}

static Value *generic_trunc(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateTrunc(x, to);
}

static Value *generic_trunc_uchecked(Type *to, Value *x, jl_codectx_t *ctx)
{
    Value *ans = builder.CreateTrunc(x, to);
    Value *back = builder.CreateZExt(ans, x->getType());
    raise_exception_unless(builder.CreateICmpEQ(back, x),
                           literal_pointer_val(jl_inexact_exception), ctx);
    return ans;
}

static Value *generic_trunc_schecked(Type *to, Value *x, jl_codectx_t *ctx)
{
    Value *ans = builder.CreateTrunc(x, to);
    Value *back = builder.CreateSExt(ans, x->getType());
    raise_exception_unless(builder.CreateICmpEQ(back, x),
                           literal_pointer_val(jl_inexact_exception), ctx);
    return ans;
}

static Value *generic_sext(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateSExt(x, to);
}

static Value *generic_zext(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateZExt(x, to);
}

static Value *generic_uitofp(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateUIToFP(x, to);
}

static Value *generic_sitofp(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateSIToFP(x, to);
}

static Value *generic_fptoui(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateFPToUI(x, to);
}

static Value *generic_fptosi(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateFPToSI(x, to);
}

static Value *generic_fptrunc(Type *to, Value *x, jl_codectx_t *ctx)
{
    return builder.CreateFPTrunc(x, to);
}

static Value *generic_fpext(Type *to, Value *x, jl_codectx_t *ctx)
{
#ifdef JL_NEED_FLOATTEMP_VAR
    // Target platform might carry extra precision.
    // Force rounding to single precision first. The reason is that it's
    // fine to keep working in extended precision as long as it's
    // understood that everything is implicitly rounded to 23 bits,
    // but if we start looking at more bits we need to actually do the
    // rounding first instead of carrying around incorrect low bits.
    Value *jlfloattemp_var = emit_static_alloca(x->getType());
    builder.CreateStore(x, jlfloattemp_var);
    x  = builder.CreateLoad(jlfloattemp_var, true);
#endif
    return builder.CreateFPExt(x, to);
}

static jl_cgval_t emit_runtime_pointerref(jl_cgval_t *argv, jl_codectx_t *ctx)
{
    return emit_runtime_call(pointerref, argv, 3, ctx);
}

static jl_cgval_t emit_pointerref(jl_cgval_t *argv, jl_codectx_t *ctx)
{
    const jl_cgval_t &e = argv[0];
    const jl_cgval_t &i = argv[1];
    const jl_cgval_t &align = argv[2];

    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerref(argv, ctx);
    unsigned align_nb = jl_unbox_long(align.constant);

    if (i.typ != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerref(argv, ctx);
    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerref(argv, ctx);
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerref(argv, ctx);
    if (!jl_is_datatype(ety))
        ety = (jl_value_t*)jl_any_type;

    Value *idx = emit_unbox(T_size, i, (jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));

    if (!jl_isbits(ety)) {
        if (ety == (jl_value_t*)jl_any_type) {
            Value *thePtr = emit_unbox(T_ppjlvalue, e, e.typ);
            return mark_julia_type(
                    builder.CreateAlignedLoad(builder.CreateGEP(thePtr, im1), align_nb),
                    true,
                    ety, ctx);
        }
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerref: invalid pointer type", ctx);
            return jl_cgval_t();
        }
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(ctx, size,
                                     literal_pointer_val((jl_value_t*)ety));
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->layout->alignment)));
        Value *thePtr = emit_unbox(T_pint8, e, e.typ);
        thePtr = builder.CreateGEP(emit_bitcast(thePtr, T_pint8), im1);
        builder.CreateMemCpy(emit_bitcast(strct, T_pint8), thePtr, size, 1);
        return mark_julia_type(strct, true, ety, ctx);
    }

    bool isboxed;
    Type *ptrty = julia_type_to_llvm(e.typ, &isboxed);
    assert(!isboxed);
    Value *thePtr = emit_unbox(ptrty, e, e.typ);
    return typed_load(thePtr, im1, ety, ctx, tbaa_data, align_nb);
}

static jl_cgval_t emit_runtime_pointerset(jl_cgval_t *argv, jl_codectx_t *ctx)
{
    return emit_runtime_call(pointerset, argv, 4, ctx);
}

// e[i] = x
static jl_cgval_t emit_pointerset(jl_cgval_t *argv, jl_codectx_t *ctx)
{
    const jl_cgval_t &e = argv[0];
    const jl_cgval_t &x = argv[1];
    const jl_cgval_t &i = argv[2];
    const jl_cgval_t &align = argv[3];

    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerset(argv, ctx);
    unsigned align_nb = jl_unbox_long(align.constant);

    if (i.typ != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerset(argv, ctx);
    jl_value_t *aty = e.typ;
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerset(argv, ctx);
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerset(argv, ctx);
    if (align.constant == NULL || !jl_is_long(align.constant))
        return emit_runtime_pointerset(argv, ctx);
    if (!jl_is_datatype(ety))
        ety = (jl_value_t*)jl_any_type;
    jl_value_t *xty = x.typ;
    if (!jl_subtype(xty, ety))
        emit_typecheck(x, ety, "pointerset: type mismatch in assign", ctx);

    Value *idx = emit_unbox(T_size, i, (jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));

    Value *thePtr;
    if (!jl_isbits(ety) && ety != (jl_value_t*)jl_any_type) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerset: invalid pointer type", ctx);
            return jl_cgval_t();
        }
        thePtr = emit_unbox(T_pint8, e, e.typ);
        uint64_t size = jl_datatype_size(ety);
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->layout->alignment)));
        builder.CreateMemCpy(builder.CreateGEP(thePtr, im1),
                             data_pointer(x, ctx, T_pint8), size, align_nb);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(e.typ, &isboxed);
        assert(!isboxed);
        thePtr = emit_unbox(ptrty, e, e.typ);
        typed_store(thePtr, im1, x, ety, ctx, tbaa_data, NULL, align_nb);
    }
    return mark_julia_type(thePtr, false, aty, ctx);
}

static Value *emit_checked_srem_int(Value *x, Value *den, jl_codectx_t *ctx)
{
    Type *t = den->getType();
    raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                           literal_pointer_val(jl_diverror_exception), ctx);
    BasicBlock *m1BB = BasicBlock::Create(jl_LLVMContext,"minus1",ctx->f);
    BasicBlock *okBB = BasicBlock::Create(jl_LLVMContext,"oksrem",ctx->f);
    BasicBlock *cont = BasicBlock::Create(jl_LLVMContext,"after_srem",ctx->f);
    PHINode *ret = PHINode::Create(t, 2);
    builder.CreateCondBr(builder.CreateICmpEQ(den,ConstantInt::get(t,-1,true)),
                         m1BB, okBB);
    builder.SetInsertPoint(m1BB);
    builder.CreateBr(cont);
    builder.SetInsertPoint(okBB);
    Value *sremval = builder.CreateSRem(x, den);
    builder.CreateBr(cont);
    builder.SetInsertPoint(cont);
    ret->addIncoming(// rem(typemin, -1) is undefined
                     ConstantInt::get(t,0), m1BB);
    ret->addIncoming(sremval, okBB);
    builder.Insert(ret);
    return ret;
}

// Temporarily switch the builder to fast-math mode if requested
struct math_builder {
    FastMathFlags old_fmf;
    math_builder(jl_codectx_t *ctx, bool always_fast = false):
        old_fmf(builder.getFastMathFlags())
    {
        if (jl_options.fast_math != JL_OPTIONS_FAST_MATH_OFF &&
            (always_fast ||
             jl_options.fast_math == JL_OPTIONS_FAST_MATH_ON)) {
            FastMathFlags fmf;
            fmf.setUnsafeAlgebra();
#if JL_LLVM_VERSION >= 30800
            builder.setFastMathFlags(fmf);
#else
            builder.SetFastMathFlags(fmf);
#endif
        }
    }
    IRBuilder<>& operator()() const { return builder; }
    ~math_builder() {
#if JL_LLVM_VERSION >= 30800
        builder.setFastMathFlags(old_fmf);
#else
        builder.SetFastMathFlags(old_fmf);
#endif
    }
};

static Value *emit_untyped_intrinsic(intrinsic f, Value **argvalues, size_t nargs,
                                     jl_codectx_t *ctx, jl_datatype_t **newtyp, jl_value_t *xtyp);

static jl_cgval_t emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                                 jl_codectx_t *ctx)
{
    assert(f < num_intrinsics);
    if (f == cglobal && nargs == 1)
        f = cglobal_auto;
    unsigned expected_nargs = intrinsic_nargs[f];
    if (expected_nargs && expected_nargs != nargs) {
        jl_errorf("intrinsic #%d %s: wrong number of arguments", f, JL_I::jl_intrinsic_name((int)f));
    }

    if (f == llvmcall)
        return emit_llvmcall(args, nargs, ctx);
    if (f == cglobal_auto || f == cglobal)
        return emit_cglobal(args, nargs, ctx);

    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(args[i + 1], ctx);
    }

    // this forces everything to use runtime-intrinsics (e.g. for testing)
    // return emit_runtime_call(f, argv, nargs, ctx);

    switch (f) {
    case arraylen:
        return mark_julia_type(emit_arraylen(argv[0], args[1], ctx), false, jl_long_type, ctx);
    case pointerref:
        return emit_pointerref(argv, ctx);
    case pointerset:
        return emit_pointerset(argv, ctx);
    case bitcast:
        return generic_bitcast(argv, ctx);
    case trunc_int:
        return generic_cast(f, generic_trunc, argv, ctx, true, true);
    case checked_trunc_uint:
        return generic_cast(f, generic_trunc_uchecked, argv, ctx, true, true);
    case checked_trunc_sint:
        return generic_cast(f, generic_trunc_schecked, argv, ctx, true, true);
    case sext_int:
        return generic_cast(f, generic_sext, argv, ctx, true, true);
    case zext_int:
        return generic_cast(f, generic_zext, argv, ctx, true, true);
    case uitofp:
        return generic_cast(f, generic_uitofp, argv, ctx, false, true);
    case sitofp:
        return generic_cast(f, generic_sitofp, argv, ctx, false, true);
    case fptoui:
        return generic_cast(f, generic_fptoui, argv, ctx, true, false);
    case fptosi:
        return generic_cast(f, generic_fptosi, argv, ctx, true, false);
    case fptrunc:
        return generic_cast(f, generic_fptrunc, argv, ctx, false, false);
    case fpext:
        return generic_cast(f, generic_fpext, argv, ctx, false, false);

    case select_value: {
        Value *isfalse = emit_condition(argv[0], "select_value", ctx); // emit the first argument
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
            ifelse_result = builder.CreateSelect(isfalse,
                    emit_unbox(llt1, y, t1),
                    emit_unbox(llt1, x, t1));
        }
        else {
            ifelse_result = builder.CreateSelect(isfalse,
                    boxed(y, ctx),
                    boxed(x, ctx));
        }
        jl_value_t *jt = (t1 == t2 ? t1 : (jl_value_t*)jl_any_type);
        mark_gc_use(x);
        mark_gc_use(y);
        return mark_julia_type(ifelse_result, isboxed, jt, ctx);
    }

    case not_int: {
        const jl_cgval_t &x = argv[0];
        if (!jl_is_bitstype(x.typ))
            return emit_runtime_call(f, argv, nargs, ctx);
        Type *xt = INTT(bitstype_to_llvm(x.typ));
        Value *from = emit_unbox(xt, x, x.typ);
        Value *ans;
        if (x.typ == (jl_value_t*)jl_bool_type)
            ans = builder.CreateXor(from, ConstantInt::get(T_int8, 1, true));
        else
            ans = builder.CreateXor(from, ConstantInt::get(xt, -1, true));
        return mark_julia_type(ans, false, x.typ, ctx);
    }

    case powi_llvm: {
        const jl_cgval_t &x = argv[0];
        const jl_cgval_t &y = argv[1];
        if (!jl_is_bitstype(x.typ) || !jl_is_bitstype(y.typ) || jl_datatype_size(y.typ) != 4)
            return emit_runtime_call(f, argv, nargs, ctx);
        Type *xt = FLOATT(bitstype_to_llvm(x.typ));
        Type *yt = T_int32;
        if (!xt)
            return emit_runtime_call(f, argv, nargs, ctx);

        Value *xv = emit_unbox(xt, x, x.typ);
        Value *yv = emit_unbox(yt, y, y.typ);
#if JL_LLVM_VERSION >= 30600
        Value *powi = Intrinsic::getDeclaration(jl_Module, Intrinsic::powi, makeArrayRef(xt));
#if JL_LLVM_VERSION >= 30700
        Value *ans = builder.CreateCall(powi, {xv, yv});
#else
        Value *ans = builder.CreateCall2(powi, xv, yv);
#endif
#else
        // issue #6506
        Value *ans = builder.CreateCall2(prepare_call(xt == T_float64 ? jlpow_func : jlpowf_func),
                xv, builder.CreateSIToFP(yv, xt));
#endif
        return mark_julia_type(ans, false, x.typ, ctx);
    }

    default: {
        assert(nargs >= 1 && "invalid nargs for intrinsic call");
        const jl_cgval_t &xinfo = argv[0];

        // verify argument types
        if (!jl_is_bitstype(xinfo.typ))
            return emit_runtime_call(f, argv, nargs, ctx);
        Type *xtyp = bitstype_to_llvm(xinfo.typ);
        if (float_func[f])
            xtyp = FLOATT(xtyp);
        else
            xtyp = INTT(xtyp);
        if (!xtyp)
            return emit_runtime_call(f, argv, nargs, ctx);

        Type **argt = (Type**)alloca(sizeof(Type*) * nargs);
        argt[0] = xtyp;

        if (f == shl_int || f == lshr_int || f == ashr_int) {
            if (!jl_is_bitstype(argv[1].typ))
                return emit_runtime_call(f, argv, nargs, ctx);
            argt[1] = INTT(bitstype_to_llvm(argv[1].typ));
        }
        else {
            for (size_t i = 1; i < nargs; ++i) {
                if (xinfo.typ != argv[i].typ)
                    return emit_runtime_call(f, argv, nargs, ctx);
                argt[i] = xtyp;
            }
        }

        // unbox the arguments
        Value **argvalues = (Value**)alloca(sizeof(Value*) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argvalues[i] = emit_unbox(argt[i], argv[i], argv[i].typ);
        }

        // call the intrinsic
        jl_value_t *newtyp = NULL;
        Value *r = emit_untyped_intrinsic(f, argvalues, nargs, ctx, (jl_datatype_t**)&newtyp, xinfo.typ);
        if (r->getType() == T_int1)
            r = builder.CreateZExt(r, T_int8);
        return mark_julia_type(r, false, newtyp ? newtyp : xinfo.typ, ctx);
    }
    }
    assert(0 && "unreachable");
}

static Value *emit_untyped_intrinsic(intrinsic f, Value **argvalues, size_t nargs,
                                     jl_codectx_t *ctx, jl_datatype_t **newtyp, jl_value_t *xtyp)
{
    Value *x = nargs > 0 ? argvalues[0] : NULL;
    Value *y = nargs > 1 ? argvalues[1] : NULL;
    Value *z = nargs > 2 ? argvalues[2] : NULL;
    Type *t = x->getType();

    switch (f) {
    case neg_int:
#if JL_LLVM_VERSION >= 30700
        return builder.CreateNeg(x);
#else
        return builder.CreateSub(ConstantInt::get(t, 0), x);
#endif
    case add_int: return builder.CreateAdd(x, y);
    case sub_int: return builder.CreateSub(x, y);
    case mul_int: return builder.CreateMul(x, y);
    case sdiv_int: return builder.CreateSDiv(x, y);
    case udiv_int: return builder.CreateUDiv(x, y);
    case srem_int: return builder.CreateSRem(x, y);
    case urem_int: return builder.CreateURem(x, y);

// Implements IEEE negate. Unfortunately there is no compliant way
// to implement this in LLVM 3.4, though there are two different idioms
// that do the correct thing on LLVM <= 3.3 and >= 3.5 respectively.
// See issue #7868
#if JL_LLVM_VERSION >= 30500
    case neg_float: return math_builder(ctx)().CreateFSub(ConstantFP::get(t, -0.0), x);
    case neg_float_fast: return math_builder(ctx, true)().CreateFNeg(x);
#else
    case neg_float:
        return math_builder(ctx)().CreateFMul(ConstantFP::get(t, -1.0), x);
    case neg_float_fast:
        return math_builder(ctx, true)().CreateFMul(ConstantFP::get(t, -1.0), x);
#endif
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
#if JL_LLVM_VERSION >= 30700
        return builder.CreateCall(fmaintr, {x, y, z});
#else
        return builder.CreateCall3(fmaintr, x, y, z);
#endif
    }
    case muladd_float: {
#if JL_LLVM_VERSION >= 30400
        assert(y->getType() == x->getType());
        assert(z->getType() == y->getType());
        Value *muladdintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fmuladd, makeArrayRef(t));
#if JL_LLVM_VERSION >= 30700
        return builder.CreateCall(muladdintr, {x, y, z});
#else
        return builder.CreateCall3(muladdintr, x, y, z);
#endif
#else
        return math_builder(ctx, true)().CreateFAdd(builder.CreateFMul(x, y), z);
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
#if JL_LLVM_VERSION >= 30700
        Value *res = builder.CreateCall(intr, {x, y});
#else
        Value *res = builder.CreateCall2(intr, x, y);
#endif
        Value *val = builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
        Value *obit = builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        Value *obyte = builder.CreateZExt(obit, T_int8);

        jl_value_t *params[2];
        params[0] = xtyp;
        params[1] = (jl_value_t*)jl_bool_type;
        jl_datatype_t *tuptyp = jl_apply_tuple_type_v(params, 2);
        *newtyp = tuptyp;

        Value *tupval;
        tupval = UndefValue::get(julia_type_to_llvm((jl_value_t*)tuptyp));
        tupval = builder.CreateInsertValue(tupval, val, ArrayRef<unsigned>(0));
        tupval = builder.CreateInsertValue(tupval, obyte, ArrayRef<unsigned>(1));
        return tupval;
    }

    case checked_sdiv_int: {
        Value *typemin = builder.CreateShl(ConstantInt::get(t, 1), t->getPrimitiveSizeInBits() - 1);
        raise_exception_unless(
                builder.CreateAnd(
                    builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                    builder.CreateOr(
                        builder.CreateICmpNE(y, ConstantInt::get(t, -1, true)),
                        builder.CreateICmpNE(x, typemin))),
                literal_pointer_val(jl_diverror_exception), ctx);

        return builder.CreateSDiv(x, y);
    }
    case checked_udiv_int:
        raise_exception_unless(builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                               literal_pointer_val(jl_diverror_exception), ctx);
        return builder.CreateUDiv(x, y);

    case checked_srem_int:
        return emit_checked_srem_int(x, y, ctx);

    case checked_urem_int:
        raise_exception_unless(builder.CreateICmpNE(y, ConstantInt::get(t, 0)),
                               literal_pointer_val(jl_diverror_exception), ctx);
        return builder.CreateURem(x, y);

    case check_top_bit:
        // raise InexactError if argument's top bit is set
        raise_exception_if(
                builder.CreateTrunc(
                    builder.CreateLShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits() - 1)),
                    T_int1),
                literal_pointer_val(jl_inexact_exception), ctx);
        return x;

    case eq_int:  *newtyp = jl_bool_type; return builder.CreateICmpEQ(x, y);
    case ne_int:  *newtyp = jl_bool_type; return builder.CreateICmpNE(x, y);
    case slt_int: *newtyp = jl_bool_type; return builder.CreateICmpSLT(x, y);
    case ult_int: *newtyp = jl_bool_type; return builder.CreateICmpULT(x, y);
    case sle_int: *newtyp = jl_bool_type; return builder.CreateICmpSLE(x, y);
    case ule_int: *newtyp = jl_bool_type; return builder.CreateICmpULE(x, y);

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
        Value *xi = builder.CreateBitCast(x, it);
        Value *yi = builder.CreateBitCast(y, it);
        return builder.CreateOr(builder.CreateAnd(builder.CreateFCmpUNO(x, x),
                                                  builder.CreateFCmpUNO(y, y)),
                                builder.CreateICmpEQ(xi, yi));
    }

    case fpislt: {
        *newtyp = jl_bool_type;
        Type *it = INTT(t);
        Value *xi = builder.CreateBitCast(x, it);
        Value *yi = builder.CreateBitCast(y, it);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpORD(x, x),
                builder.CreateFCmpUNO(y, y)),
            builder.CreateAnd(
                builder.CreateFCmpORD(x, y),
                builder.CreateOr(
                    builder.CreateAnd(
                        builder.CreateICmpSGE(xi, ConstantInt::get(it, 0)),
                        builder.CreateICmpSLT(xi, yi)),
                    builder.CreateAnd(
                        builder.CreateICmpSLT(xi, ConstantInt::get(it, 0)),
                        builder.CreateICmpUGT(xi, yi)))));
    }

    case and_int: return builder.CreateAnd(x, y);
    case or_int:  return builder.CreateOr(x, y);
    case xor_int: return builder.CreateXor(x, y);

    case shl_int:
        return builder.CreateSelect(
                builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                ConstantInt::get(t, 0),
                builder.CreateShl(x, uint_cnvt(t, y)));
    case lshr_int:
        return builder.CreateSelect(
                builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                ConstantInt::get(t, 0),
                builder.CreateLShr(x, uint_cnvt(t, y)));
    case ashr_int:
        return builder.CreateSelect(
                builder.CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                          t->getPrimitiveSizeInBits())),
                builder.CreateAShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits() - 1)),
                builder.CreateAShr(x, uint_cnvt(t, y)));

    case bswap_int: {
        Value *bswapintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap, makeArrayRef(t));
        return builder.CreateCall(bswapintr, x);
    }
    case ctpop_int: {
        Value *ctpopintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop, makeArrayRef(t));
        return builder.CreateCall(ctpopintr, x);
    }
    case ctlz_int: {
        Value *ctlz = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz, makeArrayRef(t));
        y = ConstantInt::get(T_int1, 0);
#if JL_LLVM_VERSION >= 30700
        return builder.CreateCall(ctlz, {x, y});
#else
        return builder.CreateCall2(ctlz, x, y);
#endif
    }
    case cttz_int: {
        Value *cttz = Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, makeArrayRef(t));
        y = ConstantInt::get(T_int1, 0);
#if JL_LLVM_VERSION >= 30700
        return builder.CreateCall(cttz, {x, y});
#else
        return builder.CreateCall2(cttz, x, y);
#endif
    }

    case abs_float: {
#if JL_LLVM_VERSION >= 30400
        Value *absintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fabs, makeArrayRef(t));
        return builder.CreateCall(absintr, x);
#else
        Type *intt = INTT(t);
        Value *bits = builder.CreateBitCast(x, intt);
        Value *absbits =
            builder.CreateAnd(bits,
                              ConstantInt::get(intt, APInt::getSignedMaxValue(cast<IntegerType>(intt)->getBitWidth())));
        return builder.CreateBitCast(absbits, t);
#endif
    }
    case copysign_float: {
        Value *bits = builder.CreateBitCast(x, t);
        Value *sbits = builder.CreateBitCast(y, t);
        unsigned nb = cast<IntegerType>(t)->getBitWidth();
        APInt notsignbit = APInt::getSignedMaxValue(nb);
        APInt signbit0(nb, 0); signbit0.setBit(nb - 1);
        return builder.CreateOr(
                    builder.CreateAnd(bits, ConstantInt::get(t, notsignbit)),
                    builder.CreateAnd(sbits, ConstantInt::get(t, signbit0)));
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
            return iy.isNonNegative() ? x : builder.CreateSub(ConstantInt::get(t, 0), x);
        }
        Value *tmp = builder.CreateAShr(y, ConstantInt::get(t, cast<IntegerType>(t)->getBitWidth() - 1));
        return builder.CreateXor(builder.CreateAdd(x, tmp), tmp);
    }
    case ceil_llvm: {
        Value *ceilintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil, makeArrayRef(t));
        return builder.CreateCall(ceilintr, x);
    }
    case floor_llvm: {
        Value *floorintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::floor, makeArrayRef(t));
        return builder.CreateCall(floorintr, x);
    }
    case trunc_llvm: {
        Value *truncintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc, makeArrayRef(t));
        return builder.CreateCall(truncintr, x);
    }
    case rint_llvm: {
        Value *rintintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::rint, makeArrayRef(t));
        return builder.CreateCall(rintintr, x);
    }
    case sqrt_llvm:
        raise_exception_unless(builder.CreateFCmpUGE(x, ConstantFP::get(t, 0.0)),
                               literal_pointer_val(jl_domain_exception), ctx);
        // fall-through
    case sqrt_llvm_fast: {
        Value *sqrtintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt, makeArrayRef(t));
        return builder.CreateCall(sqrtintr, x);
    }

    default:
        assert(0 && "invalid intrinsic");
        abort();
    }
    assert(0 && "unreachable");
}

#define BOX_F(ct,jl_ct)                                                 \
    box_##ct##_func = boxfunc_llvm(ft1arg(T_pjlvalue, T_##jl_ct),       \
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
