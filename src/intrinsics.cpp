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
    float_func[sqrt_llvm_fast] = true;
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

static Constant *julia_const_to_llvm(const void *ptr, jl_datatype_t *bt)
{
    // assumes `jl_is_pointerfree(bt)`.
    // `ptr` can point to a inline field, do not read the tag from it.
    // make sure to return exactly the type specified by
    // julia_type_to_llvm as this will be assumed by the callee.
    if (bt == jl_bool_type)
        return ConstantInt::get(T_int8, (*(const uint8_t*)ptr) ? 1 : 0);

    Type *lt = julia_struct_to_llvm((jl_value_t*)bt, NULL, NULL);

    if (type_is_ghost(lt))
        return UndefValue::get(lt);

    if (lt->isFloatTy()) {
        uint32_t data32 = *(const uint32_t*)ptr;
        return ConstantFP::get(jl_LLVMContext,
                APFloat(lt->getFltSemantics(), APInt(32, data32)));
    }
    if (lt->isDoubleTy()) {
        uint64_t data64 = *(const uint64_t*)ptr;
        return ConstantFP::get(jl_LLVMContext,
                APFloat(lt->getFltSemantics(), APInt(64, data64)));
    }
    if (lt->isFloatingPointTy() || lt->isIntegerTy()) {
        int nb = jl_datatype_size(bt);
        APInt val(8 * nb, 0);
        void *bits = const_cast<uint64_t*>(val.getRawData());
        assert(sys::IsLittleEndianHost);
        memcpy(bits, ptr, nb);
        if (lt->isFloatingPointTy()) {
            return ConstantFP::get(jl_LLVMContext,
                    APFloat(lt->getFltSemantics(), val));
        }
        assert(cast<IntegerType>(lt)->getBitWidth() == 8u * nb);
        return ConstantInt::get(lt, val);
    }

    CompositeType *lct = cast<CompositeType>(lt);
    size_t nf = jl_datatype_nfields(bt);
    std::vector<Constant*> fields(0);
    for (size_t i = 0; i < nf; i++) {
        size_t offs = jl_field_offset(bt, i);
        jl_value_t *ft = jl_field_type(bt, i);
        Type *lft = julia_type_to_llvm(ft);
        if (type_is_ghost(lft))
            continue;
        assert(!jl_field_isptr(bt, i));
        unsigned llvm_idx = isa<StructType>(lt) ? convert_struct_offset(lt, offs) : i;
        while (fields.size() < llvm_idx)
            fields.push_back(UndefValue::get(lct->getTypeAtIndex(fields.size())));
        const uint8_t *ov = (const uint8_t*)ptr + offs;
        if (jl_is_uniontype(ft)) {
            // compute the same type layout as julia_struct_to_llvm
            size_t fsz = jl_field_size(bt, i);
            size_t al = jl_field_align(bt, i);
            uint8_t sel = ((const uint8_t*)ptr)[offs + fsz - 1];
            jl_value_t *active_ty = jl_nth_union_component(ft, sel);
            size_t active_sz = jl_datatype_size(active_ty);
            Type *AlignmentType = IntegerType::get(jl_LLVMContext, 8 * al);
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
                    fld = ConstantInt::get(T_int8, Elem);
                }
                else {
                    fld = UndefValue::get(T_int8);
                }
                ov += 1;
                fields.push_back(fld);
            }
            fields.push_back(ConstantInt::get(T_int8, sel));
        }
        else {
            Constant *val = julia_const_to_llvm(ov, (jl_datatype_t*)ft);
            fields.push_back(val);
        }
    }

    if (lct->isVectorTy())
        return ConstantVector::get(fields);
    if (StructType *st = dyn_cast<StructType>(lct))
        return ConstantStruct::get(st, fields);
    ArrayType *at = cast<ArrayType>(lct);
    return ConstantArray::get(at, fields);
}

static Constant *julia_const_to_llvm(jl_value_t *e)
{
    if (e == jl_true)
        return ConstantInt::get(T_int8, 1);
    if (e == jl_false)
        return ConstantInt::get(T_int8, 0);
    jl_value_t *bt = jl_typeof(e);
    if (!jl_is_pointerfree(bt))
        return NULL;
    return julia_const_to_llvm(e, (jl_datatype_t*)bt);
}

static jl_cgval_t ghostValue(jl_value_t *ty);

static Value *emit_unboxed_coercion(jl_codectx_t &ctx, Type *to, Value *unboxed)
{
    Type *ty = unboxed->getType();
    bool frompointer = ty->isPointerTy();
    bool topointer = to->isPointerTy();
    const DataLayout &DL = jl_data_layout;
    if (ty == T_int1 && to == T_int8) {
        // bools may be stored internally as int8
        unboxed = ctx.builder.CreateZExt(unboxed, T_int8);
    }
    else if (ty == T_int8 && to == T_int1) {
        // bools may be stored internally as int8
        unboxed = ctx.builder.CreateTrunc(unboxed, T_int1);
    }
    else if (ty == T_void || DL.getTypeSizeInBits(ty) != DL.getTypeSizeInBits(to)) {
        // this can happen in dead code
        //emit_unreachable(ctx);
        return UndefValue::get(to);
    }
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
    else if (ty != to) {
        unboxed = ctx.builder.CreateBitCast(unboxed, to);
    }
    return unboxed;
}

// emit code to unpack a raw value from a box into registers or a stack slot
static Value *emit_unbox(jl_codectx_t &ctx, Type *to, const jl_cgval_t &x, jl_value_t *jt, Value *dest, MDNode *tbaa_dest, bool isVolatile)
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
        //emit_unreachable(ctx);
        return UndefValue::get(to); // type mismatch error
    }

    Constant *c = x.constant ? julia_const_to_llvm(x.constant) : NULL;
    if (!x.ispointer() || c) { // already unboxed, but sometimes need conversion
        Value *unboxed = c ? c : x.V;
        if (!dest)
            return emit_unboxed_coercion(ctx, to, unboxed);
        Type *dest_ty = unboxed->getType()->getPointerTo();
        if (dest->getType() != dest_ty)
            dest = emit_bitcast(ctx, dest, dest_ty);
        tbaa_decorate(tbaa_dest, ctx.builder.CreateAlignedStore(unboxed, dest, julia_alignment(jt)));
        return NULL;
    }

    // bools stored as int8, so an extra Trunc is needed to get an int1
    Value *p = x.constant ? literal_pointer_val(ctx, x.constant) : x.V;

    if (jt == (jl_value_t*)jl_bool_type || to == T_int1) {
        Instruction *unbox_load = tbaa_decorate(x.tbaa, ctx.builder.CreateLoad(T_int8, maybe_bitcast(ctx, p, T_pint8)));
        if (jt == (jl_value_t*)jl_bool_type)
            unbox_load->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
                ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
                ConstantAsMetadata::get(ConstantInt::get(T_int8, 2)) }));
        Value *unboxed;
        if (to == T_int1)
            unboxed = ctx.builder.CreateTrunc(unbox_load, T_int1);
        else
            unboxed = unbox_load; // `to` must be T_int8
        if (!dest)
            return unboxed;
        Type *dest_ty = unboxed->getType()->getPointerTo();
        if (dest->getType() != dest_ty)
            dest = emit_bitcast(ctx, dest, dest_ty);
        tbaa_decorate(tbaa_dest, ctx.builder.CreateStore(unboxed, dest));
        return NULL;
    }

    unsigned alignment = julia_alignment(jt);
    Type *ptype = to->getPointerTo();
    if (dest) {
        emit_memcpy(ctx, dest, tbaa_dest, p, x.tbaa, jl_datatype_size(jt), alignment, false);
        return NULL;
    }
    else {
        if (p->getType() != ptype && isa<AllocaInst>(p)) {
            // LLVM's mem2reg can't handle coercion if the load/store type does
            // not match the type of the alloca. As such, it is better to
            // perform the load using the alloca's type and then perform the
            // appropriate coercion manually.
            AllocaInst *AI = cast<AllocaInst>(p);
            Type *AllocType = AI->getAllocatedType();
            const DataLayout &DL = jl_data_layout;
            if (!AI->isArrayAllocation() &&
                    (AllocType->isFloatingPointTy() || AllocType->isIntegerTy() || AllocType->isPointerTy()) &&
                    (to->isFloatingPointTy() || to->isIntegerTy() || to->isPointerTy()) &&
                    DL.getTypeSizeInBits(AllocType) == DL.getTypeSizeInBits(to)) {
                Instruction *load = ctx.builder.CreateAlignedLoad(p, alignment);
                return emit_unboxed_coercion(ctx, to, tbaa_decorate(x.tbaa, load));
            }
        }
        p = maybe_bitcast(ctx, p, ptype);
        Instruction *load = ctx.builder.CreateAlignedLoad(p, alignment);
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
                Value *isprimitive = emit_datatype_isprimitivetype(ctx, typ);
                error_unless(ctx, isprimitive, "bitcast: expected primitive type value for second argument");
            }
            else {
                emit_error(ctx, "bitcast: expected primitive type value for second argument");
                return jl_cgval_t();
            }
        }
        if (!jl_is_datatype(v.typ) || jl_datatype_size(v.typ) != nb) {
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
                    emit_bitcast(ctx, data_pointer(ctx, v),
                        vxt == T_int1 ? T_pint8 : vxt->getPointerTo())));
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

    if (jl_is_concrete_type(bt)) {
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
        intrinsic f, Instruction::CastOps Op,
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
    if (!CastInst::castIsValid(Op, from, to))
        return emit_runtime_call(ctx, f, argv, 2);
    if (Op == Instruction::FPExt) {
#ifdef JL_NEED_FLOATTEMP_VAR
        // Target platform might carry extra precision.
        // Force rounding to single precision first. The reason is that it's
        // fine to keep working in extended precision as long as it's
        // understood that everything is implicitly rounded to 23 bits,
        // but if we start looking at more bits we need to actually do the
        // rounding first instead of carrying around incorrect low bits.
        Value *jlfloattemp_var = emit_static_alloca(ctx, from->getType());
        ctx.builder.CreateStore(from, jlfloattemp_var);
        from  = ctx.builder.CreateLoad(jlfloattemp_var, /*force this to load from the stack*/true);
#endif
    }
    Value *ans = ctx.builder.CreateCast(Op, from, to);
    return mark_julia_type(ctx, ans, false, jlto);
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

    if (ety == (jl_value_t*)jl_any_type) {
        Value *thePtr = emit_unbox(ctx, T_pprjlvalue, e, e.typ);
        return mark_julia_type(
                ctx,
                ctx.builder.CreateAlignedLoad(ctx.builder.CreateGEP(T_prjlvalue, thePtr, im1), align_nb),
                true,
                ety);
    }
    else if (!jl_isbits(ety)) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_concrete_type(ety)) {
            emit_error(ctx, "pointerref: invalid pointer type");
            return jl_cgval_t();
        }
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(ctx, size,
                                     literal_pointer_val(ctx, ety));
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        Value *thePtr = emit_unbox(ctx, T_pint8, e, e.typ);
        thePtr = ctx.builder.CreateGEP(T_int8, emit_bitcast(ctx, thePtr, T_pint8), im1);
        MDNode *tbaa = best_tbaa(ety);
        emit_memcpy(ctx, strct, tbaa, thePtr, nullptr, size, 1);
        return mark_julia_type(ctx, strct, true, ety);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ety, &isboxed);
        assert(!isboxed);
        if (!type_is_ghost(ptrty)) {
            Value *thePtr = emit_unbox(ctx, ptrty->getPointerTo(), e, e.typ);
            return typed_load(ctx, thePtr, im1, ety, tbaa_data, nullptr, true, align_nb);
        }
        else {
            return ghostValue(ety);
        }
    }
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
    emit_typecheck(ctx, x, ety, "pointerset");

    Value *idx = emit_unbox(ctx, T_size, i, (jl_value_t*)jl_long_type);
    Value *im1 = ctx.builder.CreateSub(idx, ConstantInt::get(T_size, 1));

    Value *thePtr;
    if (ety == (jl_value_t*)jl_any_type) {
        // unsafe_store to Ptr{Any} is allowed to implicitly drop GC roots.
        thePtr = emit_unbox(ctx, T_psize, e, e.typ);
        Instruction *store = ctx.builder.CreateAlignedStore(
          ctx.builder.CreatePtrToInt(emit_pointer_from_objref(ctx, boxed(ctx, x)), T_size),
            ctx.builder.CreateGEP(T_size, thePtr, im1), align_nb);
        tbaa_decorate(tbaa_data, store);
    }
    else if (!jl_isbits(ety)) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_concrete_type(ety)) {
            emit_error(ctx, "pointerset: invalid pointer type");
            return jl_cgval_t();
        }
        thePtr = emit_unbox(ctx, T_pint8, e, e.typ);
        uint64_t size = jl_datatype_size(ety);
        im1 = ctx.builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, jl_datatype_align(ety))));
        emit_memcpy(ctx, ctx.builder.CreateGEP(T_int8, thePtr, im1), nullptr, x, size, align_nb);
    }
    else {
        bool isboxed;
        Type *ptrty = julia_type_to_llvm(ety, &isboxed);
        assert(!isboxed);
        if (!type_is_ghost(ptrty)) {
            thePtr = emit_unbox(ctx, ptrty->getPointerTo(), e, e.typ);
            typed_store(ctx, thePtr, im1, x, ety, tbaa_data, nullptr, nullptr, align_nb);
        }
    }
    return e;
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

static Value *emit_untyped_intrinsic(jl_codectx_t &ctx, intrinsic f, Value **argvalues, size_t nargs,
                                     jl_datatype_t **newtyp, jl_value_t *xtyp);


static jl_cgval_t emit_ifelse(jl_codectx_t &ctx, jl_cgval_t c, jl_cgval_t x, jl_cgval_t y, jl_value_t *rt_hint)
{
    Value *isfalse = emit_condition(ctx, c, "ifelse");
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
    Type *llt1 = isboxed ? T_prjlvalue : julia_type_to_llvm(t1);
    if (!isboxed) {
        if (type_is_ghost(llt1))
            return x;
        ifelse_result = ctx.builder.CreateSelect(isfalse,
                emit_unbox(ctx, llt1, y, t1),
                emit_unbox(ctx, llt1, x, t1));
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
                ifelse_tbaa = tbaa_stack;
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
                x_ptr = decay_derived(x_ptr);
                y_ptr = decay_derived(y_ptr);
                if (x_ptr->getType() != y_ptr->getType())
                    y_ptr = ctx.builder.CreateBitCast(y_ptr, x_ptr->getType());
                ifelse_result = ctx.builder.CreateSelect(isfalse, y_ptr, x_ptr);
                ifelse_tbaa = MDNode::getMostGenericTBAA(x.tbaa, y.tbaa);
                if (ifelse_tbaa == NULL) {
                    // LLVM won't return a TBAA result for the root, but mark_julia_struct requires it: make it now
                    auto *OffsetNode = ConstantAsMetadata::get(ConstantInt::get(T_int64, 0));
                    Metadata *Ops[] = {tbaa_root, tbaa_root, OffsetNode};
                    ifelse_tbaa = MDNode::get(jl_LLVMContext, Ops);
                }
            }
            Value *tindex;
            if (!x_tindex && x.constant) {
                x_tindex = ConstantInt::get(T_int8, 0x80 | get_box_tindex((jl_datatype_t*)jl_typeof(x.constant), rt_hint));
            }
            if (!y_tindex && y.constant) {
                y_tindex = ConstantInt::get(T_int8, 0x80 | get_box_tindex((jl_datatype_t*)jl_typeof(y.constant), rt_hint));
            }
            if (x_tindex && y_tindex) {
                tindex = ctx.builder.CreateSelect(isfalse, y_tindex, x_tindex);
            }
            else {
                PHINode *ret = PHINode::Create(T_int8, 2);
                BasicBlock *post = BasicBlock::Create(jl_LLVMContext, "post", ctx.f);
                BasicBlock *compute = BasicBlock::Create(jl_LLVMContext, "compute_tindex", ctx.f);
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
                tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
                ret->addIncoming(tindex, compute);
                ctx.builder.CreateBr(post);
                ctx.builder.SetInsertPoint(post);
                ctx.builder.Insert(ret);
                tindex = ret;
            }
            jl_cgval_t ret = mark_julia_slot(ifelse_result, rt_hint, tindex, ifelse_tbaa);
            if (x_vboxed || y_vboxed) {
                if (!x_vboxed)
                    x_vboxed = ConstantPointerNull::get(cast<PointerType>(y_vboxed->getType()));
                if (!y_vboxed)
                    y_vboxed = ConstantPointerNull::get(cast<PointerType>(x_vboxed->getType()));
                ret.Vboxed = ctx.builder.CreateSelect(isfalse, y_vboxed, x_vboxed);
            }
            return ret;
        }
        ifelse_result = ctx.builder.CreateSelect(isfalse,
                boxed(ctx, y),
                boxed(ctx, x));
    }
    jl_value_t *jt = (t1 == t2 ? t1 : rt_hint);
    return mark_julia_type(ctx, ifelse_result, isboxed, jt);
}

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
        return mark_julia_type(ctx, emit_arraylen(ctx, argv[0]), false, jl_long_type);
    case pointerref:
        return emit_pointerref(ctx, argv);
    case pointerset:
        return emit_pointerset(ctx, argv);
    case bitcast:
        return generic_bitcast(ctx, argv);
    case trunc_int:
        return generic_cast(ctx, f, Instruction::Trunc, argv, true, true);
    case sext_int:
        return generic_cast(ctx, f, Instruction::SExt, argv, true, true);
    case zext_int:
        return generic_cast(ctx, f, Instruction::ZExt, argv, true, true);
    case uitofp:
        return generic_cast(ctx, f, Instruction::UIToFP, argv, false, true);
    case sitofp:
        return generic_cast(ctx, f, Instruction::SIToFP, argv, false, true);
    case fptoui:
        return generic_cast(ctx, f, Instruction::FPToUI, argv, true, false);
    case fptosi:
        return generic_cast(ctx, f, Instruction::FPToSI, argv, true, false);
    case fptrunc:
        return generic_cast(ctx, f, Instruction::FPTrunc, argv, false, false);
    case fpext:
        return generic_cast(ctx, f, Instruction::FPExt, argv, false, false);

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
        ////Bool are required to be in the range [0,1]
        ////so while they are represented as i8,
        ////the operations need to be done in mod 1
        ////we can either do that now, or truncate them
        ////later into mod 1.
        ////LLVM seems to emit better code if we do the latter,
        ////(more likely to fold away the cast) so that's what we'll do.
        //if (xtyp == (jl_value_t*)jl_bool_type)
        //    r = T_int1;

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
        jl_value_t *newtyp = xinfo.typ;
        Value *r = emit_untyped_intrinsic(ctx, f, argvalues, nargs, (jl_datatype_t**)&newtyp, xinfo.typ);
        // Turn Bool operations into mod 1 now, if needed
        if (newtyp == (jl_value_t*)jl_bool_type && r->getType() != T_int1)
            r = ctx.builder.CreateTrunc(r, T_int1);
        return mark_julia_type(ctx, r, false, newtyp);
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

    // LLVM will not fold ptrtoint+arithmetic+inttoptr to GEP. The reason for this
    // has to do with alias analysis. When adding two integers, either one of them
    // could be the pointer base. With getelementptr, it is clear which of the
    // operands is the pointer base. We also have this information at the julia
    // level. Thus, to not lose information, we need to have a separate intrinsic
    // for pointer arithmetic which lowers to getelementptr.
    case add_ptr: {
        return ctx.builder.CreatePtrToInt(
            ctx.builder.CreateGEP(T_int8,
                ctx.builder.CreateIntToPtr(x, T_pint8), y), t);

    }

    case sub_ptr: {
        return ctx.builder.CreatePtrToInt(
            ctx.builder.CreateGEP(T_int8,
                ctx.builder.CreateIntToPtr(x, T_pint8), ctx.builder.CreateNeg(y)), t);

    }

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
        // LLVM 5.0 can create FMA in the backend for contractable fmul and fadd
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
    case sqrt_llvm_fast: {
        Value *sqrtintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt, makeArrayRef(t));
        return math_builder(ctx, true)().CreateCall(sqrtintr, x);
    }

    default:
        assert(0 && "invalid intrinsic");
        abort();
    }
    assert(0 && "unreachable");
}

#define BOX_F(ct,jl_ct,rt)                                      \
    box_##ct##_func = boxfunc_llvm(ft1arg(rt, T_##jl_ct),       \
                                   "jl_box_"#ct, &jl_box_##ct, m);

#define SBOX_F(ct,jl_ct) BOX_F(ct,jl_ct,T_prjlvalue); box_##ct##_func->addAttribute(1, Attribute::SExt);
#define UBOX_F(ct,jl_ct) BOX_F(ct,jl_ct,T_prjlvalue); box_##ct##_func->addAttribute(1, Attribute::ZExt);
#define SBOX_F_PERM(ct,jl_ct) BOX_F(ct,jl_ct,T_pjlvalue); box_##ct##_func->addAttribute(1, Attribute::SExt);
#define UBOX_F_PERM(ct,jl_ct) BOX_F(ct,jl_ct,T_pjlvalue); box_##ct##_func->addAttribute(1, Attribute::ZExt);

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
    return FunctionType::get(ret, { arg }, false);
}
