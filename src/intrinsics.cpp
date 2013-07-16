namespace JL_I {
    enum intrinsic {
        // wrap and unwrap
        box=0, unbox,
        // arithmetic
        neg_int, add_int, sub_int, mul_int,
        sdiv_int, udiv_int, srem_int, urem_int, smod_int,
        neg_float, add_float, sub_float, mul_float, div_float, rem_float,
        // same-type comparisons
        eq_int,  ne_int,
        slt_int, ult_int,
        sle_int, ule_int,
        eq_float, ne_float,
        lt_float, le_float,
        fpiseq, fpislt,
        // mixed-type comparisons
        eqfsi64, eqfui64,
        ltfsi64, ltfui64,
        lefsi64, lefui64,
        ltsif64, ltuif64,
        lesif64, leuif64,
        // bitwise operators
        and_int, or_int, xor_int, not_int, shl_int, lshr_int, ashr_int,
        bswap_int, ctpop_int, ctlz_int, cttz_int,
        // conversion
        sext_int, zext_int, trunc_int,
        fptoui, fptosi, uitofp, sitofp,
        fptrunc, fpext,
        // checked conversion
        fpsiround, fpuiround, checked_fptoui, checked_fptosi,
        // checked arithmetic
        checked_sadd, checked_uadd, checked_ssub, checked_usub,
        checked_smul, checked_umul,
        nan_dom_err,
        // functions
        abs_float, copysign_float, flipsign_int,
        // pointer access
        pointerref, pointerset, pointertoref,
        // c interface
        ccall, cglobal, jl_alloca
    };
};

using namespace JL_I;

#include "ccall.cpp"

/*
  low-level intrinsics design:
  functions like add_int expect unboxed values of matching bit-length.
  every operation that can return an unboxed value does so.
  this maximizes opportunities for composing functions without
    unnecessary boxing.
  this means that box and unbox functions might do nothing except change
    the type tag of a value.
  boxing is delayed until absolutely necessary, and handled at the point
    where the box is needed.
*/

static Type *FTnbits(size_t nb)
{
#ifndef DISABLE_FLOAT16
    if(nb == 16)
        return Type::getHalfTy(jl_LLVMContext);
    else
#endif
    if(nb == 32)
        return Type::getFloatTy(jl_LLVMContext);
    else if(nb == 64)
        return Type::getDoubleTy(jl_LLVMContext);
    else if(nb == 128)
        return Type::getFP128Ty(jl_LLVMContext);
    else 
        jl_error("Unsupported Float Size");
}
// convert int type to same-size float type
static Type *FT(Type *t)
{
    if (t->isFloatingPointTy())
        return t;
    return FTnbits(t->getPrimitiveSizeInBits());
}

// reinterpret-cast to float
static Value *FP(Value *v)
{
    if (v->getType()->isFloatingPointTy())
        return v;
    return builder.CreateBitCast(v, FT(v->getType()));
}

// convert float type to same-size int type
static Type *JL_INTT(Type *t)
{
    if (t->isIntegerTy())
        return t;
    if (t->isPointerTy())
        return T_size;
    if (t == T_float32) return T_int32;
    assert(t == T_float64);
    return T_int64;
}

// reinterpret-cast to int
static Value *JL_INT(Value *v)
{
    Type *t = v->getType();
    if (t->isIntegerTy())
        return v;
    if (t->isPointerTy())
        return builder.CreatePtrToInt(v, JL_INTT(t));
    return builder.CreateBitCast(v, JL_INTT(t));
}

static Value *uint_cnvt(Type *to, Value *x)
{
    Type *t = x->getType();
    if (t == to) return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return builder.CreateTrunc(x, to);
    return builder.CreateZExt(x, to);
}

static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx)
{
    if (e == jl_true) {
        return ConstantInt::get(T_int1, 1);
    }
    else if (e == jl_false) {
        return ConstantInt::get(T_int1, 0);
    }
    else if (jl_is_bitstype(jl_typeof(e))) {
        jl_datatype_t *bt = (jl_datatype_t*)jl_typeof(e);
        int nb = jl_datatype_size(bt);
        //APInt copies the data (but only as much as needed, so it doesn't matter if ArrayRef extends too far)
        APInt val = APInt(8*nb,ArrayRef<uint64_t>((uint64_t*)jl_data_ptr(e),(nb+7)/8));
        if (jl_is_float(e)) {
#ifdef LLVM33
            #define LLVM_FP(a,b) APFloat(a,b)
#else
            #define LLVM_FP(a,b) APFloat(b,true)
#endif
#ifndef DISABLE_FLOAT16
            if (nb == 2)
                return mark_julia_type(ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEhalf,val)),(jl_value_t*)bt);
            else
#endif
            if (nb == 4)
                return mark_julia_type(ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEsingle,val)),(jl_value_t*)bt);
            else if (nb == 8)
                return mark_julia_type(ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEdouble,val)),(jl_value_t*)bt);
            else if (nb == 16)
                return mark_julia_type(ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEquad,val)),(jl_value_t*)bt);
            // If we have a floating point type that's not hardware supported, just treat it like an integer for LLVM purposes
        }
        return mark_julia_type(ConstantInt::get(IntegerType::get(jl_LLVMContext,8*nb),val),(jl_value_t*)bt);
    }
    return emit_expr(e, ctx, false);
}

// emit code to unpack a raw value from a box
static Value *emit_unbox(Type *to, Type *pto, Value *x)
{
    Type *ty = x->getType();
    if (ty != jl_pvalue_llvmt) {
        // bools are stored internally as int8 (for now)
        if (ty == T_int1 && to == T_int8)
            return builder.CreateZExt(x, T_int8);
        if (ty->isPointerTy() && !to->isPointerTy())
            return builder.CreatePtrToInt(x, to);
        if (!ty->isPointerTy() && to->isPointerTy())
            return builder.CreateIntToPtr(x, to);
        if (ty != to)
            jl_error("unbox: T != typeof(x)");
        return x;
    }
    Value *p = data_pointer(x);
    if (to == T_int1) {
        // bools stored as int8, so an extra Trunc is needed to get an int1
        return builder.CreateTrunc(builder.
                                   CreateLoad(builder.
                                              CreateBitCast(p, T_pint8), false),
                                   T_int1);
    }
    if (to->isStructTy() && !to->isSized()) {
        // empty struct - TODO - is this a good way to represent it?
        return UndefValue::get(to);
    }
    return builder.CreateLoad(builder.CreateBitCast(p, pto), false);
}

// unbox trying to determine type automatically
static Value *auto_unbox(jl_value_t *x, jl_codectx_t *ctx)
{
    Value *v = emit_unboxed(x, ctx);
    if (v->getType() != jl_pvalue_llvmt) {
        return v;
    }
    jl_value_t *bt = expr_type(x, ctx);
    if (!jl_is_bitstype(bt)) {
        if (jl_is_symbol(x)) {
            std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find((jl_sym_t*)x);
            if (it != ctx->vars.end())
                bt = (*it).second.declType;
        }
        if (bt == NULL || !jl_is_bitstype(bt)) {
            // TODO: make sure this code is valid; hopefully it is
            // unreachable but it should still be well-formed.
            return emit_error("auto_unbox: unable to determine argument type", ctx);
        }
    }
    Type *to = julia_type_to_llvm(bt);
    if (to == NULL || to == jl_pvalue_llvmt) {
        unsigned int nb = jl_datatype_size(bt)*8;
        to = IntegerType::get(jl_LLVMContext, nb);
    }
    return emit_unbox(to, PointerType::get(to, 0), v);
}

// figure out how many bits a bitstype has at compile time, or -1
static int try_to_determine_bitstype_nbits(jl_value_t *targ, jl_codectx_t *ctx)
{
    jl_value_t *et = expr_type(targ, ctx);
    if (jl_is_type_type(et)) {
        jl_value_t *p = jl_tparam0(et);
        if (p == (jl_value_t*)jl_bool_type)
            return 1;
        if (jl_is_bitstype(p))
            return jl_datatype_size(p)*8;
        if (jl_is_typevar(p)) {
            jl_value_t *ub = ((jl_tvar_t*)p)->ub;
            if (jl_is_bitstype(ub))
                return jl_datatype_size(ub)*8;
        }
    }
    return -1;
}

// unbox using user-specified type
static Value *generic_unbox(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *et = expr_type(targ, ctx);
    if (jl_is_type_type(et)) {
        jl_value_t *p = jl_tparam0(et);
        if (jl_is_leaf_type(p)) {
            Type *to = julia_type_to_llvm(p);
            return emit_unbox(to, PointerType::get(to,0), emit_unboxed(x,ctx));
        }
    }
    int nb = try_to_determine_bitstype_nbits(targ, ctx);
    if (nb == -1) {
        jl_value_t *bt=NULL;
        JL_TRY {
            bt = jl_interpret_toplevel_expr_in(ctx->module, targ,
                                               &jl_tupleref(ctx->sp,0),
                                               jl_tuple_len(ctx->sp)/2);
        }
        JL_CATCH {
        }
        if (bt == NULL || !jl_is_bitstype(bt))
            jl_error("unbox: could not determine argument size");
        nb = (bt==(jl_value_t*)jl_bool_type) ? 1 : jl_datatype_size(bt)*8;
    }
    Type *to = IntegerType::get(jl_LLVMContext, nb);
    return emit_unbox(to, PointerType::get(to, 0), emit_unboxed(x, ctx));
}

static Value *generic_box(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    int nb = try_to_determine_bitstype_nbits(targ, ctx);

    Type *llvmt = NULL;
    jl_value_t *bt = NULL;
    jl_value_t *et = expr_type(targ, ctx);
    if (jl_is_type_type(et) && jl_is_leaf_type(jl_tparam0(et)) &&
        jl_is_bitstype(jl_tparam0(et))) {
        bt = jl_tparam0(et);
    }
    else {
        JL_TRY {
            bt = jl_interpret_toplevel_expr_in(ctx->module, targ,
                                               &jl_tupleref(ctx->sp,0),
                                               jl_tuple_len(ctx->sp)/2);
        }
        JL_CATCH {
        }
    }

    if (bt == NULL) {
    }
    else if (!jl_is_bitstype(bt)) {
        jl_error("box: expected bits type as first argument");
    }
    else {
        llvmt = julia_type_to_llvm(bt);
        if (nb == -1)
            nb = (bt==(jl_value_t*)jl_bool_type) ? 1 : jl_datatype_size(bt)*8;
    }

    if (nb == -1)
        jl_error("box: could not determine argument size");

    if (llvmt == NULL)
        llvmt = IntegerType::get(jl_LLVMContext, nb);

    Value *vx = auto_unbox(x, ctx);
    //if (vx->getType()->getPrimitiveSizeInBits() != (unsigned)nb)
    //    jl_errorf("box: expected argument with %d bits, got %d", nb,
    //              vx->getType()->getPrimitiveSizeInBits());

    if (vx->getType() != llvmt) {
        if (vx->getType()->isPointerTy() && !llvmt->isPointerTy()) {
            vx = builder.CreatePtrToInt(vx, llvmt);
        }
        else if (!vx->getType()->isPointerTy() && llvmt->isPointerTy()) {
            vx = builder.CreateIntToPtr(vx, llvmt);
        }
        else {
            if (llvmt == T_int1) {
                vx = builder.CreateTrunc(vx, llvmt);
            }
            else {
                if (vx->getType()->getPrimitiveSizeInBits() != llvmt->getPrimitiveSizeInBits()) {
                    jl_error("box: argument is of incorrect size");
                }
                vx = builder.CreateBitCast(vx, llvmt);
            }
        }
    }

    if (bt != NULL) {
        return mark_julia_type(vx, bt);
    }

    // dynamically-determined type; evaluate.
    return allocate_box_dynamic(emit_expr(targ, ctx), (nb+7)/8, vx);
}

static Type *staticeval_bitstype(jl_value_t *targ, const char *fname, jl_codectx_t *ctx)
{
    jl_value_t *bt =
        jl_interpret_toplevel_expr_in(ctx->module, targ,
                                      &jl_tupleref(ctx->sp,0),
                                      jl_tuple_len(ctx->sp)/2);
    if (!jl_is_bitstype(bt))
        jl_errorf("%s: expected bits type as first argument", fname);
    Type *to = julia_type_to_llvm(bt);
    if (to == NULL) {
        unsigned int nb = jl_datatype_size(bt)*8;
        to = IntegerType::get(jl_LLVMContext, nb);
    }
    return to;
}

static Value *generic_trunc(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    Type *to = staticeval_bitstype(targ, "trunc_int", ctx);
    return builder.CreateTrunc(JL_INT(auto_unbox(x,ctx)), to);
}

static Value *generic_sext(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    Type *to = staticeval_bitstype(targ, "sext_int", ctx);
    return builder.CreateSExt(JL_INT(auto_unbox(x,ctx)), to);
}

static Value *generic_zext(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    Type *to = staticeval_bitstype(targ, "zext_int", ctx);
    return builder.CreateZExt(JL_INT(auto_unbox(x,ctx)), to);
}

static Value *emit_eqfsi64(Value *x, Value *y)
{
    x = FP(x);
    Value *fy = JL_INT(y);
    return builder.CreateAnd
        (builder.CreateFCmpOEQ(x, builder.CreateSIToFP(fy, T_float64)),
         builder.CreateICmpEQ(fy, builder.CreateFPToSI
                              (builder.CreateSIToFP(fy, T_float64),
                               T_int64)));
}

static Value *emit_eqfui64(Value *x, Value *y)
{
    x = FP(x);
    Value *fy = JL_INT(y);
    return builder.CreateAnd
        (builder.CreateFCmpOEQ(x, builder.CreateUIToFP(fy, T_float64)),
         builder.CreateICmpEQ(fy, builder.CreateFPToUI
                              (builder.CreateUIToFP(fy, T_float64),
                               T_int64)));
}

static Value *emit_checked_fptosi(Type *to, Value *x, jl_codectx_t *ctx)
{
    x = FP(x);
    Value *v = builder.CreateFPToSI(x, to);
    if (x->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(x, T_float64),
                                   builder.CreateSIToFP(v, T_float64)),
             jlinexacterr_var, ctx);
    }
    else {
        Value *xx = x, *vv = v;
        if (x->getType() == T_float32)
            xx = builder.CreateFPExt(x, T_float64);
        if (to->getPrimitiveSizeInBits() < 64)
            vv = builder.CreateSExt(v, T_int64);
        raise_exception_unless(emit_eqfsi64(xx, vv), jlinexacterr_var, ctx);
    }
    return v;
}

static Value *emit_checked_fptosi(jl_value_t *targ, Value *x, jl_codectx_t *ctx)
{
    return emit_checked_fptosi(staticeval_bitstype(targ, "checked_fptosi", ctx), x, ctx);
}

static Value *emit_checked_fptoui(Type *to, Value *x, jl_codectx_t *ctx)
{
    x = FP(x);
    Value *v = builder.CreateFPToUI(x, to);
    if (x->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(x, T_float64),
                                   builder.CreateUIToFP(v, T_float64)),
             jlinexacterr_var, ctx);
    }
    else {
        Value *xx = x, *vv = v;
        if (x->getType() == T_float32)
            xx = builder.CreateFPExt(x, T_float64);
        if (to->getPrimitiveSizeInBits() < 64)
            vv = builder.CreateZExt(v, T_int64);
        raise_exception_unless(emit_eqfui64(xx, vv), jlinexacterr_var, ctx);
    }
    return v;
}

static Value *emit_checked_fptoui(jl_value_t *targ, Value *x, jl_codectx_t *ctx)
{
    return emit_checked_fptoui(staticeval_bitstype(targ, "checked_fptoui", ctx), x, ctx);
}

static Value *emit_iround(Value *x, bool issigned, jl_codectx_t *ctx)
{
    int nmantissa, expoffs, expbits;
    int64_t topbit;
    Type *intt, *floatt;
    Value *bits = JL_INT(x);
    Value *max, *min;

    if (bits->getType()->getPrimitiveSizeInBits() == 32) {
        nmantissa = 23;
        expoffs = 127;
        expbits = 0xff;
        topbit = BIT31;
        intt = T_int32; floatt = T_float32;
        if (issigned) {
            max = ConstantFP::get(floatt,  2.1474835e9);
            min = ConstantFP::get(floatt, -2.1474836e9);
        }
        else {
            max = ConstantFP::get(floatt, 4.294967e9);
            // most negative number that truncates to zero
            min = ConstantFP::get(floatt, -0.99999994);
        }
    }
    else {
        nmantissa = 52;
        expoffs = 1023;
        expbits = 0x7ff;
        topbit = BIT63;
        intt = T_int64; floatt = T_float64;
        if (issigned) {
            max = ConstantFP::get(floatt,  9.223372036854775e18);
            min = ConstantFP::get(floatt, -9.223372036854776e18);
        }
        else {
            max = ConstantFP::get(floatt, 1.844674407370955e19);
            min = ConstantFP::get(floatt, -0.9999999999999999);
        }
    }

    // itrunc(x + copysign(0.5,x))
    // values with exponent >= nbits are already integers, and this
    // rounding method doesn't always give the right answer there.
    x = FP(x);
    Value *expo = builder.CreateAShr(bits, ConstantInt::get(intt,nmantissa));
    expo = builder.CreateAnd(expo, ConstantInt::get(intt,expbits));
    Value *isint = builder.CreateICmpSGE(expo,
                                         ConstantInt::get(intt,expoffs+nmantissa));
    Value *half = builder.CreateBitCast(ConstantFP::get(floatt, 0.5), intt);
    Value *signedhalf =
        builder.CreateOr(half,
                         builder.CreateAnd(bits,
                                           ConstantInt::get(intt,topbit)));
    Value *sum = builder.CreateFAdd(x,
                                    builder.CreateBitCast(signedhalf, floatt));

    Value *src = builder.
        CreateSelect(builder.
                     CreateOr(isint, builder.
                              // need to give 0 for -0.5 < x < 0.5 (exponent < -1)
                              // otherwise iround(prevfloat(0.5)) == 1
                              CreateICmpSLT(expo,
                                            ConstantInt::get(intt,expoffs-1))),
                     x, sum);

    raise_exception_unless(builder.CreateAnd(builder.CreateFCmpOLE(src, max),
                                             builder.CreateFCmpOGE(src, min)),
                           jlinexacterr_var, ctx);
    if (issigned)
        return builder.CreateFPToSI(src, intt);
    else
        return builder.CreateFPToUI(src, intt);
}

static Value *emit_pointerref(jl_value_t *e, jl_value_t *i, jl_codectx_t *ctx)
{
    jl_value_t *aty = expr_type(e, ctx);
    if (!jl_is_cpointer_type(aty))
        jl_error("pointerref: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        jl_error("pointerref: invalid pointer");
    if ((jl_datatype_t*)expr_type(i, ctx) != jl_long_type) {
        jl_error("pointerref: invalid index type");
    }
    Value *thePtr = auto_unbox(e,ctx);
    Value *idx = emit_unbox(T_size, T_psize, emit_unboxed(i, ctx));
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    if (!jl_isbits(ety)) {
        if (ety == (jl_value_t*)jl_any_type)
            return builder.CreateLoad(builder.CreateGEP(
                        builder.CreateBitCast(thePtr, jl_ppvalue_llvmt),
                        im1));
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            return emit_error("pointerref: invalid pointer type", ctx);
        }
        uint64_t size = ((jl_datatype_t*)ety)->size;
        Value *strct =
            builder.CreateCall(jlallocobj_func,
                               ConstantInt::get(T_size,
                                    sizeof(void*)+size));
        builder.CreateStore(literal_pointer_val((jl_value_t*)ety),
                            emit_nthptr_addr(strct, (size_t)0));
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size, size));
        thePtr = builder.CreateGEP(builder.CreateBitCast(thePtr, T_pint8), im1);
        builder.CreateMemCpy(builder.CreateBitCast(emit_nthptr_addr(strct, (size_t)1), T_pint8),
                            thePtr, size, 1);
        return mark_julia_type(strct, ety);
    }
    return typed_load(thePtr, im1, ety, ctx);
}

// e[i] = x
static Value *emit_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_codectx_t *ctx)
{
    jl_value_t *aty = expr_type(e, ctx);
    if (!jl_is_cpointer_type(aty))
        jl_error("pointerset: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        jl_error("pointerset: invalid pointer");
    jl_value_t *xty = expr_type(x, ctx);    
    if (!jl_subtype(xty, ety, 0)) {
        return emit_error("pointerset: type mismatch in assign", ctx);
    }
    if (!jl_isbits(ety)) {
        return emit_error("pointerset: invalid pointer type", ctx); //ety = (jl_value_t*)jl_any_type;
    }
    if ((jl_datatype_t*)expr_type(i, ctx) != jl_long_type) {
        jl_error("pointerset: invalid index type");
    }
    Value *idx = emit_unbox(T_size, T_psize, emit_unboxed(i, ctx));
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    Value *thePtr = auto_unbox(e,ctx);
    (void)typed_store(thePtr, im1, emit_unboxed(x,ctx), ety, ctx);
    return mark_julia_type(thePtr, aty);
}

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    switch (f) {
    case ccall: return emit_ccall(args, nargs, ctx);
    case cglobal: return emit_cglobal(args, nargs, ctx);

    HANDLE(box,2)         return generic_box(args[1], args[2], ctx);
    HANDLE(unbox,2)       return generic_unbox(args[1], args[2], ctx);
    HANDLE(trunc_int,2)   return generic_trunc(args[1], args[2], ctx);
    HANDLE(sext_int,2)    return generic_sext(args[1], args[2], ctx);
    HANDLE(zext_int,2)    return generic_zext(args[1], args[2], ctx);
    HANDLE(pointerref,2)  return emit_pointerref(args[1], args[2], ctx);
    HANDLE(pointerset,3)  return emit_pointerset(args[1], args[2], args[3], ctx);
    HANDLE(pointertoref,1) {
        Value *p = auto_unbox(args[1], ctx);
        if (p->getType()->isIntegerTy()) {
            return builder.CreateIntToPtr(p, jl_pvalue_llvmt);
        }
        return builder.CreateBitCast(p, jl_pvalue_llvmt);
    }
    HANDLE(checked_fptosi,2) {
        Value *x = FP(auto_unbox(args[2], ctx));
        return emit_checked_fptosi(args[1], x, ctx);
    }
    HANDLE(checked_fptoui,2) {
        Value *x = FP(auto_unbox(args[2], ctx));
        return emit_checked_fptoui(args[1], x, ctx);
    }
    HANDLE(uitofp,2) return builder.CreateUIToFP(JL_INT(auto_unbox(args[2],ctx)), FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    HANDLE(sitofp,2) return builder.CreateSIToFP(JL_INT(auto_unbox(args[2],ctx)), FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    HANDLE(fptrunc,2) return builder.CreateFPTrunc(FP(auto_unbox(args[2],ctx)), FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    HANDLE(fpext,2) {
        // when extending a float32 to a float64, we need to force
        // rounding to single precision first. the reason is that it's
        // fine to keep working in extended precision as long as it's
        // understood that everything is implicitly rounded to 23 bits,
        // but if we start looking at more bits we need to actually do the
        // rounding first instead of carrying around incorrect low bits.
        Value *x = auto_unbox(args[2],ctx);
        builder.CreateStore(FP(x), builder.CreateBitCast(jlfloattemp_var,FT(x->getType())->getPointerTo()), true);
        return builder.CreateFPExt(builder.CreateLoad(builder.CreateBitCast(jlfloattemp_var,FT(x->getType())->getPointerTo()), true),
                                   FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    }
    default: ;
    }

    if (nargs < 1) jl_error("invalid intrinsic call");
    Value *x = auto_unbox(args[1], ctx);
    Value *y = NULL;
    if (nargs>1) {
        y = auto_unbox(args[2], ctx);
    }
    Type *t = x->getType();
    Value *fy;
    Value *den;
    Value *typemin;
    switch (f) {
    HANDLE(neg_int,1) return builder.CreateSub(ConstantInt::get(t, 0), JL_INT(x));
    HANDLE(add_int,2) return builder.CreateAdd(JL_INT(x), JL_INT(y));
    HANDLE(sub_int,2) return builder.CreateSub(JL_INT(x), JL_INT(y));
    HANDLE(mul_int,2) return builder.CreateMul(JL_INT(x), JL_INT(y));
    HANDLE(sdiv_int,2)
        den = JL_INT(y);
        x = JL_INT(x);

        typemin = builder.CreateShl(ConstantInt::get(t,1),
                                    x->getType()->getPrimitiveSizeInBits()-1);
        raise_exception_unless(builder.
                               CreateAnd(builder.
                                         CreateICmpNE(den, ConstantInt::get(t,0)),
                                         builder.
                                         CreateOr(builder.
                                                  CreateICmpNE(den,
                                                               ConstantInt::get(t,-1,true)),
                                                  builder.CreateICmpNE(x, typemin))),
                               jldiverr_var, ctx);

        return builder.CreateSDiv(x, den);
    HANDLE(udiv_int,2)
        den = JL_INT(y);
        raise_exception_unless(builder.CreateICmpNE(den,
                                                    ConstantInt::get(t,0)),
                               jldiverr_var, ctx);
        return builder.CreateUDiv(JL_INT(x), den);

    HANDLE(srem_int,2) return builder.CreateSRem(JL_INT(x), JL_INT(y));
    HANDLE(urem_int,2) return builder.CreateURem(JL_INT(x), JL_INT(y));
    HANDLE(smod_int,2)
        x = JL_INT(x); y = JL_INT(y);
        fy = builder.CreateSRem(x,y);
        return builder.
            CreateSelect(builder.CreateICmpEQ(builder.CreateICmpSLT(x,ConstantInt::get(x->getType(),0)),
                                              builder.CreateICmpSLT(y,ConstantInt::get(y->getType(),0))),
                         // mod == rem for arguments with same sign
                         fy,
                         builder.CreateSRem(builder.CreateAdd(y,fy),y));

    HANDLE(neg_float,1) return builder.CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
    HANDLE(add_float,2) return builder.CreateFAdd(FP(x), FP(y));
    HANDLE(sub_float,2) return builder.CreateFSub(FP(x), FP(y));
    HANDLE(mul_float,2) return builder.CreateFMul(FP(x), FP(y));
    HANDLE(div_float,2) return builder.CreateFDiv(FP(x), FP(y));
    HANDLE(rem_float,2) return builder.CreateFRem(FP(x), FP(y));

    HANDLE(checked_sadd,2)
    HANDLE(checked_uadd,2)
    HANDLE(checked_ssub,2)
    HANDLE(checked_usub,2)
    HANDLE(checked_smul,2)
    HANDLE(checked_umul,2) {
        Value *ix = JL_INT(x); Value *iy = JL_INT(y);
        Type *atypes[2] = { ix->getType(), iy->getType() };
        Value *res = builder.CreateCall2
            (Intrinsic::getDeclaration(jl_Module,
                                       f==checked_sadd ?
                                       Intrinsic::sadd_with_overflow :
                                       (f==checked_uadd ?
                                        Intrinsic::uadd_with_overflow :
                                        (f==checked_ssub ?
                                         Intrinsic::ssub_with_overflow :
                                         (f==checked_usub ?
                                          Intrinsic::usub_with_overflow :
                                          (f==checked_smul ?
                                           Intrinsic::smul_with_overflow :
                                           Intrinsic::umul_with_overflow)))),
                                       ArrayRef<Type*>(atypes)),
             ix, iy);
        Value *obit = builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        raise_exception_if(obit, jlovferr_var, ctx);
        return builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
    }

    HANDLE(eq_int,2)  return builder.CreateICmpEQ(JL_INT(x), JL_INT(y));
    HANDLE(ne_int,2)  return builder.CreateICmpNE(JL_INT(x), JL_INT(y));
    HANDLE(slt_int,2) return builder.CreateICmpSLT(JL_INT(x), JL_INT(y));
    HANDLE(ult_int,2) return builder.CreateICmpULT(JL_INT(x), JL_INT(y));
    HANDLE(sle_int,2) return builder.CreateICmpSLE(JL_INT(x), JL_INT(y));
    HANDLE(ule_int,2) return builder.CreateICmpULE(JL_INT(x), JL_INT(y));

    HANDLE(eq_float,2) return builder.CreateFCmpOEQ(FP(x), FP(y));
    HANDLE(ne_float,2) return builder.CreateFCmpUNE(FP(x), FP(y));
    HANDLE(lt_float,2) return builder.CreateFCmpOLT(FP(x), FP(y));
    HANDLE(le_float,2) return builder.CreateFCmpOLE(FP(x), FP(y));

    HANDLE(eqfsi64,2) return emit_eqfsi64(x, y);
    HANDLE(eqfui64,2) return emit_eqfui64(x, y);
    HANDLE(ltfsi64,2) {
        x = FP(x);
        fy = JL_INT(y);
        Value *ffy = builder.CreateSIToFP(fy, T_float64);
        Value *siffy = builder.CreateFPToSI(ffy, T_int64);
        return builder.CreateOr(
            builder.CreateFCmpOLT(x, ffy),
            builder.CreateAnd(
                              // ensure x < 2.0^63
                builder.CreateFCmpOLT(x, ConstantFP::get(T_float64,
                                                         9.223372036854776e18)),
                builder.CreateAnd(builder.CreateFCmpOEQ(x, ffy),
                                  builder.CreateICmpSGT(fy, siffy))
            )
        );
    }
    HANDLE(ltfui64,2) {
        x = FP(x);
        fy = JL_INT(y);
        Value *ffy = builder.CreateUIToFP(fy, T_float64);
        Value *uiffy = builder.CreateFPToUI(ffy, T_int64);
        return builder.CreateOr(
            builder.CreateFCmpOLT(x, ffy),
            builder.CreateAnd(
                              // ensure x < 2.0^64
                builder.CreateFCmpOLT(x, ConstantFP::get(T_float64,
                                                         1.8446744073709552e19)),
                builder.CreateAnd(builder.CreateFCmpOEQ(x, ffy),
                                  builder.CreateICmpUGT(fy, uiffy))
            )
        );
    }
    HANDLE(lefsi64,2) {
        x = FP(x);
        fy = JL_INT(y);
        Value *ffy = builder.CreateSIToFP(fy, T_float64);
        return builder.CreateOr(
            builder.CreateFCmpOLT(x, ffy),
            builder.CreateAnd(
                builder.CreateFCmpOLT(x, ConstantFP::get(T_float64,
                                                         9.223372036854776e18)),
                builder.CreateAnd(
                    builder.CreateFCmpOEQ(x, ffy),
                    builder.CreateICmpSGE(fy, builder.CreateFPToSI(ffy,T_int64))
                )
            )
        );
    }
    HANDLE(lefui64,2) {
        x = FP(x);
        fy = JL_INT(y);
        Value *ffy = builder.CreateUIToFP(fy, T_float64);
        return builder.CreateOr(
            builder.CreateFCmpOLT(x, ffy),
            builder.CreateAnd(
                builder.CreateFCmpOLT(x, ConstantFP::get(T_float64,
                                                         1.8446744073709552e19)),
                builder.CreateAnd(
                    builder.CreateFCmpOEQ(x, ffy),
                    builder.CreateICmpUGE(fy, builder.CreateFPToUI(ffy, T_int64))
                )
            )
        );
    }
    HANDLE(ltsif64,2) {
        x = JL_INT(x);
        fy = FP(y);
        Value *fx = builder.CreateSIToFP(x, T_float64);
        return builder.CreateOr(
                   builder.CreateOr(
                       builder.CreateFCmpOGE(fy, ConstantFP::get(T_float64,
                                                                 9.223372036854776e18)),
                       builder.CreateFCmpOLT(fx, fy)),
                   builder.CreateAnd(
                       builder.CreateFCmpOEQ(fx, fy),
                       builder.CreateICmpSLT(x, builder.CreateFPToSI(fx, T_int64))
                   )
               );
    }
    HANDLE(ltuif64,2) {
        x = JL_INT(x);
        fy = FP(y);
        Value *fx = builder.CreateUIToFP(x, T_float64);
        return builder.CreateOr(
                   builder.CreateOr(
                       builder.CreateFCmpOGE(fy, ConstantFP::get(T_float64,
                                                                 1.8446744073709552e19)),
                       builder.CreateFCmpOLT(fx, fy)),
                   builder.CreateAnd(
                       builder.CreateFCmpOEQ(fx, fy),
                       builder.CreateICmpULT(x, builder.CreateFPToUI(fx, T_int64))
                   )
               );
    }
    HANDLE(lesif64,2) {
        x = JL_INT(x);
        fy = FP(y);
        Value *fx = builder.CreateSIToFP(x, T_float64);
        return builder.CreateOr(
                   builder.CreateOr(
                       builder.CreateFCmpOGE(fy, ConstantFP::get(T_float64,
                                                                 9.223372036854776e18)),
                       builder.CreateFCmpOLT(fx, fy)),
                   builder.CreateAnd(
                       builder.CreateFCmpOEQ(fx, fy),
                       builder.CreateICmpSLE(x, builder.CreateFPToSI(fx, T_int64))
                   )
               );
    }
    HANDLE(leuif64,2) {
        x = JL_INT(x);
        fy = FP(y);
        Value *fx = builder.CreateUIToFP(x, T_float64);
        return builder.CreateOr(
                   builder.CreateOr(
                       builder.CreateFCmpOGE(fy, ConstantFP::get(T_float64,
                                                                 1.8446744073709552e19)),
                       builder.CreateFCmpOLT(fx, fy)),
                   builder.CreateAnd(
                       builder.CreateFCmpOEQ(fx, fy),
                       builder.CreateICmpULE(x, builder.CreateFPToUI(fx, T_int64))
                   )
               );
    }

    HANDLE(fpiseq,2) {
        Value *xi = JL_INT(x);
        Value *yi = JL_INT(y);
        x = FP(x);
        fy = FP(y);
        return builder.CreateOr(builder.CreateAnd(builder.CreateFCmpUNO(x, x),
                                                  builder.CreateFCmpUNO(fy, fy)),
                                builder.CreateICmpEQ(xi, yi));
    }

    HANDLE(fpislt,2) {
        Value *xi = JL_INT(x);
        Value *yi = JL_INT(y);
        x = FP(x);
        fy = FP(y);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpORD(x, x),
                builder.CreateFCmpUNO(fy, fy)
            ),
            builder.CreateAnd(
                builder.CreateFCmpORD(x, fy),
                builder.CreateOr(
                    builder.CreateAnd(
                        builder.CreateICmpSGE(xi, ConstantInt::get(xi->getType(), 0)),
                        builder.CreateICmpSLT(xi, yi)
                    ),
                    builder.CreateAnd(
                        builder.CreateICmpSLT(xi, ConstantInt::get(xi->getType(), 0)),
                        builder.CreateICmpUGT(xi, yi)
                    )
                )
            )
        );
    }

    HANDLE(and_int,2) return builder.CreateAnd(JL_INT(x), JL_INT(y));
    HANDLE(or_int,2)  return builder.CreateOr(JL_INT(x), JL_INT(y));
    HANDLE(xor_int,2) return builder.CreateXor(JL_INT(x), JL_INT(y));
    HANDLE(not_int,1) return builder.CreateXor(JL_INT(x), ConstantInt::get(t, -1, true));
    HANDLE(shl_int,2)
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateShl(x, uint_cnvt(t,y)));
    HANDLE(lshr_int,2)
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateLShr(x, uint_cnvt(t,y)));
    HANDLE(ashr_int,2)
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         builder.CreateAShr(x, ConstantInt::get(x->getType(),
                                                                x->getType()->getPrimitiveSizeInBits()-1)),
                         builder.CreateAShr(x, uint_cnvt(t,y)));
    HANDLE(bswap_int,1)
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap,
                                      ArrayRef<Type*>(x->getType())), x);
    HANDLE(ctpop_int,1)
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop,
                                      ArrayRef<Type*>(x->getType())), x);
#if !defined(LLVM_VERSION_MAJOR) || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 0)
    HANDLE(ctlz_int,1)
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz,
                                      ArrayRef<Type*>(x->getType())), x);
    HANDLE(cttz_int,1)
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz,
                                      ArrayRef<Type*>(x->getType())), x);
#elif LLVM_VERSION_MAJOR==3 && LLVM_VERSION_MINOR >= 1
    HANDLE(ctlz_int,1) {
        x = JL_INT(x);
        Type *types[1] = {x->getType()};
        return builder.CreateCall2(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz,
                                      ArrayRef<Type*>(types)), x, ConstantInt::get(T_int1,0));
    }
    HANDLE(cttz_int,1) {
        x = JL_INT(x);
        Type *types[1] = {x->getType()};
        return builder.CreateCall2(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, ArrayRef<Type*>(types)), x, ConstantInt::get(T_int1, 0));
    }
#endif

    HANDLE(fptoui,1) return builder.CreateFPToUI(FP(x), JL_INTT(x->getType()));
    HANDLE(fptosi,1) return builder.CreateFPToSI(FP(x), JL_INTT(x->getType()));
    HANDLE(fpsiround,1)
    HANDLE(fpuiround,1)
    {
        return emit_iround(x, f == fpsiround, ctx);
    }
    HANDLE(nan_dom_err,2) {
        // nan_dom_err(f, x) throw DomainError if isnan(f)&&!isnan(x)
        Value *f = FP(x); x = FP(y);
        raise_exception_unless(builder.CreateOr(builder.CreateFCmpORD(f,f),
                                                builder.CreateFCmpUNO(x,x)),
                               jldomerr_var, ctx);
        return f;
    }

    HANDLE(abs_float,1)
    {
        x = FP(x);
        Type *intt = JL_INTT(x->getType());
        Value *bits = builder.CreateBitCast(FP(x), intt);
        Value *absbits =
            builder.CreateAnd(bits,
                              ConstantInt::get(intt, APInt::getSignedMaxValue(((IntegerType*)intt)->getBitWidth())));
        return builder.CreateBitCast(absbits, x->getType());
    }
    HANDLE(copysign_float,2)
    {
        x = FP(x);
        fy = FP(y);
        Type *intt = JL_INTT(x->getType());
        Value *bits = builder.CreateBitCast(x, intt);
        Value *sbits = builder.CreateBitCast(fy, intt);
        unsigned nb = ((IntegerType*)intt)->getBitWidth();
        APInt notsignbit = APInt::getSignedMaxValue(nb);
        APInt signbit0(nb, 0); signbit0.setBit(nb-1);
        Value *rbits =
            builder.CreateOr(builder.CreateAnd(bits,
                                               ConstantInt::get(intt,
                                                                notsignbit)),
                             builder.CreateAnd(sbits,
                                               ConstantInt::get(intt,
                                                                signbit0)));
        return builder.CreateBitCast(rbits, x->getType());
    }
    HANDLE(flipsign_int,2)
    {
        x = JL_INT(x);
        fy = JL_INT(y);
        Type *intt = x->getType();
        ConstantInt *cx = dyn_cast<ConstantInt>(x);
        ConstantInt *cy = dyn_cast<ConstantInt>(fy);
        if (cx && cy) {
            APInt ix = cx->getValue();
            APInt iy = cy->getValue();
            return ConstantInt::get(intt, iy.isNonNegative() ? ix : -ix);
        }
        if (cy) {
            APInt iy = cy->getValue();
            return iy.isNonNegative() ? x : builder.CreateSub(ConstantInt::get(intt,0), x);
        }
        Value *tmp = builder.CreateAShr(fy, ConstantInt::get(intt,((IntegerType*)intt)->getBitWidth()-1));
        return builder.CreateXor(builder.CreateAdd(x,tmp),tmp);
    }
    HANDLE(jl_alloca,1) {
        return builder.CreateAlloca(IntegerType::get(jl_LLVMContext, 8),JL_INT(x));
    }
    default:
        assert(false);
    }
    assert(false);
    return NULL;
}

#undef HANDLE

static Function *boxfunc_llvm(FunctionType *ft, const std::string &cname,
                              void *addr)
{
    Function *f =
        Function::Create(ft, Function::ExternalLinkage, cname, jl_Module);
    jl_ExecutionEngine->addGlobalMapping(f, addr);
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

#define BOX_F(ct,jl_ct)                                                       \
    box_##ct##_func = boxfunc_llvm(ft1arg(jl_pvalue_llvmt, T_##jl_ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct);

static void add_intrinsic(jl_module_t *m, const std::string &name, intrinsic f)
{
    jl_value_t *i = jl_box32(jl_intrinsic_type, (int32_t)f);
    jl_sym_t *sym = jl_symbol(const_cast<char*>(name.c_str()));
    jl_set_const(m, sym, i);
    jl_module_export(m, sym);
}

#define ADD_I(name) add_intrinsic(inm, #name, name)

extern "C" void jl_init_intrinsic_functions(void)
{
    jl_module_t *inm = jl_new_module(jl_symbol("Intrinsics"));
    inm->parent = jl_core_module;
    jl_set_const(jl_core_module, jl_symbol("Intrinsics"), (jl_value_t*)inm);
    ADD_I(box); ADD_I(unbox);
    ADD_I(neg_int); ADD_I(add_int); ADD_I(sub_int); ADD_I(mul_int);
    ADD_I(sdiv_int); ADD_I(udiv_int); ADD_I(srem_int); ADD_I(urem_int);
    ADD_I(smod_int);
    ADD_I(neg_float); ADD_I(add_float); ADD_I(sub_float); ADD_I(mul_float);
    ADD_I(div_float); ADD_I(rem_float);
    ADD_I(eq_int); ADD_I(ne_int);
    ADD_I(slt_int); ADD_I(ult_int);
    ADD_I(sle_int); ADD_I(ule_int);
    ADD_I(eq_float); ADD_I(ne_float);
    ADD_I(lt_float); ADD_I(le_float);
    ADD_I(eqfsi64); ADD_I(eqfui64);
    ADD_I(ltfsi64); ADD_I(ltfui64);
    ADD_I(lefsi64); ADD_I(lefui64);
    ADD_I(ltsif64); ADD_I(ltuif64);
    ADD_I(lesif64); ADD_I(leuif64);
    ADD_I(fpiseq); ADD_I(fpislt);
    ADD_I(and_int); ADD_I(or_int); ADD_I(xor_int); ADD_I(not_int);
    ADD_I(shl_int); ADD_I(lshr_int); ADD_I(ashr_int); ADD_I(bswap_int);
    ADD_I(ctpop_int); ADD_I(ctlz_int); ADD_I(cttz_int);
    ADD_I(sext_int); ADD_I(zext_int); ADD_I(trunc_int);
    ADD_I(fptoui); ADD_I(fptosi);
    ADD_I(fpsiround); ADD_I(fpuiround);
    ADD_I(uitofp); ADD_I(sitofp);
    ADD_I(fptrunc); ADD_I(fpext);
    ADD_I(abs_float); ADD_I(copysign_float);
    ADD_I(flipsign_int);
    ADD_I(pointerref); ADD_I(pointerset); ADD_I(pointertoref);
    ADD_I(checked_sadd); ADD_I(checked_uadd);
    ADD_I(checked_ssub); ADD_I(checked_usub);
    ADD_I(checked_smul); ADD_I(checked_umul);
    ADD_I(checked_fptosi); ADD_I(checked_fptoui);
    ADD_I(nan_dom_err);
    ADD_I(ccall); ADD_I(cglobal);
    ADD_I(jl_alloca);
}
