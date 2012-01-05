namespace JL_I {
    enum intrinsic {
        // wrap and unwrap
        boxui8=0, boxsi8, boxui16, boxsi16, boxui32, boxsi32, boxui64, boxsi64,
        boxf32, boxf64, box,
        unbox8, unbox16, unbox32, unbox64, unbox,
        // arithmetic
        neg_int, add_int, sub_int, mul_int,
        sdiv_int, udiv_int, srem_int, urem_int,
        neg_float, add_float, sub_float, mul_float, div_float, rem_float,
        // comparison
        eq_int,  ne_int,
        slt_int, ult_int,
        sle_int, ule_int,
        eq_float, ne_float,
        lt_float, le_float,
        eq_f64_i64, eq_f64_u64,
        fpiseq32, fpiseq64,
        fpislt32, fpislt64,
        // bitwise operators
        and_int, or_int, xor_int, not_int, shl_int, lshr_int, ashr_int,
        bswap_int, ctpop_int, ctlz_int, cttz_int,
        // conversion
        sext16, zext16, sext32, zext32, sext64, zext64, zext_int,
        trunc8, trunc16, trunc32, trunc64, trunc_int,
        fptoui8, fptosi8, fptoui16, fptosi16, fptoui32, fptosi32,
        fptoui64, fptosi64, 
        fpsiround32, fpsiround64, fpuiround32, fpuiround64,
        uitofp32, sitofp32, uitofp64, sitofp64,
        fptrunc32, fpext64,
        // functions
        sqrt_float, abs_float32, abs_float64,
        copysign_float32, copysign_float64,
        // c interface
        ccall,
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

// convert int type to same-size float type
static Type *FT(Type *t)
{
    if (t->isFloatingPointTy())
        return t;
    if (t == T_int32) return T_float32;
    assert(t == T_int64);
    return T_float64;
}

// reinterpret-cast to float
static Value *FP(Value *v)
{
    if (v->getType()->isFloatingPointTy())
        return v;
    return builder.CreateBitCast(v, FT(v->getType()));
}

// convert float type to same-size int type
static Type *INTT(Type *t)
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
static Value *INT(Value *v)
{
    if (v->getType()->isIntegerTy())
        return v;
    return builder.CreateBitCast(v, INTT(v->getType()));
}

static Value *uint_cnvt(Type *to, Value *x)
{
    Type *t = x->getType();
    if (t == to) return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return builder.CreateTrunc(x, to);
    return builder.CreateZExt(x, to);
}

static Value *emit_unbox(Type *to, Type *pto, Value *x)
{
    if (x->getType() != jl_pvalue_llvmt) {
        // bools are stored internally as int8 (for now), so we need to make
        // unbox8(x::Bool) work.
        if (x->getType() == T_int1 && to == T_int8)
            return builder.CreateZExt(x, T_int8);
        if (x->getType()->isPointerTy() && !to->isPointerTy())
            return builder.CreatePtrToInt(x, to);
        return x;
    }
    Value *p = bitstype_pointer(x);
    if (to == T_int1) {
        // bools stored as int8, so an extra Trunc is needed to get an int1
        return builder.CreateTrunc(builder.
                                   CreateLoad(builder.
                                              CreateBitCast(p, T_pint8), false),
                                   T_int1);
    }
    return builder.CreateLoad(builder.CreateBitCast(p, pto), false);
}

static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx)
{
    if (jl_is_int32(e)) {
        return ConstantInt::get(T_int32, jl_unbox_int32(e));
    }
    else if (jl_is_int64(e)) {
        return ConstantInt::get(T_int64, jl_unbox_int64(e));
    }
    else if (jl_is_uint64(e)) {
        return mark_julia_type(ConstantInt::get(T_int64,
                                                (int64_t)jl_unbox_uint64(e)),
                               jl_uint64_type);
    }
    else if (jl_is_float64(e)) {
        return ConstantFP::get(T_float64, jl_unbox_float64(e));
    }
    else if (e == jl_true) {
        return ConstantInt::get(T_int1, 1);
    }
    else if (e == jl_false) {
        return ConstantInt::get(T_int1, 0);
    }
    else if (jl_is_bits_type(jl_typeof(e))) {
        jl_bits_type_t *bt = (jl_bits_type_t*)jl_typeof(e);
        int nb = jl_bitstype_nbits(bt);
        if (nb == 8)
            return mark_julia_type(ConstantInt::get(T_int8,
                                                    jl_unbox_int8(e)),
                                   (jl_value_t*)bt);
        if (nb == 16)
            return mark_julia_type(ConstantInt::get(T_int16,
                                                    jl_unbox_int16(e)),
                                   (jl_value_t*)bt);
        if (nb == 32)
            return mark_julia_type(ConstantInt::get(T_int32,
                                                    jl_unbox_int32(e)),
                                   (jl_value_t*)bt);
        if (nb == 64)
            return mark_julia_type(ConstantInt::get(T_int64,
                                                    jl_unbox_int64(e)),
                                   (jl_value_t*)bt);
        // TODO: bigger sizes
    }
    return emit_expr(e, ctx, true);
}

static Value *generic_box(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *bt =
        jl_interpret_toplevel_expr_with(targ,
                                        &jl_tupleref(ctx->sp,0),
                                        ctx->sp->length/2);
    if (!jl_is_bits_type(bt))
        jl_error("box: expected bits type as first argument");
    unsigned int nb = jl_bitstype_nbits(bt);
    Value *vx = emit_unboxed(x, ctx);
    if (vx->getType()->getPrimitiveSizeInBits() != nb)
        jl_errorf("box: expected argument with %d bits", nb);
    Type *llvmt = julia_type_to_llvm(bt, ctx);
    if (llvmt == NULL) {
        return literal_pointer_val(jl_nothing);
    }
    if (vx->getType() != llvmt) {
        if (vx->getType()->isPointerTy() && !llvmt->isPointerTy()) {
            vx = builder.CreatePtrToInt(vx, llvmt);
        }
        else if (!vx->getType()->isPointerTy() && llvmt->isPointerTy()) {
            vx = builder.CreateIntToPtr(vx, llvmt);
        }
        else {
            if (llvmt == T_int1)
                vx = builder.CreateTrunc(vx, llvmt);
            else
                vx = builder.CreateBitCast(vx, llvmt);
        }
    }
    return mark_julia_type(vx, bt);
}

static Value *generic_unbox(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *bt =
        jl_interpret_toplevel_expr_with(targ,
                                        &jl_tupleref(ctx->sp,0),
                                        ctx->sp->length/2);
    if (!jl_is_bits_type(bt))
        jl_error("unbox: expected bits type as first argument");
    unsigned int nb = jl_bitstype_nbits(bt);
    Type *to = IntegerType::get(jl_LLVMContext, nb);
    return emit_unbox(to, PointerType::get(to, 0), emit_unboxed(x, ctx));
}

static Value *generic_trunc(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *bt =
        jl_interpret_toplevel_expr_with(targ,
                                        &jl_tupleref(ctx->sp,0),
                                        ctx->sp->length/2);
    if (!jl_is_bits_type(bt))
        jl_error("trunc_int: expected bits type as first argument");
    unsigned int nb = jl_bitstype_nbits(bt);
    Type *to = IntegerType::get(jl_LLVMContext, nb);
    return builder.CreateTrunc(INT(emit_unboxed(x,ctx)), to);
}

static Value *generic_zext(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *bt =
        jl_interpret_toplevel_expr_with(targ,
                                        &jl_tupleref(ctx->sp,0),
                                        ctx->sp->length/2);
    if (!jl_is_bits_type(bt))
        jl_error("zext_int: expected bits type as first argument");
    unsigned int nb = jl_bitstype_nbits(bt);
    Type *to = IntegerType::get(jl_LLVMContext, nb);
    return builder.CreateZExt(INT(emit_unboxed(x,ctx)), to);
}

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    if (f == ccall) return emit_ccall(args, nargs, ctx);
    if (f == box) {
        if (nargs!=2)
            jl_error("box: wrong number of arguments");
        return generic_box(args[1], args[2], ctx);
    }
    if (f == unbox) {
        if (nargs!=2)
            jl_error("unbox: wrong number of arguments");
        return generic_unbox(args[1], args[2], ctx);
    }
    if (f == trunc_int) {
        if (nargs!=2)
            jl_error("trunc_int: wrong number of arguments");
        return generic_trunc(args[1], args[2], ctx);
    }
    if (f == zext_int) {
        if (nargs!=2)
            jl_error("zext_int: wrong number of arguments");
        return generic_zext(args[1], args[2], ctx);
    }
    if (nargs < 1) jl_error("invalid intrinsic call");
    Value *x = emit_unboxed(args[1], ctx);
    Type *t = x->getType();
    Type *fxt;
    Value *fx, *fy;
    Value *den;
    ConstantInt *ci=NULL;
    switch (f) {
    HANDLE(boxui8,1)
        if (t != T_int8) x = builder.CreateBitCast(x, T_int8);
        return mark_julia_type(x, jl_uint8_type);
    HANDLE(boxsi8,1)
        if (t != T_int8) x = builder.CreateBitCast(x, T_int8);
        return mark_julia_type(x, jl_int8_type);
    HANDLE(boxui16,1)
        if (t != T_int16) x = builder.CreateBitCast(x, T_int16);
        return mark_julia_type(x, jl_uint16_type);
    HANDLE(boxsi16,1)
        if (t != T_int16) x = builder.CreateBitCast(x, T_int16);
        return mark_julia_type(x, jl_int16_type);
    HANDLE(boxui32,1)
        if (t != T_int32) x = builder.CreateBitCast(x, T_int32);
        return mark_julia_type(x, jl_uint32_type);
    HANDLE(boxsi32,1)
        if (t != T_int32) x = builder.CreateBitCast(x, T_int32);
        return mark_julia_type(x, jl_int32_type);
    HANDLE(boxui64,1)
        if (t != T_int64) x = builder.CreateBitCast(x, T_int64);
        return mark_julia_type(x, jl_uint64_type);
    HANDLE(boxsi64,1)
        if (t != T_int64) x = builder.CreateBitCast(x, T_int64);
        return mark_julia_type(x, jl_int64_type);
    HANDLE(boxf32,1)
        if (t != T_float32) x = builder.CreateBitCast(x, T_float32);
        return mark_julia_type(x, jl_float32_type);
    HANDLE(boxf64,1)
        if (t != T_float64) x = builder.CreateBitCast(x, T_float64);
        return mark_julia_type(x, jl_float64_type);

    HANDLE(unbox8,1)
        return emit_unbox(T_int8, T_pint8, x);
    HANDLE(unbox16,1)
        return emit_unbox(T_int16, T_pint16, x);
    HANDLE(unbox32,1)
        return emit_unbox(T_int32, T_pint32, x);
    HANDLE(unbox64,1)
        return emit_unbox(T_int64, T_pint64, x);

    HANDLE(neg_int,1)
        return builder.CreateSub(ConstantInt::get(t, 0), x);
    HANDLE(add_int,2)
        return builder.CreateAdd(x, emit_expr(args[2],ctx,true));
    HANDLE(sub_int,2)
        return builder.CreateSub(x, emit_expr(args[2],ctx,true));
    HANDLE(mul_int,2)
        return builder.CreateMul(x, emit_expr(args[2],ctx,true));
    HANDLE(sdiv_int,2)
        den = emit_expr(args[2],ctx,true);
        call_error_func_unless(builder.CreateICmpNE(den,
                                                    ConstantInt::get(t,0)),
                               jldiverror_func, ctx);
        return builder.CreateSDiv(x, den);
    HANDLE(udiv_int,2)
        den = emit_expr(args[2],ctx,true);
        call_error_func_unless(builder.CreateICmpNE(den,
                                                    ConstantInt::get(t,0)),
                               jldiverror_func, ctx);
        return builder.CreateUDiv(x, den);
    HANDLE(srem_int,2)
        return builder.CreateSRem(x, emit_expr(args[2],ctx,true));
    HANDLE(urem_int,2)
        return builder.CreateURem(x, emit_expr(args[2],ctx,true));

    HANDLE(neg_float,1)
        return builder.CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
    HANDLE(add_float,2)
        return builder.CreateFAdd(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(sub_float,2)
        return builder.CreateFSub(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(mul_float,2)
        return builder.CreateFMul(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(div_float,2)
        return builder.CreateFDiv(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(rem_float,2)
        return builder.CreateFRem(FP(x), FP(emit_expr(args[2],ctx,true)));

    HANDLE(eq_int,2)
        return builder.CreateICmpEQ(INT(x), INT(emit_expr(args[2],ctx,true)));
    HANDLE(ne_int,2)
        return builder.CreateICmpNE(INT(x), INT(emit_expr(args[2],ctx,true)));
    HANDLE(slt_int,2)
        return builder.CreateICmpSLT(INT(x), INT(emit_expr(args[2],ctx,true)));
    HANDLE(ult_int,2)
        return builder.CreateICmpULT(INT(x), INT(emit_expr(args[2],ctx,true)));
    HANDLE(sle_int,2)
        return builder.CreateICmpSLE(INT(x), INT(emit_expr(args[2],ctx,true)));
    HANDLE(ule_int,2)
        return builder.CreateICmpULE(INT(x), INT(emit_expr(args[2],ctx,true)));

    HANDLE(eq_float,2)
        return builder.CreateFCmpOEQ(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(ne_float,2)
        return builder.CreateFCmpUNE(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(lt_float,2)
        return builder.CreateFCmpOLT(FP(x), FP(emit_expr(args[2],ctx,true)));
    HANDLE(le_float,2)
        return builder.CreateFCmpOLE(FP(x), FP(emit_expr(args[2],ctx,true)));

    HANDLE(eq_f64_i64,2) {
        x = FP(x);
        fy = INT(emit_expr(args[2],ctx,true));
        ci = dyn_cast<ConstantInt>(fy);
        if (ci != NULL) {
            int64_t yval = ci->getSExtValue();
            if (yval < 0) yval = -yval;
            return __builtin_clzll(yval)+__builtin_ctzll(yval) > 10 ?
                builder.CreateFCmpOEQ(x, builder.CreateSIToFP(fy, T_float64)) :
                ConstantInt::get(T_int1, 0);
        }
        return builder.CreateAnd(
            builder.CreateFCmpOEQ(x, builder.CreateSIToFP(fy, T_float64)),
            builder.CreateICmpEQ(builder.CreateFPToSI(x, T_int64), fy)
        );
    }
    HANDLE(eq_f64_u64,2) {
        x = FP(x);
        fy = INT(emit_expr(args[2],ctx,true));
        ci = dyn_cast<ConstantInt>(fy);
        if (ci != NULL) {
            uint64_t yval = ci->getZExtValue();
            return __builtin_clzll(yval)+__builtin_ctzll(yval) > 10 ?
                builder.CreateFCmpOEQ(x, builder.CreateUIToFP(fy, T_float64)) :
                ConstantInt::get(T_int1, 0);
        }
        return builder.CreateAnd(
            builder.CreateFCmpOEQ(x, builder.CreateUIToFP(fy, T_float64)),
            builder.CreateICmpEQ(builder.CreateFPToUI(x, T_int64), fy)
        );
    }
    HANDLE(fpiseq32,2) {
        x = FP(x);
        fy = FP(emit_expr(args[2],ctx,true));
        Value *xi = builder.CreateBitCast(x,  T_int32);
        Value *yi = builder.CreateBitCast(fy, T_int32);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpUNO(x, x),
                builder.CreateFCmpUNO(fy, fy)
            ),
            builder.CreateAnd(
                builder.CreateICmpEQ(xi, yi),
                builder.CreateFCmpORD(x, fy)
            )
        );
    }
    HANDLE(fpiseq64,2) {
        x = FP(x);
        fy = FP(emit_expr(args[2],ctx,true));
        Value *xi = builder.CreateBitCast(x,  T_int64);
        Value *yi = builder.CreateBitCast(fy, T_int64);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpUNO(x, x),
                builder.CreateFCmpUNO(fy, fy)
            ),
            builder.CreateAnd(
                builder.CreateICmpEQ(xi, yi),
                builder.CreateFCmpORD(x, fy)
            )
        );
    }
    HANDLE(fpislt32,2) {
        x = FP(x);
        fy = FP(emit_expr(args[2],ctx,true));
        Value *xi = builder.CreateBitCast(x,  T_int32);
        Value *yi = builder.CreateBitCast(fy, T_int32);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpORD(x, x),
                builder.CreateFCmpUNO(fy, fy)
            ),
            builder.CreateAnd(
                builder.CreateFCmpORD(x, fy),
                builder.CreateOr(
                    builder.CreateAnd(
                        builder.CreateICmpSGE(xi, ConstantInt::get(T_int32, 0)),
                        builder.CreateICmpSLT(xi, yi)
                    ),
                    builder.CreateAnd(
                        builder.CreateICmpSLT(xi, ConstantInt::get(T_int32, 0)),
                        builder.CreateICmpUGT(xi, yi)
                    )
                )
            )
        );
    }
    HANDLE(fpislt64,2) {
        x = FP(x);
        fy = FP(emit_expr(args[2],ctx,true));
        Value *xi = builder.CreateBitCast(x,  T_int64);
        Value *yi = builder.CreateBitCast(fy, T_int64);
        return builder.CreateOr(
            builder.CreateAnd(
                builder.CreateFCmpORD(x, x),
                builder.CreateFCmpUNO(fy, fy)
            ),
            builder.CreateAnd(
                builder.CreateFCmpORD(x, fy),
                builder.CreateOr(
                    builder.CreateAnd(
                        builder.CreateICmpSGE(xi, ConstantInt::get(T_int64, 0)),
                        builder.CreateICmpSLT(xi, yi)
                    ),
                    builder.CreateAnd(
                        builder.CreateICmpSLT(xi, ConstantInt::get(T_int64, 0)),
                        builder.CreateICmpUGT(xi, yi)
                    )
                )
            )
        );
    }

    HANDLE(and_int,2)
        return builder.CreateAnd(INT(x), emit_expr(args[2],ctx,true));
    HANDLE(or_int,2)
        return builder.CreateOr(INT(x), emit_expr(args[2],ctx,true));
    HANDLE(xor_int,2)
        return builder.CreateXor(INT(x), emit_expr(args[2],ctx,true));
    HANDLE(not_int,1)
        return builder.CreateXor(INT(x), ConstantInt::get(t, -1));
    HANDLE(shl_int,2)
        return builder.CreateShl(INT(x), uint_cnvt(t,emit_unboxed(args[2],ctx)));
    HANDLE(lshr_int,2)
        return builder.CreateLShr(INT(x), uint_cnvt(t,emit_unboxed(args[2],ctx)));
    HANDLE(ashr_int,2)
        return builder.CreateAShr(INT(x), uint_cnvt(t,emit_unboxed(args[2],ctx)));
    HANDLE(bswap_int,1)
        x = INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap,
                                      ArrayRef<Type*>(x->getType())), x);
    HANDLE(ctpop_int,1)
        x = INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop,
                                      ArrayRef<Type*>(x->getType())), x);
    HANDLE(ctlz_int,1)
        x = INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz,
                                      ArrayRef<Type*>(x->getType())), x);
    HANDLE(cttz_int,1)
        x = INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz,
                                      ArrayRef<Type*>(x->getType())), x);

    HANDLE(sext16,1)
        return builder.CreateSExt(INT(x), T_int16);
    HANDLE(zext16,1)
        return builder.CreateZExt(INT(x), T_int16);
    HANDLE(sext32,1)
        return builder.CreateSExt(INT(x), T_int32);
    HANDLE(zext32,1)
        return builder.CreateZExt(INT(x), T_int32);
    HANDLE(sext64,1)
        return builder.CreateSExt(INT(x), T_int64);
    HANDLE(zext64,1)
        return builder.CreateZExt(INT(x), T_int64);
    HANDLE(trunc8,1)
        return builder.CreateTrunc(INT(x), T_int8);
    HANDLE(trunc16,1)
        return builder.CreateTrunc(INT(x), T_int16);
    HANDLE(trunc32,1)
        return builder.CreateTrunc(INT(x), T_int32);
    HANDLE(trunc64,1)
        return builder.CreateTrunc(INT(x), T_int64);
    HANDLE(fptoui8,1)
        return builder.CreateFPToUI(FP(x), T_int8);
    HANDLE(fptosi8,1)
        return builder.CreateFPToSI(FP(x), T_int8);
    HANDLE(fptoui16,1)
        return builder.CreateFPToUI(FP(x), T_int16);
    HANDLE(fptosi16,1)
        return builder.CreateFPToSI(FP(x), T_int16);
    HANDLE(fptoui32,1)
        return builder.CreateFPToUI(FP(x), T_int32);
    HANDLE(fptosi32,1)
        return builder.CreateFPToSI(FP(x), T_int32);
    HANDLE(fptoui64,1)
        return builder.CreateFPToUI(FP(x), T_int64);
    HANDLE(fptosi64,1)
        return builder.CreateFPToSI(FP(x), T_int64);
    HANDLE(fpsiround32,1)
    HANDLE(fpuiround32,1)
    {
        // itrunc(x + copysign(0.5,x))
        Value *bits = INT(x);
        Value *half = builder.CreateBitCast(ConstantFP::get(T_float32, 0.5),
                                            T_int32);
        Value *signedhalf =
            builder.CreateOr(half,
                             builder.CreateAnd(bits,
                                               ConstantInt::get(T_int32,
                                                                BIT31)));
        Value *sum = builder.CreateFAdd(FP(x),
                                        builder.CreateBitCast(signedhalf,
                                                              T_float32));
        if (f == fpuiround32) {
            return builder.CreateFPToUI(sum, T_int32);
        }
        else {
            return builder.CreateFPToSI(sum, T_int32);
        }
    }
    HANDLE(fpsiround64,1)
    HANDLE(fpuiround64,1)
    {
        Value *bits = INT(x);
        Value *half = builder.CreateBitCast(ConstantFP::get(T_float64, 0.5),
                                            T_int64);
        Value *signedhalf =
            builder.CreateOr(half,
                             builder.CreateAnd(bits,
                                               ConstantInt::get(T_int64,
                                                                BIT63)));
        Value *sum = builder.CreateFAdd(FP(x),
                                        builder.CreateBitCast(signedhalf,
                                                              T_float64));
        if (f == fpuiround64) {
            return builder.CreateFPToUI(sum, T_int64);
        }
        else {
            return builder.CreateFPToSI(sum, T_int64);
        }
    }
    HANDLE(uitofp32,1)
        return builder.CreateUIToFP(x, T_float32);
    HANDLE(sitofp32,1)
        return builder.CreateSIToFP(x, T_float32);
    HANDLE(uitofp64,1)
        return builder.CreateUIToFP(x, T_float64);
    HANDLE(sitofp64,1)
        return builder.CreateSIToFP(x, T_float64);
    HANDLE(fptrunc32,1)
        return builder.CreateFPTrunc(FP(x), T_float32);
    HANDLE(fpext64,1)
        // when extending a float32 to a float64, we need to force
        // rounding to single precision first. the reason is that it's
        // fine to keep working in extended precision as long as it's
        // understood that everything is implicitly rounded to 23 bits,
        // but if we start looking at more bits we need to actually do the
        // rounding first instead of carrying around incorrect low bits.
        builder.CreateStore(FP(x), jlfloat32temp_var, true);
        return builder.CreateFPExt(builder.CreateLoad(jlfloat32temp_var, true),
                                   T_float64);

    HANDLE(sqrt_float,1)
        fx = FP(x);
        fxt = fx->getType();
        return builder.
            CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                 Intrinsic::sqrt,
                                                 ArrayRef<Type*>(fxt)),
                       fx);
    HANDLE(abs_float32,1)
    {
        Value *bits = builder.CreateBitCast(x, T_int32);
        Value *absbits = builder.CreateAnd(bits,
                                           ConstantInt::get(T_int32, ~BIT31));
        return builder.CreateBitCast(absbits, T_float32);
    }
    HANDLE(abs_float64,1)
    {
        Value *bits = builder.CreateBitCast(x, T_int64);
        Value *absbits = builder.CreateAnd(bits,
                                           ConstantInt::get(T_int64, ~BIT63));
        return builder.CreateBitCast(absbits, T_float64);
    }
    HANDLE(copysign_float32,2)
    {
        fy = emit_expr(args[2],ctx,true);
        Value *bits = builder.CreateBitCast(x, T_int32);
        Value *sbits = builder.CreateBitCast(fy, T_int32);
        Value *rbits =
            builder.CreateOr(builder.CreateAnd(bits,
                                               ConstantInt::get(T_int32,
                                                                ~BIT31)),
                             builder.CreateAnd(sbits,
                                               ConstantInt::get(T_int32,
                                                                BIT31)));
        return builder.CreateBitCast(rbits, T_float32);
    }
    HANDLE(copysign_float64,2)
    {
        fy = emit_expr(args[2],ctx,true);
        Value *bits = builder.CreateBitCast(x, T_int64);
        Value *sbits = builder.CreateBitCast(fy, T_int64);
        Value *rbits =
            builder.CreateOr(builder.CreateAnd(bits,
                                               ConstantInt::get(T_int64,
                                                                ~BIT63)),
                             builder.CreateAnd(sbits,
                                               ConstantInt::get(T_int64,
                                                                BIT63)));
        return builder.CreateBitCast(rbits, T_float64);
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

static void add_intrinsic(const std::string &name, intrinsic f)
{
    jl_value_t *i = jl_box32(jl_intrinsic_type, (int32_t)f);
    jl_set_const(jl_system_module, jl_symbol((char*)name.c_str()), i);
}

#define ADD_I(name) add_intrinsic(#name, name)
#define BOX_F(ct,jl_ct)                                                       \
    box_##ct##_func = boxfunc_llvm(ft1arg(jl_pvalue_llvmt, T_##jl_ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct);

extern "C" void jl_init_intrinsic_functions()
{
    ADD_I(boxui8); ADD_I(boxsi8); ADD_I(boxui16); ADD_I(boxsi16);
    ADD_I(boxui32); ADD_I(boxsi32); ADD_I(boxui64); ADD_I(boxsi64);
    ADD_I(boxf32); ADD_I(boxf64); ADD_I(box); ADD_I(unbox);
    ADD_I(unbox8); ADD_I(unbox16); ADD_I(unbox32); ADD_I(unbox64);
    ADD_I(neg_int); ADD_I(add_int); ADD_I(sub_int); ADD_I(mul_int);
    ADD_I(sdiv_int); ADD_I(udiv_int); ADD_I(srem_int); ADD_I(urem_int);
    ADD_I(neg_float); ADD_I(add_float); ADD_I(sub_float); ADD_I(mul_float);
    ADD_I(div_float); ADD_I(rem_float);
    ADD_I(eq_int); ADD_I(ne_int);
    ADD_I(slt_int); ADD_I(ult_int);
    ADD_I(sle_int); ADD_I(ule_int);
    ADD_I(eq_float); ADD_I(ne_float);
    ADD_I(lt_float); ADD_I(le_float);
    ADD_I(eq_f64_i64); ADD_I(eq_f64_u64);
    ADD_I(fpiseq32); ADD_I(fpiseq64);
    ADD_I(fpislt32); ADD_I(fpislt64);
    ADD_I(and_int); ADD_I(or_int); ADD_I(xor_int); ADD_I(not_int);
    ADD_I(shl_int); ADD_I(lshr_int); ADD_I(ashr_int); ADD_I(bswap_int);
    ADD_I(ctpop_int); ADD_I(ctlz_int); ADD_I(cttz_int);
    ADD_I(sext16); ADD_I(zext16); ADD_I(sext32); ADD_I(zext32);
    ADD_I(sext64); ADD_I(zext64); ADD_I(zext_int);
    ADD_I(trunc8); ADD_I(trunc16); ADD_I(trunc32); ADD_I(trunc64);
    ADD_I(trunc_int);
    ADD_I(fptoui8); ADD_I(fptosi8);
    ADD_I(fptoui16); ADD_I(fptosi16); ADD_I(fptoui32); ADD_I(fptosi32);
    ADD_I(fptoui64); ADD_I(fptosi64);
    ADD_I(fpsiround32); ADD_I(fpsiround64);
    ADD_I(fpuiround32); ADD_I(fpuiround64);
    ADD_I(uitofp32); ADD_I(sitofp32); ADD_I(uitofp64); ADD_I(sitofp64);
    ADD_I(fptrunc32); ADD_I(fpext64);
    ADD_I(sqrt_float);
    ADD_I(abs_float32); ADD_I(abs_float64);
    ADD_I(copysign_float32); ADD_I(copysign_float64);
    ADD_I(ccall);
    
    BOX_F(int8,int32);  BOX_F(uint8,uint32);
    BOX_F(int16,int16); BOX_F(uint16,uint16);
    BOX_F(int32,int32); BOX_F(uint32,uint32);
    BOX_F(int64,int64); BOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);

    box8_func  = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int8),
                              "jl_box8", (void*)*jl_box8);
    box16_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int16),
                              "jl_box16", (void*)*jl_box16);
    box32_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int32),
                              "jl_box32", (void*)*jl_box32);
    box64_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int64),
                              "jl_box64", (void*)*jl_box64);

    std::vector<Type*> toptrargs(0);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(T_int32);
    value_to_pointer_func =
        Function::Create(FunctionType::get(T_pint8, toptrargs, false),
                         Function::ExternalLinkage, "jl_value_to_pointer",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(value_to_pointer_func,
                                         (void*)&jl_value_to_pointer);

    temp_arg_area = (char*)allocb_permanent(arg_area_sz);
    arg_area_loc = 0;

    std::vector<Type*> noargs(0);
    save_arg_area_loc_func =
        Function::Create(FunctionType::get(T_uint64, noargs, false),
                         Function::ExternalLinkage, "save_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(save_arg_area_loc_func,
                                         (void*)&save_arg_area_loc);

    restore_arg_area_loc_func =
        Function::Create(ft1arg(T_void, T_uint64),
                         Function::ExternalLinkage, "restore_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(restore_arg_area_loc_func,
                                         (void*)&restore_arg_area_loc);
}
