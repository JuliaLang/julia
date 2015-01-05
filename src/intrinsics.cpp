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
        // bitwise operators
        and_int, or_int, xor_int, not_int, shl_int, lshr_int, ashr_int,
        bswap_int, ctpop_int, ctlz_int, cttz_int,
        // conversion
        sext_int, zext_int, trunc_int,
        fptoui, fptosi, uitofp, sitofp,
        fptrunc, fpext,
        // checked conversion
        checked_fptosi, checked_fptoui,
        checked_trunc_sint, checked_trunc_uint, check_top_bit,
        // checked arithmetic
        checked_sadd, checked_uadd, checked_ssub, checked_usub,
        checked_smul, checked_umul,
        nan_dom_err,
        // functions
        abs_float, copysign_float, flipsign_int, select_value,
        ceil_llvm, floor_llvm, trunc_llvm, rint_llvm,
        sqrt_llvm, powi_llvm,
        // pointer access
        pointerref, pointerset, pointertoref,
        // c interface
        ccall, cglobal, jl_alloca, llvmcall
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
    if (nb == 16)
        return Type::getHalfTy(jl_LLVMContext);
    else
#endif
    if (nb == 32)
        return Type::getFloatTy(jl_LLVMContext);
    else if (nb == 64)
        return Type::getDoubleTy(jl_LLVMContext);
    else if (nb == 128)
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

static Constant *julia_const_to_llvm(jl_value_t *e)
{
    jl_value_t *jt = jl_typeof(e);
    jl_datatype_t *bt = (jl_datatype_t*)jt;

    if (!jl_is_datatype(bt))
        return NULL;

    if (e == jl_true)
        return ConstantInt::get(T_int1, 1);
    else if (e == jl_false)
        return ConstantInt::get(T_int1, 0);

    if (jl_is_bitstype(jt)) {
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
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEhalf,val));
            else
#endif
            if (nb == 4)
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEsingle,val));
            else if (nb == 8)
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEdouble,val));
            else if (nb == 16)
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEquad,val));
            // If we have a floating point type that's not hardware supported, just treat it like an integer for LLVM purposes
        }
        Constant *asInt = ConstantInt::get(IntegerType::get(jl_LLVMContext,8*nb),val);
        if (jl_is_cpointer_type(bt)) {
            return ConstantExpr::getIntToPtr(asInt, julia_type_to_llvm((jl_value_t*)bt));
        }
        return asInt;
    }
    else if (jl_isbits(jt)) {
        size_t nf = jl_tuple_len(bt->names), i;
        size_t llvm_nf = 0;
        Constant **fields = (Constant**)alloca(nf * sizeof(Constant*));
        jl_value_t *f=NULL;
        JL_GC_PUSH1(&f);
        for(i=0; i < nf; i++) {
            f = jl_get_nth_field(e, i);
            Constant *val;
            if (f == jl_true)
                val = ConstantInt::get(T_int8,1);
            else if (f == jl_false)
                val = ConstantInt::get(T_int8,0);
            else
                val = julia_const_to_llvm(f);
            if (val == NULL) {
                JL_GC_POP();
                return NULL;
            }
            if (val->getType() != NoopType)
                fields[llvm_nf++] = val;
        }
        JL_GC_POP();
        Type *t = julia_struct_to_llvm(jt);
        if (t == T_void || t->isEmptyTy())
            return UndefValue::get(NoopType);
        StructType *st = dyn_cast<StructType>(t);
        assert(st);
        return ConstantStruct::get(st, ArrayRef<Constant*>(fields,llvm_nf));
    }
    return NULL;
}

static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx)
{
    Constant *c = julia_const_to_llvm(e);
    if (c) return mark_julia_type(c, jl_typeof(e));
    return emit_expr(e, ctx, false);
}

static Type *jl_llvmtuple_eltype(Type *tuple, jl_value_t *jt, size_t i)
{
    Type *ety = NULL;
    if (tuple->isArrayTy())
        ety = dyn_cast<ArrayType>(tuple)->getElementType();
    else if (tuple->isVectorTy())
        ety = dyn_cast<VectorType>(tuple)->getElementType();
    else if (tuple == T_void)
        ety = T_void;
    else if (tuple->isStructTy())
        ety = julia_type_to_llvm(jl_tupleref((jl_tuple_t*)jt,i));
    else
        assert(false);
    return ety;
}

static Value *ghostValue(jl_value_t *ty);

// emit code to unpack a raw value from a box
static Value *emit_unbox(Type *to, Value *x, jl_value_t *jt)
{
    Type *ty = (x == NULL) ? NULL : x->getType();
    if (x == NULL || ty == NoopType) {
        if (to == T_void) {
            if (jt != NULL)
                return (ty == NoopType && julia_type_of(x) == jt) ? x : ghostValue(jt);
            return NULL;
        }
        return UndefValue::get(to);
    }
    if (ty != jl_pvalue_llvmt) {
        // bools are stored internally as int8 (for now)
        if (ty == T_int1 && to == T_int8)
            return builder.CreateZExt(x, T_int8);
        if (ty->isPointerTy() && !to->isPointerTy())
            return builder.CreatePtrToInt(x, to);
        if (!ty->isPointerTy() && to->isPointerTy())
            return builder.CreateIntToPtr(x, to);
        if (ty != to) {
            // this can happen when a branch yielding a different type ends
            // up being dead code, and type inference knows that the other
            // branch's type is the only one that matters.
            // assert(ty == T_void);
            return UndefValue::get(to);
        }
        return x;
    }
    if ( (jt != NULL && jl_is_tuple(jt)) || to->isVectorTy() || to->isArrayTy() ||
         (to->isStructTy() && dyn_cast<StructType>(to)->isLiteral()) ) {
        assert(jt != 0);
        assert(jl_is_tuple(jt));
        assert(to != T_void);
        Value *tpl = UndefValue::get(to);
        for (size_t i = 0; i < jl_tuple_len(jt); ++i) {
            Type *ety = jl_llvmtuple_eltype(to,jt,i);
            if (ety == T_void)
                continue;
            Value *ref = emit_tupleref(x,ConstantInt::get(T_size,i+1),jt,NULL);
            Value *elt = emit_unbox(ety,ref,jl_tupleref(jt,i));
            tpl = emit_tupleset(tpl,ConstantInt::get(T_size,i+1),elt,jt,NULL);
        }
        return tpl;
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
        assert(to != T_void);
        return UndefValue::get(to);
    }
    return builder.CreateLoad(builder.CreateBitCast(p, to->getPointerTo()), false);
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
            emit_error("auto_unbox: unable to determine argument type", ctx);
            // This isn't correct but probably most likely to cause
            // the least amount of trouble
            return UndefValue::get(T_int64);
        }
    }
    Type *to = julia_type_to_llvm(bt);
    if (to == NULL || to == jl_pvalue_llvmt) {
        unsigned int nb = jl_datatype_size(bt)*8;
        to = IntegerType::get(jl_LLVMContext, nb);
    }
    if (to == T_void) {
        return NULL;
    }
    return emit_unbox(to, v, bt);
}

// figure out how many bits a bitstype has at compile time, or -1
int try_to_determine_bitstype_nbits(jl_value_t *targ, jl_codectx_t *ctx)
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
            return emit_unbox(to, emit_unboxed(x,ctx), p);
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
        if (bt == NULL || !jl_is_bitstype(bt)) {
            //jl_error("unbox: could not determine argument size");
            emit_error("unbox: could not determine argument size", ctx);
            return UndefValue::get(T_void);
        }
        nb = (bt==(jl_value_t*)jl_bool_type) ? 1 : jl_datatype_size(bt)*8;
    }
    Type *to = IntegerType::get(jl_LLVMContext, nb);
    return emit_unbox(to, emit_unboxed(x, ctx), et);
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
        emit_error("reinterpret: expected bits type as first argument", ctx);
        return UndefValue::get(jl_pvalue_llvmt);
    }
    else {
        llvmt = julia_type_to_llvm(bt);
        if (llvmt == jl_pvalue_llvmt) {
            // this happens if !jl_is_leaf_type(bt)
            llvmt = NULL;
            bt = NULL;
        }
        if (nb == -1)
            nb = (bt==(jl_value_t*)jl_bool_type) ? 1 : jl_datatype_size(bt)*8;
    }

    if (nb == -1) {
        emit_error("box: could not determine argument size", ctx);
        return UndefValue::get(jl_pvalue_llvmt);
    }

    if (llvmt == NULL)
        llvmt = IntegerType::get(jl_LLVMContext, nb);

    Value *vx = auto_unbox(x, ctx);
    Type *vxt = vx->getType();
    //if (vx->getType()->getPrimitiveSizeInBits() != (unsigned)nb)
    //    jl_errorf("box: expected argument with %d bits, got %d", nb,
    //              vx->getType()->getPrimitiveSizeInBits());

    if (vxt != llvmt) {
        if (vxt == T_void)
            return vx;
        if (!vxt->isSingleValueType()) {
            jl_error("box: argument not of a primitive type");
        }
        if (llvmt == T_int1) {
            vx = builder.CreateTrunc(vx, llvmt);
        }
        else if (vxt == T_int1 && llvmt == T_int8) {
            vx = builder.CreateZExt(vx, llvmt);
        }
        else {
            // getPrimitiveSizeInBits() == 0 for pointers
            if (vxt->getPrimitiveSizeInBits() != llvmt->getPrimitiveSizeInBits() &&
                !(vxt->isPointerTy() && llvmt->getPrimitiveSizeInBits() == sizeof(void*)*8) &&
                !(llvmt->isPointerTy() && vxt->getPrimitiveSizeInBits() == sizeof(void*)*8)) {
                emit_error("box: argument is of incorrect size", ctx);
                return UndefValue::get(llvmt);
            }
            // PtrToInt and IntToPtr ignore size differences
            if (vxt->isPointerTy() && !llvmt->isPointerTy()) {
                vx = builder.CreatePtrToInt(vx, llvmt);
            }
            else if (!vxt->isPointerTy() && llvmt->isPointerTy()) {
                vx = builder.CreateIntToPtr(vx, llvmt);
            }
            else {
                vx = builder.CreateBitCast(vx, llvmt);
            }
        }
    }

    if (bt != NULL) {
        return mark_julia_type(vx, bt);
    }

    // dynamically-determined type; evaluate.
    return allocate_box_dynamic(emit_expr(targ, ctx), ConstantInt::get(T_size,(nb+7)/8), vx);
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

// NOTE: signd (signed) only relevant if check == true
static Value *generic_trunc(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx, bool check, bool signd)
{
    Type *to = staticeval_bitstype(targ, "trunc_int", ctx);
    Value *ix = JL_INT(auto_unbox(x,ctx));
    Value *ans = builder.CreateTrunc(ix, to);
    if (check) {
        Value *back = signd ? builder.CreateSExt(ans, ix->getType()) :
            builder.CreateZExt(ans, ix->getType());
        raise_exception_unless(builder.CreateICmpEQ(back, ix),
                               prepare_global(jlinexacterr_var), ctx);
    }
    return ans;
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

static Value *emit_eqfsi(Value *x, Value *y)
{
    x = FP(x);
    Value *fy = JL_INT(y);

    // using all 64-bit is slightly faster than using mixed sizes
    Value *xx = x, *vv = fy;
    if (x->getType() == T_float32)
        xx = builder.CreateFPExt(xx, T_float64);
    if (vv->getType()->getPrimitiveSizeInBits() < 64)
        vv = builder.CreateSExt(vv, T_int64);

    Value *back = builder.CreateSIToFP(vv, xx->getType());
    return builder.CreateAnd
        (builder.CreateFCmpOEQ(xx, back),
         builder.CreateICmpEQ(vv, builder.CreateFPToSI(back, vv->getType())));
}

static Value *emit_eqfui(Value *x, Value *y)
{
    x = FP(x);
    Value *fy = JL_INT(y);

    // using all 64-bit is slightly faster than using mixed sizes
    Value *xx = x, *vv = fy;
    if (x->getType() == T_float32)
        xx = builder.CreateFPExt(xx, T_float64);
    if (vv->getType()->getPrimitiveSizeInBits() < 64)
        vv = builder.CreateZExt(vv, T_int64);

    Value *back = builder.CreateUIToFP(vv, xx->getType());
    return builder.CreateAnd
        (builder.CreateFCmpOEQ(xx, back),
         builder.CreateICmpEQ(vv, builder.CreateFPToUI(back, vv->getType())));
}

static Value *emit_checked_fptosi(Type *to, Value *x, jl_codectx_t *ctx)
{
    x = FP(x);
    Value *v = builder.CreateFPToSI(x, to);
    if (x->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(x, T_float64),
                                   builder.CreateSIToFP(v, T_float64)),
             prepare_global(jlinexacterr_var), ctx);
    }
    else {
        raise_exception_unless(emit_eqfsi(x, v), prepare_global(jlinexacterr_var), ctx);
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
             prepare_global(jlinexacterr_var), ctx);
    }
    else {
        raise_exception_unless(emit_eqfui(x, v), prepare_global(jlinexacterr_var), ctx);
    }
    return v;
}

static Value *emit_checked_fptoui(jl_value_t *targ, Value *x, jl_codectx_t *ctx)
{
    return emit_checked_fptoui(staticeval_bitstype(targ, "checked_fptoui", ctx), x, ctx);
}

static Value *emit_runtime_pointerref(jl_value_t *e, jl_value_t *i, jl_codectx_t *ctx)
{
    Value *preffunc =
        jl_Module->getOrInsertFunction("jl_pointerref",
                                       FunctionType::get(jl_pvalue_llvmt, two_pvalue_llvmt, false));
    int ldepth = ctx->argDepth;
    Value *parg = emit_boxed_rooted(e, ctx);
    Value *iarg = boxed(emit_expr(i, ctx), ctx);
    Value *ret = builder.CreateCall2(prepare_call(preffunc), parg, iarg);
    ctx->argDepth = ldepth;
    return ret;
}

static Value *emit_pointerref(jl_value_t *e, jl_value_t *i, jl_codectx_t *ctx)
{
    jl_value_t *aty = expr_type(e, ctx);
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerref(e, i, ctx);
        //jl_error("pointerref: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerref(e, i, ctx);
        //jl_error("pointerref: invalid pointer");
    if (expr_type(i, ctx) != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerref(e, i, ctx);
        //jl_error("pointerref: invalid index type");
    Value *thePtr = auto_unbox(e,ctx);
    Value *idx = emit_unbox(T_size, emit_unboxed(i, ctx), (jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    if (!jl_isbits(ety)) {
        if (ety == (jl_value_t*)jl_any_type)
            return builder.CreateLoad(builder.CreateGEP(
                        builder.CreateBitCast(thePtr, jl_ppvalue_llvmt),
                        im1));
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerref: invalid pointer type", ctx);
            return NULL;
        }
        assert(jl_is_datatype(ety));
        uint64_t size = ((jl_datatype_t*)ety)->size;
        Value *strct =
            builder.CreateCall(prepare_call(jlallocobj_func),
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
    return typed_load(thePtr, im1, ety, ctx, tbaa_user);
}

static Value *emit_runtime_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_codectx_t *ctx)
{
    Value *psetfunc =
        jl_Module->getOrInsertFunction("jl_pointerset",
                                       FunctionType::get(T_void, three_pvalue_llvmt, false));
    int ldepth = ctx->argDepth;
    Value *parg = emit_boxed_rooted(e, ctx);
    Value *iarg = emit_boxed_rooted(i, ctx);
    Value *xarg = boxed(emit_expr(x, ctx), ctx);
    builder.CreateCall3(prepare_call(psetfunc), parg, xarg, iarg);
    ctx->argDepth = ldepth;
    return parg;
}

// e[i] = x
static Value *emit_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_codectx_t *ctx)
{
    jl_value_t *aty = expr_type(e, ctx);
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerset(e, x, i, ctx);
        //jl_error("pointerset: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerset(e, x, i, ctx);
        //jl_error("pointerset: invalid pointer");
    jl_value_t *xty = expr_type(x, ctx);
    Value *val=NULL;
    if (!jl_subtype(xty, ety, 0)) {
        val = emit_expr(x,ctx);
        emit_typecheck(val, ety, "pointerset: type mismatch in assign", ctx);
    }
    if (expr_type(i, ctx) != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerset(e, x, i, ctx);
        //jl_error("pointerset: invalid index type");
    Value *idx = emit_unbox(T_size, emit_unboxed(i, ctx),(jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    Value *thePtr = auto_unbox(e,ctx);
    if (!jl_isbits(ety) && ety != (jl_value_t*)jl_any_type) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerset: invalid pointer type", ctx);
            return NULL;
        }
        if (val==NULL) val = emit_expr(x,ctx,true,true);
        assert(val->getType() == jl_pvalue_llvmt); //Boxed
        assert(jl_is_datatype(ety));
        uint64_t size = ((jl_datatype_t*)ety)->size;
        builder.CreateMemCpy(builder.CreateGEP(builder.CreateBitCast(thePtr, T_pint8), im1),
                             builder.CreateBitCast(emit_nthptr_addr(val, (size_t)1),T_pint8), size, 1);
    }
    else {
        if (val == NULL) {
            if (ety == (jl_value_t*)jl_any_type)
                val = emit_expr(x,ctx);
            else
                val = emit_unboxed(x,ctx);
        }
        typed_store(thePtr, im1, val, ety, ctx, tbaa_user);
    }
    return mark_julia_type(thePtr, aty);
}

static Value *emit_srem(Value *x, Value *den, jl_codectx_t *ctx)
{
    Type *t = den->getType();
    raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                           prepare_global(jldiverr_var), ctx);
    BasicBlock *m1BB = BasicBlock::Create(getGlobalContext(),"minus1",ctx->f);
    BasicBlock *okBB = BasicBlock::Create(getGlobalContext(),"oksrem",ctx->f);
    BasicBlock *cont = BasicBlock::Create(getGlobalContext(),"after_srem",ctx->f);
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

static Value *emit_smod(Value *x, Value *den, jl_codectx_t *ctx)
{
    Type *t = den->getType();
    raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                           prepare_global(jldiverr_var), ctx);
    BasicBlock *m1BB = BasicBlock::Create(getGlobalContext(),"minus1",ctx->f);
    BasicBlock *okBB = BasicBlock::Create(getGlobalContext(),"oksmod",ctx->f);
    BasicBlock *cont = BasicBlock::Create(getGlobalContext(),"after_smod",ctx->f);
    PHINode *ret = PHINode::Create(t, 2);
    builder.CreateCondBr(builder.CreateICmpEQ(den,ConstantInt::get(t,-1,true)),
                         m1BB, okBB);
    builder.SetInsertPoint(m1BB);
    builder.CreateBr(cont);
    builder.SetInsertPoint(okBB);

    Value *rem = builder.CreateSRem(x,den);
    Value *smodval =
        builder.
        CreateSelect(builder.CreateICmpEQ(builder.CreateICmpSLT(x,ConstantInt::get(t,0)),
                                          builder.CreateICmpSLT(den,ConstantInt::get(t,0))),
                     // mod == rem for arguments with same sign
                     rem,
                     builder.CreateSRem(builder.CreateAdd(den,rem),den));

    builder.CreateBr(cont);
    builder.SetInsertPoint(cont);
    ret->addIncoming(// rem(typemin, -1) is undefined
                     ConstantInt::get(t,0), m1BB);
    ret->addIncoming(smodval, okBB);
    builder.Insert(ret);
    return ret;
}

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    switch (f) {
    case ccall: return emit_ccall(args, nargs, ctx);
    case cglobal: return emit_cglobal(args, nargs, ctx);
    case llvmcall: return emit_llvmcall(args, nargs, ctx);

    HANDLE(box,2)         return generic_box(args[1], args[2], ctx);
    HANDLE(unbox,2)       return generic_unbox(args[1], args[2], ctx);
    HANDLE(trunc_int,2)   return generic_trunc(args[1], args[2], ctx, false, false);
    HANDLE(checked_trunc_sint,2)
        return generic_trunc(args[1], args[2], ctx, true, true);
    HANDLE(checked_trunc_uint,2)
        return generic_trunc(args[1], args[2], ctx, true, false);
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

    case fptoui:
        if (nargs == 1) {
            Value *x = FP(auto_unbox(args[1], ctx));
            return builder.CreateFPToUI(FP(x), JL_INTT(x->getType()));
        }
        else if (nargs == 2) {
            return builder.CreateFPToUI(FP(auto_unbox(args[2],ctx)),
                                        Type::getIntNTy(jl_LLVMContext, try_to_determine_bitstype_nbits(args[1],ctx)));
        }
        else {
            jl_error("fptoui: wrong number of arguments");
        }
    case fptosi:
        if (nargs == 1) {
            Value *x = FP(auto_unbox(args[1], ctx));
            return builder.CreateFPToSI(FP(x), JL_INTT(x->getType()));
        }
        else if (nargs == 2) {
            return builder.CreateFPToSI(FP(auto_unbox(args[2],ctx)),
                                        Type::getIntNTy(jl_LLVMContext, try_to_determine_bitstype_nbits(args[1],ctx)));
        }
        else {
            jl_error("fptosi: wrong number of arguments");
        }

    HANDLE(fptrunc,2) return builder.CreateFPTrunc(FP(auto_unbox(args[2],ctx)), FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    HANDLE(fpext,2) {
        Value *x = auto_unbox(args[2],ctx);
#if JL_NEED_FLOATTEMP_VAR
        // Target platform might carry extra precision.
        // Force rounding to single precision first. The reason is that it's
        // fine to keep working in extended precision as long as it's
        // understood that everything is implicitly rounded to 23 bits,
        // but if we start looking at more bits we need to actually do the
        // rounding first instead of carrying around incorrect low bits.
        builder.CreateStore(FP(x), builder.CreateBitCast(prepare_global(jlfloattemp_var),FT(x->getType())->getPointerTo()), true);
        x  = builder.CreateLoad(builder.CreateBitCast(prepare_global(jlfloattemp_var),FT(x->getType())->getPointerTo()), true);
#endif
        return builder.CreateFPExt(x, FTnbits(try_to_determine_bitstype_nbits(args[1],ctx)));
    }
    HANDLE(select_value,3) {
        Value *isfalse = emit_condition(args[1], "select_value", ctx);
        jl_value_t *t1 = expr_type(args[2], ctx);
        Type *llt1 = julia_type_to_llvm(t1);
        jl_value_t *t2 = expr_type(args[3], ctx);
        Type *llt2 = julia_type_to_llvm(t2);
        int argStart = ctx->argDepth;
        Value *ifelse_result;
        if (llt1 == jl_pvalue_llvmt && llt2 == jl_pvalue_llvmt) {
            Value *arg1 = emit_expr(args[3], ctx, false);
            if (arg1->getType() == jl_pvalue_llvmt)
                make_gcroot(arg1, ctx);
            ifelse_result = builder.CreateSelect(isfalse,
                                                 arg1,
                                                 emit_expr(args[2], ctx, false));
        }
        else if (t1 == t2 && llt1 == llt2 && llt1 != jl_pvalue_llvmt) {
            ifelse_result = builder.CreateSelect(isfalse,
                                                 auto_unbox(args[3], ctx),
                                                 auto_unbox(args[2], ctx));
        }
        else {
            Value *arg1 = boxed(emit_expr(args[3],ctx,false), ctx, expr_type(args[3],ctx));
            make_gcroot(arg1, ctx);
            ifelse_result = builder.CreateSelect(isfalse,
                                                 arg1,
                                                 boxed(emit_expr(args[2],ctx,false), ctx, expr_type(args[2],ctx)));
        }
        ctx->argDepth = argStart;
        return ifelse_result;
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
    if (t == T_void || (y && y->getType() == T_void))
        return t == T_void ? x : y;

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
        t = den->getType();
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
                               prepare_global(jldiverr_var), ctx);

        return builder.CreateSDiv(x, den);
    HANDLE(udiv_int,2)
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               prepare_global(jldiverr_var), ctx);
        return builder.CreateUDiv(JL_INT(x), den);

    HANDLE(srem_int,2)
        return emit_srem(JL_INT(x), JL_INT(y), ctx);

    HANDLE(urem_int,2)
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               prepare_global(jldiverr_var), ctx);
        return builder.CreateURem(JL_INT(x), den);

    HANDLE(smod_int,2)
        return emit_smod(JL_INT(x), JL_INT(y), ctx);

// Implements IEEE negate. Unfortunately there is no compliant way
// to implement this in LLVM 3.4, though there are two different idioms
// that do the correct thing on LLVM <= 3.3 and >= 3.5 respectively.
// See issue #7868
#ifdef LLVM35
    HANDLE(neg_float,1) return builder.CreateFSub(ConstantFP::get(FT(t), -0.0), FP(x));
#else
    HANDLE(neg_float,1) return builder.CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
#endif
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
        assert(ix->getType() == iy->getType());
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
                                       ArrayRef<Type*>(ix->getType())),
             ix, iy);
        Value *obit = builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        raise_exception_if(obit, prepare_global(jlovferr_var), ctx);
        return builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
    }

    HANDLE(check_top_bit,1)
        // raise InexactError if argument's top bit is set
        x = JL_INT(x);
        raise_exception_if(builder.
                           CreateTrunc(builder.
                                       CreateLShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits()-1)),
                                       T_int1),
                           prepare_global(jlinexacterr_var), ctx);
        return x;

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

    HANDLE(nan_dom_err,2) {
        // nan_dom_err(f, x) throw DomainError if isnan(f)&&!isnan(x)
        Value *f = FP(x); x = FP(y);
        raise_exception_unless(builder.CreateOr(builder.CreateFCmpORD(f,f),
                                                builder.CreateFCmpUNO(x,x)),
                               prepare_global(jldomerr_var), ctx);
        return f;
    }

    HANDLE(abs_float,1)
    {
        x = FP(x);
#ifdef LLVM34
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::fabs,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
#else
        Type *intt = JL_INTT(x->getType());
        Value *bits = builder.CreateBitCast(FP(x), intt);
        Value *absbits =
            builder.CreateAnd(bits,
                              ConstantInt::get(intt, APInt::getSignedMaxValue(((IntegerType*)intt)->getBitWidth())));
        return builder.CreateBitCast(absbits, x->getType());
#endif
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
    HANDLE(ceil_llvm,1) {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    HANDLE(floor_llvm,1) {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::floor,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    HANDLE(trunc_llvm,1) {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    HANDLE(rint_llvm,1) {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::rint,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    HANDLE(sqrt_llvm,1) {
        x = FP(x);
        raise_exception_unless(builder.CreateFCmpUGE(x, ConstantFP::get(x->getType(),0.0)),
                               prepare_global(jldomerr_var), ctx);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    HANDLE(powi_llvm,2) {
        x = FP(x);
        y = JL_INT(y);
        Type *tx = x->getType();
#ifdef LLVM36
        Type *ts[1] = { tx };
        return builder.CreateCall2(Intrinsic::getDeclaration(jl_Module, Intrinsic::powi,
                                                             ArrayRef<Type*>(ts)),
                                   x, y);
#else
        // issue #6506
        Type *ts[2] = { tx, tx };
        return builder.
            CreateCall2(jl_Module->getOrInsertFunction(tx==T_float64 ? "pow" : "powf",
                                                       FunctionType::get(tx, ts, false)),
                        x, builder.CreateSIToFP(y, tx));
#endif
    }
    default:
        assert(false);
    }
    assert(false);
    return NULL;
}

#undef HANDLE

static Function *boxfunc_llvm(FunctionType *ft, const std::string &cname,
                              void *addr, Module *m)
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

#define BOX_F(ct,jl_ct)                                                       \
    box_##ct##_func = boxfunc_llvm(ft1arg(jl_pvalue_llvmt, T_##jl_ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct, m);

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
    ADD_I(fpiseq); ADD_I(fpislt);
    ADD_I(and_int); ADD_I(or_int); ADD_I(xor_int); ADD_I(not_int);
    ADD_I(shl_int); ADD_I(lshr_int); ADD_I(ashr_int); ADD_I(bswap_int);
    ADD_I(ctpop_int); ADD_I(ctlz_int); ADD_I(cttz_int);
    ADD_I(sext_int); ADD_I(zext_int); ADD_I(trunc_int);
    ADD_I(fptoui); ADD_I(fptosi);
    ADD_I(uitofp); ADD_I(sitofp);
    ADD_I(fptrunc); ADD_I(fpext);
    ADD_I(abs_float); ADD_I(copysign_float);
    ADD_I(flipsign_int); ADD_I(select_value);
    ADD_I(ceil_llvm); ADD_I(floor_llvm); ADD_I(trunc_llvm); ADD_I(rint_llvm);
    ADD_I(sqrt_llvm); ADD_I(powi_llvm);
    ADD_I(pointerref); ADD_I(pointerset); ADD_I(pointertoref);
    ADD_I(checked_sadd); ADD_I(checked_uadd);
    ADD_I(checked_ssub); ADD_I(checked_usub);
    ADD_I(checked_smul); ADD_I(checked_umul);
    ADD_I(checked_fptosi); ADD_I(checked_fptoui);
    ADD_I(checked_trunc_sint);
    ADD_I(checked_trunc_uint);
    ADD_I(check_top_bit);
    ADD_I(nan_dom_err);
    ADD_I(ccall); ADD_I(cglobal);
    ADD_I(jl_alloca);
    ADD_I(llvmcall);
}
