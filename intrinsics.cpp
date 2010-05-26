namespace JL_I {
    enum intrinsic {
        // wrap and unwrap
        boxui8=0, boxsi8, boxui16, boxsi16, boxui32, boxsi32, boxui64, boxsi64,
        boxf32, boxf64,
        unbox8, unbox16, unbox32, unbox64,
        // arithmetic
        neg_int, add_int, sub_int, mul_int, sdiv_int, udiv_int,
        smod_int, umod_int,
        neg_float, add_float, sub_float, mul_float, div_float,
        // comparison
        eq_int, slt_int, ult_int,
        eq_float, lt_float, ne_float,
        // bitwise operators
        and_int, or_int, xor_int, not_int, shl_int, lshr_int, ashr_int,
        // conversion
        sext16, zext16, sext32, zext32, sext64, zext64,
        trunc8, trunc16, trunc32,
        fptoui8, fptosi8, fptoui16, fptosi16, fptoui32, fptosi32,
        fptoui64, fptosi64, 
        uitofp32, sitofp32, uitofp64, sitofp64,
        fptrunc32, fpext64,
        // functions
        sqrt_float, powi_float, sin_float, cos_float, pow_float,
        // c interface
        ccall,
    };
};

using namespace JL_I;

static Function *box_int8_func;
static Function *box_uint8_func;
static Function *box_int16_func;
static Function *box_uint16_func;
static Function *box_int32_func;
static Function *box_uint32_func;
static Function *box_int64_func;
static Function *box_uint64_func;
static Function *box_float32_func;
static Function *box_float64_func;
static Function *box_pointer_func;

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

static int is_unsigned(Value *v)
{
    const char *name = v->getName().data();
    return (!strncmp(name, "$u", 2));
}

static Value *mark_unsigned(Value *v)
{
    if (is_unsigned(v)) return v;
    // use value name as a hack to tag as unsigned
    Value *x = builder.CreateBitCast(v, v->getType());
    x->setName("$u");
    return x;
}

static const Type *julia_type_to_llvm(jl_value_t *jt)
{
    if (jt == (jl_value_t*)jl_int8_type || jt == (jl_value_t*)jl_uint8_type) return T_int8;
    if (jt == (jl_value_t*)jl_int16_type || jt == (jl_value_t*)jl_uint16_type) return T_int16;
    if (jt == (jl_value_t*)jl_int32_type || jt == (jl_value_t*)jl_uint32_type) return T_int32;
    if (jt == (jl_value_t*)jl_int64_type || jt == (jl_value_t*)jl_uint64_type) return T_int64;
    if (jt == (jl_value_t*)jl_float32_type) return T_float32;
    if (jt == (jl_value_t*)jl_float64_type) return T_float64;
    if (jt == (jl_value_t*)jl_bottom_type) return T_void;
    if (jt == (jl_value_t*)jl_null) return T_void;
    if (jl_is_bits_type(jt) && jl_is_cpointer_type(jt)) {
        const Type *lt = julia_type_to_llvm(jl_tparam0(jt));
        if (lt == T_void)
            lt = T_pint8;
        return PointerType::get(lt, 0);
    }
    if (jt == (jl_value_t*)jl_any_type)
        return jl_pvalue_llvmt;
    jl_errorf("cannot convert type %s to a native type",
              jl_print_to_string(jt));
    return NULL;
}

static jl_value_t *llvm_type_to_julia(const Type *t)
{
    if (t == T_int8)  return (jl_value_t*)jl_int8_type;
    if (t == T_int16) return (jl_value_t*)jl_int16_type;
    if (t == T_int32) return (jl_value_t*)jl_int32_type;
    if (t == T_int64) return (jl_value_t*)jl_int64_type;
    if (t == T_float32) return (jl_value_t*)jl_float32_type;
    if (t == T_float64) return (jl_value_t*)jl_float64_type;
    if (t == T_void) return (jl_value_t*)jl_bottom_type;
    if (t->isPointerTy()) {
        jl_value_t *elty = llvm_type_to_julia(t->getContainedType(0));
        return (jl_value_t*)jl_apply_type_ctor(jl_pointer_typector,
                                               jl_tuple(1, elty));
    }
    if (t == jl_pvalue_llvmt)
        return (jl_value_t*)jl_any_type;
    jl_errorf("cannot convert type %s to a julia type",
              t->getDescription().c_str());
    return NULL;
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(Value *v)
{
    const Type *t = v->getType();
    if (t == jl_pvalue_llvmt)
        return v;
    if (is_unsigned(v)) {
        if (t == T_int8)  return builder.CreateCall(box_uint8_func, v);
        if (t == T_int16) return builder.CreateCall(box_uint16_func, v);
        if (t == T_int32) return builder.CreateCall(box_uint32_func, v);
        if (t == T_int64) return builder.CreateCall(box_uint64_func, v);
    }
    else {
        if (t == T_int8)  return builder.CreateCall(box_int8_func, v);
        if (t == T_int16) return builder.CreateCall(box_int16_func, v);
        if (t == T_int32) return builder.CreateCall(box_int32_func, v);
        if (t == T_int64) return builder.CreateCall(box_int64_func, v);
        if (t == T_float32) return builder.CreateCall(box_float32_func, v);
        if (t == T_float64) return builder.CreateCall(box_float64_func, v);
    }
    if (t->isPointerTy()) {
        jl_value_t *jt = llvm_type_to_julia(t);
        return builder.CreateCall2(box_pointer_func,
                                   literal_pointer_val(jt),
                                   builder.CreateBitCast(v, T_pint8));
    }
    assert("Don't know how to box this type" && false);
    return NULL;
}

static Value *bitstype_pointer(Value *x)
{
    return builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                             ConstantInt::get(T_int32, 1));
}

static Function *value_to_pointer_func;

// TODO: per-thread
static char *temp_arg_area;
static const uint32_t arg_area_sz = 4196;
static uint32_t arg_area_loc;
static Function *save_arg_area_loc_func;
static Function *restore_arg_area_loc_func;

static uint32_t save_arg_area_loc() { return arg_area_loc; }
static void restore_arg_area_loc(uint32_t l) { arg_area_loc = l; }

static Value *get_saved_arg_area_loc(jl_codectx_t *ctx)
{
    if (ctx->last_arg_area_loc == NULL) {
        ctx->last_arg_area_loc = builder.CreateAlloca(T_uint32);
    }
    return ctx->last_arg_area_loc;
}

static void *alloc_temp_arg_space(uint32_t sz)
{
    void *p;
    if (arg_area_loc+sz > arg_area_sz) {
        p = allocb(sz);
    }
    else {
        p = &temp_arg_area[arg_area_loc];
        arg_area_loc += sz;
    }
    return p;
}

static void *alloc_temp_arg_copy(void *obj, uint32_t sz)
{
    void *p = alloc_temp_arg_space(sz);
    memcpy(p, obj, sz);
    return p;
}

extern "C" void *jl_value_to_pointer(jl_value_t *jt, jl_value_t *v, int argn)
{
    // this is a custom version of convert_to_ptr that is able to use
    // the temporary argument space.
    if (v == (jl_value_t*)jl_null)
        return NULL;
    if (jl_is_cpointer(v))
        return jl_unbox_pointer(v);
    if ((jl_value_t*)jl_typeof(v) == jt) {
        assert(jl_is_bits_type(jt));
        size_t osz = ((jl_bits_type_t*)jt)->nbits/8;
        return alloc_temp_arg_copy(jl_bits_data(v), osz);
    }
    if (jl_is_array(v)) {
        if (jl_tparam0(jl_typeof(v)) == jt)
            return ((jl_array_t*)v)->data;
        if (jl_is_cpointer_type(jt)) {
            jl_array_t *ar = (jl_array_t*)v;
            void **temp=(void**)alloc_temp_arg_space(ar->length*sizeof(void*));
            size_t i;
            for(i=0; i < ar->length; i++) {
                temp[i] = jl_value_to_pointer(jl_tparam0(jt),
                                              jl_arrayref(ar, i), argn);
            }
            return temp;
        }
    }
    jl_errorf("ccall: expected Ptr{%s} as argument %d",
              jl_print_to_string(jt), argn);
    return (jl_value_t*)jl_null;
}

static Value *julia_to_native(const Type *ty, jl_value_t *jt, Value *jv,
                              int argn, jl_codectx_t *ctx)
{
    const Type *vt = jv->getType();
    if (ty == jl_pvalue_llvmt) {
        return boxed(jv);
    }
    else if (ty == vt) {
        return jv;
    }
    else if (vt != jl_pvalue_llvmt) {
        if ((vt->isIntegerTy() || vt->isFloatingPointTy() ||
             vt->isPointerTy()) &&
            (ty->isIntegerTy() || ty->isFloatingPointTy() ||
             ty->isPointerTy())) {
            if (vt->getPrimitiveSizeInBits() ==
                ty->getPrimitiveSizeInBits()) {
                return builder.CreateBitCast(jv, ty);
            }
            else {
                // error. box for error handling.
                jv = boxed(jv);
            }
        }
        else {
            assert(false && "Unsupported native type.");
        }
    }
    else if (jl_is_cpointer_type(jt)) {
        Value *p = builder.CreateCall3(value_to_pointer_func,
                                       literal_pointer_val(jl_tparam0(jt)), jv,
                                       ConstantInt::get(T_int32, argn));
        assert(ty->isPointerTy());
        return builder.CreateBitCast(p, ty);
    }
    assert(jl_is_bits_type(jt));
    std::stringstream msg;
    msg << "ccall: expected ";
    msg << std::string(jl_print_to_string(jt));
    msg << " as argument ";
    msg << argn;
    emit_typecheck(jv, jt, msg.str(), ctx);
    Value *p = bitstype_pointer(jv);
    return builder.CreateLoad(builder.CreateBitCast(p,
                                                    PointerType::get(ty,0)),
                              false);
}

static bool is_unsigned_julia_type(jl_value_t *t)
{
    return (t==(jl_value_t*)jl_uint8_type  || t==(jl_value_t*)jl_uint16_type ||
            t==(jl_value_t*)jl_uint32_type || t==(jl_value_t*)jl_uint64_type);
}

static bool is_punsigned_julia_type(jl_value_t *t)
{
    return is_unsigned_julia_type(t) ||
        (jl_is_cpointer_type(t) && is_punsigned_julia_type(jl_tparam0(t)));
}

// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *ptr = jl_interpret_toplevel_expr(args[1]);
    jl_value_t *rt  = jl_interpret_toplevel_expr(args[2]);
    jl_value_t *at  = jl_interpret_toplevel_expr(args[3]);
    JL_TYPECHK(ccall, cpointer, ptr);
    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, tuple, at);
    JL_TYPECHK(ccall, type, at);
    jl_tuple_t *tt = (jl_tuple_t*)at;
    if (tt->length != nargs-3)
        jl_error("ccall: wrong number of arguments to C function");
    void *fptr = *(void**)jl_bits_data(ptr);
    std::vector<const Type *> fargt(0);
    const Type *lrt = julia_type_to_llvm(rt);
    size_t i;
    bool haspointers = false;
    for(i=0; i < tt->length; i++) {
        const Type *t = julia_type_to_llvm(jl_tupleref(tt,i));
        haspointers = haspointers || (t->isPointerTy() && t!=jl_pvalue_llvmt);
        fargt.push_back(t);
    }
    // make LLVM function object for the target
    Function *llvmf =
        Function::Create(FunctionType::get(lrt, fargt, false),
                         Function::ExternalLinkage,
                         "ccall_", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(llvmf, fptr);

    // save temp argument area stack pointer
    if (haspointers) {
        Value *saveloc = get_saved_arg_area_loc(ctx);
        Value *aalval = builder.CreateCall(save_arg_area_loc_func);
        builder.CreateStore(aalval, saveloc);
    }

    // emit arguments
    std::vector<Value*> argvals(0);
    for(i=4; i < nargs+1; i++) {
        Value *arg = emit_expr(args[i], ctx, true);
        argvals.push_back(julia_to_native(fargt[i-4], jl_tupleref(tt,i-4),
                                          arg, i-3, ctx));
    }
    // the actual call
    Value *result = builder.CreateCall(llvmf, argvals.begin(), argvals.end());

    // restore temp argument area stack pointer
    if (haspointers) {
        Value *saveloc = get_saved_arg_area_loc(ctx);
        Value *lastval = builder.CreateLoad(saveloc);
        builder.CreateCall(restore_arg_area_loc_func, lastval);
    }

    if (is_unsigned_julia_type(rt))
        return mark_unsigned(result);
    if (lrt == T_void)
        return literal_pointer_val((jl_value_t*)jl_null);
    if (jl_is_cpointer_type(rt) && is_punsigned_julia_type(rt))
        return builder.CreateCall2(box_pointer_func,
                                   literal_pointer_val(rt),
                                   builder.CreateBitCast(result,T_pint8));
    return result;
}

// convert int type to same-size float type
static const Type *FT(const Type *t)
{
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

static Value *uint_cnvt(const Type *to, Value *x)
{
    const Type *t = x->getType();
    if (t == to) return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return builder.CreateTrunc(x, to);
    return builder.CreateZExt(x, to);
}

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    if (f == ccall) return emit_ccall(args, nargs, ctx);
    if (nargs < 1) jl_error("invalid intrinsic call");
    Value *x = emit_expr(args[1], ctx, true);
    const Type *t = x->getType();
    const Type *fxt;
    const Type *fxts[2];
    Value *p, *fx, *fy;
    switch (f) {
    HANDLE(boxui8,1)
        if (t != T_int8) x = builder.CreateBitCast(x, T_int8);
        return mark_unsigned(x);
    HANDLE(boxsi8,1)
        if (t != T_int8) x = builder.CreateBitCast(x, T_int8);
        return x;
    HANDLE(boxui16,1)
        if (t != T_int16) x = builder.CreateBitCast(x, T_int16);
        return mark_unsigned(x);
    HANDLE(boxsi16,1)
        if (t != T_int16) x = builder.CreateBitCast(x, T_int16);
        return x;
    HANDLE(boxui32,1)
        if (t != T_int32) x = builder.CreateBitCast(x, T_int32);
        return mark_unsigned(x);
    HANDLE(boxsi32,1)
        if (t != T_int32) x = builder.CreateBitCast(x, T_int32);
        return x;
    HANDLE(boxui64,1)
        if (t != T_int64) x = builder.CreateBitCast(x, T_int64);
        return mark_unsigned(x);
    HANDLE(boxsi64,1)
        if (t != T_int64) x = builder.CreateBitCast(x, T_int64);
        return x;
    HANDLE(boxf32,1)
        if (t != T_float32) x = builder.CreateBitCast(x, T_float32);
        return x;
    HANDLE(boxf64,1)
        if (t != T_float64) x = builder.CreateBitCast(x, T_float64);
        return x;
    HANDLE(unbox8,1)
        p = bitstype_pointer(x);
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint8),false);
    HANDLE(unbox16,1)
        p = bitstype_pointer(x);
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint16),false);
    HANDLE(unbox32,1)
        p = bitstype_pointer(x);
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint32),false);
    HANDLE(unbox64,1)
        p = bitstype_pointer(x);
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint64),false);
    HANDLE(neg_int,1)
        return builder.CreateSub(ConstantInt::get(t, 0), x);
    HANDLE(add_int,2)
        return builder.CreateAdd(x, emit_expr(args[2],ctx,true));
    HANDLE(sub_int,2)
        return builder.CreateSub(x, emit_expr(args[2],ctx,true));
    HANDLE(mul_int,2)
        return builder.CreateMul(x, emit_expr(args[2],ctx,true));
    HANDLE(sdiv_int,2)
        return builder.CreateSDiv(x, emit_expr(args[2],ctx,true));
    HANDLE(udiv_int,2)
        return builder.CreateUDiv(x, emit_expr(args[2],ctx,true));
    HANDLE(smod_int,2)
        return builder.CreateSRem(x, emit_expr(args[2],ctx,true));
    HANDLE(umod_int,2)
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
    HANDLE(eq_int,2)
        return julia_bool(builder.CreateICmpEQ(x,
                                               emit_expr(args[2],ctx,true)));
    HANDLE(slt_int,2)
        return julia_bool(builder.CreateICmpSLT(x,
                                                emit_expr(args[2],ctx,true)));
    HANDLE(ult_int,2)
        return julia_bool(builder.CreateICmpULT(x,
                                                emit_expr(args[2],ctx,true)));
    HANDLE(eq_float,2)
        return
        julia_bool(builder.CreateFCmpOEQ(FP(x),
                                         FP(emit_expr(args[2],ctx,true))));
    HANDLE(lt_float,2)
        return
        julia_bool(builder.CreateFCmpOLT(FP(x),
                                         FP(emit_expr(args[2],ctx,true))));
    HANDLE(ne_float,2)
        return
        julia_bool(builder.CreateFCmpONE(FP(x),
                                         FP(emit_expr(args[2],ctx,true))));
    HANDLE(and_int,2)
        return builder.CreateAnd(x, emit_expr(args[2],ctx,true));
    HANDLE(or_int,2)
        return builder.CreateOr(x, emit_expr(args[2],ctx,true));
    HANDLE(xor_int,2)
        return builder.CreateXor(x, emit_expr(args[2],ctx,true));
    HANDLE(not_int,1)
        return builder.CreateXor(x, ConstantInt::get(t, -1));
    HANDLE(shl_int,2)
        return builder.CreateShl(x, uint_cnvt(t,emit_expr(args[2],ctx,true)));
    HANDLE(lshr_int,2)
        return builder.CreateLShr(x, uint_cnvt(t,emit_expr(args[2],ctx,true)));
    HANDLE(ashr_int,2)
        return builder.CreateAShr(x, uint_cnvt(t,emit_expr(args[2],ctx,true)));
    HANDLE(sext16,1)
        return builder.CreateSExt(x, T_int16);
    HANDLE(zext16,1)
        return builder.CreateZExt(x, T_int16);
    HANDLE(sext32,1)
        return builder.CreateSExt(x, T_int32);
    HANDLE(zext32,1)
        return builder.CreateZExt(x, T_int32);
    HANDLE(sext64,1)
        return builder.CreateSExt(x, T_int64);
    HANDLE(zext64,1)
        return builder.CreateZExt(x, T_int64);
    HANDLE(trunc8,1)
        return builder.CreateTrunc(x, T_int8);
    HANDLE(trunc16,1)
        return builder.CreateTrunc(x, T_int16);
    HANDLE(trunc32,1)
        return builder.CreateTrunc(x, T_int32);
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
        return builder.CreateFPExt(FP(x), T_float64);
    HANDLE(sqrt_float,1)
        fx = FP(x);
        fxt = fx->getType();
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                            Intrinsic::sqrt,
                                                            &fxt, 1),
                                  fx);
    HANDLE(sin_float,1)
        fx = FP(x);
        fxt = fx->getType();
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                            Intrinsic::sin,
                                                            &fxt, 1),
                                  fx);
    HANDLE(cos_float,1)
        fx = FP(x);
        fxt = fx->getType();
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                            Intrinsic::cos,
                                                            &fxt, 1),
                                  fx);
    HANDLE(pow_float,2)
        fx = FP(x);
        fy = FP(emit_expr(args[2],ctx,true));
        fxts[0] = fx->getType(); fxts[1] = fy->getType();
        if (fxts[0] != fxts[1] ||
            !fxts[0]->isFloatingPointTy() || !fxts[1]->isFloatingPointTy())
            jl_error("invalid arguments to pow_float");
        return builder.CreateCall2(Intrinsic::getDeclaration(jl_Module,
                                                             Intrinsic::pow,
                                                             fxts, 2),
                                   fx, fy);
    HANDLE(powi_float,2)
        fx = FP(x);
        fy = emit_expr(args[2],ctx,true);
        fxts[0] = fx->getType(); fxts[1] = fy->getType();
        if (!fxts[0]->isFloatingPointTy() || fxts[1] != T_int32)
            jl_error("invalid arguments to powi_float");
        return builder.CreateCall2(Intrinsic::getDeclaration(jl_Module,
                                                             Intrinsic::powi,
                                                             fxts, 2),
                                   fx, fy);
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

static FunctionType *ft1arg(const Type *ret, const Type *arg)
{
    std::vector<const Type*> args1(0);
    args1.push_back(arg);
    return FunctionType::get(ret, args1, false);
}

static void add_intrinsic(const std::string &name, intrinsic f)
{
    jl_value_t *i = jl_new_box_uint32((uint32_t)f);
    i->type = (jl_type_t*)jl_intrinsic_type;
    jl_set_const(jl_system_module, jl_symbol((char*)name.c_str()), i);
}

#define ADD_I(name) add_intrinsic(#name, name)
#define BOX_F(ct)                                                       \
    box_##ct##_func = boxfunc_llvm(ft1arg(jl_pvalue_llvmt, T_##ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct);

extern "C" void jl_init_intrinsic_functions()
{
    ADD_I(boxui8); ADD_I(boxsi8); ADD_I(boxui16); ADD_I(boxsi16);
    ADD_I(boxui32); ADD_I(boxsi32); ADD_I(boxui64); ADD_I(boxsi64);
    ADD_I(boxf32); ADD_I(boxf64);
    ADD_I(unbox8); ADD_I(unbox16); ADD_I(unbox32); ADD_I(unbox64);
    ADD_I(neg_int); ADD_I(add_int); ADD_I(sub_int); ADD_I(mul_int);
    ADD_I(sdiv_int); ADD_I(udiv_int); ADD_I(smod_int); ADD_I(umod_int);
    ADD_I(neg_float); ADD_I(add_float); ADD_I(sub_float);
    ADD_I(mul_float); ADD_I(div_float);
    ADD_I(eq_int); ADD_I(slt_int); ADD_I(ult_int);
    ADD_I(eq_float); ADD_I(lt_float); ADD_I(ne_float);
    ADD_I(and_int); ADD_I(or_int); ADD_I(xor_int); ADD_I(not_int);
    ADD_I(shl_int); ADD_I(lshr_int); ADD_I(ashr_int);
    ADD_I(sext16); ADD_I(zext16); ADD_I(sext32); ADD_I(zext32);
    ADD_I(sext64); ADD_I(zext64);
    ADD_I(trunc8); ADD_I(trunc16); ADD_I(trunc32);
    ADD_I(fptoui8); ADD_I(fptosi8);
    ADD_I(fptoui16); ADD_I(fptosi16); ADD_I(fptoui32); ADD_I(fptosi32);
    ADD_I(fptoui64); ADD_I(fptosi64);
    ADD_I(uitofp32); ADD_I(sitofp32); ADD_I(uitofp64); ADD_I(sitofp64);
    ADD_I(fptrunc32); ADD_I(fpext64);
    ADD_I(sqrt_float); ADD_I(powi_float); ADD_I(pow_float);
    ADD_I(sin_float); ADD_I(cos_float);
    ADD_I(ccall);
    
    BOX_F(int8);  BOX_F(uint8);
    BOX_F(int16); BOX_F(uint16);
    BOX_F(int32); BOX_F(uint32);
    BOX_F(int64); BOX_F(uint64);
    BOX_F(float32); BOX_F(float64);

    std::vector<const Type*> boxpointerargs(0);
    boxpointerargs.push_back(jl_pvalue_llvmt);
    boxpointerargs.push_back(T_pint8);
    box_pointer_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt,
                                           boxpointerargs, false),
                         Function::ExternalLinkage, "jl_box_pointer",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(box_pointer_func,
                                         (void*)&jl_box_pointer);

    std::vector<const Type*> toptrargs(0);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(T_int32);
    value_to_pointer_func =
        Function::Create(FunctionType::get(T_pint8, toptrargs, false),
                         Function::ExternalLinkage, "jl_value_to_pointer",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(value_to_pointer_func,
                                         (void*)&jl_value_to_pointer);

    temp_arg_area = (char*)allocb(arg_area_sz);
    arg_area_loc = 0;

    std::vector<const Type*> noargs(0);
    save_arg_area_loc_func =
        Function::Create(FunctionType::get(T_uint32, noargs, false),
                         Function::ExternalLinkage, "save_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(save_arg_area_loc_func,
                                         (void*)&save_arg_area_loc);

    restore_arg_area_loc_func =
        Function::Create(ft1arg(T_void, T_uint32),
                         Function::ExternalLinkage, "restore_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(restore_arg_area_loc_func,
                                         (void*)&restore_arg_area_loc);
}
