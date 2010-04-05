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
        // conversion
        sext16, zext16, sext32, zext32, sext64, zext64,
        trunc8, trunc16, trunc32,
        fptoui8, fptosi8, fptoui16, fptosi16, fptoui32, fptosi32,
        fptoui64, fptosi64, 
        uitofp32, sitofp32, uitofp64, sitofp64,
        fptrunc32, fpext64,
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

static Value *mark_unsigned(Value *v)
{
    // use value name as a hack to tag as unsigned
    Value *x = builder.CreateBitCast(v, v->getType());
    x->setName("$u");
    return x;
}

static int is_unsigned(Value *v)
{
    const char *name = v->getName().data();
    return (!strncmp(name, "$u", 2));
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(Value *v)
{
    const Type *t = v->getType();
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
    return v;
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
    if (v->getType()->isFloatingPoint())
        return v;
    return builder.CreateBitCast(v, FT(v->getType()));
}

#define HANDLE(intr,n)                                                  \
    case intr: if (nargs!=n) jl_error(#intr": wrong number of arguments");

static Value *emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                             jl_codectx_t *ctx)
{
    if (nargs < 1) jl_error("invalid intrinsic call");
    Value *x = emit_expr(args[1], ctx, true);
    const Type *t = x->getType();
    Value *p;
    switch (f) {
    HANDLE(boxui8,1)
        assert(t == T_int8);
        return mark_unsigned(x);
    HANDLE(boxsi8,1)
        assert(t == T_int8);
        return x;
    HANDLE(boxui16,1)
        assert(t == T_int16);
        return mark_unsigned(x);
    HANDLE(boxsi16,1)
        assert(t == T_int16);
        return x;
    HANDLE(boxui32,1)
        assert(t == T_int32);
        return mark_unsigned(x);
    HANDLE(boxsi32,1)
        assert(t == T_int32);
        return x;
    HANDLE(boxui64,1)
        assert(t == T_int64);
        return mark_unsigned(x);
    HANDLE(boxsi64,1)
        assert(t == T_int64);
        return x;
    HANDLE(boxf32,1)
        if (t == T_float32) return x;
        assert(t == T_int32);
        return builder.CreateBitCast(x, T_float32);
    HANDLE(boxf64,1)
        if (t == T_float64) return x;
        assert(t == T_int64);
        return builder.CreateBitCast(x, T_float64);
    HANDLE(unbox8,1)
        p = builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                              ConstantInt::get(T_int32, 1));
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint8),false);
    HANDLE(unbox16,1)
        p = builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                              ConstantInt::get(T_int32, 1));
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint16),false);
    HANDLE(unbox32,1)
        p = builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                              ConstantInt::get(T_int32, 1));
        return builder.CreateLoad(builder.CreateBitCast(p,T_pint32),false);
    HANDLE(unbox64,1)
        p = builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                              ConstantInt::get(T_int32, 1));
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
    default:
        assert(false);
    }
    assert(false);
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
    jl_value_t *i = jl_box_uint32((uint32_t)f);
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
    ADD_I(sext16); ADD_I(zext16); ADD_I(sext32); ADD_I(zext32);
    ADD_I(sext64); ADD_I(zext64);
    ADD_I(trunc8); ADD_I(trunc16); ADD_I(trunc32);
    ADD_I(fptoui8); ADD_I(fptosi8);
    ADD_I(fptoui16); ADD_I(fptosi16); ADD_I(fptoui32); ADD_I(fptosi32);
    ADD_I(fptoui64); ADD_I(fptosi64);
    ADD_I(uitofp32); ADD_I(sitofp32); ADD_I(uitofp64); ADD_I(sitofp64);
    ADD_I(fptrunc32); ADD_I(fpext64);
    
    BOX_F(int8);  BOX_F(uint8);
    BOX_F(int16); BOX_F(uint16);
    BOX_F(int32); BOX_F(uint32);
    BOX_F(int64); BOX_F(uint64);
    BOX_F(float32); BOX_F(float64);
}
