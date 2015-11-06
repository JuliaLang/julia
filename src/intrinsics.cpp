// This file is a part of Julia. License is MIT: http://julialang.org/license

namespace JL_I {
#include "intrinsics.h"
}

#include "ccall.cpp"

using namespace JL_I;
static Function *runtime_func[num_intrinsics];
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
    args3.push_back(T_pjlvalue);

#define ADD_I(name, nargs) do { \
        Function *func = Function::Create(FunctionType::get(T_pjlvalue, args##nargs, false), \
                                          Function::ExternalLinkage, "jl_"#name, m); \
        runtime_func[name] = func; \
        add_named_global(func, (void*)&jl_##name); \
    } while (0);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) runtime_func[alias] = runtime_func[base];
    ADD_HIDDEN(reinterpret, 2);
    INTRINSICS
#undef ADD_I
#undef ADD_HIDDEN
#undef ALIAS
}

/*
  low-level intrinsics design: TODO: fix description below
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
    if (t == T_float64) return T_int64;
    assert(t == T_void);
    return T_void;
}
// convert float type to same-size int type (as a Julia type)
static jl_value_t *JL_JLUINTT(Type *t)
{
    assert(!t->isIntegerTy());
    if (t == T_float32) return (jl_value_t*)jl_uint32_type;
    if (t == T_float64) return (jl_value_t*)jl_uint64_type;
    if (t == Type::getHalfTy(jl_LLVMContext)) return (jl_value_t*)jl_uint16_type;
    assert(t == T_void);
    return jl_bottom_type;
}
static jl_value_t *JL_JLSINTT(Type *t)
{
    assert(!t->isIntegerTy());
    if (t == T_float32) return (jl_value_t*)jl_int32_type;
    if (t == T_float64) return (jl_value_t*)jl_int64_type;
    if (t == Type::getHalfTy(jl_LLVMContext)) return (jl_value_t*)jl_int16_type;
    assert(t == T_void);
    return jl_bottom_type;
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

#define LLVM_FP(a,b) APFloat(a,b)
static Constant *julia_const_to_llvm(jl_value_t *e, bool nested=false)
{
    jl_value_t *jt = jl_typeof(e);
    jl_datatype_t *bt = (jl_datatype_t*)jt;

    if (!jl_is_datatype(bt) || bt == jl_gensym_type)
        return NULL;

    if (e == jl_true)
        return ConstantInt::get(T_int1, 1);
    if (e == jl_false)
        return ConstantInt::get(T_int1, 0);

    if (jl_is_cpointer_type(jt))
        return ConstantExpr::getIntToPtr(ConstantInt::get(T_size, jl_unbox_long(e)), julia_type_to_llvm((jl_value_t*)bt));
    if (jl_is_bitstype(jt)) {
        int nb = jl_datatype_size(bt);
        //TODO: non-power-of-2 size datatypes may not be interpreted correctly on big-endian systems
        switch (nb) {
        case 1: {
            uint8_t data8 = *(uint8_t*)jl_data_ptr(e);
            return ConstantInt::get(T_int8, data8);
        }
        case 2: {
            uint16_t data16 = *(uint16_t*)jl_data_ptr(e);
#ifndef DISABLE_FLOAT16
            if (jl_is_float(e)) {
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEhalf,APInt(16,data16)));
            }
#endif
            return ConstantInt::get(T_int16, data16);
        }
        case 4: {
            uint32_t data32 = *(uint32_t*)jl_data_ptr(e);
            if (jl_is_float(e)) {
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEsingle,APInt(32,data32)));
            }
            return ConstantInt::get(T_int32, data32);
        }
        case 8: {
            uint64_t data64 = *(uint64_t*)jl_data_ptr(e);
            if (jl_is_float(e)) {
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEdouble,APInt(64,data64)));
            }
            return ConstantInt::get(T_int64, data64);
        }
        default:
            size_t nw = (nb+sizeof(uint64_t)-1)/sizeof(uint64_t);
            uint64_t *data = (uint64_t*)jl_data_ptr(e);
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
            if (nb == 16 && jl_is_float(e)) {
                return ConstantFP::get(jl_LLVMContext,LLVM_FP(APFloat::IEEEquad,val));
                // If we have a floating point type that's not hardware supported, just treat it like an integer for LLVM purposes
            }
            return ConstantInt::get(IntegerType::get(jl_LLVMContext,8*nb),val);
        }
    }
    if (jl_isbits(jt)) {
        size_t nf = jl_datatype_nfields(bt), i;
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
                val = julia_const_to_llvm(f, true);
            if (val == NULL) {
                JL_GC_POP();
                return NULL;
            }
            fields[llvm_nf++] = val;
        }
        JL_GC_POP();

        Type *t = julia_struct_to_llvm(jt, NULL);
        if (type_is_ghost(t))
            return UndefValue::get(NoopType);
        if (t->isVectorTy())
            return ConstantVector::get(ArrayRef<Constant*>(fields,llvm_nf));
        if (t->isStructTy()) {
            StructType *st = dyn_cast<StructType>(t);
            assert(st);
            return ConstantStruct::get(st, ArrayRef<Constant*>(fields,llvm_nf));
        }
        else {
            assert(t->isArrayTy());
            ArrayType *at = dyn_cast<ArrayType>(t);
            assert(at);
            return ConstantArray::get(at, ArrayRef<Constant*>(fields,llvm_nf));
        }
    }
    return NULL;
}

static jl_cgval_t emit_unboxed(jl_value_t *e, jl_codectx_t *ctx)
{
    Constant *c = julia_const_to_llvm(e);
    if (c) return mark_julia_type(c, false, expr_type(e, ctx));
    return emit_expr(e, ctx, false);
}

static jl_cgval_t ghostValue(jl_value_t *ty);

// emit code to unpack a raw value from a box into registers
static Value *emit_unbox(Type *to, const jl_cgval_t &x, jl_value_t *jt)
{
    assert(to != T_pjlvalue);
    // TODO: fully validate that x.typ == jt?
    if (x.isghost) {
        if (type_is_ghost(to)) {
            return NULL;
        }
        //emit_error("emit_unbox: a type mismatch error in occurred during codegen", ctx);
        return UndefValue::get(to); // type mismatch error
    }
    if (!x.isboxed) { // already unboxed, but sometimes need conversion
        Value *unboxed;
        if (x.ispointer) {
            if (jt == (jl_value_t*)jl_bool_type && to != T_int1)
                return builder.CreateZExt(builder.CreateLoad(builder.CreatePointerCast(x.V, T_int1->getPointerTo())), to);
            else
                return builder.CreateLoad(builder.CreatePointerCast(x.V, to->getPointerTo()));
        }
        else {
            unboxed = x.V;
        }
        Type *ty = unboxed->getType();
        // bools are stored internally as int8 (for now)
        if (ty == T_int1 && to == T_int8)
            return builder.CreateZExt(unboxed, T_int8);
        if (ty->isPointerTy() && !to->isPointerTy())
            return builder.CreatePtrToInt(unboxed, to);
        if (!ty->isPointerTy() && to->isPointerTy())
            return builder.CreateIntToPtr(unboxed, to);
        if (ty->isPointerTy() && to->isPointerTy())
            // pointer types are going away anyways, and this can come up in ccall argument conversion
            return builder.CreatePointerCast(unboxed, to);
        if (ty != to) {
            // this can happen when a branch yielding a different type ends
            // up being dead code, and type inference knows that the other
            // branch's type is the only one that matters.
            // assert(ty == T_void);
            //emit_error("emit_unbox: a type mismatch error in occurred during codegen", ctx);
            return UndefValue::get(to); // type mismatch error
        }
        return unboxed;
    }
    Value *p = x.V;
    if (to == T_int1) {
        // bools stored as int8, so an extra Trunc is needed to get an int1
        return builder.CreateTrunc(
                builder.CreateLoad(builder.CreateBitCast(p, T_pint8)),
                T_int1);
    }
    return builder.CreateAlignedLoad(builder.CreateBitCast(p, to->getPointerTo()), 16); // julia's gc gives 16-byte aligned addresses
}

// unbox, trying to determine correct bitstype automatically
// returns some sort of raw, unboxed numeric type (e.g. in registers)
static Value *auto_unbox(const jl_cgval_t &v, jl_codectx_t *ctx)
{
    jl_value_t *bt = v.typ;
    if (!jl_is_bitstype(bt)) {
        // This can be reached with a direct invalid call to an Intrinsic, such as:
        //   Intrinsics.neg_int("")
        emit_error("auto_unbox: unable to determine argument type", ctx);
        return UndefValue::get(T_void);
    }
    bool isboxed;
    Type *to = julia_type_to_llvm(v.typ, &isboxed);
    if (to == NULL || isboxed) {
        // might be some sort of incomplete (but valid) Ptr{T} type, for example
        unsigned int nb = jl_datatype_size(bt)*8;
        to = IntegerType::get(jl_LLVMContext, nb);
    }
    if (type_is_ghost(to)) {
        return NULL;
    }
    assert(!to->isAggregateType()); // expecting some sort of jl_bitstype
    return emit_unbox(to, v, bt);
}
static Value *auto_unbox(jl_value_t *x, jl_codectx_t *ctx)
{
    jl_cgval_t v = emit_unboxed(x, ctx);
    return auto_unbox(v, ctx);
}

static jl_value_t *staticeval_bitstype(jl_value_t *targ, const char *fname, jl_codectx_t *ctx)
{
    // evaluate an argument at compile time to determine what type it is
    // does bitstype validation if and only if fname != NULL
    jl_value_t *et = expr_type(targ, ctx);
    jl_value_t *bt = NULL;
    if (jl_is_type_type(et) && jl_is_leaf_type(jl_tparam0(et))) {
        bt = jl_tparam0(et);
    }
    else {
        JL_TRY { // TODO: change this to an actual call to staticeval rather than actually executing code
            bt = jl_interpret_toplevel_expr_in(ctx->module, targ,
                                               jl_svec_data(ctx->sp),
                                               jl_svec_len(ctx->sp)/2);
        }
        JL_CATCH {
            bt = NULL;
        }
    }
    if (fname && !jl_is_bitstype(bt)) {
        jl_errorf("%s: expected bits type as first argument", fname);
        return NULL;
    }
    return bt;
}

static Type *staticeval_bitstype(jl_value_t *bt)
{
    assert(jl_is_bitstype(bt));
    bool isboxed;
    Type *to = julia_type_to_llvm(bt, &isboxed);
    if (to == NULL || isboxed) {
        unsigned int nb = jl_datatype_size(bt)*8;
        to = IntegerType::get(jl_LLVMContext, nb);
    }
    assert(!to->isAggregateType()); // expecting a bits type
    return to;
}

// figure out how many bits a bitstype has at compile time
int get_bitstype_nbits(jl_value_t *bt)
{
    assert(jl_is_bitstype(bt));
    if (bt == (jl_value_t*)jl_bool_type)
        return 1;
    return jl_datatype_size(bt)*8;
}

// put a bits type tag on some value (despite the name, this doesn't necessarily actually "box" the value however)
static jl_cgval_t generic_box(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    // Examine the first argument //
    jl_value_t *bt = static_eval(targ, ctx, true, true);
    if (bt && !jl_is_leaf_type(bt)) {
        jl_add_linfo_root(ctx->linfo, bt);
    }

    if (!bt || !jl_is_bitstype(bt)) {
        // it's easier to throw a good error from C than llvm
        if (bt) targ = bt;
        int last_depth = ctx->gc.argDepth;
        Value *arg1 = emit_boxed_rooted(targ, ctx).V;
        Value *arg2 = emit_boxed_rooted(x, ctx).V;
        Value *func = prepare_call(runtime_func[reinterpret]);
#ifdef LLVM37
        Value *r = builder.CreateCall(func, {arg1, arg2});
#else
        Value *r = builder.CreateCall2(func, arg1, arg2);
#endif
        ctx->gc.argDepth = last_depth;
        jl_value_t *et = expr_type(targ, ctx);
        return mark_julia_type(r, true, jl_is_type_type(et) ? jl_tparam0(et) : (jl_value_t*)jl_any_type);
    }

    Type *llvmt = staticeval_bitstype(bt);
    int nb = jl_datatype_size(bt);

    // Examine the second argument //
    jl_cgval_t v = emit_unboxed(x, ctx);
    bool isboxed;
    Type *vxt = julia_type_to_llvm(v.typ, &isboxed);

    if (!jl_is_datatype(v.typ)
        || !jl_is_bitstype(v.typ)
        || jl_datatype_size(v.typ) != nb) {
        Value *typ = emit_typeof(v);
        if (!jl_is_bitstype(v.typ)) {
            if (isboxed) {
                Value *isbits = emit_datatype_isbitstype(typ);
                error_unless(isbits, "reinterpret: expected bitstype value for second argument", ctx);
            }
            else {
                emit_error("reinterpet: expected bitstype value for second argument", ctx);
                return jl_cgval_t();
            }
        }
        if (jl_datatype_size(v.typ) != nb) {
            if (isboxed) {
                Value *size = emit_datatype_size(typ);
                error_unless(builder.CreateICmpEQ(size, ConstantInt::get(T_int32, nb)),
                            "reinterpet: argument size does not match size of target type", ctx);
            }
            else {
                emit_error("reinterpet: argument size does not match size of target type", ctx);
                return jl_cgval_t();
            }
        }
    }

    Value *vx = v.V;
    if (v.ispointer) {
        vx = v.V;
        if (isboxed) // try to load as original Type, to preserve llvm optimizations
            vxt = llvmt; // but if the v.typ is not well known, use T
        if (vx->getType()->getPointerElementType() != vxt)
            vx = builder.CreatePointerCast(vx, vxt->getPointerTo());
        vx = builder.CreateLoad(vx);
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
            vx = builder.CreateBitCast(vx, llvmt);
    }

    return mark_julia_type(vx, false, bt);
}

// put a bits type tag on some value
static jl_cgval_t generic_unbox(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    // Examine the first argument //
    jl_value_t *bt = staticeval_bitstype(targ, NULL, ctx);

    // Examine the second argument //
    jl_cgval_t v = emit_unboxed(x, ctx);

    if (bt == NULL || !jl_is_leaf_type(bt)) {
        // dynamically-determined type; evaluate.
        int nb, alignment;
        Type *llvmt;
        if (bt && jl_is_bitstype(bt)) {
            // always fixed size
            nb = jl_datatype_size(bt);
            llvmt = staticeval_bitstype(bt);
            alignment = ((jl_datatype_t*)bt)->alignment;
        }
        else {
            if (!jl_is_leaf_type(v.typ) && !jl_is_bitstype(v.typ)) {
                // TODO: currently doesn't handle the case where the type of neither argument is understood at compile time
                // since codegen has no idea what size it might have
                jl_error("codegen: failed during evaluation of a call to unbox");
                return jl_cgval_t();
            }
            nb = jl_datatype_size(v.typ);
            llvmt = staticeval_bitstype(v.typ);
            alignment = ((jl_datatype_t*)v.typ)->alignment;
        }
        Value *runtime_bt = boxed(emit_expr(targ, ctx), ctx);
        // XXX: emit type validity check on runtime_bt (bitstype of size nb)

        Value *newobj = emit_allocobj(nb);
        builder.CreateStore(runtime_bt, emit_typeptr_addr(newobj));
        if (!v.ispointer)
            builder.CreateAlignedStore(emit_unbox(llvmt, v, v.typ), builder.CreatePointerCast(newobj, llvmt->getPointerTo()), alignment);
        else
            builder.CreateMemCpy(newobj, builder.CreateBitCast(v.V, T_pint8), nb, alignment);
        return mark_julia_type(newobj, true, bt ? bt : (jl_value_t*)jl_any_type);
    }

    if (!jl_is_bitstype(bt)) {
        // TODO: to accept arbitrary types, replace this function with a call to llvm_type_rewrite
        emit_error("unbox: expected bits type as first argument", ctx);
        return jl_cgval_t();
    }

    Type *llvmt = staticeval_bitstype(bt);
    if (v.typ == bt)
        return v;

    Value *vx;
    if (v.ispointer) {
        // TODO: validate the size and type of the pointer contents
        if (v.isimmutable && !v.needsgcroot) { // wrong type, but can lazy load this later as needed
            v.typ = bt;
            v.isboxed = false;
            return v;
        }
        vx = builder.CreateLoad(builder.CreateBitCast(v.V, llvmt->getPointerTo()));
    }
    else {
        vx = v.V;
        if (!jl_is_bitstype(v.typ)) {
            emit_error("unbox: expected bits type value for second argument", ctx);
            return jl_cgval_t();
        }
    }

    Type *vxt = vx->getType();
    if (llvmt == T_int1) {
        vx = builder.CreateTrunc(vx, llvmt);
    }
    else if (vxt == T_int1 && llvmt == T_int8) {
        vx = builder.CreateZExt(vx, llvmt);
    }
    else if (vxt != llvmt) {
        // getPrimitiveSizeInBits() == 0 for pointers
        // PtrToInt and IntToPtr ignore size differences
        if (vxt->getPrimitiveSizeInBits() != llvmt->getPrimitiveSizeInBits() &&
            !(vxt->isPointerTy() && llvmt->getPrimitiveSizeInBits() == sizeof(void*)*8) &&
            !(llvmt->isPointerTy() && vxt->getPrimitiveSizeInBits() == sizeof(void*)*8)) {
            emit_error("unbox: argument is of incorrect size", ctx);
            return jl_cgval_t();
        }
        if (vxt->isPointerTy() && !llvmt->isPointerTy())
            vx = builder.CreatePtrToInt(vx, llvmt);
        else if (!vxt->isPointerTy() && llvmt->isPointerTy())
            vx = builder.CreateIntToPtr(vx, llvmt);
        else
            vx = builder.CreateBitCast(vx, llvmt);
    }

    return mark_julia_type(vx, false, bt);
}

// NOTE: signd (signed) only relevant if check == true
static jl_cgval_t generic_trunc(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx, bool check, bool signd)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "trunc_int", ctx);
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x, ctx));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateTrunc(ix, to);
    if (check) {
        Value *back = signd ? builder.CreateSExt(ans, ix->getType()) :
            builder.CreateZExt(ans, ix->getType());
        raise_exception_unless(builder.CreateICmpEQ(back, ix),
                               prepare_global(jlinexacterr_var), ctx);
    }
    return mark_julia_type(ans, false, jlto);
}

static jl_cgval_t generic_sext(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "sext_int", ctx);
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x, ctx));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateSExt(ix, to);
    return mark_julia_type(ans, false, jlto);
}

static jl_cgval_t generic_zext(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "zext_int", ctx);
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x, ctx));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateZExt(ix, to);
    return mark_julia_type(ans, false, jlto);
}

static Value *emit_eqfsi(Value *x, Value *y)
{
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

static jl_cgval_t emit_checked_fptosi(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "checked_fptosi", ctx);
    if (!jlto) return jl_cgval_t();
    Type *to = staticeval_bitstype(jlto);
    Value *fx = FP(auto_unbox(x, ctx));
    if (fx->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateFPToSI(fx, to);
    if (fx->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(fx, T_float64),
                                   builder.CreateSIToFP(ans, T_float64)),
             prepare_global(jlinexacterr_var), ctx);
    }
    else {
        raise_exception_unless(emit_eqfsi(fx, ans), prepare_global(jlinexacterr_var), ctx);
    }
    return mark_julia_type(ans, false, jlto);
}

static jl_cgval_t emit_checked_fptoui(jl_value_t *targ, jl_value_t *x, jl_codectx_t *ctx)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "checked_fptoui", ctx);
    if (!jlto) return jl_cgval_t();
    Type *to = staticeval_bitstype(jlto);
    Value *fx = FP(auto_unbox(x, ctx));
    if (fx->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateFPToUI(fx, to);
    if (fx->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(fx, T_float64),
                                   builder.CreateUIToFP(ans, T_float64)),
             prepare_global(jlinexacterr_var), ctx);
    }
    else {
        raise_exception_unless(emit_eqfui(fx, ans), prepare_global(jlinexacterr_var), ctx);
    }
    return mark_julia_type(ans, false, jlto);
}

static jl_cgval_t emit_runtime_pointerref(jl_value_t *e, jl_value_t *i, jl_codectx_t *ctx)
{
    Value *preffunc = // TODO: move this to the codegen initialization code section
        jl_Module->getOrInsertFunction("jl_pointerref",
                                       FunctionType::get(T_pjlvalue, two_pvalue_llvmt, false));
    int ldepth = ctx->gc.argDepth;
    jl_cgval_t parg = emit_boxed_rooted(e, ctx);
    Value *iarg = boxed(emit_expr(i, ctx), ctx);
#ifdef LLVM37
    Value *ret = builder.CreateCall(prepare_call(preffunc), { parg.V, iarg });
#else
    Value *ret = builder.CreateCall2(prepare_call(preffunc), parg.V, iarg);
#endif
    ctx->gc.argDepth = ldepth;
    jl_value_t *ety;
    if (jl_is_cpointer_type(parg.typ)) {
        ety = jl_tparam0(parg.typ);
    }
    else {
        ety = (jl_value_t*)jl_any_type;
    }
    return mark_julia_type(ret, true, ety);
}

static jl_cgval_t emit_pointerref(jl_value_t *e, jl_value_t *i, jl_codectx_t *ctx)
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
            return mark_julia_type(
                    builder.CreateLoad(builder.CreateGEP(
                        builder.CreateBitCast(thePtr, T_ppjlvalue),
                        im1)),
                    true,
                    ety);
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerref: invalid pointer type", ctx);
            return jl_cgval_t();
        }
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(size);
        builder.CreateStore(literal_pointer_val((jl_value_t*)ety),
                            emit_typeptr_addr(strct));
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->alignment)));
        thePtr = builder.CreateGEP(builder.CreateBitCast(thePtr, T_pint8), im1);
        builder.CreateMemCpy(builder.CreateBitCast(strct, T_pint8),
                             thePtr, size, 1);
        return mark_julia_type(strct, true, ety);
    }
    // TODO: alignment?
    return typed_load(thePtr, im1, ety, ctx, tbaa_user, 1);
}

static jl_cgval_t emit_runtime_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_codectx_t *ctx)
{
    Value *psetfunc = // TODO: move this to the codegen initialization code section
        jl_Module->getOrInsertFunction("jl_pointerset",
                                       FunctionType::get(T_pjlvalue, three_pvalue_llvmt, false));
    int ldepth = ctx->gc.argDepth;
    jl_cgval_t parg = emit_boxed_rooted(e, ctx);
    Value *iarg = emit_boxed_rooted(i, ctx).V;
    Value *xarg = boxed(emit_expr(x, ctx), ctx);
#ifdef LLVM37
    builder.CreateCall(prepare_call(psetfunc), { parg.V, xarg, iarg });
#else
    builder.CreateCall3(prepare_call(psetfunc), parg.V, xarg, iarg);
#endif
    ctx->gc.argDepth = ldepth;
    return parg;
}

// e[i] = x
static jl_cgval_t emit_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_codectx_t *ctx)
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
    jl_cgval_t val;
    bool emitted = false;
    if (!jl_subtype(xty, ety, 0)) {
        emitted = true;
        val = emit_expr(x, ctx);
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
            return jl_cgval_t();
        }
        if (!emitted)
            val = emit_expr(x,ctx,true,true);
        assert(val.isboxed);
        assert(jl_is_datatype(ety));
        uint64_t size = ((jl_datatype_t*)ety)->size;
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->alignment)));
        builder.CreateMemCpy(builder.CreateGEP(builder.CreateBitCast(thePtr, T_pint8), im1),
                             builder.CreateBitCast(val.V, T_pint8), size, 1);
    }
    else {
        if (!emitted) {
            if (ety == (jl_value_t*)jl_any_type)
                val = emit_expr(x,ctx);
            else
                val = emit_unboxed(x,ctx);
        }
        // TODO: alignment?
        typed_store(thePtr, im1, val, ety, ctx, tbaa_user, NULL, 1);
    }
    return mark_julia_type(thePtr, false, aty);
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
            builder.SetFastMathFlags(fmf);
        }
    }
    IRBuilder<>& operator()() const { return builder; }
    ~math_builder() {
        builder.SetFastMathFlags(old_fmf);
    }
};

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

static Value *emit_untyped_intrinsic(intrinsic f, Value *x, Value *y, Value *z, size_t nargs,
                                       jl_codectx_t *ctx, jl_datatype_t* *newtyp);
static jl_cgval_t emit_intrinsic(intrinsic f, jl_value_t **args, size_t nargs,
                                       jl_codectx_t *ctx)
{
    assert(f < num_intrinsics);
    if (f == fptoui && nargs == 1)
        f = fptoui_auto;
    if (f == fptosi && nargs == 1)
        f = fptosi_auto;
    unsigned expected_nargs = intrinsic_nargs[f];
    if (expected_nargs && expected_nargs != nargs) {
        jl_errorf("intrinsic #%d %s: wrong number of arguments", f, JL_I::jl_intrinsic_name((int)f));
    }

    switch (f) {
    case ccall: return emit_ccall(args, nargs, ctx);
    case cglobal: return emit_cglobal(args, nargs, ctx);
    case llvmcall: return emit_llvmcall(args, nargs, ctx);
    case arraylen:
        return mark_julia_type(emit_arraylen(emit_expr(args[1], ctx), args[1], ctx), false,
                               jl_long_type);
#if 0 // this section enables runtime-intrinsics (e.g. for testing), and disables their llvm counterparts
    default:
        int ldepth = ctx->gc.argDepth;
        Value *r;
        Value *func = prepare_call(runtime_func[f]);
        if (nargs == 1) {
            Value *x = emit_boxed_rooted(args[1], ctx).V;
#ifdef LLVM37
            r = builder.CreateCall(func, {x});
#else
            r = builder.CreateCall(func, x);
#endif
        }
        else if (nargs == 2) {
            Value *x = emit_boxed_rooted(args[1], ctx).V;
            Value *y = emit_boxed_rooted(args[2], ctx).V;
#ifdef LLVM37
            r = builder.CreateCall(func, {x, y});
#else
            r = builder.CreateCall2(func, x, y);
#endif
        }
        else if (nargs == 3) {
            Value *x = emit_boxed_rooted(args[1], ctx).V;
            Value *y = emit_boxed_rooted(args[2], ctx).V;
            Value *z = emit_boxed_rooted(args[3], ctx).V;
#ifdef LLVM37
            r = builder.CreateCall(func, {x, y, z});
#else
            r = builder.CreateCall3(func, x, y, z);
#endif
        }
        else {
            assert(0);
        }
        ctx->gc.argDepth = ldepth;
        return mark_julia_type(r, true, (jl_value_t*)jl_any_type);
#else
    case pointerref:
        return emit_pointerref(args[1], args[2], ctx);
    case pointerset:
        return emit_pointerset(args[1], args[2], args[3], ctx);
    case box:
        return generic_box(args[1], args[2], ctx);
    case unbox:
        return generic_unbox(args[1], args[2], ctx); // TODO: replace with generic_box
    case trunc_int:
        return generic_trunc(args[1], args[2], ctx, false, false);
    case checked_trunc_sint:
        return generic_trunc(args[1], args[2], ctx, true, true);
    case checked_trunc_uint:
        return generic_trunc(args[1], args[2], ctx, true, false);
    case sext_int:
        return generic_sext(args[1], args[2], ctx);
    case zext_int:
        return generic_zext(args[1], args[2], ctx);
    case checked_fptosi:
        return emit_checked_fptosi(args[1], args[2], ctx);
    case checked_fptoui:
        return emit_checked_fptoui(args[1], args[2], ctx);

    case uitofp: {
        jl_value_t *bt = staticeval_bitstype(args[1], "uitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xi = JL_INT(auto_unbox(args[2],ctx));
        if (xi->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateUIToFP(xi, FTnbits(nb)), false, bt);
    }

    case sitofp: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xi = JL_INT(auto_unbox(args[2],ctx));
        if (xi->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateSIToFP(xi, FTnbits(nb)), false, bt);
    }

    case fptoui_auto: {
        Value *x = FP(auto_unbox(args[1], ctx));
        if (x->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(
                builder.CreateFPToUI(FP(x), JL_INTT(x->getType())),
                false,
                JL_JLUINTT(x->getType()));
    }
    case fptoui: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2],ctx));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPToUI(xf, Type::getIntNTy(jl_LLVMContext, nb)), false, bt);
    }

    case fptosi_auto: {
        Value *x = FP(auto_unbox(args[1], ctx));
        return mark_julia_type(
                builder.CreateFPToSI(FP(x), JL_INTT(x->getType())),
                false,
                JL_JLSINTT(x->getType()));
    }
    case fptosi: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2],ctx));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPToSI(xf, Type::getIntNTy(jl_LLVMContext, nb)), false, bt);
    }

    case fptrunc: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2],ctx));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPTrunc(xf, FTnbits(nb)), false, bt);
    }

    case fpext: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp", ctx);
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *x = auto_unbox(args[2],ctx);
        if (x->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
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
        return mark_julia_type(builder.CreateFPExt(x, FTnbits(nb)), false, bt);
    }

    case select_value: {
        Value *isfalse = emit_condition(args[1], "select_value", ctx); // emit the first argument
        jl_value_t *t1 = expr_type(args[2], ctx);
        jl_value_t *t2 = expr_type(args[3], ctx);
        bool isboxed;
        Type *llt1 = julia_type_to_llvm(t1, &isboxed);
        Value *ifelse_result;
        if (!isboxed && t1 == t2) {
            // emit X and Y arguments
            jl_cgval_t x = emit_unboxed(args[2], ctx);
            jl_cgval_t y = emit_unboxed(args[3], ctx);
            // check the return value was valid
            if (type_is_ghost(llt1))
                return x;
            if (x.V->getType() == T_void && y.V->getType() == T_void)
                return jl_cgval_t(); // undefined
            if (x.V->getType() == T_void)
                return y;
            if (y.V->getType() == T_void)
                return x;
            // ensure that X and Y have the same llvm Type
            Value *vx = x.V, *vy = y.V;
            if (x.ispointer) // TODO: elid this load if unnecessary
                vx = builder.CreateLoad(builder.CreatePointerCast(vx, llt1->getPointerTo(0)));
            if (y.ispointer) // TODO: elid this load if unnecessary
                vy = builder.CreateLoad(builder.CreatePointerCast(vy, llt1->getPointerTo(0)));
            ifelse_result = builder.CreateSelect(isfalse, vy, vx);
            return mark_julia_type(ifelse_result, false, t2);
        }
        else {
            int argStart = ctx->gc.argDepth;
            Value *arg1 = boxed(emit_expr(args[2],ctx,false), ctx, expr_type(args[2],ctx));
            // TODO: if (!arg1.isboxed || arg1.needsgcroot)
                make_gcroot(arg1, ctx);
            Value *arg2 = boxed(emit_expr(args[3],ctx,false), ctx, expr_type(args[3],ctx));
            ifelse_result = builder.CreateSelect(isfalse, arg2, arg1);
            ctx->gc.argDepth = argStart;
            jl_value_t *jt = (t1 == t2 ? t1 : (jl_value_t*)jl_any_type);
            return mark_julia_type(ifelse_result, true, jt);
        }
    }

    default: {
        if (nargs < 1) jl_error("invalid intrinsic call");
        jl_cgval_t xinfo = emit_unboxed(args[1], ctx);
        Value *x = auto_unbox(xinfo, ctx);
        if (!x || type_is_ghost(x->getType())) {
            emit_error("invalid intrinsic argument at 1", ctx);
            return jl_cgval_t();
        }
        Value *y = NULL;
        if (nargs>1) {
            y = auto_unbox(args[2], ctx);
            if (!y || type_is_ghost(y->getType())) {
                emit_error("invalid intrinsic argument at 2", ctx);
                return jl_cgval_t();
            }
        }
        Value *z = NULL;
        if (nargs>2) {
            z = auto_unbox(args[3], ctx);
            if (!z || type_is_ghost(z->getType())) {
                emit_error("invalid intrinsic argument at 3", ctx);
                return jl_cgval_t();
            }
        }
        jl_value_t *newtyp = NULL;
        // TODO: compare the type validity of x,y,z before emitting the intrinsic
        Value *r = emit_untyped_intrinsic(f, x, y, z, nargs, ctx, (jl_datatype_t**)&newtyp);
        if (!newtyp && r->getType() != x->getType())
            // cast back to the exact original type (e.g. float vs. int) before remarking as a julia type
            r = builder.CreateBitCast(r, x->getType());
        return mark_julia_type(r, false, newtyp ? newtyp : xinfo.typ);
    }
#endif
    }
    assert(0);
}

static Value *emit_untyped_intrinsic(intrinsic f, Value *x, Value *y, Value *z, size_t nargs,
                                       jl_codectx_t *ctx, jl_datatype_t* *newtyp)
{
    Type *t = x->getType();
    Value *fy;
    Value *den;
    Value *typemin;
    switch (f) {
    case neg_int: return builder.CreateSub(ConstantInt::get(t, 0), JL_INT(x));
    case add_int: return builder.CreateAdd(JL_INT(x), JL_INT(y));
    case sub_int: return builder.CreateSub(JL_INT(x), JL_INT(y));
    case mul_int: return builder.CreateMul(JL_INT(x), JL_INT(y));
    case sdiv_int:
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
    case udiv_int:
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               prepare_global(jldiverr_var), ctx);
        return builder.CreateUDiv(JL_INT(x), den);

    case srem_int:
        return emit_srem(JL_INT(x), JL_INT(y), ctx);

    case urem_int:
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               prepare_global(jldiverr_var), ctx);
        return builder.CreateURem(JL_INT(x), den);

    case smod_int:
        return emit_smod(JL_INT(x), JL_INT(y), ctx);

// Implements IEEE negate. Unfortunately there is no compliant way
// to implement this in LLVM 3.4, though there are two different idioms
// that do the correct thing on LLVM <= 3.3 and >= 3.5 respectively.
// See issue #7868
#ifdef LLVM35
    case neg_float: return math_builder(ctx)().CreateFSub(ConstantFP::get(FT(t), -0.0), FP(x));
    case neg_float_fast: return math_builder(ctx, true)().CreateFNeg(FP(x));
#else
    case neg_float:
        return math_builder(ctx)().CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
    case neg_float_fast:
        return math_builder(ctx, true)().CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
#endif
    case add_float: return math_builder(ctx)().CreateFAdd(FP(x), FP(y));
    case sub_float: return math_builder(ctx)().CreateFSub(FP(x), FP(y));
    case mul_float: return math_builder(ctx)().CreateFMul(FP(x), FP(y));
    case div_float: return math_builder(ctx)().CreateFDiv(FP(x), FP(y));
    case rem_float: return math_builder(ctx)().CreateFRem(FP(x), FP(y));
    case add_float_fast: return math_builder(ctx, true)().CreateFAdd(FP(x), FP(y));
    case sub_float_fast: return math_builder(ctx, true)().CreateFSub(FP(x), FP(y));
    case mul_float_fast: return math_builder(ctx, true)().CreateFMul(FP(x), FP(y));
    case div_float_fast: return math_builder(ctx, true)().CreateFDiv(FP(x), FP(y));
    case rem_float_fast: return math_builder(ctx, true)().CreateFRem(FP(x), FP(y));
    case fma_float: {
      assert(y->getType() == x->getType());
      assert(z->getType() == y->getType());
      Value *fmaintr = Intrinsic::getDeclaration(jl_Module, Intrinsic::fma,
                                   ArrayRef<Type*>(x->getType()));
#ifdef LLVM37
      return builder.CreateCall(fmaintr,{ FP(x), FP(y), FP(z) });
#else
      return builder.CreateCall3(fmaintr, FP(x), FP(y), FP(z));
#endif
    }
    case muladd_float:
#ifdef LLVM34
    {
      assert(y->getType() == x->getType());
      assert(z->getType() == y->getType());
#ifdef LLVM37
      return builder.CreateCall
#else
      return builder.CreateCall3
#endif
        (Intrinsic::getDeclaration(jl_Module, Intrinsic::fmuladd,
                                   ArrayRef<Type*>(x->getType())),
         #ifdef LLVM37
         {FP(x), FP(y), FP(z)}
         #else
         FP(x), FP(y), FP(z)
         #endif
        );
    }
#else
      return math_builder(ctx, true)().
        CreateFAdd(builder.CreateFMul(FP(x), FP(y)), FP(z));
#endif

    case checked_sadd:
    case checked_uadd:
    case checked_ssub:
    case checked_usub:
    case checked_smul:
    case checked_umul: {
        Value *ix = JL_INT(x); Value *iy = JL_INT(y);
        assert(ix->getType() == iy->getType());
        Value *intr =
            Intrinsic::getDeclaration(jl_Module,
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
               ArrayRef<Type*>(ix->getType()));
#ifdef LLVM37
        Value *res = builder.CreateCall(intr,{ix, iy});
#else
        Value *res = builder.CreateCall2(intr, ix, iy);
#endif
        Value *obit = builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        raise_exception_if(obit, prepare_global(jlovferr_var), ctx);
        return builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
    }

    case check_top_bit:
        // raise InexactError if argument's top bit is set
        x = JL_INT(x);
        raise_exception_if(builder.
                           CreateTrunc(builder.
                                       CreateLShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits()-1)),
                                       T_int1),
                           prepare_global(jlinexacterr_var), ctx);
        return x;

    case eq_int:  *newtyp = jl_bool_type; return builder.CreateICmpEQ(JL_INT(x), JL_INT(y));
    case ne_int:  *newtyp = jl_bool_type; return builder.CreateICmpNE(JL_INT(x), JL_INT(y));
    case slt_int: *newtyp = jl_bool_type; return builder.CreateICmpSLT(JL_INT(x), JL_INT(y));
    case ult_int: *newtyp = jl_bool_type; return builder.CreateICmpULT(JL_INT(x), JL_INT(y));
    case sle_int: *newtyp = jl_bool_type; return builder.CreateICmpSLE(JL_INT(x), JL_INT(y));
    case ule_int: *newtyp = jl_bool_type; return builder.CreateICmpULE(JL_INT(x), JL_INT(y));

    case eq_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOEQ(FP(x), FP(y));
    case ne_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpUNE(FP(x), FP(y));
    case lt_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOLT(FP(x), FP(y));
    case le_float: *newtyp = jl_bool_type; return math_builder(ctx)().CreateFCmpOLE(FP(x), FP(y));

    case eq_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOEQ(FP(x), FP(y));
    case ne_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpUNE(FP(x), FP(y));
    case lt_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOLT(FP(x), FP(y));
    case le_float_fast: *newtyp = jl_bool_type; return math_builder(ctx, true)().CreateFCmpOLE(FP(x), FP(y));

    case fpiseq: {
        *newtyp = jl_bool_type;
        Value *xi = JL_INT(x);
        Value *yi = JL_INT(y);
        x = FP(x);
        fy = FP(y);
        return builder.CreateOr(builder.CreateAnd(builder.CreateFCmpUNO(x, x),
                                                  builder.CreateFCmpUNO(fy, fy)),
                                builder.CreateICmpEQ(xi, yi));
    }

    case fpislt: {
        *newtyp = jl_bool_type;
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

    case and_int: return builder.CreateAnd(JL_INT(x), JL_INT(y));
    case or_int:  return builder.CreateOr(JL_INT(x), JL_INT(y));
    case xor_int: return builder.CreateXor(JL_INT(x), JL_INT(y));
    case not_int: return builder.CreateXor(JL_INT(x), ConstantInt::get(t, -1, true));
    case shl_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateShl(x, uint_cnvt(t,y)));
    case lshr_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateLShr(x, uint_cnvt(t,y)));
    case ashr_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         builder.CreateAShr(x, ConstantInt::get(x->getType(),
                                                                x->getType()->getPrimitiveSizeInBits()-1)),
                         builder.CreateAShr(x, uint_cnvt(t,y)));
    case bswap_int:
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap,
                                      ArrayRef<Type*>(x->getType())), x);
    case ctpop_int:
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop,
                                      ArrayRef<Type*>(x->getType())), x);
    case ctlz_int: {
        x = JL_INT(x);
        Type *types[1] = {x->getType()};
        Value *ctlz = Intrinsic::getDeclaration(jl_Module, Intrinsic::ctlz,
                                      ArrayRef<Type*>(types));
#ifdef LLVM37
        return builder.CreateCall(ctlz, {x, ConstantInt::get(T_int1,0)});
#else
        return builder.CreateCall2(ctlz, x, ConstantInt::get(T_int1,0));
#endif
    }
    case cttz_int: {
        x = JL_INT(x);
        Type *types[1] = {x->getType()};
        Value *cttz = Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, ArrayRef<Type*>(types));
#ifdef LLVM37
        return builder.CreateCall(cttz, {x, ConstantInt::get(T_int1, 0)});
#else
        return builder.CreateCall2(cttz, x, ConstantInt::get(T_int1, 0));
#endif
    }

    case nan_dom_err: {
        // nan_dom_err(f, x) throw DomainError if isnan(f)&&!isnan(x)
        Value *f = FP(x); x = FP(y);
        raise_exception_unless(builder.CreateOr(builder.CreateFCmpORD(f,f),
                                                builder.CreateFCmpUNO(x,x)),
                               prepare_global(jldomerr_var), ctx);
        return f;
    }

    case abs_float:
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
    case copysign_float:
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
    case flipsign_int:
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
    case ceil_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case floor_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::floor,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case trunc_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case rint_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::rint,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case sqrt_llvm: {
        x = FP(x);
        raise_exception_unless(builder.CreateFCmpUGE(x, ConstantFP::get(x->getType(),0.0)),
                               prepare_global(jldomerr_var), ctx);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case powi_llvm: {
        x = FP(x);
        y = JL_INT(y);
        Type *tx = x->getType(); // TODO: LLVM expects this to be i32
#ifdef LLVM36
        Type *ts[1] = { tx };
        Value *powi = Intrinsic::getDeclaration(jl_Module, Intrinsic::powi,
            ArrayRef<Type*>(ts));
#ifdef LLVM37
        return builder.CreateCall(powi, {x, y});
#else
        return builder.CreateCall2(powi, x, y);
#endif
#else
        // issue #6506
        return builder.CreateCall2(prepare_call(tx == T_float64 ? jlpow_func : jlpowf_func),
                x, builder.CreateSIToFP(y, tx));
#endif
    }
    case sqrt_llvm_fast: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }

    default:
        assert(false);
    }
    assert(false);
    return NULL;
}

#define BOX_F(ct,jl_ct)                                                    \
    box_##ct##_func = boxfunc_llvm(ft1arg(T_pjlvalue, T_##jl_ct),     \
                                   "jl_box_"#ct, (void*)&jl_box_##ct, m);

#define SBOX_F(ct,jl_ct) BOX_F(ct,jl_ct); box_##ct##_func->addAttribute(1, Attribute::SExt);
#define UBOX_F(ct,jl_ct) BOX_F(ct,jl_ct); box_##ct##_func->addAttribute(1, Attribute::ZExt);

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
