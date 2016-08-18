// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "ccall.cpp"

static Function *runtime_func[JL_I::num_intrinsics];
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
        runtime_func[JL_I::name] = func; \
        add_named_global(func, &jl_##name); \
    } while (0);
#define ADD_HIDDEN ADD_I
#define ALIAS(alias, base) runtime_func[JL_I::alias] = runtime_func[JL_I::base];
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

Type *FTnbits(size_t nb)
{
#ifndef DISABLE_FLOAT16
    if (nb == 16)
        return T_float16;
    else
#endif
    if (nb == 32)
        return T_float32;
    else if (nb == 64)
        return T_float64;
    else if (nb == 128)
        return T_float128;
    else
        jl_error("Unsupported Float Size");
}
// convert int type to same-size float type
Type *FT(Type *t)
{
    if (t->isFloatingPointTy())
        return t;
    return FTnbits(t->getPrimitiveSizeInBits());
}

// reinterpret-cast to float
Value *FP(Value *v)
{
    if (v->getType()->isFloatingPointTy())
        return v;
    return emit_bitcast(v, FT(v->getType()));
}

// convert float type to same-size int type
Type *JL_INTT(Type *t)
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
jl_value_t *JL_JLUINTT(Type *t)
{
    assert(!t->isIntegerTy());
    if (t == T_float32) return (jl_value_t*)jl_uint32_type;
    if (t == T_float64) return (jl_value_t*)jl_uint64_type;
    if (t == T_float16) return (jl_value_t*)jl_uint16_type;
    assert(t == T_void);
    return jl_bottom_type;
}
jl_value_t *JL_JLSINTT(Type *t)
{
    assert(!t->isIntegerTy());
    if (t == T_float32) return (jl_value_t*)jl_int32_type;
    if (t == T_float64) return (jl_value_t*)jl_int64_type;
    if (t == T_float16) return (jl_value_t*)jl_int16_type;
    assert(t == T_void);
    return jl_bottom_type;
}

// reinterpret-cast to int
Value *JL_INT(Value *v)
{
    Type *t = v->getType();
    if (t->isIntegerTy())
        return v;
    if (t->isPointerTy())
        return builder.CreatePtrToInt(v, JL_INTT(t));
    return emit_bitcast(v, JL_INTT(t));
}

Value *uint_cnvt(Type *to, Value *x)
{
    Type *t = x->getType();
    if (t == to) return x;
    if (to->getPrimitiveSizeInBits() < x->getType()->getPrimitiveSizeInBits())
        return builder.CreateTrunc(x, to);
    return builder.CreateZExt(x, to);
}

#define LLVM_FP(a,b) APFloat(a,b)
Constant *julia_const_to_llvm(void *ptr, jl_value_t *bt)
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

    Type *t = julia_struct_to_llvm(bt, NULL);
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

Constant *julia_const_to_llvm(jl_value_t *e)
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

// emit code to unpack a raw value from a box into registers or a stack slot
Value *emit_unbox(Type *to, const jl_cgval_t &x, jl_value_t *jt, Value *dest = NULL, bool volatile_store = false)
{
    assert(to != T_pjlvalue);
    // TODO: fully validate that x.typ == jt?
    if (x.isghost) {
        if (type_is_ghost(to)) {
            return NULL;
        }
        //emit_error("emit_unbox: a type mismatch error in occurred during codegen");
        return UndefValue::get(to); // type mismatch error
    }

    Constant *c = x.constant ? julia_const_to_llvm(x.constant) : NULL;
    if (!x.ispointer() || c) { // already unboxed, but sometimes need conversion
        Value *unboxed = c ? c : x.V;
        Type *ty = unboxed->getType();
        // bools are stored internally as int8 (for now)
        if (ty == T_int1 && to == T_int8)
            unboxed = builder.CreateZExt(unboxed, T_int8);
        else if (ty->isPointerTy() && !to->isPointerTy())
            unboxed = builder.CreatePtrToInt(unboxed, to);
        else if (!ty->isPointerTy() && to->isPointerTy())
            unboxed = builder.CreateIntToPtr(unboxed, to);
        else if (ty->isPointerTy() && to->isPointerTy())
            // pointer types are going away anyways, and this can come up in ccall argument conversion
            unboxed = builder.CreatePointerCast(unboxed, to);
        else if (ty != to) {
            // this can happen when a branch yielding a different type ends
            // up being dead code, and type inference knows that the other
            // branch's type is the only one that matters.
            // assert(ty == T_void);
            //emit_error("emit_unbox: a type mismatch error in occurred during codegen");
            unboxed = UndefValue::get(to); // type mismatch error
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

// unbox, trying to determine correct bitstype automatically
// returns some sort of raw, unboxed numeric type (e.g. in registers)
Value *auto_unbox(const jl_cgval_t &v)
{
    jl_value_t *bt = v.typ;
    if (!jl_is_bitstype(bt)) {
        // This can be reached with a direct invalid call to an Intrinsic, such as:
        //   Intrinsics.neg_int("")
        emit_error("auto_unbox: unable to determine argument type");
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
Value *auto_unbox(jl_value_t *x)
{
    jl_cgval_t v = emit_expr(x);
    return auto_unbox(v);
}

jl_value_t *staticeval_bitstype(jl_value_t *targ, const char *fname)
{
    // evaluate an argument at compile time to determine what type it is
    jl_cgval_t bt_value = emit_expr(targ);
    jl_value_t *bt = NULL;
    if (jl_is_type_type(bt_value.typ))
        bt = jl_tparam0(bt_value.typ);
    if (!bt || !jl_is_bitstype(bt)) {
        emit_error("expected bits type as first argument");
        return NULL;
    }
    return bt;
}

Type *staticeval_bitstype(jl_value_t *bt)
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
    return jl_datatype_size(bt)*8;
}

// put a bits type tag on some value (despite the name, this doesn't necessarily actually "box" the value however)
jl_cgval_t generic_box(jl_value_t *targ, jl_value_t *x)
{
    // Examine the first argument //
    jl_cgval_t bt_value = emit_expr(targ);
    jl_cgval_t v = emit_expr(x);
    jl_value_t *bt = NULL;
    if (jl_is_type_type(bt_value.typ))
        bt = jl_tparam0(bt_value.typ);

    if (!bt || !jl_is_bitstype(bt)) {
        // it's easier to throw a good error from C than llvm
        Value *arg1 = boxed(bt_value);
        Value *arg2 = boxed(v);
        Value *func = prepare_call(runtime_func[JL_I::reinterpret]);
#ifdef LLVM37
        Value *r = builder.CreateCall(func, {arg1, arg2});
#else
        Value *r = builder.CreateCall2(func, arg1, arg2);
#endif
        jl_value_t *et = expr_type(targ);
        return mark_julia_type(r, true, jl_is_type_type(et) ? jl_tparam0(et) : (jl_value_t*)jl_any_type);
    }

    Type *llvmt = staticeval_bitstype(bt);
    int nb = jl_datatype_size(bt);

    // Examine the second argument //
    bool isboxed;
    Type *vxt = julia_type_to_llvm(v.typ, &isboxed);

    if (!jl_is_datatype(v.typ)
        || !jl_is_bitstype(v.typ)
        || jl_datatype_size(v.typ) != nb) {
        Value *typ = emit_typeof_boxed(v);
        if (!jl_is_bitstype(v.typ)) {
            if (isboxed) {
                Value *isbits = emit_datatype_isbitstype(typ);
                error_unless(isbits, "reinterpret: expected bitstype value for second argument");
            }
            else {
                emit_error("reinterpret: expected bitstype value for second argument");
                return jl_cgval_t();
            }
        }
        if (jl_datatype_size(v.typ) != nb) {
            if (isboxed) {
                Value *size = emit_datatype_size(typ);
                error_unless(builder.CreateICmpEQ(size, ConstantInt::get(T_int32, nb)),
                            "reinterpret: argument size does not match size of target type");
            }
            else {
                emit_error("reinterpret: argument size does not match size of target type");
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
        vx = tbaa_decorate(v.tbaa, builder.CreateLoad(data_pointer(v,
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
        return mark_julia_type(vx, false, bt);
    else
        return mark_julia_type(
            init_bits_value(emit_allocobj(nb, boxed(bt_value)),
                            vx, tbaa_immut),
            true, bt);
}

// put a bits type tag on some value
jl_cgval_t generic_unbox(jl_value_t *targ, jl_value_t *x)
{
    // Examine the first argument //
    jl_cgval_t bt_value = emit_expr(targ);
    jl_value_t *bt = NULL;
    if (jl_is_type_type(bt_value.typ))
        bt = jl_tparam0(bt_value.typ);

    // Examine the second argument //
    jl_cgval_t v = emit_expr(x);

    if (bt == NULL || !jl_is_leaf_type(bt)) {
        // dynamically-determined type; evaluate.
        int nb, alignment;
        Type *llvmt;
        if (bt && jl_is_bitstype(bt)) {
            // always fixed size
            nb = jl_datatype_size(bt);
            llvmt = staticeval_bitstype(bt);
            alignment = ((jl_datatype_t*)bt)->layout->alignment;
        }
        else {
            bt = v.typ;
            if (!jl_is_leaf_type(bt) && !jl_is_bitstype(bt)) {
                // TODO: currently doesn't handle the case where the type of neither argument is understood at compile time
                // since codegen has no idea what size it might have
                jl_error("codegen: failed during evaluation of a call to unbox");
                return jl_cgval_t();
            }
            nb = jl_datatype_size(bt);
            llvmt = staticeval_bitstype(bt);
            alignment = ((jl_datatype_t*)bt)->layout->alignment;
        }
        Value *runtime_bt = boxed(bt_value);
        // XXX: emit type validity check on runtime_bt (bitstype of size nb)

        Value *newobj = emit_allocobj(nb, runtime_bt);
        if (!v.ispointer()) {
            tbaa_decorate(tbaa_value, builder.CreateAlignedStore(emit_unbox(llvmt, v, v.typ), builder.CreatePointerCast(newobj, llvmt->getPointerTo()), alignment));
        }
        else {
            prepare_call(builder.CreateMemCpy(newobj, data_pointer(v, T_pint8), nb, alignment)->getCalledValue());
            mark_gc_use(v);
        }
        return mark_julia_type(newobj, true, bt ? bt : (jl_value_t*)jl_any_type);
    }

    if (!jl_is_bitstype(bt)) {
        // TODO: to accept arbitrary types, replace this function with a call to llvm_type_rewrite
        emit_error("unbox: expected bits type as first argument");
        return jl_cgval_t();
    }

    Type *llvmt = staticeval_bitstype(bt);
    if (v.typ == bt)
        return v;

    Value *vx;
    if (v.ispointer()) {
        vx = tbaa_decorate(v.tbaa, builder.CreateLoad(data_pointer(v, llvmt->getPointerTo())));
    }
    else {
        vx = v.V;
        if (!jl_is_bitstype(v.typ)) {
            emit_error("unbox: expected bits type value for second argument");
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
            emit_error("unbox: argument is of incorrect size");
            return jl_cgval_t();
        }
        if (vxt->isPointerTy() && !llvmt->isPointerTy())
            vx = builder.CreatePtrToInt(vx, llvmt);
        else if (!vxt->isPointerTy() && llvmt->isPointerTy())
            vx = builder.CreateIntToPtr(vx, llvmt);
        else
            vx = emit_bitcast(vx, llvmt);
    }

    return mark_julia_type(vx, false, bt);
}

// NOTE: signd (signed) only relevant if check == true
jl_cgval_t generic_trunc(jl_value_t *targ, jl_value_t *x, bool check, bool signd)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "trunc_int");
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateTrunc(ix, to);
    if (check) {
        Value *back = signd ? builder.CreateSExt(ans, ix->getType()) :
            builder.CreateZExt(ans, ix->getType());
        raise_exception_unless(builder.CreateICmpEQ(back, ix),
                               literal_pointer_val(jl_inexact_exception));
    }
    return mark_julia_type(ans, false, jlto);
}

jl_cgval_t generic_sext(jl_value_t *targ, jl_value_t *x)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "sext_int");
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateSExt(ix, to);
    return mark_julia_type(ans, false, jlto);
}

jl_cgval_t generic_zext(jl_value_t *targ, jl_value_t *x)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "zext_int");
    if (!jlto) return jl_cgval_t(); // jlto threw an error
    Type *to = staticeval_bitstype(jlto);
    Value *ix = JL_INT(auto_unbox(x));
    if (ix->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateZExt(ix, to);
    return mark_julia_type(ans, false, jlto);
}

Value *emit_eqfsi(Value *x, Value *y)
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

Value *emit_eqfui(Value *x, Value *y)
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

jl_cgval_t emit_checked_fptosi(jl_value_t *targ, jl_value_t *x)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "checked_fptosi");
    if (!jlto) return jl_cgval_t();
    Type *to = staticeval_bitstype(jlto);
    Value *fx = FP(auto_unbox(x));
    if (fx->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateFPToSI(fx, to);
    if (fx->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(fx, T_float64),
                                   builder.CreateSIToFP(ans, T_float64)),
             literal_pointer_val(jl_inexact_exception));
    }
    else {
        raise_exception_unless(emit_eqfsi(fx, ans), literal_pointer_val(jl_inexact_exception));
    }
    return mark_julia_type(ans, false, jlto);
}

jl_cgval_t emit_checked_fptoui(jl_value_t *targ, jl_value_t *x)
{
    jl_value_t *jlto = staticeval_bitstype(targ, "checked_fptoui");
    if (!jlto) return jl_cgval_t();
    Type *to = staticeval_bitstype(jlto);
    Value *fx = FP(auto_unbox(x));
    if (fx->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
    Value *ans = builder.CreateFPToUI(fx, to);
    if (fx->getType() == T_float32 && to == T_int32) {
        raise_exception_unless
            (builder.CreateFCmpOEQ(builder.CreateFPExt(fx, T_float64),
                                   builder.CreateUIToFP(ans, T_float64)),
             literal_pointer_val(jl_inexact_exception));
    }
    else {
        raise_exception_unless(emit_eqfui(fx, ans), literal_pointer_val(jl_inexact_exception));
    }
    return mark_julia_type(ans, false, jlto);
}

jl_cgval_t emit_runtime_pointerref(jl_value_t *e, jl_value_t *i, jl_value_t *align)
{
    jl_cgval_t parg = emit_expr(e);
    Value *iarg = boxed(emit_expr(i));
    Value *alignarg = boxed(emit_expr(align));
#ifdef LLVM37
    Value *ret = builder.CreateCall(prepare_call(jlpref_func), { boxed(parg), iarg, alignarg });
#else
    Value *ret = builder.CreateCall3(prepare_call(jlpref_func), boxed(parg), iarg, alignarg);
#endif
    jl_value_t *ety;
    if (jl_is_cpointer_type(parg.typ)) {
        ety = jl_tparam0(parg.typ);
    }
    else {
        ety = (jl_value_t*)jl_any_type;
    }
    return mark_julia_type(ret, true, ety);
}

jl_cgval_t emit_pointerref(jl_value_t *e, jl_value_t *i, jl_value_t *align)
{
    jl_value_t *aty = expr_type(e);
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerref(e, i, align);
        //jl_error("pointerref: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerref(e, i, align);
        //jl_error("pointerref: invalid pointer");
    if (expr_type(i) != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerref(e, i, align);
        //jl_error("pointerref: invalid index type");
    jl_cgval_t align_val = emit_expr(align);
    if (align_val.constant == NULL || !jl_is_long(align_val.constant))
        return emit_runtime_pointerref(e, i, align);
        //jl_error("pointerref: invalid or non-statically evaluatable alignment")
    Value *thePtr = auto_unbox(e);
    Value *idx = emit_unbox(T_size, emit_expr(i), (jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    if (!jl_isbits(ety)) {
        if (ety == (jl_value_t*)jl_any_type)
            return mark_julia_type(
                    builder.CreateAlignedLoad(builder.CreateGEP(
                        emit_bitcast(thePtr, T_ppjlvalue),
                        im1), jl_unbox_long(align_val.constant)),
                    true,
                    ety);
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerref: invalid pointer type");
            return jl_cgval_t();
        }
        assert(jl_is_datatype(ety));
        uint64_t size = jl_datatype_size(ety);
        Value *strct = emit_allocobj(size,
                                     literal_pointer_val((jl_value_t*)ety));
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->layout->alignment)));
        thePtr = builder.CreateGEP(emit_bitcast(thePtr, T_pint8), im1);
        prepare_call(builder.CreateMemCpy(emit_bitcast(strct, T_pint8),
                             thePtr, size, 1)->getCalledValue());
        return mark_julia_type(strct, true, ety);
    }
    return typed_load(thePtr, im1, ety, tbaa_data, jl_unbox_long(align_val.constant));
}

jl_cgval_t emit_runtime_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_value_t *align)
{
    jl_cgval_t parg = emit_expr(e);
    Value *xarg = boxed(emit_expr(x));
    Value *iarg = boxed(emit_expr(i));
    Value *alignarg = boxed(emit_expr(align));
#ifdef LLVM37
    builder.CreateCall(prepare_call(jlpset_func), { boxed(parg), xarg, iarg, alignarg });
#else
    builder.CreateCall4(prepare_call(jlpset_func), boxed(parg), xarg, iarg, alignarg);
#endif
    return parg;
}

// e[i] = x
jl_cgval_t emit_pointerset(jl_value_t *e, jl_value_t *x, jl_value_t *i, jl_value_t *align)
{
    jl_value_t *aty = expr_type(e);
    if (!jl_is_cpointer_type(aty))
        return emit_runtime_pointerset(e, x, i, align);
        //jl_error("pointerset: expected pointer type as first argument");
    jl_value_t *ety = jl_tparam0(aty);
    if (jl_is_typevar(ety))
        return emit_runtime_pointerset(e, x, i, align);
        //jl_error("pointerset: invalid pointer");
    jl_value_t *xty = expr_type(x);
    jl_cgval_t val;
    bool emitted = false;
    if (!jl_subtype(xty, ety, 0)) {
        emitted = true;
        val = emit_expr(x);
        emit_typecheck(val, ety, "pointerset: type mismatch in assign");
    }
    if (expr_type(i) != (jl_value_t*)jl_long_type)
        return emit_runtime_pointerset(e, x, i, align);
        //jl_error("pointerset: invalid index type");
    jl_cgval_t align_val = emit_expr(align);
    if (align_val.constant == NULL || !jl_is_long(align_val.constant))
        return emit_runtime_pointerset(e, x, i, align);
    //jl_error("pointerset: invalid or non-statically evaluatable alignment")
    Value *idx = emit_unbox(T_size, emit_expr(i), (jl_value_t*)jl_long_type);
    Value *im1 = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
    Value *thePtr = auto_unbox(e);
    if (!jl_isbits(ety) && ety != (jl_value_t*)jl_any_type) {
        if (!jl_is_structtype(ety) || jl_is_array_type(ety) || !jl_is_leaf_type(ety)) {
            emit_error("pointerset: invalid pointer type");
            return jl_cgval_t();
        }
        if (!emitted)
            val = emit_expr(x);
        assert(val.isboxed);
        assert(jl_is_datatype(ety));
        uint64_t size = ((jl_datatype_t*)ety)->size;
        im1 = builder.CreateMul(im1, ConstantInt::get(T_size,
                    LLT_ALIGN(size, ((jl_datatype_t*)ety)->layout->alignment)));
        prepare_call(builder.CreateMemCpy(builder.CreateGEP(emit_bitcast(thePtr, T_pint8), im1),
                             data_pointer(val, T_pint8), size, jl_unbox_long(align_val.constant))->getCalledValue());
    }
    else {
        if (!emitted) {
            val = emit_expr(x);
        }
        assert(jl_is_datatype(ety));
        typed_store(thePtr, im1, val, ety, tbaa_data, NULL, jl_unbox_long(align_val.constant));
    }
    return mark_julia_type(thePtr, false, aty);
}

Value *emit_checked_srem_int(Value *x, Value *den)
{
    Type *t = den->getType();
    raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                           literal_pointer_val(jl_diverror_exception));
    BasicBlock *m1BB = BasicBlock::Create(jl_LLVMContext,"minus1",this->f);
    BasicBlock *okBB = BasicBlock::Create(jl_LLVMContext,"oksrem",this->f);
    BasicBlock *cont = BasicBlock::Create(jl_LLVMContext,"after_srem",this->f);
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
    math_builder(bool always_fast = false):
        old_fmf(builder.getFastMathFlags())
    {
        if (jl_options.fast_math != JL_OPTIONS_FAST_MATH_OFF &&
            (always_fast ||
             jl_options.fast_math == JL_OPTIONS_FAST_MATH_ON)) {
            FastMathFlags fmf;
            fmf.setUnsafeAlgebra();
#ifdef LLVM38
            builder.setFastMathFlags(fmf);
#else
            builder.SetFastMathFlags(fmf);
#endif
        }
    }
    IRBuilder<>& operator()() const { return builder; }
    ~math_builder() {
#ifdef LLVM38
        builder.setFastMathFlags(old_fmf);
#else
        builder.SetFastMathFlags(old_fmf);
#endif
    }
};

jl_cgval_t emit_intrinsic(JL_I::intrinsic f, jl_value_t **args, size_t nargs)
{
    assert(f < JL_I::num_intrinsics);
    if (f == JL_I::fptoui && nargs == 1)
        f = JL_I::fptoui_auto;
    if (f == JL_I::fptosi && nargs == 1)
        f = JL_I::fptosi_auto;
    unsigned expected_nargs = JL_I::intrinsic_nargs[f];
    if (expected_nargs && expected_nargs != nargs) {
        jl_errorf("intrinsic #%d %s: wrong number of arguments", f, JL_I::jl_intrinsic_name((int)f));
    }

    switch (f) {
    case JL_I::ccall: return emit_ccall(args, nargs);
    case JL_I::cglobal: return emit_cglobal(args, nargs);
    case JL_I::llvmcall: return emit_llvmcall(args, nargs);
    case JL_I::arraylen:
        return mark_julia_type(emit_arraylen(emit_expr(args[1]), args[1]), false,
                               jl_long_type);
#if 0 // this section enables runtime-intrinsics (e.g. for testing), and disables their llvm counterparts
    default:
        Value *r;
        Value *func = prepare_call(runtime_func[f]);
        if (nargs == 1) {
            Value *x = boxed(emit_expr(args[1]));
#ifdef LLVM37
            r = builder.CreateCall(func, {x});
#else
            r = builder.CreateCall(func, x);
#endif
        }
        else if (nargs == 2) {
            Value *x = boxed(emit_expr(args[1]));
            Value *y = boxed(emit_expr(args[2]));
#ifdef LLVM37
            r = builder.CreateCall(func, {x, y});
#else
            r = builder.CreateCall2(func, x, y);
#endif
        }
        else if (nargs == 3) {
            Value *x = boxed(emit_expr(args[1]));
            Value *y = boxed(emit_expr(args[2]));
            Value *z = boxed(emit_expr(args[3]));
#ifdef LLVM37
            r = builder.CreateCall(func, {x, y, z});
#else
            r = builder.CreateCall3(func, x, y, z);
#endif
        }
        else {
            assert(0);
        }
        return mark_julia_type(r, true, (jl_value_t*)jl_any_type);
#else
    case JL_I::pointerref:
        return emit_pointerref(args[1], args[2], args[3]);
    case JL_I::pointerset:
        return emit_pointerset(args[1], args[2], args[3], args[4]);
    case JL_I::box:
        return generic_box(args[1], args[2]);
    case JL_I::unbox:
        return generic_unbox(args[1], args[2]); // TODO: replace with generic_box
    case JL_I::trunc_int:
        return generic_trunc(args[1], args[2], false, false);
    case JL_I::checked_trunc_sint:
        return generic_trunc(args[1], args[2], true, true);
    case JL_I::checked_trunc_uint:
        return generic_trunc(args[1], args[2], true, false);
    case JL_I::sext_int:
        return generic_sext(args[1], args[2]);
    case JL_I::zext_int:
        return generic_zext(args[1], args[2]);
    case JL_I::checked_fptosi:
        return emit_checked_fptosi(args[1], args[2]);
    case JL_I::checked_fptoui:
        return emit_checked_fptoui(args[1], args[2]);

    case JL_I::uitofp: {
        jl_value_t *bt = staticeval_bitstype(args[1], "uitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xi = JL_INT(auto_unbox(args[2]));
        if (xi->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateUIToFP(xi, FTnbits(nb)), false, bt);
    }

    case JL_I::sitofp: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xi = JL_INT(auto_unbox(args[2]));
        if (xi->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateSIToFP(xi, FTnbits(nb)), false, bt);
    }

    case JL_I::fptoui_auto: {
        Value *x = FP(auto_unbox(args[1]));
        if (x->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(
                builder.CreateFPToUI(FP(x), JL_INTT(x->getType())),
                false,
                JL_JLUINTT(x->getType()));
    }
    case JL_I::fptoui: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2]));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPToUI(xf, Type::getIntNTy(jl_LLVMContext, nb)), false, bt);
    }

    case JL_I::fptosi_auto: {
        Value *x = FP(auto_unbox(args[1]));
        return mark_julia_type(
                builder.CreateFPToSI(FP(x), JL_INTT(x->getType())),
                false,
                JL_JLSINTT(x->getType()));
    }
    case JL_I::fptosi: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2]));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPToSI(xf, Type::getIntNTy(jl_LLVMContext, nb)), false, bt);
    }

    case JL_I::fptrunc: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *xf = FP(auto_unbox(args[2]));
        if (xf->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
        return mark_julia_type(builder.CreateFPTrunc(xf, FTnbits(nb)), false, bt);
    }

    case JL_I::fpext: {
        jl_value_t *bt = staticeval_bitstype(args[1], "sitofp");
        if (!bt) return jl_cgval_t();
        int nb = get_bitstype_nbits(bt);
        Value *x = auto_unbox(args[2]);
        if (x->getType() == T_void) return jl_cgval_t(); // auto_unbox threw an error
#ifdef JL_NEED_FLOATTEMP_VAR
        // Target platform might carry extra precision.
        // Force rounding to single precision first. The reason is that it's
        // fine to keep working in extended precision as long as it's
        // understood that everything is implicitly rounded to 23 bits,
        // but if we start looking at more bits we need to actually do the
        // rounding first instead of carrying around incorrect low bits.
        Value *jlfloattemp_var = emit_static_alloca(FT(x->getType()));
        builder.CreateStore(FP(x), jlfloattemp_var);
        x  = builder.CreateLoad(jlfloattemp_var, true);
#endif
        return mark_julia_type(builder.CreateFPExt(x, FTnbits(nb)), false, bt);
    }

    case JL_I::select_value: {
        Value *isfalse = emit_condition(args[1], "select_value"); // emit the first argument
        jl_value_t *t1 = expr_type(args[2]);
        jl_value_t *t2 = expr_type(args[3]);
        bool isboxed;
        Type *llt1 = julia_type_to_llvm(t1, &isboxed);
        Value *ifelse_result;
        // emit X and Y arguments
        jl_cgval_t x = emit_expr(args[2]);
        jl_cgval_t y = emit_expr(args[3]);
        // check the return value was valid
        if (x.typ == jl_bottom_type && y.typ == jl_bottom_type)
            return jl_cgval_t(); // undefined
        if (x.typ == jl_bottom_type)
            return y;
        if (y.typ == jl_bottom_type)
            return x;
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
                    boxed(y),
                    boxed(x));
        }
        jl_value_t *jt = (t1 == t2 ? t1 : (jl_value_t*)jl_any_type);
        mark_gc_use(x);
        mark_gc_use(y);
        return mark_julia_type(ifelse_result, isboxed, jt);
    }

    default: {
        if (nargs < 1) jl_error("invalid intrinsic call");
        jl_cgval_t xinfo = emit_expr(args[1]);
        Value *x = auto_unbox(xinfo);
        if (!x || type_is_ghost(x->getType())) {
            emit_error("invalid intrinsic argument at 1");
            return jl_cgval_t();
        }
        Value *y = NULL;
        if (nargs>1) {
            y = auto_unbox(args[2]);
            if (!y || type_is_ghost(y->getType())) {
                emit_error("invalid intrinsic argument at 2");
                return jl_cgval_t();
            }
        }
        Value *z = NULL;
        if (nargs>2) {
            z = auto_unbox(args[3]);
            if (!z || type_is_ghost(z->getType())) {
                emit_error("invalid intrinsic argument at 3");
                return jl_cgval_t();
            }
        }
        jl_value_t *newtyp = NULL;
        // TODO: compare the type validity of x,y,z before emitting the intrinsic
        Value *r;
        if (f == JL_I::not_int && xinfo.typ == (jl_value_t*)jl_bool_type)
            r = builder.CreateXor(x, ConstantInt::get(T_int8, 1, true));
        else
            r = emit_untyped_intrinsic(f, x, y, z, nargs, (jl_datatype_t**)&newtyp);
        if (!newtyp && r->getType() != x->getType())
            // cast back to the exact original type (e.g. float vs. int) before remarking as a julia type
            r = emit_bitcast(r, x->getType());
        if (r->getType() == T_int1)
            r = builder.CreateZExt(r, T_int8);
        return mark_julia_type(r, false, newtyp ? newtyp : xinfo.typ);
    }
#endif
    }
    assert(0);
}

Value *emit_untyped_intrinsic(JL_I::intrinsic f, Value *x, Value *y, Value *z, size_t nargs,
                                     jl_datatype_t **newtyp)
{
    Type *t = x->getType();
    Value *fy;
    Value *den;
    Value *typemin;
    switch (f) {
    case JL_I::neg_int:
#ifdef LLVM37
     return builder.CreateNeg(JL_INT(x));
#else
     return builder.CreateSub(ConstantInt::get(t, 0), JL_INT(x));
#endif
    case JL_I::add_int: return builder.CreateAdd(JL_INT(x), JL_INT(y));
    case JL_I::sub_int: return builder.CreateSub(JL_INT(x), JL_INT(y));
    case JL_I::mul_int: return builder.CreateMul(JL_INT(x), JL_INT(y));
    case JL_I::sdiv_int: return builder.CreateSDiv(JL_INT(x), JL_INT(y));
    case JL_I::udiv_int: return builder.CreateUDiv(JL_INT(x), JL_INT(y));
    case JL_I::srem_int: return builder.CreateSRem(JL_INT(x), JL_INT(y));
    case JL_I::urem_int: return builder.CreateURem(JL_INT(x), JL_INT(y));

// Implements IEEE negate. Unfortunately there is no compliant way
// to implement this in LLVM 3.4, though there are two different idioms
// that do the correct thing on LLVM <= 3.3 and >= 3.5 respectively.
// See issue #7868
#ifdef LLVM35
    case JL_I::neg_float: return math_builder()().CreateFSub(ConstantFP::get(FT(t), -0.0), FP(x));
    case JL_I::neg_float_fast: return math_builder(true)().CreateFNeg(FP(x));
#else
    case JL_I::neg_float:
        return math_builder()().CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
    case JL_I::neg_float_fast:
        return math_builder(true)().CreateFMul(ConstantFP::get(FT(t), -1.0), FP(x));
#endif
    case JL_I::add_float: return math_builder()().CreateFAdd(FP(x), FP(y));
    case JL_I::sub_float: return math_builder()().CreateFSub(FP(x), FP(y));
    case JL_I::mul_float: return math_builder()().CreateFMul(FP(x), FP(y));
    case JL_I::div_float: return math_builder()().CreateFDiv(FP(x), FP(y));
    case JL_I::rem_float: return math_builder()().CreateFRem(FP(x), FP(y));
    case JL_I::add_float_fast: return math_builder(true)().CreateFAdd(FP(x), FP(y));
    case JL_I::sub_float_fast: return math_builder(true)().CreateFSub(FP(x), FP(y));
    case JL_I::mul_float_fast: return math_builder(true)().CreateFMul(FP(x), FP(y));
    case JL_I::div_float_fast: return math_builder(true)().CreateFDiv(FP(x), FP(y));
    case JL_I::rem_float_fast: return math_builder(true)().CreateFRem(FP(x), FP(y));
    case JL_I::fma_float: {
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
    case JL_I::muladd_float:
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
      return math_builder(true)().
        CreateFAdd(builder.CreateFMul(FP(x), FP(y)), FP(z));
#endif

    case JL_I::checked_sadd_int:
    case JL_I::checked_uadd_int:
    case JL_I::checked_ssub_int:
    case JL_I::checked_usub_int:
    case JL_I::checked_smul_int:
    case JL_I::checked_umul_int: {
        Value *ix = JL_INT(x); Value *iy = JL_INT(y);
        assert(ix->getType() == iy->getType());
        Value *intr =
            Intrinsic::getDeclaration(jl_Module,
                f == JL_I::checked_sadd_int ?
                Intrinsic::sadd_with_overflow :
                (f == JL_I::checked_uadd_int ?
                 Intrinsic::uadd_with_overflow :
                 (f == JL_I::checked_ssub_int ?
                  Intrinsic::ssub_with_overflow :
                  (f == JL_I::checked_usub_int ?
                   Intrinsic::usub_with_overflow :
                   (f == JL_I::checked_smul_int ?
                    Intrinsic::smul_with_overflow :
                    Intrinsic::umul_with_overflow)))),
                ArrayRef<Type*>(ix->getType()));
#ifdef LLVM37
        Value *res = builder.CreateCall(intr,{ix, iy});
#else
        Value *res = builder.CreateCall2(intr, ix, iy);
#endif
        Value *obit = builder.CreateExtractValue(res, ArrayRef<unsigned>(1));
        raise_exception_if(obit, literal_pointer_val(jl_overflow_exception));
        return builder.CreateExtractValue(res, ArrayRef<unsigned>(0));
    }

    case JL_I::checked_sdiv_int:
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
                               literal_pointer_val(jl_diverror_exception));

        return builder.CreateSDiv(x, den);
    case JL_I::checked_udiv_int:
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               literal_pointer_val(jl_diverror_exception));
        return builder.CreateUDiv(JL_INT(x), den);

    case JL_I::checked_srem_int:
        return emit_checked_srem_int(JL_INT(x), JL_INT(y));

    case JL_I::checked_urem_int:
        den = JL_INT(y);
        t = den->getType();
        raise_exception_unless(builder.CreateICmpNE(den, ConstantInt::get(t,0)),
                               literal_pointer_val(jl_diverror_exception));
        return builder.CreateURem(JL_INT(x), den);

    case JL_I::check_top_bit:
        // raise InexactError if argument's top bit is set
        x = JL_INT(x);
        raise_exception_if(builder.
                           CreateTrunc(builder.
                                       CreateLShr(x, ConstantInt::get(t, t->getPrimitiveSizeInBits()-1)),
                                       T_int1),
                           literal_pointer_val(jl_inexact_exception));
        return x;

    case JL_I::eq_int:  *newtyp = jl_bool_type; return builder.CreateICmpEQ(JL_INT(x), JL_INT(y));
    case JL_I::ne_int:  *newtyp = jl_bool_type; return builder.CreateICmpNE(JL_INT(x), JL_INT(y));
    case JL_I::slt_int: *newtyp = jl_bool_type; return builder.CreateICmpSLT(JL_INT(x), JL_INT(y));
    case JL_I::ult_int: *newtyp = jl_bool_type; return builder.CreateICmpULT(JL_INT(x), JL_INT(y));
    case JL_I::sle_int: *newtyp = jl_bool_type; return builder.CreateICmpSLE(JL_INT(x), JL_INT(y));
    case JL_I::ule_int: *newtyp = jl_bool_type; return builder.CreateICmpULE(JL_INT(x), JL_INT(y));

    case JL_I::eq_float: *newtyp = jl_bool_type; return math_builder()().CreateFCmpOEQ(FP(x), FP(y));
    case JL_I::ne_float: *newtyp = jl_bool_type; return math_builder()().CreateFCmpUNE(FP(x), FP(y));
    case JL_I::lt_float: *newtyp = jl_bool_type; return math_builder()().CreateFCmpOLT(FP(x), FP(y));
    case JL_I::le_float: *newtyp = jl_bool_type; return math_builder()().CreateFCmpOLE(FP(x), FP(y));

    case JL_I::eq_float_fast: *newtyp = jl_bool_type; return math_builder(true)().CreateFCmpOEQ(FP(x), FP(y));
    case JL_I::ne_float_fast: *newtyp = jl_bool_type; return math_builder(true)().CreateFCmpUNE(FP(x), FP(y));
    case JL_I::lt_float_fast: *newtyp = jl_bool_type; return math_builder(true)().CreateFCmpOLT(FP(x), FP(y));
    case JL_I::le_float_fast: *newtyp = jl_bool_type; return math_builder(true)().CreateFCmpOLE(FP(x), FP(y));

    case JL_I::fpiseq: {
        *newtyp = jl_bool_type;
        Value *xi = JL_INT(x);
        Value *yi = JL_INT(y);
        x = FP(x);
        fy = FP(y);
        return builder.CreateOr(builder.CreateAnd(builder.CreateFCmpUNO(x, x),
                                                  builder.CreateFCmpUNO(fy, fy)),
                                builder.CreateICmpEQ(xi, yi));
    }

    case JL_I::fpislt: {
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

    case JL_I::and_int: return builder.CreateAnd(JL_INT(x), JL_INT(y));
    case JL_I::or_int:  return builder.CreateOr(JL_INT(x), JL_INT(y));
    case JL_I::xor_int: return builder.CreateXor(JL_INT(x), JL_INT(y));
    case JL_I::not_int: return builder.CreateXor(JL_INT(x), ConstantInt::get(t, -1, true));
    case JL_I::shl_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateShl(x, uint_cnvt(t,y)));
    case JL_I::lshr_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         ConstantInt::get(x->getType(),0),
                         builder.CreateLShr(x, uint_cnvt(t,y)));
    case JL_I::ashr_int:
        x = JL_INT(x); y = JL_INT(y);
        return builder.
            CreateSelect(builder.
                         CreateICmpUGE(y, ConstantInt::get(y->getType(),
                                                           x->getType()->getPrimitiveSizeInBits())),
                         builder.CreateAShr(x, ConstantInt::get(x->getType(),
                                                                x->getType()->getPrimitiveSizeInBits()-1)),
                         builder.CreateAShr(x, uint_cnvt(t,y)));
    case JL_I::bswap_int:
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::bswap,
                                      ArrayRef<Type*>(x->getType())), x);
    case JL_I::ctpop_int:
        x = JL_INT(x);
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::ctpop,
                                      ArrayRef<Type*>(x->getType())), x);
    case JL_I::ctlz_int: {
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
    case JL_I::cttz_int: {
        x = JL_INT(x);
        Type *types[1] = {x->getType()};
        Value *cttz = Intrinsic::getDeclaration(jl_Module, Intrinsic::cttz, ArrayRef<Type*>(types));
#ifdef LLVM37
        return builder.CreateCall(cttz, {x, ConstantInt::get(T_int1, 0)});
#else
        return builder.CreateCall2(cttz, x, ConstantInt::get(T_int1, 0));
#endif
    }

    case JL_I::abs_float:
    {
        x = FP(x);
#ifdef LLVM34
        return builder.CreateCall(
            Intrinsic::getDeclaration(jl_Module, Intrinsic::fabs,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
#else
        Type *intt = JL_INTT(x->getType());
        Value *bits = emit_bitcast(FP(x), intt);
        Value *absbits =
            builder.CreateAnd(bits,
                              ConstantInt::get(intt, APInt::getSignedMaxValue(((IntegerType*)intt)->getBitWidth())));
        return emit_bitcast(absbits, x->getType());
#endif
    }
    case JL_I::copysign_float:
    {
        x = FP(x);
        fy = FP(y);
        Type *intt = JL_INTT(x->getType());
        Value *bits = emit_bitcast(x, intt);
        Value *sbits = emit_bitcast(fy, intt);
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
        return emit_bitcast(rbits, x->getType());
    }
    case JL_I::flipsign_int:
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
    case JL_I::ceil_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::ceil,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case JL_I::floor_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::floor,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case JL_I::trunc_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::trunc,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case JL_I::rint_llvm: {
        x = FP(x);
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::rint,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case JL_I::sqrt_llvm: {
        x = FP(x);
        raise_exception_unless(builder.CreateFCmpUGE(x, ConstantFP::get(x->getType(),0.0)),
                               literal_pointer_val(jl_domain_exception));
        return builder.CreateCall(Intrinsic::getDeclaration(jl_Module, Intrinsic::sqrt,
                                                            ArrayRef<Type*>(x->getType())),
                                  x);
    }
    case JL_I::powi_llvm: {
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
    case JL_I::sqrt_llvm_fast: {
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
