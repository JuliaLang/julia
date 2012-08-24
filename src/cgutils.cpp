// utility procedures used in code generation

// --- string constants ---

static std::map<const std::string, GlobalVariable*> stringConstants;

static GlobalVariable *stringConst(const std::string &txt)
{
    GlobalVariable *gv = stringConstants[txt];
    static int strno = 0;
    if (gv == NULL) {
        std::stringstream ssno;
        std::string vname;
        ssno << strno;
        vname += "_j_str";
        vname += ssno.str();
        gv = new GlobalVariable(*jl_Module,
                                ArrayType::get(T_int8, txt.length()+1),
                                true,
                                GlobalVariable::ExternalLinkage,
#ifndef LLVM_VERSION_MAJOR
                                ConstantArray::get(getGlobalContext(),
                                                       txt.c_str()),
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 1
                                ConstantDataArray::get(getGlobalContext(),
                                                       ArrayRef<unsigned char>(
                                                       (const unsigned char*)txt.c_str(),
                                                       txt.length()+1)),
#endif
        vname);
        stringConstants[txt] = gv;
        strno++;
    }
    return gv;

}

// --- emitting pointers directly into code ---

static Value *literal_pointer_val(void *p, Type *t)
{
#ifdef __LP64__
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int64, (uint64_t)p),
                                     t);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int32, (uint32_t)p),
                                     t);
#endif
}

static Value *literal_pointer_val(jl_value_t *p)
{
    return literal_pointer_val(p, jl_pvalue_llvmt);
}

static Value *literal_pointer_val(void *p)
{
    return literal_pointer_val(p, T_pint8);
}

// --- generating various error checks ---

static jl_value_t *llvm_type_to_julia(Type *t, bool err=true);

static Value *emit_typeof(Value *p)
{
    // given p, a jl_value_t*, compute its type tag
    if (p->getType() == jl_pvalue_llvmt) {
        Value *tt = builder.CreateBitCast(p, jl_ppvalue_llvmt);
        tt = builder.
            CreateLoad(builder.CreateGEP(tt,ConstantInt::get(T_size,0)),
                       false);
        return tt;
    }
    return literal_pointer_val(llvm_type_to_julia(p->getType()));
}

static void emit_error(const std::string &txt, jl_codectx_t *ctx)
{
    std::string txt2 = "in " + ctx->funcName + ": " + txt;
    Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                        ConstantInt::get(T_int32, 0) };
    builder.CreateCall(jlerror_func,
                       builder.CreateGEP(stringConst(txt2),
                                         ArrayRef<Value*>(zeros)));
}

static void error_unless(Value *cond, const std::string &msg, jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    emit_error(msg, ctx);
    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void raise_exception_unless(Value *cond, Value *exc, jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    builder.CreateCall(jlraise_func, exc);
    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void raise_exception_unless(Value *cond, GlobalVariable *exc,
                                   jl_codectx_t *ctx)
{
    raise_exception_unless(cond, (Value*)builder.CreateLoad(exc, false), ctx);
}

static void raise_exception_if(Value *cond, Value *exc, jl_codectx_t *ctx)
{
    raise_exception_unless(builder.CreateXor(cond, ConstantInt::get(T_int1,-1)),
                           exc, ctx);
}

static void raise_exception_if(Value *cond, GlobalVariable *exc,
                               jl_codectx_t *ctx)
{
    raise_exception_if(cond, (Value*)builder.CreateLoad(exc, false), ctx);
}

static void null_pointer_check(Value *v, jl_codectx_t *ctx)
{
    raise_exception_unless(builder.CreateICmpNE(v,V_null), jlundeferr_var, ctx);
}

static Value *boxed(Value *v);

static void emit_type_error(Value *x, jl_value_t *type, const std::string &msg,
                            jl_codectx_t *ctx)
{
    Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                        ConstantInt::get(T_int32, 0) };
    Value *fname_val = builder.CreateGEP(stringConst(ctx->funcName),
                                         ArrayRef<Value*>(zeros));
    Value *msg_val = builder.CreateGEP(stringConst(msg),
                                       ArrayRef<Value*>(zeros));
    builder.CreateCall4(jltypeerror_func,
                        fname_val, msg_val,
                        literal_pointer_val(type), boxed(x));
}

static void emit_typecheck(Value *x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype =
        builder.CreateICmpEQ(emit_typeof(x), literal_pointer_val(type));
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    emit_type_error(x, type, msg, ctx);

    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static Value *emit_bounds_check(Value *i, Value *len, const std::string &msg,
                                jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_size, 1));
    Value *ok = builder.CreateICmpULT(im1, len);
    error_unless(ok, msg, ctx);
    return im1;
}

static void emit_func_check(Value *x, jl_codectx_t *ctx)
{
    Value *xty = emit_typeof(x);
    Value *isfunc =
        builder.
        CreateOr(builder.
                 CreateICmpEQ(xty,
                              literal_pointer_val((jl_value_t*)jl_function_type)),
                 builder.
                 CreateICmpEQ(xty,
                              literal_pointer_val((jl_value_t*)jl_struct_kind)));
    BasicBlock *elseBB1 = BasicBlock::Create(getGlobalContext(),"notf", ctx->f);
    BasicBlock *mergeBB1 = BasicBlock::Create(getGlobalContext(),"isf");
    builder.CreateCondBr(isfunc, mergeBB1, elseBB1);

    builder.SetInsertPoint(elseBB1);
    emit_type_error(x, (jl_value_t*)jl_function_type, "apply", ctx);

    builder.CreateBr(mergeBB1);
    ctx->f->getBasicBlockList().push_back(mergeBB1);
    builder.SetInsertPoint(mergeBB1);
}

// --- loading and storing the Nth word in an object ---

static Value *emit_nthptr_addr(Value *v, size_t n)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt),
                             ConstantInt::get(T_size, n));
}

static Value *emit_nthptr_addr(Value *v, Value *idx)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt), idx);
}

static Value *emit_nthptr(Value *v, size_t n)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, n);
    return builder.CreateLoad(vptr, false);
}

static Value *emit_nthptr(Value *v, Value *idx)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, idx);
    return builder.CreateLoad(vptr, false);
}

// --- convert boolean value to julia ---

static Value *julia_bool(Value *cond)
{
    return builder.CreateSelect(cond,
                                literal_pointer_val(jl_true),
                                literal_pointer_val(jl_false));
}

// --- get the inferred type of an AST node ---

static jl_value_t *expr_type(jl_value_t *e, jl_codectx_t *ctx)
{
    if (jl_is_expr(e))
        return ((jl_expr_t*)e)->etype;
    if (jl_is_symbolnode(e))
        return jl_symbolnode_type(e);
    if (jl_is_quotenode(e))
        return (jl_value_t*)jl_typeof(jl_fieldref(e,0));
    if (jl_is_symbol(e))
        return (jl_value_t*)jl_any_type;
    if (jl_is_lambda_info(e))
        return (jl_value_t*)jl_function_type;
    if (jl_is_topnode(e)) {
        jl_binding_t *b = jl_get_binding(ctx->module,
                                         (jl_sym_t*)jl_fieldref(e,0));
        if (!b || !b->value)
            return jl_top_type;
        if (b->constp)
            e = b->value;
        else
            return (jl_value_t*)jl_any_type;
    }
    if (jl_is_getfieldnode(e))
        return jl_getfieldnode_type(e);
    if (jl_is_some_tag_type(e))
        return (jl_value_t*)jl_wrap_Type(e);
    return (jl_value_t*)jl_typeof(e);
}

// --- accessing the representations of built-in data types ---

static Value *emit_tuplelen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 1);
#ifdef __LP64__
    return builder.CreatePtrToInt(lenbits, T_int64);
#else
    return builder.CreatePtrToInt(lenbits, T_int32);
#endif
}

// emit length of vararg tuple
static Value *emit_n_varargs(jl_codectx_t *ctx)
{
    int nreq = ctx->nReqArgs;
    Value *valen = builder.CreateSub((Value*)ctx->argCount,
                                     ConstantInt::get(T_int32, nreq));
#ifdef __LP64__
    return builder.CreateSExt(valen, T_int64);
#else
    return valen;
#endif
}

static Value *emit_arraysize(Value *t, Value *dim)
{
#ifdef __LP64__
    int o = 3;
#else
    int o = 4;
#endif
    Value *dbits =
        emit_nthptr(t, builder.CreateAdd(dim,
                                         ConstantInt::get(dim->getType(), o)));
    return builder.CreatePtrToInt(dbits, T_size);
}

static Value *emit_arraysize(Value *t, int dim)
{
    return emit_arraysize(t, ConstantInt::get(T_int32, dim));
}

static Value *emit_arraylen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 2);
    return builder.CreatePtrToInt(lenbits, T_size);
}

static Value *emit_arrayptr(Value *t)
{
    return emit_nthptr(t, 1);
}

static Value *bitstype_pointer(Value *x)
{
    return builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                             ConstantInt::get(T_size, 1));
}

// --- scheme for tagging llvm values with julia types using metadata ---

static std::map<int, jl_value_t*> typeIdToType;
static std::map<jl_value_t*, int> typeToTypeId;
static int cur_type_id = 1;

static int jl_type_to_typeid(jl_value_t *t)
{
    std::map<jl_value_t*, int>::iterator it = typeToTypeId.find(t);
    if (it == typeToTypeId.end()) {
        int mine = cur_type_id++;
        if (mine > 65025)
            jl_error("unexpected error: too many bits types");
        typeToTypeId[t] = mine;
        typeIdToType[mine] = t;
        return mine;
    }
    return (*it).second;
}

static jl_value_t *jl_typeid_to_type(int i)
{
    std::map<int, jl_value_t*>::iterator it = typeIdToType.find(i);
    if (it == typeIdToType.end()) {
        jl_error("unexpected error: invalid type id");
    }
    return (*it).second;
}

static bool has_julia_type(Value *v)
{
    return ((dyn_cast<Instruction>(v) != NULL) &&
            ((Instruction*)v)->getMetadata("julia_type")!=NULL);
}

static jl_value_t *julia_type_of_without_metadata(Value *v, bool err=true)
{
    if (dyn_cast<AllocaInst>(v) != NULL ||
        dyn_cast<GetElementPtrInst>(v) != NULL) {
        // an alloca always has llvm type pointer
        return llvm_type_to_julia(v->getType()->getContainedType(0), err);
    }
    return llvm_type_to_julia(v->getType(), err);
}

static jl_value_t *julia_type_of(Value *v)
{
    MDNode *mdn;
    if (dyn_cast<Instruction>(v) == NULL ||
        (mdn = ((Instruction*)v)->getMetadata("julia_type")) == NULL) {
        return julia_type_of_without_metadata(v, true);
    }
    MDString *md = (MDString*)mdn->getOperand(0);
    const char *vts = md->getString().data();
    int id = (vts[0]-1) + (vts[1]-1)*255;
    return jl_typeid_to_type(id);
}

static Value *NoOpCast(Value *v)
{
    v = CastInst::Create(Instruction::BitCast, v, v->getType());
    builder.Insert((Instruction*)v);
    return v;
}

static Value *mark_julia_type(Value *v, jl_value_t *jt)
{
    if (jt == (jl_value_t*)jl_any_type)
        return v;
    if (has_julia_type(v) && julia_type_of(v) == jt)
        return v;
    if (julia_type_of_without_metadata(v,false) == jt)
        return NoOpCast(v);
    if (dyn_cast<Instruction>(v) == NULL)
        v = NoOpCast(v);
    assert(dyn_cast<Instruction>(v));
    char name[3];
    int id = jl_type_to_typeid(jt);
    // store id as base-255 to avoid NUL
    name[0] = (id%255)+1;
    name[1] = (id/255)+1;
    name[2] = '\0';
    MDString *md = MDString::get(jl_LLVMContext, name);
    MDNode *mdn = MDNode::get(jl_LLVMContext, ArrayRef<Value*>(md));
    ((Instruction*)v)->setMetadata("julia_type", mdn);
    return v;
}

static Value *mark_julia_type(Value *v, jl_bits_type_t *jt)
{
    return mark_julia_type(v, (jl_value_t*)jt);
}

// --- propagate julia type from value a to b. returns b. ---

static Value *tpropagate(Value *a, Value *b)
{
    if (has_julia_type(a))
        return mark_julia_type(b, julia_type_of(a));
    return b;
}

// --- mapping between julia and llvm types ---

static Type *julia_type_to_llvm(jl_value_t *jt, jl_codectx_t *ctx)
{
    if (jt == (jl_value_t*)jl_bool_type) return T_int1;
    if (jt == (jl_value_t*)jl_float32_type) return T_float32;
    if (jt == (jl_value_t*)jl_float64_type) return T_float64;
    //if (jt == (jl_value_t*)jl_null) return T_void;
    if (jl_is_cpointer_type(jt)) {
        Type *lt = julia_type_to_llvm(jl_tparam0(jt), ctx);
        if (lt == NULL)
            return NULL;
        if (lt == T_void)
            lt = T_int8;
        return PointerType::get(lt, 0);
    }
    if (jl_is_bits_type(jt)) {
        int nb = jl_bitstype_nbits(jt);
        if (nb == 8)  return T_int8;
        if (nb == 16) return T_int16;
        if (nb == 32) return T_int32;
        if (nb == 64) return T_int64;
        else          return Type::getIntNTy(getGlobalContext(), nb);
    }
    if (jt == (jl_value_t*)jl_bottom_type) return T_void;
    //if (jt == (jl_value_t*)jl_any_type)
    //    return jl_pvalue_llvmt;
    return jl_pvalue_llvmt;
    //emit_type_error(literal_pointer_val(jt), (jl_value_t*)jl_bits_kind,
    //                "conversion to native type", ctx);
    //return NULL;
}

// NOTE: llvm cannot express all julia types (for example unsigned),
// so this is an approximation. it's only correct if the associated LLVM
// value is not tagged with our value name hack.
// boxed(v) below gets the correct type.
static jl_value_t *llvm_type_to_julia(Type *t, bool throw_error)
{
    if (t == T_int1)  return (jl_value_t*)jl_bool_type;
    if (t == T_int8)  return (jl_value_t*)jl_int8_type;
    if (t == T_int16) return (jl_value_t*)jl_int16_type;
    if (t == T_int32) return (jl_value_t*)jl_int32_type;
    if (t == T_int64) return (jl_value_t*)jl_int64_type;
    if (t == T_float32) return (jl_value_t*)jl_float32_type;
    if (t == T_float64) return (jl_value_t*)jl_float64_type;
    if (t == T_void) return (jl_value_t*)jl_bottom_type;
    if (t == jl_pvalue_llvmt)
        return (jl_value_t*)jl_any_type;
    if (t->isPointerTy()) {
        jl_value_t *elty = llvm_type_to_julia(t->getContainedType(0),
                                              throw_error);
        if (elty != NULL) {
            return (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                              jl_tuple1(elty));
        }
    }
    if (throw_error) {
        jl_error("cannot convert type to a julia type");
    }
    return NULL;
}

// --- boxing ---

static Value *init_bits_value(Value *newv, Value *jt, Type *t, Value *v)
{
    builder.CreateStore(jt, builder.CreateBitCast(newv, jl_ppvalue_llvmt));
    builder.CreateStore(v , builder.CreateBitCast(bitstype_pointer(newv),
                                                  PointerType::get(t,0)));
    return newv;
}

// allocate a box where the type might not be known at compile time
static Value *allocate_box_dynamic(Value *jlty, int nb, Value *v)
{
    if (v->getType()->isPointerTy()) {
        v = builder.CreatePtrToInt(v, T_size);
    }
    if (nb == 8)
        return builder.CreateCall2(box8_func,  jlty, v);
    if (nb == 16)
        return builder.CreateCall2(box16_func, jlty, v);
    if (nb == 32)
        return builder.CreateCall2(box32_func, jlty, v);
    if (nb == 64)
        return builder.CreateCall2(box64_func, jlty, v);
    size_t sz = sizeof(void*) + (nb+7)/8;
    Value *newv = builder.CreateCall(jlallocobj_func,
                                     ConstantInt::get(T_size, sz));
    // TODO: make sure this is rooted. I think it is.
    return init_bits_value(newv, jlty, v->getType(), v);
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(Value *v)
{
    Type *t = v->getType();
    if (t == jl_pvalue_llvmt)
        return v;
    if (t == T_void)
        return literal_pointer_val((jl_value_t*)jl_nothing);
    if (t == T_int1) return julia_bool(v);
    jl_value_t *jt = julia_type_of(v);
    jl_bits_type_t *jb = (jl_bits_type_t*)jt;
    if (jb == jl_int8_type)
        return builder.CreateCall(box_int8_func,
                                  builder.CreateSExt(v, T_int32));
    if (jb == jl_int16_type) return builder.CreateCall(box_int16_func, v);
    if (jb == jl_int32_type) return builder.CreateCall(box_int32_func, v);
    if (jb == jl_int64_type) return builder.CreateCall(box_int64_func, v);
    if (jb == jl_float32_type) return builder.CreateCall(box_float32_func, v);
    //if (jb == jl_float64_type) return builder.CreateCall(box_float64_func, v);
    if (jb == jl_float64_type) {
        // manually inline alloc & init of Float64 box. cheap, I know.
#ifdef __LP64__
        Value *newv = builder.CreateCall(jlalloc2w_func);
#else
        Value *newv = builder.CreateCall(jlalloc3w_func);
#endif
        return init_bits_value(newv, literal_pointer_val(jt), t, v);
    }
    if (jb == jl_uint8_type)
        return builder.CreateCall(box_uint8_func,
                                  builder.CreateZExt(v, T_int32));
    if (jb == jl_uint16_type) return builder.CreateCall(box_uint16_func, v);
    if (jb == jl_uint32_type) return builder.CreateCall(box_uint32_func, v);
    if (jb == jl_uint64_type) return builder.CreateCall(box_uint64_func, v);
    if (jb == jl_char_type)   return builder.CreateCall(box_char_func, v);
    // TODO: skip the call for constant arguments
    if (!jl_is_bits_type(jt)) {
        assert("Don't know how to box this type" && false);
        return NULL;
    }
    int nb = jl_bitstype_nbits(jt);
    return allocate_box_dynamic(literal_pointer_val(jt), nb, v);
}
