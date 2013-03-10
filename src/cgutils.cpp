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
#if defined(_P64)
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

// --- mapping between julia and llvm types ---

static Type *julia_struct_to_llvm(jl_value_t *jt);

static Type *julia_type_to_llvm(jl_value_t *jt)
{
    if (jt == (jl_value_t*)jl_bool_type) return T_int1;
    if (jt == (jl_value_t*)jl_float32_type) return T_float32;
    if (jt == (jl_value_t*)jl_float64_type) return T_float64;
    if (jl_is_cpointer_type(jt)) {
        Type *lt = julia_type_to_llvm(jl_tparam0(jt));
        if (lt == NULL)
            return NULL;
        if (lt == T_void)
            lt = T_int8;
        return PointerType::get(lt, 0);
    }
    if (jl_is_bitstype(jt)) {
        int nb = jl_datatype_size(jt)*8;
        if (nb == 8)  return T_int8;
        if (nb == 16) return T_int16;
        if (nb == 32) return T_int32;
        if (nb == 64) return T_int64;
        else          return Type::getIntNTy(getGlobalContext(), nb);
    }
    if (jl_isbits(jt)) {
        if (((jl_datatype_t*)jt)->size == 0) {
            // TODO: come up with a representation for a 0-size value,
            // and make this 0 size everywhere. as an argument, simply
            // skip passing it.
            return jl_pvalue_llvmt;
        }
        return julia_struct_to_llvm(jt);
    }
    if (jt == (jl_value_t*)jl_bottom_type) return T_void;
    return jl_pvalue_llvmt;
}

static Type *julia_struct_to_llvm(jl_value_t *jt)
{
    if (jl_is_structtype(jt) && !jl_is_array_type(jt)) {
        if (!jl_is_leaf_type(jt))
            return NULL;
        jl_datatype_t *jst = (jl_datatype_t*)jt;
        if (jst->struct_decl == NULL) {
            size_t ntypes = jl_tuple_len(jst->types);
            if (ntypes == 0)
                return NULL;
            std::vector<Type *> latypes(0);
            size_t i;
            for(i = 0; i < ntypes; i++) {
                jl_value_t *ty = jl_tupleref(jst->types, i);
                Type *lty = ty==(jl_value_t*)jl_bool_type ? T_int8 : julia_type_to_llvm(ty);
                if (jst->fields[i].isptr)
                    lty = jl_pvalue_llvmt;
                latypes.push_back(lty);
            }
            jst->struct_decl = (void*)StructType::create(latypes, jst->name->name->name);
        }
        return (Type*)jst->struct_decl;
    }
    return julia_type_to_llvm(jt);
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
            jl_error("internal compiler error: too many bits types");
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
        jl_error("internal compiler error: invalid type id");
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
    const unsigned char *vts = (const unsigned char*)md->getString().data();
    int id = (vts[0]-1) + (vts[1]-1)*255;
    return jl_typeid_to_type(id);
}

static Value *NoOpInst(Value *v)
{
    v = SelectInst::Create(ConstantInt::get(T_int1,1), v, v);
    builder.Insert((Instruction*)v);
    return v;
}

static Value *mark_julia_type(Value *v, jl_value_t *jt)
{
    if (jt == (jl_value_t*)jl_any_type || v->getType()==jl_pvalue_llvmt)
        return v;
    if (has_julia_type(v)) {
        if (julia_type_of(v) == jt)
            return v;
    }
    else if (julia_type_of_without_metadata(v,false) == jt) {
        return v;
    }
    if (dyn_cast<Instruction>(v) == NULL)
        v = NoOpInst(v);
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

static Value *mark_julia_type(Value *v, jl_datatype_t *jt)
{
    return mark_julia_type(v, (jl_value_t*)jt);
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
    return literal_pointer_val(julia_type_of(p));
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
    builder.CreateUnreachable();
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void raise_exception_unless(Value *cond, Value *exc, jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    builder.CreateCall2(jlthrow_line_func, exc,
                        ConstantInt::get(T_int32, ctx->lineno));
    builder.CreateUnreachable();
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
    raise_exception_unless(builder.CreateICmpNE(v,Constant::getNullValue(v->getType())),
                           jlundeferr_var, ctx);
}

static Value *boxed(Value *v, jl_value_t *jt=NULL);

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

#define CHECK_BOUNDS 1

static Value *emit_bounds_check(Value *i, Value *len, jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_size, 1));
#if CHECK_BOUNDS==1
    Value *ok = builder.CreateICmpULT(im1, len);
    raise_exception_unless(ok, jlboundserr_var, ctx);
#endif
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
                              literal_pointer_val((jl_value_t*)jl_datatype_type)));
    BasicBlock *elseBB1 = BasicBlock::Create(getGlobalContext(),"notf", ctx->f);
    BasicBlock *mergeBB1 = BasicBlock::Create(getGlobalContext(),"isf");
    builder.CreateCondBr(isfunc, mergeBB1, elseBB1);

    builder.SetInsertPoint(elseBB1);
    emit_type_error(x, (jl_value_t*)jl_function_type, "apply", ctx);

    builder.CreateBr(mergeBB1);
    ctx->f->getBasicBlockList().push_back(mergeBB1);
    builder.SetInsertPoint(mergeBB1);
}

// --- loading and storing ---

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

static Value *typed_load(Value *ptr, Value *idx_0based, jl_value_t *jltype,
                         jl_codectx_t *ctx)
{
    Type *elty = julia_type_to_llvm(jltype);
    assert(elty != NULL);
    bool isbool=false;
    if (elty==T_int1) { elty = T_int8; isbool=true; }
    Value *data = builder.CreateBitCast(ptr, PointerType::get(elty, 0));
    Value *elt = builder.CreateLoad(builder.CreateGEP(data, idx_0based), false);
    if (elty == jl_pvalue_llvmt) {
        null_pointer_check(elt, ctx);
    }
    if (isbool)
        return builder.CreateTrunc(elt, T_int1);
    return mark_julia_type(elt, jltype);
}

static Value *emit_unbox(Type *to, Type *pto, Value *x);

static Value *typed_store(Value *ptr, Value *idx_0based, Value *rhs,
                          jl_value_t *jltype, jl_codectx_t *ctx)
{
    Type *elty = julia_type_to_llvm(jltype);
    assert(elty != NULL);
    if (elty==T_int1) { elty = T_int8; }
    if (jl_isbits(jltype) && ((jl_datatype_t*)jltype)->size > 0)
        rhs = emit_unbox(elty, PointerType::get(elty,0), rhs);
    else
        rhs = boxed(rhs);
    Value *data = builder.CreateBitCast(ptr, PointerType::get(elty, 0));
    return builder.CreateStore(rhs, builder.CreateGEP(data, idx_0based));
}

// --- convert boolean value to julia ---

static Value *julia_bool(Value *cond)
{
    return builder.CreateSelect(cond,
                                literal_pointer_val(jl_true),
                                literal_pointer_val(jl_false));
}

// --- get the inferred type of an AST node ---

static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true);

static inline jl_module_t *topmod(jl_codectx_t *ctx)
{
    return jl_base_relative_to(ctx->module);
}

static jl_value_t *expr_type(jl_value_t *e, jl_codectx_t *ctx)
{
    if (jl_is_expr(e))
        return ((jl_expr_t*)e)->etype;
    if (jl_is_symbolnode(e))
        return jl_symbolnode_type(e);
    if (jl_is_quotenode(e))
        return (jl_value_t*)jl_typeof(jl_fieldref(e,0));
    if (jl_is_lambda_info(e))
        return (jl_value_t*)jl_function_type;
    if (jl_is_getfieldnode(e)) {
        jl_value_t *v = static_eval(e, ctx);
        if (v == NULL)
            return jl_getfieldnode_type(e);
        e = v;
        goto type_of_constant;
    }
    if (jl_is_topnode(e)) {
        e = jl_fieldref(e,0);
        jl_binding_t *b = jl_get_binding(topmod(ctx), (jl_sym_t*)e);
        if (!b || !b->value)
            return jl_top_type;
        if (b->constp) {
            e = b->value;
            goto type_of_constant;
        }
        else {
            return (jl_value_t*)jl_any_type;
        }
    }
    if (jl_is_symbol(e)) {
        if (jl_is_symbol(e)) {
            if (is_global((jl_sym_t*)e, ctx)) {
                // look for static parameter
                for(size_t i=0; i < jl_tuple_len(ctx->sp); i+=2) {
                    assert(jl_is_symbol(jl_tupleref(ctx->sp, i)));
                    if (e == jl_tupleref(ctx->sp, i)) {
                        e = jl_tupleref(ctx->sp, i+1);
                        goto type_of_constant;
                    }
                }
            }
            else {
                return (jl_value_t*)jl_any_type;
            }
        }
        jl_binding_t *b = jl_get_binding(ctx->module, (jl_sym_t*)e);
        if (!b || !b->value)
            return jl_top_type;
        if (b->constp)
            e = b->value;
        else
            return (jl_value_t*)jl_any_type;
    }
type_of_constant:
    if (jl_is_datatype(e))
        return (jl_value_t*)jl_wrap_Type(e);
    return (jl_value_t*)jl_typeof(e);
}

// --- accessing the representations of built-in data types ---

static Value *emit_tuplelen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 1);
#ifdef _P64
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
#ifdef _P64
    return builder.CreateSExt(valen, T_int64);
#else
    return valen;
#endif
}

static Value *emit_arraysize(Value *t, Value *dim)
{
#ifdef _P64
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

static Value *data_pointer(Value *x)
{
    return builder.CreateGEP(builder.CreateBitCast(x, jl_ppvalue_llvmt),
                             ConstantInt::get(T_size, 1));
}

static Value *emit_array_nd_index(Value *a, size_t nd, jl_value_t **args,
                                  size_t nidxs, jl_codectx_t *ctx)
{
    Value *i = ConstantInt::get(T_size, 0);
    Value *stride = ConstantInt::get(T_size, 1);
#if CHECK_BOUNDS==1
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(), "oob");
    BasicBlock *endBB = BasicBlock::Create(getGlobalContext(), "idxend");
#endif
    for(size_t k=0; k < nidxs; k++) {
        Value *ii = emit_unbox(T_size, T_psize, emit_unboxed(args[k], ctx));
        ii = builder.CreateSub(ii, ConstantInt::get(T_size, 1));
        i = builder.CreateAdd(i, builder.CreateMul(ii, stride));
        if (k < nidxs-1) {
            Value *d =
                k >= nd ? ConstantInt::get(T_size, 1) : emit_arraysize(a, k+1);
#if CHECK_BOUNDS==1
            BasicBlock *okBB = BasicBlock::Create(getGlobalContext(), "ib");
            // if !(i < d) goto error
            builder.CreateCondBr(builder.CreateICmpULT(ii, d), okBB, failBB);
            ctx->f->getBasicBlockList().push_back(okBB);
            builder.SetInsertPoint(okBB);
#endif
            stride = builder.CreateMul(stride, d);
        }
    }
#if CHECK_BOUNDS==1
    Value *alen = emit_arraylen(a);
    // if !(i < alen) goto error
    builder.CreateCondBr(builder.CreateICmpULT(i, alen), endBB, failBB);

    ctx->f->getBasicBlockList().push_back(failBB);
    builder.SetInsertPoint(failBB);
    builder.CreateCall2(jlthrow_line_func, builder.CreateLoad(jlboundserr_var),
                        ConstantInt::get(T_int32, ctx->lineno));
    builder.CreateUnreachable();

    ctx->f->getBasicBlockList().push_back(endBB);
    builder.SetInsertPoint(endBB);
#endif

    return i;
}

// --- propagate julia type from value a to b. returns b. ---

static Value *tpropagate(Value *a, Value *b)
{
    if (has_julia_type(a))
        return mark_julia_type(b, julia_type_of(a));
    return b;
}

// --- boxing ---

static Value *init_bits_value(Value *newv, Value *jt, Type *t, Value *v)
{
    builder.CreateStore(jt, builder.CreateBitCast(newv, jl_ppvalue_llvmt));
    builder.CreateStore(v , builder.CreateBitCast(data_pointer(newv),
                                                  PointerType::get(t,0)));
    return newv;
}

// allocate a box where the type might not be known at compile time
static Value *allocate_box_dynamic(Value *jlty, int nb, Value *v)
{
    if (v->getType()->isPointerTy()) {
        v = builder.CreatePtrToInt(v, T_size);
    }
    size_t sz = sizeof(void*) + nb;
    Value *newv = builder.CreateCall(jlallocobj_func,
                                     ConstantInt::get(T_size, sz));
    // TODO: make sure this is rooted. I think it is.
    return init_bits_value(newv, jlty, v->getType(), v);
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(Value *v, jl_value_t *jt)
{
    Type *t = v->getType();
    if (t == jl_pvalue_llvmt)
        return v;
    if (t == T_void)
        return literal_pointer_val((jl_value_t*)jl_nothing);
    if (t == T_int1) return julia_bool(v);
    if (jt == NULL)
        jt = julia_type_of(v);
    jl_datatype_t *jb = (jl_datatype_t*)jt;
    assert(jl_is_datatype(jb));
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
#ifdef _P64
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
    if (!jl_isbits(jt)) {
        assert("Don't know how to box this type" && false);
        return NULL;
    }
    if (!jb->abstract && jb->size == 0) {
        if (jb->instance == NULL)
            jl_new_struct_uninit(jb);
        assert(jb->instance != NULL);
        return literal_pointer_val(jb->instance);
    }
    return allocate_box_dynamic(literal_pointer_val(jt),jl_datatype_size(jt),v);
}


static void emit_cpointercheck(Value *x, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *t = emit_typeof(x);
    emit_typecheck(t, (jl_value_t*)jl_datatype_type, msg, ctx);

    Value *istype =
        builder.CreateICmpEQ(emit_nthptr(t, offsetof(jl_datatype_t,name)/sizeof(char*)),
                literal_pointer_val((jl_value_t*)jl_pointer_type->name));
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    emit_type_error(x, (jl_value_t*)jl_pointer_type, msg, ctx);

    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}
