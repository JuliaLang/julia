// --- the ccall intrinsic ---

static Function *value_to_pointer_func;

// TODO: per-thread
static char *temp_arg_area;
static const uint32_t arg_area_sz = 4196;
static uint32_t arg_area_loc;
#define N_TEMP_ARG_BLOCKS 1024
static void *temp_arg_blocks[N_TEMP_ARG_BLOCKS];
static uint32_t arg_block_n = 0;
static Function *save_arg_area_loc_func;
static Function *restore_arg_area_loc_func;

static uint64_t save_arg_area_loc()
{
    return (((uint64_t)arg_block_n)<<32) | ((uint64_t)arg_area_loc);
}

static void restore_arg_area_loc(uint64_t l)
{
    arg_area_loc = l&0xffffffff;
    uint32_t ab = l>>32;
    while (arg_block_n > ab) {
        arg_block_n--;
        free(temp_arg_blocks[arg_block_n]);
    }
}

static void *alloc_temp_arg_space(uint32_t sz)
{
    void *p;
    if (arg_area_loc+sz > arg_area_sz) {
#ifdef JL_GC_MARKSWEEP
        if (arg_block_n >= N_TEMP_ARG_BLOCKS)
            jl_error("internal compiler error: out of temporary argument space in ccall");
        p = malloc(sz);
        temp_arg_blocks[arg_block_n++] = p;
#else
#error "fixme"
#endif
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

// this is a run-time function
// warning: cannot allocate memory except using alloc_temp_arg_space
extern "C" void *jl_value_to_pointer(jl_value_t *jt, jl_value_t *v, int argn)
{
    if ((jl_value_t*)jl_typeof(v) == jt) {
        assert(jl_is_bits_type(jt));
        size_t osz = jl_bitstype_nbits(jt)/8;
        return alloc_temp_arg_copy(jl_bits_data(v), osz);
    }
    if (((jl_value_t*)jl_uint8_type == jt ||
         (jl_value_t*)jl_int8_type == jt) && jl_is_byte_string(v)) {
        return jl_string_data(v);
    }
    if (jl_is_array(v)) {
        if (jl_tparam0(jl_typeof(v)) == jt || jt==(jl_value_t*)jl_bottom_type)
            return ((jl_array_t*)v)->data;
        if (jl_is_cpointer_type(jt)) {
            jl_array_t *ar = (jl_array_t*)v;
            void **temp=(void**)alloc_temp_arg_space(jl_array_len(ar)*sizeof(void*));
            size_t i;
            for(i=0; i < jl_array_len(ar); i++) {
                temp[i] = jl_value_to_pointer(jl_tparam0(jt),
                                              jl_arrayref(ar, i), argn);
            }
            return temp;
        }
    }
    std::map<int, std::string>::iterator it = argNumberStrings.find(argn);
    if (it == argNumberStrings.end()) {
        std::stringstream msg;
        msg << "argument ";
        msg << argn;
        argNumberStrings[argn] = msg.str();
        it = argNumberStrings.find(argn);
    }
    jl_value_t *targ=NULL, *pty=NULL;
    JL_GC_PUSH(&targ, &pty);
    targ = (jl_value_t*)jl_tuple1(jt);
    pty = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                     (jl_tuple_t*)targ);
    jl_type_error_rt("ccall", (*it).second.c_str(), pty, v);
    // doesn't return
    return (jl_value_t*)jl_null;
}

static Value *julia_to_native(Type *ty, jl_value_t *jt, Value *jv,
                              jl_value_t *argex, bool addressOf,
                              int argn, jl_codectx_t *ctx)
{
    Type *vt = jv->getType();
    if (ty == jl_pvalue_llvmt) {
        return boxed(jv);
    }
    else if (ty == vt && !addressOf) {
        return jv;
    }
    else if (vt != jl_pvalue_llvmt) {
        // argument value is unboxed
        if (addressOf) {
            if (ty->isPointerTy() && ty->getContainedType(0)==vt) {
                // pass the address of an alloca'd thing, not a box
                // since those are immutable.
                Value *slot = builder.CreateAlloca(vt);
                builder.CreateStore(jv, slot);
                return builder.CreateBitCast(slot, ty);
            }
        }
        else if ((vt->isIntegerTy() && ty->isIntegerTy()) ||
                 (vt->isFloatingPointTy() && ty->isFloatingPointTy()) ||
                 (vt->isPointerTy() && ty->isPointerTy())) {
            if (vt->getPrimitiveSizeInBits() ==
                ty->getPrimitiveSizeInBits()) {
                return builder.CreateBitCast(jv, ty);
            }
        }
        // error. box for error handling.
        jv = boxed(jv);
    }
    else if (jl_is_cpointer_type(jt) && addressOf) {
        jl_value_t *aty = expr_type(argex, ctx);
        if (jl_is_array_type(aty) &&
            (jl_tparam0(jt) == jl_tparam0(aty) ||
             jl_tparam0(jt) == (jl_value_t*)jl_bottom_type)) {
            // array to pointer
            return builder.CreateBitCast(emit_arrayptr(jv), ty);
        }
        Value *p = builder.CreateCall3(value_to_pointer_func,
                                       literal_pointer_val(jl_tparam0(jt)), jv,
                                       ConstantInt::get(T_int32, argn));
        assert(ty->isPointerTy());
        return builder.CreateBitCast(p, ty);
    }
    // TODO: error for & with non-pointer argument type
    assert(jl_is_bits_type(jt));
    std::stringstream msg;
    msg << "ccall argument ";
    msg << argn;
    emit_typecheck(jv, jt, msg.str(), ctx);
    Value *p = bitstype_pointer(jv);
    return builder.CreateLoad(builder.CreateBitCast(p,
                                                    PointerType::get(ty,0)),
                              false);
}

// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *ptr=NULL, *rt=NULL, *at=NULL;
    JL_GC_PUSH(&ptr, &rt, &at);
    ptr = jl_interpret_toplevel_expr_in(ctx->module, args[1],
                                        &jl_tupleref(ctx->sp,0),
                                        jl_tuple_len(ctx->sp)/2);
    rt  = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                        &jl_tupleref(ctx->sp,0),
                                        jl_tuple_len(ctx->sp)/2);
    if (jl_is_tuple(rt)) {
        std::string msg = "in " + ctx->funcName +
            ": ccall: missing return type";
        jl_error(msg.c_str());
    }
    at  = jl_interpret_toplevel_expr_in(ctx->module, args[3],
                                        &jl_tupleref(ctx->sp,0),
                                        jl_tuple_len(ctx->sp)/2);
    void *fptr;
    if (jl_is_symbol(ptr)) {
        // just symbol, default to JuliaDLHandle
#ifdef __WIN32__

        fptr = jl_dlsym_e(jl_dl_handle, ((jl_sym_t*)ptr)->name);
        if(!fptr) {
            fptr = jl_dlsym_e(jl_kernel32_handle, ((jl_sym_t*)ptr)->name);
            if(!fptr) {
                fptr = jl_dlsym_e(jl_ntdll_handle, ((jl_sym_t*)ptr)->name);
                if(!fptr) {
                    fptr = jl_dlsym_e(jl_crtdll_handle, ((jl_sym_t*)ptr)->name);
                    if(!fptr) {
                        fptr = jl_dlsym(jl_winsock_handle, ((jl_sym_t*)ptr)->name);
                    }
                }
            }
        }
#else
        fptr = jl_dlsym(jl_dl_handle, ((jl_sym_t*)ptr)->name);
#endif
    }
    else {
        JL_TYPECHK(ccall, pointer, ptr);
        fptr = *(void**)jl_bits_data(ptr);
    }
    if (fptr == NULL) {
        JL_GC_POP();
        emit_error("ccall: null function pointer", ctx);
        return literal_pointer_val(jl_nothing);
    }
    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, tuple, at);
    JL_TYPECHK(ccall, type, at);
    jl_tuple_t *tt = (jl_tuple_t*)at;
    std::vector<Type *> fargt(0);
    std::vector<Type *> fargt_sig(0);
    Type *lrt = julia_type_to_llvm(rt);
    if (lrt == NULL) {
        JL_GC_POP();
        return literal_pointer_val(jl_nothing);
    }
    size_t i;
    bool haspointers = false;
    bool isVa = false;
    for(i=0; i < jl_tuple_len(tt); i++) {
        jl_value_t *tti = jl_tupleref(tt,i);
        if (jl_is_seq_type(tti)) {
            isVa = true;
            tti = jl_tparam0(tti);
        }
        Type *t = julia_type_to_llvm(tti);
        if (t == NULL) {
            JL_GC_POP();
            return literal_pointer_val(jl_nothing);
        }
        fargt.push_back(t);
        if (!isVa)
            fargt_sig.push_back(t);
    }
    // check for calling convention specifier
    CallingConv::ID cc = CallingConv::C;
    jl_value_t *last = args[nargs];
    if (jl_is_expr(last)) {
        jl_sym_t *lhd = ((jl_expr_t*)last)->head;
        if (lhd == jl_symbol("stdcall")) {
            cc = CallingConv::X86_StdCall;
            nargs--;
        }
        else if (lhd == jl_symbol("cdecl")) {
            cc = CallingConv::C;
            nargs--;
        }
        else if (lhd == jl_symbol("fastcall")) {
            cc = CallingConv::X86_FastCall;
            nargs--;
        }
    }
    
    if ((!isVa && jl_tuple_len(tt)  != (nargs-2)/2) ||
        ( isVa && jl_tuple_len(tt)-1 > (nargs-2)/2))
        jl_error("ccall: wrong number of arguments to C function");

    // some special functions
    if (fptr == &jl_array_ptr) {
        Value *ary = emit_expr(args[4], ctx);
        JL_GC_POP();
        return mark_julia_type(builder.CreateBitCast(emit_arrayptr(ary),lrt),
                               rt);
    }

    // see if there are & arguments
    for(i=4; i < nargs+1; i+=2) {
        jl_value_t *argi = args[i];
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            haspointers = true;
            break;
        }
    }

    // make LLVM function object for the target
    Function *llvmf =
        Function::Create(FunctionType::get(lrt, fargt_sig, isVa),
                         Function::ExternalLinkage,
                         "ccall_", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(llvmf, fptr);

    // save temp argument area stack pointer
    Value *saveloc=NULL;
    Value *stacksave=NULL;
    if (haspointers) {
        // TODO: inline this
        saveloc = builder.CreateCall(save_arg_area_loc_func);
        stacksave =
            builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                         Intrinsic::stacksave));
    }

    // emit arguments
    Value *argvals[(nargs-3)/2];
    int last_depth = ctx->argDepth;
    int nargty = jl_tuple_len(tt);
    for(i=4; i < nargs+1; i+=2) {
        int ai = (i-4)/2;
        jl_value_t *argi = args[i];
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        Type *largty;
        jl_value_t *jargty;
        if (isVa && ai >= nargty-1) {
            largty = fargt[nargty-1];
            jargty = jl_tparam0(jl_tupleref(tt,nargty-1));
        }
        else {
            largty = fargt[ai];
            jargty = jl_tupleref(tt,ai);
        }
        Value *arg;
        if (largty == jl_pvalue_llvmt)
            arg = emit_expr(argi, ctx, true);
        else
            arg = emit_unboxed(argi, ctx);
        /*
#ifdef JL_GC_MARKSWEEP
        // make sure args are rooted
        if (largty->isPointerTy() &&
            (largty == jl_pvalue_llvmt ||
             !jl_is_bits_type(expr_type(args[i], ctx)))) {
            make_gcroot(boxed(arg), ctx);
        }
#endif
        */
        argvals[ai] = julia_to_native(largty, jargty, arg, argi, addressOf,
                                      ai+1, ctx);
    }
    // the actual call
    Value *result = builder.CreateCall(llvmf,
                                       ArrayRef<Value*>(&argvals[0],(nargs-3)/2));
    if (cc != CallingConv::C)
        ((CallInst*)result)->setCallingConv(cc);

    // restore temp argument area stack pointer
    if (haspointers) {
        assert(saveloc != NULL);
        builder.CreateCall(restore_arg_area_loc_func, saveloc);
        assert(stacksave != NULL);
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stackrestore),
                           stacksave);
    }
    ctx->argDepth = last_depth;

    JL_GC_POP();
    if (lrt == T_void)
        return literal_pointer_val((jl_value_t*)jl_nothing);
    return mark_julia_type(result, rt);
}
