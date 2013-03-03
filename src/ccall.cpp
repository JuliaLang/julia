// --- the ccall intrinsic ---

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__)
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

static void read_sonames()
{
    char *line=NULL;
    size_t sz=0;
    FILE *ldc = popen("/sbin/ldconfig -p", "r");

    while (!feof(ldc)) {
        ssize_t n = getline(&line, &sz, ldc);
        if (n == -1)
            break;
        if (n > 2 && isspace(line[0])) {
            int i=0;
            while (isspace(line[++i])) ;
            char *name = &line[i];
            char *dot = strstr(name, ".so");
            char *nxt = strchr(name, ' ');
            if (dot != NULL && nxt != NULL) {
                std::string pfx(name, dot - name);
                std::string soname(name, nxt - name);
                sonameMap[pfx] = soname;
            }
        }
    }

    free(line);
    pclose(ldc);
}

extern "C" const char *jl_lookup_soname(char *pfx, size_t n)
{
    if (!got_sonames) {
        read_sonames();
        got_sonames = true;
    }
    std::string str(pfx, n);
    if (sonameMap.find(str) != sonameMap.end()) {
        return sonameMap[str].c_str();
    }
    return NULL;
}
#endif

// map from user-specified lib names to handles
static std::map<std::string, void*> libMap;

int add_library_mapping(char *lib, void *hnd)
{
    if(libMap[lib] == NULL && hnd != NULL) {
        libMap[lib] = hnd;
        return 0;
    } else
        return -1;
}

static void *add_library_sym(char *name, char *lib)
{
    void *hnd;
    if (lib == NULL) {
        hnd = jl_dl_handle;
    }
    else {
        hnd = libMap[lib];
        if (hnd == NULL) {
            hnd = jl_load_dynamic_library(lib, JL_RTLD_DEFAULT);
            if (hnd != NULL)
            libMap[lib] = hnd;
            else
                return NULL;
        }
    }
    // add a symbol->address mapping for the JIT
    void *sval = jl_dlsym_e((uv_lib_t*)hnd, name);
    if (lib != NULL && hnd != jl_dl_handle) {
        void *exist = sys::DynamicLibrary::SearchForAddressOfSymbol(name);
        if (exist != NULL && exist != sval &&
            // openlibm conflicts with libm, and lots of our libraries
            // (including LLVM) link to libm. fortunately AddSymbol() is
            // able to resolve these in favor of openlibm, but this could
            // be an issue in the future (TODO).
            strcmp(lib,"libopenlibm")) {
            ios_printf(ios_stderr, "Warning: Possible conflict in library symbol %s\n", name);
        }
        sys::DynamicLibrary::AddSymbol(name, sval);
    }
    return sval;
}

// --- argument passing and scratch space utilities ---

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
extern "C" void *jl_value_to_pointer(jl_value_t *jt, jl_value_t *v, int argn,
                                     int addressof)
{
    jl_value_t *jvt = (jl_value_t*)jl_typeof(v);
    if (addressof) {
        if (jvt == jt && jl_is_bits_type(jvt)) {
            size_t osz = jl_bitstype_nbits(jt)/8;
            return alloc_temp_arg_copy(jl_bits_data(v), osz);
        }
        if (jl_is_struct_type(jvt) && jl_is_leaf_type(jvt) && !jl_is_array_type(jvt)) {
            if (jl_subtype(jvt, jt, 0))
                return v + 1;
        }
        goto value_to_pointer_error;
    }
    else {
        if (jl_is_cpointer_type(jvt) && jl_tparam0(jvt) == jt) {
            return (void*)jl_unbox_voidpointer(v);
        }
    }

    if (((jl_value_t*)jl_uint8_type == jt ||
         (jl_value_t*)jl_int8_type == jt) && jl_is_byte_string(v)) {
        return jl_string_data(v);
    }
    if (jl_is_array_type(jvt)) {
        if (jl_tparam0(jl_typeof(v)) == jt || jt==(jl_value_t*)jl_bottom_type)
            return ((jl_array_t*)v)->data;
        if (jl_is_cpointer_type(jt)) {
            jl_array_t *ar = (jl_array_t*)v;
            void **temp=(void**)alloc_temp_arg_space(jl_array_len(ar)*sizeof(void*));
            size_t i;
            for(i=0; i < jl_array_len(ar); i++) {
                temp[i] = jl_value_to_pointer(jl_tparam0(jt),
                                              jl_arrayref(ar, i), argn, 0);
            }
            return temp;
        }
    }

 value_to_pointer_error:
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
    if (jt == (jl_value_t*)jl_any_type) {
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
    else if (jl_is_cpointer_type(jt)) {
        assert(ty->isPointerTy());
        jl_value_t *jet = jl_tparam0(jt);
        jl_value_t *aty = expr_type(argex, ctx);
        if (jl_is_array_type(aty) &&
            (jet == jl_tparam0(aty) ||
             jet == (jl_value_t*)jl_bottom_type)) {
            // array to pointer
            return builder.CreateBitCast(emit_arrayptr(jv), ty);
        }
        if (aty == (jl_value_t*)jl_ascii_string_type || aty == (jl_value_t*)jl_utf8_string_type) {
            return builder.CreateBitCast(emit_arrayptr(emit_nthptr(jv,1)), ty);
        }
        if (jl_is_struct_type(aty) && jl_is_leaf_type(aty) && !jl_is_array_type(aty)) {
            if (!jl_subtype(aty, jet, 0)) {
                emit_error("ccall: argument not a subtype of parameter", ctx);
                return literal_pointer_val(jl_nothing);
            }
            if (!addressOf) {
                emit_error("ccall: expected addressOf operator", ctx);
                return literal_pointer_val(jl_nothing);
            }
            return builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), ty); // skipping type tag field
        }
        Value *p = builder.CreateCall4(value_to_pointer_func,
                                       literal_pointer_val(jl_tparam0(jt)), jv,
                                       ConstantInt::get(T_int32, argn),
                                       ConstantInt::get(T_int32, (int)addressOf));
        return builder.CreateBitCast(p, ty);
    }
    else if (jl_is_struct_type(jt)) {
        if (addressOf)
            jl_error("ccall: unexpected addressOf operator"); // the only "safe" thing to emit here is the expected struct
        jl_value_t *aty = expr_type(argex, ctx);
        if (aty != jt) {
            std::stringstream msg;
            msg << "ccall argument ";
            msg << argn;
            emit_typecheck(jv, jt, msg.str(), ctx);
        }
        //TODO: maybe check instead that prefix matches?
        //if (!jl_is_struct_type(aty))
        //    emit_typecheck(emit_typeof(jv), (jl_value_t*)jl_struct_kind, "ccall: Struct argument called with something that isn't a CompositeKind", ctx);
        // //safe thing would be to also check that jl_typeof(aty)->size > sizeof(ty) here and/or at runtime
        Type *pty = PointerType::get(ty,0);
        assert (ty->isStructTy());
        return builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), pty);
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


// --- code generator for ccall itself ---

// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *ptr=NULL, *rt=NULL, *at=NULL;
    Value *jl_ptr=NULL;
    JL_GC_PUSH(&ptr, &rt, &at);
    ptr = static_eval(args[1], ctx, true);
    if (ptr == NULL) {
        jl_value_t *ptr_ty = expr_type(args[1], ctx);
        Value *arg1 = emit_unboxed(args[1], ctx);
        if (!jl_is_cpointer_type(ptr_ty)) {
            emit_cpointercheck(arg1, 
                    "ccall: function argument not a pointer or valid, constant expression", ctx);
        }
        jl_ptr = emit_unbox(T_size, T_psize, arg1);
    }
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
    void *fptr=NULL;
    char *f_name=NULL, *f_lib=NULL;
    if (ptr != NULL) {
        if (jl_is_tuple(ptr) && jl_tuple_len(ptr)==1) {
            ptr = jl_tupleref(ptr,0);
        }
        if (jl_is_symbol(ptr))
            f_name = ((jl_sym_t*)ptr)->name;
        else if (jl_is_byte_string(ptr))
            f_name = jl_string_data(ptr);
        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
#ifdef __WIN32__
         //TODO: store the f_lib name instead of fptr
        fptr = jl_dlsym_win32(f_name);
#else
            // will look in process symbol table
#endif
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void**)jl_bits_data(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_tuple_len(ptr)>1) {
            jl_value_t *t0 = jl_tupleref(ptr,0);
            jl_value_t *t1 = jl_tupleref(ptr,1);
            if (jl_is_symbol(t0))
                f_name = ((jl_sym_t*)t0)->name;
            else if (jl_is_byte_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHK(ccall, symbol, t0);
            if (jl_is_symbol(t1))
                f_lib = ((jl_sym_t*)t1)->name;
            else if (jl_is_byte_string(t1))
                f_lib = jl_string_data(t1);
            else
                JL_TYPECHK(ccall, symbol, t1);
        }
        else {
            JL_TYPECHK(ccall, pointer, ptr);
        }
    }
    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        JL_GC_POP();
        emit_error("ccall: null function pointer", ctx);
        return literal_pointer_val(jl_nothing);
    }

    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, tuple, at);
    JL_TYPECHK(ccall, type, at);
    bool error = false;
    clang::CanQualType lrt = julia_type_to_clang(rt, &error, true);
    if (error) {
        JL_GC_POP();
        emit_error("ccall: return type doesn't correspond to a C type", ctx);
        return literal_pointer_val(jl_nothing);
    }
    jl_tuple_t *tt = (jl_tuple_t*)at;
    std::vector<clang::CanQualType> fargt;
    std::vector<AttributeWithIndex> attrs;
    size_t i;
    bool haspointers = false;
    clang::CanQualType va = cT_void;
    size_t nargty = jl_tuple_len(tt);

    for(i=0; i < nargty; i++) {
        jl_value_t *tti = jl_tupleref(tt,i);
        bool isVa = false;
        if (jl_is_vararg_type(tti)) {
            isVa = true;
            tti = jl_tparam0(tti);
        }
        clang::CanQualType t = julia_type_to_clang(tti, &error, true);
        if (error || t == cT_void) {
            JL_GC_POP();
            std::stringstream msg;
            msg << "ccall: the type of argument ";
            msg << i+1;
            msg << " doesn't correspond to a C type containing only BitsKinds";
            emit_error(msg.str(), ctx);
            return literal_pointer_val(jl_nothing);
        }
        if (!isVa) {
            fargt.push_back(t);
        } else {
            va = t;
        }
    }
    if (va!=cT_void) nargty--;

    // check for calling convention specifier
    clang::CallingConv cc = clang::CC_Default;
    jl_value_t *last = args[nargs];
    if (jl_is_expr(last)) {
        jl_sym_t *lhd = ((jl_expr_t*)last)->head;
        if (lhd == jl_symbol("stdcall")) {
            cc = clang::CC_X86StdCall;
            nargs--;
        }
        else if (lhd == jl_symbol("cdecl")) {
            cc = clang::CC_C;
            nargs--;
        }
        else if (lhd == jl_symbol("fastcall")) {
            cc = clang::CC_X86FastCall;
            nargs--;
        }
        else if (lhd == jl_symbol("thiscall")) {
            cc = clang::CC_X86ThisCall;
            nargs--;
        }
    }
    clang::TargetInfo::CallingConvCheckResult A =
        clang_compiler->getTarget().checkCallingConvention(cc);
    if (A != clang::TargetInfo::CCCR_OK) {
        JL_PRINTF(JL_STDERR,
                "Warning: calling convention %s not supported on this machine, using default instead\n",
                clang::FunctionType::getNameForCallConv(cc).str().data());
        cc = clang_compiler->getTarget().getDefaultCallingConv();
    }
    
    if ((va==cT_void && nargty != (nargs-2)/2) ||
        (va!=cT_void && nargty  > (nargs-2)/2))
        jl_error("ccall: wrong number of arguments to C function");
    for (i=nargty; i<(nargs-2)/2; i++)
        fargt.push_back(va);

    // some special functions
    if (fptr == &jl_array_ptr ||
        (strcmp(f_name,"jl_array_ptr")==0 && f_lib==NULL)) {
        Type* llrt = clang_cgt->ConvertType(lrt);
        assert(llrt->isPointerTy());
        Value *ary = emit_expr(args[4], ctx);
        JL_GC_POP();
        return mark_julia_type(builder.CreateBitCast(emit_arrayptr(ary),llrt),rt);
    }
    if (fptr == &jl_value_ptr ||
       (strcmp(f_name,"jl_value_ptr")==0 && f_lib==NULL)) {
        Type* llrt = clang_cgt->ConvertType(lrt);
        assert(llrt->isPointerTy());
        Value *ary = emit_expr(args[4], ctx);
        JL_GC_POP();
        return mark_julia_type(builder.CreateBitCast(ary,llrt),rt);
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
    Value *llvmf;
    clang::CodeGen::RequiredArgs required =
        (va==cT_void ?
         clang::CodeGen::RequiredArgs::All :
         clang::CodeGen::RequiredArgs(nargty));
    clang::FunctionType::ExtInfo info(false, false, 0, cc, lrt!=cT_void);
    const clang::CodeGen::CGFunctionInfo *cgfi =
        &clang_cgt->arrangeLLVMFunctionInfo(
            lrt, fargt, info, required);
    FunctionType *functype = clang_cgt->GetFunctionType(*cgfi);
    
    if (jl_ptr != NULL) {
        null_pointer_check(jl_ptr,ctx);
        Type *funcptype = PointerType::get(functype,0);
        llvmf = builder.CreateIntToPtr(jl_ptr, funcptype);
    } else if (fptr != NULL) {
        Type *funcptype = PointerType::get(functype,0);
        llvmf = literal_pointer_val(fptr, funcptype);
    }
    else {
        void *symaddr;
        if (f_lib != NULL)
            symaddr = add_library_sym(f_name, f_lib);
        else
            symaddr = sys::DynamicLibrary::SearchForAddressOfSymbol(f_name);
        if (symaddr == NULL) {
            JL_GC_POP();
            std::stringstream msg;
            msg << "ccall: could not find function ";
            msg << f_name;
            if (f_lib != NULL) {
                msg << " in library ";
                msg << f_lib;
            }
            emit_error(msg.str(), ctx);
            return literal_pointer_val(jl_nothing);
        }
        llvmf = jl_Module->getOrInsertFunction(f_name, functype);
    }

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
    clang::CodeGen::CallArgList argvals;
    int last_depth = ctx->argDepth;
    for(i=4; i < nargs+1; i+=2) {
        size_t ai = (i-4)/2;
        jl_value_t *argi = args[i];
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        clang::CanQualType largty;
        jl_value_t *jargty;
        if (va!=cT_void && ai >= nargty) {
            largty = va;
            jargty = jl_tparam0(jl_tupleref(tt,nargty));
        }
        else {
            largty = fargt[ai];
            jargty = jl_tupleref(tt,ai);
        }
        Value *arg;
        Type *llargty;
        bool is_complex = false;
        if (!isa<clang::ComplexType>(lrt)) {
            llargty = clang_cgt->ConvertType(largty);
        } else {
            assert(jl_is_bits_type(jargty));
            llargty = Type::getIntNTy(getGlobalContext(), jl_bitstype_nbits(jargty));
            is_complex = true;
        }
        if (jargty == (jl_value_t*)jl_any_type) {
            arg = emit_expr(argi, ctx, true);
        }
        else {
            arg = emit_unboxed(argi, ctx);
            if (jl_is_bits_type(expr_type(argi, ctx))) {
                if (addressOf)
                    arg = emit_unbox(llargty->getContainedType(0), llargty, arg);
                else
                    arg = emit_unbox(llargty, PointerType::get(llargty,0), arg);
            }
        }
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
        Value *v = julia_to_native(llargty, jargty, arg, argi,
                    addressOf, ai+1, ctx);
        std::pair <Value*, Value*> complexv;
        if (is_complex) {
            Value *temp = builder.CreateAlloca(llargty);
            builder.CreateStore(v, temp);
            llargty = clang_cgt->ConvertType(lrt);
            temp = builder.CreateBitCast(temp, PointerType::get(llargty,0));
            Value *re = builder.CreateLoad(builder.CreateConstGEP2_32(temp, 0, 0));
            Value *im = builder.CreateLoad(builder.CreateConstGEP2_32(temp, 0, 1));
            complexv = std::make_pair(re,im);
        }
        clang::CodeGen::RValue rv = (
                is_complex ? clang::CodeGen::RValue::getComplex(complexv) :
                llargty->isStructTy() ? clang::CodeGen::RValue::getAggregate(v, false) :
                clang::CodeGen::RValue::get(v));
        argvals.add(rv, largty);
    }
    // the actual call
    clang::CodeGen::ReturnValueSlot return_slot;
    Value *result = NULL;
    if (isa<clang::RecordType>(lrt)) {
        // pre-allocate a return value structure
        // clang will make sure this is used efficiently (zero-copy)
        result = builder.CreateCall(jlallocobj_func,
                ConstantInt::get(T_size,
                     sizeof(void*)+((jl_struct_type_t*)rt)->size));
        builder.CreateStore(literal_pointer_val((jl_value_t*)rt),
                emit_nthptr_addr(result, (size_t)0));
        return_slot = clang::CodeGen::ReturnValueSlot(
                builder.CreateBitCast(emit_nthptr_addr(result, (size_t)1),
                    PointerType::get(clang_cgt->ConvertType(lrt),0)),
                false);
        make_gcroot(result, ctx);
    }
    // setup the environment to clang's expecations
    clang_cgf->Builder.SetInsertPoint( builder.GetInsertBlock(), builder.GetInsertPoint() );
    // clang expects to alloca memory before the AllocaInsertPt
    // typically, clang would create this pointer when it started emitting the function
    // instead, we create a dummy reference here
    // for efficiency, we avoid creating a new placehold instruction if possible
    BasicBlock* alloca_bb = &ctx->f->getEntryBlock();
    llvm::Instruction *alloca_bb_ptr = NULL;
    if (alloca_bb->empty()) {
        llvm::Value *Undef = llvm::UndefValue::get(T_int32);
        clang_cgf->AllocaInsertPt = alloca_bb_ptr = new llvm::BitCastInst(Undef, T_int32, "", alloca_bb);
    } else
        clang_cgf->AllocaInsertPt = &alloca_bb->front();
    // emit the actual call
    clang::CodeGen::RValue rv = clang_cgf->EmitCall(
            *cgfi, llvmf, return_slot,
            argvals, NULL, NULL);
    // cleanup the environment
    clang_cgf->AllocaInsertPt = 0; // free this ptr reference
    if (alloca_bb_ptr)
        alloca_bb_ptr->eraseFromParent();
    // extract the result from clang
    if (result == NULL) {
        if (rv.isScalar())
            result = rv.getScalarVal();
        else if (rv.isComplex()) {
            assert(jl_is_bits_type(rt));
            std::pair<Value*, Value*> C = rv.getComplexVal();
            Type *llrt = clang_cgt->ConvertType(lrt);
            result = builder.CreateAlloca(llrt);
            Value *slot = builder.CreateConstGEP2_32(result, 0, 0);
            builder.CreateStore(C.first, slot);
            slot = builder.CreateConstGEP2_32(result, 0, 1);
            builder.CreateStore(C.second, slot);
            Type *rtype = Type::getIntNTy(getGlobalContext(), jl_bitstype_nbits(rt));
            result = builder.CreateLoad(
                    builder.CreateBitCast(
                        result,
                        PointerType::get(rtype,0)));
        } else {
            assert(0);
        }
    }
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
    if (lrt == cT_void)
        return literal_pointer_val((jl_value_t*)jl_nothing);
    return mark_julia_type(
            builder.CreateBitCast(result, julia_type_to_llvm(rt)),
            rt);
}
