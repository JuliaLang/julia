// This file is a part of Julia. License is MIT: http://julialang.org/license

// --- the ccall intrinsic ---

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__) || defined(__FreeBSD__)
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

extern "C" DLLEXPORT void jl_read_sonames(void)
{
    char *line=NULL;
    size_t sz=0;
#if defined(__linux__)
    FILE *ldc = popen("/sbin/ldconfig -p", "r");
#else
    FILE *ldc = popen("/sbin/ldconfig -r", "r");
#endif

    while (!feof(ldc)) {
        ssize_t n = getline(&line, &sz, ldc);
        if (n == -1)
            break;
        if (n > 2 && isspace((unsigned char)line[0])) {
#ifdef __linux__
            int i = 0;
            while (isspace((unsigned char)line[++i])) ;
            char *name = &line[i];
            char *dot = strstr(name, ".so");
            i = 0;
#else
            char *name = strstr(line, ":-l");
            if (name == NULL) continue;
            strncpy(name, "lib", 3);
            char *dot = strchr(name, '.');
#endif

            if (NULL == dot)
                continue;

#ifdef __linux__
            // Detect if this entry is for the current architecture
            while (!isspace((unsigned char)dot[++i])) ;
            while (isspace((unsigned char)dot[++i])) ;
            int j = i;
            while (!isspace((unsigned char)dot[++j])) ;
            char *arch = strstr(dot+i,"x86-64");
            if (arch != NULL && arch < dot + j) {
#ifdef _P32
                continue;
#endif
            }
            else {
#ifdef _P64
                continue;
#endif
            }
#endif // __linux__

            char *abslibpath = strrchr(line, ' ');
            if (dot != NULL && abslibpath != NULL) {
                std::string pfx(name, dot - name);
                // Do not include ' ' in front and '\n' at the end
                std::string soname(abslibpath+1, line+n-(abslibpath+1)-1);
                sonameMap[pfx] = soname;
            }
        }
    }

    free(line);
    pclose(ldc);
}

extern "C" DLLEXPORT const char *jl_lookup_soname(const char *pfx, size_t n)
{
    if (!got_sonames) {
        jl_read_sonames();
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
static std::map<std::string, uv_lib_t*> libMap;

static uv_lib_t *get_library(char *lib)
{
    uv_lib_t *hnd;
#ifdef _OS_WINDOWS_
    if ((intptr_t)lib == 1)
        return jl_exe_handle;
    if ((intptr_t)lib == 2)
        return jl_dl_handle;
#endif
    if (lib == NULL)
        return jl_RTLD_DEFAULT_handle;
    hnd = libMap[lib];
    if (hnd != NULL)
        return hnd;
    hnd = (uv_lib_t *) jl_load_dynamic_library(lib, JL_RTLD_DEFAULT);
    if (hnd != NULL)
        libMap[lib] = hnd;
    return hnd;
}

extern "C" DLLEXPORT
void *jl_load_and_lookup(char *f_lib, char *f_name, uv_lib_t **hnd)
{
    uv_lib_t *handle = *hnd;
    if (!handle)
        *hnd = handle = get_library(f_lib);
    void *ptr = jl_dlsym_e(handle, f_name);
    if (!ptr)
        jl_errorf("symbol could not be found %s: %s\n", f_name, uv_dlerror(handle));
    return ptr;
}

static std::map<std::string, GlobalVariable*> libMapGV;
static std::map<std::string, GlobalVariable*> symMapGV;
static Value *runtime_sym_lookup(PointerType *funcptype, char *f_lib, char *f_name, jl_codectx_t *ctx)
{
    // in pseudo-code, this function emits the following:
    //   global uv_lib_t **libptrgv
    //   global void **llvmgv
    //   if (*llvmgv == NULL) {
    //       *llvmgv = jl_load_and_lookup(f_lib, f_name, libptrgv);
    //   }
    //   return (*llvmgv)
    Constant *initnul = ConstantPointerNull::get((PointerType*)T_pint8);

    uv_lib_t *libsym = NULL;
    bool runtime_lib = false;
    GlobalVariable *libptrgv;
#ifdef _OS_WINDOWS_
    if ((intptr_t)f_lib == 1) {
        libptrgv = prepare_global(jlexe_var);
        libsym = jl_exe_handle;
    }
    else if ((intptr_t)f_lib == 2) {
        libptrgv = prepare_global(jldll_var);
        libsym = jl_dl_handle;
    }
    else
#endif
    if (f_lib == NULL) {
        libptrgv = prepare_global(jlRTLD_DEFAULT_var);
        libsym = jl_RTLD_DEFAULT_handle;
    }
    else {
        runtime_lib = true;
        libptrgv = libMapGV[f_lib];
        if (libptrgv == NULL) {
            libptrgv = new GlobalVariable(*jl_Module, T_pint8,
               false, GlobalVariable::PrivateLinkage,
               initnul, f_lib);
            libMapGV[f_lib] = libptrgv;
            libsym = get_library(f_lib);
            assert(libsym != NULL);
#ifdef USE_MCJIT
            llvm_to_jl_value[libptrgv] = libsym;
#else
            *((uv_lib_t**)jl_ExecutionEngine->getPointerToGlobal(libptrgv)) = libsym;
#endif
        }
    }
    if (libsym == NULL) {
#ifdef USE_MCJIT
        libsym = (uv_lib_t*)llvm_to_jl_value[libptrgv];
#else
        libsym = *((uv_lib_t**)jl_ExecutionEngine->getPointerToGlobal(libptrgv));
#endif
    }

    assert(libsym != NULL);

    GlobalVariable *llvmgv = symMapGV[f_name];
    if (llvmgv == NULL) {
        // MCJIT forces this to have external linkage eventually, so we would clobber
        // the symbol of the actual function.
        std::string name = f_name;
        name = "ccall_" + name;
        llvmgv = new GlobalVariable(*jl_Module, T_pint8,
           false, GlobalVariable::PrivateLinkage,
           initnul, name);
        symMapGV[f_name] = llvmgv;
#ifdef USE_MCJIT
        llvm_to_jl_value[llvmgv] = jl_dlsym_e(libsym, f_name);
#else
        *((void**)jl_ExecutionEngine->getPointerToGlobal(llvmgv)) = jl_dlsym_e(libsym, f_name);
#endif
    }

    BasicBlock *dlsym_lookup = BasicBlock::Create(jl_LLVMContext, "dlsym"),
               *ccall_bb = BasicBlock::Create(jl_LLVMContext, "ccall");
    builder.CreateCondBr(builder.CreateICmpNE(builder.CreateLoad(llvmgv), initnul), ccall_bb, dlsym_lookup);

    ctx->f->getBasicBlockList().push_back(dlsym_lookup);
    builder.SetInsertPoint(dlsym_lookup);
    Value *libname;
    if (runtime_lib) {
        libname = builder.CreateGlobalStringPtr(f_lib);
    }
    else {
        libname = literal_static_pointer_val(f_lib, T_pint8);
    }
#ifdef LLVM37
    Value *llvmf = builder.CreateCall(prepare_call(jldlsym_func), { libname, builder.CreateGlobalStringPtr(f_name), libptrgv });
#else
    Value *llvmf = builder.CreateCall3(prepare_call(jldlsym_func), libname, builder.CreateGlobalStringPtr(f_name), libptrgv);
#endif
    builder.CreateStore(llvmf, llvmgv);
    builder.CreateBr(ccall_bb);

    ctx->f->getBasicBlockList().push_back(ccall_bb);
    builder.SetInsertPoint(ccall_bb);
    llvmf = builder.CreateLoad(llvmgv);
    return builder.CreatePointerCast(llvmf,funcptype);
}

// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

#if defined ABI_LLVM
#  include "abi_llvm.cpp"
#elif defined _CPU_X86_64_
#  if defined _OS_WINDOWS_
#    include "abi_win64.cpp"
#  else
#    include "abi_x86_64.cpp"
#  endif
#elif defined _CPU_X86_
#  if defined _OS_WINDOWS_
#    include "abi_win32.cpp"
#  else
#    include "abi_x86.cpp"
#  endif
#else
#  warning "ccall is defaulting to llvm ABI, since no platform ABI has been defined for this CPU/OS combination"
#  include "abi_llvm.cpp"
#endif

Value *llvm_type_rewrite(Value *v, Type *target_type, jl_value_t *ty, bool isret)
{
    if (preferred_llvm_type(ty,isret) == NULL || target_type == NULL || target_type == v->getType())
        return v;

    assert(!v->getType()->isPointerTy());

    // LLVM doesn't allow us to cast values directly, so
    // we need to use this alloca trick
    Value *mem = builder.CreateAlloca(target_type);
    builder.CreateStore(v,builder.CreatePointerCast(mem,v->getType()->getPointerTo()));
    return builder.CreateLoad(mem);
}

// --- argument passing and scratch space utilities ---

static Value *julia_to_native(Type *ty, jl_value_t *jt, Value *jv,
                              jl_value_t *aty, bool addressOf,
                              bool byRef, bool inReg,
                              bool needCopy,
                              int argn, jl_codectx_t *ctx,
                              bool *needStackRestore)
{
    Type *vt = jv->getType();

    // We're passing any
    if (ty == jl_pvalue_llvmt) {
        return boxed(jv,ctx);
    }
    if (ty == vt && !addressOf && !byRef) {
        return jv;
    }
    if (vt != jl_pvalue_llvmt) {
        // argument value is unboxed
        if (addressOf || (byRef && inReg)) {
            if (ty->isPointerTy() && ty->getContainedType(0)==vt) {
                // pass the address of an alloca'd thing, not a box
                // since those are immutable.
                *needStackRestore = true;
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
                if (!byRef) {
                    return builder.CreateBitCast(jv, ty);
                }
                else {
                    *needStackRestore = true;
                    Value *mem = builder.CreateAlloca(ty);
                    builder.CreateStore(jv,builder.CreateBitCast(mem,vt->getPointerTo()));
                    return mem;
                }
            }
        }
        else if (vt->isStructTy()) {
            if (!byRef) {
                return jv;
            }
            else {
                *needStackRestore = true;
                Value *mem = builder.CreateAlloca(vt);
                builder.CreateStore(jv,mem);
                return mem;
            }
        }

        emit_error("ccall: argument type did not match declaration", ctx);
    }
    if (jl_is_tuple(jt)) {
        return emit_unbox(ty,jv,jt);
    }
    if (jl_is_cpointer_type(jt) && addressOf) {
        assert(ty->isPointerTy());
        jl_value_t *ety = jl_tparam0(jt);
        if (aty != ety && ety != (jl_value_t*)jl_any_type && jt != (jl_value_t*)jl_voidpointer_type) {
            std::stringstream msg;
            msg << "ccall argument ";
            msg << argn;
            emit_typecheck(jv, ety, msg.str(), ctx);
        }
        if (jl_is_mutable_datatype(ety)) {
            // no copy, just reference the data field
            return builder.CreateBitCast(jv, ty);
        }
        else if (jl_is_immutable_datatype(ety) && jt != (jl_value_t*)jl_voidpointer_type) {
            // yes copy
            Value *nbytes;
            if (jl_is_leaf_type(ety))
                nbytes = ConstantInt::get(T_int32, jl_datatype_size(ety));
            else
                nbytes = tbaa_decorate(tbaa_datatype, builder.CreateLoad(
                                builder.CreateGEP(builder.CreatePointerCast(emit_typeof(jv), T_pint32),
                                    ConstantInt::get(T_size, offsetof(jl_datatype_t,size)/sizeof(int32_t))),
                                false));
            *needStackRestore = true;
            AllocaInst *ai = builder.CreateAlloca(T_int8, nbytes);
            ai->setAlignment(16);
            builder.CreateMemCpy(ai, builder.CreateBitCast(jv, T_pint8), nbytes, 1);
            return builder.CreateBitCast(ai, ty);
        }
        // emit maybe copy
        *needStackRestore = true;
        Value *jvt = emit_typeof(jv);
        BasicBlock *mutableBB = BasicBlock::Create(getGlobalContext(),"is-mutable",ctx->f);
        BasicBlock *immutableBB = BasicBlock::Create(getGlobalContext(),"is-immutable",ctx->f);
        BasicBlock *afterBB = BasicBlock::Create(getGlobalContext(),"after",ctx->f);
        Value *ismutable = builder.CreateTrunc(
                tbaa_decorate(tbaa_datatype, builder.CreateLoad(
                        builder.CreateGEP(builder.CreatePointerCast(jvt, T_pint8),
                            ConstantInt::get(T_size, offsetof(jl_datatype_t,mutabl))),
                        false)),
                T_int1);
        builder.CreateCondBr(ismutable, mutableBB, immutableBB);
        builder.SetInsertPoint(mutableBB);
        Value *p1 = builder.CreatePointerCast(jv, ty);
        builder.CreateBr(afterBB);
        builder.SetInsertPoint(immutableBB);
        Value *nbytes = tbaa_decorate(tbaa_datatype, builder.CreateLoad(
                    builder.CreateGEP(builder.CreatePointerCast(jvt, T_pint32),
                        ConstantInt::get(T_size, offsetof(jl_datatype_t,size)/sizeof(int32_t))),
                    false));
        AllocaInst *ai = builder.CreateAlloca(T_int8, nbytes);
        ai->setAlignment(16);
        builder.CreateMemCpy(ai, builder.CreatePointerCast(jv, T_pint8), nbytes, 1);
        Value *p2 = builder.CreatePointerCast(ai, ty);
        builder.CreateBr(afterBB);
        builder.SetInsertPoint(afterBB);
        PHINode *p = builder.CreatePHI(ty, 2);
        p->addIncoming(p1, mutableBB);
        p->addIncoming(p2, immutableBB);
        return p;
    }
    if (addressOf)
        jl_error("ccall: unexpected & on argument"); // the only "safe" thing to emit here is the expected struct
    assert(jl_is_datatype(jt));
    if (aty != jt) {
        std::stringstream msg;
        msg << "ccall argument ";
        msg << argn;
        emit_typecheck(jv, jt, msg.str(), ctx);
    }
    Value *p = data_pointer(jv);
    Value *pjv = builder.CreatePointerCast(p, PointerType::get(ty,0));
    if (byRef) {
        if (!needCopy) {
            return pjv;
        }
        else {
            *needStackRestore = true;
            Value *mem = builder.CreateAlloca(ty);
            builder.CreateMemCpy(mem,pjv,(uint64_t)jl_datatype_size(jt),(uint64_t)((jl_datatype_t*)jt)->alignment);
            return mem;
        }
    }
    else {
        return builder.CreateLoad(pjv,false);
    }
}

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void *fptr;     // if the argument is a constant pointer
    char *f_name;   // if the symbol name is known
    char *f_lib;    // if a library name is specified
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
static native_sym_arg_t interpret_symbol_arg(jl_value_t *arg, jl_codectx_t *ctx, const char *fname)
{
    jl_value_t *ptr = NULL;
    Value *jl_ptr=NULL;

    ptr = static_eval(arg, ctx, true);
    if (ptr == NULL) {
        jl_value_t *ptr_ty = expr_type(arg, ctx);
        Value *arg1 = emit_unboxed(arg, ctx);
        if (!jl_is_cpointer_type(ptr_ty)) {
            emit_cpointercheck(arg1,
                               !strcmp(fname,"ccall") ?
                               "ccall: first argument not a pointer or valid constant expression" :
                               "cglobal: first argument not a pointer or valid constant expression",
                               ctx);
        }
        jl_ptr = emit_unbox(T_size, arg1, (jl_value_t*)jl_voidpointer_type);
    }

    void *fptr=NULL;
    char *f_name=NULL, *f_lib=NULL;
    jl_value_t *t0 = NULL, *t1 = NULL;
    JL_GC_PUSH3(&ptr, &t0, &t1);
    if (ptr != NULL) {
        if (jl_is_tuple(ptr) && jl_nfields(ptr)==1) {
            ptr = jl_fieldref(ptr,0);
        }
        if (jl_is_symbol(ptr))
            f_name = ((jl_sym_t*)ptr)->name;
        else if (jl_is_byte_string(ptr))
            f_name = jl_string_data(ptr);
        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
            // will look in process symbol table
#ifdef _OS_WINDOWS_
            f_lib = jl_dlfind_win32(f_name);
#endif
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void**)jl_data_ptr(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_nfields(ptr)>1) {
            jl_value_t *t0 = jl_fieldref(ptr,0);
            jl_value_t *t1 = jl_fieldref(ptr,1);
            if (jl_is_symbol(t0))
                f_name = ((jl_sym_t*)t0)->name;
            else if (jl_is_byte_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHKS(fname, symbol, t0);
            if (jl_is_symbol(t1))
                f_lib = ((jl_sym_t*)t1)->name;
            else if (jl_is_byte_string(t1))
                f_lib = jl_string_data(t1);
            else
                JL_TYPECHKS(fname, symbol, t1);
        }
        else {
            JL_TYPECHKS(fname, pointer, ptr);
        }
    }
    JL_GC_POP();
    native_sym_arg_t r;
    r.jl_ptr = jl_ptr;
    r.fptr = fptr;
    r.f_name = f_name;
    r.f_lib = f_lib;
    return r;
}


typedef AttributeSet attr_type;

// --- code generator for cglobal ---

static Value *emit_cglobal(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt=NULL;
    Value *res;
    JL_GC_PUSH1(&rt);

    if (nargs == 2) {
        JL_TRY {
            rt = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                               jl_svec_data(ctx->sp),
                                               jl_svec_len(ctx->sp)/2);
        }
        JL_CATCH {
            jl_rethrow_with_add("error interpreting cglobal type");
        }

        JL_TYPECHK(cglobal, type, rt);
        rt = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type, jl_svec1(rt));
    }
    else {
        rt = (jl_value_t*)jl_voidpointer_type;
    }
    Type *lrt = julia_type_to_llvm(rt);
    if (lrt == NULL) lrt = T_pint8;

    native_sym_arg_t sym = interpret_symbol_arg(args[1], ctx, "cglobal");

    if (sym.jl_ptr != NULL) {
        res = builder.CreateIntToPtr(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = literal_static_pointer_val(sym.fptr, lrt);
        if (imaging_mode)
            jl_printf(JL_STDERR,"warning: literal address used in cglobal for %s; code cannot be statically compiled\n", sym.f_name);
    }
    else {
        if (imaging_mode) {
            res = runtime_sym_lookup((PointerType*)lrt, sym.f_lib, sym.f_name, ctx);
        }
        else {
            void *symaddr = jl_dlsym_e(get_library(sym.f_lib), sym.f_name);
            if (symaddr == NULL) {
                std::stringstream msg;
                msg << "cglobal: could not find symbol ";
                msg << sym.f_name;
                if (sym.f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)sym.f_lib != 1 && (intptr_t)sym.f_lib != 2);
#endif
                    msg << " in library ";
                    msg << sym.f_lib;
                }
                emit_error(msg.str(), ctx);
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the address of the cglobal
            res = literal_static_pointer_val(symaddr, lrt);
        }
    }

    JL_GC_POP();
    return mark_julia_type(res, rt);
}

// llvmcall(ir, (rettypes...), (argtypes...), args...)
static Value *emit_llvmcall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(llvmcall, 3)
    jl_value_t *rt = NULL, *at = NULL, *ir = NULL;
    jl_svec_t *stt = NULL;
    JL_GC_PUSH4(&ir, &rt, &at, &stt);
    {
    JL_TRY {
        at  = jl_interpret_toplevel_expr_in(ctx->module, args[3],
                                            jl_svec_data(ctx->sp),
                                            jl_svec_len(ctx->sp)/2);
    }
    JL_CATCH {
        jl_rethrow_with_add("error interpreting llvmcall return type");
    }
    }
    {
    JL_TRY {
        rt  = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                            jl_svec_data(ctx->sp),
                                            jl_svec_len(ctx->sp)/2);
    }
    JL_CATCH {
        jl_rethrow_with_add("error interpreting llvmcall argument tuple");
    }
    }
    {
    JL_TRY {
        ir  = jl_interpret_toplevel_expr_in(ctx->module, args[1],
                                            jl_svec_data(ctx->sp),
                                            jl_svec_len(ctx->sp)/2);
    }
    JL_CATCH {
        jl_rethrow_with_add("error interpreting IR argument");
    }
    }
    int i = 1;
    if (ir == NULL) {
        jl_error("Cannot statically evaluate first argument to llvmcall");
    }
    bool isString = jl_is_byte_string(ir);
    bool isPtr = jl_is_cpointer(ir);
    if (!isString && !isPtr) {
        jl_error("First argument to llvmcall must be a string or pointer to an LLVM Function");
    }

    JL_TYPECHK(llvmcall, type, rt);
    JL_TYPECHK(llvmcall, type, at);

    std::stringstream ir_stream;

    stt = jl_alloc_svec(nargs - 3);

    for (size_t i = 0; i < nargs-3; ++i) {
        jl_svecset(stt,i,expr_type(args[4+i],ctx));
    }

    // Generate arguments
    std::string arguments;
    llvm::raw_string_ostream argstream(arguments);
    jl_svec_t *tt = ((jl_datatype_t*)at)->parameters;
    jl_value_t *rtt = rt;

    size_t nargt = jl_svec_len(tt);
    Value **argvals = (Value**) alloca(nargt*sizeof(Value*));
    std::vector<llvm::Type*> argtypes;
    /*
     * Semantics for arguments are as follows:
     * If the argument type is immutable (including bitstype), we pass the loaded llvm value
     * type. Otherwise we pass a pointer to a jl_value_t.
     */
    for (size_t i = 0; i < nargt; ++i) {
        jl_value_t *tti = jl_svecref(tt,i);
        Type *t = julia_type_to_llvm(tti);
        argtypes.push_back(t);
        if (4+i > nargs) {
            jl_error("Missing arguments to llvmcall!");
        }
        jl_value_t *argi = args[4+i];
        Value *arg;
        bool needroot = false;
        if (t == jl_pvalue_llvmt || !jl_isbits(tti)) {
            arg = emit_expr(argi, ctx, true);
            if (t == jl_pvalue_llvmt && arg->getType() != jl_pvalue_llvmt) {
                arg = boxed(arg, ctx);
                needroot = true;
            }
        }
        else {
            arg = emit_unboxed(argi, ctx);
            if (jl_is_bitstype(expr_type(argi, ctx))) {
                arg = emit_unbox(t, arg, tti);
            }
        }

#ifdef JL_GC_MARKSWEEP
        // make sure args are rooted
        if (t == jl_pvalue_llvmt && (needroot || might_need_root(argi))) {
            make_gcroot(arg, ctx);
        }
#endif
        argvals[i] = julia_to_native(t, tti, arg, expr_type(argi, ctx), false, false, false, false, i, ctx, NULL);
    }

    Function *f;
    Type *rettype = julia_type_to_llvm(rtt);
    if (isString) {
        // Make sure to find a unique name
        std::string ir_name;
        while(true) {
            std::stringstream name;
            name << (ctx->f->getName().str()) << "u" << i++;
            ir_name = name.str();
            if (jl_Module->getFunction(ir_name) == NULL)
                break;
        }

        bool first = true;
        for (std::vector<Type *>::iterator it = argtypes.begin(); it != argtypes.end(); ++it) {
            if (!first)
                argstream << ",";
            else
                first = false;
            (*it)->print(argstream);
            argstream << " ";
        }

        std::string rstring;
        llvm::raw_string_ostream rtypename(rstring);
        rettype->print(rtypename);

        ir_stream << "; Number of arguments: " << nargt << "\n"
        << "define "<<rtypename.str()<<" @\"" << ir_name << "\"("<<argstream.str()<<") {\n"
        << jl_string_data(ir) << "\n}";
        SMDiagnostic Err = SMDiagnostic();
        std::string ir_string = ir_stream.str();
#ifdef LLVM36
        Module *m = NULL;
        bool failed = parseAssemblyInto(llvm::MemoryBufferRef(ir_string,"llvmcall"),*jl_Module,Err);
        if (!failed)
            m = jl_Module;
#else
        Module *m = ParseAssemblyString(ir_string.c_str(),jl_Module,Err,jl_LLVMContext);
#endif
        if (m == NULL) {
            std::string message = "Failed to parse LLVM Assembly: \n";
            llvm::raw_string_ostream stream(message);
            Err.print("julia",stream,true);
            jl_error(stream.str().c_str());
        }
        f = m->getFunction(ir_name);
    }
    else {
        assert(isPtr);
        // Create Function skeleton
        f = (llvm::Function*)jl_unbox_voidpointer(ir);
        assert(f->getReturnType() == rettype);
        int i = 0;
        for (std::vector<Type *>::iterator it = argtypes.begin();
            it != argtypes.end(); ++it, ++i)
            assert(*it == f->getFunctionType()->getParamType(i));

#ifdef USE_MCJIT
        if (f->getParent() != jl_Module) {
            FunctionMover mover(jl_Module,f->getParent());
            f = mover.CloneFunction(f);
        }
#endif

        //f->dump();
        #ifndef LLVM35
        if (verifyFunction(*f,PrintMessageAction)) {
        #else
        llvm::raw_fd_ostream out(1,false);
        if (verifyFunction(*f,&out)) {
        #endif
            f->dump();
            jl_error("Malformed LLVM Function");
        }
    }

    /*
     * It might be tempting to just try to set the Always inline attribute on the function
     * and hope for the best. However, this doesn't work since that would require an inlining
     * pass (which is a Call Graph pass and cannot be managed by a FunctionPassManager). Instead
     * We are sneaky and call the inliner directly. This however doesn't work until we've actually
     * generated the entire function, so we need to store it in the context until the end of the
     * function. This also has the benefit of looking exactly like we cut/pasted it in in `code_llvm`.
     */
    f->setLinkage(GlobalValue::LinkOnceODRLinkage);

    // the actual call
    assert(f->getParent() == jl_Module); // no prepare_call(f) is needed below, since this was just emitted into the same module
    CallInst *inst = builder.CreateCall(f,ArrayRef<Value*>(&argvals[0],nargt));
    ctx->to_inline.push_back(inst);

    JL_GC_POP();

    if (inst->getType() != rettype) {
        jl_error("Return type of llvmcall'ed function does not match declared return type");
    }

    return mark_julia_type(inst,rtt);
}

// --- code generator for ccall itself ---

int try_to_determine_bitstype_nbits(jl_value_t *targ, jl_codectx_t *ctx);

static Value *mark_or_box_ccall_result(Value *result, jl_value_t *rt_expr, jl_value_t *rt, bool static_rt, jl_codectx_t *ctx)
{
    if (!static_rt && rt != (jl_value_t*)jl_any_type) {
        // box if type was not statically known
        int nbits = try_to_determine_bitstype_nbits(rt_expr, ctx);
        return allocate_box_dynamic(emit_expr(rt_expr, ctx),
                                    ConstantInt::get(T_size, nbits/8),
                                    result);
    }

    return mark_julia_type(result, rt);
}

typedef AttributeSet attr_type;

static std::string generate_func_sig(Type **lrt, Type **prt, int &sret,
        std::vector<Type *> &fargt, std::vector<Type *> &fargt_sig,
        std::vector<bool> &inRegList,
        std::vector<bool> &byRefList, attr_type &attributes,
        jl_value_t *rt, jl_svec_t *tt)
{
    size_t nargt = jl_svec_len(tt);
    if (nargt > 0 && jl_svecref(tt,nargt-1) == (jl_value_t*)dots_sym) {
        nargt--;
    }
    assert(rt && !jl_is_abstract_ref_type(rt));

    AttrBuilder retattrs;
    std::vector<AttrBuilder> paramattrs;
    AbiState abi = default_abi_state;
    sret = 0;

    if (type_is_ghost(*lrt)) {
        *prt = *lrt = T_void;
    }
    else {
        *prt = preferred_llvm_type(rt,true);
        if (*prt == NULL)
            *prt = *lrt;

        if (jl_is_datatype(rt) && !jl_is_abstracttype(rt) && use_sret(&abi, rt)) {
            paramattrs.push_back(AttrBuilder());
            paramattrs[0].clear();
#if !defined(_OS_WINDOWS_) || defined(LLVM35)
            paramattrs[0].addAttribute(Attribute::StructRet);
#endif
            fargt.push_back(PointerType::get(*prt, 0));
            fargt_sig.push_back(PointerType::get(*prt, 0));
            sret = 1;
        }
    }

    size_t i;
    bool current_isVa = false;
    for(i = 0; i < nargt; i++) {
        paramattrs.push_back(AttrBuilder());
        jl_value_t *tti = jl_svecref(tt,i);
        if (jl_is_vararg_type(tti)) {
            current_isVa = true;
            tti = jl_tparam0(tti);
        }
        Type *t = NULL;
        if (jl_is_abstract_ref_type(tti)) {
            if (jl_is_typevar(jl_tparam0(tti)))
                jl_error("ccall: argument type Ref should have an element type, not Ref{T}");
            tti = (jl_value_t*)jl_voidpointer_type;
            t = T_pint8;
        }
        else {
            if (jl_is_cpointer_type(tti) && jl_is_typevar(jl_tparam0(tti)))
                jl_error("ccall: argument type Ptr should have an element type, not Ptr{T}");
            if (jl_is_bitstype(tti)) {
                // see pull req #978. need to annotate signext/zeroext for
                // small integer arguments.
                jl_datatype_t *bt = (jl_datatype_t*)tti;
                if (bt->size < 4) {
                    Attribute::AttrKind av;
                    if (jl_signed_type && jl_subtype(tti, (jl_value_t*)jl_signed_type, 0))
                        av = Attribute::SExt;
                    else
                        av = Attribute::ZExt;
                    paramattrs[i+sret].addAttribute(av);
                }
            }

            t = julia_struct_to_llvm(tti);
            if (t == NULL || t == T_void) {
                JL_GC_POP();
                std::stringstream msg;
                msg << "ccall: the type of argument ";
                msg << i+1;
                msg << " doesn't correspond to a C type";
                return msg.str();
            }
        }

        // Whether the ABI needs us to pass this by ref and/or in registers
        // Valid combinations are:
        bool byRefAttr = false;

        // Whether or not LLVM wants us to emit a pointer to the data
        bool byRef = false;

        // Whether or not to pass this in registers
        bool inReg = false;

        if (jl_is_datatype(tti) && !jl_is_abstracttype(tti))
            needPassByRef(&abi, tti, &byRef, &inReg, &byRefAttr);

        // Add the appropriate LLVM parameter attributes
        // Note that even though the LLVM argument is called ByVal
        // this really means that the thing we're passing is pointing to
        // the thing we want to pass by value
        if (byRefAttr)
            paramattrs[i+sret].addAttribute(Attribute::ByVal);
        if (inReg)
            paramattrs[i+sret].addAttribute(Attribute::InReg);

        byRefList.push_back(byRef);
        inRegList.push_back(inReg);

        fargt.push_back(t);

        Type *pat = preferred_llvm_type(tti,false);
        if (pat != NULL)
            t = pat;
        else if (byRef)
            t = PointerType::get(t,0);

        if (!current_isVa) {
            fargt_sig.push_back(t);
        }
    }

    if (retattrs.hasAttributes())
        attributes = AttributeSet::get(jl_LLVMContext, AttributeSet::ReturnIndex, retattrs);
    for (i = 0; i < nargt+sret; ++i)
        if (paramattrs[i].hasAttributes())
            attributes = attributes.addAttributes(jl_LLVMContext, i+1,
                                                  AttributeSet::get(jl_LLVMContext, i+1, paramattrs[i]));
    return "";
}


// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *rt=NULL, *at=NULL;
    JL_GC_PUSH2(&rt, &at);

    native_sym_arg_t symarg = interpret_symbol_arg(args[1], ctx, "ccall");
    Value *jl_ptr=NULL;
    void *fptr = NULL;
    char *f_name = NULL, *f_lib = NULL;
    jl_ptr = symarg.jl_ptr;
    fptr = symarg.fptr;
    f_name = symarg.f_name;
    f_lib = symarg.f_lib;
    bool isVa = false;

    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        JL_GC_POP();
        emit_error("ccall: null function pointer", ctx);
        return literal_pointer_val(jl_nothing);
    }

    jl_value_t *rtt_ = expr_type(args[2], ctx);
    bool static_rt = true;  // is return type fully statically known?
    if (jl_is_type_type(rtt_) && jl_is_leaf_type(jl_tparam0(rtt_))) {
        rt = jl_tparam0(rtt_);
    }
    else {
        JL_TRY {
            rt  = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                                jl_svec_data(ctx->sp),
                                                jl_svec_len(ctx->sp)/2);
        }
        JL_CATCH {
            static_rt = false;
            if (jl_is_type_type(rtt_)) {
                if (jl_subtype(jl_tparam0(rtt_), (jl_value_t*)jl_pointer_type, 0)) {
                    // substitute Ptr{Void} for statically-unknown pointer type
                    rt = (jl_value_t*)jl_voidpointer_type;
                }
                else if (jl_subtype(jl_tparam0(rtt_), (jl_value_t*)jl_array_type, 0)) {
                    // `Array` used as return type just returns a julia object reference
                    rt = (jl_value_t*)jl_any_type;
                }
            }
            if (rt == NULL) {
                emit_error("error interpreting ccall return type", ctx);
                JL_GC_POP();
                return UndefValue::get(T_void);
            }
        }
    }

    if (jl_is_svec(rt)) {
        std::string msg = "in " + ctx->funcName +
            ": ccall: missing return type";
        jl_error(msg.c_str());
    }
    if (jl_is_cpointer_type(rt) && jl_is_typevar(jl_tparam0(rt)))
        jl_error("ccall: return type Ptr should have an element type, not Ptr{_<:T}");

    if (jl_is_abstract_ref_type(rt)) {
        if (jl_tparam0(rt) == (jl_value_t*)jl_any_type)
            jl_error("ccall: return type Ref{Any} is invalid. use Ptr{Any} instead.");
        rt = (jl_value_t*)jl_any_type; // convert return type to jl_value_t*
    }

    if (jl_is_array_type(rt)) {
        // `Array` used as return type just returns a julia object reference
        rt = (jl_value_t*)jl_any_type;
    }

    JL_TYPECHK(ccall, type, rt);
    Type *lrt = julia_struct_to_llvm(rt);
    if (lrt == NULL) {
        JL_GC_POP();
        emit_error("ccall: return type doesn't correspond to a C type", ctx);
        return literal_pointer_val(jl_nothing);
    }

    {
        JL_TRY {
            at  = jl_interpret_toplevel_expr_in(ctx->module, args[3],
                                                jl_svec_data(ctx->sp),
                                                jl_svec_len(ctx->sp)/2);
        }
        JL_CATCH {
            //jl_rethrow_with_add("error interpreting ccall argument tuple");
            emit_error("error interpreting ccall argument tuple", ctx);
            JL_GC_POP();
            return UndefValue::get(lrt);
        }
    }

    JL_TYPECHK(ccall, simplevector, at);
    //JL_TYPECHK(ccall, type, at);
    jl_svec_t *tt = (jl_svec_t*)at;

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
        else if (lhd == jl_symbol("thiscall")) {
            cc = CallingConv::X86_ThisCall;
            nargs--;
        }
    }

    // some sanity checking and check whether there's a vararg
    size_t i;
    size_t nargt = jl_svec_len(tt);
    for(i=0; i < nargt; i++) {
        jl_value_t *tti = jl_svecref(tt,i);
        if (jl_is_cpointer_type(tti) && jl_is_typevar(jl_tparam0(tti))) {
            JL_GC_POP();
            emit_error("ccall: argument type Ptr should have an element type, Ptr{T}",ctx);
            return literal_pointer_val(jl_nothing);
        }
        if (jl_is_vararg_type(tti))
            isVa = true;
    }

    if ((!isVa && nargt  != (nargs-2)/2) ||
        ( isVa && nargt-1 > (nargs-2)/2))
        jl_error("ccall: wrong number of arguments to C function");

    // some special functions
    if (fptr == (void *) &jl_array_ptr ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name,"jl_array_ptr"))) {
        assert(lrt->isPointerTy());
        assert(!isVa);
        assert(nargt==1);
        jl_value_t *argi = args[4];
        assert(!(jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym));
        Value *ary = emit_expr(argi, ctx);
        JL_GC_POP();
        return mark_or_box_ccall_result(builder.CreateBitCast(emit_arrayptr(ary),lrt),
                                        args[2], rt, static_rt, ctx);
    }
    if (fptr == (void *) &jl_value_ptr ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name,"jl_value_ptr"))) {
        assert(lrt->isPointerTy());
        assert(!isVa);
        assert(nargt==1);
        jl_value_t *argi = args[4];
        bool addressOf = false;
        jl_value_t *tti = jl_svecref(tt,0);
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        else if (jl_is_abstract_ref_type(tti)) {
            tti = (jl_value_t*)jl_voidpointer_type;
        }
        Value *ary;
        Type *largty;
        if (addressOf)
            largty = jl_pvalue_llvmt;
        else
            largty = julia_struct_to_llvm(jl_svecref(tt, 0));
        if (largty == jl_pvalue_llvmt) {
            ary = boxed(emit_expr(argi, ctx),ctx);
        }
        else {
            assert(!addressOf);
            ary = emit_unbox(largty, emit_unboxed(argi, ctx), jl_svecref(tt, 0));
        }
        JL_GC_POP();
        return mark_or_box_ccall_result(builder.CreateBitCast(ary, lrt),
                                        args[2], rt, static_rt, ctx);
    }
    if (fptr == (void *) &jl_is_leaf_type ||
        ((f_lib==NULL || (intptr_t)f_lib==2)
         && f_name && !strcmp(f_name, "jl_is_leaf_type"))) {
        jl_value_t *arg = args[4];
        jl_value_t *ty = expr_type(arg, ctx);
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            int isleaf = jl_is_leaf_type(jl_tparam0(ty));
            JL_GC_POP();
            return ConstantInt::get(T_int32, isleaf);
        }
    }

    // save place before arguments, for possible insertion of temp arg
    // area saving code.
    Value *stacksave=NULL;
    BasicBlock::InstListType &instList = builder.GetInsertBlock()->getInstList();
    Instruction *savespot;
    if (instList.empty()) {
        savespot = NULL;
    }
    else {
        // hey C++, there's this thing called pointers...
        Instruction &_savespot = builder.GetInsertBlock()->back();
        savespot = &_savespot;
    }

    std::vector<Type*> fargt(0);
    std::vector<Type*> fargt_sig(0);
    std::vector<bool> inRegList(0);
    std::vector<bool> byRefList(0);
    attr_type attrs;
    Type *prt = NULL;
    int sret = 0;
    std::string err_msg = generate_func_sig(&lrt, &prt, sret, fargt, fargt_sig, inRegList, byRefList, attrs, rt, tt);
    if (!err_msg.empty()) {
        JL_GC_POP();
        emit_error(err_msg,ctx);
        return literal_pointer_val(jl_nothing);
    }

    // emit arguments
    Value **argvals = (Value**) alloca(((nargs-3)/2 + sret)*sizeof(Value*));
    Value *result = NULL;

    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type
    if (sret) {
        result = emit_new_struct(rt,1,NULL,ctx);
        assert(result != NULL && "Type was not concrete");
        if (!result->getType()->isPointerTy()) {
            Value *mem = builder.CreateAlloca(lrt);
            builder.CreateStore(result, mem);
            result = mem;
            argvals[0] = result;
        }
        else {
            argvals[0] = builder.CreateBitCast(result, fargt_sig[0]);
        }
    }

    // save argument depth until after we're done emitting arguments
    int last_depth = ctx->argDepth;

    // number of parameters to the c function
    bool needStackRestore = false;
    for(i=4; i < nargs+1; i+=2) {
        // Current C function parameter
        size_t ai = (i-4)/2;

        // Julia (expression) value of current parameter
        jl_value_t *argi = args[i];

        // pass the address of the argument rather than the argument itself
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }

        // LLVM type of the current parameter
        Type *largty;

        // Julia type of the current parameter
        jl_value_t *jargty;
        if (isVa && ai >= nargt-1) {
            largty = fargt[nargt-1];
            jargty = jl_tparam0(jl_svecref(tt,nargt-1));
        }
        else {
            largty = fargt[sret+ai];
            jargty = jl_svecref(tt,ai);
        }

        Value *arg;
        bool needroot = false;
        if (jl_is_abstract_ref_type(jargty)) {
            if (addressOf)
                emit_error("ccall: & on a Ref{T} argument is invalid", ctx);
            arg = emit_unboxed((jl_value_t*)argi, ctx);
            if (arg->getType() == jl_pvalue_llvmt) {
                emit_cpointercheck(arg, "ccall: argument to Ref{T} is not a pointer", ctx);
                arg = emit_unbox(largty, arg, (jl_value_t*)jl_voidpointer_type);
            }
            if (arg->getType() != T_pint8)
                arg = builder.CreatePointerCast(arg, T_pint8);
            jargty = (jl_value_t*)jl_voidpointer_type;
        }
        else if (largty == jl_pvalue_llvmt || largty->isStructTy()) {
            arg = emit_expr(argi, ctx, true);
            if (largty == jl_pvalue_llvmt && arg->getType() != jl_pvalue_llvmt) {
                arg = boxed(arg,ctx);
                needroot = true;
            }
        }
        else {
            arg = emit_unboxed(argi, ctx);
            if (jl_is_bitstype(expr_type(argi, ctx))) {
                Type *at = arg->getType();
                Type *totype = addressOf ? largty->getContainedType(0) : largty;
                if (at != jl_pvalue_llvmt && at != totype &&
                    !(at->isPointerTy() && jargty==(jl_value_t*)jl_voidpointer_type)) {
                    emit_type_error(arg, jargty, "ccall", ctx);
                    arg = UndefValue::get(totype);
                }
                else {
                    arg = emit_unbox(totype, arg, jargty);
                }
            }
        }

#ifdef JL_GC_MARKSWEEP
        // make sure args are rooted
        if (largty == jl_pvalue_llvmt && (needroot || might_need_root(argi))) {
            make_gcroot(arg, ctx);
        }
#endif

        bool nSR=false;
        argvals[ai + sret] = llvm_type_rewrite(
                julia_to_native(largty, jargty, arg, expr_type(argi, ctx), addressOf, byRefList[ai], inRegList[ai],
                    need_private_copy(jargty, byRefList[ai]), ai + 1, ctx, &nSR),
                fargt_sig.size() > ai + sret ? fargt_sig[ai + sret] : preferred_llvm_type(jargty, false),
                jargty, false);
        needStackRestore |= nSR;
    }


    // make LLVM function object for the target
    // keep this close to the function call, so that the compiler can
    // optimize the global pointer load in the common case
    Value *llvmf;
    FunctionType *functype = FunctionType::get(sret?T_void:prt, fargt_sig, isVa);

    if (jl_ptr != NULL) {
        null_pointer_check(jl_ptr,ctx);
        Type *funcptype = PointerType::get(functype,0);
        llvmf = builder.CreateIntToPtr(jl_ptr, funcptype);
    }
    else if (fptr != NULL) {
        Type *funcptype = PointerType::get(functype,0);
        llvmf = literal_static_pointer_val(fptr, funcptype);
        if (imaging_mode)
            jl_printf(JL_STDERR,"warning: literal address used in ccall for %s; code cannot be statically compiled\n", f_name);
    }
    else {
        assert(f_name != NULL);

        PointerType *funcptype = PointerType::get(functype,0);
        if (imaging_mode) {
            llvmf = runtime_sym_lookup(funcptype, f_lib, f_name, ctx);
        }
        else {
            void *symaddr = jl_dlsym_e(get_library(f_lib), f_name);
            if (symaddr == NULL) {
                JL_GC_POP();
                std::stringstream msg;
                msg << "ccall: could not find function ";
                msg << f_name;
                if (f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)f_lib != 1 && (intptr_t)f_lib != 2);
#endif
                    msg << " in library ";
                    msg << f_lib;
                }
                emit_error(msg.str(), ctx);
                return literal_pointer_val(jl_nothing);
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the function address
            llvmf = literal_static_pointer_val(symaddr, funcptype);
        }
    }

    if (needStackRestore) {
        stacksave = CallInst::Create(Intrinsic::getDeclaration(jl_Module,
                                                               Intrinsic::stacksave));
        if (savespot)
            instList.insertAfter((Instruction*)savespot, (Instruction*)stacksave);
        else
            instList.push_front((Instruction*)stacksave);
    }

    //llvmf->dump();
    //for (std::vector<Value *>::iterator it = argvals.begin() ; it != argvals.end(); ++it)
    //    (*it)->dump();

    // the actual call
    Value *ret = builder.CreateCall(prepare_call(llvmf),
                                    ArrayRef<Value*>(&argvals[0],(nargs-3)/2+sret));
    ((CallInst*)ret)->setAttributes(attrs);

    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret;
    if (needStackRestore) {
        assert(stacksave != NULL);
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stackrestore),
                           stacksave);
    }
    ctx->argDepth = last_depth;
    if (0) { // Enable this to turn on SSPREQ (-fstack-protector) on the function containing this ccall
        ctx->f->addFnAttr(Attribute::StackProtectReq);
    }

    JL_GC_POP();
    // Finally we need to box the result into julia type
    // However, if we have already created a box for the return
    // type because we the ABI required us to pass a pointer (sret),
    // then we do not need to do this.
    if (!sret) {
        if (lrt == T_void)
            result = literal_pointer_val((jl_value_t*)jl_nothing);
        else if (lrt->isStructTy()) {
            //fprintf(stderr, "ccall rt: %s -> %s\n", f_name, ((jl_tag_type_t*)rt)->name->name->name);
            assert(jl_is_structtype(rt));

            Value *newst = emit_new_struct(rt,1,NULL,ctx);
            assert(newst != NULL && "Type was not concrete");
            if (newst->getType()->isPointerTy()) {
                builder.CreateStore(result,builder.CreateBitCast(newst, prt->getPointerTo()));
                result = newst;
            }
            else if (lrt != prt) {
                result = llvm_type_rewrite(result,lrt,rt,true);
            }
            // otherwise it's fine to pass this by value. Technically we could do alloca/store/load,
            // but why should we?
        }
        else {
            if (prt->getPrimitiveSizeInBits() == lrt->getPrimitiveSizeInBits()) {
                result = builder.CreateBitCast(result,lrt);
            }
            else {
                Value *rloc = builder.CreateAlloca(prt);
                builder.CreateStore(result, rloc);
                result = builder.CreateLoad(builder.CreatePointerCast(rloc, PointerType::get(lrt,0)));
            }
        }
    }
    else {
        if (result->getType() != jl_pvalue_llvmt)
            result = builder.CreateLoad(result);
    }

    return mark_or_box_ccall_result(result, args[2], rt, static_rt, ctx);
}
