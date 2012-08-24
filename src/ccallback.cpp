
//make_callback(f::Function, rt::Any, tt::Tuple, call_conv::Union(Symbol,Nothing))
extern "C" DLLEXPORT void *ccallback(jl_function_t *f, jl_value_t *rt, jl_tuple_t *tt, jl_sym_t *call_conv) {
    JL_TYPECHK(make_callback, type, rt);
    JL_TYPECHK(make_callback, tuple, (jl_value_t*)tt);
    JL_TYPECHK(make_callback, type, (jl_value_t*)tt);

    char *name;
    if (f->linfo != NULL) {
        size_t name_len = strlen(f->linfo->name->name) + 3;
        name = (char*)alloca(name_len);
        memcpy(name, f->linfo->name->name, name_len - 3);
        memcpy(name + name_len - 3, "_cb", 4);
    } else {
        name = (char*)alloca(3);
        memcpy(name,"_cb",4);
    }

    jl_codectx_t ctx;
    ctx.f = NULL;
    ctx.vars = NULL;
    //ctx.arguments = &argumentMap;
    ctx.passedArguments = NULL;
    ctx.closureEnv = NULL;
    ctx.isAssigned = NULL;
    ctx.isCaptured = NULL;
    ctx.escapes = NULL;
    ctx.volatilevars = NULL;
    ctx.declTypes = NULL;
    ctx.labels = NULL;
    ctx.savestates = NULL;
    ctx.jmpbufs = NULL;
    ctx.module = (f->linfo != NULL ? f->linfo->module : NULL);
    ctx.ast = NULL;
    ctx.sp = NULL;
    ctx.linfo = f->linfo;
    ctx.argArray = NULL;
    ctx.argCount = 0;
    ctx.funcName = name;
    ctx.vaName = NULL;
    ctx.vaStack = false;

    Type *lrt = julia_type_to_llvm(rt, &ctx);
    if (lrt == NULL) {
        return literal_pointer_val(jl_nothing);
    }
    std::vector<Type *> fargt(0);
    std::vector<jl_value_t *> fargjt(0);
    std::vector<Type *> fargt_sig(0);
    size_t i;
    bool isVa = false;
    int n_roots = jl_tuple_len(tt);
    for(i=0; i < n_roots; i++) {
        jl_value_t *tti = jl_tupleref(tt,i);
        if (jl_is_seq_type(tti)) {
            isVa = true;
            tti = jl_tparam0(tti);
        }
        Type *t = julia_type_to_llvm(tti, &ctx);
        if (t == NULL) {
            return literal_pointer_val(jl_nothing);
        }
        fargt.push_back(t);
        fargjt.push_back(tti);
        if (!isVa)
            fargt_sig.push_back(t);
    }
    
    // check for calling convention specifier
    CallingConv::ID cc = CallingConv::C;
    if (call_conv == jl_symbol("stdcall")) {
        cc = CallingConv::X86_StdCall;
    } else if (call_conv == jl_symbol("cdecl")) {
        cc = CallingConv::C;
    } else if (call_conv == jl_symbol("fastcall")) {
        cc = CallingConv::X86_FastCall;
    } else if (call_conv != NULL && (jl_value_t*)call_conv != jl_nothing) {
        jl_error("callback: invalid call convention");
        return literal_pointer_val(jl_nothing);
    }

    Function *llvmf =
        Function::Create(FunctionType::get(lrt, fargt_sig, isVa),
                         Function::ExternalLinkage,
                         name, jl_Module);
    if (cc != CallingConv::C)
        llvmf->setCallingConv(cc);
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", llvmf);
    builder.SetInsertPoint(b0);
    
    // TODO: Fix when moving to new LLVM version
    const char * const filename = "callback.cpp";
    dbuilder->createCompileUnit(0x01, filename, ".", "julia", true, "", 0); 
    llvm::DIArray EltTypeArray = dbuilder->getOrCreateArray(ArrayRef<Value*>());
    DIFile fil = dbuilder->createFile(filename, ".");
    DISubprogram SP =
        dbuilder->createFunction((DIDescriptor)dbuilder->getCU(),
                                 name,
                                 name,
                                 fil,
                                 0,
                                 dbuilder->createSubroutineType(fil,EltTypeArray),
                                 false, true,
                                 0, true, f);
    builder.SetCurrentDebugLocation(DebugLoc::get(0, 0, (MDNode*)SP, NULL));

    // Emit return value boxes
    ctx.argDepth = 0;
    //ctx.maxDepth = 0;
    ctx.argSpace = n_roots;
#ifdef JL_GC_MARKSWEEP
    AllocaInst *gcframe = NULL;
#endif
    Value *myargs;
    Function::arg_iterator AI = llvmf->arg_begin();
    std::vector<jl_value_t *>::iterator AIt = fargjt.begin();
    if (n_roots > 0) {
        ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                           ConstantInt::get(T_int32, n_roots));
#ifdef JL_GC_MARKSWEEP
        // create gc frame
        gcframe = builder.CreateAlloca(T_gcframe, 0);
        builder.CreateStore(builder.CreateBitCast(ctx.argTemp,
                                                  PointerType::get(jl_ppvalue_llvmt,0)),
                            builder.CreateConstGEP2_32(gcframe, 0, 0));
        builder.CreateStore(ConstantInt::get(T_size, n_roots),
                            builder.CreateConstGEP2_32(gcframe, 0, 1));
        builder.CreateStore(ConstantInt::get(T_int32, 0),
                            builder.CreateConstGEP2_32(gcframe, 0, 2));
        builder.CreateStore(builder.CreateLoad(jlpgcstack_var, false),
                            builder.CreateConstGEP2_32(gcframe, 0, 3));
        builder.CreateStore(gcframe, jlpgcstack_var, false);
        // initialize stack roots to null
        for(i=0; i < (size_t)n_roots; i++) {
            Value *argTempi = builder.CreateConstGEP1_32(ctx.argTemp,i);
            builder.CreateStore(V_null, argTempi);
        }
#endif
         // box values for function arguments
        for(i=0; i < (size_t)n_roots; i++) {
            Value *lv = builder.CreateConstGEP1_32(ctx.argTemp,i);
            //LoadInst *theArg = builder.CreateLoad(argPtr, false);
            Value *theArg = AI++;
            //mark_julia_type(theArg, *AIt++);
            builder.CreateStore(boxed(theArg), lv);
        }
        myargs = ctx.argTemp;
    }
    else {
        ctx.argTemp = NULL;
        myargs = Constant::getNullValue(jl_ppvalue_llvmt);
    }
    Value *result = builder.CreateCall3(jlapplygeneric_func, literal_pointer_val((jl_value_t*)f), myargs,
                                        ConstantInt::get(T_int32,n_roots));
#ifdef JL_GC_MARKSWEEP
    // JL_GC_POP();
    if (n_roots > 0) {
        builder.CreateStore(builder.CreateLoad(builder.CreateConstGEP2_32(gcframe, 0, 3), false),
                jlpgcstack_var);
    }
#endif
    Value *retval = (lrt == jl_pvalue_llvmt ? result : emit_unbox(lrt, PointerType::get(lrt, 0), result));
    builder.CreateRet(retval);
    FPM->run(*llvmf);
    void *fptr = jl_ExecutionEngine->getPointerToFunction(llvmf);
    //llvmf->dump();
    llvmf->deleteBody();
    return fptr;
}

