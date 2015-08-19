// This file is a part of Julia. License is MIT: http://julialang.org/license

// utility procedures used in code generation

#if defined(USE_MCJIT) && defined(_OS_WINDOWS_)
template<class T> // for GlobalObject's
static T* addComdat(T *G)
{
    if (imaging_mode && (!G->isDeclarationForLinker())) {
        Comdat *jl_Comdat = G->getParent()->getOrInsertComdat(G->getName());
        jl_Comdat->setSelectionKind(Comdat::NoDuplicates);
        G->setComdat(jl_Comdat);
    }
    return G;
}
#else
template<class T>
static T* addComdat(T *G) { return G; }
#endif

static Value *tbaa_decorate(MDNode* md, Instruction* load_or_store)
{
    load_or_store->setMetadata( llvm::LLVMContext::MD_tbaa, md );
    return load_or_store;
}

// Fixing up references to other modules for MCJIT
static GlobalVariable *prepare_global(GlobalVariable *G)
{
#ifdef USE_MCJIT
    if (G->getParent() != jl_Module) {
        GlobalVariable *gv = jl_Module->getGlobalVariable(G->getName());
        if (!gv) {
            gv = new GlobalVariable(*jl_Module, G->getType()->getElementType(),
                                    G->isConstant(), GlobalVariable::ExternalLinkage,
                                    NULL, G->getName());
        }
        return gv;
    }
#endif
    return G;
}

static llvm::Value *prepare_call(llvm::Value* Callee)
{
#ifdef USE_MCJIT
    llvm::Function *F = dyn_cast<Function>(Callee);
    if (!F)
        return Callee;
    if (F->getParent() != jl_Module) {
        Function *ModuleF = jl_Module->getFunction(F->getName());
        if (ModuleF) {
            return ModuleF;
        }
        else {
            return Function::Create(F->getFunctionType(),
                                    Function::ExternalLinkage,
                                    F->getName(),
                                    jl_Module);
        }
    }
#endif
    return Callee;
}

#ifdef LLVM35
static inline void add_named_global(GlobalObject *gv, void *addr)
#else
static inline void add_named_global(GlobalValue *gv, void *addr)
#endif
{
#ifdef USE_MCJIT

    StringRef name = gv->getName();
#ifdef _OS_WINDOWS_
    std::string imp_name;
    // setting DLLEXPORT correctly only matters when building a binary
    if (jl_generating_output()) {
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
        // this will cause llvm to rename it, so we do the same
        imp_name = Twine("__imp_", name).str();
        name = StringRef(imp_name);
        // __imp_ functions are jmp stubs (no additional work needed)
        // __imp_ variables are indirection pointers, so use malloc to simulate that too
        if (isa<GlobalVariable>(gv)) {
            void** imp_addr = (void**)malloc(sizeof(void**));
            *imp_addr = addr;
            addr = (void*)imp_addr;
        }
    }
#endif
    addComdat(gv);
    sys::DynamicLibrary::AddSymbol(name, addr);

#else // USE_MCJIT

#ifdef _OS_WINDOWS_
    // setting DLLEXPORT correctly only matters when building a binary
    if (jl_generating_output()) {
        if (gv->getLinkage() == GlobalValue::ExternalLinkage)
            gv->setLinkage(GlobalValue::DLLImportLinkage);
#ifdef _P64
        // the following is correct by observation,
        // as long as everything stays within a 32-bit offset :/
        void** imp_addr = (void**)malloc(sizeof(void**));
        *imp_addr = addr;
        addr = (void*)imp_addr;
#endif
    }
#endif // _OS_WINDOWS_
    jl_ExecutionEngine->addGlobalMapping(gv, addr);
#endif // USE_MCJIT
}

// --- string constants ---
static std::map<const std::string, GlobalVariable*> stringConstants;

extern "C" {
    extern int jl_in_inference;
}

static GlobalVariable *stringConst(const std::string &txt)
{
    GlobalVariable *gv = stringConstants[txt];
    static int strno = 0;
    // in inference, we can not share string constants between
    // modules as there might be multiple compiles on the stack
    // with calls in between them.
    if (gv == NULL || jl_in_inference) {
        std::stringstream ssno;
        std::string vname;
        ssno << strno;
        vname += "_j_str";
        vname += ssno.str();
        gv = new GlobalVariable(*jl_Module,
                                ArrayType::get(T_int8, txt.length()+1),
                                true,
                                imaging_mode ? GlobalVariable::PrivateLinkage : GlobalVariable::ExternalLinkage,
                                ConstantDataArray::get(getGlobalContext(),
                                                       ArrayRef<unsigned char>(
                                                       (const unsigned char*)txt.c_str(),
                                                       txt.length()+1)),
                                vname);
        gv->setUnnamedAddr(true);
        stringConstants[txt] = gv;
        strno++;
    }
    return gv;

}

// --- Shadow module handling ---

typedef struct {Value* gv; int32_t index;} jl_value_llvm; // uses 1-based indexing
static std::map<void*, jl_value_llvm> jl_value_to_llvm;
DLLEXPORT std::map<Value *, void*> jl_llvm_to_jl_value;

#ifdef USE_MCJIT
class FunctionMover : public ValueMaterializer
{
public:
    FunctionMover(llvm::Module *dest,llvm::Module *src) :
        ValueMaterializer(), VMap(), destModule(dest), srcModule(src),
        LazyFunctions(0)
    {

    }
    ValueToValueMapTy VMap;
    llvm::Module *destModule;
    llvm::Module *srcModule;
    std::vector<Function *> LazyFunctions;

    Function *CloneFunctionProto(Function *F)
    {
        Function *NewF = Function::Create(F->getFunctionType(),
                                          Function::ExternalLinkage,
                                          F->getName(),
                                          destModule);
        LazyFunctions.push_back(F);
        VMap[F] = NewF;
        return NewF;
    }

    void CloneFunctionBody(Function *F)
    {
        Function *NewF = (Function*)(Value*)VMap[F];
        assert(NewF != NULL);

        Function::arg_iterator DestI = NewF->arg_begin();
        for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
            DestI->setName(I->getName());    // Copy the name over...
            VMap[I] = DestI++;        // Add mapping to VMap
        }

    #ifdef LLVM36
        // Clone debug info - Not yet public API
        // llvm::CloneDebugInfoMetadata(NewF,F,VMap);
    #endif

        SmallVector<ReturnInst*, 8> Returns;
        llvm::CloneFunctionInto(NewF,F,VMap,true,Returns,"",NULL,NULL,this);
    }

    Function *CloneFunction(Function *F)
    {
        Function *NewF = (llvm::Function*)MapValue(F,VMap,RF_None,NULL,this);
        ResolveLazyFunctions();
        return NewF;
    }

    void ResolveLazyFunctions()
    {
        while (!LazyFunctions.empty()) {
            Function *F = LazyFunctions.back();
            LazyFunctions.pop_back();

            CloneFunctionBody(F);
        }
    }

    virtual Value *materializeValueFor (Value *V)
    {
        Function *F = dyn_cast<Function>(V);
        if (F) {
            if (F->isIntrinsic()) {
                return destModule->getOrInsertFunction(F->getName(),F->getFunctionType());
            }
            if (F->isDeclaration() || F->getParent() != destModule) {
                if (F->getName().empty())
                    return CloneFunctionProto(F);
                Function *shadow = srcModule->getFunction(F->getName());
                if (shadow != NULL && !shadow->isDeclaration()) {
                    // Not truly external
                    // Check whether we already emitted it once
                    uint64_t addr = jl_mcjmm->getSymbolAddress(F->getName());
                    if (addr == 0) {
                        Function *oldF = destModule->getFunction(F->getName());
                        if (oldF)
                            return oldF;
                        return CloneFunctionProto(F);
                    }
                    else {
                        return destModule->getOrInsertFunction(F->getName(),F->getFunctionType());
                    }
                }
                else if (!F->isDeclaration()) {
                    return CloneFunctionProto(F);
                }
            }
            // Still a declaration and still in a different module
            if (F->isDeclaration() && F->getParent() != destModule) {
                // Create forward declaration in current module
                return destModule->getOrInsertFunction(F->getName(),F->getFunctionType());
            }
        }
        else if (isa<GlobalVariable>(V)) {
            GlobalVariable *GV = cast<GlobalVariable>(V);
            assert(GV != NULL);
            GlobalVariable *oldGV = destModule->getGlobalVariable(GV->getName());
            if (oldGV != NULL)
                return oldGV;
            GlobalVariable *newGV = new GlobalVariable(*destModule,
                GV->getType()->getElementType(),
                GV->isConstant(),
                GlobalVariable::ExternalLinkage,
                NULL,
                GV->getName());
            newGV->copyAttributesFrom(GV);
            if (GV->isDeclaration())
                return newGV;
            if (!GV->getName().empty()) {
                uint64_t addr = jl_ExecutionEngine->getGlobalValueAddress(GV->getName());
                if (addr != 0) {
                    newGV->setExternallyInitialized(true);
                    return newGV;
                }
            }
            std::map<Value*, void *>::iterator it;
            it = jl_llvm_to_jl_value.find(GV);
            if (it != jl_llvm_to_jl_value.end()) {
                newGV->setInitializer(Constant::getIntegerValue(GV->getType()->getElementType(),APInt(sizeof(void*)*8,(ptrint_t)it->second)));
                newGV->setConstant(true);
            }
            else if (GV->hasInitializer()) {
                Value *C = MapValue(GV->getInitializer(),VMap,RF_None,NULL,this);
                newGV->setInitializer(cast<Constant>(C));
            }
            return newGV;
        }
        return NULL;
    };
};
#endif

#ifdef LLVM37
static DIType *julia_type_to_di(jl_value_t *jt, DIBuilder *dbuilder, bool isboxed = false)
#else
static DIType julia_type_to_di(jl_value_t *jt, DIBuilder *dbuilder, bool isboxed = false)
#endif
{
    if (jl_is_abstracttype(jt) || !jl_is_datatype(jt) || !jl_isbits(jt) || isboxed)
        return jl_pvalue_dillvmt;
    jl_datatype_t *jdt = (jl_datatype_t*)jt;
    if (jdt->ditype != NULL) {
#ifdef LLVM37
        return (llvm::DIType*)jdt->ditype;
#else
        return DIType((llvm::MDNode*)jdt->ditype);
#endif
    }
    if (jl_is_bitstype(jt)) {
    #ifdef LLVM37
        llvm::DIType *t = dbuilder->createBasicType(jdt->name->name->name,jdt->size,jdt->alignment,llvm::dwarf::DW_ATE_unsigned);
        jdt->ditype = t;
        return t;
    #else
        DIType t = dbuilder->createBasicType(jdt->name->name->name,jdt->size,jdt->alignment,llvm::dwarf::DW_ATE_unsigned);
        MDNode *M = t;
        jdt->ditype = M;
        return t;
    #endif
    }
    // TODO: Fixme
    return jl_pvalue_dillvmt;
}

// --- emitting pointers directly into code ---

static Value *literal_static_pointer_val(void *p, Type *t)
{
    // this function will emit a static pointer into the generated code
    // the generated code will only be valid during the current session,
    // and thus, this should typically be avoided in new API's
#if defined(_P64)
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int64, (uint64_t)p), t);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int32, (uint32_t)p), t);
#endif
}

static std::vector<Constant*> jl_sysimg_gvars;

extern "C" int32_t jl_get_llvm_gv(jl_value_t *p)
{
    // map a jl_value_t memory location to a GlobalVariable
    std::map<void*, jl_value_llvm>::iterator it;
    it = jl_value_to_llvm.find(p);
    if (it == jl_value_to_llvm.end())
        return 0;
    return it->second.index;
}

#ifdef HAVE_CPUID
extern "C" {
    extern void jl_cpuid(int32_t CPUInfo[4], int32_t InfoType);
}
#endif

static void jl_gen_llvm_globaldata(llvm::Module *mod, ValueToValueMapTy &VMap, const char *sysimg_data, size_t sysimg_len)
{
    ArrayType *atype = ArrayType::get(T_psize, jl_sysimg_gvars.size());
    addComdat(new GlobalVariable(*mod,
                                 atype,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 MapValue(ConstantArray::get(atype, ArrayRef<Constant*>(jl_sysimg_gvars)), VMap),
                                 "jl_sysimg_gvars"));
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size,globalUnique+1),
                                 "jl_globalUnique"));

    Constant *feature_string = ConstantDataArray::getString(jl_LLVMContext, jl_options.cpu_target);
    addComdat(new GlobalVariable(*mod,
                                 feature_string->getType(),
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 feature_string,
                                 "jl_sysimg_cpu_target"));

#ifdef HAVE_CPUID
    // For native also store the cpuid
    if (strcmp(jl_options.cpu_target,"native") == 0) {
        uint32_t info[4];

        jl_cpuid((int32_t*)info, 1);
        addComdat(new GlobalVariable(*mod,
                                     T_int64,
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     ConstantInt::get(T_int64,((uint64_t)info[2])|(((uint64_t)info[3])<<32)),
                                     "jl_sysimg_cpu_cpuid"));
    }
#endif

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(jl_LLVMContext, ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
        addComdat(new GlobalVariable(*mod, data->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data"));
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*mod, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"));
    }
}

static void jl_dump_shadow(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len, bool dump_as_bc)
{
#ifdef LLVM36
    std::error_code err;
    StringRef fname_ref = StringRef(fname);
    raw_fd_ostream OS(fname_ref, err, sys::fs::F_None);
#elif  LLVM35
    std::string err;
    raw_fd_ostream OS(fname, err, sys::fs::F_None);
#else
    std::string err;
    raw_fd_ostream OS(fname, err);
#endif
#ifdef LLVM37 // 3.7 simplified formatted output; just use the raw stream alone
    raw_fd_ostream& FOS(OS);
#else
    formatted_raw_ostream FOS(OS);
#endif

    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
#if defined(_OS_WINDOWS_) && defined(FORCE_ELF)
#ifdef LLVM35
    TheTriple.setObjectFormat(Triple::COFF);
#else
    TheTriple.setEnvironment(Triple::UnknownEnvironment);
#endif
#elif defined(_OS_DARWIN_) && defined(FORCE_ELF)
#ifdef LLVM35
    TheTriple.setObjectFormat(Triple::MachO);
#else
    TheTriple.setEnvironment(Triple::MachO);
#endif
#endif
#ifdef LLVM35
    std::unique_ptr<TargetMachine>
#else
    OwningPtr<TargetMachine>
#endif
    TM(jl_TargetMachine->getTarget().createTargetMachine(
        TheTriple.getTriple(),
        jl_TargetMachine->getTargetCPU(),
        jl_TargetMachine->getTargetFeatureString(),
        jl_TargetMachine->Options,
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        Reloc::PIC_,
#else
        jit_model ? Reloc::PIC_ : Reloc::Default,
#endif
        jit_model ? CodeModel::JITDefault : CodeModel::Default,
        CodeGenOpt::Aggressive // -O3
        ));

    PassManager PM;
    if (!dump_as_bc) {
#ifndef LLVM37
        PM.add(new TargetLibraryInfo(Triple(TM->getTargetTriple())));
#else
        PM.add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
#endif
#ifdef LLVM37
    // No DataLayout pass needed anymore.
#elif LLVM36
        PM.add(new DataLayoutPass());
#elif LLVM35
        PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
        PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
#endif


        if (TM->addPassesToEmitFile(PM, FOS, TargetMachine::CGFT_ObjectFile, false)) {
            jl_error("Could not generate obj file for this target");
        }
    }

    // now copy the module, since PM.run may modify it
    ValueToValueMapTy VMap;
    Module *clone = CloneModule(shadow_module, VMap);
#ifdef LLVM37
    // Reset the target triple to make sure it matches the new target machine
    clone->setTargetTriple(TM->getTargetTriple().str());
#ifdef LLVM38
    clone->setDataLayout(TM->createDataLayout());
#else
    clone->setDataLayout(TM->getDataLayout()->getStringRepresentation());
#endif
#endif

    // add metadata information
    jl_gen_llvm_globaldata(clone, VMap, sysimg_data, sysimg_len);

    // do the actual work
    if (!dump_as_bc)
        PM.run(*clone);
    else
        WriteBitcodeToFile(clone, FOS);
    delete clone;
}

static int32_t jl_assign_functionID(Function *functionObject)
{
    // give the function an index in the constant lookup table
    if (!imaging_mode)
        return 0;
    jl_sysimg_gvars.push_back(ConstantExpr::getBitCast(functionObject,T_psize));
    return jl_sysimg_gvars.size();
}

static Value *julia_gv(const char *cname, void *addr)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    std::map<void*, jl_value_llvm>::iterator it;
    // first see if there already is a GlobalVariable for this address
    it = jl_value_to_llvm.find(addr);
    if (it != jl_value_to_llvm.end())
        return builder.CreateLoad(it->second.gv);

    std::stringstream gvname;
    gvname << cname << globalUnique++;
    // no existing GlobalVariable, create one and store it
    GlobalVariable *gv = new GlobalVariable(*jl_Module, jl_pvalue_llvmt,
                           false, imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                           ConstantPointerNull::get((PointerType*)jl_pvalue_llvmt), gvname.str());
    addComdat(gv);

    // make the pointer valid for this session
#ifdef USE_MCJIT
    jl_llvm_to_jl_value[gv] = addr;
#else
    void **p = (void**)jl_ExecutionEngine->getPointerToGlobal(gv);
    *p = addr;
#endif
    // make the pointer valid for future sessions
    jl_sysimg_gvars.push_back(ConstantExpr::getBitCast(gv, T_psize));
    jl_value_llvm gv_struct;
    gv_struct.gv = gv;
    gv_struct.index = jl_sysimg_gvars.size();
    jl_value_to_llvm[addr] = gv_struct;
    return builder.CreateLoad(gv);
}

static Value *julia_gv(const char *prefix, jl_sym_t *name, jl_module_t *mod, void *addr)
{
    // emit a GlobalVariable for a jl_value_t, using the prefix, name, and module to
    // to create a readable name of the form prefixModA.ModB.name
    size_t len = strlen(name->name)+strlen(prefix)+1;
    jl_module_t *parent = mod, *prev = NULL;
    while (parent != NULL && parent != prev) {
        len += strlen(parent->name->name)+1;
        prev = parent;
        parent = parent->parent;
    }
    char *fullname = (char*)alloca(len);
    strcpy(fullname, prefix);
    len -= strlen(name->name)+1;
    strcpy(fullname+len,name->name);
    parent = mod;
    prev = NULL;
    while (parent != NULL && parent != prev) {
        size_t part = strlen(parent->name->name)+1;
        strcpy(fullname+len-part,parent->name->name);
        fullname[len-1] = '.';
        len -= part;
        prev = parent;
        parent = parent->parent;
    }
    return julia_gv(fullname, addr);
}

static Value *literal_pointer_val(jl_value_t *p)
{
    // emit a pointer to any jl_value_t which will be valid across reloading code
    // also, try to give it a nice name for gdb, for easy identification
    if (p == NULL)
        return ConstantPointerNull::get((PointerType*)jl_pvalue_llvmt);
    // some common constant values
    if (p == jl_false)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var)));
    if (p == jl_true)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jltrue_var)));
    if (p == (jl_value_t*)jl_emptysvec)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlemptysvec_var)));
    if (!imaging_mode)
        return literal_static_pointer_val(p, jl_pvalue_llvmt);
    if (jl_is_datatype(p)) {
        jl_datatype_t *addr = (jl_datatype_t*)p;
        // DataTypes are prefixed with a +
        return julia_gv("+", addr->name->name, addr->name->module, p);
    }
    if (jl_is_func(p)) {
        jl_lambda_info_t *linfo = ((jl_function_t*)p)->linfo;
        // Functions are prefixed with a -
        if (linfo != NULL)
            return julia_gv("-", linfo->name, linfo->module, p);
        // Anonymous lambdas are prefixed with jl_method#
        return julia_gv("jl_method#", p);
    }
    if (jl_is_lambda_info(p)) {
        jl_lambda_info_t *linfo = (jl_lambda_info_t*)p;
        // Type-inferred functions are prefixed with a -
        return julia_gv("-", linfo->name, linfo->module, p);
    }
    if (jl_is_symbol(p)) {
        jl_sym_t *addr = (jl_sym_t*)p;
        // Symbols are prefixed with jl_sym#
        return julia_gv("jl_sym#", addr, NULL, p);
    }
    if (jl_is_gensym(p)) {
        // GenSyms are prefixed with jl_gensym#
        return julia_gv("jl_gensym#", p);
    }
    // something else gets just a generic name
    return julia_gv("jl_global#", p);
}

static Value *literal_pointer_val(jl_binding_t *p)
{
    // emit a pointer to any jl_value_t which will be valid across reloading code
    if (p == NULL)
        return ConstantPointerNull::get((PointerType*)jl_pvalue_llvmt);
    if (!imaging_mode)
        return literal_static_pointer_val(p, jl_pvalue_llvmt);
    // bindings are prefixed with jl_bnd#
    return julia_gv("jl_bnd#", p->name, p->owner, p);
}

static Value *julia_binding_gv(Value *bv)
{
    return builder.
        CreateGEP(bv,ConstantInt::get(T_size,
                                      offsetof(jl_binding_t,value)/sizeof(size_t)));
}

static Value *julia_binding_gv(jl_binding_t *b)
{
    // emit a literal_pointer_val to the value field of a jl_binding_t
    // binding->value are prefixed with *
    Value *bv = imaging_mode ?
        builder.CreateBitCast(julia_gv("*", b->name, b->owner, b), jl_ppvalue_llvmt) :
        literal_static_pointer_val(b,jl_ppvalue_llvmt);
    return julia_binding_gv(bv);
}

// --- mapping between julia and llvm types ---

static bool type_is_ghost(Type *ty)
{
    return (ty == T_void || ty->isEmptyTy());
}

static Type *julia_struct_to_llvm(jl_value_t *jt);

extern "C" {
DLLEXPORT Type *julia_type_to_llvm(jl_value_t *jt)
{
    // this function converts a Julia Type into the equivalent LLVM type
    if (jt == (jl_value_t*)jl_bool_type) return T_int1;
    if (jt == (jl_value_t*)jl_bottom_type) return T_void;
    if (!jl_is_leaf_type(jt))
        return jl_pvalue_llvmt;
    if (jl_is_cpointer_type(jt)) {
        Type *lt = julia_type_to_llvm(jl_tparam0(jt));
        if (lt == NULL)
            return NULL;
        if (lt == T_void)
            return T_pint8;
        return PointerType::get(lt, 0);
    }
    if (jl_is_bitstype(jt)) {
        int nb = jl_datatype_size(jt);
        if (jl_is_floattype(jt)) {
#ifndef DISABLE_FLOAT16
            if (nb == 2)
                return Type::getHalfTy(jl_LLVMContext);
            else
#endif
            if (nb == 4)
                return Type::getFloatTy(jl_LLVMContext);
            else if (nb == 8)
                return Type::getDoubleTy(jl_LLVMContext);
            else if (nb == 16)
                return Type::getFP128Ty(jl_LLVMContext);
        }
        return Type::getIntNTy(jl_LLVMContext, nb*8);
    }
    if (jl_isbits(jt)) {
        if (((jl_datatype_t*)jt)->size == 0) {
            return T_void;
        }
        return julia_struct_to_llvm(jt);
    }
    return jl_pvalue_llvmt;
}
}

static Type *julia_struct_to_llvm(jl_value_t *jt)
{
    // this function converts a Julia Type into the equivalent LLVM struct
    // use this where C-compatible (unboxed) structs are desired
    // use julia_type_to_llvm directly when you want to preserve Julia's type semantics
    bool isTuple = jl_is_tuple_type(jt);
    if ((isTuple || jl_is_structtype(jt)) && !jl_is_array_type(jt)) {
        if (!jl_is_leaf_type(jt))
            return NULL;
        jl_datatype_t *jst = (jl_datatype_t*)jt;
        if (jst->struct_decl == NULL) {
            size_t ntypes = jl_datatype_nfields(jst);
            if (ntypes == 0 || jst->size == 0)
                return T_void;
            StructType *structdecl;
            if (!isTuple) {
                structdecl = StructType::create(jl_LLVMContext, jst->name->name->name);
                jst->struct_decl = structdecl;
            }
            std::vector<Type*> latypes(0);
            size_t i;
            bool isvector = true;
            Type *lasttype = NULL;
            for(i = 0; i < ntypes; i++) {
                jl_value_t *ty = jl_svecref(jst->types, i);
                Type *lty;
                if (jst->fields[i].isptr)
                    lty = jl_pvalue_llvmt;
                else
                    lty = ty==(jl_value_t*)jl_bool_type ? T_int8 : julia_type_to_llvm(ty);
                if (lasttype != NULL && lasttype != lty)
                    isvector = false;
                lasttype = lty;
                if (lty == T_void || lty->isEmptyTy())
                    lty = NoopType;
                latypes.push_back(lty);
            }
            if (!isTuple) {
                structdecl->setBody(latypes);
            }
            else {
                if (isvector && lasttype != T_int1 && lasttype != T_void) {
                    // TODO: currently we get LLVM assertion failures for other vector sizes
                    bool validVectorSize = (ntypes == 2 || ntypes == 4 || ntypes == 6);
                    if (0 && lasttype->isSingleValueType() && !lasttype->isVectorTy() && validVectorSize) // currently disabled due to load/store alignment issues
                        jst->struct_decl = VectorType::get(lasttype, ntypes);
                    else
                        jst->struct_decl = ArrayType::get(lasttype, ntypes);
                }
                else {
                    jst->struct_decl = StructType::get(jl_LLVMContext,ArrayRef<Type*>(&latypes[0],ntypes));
                }
            }
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
    if (t == T_void) return (jl_value_t*)jl_void_type;
    if (t->isEmptyTy()) return (jl_value_t*)jl_void_type;
    if (t == jl_pvalue_llvmt)
        return (jl_value_t*)jl_any_type;
    if (t->isPointerTy()) {
        jl_value_t *elty = llvm_type_to_julia(t->getContainedType(0),
                                              throw_error);
        if (elty != NULL) {
            return (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                              jl_svec1(elty));
        }
    }
    if (throw_error) {
        jl_error("cannot convert type to a julia type");
    }
    return NULL;
}

static bool is_datatype_all_pointers(jl_datatype_t *dt)
{
    size_t i, l = jl_datatype_nfields(dt);
    for(i=0; i < l; i++) {
        if (!dt->fields[i].isptr)
            return false;
    }
    return true;
}

static bool is_tupletype_homogeneous(jl_svec_t *t)
{
    size_t i, l = jl_svec_len(t);
    if (l > 0) {
        jl_value_t *t0 = jl_svecref(t, 0);
        if (!jl_is_leaf_type(t0))
            return false;
        for(i=1; i < l; i++) {
            if (!jl_types_equal(t0, jl_svecref(t,i)))
                return false;
        }
    }
    return true;
}

// --- scheme for tagging llvm values with julia types using metadata ---

static std::map<int, jl_value_t*> typeIdToType;
extern "C" {
    jl_array_t *typeToTypeId;
}
static int cur_type_id = 1;

static int jl_type_to_typeid(jl_value_t *t)
{
    jl_value_t *idx = jl_eqtable_get(typeToTypeId, t, NULL);
    if (idx == NULL) {
        int mine = cur_type_id++;
        if (mine > 65025)
            jl_error("internal compiler error: too many bits types");
        JL_GC_PUSH1(&idx);
        idx = jl_box_long(mine);
        typeToTypeId = jl_eqtable_put(typeToTypeId, t, idx);
        typeIdToType[mine] = t;
        JL_GC_POP();
        return mine;
    }
    return jl_unbox_long(idx);
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
    Instruction *inst = (dyn_cast<Instruction>(v));
    return (inst != NULL) &&
            (inst->getMetadata("julia_type")!=NULL);
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
    assert(v != NULL);
    if (dyn_cast<Instruction>(v) == NULL ||
        (mdn = ((Instruction*)v)->getMetadata("julia_type")) == NULL) {
        return julia_type_of_without_metadata(v, true);
    }
#ifdef LLVM36
    MDString *md = dyn_cast<MDString>(mdn->getOperand(0).get());
#else
    MDString *md = (MDString*)mdn->getOperand(0);
#endif
    assert(md != NULL);
    const unsigned char *vts = (const unsigned char*)md->getString().data();
    int idx = (vts[0]-1) + (vts[1]-1)*255;
    return jl_typeid_to_type(idx);
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
        v = NoOpInst(v);
    }
    else if (julia_type_of_without_metadata(v,false) == jt) {
        return v;
    }
    if (dyn_cast<Instruction>(v) == NULL)
        v = NoOpInst(v);
    assert(dyn_cast<Instruction>(v));
    char name[3];
    int idx = jl_type_to_typeid(jt);
    // store idx as base-255 to avoid NUL
    name[0] = (idx%255)+1;
    name[1] = (idx/255)+1;
    name[2] = '\0';
    MDString *md = MDString::get(jl_LLVMContext, name);
#ifdef LLVM36
    MDNode *mdn = MDNode::get(jl_LLVMContext, ArrayRef<Metadata*>(md));
#else
    MDNode *mdn = MDNode::get(jl_LLVMContext, ArrayRef<Value*>(md));
#endif
    ((Instruction*)v)->setMetadata("julia_type", mdn);
    return v;
}

// --- generating various field accessors ---

static Value *emit_nthptr_addr(Value *v, ssize_t n)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt),
                             ConstantInt::get(T_size, (ssize_t)n));
}

static Value *emit_nthptr_addr(Value *v, Value *idx)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt), idx);
}

static Value *emit_nthptr(Value *v, ssize_t n, MDNode *tbaa)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, n);
    return tbaa_decorate(tbaa,builder.CreateLoad(vptr, false));
}

static Value *emit_nthptr_recast(Value *v, ssize_t n, MDNode *tbaa, Type* ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(v, n);
    return tbaa_decorate(tbaa,builder.CreateLoad(builder.CreateBitCast(vptr,ptype), false));
}

static Value *emit_nthptr_recast(Value *v, Value *idx, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(v, idx);
    return tbaa_decorate(tbaa,builder.CreateLoad(builder.CreateBitCast(vptr,ptype), false));
}

static Value *emit_typeptr_addr(Value *p)
{
   ssize_t offset = (offsetof(jl_taggedvalue_t,value) - offsetof(jl_taggedvalue_t,type)) / sizeof(jl_value_t*);
   return emit_nthptr_addr(p, -offset);
}

static Value *emit_typeof(Value *p)
{
    // given p, a jl_value_t*, compute its type tag
    if (p->getType() == jl_pvalue_llvmt) {
        Value *tt = builder.CreateBitCast(p, jl_ppvalue_llvmt);
        tt = builder.CreateLoad(emit_typeptr_addr(tt), false);
        tt = builder.CreateIntToPtr(builder.CreateAnd(
                    builder.CreatePtrToInt(tt, T_size),
                    ConstantInt::get(T_size,~(uptrint_t)15)),
                jl_pvalue_llvmt);
        return tt;
    }
    return literal_pointer_val(julia_type_of(p));
}

static Value *emit_datatype_types(Value *dt)
{
    return builder.
        CreateLoad(builder.
                   CreateBitCast(builder.
                                 CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                           ConstantInt::get(T_size, offsetof(jl_datatype_t, types))),
                                 jl_ppvalue_llvmt));
}

static Value *emit_datatype_nfields(Value *dt)
{
    Value *nf = builder.
        CreateLoad(builder.
                   CreateBitCast(builder.
                                 CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                           ConstantInt::get(T_size, offsetof(jl_datatype_t, nfields))),
                                 T_pint32));
#ifdef _P64
    nf = builder.CreateSExt(nf, T_int64);
#endif
    return nf;
}

// --- generating various error checks ---

static jl_value_t *llvm_type_to_julia(Type *t, bool err=true);

static void just_emit_error(const std::string &txt, jl_codectx_t *ctx)
{
    Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                        ConstantInt::get(T_int32, 0) };
    builder.CreateCall(prepare_call(jlerror_func),
                       builder.CreateGEP(stringConst(txt),
                                         ArrayRef<Value*>(zeros)));
}

static void emit_error(const std::string &txt, jl_codectx_t *ctx)
{
    just_emit_error(txt, ctx);
    builder.CreateUnreachable();
    BasicBlock *cont = BasicBlock::Create(getGlobalContext(),"after_error",ctx->f);
    builder.SetInsertPoint(cont);
}

static void error_unless(Value *cond, const std::string &msg, jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    just_emit_error(msg, ctx);
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
#ifdef LLVM37
    builder.CreateCall(prepare_call(jlthrow_line_func), { exc,
                        ConstantInt::get(T_int32, ctx->lineno) });
#else
    builder.CreateCall2(prepare_call(jlthrow_line_func), exc,
                        ConstantInt::get(T_int32, ctx->lineno));
#endif
    builder.CreateUnreachable();
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void raise_exception_unless(Value *cond, GlobalVariable *exc,
                                   jl_codectx_t *ctx)
{
    raise_exception_unless(cond, (Value*)tbaa_decorate(tbaa_const,builder.CreateLoad(exc, false)), ctx);
}

static void raise_exception_if(Value *cond, Value *exc, jl_codectx_t *ctx)
{
    raise_exception_unless(builder.CreateXor(cond, ConstantInt::get(T_int1,-1)),
                           exc, ctx);
}

static void raise_exception_if(Value *cond, GlobalVariable *exc, jl_codectx_t *ctx)
{
    raise_exception_if(cond, (Value*)builder.CreateLoad(exc, false), ctx);
}

static void null_pointer_check(Value *v, jl_codectx_t *ctx)
{
    raise_exception_unless(builder.CreateICmpNE(v,Constant::getNullValue(v->getType())),
                           prepare_global(jlundeferr_var), ctx);
}

static Value *boxed(Value *v, jl_codectx_t *ctx, jl_value_t *jt=NULL);

static void emit_type_error(Value *x, jl_value_t *type, const std::string &msg,
                            jl_codectx_t *ctx)
{
    Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                        ConstantInt::get(T_int32, 0) };
    Value *fname_val = builder.CreateGEP(stringConst(ctx->funcName),
                                         ArrayRef<Value*>(zeros));
    Value *msg_val = builder.CreateGEP(stringConst(msg),
                                       ArrayRef<Value*>(zeros));
#ifdef LLVM37
    builder.CreateCall(prepare_call(jltypeerror_func),
                        { fname_val, msg_val,
                        literal_pointer_val(type), boxed(x,ctx),
                        ConstantInt::get(T_int32, ctx->lineno) });
#else
    builder.CreateCall5(prepare_call(jltypeerror_func),
                    fname_val, msg_val,
                    literal_pointer_val(type), boxed(x,ctx),
                    ConstantInt::get(T_int32, ctx->lineno));
#endif
}

static void emit_typecheck(Value *x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype;
    if (jl_is_type_type(type) || !jl_is_leaf_type(type)) {
        istype = builder.
            CreateICmpNE(
#ifdef LLVM37
                builder.CreateCall(prepare_call(jlsubtype_func), { x, literal_pointer_val(type),
                                             ConstantInt::get(T_int32,1) }),
#else
                builder.CreateCall3(prepare_call(jlsubtype_func), x, literal_pointer_val(type),
                                             ConstantInt::get(T_int32,1)),
#endif
                         ConstantInt::get(T_int32,0));
    }
    else {
        istype = builder.CreateICmpEQ(emit_typeof(x), literal_pointer_val(type));
    }
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    emit_type_error(x, type, msg, ctx);
    builder.CreateUnreachable();

    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

#define CHECK_BOUNDS 1
static Value *emit_bounds_check(Value *a, jl_value_t *ty, Value *i, Value *len, jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_size, 1));
#if CHECK_BOUNDS==1
    if (((ctx->boundsCheck.empty() || ctx->boundsCheck.back()==true) &&
         jl_options.check_bounds != JL_OPTIONS_CHECK_BOUNDS_OFF) ||
         jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_ON) {
        Value *ok = builder.CreateICmpULT(im1, len);
        BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
        BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
        builder.CreateCondBr(ok, passBB, failBB);
        builder.SetInsertPoint(failBB);
        if (ty == (jl_value_t*)jl_any_type) {
#ifdef LLVM37
            builder.CreateCall(prepare_call(jlvboundserror_func), { a, len, i });
#else
            builder.CreateCall3(prepare_call(jlvboundserror_func), a, len, i);
#endif
        }
        else if (ty && a->getType() != jl_pvalue_llvmt) {
            if (!a->getType()->isPtrOrPtrVectorTy()) {
                // CreateAlloca is OK here since we are on an error branch
                Value *tempSpace = builder.CreateAlloca(a->getType());
                builder.CreateStore(a, tempSpace);
                a = tempSpace;
            }
#ifdef LLVM37
            builder.CreateCall(prepare_call(jluboundserror_func), {
                                builder.CreatePointerCast(a, T_pint8),
                                literal_pointer_val(ty),
                                i });
#else
            builder.CreateCall3(prepare_call(jluboundserror_func),
                                builder.CreatePointerCast(a, T_pint8),
                                literal_pointer_val(ty),
                                i);
#endif
        }
        else {
#ifdef LLVM37
            builder.CreateCall(prepare_call(jlboundserror_func), { a, i });
#else
            builder.CreateCall2(prepare_call(jlboundserror_func), a, i);
#endif
        }
        builder.CreateUnreachable();
        ctx->f->getBasicBlockList().push_back(passBB);
        builder.SetInsertPoint(passBB);
    }
#endif
    return im1;
}

// --- loading and storing ---

static AllocaInst *emit_static_alloca(Type *lty, jl_codectx_t *ctx) {
    return new AllocaInst(lty, "", /*InsertBefore=*/ctx->gc.gcframe);
}

static Value *emit_reg2mem(Value *v, jl_codectx_t *ctx) {
    // eagerly put this back onto the stack
    // llvm mem2reg pass will remove this if unneeded
    if (v->getType()->isAggregateType() && !type_is_ghost(v->getType())) {
        Value *loc = emit_static_alloca(v->getType(), ctx);
        builder.CreateStore(v, loc);
        return loc;
    }
    return v;
}


static Value *ghostValue(jl_value_t *ty);

static Value *typed_load(Value *ptr, Value *idx_0based, jl_value_t *jltype,
                         jl_codectx_t *ctx, MDNode* tbaa, size_t alignment = 0)
{
    Type *elty = julia_type_to_llvm(jltype);
    assert(elty != NULL);
    if (elty == T_void)
        return ghostValue(jltype);
    bool isbool=false;
    if (elty == T_int1) {
        elty = T_int8;
        isbool = true;
    }
    Value *data;
    if (ptr->getType()->getContainedType(0) != elty)
        data = builder.CreatePointerCast(ptr, PointerType::get(elty, 0));
    else
        data = ptr;
    if (idx_0based)
        data = builder.CreateGEP(data, idx_0based);
    Value *elt;
    // TODO: can only lazy load if we can create a gc root for ptr for the lifetime of elt
    //if (elty->isAggregateType() && tbaa == tbaa_immut && !alignment) { // can lazy load on demand, no copy needed
    //    elt = data;
    //}
    //else {
        if (data->getType()->getContainedType(0)->isVectorTy() && !alignment)
            alignment = ((jl_datatype_t*)jltype)->alignment; // prevent llvm from assuming 32 byte alignment of vectors
        Instruction *load = builder.CreateAlignedLoad(data, alignment, false);
        if (tbaa) {
            elt = tbaa_decorate(tbaa, load);
        }
        else {
            elt = load;
        }
        if (elty == jl_pvalue_llvmt) {
            null_pointer_check(elt, ctx);
        }
        elt = emit_reg2mem(elt, ctx);
    //}
    if (isbool)
        return builder.CreateTrunc(elt, T_int1);
    return mark_julia_type(elt, jltype);
}

static Value *emit_unbox(Type *to, Value *x, jl_value_t *jt);

static void typed_store(Value *ptr, Value *idx_0based, Value *rhs,
                        jl_value_t *jltype, jl_codectx_t *ctx, MDNode* tbaa,
                        Value* parent,  // for the write barrier, NULL if no barrier needed
                        size_t alignment = 0)
{
    Type *elty = julia_type_to_llvm(jltype);
    assert(elty != NULL);
    if (elty == T_void)
        return;
    if (elty == T_int1) {
        elty = T_int8;
    }
    if (jl_isbits(jltype) && ((jl_datatype_t*)jltype)->size > 0) {
        rhs = emit_unbox(elty, rhs, jltype);
    }
    else {
        rhs = boxed(rhs,ctx);
        if (parent != NULL) emit_write_barrier(ctx, parent, rhs);
    }
    Value *data;
    if (ptr->getType()->getContainedType(0) != elty)
        data = builder.CreateBitCast(ptr, PointerType::get(elty, 0));
    else
        data = ptr;
    if (data->getType()->getContainedType(0)->isVectorTy() && !alignment)
        alignment = ((jl_datatype_t*)jltype)->alignment; // prevent llvm from assuming 32 byte alignment of vectors
    Instruction *store = builder.CreateAlignedStore(rhs, builder.CreateGEP(data, idx_0based), alignment);
    if (tbaa)
        tbaa_decorate(tbaa, store);
}

// --- convert boolean value to julia ---

static Value *julia_bool(Value *cond)
{
    return builder.CreateSelect(cond,
                                tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jltrue_var))),
                                tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var))));
}

// --- get the inferred type of an AST node ---

static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true,
                               bool allow_alloc=true);

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
    if (jl_is_gensym(e)) {
        int idx = ((jl_gensym_t*)e)->id;
        jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
        return (jl_is_array(gensym_types) ? jl_cellref(gensym_types, idx) : (jl_value_t*)jl_any_type);
    }
    if (jl_is_quotenode(e)) {
        e = jl_fieldref(e,0);
        goto type_of_constant;
    }
    if (jl_is_lambda_info(e))
        return (jl_value_t*)jl_function_type;
    if (jl_is_globalref(e)) {
        jl_value_t *v = static_eval(e, ctx);
        if (v == NULL)
            return (jl_value_t*)jl_any_type;
        e = v;
        goto type_of_constant;
    }
    if (jl_is_topnode(e)) {
        e = jl_fieldref(e,0);
        jl_binding_t *b = jl_get_binding(topmod(ctx), (jl_sym_t*)e);
        if (!b || !b->value)
            return (jl_value_t*)jl_any_type;
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
                for(size_t i=0; i < jl_svec_len(ctx->sp); i+=2) {
                    assert(jl_is_symbol(jl_svecref(ctx->sp, i)));
                    if (e == jl_svecref(ctx->sp, i)) {
                        e = jl_svecref(ctx->sp, i+1);
                        goto type_of_constant;
                    }
                }
            }
            else {
                std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find((jl_sym_t*)e);
                if (it != ctx->vars.end())
                    return (*it).second.declType;
                return (jl_value_t*)jl_any_type;
            }
        }
        jl_binding_t *b = jl_get_binding(ctx->module, (jl_sym_t*)e);
        if (!b || !b->value)
            return (jl_value_t*)jl_any_type;
        if (b->constp)
            e = b->value;
        else
            return (jl_value_t*)jl_any_type;
    }
type_of_constant:
    if (jl_is_datatype(e) || jl_is_uniontype(e) || jl_is_typector(e))
        return (jl_value_t*)jl_wrap_Type(e);
    return (jl_value_t*)jl_typeof(e);
}

// --- accessing the representations of built-in data types ---

static Value *allocate_box_dynamic(Value *jlty, Value *nb, Value *v);
static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val);

static Value *data_pointer(Value *x)
{
    return builder.CreateBitCast(x, jl_ppvalue_llvmt);
}

static Value *emit_getfield_unknownidx(Value *strct, Value *idx, jl_datatype_t *stt, jl_codectx_t *ctx)
{
    Type *llvm_st = strct->getType();
    size_t nfields = jl_datatype_nfields(stt);
    if (llvm_st == jl_pvalue_llvmt || llvm_st->isPointerTy()) { // boxed or stack
        if (is_datatype_all_pointers(stt)) {
            idx = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            Value *fld = tbaa_decorate(tbaa_user, builder.CreateLoad(
                        builder.CreateGEP(
                            builder.CreateBitCast(strct, jl_ppvalue_llvmt),
                            idx)));
            if ((unsigned)stt->ninitialized != nfields)
                null_pointer_check(fld, ctx);
            return fld;
        }
        else if (is_tupletype_homogeneous(stt->types)) {
            assert(nfields > 0); // nf == 0 trapped by all_pointers case
            jl_value_t *jt = jl_field_type(stt, 0);
            idx = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            Value *ptr = data_pointer(strct);
            return typed_load(ptr, idx, jt, ctx, stt->mutabl ? tbaa_user : tbaa_immut);
        }
        else if (llvm_st == jl_pvalue_llvmt) {
            idx = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
#ifdef LLVM37
            Value *fld = builder.CreateCall(prepare_call(jlgetnthfieldchecked_func), { strct, idx });
#else
            Value *fld = builder.CreateCall2(prepare_call(jlgetnthfieldchecked_func), strct, idx);
#endif
            return fld;
        }
    }
    else if (is_tupletype_homogeneous(stt->types)) {
        assert(jl_isbits(stt));
        if (nfields == 0) {
            // TODO: pass correct thing to emit_bounds_check ?
            idx = emit_bounds_check(tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlemptysvec_var))),
                                    (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            return UndefValue::get(jl_pvalue_llvmt);
        }
        assert(!jl_field_isptr(stt, 0));
        jl_value_t *jt = jl_field_type(stt,0);
        if (!stt->uid) {
            // add root for types not cached
            jl_add_linfo_root(ctx->linfo, (jl_value_t*)stt);
        }
        // TODO: pass correct thing to emit_bounds_check ?
        Value *idx0 = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
        if (type_is_ghost(llvm_st)) {
            return mark_julia_type(UndefValue::get(NoopType), jt);
        }
        // llvm::VectorType
        if (sizeof(void*) != sizeof(int))
            idx0 = builder.CreateTrunc(idx0, T_int32); // llvm3.3 requires this
        Value *fld = builder.CreateExtractElement(strct, idx0);
        if (jt == (jl_value_t*)jl_bool_type) {
            fld = builder.CreateTrunc(fld, T_int1);
        }
        return mark_julia_type(fld, jt);
    }
    return NULL;
}

static Value *emit_getfield_knownidx(Value *strct, unsigned idx, jl_datatype_t *jt, jl_codectx_t *ctx)
{
    jl_value_t *jfty = jl_field_type(jt,idx);
    Type *elty = julia_type_to_llvm(jfty);
    assert(elty != NULL);
    if (jfty == jl_bottom_type) {
        raise_exception_unless(ConstantInt::get(T_int1,0), prepare_global(jlundeferr_var), ctx);
        return UndefValue::get(jl_pvalue_llvmt);
    }
    if (elty == T_void)
        return ghostValue(jfty);
    Value *fldv = NULL;
    if (strct->getType() == jl_pvalue_llvmt) {
        Value *addr =
            builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                              ConstantInt::get(T_size, jl_field_offset(jt,idx)));
        MDNode *tbaa = jt->mutabl ? tbaa_user : tbaa_immut;
        if (jl_field_isptr(jt,idx)) {
            Value *fldv = tbaa_decorate(tbaa, builder.CreateLoad(builder.CreateBitCast(addr,jl_ppvalue_llvmt)));
            if (idx >= (unsigned)jt->ninitialized)
                null_pointer_check(fldv, ctx);
            return fldv;
        }
        else {
            int align = jl_field_offset(jt,idx);
            if (align & 1) align = 1;
            else if (align & 2) align = 2;
            else if (align & 4) align = 4;
            else if (align & 8) align = 8;
            else align = 16;
            return typed_load(addr, ConstantInt::get(T_size, 0), jfty, ctx, tbaa, align);
        }
    }
    else if (strct->getType()->isPointerTy()) { // something stack allocated
#       ifdef LLVM37
        Value *addr = builder.CreateConstInBoundsGEP2_32(
            cast<PointerType>(strct->getType()->getScalarType())->getElementType(),
            strct, 0, idx);
#       else
        Value *addr = builder.CreateConstInBoundsGEP2_32(strct, 0, idx);
#       endif
        assert(!jt->mutabl);
        return typed_load(addr, NULL, jfty, ctx, NULL);
    }
    else {
        assert(strct->getType()->isVectorTy());
        fldv = builder.CreateExtractElement(strct, ConstantInt::get(T_int32, idx));
        if (jfty == (jl_value_t*)jl_bool_type) {
            fldv = builder.CreateTrunc(fldv, T_int1);
        }
        assert(!jl_field_isptr(jt, idx));
        return mark_julia_type(fldv, jfty);
    }
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
    int o = offsetof(jl_array_t, nrows)/sizeof(void*) - 1;
    return emit_nthptr_recast(t, builder.CreateAdd(dim,
                                                   ConstantInt::get(dim->getType(), o)),
                              tbaa_arraysize, T_psize);
}

static jl_arrayvar_t *arrayvar_for(jl_value_t *ex, jl_codectx_t *ctx)
{
    if (ex == NULL) return NULL;
    jl_sym_t *aname=NULL;
    if (jl_is_symbol(ex))
        aname = ((jl_sym_t*)ex);
    else if (jl_is_symbolnode(ex))
        aname = jl_symbolnode_sym(ex);
    if (aname && ctx->arrayvars->find(aname) != ctx->arrayvars->end()) {
        return &(*ctx->arrayvars)[aname];
    }
    //TODO: gensym case
    return NULL;
}

static Value *emit_arraysize(Value *t, int dim)
{
    return emit_arraysize(t, ConstantInt::get(T_int32, dim));
}

static Value *emit_arraylen_prim(Value *t, jl_value_t *ty)
{
#ifdef STORE_ARRAY_LEN
    (void)ty;
    Value *addr = builder.CreateStructGEP(
#ifdef LLVM37
                                          nullptr,
#endif
                                          builder.CreateBitCast(t,jl_parray_llvmt),
                                          1); //index (not offset) of length field in jl_parray_llvmt

    return tbaa_decorate(tbaa_arraylen, builder.CreateLoad(addr, false));
#else
    jl_value_t *p1 = jl_tparam1(ty);
    if (jl_is_long(p1)) {
        size_t nd = jl_unbox_long(p1);
        Value *l = ConstantInt::get(T_size, 1);
        for(size_t i=0; i < nd; i++) {
            l = builder.CreateMul(l, emit_arraysize(t, (int)(i+1)));
        }
        return l;
    }
    else {
        std::vector<Type *> fargt(0);
        fargt.push_back(jl_pvalue_llvmt);
        FunctionType *ft = FunctionType::get(T_size, fargt, false);
        Value *alen = jl_Module->getOrInsertFunction("jl_array_len_", ft);
        return builder.CreateCall(prepare_call(alen), t);
    }
#endif
}

static Value *emit_arraylen(Value *t, jl_value_t *ex, jl_codectx_t *ctx)
{
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av!=NULL)
        return builder.CreateLoad(av->len);
    return emit_arraylen_prim(t, expr_type(ex,ctx));
}

static Value *emit_arrayptr(Value *t)
{
    Value* addr = builder.CreateStructGEP(
#ifdef LLVM37
                                          nullptr,
#endif
                                          builder.CreateBitCast(t,jl_parray_llvmt),
                                          0); //index (not offset) of data field in jl_parray_llvmt

    return tbaa_decorate(tbaa_arrayptr, builder.CreateLoad(addr, false));
}

static Value *emit_arrayptr(Value *t, jl_value_t *ex, jl_codectx_t *ctx)
{
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av!=NULL)
        return builder.CreateLoad(av->dataptr);
    return emit_arrayptr(t);
}

static Value *emit_arraysize(Value *t, jl_value_t *ex, int dim, jl_codectx_t *ctx)
{
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av != NULL && dim <= (int)av->sizes.size())
        return builder.CreateLoad(av->sizes[dim-1]);
    return emit_arraysize(t, dim);
}

static Value *emit_arrayflags(Value *t, jl_codectx_t *ctx)
{
    Value *addr = builder.CreateStructGEP(
#ifdef LLVM37
                            nullptr,
#endif
                            builder.CreateBitCast(t,jl_parray_llvmt), 2);
    return builder.CreateLoad(addr); // TODO tbaa
}

static void assign_arrayvar(jl_arrayvar_t &av, Value *ar)
{
    tbaa_decorate(tbaa_arrayptr,builder.CreateStore(builder.CreateBitCast(emit_arrayptr(ar),
                                                    av.dataptr->getType()->getContainedType(0)),
                                                    av.dataptr));
    builder.CreateStore(emit_arraylen_prim(ar, av.ty), av.len);
    for(size_t i=0; i < av.sizes.size(); i++)
        builder.CreateStore(emit_arraysize(ar,i+1), av.sizes[i]);
}

static Value *emit_array_nd_index(Value *a, jl_value_t *ex, size_t nd, jl_value_t **args,
                                  size_t nidxs, jl_codectx_t *ctx)
{
    Value *i = ConstantInt::get(T_size, 0);
    Value *stride = ConstantInt::get(T_size, 1);
#if CHECK_BOUNDS==1
    bool bc = ((ctx->boundsCheck.empty() || ctx->boundsCheck.back()==true) &&
               jl_options.check_bounds != JL_OPTIONS_CHECK_BOUNDS_OFF) ||
        jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_ON;
    BasicBlock *failBB=NULL, *endBB=NULL;
    if (bc) {
        failBB = BasicBlock::Create(getGlobalContext(), "oob");
        endBB = BasicBlock::Create(getGlobalContext(), "idxend");
    }
#endif
    Value **idxs = (Value**)alloca(sizeof(Value*)*nidxs);
    for(size_t k=0; k < nidxs; k++) {
        idxs[k] = emit_unbox(T_size, emit_unboxed(args[k], ctx), NULL);
    }
    for(size_t k=0; k < nidxs; k++) {
        Value *ii = builder.CreateSub(idxs[k], ConstantInt::get(T_size, 1));
        i = builder.CreateAdd(i, builder.CreateMul(ii, stride));
        if (k < nidxs-1) {
            Value *d =
                k >= nd ? ConstantInt::get(T_size, 1) : emit_arraysize(a, ex, k+1, ctx);
#if CHECK_BOUNDS==1
            if (bc) {
                BasicBlock *okBB = BasicBlock::Create(getGlobalContext(), "ib");
                // if !(i < d) goto error
                builder.CreateCondBr(builder.CreateICmpULT(ii, d), okBB, failBB);
                ctx->f->getBasicBlockList().push_back(okBB);
                builder.SetInsertPoint(okBB);
            }
#endif
            stride = builder.CreateMul(stride, d);
        }
    }
#if CHECK_BOUNDS==1
    if (bc) {
        Value *alen = emit_arraylen(a, ex, ctx);
        // if !(i < alen) goto error
        builder.CreateCondBr(builder.CreateICmpULT(i, alen), endBB, failBB);

        ctx->f->getBasicBlockList().push_back(failBB);
        builder.SetInsertPoint(failBB);
        // CreateAlloca is OK here since we are on an error branch
        Value *tmp = builder.CreateAlloca(T_size, ConstantInt::get(T_size, nidxs));
        for(size_t k=0; k < nidxs; k++) {
            builder.CreateStore(idxs[k], builder.CreateGEP(tmp, ConstantInt::get(T_size, k)));
        }
#ifdef LLVM37
        builder.CreateCall(prepare_call(jlboundserrorv_func), { a, tmp, ConstantInt::get(T_size, nidxs) });
#else
        builder.CreateCall3(prepare_call(jlboundserrorv_func), a, tmp, ConstantInt::get(T_size, nidxs));
#endif
        builder.CreateUnreachable();

        ctx->f->getBasicBlockList().push_back(endBB);
        builder.SetInsertPoint(endBB);
    }
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
    builder.CreateStore(jt, builder.CreateBitCast(emit_typeptr_addr(newv), jl_ppvalue_llvmt));
    builder.CreateAlignedStore(v, builder.CreateBitCast(data_pointer(newv), PointerType::get(t,0)), 16); // Julia's gc-alignment is 16-bytes
    return newv;
}

// allocate a box where the type might not be known at compile time
static Value *allocate_box_dynamic(Value *jlty, Value *nb, Value *v)
{
    // TODO: allocate on the stack if !envescapes
    if (v->getType()->isPointerTy()) {
        v = builder.CreatePtrToInt(v, T_size);
    }
    Value *newv = builder.CreateCall(prepare_call(jlallocobj_func), nb);
    // TODO: make sure this is rooted. I think it is.
    return init_bits_value(newv, jlty, v->getType(), v);
}

static jl_value_t *static_void_instance(jl_value_t *jt)
{
    assert(jl_is_datatype(jt));
    jl_datatype_t *jb = (jl_datatype_t*)jt;
    if (jb->instance == NULL)
        // if we can't get an instance then this was an UndefValue due
        // to throwing an error.
        return (jl_value_t*)jl_nothing;
    //assert(jb->instance != NULL);
    return (jl_value_t*)jb->instance;
}

static jl_value_t *static_constant_instance(Constant *constant, jl_value_t *jt)
{
    assert(constant != NULL);

    ConstantInt *cint = dyn_cast<ConstantInt>(constant);
    if (cint != NULL) {
        assert(jl_is_datatype(jt));
        return jl_new_bits(jt,
            const_cast<uint64_t *>(cint->getValue().getRawData()));
    }

    ConstantFP *cfp = dyn_cast<ConstantFP>(constant);
    if (cfp != NULL) {
        assert(jl_is_datatype(jt));
        return jl_new_bits(jt,
            const_cast<uint64_t *>(cfp->getValueAPF().bitcastToAPInt().getRawData()));
    }

    ConstantPointerNull *cpn = dyn_cast<ConstantPointerNull>(constant);
    if (cpn != NULL) {
        assert(jl_is_cpointer_type(jt));
        uint64_t val = 0;
        return jl_new_bits(jt,&val);
    }

    // issue #8464
    ConstantExpr *ce = dyn_cast<ConstantExpr>(constant);
    if (ce != NULL) {
        if (ce->isCast()) {
            return static_constant_instance(dyn_cast<Constant>(ce->getOperand(0)), jt);
        }
    }

    assert(jl_is_tuple_type(jt));

    size_t nargs = 0;
    ConstantStruct *cst = NULL;
    ConstantVector *cvec = NULL;
    ConstantArray *carr = NULL;
    if ((cst = dyn_cast<ConstantStruct>(constant)) != NULL)
        nargs = cst->getType()->getNumElements();
    else if ((cvec = dyn_cast<ConstantVector>(constant)) != NULL)
        nargs = cvec->getType()->getNumElements();
    else if ((carr = dyn_cast<ConstantArray>(constant)) != NULL)
        nargs = carr->getType()->getNumElements();
    else if (isa<Function>(constant))
        return NULL;
    else
        assert(false && "Cannot process this type of constant");

    jl_value_t **tupleargs;
    JL_GC_PUSHARGS(tupleargs, nargs);
    for(size_t i=0; i < nargs; i++) {
        tupleargs[i] = static_constant_instance(constant->getAggregateElement(i), jl_tparam(jt,i));
    }
    jl_value_t *tpl = jl_f_tuple(NULL, tupleargs, nargs);
    JL_GC_POP();
    return tpl;
}

static Value *call_with_signed(Function *sfunc, Value *v)
{
    CallInst *Call = builder.CreateCall(prepare_call(sfunc), v);
    Call->addAttribute(1, Attribute::SExt);
    return Call;
}

static Value *call_with_unsigned(Function *ufunc, Value *v)
{
    CallInst *Call = builder.CreateCall(prepare_call(ufunc), v);
    Call->addAttribute(1, Attribute::ZExt);
    return Call;
}

// this is used to wrap values for generic contexts, where a
// dynamically-typed value is required (e.g. argument to unknown function).
// if it's already a pointer it's left alone.
static Value *boxed(Value *v, jl_codectx_t *ctx, jl_value_t *jt)
{
    Type *t = (v == NULL) ? NULL : v->getType();

    if (jt == NULL) {
        jt = julia_type_of(v);
    }
    else if (jt != jl_bottom_type && !jl_is_leaf_type(jt)) {
        // we can get a sharper type from julia_type_of than expr_type in some
        // cases, due to ccall's compile-time evaluations of types. see issue #5752
        jl_value_t *jt2 = julia_type_of(v);
        if (jl_subtype(jt2, jt, 0))
            jt = jt2;
    }
    if (jt == jl_bottom_type)
        return UndefValue::get(jl_pvalue_llvmt);
    UndefValue *uv = NULL;
    if (v == NULL || (uv = dyn_cast<UndefValue>(v)) != 0 || t == NoopType) {
        if (uv != NULL && jl_is_datatype(jt)) {
            jl_datatype_t *jb = (jl_datatype_t*)jt;
            // We have an undef value on a hopefully dead branch
            if (jl_isbits(jb) && jb->size != 0)
                return UndefValue::get(jl_pvalue_llvmt);
        }
        jl_value_t *s = static_void_instance(jt);
        return literal_pointer_val(s);
    }
    if (t == jl_pvalue_llvmt)
        return v;
    if (t == T_int1) return julia_bool(v);
    if (t == T_void || t->isEmptyTy()) {
        jl_value_t *s = static_void_instance(jt);
        return literal_pointer_val(s);
    }
    Constant *c = NULL;
    if ((c = dyn_cast<Constant>(v)) != NULL) {
        jl_value_t *s = static_constant_instance(c,jt);
        if (s) {
            jl_add_linfo_root(ctx->linfo, s);
            return literal_pointer_val(s);
        }
    }

    jl_datatype_t *jb = (jl_datatype_t*)jt;
    assert(jl_is_datatype(jb));
    if (jb == jl_int8_type)  return call_with_signed(box_int8_func, v);
    if (jb == jl_int16_type) return call_with_signed(box_int16_func, v);
    if (jb == jl_int32_type) return call_with_signed(box_int32_func, v);
    if (jb == jl_int64_type) return call_with_signed(box_int64_func, v);
    if (jb == jl_float32_type) return builder.CreateCall(prepare_call(box_float32_func), v);
    //if (jb == jl_float64_type) return builder.CreateCall(box_float64_func, v);
    if (jb == jl_float64_type) {
        // manually inline alloc & init of Float64 box. cheap, I know.
#ifdef _P64
        Value *newv = builder.CreateCall(prepare_call(jlalloc1w_func)
#ifdef LLVM37
            , {}
#endif
        );
#else
        Value *newv = builder.CreateCall(prepare_call(jlalloc2w_func)
#ifdef LLVM37
            , {}
#endif
        );
#endif
        return init_bits_value(newv, literal_pointer_val(jt), t, v);
    }
    if (jb == jl_uint8_type)  return call_with_unsigned(box_uint8_func, v);
    if (jb == jl_uint16_type) return call_with_unsigned(box_uint16_func, v);
    if (jb == jl_uint32_type) return call_with_unsigned(box_uint32_func, v);
    if (jb == jl_uint64_type) return call_with_unsigned(box_uint64_func, v);
    if (jb == jl_char_type)   return call_with_unsigned(box_char_func, v);
    if (jb == jl_gensym_type) {
        unsigned zero = 0;
        if (v->getType()->isPointerTy()) {
            v = builder.CreateLoad(v);
        }
        v = builder.CreateExtractValue(v, ArrayRef<unsigned>(&zero,1));
        return call_with_unsigned(box_gensym_func, v);
    }

    if (!jl_isbits(jt) || !jl_is_leaf_type(jt)) {
        assert("Don't know how to box this type" && false);
        return NULL;
    }

    if (!jb->abstract && jb->size == 0) {
        assert(jb->instance != NULL);
        return literal_pointer_val(jb->instance);
    }

    Type *llvmt = julia_type_to_llvm(jt);
    if (llvmt->isAggregateType() && v->getType()->isPointerTy()) {
        v = builder.CreateLoad(v);
    }
    return allocate_box_dynamic(literal_pointer_val(jt), ConstantInt::get(T_size, jl_datatype_size(jt)), v);
}

static void emit_cpointercheck(Value *x, const std::string &msg,
                               jl_codectx_t *ctx)
{
    Value *t = emit_typeof(x);
    emit_typecheck(t, (jl_value_t*)jl_datatype_type, msg, ctx);

    Value *istype =
        builder.CreateICmpEQ(emit_nthptr(t, (ssize_t)(offsetof(jl_datatype_t,name)/sizeof(char*)), tbaa_datatype),
                             literal_pointer_val((jl_value_t*)jl_pointer_type->name));
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    emit_type_error(x, (jl_value_t*)jl_pointer_type, msg, ctx);
    builder.CreateUnreachable();

    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

// allocation for known size object
static Value* emit_allocobj(size_t static_size)
{
    if (static_size == sizeof(void*)*1)

        return builder.CreateCall(prepare_call(jlalloc1w_func)
#ifdef LLVM37
            , {}
#endif
            );
    else if (static_size == sizeof(void*)*2)
        return builder.CreateCall(prepare_call(jlalloc2w_func)
#ifdef LLVM37
            , {}
#endif
            );
    else if (static_size == sizeof(void*)*3)
        return builder.CreateCall(prepare_call(jlalloc3w_func)
#ifdef LLVM37
            , {}
#endif
            );
    else
        return builder.CreateCall(prepare_call(jlallocobj_func),
                                  ConstantInt::get(T_size, static_size));
}

// if ptr is NULL this emits a write barrier _back_
static void emit_write_barrier(jl_codectx_t* ctx, Value *parent, Value *ptr)
{
    Value* parenttag = builder.CreateBitCast(emit_typeptr_addr(parent), T_psize);
    Value* parent_type = builder.CreateLoad(parenttag);
    Value* parent_mark_bits = builder.CreateAnd(parent_type, 1);

    // the branch hint does not seem to make it to the generated code
    //builder.CreateCall(expect_func, {parent_marked, ConstantInt::get(T_int1, 0)});
    Value* parent_marked = builder.CreateICmpEQ(parent_mark_bits, ConstantInt::get(T_size, 1));

    BasicBlock* cont = BasicBlock::Create(getGlobalContext(), "cont");
    BasicBlock* barrier_may_trigger = BasicBlock::Create(getGlobalContext(), "wb_may_trigger", ctx->f);
    BasicBlock* barrier_trigger = BasicBlock::Create(getGlobalContext(), "wb_trigger", ctx->f);
    builder.CreateCondBr(parent_marked, barrier_may_trigger, cont);

    builder.SetInsertPoint(barrier_may_trigger);
    Value* ptr_mark_bit = builder.CreateAnd(builder.CreateLoad(builder.CreateBitCast(emit_typeptr_addr(ptr), T_psize)), 1);
    Value* ptr_not_marked = builder.CreateICmpEQ(ptr_mark_bit, ConstantInt::get(T_size, 0));
    builder.CreateCondBr(ptr_not_marked, barrier_trigger, cont);
    builder.SetInsertPoint(barrier_trigger);
    builder.CreateCall(prepare_call(queuerootfun), builder.CreateBitCast(parent, jl_pvalue_llvmt));
    builder.CreateBr(cont);
    ctx->f->getBasicBlockList().push_back(cont);
    builder.SetInsertPoint(cont);
}

static void emit_checked_write_barrier(jl_codectx_t *ctx, Value *parent, Value *ptr)
{
    BasicBlock *cont;
    Value *not_null = builder.CreateICmpNE(ptr, V_null);
    BasicBlock *if_not_null = BasicBlock::Create(getGlobalContext(), "wb_not_null", ctx->f);
    cont = BasicBlock::Create(getGlobalContext(), "cont");
    builder.CreateCondBr(not_null, if_not_null, cont);
    builder.SetInsertPoint(if_not_null);
    emit_write_barrier(ctx, parent, ptr);
    builder.CreateBr(cont);
    ctx->f->getBasicBlockList().push_back(cont);
    builder.SetInsertPoint(cont);
}

static Value *emit_setfield(jl_datatype_t *sty, Value *strct, size_t idx0,
                            Value *rhs, jl_codectx_t *ctx, bool checked, bool wb)
{
    if (sty->mutabl || !checked) {
        Value *addr =
            builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                              ConstantInt::get(T_size, jl_field_offset(sty,idx0)));
        jl_value_t *jfty = jl_svecref(sty->types, idx0);
        if (sty->fields[idx0].isptr) {
            rhs = boxed(rhs, ctx);
            builder.CreateStore(rhs,
                                builder.CreateBitCast(addr, jl_ppvalue_llvmt));
            if (wb) emit_checked_write_barrier(ctx, strct, rhs);
        }
        else {
            int align = jl_field_offset(sty,idx0);
            if (align & 1) align = 1;
            else if (align & 2) align = 2;
            else if (align & 4) align = 4;
            else if (align & 8) align = 8;
            else align = 16;
            typed_store(addr, ConstantInt::get(T_size, 0), rhs, jfty, ctx, sty->mutabl ? tbaa_user : tbaa_immut, strct, align);
        }
    }
    else {
        // TODO: better error
        emit_error("type is immutable", ctx);
    }
    return strct;
}

static Value *emit_new_struct(jl_value_t *ty, size_t nargs, jl_value_t **args, jl_codectx_t *ctx)
{
    assert(jl_is_datatype(ty));
    assert(jl_is_leaf_type(ty));
    assert(nargs>0);
    jl_datatype_t *sty = (jl_datatype_t*)ty;
    size_t nf = jl_datatype_nfields(sty);
    if (nf > 0) {
        if (jl_isbits(sty)) {
            Type *lt = julia_type_to_llvm(ty);
            size_t na = nargs-1 < nf ? nargs-1 : nf;
            Value *strct = UndefValue::get(lt == T_void ? NoopType : lt);
            unsigned idx = 0;
            for (size_t i=0; i < na; i++) {
                jl_value_t *jtype = jl_svecref(sty->types,i);
                Type *fty = julia_type_to_llvm(jtype);
                Value *fval = emit_unboxed(args[i+1], ctx);
                if (!jl_subtype(expr_type(args[i+1],ctx), jtype, 0))
                    emit_typecheck(fval, jtype, "new", ctx);
                if (!type_is_ghost(fty)) {
                    fval = emit_unbox(fty, fval, jtype);
                    if (fty == T_int1)
                        fval = builder.CreateZExt(fval, T_int8);
                    if (lt->isVectorTy())
                        strct = builder.CreateInsertElement(strct, fval, ConstantInt::get(T_int32,idx));
                    else
                        strct = builder.CreateInsertValue(strct, fval, ArrayRef<unsigned>(&idx,1));
                }
                idx++;
            }
            return mark_julia_type(emit_reg2mem(strct, ctx), ty);
        }
        Value *f1 = NULL;
        int fieldStart = ctx->gc.argDepth;
        bool needroots = false;
        for (size_t i = 1;i < nargs;i++) {
            if (might_need_root(args[i])) {
                needroots = true;
                break;
            }
        }
        size_t j = 0;
        if (nf > 0 && sty->fields[0].isptr && nargs>1) {
            // emit first field before allocating struct to save
            // a couple store instructions. avoids initializing
            // the first field to NULL, and sometimes the GC root
            // for the new struct.
            Value *fval = emit_expr(args[1],ctx);
            f1 = boxed(fval,ctx);
            j++;
            if (might_need_root(args[1]) || fval->getType() != jl_pvalue_llvmt)
                make_gcroot(f1, ctx);
        }
        Value *strct = emit_allocobj(sty->size);
        builder.CreateStore(literal_pointer_val((jl_value_t*)ty),
                            emit_typeptr_addr(strct));
        if (f1) {
            if (!jl_subtype(expr_type(args[1],ctx), jl_field_type(sty,0), 0))
                emit_typecheck(f1, jl_field_type(sty,0), "new", ctx);
            emit_setfield(sty, strct, 0, f1, ctx, false, false);
            ctx->gc.argDepth = fieldStart;
            if (nf > 1 && needroots)
                make_gcroot(strct, ctx);
        }
        else if (nf > 0 && needroots) {
            make_gcroot(strct, ctx);
        }
        for(size_t i=j; i < nf; i++) {
            if (sty->fields[i].isptr) {
                emit_setfield(sty, strct, i, V_null, ctx, false, false);
            }
        }
        bool need_wb = false;
        for(size_t i=j+1; i < nargs; i++) {
            Value *rhs = emit_expr(args[i],ctx);
            if (sty->fields[i-1].isptr && rhs->getType() != jl_pvalue_llvmt) {
                if (!needroots) {
                    // if this struct element needs boxing and we haven't rooted
                    // the struct, root it now.
                    make_gcroot(strct, ctx);
                    needroots = true;
                }
                need_wb = true;
            }
            if (rhs->getType() == jl_pvalue_llvmt) {
                if (!jl_subtype(expr_type(args[i],ctx), jl_svecref(sty->types,i-1), 0))
                    emit_typecheck(rhs, jl_svecref(sty->types,i-1), "new", ctx);
            }
            if (!need_wb && might_need_root(args[i]))
                need_wb = true;
            emit_setfield(sty, strct, i-1, rhs, ctx, false, need_wb);
        }
        ctx->gc.argDepth = fieldStart;
        return strct;
    }
    else {
        // 0 fields, singleton
        return literal_pointer_val(jl_new_struct_uninit((jl_datatype_t*)ty));
    }
}
