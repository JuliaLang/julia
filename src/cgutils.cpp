// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <iostream>

// utility procedures used in code generation

template<class T> // for GlobalObject's
static T* addComdat(T *G)
{
#if defined(_OS_WINDOWS_)
    if (imaging_mode && !G->isDeclaration()) {
#ifdef LLVM35
        // Add comdat information to make MSVC link.exe happy
        Comdat *jl_Comdat = G->getParent()->getOrInsertComdat(G->getName());
        jl_Comdat->setSelectionKind(Comdat::NoDuplicates);
        G->setComdat(jl_Comdat);
        // add __declspec(dllexport) to everything marked for export
        if (G->getLinkage() == GlobalValue::ExternalLinkage)
            G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
#endif
    }
#endif
    return G;
}

static Instruction *tbaa_decorate(MDNode* md, Instruction* load_or_store)
{
    load_or_store->setMetadata( llvm::LLVMContext::MD_tbaa, md );
    return load_or_store;
}

// Fixing up references to other modules for MCJIT
std::set<llvm::GlobalValue*> pending_globals;
static GlobalVariable *prepare_global(GlobalVariable *G)
{
    pending_globals.insert(G);
    return G;
}

static llvm::Value *prepare_call(llvm::Value* Callee)
{
    llvm::Function *F = dyn_cast<Function>(Callee);
    if (!F)
        return Callee;
    pending_globals.insert(F);
    return Callee;
}

#if defined(USE_MCJIT) || defined(USE_ORCJIT)
static GlobalValue *realize_pending_global(Instruction *User, GlobalValue *G, std::map<llvm::Module*,llvm::GlobalValue*> &FixedGlobals)
{
    Function *UsedInHere = User->getParent()->getParent();
    assert(UsedInHere);
    Module *M = UsedInHere->getParent();
    if (M == G->getParent() && M != builtins_module) { // can happen during bootstrap
        //std::cout << "Skipping " << std::string(G->getName()) << " due to parentage" << std::endl;
        return nullptr;
    }
    // If we come across a function that is still being constructed,
    // this use needs to remain pending
    if (!M || M == builtins_module) {
        pending_globals.insert(G);
        //std::cout << "Skipping " << std::string(G->getName()) << " due to construction" << std::endl;
        return nullptr;
    }
    if (!FixedGlobals.count(M)) {
        if (GlobalVariable *GV = dyn_cast<GlobalVariable>(G)) {
            GlobalVariable *NewGV = M->getGlobalVariable(GV->getName());
            if (!NewGV) {
                NewGV = new GlobalVariable(*M, GV->getType()->getElementType(),
                                        GV->isConstant(), GlobalVariable::ExternalLinkage,
                                        NULL, GV->getName(), NULL, GV->getThreadLocalMode(),
                                        GV->getType()->getAddressSpace());
                NewGV->setUnnamedAddr(GV->hasUnnamedAddr());
                // Move over initializer
                if (GV->hasInitializer()) {
                    NewGV->setInitializer(GV->getInitializer());
                    GV->setInitializer(nullptr);
                }
            }
            FixedGlobals[M] = NewGV;
        } else {
            Function *F = cast<Function>(G);
            //std::cout << "Realizing " << std::string(F->getName()) << std::endl;
            //if (!F->getParent()) {
                //std::cout << "Skipping" << std::endl;
            //    return nullptr;
            //}
            Function *NewF = nullptr;
            if (!F->isDeclaration() && F->getParent() == builtins_module) {
                // It's a definition. Actually move the function and create a
                // declaration in the original module
                NewF = F;
                F->removeFromParent();
                M->getFunctionList().push_back(F);
                Function::Create(F->getFunctionType(),
                    Function::ExternalLinkage,
                    F->getName(),
                    active_module);
            } else {
                assert(F);
                NewF = M->getFunction(F->getName());
                if (!NewF) {
                    NewF = Function::Create(F->getFunctionType(),
                                Function::ExternalLinkage,
                                F->getName(),
                                M);
                }
            }
            FixedGlobals[M] = NewF;
        }
    }
    return FixedGlobals[M];
}

struct ExprChain {
    ConstantExpr *Expr;
    unsigned OpNo;
    struct ExprChain *Next;
};

static void handleUse(Use &Use1,llvm::GlobalValue *G,std::map<llvm::Module*,llvm::GlobalValue*> &FixedGlobals,struct ExprChain *Chain, struct ExprChain *ChainEnd)
{
    Instruction *User = dyn_cast<Instruction>(Use1.getUser());
    if (!User) {
        ConstantExpr *Expr = cast<ConstantExpr>(Use1.getUser());
        Value::use_iterator UI2 = Expr->use_begin(), E2 = Expr->use_end();
        for (; UI2 != E2;) {
            Use &Use2 = *UI2;
            ++UI2;
            struct ExprChain NextChain;
            NextChain.Expr = Expr;
            NextChain.OpNo = Use1.getOperandNo();
            NextChain.Next = nullptr;
            if (ChainEnd)
               ChainEnd->Next = &NextChain;
            handleUse(Use2,G,FixedGlobals,Chain ? Chain : &NextChain,&NextChain);
        }
        return;
    }
    llvm::Constant *Replacement = realize_pending_global(User,G,FixedGlobals);
    if (!Replacement)
        return;
    while (Chain) {
        Replacement = Chain->Expr->getWithOperandReplaced(Chain->OpNo,Replacement);
        Chain->Expr = cast<ConstantExpr>(Replacement);
        Chain = Chain->Next;
    }
    Use1.set(Replacement);
}

// RAUW, but only for those users which live in a module, and create a module
// specific copy
static void realize_pending_globals()
{
    std::set<llvm::GlobalValue *> local_pending_globals;
    std::swap(local_pending_globals,pending_globals);
    std::map<llvm::Module*,llvm::GlobalValue*> FixedGlobals;
    for (auto *G : local_pending_globals) {
        Value::use_iterator UI = G->use_begin(), E = G->use_end();
        for (; UI != E;)
            handleUse(*(UI++),G,FixedGlobals,nullptr,nullptr);
        FixedGlobals.clear();
    }
}

static void realize_cycle(jl_cyclectx_t *cyclectx)
{
    // These need to be resolved together
    for (auto *F : cyclectx->functions) {
        F->removeFromParent();
        active_module->getFunctionList().push_back(F);
    }
    for (auto *CU : cyclectx->CUs) {
        NamedMDNode *NMD = active_module->getOrInsertNamedMetadata("llvm.dbg.cu");
        NMD->addOperand(CU);
    }
}
#endif

#ifdef LLVM35
static inline void add_named_global(GlobalObject *gv, void *addr, bool dllimport = true)
#else
static inline void add_named_global(GlobalValue *gv, void *addr, bool dllimport = true)
#endif
{
#ifdef LLVM34
    StringRef name = gv->getName();
#ifdef _OS_WINDOWS_
    std::string imp_name;
#endif
#endif

#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
#ifdef LLVM35
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
        // this will cause llvm to rename it, so we do the same
        imp_name = Twine("__imp_", name).str();
        name = StringRef(imp_name);
#else
        gv->setLinkage(GlobalValue::DLLImportLinkage);
#endif
#if defined(_P64) || defined(LLVM35)
        // __imp_ variables are indirection pointers, so use malloc to simulate that
        void** imp_addr = (void**)malloc(sizeof(void**));
        *imp_addr = addr;
        addr = (void*)imp_addr;
#endif
    }
#endif // _OS_WINDOWS_

#ifdef USE_ORCJIT
    addComdat(gv);
    jl_ExecutionEngine->addGlobalMapping(name, addr);
#elif defined(USE_MCJIT)
    addComdat(gv);
    sys::DynamicLibrary::AddSymbol(name, addr);
#else // USE_MCJIT
    jl_ExecutionEngine->addGlobalMapping(gv, addr);
#endif // USE_MCJIT
}

// --- string constants ---
static std::map<const std::string, GlobalVariable*> stringConstants;

extern "C" {
    extern int jl_in_inference;
}

#if defined(USE_MCJIT) || defined(USE_ORCJIT)
static GlobalVariable *global_proto(GlobalVariable *G) {
    GlobalVariable *proto = new GlobalVariable(G->getType()->getElementType(),
            G->isConstant(), GlobalVariable::ExternalLinkage,
            NULL, G->getName(), G->getThreadLocalMode());
    return proto;
}
#else
static GlobalVariable *global_proto(GlobalVariable *G) {
    return G;
}
#endif

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
        gv = new GlobalVariable(*active_module,
                                ArrayType::get(T_int8, txt.length()+1),
                                true,
                                imaging_mode ? GlobalVariable::PrivateLinkage : GlobalVariable::ExternalLinkage,
                                ConstantDataArray::get(getGlobalContext(),
                                                       ArrayRef<unsigned char>(
                                                       (const unsigned char*)txt.c_str(),
                                                       txt.length()+1)),
                                vname);
        gv->setUnnamedAddr(true);
        gv = imaging_mode ? gv : prepare_global(global_proto(gv));
        stringConstants[txt] = gv;
        strno++;
    } else {
        prepare_global(gv);
    }
    return gv;

}

// --- Shadow module handling ---

typedef struct {Value* gv; int32_t index;} jl_value_llvm; // uses 1-based indexing
static std::map<void*, jl_value_llvm> jl_value_to_llvm;
JL_DLLEXPORT std::map<Value *, void*> jl_llvm_to_jl_value;

// In imaging mode, cache a fast mapping of Function * to code address
// because this is queried in the hot path
static std::map<Function *, uint64_t> emitted_function_symtab;

#if defined(USE_MCJIT) || defined(USE_ORCJIT)
static Function *function_proto(Function *F) {
    Function *NewF = Function::Create(F->getFunctionType(),
                            Function::ExternalLinkage,
                            F->getName());
    NewF->setAttributes(AttributeSet());

    // FunctionType does not include any attributes. Copy them over manually
    // as codegen may make decisions based on the presence of certain attributes
    NewF->copyAttributesFrom(F);

#ifdef LLVM37
    // Declarations are not allowed to have personality routines, but
    // copyAttributesFrom sets them anyway, so clear them again manually
    NewF->setPersonalityFn(nullptr);
#endif

    return NewF;
}

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
        assert(!F->isDeclaration());
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
            VMap[&*I] = &*(DestI++);        // Add mapping to VMap
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

    Value *InjectFunctionProto(Function *F)
    {
        Function *NewF = destModule->getFunction(F->getName());
        if (!NewF) {
            NewF = function_proto(F);
            destModule->getFunctionList().push_back(NewF);
        }
        return NewF;
    }

#ifdef LLVM38
    virtual Value *materializeDeclFor(Value *V)
#else
    virtual Value *materializeValueFor (Value *V)
#endif
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
                    if (emitted_function_symtab.find(shadow) != emitted_function_symtab.end())
                        return InjectFunctionProto(F);
                    uint64_t addr = jl_mcjmm->getSymbolAddress(F->getName());
                    if (addr) {
                        emitted_function_symtab[shadow] = addr;
                        return InjectFunctionProto(F);
                    }

                    Function *oldF = destModule->getFunction(F->getName());
                    if (oldF)
                        return oldF;

#ifndef USE_ORCJIT
                    // Also check if this function is pending in any other module
                    if (jl_ExecutionEngine->FindFunctionNamed(F->getName().data()))
                        return InjectFunctionProto(F);
#endif

                    return CloneFunctionProto(shadow);
                }
                else if (!F->isDeclaration()) {
                    return CloneFunctionProto(F);
                }
            }
            // Still a declaration and still in a different module
            if (F->isDeclaration() && F->getParent() != destModule) {
                // Create forward declaration in current module
                return InjectFunctionProto(F);
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
#else
static Function *function_proto(Function *F) {
    return F;
}
#endif

#ifdef LLVM37
static DIType *julia_type_to_di(jl_value_t *jt, DIBuilder *dbuilder, bool isboxed = false)
#else
static DIType julia_type_to_di(jl_value_t *jt, DIBuilder *dbuilder, bool isboxed = false)
#endif
{
    if (isboxed)
        return jl_pvalue_dillvmt;
    if (jl_is_abstracttype(jt) || jl_is_uniontype(jt) || jl_is_array_type(jt))
        return jl_pvalue_dillvmt;
    if (jl_is_typector(jt) || jl_is_typevar(jt))
        return jl_pvalue_dillvmt;
    assert(jl_is_datatype(jt));
    jl_datatype_t *jdt = (jl_datatype_t*)jt;
    if (jdt->ditype != NULL) {
#ifdef LLVM37
        return (llvm::DIType*)jdt->ditype;
#else
        return DIType((llvm::MDNode*)jdt->ditype);
#endif
    }
    if (jl_is_bitstype(jt)) {
        uint64_t SizeInBits = jdt == jl_bool_type ? 1 : 8*jdt->size;
    #ifdef LLVM37
        llvm::DIType *t = dbuilder->createBasicType(jl_symbol_name(jdt->name->name),SizeInBits,8*jdt->alignment,llvm::dwarf::DW_ATE_unsigned);
        jdt->ditype = t;
        return t;
    #else
        DIType t = dbuilder->createBasicType(jl_symbol_name(jdt->name->name),SizeInBits,8*jdt->alignment,llvm::dwarf::DW_ATE_unsigned);
        MDNode *M = t;
        jdt->ditype = M;
        return t;
    #endif
    }
    #ifdef LLVM37
    else if (jl_is_tuple_type(jt) || jl_is_structtype(jt)) {
        jl_datatype_t *jst = (jl_datatype_t*)jt;
        size_t ntypes = jl_datatype_nfields(jst);
        llvm::DICompositeType *ct = dbuilder->createStructType(
            NULL,                       // Scope
            jl_symbol_name(jdt->name->name),      // Name
            NULL,                       // File
            0,                          // LineNumber
            8*jdt->size,                // SizeInBits
            8*jdt->alignment,           // AlignmentInBits
            0,                          // Flags
            NULL,                       // DerivedFrom
            DINodeArray(),              // Elements
            dwarf::DW_LANG_Julia        // RuntimeLanguage
            );
        jdt->ditype = ct;
        std::vector<llvm::Metadata*> Elements;
        for(unsigned i = 0; i < ntypes; i++)
            Elements.push_back(julia_type_to_di(jl_svecref(jst->types,i),dbuilder,false));
        dbuilder->replaceArrays(ct, dbuilder->getOrCreateArray(ArrayRef<Metadata*>(Elements)));
        return ct;
    } else {
        jdt->ditype = dbuilder->createTypedef(jl_pvalue_dillvmt, jl_symbol_name(jdt->name->name), NULL, 0, NULL);
        return (llvm::DIType*)jdt->ditype;
    }
    #endif
    // TODO: Fixme
    return jl_pvalue_dillvmt;
}

// --- emitting pointers directly into code ---

static Value *literal_static_pointer_val(const void *p, Type *t)
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
static std::vector<Constant*> jl_sysimg_fvars;

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
    ArrayType *gvars_type = ArrayType::get(T_psize, jl_sysimg_gvars.size());
    addComdat(new GlobalVariable(*mod,
                                 gvars_type,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 MapValue(ConstantArray::get(gvars_type, ArrayRef<Constant*>(jl_sysimg_gvars)), VMap),
                                 "jl_sysimg_gvars"));
    ArrayType *fvars_type = ArrayType::get(T_pvoidfunc, jl_sysimg_fvars.size());
    addComdat(new GlobalVariable(*mod,
                                 fvars_type,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 MapValue(ConstantArray::get(fvars_type, ArrayRef<Constant*>(jl_sysimg_fvars)), VMap),
                                 "jl_sysimg_fvars"));
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size,globalUnique+1),
                                 "jl_globalUnique"));
#ifdef JULIA_ENABLE_THREADING
    addComdat(new GlobalVariable(*mod,
                                 T_size,
                                 true,
                                 GlobalVariable::ExternalLinkage,
                                 ConstantInt::get(T_size, jltls_states_func_idx),
                                 "jl_ptls_states_getter_idx"));
#endif

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
#if defined(USE_MCJIT) || defined(USE_ORCJIT)
    realize_pending_globals();
#endif
#ifdef JL_DEBUG_BUILD
    verifyModule(*shadow_module);
#endif

#ifdef LLVM36
    std::error_code err;
    StringRef fname_ref = StringRef(fname);
    raw_fd_ostream OS(fname_ref, err, sys::fs::F_None);
#elif defined(LLVM35)
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

#ifdef LLVM38
    legacy::PassManager PM;
#else
    PassManager PM;
#endif
#ifndef LLVM37
    PM.add(new TargetLibraryInfo(Triple(TM->getTargetTriple())));
#else
    PM.add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
#endif
#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif defined(LLVM36)
    PM.add(new DataLayoutPass());
#elif defined(LLVM35)
    PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
    PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
#endif

    addOptimizationPasses(&PM);
    if (!dump_as_bc) {
        if (TM->addPassesToEmitFile(PM, FOS, TargetMachine::CGFT_ObjectFile, false)) {
            jl_error("Could not generate obj file for this target");
        }
    }

    // now copy the module, since PM.run may modify it
    ValueToValueMapTy VMap;
#ifdef LLVM38
    Module *clone = CloneModule(shadow_module, VMap).release();
#else
    Module *clone = CloneModule(shadow_module, VMap);
#endif
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
    PM.run(*clone);
    if (dump_as_bc)
        WriteBitcodeToFile(clone, FOS);
    delete clone;
}

static int32_t jl_assign_functionID(Function *functionObject, int specsig)
{
    // give the function an index in the constant lookup table
    if (!imaging_mode)
        return 0;
    jl_sysimg_fvars.push_back(ConstantExpr::getBitCast(functionObject, T_pvoidfunc));
    return jl_sysimg_fvars.size();
}

static Value *julia_gv(const char *cname, void *addr)
{
    // emit a GlobalVariable for a jl_value_t named "cname"
    std::map<void*, jl_value_llvm>::iterator it;
    // first see if there already is a GlobalVariable for this address
    it = jl_value_to_llvm.find(addr);
    if (it != jl_value_to_llvm.end())
        return builder.CreateLoad(prepare_global((llvm::GlobalVariable*)it->second.gv));

    std::stringstream gvname;
    gvname << cname << globalUnique++;
    // no existing GlobalVariable, create one and store it
    GlobalVariable *gv = new GlobalVariable(imaging_mode ? *shadow_module : *builtins_module, T_pjlvalue,
                           false, imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                           ConstantPointerNull::get((PointerType*)T_pjlvalue), gvname.str());
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
    gv_struct.gv = prepare_global(gv);
    gv_struct.index = jl_sysimg_gvars.size();
    jl_value_to_llvm[addr] = gv_struct;
    return builder.CreateLoad(gv);
}

static Value *julia_gv(const char *prefix, jl_sym_t *name, jl_module_t *mod, void *addr)
{
    // emit a GlobalVariable for a jl_value_t, using the prefix, name, and module to
    // to create a readable name of the form prefixModA.ModB.name
    size_t len = strlen(jl_symbol_name(name))+strlen(prefix)+1;
    jl_module_t *parent = mod, *prev = NULL;
    while (parent != NULL && parent != prev) {
        len += strlen(jl_symbol_name(parent->name))+1;
        prev = parent;
        parent = parent->parent;
    }
    char *fullname = (char*)alloca(len);
    strcpy(fullname, prefix);
    len -= strlen(jl_symbol_name(name))+1;
    strcpy(fullname + len, jl_symbol_name(name));
    parent = mod;
    prev = NULL;
    while (parent != NULL && parent != prev) {
        size_t part = strlen(jl_symbol_name(parent->name))+1;
        strcpy(fullname+len-part,jl_symbol_name(parent->name));
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
        return ConstantPointerNull::get((PointerType*)T_pjlvalue);
    // some common constant values
    if (p == jl_false)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var)));
    if (p == jl_true)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jltrue_var)));
    if (p == (jl_value_t*)jl_emptysvec)
        return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlemptysvec_var)));
    if (!imaging_mode)
        return literal_static_pointer_val(p, T_pjlvalue);
    if (jl_is_datatype(p)) {
        jl_datatype_t *addr = (jl_datatype_t*)p;
        // DataTypes are prefixed with a +
        return julia_gv("+", addr->name->name, addr->name->module, p);
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
        return ConstantPointerNull::get((PointerType*)T_pjlvalue);
    if (!imaging_mode)
        return literal_static_pointer_val(p, T_pjlvalue);
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
        builder.CreateBitCast(julia_gv("*", b->name, b->owner, b), T_ppjlvalue) :
        literal_static_pointer_val(b,T_ppjlvalue);
    return julia_binding_gv(bv);
}

// --- mapping between julia and llvm types ---

static Type *julia_struct_to_llvm(jl_value_t *jt, bool *isboxed);

extern "C" {
JL_DLLEXPORT Type *julia_type_to_llvm(jl_value_t *jt, bool *isboxed)
{
    // this function converts a Julia Type into the equivalent LLVM type
    if (isboxed) *isboxed = false;
    if (jt == (jl_value_t*)jl_bool_type) return T_int1;
    if (jt == (jl_value_t*)jl_bottom_type) return T_void;
    if (!jl_is_leaf_type(jt)) {
        if (isboxed) *isboxed = true;
        return T_pjlvalue;
    }
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
                return T_float16;
            else
#endif
            if (nb == 4)
                return T_float32;
            else if (nb == 8)
                return T_float64;
            else if (nb == 16)
                return T_float128;
        }
        return Type::getIntNTy(jl_LLVMContext, nb*8);
    }
    if (jl_isbits(jt)) {
        if (((jl_datatype_t*)jt)->size == 0) {
            return T_void;
        }
        return julia_struct_to_llvm(jt, isboxed);
    }
    if (isboxed) *isboxed = true;
    return T_pjlvalue;
}
}

static Type *julia_struct_to_llvm(jl_value_t *jt, bool *isboxed)
{
    // this function converts a Julia Type into the equivalent LLVM struct
    // use this where C-compatible (unboxed) structs are desired
    // use julia_type_to_llvm directly when you want to preserve Julia's type semantics
    bool isTuple = jl_is_tuple_type(jt);
    if (isboxed) *isboxed = false;
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
                structdecl = StructType::create(jl_LLVMContext, jl_symbol_name(jst->name->name));
                jst->struct_decl = structdecl;
            }
            std::vector<Type*> latypes(0);
            size_t i;
            bool isvector = true;
            Type *lasttype = NULL;
            for(i = 0; i < ntypes; i++) {
                jl_value_t *ty = jl_svecref(jst->types, i);
                Type *lty;
                if (jl_field_isptr(jst, i))
                    lty = T_pjlvalue;
                else
                    lty = ty==(jl_value_t*)jl_bool_type ? T_int8 : julia_type_to_llvm(ty);
                if (lasttype != NULL && lasttype != lty)
                    isvector = false;
                lasttype = lty;
                if (type_is_ghost(lty))
                    lty = NoopType;
                latypes.push_back(lty);
            }
            if (!isTuple) {
                structdecl->setBody(latypes);
            }
            else {
                if (isvector && lasttype != T_int1 && !type_is_ghost(lasttype)) {
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
    return julia_type_to_llvm(jt, isboxed);
}

static bool is_datatype_all_pointers(jl_datatype_t *dt)
{
    size_t i, l = jl_datatype_nfields(dt);
    for(i=0; i < l; i++) {
        if (!jl_field_isptr(dt, i)) {
            return false;
        }
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

static bool deserves_sret(jl_value_t *dt, Type *T)
{
    assert(jl_is_datatype(dt));
    return (size_t)jl_datatype_size(dt) > sizeof(void*) && !T->isFloatingPointTy();
}

// --- generating various field accessors ---

static Value *emit_nthptr_addr(Value *v, ssize_t n)
{
    return builder.CreateGEP(builder.CreateBitCast(v, T_ppjlvalue),
                             ConstantInt::get(T_size, (ssize_t)n));
}

static Value *emit_nthptr_addr(Value *v, Value *idx)
{
    return builder.CreateGEP(builder.CreateBitCast(v, T_ppjlvalue), idx);
}

static Value *emit_nthptr(Value *v, ssize_t n, MDNode *tbaa)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, n);
    return tbaa_decorate(tbaa,builder.CreateLoad(vptr, false));
}

static Value *emit_nthptr_recast(Value *v, Value *idx, MDNode *tbaa, Type *ptype)
{
    // p = (jl_value_t**)v; *(ptype)&p[n]
    Value *vptr = emit_nthptr_addr(v, idx);
    return tbaa_decorate(tbaa,builder.CreateLoad(builder.CreateBitCast(vptr,ptype), false));
}

static Value *emit_typeptr_addr(Value *p)
{
    ssize_t offset = (sizeof(jl_taggedvalue_t) -
                      offsetof(jl_taggedvalue_t, type)) / sizeof(jl_value_t*);
    return emit_nthptr_addr(p, -offset);
}

static Value *emit_typeof(Value *tt)
{
    // given p, a jl_value_t*, compute its type tag
    assert(tt->getType() == T_pjlvalue);
    tt = builder.CreateLoad(emit_typeptr_addr(tt), false);
    tt = builder.CreateIntToPtr(builder.CreateAnd(
                builder.CreatePtrToInt(tt, T_size),
                ConstantInt::get(T_size,~(uptrint_t)15)),
            T_pjlvalue);
    return tt;
}
static Value *emit_typeof(const jl_cgval_t &p)
{
    // given p, compute its type
    if (p.isboxed && !jl_is_leaf_type(p.typ)) {
        return emit_typeof(p.V);
    }
    jl_value_t *aty = p.typ;
    if (jl_is_type_type(aty)) // convert Int::Type{Int} ==> typeof(Int) ==> DataType
                              // but convert 1::Type{1} ==> typeof(1) ==> Int
        aty = (jl_value_t*)jl_typeof(jl_tparam0(aty));
    return literal_pointer_val(aty);
}

static Value *emit_datatype_types(Value *dt)
{
    return builder.
        CreateLoad(builder.
                   CreateBitCast(builder.
                                 CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                           ConstantInt::get(T_size, offsetof(jl_datatype_t, types))),
                                 T_ppjlvalue));
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

static Value *emit_datatype_size(Value *dt)
{
    Value *size = builder.
        CreateLoad(builder.
                   CreateBitCast(builder.
                                 CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                           ConstantInt::get(T_size, offsetof(jl_datatype_t, size))),
                                 T_pint32));
    return size;
}

static Value *emit_datatype_mutabl(Value *dt)
{
    Value *mutabl = builder.
        CreateLoad(builder.CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                     ConstantInt::get(T_size, offsetof(jl_datatype_t, mutabl))));
    return builder.CreateTrunc(mutabl, T_int1);
}

static Value *emit_datatype_abstract(Value *dt)
{
    Value *abstract = builder.
        CreateLoad(builder.CreateGEP(builder.CreateBitCast(dt, T_pint8),
                                     ConstantInt::get(T_size, offsetof(jl_datatype_t, abstract))));
    return builder.CreateTrunc(abstract, T_int1);
}

static Value *emit_datatype_isbitstype(Value *dt)
{
    Value *immut = builder.CreateXor(emit_datatype_mutabl(dt), ConstantInt::get(T_int1, -1));
    Value *nofields = builder.CreateICmpEQ(emit_datatype_nfields(dt), ConstantInt::get(T_size, 0));
    Value *isbitstype = builder.CreateAnd(immut, builder.CreateAnd(nofields,
            builder.CreateXor(builder.CreateAnd(emit_datatype_abstract(dt),
                    builder.CreateICmpSGT(emit_datatype_size(dt), ConstantInt::get(T_int32, 0))),
                ConstantInt::get(T_int1, -1))));
    return isbitstype;
}


// --- generating various error checks ---

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
    builder.CreateCall(prepare_call(jlthrow_func), { exc });
#else
    builder.CreateCall(prepare_call(jlthrow_func), exc);
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

static Value *boxed(const jl_cgval_t &v, jl_codectx_t *ctx, jl_value_t *jt=NULL);

static void emit_type_error(const jl_cgval_t &x, jl_value_t *type, const std::string &msg,
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
                        literal_pointer_val(type), boxed(x,ctx)});
#else
    builder.CreateCall4(prepare_call(jltypeerror_func),
                    fname_val, msg_val,
                    literal_pointer_val(type), boxed(x,ctx));
#endif
}

static void emit_typecheck(const jl_cgval_t &x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype;
    if (jl_is_type_type(type) || !jl_is_leaf_type(type)) {
        Value *vx = boxed(x, ctx, type);
        istype = builder.
            CreateICmpNE(
#ifdef LLVM37
                builder.CreateCall(prepare_call(jlsubtype_func), { vx, literal_pointer_val(type),
                                             ConstantInt::get(T_int32,1) }),
#else
                builder.CreateCall3(prepare_call(jlsubtype_func), vx, literal_pointer_val(type),
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

static bool is_inbounds(jl_codectx_t *ctx)
{
    // inbounds rule is either of top two values on inbounds stack are true
    bool inbounds = !ctx->inbounds.empty() && ctx->inbounds.back();
    if (ctx->inbounds.size() > 1)
        inbounds |= ctx->inbounds[ctx->inbounds.size()-2];
    return inbounds;
}

static bool is_bounds_check_block(jl_codectx_t *ctx)
{
    return !ctx->boundsCheck.empty() && ctx->boundsCheck.back();
}

#define CHECK_BOUNDS 1
static Value *emit_bounds_check(const jl_cgval_t &ainfo, jl_value_t *ty, Value *i, Value *len, jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_size, 1));
#if CHECK_BOUNDS==1
    if ((!is_inbounds(ctx) &&
         jl_options.check_bounds != JL_OPTIONS_CHECK_BOUNDS_OFF) ||
         jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_ON) {
        Value *ok = builder.CreateICmpULT(im1, len);
        BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
        BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
        builder.CreateCondBr(ok, passBB, failBB);
        builder.SetInsertPoint(failBB);
        if (!ty) { // jl_value_t** tuple (e.g. the vararg)
#ifdef LLVM37
            builder.CreateCall(prepare_call(jlvboundserror_func), { ainfo.V, len, i });
#else
            builder.CreateCall3(prepare_call(jlvboundserror_func), ainfo.V, len, i);
#endif
        }
        else if (ainfo.isboxed) { // jl_datatype_t or boxed jl_value_t
#ifdef LLVM37
            builder.CreateCall(prepare_call(jlboundserror_func), { ainfo.V, i });
#else
            builder.CreateCall2(prepare_call(jlboundserror_func), ainfo.V, i);
#endif
        }
        else { // unboxed jl_value_t*
            Value *a = ainfo.V;
            if (ainfo.isghost) {
                a = Constant::getNullValue(T_pint8);
            }
            else if (!ainfo.ispointer) {
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
        builder.CreateUnreachable();
        ctx->f->getBasicBlockList().push_back(passBB);
        builder.SetInsertPoint(passBB);
    }
#endif
    return im1;
}

// --- loading and storing ---

static Value *emit_unbox(Type *to, const jl_cgval_t &x, jl_value_t *jt);

static jl_cgval_t typed_load(Value *ptr, Value *idx_0based, jl_value_t *jltype,
                         jl_codectx_t *ctx, MDNode* tbaa, size_t alignment = 0)
{
    bool isboxed;
    Type *elty = julia_type_to_llvm(jltype, &isboxed);
    assert(elty != NULL);
    if (type_is_ghost(elty))
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
        if (isboxed) {
            null_pointer_check(elt, ctx);
        }
    //}
    if (isbool)
        return mark_julia_type(builder.CreateTrunc(elt, T_int1), false, jltype);
    return mark_julia_type(elt, isboxed, jltype);
}

static void typed_store(Value *ptr, Value *idx_0based, const jl_cgval_t &rhs,
                        jl_value_t *jltype, jl_codectx_t *ctx, MDNode* tbaa,
                        Value* parent,  // for the write barrier, NULL if no barrier needed
                        size_t alignment = 0)
{
    Type *elty = julia_type_to_llvm(jltype);
    assert(elty != NULL);
    if (type_is_ghost(elty))
        return;
    if (elty == T_int1) {
        elty = T_int8;
    }
    Value *r;
    if (jl_isbits(jltype) && ((jl_datatype_t*)jltype)->size > 0) {
        r = emit_unbox(elty, rhs, jltype);
    }
    else {
        r = boxed(rhs, ctx, jltype);
        if (parent != NULL) emit_write_barrier(ctx, parent, r);
    }
    Value *data;
    if (ptr->getType()->getContainedType(0) != elty)
        data = builder.CreateBitCast(ptr, PointerType::get(elty, 0));
    else
        data = ptr;
    if (data->getType()->getContainedType(0)->isVectorTy() && !alignment)
        alignment = ((jl_datatype_t*)jltype)->alignment; // prevent llvm from assuming 32 byte alignment of vectors
    Instruction *store = builder.CreateAlignedStore(r, builder.CreateGEP(data, idx_0based), alignment);
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
    if (jl_is_expr(e)) {
        jl_value_t *typ = ((jl_expr_t*)e)->etype;
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
        return typ;
    }
    if (jl_is_symbolnode(e)) {
        jl_value_t *typ = jl_symbolnode_type(e);
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
        return typ;
    }
    if (jl_is_gensym(e)) {
        int idx = ((jl_gensym_t*)e)->id;
        jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
        return (jl_is_array(gensym_types) ? jl_cellref(gensym_types, idx) : (jl_value_t*)jl_any_type);
    }
    if (jl_is_quotenode(e)) {
        e = jl_fieldref(e,0);
        goto type_of_constant;
    }
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
                jl_svec_t *sp = ctx->linfo->sparam_syms;
                for (size_t i=0; i < jl_svec_len(sp); i++) {
                    assert(jl_is_symbol(jl_svecref(sp, i)));
                    if (e == jl_svecref(sp, i)) {
                        if (jl_svec_len(ctx->linfo->sparam_vals) == 0)
                            return (jl_value_t*)jl_any_type;
                        e = jl_svecref(ctx->linfo->sparam_vals, i);
                        goto type_of_constant;
                    }
                }
            }
            else {
                std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find((jl_sym_t*)e);
                if (it != ctx->vars.end())
                    return (*it).second.value.typ;
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

static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val);

static Value *data_pointer(Value *x)
{
    return builder.CreateBitCast(x, T_ppjlvalue);
}

static bool emit_getfield_unknownidx(jl_cgval_t *ret, const jl_cgval_t &strct, Value *idx, jl_datatype_t *stt, jl_codectx_t *ctx)
{
    size_t nfields = jl_datatype_nfields(stt);
    if (strct.ispointer) { // boxed or stack
        if (is_datatype_all_pointers(stt)) {
            idx = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            Value *fld = tbaa_decorate(tbaa_user, builder.CreateLoad(
                        builder.CreateGEP(
                            builder.CreateBitCast(strct.V, T_ppjlvalue),
                            idx)));
            if ((unsigned)stt->ninitialized != nfields)
                null_pointer_check(fld, ctx);
            *ret = mark_julia_type(fld, true, jl_any_type);
            ret->needsgcroot = strct.needsgcroot || !strct.isimmutable;
            return true;
        }
        else if (is_tupletype_homogeneous(stt->types)) {
            assert(nfields > 0); // nf == 0 trapped by all_pointers case
            jl_value_t *jt = jl_field_type(stt, 0);
            idx = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            Value *ptr = data_pointer(strct.V);
            *ret = typed_load(ptr, idx, jt, ctx, stt->mutabl ? tbaa_user : tbaa_immut);
            return true;
        }
        else if (strct.isboxed) {
            idx = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
#ifdef LLVM37
            Value *fld = builder.CreateCall(prepare_call(jlgetnthfieldchecked_func), { strct.V, idx });
#else
            Value *fld = builder.CreateCall2(prepare_call(jlgetnthfieldchecked_func), strct.V, idx);
#endif
            *ret = mark_julia_type(fld, true, jl_any_type);
            return true;
        }
    }
    else if (is_tupletype_homogeneous(stt->types)) {
        assert(jl_isbits(stt));
        if (nfields == 0) {
            idx = emit_bounds_check(ghostValue(stt),
                                    (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
            *ret = jl_cgval_t();
            return true;
        }
        assert(!jl_field_isptr(stt, 0));
        jl_value_t *jt = jl_field_type(stt,0);
        if (!stt->uid) {
            // add root for types not cached
            jl_add_linfo_root(ctx->linfo, (jl_value_t*)stt);
        }
        Value *idx0 = emit_bounds_check(strct, (jl_value_t*)stt, idx, ConstantInt::get(T_size, nfields), ctx);
        if (strct.isghost) {
            *ret = ghostValue(jt);
            return true;
        }
        // llvm::VectorType
        if (sizeof(void*) != sizeof(int))
            idx0 = builder.CreateTrunc(idx0, T_int32); // llvm3.3 requires this, harmless elsewhere
        Value *fld = builder.CreateExtractElement(strct.V, idx0);
        if (jt == (jl_value_t*)jl_bool_type) {
            fld = builder.CreateTrunc(fld, T_int1);
        }
        *ret = mark_julia_type(fld, false, jt);
        return true;
    }
    return false;
}

static jl_cgval_t emit_getfield_knownidx(const jl_cgval_t &strct, unsigned idx, jl_datatype_t *jt, jl_codectx_t *ctx)
{
    jl_value_t *jfty = jl_field_type(jt,idx);
    Type *elty = julia_type_to_llvm(jfty);
    assert(elty != NULL);
    if (jfty == jl_bottom_type) {
        raise_exception_unless(ConstantInt::get(T_int1,0), prepare_global(jlundeferr_var), ctx);
        return jl_cgval_t(); // unreachable
    }
    if (type_is_ghost(elty))
        return ghostValue(jfty);
    Value *fldv = NULL;
    if (strct.isboxed) {
        Value *addr =
            builder.CreateGEP(builder.CreateBitCast(strct.V, T_pint8),
                              ConstantInt::get(T_size, jl_field_offset(jt,idx)));
        MDNode *tbaa = strct.isimmutable ? tbaa_immut : tbaa_user;
        if (jl_field_isptr(jt, idx)) {
            Value *fldv = tbaa_decorate(tbaa, builder.CreateLoad(builder.CreateBitCast(addr, T_ppjlvalue)));
            if (idx >= (unsigned)jt->ninitialized)
                null_pointer_check(fldv, ctx);
            jl_cgval_t ret = mark_julia_type(fldv, true, jfty);
            ret.needsgcroot = strct.needsgcroot || !strct.isimmutable;
            return ret;
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
    else if (strct.ispointer) { // something stack allocated
        Value *addr = builder.CreateConstInBoundsGEP2_32(
            LLVM37_param(cast<PointerType>(strct.V->getType()->getScalarType())->getElementType())
            strct.V, 0, idx);
        assert(!jt->mutabl);
        return typed_load(addr, NULL, jfty, ctx, tbaa_immut);
    }
    else {
        assert(strct.V->getType()->isVectorTy());
        fldv = builder.CreateExtractElement(strct.V, ConstantInt::get(T_int32, idx));
        if (jfty == (jl_value_t*)jl_bool_type) {
            fldv = builder.CreateTrunc(fldv, T_int1);
        }
        assert(!jl_field_isptr(jt, idx));
        return mark_julia_type(fldv, false, jfty);
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

static Value *emit_arraysize(const jl_cgval_t &tinfo, Value *dim)
{
    assert(tinfo.isboxed);
    Value *t = tinfo.V;
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

static Value *emit_arraysize(const jl_cgval_t &tinfo, int dim)
{
    return emit_arraysize(tinfo, ConstantInt::get(T_int32, dim));
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
        fargt.push_back(T_pjlvalue);
        FunctionType *ft = FunctionType::get(T_size, fargt, false);
        Value *alen = jl_Module->getOrInsertFunction("jl_array_len_", ft); // TODO: move to codegen init block
        return builder.CreateCall(prepare_call(alen), t);
    }
#endif
}

static Value *emit_arraylen(const jl_cgval_t &tinfo, jl_value_t *ex, jl_codectx_t *ctx)
{
    assert(tinfo.isboxed);
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av!=NULL)
        return builder.CreateLoad(av->len);
    return emit_arraylen_prim(tinfo.V, expr_type(ex,ctx));
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

static Value *emit_arrayptr(const jl_cgval_t &tinfo, jl_value_t *ex, jl_codectx_t *ctx)
{
    assert(tinfo.isboxed);
    Value *t = tinfo.V;
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av!=NULL)
        return builder.CreateLoad(av->dataptr);
    return emit_arrayptr(t);
}

static Value *emit_arraysize(const jl_cgval_t &tinfo, jl_value_t *ex, int dim, jl_codectx_t *ctx)
{
    jl_arrayvar_t *av = arrayvar_for(ex, ctx);
    if (av != NULL && dim <= (int)av->sizes.size())
        return builder.CreateLoad(av->sizes[dim-1]);
    return emit_arraysize(tinfo, dim);
}

static Value *emit_arrayflags(const jl_cgval_t &tinfo, jl_codectx_t *ctx)
{
    assert(tinfo.isboxed);
    Value *t = tinfo.V;
    Value *addr = builder.CreateStructGEP(
#ifdef LLVM37
                            nullptr,
#endif
                            builder.CreateBitCast(t, jl_parray_llvmt), 2);
    return builder.CreateLoad(addr); // TODO: tbaa
}

static void assign_arrayvar(jl_arrayvar_t &av, const jl_cgval_t &ainfo)
{
    assert(ainfo.isboxed);
    Value *ar = ainfo.V;
    tbaa_decorate(tbaa_arrayptr,builder.CreateStore(builder.CreateBitCast(emit_arrayptr(ar),
                                                    av.dataptr->getType()->getContainedType(0)),
                                                    av.dataptr));
    builder.CreateStore(emit_arraylen_prim(ar, av.ty), av.len);
    for(size_t i=0; i < av.sizes.size(); i++)
        builder.CreateStore(emit_arraysize(ainfo,i+1), av.sizes[i]);
}

static Value *emit_array_nd_index(const jl_cgval_t &ainfo, jl_value_t *ex, size_t nd, jl_value_t **args,
                                  size_t nidxs, jl_codectx_t *ctx)
{
    assert(ainfo.isboxed);
    Value *a = ainfo.V;
    Value *i = ConstantInt::get(T_size, 0);
    Value *stride = ConstantInt::get(T_size, 1);
#if CHECK_BOUNDS==1
    bool bc = (!is_inbounds(ctx) &&
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
                k >= nd ? ConstantInt::get(T_size, 1) : emit_arraysize(ainfo, ex, k+1, ctx);
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
        Value *alen = emit_arraylen(ainfo, ex, ctx);
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

// --- boxing ---

static Value* emit_allocobj(size_t static_size);
static Value *init_bits_value(Value *newv, Value *jt, Value *v)
{
    builder.CreateStore(jt, emit_typeptr_addr(newv));
    builder.CreateAlignedStore(v, builder.CreateBitCast(newv, PointerType::get(v->getType(),0)), sizeof(void*)); // min alignment in julia's gc is pointer-aligned
    return newv;
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

    if (isa<UndefValue>(constant))
        return NULL;

    ConstantInt *cint = dyn_cast<ConstantInt>(constant);
    if (cint != NULL) {
        assert(jl_is_datatype(jt));
        if (jt == (jl_value_t*)jl_bool_type)
            return cint->isZero() ? jl_false : jl_true;
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

    assert(jl_is_tuple_type(jt));

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
static Value *boxed(const jl_cgval_t &vinfo, jl_codectx_t *ctx, jl_value_t *jt)
{
    if (jt == NULL) {
        jt = vinfo.typ;
    }
    else if (jt != jl_bottom_type && !jl_is_leaf_type(jt)) {
        // we can get a sharper type from julia_type_of than expr_type in some
        // cases, due to ccall's compile-time evaluations of types. see issue #5752
        jl_value_t *jt2 = vinfo.typ;
        if (jt2 && jl_subtype(jt2, jt, 0))
            jt = jt2;
    }
    if (jt == jl_bottom_type || jt == NULL) {
        // We have an undef value on a (hopefully) dead branch
        return UndefValue::get(T_pjlvalue);
    }
    if (vinfo.isghost) {
        jl_value_t *s = static_void_instance(jt);
        assert(s);
        return literal_pointer_val(s);
    }
    Type *t = julia_type_to_llvm(vinfo.typ);
    assert(!type_is_ghost(t)); // should have been handled by isghost above!

    if (vinfo.isboxed)
        return vinfo.V;

    Value *v = vinfo.V;
    if (vinfo.ispointer) {
        v = builder.CreateLoad(builder.CreatePointerCast(v, t->getPointerTo()));
    }
    if (t == T_int1) return julia_bool(v);
    Constant *c = NULL;
    if ((c = dyn_cast<Constant>(v)) != NULL) {
        jl_value_t *s = static_constant_instance(c, jt);
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
    // for Float64, fall through to generic case below, to inline alloc & init of Float64 box. cheap, I know.
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

    return init_bits_value(emit_allocobj(jl_datatype_size(jt)), literal_pointer_val(jt), v);
}

static void emit_cpointercheck(const jl_cgval_t &x, const std::string &msg,
                               jl_codectx_t *ctx)
{
    Value *t = emit_typeof(x);
    emit_typecheck(mark_julia_type(t, true, jl_any_type), (jl_value_t*)jl_datatype_type, msg, ctx);

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
    builder.CreateCall(prepare_call(queuerootfun), builder.CreateBitCast(parent, T_pjlvalue));
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

static void emit_setfield(jl_datatype_t *sty, const jl_cgval_t &strct, size_t idx0,
                            const jl_cgval_t &rhs, jl_codectx_t *ctx, bool checked, bool wb)
{
    if (sty->mutabl || !checked) {
        assert(strct.ispointer);
        Value *addr =
            builder.CreateGEP(builder.CreateBitCast(strct.V, T_pint8),
                              ConstantInt::get(T_size, jl_field_offset(sty,idx0)));
        jl_value_t *jfty = jl_svecref(sty->types, idx0);
        if (jl_field_isptr(sty, idx0)) {
            Value *r = boxed(rhs, ctx);
            builder.CreateStore(r,
                                builder.CreateBitCast(addr, T_ppjlvalue));
            if (wb) emit_checked_write_barrier(ctx, strct.V, r);
        }
        else {
            int align = jl_field_offset(sty, idx0);
            if (align & 1) align = 1;
            else if (align & 2) align = 2;
            else if (align & 4) align = 4;
            else if (align & 8) align = 8;
            else align = 16;
            typed_store(addr, ConstantInt::get(T_size, 0), rhs, jfty, ctx, sty->mutabl ? tbaa_user : tbaa_immut, strct.V, align);
        }
    }
    else {
        // TODO: better error
        emit_error("type is immutable", ctx);
    }
}

static jl_cgval_t emit_new_struct(jl_value_t *ty, size_t nargs, jl_value_t **args, jl_codectx_t *ctx)
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
                jl_cgval_t fval_info = emit_unboxed(args[i+1], ctx);
                if (!jl_subtype(fval_info.typ, jtype, 0))
                    emit_typecheck(fval_info, jtype, "new", ctx);
                if (!type_is_ghost(fty)) {
                    Value *fval = emit_unbox(fty, fval_info, jtype);
                    if (fty == T_int1)
                        fval = builder.CreateZExt(fval, T_int8);
                    if (lt->isVectorTy())
                        strct = builder.CreateInsertElement(strct, fval, ConstantInt::get(T_int32,idx));
                    else
                        strct = builder.CreateInsertValue(strct, fval, ArrayRef<unsigned>(&idx,1));
                }
                idx++;
            }
            return mark_julia_type(strct, false, ty);
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
        if (nf > 0 && jl_field_isptr(sty, 0) && nargs>1) {
            // emit first field before allocating struct to save
            // a couple store instructions. avoids initializing
            // the first field to NULL, and sometimes the GC root
            // for the new struct.
            jl_cgval_t fval_info = emit_expr(args[1],ctx);
            f1 = boxed(fval_info, ctx);
            j++;
            if (might_need_root(args[1]) || !fval_info.isboxed)
                make_gcroot(f1, ctx);
        }
        Value *strct = emit_allocobj(sty->size);
        jl_cgval_t strctinfo = mark_julia_type(strct, true, ty);
        builder.CreateStore(literal_pointer_val((jl_value_t*)ty),
                            emit_typeptr_addr(strct));
        if (f1) {
            jl_cgval_t f1info = mark_julia_type(f1, true, jl_any_type);
            if (!jl_subtype(expr_type(args[1],ctx), jl_field_type(sty,0), 0))
                emit_typecheck(f1info, jl_field_type(sty,0), "new", ctx);
            emit_setfield(sty, strctinfo, 0, f1info, ctx, false, false);
            ctx->gc.argDepth = fieldStart;
            if (nf > 1 && needroots)
                make_gcroot(strct, ctx);
        }
        else if (nf > 0 && needroots) {
            make_gcroot(strct, ctx);
        }
        for(size_t i=j; i < nf; i++) {
            if (jl_field_isptr(sty, i)) {
                builder.CreateStore(
                        V_null,
                        builder.CreatePointerCast(
                            builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                                ConstantInt::get(T_size, jl_field_offset(sty,i))),
                            T_ppjlvalue));
            }
        }
        bool need_wb = false;
        for(size_t i=j+1; i < nargs; i++) {
            jl_cgval_t rhs = emit_expr(args[i],ctx);
            if (jl_field_isptr(sty, i - 1) && !rhs.isboxed) {
                if (!needroots) {
                    // if this struct element needs boxing and we haven't rooted
                    // the struct, root it now.
                    make_gcroot(strct, ctx);
                    needroots = true;
                }
                need_wb = true;
            }
            if (rhs.isboxed) {
                if (!jl_subtype(expr_type(args[i],ctx), jl_svecref(sty->types,i-1), 0))
                    emit_typecheck(rhs, jl_svecref(sty->types,i-1), "new", ctx);
            }
            if (!need_wb && might_need_root(args[i]))
                need_wb = true;
            emit_setfield(sty, strctinfo, i-1, rhs, ctx, false, need_wb);
        }
        ctx->gc.argDepth = fieldStart;
        return strctinfo;
    }
    else if (!sty->mutabl) {
        // 0 fields, ghost or bitstype
        if (sty->size == 0)
            return ghostValue(sty);
        if (nargs >= 2)
            return emit_unboxed(args[1], ctx);  // do side effects
        Type *lt = julia_type_to_llvm(ty);
        assert(lt != T_pjlvalue);
        return mark_julia_type(UndefValue::get(lt), false, ty);
    }
    else {
        // 0 fields, singleton
        assert(sty->instance != NULL);
        return mark_julia_const(sty->instance);
    }
}

static Value *emit_pgcstack(jl_codectx_t *ctx)
{
    Value * addr = emit_nthptr_addr(
        ctx->gc.ptlsStates,
        (ssize_t)(offsetof(jl_tls_states_t, pgcstack) / sizeof(void*)));
    return builder.CreateBitCast(addr, PointerType::get(T_ppjlvalue, 0),
                                 "jl_pgcstack");
}

static Value *emit_exc_in_transit(jl_codectx_t *ctx)
{
    Value * addr = emit_nthptr_addr(
        ctx->gc.ptlsStates,
        (ssize_t)(offsetof(jl_tls_states_t,
                           exception_in_transit) / sizeof(void*)));
    return builder.CreateBitCast(addr, T_ppjlvalue, "jl_exception_in_transit");
}
