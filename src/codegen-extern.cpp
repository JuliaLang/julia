// This file is a part of Julia. License is MIT: http://julialang.org/license

// this file contains most of the external entry points to codegen

extern "C" void jl_compile_linfo(jl_lambda_info_t *li)
{
    if (!jl_compile_linfo_nothrow(li))
        // rethrow from a separate function to try to keep the C++ state valid
        jl_rethrow_with_add("error compiling %s", jl_symbol_name(li->def ? li->def->name : anonymous_sym));
}

// this ensures that llvmf has been emitted to the execution engine,
// returning the function pointer to it
extern void jl_callback_triggered_linfos(void);
static uint64_t getAddressForFunction(llvm::Function *llvmf)
{
    JL_TIMING(LLVM_EMIT);
#ifdef JL_DEBUG_BUILD
    llvm::raw_fd_ostream out(1,false);
#endif
#ifdef USE_MCJIT
    jl_finalize_function(llvmf, NULL);
    uint64_t ret = jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
    // delay executing trace callbacks until here to make sure there's no
    // recursive compilation.
    jl_callback_triggered_linfos();
    return ret;
#else
    return (uint64_t)jl_ExecutionEngine->getPointerToFunction(
            cast<Function>(shadow_output->getNamedValue(llvmf->getName())));
#endif
}

extern "C" JL_DLLEXPORT
uint64_t jl_get_llvm_fptr(llvm::Function *llvmf)
{
    uint64_t addr = getAddressForFunction(llvmf);
#ifdef USE_ORCJIT
    if (!addr)
        addr = jl_ExecutionEngine->findUnmangledSymbol(llvmf->getName()).getAddress();
#endif
    return addr;
}

// this assumes that jl_compile_linfo has already been called
// and forces compilation of the lambda info
extern "C" void jl_generate_fptr(jl_lambda_info_t *li)
{
    if (li->jlcall_api == 2) return;
    JL_LOCK(&codegen_lock);
    // objective: assign li->fptr
    assert(li->functionObjectsDecls.functionObject);
    assert(!li->inCompile);
    if (li->fptr == NULL) {
        li->fptr = (jl_fptr_t)getAddressForFunction((Function*)li->functionObjectsDecls.functionObject);
        assert(li->fptr != NULL);
    }
    JL_UNLOCK(&codegen_lock); // Might GC
}

static Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt);
// get the address of a C-callable entry point for a function
extern "C" JL_DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_GC_PUSH1(&argt);
    if (jl_is_tuple(argt)) {
        // TODO: maybe deprecation warning, better checking
        argt = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argt), jl_nfields(argt));
    }
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    JL_GC_POP();
    return (void*)getAddressForFunction(llvmf);
}


// convenience function for debugging from gdb (pre-OrcJIT)
// it generally helps to have define KEEP_BODIES if you plan on using this
extern "C" JL_DLLEXPORT
void *jl_function_ptr_by_llvm_name(char *name) {
#ifdef JL_MSAN_ENABLED
    __msan_unpoison_string(name);
#endif
    return (void*)(intptr_t)jl_ExecutionEngine->FindFunctionNamed(name);
}

// export a C-callable entry point for a function (dllexport'ed dlsym), with a given name
extern "C" JL_DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    assert(jl_is_tuple_type(argt));
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    if (llvmf) {
        // force eager emission of the function (llvm 3.3 gets confused otherwise and tries to do recursive compilation)
        uint64_t Addr = getAddressForFunction(llvmf);

#if defined(USE_ORCJIT) || defined(USE_MCJIT)
        if (imaging_mode)
             // in the old JIT, the shadow_module aliases the engine_module,
             // otherwise, just point the alias to the declaration
#endif
            llvmf = cast<Function>(shadow_output->getNamedValue(llvmf->getName()));

        // make the alias to the shadow_module
        GlobalAlias *GA =
#if defined(LLVM38)
            GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#elif defined(LLVM37)
            GlobalAlias::create(cast<PointerType>(llvmf->getType()),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#else
            new GlobalAlias(llvmf->getType(), GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#endif

#if defined(USE_ORCJIT) || defined(USE_MCJIT)
        // make the alias name is valid for the current session
        jl_ExecutionEngine->addGlobalMapping(GA, (void*)(uintptr_t)Addr);
#else
        (void)GA; (void)Addr;
#endif
    }
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir and jl_dump_function_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT jl_lambda_info_t *jl_get_specialized(jl_method_t *m, jl_tupletype_t *types, jl_svec_t *sp);

extern "C" JL_DLLEXPORT
void *jl_get_llvmf_defn(jl_lambda_info_t *linfo, bool getwrapper)
{
    if (linfo->def && linfo->def->lambda_template->code == jl_nothing) {
        // not a generic function
        return NULL;
    }

    jl_lambda_info_t *temp = NULL;
    JL_GC_PUSH1(&temp);
    if (linfo->code == jl_nothing && linfo->def) {
        // re-infer if we've deleted the code
        // first copy the linfo to avoid corrupting it and
        // confusing the compiler about the
        // validity of the code it already generated
        temp = jl_get_specialized(linfo->def, linfo->specTypes, linfo->sparam_vals);
        jl_type_infer(temp, 0);
        if (temp->code == jl_nothing || temp->inInference) {
            // something went wrong: abort!
            JL_GC_POP();
            return NULL;
        }
    }

    // Backup the info for the nested compile
    JL_LOCK(&codegen_lock);
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    // emit this function into a new module
    jl_llvm_functions_t declarations;
    std::unique_ptr<Module> m;
    JL_TRY {
         m = emit_function(temp ? temp : linfo, &declarations);
    }
    JL_CATCH {
        // something failed!
        nested_compile = last_n_c;
        if (old != NULL) {
            builder.SetInsertPoint(old);
            builder.SetCurrentDebugLocation(olddl);
        }
        JL_UNLOCK(&codegen_lock); // Might GC
        jl_rethrow_with_add("error compiling %s", jl_symbol_name(linfo->def ? linfo->def->name : anonymous_sym));
    }
    // Restore the previous compile context
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    nested_compile = last_n_c;

    jl_globalPM->run(*m.get());
    Function *f = (llvm::Function*)declarations.functionObject;
    Function *specf = (llvm::Function*)declarations.specFunctionObject;
    // swap declarations for definitions and destroy declarations
    if (specf) {
        Function *tempf = cast<Function>(m->getNamedValue(specf->getName()));
        delete specf;
        specf = tempf;
    }
    if (f) {
        Function *tempf = cast<Function>(m->getNamedValue(f->getName()));
        delete f;
        f = tempf;
    }
    // clone the name from the runtime linfo, if it exists
    // to give the user a (false) sense of stability
    Function *specf_decl = (Function*)linfo->functionObjectsDecls.specFunctionObject;
    if (specf_decl) {
        specf->setName(specf_decl->getName());
    }
    Function *f_decl = (Function*)linfo->functionObjectsDecls.functionObject;
    if (f_decl) {
        f->setName(f_decl->getName());
    }
    m.release(); // the return object `llvmf` will be the owning pointer
    JL_UNLOCK(&codegen_lock); // Might GC
    JL_GC_POP();
    if (getwrapper || !specf)
        return f;
    else
        return specf;
}


extern "C" JL_DLLEXPORT
void *jl_get_llvmf_decl(jl_lambda_info_t *linfo, bool getwrapper)
{
    if (linfo->def && linfo->def->lambda_template->code == jl_nothing) {
        // not a generic function
        return NULL;
    }

    // compile this normally
    linfo = jl_compile_for_dispatch(linfo);

    if (linfo->jlcall_api == 2 && linfo->def) {
        // normally we don't generate native code for these functions, so need an exception here
        // This leaks a bit of memory to cache native code that we'll never actually need
        if (linfo->functionObjectsDecls.functionObject == NULL) {
            jl_lambda_info_t *temp = NULL;
            JL_GC_PUSH1(&temp);
            temp = jl_get_specialized(linfo->def, linfo->specTypes, linfo->sparam_vals);
            jl_type_infer(temp, 0);
            temp->jlcall_api = 0;
            temp->constval = jl_nothing;
            if (temp->code == jl_nothing || temp->inInference) {
                JL_GC_POP();
                return NULL;
            }
            jl_compile_linfo(temp);
            linfo->functionObjectsDecls = temp->functionObjectsDecls;
            JL_GC_POP();
        }
        jl_set_lambda_code_null(linfo);
    }

    if (getwrapper || !linfo->functionObjectsDecls.specFunctionObject)
        return linfo->functionObjectsDecls.functionObject;
    else
        return linfo->functionObjectsDecls.specFunctionObject;
}


extern "C" JL_DLLEXPORT
void *jl_get_llvmf(jl_tupletype_t *tt, bool getwrapper, bool getdeclarations)
{ // DEPRECATED
    jl_lambda_info_t *linfo = NULL, *temp = NULL;
    JL_GC_PUSH3(&linfo, &temp, &tt);
    if (tt != NULL) {
        linfo = jl_get_specialization1(tt);
        if (linfo == NULL) {
            linfo = jl_method_lookup_by_type(
                ((jl_datatype_t*)jl_tparam0(tt))->name->mt, tt, 0, 0, 1);
            if (linfo == NULL || jl_has_call_ambiguities(tt, linfo->def)) {
                JL_GC_POP();
                return NULL;
            }
        }
    }
    if (linfo == NULL) {
        // no function found for argument tuple type
        JL_GC_POP();
        return NULL;
    }
    void *f;
    if (getdeclarations)
        f = jl_get_llvmf_decl(linfo, getwrapper);
    else
        f = jl_get_llvmf_defn(linfo, getwrapper);
    JL_GC_POP();
    return f;
}


// print an llvm IR acquired from jl_get_llvmf
// warning: this takes ownership of, and destroys, f->getParent()
extern "C" JL_DLLEXPORT
const jl_value_t *jl_dump_function_ir(void *f, bool strip_ir_metadata, bool dump_module)
{
    std::string code;
    llvm::raw_string_ostream stream(code);

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf || (!llvmf->isDeclaration() && !llvmf->getParent()))
        jl_error("jl_dump_function_ir: Expected Function* in a temporary Module");

    JL_LOCK(&codegen_lock); // Might GC
    if (!llvmf->getParent()) {
        // print the function declaration as-is
        llvmf->print(stream);
    }
    else {
        Module *m = llvmf->getParent();
        if (strip_ir_metadata) {
            // strip metadata from all instructions in the module
            for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
                Function *f2 = &*I;
                Function::BasicBlockListType::iterator f2_bb = f2->getBasicBlockList().begin();
                // iterate over all basic blocks in the function
                for (; f2_bb != f2->getBasicBlockList().end(); ++f2_bb) {
                    BasicBlock::InstListType::iterator f2_il = (*f2_bb).getInstList().begin();
                    // iterate over instructions in basic block
                    for (; f2_il != (*f2_bb).getInstList().end(); ) {
                        Instruction *inst = &*f2_il++;
                        // remove dbg.declare and dbg.value calls
                        if (isa<DbgDeclareInst>(inst) || isa<DbgValueInst>(inst)) {
                            inst->eraseFromParent();
                            continue;
                        }

                        SmallVector<std::pair<unsigned, MDNode*>, 4> MDForInst;
                        inst->getAllMetadata(MDForInst);
                        SmallVector<std::pair<unsigned, MDNode*>, 4>::iterator md_iter = MDForInst.begin();

                        // iterate over all metadata kinds and set to NULL to remove
                        for (; md_iter != MDForInst.end(); ++md_iter) {
                            inst->setMetadata((*md_iter).first, NULL);
                        }
                    }
                }
            }
        }
        if (dump_module) {
            m->print(stream, NULL);
        }
        else {
            llvmf->print(stream);
        }
        delete m;
    }
    JL_UNLOCK(&codegen_lock); // Might GC

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// This isn't particularly fast, but it's only used for interactive mode
static uint64_t compute_obj_symsize(const object::ObjectFile *obj, uint64_t offset)
{
    // Scan the object file for the closest symbols above and below offset in the .text section
    uint64_t lo = 0;
    uint64_t hi = 0;
    bool setlo = false;
#ifdef LLVM37
    for (const object::SectionRef &Section : obj->sections()) {
#else
    llvm::error_code err;
    for (object::section_iterator I = obj->begin_sections(), E = obj->end_sections();
            !err && I != E; I.increment(err)) {
        object::SectionRef Section = *I;
#endif
        uint64_t SAddr, SSize;
#ifdef LLVM35
        if (!Section.isText()) continue;
#else
        bool isText;
        if (Section.isText(isText) || !isText) continue;
#endif
#ifdef LLVM36
        SAddr = Section.getAddress();
        SSize = Section.getSize();
#else
        Section.getAddress(SAddr);
        Section.getSize(SSize);
#endif
        if (offset < SAddr || offset >= SAddr + SSize) continue;
        assert(hi == 0);

        // test for lower and upper symbol bounds relative to other symbols
        hi = SAddr + SSize;
#ifdef LLVM37
        object::section_iterator ESection = obj->section_end();
        for (const object::SymbolRef &Sym : obj->symbols()) {
#else
        llvm::error_code err;
        object::section_iterator ESection = obj->end_sections();
        for (object::symbol_iterator I = obj->begin_symbols(), E = obj->end_symbols();
                !err && I != E; I.increment(err)) {
            object::SymbolRef Sym = *I;
#endif
            uint64_t Addr;
            object::section_iterator Sect = ESection;
#ifdef LLVM38
            auto SectOrError = Sym.getSection();
            assert(SectOrError);
            Sect = SectOrError.get();
#else
            if (Sym.getSection(Sect)) continue;
#endif
            if (Sect == ESection) continue;
            if (Sect != Section) continue;
#ifdef LLVM37
            auto AddrOrError = Sym.getAddress();
            assert(AddrOrError);
            Addr = AddrOrError.get();
#else
            if (Sym.getAddress(Addr)) continue;
#endif
            if (Addr <= offset && Addr >= lo) {
                // test for lower bound on symbol
                lo = Addr;
                setlo = true;
            }
            if (Addr > offset && Addr < hi) {
                // test for upper bound on symbol
                hi = Addr;
            }
        }
    }
    if (setlo)
        return hi - lo;
    return 0;
}

// print a native disassembly for f (an LLVM function)
extern "C" JL_DLLEXPORT
const jl_value_t *jl_dump_function_asm(void *f, int raw_mc)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    std::string code;
    llvm::raw_string_ostream stream(code);
#ifndef LLVM37
    llvm::formatted_raw_ostream fstream(stream);
#endif

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_asm: Expected Function*");

    // Dump assembly code
    uint64_t symsize = 0;
    int64_t slide = 0, section_slide = 0;
    uint64_t fptr = getAddressForFunction(llvmf);
#ifdef USE_MCJIT
    // Look in the system image as well
    if (fptr == 0)
        fptr = (uintptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(llvmf);
    llvm::DIContext *context = NULL;
    llvm::DIContext *&objcontext = context;
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> context;
    llvm::DIContext *objcontext = NULL;
#endif
    const object::ObjectFile *object = NULL;
    assert(fptr != 0);
    if (!jl_DI_for_fptr(fptr, &symsize, &slide, &section_slide, &object, &context)) {
        if (!jl_dylib_DI_for_fptr(fptr, &object, &objcontext, &slide, &section_slide, false,
            NULL, NULL, NULL, NULL)) {
                jl_printf(JL_STDERR, "WARNING: Unable to find function pointer\n");
                return jl_cstr_to_string("");
        }
    }
    if (symsize == 0 && object != NULL)
        symsize = compute_obj_symsize(object, fptr + slide + section_slide);
    if (symsize == 0) {
        jl_printf(JL_STDERR, "WARNING: Could not determine size of symbol\n");
        return jl_cstr_to_string("");
    }

    if (raw_mc) {
        return (jl_value_t*)jl_pchar_to_array((char*)fptr, symsize);
    }

    int8_t gc_state = jl_gc_safe_enter(ptls);
    jl_dump_asm_internal(fptr, symsize, slide,
#ifndef USE_MCJIT
            context,
#endif
            object, objcontext,
#ifdef LLVM37
            stream
#else
            fstream
#endif
            );

#ifndef LLVM37
    fstream.flush();
#endif
    jl_gc_safe_leave(ptls, gc_state);

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}


extern "C" JL_DLLEXPORT void jl_clear_malloc_data(void)
{
    jl_codectx_t::clear_malloc_data();
}

extern "C" void jl_write_coverage_data(void)
{
    jl_codectx_t::write_coverage_data();
}

extern "C" void jl_write_malloc_log(void)
{
    jl_codectx_t::write_malloc_log();
}


extern "C" JL_DLLEXPORT
uint32_t jl_get_LLVM_VERSION(void)
{
    return 10000 * LLVM_VERSION_MAJOR + 100 * LLVM_VERSION_MINOR
#ifdef LLVM_VERSION_PATCH
        + LLVM_VERSION_PATCH
#endif
        ;
}

// for debugging from gdb
extern "C" void jl_dump_llvm_value(void *v)
{
    ((Value*)v)->dump();
}
extern "C" void jl_dump_llvm_type(void *v)
{
    ((Type*)v)->dump(); putchar('\n');
}
