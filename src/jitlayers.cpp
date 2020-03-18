// This file is a part of Julia. License is MIT: https://julialang.org/license

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

#include "llvm-version.h"
#include "platform.h"
#include "options.h"

#include <iostream>
#include <sstream>

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/DynamicLibrary.h>

#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/ADT/SmallSet.h>

using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "codegen_shared.h"
#include "jitlayers.h"
#include "julia_assert.h"

RTDyldMemoryManager* createRTDyldMemoryManager(void);

void jl_init_jit(void) { }

// Snooping on which functions are being compiled, and how long it takes
JL_STREAM *dump_compiles_stream = NULL;
extern "C" JL_DLLEXPORT
void jl_dump_compiles(void *s)
{
    dump_compiles_stream = (JL_STREAM*)s;
}

static void jl_add_to_ee();
static uint64_t getAddressForFunction(StringRef fname);

void jl_jit_globals(std::map<void *, GlobalVariable*> &globals)
{
    for (auto &global : globals) {
        Constant *P = literal_static_pointer_val(global.first, global.second->getValueType());
        global.second->setInitializer(P);
        global.second->setConstant(true);
        global.second->setLinkage(GlobalValue::PrivateLinkage);
        global.second->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    }
}

// this generates llvm code for the lambda info
// and adds the result to the jitlayers
// (and the shadow module),
// and generates code for it
static jl_callptr_t _jl_compile_codeinst(
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        size_t world)
{
    // TODO: Merge with jl_dump_compiles?
    static ios_t f_precompile;
    static JL_STREAM* s_precompile = NULL;

    // caller must hold codegen_lock
    // and have disabled finalizers
    JL_TIMING(CODEGEN);
    uint64_t start_time = 0;
    if (dump_compiles_stream != NULL)
        start_time = jl_hrtime();

    assert(jl_is_code_instance(codeinst));
    assert(codeinst->min_world <= world && (codeinst->max_world >= world || codeinst->max_world == 0) &&
        "invalid world for method-instance");
    assert(src && jl_is_code_info(src));

    jl_callptr_t fptr = NULL;
    // emit the code in LLVM IR form
    jl_codegen_params_t params;
    params.cache = true;
    params.world = world;
    std::map<jl_code_instance_t*, jl_compile_result_t> emitted;
    jl_compile_result_t result = jl_emit_codeinst(codeinst, src, params);
    if (std::get<0>(result))
        emitted[codeinst] = std::move(result);
    jl_compile_workqueue(emitted, params);

    jl_jit_globals(params.globals);
    for (auto &def : emitted) {
        // Add the results to the execution engine now
        jl_finalize_module(std::move(std::get<0>(def.second)));
    }
    jl_add_to_ee();
    for (auto &def : emitted) {
        jl_code_instance_t *this_code = def.first;
        jl_llvm_functions_t decls = std::get<1>(def.second);
        jl_callptr_t addr;
        bool isspecsig = false;
        if (decls.functionObject == "jl_fptr_args") {
            addr = &jl_fptr_args;
        }
        else if (decls.functionObject == "jl_fptr_sparam") {
            addr = &jl_fptr_sparam;
        }
        else {
            addr = (jl_callptr_t)getAddressForFunction(decls.functionObject);
            isspecsig = true;
        }
        if (this_code->invoke == NULL) {
            // once set, don't change invoke-ptr, as that leads to race conditions
            // with the (not) simultaneous updates to invoke and specptr
            if (!decls.specFunctionObject.empty()) {
                this_code->specptr.fptr = (void*)getAddressForFunction(decls.specFunctionObject);
                this_code->isspecsig = isspecsig;
            }
            this_code->invoke = addr;
        }
        else if (this_code->invoke == jl_fptr_const_return && !decls.specFunctionObject.empty()) {
            // hack to export this pointer value to jl_dump_method_asm
            this_code->specptr.fptr = (void*)getAddressForFunction(decls.specFunctionObject);
        }
        if (this_code== codeinst)
            fptr = addr;
    }

    uint64_t end_time = 0;
    if (dump_compiles_stream != NULL)
        end_time = jl_hrtime();

    // If logging of the compilation stream is enabled,
    // then dump the method-instance specialization type to the stream
    jl_method_instance_t *mi = codeinst->def;
    if (jl_is_method(mi->def.method)) {
        if (jl_options.trace_compile != NULL) {
            if (s_precompile == NULL) {
                const char* t = jl_options.trace_compile;
                if (!strncmp(t, "stderr", 6))
                    s_precompile = JL_STDERR;
                else {
                    if (ios_file(&f_precompile, t, 1, 1, 1, 1) == NULL)
                        jl_errorf("cannot open precompile statement file \"%s\" for writing", t);
                    s_precompile = (JL_STREAM*) &f_precompile;
                }
            }
            if (!jl_has_free_typevars(mi->specTypes)) {
                jl_printf(s_precompile, "precompile(");
                jl_static_show(s_precompile, mi->specTypes);
                jl_printf(s_precompile, ")\n");

                if (s_precompile != JL_STDERR)
                    ios_flush(&f_precompile);
            }
        }
        if (dump_compiles_stream != NULL) {
            jl_printf(dump_compiles_stream, "%" PRIu64 "\t\"", end_time - start_time);
            jl_static_show(dump_compiles_stream, mi->specTypes);
            jl_printf(dump_compiles_stream, "\"\n");
        }
    }
    return fptr;
}

// get the address of a C-callable entry point for a function
extern "C" JL_DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_GC_PUSH1(&argt);
    JL_LOCK(&codegen_lock);
    jl_codegen_params_t params;
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt, params);
    jl_jit_globals(params.globals);
    assert(params.workqueue.empty());
    jl_add_to_ee();
    JL_GC_POP();
    void *ptr = (void*)getAddressForFunction(llvmf->getName());
    JL_UNLOCK(&codegen_lock);
    return ptr;
}

// export a C-callable entry point for a function (dllexport'ed dlsym), with a given name
extern "C" JL_DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    JL_LOCK(&codegen_lock);
    jl_codegen_params_t params;
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt, params);
    jl_jit_globals(params.globals);
    assert(params.workqueue.empty());
    // force eager emission of the function (llvm 3.3 gets confused otherwise and tries to do recursive compilation)
    jl_add_to_ee();
    uint64_t Addr = getAddressForFunction(llvmf->getName());

    if (imaging_mode)
        llvmf = cast<Function>(shadow_output->getNamedValue(llvmf->getName()));

    // make the alias to the shadow_module
    GlobalAlias *GA =
        GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                            GlobalValue::ExternalLinkage, name, llvmf, shadow_output);

    // make sure the alias name is valid for the current session
    jl_ExecutionEngine->addGlobalMapping(GA, (void*)(uintptr_t)Addr);
    JL_UNLOCK(&codegen_lock);
}

// this compiles li and emits fptr
extern "C"
jl_code_instance_t *jl_generate_fptr(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world)
{
    JL_LOCK(&codegen_lock); // also disables finalizers, to prevent any unexpected recursion
    // if we don't have any decls already, try to generate it now
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    jl_value_t *ci = jl_rettype_inferred(mi, world, world);
    jl_code_instance_t *codeinst = (ci == jl_nothing ? NULL : (jl_code_instance_t*)ci);
    if (codeinst) {
        src = (jl_code_info_t*)codeinst->inferred;
        if ((jl_value_t*)src == jl_nothing)
            src = NULL;
        else if (jl_is_method(mi->def.method))
            src = jl_uncompress_ir(mi->def.method, codeinst, (jl_array_t*)src);
    }
    if (src == NULL && jl_is_method(mi->def.method) &&
             jl_symbol_name(mi->def.method->name)[0] != '@') {
        // If the caller didn't provide the source,
        // see if it is inferred, or try to infer it for ourself.
        // (but don't bother with typeinf on macros or toplevel thunks)
        src = jl_type_infer(mi, world, 0);
    }
    jl_code_instance_t *compiled = jl_method_compiled(mi, world);
    if (compiled) {
        codeinst = compiled;
    }
    else if (src && jl_is_code_info(src)) {
        if (!codeinst) {
            codeinst = jl_get_method_inferred(mi, src->rettype, src->min_world, src->max_world);
            if (src->inferred && !codeinst->inferred)
                codeinst->inferred = jl_nothing;
        }
        _jl_compile_codeinst(codeinst, src, world);
        if (codeinst->invoke == NULL)
            codeinst = NULL;
    }
    else {
        codeinst = NULL;
    }
    JL_UNLOCK(&codegen_lock);
    JL_GC_POP();
    return codeinst;
}

extern "C"
void jl_generate_fptr_for_unspecialized(jl_code_instance_t *unspec)
{
    if (unspec->invoke != NULL)
        return;
    JL_LOCK(&codegen_lock);
    if (unspec->invoke == NULL) {
        jl_code_info_t *src = NULL;
        JL_GC_PUSH1(&src);
        jl_method_t *def = unspec->def->def.method;
        if (jl_is_method(def)) {
            src = (jl_code_info_t*)def->source;
            if (src == NULL) {
                // TODO: this is wrong
                assert(def->generator);
                // TODO: jl_code_for_staged can throw
                src = jl_code_for_staged(unspec->def);
            }
            if (src && (jl_value_t*)src != jl_nothing)
                src = jl_uncompress_ir(def, NULL, (jl_array_t*)src);
        }
        else {
            src = (jl_code_info_t*)unspec->def->uninferred;
        }
        assert(src && jl_is_code_info(src));
        _jl_compile_codeinst(unspec, src, unspec->min_world);
        if (unspec->invoke == NULL)
            // if we hit a codegen bug (or ran into a broken generated function or llvmcall), fall back to the interpreter as a last resort
            unspec->invoke = &jl_fptr_interpret_call;
        JL_GC_POP();
    }
    JL_UNLOCK(&codegen_lock); // Might GC
}


// get a native disassembly for a compiled method
extern "C" JL_DLLEXPORT
jl_value_t *jl_dump_method_asm(jl_method_instance_t *mi, size_t world,
        int raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo)
{
    // printing via disassembly
    jl_code_instance_t *codeinst = jl_generate_fptr(mi, world);
    if (codeinst) {
        uintptr_t fptr = (uintptr_t)codeinst->invoke;
        if (getwrapper)
            return jl_dump_fptr_asm(fptr, raw_mc, asm_variant, debuginfo);
        uintptr_t specfptr = (uintptr_t)codeinst->specptr.fptr;
        if (fptr == (uintptr_t)&jl_fptr_const_return && specfptr == 0) {
            // normally we prevent native code from being generated for these functions,
            // so create an exception here so we can print pretty lies
            JL_LOCK(&codegen_lock); // also disables finalizers, to prevent any unexpected recursion
            specfptr = (uintptr_t)codeinst->specptr.fptr;
            if (specfptr == 0) {
                jl_code_info_t *src = jl_type_infer(mi, world, 0);
                JL_GC_PUSH1(&src);
                jl_method_t *def = mi->def.method;
                if (jl_is_method(def)) {
                    if (!src) {
                        // TODO: jl_code_for_staged can throw
                        src = def->generator ? jl_code_for_staged(mi) : (jl_code_info_t*)def->source;
                    }
                    if (src && (jl_value_t*)src != jl_nothing)
                        src = jl_uncompress_ir(mi->def.method, codeinst, (jl_array_t*)src);
                }
                fptr = (uintptr_t)codeinst->invoke;
                specfptr = (uintptr_t)codeinst->specptr.fptr;
                if (src && jl_is_code_info(src)) {
                    if (fptr == (uintptr_t)&jl_fptr_const_return && specfptr == 0) {
                        fptr = (uintptr_t)_jl_compile_codeinst(codeinst, src, world);
                        specfptr = (uintptr_t)codeinst->specptr.fptr;
                    }
                }
                JL_GC_POP();
            }
            JL_UNLOCK(&codegen_lock);
        }
        if (specfptr)
            return jl_dump_fptr_asm(specfptr, raw_mc, asm_variant, debuginfo);
    }

    // whatever, that didn't work - use the assembler output instead
    if (raw_mc) // eh, give up, this flag doesn't really work anyways normally
        return (jl_value_t*)jl_pchar_to_array("", 0);
    return jl_dump_llvm_asm(jl_get_llvmf_defn(mi, world, getwrapper, true, jl_default_cgparams), asm_variant, debuginfo);
}

#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
// Resolve non-lock free atomic functions in the libatomic1 library.
// This is the library that provides support for c11/c++11 atomic operations.
static uint64_t resolve_atomic(const char *name)
{
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
    static const char *const libatomic = "libatomic.so.1";
#elif defined(_OS_WINDOWS_)
    static const char *const libatomic = "libatomic-1.dll";
#endif
    static void *atomic_hdl = jl_load_dynamic_library(libatomic,
                                                      JL_RTLD_LOCAL, 0);
    static const char *const atomic_prefix = "__atomic_";
    if (!atomic_hdl)
        return 0;
    if (strncmp(name, atomic_prefix, strlen(atomic_prefix)) != 0)
        return 0;
    uintptr_t value;
    jl_dlsym(atomic_hdl, name, (void **)&value, 0);
    return value;
}
#endif

// Custom object emission notification handler for the JuliaOJIT
extern JITEventListener *CreateJuliaJITEventListener();
JuliaOJIT::DebugObjectRegistrar::DebugObjectRegistrar(JuliaOJIT &JIT)
    : JuliaListener(CreateJuliaJITEventListener()),
      JIT(JIT) {}

JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr);

template <typename ObjT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::registerObject(RTDyldObjHandleT H, const ObjT &Obj,
                                                     const LoadResult &LO)
{
    const ObjT* Object = &Obj;

    JIT.NotifyFinalizer(H, *Object, *LO);
    ORCNotifyObjectEmitted(JuliaListener.get(), *Object, *LO, JIT.MemMgr.get());

    // record all of the exported symbols defined in this object
    // in the primary hash table for the enclosing JIT
    for (auto &Symbol : Object->symbols()) {
        auto Flags = Symbol.getFlags();
        if (Flags & object::BasicSymbolRef::SF_Undefined)
            continue;
        if (!(Flags & object::BasicSymbolRef::SF_Exported))
            continue;
        auto NameOrError = Symbol.getName();
        assert(NameOrError);
        auto Name = NameOrError.get();
        auto Sym = JIT.CompileLayer.findSymbolIn(H, Name.str(), true);
        assert(Sym);
        // note: calling getAddress here eagerly finalizes H
        // as an alternative, we could store the JITSymbol instead
        // (which would present a lazy-initializer functor interface instead)
        JIT.LocalSymbolTable[Name] = (void*)(uintptr_t)cantFail(Sym.getAddress());
    }
}

template <typename ObjSetT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::operator()(RTDyldObjHandleT H,
                const ObjSetT &Object, const LoadResult &LOS)
{
    registerObject(H, Object,
                   static_cast<const RuntimeDyld::LoadedObjectInfo*>(&LOS));
}


CompilerResultT JuliaOJIT::CompilerT::operator()(Module &M)
{
    JL_TIMING(LLVM_OPT);
    jit.PM.run(M);
    std::unique_ptr<MemoryBuffer> ObjBuffer(
        new SmallVectorMemoryBuffer(std::move(jit.ObjBufferSV)));
    auto Obj = object::ObjectFile::createObjectFile(ObjBuffer->getMemBufferRef());

    if (!Obj) {
        llvm_dump(&M);
        std::string Buf;
        raw_string_ostream OS(Buf);
        logAllUnhandledErrors(Obj.takeError(), OS, "");
        OS.flush();
        llvm::report_fatal_error("FATAL: Unable to compile LLVM Module: '" + Buf + "'\n"
                                 "The module's content was printed above. Please file a bug report");
    }

    return CompilerResultT(std::move(ObjBuffer));
}

JuliaOJIT::JuliaOJIT(TargetMachine &TM)
  : TM(TM),
    DL(TM.createDataLayout()),
    ObjStream(ObjBufferSV),
    MemMgr(createRTDyldMemoryManager()),
    registrar(*this),
    ES(),
    SymbolResolver(llvm::orc::createLegacyLookupResolver(
          ES,
          [this](StringRef name) -> llvm::JITSymbol {
            return this->resolveSymbol(name);
          },
          [](llvm::Error Err) {
            cantFail(std::move(Err), "resolveSymbol failed");
          })),
    ObjectLayer(
        AcknowledgeORCv1Deprecation,
        ES,
        [this](RTDyldObjHandleT) {
                      ObjLayerT::Resources result;
                      result.MemMgr = MemMgr;
                      result.Resolver = SymbolResolver;
                      return result;
                    },
        std::ref(registrar)
        ),
    CompileLayer(
            AcknowledgeORCv1Deprecation,
            ObjectLayer,
            CompilerT(this)
        )
{
    addTargetPasses(&PM, &TM);
    addOptimizationPasses(&PM, jl_generating_output() ? 0 : jl_options.opt_level);
    if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
        llvm_unreachable("Target does not support MC emission.");

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    std::string ErrorStr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, &ErrorStr))
        report_fatal_error("FATAL: unable to dlopen self\n" + ErrorStr);
}

void JuliaOJIT::addGlobalMapping(StringRef Name, uint64_t Addr)
{
    bool successful = GlobalSymbolTable.insert(std::make_pair(Name, (void*)Addr)).second;
    (void)successful;
    assert(successful);
}

void JuliaOJIT::addGlobalMapping(const GlobalValue *GV, void *Addr)
{
    addGlobalMapping(getMangledName(GV), (uintptr_t)Addr);
}

void *JuliaOJIT::getPointerToGlobalIfAvailable(StringRef S)
{
    SymbolTableT::const_iterator pos = GlobalSymbolTable.find(S);
    if (pos != GlobalSymbolTable.end())
        return pos->second;
    return nullptr;
}

void *JuliaOJIT::getPointerToGlobalIfAvailable(const GlobalValue *GV)
{
    return getPointerToGlobalIfAvailable(getMangledName(GV));
}


void JuliaOJIT::addModule(std::unique_ptr<Module> M)
{
    std::vector<StringRef> NewExports;
    for (auto &F : M->functions()) {
        if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
            NewExports.push_back(strdup(F.getName().str().c_str()));
        }
    }
#ifndef JL_NDEBUG
    // validate the relocations for M
    for (Module::global_object_iterator I = M->global_objects().begin(), E = M->global_objects().end(); I != E; ) {
        GlobalObject *F = &*I;
        ++I;
        if (F->isDeclaration()) {
            if (F->use_empty())
                F->eraseFromParent();
            else if (!((isa<Function>(F) && isIntrinsicFunction(cast<Function>(F))) ||
                       findUnmangledSymbol(F->getName()) ||
                       SectionMemoryManager::getSymbolAddressInProcess(
                           getMangledName(F->getName())))) {
                std::cerr << "FATAL ERROR: "
                          << "Symbol \"" << F->getName().str() << "\""
                          << "not found";
                abort();
            }
        }
    }
#endif
    JL_TIMING(LLVM_MODULE_FINISH);

    auto key = ES.allocateVModule();
    cantFail(CompileLayer.addModule(key, std::move(M)));
    // Force LLVM to emit the module so that we can register the symbols
    // in our lookup table.
    Error Err = CompileLayer.emitAndFinalize(key);
    // Check for errors to prevent LLVM from crashing the program.
    if (Err)
        report_fatal_error(std::move(Err));
    // record a stable name for this fptr address
    for (auto Name : NewExports) {
        void *addr = LocalSymbolTable[getMangledName(Name)];
        ReverseLocalSymbolTable[addr] = Name;
    }
}

void JuliaOJIT::removeModule(ModuleHandleT H)
{
    (void)CompileLayer.removeModule(H);
}

JL_JITSymbol JuliaOJIT::findSymbol(StringRef Name, bool ExportedSymbolsOnly)
{
    void *Addr = nullptr;
    if (ExportedSymbolsOnly) {
        // Step 1: Check against list of known external globals
        Addr = getPointerToGlobalIfAvailable(Name);
    }
    // Step 2: Search all previously emitted symbols
    if (Addr == nullptr) {
        auto it = LocalSymbolTable.find(Name);
        if (it != LocalSymbolTable.end())
            Addr = it->second;
    }
    return JL_JITSymbol((uintptr_t)Addr, JITSymbolFlags::Exported);
}

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(StringRef Name)
{
    return findSymbol(getMangledName(Name), true);
}

JL_JITSymbol JuliaOJIT::resolveSymbol(StringRef Name)
{
    // Step 0: ObjectLinkingLayer has checked whether it is in the current module
    // Step 1: See if it's something known to the ExecutionEngine
    if (auto Sym = findSymbol(Name, true)) {
        // `findSymbol` already eagerly resolved the address
        // return it directly.
        return Sym;
    }
    // Step 2: Search the program symbols
    if (uint64_t addr = SectionMemoryManager::getSymbolAddressInProcess(Name.str()))
        return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
    if (uint64_t addr = resolve_atomic(Name.str().c_str()))
        return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
    // Return failure code
    return JL_SymbolInfo(nullptr);
}

uint64_t JuliaOJIT::getGlobalValueAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
}

StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst)
{
    auto &fname = ReverseLocalSymbolTable[(void*)(uintptr_t)Addr];
    if (fname.empty()) {
        std::stringstream stream_fname;
        // try to pick an appropriate name that describes it
        if (Addr == (uintptr_t)codeinst->invoke) {
            stream_fname << "jsysw_";
        }
        else if (codeinst->invoke == &jl_fptr_args) {
            stream_fname << "jsys1_";
        }
        else if (codeinst->invoke == &jl_fptr_sparam) {
            stream_fname << "jsys3_";
        }
        else {
            stream_fname << "jlsys_";
        }
        const char* unadorned_name = jl_symbol_name(codeinst->def->def.method->name);
        stream_fname << unadorned_name << "_" << globalUnique++;
        std::string string_fname = stream_fname.str();
        fname = strdup(string_fname.c_str());
        LocalSymbolTable[getMangledName(string_fname)] = (void*)(uintptr_t)Addr;
    }
    return fname;
}


void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    if (!L)
        return;
    EventListeners.push_back(L);
}

void JuliaOJIT::NotifyFinalizer(RTDyldObjHandleT Key,
                                const object::ObjectFile &Obj,
                                const RuntimeDyld::LoadedObjectInfo &LoadedObjectInfo)
{
    for (auto &Listener : EventListeners)
        Listener->notifyObjectLoaded(Key, Obj, LoadedObjectInfo);
}

const DataLayout& JuliaOJIT::getDataLayout() const
{
    return DL;
}

const Triple& JuliaOJIT::getTargetTriple() const
{
    return TM.getTargetTriple();
}

std::string JuliaOJIT::getMangledName(StringRef Name)
{
    SmallString<128> FullName;
    Mangler::getNameWithPrefix(FullName, Name, DL);
    return FullName.str().str();
}

std::string JuliaOJIT::getMangledName(const GlobalValue *GV)
{
    return getMangledName(GV->getName());
}

JuliaOJIT *jl_ExecutionEngine;

// destructively move the contents of src into dest
// this assumes that the targets of the two modules are the same
// including the DataLayout and ModuleFlags (for example)
// and that there is no module-level assembly
// Comdat is also removed, since the JIT doesn't need it
void jl_merge_module(Module *dest, std::unique_ptr<Module> src)
{
    assert(dest != src.get());
    for (Module::global_iterator I = src->global_begin(), E = src->global_end(); I != E;) {
        GlobalVariable *sG = &*I;
        GlobalVariable *dG = cast_or_null<GlobalVariable>(dest->getNamedValue(sG->getName()));
        ++I;
        // Replace a declaration with the definition:
        if (dG) {
            if (sG->isDeclaration()) {
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                assert(dG->isDeclaration() || dG->getInitializer() == sG->getInitializer());
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        // Reparent the global variable:
        sG->removeFromParent();
        dest->getGlobalList().push_back(sG);
        // Comdat is owned by the Module
        sG->setComdat(nullptr);
    }

    for (Module::iterator I = src->begin(), E = src->end(); I != E;) {
        Function *sG = &*I;
        Function *dG = cast_or_null<Function>(dest->getNamedValue(sG->getName()));
        ++I;
        // Replace a declaration with the definition:
        if (dG) {
            if (sG->isDeclaration()) {
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                assert(dG->isDeclaration());
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        // Reparent the global variable:
        sG->removeFromParent();
        dest->getFunctionList().push_back(sG);
        // Comdat is owned by the Module
        sG->setComdat(nullptr);
    }

    for (Module::alias_iterator I = src->alias_begin(), E = src->alias_end(); I != E;) {
        GlobalAlias *sG = &*I;
        GlobalAlias *dG = cast_or_null<GlobalAlias>(dest->getNamedValue(sG->getName()));
        ++I;
        if (dG) {
            if (!dG->isDeclaration()) { // aliases are always definitions, so this test is reversed from the above two
                sG->replaceAllUsesWith(dG);
                sG->eraseFromParent();
                continue;
            }
            else {
                dG->replaceAllUsesWith(sG);
                dG->eraseFromParent();
            }
        }
        sG->removeFromParent();
        dest->getAliasList().push_back(sG);
    }

    // metadata nodes need to be explicitly merged not just copied
    // so there are special passes here for each known type of metadata
    NamedMDNode *sNMD = src->getNamedMetadata("llvm.dbg.cu");
    if (sNMD) {
        NamedMDNode *dNMD = dest->getOrInsertNamedMetadata("llvm.dbg.cu");
        for (NamedMDNode::op_iterator I = sNMD->op_begin(), E = sNMD->op_end(); I != E; ++I) {
            dNMD->addOperand(*I);
        }
    }
}

// this is a unique_ptr, but we don't want
// C++ to attempt to run out finalizer on exit
// since it also owned by jl_LLVMContext
static Module *ready_to_emit;

static void jl_add_to_ee()
{
    JL_TIMING(LLVM_EMIT);
    std::unique_ptr<Module> m(ready_to_emit);
    ready_to_emit = NULL;
    if (m) {
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_)
        // Add special values used by debuginfo to build the UnwindData table registration for Win64
        Type *T_uint32 = Type::getInt32Ty(m->getContext());
        ArrayType *atype = ArrayType::get(T_uint32, 3); // want 4-byte alignment of 12-bytes of data
        (new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__UnwindData"))->setSection(".text");
        (new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__catchjmp"))->setSection(".text");
#endif
        assert(jl_ExecutionEngine);
        jl_ExecutionEngine->addModule(std::move(m));
    }
}

// this passes ownership of a module to the JIT after code emission is complete
void jl_finalize_module(std::unique_ptr<Module> m)
{
    if (ready_to_emit)
        jl_merge_module(ready_to_emit, std::move(m));
    else
        ready_to_emit = m.release();
}

static uint64_t getAddressForFunction(StringRef fname)
{
    return jl_ExecutionEngine->getFunctionAddress(fname);
}

// helper function for adding a DLLImport (dlsym) address to the execution engine
void add_named_global(GlobalObject *gv, void *addr, bool dllimport)
{
#ifdef _OS_WINDOWS_
    // setting JL_DLLEXPORT correctly only matters when building a binary
    // (global_proto will strip this from the JIT)
    if (dllimport && imaging_mode) {
        assert(gv->getLinkage() == GlobalValue::ExternalLinkage);
        // add the __declspec(dllimport) attribute
        gv->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
    }
#endif // _OS_WINDOWS_

    jl_ExecutionEngine->addGlobalMapping(gv, addr);
}
