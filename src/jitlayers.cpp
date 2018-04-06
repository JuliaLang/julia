// This file is a part of Julia. License is MIT: https://julialang.org/license

// Except for parts of this file which were copied from LLVM, under the UIUC license (marked below).

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#include <iostream>
#include <sstream>

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/DynamicLibrary.h>

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
extern "C" tracer_cb jl_linfo_tracer;

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
static jl_callptr_t _jl_compile_linfo(
        jl_method_instance_t *li,
        jl_code_info_t *src,
        size_t world,
        std::vector<jl_method_instance_t*> &triggered_linfos)
{
    // caller must hold codegen_lock
    // and have disabled finalizers
    JL_TIMING(CODEGEN);
    uint64_t start_time = 0;
    if (dump_compiles_stream != NULL)
        start_time = jl_hrtime();

    assert(jl_is_method_instance(li));
    assert(li->min_world <= world && (li->max_world >= world || li->max_world == 0) &&
        "invalid world for method-instance");
    assert(src && jl_is_code_info(src));

    jl_callptr_t fptr = NULL;
    // emit the code in LLVM IR form
    jl_codegen_params_t params;
    params.cache = true;
    params.world = world;
    std::map<jl_method_instance_t *, jl_compile_result_t> emitted;
    jl_compile_result_t result = jl_compile_linfo1(li, src, params);
    if (std::get<0>(result))
        emitted[li] = std::move(result);
    jl_compile_workqueue(emitted, params);

    jl_jit_globals(params.globals);
    for (auto &def : emitted) {
        // Add the results to the execution engine now
        jl_finalize_module(std::move(std::get<0>(def.second)));
    }
    jl_add_to_ee();
    for (auto &def : emitted) {
        jl_method_instance_t *this_li = def.first;
        jl_llvm_functions_t decls = std::get<1>(def.second);
        jl_value_t *rettype = std::get<2>(def.second);
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
            isspecsig = jl_egal(rettype, this_li->rettype);
        }
        if (this_li->compile_traced)
            triggered_linfos.push_back(this_li);
        if (this_li->invoke == jl_fptr_trampoline) {
            // once set, don't change invoke-ptr, as that leads to race conditions
            // with the (not) simultaneous updates to invoke and specptr
            if (!decls.specFunctionObject.empty()) {
                this_li->specptr.fptr = (void*)getAddressForFunction(decls.specFunctionObject);
                this_li->isspecsig = isspecsig;
            }
            this_li->invoke = addr;
        }
        else if (this_li->invoke == jl_fptr_const_return && !decls.specFunctionObject.empty()) {
            // hack to export this pointer value to jl_dump_method_asm
            this_li->specptr.fptr = (void*)getAddressForFunction(decls.specFunctionObject);
        }
        if (this_li == li)
            fptr = addr;
    }

    uint64_t end_time = 0;
    if (dump_compiles_stream != NULL)
        end_time = jl_hrtime();

    // If logging of the compilation stream is enabled,
    // then dump the method-instance specialization type to the stream
    if (dump_compiles_stream != NULL && jl_is_method(li->def.method)) {
        jl_printf(dump_compiles_stream, "%" PRIu64 "\t\"", end_time - start_time);
        jl_static_show(dump_compiles_stream, li->specTypes);
        jl_printf(dump_compiles_stream, "\"\n");
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


static jl_callptr_t _jl_generate_fptr(jl_method_instance_t *li, jl_code_info_t *src, size_t world)
{
    jl_callptr_t fptr = NULL;
    if (src && jl_is_code_info(src)) {
        std::vector<jl_method_instance_t*> triggered_linfos;
        fptr = _jl_compile_linfo(li, src, world, triggered_linfos);
        for (jl_method_instance_t *linfo : triggered_linfos) // TODO: would be better to have released the locks first
            if (jl_linfo_tracer)
                jl_call_tracer(jl_linfo_tracer, (jl_value_t*)linfo);
    }
    return fptr;
}

// this compiles li and emits fptr
extern "C"
jl_callptr_t jl_generate_fptr(jl_method_instance_t **pli, size_t world)
{
    jl_method_instance_t *li = *pli;
    jl_callptr_t fptr = li->invoke;
    if (fptr != jl_fptr_trampoline) {
        return fptr;
    }
    JL_LOCK(&codegen_lock); // also disables finalizers, to prevent any unexpected recursion
    fptr = li->invoke;
    if (fptr != jl_fptr_trampoline) {
        JL_UNLOCK(&codegen_lock);
        return fptr;
    }
    // if not already present, see if we have source code
    // get a CodeInfo object to compile
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    if (!jl_is_method(li->def.method)) {
        src = (jl_code_info_t*)li->inferred;
    }
    else {
        // If the caller didn't provide the source,
        // see if it is inferred
        // or try to infer it for ourself
        src = (jl_code_info_t*)li->inferred;
        if (src && (jl_value_t*)src != jl_nothing)
            src = jl_uncompress_ast(li->def.method, (jl_array_t*)src);
        if (!src || !jl_is_code_info(src)) {
            src = jl_type_infer(pli, world, 0);
            li = *pli;
        }
        fptr = li->invoke;
        if (fptr != jl_fptr_trampoline) {
            JL_UNLOCK(&codegen_lock); // Might GC
            JL_GC_POP();
            return fptr;
        }
    }
    fptr = _jl_generate_fptr(li, src, world);
    JL_UNLOCK(&codegen_lock); // Might GC
    JL_GC_POP();
    return fptr;
}

// this compiles li and emits fptr
extern "C"
jl_callptr_t jl_generate_fptr_for_unspecialized(jl_method_instance_t *unspec)
{
    assert(jl_is_method(unspec->def.method));
    jl_callptr_t fptr = unspec->invoke;
    if (fptr != jl_fptr_trampoline) {
        return fptr;
    }
    JL_LOCK(&codegen_lock);
    fptr = unspec->invoke;
    if (fptr != jl_fptr_trampoline) {
        JL_UNLOCK(&codegen_lock); // Might GC
        return fptr;
    }
    jl_code_info_t *src = (jl_code_info_t*)unspec->def.method->source;
    JL_GC_PUSH1(&src);
    if (src == NULL) {
        // TODO: this is wrong
        assert(unspec->def.method->generator);
        // TODO: jl_code_for_staged can throw
        src = jl_code_for_staged(unspec);
    }
    if (src && (jl_value_t*)src != jl_nothing)
        src = jl_uncompress_ast(unspec->def.method, (jl_array_t*)src);
    fptr = unspec->invoke;
    if (fptr == jl_fptr_trampoline) {
        fptr = _jl_generate_fptr(unspec, src, unspec->min_world);
    }
    JL_UNLOCK(&codegen_lock); // Might GC
    JL_GC_POP();
    return fptr;
}


// get a native disassembly for linfo
extern "C" JL_DLLEXPORT
jl_value_t *jl_dump_method_asm(jl_method_instance_t *linfo, size_t world,
        int raw_mc, char getwrapper, const char* asm_variant)
{
    // printing via disassembly
    uintptr_t fptr = (uintptr_t)jl_generate_fptr(&linfo, world);
    if (fptr) {
        if (getwrapper)
            return jl_dump_fptr_asm(fptr, raw_mc, asm_variant);
        uintptr_t specfptr = (uintptr_t)linfo->specptr.fptr;
        if (fptr == (uintptr_t)&jl_fptr_const_return && specfptr == 0) {
            // normally we prevent native code from being generated for these functions,
            // so create an exception here so we can print pretty lies
            JL_LOCK(&codegen_lock); // also disables finalizers, to prevent any unexpected recursion
            specfptr = (uintptr_t)linfo->specptr.fptr;
            if (specfptr == 0) {
                jl_code_info_t *src = jl_type_infer(&linfo, world, 0);
                JL_GC_PUSH1(&src);
                if (jl_is_method(linfo->def.method)) {
                    if (!src) {
                        // TODO: jl_code_for_staged can throw
                        src = linfo->def.method->generator ? jl_code_for_staged(linfo) : (jl_code_info_t*)linfo->def.method->source;
                    }
                    if (src && (jl_value_t*)src != jl_nothing)
                        src = jl_uncompress_ast(linfo->def.method, (jl_array_t*)src);
                }
                fptr = (uintptr_t)linfo->invoke;
                specfptr = (uintptr_t)linfo->specptr.fptr;
                if (fptr == (uintptr_t)&jl_fptr_const_return && specfptr == 0) {
                    fptr = (uintptr_t)_jl_generate_fptr(linfo, src, world);
                    specfptr = (uintptr_t)linfo->specptr.fptr;
                }
                JL_GC_POP();
            }
            JL_UNLOCK(&codegen_lock);
        }
        if (specfptr)
            return jl_dump_fptr_asm(specfptr, raw_mc, asm_variant);
    }

    // whatever, that didn't work - use the assembler output instead
    if (raw_mc) // this flag doesn't really work anyways
        return (jl_value_t*)jl_pchar_to_array("", 0);
    return jl_dump_llvm_asm(jl_get_llvmf_defn(linfo, world, getwrapper, true, jl_default_cgparams), asm_variant);
}

// ------------------------ TEMPORARILY COPIED FROM LLVM -----------------
// This must be kept in sync with gdb/gdb/jit.h .
extern "C" {

  typedef enum {
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN
  } jit_actions_t;

  struct jit_code_entry {
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
  };

  struct jit_descriptor {
    uint32_t version;
    // This should be jit_actions_t, but we want to be specific about the
    // bit-width.
    uint32_t action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
  };

  // We put information about the JITed function in this global, which the
  // debugger reads.  Make sure to specify the version statically, because the
  // debugger checks the version before we can set it during runtime.
  extern struct jit_descriptor __jit_debug_descriptor;

  LLVM_ATTRIBUTE_NOINLINE extern void __jit_debug_register_code();
}

namespace {

// Use a local variable to hold the addresses to avoid generating a PLT
// on the function call.
// It messes up the GDB lookup logic with dynamically linked LLVM.
// (Ref https://sourceware.org/bugzilla/show_bug.cgi?id=20633)
// Use `volatile` to make sure the call always loads this slot.
void (*volatile jit_debug_register_code)() = __jit_debug_register_code;

using namespace llvm;
using namespace llvm::object;
using namespace llvm::orc;

/// Do the registration.
void NotifyDebugger(jit_code_entry *JITCodeEntry)
{
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;

    // Insert this entry at the head of the list.
    JITCodeEntry->prev_entry = nullptr;
    jit_code_entry *NextEntry = __jit_debug_descriptor.first_entry;
    JITCodeEntry->next_entry = NextEntry;
    if (NextEntry) {
        NextEntry->prev_entry = JITCodeEntry;
    }
    __jit_debug_descriptor.first_entry = JITCodeEntry;
    __jit_debug_descriptor.relevant_entry = JITCodeEntry;
    jit_debug_register_code();
}
}
// ------------------------ END OF TEMPORARY COPY FROM LLVM -----------------

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
    static void *atomic_hdl = jl_load_dynamic_library_e(libatomic,
                                                        JL_RTLD_LOCAL);
    static const char *const atomic_prefix = "__atomic_";
    if (!atomic_hdl)
        return 0;
    if (strncmp(name, atomic_prefix, strlen(atomic_prefix)) != 0)
        return 0;
    return (uintptr_t)jl_dlsym_e(atomic_hdl, name);
}
#endif

// Custom object emission notification handler for the JuliaOJIT
extern JITEventListener *CreateJuliaJITEventListener();
JuliaOJIT::DebugObjectRegistrar::DebugObjectRegistrar(JuliaOJIT &JIT)
    : JuliaListener(CreateJuliaJITEventListener()),
      JIT(JIT) {}

JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const object::ObjectFile &debugObj,
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr);

template <typename ObjT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::registerObject(RTDyldObjHandleT H, const ObjT &Object,
                                                     const LoadResult &LO)
{
    OwningBinary<object::ObjectFile> SavedObject = LO->getObjectForDebug(*Object);

    // If the debug object is unavailable, save (a copy of) the original object
    // for our backtraces
    if (!SavedObject.getBinary()) {
        // This is unfortunate, but there doesn't seem to be a way to take
        // ownership of the original buffer
        auto NewBuffer = MemoryBuffer::getMemBufferCopy(Object->getData(),
                                                        Object->getFileName());
        auto NewObj = ObjectFile::createObjectFile(NewBuffer->getMemBufferRef());
        assert(NewObj);
        SavedObject = OwningBinary<object::ObjectFile>(std::move(*NewObj),
                                                       std::move(NewBuffer));
    }
    else {
        NotifyGDB(SavedObject);
    }

    object::ObjectFile *SavedBinary = SavedObject.getBinary();
    SavedObjects.push_back(std::move(SavedObject));

    ORCNotifyObjectEmitted(JuliaListener.get(), *Object, *SavedBinary, *LO, JIT.MemMgr.get());

    // record all of the exported symbols defined in this object
    // in the primary hash table for the enclosing JIT
    for (auto &Symbol : SavedBinary->symbols()) {
        auto Flags = Symbol.getFlags();
        if (Flags & object::BasicSymbolRef::SF_Undefined)
            continue;
        if (!(Flags & object::BasicSymbolRef::SF_Exported))
            continue;
        auto NameOrError = Symbol.getName();
        assert(NameOrError);
        auto Name = NameOrError.get();
        auto Sym = JIT.CompileLayer.findSymbolIn(H, Name, true);
        assert(Sym);
        // note: calling getAddress here eagerly finalizes H
        // as an alternative, we could store the JITSymbol instead
        // (which would present a lazy-initializer functor interface instead)
#if JL_LLVM_VERSION >= 50000
        void *addr = (void*)(uintptr_t)cantFail(Sym.getAddress());
#else
        void *addr = (void*)(uintptr_t)Sym.getAddress();
#endif
        JIT.LocalSymbolTable[Name] = addr;
    }
}

// TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
template <typename ObjSetT, typename LoadResult>
void JuliaOJIT::DebugObjectRegistrar::operator()(RTDyldObjHandleT H,
                const ObjSetT &Objects, const LoadResult &LOS)
{
#if JL_LLVM_VERSION >= 50000
    registerObject(H, Objects->getBinary(),
                   static_cast<const RuntimeDyld::LoadedObjectInfo*>(&LOS));
#else
    auto oit = Objects.begin();
    auto lit = LOS.begin();
    for (; oit != Objects.end(); ++oit, ++lit) {
        const auto &Object = (*oit)->getBinary();
        auto &LO = *lit;

        registerObject(H, Object, LO);
    }
#endif
}

void JuliaOJIT::DebugObjectRegistrar::NotifyGDB(OwningBinary<object::ObjectFile> &DebugObj)
{
    const char *Buffer = DebugObj.getBinary()->getMemoryBufferRef().getBufferStart();
    size_t      Size = DebugObj.getBinary()->getMemoryBufferRef().getBufferSize();

    assert(Buffer && "Attempt to register a null object with a debugger.");
    jit_code_entry *JITCodeEntry = new jit_code_entry();

    if (!JITCodeEntry) {
        jl_printf(JL_STDERR, "WARNING: Allocation failed when registering a JIT entry!\n");
    }
    else {
        JITCodeEntry->symfile_addr = Buffer;
        JITCodeEntry->symfile_size = Size;

        NotifyDebugger(JITCodeEntry);
    }
}

object::OwningBinary<object::ObjectFile> JuliaOJIT::CompilerT::operator()(Module &M)
{
    JL_TIMING(LLVM_OPT);
    jit.PM.run(M);
    std::unique_ptr<MemoryBuffer> ObjBuffer(
        new ObjectMemoryBuffer(std::move(jit.ObjBufferSV)));
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

    return OwningObj(std::move(*Obj), std::move(ObjBuffer));
}

JuliaOJIT::JuliaOJIT(TargetMachine &TM)
  : TM(TM),
    DL(TM.createDataLayout()),
    ObjStream(ObjBufferSV),
    MemMgr(createRTDyldMemoryManager()),
    registrar(*this),
    ObjectLayer(
#if JL_LLVM_VERSION >= 50000
        [&] { return MemMgr; },
#endif
        std::ref(registrar)
        ),
    CompileLayer(
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
    std::string *ErrorStr = nullptr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, ErrorStr))
        report_fatal_error("FATAL: unable to dlopen self\n" + *ErrorStr);
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
    for (Module::iterator I = M->begin(), E = M->end(); I != E; ) {
        Function *F = &*I;
        ++I;
        if (F->isDeclaration()) {
            if (F->use_empty())
                F->eraseFromParent();
            else if (!(isIntrinsicFunction(F) ||
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
    // We need a memory manager to allocate memory and resolve symbols for this
    // new module. Create one that resolves symbols by looking back into the JIT.
    auto Resolver = orc::createLambdaResolver(
                      [&](const std::string &Name) {
                        // TODO: consider moving the FunctionMover resolver here
                        // Step 0: ObjectLinkingLayer has checked whether it is in the current module
                        // Step 1: See if it's something known to the ExecutionEngine
                        if (auto Sym = findSymbol(Name, true)) {
#if JL_LLVM_VERSION >= 40000
                            // `findSymbol` already eagerly resolved the address
                            // return it directly.
                            return Sym;
#else
                            return RuntimeDyld::SymbolInfo(Sym.getAddress(),
                                                           Sym.getFlags());
#endif
                        }
                        // Step 2: Search the program symbols
                        if (uint64_t addr = SectionMemoryManager::getSymbolAddressInProcess(Name))
                            return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#if defined(_OS_LINUX_) || defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
                        if (uint64_t addr = resolve_atomic(Name.c_str()))
                            return JL_SymbolInfo(addr, JITSymbolFlags::Exported);
#endif
                        // Return failure code
                        return JL_SymbolInfo(nullptr);
                      },
                      [](const std::string &S) { return nullptr; }
                    );
#if JL_LLVM_VERSION >= 50000
    auto modset = cantFail(CompileLayer.addModule(std::move(M), std::move(Resolver)));
#else
    SmallVector<std::unique_ptr<Module>,1> Ms;
    Ms.push_back(std::move(M));
    auto modset = CompileLayer.addModuleSet(std::move(Ms), MemMgr.get(),
                                            std::move(Resolver));
#endif
    // Force LLVM to emit the module so that we can register the symbols
    // in our lookup table.
#if JL_LLVM_VERSION >= 50000
    auto Err = CompileLayer.emitAndFinalize(modset);
    // Check for errors to prevent LLVM from crashing the program.
    assert(!Err);
#else
    CompileLayer.emitAndFinalize(modset);
#endif
    // record a stable name for this fptr address
    for (auto Name : NewExports) {
        void *addr = LocalSymbolTable[getMangledName(Name)];
        ReverseLocalSymbolTable[addr] = Name;
    }
}

void JuliaOJIT::removeModule(ModuleHandleT H)
{
#if JL_LLVM_VERSION >= 50000
    CompileLayer.removeModule(H);
#else
    CompileLayer.removeModuleSet(H);
#endif
}

JL_JITSymbol JuliaOJIT::findSymbol(const std::string &Name, bool ExportedSymbolsOnly)
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

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(const std::string Name)
{
    return findSymbol(getMangledName(Name), true);
}

uint64_t JuliaOJIT::getGlobalValueAddress(const std::string &Name)
{
#if JL_LLVM_VERSION >= 50000
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
#else
    return findSymbol(getMangledName(Name), false).getAddress();
#endif
}

uint64_t JuliaOJIT::getFunctionAddress(const std::string &Name)
{
#if JL_LLVM_VERSION >= 50000
    auto addr = findSymbol(getMangledName(Name), false).getAddress();
    return addr ? addr.get() : 0;
#else
    return findSymbol(getMangledName(Name), false).getAddress();
#endif
}

StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_method_instance_t *li)
{
    auto &fname = ReverseLocalSymbolTable[(void*)(uintptr_t)Addr];
    if (fname.empty()) {
        std::stringstream stream_fname;
        // try to pick an appropriate name that describes it
        if (Addr == (uintptr_t)li->invoke) {
            stream_fname << "jsysw_";
        }
        else if (li->invoke == &jl_fptr_args) {
            stream_fname << "jsys1_";
        }
        else if (li->invoke == &jl_fptr_sparam) {
            stream_fname << "jsys3_";
        }
        else {
            stream_fname << "jlsys_";
        }
        const char* unadorned_name = jl_symbol_name(li->def.method->name);
        stream_fname << unadorned_name << "_" << globalUnique++;
        std::string string_fname = stream_fname.str();
        fname = strdup(string_fname.c_str());
        LocalSymbolTable[getMangledName(string_fname)] = (void*)(uintptr_t)Addr;
    }
    return fname;
}


void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    // TODO
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
    return FullName.str();
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
// (for values created locally or in the sysimage, jl_emit_and_add_to_shadow is generally preferable)
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

extern "C" JL_DLLEXPORT
uint64_t jl_get_llvm_fptr(void *function)
{
    Function *F = (Function*)function;
    uint64_t addr = getAddressForFunction(F->getName());
    if (!addr) {
#if JL_LLVM_VERSION >= 50000
        if (auto exp_addr = jl_ExecutionEngine->findUnmangledSymbol(F->getName()).getAddress()) {
            addr = exp_addr.get();
        }
#else
        addr = jl_ExecutionEngine->findUnmangledSymbol(F->getName()).getAddress();
#endif
    }
    return addr;
}
