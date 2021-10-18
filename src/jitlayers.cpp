// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"


#include "llvm/IR/Mangler.h"
#include <llvm/ADT/StringMap.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#if JL_LLVM_VERSION >= 130000
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#endif
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>

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
void jl_dump_compiles_impl(void *s)
{
    dump_compiles_stream = (JL_STREAM*)s;
}
JL_STREAM *dump_llvm_opt_stream = NULL;
extern "C" JL_DLLEXPORT
void jl_dump_llvm_opt(void *s)
{
    dump_llvm_opt_stream = (JL_STREAM*)s;
}

static void jl_add_to_ee(std::unique_ptr<Module> m);
static void jl_add_to_ee(std::unique_ptr<Module> &M, StringMap<std::unique_ptr<Module>*> &NewExports);
static uint64_t getAddressForFunction(StringRef fname);

void jl_link_global(GlobalVariable *GV, void *addr)
{
    Constant *P = literal_static_pointer_val(addr, GV->getValueType());
    GV->setInitializer(P);
    if (jl_options.image_codegen) {
        // If we are forcing imaging mode codegen for debugging,
        // emit external non-const symbol to avoid LLVM optimizing the code
        // similar to non-imaging mode.
        GV->setLinkage(GlobalValue::ExternalLinkage);
    }
    else {
        GV->setConstant(true);
        GV->setLinkage(GlobalValue::PrivateLinkage);
        GV->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    }
}

void jl_jit_globals(std::map<void *, GlobalVariable*> &globals)
{
    for (auto &global : globals) {
        jl_link_global(global.second, global.first);
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
    {
        jl_compile_result_t result = jl_emit_codeinst(codeinst, src, params);
        if (std::get<0>(result))
            emitted[codeinst] = std::move(result);
        jl_compile_workqueue(emitted, params, CompilationPolicy::Default);

        if (params._shared_module)
            jl_add_to_ee(std::unique_ptr<Module>(params._shared_module));
        StringMap<std::unique_ptr<Module>*> NewExports;
        StringMap<void*> NewGlobals;
        for (auto &global : params.globals) {
            NewGlobals[global.second->getName()] = global.first;
        }
        for (auto &def : emitted) {
            std::unique_ptr<Module> &M = std::get<0>(def.second);
            for (auto &F : M->global_objects()) {
                if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                    NewExports[F.getName()] = &M;
                }
            }
            // Let's link all globals here also (for now)
            for (auto &GV : M->globals()) {
                auto InitValue = NewGlobals.find(GV.getName());
                if (InitValue != NewGlobals.end()) {
                    jl_link_global(&GV, InitValue->second);
                }
            }
        }
        for (auto &def : emitted) {
            // Add the results to the execution engine now
            std::unique_ptr<Module> &M = std::get<0>(def.second);
            jl_add_to_ee(M, NewExports);
        }
    }
    JL_TIMING(LLVM_MODULE_FINISH);

    for (auto &def : emitted) {
        jl_code_instance_t *this_code = def.first;
        jl_llvm_functions_t decls = std::get<1>(def.second);
        jl_callptr_t addr;
        bool isspecsig = false;
        if (decls.functionObject == "jl_fptr_args") {
            addr = jl_fptr_args_addr;
        }
        else if (decls.functionObject == "jl_fptr_sparam") {
            addr = jl_fptr_sparam_addr;
        }
        else {
            addr = (jl_callptr_t)getAddressForFunction(decls.functionObject);
            isspecsig = true;
        }
        if (this_code->invoke == NULL) {
            // once set, don't change invoke-ptr, as that leads to race conditions
            // with the (not) simultaneous updates to invoke and specptr
            if (!decls.specFunctionObject.empty()) {
                jl_atomic_store_release(&this_code->specptr.fptr, (void*)getAddressForFunction(decls.specFunctionObject));
                this_code->isspecsig = isspecsig;
            }
            jl_atomic_store_release(&this_code->invoke, addr);
        }
        else if (this_code->invoke == jl_fptr_const_return_addr && !decls.specFunctionObject.empty()) {
            // hack to export this pointer value to jl_dump_method_disasm
            jl_atomic_store_release(&this_code->specptr.fptr, (void*)getAddressForFunction(decls.specFunctionObject));
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

const char *jl_generate_ccallable(void *llvmmod, void *sysimg_handle, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params);

// compile a C-callable alias
extern "C" JL_DLLEXPORT
int jl_compile_extern_c_impl(void *llvmmod, void *p, void *sysimg, jl_value_t *declrt, jl_value_t *sigt)
{
    JL_LOCK(&jl_codegen_lock);
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
    jl_codegen_params_t params;
    jl_codegen_params_t *pparams = (jl_codegen_params_t*)p;
    if (pparams == NULL)
        pparams = &params;
    Module *into = (Module*)llvmmod;
    if (into == NULL)
        into = jl_create_llvm_module("cextern");
    const char *name = jl_generate_ccallable(into, sysimg, declrt, sigt, *pparams);
    bool success = true;
    if (!sysimg) {
        if (jl_ExecutionEngine->getGlobalValueAddress(name)) {
            success = false;
        }
        if (success && p == NULL) {
            jl_jit_globals(params.globals);
            assert(params.workqueue.empty());
            if (params._shared_module)
                jl_add_to_ee(std::unique_ptr<Module>(params._shared_module));
        }
        if (success && llvmmod == NULL)
            jl_add_to_ee(std::unique_ptr<Module>(into));
    }
    if (jl_codegen_lock.count == 1 && measure_compile_time_enabled)
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
    JL_UNLOCK(&jl_codegen_lock);
    return success;
}

// declare a C-callable entry point; called during code loading from the toplevel
extern "C" JL_DLLEXPORT
void jl_extern_c_impl(jl_value_t *declrt, jl_tupletype_t *sigt)
{
    // validate arguments. try to do as many checks as possible here to avoid
    // throwing errors later during codegen.
    JL_TYPECHK(@ccallable, type, declrt);
    if (!jl_is_tuple_type(sigt))
        jl_type_error("@ccallable", (jl_value_t*)jl_anytuple_type_type, (jl_value_t*)sigt);
    // check that f is a guaranteed singleton type
    jl_datatype_t *ft = (jl_datatype_t*)jl_tparam0(sigt);
    if (!jl_is_datatype(ft) || ft->instance == NULL)
        jl_error("@ccallable: function object must be a singleton");

    // compute / validate return type
    if (!jl_is_concrete_type(declrt) || jl_is_kind(declrt))
        jl_error("@ccallable: return type must be concrete and correspond to a C type");
    JL_LOCK(&jl_codegen_lock);
    if (!jl_type_mappable_to_c(declrt))
        jl_error("@ccallable: return type doesn't correspond to a C type");
    JL_UNLOCK(&jl_codegen_lock);

    // validate method signature
    size_t i, nargs = jl_nparams(sigt);
    for (i = 1; i < nargs; i++) {
        jl_value_t *ati = jl_tparam(sigt, i);
        if (!jl_is_concrete_type(ati) || jl_is_kind(ati) || !jl_type_mappable_to_c(ati))
            jl_error("@ccallable: argument types must be concrete");
    }

    // save a record of this so that the alias is generated when we write an object file
    jl_method_t *meth = (jl_method_t*)jl_methtable_lookup(ft->name->mt, (jl_value_t*)sigt, jl_atomic_load_acquire(&jl_world_counter));
    if (!jl_is_method(meth))
        jl_error("@ccallable: could not find requested method");
    JL_GC_PUSH1(&meth);
    meth->ccallable = jl_svec2(declrt, (jl_value_t*)sigt);
    jl_gc_wb(meth, meth->ccallable);
    JL_GC_POP();

    // create the alias in the current runtime environment
    int success = jl_compile_extern_c(NULL, NULL, NULL, declrt, (jl_value_t*)sigt);
    if (!success)
        jl_error("@ccallable was already defined for this method name");
}

// this compiles li and emits fptr
extern "C" JL_DLLEXPORT
jl_code_instance_t *jl_generate_fptr_impl(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world)
{
    JL_LOCK(&jl_codegen_lock); // also disables finalizers, to prevent any unexpected recursion
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
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
    if (jl_codegen_lock.count == 1 && measure_compile_time_enabled)
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
    JL_UNLOCK(&jl_codegen_lock);
    JL_GC_POP();
    return codeinst;
}

extern "C" JL_DLLEXPORT
void jl_generate_fptr_for_unspecialized_impl(jl_code_instance_t *unspec)
{
    if (jl_atomic_load_relaxed(&unspec->invoke) != NULL) {
        return;
    }
    JL_LOCK(&jl_codegen_lock);
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
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
        if (unspec->invoke == NULL) {
            // if we hit a codegen bug (or ran into a broken generated function or llvmcall), fall back to the interpreter as a last resort
            jl_atomic_store_release(&unspec->invoke, jl_fptr_interpret_call_addr);
        }
        JL_GC_POP();
    }
    if (jl_codegen_lock.count == 1 && measure_compile_time_enabled)
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
    JL_UNLOCK(&jl_codegen_lock); // Might GC
}


// get a native disassembly for a compiled method
extern "C" JL_DLLEXPORT
jl_value_t *jl_dump_method_asm_impl(jl_method_instance_t *mi, size_t world,
        char raw_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary)
{
    // printing via disassembly
    jl_code_instance_t *codeinst = jl_generate_fptr(mi, world);
    if (codeinst) {
        uintptr_t fptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->invoke);
        if (getwrapper)
            return jl_dump_fptr_asm(fptr, raw_mc, asm_variant, debuginfo, binary);
        uintptr_t specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
        if (fptr == (uintptr_t)jl_fptr_const_return_addr && specfptr == 0) {
            // normally we prevent native code from being generated for these functions,
            // (using sentinel value `1` instead)
            // so create an exception here so we can print pretty our lies
            JL_LOCK(&jl_codegen_lock); // also disables finalizers, to prevent any unexpected recursion
            uint64_t compiler_start_time = 0;
            uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
            if (measure_compile_time_enabled)
                compiler_start_time = jl_hrtime();
            specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
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
                fptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->invoke);
                specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
                if (src && jl_is_code_info(src)) {
                    if (fptr == (uintptr_t)jl_fptr_const_return_addr && specfptr == 0) {
                        fptr = (uintptr_t)_jl_compile_codeinst(codeinst, src, world);
                        specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
                    }
                }
                JL_GC_POP();
            }
            if (measure_compile_time_enabled)
                jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
            JL_UNLOCK(&jl_codegen_lock);
        }
        if (specfptr != 0)
            return jl_dump_fptr_asm(specfptr, raw_mc, asm_variant, debuginfo, binary);
    }

    // whatever, that didn't work - use the assembler output instead
    void *F = jl_get_llvmf_defn(mi, world, getwrapper, true, jl_default_cgparams);
    if (!F)
        return jl_an_empty_string;
    return jl_dump_function_asm(F, raw_mc, asm_variant, debuginfo, binary);
}

// A simple forwarding class, since OrcJIT v2 needs a unique_ptr, while we have a shared_ptr
class ForwardingMemoryManager : public RuntimeDyld::MemoryManager {
private:
    std::shared_ptr<RuntimeDyld::MemoryManager> MemMgr;

public:
    ForwardingMemoryManager(std::shared_ptr<RuntimeDyld::MemoryManager> MemMgr) : MemMgr(MemMgr) {}
    virtual ~ForwardingMemoryManager() = default;
    virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                     unsigned SectionID,
                                     StringRef SectionName) override {
        return MemMgr->allocateCodeSection(Size, Alignment, SectionID, SectionName);
    }
    virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                     unsigned SectionID,
                                     StringRef SectionName,
                                     bool IsReadOnly) override {
        return MemMgr->allocateDataSection(Size, Alignment, SectionID, SectionName, IsReadOnly);
    }
    virtual void reserveAllocationSpace(uintptr_t CodeSize, uint32_t CodeAlign,
                                        uintptr_t RODataSize,
                                        uint32_t RODataAlign,
                                        uintptr_t RWDataSize,
                                        uint32_t RWDataAlign) override {
        return MemMgr->reserveAllocationSpace(CodeSize, CodeAlign, RODataSize, RODataAlign, RWDataSize, RWDataAlign);
    }
    virtual bool needsToReserveAllocationSpace() override {
        return MemMgr->needsToReserveAllocationSpace();
    }
    virtual void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr,
                                  size_t Size) override {
        return MemMgr->registerEHFrames(Addr, LoadAddr, Size);
    }
    virtual void deregisterEHFrames() override {
        return MemMgr->deregisterEHFrames();
    }
    virtual bool finalizeMemory(std::string *ErrMsg = nullptr) override {
        return MemMgr->finalizeMemory(ErrMsg);
    }
    virtual void notifyObjectLoaded(RuntimeDyld &RTDyld,
                                    const object::ObjectFile &Obj) override {
        return MemMgr->notifyObjectLoaded(RTDyld, Obj);
    }
};


// Custom object emission notification handler for the JuliaOJIT
extern JITEventListener *CreateJuliaJITEventListener();

JL_DLLEXPORT void ORCNotifyObjectEmitted(JITEventListener *Listener,
                                         const object::ObjectFile &obj,
                                         const RuntimeDyld::LoadedObjectInfo &L,
                                         RTDyldMemoryManager *memmgr);

#if JL_LLVM_VERSION >= 120000
template <typename ObjT, typename LoadResult>
void JuliaOJIT::registerObject(const ObjT &Obj, const LoadResult &LO)
{
    const ObjT* Object = &Obj;
    ORCNotifyObjectEmitted(JuliaListener.get(), *Object, *LO, MemMgr.get());
}
#else
template <typename ObjT, typename LoadResult>
void JuliaOJIT::registerObject(RTDyldObjHandleT H, const ObjT &Obj, const LoadResult &LO)
{
    const ObjT* Object = &Obj;
    NotifyFinalizer(H, *Object, *LO);
    ORCNotifyObjectEmitted(JuliaListener.get(), *Object, *LO, MemMgr.get());
}
#endif

CodeGenOpt::Level CodeGenOptLevelFor(int optlevel)
{
#ifdef DISABLE_OPT
    return CodeGenOpt::None;
#else
    return optlevel < 2 ? CodeGenOpt::None :
        optlevel == 2 ? CodeGenOpt::Default :
        CodeGenOpt::Aggressive;
#endif
}

static void addPassesForOptLevel(legacy::PassManager &PM, TargetMachine &TM, raw_svector_ostream &ObjStream, MCContext *Ctx, int optlevel)
{
    addTargetPasses(&PM, &TM);
    addOptimizationPasses(&PM, optlevel);
    addMachinePasses(&PM, &TM);
    if (TM.addPassesToEmitMC(PM, Ctx, ObjStream))
        llvm_unreachable("Target does not support MC emission.");
}

static auto countBasicBlocks(const Function &F)
{
    return std::distance(F.begin(), F.end());
}

CompilerResultT JuliaOJIT::CompilerT::operator()(Module &M)
{
    uint64_t start_time = 0;
    if (dump_llvm_opt_stream != NULL) {
        // Print LLVM function statistics _before_ optimization
        // Print all the information about this invocation as a YAML object
        jl_printf(dump_llvm_opt_stream, "- \n");
        // We print the name and some statistics for each function in the module, both
        // before optimization and again afterwards.
        jl_printf(dump_llvm_opt_stream, "  before: \n");
        for (auto &F : M.functions()) {
            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                continue;
            }
            // Each function is printed as a YAML object with several attributes
            jl_printf(dump_llvm_opt_stream, "    \"%s\":\n", F.getName().str().c_str());
            jl_printf(dump_llvm_opt_stream, "        instructions: %u\n", F.getInstructionCount());
            jl_printf(dump_llvm_opt_stream, "        basicblocks: %lu\n", countBasicBlocks(F));
        }

        start_time = jl_hrtime();
    }

    JL_TIMING(LLVM_OPT);

    int optlevel;
    int optlevel_min;
    if (jl_generating_output()) {
        optlevel = 0;
    }
    else {
        optlevel = jl_options.opt_level;
        optlevel_min = jl_options.opt_level_min;
        for (auto &F : M.functions()) {
            if (!F.getBasicBlockList().empty()) {
                Attribute attr = F.getFnAttribute("julia-optimization-level");
                StringRef val = attr.getValueAsString();
                if (val != "") {
                    int ol = (int)val[0] - '0';
                    if (ol >= 0 && ol < optlevel)
                        optlevel = ol;
                }
            }
        }
        optlevel = std::max(optlevel, optlevel_min);
    }
    if (optlevel == 0)
        jit.PM0.run(M);
    else if (optlevel == 1)
        jit.PM1.run(M);
    else if (optlevel == 2)
        jit.PM2.run(M);
    else if (optlevel >= 3)
        jit.PM3.run(M);

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

    uint64_t end_time = 0;
    if (dump_llvm_opt_stream != NULL) {
        end_time = jl_hrtime();
        jl_printf(dump_llvm_opt_stream, "  time_ns: %" PRIu64 "\n", end_time - start_time);
        jl_printf(dump_llvm_opt_stream, "  optlevel: %d\n", optlevel);

        // Print LLVM function statistics _after_ optimization
        jl_printf(dump_llvm_opt_stream, "  after: \n");
        for (auto &F : M.functions()) {
            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                continue;
            }
            jl_printf(dump_llvm_opt_stream, "    \"%s\":\n", F.getName().str().c_str());
            jl_printf(dump_llvm_opt_stream, "        instructions: %u\n", F.getInstructionCount());
            jl_printf(dump_llvm_opt_stream, "        basicblocks: %lu\n", countBasicBlocks(F));
        }
    }

    return CompilerResultT(std::move(ObjBuffer));
}

JuliaOJIT::JuliaOJIT(TargetMachine &TM, LLVMContext *LLVMCtx)
  : TM(TM),
    DL(TM.createDataLayout()),
    ObjStream(ObjBufferSV),
    MemMgr(createRTDyldMemoryManager()),
    JuliaListener(CreateJuliaJITEventListener()),
    TSCtx(std::unique_ptr<LLVMContext>(LLVMCtx)),
#if JL_LLVM_VERSION >= 130000
    ES(cantFail(orc::SelfExecutorProcessControl::Create())),
#else
    ES(),
#endif
    GlobalJD(ES.createBareJITDylib("JuliaGlobals")),
    JD(ES.createBareJITDylib("JuliaOJIT")),
    ObjectLayer(
            ES,
            [this]() {
                std::unique_ptr<RuntimeDyld::MemoryManager> result(new ForwardingMemoryManager(MemMgr));
                return result;
            }
        ),
    CompileLayer(ES, ObjectLayer, std::make_unique<CompilerT>(this))
{
#if JL_LLVM_VERSION >= 120000
    ObjectLayer.setNotifyLoaded(
        [this](orc::MaterializationResponsibility &MR,
               const object::ObjectFile &Object,
               const RuntimeDyld::LoadedObjectInfo &LOS) {
            registerObject(Object, &LOS);
        });
#else
    ObjectLayer.setNotifyLoaded(
        [this](RTDyldObjHandleT H,
               const object::ObjectFile &Object,
               const RuntimeDyld::LoadedObjectInfo &LOS) {
            registerObject(H, Object, &LOS);
        });
#endif
    for (int i = 0; i < 4; i++) {
        TMs[i] = TM.getTarget().createTargetMachine(TM.getTargetTriple().getTriple(), TM.getTargetCPU(),
                TM.getTargetFeatureString(), TM.Options, Reloc::Static, TM.getCodeModel(),
                CodeGenOptLevelFor(i), true);
    }
    addPassesForOptLevel(PM0, *TMs[0], ObjStream, Ctx, 0);
    addPassesForOptLevel(PM1, *TMs[1], ObjStream, Ctx, 1);
    addPassesForOptLevel(PM2, *TMs[2], ObjStream, Ctx, 2);
    addPassesForOptLevel(PM3, *TMs[3], ObjStream, Ctx, 3);

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    std::string ErrorStr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, &ErrorStr))
        report_fatal_error("FATAL: unable to dlopen self\n" + ErrorStr);

    GlobalJD.addGenerator(
      cantFail(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        DL.getGlobalPrefix())));

    // Resolve non-lock free atomic functions in the libatomic1 library.
    // This is the library that provides support for c11/c++11 atomic operations.
    const char *const libatomic =
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        "libatomic.so.1";
#elif defined(_OS_WINDOWS_)
        "libatomic-1.dll";
#else
        NULL;
#endif
    if (libatomic) {
        static void *atomic_hdl = jl_load_dynamic_library(libatomic, JL_RTLD_LOCAL, 0);
        if (atomic_hdl != NULL) {
            GlobalJD.addGenerator(
              cantFail(orc::DynamicLibrarySearchGenerator::Load(
                  libatomic,
                  DL.getGlobalPrefix(),
                  [&](const orc::SymbolStringPtr &S) {
                        const char *const atomic_prefix = "__atomic_";
                        return (*S).startswith(atomic_prefix);
                  })));
        }
    }

    JD.addToLinkOrder(GlobalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
}

void JuliaOJIT::addGlobalMapping(StringRef Name, uint64_t Addr)
{
    std::string MangleName = getMangledName(Name);
    cantFail(JD.define(orc::absoluteSymbols({{ES.intern(MangleName), JITEvaluatedSymbol::fromPointer((void*)Addr)}})));
}

void JuliaOJIT::addModule(std::unique_ptr<Module> M)
{
    JL_TIMING(LLVM_MODULE_FINISH);
    std::vector<std::string> NewExports;
    for (auto &F : M->global_values()) {
        if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
            NewExports.push_back(getMangledName(F.getName()));
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
                llvm::errs() << "FATAL ERROR: "
                             << "Symbol \"" << F->getName().str() << "\""
                             << "not found";
                abort();
            }
        }
    }
#endif
#if JL_LLVM_VERSION >= 120000
    // TODO: what is the performance characteristics of this?
    cantFail(CompileLayer.add(JD, orc::ThreadSafeModule(std::move(M), TSCtx)));
#else
    auto key = ES.allocateVModule();
    // TODO: what is the performance characteristics of this?
    cantFail(CompileLayer.add(JD, orc::ThreadSafeModule(std::move(M), TSCtx), key));
#endif
    // force eager compilation (for now), due to memory management specifics
    // (can't handle compilation recursion)
    for (auto Name : NewExports)
        cantFail(ES.lookup({&JD}, Name));

}

#if JL_LLVM_VERSION < 120000
void JuliaOJIT::removeModule(ModuleHandleT H)
{
    //(void)CompileLayer.remove(H);
}
#endif

JL_JITSymbol JuliaOJIT::findSymbol(StringRef Name, bool ExportedSymbolsOnly)
{
    orc::JITDylib* SearchOrders[2] = {&GlobalJD, &JD};
    ArrayRef<orc::JITDylib*> SearchOrder = makeArrayRef(&SearchOrders[ExportedSymbolsOnly ? 0 : 1], ExportedSymbolsOnly ? 2 : 1);
    auto Sym = ES.lookup(SearchOrder, Name);
    if (Sym)
        return *Sym;
    return Sym.takeError();
}

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(StringRef Name)
{
    return findSymbol(getMangledName(Name), true);
}

uint64_t JuliaOJIT::getGlobalValueAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(addr.getAddress());
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(addr.getAddress());
}

static int globalUniqueGeneratedNames;
StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst)
{
    std::string *fname = &ReverseLocalSymbolTable[(void*)(uintptr_t)Addr];
    if (fname->empty()) {
        std::string string_fname;
        raw_string_ostream stream_fname(string_fname);
        // try to pick an appropriate name that describes it
        jl_callptr_t invoke = jl_atomic_load_relaxed(&codeinst->invoke);
        if (Addr == (uintptr_t)invoke) {
            stream_fname << "jsysw_";
        }
        else if (invoke == jl_fptr_args_addr) {
            stream_fname << "jsys1_";
        }
        else if (invoke == jl_fptr_sparam_addr) {
            stream_fname << "jsys3_";
        }
        else {
            stream_fname << "jlsys_";
        }
        const char* unadorned_name = jl_symbol_name(codeinst->def->def.method->name);
        stream_fname << unadorned_name << "_" << globalUniqueGeneratedNames++;
        *fname = std::move(stream_fname.str()); // store to ReverseLocalSymbolTable
        addGlobalMapping(*fname, Addr);
    }
    return *fname;
}


void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    if (!L)
        return;
#if JL_LLVM_VERSION >= 120000
    this->ObjectLayer.registerJITEventListener(*L);
#else
    EventListeners.push_back(L);
#endif
}

#if JL_LLVM_VERSION < 120000
void JuliaOJIT::NotifyFinalizer(RTDyldObjHandleT Key,
                                const object::ObjectFile &Obj,
                                const RuntimeDyld::LoadedObjectInfo &LoadedObjectInfo)
{
    for (auto &Listener : EventListeners)
        Listener->notifyObjectLoaded(Key, Obj, LoadedObjectInfo);
}
#endif

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

size_t getRTDyldMemoryManagerTotalBytes(RTDyldMemoryManager *mm);

size_t JuliaOJIT::getTotalBytes() const
{
    return getRTDyldMemoryManagerTotalBytes(MemMgr.get());
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
            //// If we start using llvm.used, we need to enable and test this
            //else if (!dG->isDeclaration() && dG->hasAppendingLinkage() && sG->hasAppendingLinkage()) {
            //    auto *dCA = cast<ConstantArray>(dG->getInitializer());
            //    auto *sCA = cast<ConstantArray>(sG->getInitializer());
            //    SmallVector<Constant *, 16> Init;
            //    for (auto &Op : dCA->operands())
            //        Init.push_back(cast_or_null<Constant>(Op));
            //    for (auto &Op : sCA->operands())
            //        Init.push_back(cast_or_null<Constant>(Op));
            //    Type *Int8PtrTy = Type::getInt8PtrTy(dest.getContext());
            //    ArrayType *ATy = ArrayType::get(Int8PtrTy, Init.size());
            //    GlobalVariable *GV = new GlobalVariable(dest, ATy, dG->isConstant(),
            //            GlobalValue::AppendingLinkage, ConstantArray::get(ATy, Init), "",
            //            dG->getThreadLocalMode(), dG->getType()->getAddressSpace());
            //    GV->copyAttributesFrom(dG);
            //    sG->replaceAllUsesWith(GV);
            //    dG->replaceAllUsesWith(GV);
            //    GV->takeName(sG);
            //    sG->eraseFromParent();
            //    dG->eraseFromParent();
            //    continue;
            //}
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

// optimize memory by turning long strings into memoized copies, instead of
// making a copy per object file of output.
void jl_jit_share_data(Module &M)
{
    std::vector<GlobalVariable*> erase;
    for (auto &GV : M.globals()) {
        if (!GV.hasInitializer() || !GV.isConstant())
            continue;
        ConstantDataSequential *CDS = dyn_cast<ConstantDataSequential>(GV.getInitializer());
        if (CDS == nullptr)
            continue;
        StringRef data = CDS->getRawDataValues();
        if (data.size() > 16) { // only for long strings: keep short ones as values
            Type *T_size = Type::getIntNTy(GV.getContext(), sizeof(void*) * 8);
            Constant *v = ConstantExpr::getIntToPtr(
                ConstantInt::get(T_size, (uintptr_t)data.data()),
                GV.getType());
            GV.replaceAllUsesWith(v);
            erase.push_back(&GV);
        }
    }
    for (auto GV : erase)
        GV->eraseFromParent();
}

static void jl_add_to_ee(std::unique_ptr<Module> m)
{
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_)
    // Add special values used by debuginfo to build the UnwindData table registration for Win64
    Type *T_uint32 = Type::getInt32Ty(m->getContext());
    ArrayType *atype = ArrayType::get(T_uint32, 3); // want 4-byte alignment of 12-bytes of data
    GlobalVariable *gvs[2] = {
        new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__UnwindData"),
        new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__catchjmp") };
    gvs[0]->setSection(".text");
    gvs[1]->setSection(".text");
    appendToUsed(*m, makeArrayRef((GlobalValue**)gvs, 2));
#endif
    jl_jit_share_data(*m);
    assert(jl_ExecutionEngine);
    jl_ExecutionEngine->addModule(std::move(m));
}

static int jl_add_to_ee(
        std::unique_ptr<Module> &M,
        StringMap<std::unique_ptr<Module>*> &NewExports,
        DenseMap<Module*, int> &Queued,
        std::vector<std::vector<std::unique_ptr<Module>*>> &ToMerge,
        int depth)
{
    // DAG-sort (post-dominator) the compile to compute the minimum
    // merge-module sets for linkage
    if (!M)
        return 0;
    // First check and record if it's on the stack somewhere
    {
        auto &Cycle = Queued[M.get()];
        if (Cycle)
            return Cycle;
        ToMerge.push_back({});
        Cycle = depth;
    }
    int MergeUp = depth;
    // Compute the cycle-id
    for (auto &F : M->global_objects()) {
        if (F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
            auto Callee = NewExports.find(F.getName());
            if (Callee != NewExports.end()) {
                auto &CM = Callee->second;
                int Down = jl_add_to_ee(*CM, NewExports, Queued, ToMerge, depth + 1);
                assert(Down <= depth);
                if (Down && Down < MergeUp)
                    MergeUp = Down;
            }
        }
    }
    if (MergeUp == depth) {
        // Not in a cycle (or at the top of it)
        Queued.erase(M.get());
        for (auto &CM : ToMerge.at(depth - 1)) {
            assert(Queued.find(CM->get())->second == depth);
            Queued.erase(CM->get());
            jl_merge_module(M.get(), std::move(*CM));
        }
        jl_add_to_ee(std::move(M));
        MergeUp = 0;
    }
    else {
        // Add our frame(s) to the top of the cycle
        Queued[M.get()] = MergeUp;
        auto &Top = ToMerge.at(MergeUp - 1);
        Top.push_back(&M);
        for (auto &CM : ToMerge.at(depth - 1)) {
            assert(Queued.find(CM->get())->second == depth);
            Queued[CM->get()] = MergeUp;
            Top.push_back(CM);
        }
    }
    ToMerge.pop_back();
    return MergeUp;
}

static void jl_add_to_ee(std::unique_ptr<Module> &M, StringMap<std::unique_ptr<Module>*> &NewExports)
{
    DenseMap<Module*, int> Queued;
    std::vector<std::vector<std::unique_ptr<Module>*>> ToMerge;
    jl_add_to_ee(M, NewExports, Queued, ToMerge, 1);
    assert(!M);
}


static uint64_t getAddressForFunction(StringRef fname)
{
    auto addr = jl_ExecutionEngine->getFunctionAddress(fname);
    assert(addr);
    return addr;
}

// helper function for adding a DLLImport (dlsym) address to the execution engine
void add_named_global(StringRef name, void *addr)
{
    jl_ExecutionEngine->addGlobalMapping(name, (uint64_t)(uintptr_t)addr);
}

extern "C" JL_DLLEXPORT
size_t jl_jit_total_bytes(void)
{
    return jl_ExecutionEngine->getTotalBytes();
}
