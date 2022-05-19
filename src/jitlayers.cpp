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
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Bitcode/BitcodeWriter.h>

// target machine computation
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#if JL_LLVM_VERSION >= 140000
#include <llvm/MC/TargetRegistry.h>
#else
#include <llvm/Support/TargetRegistry.h>
#endif
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Object/SymbolSize.h>

using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "codegen_shared.h"
#include "jitlayers.h"
#include "julia_assert.h"
#include "processor.h"

#ifdef JL_USE_JITLINK
# if JL_LLVM_VERSION >= 140000
#  include <llvm/ExecutionEngine/Orc/DebuggerSupportPlugin.h>
# endif
# include <llvm/ExecutionEngine/JITLink/EHFrameSupport.h>
# include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
#else
# include <llvm/ExecutionEngine/SectionMemoryManager.h>
#endif

#define DEBUG_TYPE "jitlayers"

// Snooping on which functions are being compiled, and how long it takes
extern "C" JL_DLLEXPORT
void jl_dump_compiles_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_compiles_stream() = (JL_STREAM*)s;
}
extern "C" JL_DLLEXPORT
void jl_dump_llvm_opt_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_llvm_opt_stream() = (JL_STREAM*)s;
}

static void jl_add_to_ee(orc::ThreadSafeModule &M, StringMap<orc::ThreadSafeModule*> &NewExports);
static void jl_decorate_module(Module &M);
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
        size_t world,
        orc::ThreadSafeContext context)
{
    // caller must hold codegen_lock
    // and have disabled finalizers
    uint64_t start_time = 0;
    bool timed = !!*jl_ExecutionEngine->get_dump_compiles_stream();
    if (timed)
        start_time = jl_hrtime();

    assert(jl_is_code_instance(codeinst));
    assert(codeinst->min_world <= world && (codeinst->max_world >= world || codeinst->max_world == 0) &&
        "invalid world for method-instance");
    assert(src && jl_is_code_info(src));

    jl_callptr_t fptr = NULL;
    // emit the code in LLVM IR form
    jl_codegen_params_t params(std::move(context)); // Locks the context
    params.cache = true;
    params.world = world;
    jl_workqueue_t emitted;
    {
        orc::ThreadSafeModule result_m =
            jl_create_llvm_module(name_from_method_instance(codeinst->def), params.tsctx, params.imaging);
        jl_llvm_functions_t decls = jl_emit_codeinst(result_m, codeinst, src, params);
        if (result_m)
            emitted[codeinst] = {std::move(result_m), std::move(decls)};
        {
            auto temp_module = jl_create_llvm_module(name_from_method_instance(codeinst->def), params.tsctx, params.imaging);
            jl_compile_workqueue(emitted, *temp_module.getModuleUnlocked(), params, CompilationPolicy::Default);
        }

        if (params._shared_module)
            jl_ExecutionEngine->addModule(std::move(params._shared_module));
        StringMap<orc::ThreadSafeModule*> NewExports;
        StringMap<void*> NewGlobals;
        for (auto &global : params.globals) {
            NewGlobals[global.second->getName()] = global.first;
        }
        for (auto &def : emitted) {
            orc::ThreadSafeModule &TSM = std::get<0>(def.second);
            //The underlying context object is still locked because params is not destroyed yet
            auto M = TSM.getModuleUnlocked();
            for (auto &F : M->global_objects()) {
                if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                    NewExports[F.getName()] = &TSM;
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
            orc::ThreadSafeModule &M = std::get<0>(def.second);
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
        if (jl_atomic_load_relaxed(&this_code->invoke) == NULL) {
            // once set, don't change invoke-ptr, as that leads to race conditions
            // with the (not) simultaneous updates to invoke and specptr
            if (!decls.specFunctionObject.empty()) {
                jl_atomic_store_release(&this_code->specptr.fptr, (void*)getAddressForFunction(decls.specFunctionObject));
                this_code->isspecsig = isspecsig;
            }
            jl_atomic_store_release(&this_code->invoke, addr);
        }
        else if (jl_atomic_load_relaxed(&this_code->invoke) == jl_fptr_const_return_addr && !decls.specFunctionObject.empty()) {
            // hack to export this pointer value to jl_dump_method_disasm
            jl_atomic_store_release(&this_code->specptr.fptr, (void*)getAddressForFunction(decls.specFunctionObject));
        }
        if (this_code== codeinst)
            fptr = addr;
    }

    uint64_t end_time = 0;
    if (timed)
        end_time = jl_hrtime();

    // If logging of the compilation stream is enabled,
    // then dump the method-instance specialization type to the stream
    jl_method_instance_t *mi = codeinst->def;
    if (jl_is_method(mi->def.method)) {
        auto stream = *jl_ExecutionEngine->get_dump_compiles_stream();
        if (stream) {
            jl_printf(stream, "%" PRIu64 "\t\"", end_time - start_time);
            jl_static_show(stream, mi->specTypes);
            jl_printf(stream, "\"\n");
        }
    }
    return fptr;
}

const char *jl_generate_ccallable(LLVMOrcThreadSafeModuleRef llvmmod, void *sysimg_handle, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params);

// compile a C-callable alias
extern "C" JL_DLLEXPORT
int jl_compile_extern_c_impl(LLVMOrcThreadSafeModuleRef llvmmod, void *p, void *sysimg, jl_value_t *declrt, jl_value_t *sigt)
{
    JL_LOCK(&jl_codegen_lock);
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
    orc::ThreadSafeContext ctx;
    auto into = unwrap(llvmmod);
    jl_codegen_params_t *pparams = (jl_codegen_params_t*)p;
    orc::ThreadSafeModule backing;
    if (into == NULL) {
        if (!pparams) {
            ctx = jl_ExecutionEngine->acquireContext();
        }
        backing = jl_create_llvm_module("cextern", pparams ? pparams->tsctx : ctx, pparams ? pparams->imaging : imaging_default());
        into = &backing;
    }
    jl_codegen_params_t params(into->getContext());
    if (pparams == NULL)
        pparams = &params;
    assert(pparams->tsctx.getContext() == into->getContext().getContext());
    const char *name = jl_generate_ccallable(wrap(into), sysimg, declrt, sigt, *pparams);
    bool success = true;
    if (!sysimg) {
        if (jl_ExecutionEngine->getGlobalValueAddress(name)) {
            success = false;
        }
        if (success && p == NULL) {
            jl_jit_globals(params.globals);
            assert(params.workqueue.empty());
            if (params._shared_module)
                jl_ExecutionEngine->addModule(std::move(params._shared_module));
        }
        if (success && llvmmod == NULL)
            jl_ExecutionEngine->addModule(std::move(*into));
    }
    if (jl_codegen_lock.count == 1 && measure_compile_time_enabled)
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
    if (ctx.getContext()) {
        jl_ExecutionEngine->releaseContext(std::move(ctx));
    }
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
    if (!jl_type_mappable_to_c(declrt))
        jl_error("@ccallable: return type doesn't correspond to a C type");

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
    auto ctx = jl_ExecutionEngine->getContext();
    auto &context = *ctx;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    bool is_recompile = false;
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
    else {
        // identify whether this is an invalidated method that is being recompiled
        is_recompile = jl_atomic_load_relaxed(&mi->cache) != NULL;
    }
    if (src == NULL && jl_is_method(mi->def.method) &&
             jl_symbol_name(mi->def.method->name)[0] != '@') {
        if (mi->def.method->source != jl_nothing) {
            // If the caller didn't provide the source and IR is available,
            // see if it is inferred, or try to infer it for ourself.
            // (but don't bother with typeinf on macros or toplevel thunks)
            src = jl_type_infer(mi, world, 0);
        }
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
        _jl_compile_codeinst(codeinst, src, world, context);
        if (jl_atomic_load_relaxed(&codeinst->invoke) == NULL)
            codeinst = NULL;
    }
    else {
        codeinst = NULL;
    }
    if (jl_codegen_lock.count == 1 && measure_compile_time_enabled) {
        uint64_t t_comp = jl_hrtime() - compiler_start_time;
        if (is_recompile)
            jl_atomic_fetch_add_relaxed(&jl_cumulative_recompile_time, t_comp);
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, t_comp);
    }
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
    auto ctx = jl_ExecutionEngine->getContext();
    auto &context = *ctx;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
    if (jl_atomic_load_relaxed(&unspec->invoke) == NULL) {
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
        _jl_compile_codeinst(unspec, src, unspec->min_world, context);
        if (jl_atomic_load_relaxed(&unspec->invoke) == NULL) {
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
            auto ctx = jl_ExecutionEngine->getContext();
            auto &context = *ctx;
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
                        fptr = (uintptr_t)_jl_compile_codeinst(codeinst, src, world, context);
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

static auto countBasicBlocks(const Function &F)
{
    return std::distance(F.begin(), F.end());
}

void JuliaOJIT::OptSelLayerT::emit(std::unique_ptr<orc::MaterializationResponsibility> R, orc::ThreadSafeModule TSM) {
    size_t optlevel = ~0ull;
    TSM.withModuleDo([&](Module &M) {
        if (jl_generating_output()) {
            optlevel = 0;
        }
        else {
            optlevel = std::max(static_cast<int>(jl_options.opt_level), 0);
            size_t optlevel_min = std::max(static_cast<int>(jl_options.opt_level_min), 0);
            for (auto &F : M.functions()) {
                if (!F.getBasicBlockList().empty()) {
                    Attribute attr = F.getFnAttribute("julia-optimization-level");
                    StringRef val = attr.getValueAsString();
                    if (val != "") {
                        size_t ol = (size_t)val[0] - '0';
                        if (ol < optlevel)
                            optlevel = ol;
                    }
                }
            }
            optlevel = std::min(std::max(optlevel, optlevel_min), this->count);
        }
    });
    assert(optlevel != ~0ull && "Failed to select a valid optimization level!");
    this->optimizers[optlevel]->OptimizeLayer.emit(std::move(R), std::move(TSM));
}

void jl_register_jit_object(const object::ObjectFile &debugObj,
                            std::function<uint64_t(const StringRef &)> getLoadAddress,
                            std::function<void *(void *)> lookupWriteAddress);

#ifdef JL_USE_JITLINK

namespace {

using namespace llvm::orc;

struct JITObjectInfo {
    std::unique_ptr<MemoryBuffer> BackingBuffer;
    std::unique_ptr<object::ObjectFile> Object;
    StringMap<uint64_t> SectionLoadAddresses;
};

class JLDebuginfoPlugin : public ObjectLinkingLayer::Plugin {
    std::map<MaterializationResponsibility *, std::unique_ptr<JITObjectInfo>> PendingObjs;
    // Resources from distinct MaterializationResponsibilitys can get merged
    // after emission, so we can have multiple debug objects per resource key.
    std::map<ResourceKey, std::vector<std::unique_ptr<JITObjectInfo>>> RegisteredObjs;

public:
    void notifyMaterializing(MaterializationResponsibility &MR, jitlink::LinkGraph &G,
                             jitlink::JITLinkContext &Ctx,
                             MemoryBufferRef InputObject) override
    {
        // Keeping around a full copy of the input object file (and re-parsing it) is
        // wasteful, but for now, this lets us reuse the existing debuginfo.cpp code.
        // Should look into just directly pulling out all the information required in
        // a JITLink pass and just keeping the required tables/DWARF sections around
        // (perhaps using the LLVM DebuggerSupportPlugin as a reference).
        auto NewBuffer =
            MemoryBuffer::getMemBufferCopy(InputObject.getBuffer(), G.getName());
        auto NewObj =
            cantFail(object::ObjectFile::createObjectFile(NewBuffer->getMemBufferRef()));

        assert(PendingObjs.count(&MR) == 0);
        PendingObjs[&MR] = std::unique_ptr<JITObjectInfo>(
            new JITObjectInfo{std::move(NewBuffer), std::move(NewObj), {}});
    }

    Error notifyEmitted(MaterializationResponsibility &MR) override
    {
        auto It = PendingObjs.find(&MR);
        if (It == PendingObjs.end())
            return Error::success();

        auto NewInfo = PendingObjs[&MR].get();
        auto getLoadAddress = [NewInfo](const StringRef &Name) -> uint64_t {
            auto result = NewInfo->SectionLoadAddresses.find(Name);
            if (result == NewInfo->SectionLoadAddresses.end()) {
                LLVM_DEBUG({
                    dbgs() << "JLDebuginfoPlugin: No load address found for section '"
                           << Name << "'\n";
                });
                return 0;
            }
            return result->second;
        };

        jl_register_jit_object(*NewInfo->Object, getLoadAddress, nullptr);

        cantFail(MR.withResourceKeyDo([&](ResourceKey K) {
            RegisteredObjs[K].push_back(std::move(PendingObjs[&MR]));
            PendingObjs.erase(&MR);
        }));

        return Error::success();
    }

    Error notifyFailed(MaterializationResponsibility &MR) override
    {
        PendingObjs.erase(&MR);
        return Error::success();
    }

    Error notifyRemovingResources(ResourceKey K) override
    {
        RegisteredObjs.erase(K);
        // TODO: If we ever unload code, need to notify debuginfo registry.
        return Error::success();
    }

    void notifyTransferringResources(ResourceKey DstKey, ResourceKey SrcKey) override
    {
        auto SrcIt = RegisteredObjs.find(SrcKey);
        if (SrcIt != RegisteredObjs.end()) {
            for (std::unique_ptr<JITObjectInfo> &Info : SrcIt->second)
                RegisteredObjs[DstKey].push_back(std::move(Info));
            RegisteredObjs.erase(SrcIt);
        }
    }

    void modifyPassConfig(MaterializationResponsibility &MR, jitlink::LinkGraph &,
                          jitlink::PassConfiguration &PassConfig) override
    {
        auto It = PendingObjs.find(&MR);
        if (It == PendingObjs.end())
            return;

        JITObjectInfo &Info = *It->second;
        PassConfig.PostAllocationPasses.push_back([&Info](jitlink::LinkGraph &G) -> Error {
            for (const jitlink::Section &Sec : G.sections()) {
                // Canonical JITLink section names have the segment name included, e.g.
                // "__TEXT,__text" or "__DWARF,__debug_str". There are some special internal
                // sections without a comma separator, which we can just ignore.
                size_t SepPos = Sec.getName().find(',');
                if (SepPos >= 16 || (Sec.getName().size() - (SepPos + 1) > 16)) {
                    LLVM_DEBUG({
                        dbgs() << "JLDebuginfoPlugin: Ignoring section '" << Sec.getName()
                               << "'\n";
                    });
                    continue;
                }
                auto SecName = Sec.getName().substr(SepPos + 1);
                // https://github.com/llvm/llvm-project/commit/118e953b18ff07d00b8f822dfbf2991e41d6d791
#if JL_LLVM_VERSION >= 140000
               Info.SectionLoadAddresses[SecName] = jitlink::SectionRange(Sec).getStart().getValue();
#else
               Info.SectionLoadAddresses[SecName] = jitlink::SectionRange(Sec).getStart();
#endif
            }
            return Error::success();
        });
    }
};
}

# ifdef LLVM_SHLIB

#  if JL_LLVM_VERSION >= 140000
#   define EHFRAME_RANGE(name) orc::ExecutorAddrRange name
#   define UNPACK_EHFRAME_RANGE(name) \
        name.Start.toPtr<uint8_t *>(), \
        static_cast<size_t>(name.size())
#  else
#   define EHFRAME_RANGE(name) JITTargetAddress name##Addr, size_t name##Size
#   define UNPACK_EHFRAME_RANGE(name) \
        jitTargetAddressToPointer<uint8_t *>(name##Addr), \
        name##Size
#  endif

class JLEHFrameRegistrar final : public jitlink::EHFrameRegistrar {
public:
    Error registerEHFrames(EHFRAME_RANGE(EHFrameSection)) override {
        register_eh_frames(
            UNPACK_EHFRAME_RANGE(EHFrameSection));
        return Error::success();
    }

    Error deregisterEHFrames(EHFRAME_RANGE(EHFrameSection)) override {
        deregister_eh_frames(
            UNPACK_EHFRAME_RANGE(EHFrameSection));
        return Error::success();
    }
};
# endif

#else // !JL_USE_JITLINK

RTDyldMemoryManager* createRTDyldMemoryManager(void);

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


#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
void *lookupWriteAddressFor(RTDyldMemoryManager *MemMgr, void *rt_addr);
#endif

void registerRTDyldJITObject(const object::ObjectFile &Object,
                             const RuntimeDyld::LoadedObjectInfo &L,
                             const std::shared_ptr<RTDyldMemoryManager> &MemMgr)
{
    auto SavedObject = L.getObjectForDebug(Object).takeBinary();
    // If the debug object is unavailable, save (a copy of) the original object
    // for our backtraces.
    // This copy seems unfortunate, but there doesn't seem to be a way to take
    // ownership of the original buffer.
    if (!SavedObject.first) {
        auto NewBuffer =
            MemoryBuffer::getMemBufferCopy(Object.getData(), Object.getFileName());
        auto NewObj =
            cantFail(object::ObjectFile::createObjectFile(NewBuffer->getMemBufferRef()));
        SavedObject = std::make_pair(std::move(NewObj), std::move(NewBuffer));
    }
    const object::ObjectFile *DebugObj = SavedObject.first.release();
    SavedObject.second.release();

    StringMap<object::SectionRef> loadedSections;
    // Use the original Object, not the DebugObject, as this is used for the
    // RuntimeDyld::LoadedObjectInfo lookup.
    for (const object::SectionRef &lSection : Object.sections()) {
        auto sName = lSection.getName();
        if (sName) {
            bool inserted = loadedSections.insert(std::make_pair(*sName, lSection)).second;
            assert(inserted);
            (void)inserted;
        }
    }
    auto getLoadAddress = [loadedSections = std::move(loadedSections),
                           &L](const StringRef &sName) -> uint64_t {
        auto search = loadedSections.find(sName);
        if (search == loadedSections.end())
            return 0;
        return L.getSectionLoadAddress(search->second);
    };

    jl_register_jit_object(*DebugObj, getLoadAddress,
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
        [MemMgr](void *p) { return lookupWriteAddressFor(MemMgr.get(), p); }
#else
        nullptr
#endif
    );
}
#endif
namespace {
    std::unique_ptr<TargetMachine> createTargetMachine() {

        TargetOptions options = TargetOptions();
#if defined(_OS_WINDOWS_)
        // use ELF because RuntimeDyld COFF i686 support didn't exist
        // use ELF because RuntimeDyld COFF X86_64 doesn't seem to work (fails to generate function pointers)?
#define FORCE_ELF
#endif
        //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_) && JL_LLVM_VERSION < 130000
        // tell Win32 to assume the stack is always 16-byte aligned,
        // and to ensure that it is 16-byte aligned for out-going calls,
        // to ensure compatibility with GCC codes
        // In LLVM 13 and onwards this has turned into a module option
        options.StackAlignmentOverride = 16;
#endif
#if defined(JL_DEBUG_BUILD) && JL_LLVM_VERSION < 130000
        // LLVM defaults to tls stack guard, which causes issues with Julia's tls implementation
        options.StackProtectorGuard = StackProtectorGuards::Global;
#endif
        Triple TheTriple(sys::getProcessTriple());
#if defined(FORCE_ELF)
        TheTriple.setObjectFormat(Triple::ELF);
#endif
        uint32_t target_flags = 0;
        auto target = jl_get_llvm_target(imaging_default(), target_flags);
        auto &TheCPU = target.first;
        SmallVector<std::string, 10> targetFeatures(target.second.begin(), target.second.end());
        std::string errorstr;
        const Target *TheTarget = TargetRegistry::lookupTarget("", TheTriple, errorstr);
        if (!TheTarget)
            jl_errorf("%s", errorstr.c_str());
        if (jl_processor_print_help || (target_flags & JL_TARGET_UNKNOWN_NAME)) {
            std::unique_ptr<MCSubtargetInfo> MSTI(
                TheTarget->createMCSubtargetInfo(TheTriple.str(), "", ""));
            if (!MSTI->isCPUStringValid(TheCPU))
                jl_errorf("Invalid CPU name \"%s\".", TheCPU.c_str());
            if (jl_processor_print_help) {
                // This is the only way I can find to print the help message once.
                // It'll be nice if we can iterate through the features and print our own help
                // message...
                MSTI->setDefaultFeatures("help", "", "");
            }
        }
        // Package up features to be passed to target/subtarget
        std::string FeaturesStr;
        if (!targetFeatures.empty()) {
            SubtargetFeatures Features;
            for (unsigned i = 0; i != targetFeatures.size(); ++i)
                Features.AddFeature(targetFeatures[i]);
            FeaturesStr = Features.getString();
        }
        // Allocate a target...
        Optional<CodeModel::Model> codemodel =
#ifdef _P64
            // Make sure we are using the large code model on 64bit
            // Let LLVM pick a default suitable for jitting on 32bit
            CodeModel::Large;
#else
            None;
#endif
        auto optlevel = CodeGenOptLevelFor(jl_options.opt_level);
        auto TM = TheTarget->createTargetMachine(
                TheTriple.getTriple(), TheCPU, FeaturesStr,
                options,
                Reloc::Static, // Generate simpler code for JIT
                codemodel,
                optlevel,
                true // JIT
                );
        assert(TM && "Failed to select target machine -"
                                " Is the LLVM backend for this CPU enabled?");
        #if (!defined(_CPU_ARM_) && !defined(_CPU_PPC64_))
        // FastISel seems to be buggy for ARM. Ref #13321
        if (jl_options.opt_level < 2)
            TM->setFastISel(true);
        #endif
        return std::unique_ptr<TargetMachine>(TM);
    }
} // namespace

namespace {

    typedef legacy::PassManager PassManager;

    orc::JITTargetMachineBuilder createJTMBFromTM(TargetMachine &TM, int optlevel) {
        return orc::JITTargetMachineBuilder(TM.getTargetTriple())
        .setCPU(TM.getTargetCPU().str())
        .setFeatures(TM.getTargetFeatureString())
        .setOptions(TM.Options)
        .setRelocationModel(Reloc::Static)
        .setCodeModel(TM.getCodeModel())
        .setCodeGenOptLevel(CodeGenOptLevelFor(optlevel));
    }

    struct TMCreator {
        orc::JITTargetMachineBuilder JTMB;

        TMCreator(TargetMachine &TM, int optlevel) : JTMB(createJTMBFromTM(TM, optlevel)) {}

        std::unique_ptr<TargetMachine> operator()() {
            return cantFail(JTMB.createTargetMachine());
        }
    };

    struct PMCreator {
        std::unique_ptr<TargetMachine> TM;
        int optlevel;
        PMCreator(TargetMachine &TM, int optlevel) : TM(cantFail(createJTMBFromTM(TM, optlevel).createTargetMachine())), optlevel(optlevel) {}
        PMCreator(const PMCreator &other) : PMCreator(*other.TM, other.optlevel) {}
        PMCreator(PMCreator &&other) : TM(std::move(other.TM)), optlevel(other.optlevel) {}
        friend void swap(PMCreator &self, PMCreator &other) {
            using std::swap;
            swap(self.TM, other.TM);
            swap(self.optlevel, other.optlevel);
        }
        PMCreator &operator=(PMCreator other) {
            swap(*this, other);
            return *this;
        }
        std::unique_ptr<PassManager> operator()() {
            auto PM = std::make_unique<legacy::PassManager>();
            addTargetPasses(PM.get(), TM->getTargetTriple(), TM->getTargetIRAnalysis());
            addOptimizationPasses(PM.get(), optlevel);
            addMachinePasses(PM.get(), optlevel);
            return PM;
        }
    };

    struct OptimizerT {
        OptimizerT(TargetMachine &TM, int optlevel) : optlevel(optlevel), PMs(PMCreator(TM, optlevel)) {}

        OptimizerResultT operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) {
            TSM.withModuleDo([&](Module &M) {
                uint64_t start_time = 0;
                {
                    auto stream = *jl_ExecutionEngine->get_dump_llvm_opt_stream();
                    if (stream) {
                        // Print LLVM function statistics _before_ optimization
                        // Print all the information about this invocation as a YAML object
                        jl_printf(stream, "- \n");
                        // We print the name and some statistics for each function in the module, both
                        // before optimization and again afterwards.
                        jl_printf(stream, "  before: \n");
                        for (auto &F : M.functions()) {
                            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                                continue;
                            }
                            // Each function is printed as a YAML object with several attributes
                            jl_printf(stream, "    \"%s\":\n", F.getName().str().c_str());
                            jl_printf(stream, "        instructions: %u\n", F.getInstructionCount());
                            jl_printf(stream, "        basicblocks: %lu\n", countBasicBlocks(F));
                        }

                        start_time = jl_hrtime();
                    }
                }

                JL_TIMING(LLVM_OPT);

                //Run the optimization
                (***PMs).run(M);

                uint64_t end_time = 0;
                {
                    auto stream = *jl_ExecutionEngine->get_dump_llvm_opt_stream();
                    if (stream) {
                        end_time = jl_hrtime();
                        jl_printf(stream, "  time_ns: %" PRIu64 "\n", end_time - start_time);
                        jl_printf(stream, "  optlevel: %d\n", optlevel);

                        // Print LLVM function statistics _after_ optimization
                        jl_printf(stream, "  after: \n");
                        for (auto &F : M.functions()) {
                            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                                continue;
                            }
                            jl_printf(stream, "    \"%s\":\n", F.getName().str().c_str());
                            jl_printf(stream, "        instructions: %u\n", F.getInstructionCount());
                            jl_printf(stream, "        basicblocks: %lu\n", countBasicBlocks(F));
                        }
                    }
                }
            });
            return Expected<orc::ThreadSafeModule>{std::move(TSM)};
        }
    private:
        int optlevel;
        JuliaOJIT::ResourcePool<std::unique_ptr<PassManager>> PMs;
    };

    struct CompilerT : orc::IRCompileLayer::IRCompiler {

        CompilerT(orc::IRSymbolMapper::ManglingOptions MO, TargetMachine &TM, int optlevel)
        : orc::IRCompileLayer::IRCompiler(MO), TMs(TMCreator(TM, optlevel)) {}

        Expected<std::unique_ptr<MemoryBuffer>> operator()(Module &M) override {
            return orc::SimpleCompiler(***TMs)(M);
        }

        JuliaOJIT::ResourcePool<std::unique_ptr<TargetMachine>> TMs;
    };
}

llvm::DataLayout jl_create_datalayout(TargetMachine &TM) {
    // Mark our address spaces as non-integral
    auto jl_data_layout = TM.createDataLayout();
    jl_data_layout.reset(jl_data_layout.getStringRepresentation() + "-ni:10:11:12:13");
    return jl_data_layout;
}

JuliaOJIT::PipelineT::PipelineT(orc::ObjectLayer &BaseLayer, TargetMachine &TM, int optlevel)
: CompileLayer(BaseLayer.getExecutionSession(), BaseLayer,
    std::make_unique<CompilerT>(orc::irManglingOptionsFromTargetOptions(TM.Options), TM, optlevel)),
  OptimizeLayer(CompileLayer.getExecutionSession(), CompileLayer, OptimizerT(TM, optlevel)) {}

JuliaOJIT::JuliaOJIT()
  : TM(createTargetMachine()),
    DL(jl_create_datalayout(*TM)),
#if JL_LLVM_VERSION >= 130000
    ES(cantFail(orc::SelfExecutorProcessControl::Create())),
#else
    ES(),
#endif
    GlobalJD(ES.createBareJITDylib("JuliaGlobals")),
    JD(ES.createBareJITDylib("JuliaOJIT")),
    ContextPool([](){
        auto ctx = std::make_unique<LLVMContext>();
#ifdef JL_LLVM_OPAQUE_POINTERS
        ctx->enableOpaquePointers();
#endif
        return orc::ThreadSafeContext(std::move(ctx));
    }),
#ifdef JL_USE_JITLINK
    // TODO: Port our memory management optimisations to JITLink instead of using the
    // default InProcessMemoryManager.
# if JL_LLVM_VERSION < 140000
    ObjectLayer(ES, std::make_unique<jitlink::InProcessMemoryManager>()),
# else
    ObjectLayer(ES, cantFail(jitlink::InProcessMemoryManager::Create())),
# endif
#else
    MemMgr(createRTDyldMemoryManager()),
    ObjectLayer(
            ES,
            [this]() {
                std::unique_ptr<RuntimeDyld::MemoryManager> result(new ForwardingMemoryManager(MemMgr));
                return result;
            }
        ),
#endif
    Pipelines{
        std::make_unique<PipelineT>(ObjectLayer, *TM, 0),
        std::make_unique<PipelineT>(ObjectLayer, *TM, 1),
        std::make_unique<PipelineT>(ObjectLayer, *TM, 2),
        std::make_unique<PipelineT>(ObjectLayer, *TM, 3),
    },
    OptSelLayer(Pipelines)
{
#ifdef JL_USE_JITLINK
# if defined(_OS_DARWIN_) && defined(LLVM_SHLIB)
    // When dynamically linking against LLVM, use our custom EH frame registration code
    // also used with RTDyld to inform both our and the libc copy of libunwind.
    auto ehRegistrar = std::make_unique<JLEHFrameRegistrar>();
# else
    auto ehRegistrar = std::make_unique<jitlink::InProcessEHFrameRegistrar>();
# endif
    ObjectLayer.addPlugin(std::make_unique<EHFrameRegistrationPlugin>(
        ES, std::move(ehRegistrar)));

    ObjectLayer.addPlugin(std::make_unique<JLDebuginfoPlugin>());
#else
    ObjectLayer.setNotifyLoaded(
        [this](orc::MaterializationResponsibility &MR,
               const object::ObjectFile &Object,
               const RuntimeDyld::LoadedObjectInfo &LO) {
            registerRTDyldJITObject(Object, LO, MemMgr);
        });
#endif

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    std::string ErrorStr;
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, &ErrorStr))
        report_fatal_error(llvm::Twine("FATAL: unable to dlopen self\n") + ErrorStr);

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

void JuliaOJIT::addModule(orc::ThreadSafeModule TSM)
{
    JL_TIMING(LLVM_MODULE_FINISH);
    std::vector<std::string> NewExports;
    TSM.withModuleDo([&](Module &M) {
        jl_decorate_module(M);
        shareStrings(M);
        for (auto &F : M.global_values()) {
            if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                NewExports.push_back(getMangledName(F.getName()));
            }
        }
#if !defined(JL_NDEBUG) && !defined(JL_USE_JITLINK)
        // validate the relocations for M (not implemented for the JITLink memory manager yet)
        for (Module::global_object_iterator I = M.global_objects().begin(), E = M.global_objects().end(); I != E; ) {
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
    });
    // TODO: what is the performance characteristics of this?
    cantFail(OptSelLayer.add(JD, std::move(TSM)));
    // force eager compilation (for now), due to memory management specifics
    // (can't handle compilation recursion)
    for (auto Name : NewExports)
        cantFail(ES.lookup({&JD}, Name));

}

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

StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst)
{
    std::lock_guard<std::mutex> lock(RLST_mutex);
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
        stream_fname << unadorned_name << "_" << RLST_inc++;
        *fname = std::move(stream_fname.str()); // store to ReverseLocalSymbolTable
        addGlobalMapping(*fname, Addr);
    }
    return *fname;
}


#ifdef JL_USE_JITLINK
# if JL_LLVM_VERSION < 140000
#  pragma message("JIT debugging (GDB integration) not available on LLVM < 14.0 (for JITLink)")
void JuliaOJIT::enableJITDebuggingSupport() {}
# else
extern "C" orc::shared::CWrapperFunctionResult
llvm_orc_registerJITLoaderGDBAllocAction(const char *Data, size_t Size);

void JuliaOJIT::enableJITDebuggingSupport()
{
    // We do not use GDBJITDebugInfoRegistrationPlugin::Create, as the runtime name
    // lookup is unnecessarily involved/fragile for our in-process JIT use case
    // (with the llvm_orc_registerJITLoaderGDBAllocAction symbol being in either
    // libjulia-codegen or yet another shared library for LLVM depending on the build
    // flags, etc.).
    const auto Addr = ExecutorAddr::fromPtr(&llvm_orc_registerJITLoaderGDBAllocAction);
    ObjectLayer.addPlugin(std::make_unique<orc::GDBJITDebugInfoRegistrationPlugin>(Addr));
}
# endif
#else
void JuliaOJIT::enableJITDebuggingSupport()
{
    RegisterJITEventListener(JITEventListener::createGDBRegistrationListener());
}

void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    if (!L)
        return;
    this->ObjectLayer.registerJITEventListener(*L);
}
#endif

const DataLayout& JuliaOJIT::getDataLayout() const
{
    return DL;
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

#ifdef JL_USE_JITLINK
size_t JuliaOJIT::getTotalBytes() const
{
    // TODO: Implement in future custom JITLink memory manager.
    return 0;
}
#else
size_t getRTDyldMemoryManagerTotalBytes(RTDyldMemoryManager *mm);

size_t JuliaOJIT::getTotalBytes() const
{
    return getRTDyldMemoryManagerTotalBytes(MemMgr.get());
}
#endif

JuliaOJIT *jl_ExecutionEngine;

// destructively move the contents of src into dest
// this assumes that the targets of the two modules are the same
// including the DataLayout and ModuleFlags (for example)
// and that there is no module-level assembly
// Comdat is also removed, since the JIT doesn't need it
void jl_merge_module(orc::ThreadSafeModule &destTSM, orc::ThreadSafeModule srcTSM)
{
    destTSM.withModuleDo([&](Module &dest) {
        srcTSM.withModuleDo([&](Module &src) {
            assert(&dest != &src && "Cannot merge module with itself!");
            assert(&dest.getContext() == &src.getContext() && "Cannot merge modules with different contexts!");
            assert(dest.getDataLayout() == src.getDataLayout() && "Cannot merge modules with different data layouts!");
            assert(dest.getTargetTriple() == src.getTargetTriple() && "Cannot merge modules with different target triples!");

            for (Module::global_iterator I = src.global_begin(), E = src.global_end(); I != E;) {
                GlobalVariable *sG = &*I;
                GlobalVariable *dG = cast_or_null<GlobalVariable>(dest.getNamedValue(sG->getName()));
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
                dest.getGlobalList().push_back(sG);
                // Comdat is owned by the Module
                sG->setComdat(nullptr);
            }

            for (Module::iterator I = src.begin(), E = src.end(); I != E;) {
                Function *sG = &*I;
                Function *dG = cast_or_null<Function>(dest.getNamedValue(sG->getName()));
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
                dest.getFunctionList().push_back(sG);
                // Comdat is owned by the Module
                sG->setComdat(nullptr);
            }

            for (Module::alias_iterator I = src.alias_begin(), E = src.alias_end(); I != E;) {
                GlobalAlias *sG = &*I;
                GlobalAlias *dG = cast_or_null<GlobalAlias>(dest.getNamedValue(sG->getName()));
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
                dest.getAliasList().push_back(sG);
            }

            // metadata nodes need to be explicitly merged not just copied
            // so there are special passes here for each known type of metadata
            NamedMDNode *sNMD = src.getNamedMetadata("llvm.dbg.cu");
            if (sNMD) {
                NamedMDNode *dNMD = dest.getOrInsertNamedMetadata("llvm.dbg.cu");
                for (NamedMDNode::op_iterator I = sNMD->op_begin(), E = sNMD->op_end(); I != E; ++I) {
                    dNMD->addOperand(*I);
                }
            }
        });
    });
}

// optimize memory by turning long strings into memoized copies, instead of
// making a copy per object file of output.
void JuliaOJIT::shareStrings(Module &M)
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
                ConstantInt::get(T_size, (uintptr_t)(*ES.intern(data)).data()),
                GV.getType());
            GV.replaceAllUsesWith(v);
            erase.push_back(&GV);
        }
    }
    for (auto GV : erase)
        GV->eraseFromParent();
}

//TargetMachine pass-through methods

std::unique_ptr<TargetMachine> JuliaOJIT::cloneTargetMachine() const
{
    return std::unique_ptr<TargetMachine>(getTarget()
        .createTargetMachine(
            getTargetTriple().str(),
            getTargetCPU(),
            getTargetFeatureString(),
            getTargetOptions(),
            TM->getRelocationModel(),
            TM->getCodeModel(),
            TM->getOptLevel()));
}

const Triple& JuliaOJIT::getTargetTriple() const {
    return TM->getTargetTriple();
}
StringRef JuliaOJIT::getTargetFeatureString() const {
    return TM->getTargetFeatureString();
}
StringRef JuliaOJIT::getTargetCPU() const {
    return TM->getTargetCPU();
}
const TargetOptions &JuliaOJIT::getTargetOptions() const {
    return TM->Options;
}
const Target &JuliaOJIT::getTarget() const {
    return TM->getTarget();
}
TargetIRAnalysis JuliaOJIT::getTargetIRAnalysis() const {
    return TM->getTargetIRAnalysis();
}

static void jl_decorate_module(Module &M) {
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_)
    // Add special values used by debuginfo to build the UnwindData table registration for Win64
    ArrayType *atype = ArrayType::get(Type::getInt32Ty(M.getContext()), 3); // want 4-byte alignment of 12-bytes of data
    GlobalVariable *gvs[2] = {
        new GlobalVariable(M, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__UnwindData"),
        new GlobalVariable(M, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__catchjmp") };
    gvs[0]->setSection(".text");
    gvs[1]->setSection(".text");
    appendToCompilerUsed(M, makeArrayRef((GlobalValue**)gvs, 2));
#endif
}

static int jl_add_to_ee(
        orc::ThreadSafeModule &M,
        StringMap<orc::ThreadSafeModule*> &NewExports,
        DenseMap<orc::ThreadSafeModule*, int> &Queued,
        std::vector<std::vector<orc::ThreadSafeModule*>> &ToMerge,
        int depth)
{
    // DAG-sort (post-dominator) the compile to compute the minimum
    // merge-module sets for linkage
    if (!M)
        return 0;
    // First check and record if it's on the stack somewhere
    {
        auto &Cycle = Queued[&M];
        if (Cycle)
            return Cycle;
        ToMerge.push_back({});
        Cycle = depth;
    }
    int MergeUp = depth;
    // Compute the cycle-id
    M.withModuleDo([&](Module &m) {
        for (auto &F : m.global_objects()) {
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
    });
    if (MergeUp == depth) {
        // Not in a cycle (or at the top of it)
        Queued.erase(&M);
        for (auto &CM : ToMerge.at(depth - 1)) {
            assert(Queued.find(CM)->second == depth);
            Queued.erase(CM);
            jl_merge_module(M, std::move(*CM));
        }
        jl_ExecutionEngine->addModule(std::move(M));
        MergeUp = 0;
    }
    else {
        // Add our frame(s) to the top of the cycle
        Queued[&M] = MergeUp;
        auto &Top = ToMerge.at(MergeUp - 1);
        Top.push_back(&M);
        for (auto &CM : ToMerge.at(depth - 1)) {
            assert(Queued.find(CM)->second == depth);
            Queued[CM] = MergeUp;
            Top.push_back(CM);
        }
    }
    ToMerge.pop_back();
    return MergeUp;
}

static void jl_add_to_ee(orc::ThreadSafeModule &M, StringMap<orc::ThreadSafeModule*> &NewExports)
{
    DenseMap<orc::ThreadSafeModule*, int> Queued;
    std::vector<std::vector<orc::ThreadSafeModule*>> ToMerge;
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
size_t jl_jit_total_bytes_impl(void)
{
    return jl_ExecutionEngine->getTotalBytes();
}
