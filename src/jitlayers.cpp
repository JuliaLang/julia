// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include <stdint.h>
#include <sstream>

#include "llvm/IR/Mangler.h"
#include <llvm/ADT/Statistic.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/DebugObjectManagerPlugin.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderGDB.h>
#include <llvm/ExecutionEngine/Orc/ExecutorProcessControl.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/SmallVectorMemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Bitcode/BitcodeWriter.h>

// target machine computation
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#if JL_LLVM_VERSION >= 170000
#include <llvm/TargetParser/Host.h>
#else
#include <llvm/Support/Host.h>
#endif
#include <llvm/Support/TargetSelect.h>
#include <llvm/Object/SymbolSize.h>

using namespace llvm;

#include "jitlayers.h"
#include "julia_assert.h"
#include "processor.h"

# include <llvm/ExecutionEngine/Orc/DebuggerSupportPlugin.h>
# include <llvm/ExecutionEngine/JITLink/EHFrameSupport.h>
# include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h>
# include <llvm/ExecutionEngine/SectionMemoryManager.h>

#define DEBUG_TYPE "julia_jitlayers"

STATISTIC(LinkedGlobals, "Number of globals linked");
STATISTIC(CompiledCodeinsts, "Number of codeinsts compiled directly");
STATISTIC(MaxWorkqueueSize, "Maximum number of elements in the workqueue");
STATISTIC(IndirectCodeinsts, "Number of dependent codeinsts compiled");
STATISTIC(SpecFPtrCount, "Number of specialized function pointers compiled");
STATISTIC(UnspecFPtrCount, "Number of specialized function pointers compiled");
STATISTIC(ModulesAdded, "Number of modules added to the JIT");
STATISTIC(ModulesOptimized, "Number of modules optimized by the JIT");
STATISTIC(OptO0, "Number of modules optimized at level -O0");
STATISTIC(OptO1, "Number of modules optimized at level -O1");
STATISTIC(OptO2, "Number of modules optimized at level -O2");
STATISTIC(OptO3, "Number of modules optimized at level -O3");
STATISTIC(ModulesMerged, "Number of modules merged");
STATISTIC(InternedGlobals, "Number of global constants interned in the string pool");

#ifdef _COMPILER_MSAN_ENABLED_
// TODO: This should not be necessary on ELF x86_64, but LLVM's implementation
// of the TLS relocations is currently broken, so enable this unconditionally.
#define MSAN_EMUTLS_WORKAROUND 1

// See https://github.com/google/sanitizers/wiki/MemorySanitizerJIT
namespace msan_workaround {

extern "C" {
    extern __thread unsigned long long __msan_param_tls[];
    extern __thread unsigned int __msan_param_origin_tls[];
    extern __thread unsigned long long __msan_retval_tls[];
    extern __thread unsigned int __msan_retval_origin_tls;
    extern __thread unsigned long long __msan_va_arg_tls[];
    extern __thread unsigned int __msan_va_arg_origin_tls[];
    extern __thread unsigned long long __msan_va_arg_overflow_size_tls;
    extern __thread unsigned int __msan_origin_tls;
}

enum class MSanTLS
{
    param = 1,             // __msan_param_tls
    param_origin,          //__msan_param_origin_tls
    retval,                // __msan_retval_tls
    retval_origin,         //__msan_retval_origin_tls
    va_arg,                // __msan_va_arg_tls
    va_arg_origin,         // __msan_va_arg_origin_tls
    va_arg_overflow_size,  // __msan_va_arg_overflow_size_tls
    origin,                //__msan_origin_tls
};

static void *getTLSAddress(void *control)
{
    auto tlsIndex = static_cast<MSanTLS>(reinterpret_cast<uintptr_t>(control));
    switch(tlsIndex)
    {
    case MSanTLS::param: return reinterpret_cast<void *>(&__msan_param_tls);
    case MSanTLS::param_origin: return reinterpret_cast<void *>(&__msan_param_origin_tls);
    case MSanTLS::retval: return reinterpret_cast<void *>(&__msan_retval_tls);
    case MSanTLS::retval_origin: return reinterpret_cast<void *>(&__msan_retval_origin_tls);
    case MSanTLS::va_arg: return reinterpret_cast<void *>(&__msan_va_arg_tls);
    case MSanTLS::va_arg_origin: return reinterpret_cast<void *>(&__msan_va_arg_origin_tls);
    case MSanTLS::va_arg_overflow_size: return reinterpret_cast<void *>(&__msan_va_arg_overflow_size_tls);
    case MSanTLS::origin: return reinterpret_cast<void *>(&__msan_origin_tls);
    default:
        assert(false && "BAD MSAN TLS INDEX");
        return nullptr;
    }
}
}
#endif

// Snooping on which functions are being compiled, and how long it takes
extern "C" JL_DLLEXPORT_CODEGEN
void jl_dump_compiles_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_compiles_stream() = (ios_t*)s;
}
extern "C" JL_DLLEXPORT_CODEGEN
void jl_dump_llvm_opt_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_llvm_opt_stream() = (ios_t*)s;
}

static int jl_add_to_ee(
        orc::ThreadSafeModule &M,
        const StringMap<orc::ThreadSafeModule*> &NewExports,
        DenseMap<orc::ThreadSafeModule*, int> &Queued,
        SmallVectorImpl<orc::ThreadSafeModule*> &Stack) JL_NOTSAFEPOINT;
static void jl_decorate_module(Module &M) JL_NOTSAFEPOINT;
static uint64_t getAddressForFunction(StringRef fname) JL_NOTSAFEPOINT;

void jl_link_global(GlobalVariable *GV, void *addr) JL_NOTSAFEPOINT
{
    ++LinkedGlobals;
    Constant *P = literal_static_pointer_val(addr, GV->getValueType());
    GV->setInitializer(P);
    GV->setDSOLocal(true);
    if (jl_options.image_codegen) {
        // If we are forcing imaging mode codegen for debugging,
        // emit external non-const symbol to avoid LLVM optimizing the code
        // similar to non-imaging mode.
        assert(GV->hasExternalLinkage());
    }
    else {
        GV->setConstant(true);
        GV->setLinkage(GlobalValue::PrivateLinkage);
        GV->setVisibility(GlobalValue::DefaultVisibility);
        GV->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    }
}

void jl_jit_globals(std::map<void *, GlobalVariable*> &globals) JL_NOTSAFEPOINT
{
    for (auto &global : globals) {
        jl_link_global(global.second, global.first);
    }
}

// used for image_codegen, where we keep all the gvs external
// so we can't jit them directly into each module
static orc::ThreadSafeModule jl_get_globals_module(orc::ThreadSafeContext &ctx, const DataLayout &DL, const Triple &T, std::map<void *, GlobalVariable*> &globals) JL_NOTSAFEPOINT
{
    auto lock = ctx.getLock();
    auto GTSM = jl_create_ts_module("globals", ctx, DL, T);
    auto GM = GTSM.getModuleUnlocked();
    for (auto &global : globals) {
        auto GV = global.second;
        auto GV2 = new GlobalVariable(*GM, GV->getValueType(), GV->isConstant(), GlobalValue::ExternalLinkage, literal_static_pointer_val(global.first, GV->getValueType()), GV->getName(), nullptr, GV->getThreadLocalMode(), GV->getAddressSpace(), false);
        GV2->copyAttributesFrom(GV);
        GV2->setDSOLocal(true);
        GV2->setAlignment(GV->getAlign());
    }
    return GTSM;
}

extern jl_value_t *jl_fptr_wait_for_compiled(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m);

// this generates llvm code for the lambda info
// and adds the result to the jitlayers
// (and the shadow module),
// and generates code for it
static jl_callptr_t _jl_compile_codeinst(
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        orc::ThreadSafeContext context)
{
    // caller must hold codegen_lock
    // and have disabled finalizers
    uint64_t start_time = 0;
    bool timed = !!*jl_ExecutionEngine->get_dump_compiles_stream();
    if (timed)
        start_time = jl_hrtime();

    assert(jl_is_code_instance(codeinst));

    JL_TIMING(CODEINST_COMPILE, CODEINST_COMPILE);
    jl_callptr_t fptr = NULL;
    // emit the code in LLVM IR form
    jl_codegen_params_t params(std::move(context), jl_ExecutionEngine->getDataLayout(), jl_ExecutionEngine->getTargetTriple()); // Locks the context
    params.cache = true;
    params.imaging_mode = imaging_default();
    params.debug_level = jl_options.debug_level;
    {
        orc::ThreadSafeModule result_m =
            jl_create_ts_module(name_from_method_instance(codeinst->def), params.tsctx, params.DL, params.TargetTriple);
        jl_llvm_functions_t decls = jl_emit_codeinst(result_m, codeinst, src, params);
        if (result_m)
            params.compiled_functions[codeinst] = {std::move(result_m), std::move(decls)};
        jl_compile_workqueue(params, CompilationPolicy::Default);

        if (params._shared_module) {
            jl_ExecutionEngine->optimizeDLSyms(*params._shared_module);
            jl_ExecutionEngine->addModule(orc::ThreadSafeModule(std::move(params._shared_module), params.tsctx));
        }

        // In imaging mode, we can't inline global variable initializers in order to preserve
        // the fiction that we don't know what loads from the global will return. Thus, we
        // need to emit a separate module for the globals before any functions are compiled,
        // to ensure that the globals are defined when they are compiled.
        if (params.imaging_mode) {
            // Won't contain any PLT/dlsym calls, so no need to optimize those
            jl_ExecutionEngine->addModule(jl_get_globals_module(params.tsctx, params.DL, params.TargetTriple, params.global_targets));
        } else {
            StringMap<void*> NewGlobals;
            for (auto &global : params.global_targets) {
                NewGlobals[global.second->getName()] = global.first;
            }
            for (auto &def : params.compiled_functions) {
                auto M = std::get<0>(def.second).getModuleUnlocked();
                for (auto &GV : M->globals()) {
                    auto InitValue = NewGlobals.find(GV.getName());
                    if (InitValue != NewGlobals.end()) {
                        jl_link_global(&GV, InitValue->second);
                    }
                }
            }
        }

        // Collect the exported functions from the params.compiled_functions modules,
        // which form dependencies on which functions need to be
        // compiled first. Cycles of functions are compiled together.
        // (essentially we compile a DAG of SCCs in reverse topological order,
        // if we treat declarations of external functions as edges from declaration
        // to definition)
        StringMap<orc::ThreadSafeModule*> NewExports;
        for (auto &def : params.compiled_functions) {
            orc::ThreadSafeModule &TSM = std::get<0>(def.second);
            //The underlying context object is still locked because params is not destroyed yet
            auto M = TSM.getModuleUnlocked();
            jl_ExecutionEngine->optimizeDLSyms(*M);
            for (auto &F : M->global_objects()) {
                if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                    NewExports[F.getName()] = &TSM;
                }
            }
        }
        DenseMap<orc::ThreadSafeModule*, int> Queued;
        SmallVector<orc::ThreadSafeModule*, 0> Stack;
        for (auto &def : params.compiled_functions) {
            // Add the results to the execution engine now
            orc::ThreadSafeModule &M = std::get<0>(def.second);
            jl_add_to_ee(M, NewExports, Queued, Stack);
            assert(Queued.empty() && Stack.empty() && !M);
        }
        ++CompiledCodeinsts;
        MaxWorkqueueSize.updateMax(params.compiled_functions.size());
        IndirectCodeinsts += params.compiled_functions.size() - 1;
    }

    size_t i = 0;
    for (auto &def : params.compiled_functions) {
        jl_code_instance_t *this_code = def.first;
        if (i < jl_timing_print_limit)
            jl_timing_show_func_sig(this_code->def->specTypes, JL_TIMING_DEFAULT_BLOCK);

        jl_llvm_functions_t decls = std::get<1>(def.second);
        jl_callptr_t addr;
        bool isspecsig = false;
        if (decls.functionObject == "jl_fptr_args") {
            addr = jl_fptr_args_addr;
        }
        else if (decls.functionObject == "jl_fptr_sparam") {
            addr = jl_fptr_sparam_addr;
        }
        else if (decls.functionObject == "jl_f_opaque_closure_call") {
            addr = jl_f_opaque_closure_call_addr;
        }
        else {
            addr = (jl_callptr_t)getAddressForFunction(decls.functionObject);
            isspecsig = true;
        }
        if (!decls.specFunctionObject.empty()) {
            void *prev_specptr = NULL;
            auto spec = (void*)getAddressForFunction(decls.specFunctionObject);
            if (jl_atomic_cmpswap_acqrel(&this_code->specptr.fptr, &prev_specptr, spec)) {
                // only set specsig and invoke if we were the first to set specptr
                jl_atomic_store_relaxed(&this_code->specsigflags, (uint8_t) isspecsig);
                // we might overwrite invokeptr here; that's ok, anybody who relied on the identity of invokeptr
                // either assumes that specptr was null, doesn't care about specptr,
                // or will wait until specsigflags has 0b10 set before reloading invoke
                jl_atomic_store_release(&this_code->invoke, addr);
                jl_atomic_store_release(&this_code->specsigflags, (uint8_t) (0b10 | isspecsig));
            } else {
                //someone else beat us, don't commit any results
                while (!(jl_atomic_load_acquire(&this_code->specsigflags) & 0b10)) {
                    jl_cpu_pause();
                }
                addr = jl_atomic_load_relaxed(&this_code->invoke);
            }
        } else {
            jl_callptr_t prev_invoke = NULL;
            // Allow replacing addr if it is either NULL or our special waiting placeholder.
            if (!jl_atomic_cmpswap_acqrel(&this_code->invoke, &prev_invoke, addr)) {
                if (prev_invoke == &jl_fptr_wait_for_compiled && !jl_atomic_cmpswap_acqrel(&this_code->invoke, &prev_invoke, addr)) {
                    addr = prev_invoke;
                    //TODO do we want to potentially promote invoke anyways? (e.g. invoke is jl_interpret_call or some other
                    //known lesser function)
                }
            }
        }
        if (this_code == codeinst)
            fptr = addr;
        i++;
    }
    if (i > jl_timing_print_limit)
        jl_timing_printf(JL_TIMING_DEFAULT_BLOCK, "... <%d methods truncated>", i - 10);

    uint64_t end_time = 0;
    if (timed)
        end_time = jl_hrtime();

    // If logging of the compilation stream is enabled,
    // then dump the method-instance specialization type to the stream
    jl_method_instance_t *mi = codeinst->def;
    if (jl_is_method(mi->def.method)) {
        auto stream = *jl_ExecutionEngine->get_dump_compiles_stream();
        if (stream) {
            ios_printf(stream, "%" PRIu64 "\t\"", end_time - start_time);
            jl_static_show((JL_STREAM*)stream, mi->specTypes);
            ios_printf(stream, "\"\n");
        }
    }
    return fptr;
}

const char *jl_generate_ccallable(LLVMOrcThreadSafeModuleRef llvmmod, void *sysimg_handle, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params);

// compile a C-callable alias
extern "C" JL_DLLEXPORT_CODEGEN
int jl_compile_extern_c_impl(LLVMOrcThreadSafeModuleRef llvmmod, void *p, void *sysimg, jl_value_t *declrt, jl_value_t *sigt)
{
    auto ct = jl_current_task;
    bool timed = (ct->reentrant_timing & 1) == 0;
    if (timed)
        ct->reentrant_timing |= 1;
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
        backing = jl_create_ts_module("cextern", pparams ? pparams->tsctx : ctx,  pparams ? pparams->DL : jl_ExecutionEngine->getDataLayout(), pparams ? pparams->TargetTriple : jl_ExecutionEngine->getTargetTriple());
        into = &backing;
    }
    JL_LOCK(&jl_codegen_lock);
    auto target_info = into->withModuleDo([&](Module &M) {
        return std::make_pair(M.getDataLayout(), Triple(M.getTargetTriple()));
    });
    jl_codegen_params_t params(into->getContext(), std::move(target_info.first), std::move(target_info.second));
    params.imaging_mode = imaging_default();
    params.debug_level = jl_options.debug_level;
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
            jl_jit_globals(params.global_targets);
            assert(params.workqueue.empty());
            if (params._shared_module) {
                jl_ExecutionEngine->optimizeDLSyms(*params._shared_module);
                jl_ExecutionEngine->addModule(orc::ThreadSafeModule(std::move(params._shared_module), params.tsctx));
            }
        }
        if (success && llvmmod == NULL) {
            into->withModuleDo([&](Module &M) {
                jl_ExecutionEngine->optimizeDLSyms(M);
            });
            jl_ExecutionEngine->addModule(std::move(*into));
        }
    }
    JL_UNLOCK(&jl_codegen_lock);
    if (timed) {
        if (measure_compile_time_enabled) {
            auto end = jl_hrtime();
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
        }
        ct->reentrant_timing &= ~1ull;
    }
    if (ctx.getContext()) {
        jl_ExecutionEngine->releaseContext(std::move(ctx));
    }
    return success;
}

// declare a C-callable entry point; called during code loading from the toplevel
extern "C" JL_DLLEXPORT_CODEGEN
void jl_extern_c_impl(jl_value_t *declrt, jl_tupletype_t *sigt)
{
    // validate arguments. try to do as many checks as possible here to avoid
    // throwing errors later during codegen.
    JL_TYPECHK(@ccallable, type, declrt);
    if (!jl_is_tuple_type(sigt))
        jl_type_error("@ccallable", (jl_value_t*)jl_anytuple_type_type, (jl_value_t*)sigt);
    // check that f is a guaranteed singleton type
    jl_datatype_t *ft = (jl_datatype_t*)jl_tparam0(sigt);
    if (!jl_is_datatype(ft) || !jl_is_datatype_singleton(ft))
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

extern "C" JL_DLLEXPORT_CODEGEN
void jl_compile_codeinst_impl(jl_code_instance_t *ci)
{
    if (jl_atomic_load_relaxed(&ci->invoke) != NULL) {
        return;
    }
    JL_LOCK(&jl_codegen_lock);
    if (jl_atomic_load_relaxed(&ci->invoke) == NULL) {
        ++SpecFPtrCount;
        uint64_t start = jl_typeinf_timing_begin();
        _jl_compile_codeinst(ci, NULL, *jl_ExecutionEngine->getContext());
        jl_typeinf_timing_end(start, 0);
    }
    JL_UNLOCK(&jl_codegen_lock); // Might GC
}

extern "C" JL_DLLEXPORT_CODEGEN
void jl_generate_fptr_for_unspecialized_impl(jl_code_instance_t *unspec)
{
    if (jl_atomic_load_relaxed(&unspec->invoke) != NULL) {
        return;
    }
    auto ct = jl_current_task;
    bool timed = (ct->reentrant_timing & 1) == 0;
    if (timed)
        ct->reentrant_timing |= 1;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();
    JL_LOCK(&jl_codegen_lock);
    if (jl_atomic_load_relaxed(&unspec->invoke) == NULL) {
        jl_code_info_t *src = NULL;
        JL_GC_PUSH1(&src);
        jl_method_t *def = unspec->def->def.method;
        if (jl_is_method(def)) {
            src = (jl_code_info_t*)def->source;
            if (src && (jl_value_t*)src != jl_nothing)
                src = jl_uncompress_ir(def, NULL, (jl_value_t*)src);
        }
        else {
            src = (jl_code_info_t*)jl_atomic_load_relaxed(&unspec->def->uninferred);
            assert(src);
        }
        if (src) {
            assert(jl_is_code_info(src));
            ++UnspecFPtrCount;
            _jl_compile_codeinst(unspec, src, *jl_ExecutionEngine->getContext());
        }
        jl_callptr_t null = nullptr;
        // if we hit a codegen bug (or ran into a broken generated function or llvmcall), fall back to the interpreter as a last resort
        jl_atomic_cmpswap(&unspec->invoke, &null, jl_fptr_interpret_call_addr);
        JL_GC_POP();
    }
    JL_UNLOCK(&jl_codegen_lock); // Might GC
    if (timed) {
        if (measure_compile_time_enabled) {
            auto end = jl_hrtime();
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
        }
        ct->reentrant_timing &= ~1ull;
    }
}


// get a native disassembly for a compiled method
extern "C" JL_DLLEXPORT_CODEGEN
jl_value_t *jl_dump_method_asm_impl(jl_method_instance_t *mi, size_t world,
        char emit_mc, char getwrapper, const char* asm_variant, const char *debuginfo, char binary)
{
    // printing via disassembly
    jl_code_instance_t *codeinst = jl_compile_method_internal(mi, world);
    if (codeinst) {
        uintptr_t fptr = (uintptr_t)jl_atomic_load_acquire(&codeinst->invoke);
        if (getwrapper)
            return jl_dump_fptr_asm(fptr, emit_mc, asm_variant, debuginfo, binary);
        uintptr_t specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
        if (fptr == (uintptr_t)jl_fptr_const_return_addr && specfptr == 0) {
            // normally we prevent native code from being generated for these functions,
            // (using sentinel value `1` instead)
            // so create an exception here so we can print pretty our lies
            auto ct = jl_current_task;
            bool timed = (ct->reentrant_timing & 1) == 0;
            if (timed)
                ct->reentrant_timing |= 1;
            uint64_t compiler_start_time = 0;
            uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
            if (measure_compile_time_enabled)
                compiler_start_time = jl_hrtime();
            JL_LOCK(&jl_codegen_lock); // also disables finalizers, to prevent any unexpected recursion
            specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
            if (specfptr == 0) {
                // Doesn't need SOURCE_MODE_FORCE_SOURCE_UNCACHED, because the codegen lock is held,
                // so there's no concern that the ->inferred field will be deleted.
                jl_code_instance_t *forced_ci = jl_type_infer(mi, world, 0, SOURCE_MODE_FORCE_SOURCE);
                JL_GC_PUSH1(&forced_ci);
                if (forced_ci) {
                    // Force compile of this codeinst even though it already has an ->invoke
                    _jl_compile_codeinst(forced_ci, NULL, *jl_ExecutionEngine->getContext());
                    specfptr = (uintptr_t)jl_atomic_load_relaxed(&forced_ci->specptr.fptr);
                }
                JL_GC_POP();
            }
            JL_UNLOCK(&jl_codegen_lock);
            if (timed) {
                if (measure_compile_time_enabled) {
                    auto end = jl_hrtime();
                    jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
                }
                ct->reentrant_timing &= ~1ull;
            }
        }
        if (specfptr != 0)
            return jl_dump_fptr_asm(specfptr, emit_mc, asm_variant, debuginfo, binary);
    }

    // whatever, that didn't work - use the assembler output instead
    jl_llvmf_dump_t llvmf_dump;
    jl_get_llvmf_defn(&llvmf_dump, mi, world, getwrapper, true, jl_default_cgparams);
    if (!llvmf_dump.F)
        return jl_an_empty_string;
    return jl_dump_function_asm(&llvmf_dump, emit_mc, asm_variant, debuginfo, binary, false);
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

static auto countBasicBlocks(const Function &F) JL_NOTSAFEPOINT
{
    return std::distance(F.begin(), F.end());
}

static constexpr size_t N_optlevels = 4;

static Expected<orc::ThreadSafeModule> validateExternRelocations(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
#if !defined(JL_NDEBUG) && !defined(JL_USE_JITLINK)
    auto isIntrinsicFunction = [](GlobalObject &GO) JL_NOTSAFEPOINT {
        auto F = dyn_cast<Function>(&GO);
        if (!F)
            return false;
        return F->isIntrinsic() || F->getName().startswith("julia.");
    };
    // validate the relocations for M (only for RuntimeDyld, JITLink performs its own symbol validation)
    auto Err = TSM.withModuleDo([isIntrinsicFunction](Module &M) JL_NOTSAFEPOINT {
        Error Err = Error::success();
        for (auto &GO : make_early_inc_range(M.global_objects())) {
            if (!GO.isDeclarationForLinker())
                continue;
            if (GO.use_empty()) {
                GO.eraseFromParent();
                continue;
            }
            if (isIntrinsicFunction(GO))
                continue;
            auto sym = jl_ExecutionEngine->findUnmangledSymbol(GO.getName());
            if (sym)
                continue;
            // TODO have we ever run into this check? It's been guaranteed to not
            // fire in an assert build, since previously LLVM would abort due to
            // not handling the error if we didn't find the unmangled symbol
            if (SectionMemoryManager::getSymbolAddressInProcess(
                            jl_ExecutionEngine->getMangledName(GO.getName()))) {
                consumeError(sym.takeError());
                continue;
            }
            Err = joinErrors(std::move(Err), sym.takeError());
        }
        return Err;
    });
    if (Err) {
        return std::move(Err);
    }
#endif
    return std::move(TSM);
}

static Expected<orc::ThreadSafeModule> selectOptLevel(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        size_t opt_level = std::max(static_cast<int>(jl_options.opt_level), 0);
        do {
            if (jl_generating_output()) {
                opt_level = 0;
                break;
            }
            size_t opt_level_min = std::max(static_cast<int>(jl_options.opt_level_min), 0);
            for (auto &F : M) {
                if (!F.isDeclaration()) {
                    Attribute attr = F.getFnAttribute("julia-optimization-level");
                    StringRef val = attr.getValueAsString();
                    if (val != "") {
                        size_t ol = (size_t)val[0] - '0';
                        if (ol < opt_level)
                            opt_level = ol;
                    }
                }
            }
            if (opt_level < opt_level_min)
                opt_level = opt_level_min;
        } while (0);
        // currently -O3 is max
        opt_level = std::min(opt_level, N_optlevels - 1);
        M.addModuleFlag(Module::Warning, "julia.optlevel", opt_level);
    });
    return std::move(TSM);
}

static void recordDebugTSM(orc::MaterializationResponsibility &, orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT {
    auto ptr = TSM.withModuleDo([](Module &M) JL_NOTSAFEPOINT {
        auto md = M.getModuleFlag("julia.__jit_debug_tsm_addr");
        if (!md)
            return static_cast<orc::ThreadSafeModule *>(nullptr);
        return reinterpret_cast<orc::ThreadSafeModule *>(cast<ConstantInt>(cast<ConstantAsMetadata>(md)->getValue())->getZExtValue());
    });
    if (ptr) {
        *ptr = std::move(TSM);
    }
}

void jl_register_jit_object(const object::ObjectFile &debugObj,
                            std::function<uint64_t(const StringRef &)> getLoadAddress,
                            std::function<void *(void *)> lookupWriteAddress);

namespace {

using namespace llvm::orc;

struct JITObjectInfo {
    std::unique_ptr<MemoryBuffer> BackingBuffer;
    std::unique_ptr<object::ObjectFile> Object;
    StringMap<uint64_t> SectionLoadAddresses;
};

class JLDebuginfoPlugin : public ObjectLinkingLayer::Plugin {
    std::mutex PluginMutex;
    std::map<MaterializationResponsibility *, std::unique_ptr<JITObjectInfo>> PendingObjs;
    // Resources from distinct `MaterializationResponsibility`s can get merged
    // after emission, so we can have multiple debug objects per resource key.
    std::map<ResourceKey, SmallVector<std::unique_ptr<JITObjectInfo>, 0>> RegisteredObjs;

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

        {
            std::lock_guard<std::mutex> lock(PluginMutex);
            assert(PendingObjs.count(&MR) == 0);
            PendingObjs[&MR] = std::unique_ptr<JITObjectInfo>(
                new JITObjectInfo{std::move(NewBuffer), std::move(NewObj), {}});
        }
    }

    Error notifyEmitted(MaterializationResponsibility &MR) override
    {
        {
            std::lock_guard<std::mutex> lock(PluginMutex);
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
        }

        cantFail(MR.withResourceKeyDo([&](ResourceKey K) {
            std::lock_guard<std::mutex> lock(PluginMutex);
            RegisteredObjs[K].push_back(std::move(PendingObjs[&MR]));
            PendingObjs.erase(&MR);
        }));

        return Error::success();
    }

    Error notifyFailed(MaterializationResponsibility &MR) override
    {
        std::lock_guard<std::mutex> lock(PluginMutex);
        PendingObjs.erase(&MR);
        return Error::success();
    }
#if JL_LLVM_VERSION >= 160000
    Error notifyRemovingResources(JITDylib &JD, orc::ResourceKey K) override
#else
    Error notifyRemovingResources(ResourceKey K) override
#endif
    {
        std::lock_guard<std::mutex> lock(PluginMutex);
        RegisteredObjs.erase(K);
        // TODO: If we ever unload code, need to notify debuginfo registry.
        return Error::success();
    }

#if JL_LLVM_VERSION >= 160000
    void notifyTransferringResources(JITDylib &JD, ResourceKey DstKey, ResourceKey SrcKey) override
#else
    void notifyTransferringResources(ResourceKey DstKey, ResourceKey SrcKey) override
#endif
    {
        std::lock_guard<std::mutex> lock(PluginMutex);
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
        std::lock_guard<std::mutex> lock(PluginMutex);
        auto It = PendingObjs.find(&MR);
        if (It == PendingObjs.end())
            return;

        JITObjectInfo &Info = *It->second;
        PassConfig.PostAllocationPasses.push_back([&Info, this](jitlink::LinkGraph &G) -> Error {
            std::lock_guard<std::mutex> lock(PluginMutex);
            for (const jitlink::Section &Sec : G.sections()) {
#if defined(_OS_DARWIN_)
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
#else
                auto SecName = Sec.getName();
#endif
                // https://github.com/llvm/llvm-project/commit/118e953b18ff07d00b8f822dfbf2991e41d6d791
               Info.SectionLoadAddresses[SecName] = jitlink::SectionRange(Sec).getStart().getValue();
            }
            return Error::success();
        });
    }
};

class JLMemoryUsagePlugin : public ObjectLinkingLayer::Plugin {
private:
    std::atomic<size_t> &total_size;

public:

    JLMemoryUsagePlugin(std::atomic<size_t> &total_size)
        : total_size(total_size) {}

    Error notifyFailed(orc::MaterializationResponsibility &MR) override {
        return Error::success();
    }
#if JL_LLVM_VERSION >= 160000
    Error notifyRemovingResources(JITDylib &JD, orc::ResourceKey K) override
#else
    Error notifyRemovingResources(orc::ResourceKey K) override
#endif
    {
        return Error::success();
    }
#if JL_LLVM_VERSION >= 160000
    void notifyTransferringResources(JITDylib &JD, orc::ResourceKey DstKey,
                                     orc::ResourceKey SrcKey) override {}
#else
    void notifyTransferringResources(orc::ResourceKey DstKey,
                                     orc::ResourceKey SrcKey) override {}
#endif

    void modifyPassConfig(orc::MaterializationResponsibility &,
                          jitlink::LinkGraph &,
                          jitlink::PassConfiguration &Config) override {
        Config.PostAllocationPasses.push_back([this](jitlink::LinkGraph &G) {
            size_t graph_size = 0;
            size_t code_size = 0;
            size_t data_size = 0;
            for (auto block : G.blocks()) {
                graph_size += block->getSize();
            }
            for (auto &section : G.sections()) {
                size_t secsize = 0;
                for (auto block : section.blocks()) {
                    secsize += block->getSize();
                }
#if JL_LLVM_VERSION >= 160000
                if ((section.getMemProt() & orc::MemProt::Exec) == orc::MemProt::None) {
#else
                if ((section.getMemProt() & jitlink::MemProt::Exec) == jitlink::MemProt::None) {
#endif
                    data_size += secsize;
                } else {
                    code_size += secsize;
                }
                graph_size += secsize;
            }
            (void) code_size;
            (void) data_size;
            this->total_size.fetch_add(graph_size, std::memory_order_relaxed);
            jl_timing_counter_inc(JL_TIMING_COUNTER_JITSize, graph_size);
            jl_timing_counter_inc(JL_TIMING_COUNTER_JITCodeSize, code_size);
            jl_timing_counter_inc(JL_TIMING_COUNTER_JITDataSize, data_size);
            return Error::success();
        });
    }
};

// replace with [[maybe_unused]] when we get to C++17
#ifdef _COMPILER_GCC_
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#endif

#ifdef _COMPILER_CLANG_
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-function"
#endif

// TODO: Port our memory management optimisations to JITLink instead of using the
// default InProcessMemoryManager.
std::unique_ptr<jitlink::JITLinkMemoryManager> createJITLinkMemoryManager() {
#if JL_LLVM_VERSION < 160000
    return cantFail(orc::MapperJITLinkMemoryManager::CreateWithMapper<orc::InProcessMemoryMapper>());
#else
    return cantFail(orc::MapperJITLinkMemoryManager::CreateWithMapper<orc::InProcessMemoryMapper>(/*Reservation Granularity*/ 16 * 1024 * 1024));
#endif
}

#ifdef _COMPILER_CLANG_
#pragma clang diagnostic pop
#endif

#ifdef _COMPILER_GCC_
#pragma GCC diagnostic pop
#endif
}

class JLEHFrameRegistrar final : public jitlink::EHFrameRegistrar {
public:
    Error registerEHFrames(orc::ExecutorAddrRange EHFrameSection) override {
        register_eh_frames(EHFrameSection.Start.toPtr<uint8_t *>(), static_cast<size_t>(EHFrameSection.size()));
        return Error::success();
    }

    Error deregisterEHFrames(orc::ExecutorAddrRange EHFrameSection) override {
        deregister_eh_frames(EHFrameSection.Start.toPtr<uint8_t *>(), static_cast<size_t>(EHFrameSection.size()));
        return Error::success();
    }
};

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
#if JL_LLVM_VERSION >= 160000
    virtual void reserveAllocationSpace(uintptr_t CodeSize, Align CodeAlign,
                                        uintptr_t RODataSize, Align RODataAlign,
                                        uintptr_t RWDataSize, Align RWDataAlign) override {
        return MemMgr->reserveAllocationSpace(CodeSize, CodeAlign, RODataSize, RODataAlign, RWDataSize, RWDataAlign);
    }
#else
    virtual void reserveAllocationSpace(uintptr_t CodeSize, uint32_t CodeAlign,
                                        uintptr_t RODataSize,
                                        uint32_t RODataAlign,
                                        uintptr_t RWDataSize,
                                        uint32_t RWDataAlign) override {
        return MemMgr->reserveAllocationSpace(CodeSize, CodeAlign, RODataSize, RODataAlign, RWDataSize, RWDataAlign);
    }
#endif
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
namespace {
    static std::unique_ptr<TargetMachine> createTargetMachine() JL_NOTSAFEPOINT {
        TargetOptions options = TargetOptions();

        Triple TheTriple(sys::getProcessTriple());
        // use ELF because RuntimeDyld COFF i686 support didn't exist
        // use ELF because RuntimeDyld COFF X86_64 doesn't seem to work (fails to generate function pointers)?
        bool force_elf = TheTriple.isOSWindows();
#ifdef FORCE_ELF
        force_elf = true;
#endif
        if (force_elf) {
            TheTriple.setObjectFormat(Triple::ELF);
        }
        //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#if defined(MSAN_EMUTLS_WORKAROUND)
        options.EmulatedTLS = true;
        options.ExplicitEmulatedTLS = true;
#endif
        uint32_t target_flags = 0;
        auto target = jl_get_llvm_target(imaging_default(), target_flags);
        auto &TheCPU = target.first;
        SmallVector<std::string, 10> targetFeatures(target.second.begin(), target.second.end());
        std::string errorstr;
        const Target *TheTarget = TargetRegistry::lookupTarget("", TheTriple, errorstr);
        if (!TheTarget) {
            jl_errorf("Internal problem with process triple %s lookup: %s", TheTriple.str().c_str(), errorstr.c_str());
            return nullptr;
        }
        if (jl_processor_print_help || (target_flags & JL_TARGET_UNKNOWN_NAME)) {
            std::unique_ptr<MCSubtargetInfo> MSTI(
                TheTarget->createMCSubtargetInfo(TheTriple.str(), "", ""));
            if (!MSTI->isCPUStringValid(TheCPU)) {
                jl_errorf("Invalid CPU name \"%s\".", TheCPU.c_str());
                return nullptr;
            }
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
        if (TheTriple.isAArch64())
            codemodel = CodeModel::Small;
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
        fixupTM(*TM);
        return std::unique_ptr<TargetMachine>(TM);
    }
} // namespace

namespace {

    typedef NewPM PassManager;

    orc::JITTargetMachineBuilder createJTMBFromTM(TargetMachine &TM, int optlevel) JL_NOTSAFEPOINT {
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

        TMCreator(TargetMachine &TM, int optlevel) JL_NOTSAFEPOINT
            : JTMB(createJTMBFromTM(TM, optlevel)) {}

        std::unique_ptr<TargetMachine> operator()() JL_NOTSAFEPOINT {
            auto TM = cantFail(JTMB.createTargetMachine());
            fixupTM(*TM);
            return TM;
        }
    };

    struct PMCreator {
        orc::JITTargetMachineBuilder JTMB;
        OptimizationLevel O;
        SmallVector<std::function<void()>, 0> &printers;
        std::mutex &llvm_printing_mutex;
        PMCreator(TargetMachine &TM, int optlevel, SmallVector<std::function<void()>, 0> &printers, std::mutex &llvm_printing_mutex) JL_NOTSAFEPOINT
            : JTMB(createJTMBFromTM(TM, optlevel)), O(getOptLevel(optlevel)), printers(printers), llvm_printing_mutex(llvm_printing_mutex) {}

        auto operator()() JL_NOTSAFEPOINT {
            auto TM = cantFail(JTMB.createTargetMachine());
            fixupTM(*TM);
            auto NPM = std::make_unique<NewPM>(std::move(TM), O);
            // TODO this needs to be locked, as different resource pools may add to the printer vector at the same time
            {
                std::lock_guard<std::mutex> lock(llvm_printing_mutex);
                printers.push_back([NPM = NPM.get()]() JL_NOTSAFEPOINT {
                    NPM->printTimers();
                });
            }
            return NPM;
        }
    };

    template<size_t N>
    struct OptimizerT {
        OptimizerT(TargetMachine &TM, SmallVector<std::function<void()>, 0> &printers, std::mutex &llvm_printing_mutex) JL_NOTSAFEPOINT {
            for (size_t i = 0; i < N; i++) {
                PMs[i] = std::make_unique<JuliaOJIT::ResourcePool<std::unique_ptr<PassManager>>>(PMCreator(TM, i, printers, llvm_printing_mutex));
            }
        }

        OptimizerResultT operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
            TSM.withModuleDo([&](Module &M) JL_NOTSAFEPOINT {
                auto PoolIdx = cast<ConstantInt>(cast<ConstantAsMetadata>(M.getModuleFlag("julia.optlevel"))->getValue())->getZExtValue();
                assert(PoolIdx < N && "Invalid optimization pool index");

                uint64_t start_time = 0;

                struct Stat {
                    std::string name;
                    uint64_t insts;
                    uint64_t bbs;

                    void dump(ios_t *stream) JL_NOTSAFEPOINT {
                        ios_printf(stream, "    \"%s\":\n", name.c_str());
                        ios_printf(stream, "        instructions: %u\n", insts);
                        ios_printf(stream, "        basicblocks: %zd\n", bbs);
                    }

                    Stat(Function &F) JL_NOTSAFEPOINT : name(F.getName().str()), insts(F.getInstructionCount()), bbs(countBasicBlocks(F)) {}

                    ~Stat() JL_NOTSAFEPOINT = default;
                };
                SmallVector<Stat, 8> before_stats;
                {
                    if (*jl_ExecutionEngine->get_dump_llvm_opt_stream()) {
                        for (auto &F : M.functions()) {
                            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                                continue;
                            }
                            // Each function is printed as a YAML object with several attributes
                            before_stats.emplace_back(F);
                        }

                        start_time = jl_hrtime();
                    }
                }

                {
                    JL_TIMING(LLVM_JIT, JIT_Opt);
                    //Run the optimization
                    (****PMs[PoolIdx]).run(M);
                    assert(!verifyLLVMIR(M));
                }

                {
                    // Print optimization statistics as a YAML object
                    // Looks like:
                    // -
                    //   before:
                    //     "foo":
                    //       instructions: uint64
                    //       basicblocks: uint64
                    //    "bar":
                    //       instructions: uint64
                    //       basicblocks: uint64
                    //   time_ns: uint64
                    //   optlevel: int
                    //   after:
                    //     "foo":
                    //       instructions: uint64
                    //       basicblocks: uint64
                    //    "bar":
                    //       instructions: uint64
                    //       basicblocks: uint64
                    if (auto stream = *jl_ExecutionEngine->get_dump_llvm_opt_stream()) {
                        uint64_t end_time = jl_hrtime();
                        ios_printf(stream, "- \n");

                        // Print LLVM function statistic _before_ optimization
                        ios_printf(stream, "  before: \n");
                        for (auto &s : before_stats) {
                            s.dump(stream);
                        }
                        ios_printf(stream, "  time_ns: %" PRIu64 "\n", end_time - start_time);
                        ios_printf(stream, "  optlevel: %d\n", PoolIdx);

                        // Print LLVM function statistics _after_ optimization
                        ios_printf(stream, "  after: \n");
                        for (auto &F : M.functions()) {
                            if (F.isDeclaration() || F.getName().startswith("jfptr_")) {
                                continue;
                            }
                            Stat(F).dump(stream);
                        }
                    }
                }
                ++ModulesOptimized;
                switch (PoolIdx) {
                    case 0:
                        ++OptO0;
                        break;
                    case 1:
                        ++OptO1;
                        break;
                    case 2:
                        ++OptO2;
                        break;
                    case 3:
                        ++OptO3;
                        break;
                    default:
                        // Change this if we ever gain other optlevels
                        llvm_unreachable("optlevel is between 0 and 3!");
                }
            });
            return Expected<orc::ThreadSafeModule>{std::move(TSM)};
        }
    private:
        std::array<std::unique_ptr<JuliaOJIT::ResourcePool<std::unique_ptr<PassManager>>>, N> PMs;
    };

    template<size_t N>
    struct CompilerT : orc::IRCompileLayer::IRCompiler {

        CompilerT(orc::IRSymbolMapper::ManglingOptions MO, TargetMachine &TM) JL_NOTSAFEPOINT
            : orc::IRCompileLayer::IRCompiler(MO) {
            for (size_t i = 0; i < N; ++i) {
                TMs[i] = std::make_unique<JuliaOJIT::ResourcePool<std::unique_ptr<TargetMachine>>>(TMCreator(TM, i));
            }
        }

        Expected<std::unique_ptr<MemoryBuffer>> operator()(Module &M) override {
            JL_TIMING(LLVM_JIT, JIT_Compile);
            size_t PoolIdx;
            if (auto opt_level = M.getModuleFlag("julia.optlevel")) {
                PoolIdx = cast<ConstantInt>(cast<ConstantAsMetadata>(opt_level)->getValue())->getZExtValue();
            } else {
                PoolIdx = jl_options.opt_level;
            }
            assert(PoolIdx < N && "Invalid optimization level for compiler!");
            return orc::SimpleCompiler(****TMs[PoolIdx])(M);
        }

        std::array<std::unique_ptr<JuliaOJIT::ResourcePool<std::unique_ptr<TargetMachine>>>, N> TMs;
    };

    struct JITPointersT {

        JITPointersT(SharedBytesT &SharedBytes, std::mutex &Lock) JL_NOTSAFEPOINT
            : SharedBytes(SharedBytes), Lock(Lock) {}

        Expected<orc::ThreadSafeModule> operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
            TSM.withModuleDo([&](Module &M) JL_NOTSAFEPOINT {
                std::lock_guard<std::mutex> locked(Lock);
                for (auto &GV : make_early_inc_range(M.globals())) {
                    if (auto *Shared = getSharedBytes(GV)) {
                        ++InternedGlobals;
                        GV.replaceAllUsesWith(Shared);
                        GV.eraseFromParent();
                    }
                }

                // Windows needs some inline asm to help
                // build unwind tables
                jl_decorate_module(M);
            });
            return std::move(TSM);
        }

    private:
        // optimize memory by turning long strings into memoized copies, instead of
        // making a copy per object file of output.
        // we memoize them using a StringSet with a custom-alignment allocator
        // to ensure they are properly aligned
        Constant *getSharedBytes(GlobalVariable &GV) JL_NOTSAFEPOINT {
            // We could probably technically get away with
            // interning even external linkage globals,
            // as long as they have global unnamedaddr,
            // but currently we shouldn't be emitting those
            // except in imaging mode, and we don't want to
            // do this optimization there.
            if (GV.hasExternalLinkage() || !GV.hasGlobalUnnamedAddr()) {
                return nullptr;
            }
            if (!GV.hasInitializer()) {
                return nullptr;
            }
            if (!GV.isConstant()) {
                return nullptr;
            }
            auto CDS = dyn_cast<ConstantDataSequential>(GV.getInitializer());
            if (!CDS) {
                return nullptr;
            }
            StringRef Data = CDS->getRawDataValues();
            if (Data.size() < 16) {
                // Cutoff, since we don't want to intern small strings
                return nullptr;
            }
            Align Required = GV.getAlign().valueOrOne();
            Align Preferred = MaxAlignedAlloc::alignment(Data.size());
            if (Required > Preferred)
                return nullptr;
            StringRef Interned = SharedBytes.insert(Data).first->getKey();
            assert(llvm::isAddrAligned(Preferred, Interned.data()));
            return literal_static_pointer_val(Interned.data(), GV.getType());
        }

        SharedBytesT &SharedBytes;
        std::mutex &Lock;
    };
}


struct JuliaOJIT::DLSymOptimizer {
    DLSymOptimizer(bool named) JL_NOTSAFEPOINT {
        this->named = named;
#define INIT_RUNTIME_LIBRARY(libname, handle) \
        do { \
            auto libidx = (uintptr_t) libname; \
            if (libidx >= runtime_symbols.size()) { \
                runtime_symbols.resize(libidx + 1); \
            } \
            runtime_symbols[libidx].first = handle; \
        } while (0)

        INIT_RUNTIME_LIBRARY(NULL, jl_RTLD_DEFAULT_handle);
        INIT_RUNTIME_LIBRARY(JL_EXE_LIBNAME, jl_exe_handle);
        INIT_RUNTIME_LIBRARY(JL_LIBJULIA_INTERNAL_DL_LIBNAME, jl_libjulia_internal_handle);
        INIT_RUNTIME_LIBRARY(JL_LIBJULIA_DL_LIBNAME, jl_libjulia_handle);

#undef INIT_RUNTIME_LIBRARY
    }

    void *lookup_symbol(void *libhandle, const char *fname) {
        void *addr;
        jl_dlsym(libhandle, fname, &addr, 0);
        return addr;
    }

    void *lookup(const char *libname, const char *fname) {
        StringRef lib(libname);
        StringRef f(fname);
        std::lock_guard<std::mutex> lock(symbols_mutex);
        auto uit = user_symbols.find(lib);
        if (uit == user_symbols.end()) {
            void *handle = jl_get_library_(libname, 0);
            if (!handle)
                return nullptr;
            uit = user_symbols.insert(std::make_pair(lib, std::make_pair(handle, StringMap<void*>()))).first;
        }
        auto &symmap = uit->second.second;
        auto it = symmap.find(f);
        if (it != symmap.end()) {
            return it->second;
        }
        void *handle = lookup_symbol(uit->second.first, fname);
        symmap[f] = handle;
        return handle;
    }

    void *lookup(uintptr_t libidx, const char *fname) {
        std::lock_guard<std::mutex> lock(symbols_mutex);
        runtime_symbols.resize(std::max(runtime_symbols.size(), libidx + 1));
        auto it = runtime_symbols[libidx].second.find(fname);
        if (it != runtime_symbols[libidx].second.end()) {
            return it->second;
        }
        auto handle = lookup_symbol(runtime_symbols[libidx].first, fname);
        runtime_symbols[libidx].second[fname] = handle;
        return handle;
    }

    void operator()(Module &M) {
        for (auto &GV : M.globals()) {
            auto Name = GV.getName();
            if (Name.startswith("jlplt") && Name.endswith("got")) {
                auto fname = GV.getAttribute("julia.fname").getValueAsString().str();
                void *addr;
                if (GV.hasAttribute("julia.libname")) {
                    auto libname = GV.getAttribute("julia.libname").getValueAsString().str();
                    addr = lookup(libname.data(), fname.data());
                } else {
                    assert(GV.hasAttribute("julia.libidx") && "PLT entry should have either libname or libidx attribute!");
                    auto libidx = (uintptr_t)std::stoull(GV.getAttribute("julia.libidx").getValueAsString().str());
                    addr = lookup(libidx, fname.data());
                }
                if (addr) {
                    Function *Thunk = nullptr;
                    if (!GV.isDeclaration()) {
                        Thunk = cast<Function>(GV.getInitializer()->stripPointerCasts());
                        assert(++Thunk->uses().begin() == Thunk->uses().end() && "Thunk should only have one use in PLT initializer!");
                        assert(Thunk->hasLocalLinkage() && "Thunk should not have non-local linkage!");
                    } else {
                        GV.setLinkage(GlobalValue::PrivateLinkage);
                    }
                    auto init = ConstantExpr::getIntToPtr(ConstantInt::get(M.getDataLayout().getIntPtrType(M.getContext()), (uintptr_t)addr), GV.getValueType());
                    if (named) {
                        auto T = GV.getValueType();
                        assert(T->isPointerTy());
                        #if JL_LLVM_VERSION < 170000
                        if (!T->isOpaquePointerTy()) {
                            T = T->getNonOpaquePointerElementType();
                        }
                        #endif
                        init = GlobalAlias::create(T, 0, GlobalValue::PrivateLinkage, GV.getName() + ".jit", init, &M);
                    }
                    GV.setInitializer(init);
                    GV.setConstant(true);
                    GV.setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
                    if (Thunk) {
                        Thunk->eraseFromParent();
                    }
                }
            }
        }

        for (auto &F : M) {
            for (auto &BB : F) {
                SmallVector<Instruction *, 0> to_delete;
                for (auto &I : make_early_inc_range(BB)) {
                    auto CI = dyn_cast<CallInst>(&I);
                    if (!CI)
                        continue;
                    auto Callee = CI->getCalledFunction();
                    if (!Callee || Callee->getName() != XSTR(jl_load_and_lookup))
                        continue;
                    // Long-winded way of extracting fname without needing a second copy in an attribute
                    auto fname = cast<ConstantDataArray>(cast<GlobalVariable>(CI->getArgOperand(1)->stripPointerCasts())->getInitializer())->getAsCString();
                    auto libarg = CI->getArgOperand(0)->stripPointerCasts();
                    // Should only use in store and phi node
                    // Note that this uses the raw output of codegen,
                    // which is why we can assume this
                    assert(++++CI->use_begin() == CI->use_end());
                    void *addr;
                    if (auto GV = dyn_cast<GlobalVariable>(libarg)) {
                        // Can happen if the library is the empty string, just give up when that happens
                        if (isa<ConstantAggregateZero>(GV->getInitializer()))
                            continue;
                        auto libname = cast<ConstantDataArray>(GV->getInitializer())->getAsCString();
                        addr = lookup(libname.data(), fname.data());
                    } else {
                        // Can happen if we fail the compile time dlfind i.e when we try a symbol that doesn't exist in libc
                        if (dyn_cast<ConstantPointerNull>(libarg))
                            continue;
                        assert(cast<ConstantExpr>(libarg)->getOpcode() == Instruction::IntToPtr && "libarg should be either a global variable or a integer index!");
                        libarg = cast<ConstantExpr>(libarg)->getOperand(0);
                        auto libidx = cast<ConstantInt>(libarg)->getZExtValue();
                        addr = lookup(libidx, fname.data());
                    }
                    if (addr) {
                        auto init = ConstantExpr::getIntToPtr(ConstantInt::get(M.getDataLayout().getIntPtrType(M.getContext()), (uintptr_t)addr), CI->getType());
                        if (named) {
                            auto T = CI->getType();
                            assert(T->isPointerTy());
                            #if JL_LLVM_VERSION < 170000
                            if (!T->isOpaquePointerTy()) {
                                T = T->getNonOpaquePointerElementType();
                            }
                            #endif
                            init = GlobalAlias::create(T, 0, GlobalValue::PrivateLinkage, CI->getName() + ".jit", init, &M);
                        }
                        // DCE and SimplifyCFG will kill the branching structure around
                        // the call, so we don't need to worry about removing everything
                        for (auto user : make_early_inc_range(CI->users())) {
                            if (auto SI = dyn_cast<StoreInst>(user)) {
                                to_delete.push_back(SI);
                            } else {
                                auto PHI = cast<PHINode>(user);
                                PHI->replaceAllUsesWith(init);
                                to_delete.push_back(PHI);
                            }
                        }
                        to_delete.push_back(CI);
                    }
                }
                for (auto I : to_delete) {
                    I->eraseFromParent();
                }
            }
        }
    }

    std::mutex symbols_mutex;
    StringMap<std::pair<void *, StringMap<void *>>> user_symbols;
    SmallVector<std::pair<void *, StringMap<void *>>, 0> runtime_symbols;
    bool named;
};

void optimizeDLSyms(Module &M) {
    JuliaOJIT::DLSymOptimizer(true)(M);
}

void fixupTM(TargetMachine &TM) {
    auto TheTriple = TM.getTargetTriple();
    if (jl_options.opt_level < 2) {
        if (!TheTriple.isARM() && !TheTriple.isPPC64() && !TheTriple.isAArch64())
            TM.setFastISel(true);
        else    // FastISel seems to be buggy Ref #13321
            TM.setFastISel(false);
    }
}

#if JL_LLVM_VERSION < 170000
extern int jl_opaque_ptrs_set;
void SetOpaquePointer(LLVMContext &ctx) {
    if (jl_opaque_ptrs_set)
        return;
#ifndef JL_LLVM_OPAQUE_POINTERS
    ctx.setOpaquePointers(false);
#else
    ctx.setOpaquePointers(true);
#endif
}
#endif

llvm::DataLayout jl_create_datalayout(TargetMachine &TM) {
    // Mark our address spaces as non-integral
    auto jl_data_layout = TM.createDataLayout();
    jl_data_layout.reset(jl_data_layout.getStringRepresentation() + "-ni:10:11:12:13");
    return jl_data_layout;
}

#ifdef _COMPILER_ASAN_ENABLED_
int64_t ___asan_globals_registered;
#endif

JuliaOJIT::JuliaOJIT()
  : TM(createTargetMachine()),
    DL(jl_create_datalayout(*TM)),
    ES(cantFail(orc::SelfExecutorProcessControl::Create())),
    GlobalJD(ES.createBareJITDylib("JuliaGlobals")),
    JD(ES.createBareJITDylib("JuliaOJIT")),
    ExternalJD(ES.createBareJITDylib("JuliaExternal")),
    DLSymOpt(std::make_unique<DLSymOptimizer>(false)),
    ContextPool([](){
        auto ctx = std::make_unique<LLVMContext>();
        #if JL_LLVM_VERSION < 170000
        SetOpaquePointer(*ctx);
        #endif
        return orc::ThreadSafeContext(std::move(ctx));
    }),
#ifdef JL_USE_JITLINK
    MemMgr(createJITLinkMemoryManager()),
    ObjectLayer(ES, *MemMgr),
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
    LockLayer(ObjectLayer),
    CompileLayer(ES, LockLayer, std::make_unique<CompilerT<N_optlevels>>(orc::irManglingOptionsFromTargetOptions(TM->Options), *TM)),
    JITPointersLayer(ES, CompileLayer, orc::IRTransformLayer::TransformFunction(JITPointersT(SharedBytes, RLST_mutex))),
    OptimizeLayer(ES, JITPointersLayer, orc::IRTransformLayer::TransformFunction(OptimizerT<N_optlevels>(*TM, PrintLLVMTimers, llvm_printing_mutex))),
    OptSelLayer(ES, OptimizeLayer, orc::IRTransformLayer::TransformFunction(selectOptLevel)),
    DepsVerifyLayer(ES, OptSelLayer, orc::IRTransformLayer::TransformFunction(validateExternRelocations)),
    ExternalCompileLayer(ES, LockLayer,
        std::make_unique<CompilerT<N_optlevels>>(orc::irManglingOptionsFromTargetOptions(TM->Options), *TM))
{
#ifdef JL_USE_JITLINK
# if defined(LLVM_SHLIB)
    // When dynamically linking against LLVM, use our custom EH frame registration code
    // also used with RTDyld to inform both our and the libc copy of libunwind.
    auto ehRegistrar = std::make_unique<JLEHFrameRegistrar>();
# else
    auto ehRegistrar = std::make_unique<jitlink::InProcessEHFrameRegistrar>();
# endif
    ObjectLayer.addPlugin(std::make_unique<EHFrameRegistrationPlugin>(
        ES, std::move(ehRegistrar)));

    ObjectLayer.addPlugin(std::make_unique<JLDebuginfoPlugin>());
    ObjectLayer.addPlugin(std::make_unique<JLMemoryUsagePlugin>(total_size));
#else
    ObjectLayer.setNotifyLoaded(
        [this](orc::MaterializationResponsibility &MR,
               const object::ObjectFile &Object,
               const RuntimeDyld::LoadedObjectInfo &LO) {
            registerRTDyldJITObject(Object, LO, MemMgr);
        });
#endif
    CompileLayer.setNotifyCompiled(recordDebugTSM);

    std::string ErrorStr;

    // Make sure that libjulia-internal is loaded and placed first in the
    // DynamicLibrary order so that calls to runtime intrinsics are resolved
    // to the correct library when multiple libjulia-*'s have been loaded
    // (e.g. when we `ccall` into a PackageCompiler.jl-created shared library)
    sys::DynamicLibrary libjulia_internal_dylib = sys::DynamicLibrary::addPermanentLibrary(
      jl_libjulia_internal_handle, &ErrorStr);
    if(!ErrorStr.empty())
        report_fatal_error(llvm::Twine("FATAL: unable to dlopen libjulia-internal\n") + ErrorStr);

    // Make sure SectionMemoryManager::getSymbolAddressInProcess can resolve
    // symbols in the program as well. The nullptr argument to the function
    // tells DynamicLibrary to load the program, not a library.
    if (sys::DynamicLibrary::LoadLibraryPermanently(nullptr, &ErrorStr))
        report_fatal_error(llvm::Twine("FATAL: unable to dlopen self\n") + ErrorStr);

    GlobalJD.addGenerator(
      std::make_unique<orc::DynamicLibrarySearchGenerator>(
        libjulia_internal_dylib,
        DL.getGlobalPrefix(),
        orc::DynamicLibrarySearchGenerator::SymbolPredicate()));

    GlobalJD.addGenerator(
      cantFail(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
        DL.getGlobalPrefix())));

    // Resolve non-lock free atomic functions in the libatomic1 library.
    // This is the library that provides support for c11/c++11 atomic operations.
    auto TT = getTargetTriple();
    const char *const libatomic = TT.isOSLinux() || TT.isOSFreeBSD() ?
        "libatomic.so.1" : TT.isOSWindows() ?
        "libatomic-1.dll" : nullptr;
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
    JD.addToLinkOrder(ExternalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
    ExternalJD.addToLinkOrder(GlobalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
    ExternalJD.addToLinkOrder(JD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);

    orc::SymbolAliasMap jl_crt = {
        // Float16 conversion routines
#if defined(_CPU_X86_64_) && defined(_OS_DARWIN_) && JL_LLVM_VERSION >= 160000
        // LLVM 16 reverted to soft-float ABI for passing half on x86_64 Darwin
        // https://github.com/llvm/llvm-project/commit/2bcf51c7f82ca7752d1bba390a2e0cb5fdd05ca9
        { mangle("__gnu_h2f_ieee"), { mangle("julia_half_to_float"),  JITSymbolFlags::Exported } },
        { mangle("__extendhfsf2"),  { mangle("julia_half_to_float"),  JITSymbolFlags::Exported } },
        { mangle("__gnu_f2h_ieee"), { mangle("julia_float_to_half"),  JITSymbolFlags::Exported } },
        { mangle("__truncsfhf2"),   { mangle("julia_float_to_half"),  JITSymbolFlags::Exported } },
        { mangle("__truncdfhf2"),   { mangle("julia_double_to_half"), JITSymbolFlags::Exported } },
#else
        { mangle("__gnu_h2f_ieee"), { mangle("julia__gnu_h2f_ieee"),  JITSymbolFlags::Exported } },
        { mangle("__extendhfsf2"),  { mangle("julia__gnu_h2f_ieee"),  JITSymbolFlags::Exported } },
        { mangle("__gnu_f2h_ieee"), { mangle("julia__gnu_f2h_ieee"),  JITSymbolFlags::Exported } },
        { mangle("__truncsfhf2"),   { mangle("julia__gnu_f2h_ieee"),  JITSymbolFlags::Exported } },
        { mangle("__truncdfhf2"),   { mangle("julia__truncdfhf2"),    JITSymbolFlags::Exported } },
#endif
        // BFloat16 conversion routines
        { mangle("__truncsfbf2"),   { mangle("julia__truncsfbf2"),    JITSymbolFlags::Exported } },
        { mangle("__truncdfbf2"),   { mangle("julia__truncdfbf2"),    JITSymbolFlags::Exported } },
    };
    cantFail(GlobalJD.define(orc::symbolAliases(jl_crt)));

#ifdef MSAN_EMUTLS_WORKAROUND
    orc::SymbolMap msan_crt;
    #if JL_LLVM_VERSION >= 170000
    msan_crt[mangle("__emutls_get_address")] = {ExecutorAddr::fromPtr(msan_workaround::getTLSAddress), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_param_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::param))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_param_origin_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::param_origin))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_retval_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::retval))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_retval_origin_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::retval_origin))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_va_arg_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_va_arg_origin_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg_origin))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_va_arg_overflow_size_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg_overflow_size))), JITSymbolFlags::Exported};
    msan_crt[mangle("__emutls_v.__msan_origin_tls")] = {ExecutorAddr::fromPtr(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::origin))), JITSymbolFlags::Exported};
    #else
    msan_crt[mangle("__emutls_get_address")] = JITEvaluatedSymbol::fromPointer(msan_workaround::getTLSAddress, JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_param_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::param)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_param_origin_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::param_origin)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_retval_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::retval)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_retval_origin_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::retval_origin)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_va_arg_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_va_arg_origin_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg_origin)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_va_arg_overflow_size_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::va_arg_overflow_size)), JITSymbolFlags::Exported);
    msan_crt[mangle("__emutls_v.__msan_origin_tls")] = JITEvaluatedSymbol::fromPointer(
        reinterpret_cast<void *>(static_cast<uintptr_t>(msan_workaround::MSanTLS::origin)), JITSymbolFlags::Exported);
    #endif
    cantFail(GlobalJD.define(orc::absoluteSymbols(msan_crt)));
#endif
#ifdef _COMPILER_ASAN_ENABLED_
    orc::SymbolMap asan_crt;
    #if JL_LLVM_VERSION >= 170000
    asan_crt[mangle("___asan_globals_registered")] = {ExecutorAddr::fromPtr(&___asan_globals_registered), JITSymbolFlags::Exported};
    #else
    asan_crt[mangle("___asan_globals_registered")] = JITEvaluatedSymbol::fromPointer(&___asan_globals_registered, JITSymbolFlags::Exported);
    #endif
    cantFail(JD.define(orc::absoluteSymbols(asan_crt)));
#endif
}

JuliaOJIT::~JuliaOJIT() = default;

orc::SymbolStringPtr JuliaOJIT::mangle(StringRef Name)
{
    std::string MangleName = getMangledName(Name);
    return ES.intern(MangleName);
}

void JuliaOJIT::addGlobalMapping(StringRef Name, uint64_t Addr)
{
    #if JL_LLVM_VERSION >= 170000
    cantFail(JD.define(orc::absoluteSymbols({{mangle(Name), {ExecutorAddr::fromPtr((void*)Addr), JITSymbolFlags::Exported}}})));
    #else
    cantFail(JD.define(orc::absoluteSymbols({{mangle(Name), JITEvaluatedSymbol::fromPointer((void*)Addr)}})));
    #endif
}

void JuliaOJIT::addModule(orc::ThreadSafeModule TSM)
{
    JL_TIMING(LLVM_JIT, JIT_Total);
    ++ModulesAdded;
    orc::SymbolLookupSet NewExports;
    orc::ThreadSafeModule CurrentlyCompiling;
    TSM.withModuleDo([&](Module &M) JL_NOTSAFEPOINT {
        for (auto &F : M.global_values()) {
            if (!F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                auto Name = ES.intern(getMangledName(F.getName()));
                NewExports.add(std::move(Name));
            }
        }
        assert(!verifyLLVMIR(M));
        auto jit_debug_tsm_addr = ConstantInt::get(Type::getIntNTy(M.getContext(), sizeof(void*) * CHAR_BIT), (uintptr_t) &CurrentlyCompiling);
        M.addModuleFlag(Module::Error, "julia.__jit_debug_tsm_addr", jit_debug_tsm_addr);
    });

    // TODO: what is the performance characteristics of this?
    auto Err = DepsVerifyLayer.add(JD, std::move(TSM));
    if (Err) {
        ES.reportError(std::move(Err));
        errs() << "Failed to add module to JIT!\n";
        if (CurrentlyCompiling) {
            CurrentlyCompiling.withModuleDo([](Module &M) JL_NOTSAFEPOINT { errs() << "Dumping failing module\n" << M << "\n"; });
        } else {
            errs() << "Module unavailable to be printed\n";
        }
        abort();
    }
    // force eager compilation (for now), due to memory management specifics
    // (can't handle compilation recursion)
    auto Lookups = ES.lookup({{&JD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly}}, NewExports);
    if (!Lookups) {
        ES.reportError(Lookups.takeError());
        errs() << "Failed to lookup symbols in module!";
        if (CurrentlyCompiling) {
            CurrentlyCompiling.withModuleDo([](Module &M) JL_NOTSAFEPOINT { errs() << "Dumping failing module\n" << M << "\n"; });
        } else {
            errs() << "Module unavailable to be printed\n";
        }
    }
    for (auto &Sym : *Lookups) {
        #if JL_LLVM_VERSION >= 170000
        assert(Sym.second.getAddress());
        #else
        assert(Sym.second);
        #endif
        (void) Sym;
    }
}

Error JuliaOJIT::addExternalModule(orc::JITDylib &JD, orc::ThreadSafeModule TSM, bool ShouldOptimize)
{
    if (auto Err = TSM.withModuleDo([&](Module &M) JL_NOTSAFEPOINT -> Error
            {
            if (M.getDataLayout().isDefault())
                M.setDataLayout(DL);
            if (M.getDataLayout() != DL)
                return make_error<StringError>(
                    "Added modules have incompatible data layouts: " +
                    M.getDataLayout().getStringRepresentation() + " (module) vs " +
                    DL.getStringRepresentation() + " (jit)",
                inconvertibleErrorCode());

            return Error::success();
            }))
        return Err;
    return ExternalCompileLayer.add(JD.getDefaultResourceTracker(), std::move(TSM));
}

Error JuliaOJIT::addObjectFile(orc::JITDylib &JD, std::unique_ptr<MemoryBuffer> Obj) {
    assert(Obj && "Can not add null object");
    return LockLayer.add(JD.getDefaultResourceTracker(), std::move(Obj));
}

#if JL_LLVM_VERSION >= 170000
Expected<ExecutorSymbolDef> JuliaOJIT::findSymbol(StringRef Name, bool ExportedSymbolsOnly)
{
    orc::JITDylib* SearchOrders[3] = {&JD, &GlobalJD, &ExternalJD};
    ArrayRef<orc::JITDylib*> SearchOrder = ArrayRef<orc::JITDylib*>(&SearchOrders[0], ExportedSymbolsOnly ? 3 : 1);
    auto Sym = ES.lookup(SearchOrder, Name);
    return Sym;
}

Expected<ExecutorSymbolDef> JuliaOJIT::findUnmangledSymbol(StringRef Name)
{
    return findSymbol(getMangledName(Name), true);
}

Expected<ExecutorSymbolDef> JuliaOJIT::findExternalJDSymbol(StringRef Name, bool ExternalJDOnly)
{
    orc::JITDylib* SearchOrders[3] = {&ExternalJD, &GlobalJD, &JD};
    ArrayRef<orc::JITDylib*> SearchOrder = ArrayRef<orc::JITDylib*>(&SearchOrders[0], ExternalJDOnly ? 1 : 3);
    auto Sym = ES.lookup(SearchOrder, getMangledName(Name));
    return Sym;
}

uint64_t JuliaOJIT::getGlobalValueAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(std::move(addr)).getAddress().getValue();
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(std::move(addr)).getAddress().getValue();
}
#else
JL_JITSymbol JuliaOJIT::findSymbol(StringRef Name, bool ExportedSymbolsOnly)
{
    orc::JITDylib* SearchOrders[3] = {&JD, &GlobalJD, &ExternalJD};
    ArrayRef<orc::JITDylib*> SearchOrder = ArrayRef<orc::JITDylib*>(&SearchOrders[0], ExportedSymbolsOnly ? 3 : 1);
    auto Sym = ES.lookup(SearchOrder, Name);
    if (Sym)
        return *Sym;
    return Sym.takeError();
}

JL_JITSymbol JuliaOJIT::findUnmangledSymbol(StringRef Name)
{
    return findSymbol(getMangledName(Name), true);
}

Expected<JL_JITSymbol> JuliaOJIT::findExternalJDSymbol(StringRef Name, bool ExternalJDOnly)
{
    orc::JITDylib* SearchOrders[3] = {&ExternalJD, &GlobalJD, &JD};
    ArrayRef<orc::JITDylib*> SearchOrder = ArrayRef<orc::JITDylib*>(&SearchOrders[0], ExternalJDOnly ? 1 : 3);
    auto Sym = ES.lookup(SearchOrder, getMangledName(Name));
    return Sym;
}

uint64_t JuliaOJIT::getGlobalValueAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(addr).getAddress();
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return cantFail(addr).getAddress();
}
#endif

StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst)
{
    std::lock_guard<std::mutex> lock(RLST_mutex);
    assert(Addr != (uint64_t)&jl_fptr_wait_for_compiled);
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
extern "C" orc::shared::CWrapperFunctionResult
llvm_orc_registerJITLoaderGDBAllocAction(const char *Data, size_t Size);

void JuliaOJIT::enableJITDebuggingSupport()
{
    orc::SymbolMap GDBFunctions;
    #if JL_LLVM_VERSION >= 170000
    GDBFunctions[mangle("llvm_orc_registerJITLoaderGDBAllocAction")] = {ExecutorAddr::fromPtr(&llvm_orc_registerJITLoaderGDBAllocAction), JITSymbolFlags::Exported | JITSymbolFlags::Callable};
    GDBFunctions[mangle("llvm_orc_registerJITLoaderGDBWrapper")] = {ExecutorAddr::fromPtr(&llvm_orc_registerJITLoaderGDBWrapper), JITSymbolFlags::Exported | JITSymbolFlags::Callable};
    #else
    GDBFunctions[mangle("llvm_orc_registerJITLoaderGDBAllocAction")] = JITEvaluatedSymbol::fromPointer(&llvm_orc_registerJITLoaderGDBAllocAction, JITSymbolFlags::Exported | JITSymbolFlags::Callable);
    GDBFunctions[mangle("llvm_orc_registerJITLoaderGDBWrapper")] = JITEvaluatedSymbol::fromPointer(&llvm_orc_registerJITLoaderGDBWrapper, JITSymbolFlags::Exported | JITSymbolFlags::Callable);
    #endif
    cantFail(JD.define(orc::absoluteSymbols(GDBFunctions)));
    if (TM->getTargetTriple().isOSBinFormatMachO())
        ObjectLayer.addPlugin(cantFail(orc::GDBJITDebugInfoRegistrationPlugin::Create(ES, JD, TM->getTargetTriple())));
#ifndef _COMPILER_ASAN_ENABLED_ // TODO: Fix duplicated sections spam #51794
    else if (TM->getTargetTriple().isOSBinFormatELF())
        //EPCDebugObjectRegistrar doesn't take a JITDylib, so we have to directly provide the call address
        ObjectLayer.addPlugin(std::make_unique<orc::DebugObjectManagerPlugin>(ES, std::make_unique<orc::EPCDebugObjectRegistrar>(ES, orc::ExecutorAddr::fromPtr(&llvm_orc_registerJITLoaderGDBWrapper))));
#endif
}
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
    return total_size.load(std::memory_order_relaxed);
}
#else
size_t getRTDyldMemoryManagerTotalBytes(RTDyldMemoryManager *mm) JL_NOTSAFEPOINT;

size_t JuliaOJIT::getTotalBytes() const
{
    return getRTDyldMemoryManagerTotalBytes(MemMgr.get());
}
#endif

void JuliaOJIT::printTimers()
{
    for (auto &printer : PrintLLVMTimers) {
        printer();
    }
    reportAndResetTimings();
}

void JuliaOJIT::optimizeDLSyms(Module &M) {
    (*DLSymOpt)(M);
}

JuliaOJIT *jl_ExecutionEngine;

// destructively move the contents of src into dest
// this assumes that the targets of the two modules are the same
// including the DataLayout and ModuleFlags (for example)
// and that there is no module-level assembly
// Comdat is also removed, since the JIT doesn't need it
void jl_merge_module(orc::ThreadSafeModule &destTSM, orc::ThreadSafeModule srcTSM)
{
    ++ModulesMerged;
    destTSM.withModuleDo([&](Module &dest) JL_NOTSAFEPOINT {
        srcTSM.withModuleDo([&](Module &src) JL_NOTSAFEPOINT {
            assert(&dest != &src && "Cannot merge module with itself!");
            assert(&dest.getContext() == &src.getContext() && "Cannot merge modules with different contexts!");
            assert(dest.getDataLayout() == src.getDataLayout() && "Cannot merge modules with different data layouts!");
            assert(dest.getTargetTriple() == src.getTargetTriple() && "Cannot merge modules with different target triples!");

            for (auto &SG : make_early_inc_range(src.globals())) {
                GlobalVariable *dG = cast_or_null<GlobalVariable>(dest.getNamedValue(SG.getName()));
                if (SG.hasLocalLinkage()) {
                    dG = nullptr;
                }
                // Replace a declaration with the definition:
                if (dG && !dG->hasLocalLinkage()) {
                    if (SG.isDeclaration()) {
                        SG.replaceAllUsesWith(dG);
                        SG.eraseFromParent();
                        continue;
                    }
                    //// If we start using llvm.used, we need to enable and test this
                    //else if (!dG->isDeclaration() && dG->hasAppendingLinkage() && SG.hasAppendingLinkage()) {
                    //    auto *dCA = cast<ConstantArray>(dG->getInitializer());
                    //    auto *sCA = cast<ConstantArray>(SG.getInitializer());
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
                    //    SG.replaceAllUsesWith(GV);
                    //    dG->replaceAllUsesWith(GV);
                    //    GV->takeName(SG);
                    //    SG.eraseFromParent();
                    //    dG->eraseFromParent();
                    //    continue;
                    //}
                    else {
                        assert(dG->isDeclaration() || dG->getInitializer() == SG.getInitializer());
                        dG->replaceAllUsesWith(&SG);
                        dG->eraseFromParent();
                    }
                }
                // Reparent the global variable:
                SG.removeFromParent();
                #if JL_LLVM_VERSION >= 170000
                dest.insertGlobalVariable(&SG);
                #else
                dest.getGlobalList().push_back(&SG);
                #endif
                // Comdat is owned by the Module
                SG.setComdat(nullptr);
            }

            for (auto &SG : make_early_inc_range(src)) {
                Function *dG = cast_or_null<Function>(dest.getNamedValue(SG.getName()));
                if (SG.hasLocalLinkage()) {
                    dG = nullptr;
                }
                // Replace a declaration with the definition:
                if (dG && !dG->hasLocalLinkage()) {
                    if (SG.isDeclaration()) {
                        SG.replaceAllUsesWith(dG);
                        SG.eraseFromParent();
                        continue;
                    }
                    else {
                        assert(dG->isDeclaration());
                        dG->replaceAllUsesWith(&SG);
                        dG->eraseFromParent();
                    }
                }
                // Reparent the global variable:
                SG.removeFromParent();
                dest.getFunctionList().push_back(&SG);
                // Comdat is owned by the Module
                SG.setComdat(nullptr);
            }

            for (auto &SG : make_early_inc_range(src.aliases())) {
                GlobalAlias *dG = cast_or_null<GlobalAlias>(dest.getNamedValue(SG.getName()));
                if (SG.hasLocalLinkage()) {
                    dG = nullptr;
                }
                if (dG && !dG->hasLocalLinkage()) {
                    if (!dG->isDeclaration()) { // aliases are always definitions, so this test is reversed from the above two
                        SG.replaceAllUsesWith(dG);
                        SG.eraseFromParent();
                        continue;
                    }
                    else {
                        dG->replaceAllUsesWith(&SG);
                        dG->eraseFromParent();
                    }
                }
                SG.removeFromParent();
                #if JL_LLVM_VERSION >= 170000
                dest.insertAlias(&SG);
                #else
                dest.getAliasList().push_back(&SG);
                #endif
            }

            // metadata nodes need to be explicitly merged not just copied
            // so there are special passes here for each known type of metadata
            NamedMDNode *sNMD = src.getNamedMetadata("llvm.dbg.cu");
            if (sNMD) {
                NamedMDNode *dNMD = dest.getOrInsertNamedMetadata("llvm.dbg.cu");
                for (MDNode *I : sNMD->operands()) {
                    dNMD->addOperand(I);
                }
            }
        });
    });
}

//TargetMachine pass-through methods

std::unique_ptr<TargetMachine> JuliaOJIT::cloneTargetMachine() const
{
    auto NewTM = std::unique_ptr<TargetMachine>(getTarget()
        .createTargetMachine(
            getTargetTriple().str(),
            getTargetCPU(),
            getTargetFeatureString(),
            getTargetOptions(),
            TM->getRelocationModel(),
            TM->getCodeModel(),
            TM->getOptLevel()));
    fixupTM(*NewTM);
    return NewTM;
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
    auto TT = Triple(M.getTargetTriple());
    if (TT.isOSWindows() && TT.getArch() == Triple::x86_64) {
        // Add special values used by debuginfo to build the UnwindData table registration for Win64
        // This used to be GV, but with https://reviews.llvm.org/D100944 we no longer can emit GV into `.text`
        // TODO: The data is set in debuginfo.cpp but it should be okay to actually emit it here.
        M.appendModuleInlineAsm("\
    .section .text                  \n\
    .type   __UnwindData,@object    \n\
    .p2align        2, 0x90         \n\
    __UnwindData:                   \n\
        .zero   12                  \n\
        .size   __UnwindData, 12    \n\
                                    \n\
        .type   __catchjmp,@object  \n\
        .p2align        2, 0x90     \n\
    __catchjmp:                     \n\
        .zero   12                  \n\
        .size   __catchjmp, 12");
    }
}

// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
static int jl_add_to_ee(
        orc::ThreadSafeModule &M,
        const StringMap<orc::ThreadSafeModule*> &NewExports,
        DenseMap<orc::ThreadSafeModule*, int> &Queued,
        SmallVectorImpl<orc::ThreadSafeModule*> &Stack)
{
    // First check if the TSM is empty (already compiled)
    if (!M)
        return 0;
    // Next check and record if it is on the stack somewhere
    {
        auto &Id = Queued[&M];
        if (Id)
            return Id;
        Stack.push_back(&M);
        Id = Stack.size();
    }
    // Finally work out the SCC
    int depth = Stack.size();
    int MergeUp = depth;
    SmallVector<orc::ThreadSafeModule*, 0> Children;
    M.withModuleDo([&](Module &m) JL_NOTSAFEPOINT {
        for (auto &F : m.global_objects()) {
            if (F.isDeclaration() && F.getLinkage() == GlobalValue::ExternalLinkage) {
                auto Callee = NewExports.find(F.getName());
                if (Callee != NewExports.end()) {
                    auto *CM = Callee->second;
                    if (*CM && CM != &M) {
                        auto Down = Queued.find(CM);
                        if (Down != Queued.end())
                            MergeUp = std::min(MergeUp, Down->second);
                        else
                            Children.push_back(CM);
                    }
                }
            }
        }
    });
    assert(MergeUp > 0);
    for (auto *CM : Children) {
        int Down = jl_add_to_ee(*CM, NewExports, Queued, Stack);
        assert(Down <= (int)Stack.size());
        if (Down)
            MergeUp = std::min(MergeUp, Down);
    }
    if (MergeUp < depth)
        return MergeUp;
    while (1) {
        // Not in a cycle (or at the top of it)
        // remove SCC state and merge every CM from the cycle into M
        orc::ThreadSafeModule *CM = Stack.back();
        auto it = Queued.find(CM);
        assert(it->second == (int)Stack.size());
        Queued.erase(it);
        Stack.pop_back();
        if ((int)Stack.size() < depth) {
            assert(&M == CM);
            break;
        }
        jl_merge_module(M, std::move(*CM));
    }
    jl_ExecutionEngine->addModule(std::move(M));
    return 0;
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

extern "C" JL_DLLEXPORT_CODEGEN
size_t jl_jit_total_bytes_impl(void)
{
    return jl_ExecutionEngine->getTotalBytes();
}
