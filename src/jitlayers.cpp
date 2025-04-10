// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include <stdint.h>
#include <string>

#include "llvm/IR/Mangler.h"
#include <llvm/ADT/Statistic.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/DebugObjectManagerPlugin.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderGDB.h>
#if JL_LLVM_VERSION >= 200000
#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#endif
#if JL_LLVM_VERSION >= 180000
#include <llvm/ExecutionEngine/Orc/Debugging/DebugInfoSupport.h>
#include <llvm/ExecutionEngine/Orc/Debugging/PerfSupportPlugin.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderPerf.h>
#endif
#if JL_LLVM_VERSION >= 190000
#include <llvm/ExecutionEngine/Orc/Debugging/VTuneSupportPlugin.h>
#include <llvm/ExecutionEngine/Orc/TargetProcess/JITLoaderVTune.h>
#endif
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
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Object/SymbolSize.h>

using namespace llvm;

#include "jitlayers.h"
#include "julia_assert.h"
#include "processor.h"

#if JL_LLVM_VERSION >= 180000
# include <llvm/ExecutionEngine/Orc/Debugging/DebuggerSupportPlugin.h>
#else
# include <llvm/ExecutionEngine/Orc/DebuggerSupportPlugin.h>
#endif
# include <llvm/ExecutionEngine/JITLink/EHFrameSupport.h>
# include <llvm/ExecutionEngine/JITLink/JITLinkMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/MapperJITLinkMemoryManager.h>
# include <llvm/ExecutionEngine/SectionMemoryManager.h>

#define DEBUG_TYPE "julia_jitlayers"

STATISTIC(LinkedGlobals, "Number of globals linked");
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

#ifdef _OS_OPENBSD_
extern "C" {
    __int128 __divti3(__int128, __int128);
    __int128 __modti3(__int128, __int128);
    unsigned __int128 __udivti3(unsigned __int128, unsigned __int128);
    unsigned __int128 __umodti3(unsigned __int128, unsigned __int128);
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

static void jl_decorate_module(Module &M) JL_NOTSAFEPOINT;

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

// convert local roots into global roots, if they are needed
static void jl_optimize_roots(jl_codegen_params_t &params, jl_method_instance_t *mi, Module &M)
{
    JL_GC_PROMISE_ROOTED(params.temporary_roots); // rooted by caller
    if (jl_array_dim0(params.temporary_roots) == 0)
        return;
    jl_method_t *m = mi->def.method;
    if (jl_is_method(m))
        // the method might have a root for this already; use it if so
        JL_LOCK(&m->writelock);
    for (size_t i = 0; i < jl_array_dim0(params.temporary_roots); i++) {
        jl_value_t *val = jl_array_ptr_ref(params.temporary_roots, i);
        auto ref = params.global_targets.find((void*)val);
        if (ref == params.global_targets.end())
            continue;
        auto get_global_root = [val, m]() {
            if (jl_is_globally_rooted(val))
                return val;
            if (jl_is_method(m) && m->roots) {
                size_t j, len = jl_array_dim0(m->roots);
                for (j = 0; j < len; j++) {
                    jl_value_t *mval = jl_array_ptr_ref(m->roots, j);
                    if (jl_egal(mval, val)) {
                        return mval;
                    }
                }
            }
            return jl_as_global_root(val, 1);
        };
        jl_value_t *mval = get_global_root();
        if (mval != val) {
            GlobalVariable *GV = ref->second;
            params.global_targets.erase(ref);
            auto mref = params.global_targets.find((void*)mval);
            if (mref != params.global_targets.end()) {
                GV->replaceAllUsesWith(mref->second);
                GV->eraseFromParent();
            }
            else {
                params.global_targets[(void*)mval] = GV;
            }
        }
    }
    if (jl_is_method(m))
        JL_UNLOCK(&m->writelock);
}

static void finish_params(Module *M, jl_codegen_params_t &params, SmallVector<orc::ThreadSafeModule,0> &sharedmodules) JL_NOTSAFEPOINT
{
    if (params._shared_module) {
        sharedmodules.push_back(orc::ThreadSafeModule(std::move(params._shared_module), params.tsctx));
    }

    // In imaging mode, we can't inline global variable initializers in order to preserve
    // the fiction that we don't know what loads from the global will return. Thus, we
    // need to emit a separate module for the globals before any functions are compiled,
    // to ensure that the globals are defined when they are compiled.
    if (jl_options.image_codegen) {
        if (!params.global_targets.empty()) {
            void **globalslots = new void*[params.global_targets.size()];
            void **slot = globalslots;
            for (auto &global : params.global_targets) {
                auto GV = global.second;
                *slot = global.first;
                jl_ExecutionEngine->addGlobalMapping(GV->getName(), (uintptr_t)slot);
                slot++;
            }
#ifdef __clang_analyzer__
            static void **leaker = globalslots; // for the purpose of the analyzer, we need to expressly leak this variable or it thinks we forgot to free it
#endif
        }
    }
    else {
        StringMap<void*> NewGlobals;
        for (auto &global : params.global_targets) {
            NewGlobals[global.second->getName()] = global.first;
        }
        for (auto &GV : M->globals()) {
            auto InitValue = NewGlobals.find(GV.getName());
            if (InitValue != NewGlobals.end()) {
                jl_link_global(&GV, InitValue->second);
            }
        }
    }
}

extern "C" JL_DLLEXPORT_CODEGEN
void *jl_jit_abi_converter_impl(jl_task_t *ct, void *unspecialized, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, int specsig,
                                jl_code_instance_t *codeinst, jl_callptr_t invoke, void *target, int target_specsig)
{
    if (codeinst == nullptr && unspecialized != nullptr)
        return unspecialized;
    orc::ThreadSafeModule result_m;
    std::string gf_thunk_name;
    {
        jl_codegen_params_t params(std::make_unique<LLVMContext>(), jl_ExecutionEngine->getDataLayout(), jl_ExecutionEngine->getTargetTriple()); // Locks the context
        params.getContext().setDiscardValueNames(true);
        params.cache = true;
        params.imaging_mode = 0;
        result_m = jl_create_ts_module("gfthunk", params.tsctx, params.DL, params.TargetTriple);
        Module *M = result_m.getModuleUnlocked();
        if (target) {
            Value *llvmtarget = literal_static_pointer_val((void*)target, PointerType::get(M->getContext(), 0));
            gf_thunk_name = emit_abi_converter(M, params, declrt, sigt, nargs, specsig, codeinst, llvmtarget, target_specsig);
        }
        else if (invoke == jl_fptr_const_return_addr) {
            gf_thunk_name = emit_abi_constreturn(M, params, declrt, sigt, nargs, specsig, codeinst->rettype_const);
        }
        else {
            Value *llvminvoke = invoke ? literal_static_pointer_val((void*)invoke, PointerType::get(M->getContext(), 0)) : nullptr;
            gf_thunk_name = emit_abi_dispatcher(M, params, declrt, sigt, nargs, specsig, codeinst, llvminvoke);
        }
        SmallVector<orc::ThreadSafeModule,0> sharedmodules;
        finish_params(M, params, sharedmodules);
        assert(sharedmodules.empty());
    }
    int8_t gc_state = jl_gc_safe_enter(ct->ptls);
    jl_ExecutionEngine->addModule(std::move(result_m));
    uintptr_t Addr = jl_ExecutionEngine->getFunctionAddress(gf_thunk_name);
    jl_gc_safe_leave(ct->ptls, gc_state);
    assert(Addr);
    return (void*)Addr;
}


  // lock for places where only single threaded behavior is implemented, so we need GC support
static jl_mutex_t jitlock;
  // locks for adding external code to the JIT atomically
static std::mutex extern_c_lock;
  // locks and barriers for this state
static std::mutex engine_lock;
static std::condition_variable engine_wait;
static int threads_in_compiler_phase;
  // the TSM for each codeinst
static SmallVector<orc::ThreadSafeModule,0> sharedmodules;
static DenseMap<jl_code_instance_t*, orc::ThreadSafeModule> emittedmodules;
  // the invoke and specsig function names in the JIT
static DenseMap<jl_code_instance_t*, jl_llvm_functions_t> invokenames;
  // everything that any thread wants to compile right now
static DenseSet<jl_code_instance_t*> compileready;
  // everything that any thread has compiled recently
static DenseSet<jl_code_instance_t*> linkready;
  // a map from a codeinst to the outgoing edges needed before linking it
static DenseMap<jl_code_instance_t*, SmallVector<jl_code_instance_t*,0>> complete_graph;
  // the state for each codeinst and the number of unresolved edges (we don't
  // really need this once JITLink is available everywhere, since every module
  // is automatically complete, and we can emit any required fixups later as a
  // separate module)
static DenseMap<jl_code_instance_t*, std::tuple<jl_codegen_params_t, int>> incompletemodules;
  // the set of incoming unresolved edges resolved by a codeinstance
static DenseMap<jl_code_instance_t*, SmallVector<jl_code_instance_t*,0>> incomplete_rgraph;

// Lock hierarchy here:
//   jitlock is outermost, can contain others and allows GC
//   engine_lock is next
//   ThreadSafeContext locks are next, they should not be nested (unless engine_lock is also held, but this may make TSAN sad anyways)
//   extern_c_lock is next
//   jl_ExecutionEngine internal locks are exclusive to this list, since OrcJIT promises to never hold a lock over a materialization unit:
//        construct a query object from a query set and query handler
//        lock the session
//        lodge query against requested symbols, collect required materializers (if any)
//        unlock the session
//        dispatch materializers (if any)
//     However, this guarantee relies on Julia releasing all TSC locks before causing any materialization units to be dispatched
//     as materialization may need to acquire TSC locks.


static int jl_analyze_workqueue(jl_code_instance_t *callee, jl_codegen_params_t &params, bool forceall=false) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER
{
    jl_task_t *ct = jl_current_task;
    decltype(params.workqueue) edges;
    std::swap(params.workqueue, edges);
    for (auto &it : edges) {
        jl_code_instance_t *codeinst = it.first;
        JL_GC_PROMISE_ROOTED(codeinst);
        auto &proto = it.second;
        // try to emit code for this item from the workqueue
        StringRef invokeName = "";
        StringRef preal_decl = "";
        bool preal_specsig = false;
        jl_callptr_t invoke = nullptr;
        bool isedge = false;
        assert(params.cache);
        // Checking the cache here is merely an optimization and not strictly required
        // But it must be consistent with the following invokenames lookup, which is protected by the engine_lock
        uint8_t specsigflags;
        void *fptr;
        void jl_read_codeinst_invoke(jl_code_instance_t *ci, uint8_t *specsigflags, jl_callptr_t *invoke, void **specptr, int waitcompile) JL_NOTSAFEPOINT; // declare it is not a safepoint (or deadlock) in this file due to 0 parameter
        jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke, &fptr, 0);
        //if (specsig ? specsigflags & 0b1 : invoke == jl_fptr_args_addr)
        if (invoke == jl_fptr_args_addr) {
            preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, invoke, codeinst);
        }
        else if (specsigflags & 0b1) {
            preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, invoke, codeinst);
            preal_specsig = true;
        }
        bool force = forceall || invoke != nullptr;
        if (preal_decl.empty()) {
            auto it = invokenames.find(codeinst);
            if (it != invokenames.end()) {
                auto &decls = it->second;
                invokeName = decls.functionObject;
                if (decls.functionObject == "jl_fptr_args") {
                    preal_decl = decls.specFunctionObject;
                    isedge = true;
                }
                else if (decls.functionObject != "jl_fptr_sparam" && decls.functionObject != "jl_f_opaque_closure_call") {
                    preal_decl = decls.specFunctionObject;
                    preal_specsig = true;
                    isedge = true;
                }
                force = true;
            }
        }
        if (preal_decl.empty()) {
            // there may be an equivalent method already compiled (or at least registered with the JIT to compile), in which case we should be using that instead
            jl_code_instance_t *compiled_ci = jl_get_ci_equiv(codeinst, 0);
            if (compiled_ci != codeinst) {
                codeinst = compiled_ci;
                uint8_t specsigflags;
                void *fptr;
                jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke, &fptr, 0);
                //if (specsig ? specsigflags & 0b1 : invoke == jl_fptr_args_addr)
                if (invoke == jl_fptr_args_addr) {
                    preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, invoke, codeinst);
                }
                else if (specsigflags & 0b1) {
                    preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, invoke, codeinst);
                    preal_specsig = true;
                }
                if (preal_decl.empty()) {
                    auto it = invokenames.find(codeinst);
                    if (it != invokenames.end()) {
                        auto &decls = it->second;
                        invokeName = decls.functionObject;
                        if (decls.functionObject == "jl_fptr_args") {
                            preal_decl = decls.specFunctionObject;
                            isedge = true;
                        }
                        else if (decls.functionObject != "jl_fptr_sparam" && decls.functionObject != "jl_f_opaque_closure_call") {
                            preal_decl = decls.specFunctionObject;
                            preal_specsig = true;
                            isedge = true;
                        }
                    }
                }
            }
        }
        if (!preal_decl.empty() || force) {
            // if we have a prototype emitted, compare it to what we emitted earlier
            Module *mod = proto.decl->getParent();
            assert(proto.decl->isDeclaration());
            Function *pinvoke = nullptr;
            if (preal_decl.empty()) {
                if (invoke != nullptr && invokeName.empty()) {
                    assert(invoke != jl_fptr_args_addr);
                    if (invoke == jl_fptr_sparam_addr)
                        invokeName = "jl_fptr_sparam";
                    else if (invoke == jl_f_opaque_closure_call_addr)
                        invokeName = "jl_f_opaque_closure_call";
                    else
                        invokeName = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)invoke, invoke, codeinst);
                }
                pinvoke = emit_tojlinvoke(codeinst, invokeName, mod, params);
                if (!proto.specsig)
                    proto.decl->replaceAllUsesWith(pinvoke);
                isedge = false;
            }
            if (proto.specsig && !preal_specsig) {
                // get or build an fptr1 that can invoke codeinst
                if (pinvoke == nullptr)
                    pinvoke = get_or_emit_fptr1(preal_decl, mod);
                // emit specsig-to-(jl)invoke conversion
                proto.decl->setLinkage(GlobalVariable::InternalLinkage);
                //protodecl->setAlwaysInline();
                jl_init_function(proto.decl, params.TargetTriple);
                // TODO: maybe this can be cached in codeinst->specfptr?
                int8_t gc_state = jl_gc_unsafe_enter(ct->ptls); // codegen may contain safepoints (such as jl_subtype calls)
                jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
                size_t nrealargs = jl_nparams(mi->specTypes); // number of actual arguments being passed
                bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
                emit_specsig_to_fptr1(proto.decl, proto.cc, proto.return_roots, mi->specTypes, codeinst->rettype, is_opaque_closure, nrealargs, params, pinvoke);
                jl_gc_unsafe_leave(ct->ptls, gc_state);
                preal_decl = ""; // no need to fixup the name
            }
            if (!preal_decl.empty()) {
                // merge and/or rename this prototype to the real function
                if (Value *specfun = mod->getNamedValue(preal_decl)) {
                    if (proto.decl != specfun)
                        proto.decl->replaceAllUsesWith(specfun);
                }
                else {
                    proto.decl->setName(preal_decl);
                }
            }
            if (proto.oc) { // additionally, if we are dealing with an OC constructor, then we might also need to fix up the fptr1 reference too
                assert(proto.specsig);
                StringRef ocinvokeDecl = invokeName;
                if (invoke != nullptr && ocinvokeDecl.empty()) {
                    // check for some special tokens used by opaque_closure.c and convert those to their real functions
                    assert(invoke != jl_fptr_args_addr);
                    assert(invoke != jl_fptr_sparam_addr);
                    if (invoke == jl_fptr_interpret_call_addr)
                        ocinvokeDecl = "jl_fptr_interpret_call";
                    else if (invoke == jl_fptr_const_return_addr)
                        ocinvokeDecl = "jl_fptr_const_return";
                    else if (invoke == jl_f_opaque_closure_call_addr)
                        ocinvokeDecl = "jl_f_opaque_closure_call";
                    //else if (invoke == jl_interpret_opaque_closure_addr)
                    else
                        ocinvokeDecl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)invoke, invoke, codeinst);
                }
                // if OC expected a specialized specsig dispatch, but we don't have it, use the inner trampoline here too
                // XXX: this invoke translation logic is supposed to exactly match new_opaque_closure
                if (!preal_specsig || ocinvokeDecl == "jl_f_opaque_closure_call" || ocinvokeDecl == "jl_fptr_interpret_call" || ocinvokeDecl == "jl_fptr_const_return") {
                    if (pinvoke == nullptr)
                        ocinvokeDecl = get_or_emit_fptr1(preal_decl, mod)->getName();
                    else
                        ocinvokeDecl = pinvoke->getName();
                }
                assert(!ocinvokeDecl.empty());
                assert(ocinvokeDecl != "jl_fptr_args");
                assert(ocinvokeDecl != "jl_fptr_sparam");
                // merge and/or rename this prototype to the real function
                if (Value *specfun = mod->getNamedValue(ocinvokeDecl)) {
                    if (proto.oc != specfun)
                        proto.oc->replaceAllUsesWith(specfun);
                }
                else {
                    proto.oc->setName(ocinvokeDecl);
                }
            }
        }
        else {
            isedge = true;
            params.workqueue.push_back(it);
            incomplete_rgraph[codeinst].push_back(callee);
        }
        if (isedge)
            complete_graph[callee].push_back(codeinst);
    }
    return params.workqueue.size();
}

// move codeinst (and deps) from incompletemodules to emitted modules
// and populate compileready from complete_graph
static void prepare_compile(jl_code_instance_t *codeinst) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER
{
    SmallVector<jl_code_instance_t*> workqueue;
    workqueue.push_back(codeinst);
    while (!workqueue.empty()) {
        codeinst = workqueue.pop_back_val();
        if (!invokenames.count(codeinst)) {
            // this means it should be compiled already while the callee was in stasis
            assert(jl_is_compiled_codeinst(codeinst));
            continue;
        }
        // if this was incomplete, force completion now of it
        auto it = incompletemodules.find(codeinst);
        if (it != incompletemodules.end()) {
            int waiting = 0;
            auto &edges = complete_graph[codeinst];
            auto edges_end = std::remove_if(edges.begin(), edges.end(), [&waiting, codeinst] (jl_code_instance_t *edge) JL_NOTSAFEPOINT -> bool {
                auto &redges = incomplete_rgraph[edge];
                // waiting += std::erase(redges, codeinst);
                auto redges_end = std::remove(redges.begin(), redges.end(), codeinst);
                if (redges_end != redges.end()) {
                    waiting += redges.end() - redges_end;
                    redges.erase(redges_end, redges.end());
                    assert(!invokenames.count(edge));
                }
                return !invokenames.count(edge);
            });
            edges.erase(edges_end, edges.end());
            assert(waiting == std::get<1>(it->second));
            std::get<1>(it->second) = 0;
            auto &params = std::get<0>(it->second);
            params.tsctx_lock = params.tsctx.getLock();
            waiting = jl_analyze_workqueue(codeinst, params, true); // may safepoint
            assert(!waiting); (void)waiting;
            Module *M = emittedmodules[codeinst].getModuleUnlocked();
            finish_params(M, params, sharedmodules);
            incompletemodules.erase(it);
        }
        // and then indicate this should be compiled now
        if (!linkready.count(codeinst) && compileready.insert(codeinst).second) {
            auto edges = complete_graph.find(codeinst);
            if (edges != complete_graph.end()) {
                workqueue.append(edges->second);
            }
        }
    }
}

// notify any other pending work that this edge now has code defined
static void complete_emit(jl_code_instance_t *edge) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER
{
    auto notify = incomplete_rgraph.find(edge);
    if (notify == incomplete_rgraph.end())
        return;
    auto redges = std::move(notify->second);
    incomplete_rgraph.erase(notify);
    for (size_t i = 0; i < redges.size(); i++) {
        jl_code_instance_t *callee = redges[i];
        auto it = incompletemodules.find(callee);
        assert(it != incompletemodules.end());
        if (--std::get<1>(it->second) == 0) {
            auto &params = std::get<0>(it->second);
            params.tsctx_lock = params.tsctx.getLock();
            assert(callee == it->first);
            int waiting = jl_analyze_workqueue(callee, params); // may safepoint
            assert(!waiting); (void)waiting;
            Module *M = emittedmodules[callee].getModuleUnlocked();
            finish_params(M, params, sharedmodules);
            incompletemodules.erase(it);
        }
    }
}


// set the invoke field for codeinst (and all deps, and assist with other pending work from other threads) now
static void jl_compile_codeinst_now(jl_code_instance_t *codeinst)
{
    jl_unique_gcsafe_lock lock(engine_lock);
    if (!invokenames.count(codeinst))
        return;
    threads_in_compiler_phase++;
    prepare_compile(codeinst); // may safepoint
    while (1) {
        // TODO: split up this work by ThreadSafeContext, so two threads don't need to get the same locks and stall
        if (!sharedmodules.empty()) {
            auto TSM = sharedmodules.pop_back_val();
            lock.native.unlock();
            {
                auto Lock = TSM.getContext().getLock();
                jl_ExecutionEngine->optimizeDLSyms(*TSM.getModuleUnlocked()); // may safepoint
            }
            jl_ExecutionEngine->addModule(std::move(TSM));
            lock.native.lock();
        }
        else if (!compileready.empty()) {
            // move a function from compileready to linkready then compile it
            auto compilenext = compileready.begin();
            codeinst = *compilenext;
            compileready.erase(compilenext);
            auto TSMref = emittedmodules.find(codeinst);
            assert(TSMref != emittedmodules.end());
            auto TSM = std::move(TSMref->second);
            linkready.insert(codeinst);
            emittedmodules.erase(TSMref);
            lock.native.unlock();
            uint64_t start_time = jl_hrtime();
            {
                auto Lock = TSM.getContext().getLock();
                jl_ExecutionEngine->optimizeDLSyms(*TSM.getModuleUnlocked()); // may safepoint
            }
            jl_ExecutionEngine->addModule(std::move(TSM)); // may safepoint
            // If logging of the compilation stream is enabled,
            // then dump the method-instance specialization type to the stream
            jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
            uint64_t end_time = jl_hrtime();
            if (jl_is_method(mi->def.method)) {
                auto stream = *jl_ExecutionEngine->get_dump_compiles_stream();
                if (stream) {
                    ios_printf(stream, "%" PRIu64 "\t\"", end_time - start_time);
                    jl_static_show((JL_STREAM*)stream, mi->specTypes);
                    ios_printf(stream, "\"\n");
                }
            }
            jl_atomic_store_relaxed(&codeinst->time_compile,
                julia_double_to_half(julia_half_to_float(jl_atomic_load_relaxed(&codeinst->time_compile))
                    + (end_time - start_time) * 1e-9));
            lock.native.lock();
        }
        else {
            break;
        }
    }
    codeinst = nullptr;
    // barrier until all threads have finished calling addModule
    if (--threads_in_compiler_phase == 0) {
        // the last thread out will finish linking everything
        // then release all of the other threads
        // move the function pointers out from invokenames to the codeinst

        // batch compile job for all new functions
        SmallVector<StringRef> NewDefs;
        for (auto &this_code : linkready) {
            auto it = invokenames.find(this_code);
            assert(it != invokenames.end());
            jl_llvm_functions_t &decls = it->second;
            assert(!decls.functionObject.empty());
            if (decls.functionObject != "jl_fptr_args" &&
                decls.functionObject != "jl_fptr_sparam" &&
                decls.functionObject != "jl_f_opaque_closure_call")
                NewDefs.push_back(decls.functionObject);
            if (!decls.specFunctionObject.empty())
                NewDefs.push_back(decls.specFunctionObject);
        }
        auto Addrs = jl_ExecutionEngine->findSymbols(NewDefs);

        size_t nextaddr = 0;
        for (auto &this_code : linkready) {
            auto it = invokenames.find(this_code);
            assert(it != invokenames.end());
            jl_llvm_functions_t &decls = it->second;
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
                assert(NewDefs[nextaddr] == decls.functionObject);
                addr = (jl_callptr_t)Addrs[nextaddr++];
                assert(addr);
                isspecsig = true;
            }
            if (!decls.specFunctionObject.empty()) {
                void *prev_specptr = nullptr;
                assert(NewDefs[nextaddr] == decls.specFunctionObject);
                void *spec = (void*)Addrs[nextaddr++];
                assert(spec);
                if (jl_atomic_cmpswap_acqrel(&this_code->specptr.fptr, &prev_specptr, spec)) {
                    // only set specsig and invoke if we were the first to set specptr
                    jl_atomic_store_relaxed(&this_code->specsigflags, (uint8_t) isspecsig);
                    // we might overwrite invokeptr here; that's ok, anybody who relied on the identity of invokeptr
                    // either assumes that specptr was null, doesn't care about specptr,
                    // or will wait until specsigflags has 0b10 set before reloading invoke
                    jl_atomic_store_release(&this_code->invoke, addr);
                    jl_atomic_store_release(&this_code->specsigflags, (uint8_t) (0b10 | isspecsig));
                }
                else {
                    //someone else beat us, don't commit any results
                    while (!(jl_atomic_load_acquire(&this_code->specsigflags) & 0b10)) {
                        jl_cpu_pause();
                    }
                    addr = jl_atomic_load_relaxed(&this_code->invoke);
                }
            }
            else {
                jl_callptr_t prev_invoke = nullptr;
                // Allow replacing addr if it is either nullptr or our special waiting placeholder.
                if (!jl_atomic_cmpswap_acqrel(&this_code->invoke, &prev_invoke, addr)) {
                    if (prev_invoke == jl_fptr_wait_for_compiled_addr && !jl_atomic_cmpswap_acqrel(&this_code->invoke, &prev_invoke, addr)) {
                        addr = prev_invoke;
                        //TODO do we want to potentially promote invoke anyways? (e.g. invoke is jl_interpret_call or some other
                        //known lesser function)
                    }
                }
            }
            invokenames.erase(it);
            complete_graph.erase(this_code);
        }
        linkready.clear();
        engine_wait.notify_all();
    }
    else while (threads_in_compiler_phase) {
        lock.wait(engine_wait);
    }
}

void jl_add_code_in_flight(StringRef name, jl_code_instance_t *codeinst, const DataLayout &DL) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER;

extern "C" JL_DLLEXPORT_CODEGEN
void jl_emit_codeinst_to_jit_impl(
        jl_code_instance_t *codeinst,
        jl_code_info_t *src)
{
    if (jl_is_compiled_codeinst(codeinst))
        return;
    { // lock scope
        jl_unique_gcsafe_lock lock(engine_lock);
        if (invokenames.count(codeinst) || jl_is_compiled_codeinst(codeinst))
            return;
    }
    JL_TIMING(CODEINST_COMPILE, CODEINST_COMPILE);
    // emit the code in LLVM IR form to the new context
    jl_codegen_params_t params(std::make_unique<LLVMContext>(), jl_ExecutionEngine->getDataLayout(), jl_ExecutionEngine->getTargetTriple()); // Locks the context
    params.getContext().setDiscardValueNames(true);
    params.cache = true;
    params.imaging_mode = 0;
    orc::ThreadSafeModule result_m =
        jl_create_ts_module(name_from_method_instance(jl_get_ci_mi(codeinst)), params.tsctx, params.DL, params.TargetTriple);
    params.temporary_roots = jl_alloc_array_1d(jl_array_any_type, 0);
    JL_GC_PUSH1(&params.temporary_roots);
    jl_llvm_functions_t decls = jl_emit_codeinst(result_m, codeinst, src, params); // contains safepoints
    if (!result_m) {
        JL_GC_POP();
        return;
    }
    jl_optimize_roots(params, jl_get_ci_mi(codeinst), *result_m.getModuleUnlocked()); // contains safepoints
    params.temporary_roots = nullptr;
    JL_GC_POP();
    { // drop lock before acquiring engine_lock
        auto release = std::move(params.tsctx_lock);
    }
    jl_unique_gcsafe_lock lock(engine_lock);
    if (invokenames.count(codeinst) || jl_is_compiled_codeinst(codeinst))
        return; // destroy everything
    const std::string &specf = decls.specFunctionObject;
    const std::string &f = decls.functionObject;
    assert(!f.empty());
    // Prepare debug info to receive this function
    // record that this function name came from this linfo,
    // so we can build a reverse mapping for debug-info.
    bool toplevel = !jl_is_method(jl_get_ci_mi(codeinst)->def.method);
    if (!toplevel) {
        // don't remember toplevel thunks because
        // they may not be rooted in the gc for the life of the program,
        // and the runtime doesn't notify us when the code becomes unreachable :(
        if (!specf.empty())
            jl_add_code_in_flight(specf, codeinst, params.DL);
        if (f != "jl_fptr_args" && f != "jl_fptr_sparam")
            jl_add_code_in_flight(f, codeinst, params.DL);
    }
    jl_callptr_t expected = NULL;
    jl_atomic_cmpswap_relaxed(&codeinst->invoke, &expected, jl_fptr_wait_for_compiled_addr);
    invokenames[codeinst] = std::move(decls);
    complete_emit(codeinst);
    params.tsctx_lock = params.tsctx.getLock(); // re-acquire lock
    int waiting = jl_analyze_workqueue(codeinst, params);
    if (waiting) {
        auto release = std::move(params.tsctx_lock); // unlock again before moving from it
        incompletemodules.try_emplace(codeinst, std::move(params), waiting);
    }
    else {
        finish_params(result_m.getModuleUnlocked(), params, sharedmodules);
    }
    emittedmodules[codeinst] = std::move(result_m);
}


extern "C" JL_DLLEXPORT_CODEGEN
int jl_compile_codeinst_impl(jl_code_instance_t *ci)
{
    int newly_compiled = 0;
    if (!jl_is_compiled_codeinst(ci)) {
        ++SpecFPtrCount;
        uint64_t start = jl_typeinf_timing_begin();
        jl_compile_codeinst_now(ci);
        jl_typeinf_timing_end(start, 0);
        newly_compiled = 1;
    }
    return newly_compiled;
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
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    jl_method_t *def = jl_get_ci_mi(unspec)->def.method;
    if (jl_is_method(def)) {
        src = (jl_code_info_t*)def->source;
        if (src && (jl_value_t*)src != jl_nothing)
            src = jl_uncompress_ir(def, NULL, (jl_value_t*)src);
    }
    else {
        jl_method_instance_t *mi = jl_get_ci_mi(unspec);
        jl_code_instance_t *uninferred = jl_cached_uninferred(jl_atomic_load_relaxed(&mi->cache), 1);
        assert(uninferred);
        src = (jl_code_info_t*)jl_atomic_load_relaxed(&uninferred->inferred);
        assert(src);
    }
    if (src) {
        // TODO: first prepare recursive_compile_graph(unspec, src) before taking this lock to avoid recursion?
        JL_LOCK(&jitlock); // TODO: use a better lock
        if (!jl_is_compiled_codeinst(unspec)) {
            assert(jl_is_code_info(src));
            ++UnspecFPtrCount;
            jl_svec_t *edges = (jl_svec_t*)src->edges;
            if (jl_is_svec(edges)) {
                jl_atomic_store_release(&unspec->edges, edges); // n.b. this assumes the field was always empty svec(), which is not entirely true
                jl_gc_wb(unspec, edges);
            }
            jl_debuginfo_t *debuginfo = src->debuginfo;
            jl_atomic_store_release(&unspec->debuginfo, debuginfo); // n.b. this assumes the field was previously NULL, which is not entirely true
            jl_gc_wb(unspec, debuginfo);
            jl_emit_codeinst_to_jit(unspec, src);
            jl_compile_codeinst_now(unspec);
        }
        JL_UNLOCK(&jitlock); // Might GC
    }
    JL_GC_POP();
    jl_callptr_t null = nullptr;
    // if we hit a codegen bug (or ran into a broken generated function or llvmcall), fall back to the interpreter as a last resort
    jl_atomic_cmpswap(&unspec->invoke, &null, jl_fptr_interpret_call_addr);
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
        uintptr_t specfptr = (uintptr_t)jl_atomic_load_relaxed(&codeinst->specptr.fptr);
        if (getwrapper || specfptr == 0)
            specfptr = fptr;
        if (specfptr != 0)
            return jl_dump_fptr_asm(specfptr, emit_mc, asm_variant, debuginfo, binary);
    }
    return jl_an_empty_string;
}

#if JL_LLVM_VERSION >= 180000
CodeGenOptLevel CodeGenOptLevelFor(int optlevel)
{
#ifdef DISABLE_OPT
    return CodeGenOptLevel::None;
#else
    return optlevel == 0 ? CodeGenOptLevel::None :
        optlevel == 1 ? CodeGenOptLevel::Less :
        optlevel == 2 ? CodeGenOptLevel::Default :
        CodeGenOptLevel::Aggressive;
#endif
}
#else
CodeGenOpt::Level CodeGenOptLevelFor(int optlevel)
{
#ifdef DISABLE_OPT
    return CodeGenOpt::None;
#else
    return optlevel == 0 ? CodeGenOpt::None :
        optlevel == 1 ? CodeGenOpt::Less :
        optlevel == 2 ? CodeGenOpt::Default :
        CodeGenOpt::Aggressive;
#endif
}
#endif

static auto countBasicBlocks(const Function &F) JL_NOTSAFEPOINT
{
    return std::distance(F.begin(), F.end());
}

static constexpr size_t N_optlevels = 4;

static orc::ThreadSafeModule selectOptLevel(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT {
    TSM.withModuleDo([](Module &M) JL_NOTSAFEPOINT {
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
    return TSM;
}
static orc::ThreadSafeModule selectOptLevel(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
    return selectOptLevel(std::move(TSM));
}

void jl_register_jit_object(const object::ObjectFile &debugObj,
                            std::function<uint64_t(const StringRef &)> getLoadAddress);

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

public:
    void notifyMaterializing(MaterializationResponsibility &MR, jitlink::LinkGraph &G,
                             jitlink::JITLinkContext &Ctx,
                             MemoryBufferRef InputObject) override
    {
        auto NewBuffer =
            MemoryBuffer::getMemBufferCopy(InputObject.getBuffer(), G.getName());
        // Re-parsing the InputObject is wasteful, but for now, this lets us
        // reuse the existing debuginfo.cpp code. Should look into just
        // directly pulling out all the information required in a JITLink pass
        // and just keeping the required tables/DWARF sections around (perhaps
        // using the LLVM DebuggerSupportPlugin as a reference).
        auto NewObj =
            cantFail(object::ObjectFile::createObjectFile(NewBuffer->getMemBufferRef()));

        {
            std::lock_guard<std::mutex> lock(PluginMutex);
            assert(PendingObjs.count(&MR) == 0);
            PendingObjs[&MR] = std::unique_ptr<JITObjectInfo>(new JITObjectInfo{
                std::move(NewBuffer), std::move(NewObj), {}});
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

            jl_register_jit_object(*NewInfo->Object, getLoadAddress);
            PendingObjs.erase(&MR);
        }

        return Error::success();
    }

    Error notifyFailed(MaterializationResponsibility &MR) override
    {
        std::lock_guard<std::mutex> lock(PluginMutex);
        PendingObjs.erase(&MR);
        return Error::success();
    }

    Error notifyRemovingResources(JITDylib &JD, orc::ResourceKey K) override
    {
        return Error::success();
    }

    void notifyTransferringResources(JITDylib &JD, orc::ResourceKey DstKey,
                                     orc::ResourceKey SrcKey) override {}

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
    _Atomic(size_t)* jit_bytes_size;

public:

    JLMemoryUsagePlugin(_Atomic(size_t)* jit_bytes_size)
        : jit_bytes_size(jit_bytes_size) {}

    Error notifyFailed(orc::MaterializationResponsibility &MR) override {
        return Error::success();
    }
    Error notifyRemovingResources(JITDylib &JD, orc::ResourceKey K) override
    {
        return Error::success();
    }
    void notifyTransferringResources(JITDylib &JD, orc::ResourceKey DstKey,
                                     orc::ResourceKey SrcKey) override {}

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
                if ((section.getMemProt() & orc::MemProt::Exec) == orc::MemProt::None) {
                    data_size += secsize;
                } else {
                    code_size += secsize;
                }
                graph_size += secsize;
            }
            (void) code_size;
            (void) data_size;
            jl_atomic_fetch_add_relaxed(this->jit_bytes_size, graph_size);
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
std::unique_ptr<jitlink::JITLinkMemoryManager> createJITLinkMemoryManager() JL_NOTSAFEPOINT {
    return cantFail(orc::MapperJITLinkMemoryManager::CreateWithMapper<orc::InProcessMemoryMapper>(/*Reservation Granularity*/ 16 * 1024 * 1024));
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

RTDyldMemoryManager *createRTDyldMemoryManager(void) JL_NOTSAFEPOINT;

// A simple forwarding class, since OrcJIT v2 needs a unique_ptr, while we have a shared_ptr
class ForwardingMemoryManager : public RuntimeDyld::MemoryManager {
private:
    std::shared_ptr<RuntimeDyld::MemoryManager> MemMgr;

public:
    ForwardingMemoryManager(std::shared_ptr<RuntimeDyld::MemoryManager> MemMgr) : MemMgr(MemMgr) {}
    ForwardingMemoryManager(ForwardingMemoryManager &) = delete;
    virtual ~ForwardingMemoryManager() {
        assert(!MemMgr);
    }
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
    virtual void reserveAllocationSpace(uintptr_t CodeSize, Align CodeAlign,
                                        uintptr_t RODataSize, Align RODataAlign,
                                        uintptr_t RWDataSize, Align RWDataAlign) override {
        return MemMgr->reserveAllocationSpace(CodeSize, CodeAlign, RODataSize, RODataAlign, RWDataSize, RWDataAlign);
    }
    virtual bool needsToReserveAllocationSpace() override {
        return MemMgr->needsToReserveAllocationSpace();
    }
    virtual void registerEHFrames(uint8_t *Addr, uint64_t LoadAddr,
                                  size_t Size) override {
        return MemMgr->registerEHFrames(Addr, LoadAddr, Size);
    }
    virtual void deregisterEHFrames() override { /* not actually supported or allowed with this */ }
    virtual bool finalizeMemory(std::string *ErrMsg = nullptr) override {
        bool b = false;
        if (MemMgr.use_count() == 2)
            b = MemMgr->finalizeMemory(ErrMsg);
        MemMgr.reset();
        return b;
    }
    virtual void notifyObjectLoaded(RuntimeDyld &RTDyld,
                                    const object::ObjectFile &Obj) override {
        return MemMgr->notifyObjectLoaded(RTDyld, Obj);
    }
};

#ifndef JL_USE_JITLINK
static void registerRTDyldJITObject(orc::MaterializationResponsibility &MR,
                                    const object::ObjectFile &Object,
                                    const RuntimeDyld::LoadedObjectInfo &L)
{
    StringMap<object::SectionRef> loadedSections;
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

    auto DebugObject = L.getObjectForDebug(Object); // ELF requires us to make a copy to mutate the header with the section load addresses. On other platforms this is a no-op.
    jl_register_jit_object(DebugObject.getBinary() ? *DebugObject.getBinary() : Object, getLoadAddress);
}
#endif

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
#if defined(_CPU_RISCV64_)
        // we set these manually to avoid LLVM defaulting to soft-float
#if defined(__riscv_float_abi_double)
        options.MCOptions.ABIName = "lp64d";
#elif defined(__riscv_float_abi_single)
        options.MCOptions.ABIName = "lp64f";
#else
        options.MCOptions.ABIName = "lp64";
#endif
#endif
        uint32_t target_flags = 0;
        auto target = jl_get_llvm_target(jl_options.cpu_target, jl_generating_output(), target_flags);
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
        std::optional<CodeModel::Model> codemodel =
#ifdef _P64
            // Make sure we are using the large code model on 64bit
            // Let LLVM pick a default suitable for jitting on 32bit
            CodeModel::Large;
#else
            None;
#endif
        if (TheTriple.isAArch64())
            codemodel = CodeModel::Small;
        else if (TheTriple.isRISCV()) {
            // RISC-V will support large code model in LLVM 21
            // https://github.com/llvm/llvm-project/pull/70308
            codemodel = CodeModel::Medium;
        }
        // Generate simpler code for JIT
        Reloc::Model relocmodel = Reloc::Static;
        if (TheTriple.isRISCV()) {
            // until large code model is supported, use PIC for RISC-V
            // https://github.com/llvm/llvm-project/issues/106203
            relocmodel = Reloc::PIC_;
        }
        auto optlevel = CodeGenOptLevelFor(jl_options.opt_level);
        auto TM = TheTarget->createTargetMachine(
                TheTriple.getTriple(), TheCPU, FeaturesStr,
                options,
                relocmodel,
                codemodel,
                optlevel,
                true // JIT
                );
        assert(TM && "Failed to select target machine -"
                     " Is the LLVM backend for this CPU enabled?");
        fixupTM(*TM);
        return std::unique_ptr<TargetMachine>(TM);
    }

    typedef NewPM PassManager;

    orc::JITTargetMachineBuilder createJTMBFromTM(TargetMachine &TM, int optlevel) JL_NOTSAFEPOINT {
        return orc::JITTargetMachineBuilder(TM.getTargetTriple())
            .setCPU(TM.getTargetCPU().str())
            .setFeatures(TM.getTargetFeatureString())
            .setOptions(TM.Options)
            .setRelocationModel(TM.getRelocationModel())
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
    struct sizedOptimizerT {
        sizedOptimizerT(TargetMachine &TM, SmallVector<std::function<void()>, 0> &printers, std::mutex &llvm_printing_mutex) JL_NOTSAFEPOINT {
            for (size_t i = 0; i < N; i++) {
                PMs[i] = std::make_unique<JuliaOJIT::ResourcePool<std::unique_ptr<PassManager>>>(PMCreator(TM, i, printers, llvm_printing_mutex));
            }
        }

        orc::ThreadSafeModule operator()(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT {
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
                            if (F.isDeclaration() || F.getName().starts_with("jfptr_")) {
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
                            if (F.isDeclaration() || F.getName().starts_with("jfptr_")) {
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
            return TSM;
        }
    private:
        std::array<std::unique_ptr<JuliaOJIT::ResourcePool<std::unique_ptr<PassManager>>>, N> PMs;
    };

    // shim for converting a unique_ptr to a TransformFunction to a TransformFunction
    template <typename T>
    struct IRTransformRef {
        IRTransformRef(T &transform) : transform(transform) {}
        OptimizerResultT operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
            return transform(std::move(TSM), R);
        }
    private:
        T &transform;
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
            }
            else {
                PoolIdx = jl_options.opt_level;
            }
            assert(PoolIdx < N && "Invalid optimization level for compiler!");
            return orc::SimpleCompiler(****TMs[PoolIdx])(M);
        }

        std::array<std::unique_ptr<JuliaOJIT::ResourcePool<std::unique_ptr<TargetMachine>>>, N> TMs;
    };
}

struct JuliaOJIT::OptimizerT {
    OptimizerT(TargetMachine &TM, SmallVector<std::function<void()>, 0> &printers, std::mutex &llvm_printing_mutex)
        : opt(TM, printers, llvm_printing_mutex) {}
    orc::ThreadSafeModule operator()(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT {
        return opt(std::move(TSM));
    }
    OptimizerResultT operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
        return opt(std::move(TSM));
    }
private:
    struct sizedOptimizerT<N_optlevels> opt;
};

struct JuliaOJIT::JITPointersT {
    JITPointersT(SharedBytesT &SharedBytes, std::mutex &Lock) JL_NOTSAFEPOINT
        : SharedBytes(SharedBytes), Lock(Lock) {}

    orc::ThreadSafeModule operator()(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT {
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
            // build unwind tables, if they have any functions to decorate
            if (!M.functions().empty())
                jl_decorate_module(M);
        });
        return TSM;
    }
    Expected<orc::ThreadSafeModule> operator()(orc::ThreadSafeModule TSM, orc::MaterializationResponsibility &R) JL_NOTSAFEPOINT {
        return operator()(std::move(TSM));
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
    ~DLSymOptimizer() JL_NOTSAFEPOINT = default;

    void *lookup_symbol(void *libhandle, const char *fname) JL_NOTSAFEPOINT {
        void *addr;
        jl_dlsym(libhandle, fname, &addr, 0);
        return addr;
    }

    void *lookup(const char *libname, const char *fname) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER {
        StringRef lib(libname);
        StringRef f(fname);
        std::lock_guard<std::mutex> lock(symbols_mutex);
        auto uit = user_symbols.find(lib);
        if (uit == user_symbols.end()) {
            jl_task_t *ct = jl_current_task;
            int8_t gc_state = jl_gc_unsafe_enter(ct->ptls);
            void *handle = jl_get_library_(libname, 0);
            jl_gc_unsafe_leave(ct->ptls, gc_state);
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

    void *lookup(uintptr_t libidx, const char *fname) JL_NOTSAFEPOINT {
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

    void operator()(Module &M) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER {
        for (auto &GV : M.globals()) {
            auto Name = GV.getName();
            if (Name.starts_with("jlplt") && Name.ends_with("got")) {
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

void optimizeDLSyms(Module &M) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER {
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

llvm::DataLayout jl_create_datalayout(TargetMachine &TM) {
    // Mark our address spaces as non-integral
    auto jl_data_layout = TM.createDataLayout();
    jl_data_layout = DataLayout(jl_data_layout.getStringRepresentation() + "-ni:10:11:12:13");
    return jl_data_layout;
}

JuliaOJIT::JuliaOJIT()
  : TM(createTargetMachine()),
    DL(jl_create_datalayout(*TM)),
    ES(cantFail(orc::SelfExecutorProcessControl::Create())),
    GlobalJD(ES.createBareJITDylib("JuliaGlobals")),
    JD(ES.createBareJITDylib("JuliaOJIT")),
    ExternalJD(ES.createBareJITDylib("JuliaExternal")),
    DLSymOpt(std::make_unique<DLSymOptimizer>(false)),
#ifdef JL_USE_JITLINK
    MemMgr(createJITLinkMemoryManager()),
    ObjectLayer(ES, *MemMgr),
#else
    MemMgr(createRTDyldMemoryManager()),
    UnlockedObjectLayer(
            ES,
            [this]() {
                std::unique_ptr<RuntimeDyld::MemoryManager> result(new ForwardingMemoryManager(MemMgr));
                return result;
            }
        ),
    ObjectLayer(UnlockedObjectLayer),
#endif
    CompileLayer(ES, ObjectLayer, std::make_unique<CompilerT<N_optlevels>>(orc::irManglingOptionsFromTargetOptions(TM->Options), *TM)),
    JITPointers(std::make_unique<JITPointersT>(SharedBytes, RLST_mutex)),
    JITPointersLayer(ES, CompileLayer, IRTransformRef(*JITPointers)),
    Optimizers(std::make_unique<OptimizerT>(*TM, PrintLLVMTimers, llvm_printing_mutex)),
    OptimizeLayer(ES, JITPointersLayer, IRTransformRef(*Optimizers)),
    OptSelLayer(ES, OptimizeLayer, static_cast<orc::ThreadSafeModule (*)(orc::ThreadSafeModule, orc::MaterializationResponsibility&)>(selectOptLevel))
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
    ObjectLayer.addPlugin(std::make_unique<JLMemoryUsagePlugin>(&jit_bytes_size));
#else
    UnlockedObjectLayer.setNotifyLoaded(registerRTDyldJITObject);
#endif

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
                        return (*S).starts_with(atomic_prefix);
                  })));
        }
    }

    JD.addToLinkOrder(GlobalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
    JD.addToLinkOrder(ExternalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
    ExternalJD.addToLinkOrder(GlobalJD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);
    ExternalJD.addToLinkOrder(JD, orc::JITDylibLookupFlags::MatchExportedSymbolsOnly);

    orc::SymbolAliasMap jl_crt = {
        // Float16 conversion routines
#if defined(_CPU_X86_64_) && defined(_OS_DARWIN_)
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

#ifdef _OS_OPENBSD_
    orc::SymbolMap i128_crt;

    i128_crt[mangle("__divti3")] = JITEvaluatedSymbol::fromPointer(&__divti3, JITSymbolFlags::Exported);
    i128_crt[mangle("__modti3")] = JITEvaluatedSymbol::fromPointer(&__modti3, JITSymbolFlags::Exported);
    i128_crt[mangle("__udivti3")] = JITEvaluatedSymbol::fromPointer(&__udivti3, JITSymbolFlags::Exported);
    i128_crt[mangle("__umodti3")] = JITEvaluatedSymbol::fromPointer(&__umodti3, JITSymbolFlags::Exported);

    cantFail(GlobalJD.define(orc::absoluteSymbols(i128_crt)));
#endif

#ifdef MSAN_EMUTLS_WORKAROUND
    orc::SymbolMap msan_crt;
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
    cantFail(GlobalJD.define(orc::absoluteSymbols(msan_crt)));
#endif
#if JL_LLVM_VERSION < 200000
#ifdef _COMPILER_ASAN_ENABLED_
    // this is a hack to work around a bad assertion:
    //   /workspace/srcdir/llvm-project/llvm/lib/ExecutionEngine/Orc/Core.cpp:3028: llvm::Error llvm::orc::ExecutionSession::OL_notifyResolved(llvm::orc::MaterializationResponsibility&, const SymbolMap&): Assertion `(KV.second.getFlags() & ~JITSymbolFlags::Common) == (I->second & ~JITSymbolFlags::Common) && "Resolving symbol with incorrect flags"' failed.
    // hopefully fixed upstream by e7698a13e319a9919af04d3d693a6f6ea7168a44
    static int64_t jl___asan_globals_registered;
    orc::SymbolMap asan_crt;
    asan_crt[mangle("___asan_globals_registered")] = {ExecutorAddr::fromPtr(&jl___asan_globals_registered), JITSymbolFlags::Common | JITSymbolFlags::Exported};
    cantFail(JD.define(orc::absoluteSymbols(asan_crt)));
#endif
#endif
}

JuliaOJIT::~JuliaOJIT() = default;

ThreadSafeContext JuliaOJIT::makeContext()
{
    auto ctx = std::make_unique<LLVMContext>();
    return orc::ThreadSafeContext(std::move(ctx));
}

orc::SymbolStringPtr JuliaOJIT::mangle(StringRef Name)
{
    std::string MangleName = getMangledName(Name);
    return ES.intern(MangleName);
}

void JuliaOJIT::addGlobalMapping(StringRef Name, uint64_t Addr)
{
    cantFail(JD.define(orc::absoluteSymbols({{mangle(Name), {ExecutorAddr::fromPtr((void*)Addr), JITSymbolFlags::Exported}}})));
}

void JuliaOJIT::addModule(orc::ThreadSafeModule TSM)
{
    JL_TIMING(LLVM_JIT, JIT_Total);
    ++ModulesAdded;
    TSM = selectOptLevel(std::move(TSM));
    TSM = (*Optimizers)(std::move(TSM));
    TSM = (*JITPointers)(std::move(TSM));
    auto Lock = TSM.getContext().getLock();
    Module &M = *TSM.getModuleUnlocked();
    // Treat this as if one of the passes might contain a safepoint
    // even though that shouldn't be the case and might be unwise
    Expected<std::unique_ptr<MemoryBuffer>> Obj = CompileLayer.getCompiler()(M);
    if (!Obj) {
#ifndef __clang_analyzer__ // reportError calls an arbitrary function, which the static analyzer thinks might be a safepoint
        ES.reportError(Obj.takeError());
#endif
        errs() << "Failed to add module to JIT!\n";
        errs() << "Dumping failing module\n" << M << "\n";
        return;
    }
    { auto release = std::move(Lock); }
    auto Err = JuliaOJIT::addObjectFile(JD, std::move(*Obj));
    if (Err) {
#ifndef __clang_analyzer__ // reportError calls an arbitrary function, which the static analyzer thinks might be a safepoint
        ES.reportError(std::move(Err));
#endif
        errs() << "Failed to add objectfile to JIT!\n";
        abort();
    }
}

Error JuliaOJIT::addExternalModule(orc::JITDylib &JD, orc::ThreadSafeModule TSM, bool ShouldOptimize)
{
    if (auto Err = TSM.withModuleDo([&](Module &M) JL_NOTSAFEPOINT -> Error {
            if (M.getDataLayout().isDefault())
                M.setDataLayout(DL);
            if (M.getDataLayout() != DL)
                return make_error<StringError>(
                    "Added modules have incompatible data layouts: " +
                    M.getDataLayout().getStringRepresentation() + " (module) vs " +
                    DL.getStringRepresentation() + " (jit)",
                inconvertibleErrorCode());
            // OrcJIT requires that all modules / files have unique names:
            M.setModuleIdentifier((M.getModuleIdentifier() + Twine("-") + Twine(jl_atomic_fetch_add_relaxed(&jitcounter, 1))).str());
            return Error::success();
        }))
        return Err;
    //if (ShouldOptimize)
    //    return OptimizeLayer.add(JD, std::move(TSM));
    return CompileLayer.add(JD.getDefaultResourceTracker(), std::move(TSM));
}

Error JuliaOJIT::addObjectFile(orc::JITDylib &JD, std::unique_ptr<MemoryBuffer> Obj) {
    assert(Obj && "Can not add null object");
    // OrcJIT requires that all modules / files have unique names:
    // https://llvm.org/doxygen/namespacellvm_1_1orc.html#a1f5a1bc60c220cdccbab0f26b2a425e1
    // so we have to force a copy here
    std::string Name = ("jitted-" + Twine(jl_atomic_fetch_add_relaxed(&jitcounter, 1))).str();
    Obj = Obj->getMemBufferCopy(Obj->getBuffer(), Name);
    return ObjectLayer.add(JD.getDefaultResourceTracker(), std::move(Obj));
}

SmallVector<uint64_t> JuliaOJIT::findSymbols(ArrayRef<StringRef> Names)
{
    // assert(MemMgr.use_count() == 1); (true single-threaded, but slightly race-y to assert it with concurrent threads)
    DenseMap<orc::NonOwningSymbolStringPtr, size_t> Unmangled;
    orc::SymbolLookupSet Exports;
    for (StringRef Name : Names) {
        auto Mangled = ES.intern(getMangledName(Name));
        Unmangled[NonOwningSymbolStringPtr(Mangled)] = Unmangled.size();
        Exports.add(std::move(Mangled));
    }
    SymbolMap Syms = cantFail(ES.lookup(orc::makeJITDylibSearchOrder(ArrayRef(&JD)), std::move(Exports)));
    SmallVector<uint64_t> Addrs(Names.size());
    for (auto it : Syms) {
        Addrs[Unmangled.at(orc::NonOwningSymbolStringPtr(it.first))] = it.second.getAddress().getValue();
    }
    return Addrs;
}

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
    return addr->getAddress().getValue();
}

uint64_t JuliaOJIT::getFunctionAddress(StringRef Name)
{
    auto addr = findSymbol(getMangledName(Name), false);
    if (!addr) {
        consumeError(addr.takeError());
        return 0;
    }
    return addr->getAddress().getValue();
}

StringRef JuliaOJIT::getFunctionAtAddress(uint64_t Addr, jl_callptr_t invoke, jl_code_instance_t *codeinst)
{
    std::lock_guard<std::mutex> lock(RLST_mutex);
    assert(Addr != (uint64_t)jl_fptr_wait_for_compiled_addr);
    std::string *fname = &ReverseLocalSymbolTable[(void*)(uintptr_t)Addr];
    if (fname->empty()) {
        std::string string_fname;
        raw_string_ostream stream_fname(string_fname);
        // try to pick an appropriate name that describes it
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
        const char* unadorned_name = jl_symbol_name(jl_get_ci_mi(codeinst)->def.method->name);
        stream_fname << unadorned_name << "_" << RLST_inc++;
        *fname = std::move(stream_fname.str()); // store to ReverseLocalSymbolTable
        addGlobalMapping(*fname, Addr);
    }
    return *fname;
}

#ifdef JL_USE_JITLINK
#define addAbsoluteToMap(map,name) \
    (map[mangle(#name)] = {ExecutorAddr::fromPtr(&name), JITSymbolFlags::Exported | JITSymbolFlags::Callable}, orc::ExecutorAddr::fromPtr(&name))

void JuliaOJIT::enableJITDebuggingSupport()
{
    orc::SymbolMap GDBFunctions;
    addAbsoluteToMap(GDBFunctions,llvm_orc_registerJITLoaderGDBAllocAction);
    auto registerJITLoaderGDBWrapper = addAbsoluteToMap(GDBFunctions,llvm_orc_registerJITLoaderGDBWrapper);
    cantFail(JD.define(orc::absoluteSymbols(GDBFunctions)));
    (void)registerJITLoaderGDBWrapper;
    if (TM->getTargetTriple().isOSBinFormatMachO())
        ObjectLayer.addPlugin(cantFail(orc::GDBJITDebugInfoRegistrationPlugin::Create(ES, JD, TM->getTargetTriple())));
#ifndef _COMPILER_ASAN_ENABLED_ // TODO: Fix duplicated sections spam #51794
    else if (TM->getTargetTriple().isOSBinFormatELF())
        //EPCDebugObjectRegistrar doesn't take a JITDylib, so we have to directly provide the call address
        ObjectLayer.addPlugin(std::make_unique<orc::DebugObjectManagerPlugin>(ES, std::make_unique<orc::EPCDebugObjectRegistrar>(ES, registerJITLoaderGDBWrapper)));
#endif
}

void JuliaOJIT::enableIntelJITEventListener()
{
#if JL_LLVM_VERSION >= 190000
    if (TM->getTargetTriple().isOSBinFormatELF()) {
        orc::SymbolMap VTuneFunctions;
        auto RegisterImplAddr = addAbsoluteToMap(VTuneFunctions,llvm_orc_registerVTuneImpl);
        auto UnregisterImplAddr = addAbsoluteToMap(VTuneFunctions,llvm_orc_unregisterVTuneImpl);
        ObjectLayer.addPlugin(cantFail(DebugInfoPreservationPlugin::Create()));
        //ObjectLayer.addPlugin(cantFail(VTuneSupportPlugin::Create(ES.getExecutorProcessControl(),
        //                           JD, /*EmitDebugInfo=*/true,
        //                           /*TestMode=*/false)));
        bool EmitDebugInfo = true;
        ObjectLayer.addPlugin(std::make_unique<VTuneSupportPlugin>(
            ES.getExecutorProcessControl(), RegisterImplAddr, UnregisterImplAddr, EmitDebugInfo));
    }
#endif
}

void JuliaOJIT::enableOProfileJITEventListener()
{
    // implement when available in LLVM
}

void JuliaOJIT::enablePerfJITEventListener()
{
#if JL_LLVM_VERSION >= 180000
    if (TM->getTargetTriple().isOSBinFormatELF()) {
        orc::SymbolMap PerfFunctions;
        auto StartAddr = addAbsoluteToMap(PerfFunctions,llvm_orc_registerJITLoaderPerfStart);
        auto EndAddr = addAbsoluteToMap(PerfFunctions,llvm_orc_registerJITLoaderPerfEnd);
        auto ImplAddr = addAbsoluteToMap(PerfFunctions,llvm_orc_registerJITLoaderPerfImpl);
        cantFail(JD.define(orc::absoluteSymbols(PerfFunctions)));
        ObjectLayer.addPlugin(cantFail(DebugInfoPreservationPlugin::Create()));
        //ObjectLayer.addPlugin(cantFail(PerfSupportPlugin::Create(
        //    ES.getExecutorProcessControl(), *JD, true, true)));
        bool EmitDebugInfo = true, EmitUnwindInfo = true;
        ObjectLayer.addPlugin(std::make_unique<PerfSupportPlugin>(
            ES.getExecutorProcessControl(), StartAddr, EndAddr, ImplAddr, EmitDebugInfo, EmitUnwindInfo));
    }
#endif
}
#else
void JuliaOJIT::RegisterJITEventListener(JITEventListener *L)
{
    if (L)
        UnlockedObjectLayer.registerJITEventListener(*L);
}
void JuliaOJIT::enableJITDebuggingSupport()
{
    RegisterJITEventListener(JITEventListener::createGDBRegistrationListener());
}
void JuliaOJIT::enableIntelJITEventListener()
{
    RegisterJITEventListener(JITEventListener::createIntelJITEventListener());
}
void JuliaOJIT::enableOProfileJITEventListener()
{
    RegisterJITEventListener(JITEventListener::createOProfileJITEventListener());
}
void JuliaOJIT::enablePerfJITEventListener()
{
    RegisterJITEventListener(JITEventListener::createPerfJITEventListener());
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

size_t JuliaOJIT::getTotalBytes() const
{
    auto bytes = jl_atomic_load_relaxed(&jit_bytes_size);
#ifndef JL_USE_JITLINK
    size_t getRTDyldMemoryManagerTotalBytes(RTDyldMemoryManager *mm) JL_NOTSAFEPOINT;
    bytes += getRTDyldMemoryManagerTotalBytes(MemMgr.get());
#endif
    return bytes;
}

void JuliaOJIT::addBytes(size_t bytes)
{
    jl_atomic_fetch_add_relaxed(&jit_bytes_size, bytes);
}

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
                    //    ArrayType *ATy = ArrayType::get(PointerType::get(dest.getContext()), Init.size());
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
                dest.insertGlobalVariable(&SG);
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
                dest.insertAlias(&SG);
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
        // and with JITLink it became difficult to change the content afterwards, but we
        // would prefer that this simple content wasn't recompiled in every single module,
        // so we emit the necessary PLT trampoline as inline assembly.
        // This is somewhat duplicated with the .pdata section, but we haven't been able to
        // use that yet due to relocation issues.
#define ASM_USES_ELF // use ELF or COFF syntax based on FORCE_ELF
        StringRef inline_asm(
    ".section"
#if JL_LLVM_VERSION >= 180000
        " .ltext,\"ax\",@progbits\n"
#else
        " .text\n"
#endif
    ".globl __julia_personality\n"
    "\n"
#ifdef ASM_USES_ELF
    ".type __UnwindData,@object\n"
#else
    ".def __UnwindData\n"
    ".scl 2\n"
    ".type 0\n"
    ".endef\n"
#endif
    ".p2align        2, 0x90\n"
    "__UnwindData:\n"
    "  .byte 0x09;\n" // version info, UNW_FLAG_EHANDLER
    "  .byte 4;\n"    // size of prolog (bytes)
    "  .byte 2;\n"    // count of unwind codes (slots)
    "  .byte 0x05;\n" // frame register (rbp) = rsp
    "  .byte 4;\n"    // second instruction
    "  .byte 0x03;\n" // mov RBP, RSP
    "  .byte 1;\n"    // first instruction
    "  .byte 0x50;\n" // push RBP
    "  .int __catchjmp - "
#if JL_LLVM_VERSION >= 180000
    ".ltext;\n" // Section-relative offset (if using COFF and JITLink, this can be relative to __ImageBase instead, though then we could possibly use pdata/xdata directly then)
#else
    ".text;\n"
#endif
    ".size __UnwindData, 12\n"
    "\n"
#ifdef ASM_USES_ELF
    ".type __catchjmp,@function\n"
#else
    ".def __catchjmp\n"
    ".scl 2\n"
    ".type 32\n"
    ".endef\n"
#endif
    ".p2align        2, 0x90\n"
    "__catchjmp:\n"
    "  movabsq $__julia_personality, %rax\n"
    "  jmpq *%rax\n"
    ".size __catchjmp, . - __catchjmp\n"
    "\n");
        M.appendModuleInlineAsm(inline_asm);
    }
#undef ASM_USES_ELF
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

// API for adding bytes to record being owned by the JIT
void jl_jit_add_bytes(size_t bytes)
{
    jl_ExecutionEngine->addBytes(bytes);
}
