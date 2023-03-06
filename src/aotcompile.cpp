// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"

// target support
#include <llvm/ADT/Triple.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/IR/DataLayout.h>
#if JL_LLVM_VERSION >= 140000
#include <llvm/MC/TargetRegistry.h>
#else
#include <llvm/Support/TargetRegistry.h>
#endif
#include <llvm/Target/TargetMachine.h>

// analysis passes
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#include <llvm/Analysis/ScopedNoAliasAA.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/MemorySanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Scalar/SimpleLoopUnswitch.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/LinkAllPasses.h>
#include <polly/CodeGen/CodegenCleanup.h>
#if defined(USE_POLLY_ACC)
#include <polly/Support/LinkGPURuntime.h>
#endif
#endif

// for outputting code
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/Bitcode/BitcodeWriterPass.h>
#include <llvm/Bitcode/BitcodeReader.h>
#include "llvm/Object/ArchiveWriter.h"
#include <llvm/IR/IRPrintingPasses.h>

#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/FormatAdapters.h>
#include <llvm/Linker/Linker.h>


using namespace llvm;

#include "jitlayers.h"
#include "serialize.h"
#include "julia_assert.h"
#include "llvm-codegen-shared.h"
#include "processor.h"

#define DEBUG_TYPE "julia_aotcompile"

STATISTIC(CICacheLookups, "Number of codeinst cache lookups");
STATISTIC(CreateNativeCalls, "Number of jl_create_native calls made");
STATISTIC(CreateNativeMethods, "Number of methods compiled for jl_create_native");
STATISTIC(CreateNativeMax, "Max number of methods compiled at once for jl_create_native");
STATISTIC(CreateNativeGlobals, "Number of globals compiled for jl_create_native");

static void addComdat(GlobalValue *G, Triple &T)
{
    if (T.isOSBinFormatCOFF() && !G->isDeclaration()) {
        // add __declspec(dllexport) to everything marked for export
        assert(G->hasExternalLinkage() && "Cannot set DLLExport on non-external linkage!");
        G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
    }
}


typedef struct {
    orc::ThreadSafeModule M;
    std::vector<GlobalValue*> jl_sysimg_fvars;
    std::vector<GlobalValue*> jl_sysimg_gvars;
    std::map<jl_code_instance_t*, std::tuple<uint32_t, uint32_t>> jl_fvar_map;
    std::vector<void*> jl_value_to_llvm;
    std::vector<jl_code_instance_t*> jl_external_to_llvm;
} jl_native_code_desc_t;

extern "C" JL_DLLEXPORT
void jl_get_function_id_impl(void *native_code, jl_code_instance_t *codeinst,
        int32_t *func_idx, int32_t *specfunc_idx)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data) {
        // get the function index in the fvar lookup table
        auto it = data->jl_fvar_map.find(codeinst);
        if (it != data->jl_fvar_map.end()) {
            std::tie(*func_idx, *specfunc_idx) = it->second;
        }
    }
}

extern "C" JL_DLLEXPORT
void jl_get_llvm_gvs_impl(void *native_code, arraylist_t *gvs)
{
    // map a memory location (jl_value_t or jl_binding_t) to a GlobalVariable
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    arraylist_grow(gvs, data->jl_value_to_llvm.size());
    memcpy(gvs->items, data->jl_value_to_llvm.data(), gvs->len * sizeof(void*));
}

extern "C" JL_DLLEXPORT
void jl_get_llvm_external_fns_impl(void *native_code, arraylist_t *external_fns)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    arraylist_grow(external_fns, data->jl_external_to_llvm.size());
    memcpy(external_fns->items, data->jl_external_to_llvm.data(),
        external_fns->len * sizeof(jl_code_instance_t*));
}

extern "C" JL_DLLEXPORT
LLVMOrcThreadSafeModuleRef jl_get_llvm_module_impl(void *native_code)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data)
        return wrap(&data->M);
    else
        return NULL;
}

extern "C" JL_DLLEXPORT
GlobalValue* jl_get_llvm_function_impl(void *native_code, uint32_t idx)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data)
        return data->jl_sysimg_fvars[idx];
    else
        return NULL;
}


static void emit_offset_table(Module &mod, const std::vector<GlobalValue*> &vars, StringRef name, Type *T_psize)
{
    // Emit a global variable with all the variable addresses.
    // The cloning pass will convert them into offsets.
    size_t nvars = vars.size();
    std::vector<Constant*> addrs(nvars);
    for (size_t i = 0; i < nvars; i++) {
        Constant *var = vars[i];
        addrs[i] = ConstantExpr::getBitCast(var, T_psize);
    }
    ArrayType *vars_type = ArrayType::get(T_psize, nvars);
    new GlobalVariable(mod, vars_type, true,
                       GlobalVariable::ExternalLinkage,
                       ConstantArray::get(vars_type, addrs),
                       name);
}

static bool is_safe_char(unsigned char c)
{
    return ('0' <= c && c <= '9') ||
           ('A' <= c && c <= 'Z') ||
           ('a' <= c && c <= 'z') ||
           (c == '_' || c == '$') ||
           (c >= 128 && c < 255);
}

static const char hexchars[16] = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

static const char *const common_names[256] = {
//  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x00
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x10
    "SP", "NOT", "DQT", "YY", 0, "REM", "AND", "SQT", // 0x20
      "LPR", "RPR", "MUL", "SUM", 0, "SUB", "DOT", "DIV", // 0x28
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "COL", 0, "LT", "EQ", "GT", "QQ", // 0x30
    "AT", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x40
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "LBR", "RDV", "RBR", "POW", 0, // 0x50
    "TIC", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0x60
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "LCR", "OR", "RCR", "TLD", "DEL", // 0x70
    0 }; // remainder is filled with zeros, though are also all safe characters

// reversibly removes special characters from the name of GlobalObjects,
// which might cause them to be treated special by LLVM or the system linker
// the only non-identifier characters we allow to appear are '.' and '$',
// and all of UTF-8 above code-point 128 (except 255)
// most are given "friendly" abbreviations
// the remaining few will print as hex
// e.g. mangles "llvm.a≠a$a!a##" as "llvmDOT.a≠a$aNOT.aYY.YY."
static void makeSafeName(GlobalObject &G)
{
    StringRef Name = G.getName();
    SmallVector<char, 32> SafeName;
    for (unsigned char c : Name.bytes()) {
        if (is_safe_char(c)) {
            SafeName.push_back(c);
        }
        else {
            if (common_names[c]) {
                SafeName.push_back(common_names[c][0]);
                SafeName.push_back(common_names[c][1]);
                if (common_names[c][2])
                    SafeName.push_back(common_names[c][2]);
            }
            else {
                SafeName.push_back(hexchars[(c >> 4) & 0xF]);
                SafeName.push_back(hexchars[c & 0xF]);
            }
            SafeName.push_back('.');
        }
    }
    if (SafeName.size() != Name.size())
        G.setName(StringRef(SafeName.data(), SafeName.size()));
}

static void jl_ci_cache_lookup(const jl_cgparams_t &cgparams, jl_method_instance_t *mi, size_t world, jl_code_instance_t **ci_out, jl_code_info_t **src_out)
{
    ++CICacheLookups;
    jl_value_t *ci = cgparams.lookup(mi, world, world);
    JL_GC_PROMISE_ROOTED(ci);
    jl_code_instance_t *codeinst = NULL;
    if (ci != jl_nothing) {
        codeinst = (jl_code_instance_t*)ci;
        *src_out = (jl_code_info_t*)jl_atomic_load_relaxed(&codeinst->inferred);
        jl_method_t *def = codeinst->def->def.method;
        if ((jl_value_t*)*src_out == jl_nothing)
            *src_out = NULL;
        if (*src_out && jl_is_method(def))
            *src_out = jl_uncompress_ir(def, codeinst, (jl_array_t*)*src_out);
    }
    if (*src_out == NULL || !jl_is_code_info(*src_out)) {
        if (cgparams.lookup != jl_rettype_inferred) {
            jl_error("Refusing to automatically run type inference with custom cache lookup.");
        }
        else {
            *src_out = jl_type_infer(mi, world, 0);
            if (*src_out) {
                codeinst = jl_get_method_inferred(mi, (*src_out)->rettype, (*src_out)->min_world, (*src_out)->max_world);
                if ((*src_out)->inferred) {
                    jl_value_t *null = nullptr;
                    jl_atomic_cmpswap_relaxed(&codeinst->inferred, &null, jl_nothing);
                }
            }
        }
    }
    *ci_out = codeinst;
}

void replaceUsesWithLoad(Function &F, function_ref<GlobalVariable *(Instruction &I)> should_replace, MDNode *tbaa_const);

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup, and can
// also be used be extern consumers like GPUCompiler.jl to obtain a module containing
// all reachable & inferrrable functions.
// The `policy` flag switches between the default mode `0` and the extern mode `1` used by GPUCompiler.
// `_imaging_mode` controls if raw pointers can be embedded (e.g. the code will be loaded into the same session).
// `_external_linkage` create linkages between pkgimages.
extern "C" JL_DLLEXPORT
void *jl_create_native_impl(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmmod, const jl_cgparams_t *cgparams, int _policy, int _imaging_mode, int _external_linkage, size_t _world)
{
    ++CreateNativeCalls;
    CreateNativeMax.updateMax(jl_array_len(methods));
    if (cgparams == NULL)
        cgparams = &jl_default_cgparams;
    jl_native_code_desc_t *data = new jl_native_code_desc_t;
    CompilationPolicy policy = (CompilationPolicy) _policy;
    bool imaging = imaging_default() || _imaging_mode == 1;
    jl_workqueue_t emitted;
    jl_method_instance_t *mi = NULL;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    auto ct = jl_current_task;
    ct->reentrant_timing++;
    orc::ThreadSafeContext ctx;
    orc::ThreadSafeModule backing;
    if (!llvmmod) {
        ctx = jl_ExecutionEngine->acquireContext();
        backing = jl_create_ts_module("text", ctx, imaging);
    }
    orc::ThreadSafeModule &clone = llvmmod ? *unwrap(llvmmod) : backing;
    auto ctxt = clone.getContext();

    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();

    // compile all methods for the current world and type-inference world

    JL_LOCK(&jl_codegen_lock);
    jl_codegen_params_t params(ctxt);
    params.params = cgparams;
    params.imaging = imaging;
    params.external_linkage = _external_linkage;
    size_t compile_for[] = { jl_typeinf_world, _world };
    for (int worlds = 0; worlds < 2; worlds++) {
        params.world = compile_for[worlds];
        if (!params.world)
            continue;
        // Don't emit methods for the typeinf_world with extern policy
        if (policy != CompilationPolicy::Default && params.world == jl_typeinf_world)
            continue;
        size_t i, l;
        for (i = 0, l = jl_array_len(methods); i < l; i++) {
            // each item in this list is either a MethodInstance indicating something
            // to compile, or an svec(rettype, sig) describing a C-callable alias to create.
            jl_value_t *item = jl_array_ptr_ref(methods, i);
            if (jl_is_simplevector(item)) {
                if (worlds == 1)
                    jl_compile_extern_c(wrap(&clone), &params, NULL, jl_svecref(item, 0), jl_svecref(item, 1));
                continue;
            }
            mi = (jl_method_instance_t*)item;
            src = NULL;
            // if this method is generally visible to the current compilation world,
            // and this is either the primary world, or not applicable in the primary world
            // then we want to compile and emit this
            if (mi->def.method->primary_world <= params.world && params.world <= mi->def.method->deleted_world) {
                // find and prepare the source code to compile
                jl_code_instance_t *codeinst = NULL;
                jl_ci_cache_lookup(*cgparams, mi, params.world, &codeinst, &src);
                if (src && !emitted.count(codeinst)) {
                    // now add it to our compilation results
                    JL_GC_PROMISE_ROOTED(codeinst->rettype);
                    orc::ThreadSafeModule result_m = jl_create_ts_module(name_from_method_instance(codeinst->def),
                            params.tsctx, params.imaging,
                            clone.getModuleUnlocked()->getDataLayout(),
                            Triple(clone.getModuleUnlocked()->getTargetTriple()));
                    jl_llvm_functions_t decls = jl_emit_code(result_m, mi, src, codeinst->rettype, params);
                    if (result_m)
                        emitted[codeinst] = {std::move(result_m), std::move(decls)};
                }
            }
        }

        // finally, make sure all referenced methods also get compiled or fixed up
        jl_compile_workqueue(emitted, *clone.getModuleUnlocked(), params, policy);
    }
    JL_UNLOCK(&jl_codegen_lock); // Might GC
    JL_GC_POP();

    // process the globals array, before jl_merge_module destroys them
    std::vector<std::string> gvars(params.globals.size());
    data->jl_value_to_llvm.resize(params.globals.size());
    StringSet<> gvars_names;
    DenseSet<GlobalValue *> gvars_set;

    size_t idx = 0;
    for (auto &global : params.globals) {
        gvars[idx] = global.second->getName().str();
        assert(gvars_set.insert(global.second).second && "Duplicate gvar in params!");
        assert(gvars_names.insert(gvars[idx]).second && "Duplicate gvar name in params!");
        data->jl_value_to_llvm[idx] = global.first;
        idx++;
    }
    CreateNativeMethods += emitted.size();

    size_t offset = gvars.size();
    data->jl_external_to_llvm.resize(params.external_fns.size());

    for (auto &extern_fn : params.external_fns) {
        jl_code_instance_t *this_code = std::get<0>(extern_fn.first);
        bool specsig = std::get<1>(extern_fn.first);
        assert(specsig && "Error external_fns doesn't handle non-specsig yet");
        (void) specsig;
        GlobalVariable *F = extern_fn.second;
        size_t idx = gvars.size() - offset;
        assert(idx >= 0);
        assert(idx < data->jl_external_to_llvm.size());
        data->jl_external_to_llvm[idx] = this_code;
        assert(gvars_set.insert(F).second && "Duplicate gvar in params!");
        assert(gvars_names.insert(F->getName()).second && "Duplicate gvar name in params!");
        gvars.push_back(std::string(F->getName()));
    }

    // clones the contents of the module `m` to the shadow_output collector
    // while examining and recording what kind of function pointer we have
    Linker L(*clone.getModuleUnlocked());
    for (auto &def : emitted) {
        jl_merge_module(clone, std::move(std::get<0>(def.second)));
        jl_code_instance_t *this_code = def.first;
        jl_llvm_functions_t decls = std::get<1>(def.second);
        StringRef func = decls.functionObject;
        StringRef cfunc = decls.specFunctionObject;
        uint32_t func_id = 0;
        uint32_t cfunc_id = 0;
        if (func == "jl_fptr_args") {
            func_id = -1;
        }
        else if (func == "jl_fptr_sparam") {
            func_id = -2;
        }
        else {
            //Safe b/c context is locked by params
            data->jl_sysimg_fvars.push_back(cast<Function>(clone.getModuleUnlocked()->getNamedValue(func)));
            func_id = data->jl_sysimg_fvars.size();
        }
        if (!cfunc.empty()) {
            //Safe b/c context is locked by params
            data->jl_sysimg_fvars.push_back(cast<Function>(clone.getModuleUnlocked()->getNamedValue(cfunc)));
            cfunc_id = data->jl_sysimg_fvars.size();
        }
        data->jl_fvar_map[this_code] = std::make_tuple(func_id, cfunc_id);
    }
    if (params._shared_module) {
        bool error = L.linkInModule(std::move(params._shared_module));
        assert(!error && "Error linking in shared module");
        (void)error;
    }

    // now get references to the globals in the merged module
    // and set them to be internalized and initialized at startup
    for (auto &global : gvars) {
        //Safe b/c context is locked by params
        GlobalVariable *G = cast<GlobalVariable>(clone.getModuleUnlocked()->getNamedValue(global));
        G->setInitializer(ConstantPointerNull::get(cast<PointerType>(G->getValueType())));
        G->setLinkage(GlobalValue::ExternalLinkage);
        G->setVisibility(GlobalValue::HiddenVisibility);
        data->jl_sysimg_gvars.push_back(G);
    }
    CreateNativeGlobals += gvars.size();

    //Safe b/c context is locked by params
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    // setting the function personality enables stack unwinding and catching exceptions
    // so make sure everything has something set
    Type *T_int32 = Type::getInt32Ty(clone.getModuleUnlocked()->getContext());
    Function *juliapersonality_func =
       Function::Create(FunctionType::get(T_int32, true),
           Function::ExternalLinkage, "__julia_personality", clone.getModuleUnlocked());
    juliapersonality_func->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
#endif

    // move everything inside, now that we've merged everything
    // (before adding the exported headers)
    if (policy == CompilationPolicy::Default) {
        //Safe b/c context is locked by params
        for (GlobalObject &G : clone.getModuleUnlocked()->global_objects()) {
            if (!G.isDeclaration()) {
                G.setLinkage(GlobalValue::ExternalLinkage);
                G.setVisibility(GlobalValue::HiddenVisibility);
                makeSafeName(G);
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
                // Add unwind exception personalities to functions to handle async exceptions
                if (Function *F = dyn_cast<Function>(&G))
                    F->setPersonalityFn(juliapersonality_func);
#endif
            }
        }
    }

    data->M = std::move(clone);
    if (!ct->reentrant_timing-- && measure_compile_time_enabled) {
        auto end = jl_hrtime();
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
    }
    if (ctx.getContext()) {
        jl_ExecutionEngine->releaseContext(std::move(ctx));
    }
    return (void*)data;
}

static object::Archive::Kind getDefaultForHost(Triple &triple)
{
      if (triple.isOSDarwin())
          return object::Archive::K_DARWIN;
      return object::Archive::K_GNU;
}

typedef Error ArchiveWriterError;
static void reportWriterError(const ErrorInfoBase &E)
{
    std::string err = E.message();
    jl_safe_printf("ERROR: failed to emit output file %s\n", err.c_str());
}

static void injectCRTAlias(Module &M, StringRef name, StringRef alias, FunctionType *FT)
{
    Function *target = M.getFunction(alias);
    if (!target) {
        target = Function::Create(FT, Function::ExternalLinkage, alias, M);
    }
    Function *interposer = Function::Create(FT, Function::InternalLinkage, name, M);
    appendToCompilerUsed(M, {interposer});

    llvm::IRBuilder<> builder(BasicBlock::Create(M.getContext(), "top", interposer));
    SmallVector<Value *, 4> CallArgs;
    for (auto &arg : interposer->args())
        CallArgs.push_back(&arg);
    auto val = builder.CreateCall(target, CallArgs);
    builder.CreateRet(val);
}

void multiversioning_preannotate(Module &M);

// See src/processor.h for documentation about this table. Corresponds to jl_image_shard_t.
static GlobalVariable *emit_shard_table(Module &M, Type *T_size, Type *T_psize, unsigned threads) {
    SmallVector<Constant *, 0> tables(sizeof(jl_image_shard_t) / sizeof(void *) * threads);
    for (unsigned i = 0; i < threads; i++) {
        auto suffix = "_" + std::to_string(i);
        auto create_gv = [&](StringRef name, bool constant) {
            auto gv = new GlobalVariable(M, T_size, constant,
                                         GlobalValue::ExternalLinkage, nullptr, name + suffix);
            gv->setVisibility(GlobalValue::HiddenVisibility);
            return gv;
        };
        auto table = tables.data() + i * sizeof(jl_image_shard_t) / sizeof(void *);
        table[offsetof(jl_image_shard_t, fvar_base) / sizeof(void*)] = create_gv("jl_fvar_base", false);
        table[offsetof(jl_image_shard_t, fvar_offsets) / sizeof(void*)] = create_gv("jl_fvar_offsets", true);
        table[offsetof(jl_image_shard_t, fvar_idxs) / sizeof(void*)] = create_gv("jl_fvar_idxs", true);
        table[offsetof(jl_image_shard_t, gvar_base) / sizeof(void*)] = create_gv("jl_gvar_base", false);
        table[offsetof(jl_image_shard_t, gvar_offsets) / sizeof(void*)] = create_gv("jl_gvar_offsets", true);
        table[offsetof(jl_image_shard_t, gvar_idxs) / sizeof(void*)] = create_gv("jl_gvar_idxs", true);
        table[offsetof(jl_image_shard_t, clone_slots) / sizeof(void*)] = create_gv("jl_clone_slots", true);
        table[offsetof(jl_image_shard_t, clone_offsets) / sizeof(void*)] = create_gv("jl_clone_offsets", true);
        table[offsetof(jl_image_shard_t, clone_idxs) / sizeof(void*)] = create_gv("jl_clone_idxs", true);
    }
    auto tables_arr = ConstantArray::get(ArrayType::get(T_psize, tables.size()), tables);
    auto tables_gv = new GlobalVariable(M, tables_arr->getType(), false,
                                        GlobalValue::ExternalLinkage, tables_arr, "jl_shard_tables");
    tables_gv->setVisibility(GlobalValue::HiddenVisibility);
    return tables_gv;
}

// See src/processor.h for documentation about this table. Corresponds to jl_image_ptls_t.
static GlobalVariable *emit_ptls_table(Module &M, Type *T_size, Type *T_psize) {
    std::array<Constant *, 3> ptls_table{
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_pgcstack_func_slot"),
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_pgcstack_key_slot"),
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_tls_offset"),
    };
    for (auto &gv : ptls_table)
        cast<GlobalVariable>(gv)->setVisibility(GlobalValue::HiddenVisibility);
    auto ptls_table_arr = ConstantArray::get(ArrayType::get(T_psize, ptls_table.size()), ptls_table);
    auto ptls_table_gv = new GlobalVariable(M, ptls_table_arr->getType(), false,
                                            GlobalValue::ExternalLinkage, ptls_table_arr, "jl_ptls_table");
    ptls_table_gv->setVisibility(GlobalValue::HiddenVisibility);
    return ptls_table_gv;
}

// See src/processor.h for documentation about this table. Corresponds to jl_image_header_t.
static GlobalVariable *emit_image_header(Module &M, unsigned threads, unsigned nfvars, unsigned ngvars) {
    constexpr uint32_t version = 1;
    std::array<uint32_t, 4> header{
        version,
        threads,
        nfvars,
        ngvars,
    };
    auto header_arr = ConstantDataArray::get(M.getContext(), header);
    auto header_gv = new GlobalVariable(M, header_arr->getType(), false,
                                        GlobalValue::InternalLinkage, header_arr, "jl_image_header");
    return header_gv;
}

// Grab fvars and gvars data from the module
static void get_fvars_gvars(Module &M, DenseMap<GlobalValue *, unsigned> &fvars, DenseMap<GlobalValue *, unsigned> &gvars) {
    auto fvars_gv = M.getGlobalVariable("jl_fvars");
    auto gvars_gv = M.getGlobalVariable("jl_gvars");
    auto fvars_idxs = M.getGlobalVariable("jl_fvar_idxs");
    auto gvars_idxs = M.getGlobalVariable("jl_gvar_idxs");
    assert(fvars_gv);
    assert(gvars_gv);
    assert(fvars_idxs);
    assert(gvars_idxs);
    auto fvars_init = cast<ConstantArray>(fvars_gv->getInitializer());
    auto gvars_init = cast<ConstantArray>(gvars_gv->getInitializer());
    for (unsigned i = 0; i < fvars_init->getNumOperands(); ++i) {
        auto gv = cast<GlobalValue>(fvars_init->getOperand(i)->stripPointerCasts());
        assert(gv && gv->hasName() && "fvar must be a named global");
        assert(!fvars.count(gv) && "Duplicate fvar");
        fvars[gv] = i;
    }
    assert(fvars.size() == fvars_init->getNumOperands());
    for (unsigned i = 0; i < gvars_init->getNumOperands(); ++i) {
        auto gv = cast<GlobalValue>(gvars_init->getOperand(i)->stripPointerCasts());
        assert(gv && gv->hasName() && "gvar must be a named global");
        assert(!gvars.count(gv) && "Duplicate gvar");
        gvars[gv] = i;
    }
    assert(gvars.size() == gvars_init->getNumOperands());
    fvars_gv->eraseFromParent();
    gvars_gv->eraseFromParent();
    fvars_idxs->eraseFromParent();
    gvars_idxs->eraseFromParent();
}

// Weight computation
// It is important for multithreaded image building to be able to split work up
// among the threads equally. The weight calculated here is an estimation of
// how expensive a particular function is going to be to compile.

struct FunctionInfo {
    size_t weight;
    size_t bbs;
    size_t insts;
    size_t clones;
};

static FunctionInfo getFunctionWeight(const Function &F)
{
    FunctionInfo info;
    info.weight = 1;
    info.bbs = F.size();
    info.insts = 0;
    info.clones = 1;
    for (const BasicBlock &BB : F) {
        info.insts += BB.size();
    }
    if (F.hasFnAttribute("julia.mv.clones")) {
        auto val = F.getFnAttribute("julia.mv.clones").getValueAsString();
        // base16, so must be at most 4 * length bits long
        // popcount gives number of clones
        info.clones = APInt(val.size() * 4, val, 16).countPopulation() + 1;
    }
    info.weight += info.insts;
    // more basic blocks = more complex than just sum of insts,
    // add some weight to it
    info.weight += info.bbs;
    info.weight *= info.clones;
    return info;
}

struct ModuleInfo {
    size_t globals;
    size_t funcs;
    size_t bbs;
    size_t insts;
    size_t clones;
    size_t weight;
};

ModuleInfo compute_module_info(Module &M) {
    ModuleInfo info;
    info.globals = 0;
    info.funcs = 0;
    info.bbs = 0;
    info.insts = 0;
    info.clones = 0;
    info.weight = 0;
    for (auto &G : M.global_values()) {
        if (G.isDeclaration()) {
            continue;
        }
        info.globals++;
        if (auto F = dyn_cast<Function>(&G)) {
            info.funcs++;
            auto func_info = getFunctionWeight(*F);
            info.bbs += func_info.bbs;
            info.insts += func_info.insts;
            info.clones += func_info.clones;
            info.weight += func_info.weight;
        } else {
            info.weight += 1;
        }
    }
    return info;
}

struct Partition {
    StringSet<> globals;
    StringMap<unsigned> fvars;
    StringMap<unsigned> gvars;
    size_t weight;
};

static inline bool verify_partitioning(const SmallVectorImpl<Partition> &partitions, const Module &M, size_t fvars_size, size_t gvars_size) {
    bool bad = false;
#ifndef JL_NDEBUG
    SmallVector<uint32_t> fvars(fvars_size);
    SmallVector<uint32_t> gvars(gvars_size);
    StringMap<uint32_t> GVNames;
    for (uint32_t i = 0; i < partitions.size(); i++) {
        for (auto &name : partitions[i].globals) {
            if (GVNames.count(name.getKey())) {
                bad = true;
                dbgs() << "Duplicate global name " << name.getKey() << " in partitions " << i << " and " << GVNames[name.getKey()] << "\n";
            }
            GVNames[name.getKey()] = i;
        }
        for (auto &fvar : partitions[i].fvars) {
            if (fvars[fvar.second] != 0) {
                bad = true;
                dbgs() << "Duplicate fvar " << fvar.first() << " in partitions " << i << " and " << fvars[fvar.second] - 1 << "\n";
            }
            fvars[fvar.second] = i+1;
        }
        for (auto &gvar : partitions[i].gvars) {
            if (gvars[gvar.second] != 0) {
                bad = true;
                dbgs() << "Duplicate gvar " << gvar.first() << " in partitions " << i << " and " << gvars[gvar.second] - 1 << "\n";
            }
            gvars[gvar.second] = i+1;
        }
    }
    for (auto &GV : M.globals()) {
        if (GV.isDeclaration()) {
            if (GVNames.count(GV.getName())) {
                bad = true;
                dbgs() << "Global " << GV.getName() << " is a declaration but is in partition " << GVNames[GV.getName()] << "\n";
            }
        } else {
            if (!GVNames.count(GV.getName())) {
                bad = true;
                dbgs() << "Global " << GV << " not in any partition\n";
            }
            if (!GV.hasExternalLinkage()) {
                bad = true;
                dbgs() << "Global " << GV << " has non-external linkage " << GV.getLinkage() << " but is in partition " << GVNames[GV.getName()] << "\n";
            }
        }
    }
    for (uint32_t i = 0; i < fvars_size; i++) {
        if (fvars[i] == 0) {
            bad = true;
            dbgs() << "fvar " << i << " not in any partition\n";
        }
    }
    for (uint32_t i = 0; i < gvars_size; i++) {
        if (gvars[i] == 0) {
            bad = true;
            dbgs() << "gvar " << i << " not in any partition\n";
        }
    }
#endif
    return !bad;
}

// Chop a module up as equally as possible by weight into threads partitions
static SmallVector<Partition, 32> partitionModule(Module &M, unsigned threads) {
    //Start by stripping fvars and gvars, which helpfully removes their uses as well
    DenseMap<GlobalValue *, unsigned> fvars, gvars;
    get_fvars_gvars(M, fvars, gvars);

    // Partition by union-find, since we only have def->use traversal right now
    struct Partitioner {
        struct Node {
            GlobalValue *GV;
            unsigned parent;
            unsigned size;
            size_t weight;
        };
        std::vector<Node> nodes;
        DenseMap<GlobalValue *, unsigned> node_map;
        unsigned merged;

        unsigned make(GlobalValue *GV, size_t weight) {
            unsigned idx = nodes.size();
            nodes.push_back({GV, idx, 1, weight});
            node_map[GV] = idx;
            return idx;
        }

        unsigned find(unsigned idx) {
            while (nodes[idx].parent != idx) {
                nodes[idx].parent = nodes[nodes[idx].parent].parent;
                idx = nodes[idx].parent;
            }
            return idx;
        }

        unsigned merge(unsigned x, unsigned y) {
            x = find(x);
            y = find(y);
            if (x == y)
                return x;
            if (nodes[x].size < nodes[y].size)
                std::swap(x, y);
            nodes[y].parent = x;
            nodes[x].size += nodes[y].size;
            nodes[x].weight += nodes[y].weight;
            merged++;
            return x;
        }
    };

    Partitioner partitioner;

    for (auto &G : M.global_values()) {
        if (G.isDeclaration())
            continue;
        if (isa<Function>(G)) {
            partitioner.make(&G, getFunctionWeight(cast<Function>(G)).weight);
        } else {
            partitioner.make(&G, 1);
        }
    }

    // Merge all uses to go together into the same partition
    for (unsigned i = 0; i < partitioner.nodes.size(); ++i) {
        for (ConstantUses<GlobalValue> uses(partitioner.nodes[i].GV, M); !uses.done(); uses.next()) {
            auto val = uses.get_info().val;
            auto idx = partitioner.node_map.find(val);
            assert(idx != partitioner.node_map.end());
            partitioner.merge(i, idx->second);
        }
    }

    SmallVector<Partition, 32> partitions(threads);
    // always get the smallest partition first
    auto pcomp = [](const Partition *p1, const Partition *p2) {
        return p1->weight > p2->weight;
    };
    std::priority_queue<Partition *, std::vector<Partition *>, decltype(pcomp)> pq(pcomp);
    for (unsigned i = 0; i < threads; ++i) {
        pq.push(&partitions[i]);
    }

    std::vector<unsigned> idxs(partitioner.nodes.size());
    std::iota(idxs.begin(), idxs.end(), 0);
    std::sort(idxs.begin(), idxs.end(), [&](unsigned a, unsigned b) {
        //because roots have more weight than their children,
        //we can sort by weight and get the roots first
        return partitioner.nodes[a].weight > partitioner.nodes[b].weight;
    });

    // Assign the root of each partition to a partition, then assign its children to the same one
    for (unsigned idx = 0; idx < idxs.size(); ++idx) {
        auto i = idxs[idx];
        auto root = partitioner.find(i);
        assert(root == i || partitioner.nodes[root].GV == nullptr);
        if (partitioner.nodes[root].GV) {
            auto &node = partitioner.nodes[root];
            auto &P = *pq.top();
            pq.pop();
            auto name = node.GV->getName();
            P.globals.insert(name);
            if (fvars.count(node.GV))
                P.fvars[name] = fvars[node.GV];
            if (gvars.count(node.GV))
                P.gvars[name] = gvars[node.GV];
            P.weight += node.weight;
            node.GV = nullptr;
            node.size = &P - partitions.data();
            pq.push(&P);
        }
        if (root != i) {
            auto &node = partitioner.nodes[i];
            assert(node.GV != nullptr);
            // we assigned its root already, so just add it to the root's partition
            // don't touch the priority queue, since we're not changing the weight
            auto &P = partitions[partitioner.nodes[root].size];
            auto name = node.GV->getName();
            P.globals.insert(name);
            if (fvars.count(node.GV))
                P.fvars[name] = fvars[node.GV];
            if (gvars.count(node.GV))
                P.gvars[name] = gvars[node.GV];
            node.GV = nullptr;
            node.size = partitioner.nodes[root].size;
        }
    }

    bool verified = verify_partitioning(partitions, M, fvars.size(), gvars.size());
    assert(verified && "Partitioning failed to partition globals correctly");
    (void) verified;

    return partitions;
}

struct ImageTimer {
    uint64_t elapsed = 0;
    std::string name;
    std::string desc;

    void startTimer() {
        elapsed = jl_hrtime();
    }

    void stopTimer() {
        elapsed = jl_hrtime() - elapsed;
    }

    void init(const Twine &name, const Twine &desc) {
        this->name = name.str();
        this->desc = desc.str();
    }

    operator bool() const {
        return elapsed != 0;
    }

    void print(raw_ostream &out, bool clear=false) {
        if (!*this)
            return;
        out << llvm::formatv("{0:F3}  ", elapsed / 1e9) << name << "  " << desc << "\n";
        if (clear)
            elapsed = 0;
    }
};

struct ShardTimers {
    ImageTimer deserialize;
    ImageTimer materialize;
    ImageTimer construct;
    ImageTimer deletion;
    // impl timers
    ImageTimer unopt;
    ImageTimer optimize;
    ImageTimer opt;
    ImageTimer obj;
    ImageTimer asm_;

    std::string name;
    std::string desc;

    void print(raw_ostream &out, bool clear=false) {
        StringRef sep = "===-------------------------------------------------------------------------===";
        out << formatv("{0}\n{1}\n{0}\n", sep, fmt_align(name + " : " + desc, AlignStyle::Center, sep.size()));
        auto total = deserialize.elapsed + materialize.elapsed + construct.elapsed + deletion.elapsed +
            unopt.elapsed + optimize.elapsed + opt.elapsed + obj.elapsed + asm_.elapsed;
        out << "Time (s)  Name  Description\n";
        deserialize.print(out, clear);
        materialize.print(out, clear);
        construct.print(out, clear);
        deletion.print(out, clear);
        unopt.print(out, clear);
        optimize.print(out, clear);
        opt.print(out, clear);
        obj.print(out, clear);
        asm_.print(out, clear);
        out << llvm::formatv("{0:F3}  total  Total time taken\n", total / 1e9);
    }
};

// Perform the actual optimization and emission of the output files
static void add_output_impl(Module &M, TargetMachine &SourceTM, std::string *outputs, ArrayRef<StringRef> names,
                    NewArchiveMember *unopt, NewArchiveMember *opt, NewArchiveMember *obj, NewArchiveMember *asm_,
                    ShardTimers &timers, unsigned shardidx) {
    assert(names.size() == 4);
    auto TM = std::unique_ptr<TargetMachine>(
        SourceTM.getTarget().createTargetMachine(
            SourceTM.getTargetTriple().str(),
            SourceTM.getTargetCPU(),
            SourceTM.getTargetFeatureString(),
            SourceTM.Options,
            SourceTM.getRelocationModel(),
            SourceTM.getCodeModel(),
            SourceTM.getOptLevel()));

    if (unopt) {
        timers.unopt.startTimer();
        raw_string_ostream OS(*outputs);
        PassBuilder PB;
        AnalysisManagers AM{*TM, PB, OptimizationLevel::O0};
        ModulePassManager MPM;
        MPM.addPass(BitcodeWriterPass(OS));
        *unopt = NewArchiveMember(MemoryBufferRef(*outputs, names[0]));
        outputs++;
        timers.unopt.stopTimer();
    }
    if (!opt && !obj && !asm_) {
        return;
    }
    assert(!verifyModule(M, &errs()));

    timers.optimize.startTimer();

#ifndef JL_USE_NEW_PM
    legacy::PassManager optimizer;
    addTargetPasses(&optimizer, TM->getTargetTriple(), TM->getTargetIRAnalysis());
    addOptimizationPasses(&optimizer, jl_options.opt_level, true, true);
    addMachinePasses(&optimizer, jl_options.opt_level);
#else

    auto PMTM = std::unique_ptr<TargetMachine>(
        SourceTM.getTarget().createTargetMachine(
            SourceTM.getTargetTriple().str(),
            SourceTM.getTargetCPU(),
            SourceTM.getTargetFeatureString(),
            SourceTM.Options,
            SourceTM.getRelocationModel(),
            SourceTM.getCodeModel(),
            SourceTM.getOptLevel()));
    NewPM optimizer{std::move(PMTM), getOptLevel(jl_options.opt_level), OptimizationOptions::defaults(true, true)};
#endif
    optimizer.run(M);
    assert(!verifyModule(M, &errs()));
    bool inject_aliases = false;
    for (auto &F : M.functions()) {
        if (!F.isDeclaration() && F.getName() != "_DllMainCRTStartup") {
            inject_aliases = true;
            break;
        }
    }
    // no need to inject aliases if we have no functions
    if (inject_aliases) {
        // We would like to emit an alias or an weakref alias to redirect these symbols
        // but LLVM doesn't let us emit a GlobalAlias to a declaration...
        // So for now we inject a definition of these functions that calls our runtime
        // functions. We do so after optimization to avoid cloning these functions.
        injectCRTAlias(M, "__gnu_h2f_ieee", "julia__gnu_h2f_ieee",
                FunctionType::get(Type::getFloatTy(M.getContext()), { Type::getHalfTy(M.getContext()) }, false));
        injectCRTAlias(M, "__extendhfsf2", "julia__gnu_h2f_ieee",
                FunctionType::get(Type::getFloatTy(M.getContext()), { Type::getHalfTy(M.getContext()) }, false));
        injectCRTAlias(M, "__gnu_f2h_ieee", "julia__gnu_f2h_ieee",
                FunctionType::get(Type::getHalfTy(M.getContext()), { Type::getFloatTy(M.getContext()) }, false));
        injectCRTAlias(M, "__truncsfhf2", "julia__gnu_f2h_ieee",
                FunctionType::get(Type::getHalfTy(M.getContext()), { Type::getFloatTy(M.getContext()) }, false));
        injectCRTAlias(M, "__truncdfhf2", "julia__truncdfhf2",
                FunctionType::get(Type::getHalfTy(M.getContext()), { Type::getDoubleTy(M.getContext()) }, false));
    }

    timers.optimize.stopTimer();

    if (opt) {
        timers.opt.startTimer();
        raw_string_ostream OS(*outputs);
        PassBuilder PB;
        AnalysisManagers AM{*TM, PB, OptimizationLevel::O0};
        ModulePassManager MPM;
        MPM.addPass(BitcodeWriterPass(OS));
        *opt = NewArchiveMember(MemoryBufferRef(*outputs, names[1]));
        outputs++;
        timers.opt.stopTimer();
    }

    if (obj) {
        timers.obj.startTimer();
        SmallVector<char, 0> Buffer;
        raw_svector_ostream OS(Buffer);
        legacy::PassManager emitter;
        addTargetPasses(&emitter, TM->getTargetTriple(), TM->getTargetIRAnalysis());
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CGFT_ObjectFile, false))
            jl_safe_printf("ERROR: target does not support generation of object files\n");
        emitter.run(M);
        *outputs = { Buffer.data(), Buffer.size() };
        *obj = NewArchiveMember(MemoryBufferRef(*outputs, names[2]));
        outputs++;
        timers.obj.stopTimer();
    }

    if (asm_) {
        timers.asm_.startTimer();
        SmallVector<char, 0> Buffer;
        raw_svector_ostream OS(Buffer);
        legacy::PassManager emitter;
        addTargetPasses(&emitter, TM->getTargetTriple(), TM->getTargetIRAnalysis());
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CGFT_AssemblyFile, false))
            jl_safe_printf("ERROR: target does not support generation of assembly files\n");
        emitter.run(M);
        *outputs = { Buffer.data(), Buffer.size() };
        *asm_ = NewArchiveMember(MemoryBufferRef(*outputs, names[3]));
        outputs++;
        timers.asm_.stopTimer();
    }
}

// serialize module to bitcode
static auto serializeModule(const Module &M) {
    assert(!verifyModule(M, &errs()) && "Serializing invalid module!");
    SmallVector<char, 0> ClonedModuleBuffer;
    BitcodeWriter BCWriter(ClonedModuleBuffer);
    BCWriter.writeModule(M);
    BCWriter.writeSymtab();
    BCWriter.writeStrtab();
    return ClonedModuleBuffer;
}

// Modules are deserialized lazily by LLVM, to avoid deserializing
// unnecessary functions. We take advantage of this by serializing
// the entire module once, then deleting the bodies of functions
// that are not in this partition. Once unnecesary functions are
// deleted, we then materialize the entire module to make use-lists
// consistent.
static void materializePreserved(Module &M, Partition &partition) {
    DenseSet<GlobalValue *> Preserve;
    for (auto &GV : M.global_values()) {
        if (!GV.isDeclaration()) {
            if (partition.globals.count(GV.getName())) {
                Preserve.insert(&GV);
            }
        }
    }
    for (auto &F : M.functions()) {
        if (!F.isDeclaration()) {
            if (!Preserve.contains(&F)) {
                F.deleteBody();
                F.setLinkage(GlobalValue::ExternalLinkage);
            }
        }
    }
    for (auto &GV : M.globals()) {
        if (!GV.isDeclaration()) {
            if (!Preserve.contains(&GV)) {
                GV.setInitializer(nullptr);
                GV.setLinkage(GlobalValue::ExternalLinkage);
            }
        }
    }
    // Global aliases are a pain to deal with. It is illegal to have an alias to a declaration,
    // so we need to replace them with either a function or a global variable declaration. However,
    // we can't just delete the alias, because that would break the users of the alias. Therefore,
    // we do a dance where we point each global alias to a dummy function or global variable,
    // then materialize the module to access use-lists, then replace all the uses, and finally commit
    // to deleting the old alias.
    SmallVector<std::pair<GlobalAlias *, GlobalValue *>> DeletedAliases;
    for (auto &GA : M.aliases()) {
        if (!GA.isDeclaration()) {
            if (!Preserve.contains(&GA)) {
                if (GA.getValueType()->isFunctionTy()) {
                    auto F = Function::Create(cast<FunctionType>(GA.getValueType()), GlobalValue::ExternalLinkage, "", &M);
                    // This is an extremely sad hack to make sure the global alias never points to an extern function
                    auto BB = BasicBlock::Create(M.getContext(), "", F);
                    new UnreachableInst(M.getContext(), BB);
                    GA.setAliasee(F);

                    DeletedAliases.push_back({ &GA, F });
                } else {
                    auto GV = new GlobalVariable(M, GA.getValueType(), false, GlobalValue::ExternalLinkage, Constant::getNullValue(GA.getValueType()));
                    DeletedAliases.push_back({ &GA, GV });
                }
            }
        }
    }
    cantFail(M.materializeAll());
    for (auto &Deleted : DeletedAliases) {
        Deleted.second->takeName(Deleted.first);
        Deleted.first->replaceAllUsesWith(Deleted.second);
        Deleted.first->eraseFromParent();
        // undo our previous sad hack
        if (auto F = dyn_cast<Function>(Deleted.second)) {
            F->deleteBody();
        } else {
            cast<GlobalVariable>(Deleted.second)->setInitializer(nullptr);
        }
    }
}

// Reconstruct jl_fvars, jl_gvars, jl_fvars_idxs, and jl_gvars_idxs from the partition
static void construct_vars(Module &M, Partition &partition) {
    std::vector<std::pair<uint32_t, GlobalValue *>> fvar_pairs;
    fvar_pairs.reserve(partition.fvars.size());
    for (auto &fvar : partition.fvars) {
        auto F = M.getFunction(fvar.first());
        assert(F);
        assert(!F->isDeclaration());
        fvar_pairs.push_back({ fvar.second, F });
    }
    std::vector<GlobalValue *> fvars;
    std::vector<uint32_t> fvar_idxs;
    fvars.reserve(fvar_pairs.size());
    fvar_idxs.reserve(fvar_pairs.size());
    std::sort(fvar_pairs.begin(), fvar_pairs.end());
    for (auto &fvar : fvar_pairs) {
        fvars.push_back(fvar.second);
        fvar_idxs.push_back(fvar.first);
    }
    std::vector<std::pair<uint32_t, GlobalValue *>> gvar_pairs;
    gvar_pairs.reserve(partition.gvars.size());
    for (auto &gvar : partition.gvars) {
        auto GV = M.getGlobalVariable(gvar.first());
        assert(GV);
        assert(!GV->isDeclaration());
        gvar_pairs.push_back({ gvar.second, GV });
    }
    std::vector<GlobalValue *> gvars;
    std::vector<uint32_t> gvar_idxs;
    gvars.reserve(gvar_pairs.size());
    gvar_idxs.reserve(gvar_pairs.size());
    std::sort(gvar_pairs.begin(), gvar_pairs.end());
    for (auto &gvar : gvar_pairs) {
        gvars.push_back(gvar.second);
        gvar_idxs.push_back(gvar.first);
    }

    // Now commit the fvars, gvars, and idxs
    auto T_psize = M.getDataLayout().getIntPtrType(M.getContext())->getPointerTo();
    emit_offset_table(M, fvars, "jl_fvars", T_psize);
    emit_offset_table(M, gvars, "jl_gvars", T_psize);
    auto fidxs = ConstantDataArray::get(M.getContext(), fvar_idxs);
    auto fidxs_var = new GlobalVariable(M, fidxs->getType(), true,
                                        GlobalVariable::ExternalLinkage,
                                        fidxs, "jl_fvar_idxs");
    fidxs_var->setVisibility(GlobalValue::HiddenVisibility);
    auto gidxs = ConstantDataArray::get(M.getContext(), gvar_idxs);
    auto gidxs_var = new GlobalVariable(M, gidxs->getType(), true,
                                        GlobalVariable::ExternalLinkage,
                                        gidxs, "jl_gvar_idxs");
    gidxs_var->setVisibility(GlobalValue::HiddenVisibility);
}

// Materialization will leave many unused declarations, which multiversioning would otherwise clone.
// This function removes them to avoid unnecessary cloning of declarations.
static void dropUnusedDeclarations(Module &M) {
    SmallVector<GlobalValue *> unused;
    for (auto &G : M.global_values()) {
        if (G.isDeclaration()) {
            if (G.use_empty()) {
                unused.push_back(&G);
            } else {
                G.setDSOLocal(false); // These are never going to be seen in the same module again
                G.setVisibility(GlobalValue::DefaultVisibility);
            }
        }
    }
    for (auto &G : unused)
        G->eraseFromParent();
}

// Entrypoint to optionally-multithreaded image compilation. This handles global coordination of the threading,
// as well as partitioning, serialization, and deserialization.
static void add_output(Module &M, TargetMachine &TM, std::vector<std::string> &outputs, ArrayRef<StringRef> names,
                std::vector<NewArchiveMember> &unopt, std::vector<NewArchiveMember> &opt,
                std::vector<NewArchiveMember> &obj, std::vector<NewArchiveMember> &asm_,
                bool unopt_out, bool opt_out, bool obj_out, bool asm_out,
                unsigned threads, ModuleInfo module_info) {
    unsigned outcount = unopt_out + opt_out + obj_out + asm_out;
    assert(outcount);
    outputs.resize(outputs.size() + outcount * threads);
    unopt.resize(unopt.size() + unopt_out * threads);
    opt.resize(opt.size() + opt_out * threads);
    obj.resize(obj.size() + obj_out * threads);
    asm_.resize(asm_.size() + asm_out * threads);
    auto name = names[2];
    name.consume_back(".o");
    // Timers for timing purposes
    TimerGroup timer_group("add_output", ("Time to optimize and emit LLVM module " + name).str());
    SmallVector<ShardTimers, 1> timers(threads);
    for (unsigned i = 0; i < threads; ++i) {
        auto idx = std::to_string(i);
        timers[i].name = "shard_" + idx;
        timers[i].desc = ("Timings for " + name + " module shard " + idx).str();
        timers[i].deserialize.init("deserialize_" + idx, "Deserialize module");
        timers[i].materialize.init("materialize_" + idx, "Materialize declarations");
        timers[i].construct.init("construct_" + idx, "Construct partitioned definitions");
        timers[i].deletion.init("deletion_" + idx, "Delete dead declarations");
        timers[i].unopt.init("unopt_" + idx, "Emit unoptimized bitcode");
        timers[i].optimize.init("optimize_" + idx, "Optimize shard");
        timers[i].opt.init("opt_" + idx, "Emit optimized bitcode");
        timers[i].obj.init("obj_" + idx, "Emit object file");
        timers[i].asm_.init("asm_" + idx, "Emit assembly file");
    }
    Timer partition_timer("partition", "Partition module", timer_group);
    Timer serialize_timer("serialize", "Serialize module", timer_group);
    Timer output_timer("output", "Add outputs", timer_group);
    bool report_timings = false;
    if (auto env = getenv("JULIA_IMAGE_TIMINGS")) {
        char *endptr;
        unsigned long val = strtoul(env, &endptr, 10);
        if (endptr != env && !*endptr && val <= 1) {
            report_timings = val;
        } else {
            if (StringRef("true").compare_insensitive(env) == 0)
                report_timings = true;
            else if (StringRef("false").compare_insensitive(env) == 0)
                report_timings = false;
            else
                errs() << "WARNING: Invalid value for JULIA_IMAGE_TIMINGS: " << env << "\n";
        }
    }
    // Single-threaded case
    if (threads == 1) {
        output_timer.startTimer();
        add_output_impl(M, TM, outputs.data() + outputs.size() - outcount, names,
                        unopt_out ? unopt.data() + unopt.size() - 1 : nullptr,
                        opt_out ? opt.data() + opt.size() - 1 : nullptr,
                        obj_out ? obj.data() + obj.size() - 1 : nullptr,
                        asm_out ? asm_.data() + asm_.size() - 1 : nullptr,
                        timers[0], 0);
        output_timer.stopTimer();

        if (!report_timings) {
            timer_group.clear();
        } else {
            timer_group.print(dbgs(), true);
            for (auto &t : timers) {
                t.print(dbgs(), true);
            }
        }
        return;
    }

    partition_timer.startTimer();
    uint64_t counter = 0;
    // Partitioning requires all globals to have names.
    // We use a prefix to avoid name conflicts with user code.
    for (auto &G : M.global_values()) {
        if (!G.isDeclaration() && !G.hasName()) {
            G.setName("jl_ext_" + Twine(counter++));
        }
    }
    auto partitions = partitionModule(M, threads);
    partition_timer.stopTimer();

    serialize_timer.startTimer();
    auto serialized = serializeModule(M);
    serialize_timer.stopTimer();

    output_timer.startTimer();

    auto outstart = outputs.data() + outputs.size() - outcount * threads;
    auto unoptstart = unopt_out ? unopt.data() + unopt.size() - threads : nullptr;
    auto optstart = opt_out ? opt.data() + opt.size() - threads : nullptr;
    auto objstart = obj_out ? obj.data() + obj.size() - threads : nullptr;
    auto asmstart = asm_out ? asm_.data() + asm_.size() - threads : nullptr;

    // Start all of the worker threads
    std::vector<std::thread> workers(threads);
    for (unsigned i = 0; i < threads; i++) {
        workers[i] = std::thread([&, i](){
            LLVMContext ctx;
            // Lazily deserialize the entire module
            timers[i].deserialize.startTimer();
            auto M = cantFail(getLazyBitcodeModule(MemoryBufferRef(StringRef(serialized.data(), serialized.size()), "Optimized"), ctx), "Error loading module");
            timers[i].deserialize.stopTimer();

            timers[i].materialize.startTimer();
            materializePreserved(*M, partitions[i]);
            timers[i].materialize.stopTimer();

            timers[i].construct.startTimer();
            construct_vars(*M, partitions[i]);
            M->setModuleFlag(Module::Error, "julia.mv.suffix", MDString::get(M->getContext(), "_" + std::to_string(i)));
            timers[i].construct.stopTimer();

            timers[i].deletion.startTimer();
            dropUnusedDeclarations(*M);
            timers[i].deletion.stopTimer();

            add_output_impl(*M, TM, outstart + i * outcount, names,
                            unoptstart ? unoptstart + i : nullptr,
                            optstart ? optstart + i : nullptr,
                            objstart ? objstart + i : nullptr,
                            asmstart ? asmstart + i : nullptr,
                            timers[i], i);
        });
    }

    // Wait for all of the worker threads to finish
    for (auto &w : workers)
        w.join();

    output_timer.stopTimer();

    if (!report_timings) {
        timer_group.clear();
    } else {
        timer_group.print(dbgs(), true);
        for (auto &t : timers) {
            t.print(dbgs(), true);
        }
        dbgs() << "Partition weights: [";
        bool comma = false;
        for (auto &p : partitions) {
            if (comma)
                dbgs() << ", ";
            else
                comma = true;
            dbgs() << p.weight;
        }
        dbgs() << "]\n";
    }
}

static unsigned compute_image_thread_count(const ModuleInfo &info) {
    // 32-bit systems are very memory-constrained
#ifdef _P32
    LLVM_DEBUG(dbgs() << "32-bit systems are restricted to a single thread\n");
    return 1;
#endif
    // This is not overridable because empty modules do occasionally appear, but they'll be very small and thus exit early to
    // known easy behavior. Plus they really don't warrant multiple threads
    if (info.weight < 1000) {
        LLVM_DEBUG(dbgs() << "Small module, using a single thread\n");
        return 1;
    }

    unsigned threads = std::max(jl_cpu_threads() / 2, 1);

    auto max_threads = info.globals / 100;
    if (max_threads < threads) {
        LLVM_DEBUG(dbgs() << "Low global count limiting threads to " << max_threads << " (" << info.globals << "globals)\n");
        threads = max_threads;
    }

    // environment variable override
    const char *env_threads = getenv("JULIA_IMAGE_THREADS");
    bool env_threads_set = false;
    if (env_threads) {
        char *endptr;
        unsigned long requested = strtoul(env_threads, &endptr, 10);
        if (*endptr || !requested) {
            jl_safe_printf("WARNING: invalid value '%s' for JULIA_IMAGE_THREADS\n", env_threads);
        } else {
            LLVM_DEBUG(dbgs() << "Overriding threads to " << requested << " due to JULIA_IMAGE_THREADS\n");
            threads = requested;
            env_threads_set = true;
        }
    }

    // more defaults
    if (!env_threads_set && threads > 1) {
        if (auto fallbackenv = getenv("JULIA_CPU_THREADS")) {
            char *endptr;
            unsigned long requested = strtoul(fallbackenv, &endptr, 10);
            if (*endptr || !requested) {
                jl_safe_printf("WARNING: invalid value '%s' for JULIA_CPU_THREADS\n", fallbackenv);
            } else if (requested < threads) {
                LLVM_DEBUG(dbgs() << "Overriding threads to " << requested << " due to JULIA_CPU_THREADS\n");
                threads = requested;
            }
        }
    }

    threads = std::max(threads, 1u);

    return threads;
}

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
extern "C" JL_DLLEXPORT
void jl_dump_native_impl(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname,
        const char *asm_fname,
        const char *sysimg_data, size_t sysimg_len, ios_t *s)
{
    JL_TIMING(NATIVE_DUMP);
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (!bc_fname && !unopt_bc_fname && !obj_fname && !asm_fname) {
        LLVM_DEBUG(dbgs() << "No output requested, skipping native code dump?\n");
        delete data;
        return;
    }
    auto TSCtx = data->M.getContext();
    auto lock = TSCtx.getLock();
    LLVMContext &Context = *TSCtx.getContext();
    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_ExecutionEngine->getTargetTriple());
    // make sure to emit the native object format, even if FORCE_ELF was set in codegen
#if defined(_OS_WINDOWS_)
    TheTriple.setObjectFormat(Triple::COFF);
#elif defined(_OS_DARWIN_)
    TheTriple.setObjectFormat(Triple::MachO);
    TheTriple.setOS(llvm::Triple::MacOSX);
#endif
    std::unique_ptr<TargetMachine> SourceTM(
        jl_ExecutionEngine->getTarget().createTargetMachine(
            TheTriple.getTriple(),
            jl_ExecutionEngine->getTargetCPU(),
            jl_ExecutionEngine->getTargetFeatureString(),
            jl_ExecutionEngine->getTargetOptions(),
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
            Reloc::PIC_,
#else
            Optional<Reloc::Model>(),
#endif
#if defined(_CPU_PPC_) || defined(_CPU_PPC64_)
            // On PPC the small model is limited to 16bit offsets
            CodeModel::Medium,
#else
            // Use small model so that we can use signed 32bits offset in the function and GV tables
            CodeModel::Small,
#endif
            CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
            ));


    std::vector<NewArchiveMember> bc_Archive;
    std::vector<NewArchiveMember> obj_Archive;
    std::vector<NewArchiveMember> asm_Archive;
    std::vector<NewArchiveMember> unopt_bc_Archive;
    std::vector<std::string> outputs;

    // Reset the target triple to make sure it matches the new target machine
    auto dataM = data->M.getModuleUnlocked();
    dataM->setTargetTriple(SourceTM->getTargetTriple().str());
    dataM->setDataLayout(jl_create_datalayout(*SourceTM));

    Type *T_size;
    if (sizeof(size_t) == 8)
        T_size = Type::getInt64Ty(Context);
    else
        T_size = Type::getInt32Ty(Context);
    Type *T_psize = T_size->getPointerTo();

    bool imaging_mode = imaging_default() || jl_options.outputo;

    unsigned threads = 1;
    unsigned nfvars = 0;
    unsigned ngvars = 0;

    ModuleInfo module_info = compute_module_info(*dataM);
    LLVM_DEBUG(dbgs()
        << "Dumping module with stats:\n"
        << "    globals: " << module_info.globals << "\n"
        << "    functions: " << module_info.funcs << "\n"
        << "    basic blocks: " << module_info.bbs << "\n"
        << "    instructions: " << module_info.insts << "\n"
        << "    clones: " << module_info.clones << "\n"
        << "    weight: " << module_info.weight << "\n"
    );

    // add metadata information
    if (imaging_mode) {
        multiversioning_preannotate(*dataM);
        {
            DenseSet<GlobalValue *> fvars(data->jl_sysimg_fvars.begin(), data->jl_sysimg_fvars.end());
            for (auto &F : *dataM) {
                if (F.hasFnAttribute("julia.mv.reloc") || F.hasFnAttribute("julia.mv.fvar")) {
                    if (fvars.insert(&F).second) {
                        data->jl_sysimg_fvars.push_back(&F);
                    }
                }
            }
        }
        threads = compute_image_thread_count(module_info);
        LLVM_DEBUG(dbgs() << "Using " << threads << " to emit aot image\n");
        nfvars = data->jl_sysimg_fvars.size();
        ngvars = data->jl_sysimg_gvars.size();
        emit_offset_table(*dataM, data->jl_sysimg_gvars, "jl_gvars", T_psize);
        emit_offset_table(*dataM, data->jl_sysimg_fvars, "jl_fvars", T_psize);
        std::vector<uint32_t> idxs;
        idxs.resize(data->jl_sysimg_gvars.size());
        std::iota(idxs.begin(), idxs.end(), 0);
        auto gidxs = ConstantDataArray::get(Context, idxs);
        auto gidxs_var = new GlobalVariable(*dataM, gidxs->getType(), true,
                                            GlobalVariable::ExternalLinkage,
                                            gidxs, "jl_gvar_idxs");
        gidxs_var->setVisibility(GlobalValue::HiddenVisibility);
        idxs.clear();
        idxs.resize(data->jl_sysimg_fvars.size());
        std::iota(idxs.begin(), idxs.end(), 0);
        auto fidxs = ConstantDataArray::get(Context, idxs);
        auto fidxs_var = new GlobalVariable(*dataM, fidxs->getType(), true,
                                            GlobalVariable::ExternalLinkage,
                                            fidxs, "jl_fvar_idxs");
        fidxs_var->setVisibility(GlobalValue::HiddenVisibility);
        dataM->addModuleFlag(Module::Error, "julia.mv.suffix", MDString::get(Context, "_0"));

        // reflect the address of the jl_RTLD_DEFAULT_handle variable
        // back to the caller, so that we can check for consistency issues
        GlobalValue *jlRTLD_DEFAULT_var = jl_emit_RTLD_DEFAULT_var(dataM);
        addComdat(new GlobalVariable(*dataM,
                                     jlRTLD_DEFAULT_var->getType(),
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     jlRTLD_DEFAULT_var,
                                     "jl_RTLD_DEFAULT_handle_pointer"), TheTriple);
    }

    auto compile = [&](Module &M, ArrayRef<StringRef> names, unsigned threads) { add_output(
            M, *SourceTM, outputs, names,
            unopt_bc_Archive, bc_Archive, obj_Archive, asm_Archive,
            !!unopt_bc_fname, !!bc_fname, !!obj_fname, !!asm_fname,
            threads, module_info
    ); };

    std::array<StringRef, 4> text_names = {
        "text_unopt.bc",
        "text_opt.bc",
        "text.o",
        "text.s"
    };

    compile(*dataM, text_names, threads);

    auto sysimageM = std::make_unique<Module>("sysimage", Context);
    sysimageM->setTargetTriple(dataM->getTargetTriple());
    sysimageM->setDataLayout(dataM->getDataLayout());
#if JL_LLVM_VERSION >= 130000
    sysimageM->setStackProtectorGuard(dataM->getStackProtectorGuard());
    sysimageM->setOverrideStackAlignment(dataM->getOverrideStackAlignment());
#endif

    if (TheTriple.isOSWindows()) {
        // Windows expect that the function `_DllMainStartup` is present in an dll.
        // Normal compilers use something like Zig's crtdll.c instead we provide a
        // a stub implementation.
        auto T_pvoid = Type::getInt8Ty(Context)->getPointerTo();
        auto T_int32 = Type::getInt32Ty(Context);
        auto FT = FunctionType::get(T_int32, {T_pvoid, T_int32, T_pvoid}, false);
        auto F = Function::Create(FT, Function::ExternalLinkage, "_DllMainCRTStartup", *sysimageM);
        F->setCallingConv(CallingConv::X86_StdCall);

        llvm::IRBuilder<> builder(BasicBlock::Create(Context, "top", F));
        builder.CreateRet(ConstantInt::get(T_int32, 1));
    }
    bool has_veccall = dataM->getModuleFlag("julia.mv.veccall");
    data->M = orc::ThreadSafeModule(); // free memory for data->M

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(Context,
            ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
        auto sysdata = new GlobalVariable(*sysimageM, data->getType(), false,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data");
        sysdata->setAlignment(Align(64));
        addComdat(sysdata, TheTriple);
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*sysimageM, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"), TheTriple);
    }
    if (imaging_mode) {
        auto specs = jl_get_llvm_clone_targets();
        const uint32_t base_flags = has_veccall ? JL_TARGET_VEC_CALL : 0;
        std::vector<uint8_t> data;
        auto push_i32 = [&] (uint32_t v) {
            uint8_t buff[4];
            memcpy(buff, &v, 4);
            data.insert(data.end(), buff, buff + 4);
        };
        push_i32(specs.size());
        for (uint32_t i = 0; i < specs.size(); i++) {
            push_i32(base_flags | (specs[i].flags & JL_TARGET_UNKNOWN_NAME));
            auto &specdata = specs[i].data;
            data.insert(data.end(), specdata.begin(), specdata.end());
        }
        auto value = ConstantDataArray::get(Context, data);
        auto target_ids = new GlobalVariable(*sysimageM, value->getType(), true,
                                      GlobalVariable::InternalLinkage,
                                      value, "jl_dispatch_target_ids");
        auto shards = emit_shard_table(*sysimageM, T_size, T_psize, threads);
        auto ptls = emit_ptls_table(*sysimageM, T_size, T_psize);
        auto header = emit_image_header(*sysimageM, threads, nfvars, ngvars);
        auto AT = ArrayType::get(T_psize, 4);
        auto pointers = new GlobalVariable(*sysimageM, AT, false,
                                           GlobalVariable::ExternalLinkage,
                                           ConstantArray::get(AT, {
                                                ConstantExpr::getBitCast(header, T_psize),
                                                ConstantExpr::getBitCast(shards, T_psize),
                                                ConstantExpr::getBitCast(ptls, T_psize),
                                                ConstantExpr::getBitCast(target_ids, T_psize)
                                           }),
                                           "jl_image_pointers");
        addComdat(pointers, TheTriple);
        if (s) {
            write_int32(s, data.size());
            ios_write(s, (const char *)data.data(), data.size());
        }
    }

    std::array<StringRef, 4> data_names = {
        "data_unopt.bc",
        "data_opt.bc",
        "data.o",
        "data.s"
    };
    compile(*sysimageM, data_names, 1);

    object::Archive::Kind Kind = getDefaultForHost(TheTriple);
    if (unopt_bc_fname)
        handleAllErrors(writeArchive(unopt_bc_fname, unopt_bc_Archive, true,
                    Kind, true, false), reportWriterError);
    if (bc_fname)
        handleAllErrors(writeArchive(bc_fname, bc_Archive, true,
                    Kind, true, false), reportWriterError);
    if (obj_fname)
        handleAllErrors(writeArchive(obj_fname, obj_Archive, true,
                    Kind, true, false), reportWriterError);
    if (asm_fname)
        handleAllErrors(writeArchive(asm_fname, asm_Archive, true,
                    Kind, true, false), reportWriterError);

    delete data;
}

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis)
{
    PM->add(new TargetLibraryInfoWrapperPass(triple));
    PM->add(createTargetTransformInfoWrapperPass(std::move(analysis)));
}


void addMachinePasses(legacy::PassManagerBase *PM, int optlevel)
{
    // TODO: don't do this on CPUs that natively support Float16
    PM->add(createDemoteFloat16Pass());
    if (optlevel > 1)
        PM->add(createGVNPass());
}

// this defines the set of optimization passes defined for Julia at various optimization levels.
// it assumes that the TLI and TTI wrapper passes have already been added.
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level,
                           bool lower_intrinsics, bool dump_native,
                           bool external_use)
{
    // Note: LLVM 12 disabled the hoisting of common instruction
    //       before loop vectorization (https://reviews.llvm.org/D84108).
    //
    // TODO: CommonInstruction hoisting/sinking enables AllocOpt
    //       to merge allocations and sometimes eliminate them,
    //       since AllocOpt does not handle PhiNodes.
    //       Enable this instruction hoisting because of this and Union benchmarks.
    auto basicSimplifyCFGOptions = SimplifyCFGOptions()
        .convertSwitchRangeToICmp(true)
        .convertSwitchToLookupTable(true)
        .forwardSwitchCondToPhi(true);
    auto aggressiveSimplifyCFGOptions = SimplifyCFGOptions()
        .convertSwitchRangeToICmp(true)
        .convertSwitchToLookupTable(true)
        .forwardSwitchCondToPhi(true)
        //These mess with loop rotation, so only do them after that
        .hoistCommonInsts(true)
        // Causes an SRET assertion error in late-gc-lowering
        // .sinkCommonInsts(true)
        ;
#ifdef JL_DEBUG_BUILD
    PM->add(createGCInvariantVerifierPass(true));
    PM->add(createVerifierPass());
#endif

    PM->add(createConstantMergePass());
    if (opt_level < 2) {
        if (!dump_native) {
            // we won't be multiversioning, so lower CPU feature checks early on
            // so that we can avoid an additional CFG simplification pass at the end.
            PM->add(createCPUFeaturesPass());
            if (opt_level == 1)
                PM->add(createInstSimplifyLegacyPass());
        }
        PM->add(createCFGSimplificationPass(basicSimplifyCFGOptions));
        if (opt_level == 1) {
            PM->add(createSROAPass());
            PM->add(createInstructionCombiningPass());
            PM->add(createEarlyCSEPass());
            // maybe add GVN?
            // also try GVNHoist and GVNSink
        }
        PM->add(createMemCpyOptPass());
        PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline
        PM->add(createLowerSimdLoopPass()); // Annotate loop marked with "loopinfo" as LLVM parallel loop
        if (lower_intrinsics) {
            PM->add(createBarrierNoopPass());
            PM->add(createLowerExcHandlersPass());
            PM->add(createGCInvariantVerifierPass(false));
            PM->add(createRemoveNIPass());
            PM->add(createLateLowerGCFramePass());
            PM->add(createFinalLowerGCPass());
            PM->add(createLowerPTLSPass(dump_native));
        }
        else {
            PM->add(createRemoveNIPass());
        }
        PM->add(createLowerSimdLoopPass()); // Annotate loop marked with "loopinfo" as LLVM parallel loop
        if (dump_native) {
            PM->add(createMultiVersioningPass(external_use));
            PM->add(createCPUFeaturesPass());
            // minimal clean-up to get rid of CPU feature checks
            if (opt_level == 1) {
                PM->add(createInstSimplifyLegacyPass());
                PM->add(createCFGSimplificationPass(basicSimplifyCFGOptions));
            }
        }
#if defined(_COMPILER_ASAN_ENABLED_)
        PM->add(createAddressSanitizerFunctionPass());
#endif
#if defined(_COMPILER_MSAN_ENABLED_)
        PM->add(createMemorySanitizerLegacyPassPass());
#endif
#if defined(_COMPILER_TSAN_ENABLED_)
        PM->add(createThreadSanitizerLegacyPassPass());
#endif
        return;
    }
    PM->add(createPropagateJuliaAddrspaces());
    PM->add(createScopedNoAliasAAWrapperPass());
    PM->add(createTypeBasedAAWrapperPass());
    if (opt_level >= 3) {
        PM->add(createBasicAAWrapperPass());
    }

    PM->add(createCFGSimplificationPass(basicSimplifyCFGOptions));
    PM->add(createDeadCodeEliminationPass());
    PM->add(createSROAPass());

    //PM->add(createMemCpyOptPass());

    PM->add(createAlwaysInlinerLegacyPass()); // Respect always_inline

    // Running `memcpyopt` between this and `sroa` seems to give `sroa` a hard time
    // merging the `alloca` for the unboxed data and the `alloca` created by the `alloc_opt`
    // pass.
    PM->add(createAllocOptPass());
    // consider AggressiveInstCombinePass at optlevel > 2
    PM->add(createInstructionCombiningPass());
    PM->add(createCFGSimplificationPass(basicSimplifyCFGOptions));
    if (dump_native)
        PM->add(createMultiVersioningPass(external_use));
    PM->add(createCPUFeaturesPass());
    PM->add(createSROAPass());
    PM->add(createInstSimplifyLegacyPass());
    PM->add(createJumpThreadingPass());
    PM->add(createCorrelatedValuePropagationPass());

    PM->add(createReassociatePass());

    PM->add(createEarlyCSEPass());

    // Load forwarding above can expose allocations that aren't actually used
    // remove those before optimizing loops.
    PM->add(createAllocOptPass());
    PM->add(createLoopRotatePass());
    // moving IndVarSimplify here prevented removing the loop in perf_sumcartesian(10:-1:1)
#ifdef USE_POLLY
    // LCSSA (which has already run at this point due to the dependencies of the
    // above passes) introduces redundant phis that hinder Polly. Therefore we
    // run InstCombine here to remove them.
    PM->add(createInstructionCombiningPass());
    PM->add(polly::createCodePreparationPass());
    polly::registerPollyPasses(*PM);
    PM->add(polly::createCodegenCleanupPass());
#endif
    // LoopRotate strips metadata from terminator, so run LowerSIMD afterwards
    PM->add(createLowerSimdLoopPass()); // Annotate loop marked with "loopinfo" as LLVM parallel loop
    PM->add(createLICMPass());
    PM->add(createJuliaLICMPass());
#if JL_LLVM_VERSION >= 150000
    PM->add(createSimpleLoopUnswitchLegacyPass());
#else
    PM->add(createLoopUnswitchPass());
#endif
    PM->add(createLICMPass());
    PM->add(createJuliaLICMPass());
    PM->add(createInductiveRangeCheckEliminationPass()); // Must come before indvars
    // Subsequent passes not stripping metadata from terminator
    PM->add(createInstSimplifyLegacyPass());
    PM->add(createLoopIdiomPass());
    PM->add(createIndVarSimplifyPass());
    PM->add(createLoopDeletionPass());
    PM->add(createSimpleLoopUnrollPass());

    // Run our own SROA on heap objects before LLVM's
    PM->add(createAllocOptPass());
    // Re-run SROA after loop-unrolling (useful for small loops that operate,
    // over the structure of an aggregate)
    PM->add(createSROAPass());
    // might not be necessary:
    PM->add(createInstSimplifyLegacyPass());

    PM->add(createGVNPass());
    PM->add(createMemCpyOptPass());
    PM->add(createSCCPPass());

    //These next two passes must come before IRCE to eliminate the bounds check in #43308
    PM->add(createCorrelatedValuePropagationPass());
    PM->add(createDeadCodeEliminationPass());

    PM->add(createInductiveRangeCheckEliminationPass()); // Must come between the two GVN passes

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    // This needs to be InstCombine instead of InstSimplify to allow
    // loops over Union-typed arrays to vectorize.
    PM->add(createInstructionCombiningPass());
    PM->add(createJumpThreadingPass());
    if (opt_level >= 3) {
        PM->add(createGVNPass()); // Must come after JumpThreading and before LoopVectorize
    }
    PM->add(createDeadStoreEliminationPass());
    // see if all of the constant folding has exposed more loops
    // to simplification and deletion
    // this helps significantly with cleaning up iteration
    PM->add(createCFGSimplificationPass(aggressiveSimplifyCFGOptions));

    // More dead allocation (store) deletion before loop optimization
    // consider removing this:
    // Moving this after aggressive CFG simplification helps deallocate when allocations are hoisted
    PM->add(createAllocOptPass());
    PM->add(createLoopDeletionPass());
    PM->add(createInstructionCombiningPass());
    PM->add(createLoopVectorizePass());
    PM->add(createLoopLoadEliminationPass());
    // Cleanup after LV pass
    PM->add(createInstructionCombiningPass());
    PM->add(createCFGSimplificationPass( // Aggressive CFG simplification
        aggressiveSimplifyCFGOptions
    ));
    PM->add(createSLPVectorizerPass());
    // might need this after LLVM 11:
    //PM->add(createVectorCombinePass());

    PM->add(createAggressiveDCEPass());

    if (lower_intrinsics) {
        // LowerPTLS removes an indirect call. As a result, it is likely to trigger
        // LLVM's devirtualization heuristics, which would result in the entire
        // pass pipeline being re-executed. Prevent this by inserting a barrier.
        PM->add(createBarrierNoopPass());
        PM->add(createLowerExcHandlersPass());
        PM->add(createGCInvariantVerifierPass(false));
        // Needed **before** LateLowerGCFrame on LLVM < 12
        // due to bug in `CreateAlignmentAssumption`.
        PM->add(createRemoveNIPass());
        PM->add(createLateLowerGCFramePass());
        PM->add(createFinalLowerGCPass());
        // We need these two passes and the instcombine below
        // after GC lowering to let LLVM do some constant propagation on the tags.
        // and remove some unnecessary write barrier checks.
        PM->add(createGVNPass());
        PM->add(createSCCPPass());
        // Remove dead use of ptls
        PM->add(createDeadCodeEliminationPass());
        PM->add(createLowerPTLSPass(dump_native));
        PM->add(createInstructionCombiningPass());
        // Clean up write barrier and ptls lowering
        PM->add(createCFGSimplificationPass());
    }
    else {
        PM->add(createRemoveNIPass());
    }
    PM->add(createCombineMulAddPass());
    PM->add(createDivRemPairsPass());
#if defined(_COMPILER_ASAN_ENABLED_)
    PM->add(createAddressSanitizerFunctionPass());
#endif
#if defined(_COMPILER_MSAN_ENABLED_)
    PM->add(createMemorySanitizerLegacyPassPass());
#endif
#if defined(_COMPILER_TSAN_ENABLED_)
    PM->add(createThreadSanitizerLegacyPassPass());
#endif
}

// An LLVM module pass that just runs all julia passes in order. Useful for
// debugging
template <int OptLevel, bool dump_native>
class JuliaPipeline : public Pass {
public:
    static char ID;
    // A bit of a hack, but works
    struct TPMAdapter : public PassManagerBase {
        PMTopLevelManager *TPM;
        TPMAdapter(PMTopLevelManager *TPM) : TPM(TPM) {}
        void add(Pass *P) { TPM->schedulePass(P); }
    };
    void preparePassManager(PMStack &Stack) override {
        (void)jl_init_llvm();
        PMTopLevelManager *TPM = Stack.top()->getTopLevelManager();
        TPMAdapter Adapter(TPM);
        addTargetPasses(&Adapter, jl_ExecutionEngine->getTargetTriple(), jl_ExecutionEngine->getTargetIRAnalysis());
        addOptimizationPasses(&Adapter, OptLevel, true, dump_native, true);
        addMachinePasses(&Adapter, OptLevel);
    }
    JuliaPipeline() : Pass(PT_PassManager, ID) {}
    Pass *createPrinterPass(raw_ostream &O, const std::string &Banner) const override {
        return createPrintModulePass(O, Banner);
    }
};
template<> char JuliaPipeline<0,false>::ID = 0;
template<> char JuliaPipeline<2,false>::ID = 0;
template<> char JuliaPipeline<3,false>::ID = 0;
template<> char JuliaPipeline<0,true>::ID = 0;
template<> char JuliaPipeline<2,true>::ID = 0;
template<> char JuliaPipeline<3,true>::ID = 0;
static RegisterPass<JuliaPipeline<0,false>> X("juliaO0", "Runs the entire julia pipeline (at -O0)", false, false);
static RegisterPass<JuliaPipeline<2,false>> Y("julia", "Runs the entire julia pipeline (at -O2)", false, false);
static RegisterPass<JuliaPipeline<3,false>> Z("juliaO3", "Runs the entire julia pipeline (at -O3)", false, false);

static RegisterPass<JuliaPipeline<0,true>> XS("juliaO0-sysimg", "Runs the entire julia pipeline (at -O0/sysimg mode)", false, false);
static RegisterPass<JuliaPipeline<2,true>> YS("julia-sysimg", "Runs the entire julia pipeline (at -O2/sysimg mode)", false, false);
static RegisterPass<JuliaPipeline<3,true>> ZS("juliaO3-sysimg", "Runs the entire julia pipeline (at -O3/sysimg mode)", false, false);

extern "C" JL_DLLEXPORT
void jl_add_optimization_passes_impl(LLVMPassManagerRef PM, int opt_level, int lower_intrinsics) {
    addOptimizationPasses(unwrap(PM), opt_level, lower_intrinsics);
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir, jl_dump_function_asm, jl_dump_method_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT
void jl_get_llvmf_defn_impl(jl_llvmf_dump_t* dump, jl_method_instance_t *mi, size_t world, char getwrapper, char optimize, const jl_cgparams_t params)
{
    if (jl_is_method(mi->def.method) && mi->def.method->source == NULL &&
            mi->def.method->generator == NULL) {
        // not a generic function
        dump->F = NULL;
        return;
    }

    // get the source code for this function
    jl_value_t *jlrettype = (jl_value_t*)jl_any_type;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH2(&src, &jlrettype);
    if (jl_is_method(mi->def.method) && mi->def.method->source != NULL && jl_ir_flag_inferred((jl_array_t*)mi->def.method->source)) {
        src = (jl_code_info_t*)mi->def.method->source;
        if (src && !jl_is_code_info(src))
            src = jl_uncompress_ir(mi->def.method, NULL, (jl_array_t*)src);
    } else {
        jl_value_t *ci = jl_rettype_inferred(mi, world, world);
        if (ci != jl_nothing) {
            jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
            src = (jl_code_info_t*)jl_atomic_load_relaxed(&codeinst->inferred);
            if ((jl_value_t*)src != jl_nothing && !jl_is_code_info(src) && jl_is_method(mi->def.method))
                src = jl_uncompress_ir(mi->def.method, codeinst, (jl_array_t*)src);
            jlrettype = codeinst->rettype;
        }
        if (!src || (jl_value_t*)src == jl_nothing) {
            src = jl_type_infer(mi, world, 0);
            if (src)
                jlrettype = src->rettype;
            else if (jl_is_method(mi->def.method)) {
                src = mi->def.method->generator ? jl_code_for_staged(mi) : (jl_code_info_t*)mi->def.method->source;
                if (src && !jl_is_code_info(src) && jl_is_method(mi->def.method))
                    src = jl_uncompress_ir(mi->def.method, NULL, (jl_array_t*)src);
            }
            // TODO: use mi->uninferred
        }
    }

    // emit this function into a new llvm module
    if (src && jl_is_code_info(src)) {
        auto ctx = jl_ExecutionEngine->getContext();
        orc::ThreadSafeModule m = jl_create_ts_module(name_from_method_instance(mi), *ctx, imaging_default());
        uint64_t compiler_start_time = 0;
        uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
        if (measure_compile_time_enabled)
            compiler_start_time = jl_hrtime();
        JL_LOCK(&jl_codegen_lock);
        jl_codegen_params_t output(*ctx);
        output.world = world;
        output.params = &params;
        auto decls = jl_emit_code(m, mi, src, jlrettype, output);
        JL_UNLOCK(&jl_codegen_lock); // Might GC

        Function *F = NULL;
        if (m) {
            // if compilation succeeded, prepare to return the result
            // For imaging mode, global constants are currently private without initializer
            // which isn't legal. Convert them to extern linkage so that the code can compile
            // and will better match what's actually in sysimg.
            for (auto &global : output.globals)
                global.second->setLinkage(GlobalValue::ExternalLinkage);
            assert(!verifyModule(*m.getModuleUnlocked(), &errs()));
            if (optimize) {
#ifndef JL_USE_NEW_PM
                legacy::PassManager PM;
                addTargetPasses(&PM, jl_ExecutionEngine->getTargetTriple(), jl_ExecutionEngine->getTargetIRAnalysis());
                addOptimizationPasses(&PM, jl_options.opt_level);
                addMachinePasses(&PM, jl_options.opt_level);
#else
                NewPM PM{jl_ExecutionEngine->cloneTargetMachine(), getOptLevel(jl_options.opt_level)};
#endif
                //Safe b/c context lock is held by output
                PM.run(*m.getModuleUnlocked());
                assert(!verifyModule(*m.getModuleUnlocked(), &errs()));
            }
            const std::string *fname;
            if (decls.functionObject == "jl_fptr_args" || decls.functionObject == "jl_fptr_sparam")
                getwrapper = false;
            if (!getwrapper)
                fname = &decls.specFunctionObject;
            else
                fname = &decls.functionObject;
            F = cast<Function>(m.getModuleUnlocked()->getNamedValue(*fname));
        }
        JL_GC_POP();
        if (measure_compile_time_enabled) {
            auto end = jl_hrtime();
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
        }
        if (F) {
            dump->TSM = wrap(new orc::ThreadSafeModule(std::move(m)));
            dump->F = wrap(F);
            return;
        }
    }

    const char *mname = name_from_method_instance(mi);
    jl_errorf("unable to compile source for function %s", mname);
}
