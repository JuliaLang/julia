// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"

// target support
#include <llvm/ADT/Triple.h>
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
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Vectorize.h>
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/Utils/SimplifyCFGOptions.h>
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
#include "llvm/Object/ArchiveWriter.h"
#include <llvm/IR/IRPrintingPasses.h>

#include <llvm/IR/LegacyPassManagers.h>
#include <llvm/Transforms/Utils/Cloning.h>


using namespace llvm;

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "julia_assert.h"

template<class T> // for GlobalObject's
static T *addComdat(T *G)
{
#if defined(_OS_WINDOWS_)
    if (!G->isDeclaration()) {
        // add __declspec(dllexport) to everything marked for export
        if (G->getLinkage() == GlobalValue::ExternalLinkage)
            G->setDLLStorageClass(GlobalValue::DLLExportStorageClass);
        else
            G->setDLLStorageClass(GlobalValue::DefaultStorageClass);
    }
#endif
    return G;
}


typedef struct {
    orc::ThreadSafeModule M;
    std::vector<GlobalValue*> jl_sysimg_fvars;
    std::vector<GlobalValue*> jl_sysimg_gvars;
    std::map<jl_code_instance_t*, std::tuple<uint32_t, uint32_t>> jl_fvar_map;
    std::map<void*, int32_t> jl_value_to_llvm; // uses 1-based indexing
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
int32_t jl_get_llvm_gv_impl(void *native_code, jl_value_t *p)
{
    // map a jl_value_t memory location to a GlobalVariable
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data) {
        auto it = data->jl_value_to_llvm.find(p);
        if (it != data->jl_value_to_llvm.end()) {
            return it->second;
        }
    }
    return 0;
}

extern "C" JL_DLLEXPORT
LLVMOrcThreadSafeModuleRef jl_get_llvm_module_impl(void *native_code)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data)
        return reinterpret_cast<LLVMOrcThreadSafeModuleRef>(&data->M);
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
    assert(!vars.empty());
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
    jl_value_t *ci = cgparams.lookup(mi, world, world);
    JL_GC_PROMISE_ROOTED(ci);
    jl_code_instance_t *codeinst = NULL;
    if (ci != jl_nothing) {
        codeinst = (jl_code_instance_t*)ci;
        *src_out = (jl_code_info_t*)codeinst->inferred;
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
                if ((*src_out)->inferred && !codeinst->inferred)
                    codeinst->inferred = jl_nothing;
            }
        }
    }
    *ci_out = codeinst;
}

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup, and can
// also be used be extern consumers like GPUCompiler.jl to obtain a module containing
// all reachable & inferrrable functions. The `policy` flag switches between the default
// mode `0`, the extern mode `1`, and imaging mode `2`.
extern "C" JL_DLLEXPORT
void *jl_create_native_impl(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmmod, const jl_cgparams_t *cgparams, int _policy)
{
    if (cgparams == NULL)
        cgparams = &jl_default_cgparams;
    jl_native_code_desc_t *data = new jl_native_code_desc_t;
    CompilationPolicy policy = (CompilationPolicy) _policy;
    bool imaging = imaging_default() || policy == CompilationPolicy::ImagingMode;
    orc::ThreadSafeModule backing;
    if (!llvmmod) {
        backing = jl_create_llvm_module("text", jl_ExecutionEngine->getContext(), imaging);
    }
    orc::ThreadSafeModule &clone = llvmmod ? *reinterpret_cast<orc::ThreadSafeModule*>(llvmmod) : backing;
    auto ctxt = clone.getContext();
    jl_workqueue_t emitted;
    jl_method_instance_t *mi = NULL;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    JL_LOCK(&jl_codegen_lock);
    jl_codegen_params_t params(ctxt);
    params.params = cgparams;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();

    params.imaging = imaging;

    // compile all methods for the current world and type-inference world
    size_t compile_for[] = { jl_typeinf_world, jl_atomic_load_acquire(&jl_world_counter) };
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
                    jl_compile_extern_c(reinterpret_cast<LLVMOrcThreadSafeModuleRef>(&clone), &params, NULL, jl_svecref(item, 0), jl_svecref(item, 1));
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
                    orc::ThreadSafeModule result_m = jl_create_llvm_module(name_from_method_instance(codeinst->def),
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
    JL_GC_POP();

    // process the globals array, before jl_merge_module destroys them
    std::vector<std::string> gvars;

    for (auto &global : params.globals) {
        gvars.push_back(std::string(global.second->getName()));
        data->jl_value_to_llvm[global.first] = gvars.size();
    }

    // clones the contents of the module `m` to the shadow_output collector
    // while examining and recording what kind of function pointer we have
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
        jl_merge_module(clone, std::move(params._shared_module));
    }

    // now get references to the globals in the merged module
    // and set them to be internalized and initialized at startup
    for (auto &global : gvars) {
        //Safe b/c context is locked by params
        GlobalVariable *G = cast<GlobalVariable>(clone.getModuleUnlocked()->getNamedValue(global));
        G->setInitializer(ConstantPointerNull::get(cast<PointerType>(G->getValueType())));
        G->setLinkage(GlobalVariable::InternalLinkage);
        data->jl_sysimg_gvars.push_back(G);
    }

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
                G.setLinkage(Function::InternalLinkage);
                makeSafeName(G);
                addComdat(&G);
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
                // Add unwind exception personalities to functions to handle async exceptions
                if (Function *F = dyn_cast<Function>(&G))
                    F->setPersonalityFn(juliapersonality_func);
#endif
            }
        }
    }

    data->M = std::move(clone);
    if (measure_compile_time_enabled)
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
    JL_UNLOCK(&jl_codegen_lock); // Might GC
    return (void*)data;
}


static void emit_result(std::vector<NewArchiveMember> &Archive, SmallVectorImpl<char> &OS,
        StringRef Name, std::vector<std::string> &outputs)
{
    outputs.push_back({ OS.data(), OS.size() });
    Archive.push_back(NewArchiveMember(MemoryBufferRef(outputs.back(), Name)));
    OS.clear();
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


// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
extern "C" JL_DLLEXPORT
void jl_dump_native_impl(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname,
        const char *asm_fname,
        const char *sysimg_data, size_t sysimg_len)
{
    JL_TIMING(NATIVE_DUMP);
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
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
    std::unique_ptr<TargetMachine> TM(
        jl_ExecutionEngine->getTargetMachine().getTarget().createTargetMachine(
            TheTriple.getTriple(),
            jl_ExecutionEngine->getTargetMachine().getTargetCPU(),
            jl_ExecutionEngine->getTargetMachine().getTargetFeatureString(),
            jl_ExecutionEngine->getTargetMachine().Options,
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

    legacy::PassManager PM;
    addTargetPasses(&PM, TM.get());

    // set up optimization passes
    SmallVector<char, 0> bc_Buffer;
    SmallVector<char, 0> obj_Buffer;
    SmallVector<char, 0> asm_Buffer;
    SmallVector<char, 0> unopt_bc_Buffer;
    raw_svector_ostream bc_OS(bc_Buffer);
    raw_svector_ostream obj_OS(obj_Buffer);
    raw_svector_ostream asm_OS(asm_Buffer);
    raw_svector_ostream unopt_bc_OS(unopt_bc_Buffer);
    std::vector<NewArchiveMember> bc_Archive;
    std::vector<NewArchiveMember> obj_Archive;
    std::vector<NewArchiveMember> asm_Archive;
    std::vector<NewArchiveMember> unopt_bc_Archive;
    std::vector<std::string> outputs;

    if (unopt_bc_fname)
        PM.add(createBitcodeWriterPass(unopt_bc_OS));
    if (bc_fname || obj_fname || asm_fname) {
        addOptimizationPasses(&PM, jl_options.opt_level, true, true);
        addMachinePasses(&PM, TM.get(), jl_options.opt_level);
    }
    if (bc_fname)
        PM.add(createBitcodeWriterPass(bc_OS));
    if (obj_fname)
        if (TM->addPassesToEmitFile(PM, obj_OS, nullptr, CGFT_ObjectFile, false))
            jl_safe_printf("ERROR: target does not support generation of object files\n");
    if (asm_fname)
        if (TM->addPassesToEmitFile(PM, asm_OS, nullptr, CGFT_AssemblyFile, false))
            jl_safe_printf("ERROR: target does not support generation of object files\n");

    // Reset the target triple to make sure it matches the new target machine
    auto dataM = data->M.getModuleUnlocked();
    dataM->setTargetTriple(TM->getTargetTriple().str());
    dataM->setDataLayout(jl_create_datalayout(*TM));
    Type *T_size;
    if (sizeof(size_t) == 8)
        T_size = Type::getInt64Ty(Context);
    else
        T_size = Type::getInt32Ty(Context);
    Type *T_psize = T_size->getPointerTo();

    // add metadata information
    if (imaging_default()) {
        emit_offset_table(*dataM, data->jl_sysimg_gvars, "jl_sysimg_gvars", T_psize);
        emit_offset_table(*dataM, data->jl_sysimg_fvars, "jl_sysimg_fvars", T_psize);

        // reflect the address of the jl_RTLD_DEFAULT_handle variable
        // back to the caller, so that we can check for consistency issues
        GlobalValue *jlRTLD_DEFAULT_var = jl_emit_RTLD_DEFAULT_var(dataM);
        addComdat(new GlobalVariable(*dataM,
                                     jlRTLD_DEFAULT_var->getType(),
                                     true,
                                     GlobalVariable::ExternalLinkage,
                                     jlRTLD_DEFAULT_var,
                                     "jl_RTLD_DEFAULT_handle_pointer"));
    }

    // do the actual work
    auto add_output = [&] (Module &M, StringRef unopt_bc_Name, StringRef bc_Name, StringRef obj_Name, StringRef asm_Name) {
        PM.run(M);
        if (unopt_bc_fname)
            emit_result(unopt_bc_Archive, unopt_bc_Buffer, unopt_bc_Name, outputs);
        if (bc_fname)
            emit_result(bc_Archive, bc_Buffer, bc_Name, outputs);
        if (obj_fname)
            emit_result(obj_Archive, obj_Buffer, obj_Name, outputs);
        if (asm_fname)
            emit_result(asm_Archive, asm_Buffer, asm_Name, outputs);
    };

    add_output(*dataM, "unopt.bc", "text.bc", "text.o", "text.s");

    orc::ThreadSafeModule sysimage(std::make_unique<Module>("sysimage", Context), TSCtx);
    auto sysimageM = sysimage.getModuleUnlocked();
    sysimageM->setTargetTriple(dataM->getTargetTriple());
    sysimageM->setDataLayout(dataM->getDataLayout());
#if JL_LLVM_VERSION >= 130000
    sysimageM->setStackProtectorGuard(dataM->getStackProtectorGuard());
    sysimageM->setOverrideStackAlignment(dataM->getOverrideStackAlignment());
#endif
    data->M = orc::ThreadSafeModule(); // free memory for data->M

    if (sysimg_data) {
        Constant *data = ConstantDataArray::get(Context,
            ArrayRef<uint8_t>((const unsigned char*)sysimg_data, sysimg_len));
        addComdat(new GlobalVariable(*sysimageM, data->getType(), false,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data"))->setAlignment(Align(64));
        Constant *len = ConstantInt::get(T_size, sysimg_len);
        addComdat(new GlobalVariable(*sysimageM, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"));
    }
    add_output(*sysimageM, "data.bc", "data.bc", "data.o", "data.s");

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

void addTargetPasses(legacy::PassManagerBase *PM, TargetMachine *TM)
{
    PM->add(new TargetLibraryInfoWrapperPass(Triple(TM->getTargetTriple())));
    PM->add(createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
}


void addMachinePasses(legacy::PassManagerBase *PM, TargetMachine *TM, int optlevel)
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
    auto simplifyCFGOptions = SimplifyCFGOptions().hoistCommonInsts(true);
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
        PM->add(createCFGSimplificationPass(simplifyCFGOptions));
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
                PM->add(createCFGSimplificationPass(simplifyCFGOptions));
            }
        }
#if defined(_COMPILER_ASAN_ENABLED_)
        PM->add(createAddressSanitizerFunctionPass());
#endif
#if defined(_COMPILER_MSAN_ENABLED_)
        PM->add(createMemorySanitizerPass(true));
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

    PM->add(createCFGSimplificationPass(simplifyCFGOptions));
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
    PM->add(createCFGSimplificationPass(simplifyCFGOptions));
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
    PM->add(createLoopUnswitchPass());
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

    // More dead allocation (store) deletion before loop optimization
    // consider removing this:
    PM->add(createAllocOptPass());
    // see if all of the constant folding has exposed more loops
    // to simplification and deletion
    // this helps significantly with cleaning up iteration
    PM->add(createCFGSimplificationPass()); // See note above, don't hoist instructions before LV
    PM->add(createLoopDeletionPass());
    PM->add(createInstructionCombiningPass());
    PM->add(createLoopVectorizePass());
    PM->add(createLoopLoadEliminationPass());
    // Cleanup after LV pass
    PM->add(createInstructionCombiningPass());
    PM->add(createCFGSimplificationPass( // Aggressive CFG simplification
        SimplifyCFGOptions()
            .forwardSwitchCondToPhi(true)
            .convertSwitchToLookupTable(true)
            .needCanonicalLoops(false)
            .hoistCommonInsts(true)
            // .sinkCommonInsts(true) // FIXME: Causes assertion in llvm-late-lowering
    ));
    PM->add(createSLPVectorizerPass());
    // might need this after LLVM 11:
    //PM->add(createVectorCombinePass());

    PM->add(createAggressiveDCEPass());

    if (lower_intrinsics) {
        // LowerPTLS removes an indirect call. As a result, it is likely to trigger
        // LLVM's devirtualization heuristics, which would result in the entire
        // pass pipeline being re-exectuted. Prevent this by inserting a barrier.
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
    PM->add(createMemorySanitizerPass(true));
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
        addTargetPasses(&Adapter, &jl_ExecutionEngine->getTargetMachine());
        addOptimizationPasses(&Adapter, OptLevel, true, dump_native, true);
        addMachinePasses(&Adapter, &jl_ExecutionEngine->getTargetMachine(), OptLevel);
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

// new pass manager plugin

// NOTE: Instead of exporting all the constructors in passes.h we could
// forward the callbacks to the respective passes. LLVM seems to prefer this,
// and when we add the full pass builder having them directly will be helpful.
static void registerCallbacks(PassBuilder &PB) {
    PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
            if (Name == "DemoteFloat16") {
                PM.addPass(DemoteFloat16());
                return true;
            }
            if (Name == "CombineMulAdd") {
              PM.addPass(CombineMulAdd());
              return true;
            }
            if (Name == "LateLowerGCFrame") {
                PM.addPass(LateLowerGC());
                return true;
            }
            if (Name == "AllocOpt") {
                PM.addPass(AllocOptPass());
                return true;
            }
            if (Name == "PropagateJuliaAddrspaces") {
                PM.addPass(PropagateJuliaAddrspacesPass());
                return true;
            }
            if (Name == "LowerExcHandlers") {
                PM.addPass(LowerExcHandlers());
                return true;
            }
            if (Name == "GCInvariantVerifier") {
                // TODO: Parse option and allow users to set `Strong`
                PM.addPass(GCInvariantVerifierPass());
                return true;
            }
            return false;
        });

    PB.registerPipelineParsingCallback(
        [](StringRef Name, ModulePassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
            if (Name == "CPUFeatures") {
              PM.addPass(CPUFeatures());
              return true;
            }
            if (Name == "RemoveNI") {
              PM.addPass(RemoveNI());
              return true;
            }
            if (Name == "LowerSIMDLoop") {
              PM.addPass(LowerSIMDLoop());
              return true;
            }
            if (Name == "FinalLowerGC") {
                PM.addPass(FinalLowerGCPass());
                return true;
            }
            if (Name == "RemoveJuliaAddrspaces") {
                PM.addPass(RemoveJuliaAddrspacesPass());
                return true;
            }
            if (Name == "MultiVersioning") {
                PM.addPass(MultiVersioning());
                return true;
            }
            if (Name == "LowerPTLS") {
                PM.addPass(LowerPTLSPass());
                return true;
            }
            return false;
        });

    PB.registerPipelineParsingCallback(
        [](StringRef Name, LoopPassManager &PM,
           ArrayRef<PassBuilder::PipelineElement> InnerPipeline) {
            if (Name == "JuliaLICM") {
                PM.addPass(JuliaLICMPass());
                return true;
            }
            return false;
        });
}

extern "C" JL_DLLEXPORT ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
      return {LLVM_PLUGIN_API_VERSION, "Julia", "1", registerCallbacks};
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir, jl_dump_function_asm, jl_dump_method_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT
void *jl_get_llvmf_defn_impl(jl_method_instance_t *mi, size_t world, char getwrapper, char optimize, const jl_cgparams_t params)
{
    if (jl_is_method(mi->def.method) && mi->def.method->source == NULL &&
            mi->def.method->generator == NULL) {
        // not a generic function
        return NULL;
    }

    static legacy::PassManager *PM;
    if (!PM) {
        PM = new legacy::PassManager();
        addTargetPasses(PM, &jl_ExecutionEngine->getTargetMachine());
        addOptimizationPasses(PM, jl_options.opt_level);
        addMachinePasses(PM, &jl_ExecutionEngine->getTargetMachine(), jl_options.opt_level);
    }

    // get the source code for this function
    jl_value_t *jlrettype = (jl_value_t*)jl_any_type;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH2(&src, &jlrettype);
    jl_value_t *ci = jl_rettype_inferred(mi, world, world);
    if (ci != jl_nothing) {
        jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
        src = (jl_code_info_t*)codeinst->inferred;
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

    // emit this function into a new llvm module
    if (src && jl_is_code_info(src)) {
        JL_LOCK(&jl_codegen_lock);
        jl_codegen_params_t output(jl_ExecutionEngine->getContext());
        output.world = world;
        output.params = &params;
        orc::ThreadSafeModule m = jl_create_llvm_module(name_from_method_instance(mi), output.tsctx, output.imaging);
        uint64_t compiler_start_time = 0;
        uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
        if (measure_compile_time_enabled)
            compiler_start_time = jl_hrtime();
        auto decls = jl_emit_code(m, mi, src, jlrettype, output);

        Function *F = NULL;
        if (m) {
            // if compilation succeeded, prepare to return the result
            // For imaging mode, global constants are currently private without initializer
            // which isn't legal. Convert them to extern linkage so that the code can compile
            // and will better match what's actually in sysimg.
            for (auto &global : output.globals)
                global.second->setLinkage(GlobalValue::ExternalLinkage);
            if (optimize)
                //Safe b/c context lock is held by output
                PM->run(*m.getModuleUnlocked());
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
        if (measure_compile_time_enabled)
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, (jl_hrtime() - compiler_start_time));
        JL_UNLOCK(&jl_codegen_lock); // Might GC
        if (F)
            return new jl_llvmf_dump_t{std::move(m), F};
    }

    const char *mname = name_from_method_instance(mi);
    jl_errorf("unable to compile source for function %s", mname);
}
