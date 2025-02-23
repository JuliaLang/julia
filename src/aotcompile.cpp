// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"

// target support
#include <llvm/TargetParser/Triple.h>
#include "llvm/Support/CodeGen.h"
#include <llvm/ADT/Statistic.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetMachine.h>

// analysis passes
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Verifier.h>
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
#include "processor.h"

#define DEBUG_TYPE "julia_aotcompile"

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
    SmallVector<GlobalValue*, 0> jl_sysimg_fvars;
    SmallVector<GlobalValue*, 0> jl_sysimg_gvars;
    std::map<jl_code_instance_t*, std::tuple<uint32_t, uint32_t>> jl_fvar_map;
    SmallVector<void*, 0> jl_value_to_llvm;
    SmallVector<jl_code_instance_t*, 0> jl_external_to_llvm;
} jl_native_code_desc_t;

extern "C" JL_DLLEXPORT_CODEGEN
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

extern "C" JL_DLLEXPORT_CODEGEN void
jl_get_llvm_mis_impl(void *native_code, size_t *num_elements, jl_method_instance_t **data)
{
    jl_native_code_desc_t *desc = (jl_native_code_desc_t *)native_code;
    auto &map = desc->jl_fvar_map;

    if (data == NULL) {
        *num_elements = map.size();
        return;
    }

    assert(*num_elements == map.size());
    size_t i = 0;
    for (auto &ci : map) {
        data[i++] = jl_get_ci_mi(ci.first);
    }
}

extern "C" JL_DLLEXPORT_CODEGEN void jl_get_llvm_gvs_impl(void *native_code,
                                                          size_t *num_elements, void **data)
{
    // map a memory location (jl_value_t or jl_binding_t) to a GlobalVariable
    jl_native_code_desc_t *desc = (jl_native_code_desc_t *)native_code;
    auto &value_map = desc->jl_value_to_llvm;

    if (data == NULL) {
        *num_elements = value_map.size();
        return;
    }

    assert(*num_elements == value_map.size());
    memcpy(data, value_map.data(), *num_elements * sizeof(void *));
}

extern "C" JL_DLLEXPORT_CODEGEN void jl_get_llvm_external_fns_impl(void *native_code,
                                                                   size_t *num_elements,
                                                                   jl_code_instance_t *data)
{
    jl_native_code_desc_t *desc = (jl_native_code_desc_t *)native_code;
    auto &external_map = desc->jl_external_to_llvm;

    if (data == NULL) {
        *num_elements = external_map.size();
        return;
    }

    assert(*num_elements == external_map.size());
    memcpy((void *)data, (const void *)external_map.data(),
           *num_elements * sizeof(jl_code_instance_t *));
}

extern "C" JL_DLLEXPORT_CODEGEN
LLVMOrcThreadSafeModuleRef jl_get_llvm_module_impl(void *native_code)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data)
        return wrap(&data->M);
    else
        return NULL;
}

extern "C" JL_DLLEXPORT_CODEGEN
GlobalValue* jl_get_llvm_function_impl(void *native_code, uint32_t idx)
{
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (data)
        return data->jl_sysimg_fvars[idx];
    else
        return NULL;
}



template<typename T>
static inline SmallVector<T*, 0> consume_gv(Module &M, const char *name, bool allow_bad_fvars)
{
    // Get information about sysimg export functions from the two global variables.
    // Strip them from the Module so that it's easier to handle the uses.
    GlobalVariable *gv = M.getGlobalVariable(name);
    assert(gv && gv->hasInitializer());
    ArrayType *Ty = cast<ArrayType>(gv->getInitializer()->getType());
    unsigned nele = Ty->getArrayNumElements();
    SmallVector<T*, 0> res(nele);
    ConstantArray *ary = nullptr;
    if (gv->getInitializer()->isNullValue()) {
        for (unsigned i = 0; i < nele; ++i)
            res[i] = cast<T>(Constant::getNullValue(Ty->getArrayElementType()));
    }
    else {
        ary = cast<ConstantArray>(gv->getInitializer());
        unsigned i = 0;
        while (i < nele) {
            llvm::Value *val = ary->getOperand(i)->stripPointerCasts();
            if (allow_bad_fvars && (!isa<T>(val) || (isa<Function>(val) && cast<Function>(val)->isDeclaration()))) {
                // Shouldn't happen in regular use, but can happen in bugpoint.
                nele--;
                continue;
            }
            res[i++] = cast<T>(val);
        }
        res.resize(nele);
    }
    assert(gv->use_empty());
    gv->eraseFromParent();
    if (ary && ary->use_empty())
        ary->destroyConstant();
    return res;
}

static Constant *get_ptrdiff32(Type *T_size, Constant *ptr, Constant *base)
{
    if (ptr->getType()->isPointerTy())
        ptr = ConstantExpr::getPtrToInt(ptr, T_size);
    auto ptrdiff = ConstantExpr::getSub(ptr, base);
    return T_size->getPrimitiveSizeInBits() > 32 ? ConstantExpr::getTrunc(ptrdiff, Type::getInt32Ty(ptr->getContext())) : ptrdiff;
}

static Constant *emit_offset_table(Module &M, Type *T_size, ArrayRef<Constant*> vars,
                                   StringRef name, StringRef suffix)
{
    auto T_int32 = Type::getInt32Ty(M.getContext());
    uint32_t nvars = vars.size();
    ArrayType *vars_type = ArrayType::get(T_int32, nvars + 1);
    auto gv = new GlobalVariable(M, vars_type, true,
                                 GlobalVariable::ExternalLinkage,
                                 nullptr,
                                 name + "_offsets" + suffix);
    auto vbase = ConstantExpr::getPtrToInt(gv, T_size);
    SmallVector<Constant*, 0> offsets(nvars + 1);
    offsets[0] = ConstantInt::get(T_int32, nvars);
    for (uint32_t i = 0; i < nvars; i++)
        offsets[i + 1] = get_ptrdiff32(T_size, vars[i], vbase);
    gv->setInitializer(ConstantArray::get(vars_type, offsets));
    gv->setVisibility(GlobalValue::HiddenVisibility);
    gv->setDSOLocal(true);
    return vbase;
}

static void emit_table(Module &mod, ArrayRef<GlobalValue*> vars,
                       StringRef name, Type *T_psize)
{
    // Emit a global variable with all the variable addresses.
    size_t nvars = vars.size();
    SmallVector<Constant*, 0> addrs(nvars);
    for (size_t i = 0; i < nvars; i++) {
        Constant *var = vars[i];
        addrs[i] = ConstantExpr::getBitCast(var, T_psize);
    }
    ArrayType *vars_type = ArrayType::get(T_psize, nvars);
    auto GV = new GlobalVariable(mod, vars_type, true,
                       GlobalVariable::ExternalLinkage,
                       ConstantArray::get(vars_type, addrs),
                       name);
    GV->setVisibility(GlobalValue::HiddenVisibility);
    GV->setDSOLocal(true);
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

namespace { // file-local namespace
class egal_set {
public:
    jl_genericmemory_t *list = (jl_genericmemory_t*)jl_an_empty_memory_any;
    jl_genericmemory_t *keyset = (jl_genericmemory_t*)jl_an_empty_memory_any;
    egal_set(egal_set&) = delete;
    egal_set(egal_set&&) = delete;
    egal_set() = default;
    void insert(jl_value_t *val)
    {
        jl_value_t *rval = jl_idset_get(list, keyset, val);
        if (rval == NULL) {
            ssize_t idx;
            list = jl_idset_put_key(list, val, &idx);
            keyset = jl_idset_put_idx(list, keyset, idx);
        }
    }
    jl_value_t *get(jl_value_t *val)
    {
        return jl_idset_get(list, keyset, val);
    }
};
}
using ::egal_set;
struct jl_compiled_function_t {
   orc::ThreadSafeModule TSM;
   jl_llvm_functions_t decls;
};
typedef DenseMap<jl_code_instance_t*, jl_compiled_function_t> jl_compiled_functions_t;

static void record_method_roots(egal_set &method_roots, jl_method_instance_t *mi)
{
    jl_method_t *m = mi->def.method;
    if (!jl_is_method(m))
        return;
    // the method might have a root for this already; use it if so
    JL_LOCK(&m->writelock);
    if (m->roots) {
        size_t j, len = jl_array_dim0(m->roots);
        for (j = 0; j < len; j++) {
            jl_value_t *v = jl_array_ptr_ref(m->roots, j);
            if (jl_is_globally_rooted(v))
                continue;
            method_roots.insert(v);
        }
    }
    JL_UNLOCK(&m->writelock);
}

static void aot_optimize_roots(jl_codegen_params_t &params, egal_set &method_roots, jl_compiled_functions_t &compiled_functions)
{
    for (size_t i = 0; i < jl_array_dim0(params.temporary_roots); i++) {
        jl_value_t *val = jl_array_ptr_ref(params.temporary_roots, i);
        auto ref = params.global_targets.find((void*)val);
        if (ref == params.global_targets.end())
            continue;
        auto get_global_root = [val, &method_roots]() {
            if (jl_is_globally_rooted(val))
                return val;
            jl_value_t *mval = method_roots.get(val);
            if (mval)
                return mval;
            return jl_as_global_root(val, 1);
        };
        jl_value_t *mval = get_global_root();
        if (mval != val) {
            GlobalVariable *GV = ref->second;
            params.global_targets.erase(ref);
            auto mref = params.global_targets.find((void*)mval);
            if (mref != params.global_targets.end()) {
                // replace ref with mref in all Modules
                std::string OldName(GV->getName());
                StringRef NewName(mref->second->getName());
                for (auto &def : compiled_functions) {
                    orc::ThreadSafeModule &TSM = def.second.TSM;
                    Module &M = *TSM.getModuleUnlocked();
                    if (GlobalValue *GV2 = M.getNamedValue(OldName)) {
                        if (GV2 == GV)
                            GV = nullptr;
                        // either replace or rename the old value to use the other equivalent name
                        if (GlobalValue *GV3 = M.getNamedValue(NewName)) {
                            GV2->replaceAllUsesWith(GV3);
                            GV2->eraseFromParent();
                        }
                        else {
                            GV2->setName(NewName);
                        }
                    }
                }
                assert(GV == nullptr);
            }
            else {
                params.global_targets[(void*)mval] = GV;
            }
        }
    }
}

static void resolve_workqueue(jl_codegen_params_t &params, egal_set &method_roots, jl_compiled_functions_t &compiled_functions)
{
    jl_workqueue_t workqueue;
    std::swap(params.workqueue, workqueue);
    jl_code_instance_t *codeinst = NULL;
    JL_GC_PUSH1(&codeinst);
    assert(!params.cache);
    while (!workqueue.empty()) {
        auto it = workqueue.pop_back_val();
        codeinst = it.first;
        auto &proto = it.second;
        // try to emit code for this item from the workqueue
        StringRef invokeName = "";
        StringRef preal_decl = "";
        bool preal_specsig = false;
        {
            auto it = compiled_functions.find(codeinst);
            if (it != compiled_functions.end()) {
                auto &decls = it->second.decls;
                invokeName = decls.functionObject;
                if (decls.functionObject == "jl_fptr_args") {
                    preal_decl = decls.specFunctionObject;
                }
                else if (decls.functionObject != "jl_fptr_sparam" && decls.functionObject != "jl_f_opaque_closure_call" && decls.functionObject != "jl_fptr_const_return") {
                    preal_decl = decls.specFunctionObject;
                    preal_specsig = true;
                }
                if (proto.private_linkage) {
                    it->second.TSM.withModuleDo([&](Module &M) {
                        auto F = cast<Function>(M.getNamedValue(decls.specFunctionObject));
                        assert(!F->isDeclaration());
                        F->addFnAttr(Attribute::InlineHint);
                    });
                }
            }
            else if (params.params->trim) {
                jl_safe_printf("warning: no code provided for function ");
                jl_(codeinst->def);
                if (params.params->trim)
                    abort();
            }
        }
        // patch up the prototype we emitted earlier
        Module *mod = proto.decl->getParent();
        assert(proto.decl->isDeclaration());
        Function *pinvoke = nullptr;
        if (preal_decl.empty() && jl_atomic_load_relaxed(&codeinst->invoke) == jl_fptr_const_return_addr) {
            std::string gf_thunk_name = emit_abi_constreturn(mod, params, proto.specsig, codeinst);
            preal_specsig = proto.specsig;
            if (invokeName.empty())
                invokeName = "jl_fptr_const_return";
            preal_decl = mod->getNamedValue(gf_thunk_name)->getName();
        }
        if (preal_decl.empty()) {
            if (invokeName.empty() && params.params->trim) {
                jl_safe_printf("warning: bailed out to invoke when compiling: ");
                jl_(codeinst->def);
                abort();
            }
            pinvoke = emit_tojlinvoke(codeinst, invokeName, mod, params);
            if (!proto.specsig) {
                proto.decl->replaceAllUsesWith(pinvoke);
                proto.decl->eraseFromParent();
                proto.decl = pinvoke;
            }
        }
        if (proto.specsig && !preal_specsig) {
            // get or build an fptr1 that can invoke codeinst
            if (pinvoke == nullptr)
                pinvoke = get_or_emit_fptr1(preal_decl, mod);
            // emit specsig-to-(jl)invoke conversion
            proto.decl->setLinkage(GlobalVariable::InternalLinkage);
            //protodecl->setAlwaysInline();
            jl_init_function(proto.decl, params.TargetTriple);
            jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
            size_t nrealargs = jl_nparams(mi->specTypes); // number of actual arguments being passed
            bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
            // TODO: maybe this can be cached in codeinst->specfptr?
            emit_specsig_to_fptr1(proto.decl, proto.cc, proto.return_roots, mi->specTypes, codeinst->rettype, is_opaque_closure, nrealargs, params, pinvoke);
            preal_decl = ""; // no need to fixup the name
        }
        if (!preal_decl.empty()) {
            // merge and/or rename this prototype to the real function
            if (Function *specfun = cast_or_null<Function>(mod->getNamedValue(preal_decl))) {
                if (proto.decl != specfun) {
                    proto.decl->replaceAllUsesWith(specfun);
                    proto.decl->eraseFromParent();
                    proto.decl = specfun;
                }
            }
            else {
                proto.decl->setName(preal_decl);
            }
        }
        if (proto.oc) { // additionally, if we are dealing with an oc, then we might also need to fix up the fptr1 reference too
            assert(proto.specsig);
            StringRef ocinvokeDecl = invokeName;
            // if OC expected a specialized specsig dispatch, but we don't have it, use the inner trampoline here too
            // XXX: this invoke translation logic is supposed to exactly match new_opaque_closure
            if (!preal_specsig || ocinvokeDecl == "jl_f_opaque_closure_call" || ocinvokeDecl == "jl_fptr_interpret_call" || ocinvokeDecl == "jl_fptr_const_return")
                ocinvokeDecl = pinvoke->getName();
            assert(!ocinvokeDecl.empty());
            assert(ocinvokeDecl != "jl_fptr_args");
            assert(ocinvokeDecl != "jl_fptr_const_return");
            assert(ocinvokeDecl != "jl_fptr_sparam");
            // merge and/or rename this prototype to the real function
            if (Function *specfun = cast_or_null<Function>(mod->getNamedValue(ocinvokeDecl))) {
                if (proto.oc != specfun) {
                    proto.oc->replaceAllUsesWith(specfun);
                    proto.oc->eraseFromParent();
                    proto.oc = specfun;
                }
            }
            else {
                proto.oc->setName(ocinvokeDecl);
            }
        }
        workqueue.append(params.workqueue);
        params.workqueue.clear();
    }
    JL_GC_POP();
}


/// Link the function in the source module into the destination module if
/// needed, setting up mapping information.
/// Similar to orc::cloneFunctionDecl, but more complete for greater correctness
Function *IRLinker_copyFunctionProto(Module *DstM, Function *SF) {
  // If there is no linkage to be performed or we are linking from the source,
  // bring SF over, if we haven't already.
  if (SF->getParent() == DstM)
    return SF;
  if (auto *F = DstM->getNamedValue(SF->getName()))
      return cast<Function>(F);
  auto *F = Function::Create(SF->getFunctionType(), SF->getLinkage(),
                             SF->getAddressSpace(), SF->getName(), DstM);
  F->copyAttributesFrom(SF);
  F->IsNewDbgInfoFormat = SF->IsNewDbgInfoFormat;

  // Remove these copied constants since they point to the source module.
  F->setPersonalityFn(nullptr);
  F->setPrefixData(nullptr);
  F->setPrologueData(nullptr);
  return F;
}

static Function *aot_abi_converter(jl_codegen_params_t &params, Module *M, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_code_instance_t *codeinst, Module *defM, StringRef func, StringRef specfunc, bool target_specsig)
{
    std::string gf_thunk_name;
    if (!specfunc.empty()) {
        Value *llvmtarget = IRLinker_copyFunctionProto(M, defM->getFunction(specfunc));
        gf_thunk_name = emit_abi_converter(M, params, declrt, sigt, nargs, specsig, codeinst, llvmtarget, target_specsig);
    }
    else {
        Value *llvmtarget = func.empty() ? nullptr : IRLinker_copyFunctionProto(M, defM->getFunction(func));
        gf_thunk_name = emit_abi_dispatcher(M, params, declrt, sigt, nargs, specsig, codeinst, llvmtarget);
    }
    auto F = M->getFunction(gf_thunk_name);
    assert(F);
    return F;
}

static void generate_cfunc_thunks(jl_codegen_params_t &params, jl_compiled_functions_t &compiled_functions)
{
    DenseMap<jl_method_instance_t*, jl_code_instance_t*> compiled_mi;
    for (auto &def : compiled_functions) {
        jl_code_instance_t *this_code = def.first;
        jl_method_instance_t *mi = jl_get_ci_mi(this_code);
        if (this_code->owner == jl_nothing && jl_atomic_load_relaxed(&this_code->max_world) == ~(size_t)0 && this_code->def == (jl_value_t*)mi)
            compiled_mi[mi] = this_code;
    }
    size_t latestworld = jl_atomic_load_acquire(&jl_world_counter);
    for (cfunc_decl_t &cfunc : params.cfuncs) {
        Module *M = cfunc.theFptr->getParent();
        jl_value_t *sigt = cfunc.sigt;
        JL_GC_PROMISE_ROOTED(sigt);
        jl_value_t *declrt = cfunc.declrt;
        JL_GC_PROMISE_ROOTED(declrt);
        Function *unspec = aot_abi_converter(params, M, declrt, sigt, cfunc.nargs, cfunc.specsig, nullptr, nullptr, "", "", false);
        jl_code_instance_t *codeinst = nullptr;
        auto assign_fptr = [&params, &cfunc, &codeinst, &unspec](Function *f) {
            ConstantArray *init = cast<ConstantArray>(cfunc.cfuncdata->getInitializer());
            SmallVector<Constant*,6> initvals;
            for (unsigned i = 0; i < init->getNumOperands(); ++i)
                initvals.push_back(init->getOperand(i));
            assert(initvals.size() == 6);
            assert(initvals[0]->isNullValue());
            if (codeinst) {
                Constant *llvmcodeinst = literal_pointer_val_slot(params, f->getParent(), (jl_value_t*)codeinst);
                initvals[0] = llvmcodeinst; // plast_codeinst
            }
            assert(initvals[2]->isNullValue());
            initvals[2] = unspec;
            cfunc.cfuncdata->setInitializer(ConstantArray::get(init->getType(), initvals));
            cfunc.theFptr->setInitializer(f);
        };
        Module *defM = nullptr;
        StringRef func;
        jl_method_instance_t *mi = jl_get_specialization1((jl_tupletype_t*)sigt, latestworld, 0);
        if (mi) {
            auto it = compiled_mi.find(mi);
            if (it != compiled_mi.end()) {
                codeinst = it->second;
                JL_GC_PROMISE_ROOTED(codeinst);
                auto defs = compiled_functions.find(codeinst);
                defM = defs->second.TSM.getModuleUnlocked();
                const jl_llvm_functions_t &decls = defs->second.decls;
                func = decls.functionObject;
                StringRef specfunc = decls.specFunctionObject;
                jl_value_t *astrt = codeinst->rettype;
                if (astrt != (jl_value_t*)jl_bottom_type &&
                    jl_type_intersection(astrt, declrt) == jl_bottom_type) {
                    // Do not warn if the function never returns since it is
                    // occasionally required by the C API (typically error callbacks)
                    // even though we're likely to encounter memory errors in that case
                    jl_printf(JL_STDERR, "WARNING: cfunction: return type of %s does not match\n", name_from_method_instance(mi));
                }
                if (func == "jl_fptr_const_return") {
                    std::string gf_thunk_name = emit_abi_constreturn(M, params, declrt, sigt, cfunc.nargs, cfunc.specsig, codeinst->rettype_const);
                    auto F = M->getFunction(gf_thunk_name);
                    assert(F);
                    assign_fptr(F);
                    continue;
                }
                else if (func == "jl_fptr_args") {
                    assert(!specfunc.empty());
                    if (!cfunc.specsig && jl_subtype(astrt, declrt)) {
                        assign_fptr(IRLinker_copyFunctionProto(M, defM->getFunction(specfunc)));
                        continue;
                    }
                    assign_fptr(aot_abi_converter(params, M, declrt, sigt, cfunc.nargs, cfunc.specsig, codeinst, defM, func, specfunc, false));
                    continue;
                }
                else if (func == "jl_fptr_sparam" || func == "jl_f_opaque_closure_call") {
                    func = ""; // use jl_invoke instead for these, since we don't declare these prototypes
                }
                else {
                    assert(!specfunc.empty());
                    if (jl_egal(mi->specTypes, sigt) && jl_egal(declrt, astrt)) {
                        assign_fptr(IRLinker_copyFunctionProto(M, defM->getFunction(specfunc)));
                        continue;
                    }
                    assign_fptr(aot_abi_converter(params, M, declrt, sigt, cfunc.nargs, cfunc.specsig, codeinst, defM, func, specfunc, true));
                    continue;
                }
            }
        }
        Function *f = codeinst ? aot_abi_converter(params, M, declrt, sigt, cfunc.nargs, cfunc.specsig, codeinst, defM, func, "", false) : unspec;
        return assign_fptr(f);
    }
}

// destructively move the contents of src into dest
// this assumes that the targets of the two modules are the same
// including the DataLayout and ModuleFlags (for example)
// and that there is no module-level assembly
// Comdat is also removed, since this needs to be re-added later
static void jl_merge_module(Linker &L, orc::ThreadSafeModule srcTSM) JL_NOTSAFEPOINT
{
    srcTSM.consumingModuleDo([&L](std::unique_ptr<Module> src) JL_NOTSAFEPOINT {
        bool error = L.linkInModule(std::move(src));
        assert(!error && "linking llvmcall modules failed");
        (void)error;
    });
}

static bool canPartition(const Function &F)
{
    return !F.hasFnAttribute(Attribute::AlwaysInline) &&
           !F.hasFnAttribute(Attribute::InlineHint);
}

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
// `_external_linkage` create linkages between pkgimages.
extern "C" JL_DLLEXPORT_CODEGEN
void *jl_create_native_impl(jl_array_t *methods, LLVMOrcThreadSafeModuleRef llvmmod, int _trim, int _external_linkage, size_t world)
{
    JL_TIMING(INFERENCE, INFERENCE);
    auto ct = jl_current_task;
    bool timed = (ct->reentrant_timing & 1) == 0;
    if (timed)
        ct->reentrant_timing |= 1;
    uint64_t compiler_start_time = 0;
    uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
    if (measure_compile_time_enabled)
        compiler_start_time = jl_hrtime();

    jl_cgparams_t cgparams = jl_default_cgparams;
    cgparams.trim = _trim ? 1 : 0;
    size_t compile_for[] = { jl_typeinf_world, world };
    int compiler_world = 1;
    if (_trim || compile_for[0] == 0)
        compiler_world = 0;
    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 4);
    jl_array_t *codeinfos = NULL;
    if (jl_typeinf_func) {
        fargs[0] = (jl_value_t*)jl_typeinf_func;
        fargs[1] = (jl_value_t*)methods;
#ifdef _P64
        jl_value_t *jl_array_ulong_type = jl_array_uint64_type;
#else
        jl_value_t *jl_array_ulong_type = jl_array_uint32_type;
#endif
        jl_array_t *worlds = jl_alloc_array_1d(jl_array_ulong_type, 1 + compiler_world);
        fargs[2] = (jl_value_t*)worlds;
        jl_array_data(worlds, size_t)[0] = jl_typeinf_world;
        jl_array_data(worlds, size_t)[compiler_world] = world; // might overwrite previous
        fargs[3] = _trim ? jl_true : jl_false;
        size_t last_age = ct->world_age;
        ct->world_age = jl_typeinf_world;
        codeinfos = (jl_array_t*)jl_apply(fargs, 4);
        ct->world_age = last_age;
        JL_TYPECHK(create_native, array_any, (jl_value_t*)codeinfos);
    }
    else {
        // we could put a very simple generator here, but there is no reason to do that right now
        jl_error("inference not available for generating compiled output");
    }
    fargs[0] = (jl_value_t*)codeinfos;
    void *data = jl_emit_native(codeinfos, llvmmod, &cgparams, _external_linkage);

    // move everything inside, now that we've merged everything
    // (before adding the exported headers)
    ((jl_native_code_desc_t*)data)->M.withModuleDo([&](Module &M) {
        auto TT = Triple(M.getTargetTriple());
        Function *juliapersonality_func = nullptr;
        if (TT.isOSWindows() && TT.getArch() == Triple::x86_64) {
            // setting the function personality enables stack unwinding and catching exceptions
            // so make sure everything has something set
            Type *T_int32 = Type::getInt32Ty(M.getContext());
            juliapersonality_func = Function::Create(FunctionType::get(T_int32, true),
                Function::ExternalLinkage, "__julia_personality", M);
            juliapersonality_func->setDLLStorageClass(GlobalValue::DLLImportStorageClass);
        }
        for (GlobalObject &G : M.global_objects()) {
            if (!G.isDeclaration()) {
                G.setLinkage(GlobalValue::InternalLinkage);
                G.setDSOLocal(true);
                makeSafeName(G);
                if (Function *F = dyn_cast<Function>(&G)) {
                    if (juliapersonality_func) {
                        // Add unwind exception personalities to functions to handle async exceptions
                        F->setPersonalityFn(juliapersonality_func);
                    }
                }
            }
        }
    });

    JL_GC_POP();
    if (timed) {
        if (measure_compile_time_enabled) {
            auto end = jl_hrtime();
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
        }
        ct->reentrant_timing &= ~1ull;
    }
    return data;
}

// also be used be extern consumers like GPUCompiler.jl to obtain a module containing
// all reachable & inferrrable functions.
extern "C" JL_DLLEXPORT_CODEGEN
void *jl_emit_native_impl(jl_array_t *codeinfos, LLVMOrcThreadSafeModuleRef llvmmod, const jl_cgparams_t *cgparams, int _external_linkage)
{
    JL_TIMING(NATIVE_AOT, NATIVE_Create);
    ++CreateNativeCalls;
    CreateNativeMax.updateMax(jl_array_nrows(codeinfos));
    if (cgparams == NULL)
        cgparams = &jl_default_cgparams;
    jl_native_code_desc_t *data = new jl_native_code_desc_t;
    jl_method_instance_t *mi = NULL;
    orc::ThreadSafeContext ctx;
    orc::ThreadSafeModule backing;
    if (!llvmmod) {
        ctx = jl_ExecutionEngine->makeContext();
        backing = jl_create_ts_module("text", ctx, jl_ExecutionEngine->getDataLayout(), jl_ExecutionEngine->getTargetTriple());
    }
    orc::ThreadSafeModule &clone = llvmmod ? *unwrap(llvmmod) : backing;
    auto ctxt = clone.getContext();

    // compile all methods for the current world and type-inference world
    auto target_info = clone.withModuleDo([&](Module &M) {
        return std::make_pair(M.getDataLayout(), Triple(M.getTargetTriple()));
    });
    egal_set method_roots;
    jl_codegen_params_t params(ctxt, std::move(target_info.first), std::move(target_info.second));
    if (!llvmmod)
        params.getContext().setDiscardValueNames(true);
    params.params = cgparams;
    assert(params.imaging_mode); // `_imaging_mode` controls if broken features like code-coverage are disabled
    params.external_linkage = _external_linkage;
    params.temporary_roots = jl_alloc_array_1d(jl_array_any_type, 0);
    bool safepoint_on_entry = params.safepoint_on_entry;
    JL_GC_PUSH3(&params.temporary_roots, &method_roots.list, &method_roots.keyset);
    jl_compiled_functions_t compiled_functions;
    size_t i, l;
    for (i = 0, l = jl_array_nrows(codeinfos); i < l; i++) {
        // each item in this list is either a CodeInstance followed by a CodeInfo indicating something
        // to compile, or a rettype followed by a sig describing a C-callable alias to create.
        jl_value_t *item = jl_array_ptr_ref(codeinfos, i);
        if (jl_is_code_instance(item)) {
            // now add it to our compilation results
            jl_code_instance_t *codeinst = (jl_code_instance_t*)item;
            jl_code_info_t *src = (jl_code_info_t*)jl_array_ptr_ref(codeinfos, ++i);
            assert(jl_is_code_info(src));
            if (compiled_functions.count(codeinst))
                continue; // skip any duplicates that accidentally made there way in here (or make this an error?)
            if (jl_ir_inlining_cost((jl_value_t*)src) < UINT16_MAX)
                params.safepoint_on_entry = false; // ensure we don't block ExpandAtomicModifyPass from inlining this code if applicable
            orc::ThreadSafeModule result_m = jl_create_ts_module(name_from_method_instance(jl_get_ci_mi(codeinst)),
                    params.tsctx, clone.getModuleUnlocked()->getDataLayout(),
                    Triple(clone.getModuleUnlocked()->getTargetTriple()));
            jl_llvm_functions_t decls;
            if (jl_atomic_load_relaxed(&codeinst->invoke) == jl_fptr_const_return_addr)
                decls.functionObject = "jl_fptr_const_return";
            else
                decls = jl_emit_codeinst(result_m, codeinst, src, params);
            params.safepoint_on_entry = safepoint_on_entry;
            record_method_roots(method_roots, jl_get_ci_mi(codeinst));
            if (result_m)
                compiled_functions[codeinst] = {std::move(result_m), std::move(decls)};
            else if (params.params->trim) {
                // if we're building a small image, we need to compile everything
                // to ensure that we have all the information we need.
                jl_safe_printf("codegen failed to compile code root ");
                jl_(mi);
                abort();
            }
        }
        else {
            jl_value_t *sig = jl_array_ptr_ref(codeinfos, ++i);
            assert(jl_is_type(item) && jl_is_type(sig));
            jl_compile_extern_c(wrap(&clone), &params, NULL, item, sig);
        }
    }
    // finally, make sure all referenced methods get fixed up, particularly if the user declined to compile them
    resolve_workqueue(params, method_roots, compiled_functions);
    // including generating cfunction thunks
    generate_cfunc_thunks(params, compiled_functions);
    aot_optimize_roots(params, method_roots, compiled_functions);
    params.temporary_roots = nullptr;
    JL_GC_POP();

    // process the globals array, before jl_merge_module destroys them
    SmallVector<std::string, 0> gvars(params.global_targets.size());
    data->jl_value_to_llvm.resize(params.global_targets.size());
    StringSet<> gvars_names;
    DenseSet<GlobalValue *> gvars_set;

    size_t idx = 0;
    for (auto &global : params.global_targets) {
        gvars[idx] = global.second->getName().str();
        assert(gvars_set.insert(global.second).second && "Duplicate gvar in params!");
        assert(gvars_names.insert(gvars[idx]).second && "Duplicate gvar name in params!");
        data->jl_value_to_llvm[idx] = global.first;
        idx++;
    }
    CreateNativeMethods += compiled_functions.size();

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
    {
        Linker L(*clone.getModuleUnlocked());
        for (auto &def : compiled_functions) {
            jl_code_instance_t *this_code = def.first;
            JL_GC_PROMISE_ROOTED(this_code);
            jl_llvm_functions_t &decls = def.second.decls;
            StringRef func = decls.functionObject;
            StringRef cfunc = decls.specFunctionObject;
            orc::ThreadSafeModule &M = def.second.TSM;
            if (_external_linkage) {
                uint8_t specsigflags;
                jl_callptr_t invoke;
                void *fptr;
                jl_read_codeinst_invoke(this_code, &specsigflags, &invoke, &fptr, 0);
                if (invoke != NULL && (specsigflags & 0b100)) {
                    // this codeinst is already available externally: keep it only if canPartition demands it for local use
                    // TODO: for performance, avoid generating the src code when we know it would reach here anyways?
                    if (M.withModuleDo([&](Module &M) { return !canPartition(*cast<Function>(M.getNamedValue(cfunc))); })) {
                        jl_merge_module(L, std::move(M));
                    }
                    continue;
                }
            }
            jl_merge_module(L, std::move(M));
            uint32_t func_id = 0;
            uint32_t cfunc_id = 0;
            if (func == "jl_fptr_args") {
                func_id = -1;
            }
            else if (func == "jl_fptr_sparam") {
                func_id = -2;
            }
            else if (func == "jl_f_opaque_closure_call") {
                func_id = -4;
            }
            else if (func == "jl_fptr_const_return") {
                func_id = -5;
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
        bool Changed = true;
        while (Changed) {
            Changed = false;
            // make sure everything referenced got included though, since some functions aren't
            // correctly implemented by staticdata for external use, and so codegen won't emit
            // an external reference but expects a private copy here instead
            for (auto &def : compiled_functions) {
                orc::ThreadSafeModule &M = def.second.TSM;
                if (!M)
                    continue;
                jl_llvm_functions_t &decls = def.second.decls;
                StringRef func = decls.functionObject;
                StringRef cfunc = decls.specFunctionObject;
                if (func != "jl_fptr_args" &&
                    func != "jl_fptr_sparam" &&
                    func != "jl_f_opaque_closure_call" &&
                    clone.getModuleUnlocked()->getNamedValue(func)) {
                    jl_merge_module(L, std::move(M));
                    Changed = true;
                    continue;
                }
                if (!cfunc.empty() && clone.getModuleUnlocked()->getNamedValue(cfunc)) {
                    Changed = true;
                    jl_merge_module(L, std::move(M));
                }
            }
        }
#ifndef NDEBUG
        // make sure we didn't forget anything that we promised to include in here
        for (auto &def : compiled_functions) {
            jl_llvm_functions_t &decls = def.second.decls;
            StringRef func = decls.functionObject;
            StringRef cfunc = decls.specFunctionObject;
            if (func != "jl_fptr_args" &&
                func != "jl_fptr_sparam" &&
                func != "jl_f_opaque_closure_call") {
                GlobalValue *F = clone.getModuleUnlocked()->getNamedValue(func);
                assert(!F || !F->isDeclaration());
            }
            if (!cfunc.empty()) {
                GlobalValue *F = clone.getModuleUnlocked()->getNamedValue(cfunc);
                assert(!F || !F->isDeclaration());
            }
        }
#endif
        compiled_functions.clear();
        if (params._shared_module) {
            bool error = L.linkInModule(std::move(params._shared_module));
            assert(!error && "Error linking in shared module");
            (void)error;
        }
    }

    // now get references to the globals in the merged module
    // and set them to be internalized and initialized at startup
    // filter out any gvars that got optimized away
    idx = 0;
    size_t newoffset = 0;
    size_t newidx = 0;
    for (auto &global : gvars) {
        //Safe b/c context is locked by params
        GlobalVariable *G = cast_or_null<GlobalVariable>(clone.getModuleUnlocked()->getNamedValue(global));
        if (G != nullptr) {
            assert(!G->hasInitializer());
            G->setInitializer(Constant::getNullValue(G->getValueType()));
            G->setLinkage(GlobalValue::InternalLinkage);
            G->setDSOLocal(true);
            assert(newidx == data->jl_sysimg_gvars.size());
            if (idx < offset) {
                data->jl_value_to_llvm[newidx] = data->jl_value_to_llvm[idx];
                newoffset = newidx + 1;
            }
            else {
                data->jl_external_to_llvm[newidx - newoffset] = data->jl_external_to_llvm[idx - offset];
            }
            data->jl_sysimg_gvars.push_back(G);
            newidx++;
        }
        idx++;
    }
    data->jl_value_to_llvm.resize(newoffset);
    data->jl_external_to_llvm.resize(newidx - newoffset);
    gvars.clear();
    CreateNativeGlobals += idx;

    data->M = std::move(clone);
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
            gv->setDSOLocal(true);
            return gv;
        };
        auto table = tables.data() + i * sizeof(jl_image_shard_t) / sizeof(void *);
        table[offsetof(jl_image_shard_t, fvar_count) / sizeof(void*)] = create_gv("jl_fvar_count", true);
        table[offsetof(jl_image_shard_t, fvar_ptrs) / sizeof(void*)] = create_gv("jl_fvar_ptrs", true);
        table[offsetof(jl_image_shard_t, fvar_idxs) / sizeof(void*)] = create_gv("jl_fvar_idxs", true);
        table[offsetof(jl_image_shard_t, gvar_offsets) / sizeof(void*)] = create_gv("jl_gvar_offsets", true);
        table[offsetof(jl_image_shard_t, gvar_idxs) / sizeof(void*)] = create_gv("jl_gvar_idxs", true);
        table[offsetof(jl_image_shard_t, clone_slots) / sizeof(void*)] = create_gv("jl_clone_slots", true);
        table[offsetof(jl_image_shard_t, clone_ptrs) / sizeof(void*)] = create_gv("jl_clone_ptrs", true);
        table[offsetof(jl_image_shard_t, clone_idxs) / sizeof(void*)] = create_gv("jl_clone_idxs", true);
    }
    auto tables_arr = ConstantArray::get(ArrayType::get(T_psize, tables.size()), tables);
    auto tables_gv = new GlobalVariable(M, tables_arr->getType(), false,
                                        GlobalValue::ExternalLinkage, tables_arr, "jl_shard_tables");
    tables_gv->setVisibility(GlobalValue::HiddenVisibility);
    tables_gv->setDSOLocal(true);
    return tables_gv;
}

// See src/processor.h for documentation about this table. Corresponds to jl_image_ptls_t.
static GlobalVariable *emit_ptls_table(Module &M, Type *T_size, Type *T_psize) {
    std::array<Constant *, 3> ptls_table{
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_pgcstack_func_slot"),
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_pgcstack_key_slot"),
        new GlobalVariable(M, T_size, false, GlobalValue::ExternalLinkage, Constant::getNullValue(T_size), "jl_tls_offset"),
    };
    for (auto &gv : ptls_table) {
        cast<GlobalVariable>(gv)->setVisibility(GlobalValue::HiddenVisibility);
        cast<GlobalVariable>(gv)->setDSOLocal(true);
    }
    auto ptls_table_arr = ConstantArray::get(ArrayType::get(T_psize, ptls_table.size()), ptls_table);
    auto ptls_table_gv = new GlobalVariable(M, ptls_table_arr->getType(), false,
                                            GlobalValue::ExternalLinkage, ptls_table_arr, "jl_ptls_table");
    ptls_table_gv->setVisibility(GlobalValue::HiddenVisibility);
    ptls_table_gv->setDSOLocal(true);
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
        info.clones = APInt(val.size() * 4, val, 16).popcount() + 1;
    }
    info.weight += info.insts;
    // more basic blocks = more complex than just sum of insts,
    // add some weight to it
    info.weight += info.bbs;
    info.weight *= info.clones;
    return info;
}

struct ModuleInfo {
    Triple triple;
    size_t globals;
    size_t funcs;
    size_t bbs;
    size_t insts;
    size_t clones;
    size_t weight;
};

ModuleInfo compute_module_info(Module &M) {
    ModuleInfo info;
    info.triple = Triple(M.getTargetTriple());
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
    StringMap<bool> globals;
    StringMap<unsigned> fvars;
    StringMap<unsigned> gvars;
    size_t weight;
};

static inline bool verify_partitioning(const SmallVectorImpl<Partition> &partitions, const Module &M, DenseMap<GlobalValue *, unsigned> &fvars, DenseMap<GlobalValue *, unsigned> &gvars) {
    bool bad = false;
#ifndef JL_NDEBUG
    size_t fvars_size = fvars.size();
    size_t gvars_size = gvars.size();
    SmallVector<uint32_t, 0> fvars_partition(fvars_size);
    SmallVector<uint32_t, 0> gvars_partition(gvars_size);
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
            if (fvars_partition[fvar.second] != 0) {
                bad = true;
                dbgs() << "Duplicate fvar " << fvar.first() << " in partitions " << i << " and " << fvars_partition[fvar.second] - 1 << "\n";
            }
            fvars_partition[fvar.second] = i+1;
        }
        for (auto &gvar : partitions[i].gvars) {
            if (gvars_partition[gvar.second] != 0) {
                bad = true;
                dbgs() << "Duplicate gvar " << gvar.first() << " in partitions " << i << " and " << gvars_partition[gvar.second] - 1 << "\n";
            }
            gvars_partition[gvar.second] = i+1;
        }
    }
    for (auto &GV : M.global_values()) {
        if (GV.isDeclaration()) {
            if (GVNames.count(GV.getName())) {
                bad = true;
                dbgs() << "Global " << GV.getName() << " is a declaration but is in partition " << GVNames[GV.getName()] << "\n";
            }
        } else {
            // Local global values are not partitioned
            if (!GVNames.count(GV.getName())) {
                bad = true;
                dbgs() << "Global " << GV << " not in any partition\n";
            }
            for (ConstantUses<GlobalValue> uses(const_cast<GlobalValue*>(&GV), const_cast<Module&>(M)); !uses.done(); uses.next()) {
                auto val = uses.get_info().val;
                if (!GVNames.count(val->getName())) {
                    bad = true;
                    dbgs() << "Global " << val->getName() << " used by " << GV.getName() << ", which is not in any partition\n";
                    continue;
                }
                if (GVNames[val->getName()] != GVNames[GV.getName()]) {
                    bad = true;
                    dbgs() << "Global " << val->getName() << " used by " << GV.getName() << ", which is in partition " << GVNames[GV.getName()] << " but " << val->getName() << " is in partition " << GVNames[val->getName()] << "\n";
                }
            }
        }
    }
    for (uint32_t i = 0; i < fvars_size; i++) {
        if (fvars_partition[i] == 0) {
            auto gv = find_if(fvars.begin(), fvars.end(), [i](auto var) { return var.second == i; });
            bad = true;
            dbgs() << "fvar " << gv->first->getName() << " at " << i << " not in any partition\n";
        }
    }
    for (uint32_t i = 0; i < gvars_size; i++) {
        if (gvars_partition[i] == 0) {
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
        SmallVector<Node, 0> nodes;
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
        // Currently ccallable global aliases have extern linkage, we only want to make the
        // internally linked functions/global variables extern+hidden
        if (G.hasLocalLinkage()) {
            G.setLinkage(GlobalValue::ExternalLinkage);
            G.setVisibility(GlobalValue::HiddenVisibility);
        }
        if (auto F = dyn_cast<Function>(&G)) {
            partitioner.make(&G, getFunctionWeight(*F).weight);
        }
        else {
            partitioner.make(&G, 1);
        }
    }

    // Merge all uses to go together into the same partition
    for (unsigned i = 0; i < partitioner.nodes.size(); ++i) {
        for (ConstantUses<GlobalValue> uses(partitioner.nodes[i].GV, M); !uses.done(); uses.next()) {
            auto val = uses.get_info().val;
            auto idx = partitioner.node_map.find(val);
            // This can fail if we can't partition a global, but it uses something we can partition
            // This should be fixed by altering canPartition to not permit partitioning this global
            assert(idx != partitioner.node_map.end());
            partitioner.merge(i, idx->second);
        }
    }

    SmallVector<Partition, 32> partitions(threads);
    // always get the smallest partition first
    auto pcomp = [](const Partition *p1, const Partition *p2) {
        return p1->weight > p2->weight;
    };
    std::priority_queue<Partition *, SmallVector<Partition *, 0>, decltype(pcomp)> pq(pcomp);
    for (unsigned i = 0; i < threads; ++i) {
        pq.push(&partitions[i]);
    }

    SmallVector<unsigned, 0> idxs(partitioner.nodes.size());
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
        assert(root == i || partitioner.nodes[root].weight == 0);
        if (partitioner.nodes[root].weight) {
            auto &node = partitioner.nodes[root];
            auto &P = *pq.top();
            pq.pop();
            auto name = node.GV->getName();
            P.globals.insert({name, true});
            if (fvars.count(node.GV))
                P.fvars[name] = fvars[node.GV];
            if (gvars.count(node.GV))
                P.gvars[name] = gvars[node.GV];
            P.weight += node.weight;
            node.weight = 0;
            node.size = &P - partitions.data();
            pq.push(&P);
        }
        if (root != i) {
            auto &node = partitioner.nodes[i];
            assert(node.weight != 0);
            // we assigned its root already, so just add it to the root's partition
            // don't touch the priority queue, since we're not changing the weight
            auto &P = partitions[partitioner.nodes[root].size];
            auto name = node.GV->getName();
            P.globals.insert({name, true});
            if (fvars.count(node.GV))
                P.fvars[name] = fvars[node.GV];
            if (gvars.count(node.GV))
                P.gvars[name] = gvars[node.GV];
            node.weight = 0;
            node.size = partitioner.nodes[root].size;
        }
    }

    bool verified = verify_partitioning(partitions, M, fvars, gvars);
    if (!verified)
        llvm_dump(&M);
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
        auto total = deserialize.elapsed + materialize.elapsed + construct.elapsed +
            unopt.elapsed + optimize.elapsed + opt.elapsed + obj.elapsed + asm_.elapsed;
        out << "Time (s)  Name  Description\n";
        deserialize.print(out, clear);
        materialize.print(out, clear);
        construct.print(out, clear);
        unopt.print(out, clear);
        optimize.print(out, clear);
        opt.print(out, clear);
        obj.print(out, clear);
        asm_.print(out, clear);
        out << llvm::formatv("{0:F3}  total  Total time taken\n", total / 1e9);
    }
};

struct AOTOutputs {
    SmallVector<char, 0> unopt, opt, obj, asm_;
};

// Perform the actual optimization and emission of the output files
static AOTOutputs add_output_impl(Module &M, TargetMachine &SourceTM, ShardTimers &timers,
        bool unopt, bool opt, bool obj, bool asm_) {
    assert((unopt || opt || obj || asm_) && "no output requested");
    AOTOutputs out;
    auto TM = std::unique_ptr<TargetMachine>(
        SourceTM.getTarget().createTargetMachine(
            SourceTM.getTargetTriple().str(),
            SourceTM.getTargetCPU(),
            SourceTM.getTargetFeatureString(),
            SourceTM.Options,
            SourceTM.getRelocationModel(),
            SourceTM.getCodeModel(),
            SourceTM.getOptLevel()));
    fixupTM(*TM);
    if (unopt) {
        timers.unopt.startTimer();
        raw_svector_ostream OS(out.unopt);
        PassBuilder PB;
        AnalysisManagers AM{*TM, PB, OptimizationLevel::O0};
        ModulePassManager MPM;
        MPM.addPass(BitcodeWriterPass(OS));
        MPM.run(M, AM.MAM);
        timers.unopt.stopTimer();
    }
    if (!opt && !obj && !asm_) {
        return out;
    }
    assert(!verifyLLVMIR(M));

    {
        timers.optimize.startTimer();

        auto PMTM = std::unique_ptr<TargetMachine>(
            SourceTM.getTarget().createTargetMachine(
                SourceTM.getTargetTriple().str(),
                SourceTM.getTargetCPU(),
                SourceTM.getTargetFeatureString(),
                SourceTM.Options,
                SourceTM.getRelocationModel(),
                SourceTM.getCodeModel(),
                SourceTM.getOptLevel()));
        fixupTM(*PMTM);
        NewPM optimizer{std::move(PMTM), getOptLevel(jl_options.opt_level), OptimizationOptions::defaults(true, true)};
        optimizer.run(M);
        assert(!verifyLLVMIR(M));
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
            // Float16 conversion routines
#if defined(_CPU_X86_64_) && defined(_OS_DARWIN_)
            // LLVM 16 reverted to soft-float ABI for passing half on x86_64 Darwin
            // https://github.com/llvm/llvm-project/commit/2bcf51c7f82ca7752d1bba390a2e0cb5fdd05ca9
            injectCRTAlias(M, "__gnu_h2f_ieee", "julia_half_to_float",
                    FunctionType::get(Type::getFloatTy(M.getContext()), { Type::getInt16Ty(M.getContext()) }, false));
            injectCRTAlias(M, "__extendhfsf2", "julia_half_to_float",
                    FunctionType::get(Type::getFloatTy(M.getContext()), { Type::getInt16Ty(M.getContext()) }, false));
            injectCRTAlias(M, "__gnu_f2h_ieee", "julia_float_to_half",
                    FunctionType::get(Type::getInt16Ty(M.getContext()), { Type::getFloatTy(M.getContext()) }, false));
            injectCRTAlias(M, "__truncsfhf2", "julia_float_to_half",
                    FunctionType::get(Type::getInt16Ty(M.getContext()), { Type::getFloatTy(M.getContext()) }, false));
            injectCRTAlias(M, "__truncdfhf2", "julia_double_to_half",
                    FunctionType::get(Type::getInt16Ty(M.getContext()), { Type::getDoubleTy(M.getContext()) }, false));
#else
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
#endif

            // BFloat16 conversion routines
            injectCRTAlias(M, "__truncsfbf2", "julia__truncsfbf2",
                    FunctionType::get(Type::getBFloatTy(M.getContext()), { Type::getFloatTy(M.getContext()) }, false));
            injectCRTAlias(M, "__truncsdbf2", "julia__truncdfbf2",
                    FunctionType::get(Type::getBFloatTy(M.getContext()), { Type::getDoubleTy(M.getContext()) }, false));
        }
        timers.optimize.stopTimer();
    }

    if (opt) {
        timers.opt.startTimer();
        raw_svector_ostream OS(out.opt);
        PassBuilder PB;
        AnalysisManagers AM{*TM, PB, OptimizationLevel::O0};
        ModulePassManager MPM;
        MPM.addPass(BitcodeWriterPass(OS));
        MPM.run(M, AM.MAM);
        timers.opt.stopTimer();
    }

    if (obj) {
        timers.obj.startTimer();
        raw_svector_ostream OS(out.obj);
        legacy::PassManager emitter;
        addTargetPasses(&emitter, TM->getTargetTriple(), TM->getTargetIRAnalysis());
#if JL_LLVM_VERSION >= 180000
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CodeGenFileType::ObjectFile, false))
#else
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CGFT_ObjectFile, false))
#endif
            jl_safe_printf("ERROR: target does not support generation of object files\n");
        emitter.run(M);
        timers.obj.stopTimer();
    }

    if (asm_) {
        timers.asm_.startTimer();
        raw_svector_ostream OS(out.asm_);
        legacy::PassManager emitter;
        addTargetPasses(&emitter, TM->getTargetTriple(), TM->getTargetIRAnalysis());
#if JL_LLVM_VERSION >= 180000
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CodeGenFileType::AssemblyFile, false))
#else
        if (TM->addPassesToEmitFile(emitter, OS, nullptr, CGFT_AssemblyFile, false))
#endif
            jl_safe_printf("ERROR: target does not support generation of assembly files\n");
        emitter.run(M);
        timers.asm_.stopTimer();
    }

    return out;
}

// serialize module to bitcode
static auto serializeModule(const Module &M) {
    assert(!verifyLLVMIR(M) && "Serializing invalid module!");
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
// that are not in this partition. Once unnecessary functions are
// deleted, we then materialize the entire module to make use-lists
// consistent.
static void materializePreserved(Module &M, Partition &partition) {
    DenseSet<GlobalValue *> Preserve;
    for (auto &Name : partition.globals) {
        auto *GV = M.getNamedValue(Name.first());
        assert(GV && !GV->isDeclaration() && !GV->hasLocalLinkage());
        if (!Name.second) {
            // We skip partitioning for internal variables, so this has
            // the same effect as putting it in preserve.
            // This just avoids a hashtable lookup.
            GV->setLinkage(GlobalValue::InternalLinkage);
            assert(GV->hasDefaultVisibility());
        }
        else {
            Preserve.insert(GV);
        }
    }

    for (auto &F : M.functions()) {
        if (F.isDeclaration())
            continue;
        if (F.hasLocalLinkage())
            continue;
        if (Preserve.contains(&F))
            continue;
        if (!canPartition(F)) {
            F.setLinkage(GlobalValue::AvailableExternallyLinkage);
            F.setVisibility(GlobalValue::HiddenVisibility);
            F.setDSOLocal(true);
            continue;
        }
        F.deleteBody();
        F.setLinkage(GlobalValue::ExternalLinkage);
        F.setVisibility(GlobalValue::HiddenVisibility);
        F.setDSOLocal(true);
    }

    for (auto &GV : M.globals()) {
        if (GV.isDeclaration())
            continue;
        if (Preserve.contains(&GV))
            continue;
        if (GV.hasLocalLinkage())
            continue;
        GV.setInitializer(nullptr);
        GV.setLinkage(GlobalValue::ExternalLinkage);
        GV.setVisibility(GlobalValue::HiddenVisibility);
        if (GV.getDLLStorageClass() != GlobalValue::DLLStorageClassTypes::DefaultStorageClass)
            continue; // Don't mess with exported or imported globals
        GV.setDSOLocal(true);
    }

    // Global aliases are a pain to deal with. It is illegal to have an alias to a declaration,
    // so we need to replace them with either a function or a global variable declaration. However,
    // we can't just delete the alias, because that would break the users of the alias. Therefore,
    // we do a dance where we point each global alias to a dummy function or global variable,
    // then materialize the module to access use-lists, then replace all the uses, and finally commit
    // to deleting the old alias.
    SmallVector<std::pair<GlobalAlias *, GlobalValue *>> DeletedAliases;
    for (auto &GA : M.aliases()) {
        assert(!GA.isDeclaration() && "Global aliases can't be declarations!"); // because LLVM says so
        if (Preserve.contains(&GA))
            continue;
        if (GA.hasLocalLinkage())
            continue;
        if (GA.getValueType()->isFunctionTy()) {
            auto F = Function::Create(cast<FunctionType>(GA.getValueType()), GlobalValue::ExternalLinkage, "", &M);
            // This is an extremely sad hack to make sure the global alias never points to an extern function
            auto BB = BasicBlock::Create(M.getContext(), "", F);
            new UnreachableInst(M.getContext(), BB);
            GA.setAliasee(F);
            DeletedAliases.push_back({ &GA, F });
        }
        else {
            auto GV = new GlobalVariable(M, GA.getValueType(), false, GlobalValue::ExternalLinkage, Constant::getNullValue(GA.getValueType()));
            DeletedAliases.push_back({ &GA, GV });
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
static void construct_vars(Module &M, Partition &partition, StringRef suffix) {
    SmallVector<std::pair<uint32_t, GlobalValue *>> fvar_pairs;
    fvar_pairs.reserve(partition.fvars.size());
    for (auto &fvar : partition.fvars) {
        auto F = M.getFunction(fvar.first());
        assert(F);
        assert(!F->isDeclaration());
        fvar_pairs.push_back({ fvar.second, F });
    }
    SmallVector<GlobalValue *, 0> fvars;
    SmallVector<uint32_t, 0> fvar_idxs;
    fvars.reserve(fvar_pairs.size());
    fvar_idxs.reserve(fvar_pairs.size());
    std::sort(fvar_pairs.begin(), fvar_pairs.end());
    for (auto &fvar : fvar_pairs) {
        fvars.push_back(fvar.second);
        fvar_idxs.push_back(fvar.first);
    }
    SmallVector<std::pair<uint32_t, GlobalValue *>, 0> gvar_pairs;
    gvar_pairs.reserve(partition.gvars.size());
    for (auto &gvar : partition.gvars) {
        auto GV = M.getNamedGlobal(gvar.first());
        assert(GV);
        assert(!GV->isDeclaration());
        gvar_pairs.push_back({ gvar.second, GV });
    }
    SmallVector<Constant*, 0> gvars;
    SmallVector<uint32_t, 0> gvar_idxs;
    gvars.reserve(gvar_pairs.size());
    gvar_idxs.reserve(gvar_pairs.size());
    std::sort(gvar_pairs.begin(), gvar_pairs.end());
    for (auto &gvar : gvar_pairs) {
        gvars.push_back(gvar.second);
        gvar_idxs.push_back(gvar.first);
    }

    // Now commit the fvars, gvars, and idxs
    auto T_size = M.getDataLayout().getIntPtrType(M.getContext());
    emit_table(M, fvars, "jl_fvars", T_size->getPointerTo());
    emit_offset_table(M, T_size, gvars, "jl_gvar", suffix);
    auto fidxs = ConstantDataArray::get(M.getContext(), fvar_idxs);
    auto fidxs_var = new GlobalVariable(M, fidxs->getType(), true,
                                        GlobalVariable::ExternalLinkage,
                                        fidxs, "jl_fvar_idxs");
    fidxs_var->setVisibility(GlobalValue::HiddenVisibility);
    fidxs_var->setDSOLocal(true);
    auto gidxs = ConstantDataArray::get(M.getContext(), gvar_idxs);
    auto gidxs_var = new GlobalVariable(M, gidxs->getType(), true,
                                        GlobalVariable::ExternalLinkage,
                                        gidxs, "jl_gvar_idxs" + suffix);
    gidxs_var->setVisibility(GlobalValue::HiddenVisibility);
    gidxs_var->setDSOLocal(true);
}

extern "C" void lambda_trampoline(void* arg) {
    std::function<void()>* func = static_cast<std::function<void()>*>(arg);
    (*func)();
    delete func;
}

// Entrypoint to optionally-multithreaded image compilation. This handles global coordination of the threading,
// as well as partitioning, serialization, and deserialization.
template<typename ModuleReleasedFunc>
static SmallVector<AOTOutputs, 16> add_output(Module &M, TargetMachine &TM, StringRef name, unsigned threads,
                bool unopt_out, bool opt_out, bool obj_out, bool asm_out, ModuleReleasedFunc module_released) {
    SmallVector<AOTOutputs, 16> outputs(threads);
    assert(threads);
    assert(unopt_out || opt_out || obj_out || asm_out);
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
        {
            JL_TIMING(NATIVE_AOT, NATIVE_Opt);
            // convert gvars to the expected offset table format for shard 0
            if (M.getGlobalVariable("jl_gvars")) {
                auto gvars = consume_gv<Constant>(M, "jl_gvars", false);
                Type *T_size = M.getDataLayout().getIntPtrType(M.getContext());
                emit_offset_table(M, T_size, gvars, "jl_gvar", "_0"); // module flag "julia.mv.suffix"
                M.getGlobalVariable("jl_gvar_idxs")->setName("jl_gvar_idxs_0");
            }
            outputs[0] = add_output_impl(M, TM, timers[0], unopt_out, opt_out, obj_out, asm_out);
        }
        output_timer.stopTimer();
        // Don't need M anymore
        module_released(M);

        if (!report_timings) {
            timer_group.clear();
        } else {
            timer_group.print(dbgs(), true);
            for (auto &t : timers) {
                t.print(dbgs(), true);
            }
        }
        return outputs;
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

    // Don't need M anymore, since we'll only read from serialized from now on
    module_released(M);

    output_timer.startTimer();

    // Start all of the worker threads
    {
        JL_TIMING(NATIVE_AOT, NATIVE_Opt);
        std::vector<uv_thread_t> workers(threads);
        for (unsigned i = 0; i < threads; i++) {
            std::function<void()> func = [&, i]() {
                LLVMContext ctx;
                ctx.setDiscardValueNames(true);
                // Lazily deserialize the entire module
                timers[i].deserialize.startTimer();
                auto EM = getLazyBitcodeModule(MemoryBufferRef(StringRef(serialized.data(), serialized.size()), "Optimized"), ctx);
                // Make sure this also fails with only julia, but not LLVM assertions enabled,
                // otherwise, the first error we hit is the LLVM module verification failure,
                // which will look very confusing, because the module was partially deserialized.
                bool deser_succeeded = (bool)EM;
                auto M = cantFail(std::move(EM), "Error loading module");
                assert(deser_succeeded); (void)deser_succeeded;
                timers[i].deserialize.stopTimer();

                timers[i].materialize.startTimer();
                materializePreserved(*M, partitions[i]);
                timers[i].materialize.stopTimer();

                timers[i].construct.startTimer();
                std::string suffix = "_" + std::to_string(i);
                construct_vars(*M, partitions[i], suffix);
                M->setModuleFlag(Module::Error, "julia.mv.suffix", MDString::get(M->getContext(), suffix));
                // The DICompileUnit file is not used for anything, but ld64 requires it be a unique string per object file
                // or it may skip emitting debug info for that file. Here set it to ./julia#N
                DIFile *topfile = DIFile::get(M->getContext(), "julia#" + std::to_string(i), ".");
                for (DICompileUnit *CU : M->debug_compile_units())
                    CU->replaceOperandWith(0, topfile);
                timers[i].construct.stopTimer();

                outputs[i] = add_output_impl(*M, TM, timers[i], unopt_out, opt_out, obj_out, asm_out);
            };
            auto arg = new std::function<void()>(func);
            uv_thread_create(&workers[i], lambda_trampoline, arg); // Use libuv thread to avoid issues with stack sizes
        }

        // Wait for all of the worker threads to finish
        for (unsigned i = 0; i < threads; i++)
            uv_thread_join(&workers[i]);
    }

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
    return outputs;
}

extern int jl_is_timing_passes;
static unsigned compute_image_thread_count(const ModuleInfo &info) {
    // 32-bit systems are very memory-constrained
#ifdef _P32
    LLVM_DEBUG(dbgs() << "32-bit systems are restricted to a single thread\n");
    return 1;
#endif
    if (jl_is_timing_passes) // LLVM isn't thread safe when timing the passes https://github.com/llvm/llvm-project/issues/44417
        return 1;
    // COFF has limits on external symbols (even hidden) up to 65536. We reserve the last few
    // for any of our other symbols that we insert during compilation.
    if (info.triple.isOSBinFormatCOFF() && info.globals > 64000) {
        LLVM_DEBUG(dbgs() << "COFF is restricted to a single thread for large images\n");
        return 1;
    }
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

jl_emission_params_t default_emission_params = { 1 };

// takes the running content that has collected in the shadow module and dump it to disk
// this builds the object file portion of the sysimage files for fast startup
extern "C" JL_DLLEXPORT_CODEGEN
void jl_dump_native_impl(void *native_code,
        const char *bc_fname, const char *unopt_bc_fname, const char *obj_fname,
        const char *asm_fname,
        ios_t *z, ios_t *s,
        jl_emission_params_t *params)
{
    JL_TIMING(NATIVE_AOT, NATIVE_Dump);
    jl_native_code_desc_t *data = (jl_native_code_desc_t*)native_code;
    if (!bc_fname && !unopt_bc_fname && !obj_fname && !asm_fname) {
        LLVM_DEBUG(dbgs() << "No output requested, skipping native code dump?\n");
        delete data;
        return;
    }

    if (!params) {
        params = &default_emission_params;
    }

    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    // make sure to emit the native object format, even if FORCE_ELF was set in codegen
    Triple TheTriple(data->M.withModuleDo([](Module &M) { return M.getTargetTriple(); }));
    if (TheTriple.isOSWindows()) {
        TheTriple.setObjectFormat(Triple::COFF);
    } else if (TheTriple.isOSDarwin()) {
        TheTriple.setObjectFormat(Triple::MachO);
        SmallString<16> Str;
        Str += "macosx";
        if (TheTriple.isAArch64())
            Str += "11.0.0"; // Update this if MACOSX_VERSION_MIN changes
        else
            Str += "10.14.0";
        TheTriple.setOSName(Str);
    }
    std::optional<Reloc::Model> RelocModel;
    if (TheTriple.isOSLinux() || TheTriple.isOSFreeBSD() || TheTriple.isOSOpenBSD()) {
        RelocModel = Reloc::PIC_;
    }

    CodeModel::Model CMModel = CodeModel::Small;
    if (TheTriple.isPPC() || TheTriple.isRISCV() ||
        (TheTriple.isX86() && TheTriple.isArch64Bit() && TheTriple.isOSLinux())) {
        // On PPC the small model is limited to 16bit offsets. For very large images the small code model
        CMModel = CodeModel::Medium; //  isn't good enough on x86 so use Medium, it has no cost because only the image goes in .ldata
    }
    std::unique_ptr<TargetMachine> SourceTM(
        jl_ExecutionEngine->getTarget().createTargetMachine(
            TheTriple.getTriple(),
            jl_ExecutionEngine->getTargetCPU(),
            jl_ExecutionEngine->getTargetFeatureString(),
            jl_ExecutionEngine->getTargetOptions(),
            RelocModel,
            CMModel,
#if JL_LLVM_VERSION >= 180000
            CodeGenOptLevel::Aggressive // -O3 TODO: respect command -O0 flag?
#else
            CodeGenOpt::Aggressive // -O3 TODO: respect command -O0 flag?
#endif
            ));
    fixupTM(*SourceTM);
    auto DL = jl_create_datalayout(*SourceTM);
    std::string StackProtectorGuard;
    unsigned OverrideStackAlignment;
    data->M.withModuleDo([&](Module &M) {
        StackProtectorGuard = M.getStackProtectorGuard().str();
        OverrideStackAlignment = M.getOverrideStackAlignment();
    });

    auto compile = [&](Module &M, StringRef name, unsigned threads, auto module_released) {
        return add_output(M, *SourceTM, name, threads, !!unopt_bc_fname, !!bc_fname, !!obj_fname, !!asm_fname, module_released);
    };

    SmallVector<AOTOutputs, 16> sysimg_outputs;
    SmallVector<AOTOutputs, 16> data_outputs;
    SmallVector<AOTOutputs, 16> metadata_outputs;
    if (z) {
        JL_TIMING(NATIVE_AOT, NATIVE_Sysimg);
        LLVMContext Context;
        Context.setDiscardValueNames(true);
        Module sysimgM("sysimg", Context);
        sysimgM.setTargetTriple(TheTriple.str());
        sysimgM.setDataLayout(DL);
        sysimgM.setStackProtectorGuard(StackProtectorGuard);
        sysimgM.setOverrideStackAlignment(OverrideStackAlignment);
        Constant *data = ConstantDataArray::get(Context,
            ArrayRef<uint8_t>((const unsigned char*)z->buf, z->size));
        auto sysdata = new GlobalVariable(sysimgM, data->getType(), false,
                                     GlobalVariable::ExternalLinkage,
                                     data, "jl_system_image_data");
        sysdata->setAlignment(Align(64));
#if JL_LLVM_VERSION >= 180000
        sysdata->setCodeModel(CodeModel::Large);
#else
        if (TheTriple.isX86() && TheTriple.isArch64Bit() && TheTriple.isOSLinux())
            sysdata->setSection(".ldata");
#endif
        addComdat(sysdata, TheTriple);
        Constant *len = ConstantInt::get(sysimgM.getDataLayout().getIntPtrType(Context), z->size);
        addComdat(new GlobalVariable(sysimgM, len->getType(), true,
                                     GlobalVariable::ExternalLinkage,
                                     len, "jl_system_image_size"), TheTriple);
        // Free z here, since we've copied out everything into data
        // Results in serious memory savings
        ios_close(z);
        free(z);
        // Note that we don't set z to null, this allows the check in WRITE_ARCHIVE
        // to function as expected
        // no need to free the module/context, destructor handles that
        sysimg_outputs = compile(sysimgM, "sysimg", 1, [](Module &) {});
    }

    const bool imaging_mode = true;
    unsigned threads = 1;
    unsigned nfvars = 0;
    unsigned ngvars = 0;

    // Reset the target triple to make sure it matches the new target machine

    bool has_veccall = false;

    data->M.withModuleDo([&](Module &dataM) {
        JL_TIMING(NATIVE_AOT, NATIVE_Setup);
        dataM.setTargetTriple(TheTriple.str());
        dataM.setDataLayout(DL);
        dataM.setPICLevel(PICLevel::BigPIC);
        auto &Context = dataM.getContext();

        Type *T_psize = dataM.getDataLayout().getIntPtrType(Context)->getPointerTo();

        // This should really be in jl_create_native, but we haven't
        // yet set the target triple binary format correctly at that
        // point. This should be resolved when we start JITting for
        // COFF when we switch over to JITLink.
        for (auto &GA : dataM.aliases()) {
            // Global aliases are only used for ccallable things, so we should
            // mark them as dllexport
            addComdat(&GA, TheTriple);
        }

        // add metadata information
        if (imaging_mode) {
            multiversioning_preannotate(dataM);
            {
                DenseSet<GlobalValue *> fvars(data->jl_sysimg_fvars.begin(), data->jl_sysimg_fvars.end());
                for (auto &F : dataM) {
                    if (F.hasFnAttribute("julia.mv.reloc") || F.hasFnAttribute("julia.mv.fvar")) {
                        if (fvars.insert(&F).second) {
                            data->jl_sysimg_fvars.push_back(&F);
                        }
                    }
                }
            }

            ModuleInfo module_info = compute_module_info(dataM);
            LLVM_DEBUG(dbgs()
                << "Dumping module with stats:\n"
                << "    globals: " << module_info.globals << "\n"
                << "    functions: " << module_info.funcs << "\n"
                << "    basic blocks: " << module_info.bbs << "\n"
                << "    instructions: " << module_info.insts << "\n"
                << "    clones: " << module_info.clones << "\n"
                << "    weight: " << module_info.weight << "\n"
            );
            threads = compute_image_thread_count(module_info);
            LLVM_DEBUG(dbgs() << "Using " << threads << " to emit aot image\n");
            nfvars = data->jl_sysimg_fvars.size();
            ngvars = data->jl_sysimg_gvars.size();
            emit_table(dataM, data->jl_sysimg_gvars, "jl_gvars", T_psize);
            emit_table(dataM, data->jl_sysimg_fvars, "jl_fvars", T_psize);
            SmallVector<uint32_t, 0> idxs;
            idxs.resize(data->jl_sysimg_gvars.size());
            std::iota(idxs.begin(), idxs.end(), 0);
            auto gidxs = ConstantDataArray::get(Context, idxs);
            auto gidxs_var = new GlobalVariable(dataM, gidxs->getType(), true,
                                                GlobalVariable::ExternalLinkage,
                                                gidxs, "jl_gvar_idxs");
            gidxs_var->setVisibility(GlobalValue::HiddenVisibility);
            gidxs_var->setDSOLocal(true);
            idxs.clear();
            idxs.resize(data->jl_sysimg_fvars.size());
            std::iota(idxs.begin(), idxs.end(), 0);
            auto fidxs = ConstantDataArray::get(Context, idxs);
            auto fidxs_var = new GlobalVariable(dataM, fidxs->getType(), true,
                                                GlobalVariable::ExternalLinkage,
                                                fidxs, "jl_fvar_idxs");
            fidxs_var->setVisibility(GlobalValue::HiddenVisibility);
            fidxs_var->setDSOLocal(true);
            dataM.addModuleFlag(Module::Error, "julia.mv.suffix", MDString::get(Context, "_0"));

            // let the compiler know we are going to internalize a copy of this,
            // if it has a current usage with ExternalLinkage
            auto jl_small_typeof_copy = dataM.getGlobalVariable("jl_small_typeof");
            if (jl_small_typeof_copy) {
                jl_small_typeof_copy->setVisibility(GlobalValue::HiddenVisibility);
                jl_small_typeof_copy->setDSOLocal(true);
                jl_small_typeof_copy->setDLLStorageClass(GlobalValue::DLLStorageClassTypes::DefaultStorageClass);
            }
        }

        has_veccall = !!dataM.getModuleFlag("julia.mv.veccall");
    });

    {
        // Don't use withModuleDo here since we delete the TSM midway through
        auto TSCtx = data->M.getContext();
        auto lock = TSCtx.getLock();
        auto dataM = data->M.getModuleUnlocked();

        // Delete data when add_output thinks it's done with it
        // Saves memory for use when multithreading
        data_outputs = compile(*dataM, "text", threads, [data](Module &) { delete data; });
    }

    if (params->emit_metadata) {
        JL_TIMING(NATIVE_AOT, NATIVE_Metadata);
        LLVMContext Context;
        Context.setDiscardValueNames(true);
        Module metadataM("metadata", Context);
        metadataM.setTargetTriple(TheTriple.str());
        metadataM.setDataLayout(DL);
        metadataM.setStackProtectorGuard(StackProtectorGuard);
        metadataM.setOverrideStackAlignment(OverrideStackAlignment);

        // reflect the address of the jl_RTLD_DEFAULT_handle variable
        // back to the caller, so that we can check for consistency issues
        GlobalValue *jlRTLD_DEFAULT_var = jl_emit_RTLD_DEFAULT_var(&metadataM);

        Type *T_size = DL.getIntPtrType(Context);
        Type *T_psize = T_size->getPointerTo();

        auto FT = FunctionType::get(Type::getInt8Ty(Context)->getPointerTo()->getPointerTo(), {}, false);
        auto F = Function::Create(FT, Function::ExternalLinkage, "get_jl_RTLD_DEFAULT_handle_addr", metadataM);
        llvm::IRBuilder<> builder(BasicBlock::Create(Context, "top", F));
        builder.CreateRet(jlRTLD_DEFAULT_var);
        F->setLinkage(GlobalValue::ExternalLinkage);
        if (TheTriple.isOSBinFormatCOFF())
            F->setDLLStorageClass(GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);

        if (TheTriple.isOSWindows()) {
            // Windows expect that the function `_DllMainStartup` is present in an dll.
            // Normal compilers use something like Zig's crtdll.c instead we provide a
            // a stub implementation.
            auto T_pvoid = Type::getInt8Ty(Context)->getPointerTo();
            auto T_int32 = Type::getInt32Ty(Context);
            auto FT = FunctionType::get(T_int32, {T_pvoid, T_int32, T_pvoid}, false);
            auto F = Function::Create(FT, Function::ExternalLinkage, "_DllMainCRTStartup", metadataM);
            F->setCallingConv(CallingConv::X86_StdCall);

            llvm::IRBuilder<> builder(BasicBlock::Create(Context, "top", F));
            builder.CreateRet(ConstantInt::get(T_int32, 1));
        }
        if (imaging_mode) {
            auto specs = jl_get_llvm_clone_targets();
            const uint32_t base_flags = has_veccall ? JL_TARGET_VEC_CALL : 0;
            SmallVector<uint8_t, 0> data;
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
            auto target_ids = new GlobalVariable(metadataM, value->getType(), true,
                                        GlobalVariable::InternalLinkage,
                                        value, "jl_dispatch_target_ids");
            auto shards = emit_shard_table(metadataM, T_size, T_psize, threads);
            auto ptls = emit_ptls_table(metadataM, T_size, T_psize);
            auto header = emit_image_header(metadataM, threads, nfvars, ngvars);
            auto AT = ArrayType::get(T_size, sizeof(jl_small_typeof) / sizeof(void*));
            auto jl_small_typeof_copy = new GlobalVariable(metadataM, AT, false,
                                                        GlobalVariable::ExternalLinkage,
                                                        Constant::getNullValue(AT),
                                                        "jl_small_typeof");
            jl_small_typeof_copy->setVisibility(GlobalValue::HiddenVisibility);
            jl_small_typeof_copy->setDSOLocal(true);
            AT = ArrayType::get(T_psize, 5);
            auto pointers = new GlobalVariable(metadataM, AT, false,
                                            GlobalVariable::ExternalLinkage,
                                            ConstantArray::get(AT, {
                                                    ConstantExpr::getBitCast(header, T_psize),
                                                    ConstantExpr::getBitCast(shards, T_psize),
                                                    ConstantExpr::getBitCast(ptls, T_psize),
                                                    ConstantExpr::getBitCast(jl_small_typeof_copy, T_psize),
                                                    ConstantExpr::getBitCast(target_ids, T_psize)
                                            }),
                                            "jl_image_pointers");
            addComdat(pointers, TheTriple);
            if (s) {
                write_int32(s, data.size());
                ios_write(s, (const char *)data.data(), data.size());
            }
        }

        // no need to free module/context, destructor handles that
        metadata_outputs = compile(metadataM, "data", 1, [](Module &) {});
    }

    {
        JL_TIMING(NATIVE_AOT, NATIVE_Write);

        object::Archive::Kind Kind = getDefaultForHost(TheTriple);
#if JL_LLVM_VERSION >= 180000
#define WritingMode SymtabWritingMode::NormalSymtab
#else
#define WritingMode true
#endif
#define WRITE_ARCHIVE(fname, field, prefix, suffix) \
    if (fname) {\
        SmallVector<NewArchiveMember, 0> archive; \
        SmallVector<std::string, 16> filenames; \
        SmallVector<StringRef, 16> buffers; \
        for (size_t i = 0; i < threads; i++) { \
            filenames.push_back((StringRef("text") + prefix + "#" + Twine(i) + suffix).str()); \
            buffers.push_back(StringRef(data_outputs[i].field.data(), data_outputs[i].field.size())); \
        } \
        filenames.push_back("metadata" prefix suffix); \
        buffers.push_back(StringRef(metadata_outputs[0].field.data(), metadata_outputs[0].field.size())); \
        if (z) { \
            filenames.push_back("sysimg" prefix suffix); \
            buffers.push_back(StringRef(sysimg_outputs[0].field.data(), sysimg_outputs[0].field.size())); \
        } \
        for (size_t i = 0; i < filenames.size(); i++) { \
            archive.push_back(NewArchiveMember(MemoryBufferRef(buffers[i], filenames[i]))); \
        } \
        handleAllErrors(writeArchive(fname, archive, WritingMode, Kind, true, false), reportWriterError); \
    }

        WRITE_ARCHIVE(unopt_bc_fname, unopt, "_unopt", ".bc");
        WRITE_ARCHIVE(bc_fname, opt, "_opt", ".bc");
        WRITE_ARCHIVE(obj_fname, obj, "", ".o");
        WRITE_ARCHIVE(asm_fname, asm_, "", ".s");
#undef WRITE_ARCHIVE
    }
}


// sometimes in GDB you want to find out what code would be created from a mi
extern "C" JL_DLLEXPORT_CODEGEN jl_code_info_t *jl_gdbdumpcode(jl_method_instance_t *mi)
{
    jl_llvmf_dump_t llvmf_dump;
    size_t world = jl_current_task->world_age;
    JL_STREAM *stream = (JL_STREAM*)STDERR_FILENO;

    jl_code_info_t *src = jl_gdbcodetyped1(mi, world);
    JL_GC_PUSH1(&src);

    jl_printf(stream, "---- dumping IR for ----\n");
    jl_static_show(stream, (jl_value_t*)mi);
    jl_printf(stream, "\n----\n");

    jl_printf(stream, "\n---- unoptimized IR ----\n");
    jl_get_llvmf_defn(&llvmf_dump, mi, src, 0, false, jl_default_cgparams);
    if (llvmf_dump.F) {
        jl_value_t *ir = jl_dump_function_ir(&llvmf_dump, 0, 1, "source");
        if (ir != NULL && jl_is_string(ir))
            jl_printf(stream, "%s", jl_string_data(ir));
    }
    jl_printf(stream, "\n----\n");

    jl_printf(stream, "\n---- optimized IR ----\n");
    jl_get_llvmf_defn(&llvmf_dump, mi, src, 0, true, jl_default_cgparams);
    if (llvmf_dump.F) {
        jl_value_t *ir = jl_dump_function_ir(&llvmf_dump, 0, 1, "source");
        if (ir != NULL && jl_is_string(ir))
            jl_printf(stream, "%s", jl_string_data(ir));
    }
    jl_printf(stream, "\n----\n");

    jl_printf(stream, "\n---- assembly ----\n");
    jl_get_llvmf_defn(&llvmf_dump, mi, src, 0, true, jl_default_cgparams);
    if (llvmf_dump.F) {
        jl_value_t *ir = jl_dump_function_asm(&llvmf_dump, 0, "", "source", 0, true);
        if (ir != NULL && jl_is_string(ir))
            jl_printf(stream, "%s", jl_string_data(ir));
    }
    jl_printf(stream, "\n----\n");
    JL_GC_POP();

    return src;
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// This is paired with jl_dump_function_ir and jl_dump_function_asm, either of which will free all memory allocated here
extern "C" JL_DLLEXPORT_CODEGEN
void jl_get_llvmf_defn_impl(jl_llvmf_dump_t *dump, jl_method_instance_t *mi, jl_code_info_t *src, char getwrapper, char optimize, const jl_cgparams_t params)
{
    // emit this function into a new llvm module
    dump->F = nullptr;
    dump->TSM = nullptr;
    if (src && jl_is_code_info(src)) {
        auto ctx = jl_ExecutionEngine->makeContext();
        const auto &DL = jl_ExecutionEngine->getDataLayout();
        const auto &TT = jl_ExecutionEngine->getTargetTriple();
        orc::ThreadSafeModule m = jl_create_ts_module(name_from_method_instance(mi), ctx, DL, TT);
        Function *F = nullptr;
        {
            uint64_t compiler_start_time = 0;
            uint8_t measure_compile_time_enabled = jl_atomic_load_relaxed(&jl_measure_compile_time_enabled);
            if (measure_compile_time_enabled)
                compiler_start_time = jl_hrtime();
            jl_codegen_params_t output(ctx, DL, TT);
            output.params = &params;
            output.imaging_mode = jl_options.image_codegen;
            output.temporary_roots = jl_alloc_array_1d(jl_array_any_type, 0);
            JL_GC_PUSH1(&output.temporary_roots);
            jl_llvm_functions_t decls = jl_emit_code(m, mi, src, mi->specTypes, src->rettype, output);
            // while not required, also emit the cfunc thunks, based on the
            // inferred ABIs of their targets in the current latest world,
            // since otherwise it is challenging to see all relevant codes
            jl_compiled_functions_t compiled_functions;
            size_t latestworld = jl_atomic_load_acquire(&jl_world_counter);
            for (cfunc_decl_t &cfunc : output.cfuncs) {
                jl_value_t *sigt = cfunc.sigt;
                JL_GC_PROMISE_ROOTED(sigt);
                jl_method_instance_t *mi = jl_get_specialization1((jl_tupletype_t*)sigt, latestworld, 0);
                if (mi == nullptr)
                    continue;
                jl_code_instance_t *codeinst = jl_type_infer(mi, latestworld, SOURCE_MODE_NOT_REQUIRED);
                if (codeinst == nullptr || compiled_functions.count(codeinst))
                    continue;
                orc::ThreadSafeModule decl_m = jl_create_ts_module("extern", ctx, DL, TT);
                jl_llvm_functions_t decls;
                if (jl_atomic_load_relaxed(&codeinst->invoke) == jl_fptr_const_return_addr)
                    decls.functionObject = "jl_fptr_const_return";
                else
                    decls = jl_emit_codedecls(decl_m, codeinst, output);
                compiled_functions[codeinst] = {std::move(decl_m), std::move(decls)};
            }
            generate_cfunc_thunks(output, compiled_functions);
            emit_always_inline(m, output);
            output.workqueue.clear();
            compiled_functions.clear();
            output.temporary_roots = nullptr;
            JL_GC_POP(); // GC the global_targets array contents now since reflection doesn't need it

            if (m) {
                // if compilation succeeded, prepare to return the result
                // Similar to jl_link_global from jitlayers.cpp,
                // so that code_llvm shows similar codegen to the jit
                for (auto &global : output.global_targets) {
                    if (jl_options.image_codegen) {
                        global.second->setLinkage(GlobalValue::ExternalLinkage);
                    }
                    else {
                        auto p = literal_static_pointer_val(global.first, global.second->getValueType());
                        Type *elty = PointerType::get(p->getContext(), 0);
                        // For pretty printing, when LLVM inlines the global initializer into its loads
                        auto alias = GlobalAlias::create(elty, 0, GlobalValue::PrivateLinkage, global.second->getName() + ".jit", p, global.second->getParent());
                        global.second->setInitializer(ConstantExpr::getBitCast(alias, global.second->getValueType()));
                        global.second->setConstant(true);
                        global.second->setLinkage(GlobalValue::PrivateLinkage);
                        global.second->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
                        global.second->setVisibility(GlobalValue::DefaultVisibility);
                    }
                }
                if (!jl_options.image_codegen) {
                    optimizeDLSyms(*m.getModuleUnlocked());
                }
                assert(!verifyLLVMIR(*m.getModuleUnlocked()));
                if (optimize) {
                    NewPM PM{jl_ExecutionEngine->cloneTargetMachine(), getOptLevel(jl_options.opt_level)};
                    //Safe b/c context lock is held by output
                    PM.run(*m.getModuleUnlocked());
                    assert(!verifyLLVMIR(*m.getModuleUnlocked()));
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
            if (measure_compile_time_enabled) {
                auto end = jl_hrtime();
                jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, end - compiler_start_time);
            }
        }
        if (F) {
            dump->TSM = wrap(new orc::ThreadSafeModule(std::move(m)));
            dump->F = wrap(F);
            return;
        }
    }
}
