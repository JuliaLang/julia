// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include "llvm/IR/LegacyPassManager.h"

#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/JITEventListener.h>

#include <llvm/Target/TargetMachine.h>
#include "julia_assert.h"

using namespace llvm;

extern TargetMachine *jl_TargetMachine;
extern bool imaging_mode;

void addTargetPasses(legacy::PassManagerBase *PM, TargetMachine *TM);
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level, bool lower_intrinsics=true, bool dump_native=false);
void addMachinePasses(legacy::PassManagerBase *PM, TargetMachine *TM);
void jl_finalize_module(std::unique_ptr<Module>  m);
void jl_merge_module(Module *dest, std::unique_ptr<Module> src);
Module *jl_create_llvm_module(StringRef name);
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M);

typedef struct _jl_llvm_functions_t {
    std::string functionObject;     // jlcall llvm Function name
    std::string specFunctionObject; // specialized llvm Function name
} jl_llvm_functions_t;

struct jl_returninfo_t {
    llvm::Function *decl;
    enum CallingConv {
        Boxed = 0,
        Register,
        SRet,
        Union,
        Ghosts
    } cc;
    size_t union_bytes;
    size_t union_align;
    size_t union_minalign;
    unsigned return_roots;
};

typedef std::vector<std::tuple<jl_code_instance_t*, jl_returninfo_t::CallingConv, unsigned, llvm::Function*, bool>> jl_codegen_call_targets_t;
typedef std::tuple<std::unique_ptr<Module>, jl_llvm_functions_t> jl_compile_result_t;

typedef struct {
    typedef StringMap<GlobalVariable*> SymMapGV;
    // outputs
    jl_codegen_call_targets_t workqueue;
    std::map<void*, GlobalVariable*> globals;
    std::map<jl_datatype_t*, DIType*> ditypes;
    std::map<jl_datatype_t*, Type*> llvmtypes;
    DenseMap<Constant*, GlobalVariable*> mergedConstants;
    // Map from symbol name (in a certain library) to its GV in sysimg and the
    // DL handle address in the current session.
    StringMap<std::pair<GlobalVariable*,SymMapGV>> libMapGV;
#ifdef _OS_WINDOWS_
    SymMapGV symMapExe;
    SymMapGV symMapDl;
#endif
    SymMapGV symMapDefault;
    // Map from distinct callee's to its GOT entry.
    // In principle the attribute, function type and calling convention
    // don't need to be part of the key but it seems impossible to forward
    // all the arguments without writing assembly directly.
    // This doesn't matter too much in reality since a single function is usually
    // not called with multiple signatures.
    DenseMap<AttributeList, std::map<
        std::tuple<GlobalVariable*, FunctionType*, CallingConv::ID>,
        GlobalVariable*>> allPltMap;
    Module *_shared_module = NULL;
    Module *shared_module(LLVMContext &context) {
        if (!_shared_module)
            _shared_module = jl_create_llvm_module("globals");
        return _shared_module;
    }
    // inputs
    size_t world = 0;
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool cache = false;
} jl_codegen_params_t;

jl_compile_result_t jl_emit_code(
        jl_method_instance_t *mi,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params);

jl_compile_result_t jl_emit_codeinst(
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params);

enum CompilationPolicy {
    Default = 0,
    Extern = 1,
    ImagingMode = 2
};

void jl_compile_workqueue(
    std::map<jl_code_instance_t*, jl_compile_result_t> &emitted,
    jl_codegen_params_t &params,
    CompilationPolicy policy);

Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt,
    jl_codegen_params_t &params);

void add_named_global(StringRef name, void *addr);

static inline Constant *literal_static_pointer_val(const void *p, Type *T)
{
    // this function will emit a static pointer into the generated code
    // the generated code will only be valid during the current session,
    // and thus, this should typically be avoided in new API's
#if defined(_P64)
    return ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt64Ty(T->getContext()), (uint64_t)p), T);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(Type::getInt32Ty(T->getContext()), (uint32_t)p), T);
#endif
}

static const inline char *name_from_method_instance(jl_method_instance_t *li)
{
    return jl_is_method(li->def.method) ? jl_symbol_name(li->def.method->name) : "top-level scope";
}


void jl_init_jit(void);

typedef JITSymbol JL_JITSymbol;
// The type that is similar to SymbolInfo on LLVM 4.0 is actually
// `JITEvaluatedSymbol`. However, we only use this type when a JITSymbol
// is expected.
typedef JITSymbol JL_SymbolInfo;

#if JL_LLVM_VERSION < 120000
using RTDyldObjHandleT = orc::VModuleKey;
#endif

#if JL_LLVM_VERSION >= 100000
using CompilerResultT = Expected<std::unique_ptr<llvm::MemoryBuffer>>;
#else
using CompilerResultT = std::unique_ptr<llvm::MemoryBuffer>;
#endif

class JuliaOJIT {
    struct CompilerT : public orc::IRCompileLayer::IRCompiler {
        CompilerT(JuliaOJIT *pjit)
            : IRCompiler(orc::IRSymbolMapper::ManglingOptions{}),
              jit(*pjit) {}
        virtual CompilerResultT operator()(Module &M) override;
    private:
        JuliaOJIT &jit;
    };
#if JL_LLVM_VERSION >= 120000
    // Custom object emission notification handler for the JuliaOJIT
    template <typename ObjT, typename LoadResult>
    void registerObject(const ObjT &Obj, const LoadResult &LO);
#else
    // Custom object emission notification handler for the JuliaOJIT
    template <typename ObjT, typename LoadResult>
    void registerObject(RTDyldObjHandleT H, const ObjT &Obj, const LoadResult &LO);
#endif

public:
    typedef orc::RTDyldObjectLinkingLayer ObjLayerT;
    typedef orc::IRCompileLayer CompileLayerT;
#if JL_LLVM_VERSION < 120000
    typedef RTDyldObjHandleT ModuleHandleT;
#endif
    typedef object::OwningBinary<object::ObjectFile> OwningObj;

    JuliaOJIT(TargetMachine &TM, LLVMContext *Ctx);

    void RegisterJITEventListener(JITEventListener *L);
#if JL_LLVM_VERSION < 120000
    std::vector<JITEventListener *> EventListeners;
    void NotifyFinalizer(RTDyldObjHandleT Key,
                         const object::ObjectFile &Obj,
                         const RuntimeDyld::LoadedObjectInfo &LoadedObjectInfo);
#endif
    void addGlobalMapping(StringRef Name, uint64_t Addr);
    void addModule(std::unique_ptr<Module> M);
#if JL_LLVM_VERSION < 120000
    void removeModule(ModuleHandleT H);
#endif
    JL_JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly);
    JL_JITSymbol findUnmangledSymbol(StringRef Name);
    uint64_t getGlobalValueAddress(StringRef Name);
    uint64_t getFunctionAddress(StringRef Name);
    StringRef getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst);
    const DataLayout& getDataLayout() const;
    const Triple& getTargetTriple() const;
    size_t getTotalBytes() const;
private:
    std::string getMangledName(StringRef Name);
    std::string getMangledName(const GlobalValue *GV);

    TargetMachine &TM;
    const DataLayout DL;
    // Should be big enough that in the common case, The
    // object fits in its entirety
    SmallVector<char, 4096> ObjBufferSV;
    raw_svector_ostream ObjStream;
    legacy::PassManager PM0;  // per-optlevel pass managers
    legacy::PassManager PM1;
    legacy::PassManager PM2;
    legacy::PassManager PM3;
    TargetMachine *TMs[4];
    MCContext *Ctx;
    std::shared_ptr<RTDyldMemoryManager> MemMgr;
    std::unique_ptr<JITEventListener> JuliaListener;


    orc::ThreadSafeContext TSCtx;
    orc::ExecutionSession ES;
    orc::JITDylib &GlobalJD;
    orc::JITDylib &JD;

    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;

    DenseMap<void*, StringRef> ReverseLocalSymbolTable;
};
extern JuliaOJIT *jl_ExecutionEngine;

Pass *createLowerPTLSPass(bool imaging_mode);
Pass *createCombineMulAddPass();
Pass *createFinalLowerGCPass();
Pass *createLateLowerGCFramePass();
Pass *createLowerExcHandlersPass();
Pass *createGCInvariantVerifierPass(bool Strong);
Pass *createPropagateJuliaAddrspaces();
Pass *createRemoveJuliaAddrspacesPass();
Pass *createRemoveNIPass();
Pass *createJuliaLICMPass();
Pass *createMultiVersioningPass();
Pass *createAllocOptPass();
Pass *createDemoteFloat16Pass();
// Whether the Function is an llvm or julia intrinsic.
static inline bool isIntrinsicFunction(Function *F)
{
    return F->isIntrinsic() || F->getName().startswith("julia.");
}

CodeGenOpt::Level CodeGenOptLevelFor(int optlevel);
