// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/LazyEmittingLayer.h"
#if JL_LLVM_VERSION >= 50000
#  include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#else
#  include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#endif
#include "llvm/ExecutionEngine/ObjectMemoryBuffer.h"
#include "llvm/ExecutionEngine/JITEventListener.h"

#include <llvm/Target/TargetMachine.h>
#include "julia_assert.h"

extern "C" {
    extern int globalUnique;
}
extern TargetMachine *jl_TargetMachine;
extern Module *shadow_output;
extern bool imaging_mode;

void addTargetPasses(legacy::PassManagerBase *PM, TargetMachine *TM);
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level, bool dump_native=false);
void jl_finalize_module(std::unique_ptr<Module>  m);
void jl_merge_module(Module *dest, std::unique_ptr<Module> src);

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
};

typedef std::vector<std::tuple<jl_method_instance_t*, jl_returninfo_t::CallingConv, llvm::Function*, jl_value_t*, bool>> jl_codegen_call_targets_t;
typedef std::tuple<std::unique_ptr<Module>, jl_llvm_functions_t, jl_value_t*> jl_compile_result_t;

typedef struct {
    // outputs
    jl_codegen_call_targets_t workqueue;
    std::map<void *, GlobalVariable *> globals;
    std::map<jl_datatype_t *, DIType *> ditypes;
    std::map<jl_datatype_t *, Type *> llvmtypes;
    // inputs
    size_t world = 0;
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool cache = false;
} jl_codegen_params_t;

jl_compile_result_t jl_compile_linfo1(
        jl_method_instance_t *li,
        jl_code_info_t *src,
        jl_codegen_params_t &params);

void jl_compile_workqueue(
    std::map<jl_method_instance_t *, jl_compile_result_t> &emitted,
    jl_codegen_params_t &params);

Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt,
    jl_codegen_params_t &params);


// Connect Modules via prototypes, each owned by module `M`
static inline GlobalVariable *global_proto(GlobalVariable *G, Module *M = NULL)
{
    // Copy the GlobalVariable, but without the initializer, so it becomes a declaration
    GlobalVariable *proto = new GlobalVariable(G->getType()->getElementType(),
            G->isConstant(), GlobalVariable::ExternalLinkage,
            NULL, G->getName(),  G->getThreadLocalMode());
    proto->copyAttributesFrom(G);
    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    proto->setDLLStorageClass(GlobalValue::DefaultStorageClass);
    if (M)
        M->getGlobalList().push_back(proto);
    return proto;
}

static inline GlobalVariable *prepare_global_in(Module *M, GlobalVariable *G)
{
    if (G->getParent() == M)
        return G;
    GlobalValue *local = M->getNamedValue(G->getName());
    if (!local) {
        local = global_proto(G, M);
    }
    return cast<GlobalVariable>(local);
}

void add_named_global(GlobalObject *gv, void *addr, bool dllimport);
template<typename T>
static inline void add_named_global(GlobalObject *gv, T *addr, bool dllimport = true)
{
    // cast through integer to avoid c++ pedantic warning about casting between
    // data and code pointers
    add_named_global(gv, (void*)(uintptr_t)addr, dllimport);
}

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

#if JL_LLVM_VERSION >= 40000
typedef JITSymbol JL_JITSymbol;
// The type that is similar to SymbolInfo on LLVM 4.0 is actually
// `JITEvaluatedSymbol`. However, we only use this type when a JITSymbol
// is expected.
typedef JITSymbol JL_SymbolInfo;
#else
typedef orc::JITSymbol JL_JITSymbol;
typedef RuntimeDyld::SymbolInfo JL_SymbolInfo;
#endif
#if JL_LLVM_VERSION >= 50000
using RTDyldObjHandleT = orc::RTDyldObjectLinkingLayerBase::ObjHandleT;
#else
using RTDyldObjHandleT = orc::ObjectLinkingLayerBase::ObjSetHandleT;
#endif

class JuliaOJIT {
    // Custom object emission notification handler for the JuliaOJIT
    // TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
    class DebugObjectRegistrar {
    public:
        DebugObjectRegistrar(JuliaOJIT &JIT);
        template <typename ObjSetT, typename LoadResult>
        void operator()(RTDyldObjHandleT H, const ObjSetT &Objects, const LoadResult &LOS);
    private:
        template <typename ObjT, typename LoadResult>
        void registerObject(RTDyldObjHandleT H, const ObjT &Object, const LoadResult &LO);
        void NotifyGDB(object::OwningBinary<object::ObjectFile> &DebugObj);
        std::vector<object::OwningBinary<object::ObjectFile>> SavedObjects;
        std::unique_ptr<JITEventListener> JuliaListener;
        JuliaOJIT &JIT;
    };

    struct CompilerT {
        CompilerT(JuliaOJIT *pjit)
            : jit(*pjit)
        {}
        object::OwningBinary<object::ObjectFile> operator()(Module &M);
    private:
        JuliaOJIT &jit;
    };

public:
#if JL_LLVM_VERSION >= 50000
    typedef orc::RTDyldObjectLinkingLayer ObjLayerT;
    typedef orc::IRCompileLayer<ObjLayerT,CompilerT> CompileLayerT;
    typedef CompileLayerT::ModuleHandleT ModuleHandleT;
#else
    typedef orc::ObjectLinkingLayer<std::reference_wrapper<DebugObjectRegistrar>> ObjLayerT;
    typedef orc::IRCompileLayer<ObjLayerT> CompileLayerT;
    typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;
#endif
    typedef StringMap<void*> SymbolTableT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;

    JuliaOJIT(TargetMachine &TM);

    void addGlobalMapping(StringRef Name, uint64_t Addr);
    void addGlobalMapping(const GlobalValue *GV, void *Addr);
    void *getPointerToGlobalIfAvailable(StringRef S);
    void *getPointerToGlobalIfAvailable(const GlobalValue *GV);
    void addModule(std::unique_ptr<Module> M);
    void removeModule(ModuleHandleT H);
    JL_JITSymbol findSymbol(const std::string &Name, bool ExportedSymbolsOnly);
    JL_JITSymbol findUnmangledSymbol(const std::string Name);
    uint64_t getGlobalValueAddress(const std::string &Name);
    uint64_t getFunctionAddress(const std::string &Name);
    StringRef getFunctionAtAddress(uint64_t Addr, jl_method_instance_t *li);
    void RegisterJITEventListener(JITEventListener *L);
    const DataLayout& getDataLayout() const;
    const Triple& getTargetTriple() const;
private:
    std::string getMangledName(StringRef Name);
    std::string getMangledName(const GlobalValue *GV);

    TargetMachine &TM;
    const DataLayout DL;
    // Should be big enough that in the common case, The
    // object fits in its entirety
    SmallVector<char, 4096> ObjBufferSV;
    raw_svector_ostream ObjStream;
    legacy::PassManager PM;
    MCContext *Ctx;
    std::shared_ptr<RTDyldMemoryManager> MemMgr;
    DebugObjectRegistrar registrar;
    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;
    SymbolTableT GlobalSymbolTable;
    SymbolTableT LocalSymbolTable;
    DenseMap<void*, StringRef> ReverseLocalSymbolTable;
};
extern JuliaOJIT *jl_ExecutionEngine;

Pass *createLowerPTLSPass(bool imaging_mode);
Pass *createCombineMulAddPass();
Pass *createLateLowerGCFramePass();
Pass *createLowerExcHandlersPass();
Pass *createGCInvariantVerifierPass(bool Strong);
Pass *createPropagateJuliaAddrspaces();
Pass *createMultiVersioningPass();
Pass *createAllocOptPass();
// Whether the Function is an llvm or julia intrinsic.
static inline bool isIntrinsicFunction(Function *F)
{
    return F->isIntrinsic() || F->getName().startswith("julia.");
}
