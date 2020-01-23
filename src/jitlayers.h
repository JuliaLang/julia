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
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/JITEventListener.h"

#include "llvm/IR/LegacyPassManager.h"
extern legacy::PassManager *jl_globalPM;

#include <llvm/Target/TargetMachine.h>
#include "julia_assert.h"

extern "C" {
    extern int globalUnique;
}
extern TargetMachine *jl_TargetMachine;
extern Module *shadow_output;
extern bool imaging_mode;
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
extern Function *juliapersonality_func;
#endif


typedef struct {Value *gv; int32_t index;} jl_value_llvm; // uses 1-based indexing

void addTargetPasses(legacy::PassManagerBase *PM, TargetMachine *TM);
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level, bool lower_intrinsics=true, bool dump_native=false);
void** jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit = NULL);
void* jl_get_globalvar(GlobalVariable *gv);
GlobalVariable *jl_get_global_for(const char *cname, void *addr, Module *M);
void jl_add_to_shadow(Module *m);
void jl_init_function(Function *f);
bool jl_can_finalize_function(StringRef F);
void jl_finalize_function(StringRef F);
void jl_finalize_module(Module *m, bool shadow);

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

void jl_init_jit(Type *T_pjlvalue_);
typedef JITSymbol JL_JITSymbol;
// The type that is similar to SymbolInfo on LLVM 4.0 is actually
// `JITEvaluatedSymbol`. However, we only use this type when a JITSymbol
// is expected.
typedef JITSymbol JL_SymbolInfo;

using RTDyldObjHandleT = orc::VModuleKey;
using CompilerResultT = std::unique_ptr<llvm::MemoryBuffer>;

class JuliaOJIT {
    // Custom object emission notification handler for the JuliaOJIT
    class DebugObjectRegistrar {
    public:
        DebugObjectRegistrar(JuliaOJIT &JIT);
        template <typename ObjSetT, typename LoadResult>
        void operator()(RTDyldObjHandleT H, const ObjSetT &Object, const LoadResult &LOS);
    private:
        template <typename ObjT, typename LoadResult>
        void registerObject(RTDyldObjHandleT H, const ObjT &Obj, const LoadResult &LO);
        std::unique_ptr<JITEventListener> JuliaListener;
        JuliaOJIT &JIT;
    };

    struct CompilerT {
        CompilerT(JuliaOJIT *pjit)
            : jit(*pjit)
        {}
        CompilerResultT operator()(Module &M);
    private:
        JuliaOJIT &jit;
    };

public:
    typedef orc::LegacyRTDyldObjectLinkingLayer ObjLayerT;
    typedef orc::LegacyIRCompileLayer<ObjLayerT,CompilerT> CompileLayerT;
    typedef orc::VModuleKey ModuleHandleT;
    typedef StringMap<void*> SymbolTableT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;

    JuliaOJIT(TargetMachine &TM);

    void RegisterJITEventListener(JITEventListener *L);
    std::vector<JITEventListener *> EventListeners;
    void NotifyFinalizer(RTDyldObjHandleT Key,
                         const object::ObjectFile &Obj,
                         const RuntimeDyld::LoadedObjectInfo &LoadedObjectInfo);
    void addGlobalMapping(StringRef Name, uint64_t Addr);
    void addGlobalMapping(const GlobalValue *GV, void *Addr);
    void *getPointerToGlobalIfAvailable(StringRef S);
    void *getPointerToGlobalIfAvailable(const GlobalValue *GV);
    void addModule(std::unique_ptr<Module> M);
    void removeModule(ModuleHandleT H);
    JL_JITSymbol findSymbol(const std::string &Name, bool ExportedSymbolsOnly);
    JL_JITSymbol findUnmangledSymbol(const std::string Name);
    JL_JITSymbol resolveSymbol(const std::string& Name);
    uint64_t getGlobalValueAddress(const std::string &Name);
    uint64_t getFunctionAddress(const std::string &Name);
    Function *FindFunctionNamed(const std::string &Name);
    const DataLayout& getDataLayout() const;
    const Triple& getTargetTriple() const;
private:
    std::string getMangledName(const std::string &Name);
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

    llvm::orc::ExecutionSession ES;
    std::shared_ptr<llvm::orc::SymbolResolver> SymbolResolver;

    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;

    SymbolTableT GlobalSymbolTable;
    SymbolTableT LocalSymbolTable;
};
extern JuliaOJIT *jl_ExecutionEngine;
JL_DLLEXPORT extern LLVMContext &jl_LLVMContext;

Pass *createLowerPTLSPass(bool imaging_mode);
Pass *createCombineMulAddPass();
Pass *createFinalLowerGCPass();
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
