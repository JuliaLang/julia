// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#if defined(USE_ORCJIT)
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/LazyEmittingLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/ObjectMemoryBuffer.h"
#elif defined(USE_MCJIT)
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/Object/ObjectFile.h>
#else
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#endif

#if JL_LLVM_VERSION >= 30700
#include "llvm/IR/LegacyPassManager.h"
extern legacy::PassManager *jl_globalPM;
#else
#include <llvm/PassManager.h>
extern PassManager *jl_globalPM;
#endif

#if JL_LLVM_VERSION >= 30500
#include <llvm/Target/TargetMachine.h>
#endif

extern "C" {
    extern int globalUnique;
}
extern TargetMachine *jl_TargetMachine;
extern Module *shadow_output;
extern bool imaging_mode;
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
extern Function *juliapersonality_func;
#endif


#ifdef JULIA_ENABLE_THREADING
extern size_t jltls_states_func_idx;
#endif

typedef struct {Value *gv; int32_t index;} jl_value_llvm; // uses 1-based indexing

#if JL_LLVM_VERSION >= 30700
void addOptimizationPasses(legacy::PassManager *PM);
#else
void addOptimizationPasses(PassManager *PM);
#endif
void* jl_emit_and_add_to_shadow(GlobalVariable *gv, void *gvarinit = NULL);
GlobalVariable *jl_emit_sysimg_slot(Module *m, Type *typ, const char *name,
                                           uintptr_t init, size_t &idx);
void* jl_get_global(GlobalVariable *gv);
GlobalVariable *jl_get_global_for(const char *cname, void *addr, Module *M);
void jl_add_to_shadow(Module *m);
void jl_init_function(Function *f);
bool jl_can_finalize_function(Function *F);
void jl_finalize_function(Function *F);
void jl_finalize_module(Module *m, bool shadow);

// Connect Modules via prototypes, each owned by module `M`
static inline GlobalVariable *global_proto(GlobalVariable *G, Module *M = NULL)
{
    // Copy the GlobalVariable, but without the initializer, so it becomes a declaration
    GlobalVariable *proto = new GlobalVariable(G->getType()->getElementType(),
            G->isConstant(), GlobalVariable::ExternalLinkage,
            NULL, G->getName(),  G->getThreadLocalMode());
    proto->copyAttributesFrom(G);
#if JL_LLVM_VERSION >= 30500
    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    proto->setDLLStorageClass(GlobalValue::DefaultStorageClass);
#endif
    if (M)
        M->getGlobalList().push_back(proto);
    return proto;
}

static inline Function *function_proto(Function *F, Module *M = NULL)
{
    // Copy the declaration characteristics of the Function (not the body)
    Function *NewF = Function::Create(F->getFunctionType(),
                                      Function::ExternalLinkage,
                                      F->getName(), M);

    // Declarations are not allowed to have personality routines, but
    // copyAttributesFrom sets them anyway. Temporarily unset the personality
    // routine from `F`, since copying it and then resetting is more expensive
    // as well as introducing an extra use from this unowned function, which
    // can cause crashes in the LLVMContext's global destructor.
#if JL_LLVM_VERSION >= 30700
    llvm::Constant *OldPersonalityFn = nullptr;
    if (F->hasPersonalityFn()) {
        OldPersonalityFn = F->getPersonalityFn();
        F->setPersonalityFn(nullptr);
    }
#endif

     // FunctionType does not include any attributes. Copy them over manually
     // as codegen may make decisions based on the presence of certain attributes
     NewF->copyAttributesFrom(F);

#if JL_LLVM_VERSION >= 30700
    if (OldPersonalityFn)
        F->setPersonalityFn(OldPersonalityFn);
#endif

#if JL_LLVM_VERSION >= 30500
    // DLLImport only needs to be set for the shadow module
    // it just gets annoying in the JIT
    NewF->setDLLStorageClass(GlobalValue::DefaultStorageClass);
#endif

    return NewF;
}

static inline GlobalVariable *prepare_global(GlobalVariable *G, Module *M)
{
    if (G->getParent() == M)
        return G;
    GlobalValue *local = M->getNamedValue(G->getName());
    if (!local) {
        local = global_proto(G, M);
    }
    return cast<GlobalVariable>(local);
}

#if JL_LLVM_VERSION >= 30500
void add_named_global(GlobalObject *gv, void *addr, bool dllimport);
template<typename T>
static inline void add_named_global(GlobalObject *gv, T *addr, bool dllimport = true)
#else
void add_named_global(GlobalValue *gv, void *addr, bool dllimport);
template<typename T>
static inline void add_named_global(GlobalValue *gv, T *addr, bool dllimport = true)
#endif
{
    // cast through integer to avoid c++ pedantic warning about casting between
    // data and code pointers
    add_named_global(gv, (void*)(uintptr_t)addr, dllimport);
}

void jl_init_jit(Type *T_pjlvalue_);
#ifdef USE_ORCJIT
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

class JuliaOJIT {
    // Custom object emission notification handler for the JuliaOJIT
    // TODO: hook up RegisterJITEventListener, instead of hard-coding the GDB and JuliaListener targets
    class DebugObjectRegistrar {
    public:
        DebugObjectRegistrar(JuliaOJIT &JIT);
        template <typename ObjSetT, typename LoadResult>
        void operator()(orc::ObjectLinkingLayerBase::ObjSetHandleT H, const ObjSetT &Objects,
                        const LoadResult &LOS);
    private:
        void NotifyGDB(object::OwningBinary<object::ObjectFile> &DebugObj);
        std::vector<object::OwningBinary<object::ObjectFile>> SavedObjects;
        std::unique_ptr<JITEventListener> JuliaListener;
        JuliaOJIT &JIT;
    };

public:
    typedef orc::ObjectLinkingLayer<DebugObjectRegistrar> ObjLayerT;
    typedef orc::IRCompileLayer<ObjLayerT> CompileLayerT;
    typedef CompileLayerT::ModuleSetHandleT ModuleHandleT;
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
    Function *FindFunctionNamed(const std::string &Name);
    void RegisterJITEventListener(JITEventListener *L);
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
    RTDyldMemoryManager *MemMgr;
    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;
    SymbolTableT GlobalSymbolTable;
    SymbolTableT LocalSymbolTable;
};
extern JuliaOJIT *jl_ExecutionEngine;
#else
extern ExecutionEngine *jl_ExecutionEngine;
#endif
#if JL_LLVM_VERSION >= 30900
JL_DLLEXPORT extern LLVMContext jl_LLVMContext;
#else
JL_DLLEXPORT extern LLVMContext &jl_LLVMContext;
#endif

Pass *createLowerPTLSPass(bool imaging_mode);
Pass *createLowerGCFramePass();
// Whether the Function is an llvm or julia intrinsic.
static inline bool isIntrinsicFunction(Function *F)
{
    return F->isIntrinsic() || F->getName().startswith("julia.");
}
