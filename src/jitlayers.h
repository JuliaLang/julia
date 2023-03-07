// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/ADT/MapVector.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/JITEventListener.h>

#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Passes/StandardInstrumentations.h>

#include <llvm/Target/TargetMachine.h>
#include "julia_assert.h"
#include "julia.h"
#include "julia_internal.h"
#include "platform.h"

#include <stack>
#include <queue>

// As of LLVM 13, there are two runtime JIT linker implementations, the older
// RuntimeDyld (used via orc::RTDyldObjectLinkingLayer) and the newer JITLink
// (used via orc::ObjectLinkingLayer).
//
// JITLink is not only more flexible (which isn't of great importance for us, as
// we do only single-threaded in-process codegen), but crucially supports using
// the Small code model, where the linker needs to fix up relocations between
// object files that end up far apart in address space. RuntimeDyld can't do
// that and relies on the Large code model instead, which is broken on
// aarch64-darwin (macOS on ARM64), and not likely to ever be supported there
// (see https://bugs.llvm.org/show_bug.cgi?id=52029).
//
// However, JITLink is a relatively young library and lags behind in platform
// and feature support (e.g. Windows, JITEventListeners for various profilers,
// etc.). Thus, we currently only use JITLink where absolutely required, that is,
// for Mac/aarch64.
// #define JL_FORCE_JITLINK

#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_) || defined(JL_FORCE_JITLINK)
# if JL_LLVM_VERSION < 130000
#  pragma message("On aarch64-darwin, LLVM version >= 13 is required for JITLink; fallback suffers from occasional segfaults")
# endif
# define JL_USE_JITLINK
#endif

#ifdef JL_USE_JITLINK
# include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
#else
# include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#endif

using namespace llvm;

extern "C" jl_cgparams_t jl_default_cgparams;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeContext, LLVMOrcThreadSafeContextRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeModule, LLVMOrcThreadSafeModuleRef)

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis) JL_NOTSAFEPOINT;
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level, bool lower_intrinsics=true, bool dump_native=false, bool external_use=false) JL_NOTSAFEPOINT;
void addMachinePasses(legacy::PassManagerBase *PM, int optlevel) JL_NOTSAFEPOINT;
void jl_merge_module(orc::ThreadSafeModule &dest, orc::ThreadSafeModule src) JL_NOTSAFEPOINT;
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M) JL_NOTSAFEPOINT;
DataLayout jl_create_datalayout(TargetMachine &TM) JL_NOTSAFEPOINT;

static inline bool imaging_default() JL_NOTSAFEPOINT {
    return jl_options.image_codegen || (jl_generating_output() && jl_options.use_pkgimages);
}

struct OptimizationOptions {
    bool lower_intrinsics;
    bool dump_native;
    bool external_use;
    bool llvm_only;

    static constexpr OptimizationOptions defaults(
        bool lower_intrinsics=true,
        bool dump_native=false,
        bool external_use=false,
        bool llvm_only=false) {
        return {lower_intrinsics, dump_native, external_use, llvm_only};
    }
};

struct NewPM {
    std::unique_ptr<TargetMachine> TM;
    StandardInstrumentations SI;
    std::unique_ptr<PassInstrumentationCallbacks> PIC;
    PassBuilder PB;
    ModulePassManager MPM;
    OptimizationLevel O;

    NewPM(std::unique_ptr<TargetMachine> TM, OptimizationLevel O, OptimizationOptions options = OptimizationOptions::defaults()) JL_NOTSAFEPOINT;
    ~NewPM() JL_NOTSAFEPOINT;

    void run(Module &M) JL_NOTSAFEPOINT;
};

struct AnalysisManagers {
    LoopAnalysisManager LAM;
    FunctionAnalysisManager FAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;

    AnalysisManagers(PassBuilder &PB) JL_NOTSAFEPOINT;
    AnalysisManagers(TargetMachine &TM, PassBuilder &PB, OptimizationLevel O) JL_NOTSAFEPOINT;
    ~AnalysisManagers() JL_NOTSAFEPOINT;
};

OptimizationLevel getOptLevel(int optlevel) JL_NOTSAFEPOINT;

struct jl_locked_stream {
    ios_t *stream = nullptr;
    std::mutex mutex;

    struct lock {
        std::unique_lock<std::mutex> lck;
        ios_t *&stream;

        lock(std::mutex &mutex, ios_t *&stream) JL_NOTSAFEPOINT
            : lck(mutex), stream(stream) {}
        lock(lock&) = delete;
        lock(lock&&) JL_NOTSAFEPOINT = default;
        ~lock() JL_NOTSAFEPOINT = default;

        ios_t *&operator*() JL_NOTSAFEPOINT {
            return stream;
        }

        explicit operator bool() JL_NOTSAFEPOINT {
            return !!stream;
        }

        operator ios_t *() JL_NOTSAFEPOINT {
            return stream;
        }

        operator JL_STREAM *() JL_NOTSAFEPOINT {
            return (JL_STREAM*)stream;
        }
    };

    jl_locked_stream() JL_NOTSAFEPOINT = default;
    ~jl_locked_stream() JL_NOTSAFEPOINT = default;

    lock operator*() JL_NOTSAFEPOINT {
        return lock(mutex, stream);
    }
};

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

typedef std::tuple<jl_returninfo_t::CallingConv, unsigned, llvm::Function*, bool> jl_codegen_call_target_t;

typedef struct _jl_codegen_params_t {
    orc::ThreadSafeContext tsctx;
    orc::ThreadSafeContext::Lock tsctx_lock;

    inline LLVMContext &getContext() {
        return *tsctx.getContext();
    }
    typedef StringMap<GlobalVariable*> SymMapGV;
    // outputs
    std::vector<std::pair<jl_code_instance_t*, jl_codegen_call_target_t>> workqueue;
    std::map<void*, GlobalVariable*> globals;
    std::map<std::tuple<jl_code_instance_t*,bool>, GlobalVariable*> external_fns;
    std::map<jl_datatype_t*, DIType*> ditypes;
    std::map<jl_datatype_t*, Type*> llvmtypes;
    DenseMap<Constant*, GlobalVariable*> mergedConstants;
    // Map from symbol name (in a certain library) to its GV in sysimg and the
    // DL handle address in the current session.
    StringMap<std::pair<GlobalVariable*,SymMapGV>> libMapGV;
#ifdef _OS_WINDOWS_
    SymMapGV symMapExe;
    SymMapGV symMapDll;
    SymMapGV symMapDlli;
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
    std::unique_ptr<Module> _shared_module;
    inline Module &shared_module(Module &from);
    // inputs
    size_t world = 0;
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool cache = false;
    bool external_linkage = false;
    bool imaging;
    _jl_codegen_params_t(orc::ThreadSafeContext ctx) : tsctx(std::move(ctx)), tsctx_lock(tsctx.getLock()), imaging(imaging_default()) {}
} jl_codegen_params_t;

jl_llvm_functions_t jl_emit_code(
        orc::ThreadSafeModule &M,
        jl_method_instance_t *mi,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params);

jl_llvm_functions_t jl_emit_codeinst(
        orc::ThreadSafeModule &M,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params);

enum CompilationPolicy {
    Default = 0,
    Extern = 1,
};

typedef std::map<jl_code_instance_t*, std::pair<orc::ThreadSafeModule, jl_llvm_functions_t>> jl_workqueue_t;

void jl_compile_workqueue(
    jl_workqueue_t &emitted,
    Module &original,
    jl_codegen_params_t &params,
    CompilationPolicy policy);

Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt,
    jl_codegen_params_t &params);

void add_named_global(StringRef name, void *addr) JL_NOTSAFEPOINT;

static inline Constant *literal_static_pointer_val(const void *p, Type *T) JL_NOTSAFEPOINT
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

static const inline char *name_from_method_instance(jl_method_instance_t *li) JL_NOTSAFEPOINT
{
    return jl_is_method(li->def.method) ? jl_symbol_name(li->def.method->name) : "top-level scope";
}

typedef JITSymbol JL_JITSymbol;
// The type that is similar to SymbolInfo on LLVM 4.0 is actually
// `JITEvaluatedSymbol`. However, we only use this type when a JITSymbol
// is expected.
typedef JITSymbol JL_SymbolInfo;

using CompilerResultT = Expected<std::unique_ptr<llvm::MemoryBuffer>>;
using OptimizerResultT = Expected<orc::ThreadSafeModule>;

class JuliaOJIT {
public:
#ifdef JL_USE_JITLINK
    typedef orc::ObjectLinkingLayer ObjLayerT;
#else
    typedef orc::RTDyldObjectLinkingLayer ObjLayerT;
#endif
    struct LockLayerT : public orc::ObjectLayer {

        LockLayerT(orc::ObjectLayer &BaseLayer) JL_NOTSAFEPOINT : orc::ObjectLayer(BaseLayer.getExecutionSession()), BaseLayer(BaseLayer) {}
        ~LockLayerT() JL_NOTSAFEPOINT = default;

        void emit(std::unique_ptr<orc::MaterializationResponsibility> R,
                            std::unique_ptr<MemoryBuffer> O) override {
#ifndef JL_USE_JITLINK
            std::lock_guard<std::mutex> lock(EmissionMutex);
#endif
            BaseLayer.emit(std::move(R), std::move(O));
        }
    private:
        orc::ObjectLayer &BaseLayer;
        std::mutex EmissionMutex;
    };
    typedef orc::IRCompileLayer CompileLayerT;
    typedef orc::IRTransformLayer OptimizeLayerT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;
    template
    <typename ResourceT, size_t max = 0,
        typename BackingT = std::stack<ResourceT,
            std::conditional_t<max == 0,
                SmallVector<ResourceT>,
                SmallVector<ResourceT, max>
            >
        >
    >
    struct ResourcePool {
        public:
        ResourcePool(std::function<ResourceT()> creator) JL_NOTSAFEPOINT : creator(std::move(creator)), mutex(std::make_unique<WNMutex>()) {}
        ResourcePool(ResourcePool&) = delete;
        ResourcePool(ResourcePool&&) JL_NOTSAFEPOINT = default;
        ~ResourcePool() JL_NOTSAFEPOINT = default;
        class OwningResource {
            public:
            OwningResource(ResourcePool &pool, ResourceT resource) JL_NOTSAFEPOINT // _ENTER
                : pool(pool), resource(std::move(resource)) {}
            OwningResource(const OwningResource &) = delete;
            OwningResource &operator=(const OwningResource &) = delete;
            OwningResource(OwningResource &&) JL_NOTSAFEPOINT = default;
            OwningResource &operator=(OwningResource &&) JL_NOTSAFEPOINT = default;
            ~OwningResource() JL_NOTSAFEPOINT { // _LEAVE
                if (resource)
                    pool.release(std::move(*resource));
            }
            ResourceT release() JL_NOTSAFEPOINT {
                ResourceT res(std::move(*resource));
                resource.reset();
                return res;
            }
            void reset(ResourceT res) JL_NOTSAFEPOINT {
                *resource = std::move(res);
            }
            ResourceT &operator*() JL_NOTSAFEPOINT {
                return *resource;
            }
            ResourceT *operator->() JL_NOTSAFEPOINT {
                return get();
            }
            ResourceT *get() JL_NOTSAFEPOINT {
                return resource.getPointer();
            }
            const ResourceT &operator*() const JL_NOTSAFEPOINT {
                return *resource;
            }
            const ResourceT *operator->() const JL_NOTSAFEPOINT {
                return get();
            }
            const ResourceT *get() const JL_NOTSAFEPOINT {
                return resource.getPointer();
            }
            explicit operator bool() const JL_NOTSAFEPOINT {
                return resource;
            }
            private:
            ResourcePool &pool;
            llvm::Optional<ResourceT> resource;
        };

        OwningResource operator*() JL_NOTSAFEPOINT {
            return OwningResource(*this, acquire());
        }

        OwningResource get() {
            return **this;
        }

        ResourceT acquire() JL_NOTSAFEPOINT { // _ENTER
            std::unique_lock<std::mutex> lock(mutex->mutex);
            if (!pool.empty()) {
                return pop(pool);
            }
            if (!max || created < max) {
                created++;
                return creator();
            }
            mutex->empty.wait(lock, [&](){ return !pool.empty(); });
            assert(!pool.empty() && "Expected resource pool to have a value!");
            return pop(pool);
        }
        void release(ResourceT &&resource) JL_NOTSAFEPOINT { // _LEAVE
            std::lock_guard<std::mutex> lock(mutex->mutex);
            pool.push(std::move(resource));
            mutex->empty.notify_one();
        }
        private:
        template<typename T, typename Container>
        static ResourceT pop(std::queue<T, Container> &pool) JL_NOTSAFEPOINT {
            ResourceT top = std::move(pool.front());
            pool.pop();
            return top;
        }
        template<typename PoolT>
        static ResourceT pop(PoolT &pool) JL_NOTSAFEPOINT {
            ResourceT top = std::move(pool.top());
            pool.pop();
            return top;
        }
        std::function<ResourceT()> creator;
        size_t created = 0;
        BackingT pool;
        struct WNMutex {
            std::mutex mutex;
            std::condition_variable empty;
        };

        std::unique_ptr<WNMutex> mutex;
    };
    struct PipelineT {
        PipelineT(orc::ObjectLayer &BaseLayer, TargetMachine &TM, int optlevel);
        CompileLayerT CompileLayer;
        OptimizeLayerT OptimizeLayer;
    };

    struct OptSelLayerT : orc::IRLayer {

        template<size_t N>
        OptSelLayerT(const std::array<std::unique_ptr<PipelineT>, N> &optimizers) JL_NOTSAFEPOINT
            : orc::IRLayer(optimizers[0]->OptimizeLayer.getExecutionSession(),
                optimizers[0]->OptimizeLayer.getManglingOptions()),
            optimizers(optimizers.data()),
            count(N) {
            static_assert(N > 0, "Expected array with at least one optimizer!");
        }
        ~OptSelLayerT() JL_NOTSAFEPOINT = default;

        void emit(std::unique_ptr<orc::MaterializationResponsibility> R, orc::ThreadSafeModule TSM) override;

        private:
        const std::unique_ptr<PipelineT> * const optimizers;
        size_t count;
    };

private:
    // Custom object emission notification handler for the JuliaOJIT
    template <typename ObjT, typename LoadResult>
    void registerObject(const ObjT &Obj, const LoadResult &LO);

public:

    JuliaOJIT() JL_NOTSAFEPOINT;
    ~JuliaOJIT() JL_NOTSAFEPOINT;

    void enableJITDebuggingSupport() JL_NOTSAFEPOINT;
#ifndef JL_USE_JITLINK
    // JITLink doesn't support old JITEventListeners (yet).
    void RegisterJITEventListener(JITEventListener *L) JL_NOTSAFEPOINT;
#endif

    orc::SymbolStringPtr mangle(StringRef Name) JL_NOTSAFEPOINT;
    void addGlobalMapping(StringRef Name, uint64_t Addr) JL_NOTSAFEPOINT;
    void addModule(orc::ThreadSafeModule M) JL_NOTSAFEPOINT;

    JL_JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) JL_NOTSAFEPOINT;
    JL_JITSymbol findUnmangledSymbol(StringRef Name) JL_NOTSAFEPOINT;
    uint64_t getGlobalValueAddress(StringRef Name) JL_NOTSAFEPOINT;
    uint64_t getFunctionAddress(StringRef Name) JL_NOTSAFEPOINT;
    StringRef getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst) JL_NOTSAFEPOINT;
    auto getContext() JL_NOTSAFEPOINT {
        return *ContextPool;
    }
    orc::ThreadSafeContext acquireContext() { // JL_NOTSAFEPOINT_ENTER?
        return ContextPool.acquire();
    }
    void releaseContext(orc::ThreadSafeContext &&ctx) { // JL_NOTSAFEPOINT_LEAVE?
        ContextPool.release(std::move(ctx));
    }
    const DataLayout& getDataLayout() const JL_NOTSAFEPOINT;

    // TargetMachine pass-through methods
    std::unique_ptr<TargetMachine> cloneTargetMachine() const JL_NOTSAFEPOINT;
    const Triple& getTargetTriple() const JL_NOTSAFEPOINT;
    StringRef getTargetFeatureString() const JL_NOTSAFEPOINT;
    StringRef getTargetCPU() const JL_NOTSAFEPOINT;
    const TargetOptions &getTargetOptions() const JL_NOTSAFEPOINT;
    const Target &getTarget() const JL_NOTSAFEPOINT;
    TargetIRAnalysis getTargetIRAnalysis() const JL_NOTSAFEPOINT;

    size_t getTotalBytes() const JL_NOTSAFEPOINT;

    jl_locked_stream &get_dump_emitted_mi_name_stream() JL_NOTSAFEPOINT {
        return dump_emitted_mi_name_stream;
    }
    jl_locked_stream &get_dump_compiles_stream() JL_NOTSAFEPOINT {
        return dump_compiles_stream;
    }
    jl_locked_stream &get_dump_llvm_opt_stream() JL_NOTSAFEPOINT {
        return dump_llvm_opt_stream;
    }
private:
    std::string getMangledName(StringRef Name) JL_NOTSAFEPOINT;
    std::string getMangledName(const GlobalValue *GV) JL_NOTSAFEPOINT;
    void shareStrings(Module &M) JL_NOTSAFEPOINT;

    const std::unique_ptr<TargetMachine> TM;
    const DataLayout DL;

    orc::ExecutionSession ES;
    orc::JITDylib &GlobalJD;
    orc::JITDylib &JD;

    //Map and inc are guarded by RLST_mutex
    std::mutex RLST_mutex{};
    int RLST_inc = 0;
    DenseMap<void*, std::string> ReverseLocalSymbolTable;

    //Compilation streams
    jl_locked_stream dump_emitted_mi_name_stream;
    jl_locked_stream dump_compiles_stream;
    jl_locked_stream dump_llvm_opt_stream;

    ResourcePool<orc::ThreadSafeContext, 0, std::queue<orc::ThreadSafeContext>> ContextPool;

#ifndef JL_USE_JITLINK
    const std::shared_ptr<RTDyldMemoryManager> MemMgr;
#else
    std::atomic<size_t> total_size{0};
    const std::unique_ptr<jitlink::JITLinkMemoryManager> MemMgr;
#endif
    ObjLayerT ObjectLayer;
    LockLayerT LockLayer;
    const std::array<std::unique_ptr<PipelineT>, 4> Pipelines;
    OptSelLayerT OptSelLayer;
};
extern JuliaOJIT *jl_ExecutionEngine;
std::unique_ptr<Module> jl_create_llvm_module(StringRef name, LLVMContext &ctx, bool imaging_mode, const DataLayout &DL = jl_ExecutionEngine->getDataLayout(), const Triple &triple = jl_ExecutionEngine->getTargetTriple()) JL_NOTSAFEPOINT;
inline orc::ThreadSafeModule jl_create_ts_module(StringRef name, orc::ThreadSafeContext ctx, bool imaging_mode, const DataLayout &DL = jl_ExecutionEngine->getDataLayout(), const Triple &triple = jl_ExecutionEngine->getTargetTriple()) JL_NOTSAFEPOINT {
    auto lock = ctx.getLock();
    return orc::ThreadSafeModule(jl_create_llvm_module(name, *ctx.getContext(), imaging_mode, DL, triple), ctx);
}

Module &jl_codegen_params_t::shared_module(Module &from) JL_NOTSAFEPOINT {
    if (!_shared_module) {
        _shared_module = jl_create_llvm_module("globals", getContext(), imaging, from.getDataLayout(), Triple(from.getTargetTriple()));
        assert(&from.getContext() == tsctx.getContext() && "Module context differs from codegen_params context!");
    } else {
        assert(&from.getContext() == &getContext() && "Module context differs from shared module context!");
        assert(from.getDataLayout() == _shared_module->getDataLayout() && "Module data layout differs from shared module data layout!");
        assert(from.getTargetTriple() == _shared_module->getTargetTriple() && "Module target triple differs from shared module target triple!");
    }
    return *_shared_module;
}

Pass *createLowerPTLSPass(bool imaging_mode) JL_NOTSAFEPOINT;
Pass *createCombineMulAddPass() JL_NOTSAFEPOINT;
Pass *createFinalLowerGCPass() JL_NOTSAFEPOINT;
Pass *createLateLowerGCFramePass() JL_NOTSAFEPOINT;
Pass *createLowerExcHandlersPass() JL_NOTSAFEPOINT;
Pass *createGCInvariantVerifierPass(bool Strong) JL_NOTSAFEPOINT;
Pass *createPropagateJuliaAddrspaces() JL_NOTSAFEPOINT;
Pass *createRemoveJuliaAddrspacesPass() JL_NOTSAFEPOINT;
Pass *createRemoveNIPass() JL_NOTSAFEPOINT;
Pass *createJuliaLICMPass() JL_NOTSAFEPOINT;
Pass *createMultiVersioningPass(bool external_use) JL_NOTSAFEPOINT;
Pass *createAllocOptPass() JL_NOTSAFEPOINT;
Pass *createDemoteFloat16Pass() JL_NOTSAFEPOINT;
Pass *createCPUFeaturesPass() JL_NOTSAFEPOINT;
Pass *createLowerSimdLoopPass() JL_NOTSAFEPOINT;

// NewPM
#include "passes.h"

// Whether the Function is an llvm or julia intrinsic.
static inline bool isIntrinsicFunction(Function *F) JL_NOTSAFEPOINT
{
    return F->isIntrinsic() || F->getName().startswith("julia.");
}

CodeGenOpt::Level CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
