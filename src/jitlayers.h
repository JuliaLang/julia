// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/IRTransformLayer.h>
#include <llvm/ExecutionEngine/JITEventListener.h>

#include <llvm/Target/TargetMachine.h>
#include "julia_assert.h"
#include "debug-registry.h"

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
#if defined(_OS_DARWIN_) && defined(_CPU_AARCH64_)
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

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis);
void addOptimizationPasses(legacy::PassManagerBase *PM, int opt_level, bool lower_intrinsics=true, bool dump_native=false, bool external_use=false);
void addMachinePasses(legacy::PassManagerBase *PM, int optlevel);
void jl_finalize_module(orc::ThreadSafeModule  m);
void jl_merge_module(orc::ThreadSafeModule &dest, orc::ThreadSafeModule src);
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M);
DataLayout jl_create_datalayout(TargetMachine &TM);

static inline bool imaging_default() {
    return jl_options.image_codegen || (jl_generating_output() && !jl_options.incremental);
}

struct jl_locked_stream {
    JL_STREAM *stream = nullptr;
    std::mutex mutex;

    struct lock {
        std::unique_lock<std::mutex> lck;
        JL_STREAM *&stream;

        lock(std::mutex &mutex, JL_STREAM *&stream) : lck(mutex), stream(stream) {}

        JL_STREAM *&operator*() {
            return stream;
        }

        explicit operator bool() {
            return !!stream;
        }

        operator JL_STREAM *() {
            return stream;
        }
    };

    lock operator*() {
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

struct jl_llvmf_dump_t {
    orc::ThreadSafeModule TSM;
    Function *F;
};

typedef std::vector<std::tuple<jl_code_instance_t*, jl_returninfo_t::CallingConv, unsigned, llvm::Function*, bool>> jl_codegen_call_targets_t;

typedef struct _jl_codegen_params_t {
    orc::ThreadSafeContext tsctx;
    orc::ThreadSafeContext::Lock tsctx_lock;
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
    orc::ThreadSafeModule _shared_module;
    inline orc::ThreadSafeModule &shared_module(Module &from);
    // inputs
    size_t world = 0;
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool cache = false;
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
    ImagingMode = 2
};

typedef std::map<jl_code_instance_t*, std::pair<orc::ThreadSafeModule, jl_llvm_functions_t>> jl_workqueue_t;

void jl_compile_workqueue(
    jl_workqueue_t &emitted,
    Module &original,
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
    typedef orc::IRCompileLayer CompileLayerT;
    typedef orc::IRTransformLayer OptimizeLayerT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;
    template<typename ResourceT, size_t max = 0>
    struct ResourcePool {
        public:
        ResourcePool(std::function<ResourceT()> creator) : creator(std::move(creator)), mutex(std::make_unique<WNMutex>()) {}
        class OwningResource {
            public:
            OwningResource(ResourcePool &pool, ResourceT resource) : pool(pool), resource(std::move(resource)) {}
            OwningResource(const OwningResource &) = delete;
            OwningResource &operator=(const OwningResource &) = delete;
            OwningResource(OwningResource &&) = default;
            OwningResource &operator=(OwningResource &&) = default;
            ~OwningResource() {
                if (resource) pool.release(std::move(*resource));
            }
            ResourceT release() {
                ResourceT res(std::move(*resource));
                resource.reset();
                return res;
            }
            void reset(ResourceT res) {
                *resource = std::move(res);
            }
            ResourceT &operator*() {
                return *resource;
            }
            ResourceT *operator->() {
                return get();
            }
            ResourceT *get() {
                return resource.getPointer();
            }
            const ResourceT &operator*() const {
                return *resource;
            }
            const ResourceT *operator->() const {
                return get();
            }
            const ResourceT *get() const {
                return resource.getPointer();
            }
            explicit operator bool() const {
                return resource;
            }
            private:
            ResourcePool &pool;
            llvm::Optional<ResourceT> resource;
        };

        OwningResource operator*() {
            return OwningResource(*this, acquire());
        }

        OwningResource get() {
            return **this;
        }

        ResourceT acquire() {
            std::unique_lock<std::mutex> lock(mutex->mutex);
            if (!pool.empty()) {
                return pool.pop_back_val();
            }
            if (!max || created < max) {
                created++;
                return creator();
            }
            mutex->empty.wait(lock, [&](){ return !pool.empty(); });
            assert(!pool.empty() && "Expected resource pool to have a value!");
            return pool.pop_back_val();
        }
        void release(ResourceT &&resource) {
            std::lock_guard<std::mutex> lock(mutex->mutex);
            pool.push_back(std::move(resource));
            mutex->empty.notify_one();
        }
        private:
        std::function<ResourceT()> creator;
        size_t created = 0;
        llvm::SmallVector<ResourceT, max == 0 ? 8 : max> pool;
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
        OptSelLayerT(const std::array<std::unique_ptr<PipelineT>, N> &optimizers)
            : orc::IRLayer(optimizers[0]->OptimizeLayer.getExecutionSession(),
                optimizers[0]->OptimizeLayer.getManglingOptions()),
            optimizers(optimizers.data()),
            count(N) {
            static_assert(N > 0, "Expected array with at least one optimizer!");
        }

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

    JuliaOJIT();

    void enableJITDebuggingSupport();
#ifndef JL_USE_JITLINK
    // JITLink doesn't support old JITEventListeners (yet).
    void RegisterJITEventListener(JITEventListener *L);
#endif

    void addGlobalMapping(StringRef Name, uint64_t Addr);
    void addModule(orc::ThreadSafeModule M);

    JL_JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly);
    JL_JITSymbol findUnmangledSymbol(StringRef Name);
    uint64_t getGlobalValueAddress(StringRef Name);
    uint64_t getFunctionAddress(StringRef Name);
    StringRef getFunctionAtAddress(uint64_t Addr, jl_code_instance_t *codeinst);
    auto getContext() {
        return *ContextPool;
    }
    orc::ThreadSafeContext acquireContext() {
        return ContextPool.acquire();
    }
    void releaseContext(orc::ThreadSafeContext &&ctx) {
        ContextPool.release(std::move(ctx));
    }
    const DataLayout& getDataLayout() const;

    // TargetMachine pass-through methods
    std::unique_ptr<TargetMachine> cloneTargetMachine() const;
    const Triple& getTargetTriple() const;
    StringRef getTargetFeatureString() const;
    StringRef getTargetCPU() const;
    const TargetOptions &getTargetOptions() const;
    const Target &getTarget() const;
    TargetIRAnalysis getTargetIRAnalysis() const;

    size_t getTotalBytes() const;

    JITDebugInfoRegistry &getDebugInfoRegistry() JL_NOTSAFEPOINT {
        return DebugRegistry;
    }

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
    std::string getMangledName(StringRef Name);
    std::string getMangledName(const GlobalValue *GV);
    void shareStrings(Module &M);

    const std::unique_ptr<TargetMachine> TM;
    const DataLayout DL;

    orc::ExecutionSession ES;
    orc::JITDylib &GlobalJD;
    orc::JITDylib &JD;

    JITDebugInfoRegistry DebugRegistry;

    //Map and inc are guarded by RLST_mutex
    std::mutex RLST_mutex{};
    int RLST_inc = 0;
    DenseMap<void*, std::string> ReverseLocalSymbolTable;

    //Compilation streams
    jl_locked_stream dump_emitted_mi_name_stream;
    jl_locked_stream dump_compiles_stream;
    jl_locked_stream dump_llvm_opt_stream;

    ResourcePool<orc::ThreadSafeContext> ContextPool;

#ifndef JL_USE_JITLINK
    const std::shared_ptr<RTDyldMemoryManager> MemMgr;
#endif
    ObjLayerT ObjectLayer;
    const std::array<std::unique_ptr<PipelineT>, 4> Pipelines;
    OptSelLayerT OptSelLayer;
};
extern JuliaOJIT *jl_ExecutionEngine;
orc::ThreadSafeModule jl_create_llvm_module(StringRef name, orc::ThreadSafeContext ctx, bool imaging_mode, const DataLayout &DL = jl_ExecutionEngine->getDataLayout(), const Triple &triple = jl_ExecutionEngine->getTargetTriple());

orc::ThreadSafeModule &jl_codegen_params_t::shared_module(Module &from) {
    if (!_shared_module) {
        _shared_module = jl_create_llvm_module("globals", tsctx, imaging, from.getDataLayout(), Triple(from.getTargetTriple()));
        assert(&from.getContext() == tsctx.getContext() && "Module context differs from codegen_params context!");
    } else {
        assert(&from.getContext() == _shared_module.getContext().getContext() && "Module context differs from shared module context!");
        assert(from.getDataLayout() == _shared_module.getModuleUnlocked()->getDataLayout() && "Module data layout differs from shared module data layout!");
        assert(from.getTargetTriple() == _shared_module.getModuleUnlocked()->getTargetTriple() && "Module target triple differs from shared module target triple!");
    }
    return _shared_module;
}

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
Pass *createMultiVersioningPass(bool external_use);
Pass *createAllocOptPass();
Pass *createDemoteFloat16Pass();
Pass *createCPUFeaturesPass();
Pass *createLowerSimdLoopPass();

// NewPM
#include "passes.h"

// Whether the Function is an llvm or julia intrinsic.
static inline bool isIntrinsicFunction(Function *F)
{
    return F->isIntrinsic() || F->getName().startswith("julia.");
}

CodeGenOpt::Level CodeGenOptLevelFor(int optlevel);
