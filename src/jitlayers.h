// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <llvm/ADT/MapVector.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Support/AllocatorBase.h>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/PassTimingInfo.h>

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
#include "llvm-codegen-shared.h"
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
// for Mac/aarch64 and Linux/aarch64.
// #define JL_FORCE_JITLINK

#if defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_MSAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_)
# define HAS_SANITIZER
#endif
// The sanitizers don't play well with our memory manager

#if defined(JL_FORCE_JITLINK) || defined(_CPU_AARCH64_) || defined(HAS_SANITIZER)
# define JL_USE_JITLINK
#endif

# include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
# include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>

using namespace llvm;

extern "C" jl_cgparams_t jl_default_cgparams;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeContext, LLVMOrcThreadSafeContextRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeModule, LLVMOrcThreadSafeModuleRef)

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis) JL_NOTSAFEPOINT;
void jl_merge_module(orc::ThreadSafeModule &dest, orc::ThreadSafeModule src) JL_NOTSAFEPOINT;
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M) JL_NOTSAFEPOINT;
DataLayout jl_create_datalayout(TargetMachine &TM) JL_NOTSAFEPOINT;

static inline bool imaging_default() JL_NOTSAFEPOINT {
    return jl_options.image_codegen || (jl_generating_output() && (!jl_options.incremental || jl_options.use_pkgimages));
}

struct OptimizationOptions {
    bool lower_intrinsics;
    bool dump_native;
    bool external_use;
    bool llvm_only;
    bool always_inline;
    bool enable_early_simplifications;
    bool enable_early_optimizations;
    bool enable_scalar_optimizations;
    bool enable_loop_optimizations;
    bool enable_vector_pipeline;
    bool remove_ni;
    bool cleanup;

    static constexpr OptimizationOptions defaults(
        bool lower_intrinsics=true,
        bool dump_native=false,
        bool external_use=false,
        bool llvm_only=false,
        bool always_inline=true,
        bool enable_early_simplifications=true,
        bool enable_early_optimizations=true,
        bool enable_scalar_optimizations=true,
        bool enable_loop_optimizations=true,
        bool enable_vector_pipeline=true,
        bool remove_ni=true,
        bool cleanup=true) {
        return {lower_intrinsics, dump_native, external_use, llvm_only,
                always_inline, enable_early_simplifications,
                enable_early_optimizations, enable_scalar_optimizations,
                enable_loop_optimizations, enable_vector_pipeline,
                remove_ni, cleanup};
    }
};

struct NewPM {
    std::unique_ptr<TargetMachine> TM;
    OptimizationLevel O;
    OptimizationOptions options;
    TimePassesHandler TimePasses;
    NewPM(std::unique_ptr<TargetMachine> TM, OptimizationLevel O, OptimizationOptions options = OptimizationOptions::defaults()) JL_NOTSAFEPOINT;
    ~NewPM() JL_NOTSAFEPOINT;

    void run(Module &M) JL_NOTSAFEPOINT;

    void printTimers() JL_NOTSAFEPOINT;
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
    llvm::FunctionCallee decl;
    llvm::AttributeList attrs;
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

struct jl_codegen_call_target_t {
    jl_returninfo_t::CallingConv cc;
    unsigned return_roots;
    llvm::Function *decl;
    bool specsig;
};

typedef SmallVector<std::pair<jl_code_instance_t*, jl_codegen_call_target_t>, 0> jl_workqueue_t;
// TODO DenseMap?
typedef std::map<jl_code_instance_t*, std::pair<orc::ThreadSafeModule, jl_llvm_functions_t>> jl_compiled_functions_t;

struct jl_codegen_params_t {
    orc::ThreadSafeContext tsctx;
    orc::ThreadSafeContext::Lock tsctx_lock;
    DataLayout DL;
    Triple TargetTriple;

    inline LLVMContext &getContext() {
        return *tsctx.getContext();
    }
    typedef StringMap<GlobalVariable*> SymMapGV;
    // outputs
    jl_workqueue_t workqueue;
    jl_compiled_functions_t compiled_functions;
    std::map<void*, GlobalVariable*> global_targets;
    std::map<std::tuple<jl_code_instance_t*,bool>, GlobalVariable*> external_fns;
    std::map<jl_datatype_t*, DIType*> ditypes;
    std::map<jl_datatype_t*, Type*> llvmtypes;
    DenseMap<Constant*, GlobalVariable*> mergedConstants;
    // Map from symbol name (in a certain library) to its GV in sysimg and the
    // DL handle address in the current session.
    StringMap<std::pair<GlobalVariable*,SymMapGV>> libMapGV;
    SymMapGV symMapDefault;
    // These symMaps are Windows-only
    SymMapGV symMapExe;
    SymMapGV symMapDll;
    SymMapGV symMapDlli;
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
    inline Module &shared_module();
    // inputs
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool cache = false;
    bool external_linkage = false;
    bool imaging_mode;
    int debug_level;
    jl_codegen_params_t(orc::ThreadSafeContext ctx, DataLayout DL, Triple triple)
        : tsctx(std::move(ctx)), tsctx_lock(tsctx.getLock()),
            DL(std::move(DL)), TargetTriple(std::move(triple)), imaging_mode(imaging_default()) {}
};

jl_llvm_functions_t jl_emit_code(
        orc::ThreadSafeModule &M,
        jl_method_instance_t *mi,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params,
        size_t min_world, size_t max_world);

jl_llvm_functions_t jl_emit_codeinst(
        orc::ThreadSafeModule &M,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params);

enum CompilationPolicy {
    Default = 0,
    Extern = 1,
};

void jl_compile_workqueue(
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

template <size_t offset = 0>
class MaxAlignedAllocImpl
    : public AllocatorBase<MaxAlignedAllocImpl<offset>> {

public:
    MaxAlignedAllocImpl() JL_NOTSAFEPOINT = default;

    static Align alignment(size_t Size) JL_NOTSAFEPOINT {
        // Define the maximum alignment we expect to require, from offset bytes off
        // the returned pointer, this is >= alignof(std::max_align_t), which is too
        // small often to actually use.
        const size_t MaxAlignment = JL_CACHE_BYTE_ALIGNMENT;
        if (Size <= offset)
            return Align(1);
        return Align(std::min((size_t)llvm::PowerOf2Ceil(Size - offset), MaxAlignment));
    }

    LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t Size, Align Alignment) {
        Align MaxAlign = alignment(Size);
        assert(Alignment < MaxAlign); (void)Alignment;
        return jl_gc_perm_alloc(Size, 0, MaxAlign.value(), offset);
    }

    inline LLVM_ATTRIBUTE_RETURNS_NONNULL
    void * Allocate(size_t Size, size_t Alignment) {
        return Allocate(Size, Align(Alignment));
    }

    // Pull in base class overloads.
    using AllocatorBase<MaxAlignedAllocImpl>::Allocate;

    void Deallocate(const void *Ptr, size_t Size, size_t /*Alignment*/) { abort(); }

    // Pull in base class overloads.
    using AllocatorBase<MaxAlignedAllocImpl>::Deallocate;

private:
};
using MaxAlignedAlloc = MaxAlignedAllocImpl<>;

#if JL_LLVM_VERSION < 170000
typedef JITSymbol JL_JITSymbol;
// The type that is similar to SymbolInfo on LLVM 4.0 is actually
// `JITEvaluatedSymbol`. However, we only use this type when a JITSymbol
// is expected.
typedef JITSymbol JL_SymbolInfo;
#endif

using CompilerResultT = Expected<std::unique_ptr<llvm::MemoryBuffer>>;
using OptimizerResultT = Expected<orc::ThreadSafeModule>;
using SharedBytesT = StringSet<MaxAlignedAllocImpl<sizeof(StringSet<>::MapEntryTy)>>;

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
            JL_TIMING(LLVM_JIT, JIT_Link);
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
    typedef orc::IRTransformLayer JITPointersLayerT;
    typedef orc::IRTransformLayer OptimizeLayerT;
    typedef orc::IRTransformLayer OptSelLayerT;
    typedef orc::IRTransformLayer DepsVerifyLayerT;
    typedef object::OwningBinary<object::ObjectFile> OwningObj;
    template
    <typename ResourceT, size_t max = 0,
        typename BackingT = std::stack<ResourceT,
            std::conditional_t<max == 0,
                SmallVector<ResourceT, 0>,
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
            Optional<ResourceT> resource;
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

    struct DLSymOptimizer;

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

    //Methods for the C API
    Error addExternalModule(orc::JITDylib &JD, orc::ThreadSafeModule TSM,
                            bool ShouldOptimize = false) JL_NOTSAFEPOINT;
    Error addObjectFile(orc::JITDylib &JD,
                        std::unique_ptr<MemoryBuffer> Obj) JL_NOTSAFEPOINT;
    orc::IRCompileLayer &getIRCompileLayer() JL_NOTSAFEPOINT { return ExternalCompileLayer; };
    orc::ExecutionSession &getExecutionSession() JL_NOTSAFEPOINT { return ES; }
    orc::JITDylib &getExternalJITDylib() JL_NOTSAFEPOINT { return ExternalJD; }

    #if JL_LLVM_VERSION >= 170000
    Expected<llvm::orc::ExecutorSymbolDef> findSymbol(StringRef Name, bool ExportedSymbolsOnly) JL_NOTSAFEPOINT;
    Expected<llvm::orc::ExecutorSymbolDef> findUnmangledSymbol(StringRef Name) JL_NOTSAFEPOINT;
    Expected<llvm::orc::ExecutorSymbolDef> findExternalJDSymbol(StringRef Name, bool ExternalJDOnly) JL_NOTSAFEPOINT;
    #else
    JITEvaluatedSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) JL_NOTSAFEPOINT;
    JITEvaluatedSymbol findUnmangledSymbol(StringRef Name) JL_NOTSAFEPOINT;
    Expected<JITEvaluatedSymbol> findExternalJDSymbol(StringRef Name, bool ExternalJDOnly) JL_NOTSAFEPOINT;
    #endif
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
    void printTimers() JL_NOTSAFEPOINT;

    jl_locked_stream &get_dump_emitted_mi_name_stream() JL_NOTSAFEPOINT {
        return dump_emitted_mi_name_stream;
    }
    jl_locked_stream &get_dump_compiles_stream() JL_NOTSAFEPOINT {
        return dump_compiles_stream;
    }
    jl_locked_stream &get_dump_llvm_opt_stream() JL_NOTSAFEPOINT {
        return dump_llvm_opt_stream;
    }
    std::string getMangledName(StringRef Name) JL_NOTSAFEPOINT;
    std::string getMangledName(const GlobalValue *GV) JL_NOTSAFEPOINT;

    // Note that this is a safepoint due to jl_get_library_ and jl_dlsym calls
    void optimizeDLSyms(Module &M);

private:

    const std::unique_ptr<TargetMachine> TM;
    const DataLayout DL;

    orc::ExecutionSession ES;
    orc::JITDylib &GlobalJD;
    orc::JITDylib &JD;
    orc::JITDylib &ExternalJD;
    //Map and inc are guarded by RLST_mutex
    std::mutex RLST_mutex{};
    int RLST_inc = 0;
    DenseMap<void*, std::string> ReverseLocalSymbolTable;
    SharedBytesT SharedBytes;

    std::unique_ptr<DLSymOptimizer> DLSymOpt;

    //Compilation streams
    jl_locked_stream dump_emitted_mi_name_stream;
    jl_locked_stream dump_compiles_stream;
    jl_locked_stream dump_llvm_opt_stream;

    std::mutex llvm_printing_mutex{};
    SmallVector<std::function<void()>, 0> PrintLLVMTimers;

    ResourcePool<orc::ThreadSafeContext, 0, std::queue<orc::ThreadSafeContext>> ContextPool;

#ifndef JL_USE_JITLINK
    const std::shared_ptr<RTDyldMemoryManager> MemMgr;
#else
    std::atomic<size_t> total_size{0};
    const std::unique_ptr<jitlink::JITLinkMemoryManager> MemMgr;
#endif
    ObjLayerT ObjectLayer;
    LockLayerT LockLayer;
    CompileLayerT CompileLayer;
    JITPointersLayerT JITPointersLayer;
    OptimizeLayerT OptimizeLayer;
    OptSelLayerT OptSelLayer;
    DepsVerifyLayerT DepsVerifyLayer;
    CompileLayerT ExternalCompileLayer;
};
extern JuliaOJIT *jl_ExecutionEngine;
std::unique_ptr<Module> jl_create_llvm_module(StringRef name, LLVMContext &ctx, const DataLayout &DL = jl_ExecutionEngine->getDataLayout(), const Triple &triple = jl_ExecutionEngine->getTargetTriple()) JL_NOTSAFEPOINT;
inline orc::ThreadSafeModule jl_create_ts_module(StringRef name, orc::ThreadSafeContext ctx, const DataLayout &DL = jl_ExecutionEngine->getDataLayout(), const Triple &triple = jl_ExecutionEngine->getTargetTriple()) JL_NOTSAFEPOINT {
    auto lock = ctx.getLock();
    return orc::ThreadSafeModule(jl_create_llvm_module(name, *ctx.getContext(), DL, triple), ctx);
}

Module &jl_codegen_params_t::shared_module() JL_NOTSAFEPOINT {
    if (!_shared_module) {
        _shared_module = jl_create_llvm_module("globals", getContext(), DL, TargetTriple);
    }
    return *_shared_module;
}
void fixupTM(TargetMachine &TM) JL_NOTSAFEPOINT;

#if JL_LLVM_VERSION < 170000
void SetOpaquePointer(LLVMContext &ctx) JL_NOTSAFEPOINT;
#endif

void optimizeDLSyms(Module &M);

// NewPM
#include "passes.h"

CodeGenOpt::Level CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
