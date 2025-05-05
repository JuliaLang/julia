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
#include "llvm-version.h"
#include <stack>
#include <queue>
#include <tuple>

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
//#define JL_FORCE_JITLINK

#if defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_MSAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_)
# define HAS_SANITIZER
#endif
// The sanitizers don't play well with our memory manager

#if defined(JL_FORCE_JITLINK) || defined(_CPU_AARCH64_) || defined(HAS_SANITIZER)
# define JL_USE_JITLINK
#endif

#if defined(_CPU_RISCV64_)
# define JL_USE_JITLINK
#endif

# include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
# include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>

using namespace llvm;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeContext, LLVMOrcThreadSafeContextRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeModule, LLVMOrcThreadSafeModuleRef)

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis) JL_NOTSAFEPOINT;
void jl_merge_module(orc::ThreadSafeModule &dest, orc::ThreadSafeModule src) JL_NOTSAFEPOINT;
GlobalVariable *jl_emit_RTLD_DEFAULT_var(Module *M) JL_NOTSAFEPOINT;
DataLayout jl_create_datalayout(TargetMachine &TM) JL_NOTSAFEPOINT;

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
    bool warn_missed_transformations;

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
        bool cleanup=true,
        bool warn_missed_transformations=false) {
        return {lower_intrinsics, dump_native, external_use, llvm_only,
                always_inline, enable_early_simplifications,
                enable_early_optimizations, enable_scalar_optimizations,
                enable_loop_optimizations, enable_vector_pipeline,
                remove_ni, cleanup, warn_missed_transformations};
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

        lock(std::mutex &mutex, ios_t *&stream) JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER
            : lck(mutex), stream(stream) {}
        lock(lock&) = delete;
        lock(lock&&) JL_NOTSAFEPOINT = default;
        ~lock() JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT = default;

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

    jl_locked_stream() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER = default;
    ~jl_locked_stream() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE = default;

    lock operator*() JL_NOTSAFEPOINT {
        return lock(mutex, stream);
    }
};

struct jl_llvm_functions_t {
    std::string functionObject;     // jlcall llvm Function name
    std::string specFunctionObject; // specialized llvm Function name
    jl_llvm_functions_t() JL_NOTSAFEPOINT = default;
    jl_llvm_functions_t &operator=(const jl_llvm_functions_t&) JL_NOTSAFEPOINT = default;
    jl_llvm_functions_t(const jl_llvm_functions_t &) JL_NOTSAFEPOINT = default;
    jl_llvm_functions_t(jl_llvm_functions_t &&) JL_NOTSAFEPOINT = default;
    ~jl_llvm_functions_t() JL_NOTSAFEPOINT = default;
};

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
    llvm::Function *oc;
    bool specsig;
};

// reification of a call to jl_jit_abi_convert, so that it isn't necessary to parse the Modules to recover this info
struct cfunc_decl_t {
    jl_value_t *declrt;
    jl_value_t *sigt;
    size_t nargs;
    bool specsig;
    llvm::GlobalVariable *theFptr;
    llvm::GlobalVariable *cfuncdata;
};

typedef SmallVector<std::pair<jl_code_instance_t*, jl_codegen_call_target_t>, 0> jl_workqueue_t;

typedef std::list<std::tuple<std::string, std::string, unsigned int>> CallFrames;
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
    SmallVector<cfunc_decl_t,0> cfuncs;
    std::map<void*, GlobalVariable*> global_targets;
    jl_array_t *temporary_roots = nullptr;
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
    bool use_swiftcc = true;
    jl_codegen_params_t(orc::ThreadSafeContext ctx, DataLayout DL, Triple triple) JL_NOTSAFEPOINT  JL_NOTSAFEPOINT_ENTER
      : tsctx(std::move(ctx)),
        tsctx_lock(tsctx.getLock()),
        DL(std::move(DL)),
        TargetTriple(std::move(triple)),
        imaging_mode(1)
    {
        // LLVM's RISC-V back-end currently does not support the Swift calling convention
        if (TargetTriple.isRISCV())
            use_swiftcc = false;
    }
    jl_codegen_params_t(jl_codegen_params_t &&) JL_NOTSAFEPOINT = default;
    ~jl_codegen_params_t() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_LEAVE = default;
};

const char *jl_generate_ccallable(Module *llvmmod, jl_value_t *nameval, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params);

jl_llvm_functions_t jl_emit_code(
        orc::ThreadSafeModule &M,
        jl_method_instance_t *mi,
        jl_code_info_t *src,
        jl_value_t *abi_at,
        jl_value_t *abi_rt,
        jl_codegen_params_t &params);

jl_llvm_functions_t jl_emit_codeinst(
        orc::ThreadSafeModule &M,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params);

jl_llvm_functions_t jl_emit_codedecls(
        orc::ThreadSafeModule &M,
        jl_code_instance_t *codeinst,
        jl_codegen_params_t &params);

enum CompilationPolicy {
    Default = 0,
    Extern = 1,
};

Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt,
    jl_codegen_params_t &params);

extern "C" JL_DLLEXPORT_CODEGEN
void *jl_jit_abi_convert(jl_task_t *ct, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, _Atomic(void*) *fptr, _Atomic(size_t) *last_world, void *data);
std::string emit_abi_dispatcher(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_code_instance_t *codeinst, Value *invoke);
std::string emit_abi_converter(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_code_instance_t *codeinst, Value *target, bool target_specsig);
std::string emit_abi_constreturn(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_value_t *rettype_const);
std::string emit_abi_constreturn(Module *M, jl_codegen_params_t &params, bool specsig, jl_code_instance_t *codeinst);

Function *emit_tojlinvoke(jl_code_instance_t *codeinst, StringRef theFptrName, Module *M, jl_codegen_params_t &params) JL_NOTSAFEPOINT;
void emit_specsig_to_fptr1(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype, bool is_for_opaque_closure,
        size_t nargs,
        jl_codegen_params_t &params,
        Function *target) JL_NOTSAFEPOINT;
Function *get_or_emit_fptr1(StringRef Name, Module *M) JL_NOTSAFEPOINT;
void jl_init_function(Function *F, const Triple &TT) JL_NOTSAFEPOINT;

void add_named_global(StringRef name, void *addr) JL_NOTSAFEPOINT;

Constant *literal_pointer_val_slot(jl_codegen_params_t &params, Module *M, jl_value_t *p);

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

using CompilerResultT = Expected<std::unique_ptr<llvm::MemoryBuffer>>;
using OptimizerResultT = Expected<orc::ThreadSafeModule>;
using SharedBytesT = StringSet<MaxAlignedAllocImpl<sizeof(StringSet<>::MapEntryTy)>>;

class JuliaOJIT {
private:
    // any verification the user wants to do when adding an OwningResource to the pool
    template <typename AnyT>
    static void verifyResource(AnyT &resource) JL_NOTSAFEPOINT { }
    static void verifyResource(orc::ThreadSafeContext &context) JL_NOTSAFEPOINT { assert(context.getContext()); }
public:
#ifdef JL_USE_JITLINK
    typedef orc::ObjectLinkingLayer ObjLayerT;
#else
    typedef orc::RTDyldObjectLinkingLayer ObjLayerT;
    struct LockLayerT : public orc::ObjectLayer {

        LockLayerT(orc::ObjectLayer &BaseLayer) JL_NOTSAFEPOINT : orc::ObjectLayer(BaseLayer.getExecutionSession()), BaseLayer(BaseLayer) {}
        ~LockLayerT() JL_NOTSAFEPOINT = default;

        void emit(std::unique_ptr<orc::MaterializationResponsibility> R,
                            std::unique_ptr<MemoryBuffer> O) override {
            JL_TIMING(LLVM_JIT, JIT_Link);
#ifndef JL_USE_JITLINK
            std::lock_guard<std::recursive_mutex> lock(EmissionMutex);
#endif
            BaseLayer.emit(std::move(R), std::move(O));
        }
    private:
        orc::ObjectLayer &BaseLayer;
        std::recursive_mutex EmissionMutex;
    };
#endif
    typedef orc::IRCompileLayer CompileLayerT;
    typedef orc::IRTransformLayer JITPointersLayerT;
    typedef orc::IRTransformLayer OptimizeLayerT;
    typedef orc::IRTransformLayer OptSelLayerT;
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
            OwningResource(OwningResource &&other) JL_NOTSAFEPOINT
                : pool(other.pool), resource(std::move(other.resource)) {
                    other.resource.reset();
                }
            OwningResource &operator=(OwningResource &&) JL_NOTSAFEPOINT = default;
            ~OwningResource() JL_NOTSAFEPOINT { // _LEAVE
                if (resource) {
                    verifyResource(*resource);
                    pool.release(std::move(*resource));
                }
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
            std::optional<ResourceT> resource;
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

    typedef ResourcePool<orc::ThreadSafeContext, 0, std::queue<orc::ThreadSafeContext>> ContextPoolT;

    struct DLSymOptimizer;
    struct OptimizerT;
    struct JITPointersT;

#ifndef JL_USE_JITLINK
    void RegisterJITEventListener(JITEventListener *L) JL_NOTSAFEPOINT;
#endif

public:

    JuliaOJIT() JL_NOTSAFEPOINT;
    ~JuliaOJIT() JL_NOTSAFEPOINT;

    void enableJITDebuggingSupport() JL_NOTSAFEPOINT;
    void enableIntelJITEventListener() JL_NOTSAFEPOINT;
    void enableOProfileJITEventListener() JL_NOTSAFEPOINT;
    void enablePerfJITEventListener() JL_NOTSAFEPOINT;

    orc::SymbolStringPtr mangle(StringRef Name) JL_NOTSAFEPOINT;
    void addGlobalMapping(StringRef Name, uint64_t Addr) JL_NOTSAFEPOINT;
    void addModule(orc::ThreadSafeModule M) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER;

    //Methods for the C API
    Error addExternalModule(orc::JITDylib &JD, orc::ThreadSafeModule TSM,
                            bool ShouldOptimize = false) JL_NOTSAFEPOINT;
    Error addObjectFile(orc::JITDylib &JD,
                        std::unique_ptr<MemoryBuffer> Obj) JL_NOTSAFEPOINT;
    orc::IRCompileLayer &getIRCompileLayer() JL_NOTSAFEPOINT { return CompileLayer; };
    orc::ExecutionSession &getExecutionSession() JL_NOTSAFEPOINT { return ES; }
    orc::JITDylib &getExternalJITDylib() JL_NOTSAFEPOINT { return ExternalJD; }

    Expected<llvm::orc::ExecutorSymbolDef> findSymbol(StringRef Name, bool ExportedSymbolsOnly) JL_NOTSAFEPOINT;
    Expected<llvm::orc::ExecutorSymbolDef> findUnmangledSymbol(StringRef Name) JL_NOTSAFEPOINT;
    Expected<llvm::orc::ExecutorSymbolDef> findExternalJDSymbol(StringRef Name, bool ExternalJDOnly) JL_NOTSAFEPOINT;
    SmallVector<uint64_t> findSymbols(ArrayRef<StringRef> Names) JL_NOTSAFEPOINT;
    uint64_t getGlobalValueAddress(StringRef Name) JL_NOTSAFEPOINT;
    uint64_t getFunctionAddress(StringRef Name) JL_NOTSAFEPOINT;
    StringRef getFunctionAtAddress(uint64_t Addr, jl_callptr_t invoke, jl_code_instance_t *codeinst) JL_NOTSAFEPOINT;
    orc::ThreadSafeContext makeContext() JL_NOTSAFEPOINT;
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
    void addBytes(size_t bytes) JL_NOTSAFEPOINT;
    void printTimers() JL_NOTSAFEPOINT;

    jl_locked_stream &get_dump_emitted_mi_name_stream() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER {
        return dump_emitted_mi_name_stream;
    }
    jl_locked_stream &get_dump_compiles_stream() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER {
        return dump_compiles_stream;
    }
    jl_locked_stream &get_dump_llvm_opt_stream() JL_NOTSAFEPOINT JL_NOTSAFEPOINT_ENTER {
        return dump_llvm_opt_stream;
    }
    std::string getMangledName(StringRef Name) JL_NOTSAFEPOINT;
    std::string getMangledName(const GlobalValue *GV) JL_NOTSAFEPOINT;

    // Note that this is a potential safepoint due to jl_get_library_ and jl_dlsym calls
    // but may be called from inside safe-regions due to jit compilation locks
    void optimizeDLSyms(Module &M) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER;

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

    _Atomic(size_t) jit_bytes_size{0};
    _Atomic(size_t) jitcounter{0};
#ifdef JL_USE_JITLINK
    const std::unique_ptr<jitlink::JITLinkMemoryManager> MemMgr;
    ObjLayerT ObjectLayer;
#else
    const std::shared_ptr<RTDyldMemoryManager> MemMgr; // shared_ptr protected by LockLayerT.EmissionMutex
    ObjLayerT UnlockedObjectLayer;
    LockLayerT ObjectLayer;
#endif
    CompileLayerT CompileLayer;
    std::unique_ptr<JITPointersT> JITPointers;
    JITPointersLayerT JITPointersLayer;
    std::unique_ptr<OptimizerT> Optimizers;
    OptimizeLayerT OptimizeLayer;
    OptSelLayerT OptSelLayer;
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

void optimizeDLSyms(Module &M) JL_NOTSAFEPOINT_LEAVE JL_NOTSAFEPOINT_ENTER;

// NewPM
#include "passes.h"

#if JL_LLVM_VERSION >= 180000
CodeGenOptLevel CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
#else
CodeGenOpt::Level CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
#endif
