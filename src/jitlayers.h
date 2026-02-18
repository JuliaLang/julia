// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm/ADT/SmallSet.h"
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
// JITLink is now used on all platforms by default.  The support for RuntimeDyld
// will be removed when we need the ability to manipulate JITLink LinkGraphs.
//
// Of the supported profilers, only OProfile has not been ported to JITLink.

#if defined(_COMPILER_ASAN_ENABLED_) || defined(_COMPILER_MSAN_ENABLED_) || defined(_COMPILER_TSAN_ENABLED_)
# define HAS_SANITIZER
#endif

#ifndef JL_USE_OPROFILE_JITEVENTS
#define JL_USE_JITLINK
#endif

# include <llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h>
# include <llvm/ExecutionEngine/RTDyldMemoryManager.h>
# include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>

using namespace llvm;

inline int jl_is_timing_passes = 0;
inline int jl_is_timing_trace = 0;
inline unsigned jl_timing_trace_granularity = 500;
inline std::string jl_timing_trace_file;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeContext, LLVMOrcThreadSafeContextRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(orc::ThreadSafeModule, LLVMOrcThreadSafeModuleRef)

void addTargetPasses(legacy::PassManagerBase *PM, const Triple &triple, TargetIRAnalysis analysis) JL_NOTSAFEPOINT;
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
    bool sanitize_memory;
    bool sanitize_thread;
    bool sanitize_address;

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
        bool warn_missed_transformations=false,
#ifdef _COMPILER_MSAN_ENABLED_
        bool sanitize_memory=true,
#else
        bool sanitize_memory=false,
#endif
#ifdef _COMPILER_TSAN_ENABLED_
        bool sanitize_thread=true,
#else
        bool sanitize_thread=false,
#endif
#ifdef _COMPILER_ASAN_ENABLED_
        bool sanitize_address=true
#else
        bool sanitize_address=false
#endif
) JL_NOTSAFEPOINT {
        return {lower_intrinsics, dump_native, external_use, llvm_only,
                always_inline, enable_early_simplifications,
                enable_early_optimizations, enable_scalar_optimizations,
                enable_loop_optimizations, enable_vector_pipeline,
                remove_ni, cleanup, warn_missed_transformations,
                sanitize_memory, sanitize_thread, sanitize_address};
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

// jl_codeinst_funcs_t holds the results of compiling a CodeInstance, which can
// produce one, two, or zero entrypoints.  The `invoke_api` field determines
// what the CodeInstance's `invoke` should be set to, and whether `invoke` and
// `specptr` are compiled functions.
//
// JL_INVOKE_ARGS
//   specptr: jl_fptr_args_t convention
// JL_INVOKE_CONST
//   (no compiled functions)
// JL_INVOKE_SPARAM
//   specptr: jl_fptr_sparam_t convention
// JL_INVOKE_INTERPRETED
//   (not produced by compilation)
// JL_INVOKE_SPECSIG
//   invoke:  jfptr_* wrapper around specptr
//   specptr: specsig function
template <typename T>
struct jl_codeinst_funcs_t {
    jl_invoke_api_t invoke_api;
    T invoke;
    T specptr;
    jl_codeinst_funcs_t() JL_NOTSAFEPOINT = default;
    jl_codeinst_funcs_t &operator=(const jl_codeinst_funcs_t&) JL_NOTSAFEPOINT = default;
    jl_codeinst_funcs_t(const jl_codeinst_funcs_t &) JL_NOTSAFEPOINT = default;
    jl_codeinst_funcs_t(jl_codeinst_funcs_t &&) JL_NOTSAFEPOINT = default;
    ~jl_codeinst_funcs_t() JL_NOTSAFEPOINT = default;
};

using jl_llvm_functions_t = jl_codeinst_funcs_t<Function *>;

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
    bool all_roots;
};

struct jl_codegen_call_target_t {
    llvm::Function *decl;
    bool external_linkage; // whether codegen would like this edge to be externally-available
    bool private_linkage; // whether codegen would like this edge to be internally-available
    // external = ExternalLinkage (similar to "extern")
    // private = InternalLinkage (similar to "static")
    // external+private = AvailableExternallyLinkage+ExternalLinkage or ExternalLinkage (similar to "static inline")
    // neither = unused
};

// reification of a call to jl_jit_abi_convert, so that it isn't necessary to parse the Modules to recover this info
struct cfunc_decl_t {
    jl_abi_t abi;
    llvm::GlobalVariable *cfuncdata;
};

std::unique_ptr<Module> jl_create_llvm_module(StringRef name, LLVMContext &ctx,
                                              const DataLayout &DL, const Triple &triple,
                                              Module *source = nullptr) JL_NOTSAFEPOINT;

typedef std::list<std::tuple<std::string, std::string, unsigned int>> CallFrames;

class jl_name_counter_t {
public:
    template<class... Ts>
    std::string operator()(Ts... args) JL_NOTSAFEPOINT
    {
        std::string name;
        raw_string_ostream s{name};
        (s << ... << args);
        unsigned n = counter[name]++;
        s << n;
        return name;
    }

    jl_name_counter_t() JL_NOTSAFEPOINT = default;
    jl_name_counter_t(jl_name_counter_t &&) JL_NOTSAFEPOINT = default;
    ~jl_name_counter_t() JL_NOTSAFEPOINT = default;

private:
    StringMap<unsigned> counter;
};

struct jl_linker_info_t {
    DenseMap<jl_code_instance_t *, jl_codeinst_funcs_t<orc::SymbolStringPtr>> ci_funcs;
    DenseMap<std::pair<jl_code_instance_t *, jl_invoke_api_t>, orc::SymbolStringPtr>
        call_targets;
    DenseMap<void *, orc::SymbolStringPtr> global_targets;
};

struct jl_emitted_output_t {
    orc::ThreadSafeModule module;
    std::unique_ptr<jl_linker_info_t> linker_info;

    jl_emitted_output_t() JL_NOTSAFEPOINT = default;
    jl_emitted_output_t(jl_emitted_output_t &&) JL_NOTSAFEPOINT = default;
    jl_emitted_output_t &operator=(jl_emitted_output_t &&) JL_NOTSAFEPOINT = default;
    ~jl_emitted_output_t() JL_NOTSAFEPOINT = default;
};

// A jl_codegen_output_t is the target for LLVM IR generation, containing an
// LLVM module and the metadata for linking it into the current session or a
// system image.  Many code instances can be emitted to a single codegen output.
class jl_codegen_output_t {
private:
    orc::ThreadSafeModule owned_TSM;
    orc::ThreadSafeModule *TSM;
    orc::ThreadSafeContext::Lock tsctx_lock;

    jl_name_counter_t names;

public:
    LLVMContext &get_context() { return *get_tsm().getContext().getContext(); }
    Module &get_module() { return *get_tsm().getModuleUnlocked(); }
    orc::ThreadSafeModule &get_tsm() { return owned_TSM ? owned_TSM : *TSM; }
    void lock() { tsctx_lock = get_tsm().getContext().getLock(); }
    void unlock() { auto _ = std::move(tsctx_lock); }

    StringRef strip_linux(StringRef name);
    std::string make_name(jl_symbol_prefix_t type, jl_invoke_api_t api,
                          StringRef orig_name);
    std::string make_name(StringRef prefix, StringRef orig_name);
    std::string make_name(StringRef orig_name);

    StringRef get_call_target(jl_code_instance_t *ci, bool specsig, bool always_inline);

    // Discard all the context that will be invalidated when we compile the
    // module.  Must hold the context lock.
    jl_emitted_output_t finish(orc::SymbolStringPool &SSP) JL_NOTSAFEPOINT;

public:
    // outputs
    DenseMap<std::pair<jl_code_instance_t *, jl_invoke_api_t>, jl_codegen_call_target_t>
        call_targets;
    DenseMap<jl_code_instance_t *, jl_llvm_functions_t> ci_funcs;
    SmallVector<std::pair<jl_code_instance_t *, GlobalVariable *>, 0> external_fns;

    SmallVector<cfunc_decl_t,0> cfuncs;
    std::map<void*, GlobalVariable*> global_targets;
    jl_array_t *temporary_roots = nullptr;
    SmallSet<jl_value_t *, 8> temporary_roots_set;
    std::map<jl_datatype_t*, DIType*> ditypes;
    std::map<jl_datatype_t*, Type*> llvmtypes;
    DenseMap<Constant*, GlobalVariable*> mergedConstants;
    // Map from symbol name (in a certain library) to its GV in sysimg and the
    // DL handle address in the current session.
    typedef StringMap<GlobalVariable *> SymMapGV;
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
    SmallVector<std::unique_ptr<Module>, 0> llvmcall_modules;

    // inputs
    const DataLayout &DL;
    Triple TargetTriple;
    const jl_cgparams_t *params = &jl_default_cgparams;
    bool external_linkage = false;
    bool imaging_mode = true;
    bool safepoint_on_entry = true;
    bool use_swiftcc = true;

    jl_codegen_output_t(orc::ThreadSafeModule &TSM)
      : TSM(&TSM),
        tsctx_lock(TSM.getContext().getLock()),
        DL(TSM.getModuleUnlocked()->getDataLayout()),
        TargetTriple(TSM.getModuleUnlocked()->getTargetTriple())
    {
        if (TargetTriple.isRISCV())
            use_swiftcc = false;
    }

    static orc::ThreadSafeModule create_ts_module(StringRef name, const DataLayout &DL,
                                                  const Triple &triple)
    {
        auto ctx = std::make_unique<LLVMContext>();
        auto M = jl_create_llvm_module(name, *ctx, DL, triple);
        return orc::ThreadSafeModule(std::move(M), std::move(ctx));
    }

    jl_codegen_output_t(StringRef name, const DataLayout &DL, const Triple &triple)
      : owned_TSM(create_ts_module(name, DL, triple)),
        TSM(nullptr),
        tsctx_lock(owned_TSM.getContext().getLock()),
        DL(DL),
        TargetTriple(triple)
    {
        if (TargetTriple.isRISCV())
            use_swiftcc = false;
    }

    jl_codegen_output_t(jl_codegen_output_t &&) JL_NOTSAFEPOINT = default;
    ~jl_codegen_output_t() JL_NOTSAFEPOINT = default;
};

const char *jl_generate_ccallable(jl_codegen_output_t &out, jl_value_t *nameval, jl_value_t *declrt, jl_value_t *sigt);

std::optional<jl_llvm_functions_t> jl_emit_code(
        jl_codegen_output_t &out,
        jl_method_instance_t *mi,
        jl_code_info_t *src,
        jl_value_t *abi_at,
        jl_value_t *abi_rt);

std::optional<jl_llvm_functions_t> jl_emit_codeinst(
        jl_codegen_output_t &out,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src);

jl_llvm_functions_t jl_emit_codedecls(
        jl_codegen_output_t &out,
        jl_code_instance_t *codeinst);

jl_code_info_t *jl_get_method_ir(jl_code_instance_t *ci);
void emit_always_inline(jl_codegen_output_t &out,
                        unique_function<jl_code_info_t *(jl_code_instance_t *)> get_src);
void emit_llvmcall_modules(jl_codegen_output_t &out);

enum CompilationPolicy {
    Default = 0,
    Extern = 1,
};

Function *jl_cfunction_object(jl_value_t *f, jl_value_t *rt, jl_tupletype_t *argt,
    jl_codegen_output_t &out);

extern "C" JL_DLLEXPORT_CODEGEN
void *jl_jit_abi_convert(jl_task_t *ct, jl_abi_t from_abi, _Atomic(void*) *fptr, _Atomic(size_t) *last_world, void *data);
std::string emit_abi_dispatcher(jl_codegen_output_t &out, jl_abi_t from_abi, jl_code_instance_t *codeinst, Value *invoke);
std::string emit_abi_converter(jl_codegen_output_t &out, jl_abi_t from_abi, jl_code_instance_t *codeinst, Value *target, bool target_specsig);
std::string emit_abi_constreturn(jl_codegen_output_t &out, jl_abi_t from_abi, jl_value_t *rettype_const);
std::string emit_abi_constreturn(jl_codegen_output_t &out, bool specsig, jl_code_instance_t *codeinst);

Function *emit_tojlinvoke(jl_code_instance_t *codeinst, StringRef theFptrName, jl_codegen_output_t &out) JL_NOTSAFEPOINT;
void emit_specsig_to_fptr1(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype, bool is_for_opaque_closure,
        size_t nargs,
        jl_codegen_output_t &out,
        Value *target) JL_NOTSAFEPOINT;
Function *emit_specsig_to_fptr1(jl_codegen_output_t &out, jl_code_instance_t *ci,
                                Value *func) JL_NOTSAFEPOINT;
Function *get_or_emit_fptr1(StringRef Name, Module *M) JL_NOTSAFEPOINT;
void jl_init_function(Function *F, const jl_codegen_output_t &params) JL_NOTSAFEPOINT;

jl_returninfo_t get_specsig_function(jl_codegen_output_t &ctx, Module *M, Value *fval,
                                     StringRef name, jl_value_t *sig, jl_value_t *jlrettype,
                                     bool is_opaque_closure,
                                     ArrayRef<const char *> ArgNames = {},
                                     unsigned nreq = 0);

void add_named_global(StringRef name, void *addr) JL_NOTSAFEPOINT;

Constant *literal_pointer_val_slot(jl_codegen_output_t &out, jl_value_t *p);

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

static inline jl_value_t *get_ci_abi(jl_code_instance_t *ci)
{
    if (jl_typeof(ci->def) == (jl_value_t*)jl_abioverride_type)
        return ((jl_abi_override_t*)ci->def)->abi;
    return jl_get_ci_mi(ci)->specTypes;
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

using CISymbolPtr = jl_codeinst_funcs_t<orc::SymbolStringPtr>;
using CISymbolMap = DenseMap<jl_code_instance_t *, CISymbolPtr>;

class JLMaterializationUnit;
class JLTrampolineMaterializationUnit;

struct JITObjectInfo {
    std::unique_ptr<MemoryBuffer> BackingBuffer;
    std::unique_ptr<object::ObjectFile> Object;
    StringMap<uint64_t> SectionLoadAddresses;
    std::unique_ptr<jl_linker_info_t> LinkerInfo;
};

class JLDebuginfoPlugin : public orc::ObjectLinkingLayer::Plugin {
    std::mutex PluginMutex;
    std::map<orc::MaterializationResponsibility *, std::unique_ptr<JITObjectInfo>> PendingObjs;
public:
    void notifyMaterializingWithInfo(orc::MaterializationResponsibility &MR,
                                     jitlink::LinkGraph &G, MemoryBufferRef InputObject,
                                     std::unique_ptr<jl_linker_info_t> LinkerInfo)
        JL_NOTSAFEPOINT;
    Error notifyEmitted(orc::MaterializationResponsibility &MR) override;
    Error notifyFailed(orc::MaterializationResponsibility &MR) override;
    Error notifyRemovingResources(orc::JITDylib &JD, orc::ResourceKey K) override;
    void notifyTransferringResources(orc::JITDylib &JD, orc::ResourceKey DstKey,
                                     orc::ResourceKey SrcKey) override;
    void modifyPassConfig(orc::MaterializationResponsibility &MR, jitlink::LinkGraph &,
                          jitlink::PassConfiguration &PassConfig) override;
};

class JuliaOJIT {
    friend JLMaterializationUnit;
    friend JLTrampolineMaterializationUnit;
private:
    // any verification the user wants to do when adding an OwningResource to the pool
    template <typename AnyT>
    static void verifyResource(AnyT &resource) JL_NOTSAFEPOINT { }
    static void verifyResource(orc::ThreadSafeContext &context) JL_NOTSAFEPOINT { assert(context.getContext()); }
public:
    typedef orc::ObjectLinkingLayer ObjLayerT;
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

public:

    JuliaOJIT() JL_NOTSAFEPOINT;
    ~JuliaOJIT() JL_NOTSAFEPOINT;

    void enableJITDebuggingSupport();
    void enableIntelJITEventListener() JL_NOTSAFEPOINT;
    void enableOProfileJITEventListener() JL_NOTSAFEPOINT;
    void enablePerfJITEventListener() JL_NOTSAFEPOINT;

    orc::SymbolStringPtr mangle(StringRef Name) JL_NOTSAFEPOINT;
    void addGlobalMapping(StringRef Name, uint64_t Addr) JL_NOTSAFEPOINT;
    void addOutput(jl_emitted_output_t O) JL_NOTSAFEPOINT;

    //Methods for the C API
    Error addExternalModule(orc::JITDylib &JD, orc::ThreadSafeModule TSM,
                            bool ShouldOptimize = false) JL_NOTSAFEPOINT;
    Error addObjectFile(orc::JITDylib &JD,
                        std::unique_ptr<MemoryBuffer> Obj) JL_NOTSAFEPOINT;
    orc::IRCompileLayer &getIRCompileLayer() JL_NOTSAFEPOINT { return CompileLayer; };
    orc::ExecutionSession &getExecutionSession() JL_NOTSAFEPOINT { return ES; }
    orc::JITDylib &getExternalJITDylib() JL_NOTSAFEPOINT { return ExternalJD; }

    Expected<llvm::orc::ExecutorSymbolDef> findSymbol(StringRef Name, bool ExportedSymbolsOnly);
    Expected<llvm::orc::ExecutorSymbolDef> findUnmangledSymbol(StringRef Name);
    Expected<llvm::orc::ExecutorSymbolDef> findExternalJDSymbol(StringRef Name, bool ExternalJDOnly);
    SmallVector<uint64_t> findSymbols(ArrayRef<StringRef> Names);
    uint64_t getGlobalValueAddress(StringRef Name);
    uint64_t getFunctionAddress(StringRef Name);

    void publishCIs(ArrayRef<jl_code_instance_t *> CIs, bool Wait=false);

    void registerCI(jl_code_instance_t *CI);
    // When a CodeInstance is garbage collected, we must remove any existing
    // entries in CISymbols, to prevent invokes to a new CodeInstance with the
    // same address from being linked to old symbol.
    void unregisterCI(jl_code_instance_t *CI);

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

protected:
    // Choose globally unique names for the functions defined by the given CI
    // and register the mapping in CISymbols.
    CISymbolPtr makeUniqueCIName(jl_code_instance_t *CI,
                                 const CISymbolPtr &Funcs) JL_NOTSAFEPOINT;

    // void registerJITOutput(MemoryBufferRef Obj, const jl_linker_info_t &Info);

    // Rename LinkGraph symbols to match the previously chosen names and
    // register debug info for defined symbols.
    void linkOutput(orc::MaterializationResponsibility &MR, MemoryBufferRef ObjBuf,
                    jitlink::LinkGraph &G,
                    std::unique_ptr<jl_linker_info_t> Info) JL_NOTSAFEPOINT;

    // Return a symbol that should be linked to the call target.  The origin of
    // this symbol depends on the code instance:
    // - If the call target is for a specialized function defined by a CI added
    //   to the JIT, return the symbol that was registered by makeUniqueCIName.
    // - If the CI already exists and has code that matches the expected calling
    //   convention, generate a symbol for it and cache it in CISymbols.
    // - If the CI exists but the code has the wrong calling convention (a
    //   specialized function is expected but only a jlcall exists, or neither
    //   exists and we should go through jl_invoke), emit the trampoline into a
    //   new module and return a symbol for it.
    orc::SymbolStringPtr linkCallTarget(orc::MaterializationResponsibility &MR,
                                        jl_code_instance_t *CI,
                                        jl_invoke_api_t API) JL_NOTSAFEPOINT;

    // Create an ORC symbol and entry in CISymbols for the CI's specptr,
    // returning a pointer into CISymbols or NULL if the CI is not compiled.
    CISymbolPtr *linkCISymbol(jl_code_instance_t *CI) JL_NOTSAFEPOINT;

    orc::ThreadSafeModule optimizeModule(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT;
    std::unique_ptr<MemoryBuffer> compileModule(orc::ThreadSafeModule TSM) JL_NOTSAFEPOINT;

private:

    const std::unique_ptr<TargetMachine> TM;
    const DataLayout DL;

    orc::ExecutionSession ES;
    orc::JITDylib &GlobalJD;
    orc::JITDylib &JD;
    orc::JITDylib &ExternalJD;
    std::mutex SharedBytesMutex{};
    SharedBytesT SharedBytes;

    // LinkerMutex protects CISymbols, Names
    std::mutex LinkerMutex;
    // CISymbols maps CodeInstance pointers to their ORC symbols.  If a
    // CodeInstance is eligible for garbage collection, it must be removed from
    // this map first, with unregisterCI.
    CISymbolMap CISymbols;
    jl_name_counter_t Names;

    std::unique_ptr<DLSymOptimizer> DLSymOpt;

    //Compilation streams
    jl_locked_stream dump_emitted_mi_name_stream;
    jl_locked_stream dump_compiles_stream;
    jl_locked_stream dump_llvm_opt_stream;

    std::mutex llvm_printing_mutex{};
    SmallVector<std::function<void()>, 0> PrintLLVMTimers;

    _Atomic(size_t) jit_bytes_size{0};
    _Atomic(size_t) jitcounter{0};
    const std::unique_ptr<jitlink::JITLinkMemoryManager> MemMgr;
    ObjLayerT ObjectLayer;
    CompileLayerT CompileLayer;
    std::unique_ptr<JITPointersT> JITPointers;
    JITPointersLayerT JITPointersLayer;
    std::unique_ptr<OptimizerT> Optimizers;
    OptimizeLayerT OptimizeLayer;
    OptSelLayerT OptSelLayer;
    std::shared_ptr<JLDebuginfoPlugin> DebuginfoPlugin;
};
extern JuliaOJIT *jl_ExecutionEngine;

inline orc::ThreadSafeModule jl_create_ts_module(StringRef name, orc::ThreadSafeContext ctx,
                                                 const DataLayout &DL, const Triple &triple,
                                                 Module *source = nullptr) JL_NOTSAFEPOINT
{
    auto lock = ctx.getLock();
    return orc::ThreadSafeModule(
        jl_create_llvm_module(name, *ctx.getContext(), DL, triple, source), ctx);
}

void fixupTM(TargetMachine &TM) JL_NOTSAFEPOINT;

void optimizeDLSyms(Module &M);

static inline const char *jl_symbol_prefix(jl_symbol_prefix_t type,
                                           jl_invoke_api_t api) JL_NOTSAFEPOINT
{
    switch (type) {
    case JL_SYMBOL_INVOKE_DEF:
        switch (api) {
        case JL_INVOKE_SPECSIG: return JL_SYM_INVOKE_SPECSIG;
        default: jl_unreachable();
        };
    case JL_SYMBOL_INVOKE_IMG:
        switch (api) {
        case JL_INVOKE_SPECSIG: return JL_SYM_INVOKE_IMG_SPECSIG;
        default: jl_unreachable();
        }
    case JL_SYMBOL_SPECPTR_DEF:
        switch (api) {
        case JL_INVOKE_ARGS: return JL_SYM_SPECPTR_ARGS;
        case JL_INVOKE_CONST: return JL_SYM_SPECPTR_CONST;
        case JL_INVOKE_SPARAM: return JL_SYM_SPECPTR_SPARAM;
        case JL_INVOKE_SPECSIG: return JL_SYM_SPECPTR_SPECSIG;
        default: jl_unreachable();
        };
    case JL_SYMBOL_SPECPTR_PROTO:
        switch (api) {
        case JL_INVOKE_ARGS: return JL_SYM_PROTO_ARGS;
        case JL_INVOKE_SPECSIG: return JL_SYM_PROTO_SPECSIG;
        default: jl_unreachable();
        }
    case JL_SYMBOL_SPECPTR_IMG:
        switch (api) {
        case JL_INVOKE_ARGS: return JL_SYM_SPECPTR_IMG_ARGS;
        case JL_INVOKE_SPARAM: return JL_SYM_SPECPTR_IMG_SPARAM;
        case JL_INVOKE_SPECSIG: return JL_SYM_SPECPTR_IMG_SPECSIG;
        default: jl_unreachable();
        }
    default: jl_unreachable();
    }
}

// NewPM
#include "passes.h"

#if JL_LLVM_VERSION >= 180000
CodeGenOptLevel CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
#else
CodeGenOpt::Level CodeGenOptLevelFor(int optlevel) JL_NOTSAFEPOINT;
#endif
