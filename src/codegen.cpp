// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#if defined(_OS_WINDOWS_) && JL_LLVM_VERSION < 40000
// trick pre-llvm39 into skipping the generation of _chkstk calls
//   since it has some codegen issues associated with them:
//   (a) assumed to be within 32-bit offset
//   (b) bad asm is generated for certain code patterns:
//       see https://github.com/JuliaLang/julia/pull/11644#issuecomment-112276813
// also, use ELF because RuntimeDyld COFF I686 support didn't exist
// also, use ELF because RuntimeDyld COFF X86_64 doesn't seem to work (fails to generate function pointers)?
#define FORCE_ELF
#endif
#if defined(_CPU_X86_)
#define JL_NEED_FLOATTEMP_VAR 1
#endif

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <setjmp.h>
#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <array>
#include <vector>
#include <set>
#include <cstdio>
#include <cassert>
#include <iostream>

// target machine computation
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Support/TargetRegistry.h>
#if JL_LLVM_VERSION < 30700
#include <llvm/Target/TargetLibraryInfo.h>
#endif
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#if JL_LLVM_VERSION >= 30700
#include <llvm/Analysis/TargetLibraryInfo.h>
#endif

#if JL_LLVM_VERSION >= 30700
#include <llvm/Object/SymbolSize.h>
#endif

// IR building
#include <llvm/IR/IntrinsicInst.h>
#if JL_LLVM_VERSION >= 30500
#include <llvm/Object/ObjectFile.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/AsmParser/Parser.h>
#else
#include <llvm/Assembly/Parser.h>
#endif
#include <llvm/DebugInfo/DIContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#if JL_LLVM_VERSION < 30500
#include <llvm/DebugInfo.h>
#include <llvm/DIBuilder.h>
#endif

// support
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/SourceMgr.h> // for llvmcall
#include <llvm/Transforms/Utils/Cloning.h> // for llvmcall inlining
#if JL_LLVM_VERSION >= 30500
#include <llvm/IR/Verifier.h> // for llvmcall validation
#else
#include <llvm/Analysis/Verifier.h>
#endif

// C API
#if JL_LLVM_VERSION >= 30800
#include <llvm-c/Types.h>
#else
#include <llvm-c/Core.h>
#endif

// for configuration options
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/CommandLine.h>

#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
#  include <llvm/IR/InlineAsm.h>
#  include <sys/utsname.h>
#endif
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/ScopDetection.h>
#endif

using namespace llvm;
namespace llvm {
    extern bool annotateSimdLoop(BasicBlock *latch);
}

#if defined(_OS_WINDOWS_) && !defined(NOMINMAX)
#define NOMINMAX
#endif

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "codegen_internal.h"

// LLVM version compatibility macros
#if JL_LLVM_VERSION >= 30700
legacy::PassManager *jl_globalPM;
#define LLVM37_param(x) (x),
#else
#define LLVM37_param(x)
PassManager *jl_globalPM;
#endif

#if JL_LLVM_VERSION >= 40000
#define DIFlagZero (DINode::FlagZero)
#else
#define DIFlagZero (0)
#endif

#if JL_LLVM_VERSION < 30500
#define AddrSpaceCastInst BitCastInst
#endif

#if !defined(_COMPILER_MICROSOFT_) && __cplusplus < 201103L && !defined(static_assert)
#  define static_assert(...)
#endif

extern "C" {

#include "builtin_proto.h"

#ifdef HAVE_SSP
extern uintptr_t __stack_chk_guard;
extern void __stack_chk_fail();
#else
JL_DLLEXPORT uintptr_t __stack_chk_guard = (uintptr_t)0xBAD57ACCBAD67ACC; // 0xBADSTACKBADSTACK
JL_DLLEXPORT void __stack_chk_fail()
{
    /* put your panic function or similar in here */
    fprintf(stderr, "fatal error: stack corruption detected\n");
    gc_debug_critical_error();
    abort(); // end with abort, since the compiler destroyed the stack upon entry to this function, there's no going back now
}
#endif

#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
#if defined(_COMPILER_MINGW_)
extern void ___chkstk_ms(void);
#else
extern void __chkstk(void);
#endif
#else
#if defined(_COMPILER_MINGW_)
#undef _alloca
extern void _alloca(void);
#else
extern void _chkstk(void);
#endif
#endif
//void *force_chkstk(void) {
//    return alloca(40960);
//}
#endif
}

#if defined(_COMPILER_MICROSOFT_) && !defined(__alignof__)
#define __alignof__ __alignof
#endif

#define DISABLE_FLOAT16

// llvm state
#if JL_LLVM_VERSION >= 30900
JL_DLLEXPORT LLVMContext jl_LLVMContext;
#else
JL_DLLEXPORT LLVMContext &jl_LLVMContext = getGlobalContext();
#endif
static IRBuilder<> builder(jl_LLVMContext);
static bool nested_compile = false;
TargetMachine *jl_TargetMachine;

extern JITEventListener *CreateJuliaJITEventListener();

// for image reloading
bool imaging_mode = false;

Module *shadow_output;
#define jl_Module ctx->f->getParent()
#define jl_builderModule builder.GetInsertBlock()->getParent()->getParent()

#if JL_LLVM_VERSION >= 30700
// No DataLayout pass needed anymore.
#elif JL_LLVM_VERSION >= 30500
static DataLayoutPass *jl_data_layout;
#else
static DataLayout *jl_data_layout;
#endif

// types
static Type *T_pjlvalue;
static Type *T_ppjlvalue;
static Type *jl_parray_llvmt;
static FunctionType *jl_func_sig;
static FunctionType *jl_func_sig_sparams;
static Type *T_pvoidfunc;

static IntegerType *T_int1;
static IntegerType *T_int8;
static IntegerType *T_int16;
static IntegerType *T_int32;
static IntegerType *T_int64;

static IntegerType *T_uint8;
static IntegerType *T_uint16;
static IntegerType *T_uint32;
static IntegerType *T_uint64;

static IntegerType *T_char;
static IntegerType *T_size;
static IntegerType *T_sigatomic;

static Type *T_float16;
static Type *T_float32;
static Type *T_float64;
static Type *T_float128;

static Type *T_pint8;
static Type *T_pint16;
static Type *T_pint32;
static Type *T_pint64;
static Type *T_psize;
static Type *T_pfloat32;
static Type *T_pfloat64;

static Type *T_ppint8;
static Type *T_pppint8;

static Type *T_void;

// type-based alias analysis nodes.  Indentation of comments indicates hierarchy.
static MDNode *tbaa_gcframe;    // GC frame
// LLVM should have enough info for alias analysis of non-gcframe stack slot
// this is mainly a place holder for `jl_cgval_t::tbaa`
static MDNode *tbaa_stack;      // stack slot
static MDNode *tbaa_data;       // Any user data that `pointerset/ref` are allowed to alias
static MDNode *tbaa_tag;            // Type tag
static MDNode *tbaa_binding;        // jl_binding_t::value
static MDNode *tbaa_value;          // jl_value_t, that is not jl_array_t
static MDNode *tbaa_mutab;              // mutable type
static MDNode *tbaa_immut;              // immutable type
static MDNode *tbaa_arraybuf;       // Data in an array
static MDNode *tbaa_array;      // jl_array_t
static MDNode *tbaa_arrayptr;       // The pointer inside a jl_array_t
static MDNode *tbaa_arraysize;      // A size in a jl_array_t
static MDNode *tbaa_arraylen;       // The len in a jl_array_t
static MDNode *tbaa_arrayflags;     // The flags in a jl_array_t
static MDNode *tbaa_const;      // Memory that is immutable by the time LLVM can see it

// Basic DITypes
#if JL_LLVM_VERSION >= 30700
static DICompositeType *jl_value_dillvmt;
static DIDerivedType *jl_pvalue_dillvmt;
static DIDerivedType *jl_ppvalue_dillvmt;
static DISubroutineType *jl_di_func_sig;
static DISubroutineType *jl_di_func_null_sig;
#else
static DICompositeType jl_value_dillvmt;
static DIDerivedType jl_pvalue_dillvmt;
static DIDerivedType jl_ppvalue_dillvmt;
#if JL_LLVM_VERSION >= 30600
DISubroutineType jl_di_func_sig;
DISubroutineType jl_di_func_null_sig;
#else
DICompositeType jl_di_func_sig;
DICompositeType jl_di_func_null_sig;
#endif
#endif


extern "C"
int32_t jl_jlcall_api(const void *function)
{
    // give the function an index in the constant lookup table
    if (function == NULL)
        return 0;
    const Function *F = cast<const Function>((const Value*)function);
    StringRef Name = F->getName();
    if (Name.startswith("japi3_")) // jlcall abi 3 from JIT
        return 3;
    assert(Name.startswith("japi1_") || // jlcall abi 1 from JIT
           Name.startswith("jsys1_") || // jlcall abi 1 from sysimg
           Name.startswith("jlcall_") || // jlcall abi 1 from JIT wrapping a specsig method
           Name.startswith("jlsysw_")); // jlcall abi 1 from sysimg wrapping a specsig method
    return 1;
}


// constants
static Value *V_null;
static Type *NoopType;
static Value *literal_pointer_val(jl_value_t *p);
extern "C" {
JL_DLLEXPORT Type *julia_type_to_llvm(jl_value_t *jt, bool *isboxed=NULL);
}
static bool type_is_ghost(Type *ty)
{
    return (ty == T_void || ty->isEmptyTy());
}

// global vars
static GlobalVariable *jlRTLD_DEFAULT_var;
#ifdef _OS_WINDOWS_
static GlobalVariable *jlexe_var;
static GlobalVariable *jldll_var;
#if defined(_CPU_X86_64_) && !defined(USE_MCJIT)
JITMemoryManager *createJITMemoryManagerWin();
#endif
#endif //_OS_WINDOWS_

static Function *jltls_states_func;
#ifndef JULIA_ENABLE_THREADING
static GlobalVariable *jltls_states_var;
#else
// Imaging mode only
static GlobalVariable *jltls_states_func_ptr = NULL;
size_t jltls_states_func_idx = 0;
#endif

// important functions
static Function *jlnew_func;
static Function *jlthrow_func;
static Function *jlerror_func;
static Function *jltypeerror_func;
static Function *jlundefvarerror_func;
static Function *jlboundserror_func;
static Function *jluboundserror_func;
static Function *jlvboundserror_func;
static Function *jlboundserrorv_func;
static Function *jlcheckassign_func;
static Function *jldeclareconst_func;
static Function *jlgetbindingorerror_func;
static Function *jltopeval_func;
static Function *jlcopyast_func;
static Function *jltuple_func;
static Function *jlnsvec_func;
static Function *jlapplygeneric_func;
static Function *jlinvoke_func;
static Function *jlapply2va_func;
static Function *jlgetfield_func;
static Function *jlmethod_func;
static Function *jlgenericfunction_func;
static Function *jlenter_func;
static Function *jlleave_func;
static Function *jlegal_func;
static Function *jlalloc_pool_func;
static Function *jlalloc_big_func;
static Function *jlisa_func;
static Function *jlapplytype_func;
static Function *setjmp_func;
static Function *memcmp_func;
static Function *box_int8_func;
static Function *box_uint8_func;
static Function *box_int16_func;
static Function *box_uint16_func;
static Function *box_int32_func;
static Function *box_char_func;
static Function *box_uint32_func;
static Function *box_int64_func;
static Function *box_uint64_func;
static Function *box_float32_func;
static Function *box_float64_func;
static Function *box_ssavalue_func;
static Function *box8_func;
static Function *box16_func;
static Function *box32_func;
static Function *box64_func;
static Function *queuerootfun;
static Function *expect_func;
static Function *jldlsym_func;
static Function *jlnewbits_func;
static Function *jltypeassert_func;
#if JL_LLVM_VERSION < 30600
static Function *jlpow_func;
static Function *jlpowf_func;
#endif
//static Function *jlgetnthfield_func;
static Function *jlgetnthfieldchecked_func;
//static Function *jlsetnthfield_func;
#ifdef _OS_WINDOWS_
static Function *resetstkoflw_func;
#if defined(_CPU_X86_64_)
Function *juliapersonality_func;
#endif
#endif
static Function *diff_gc_total_bytes_func;
static Function *jlarray_data_owner_func;
static Function *jlgetworld_func;

// placeholder functions
static Function *gcroot_func;
static Function *gckill_func;
static Function *jlcall_frame_func;
static Function *gcroot_flush_func;
static Function *except_enter_func;

static std::vector<Type *> two_pvalue_llvmt;
static std::vector<Type *> three_pvalue_llvmt;
static std::vector<Type *> four_pvalue_llvmt;

static std::map<jl_fptr_t, Function*> builtin_func_map;

// --- code generation ---
extern "C" {
    int globalUnique = 0;
}

// metadata tracking for a llvm Value* during codegen
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    Value *gcroot; // the gcroot associated with V (if it has one)
    jl_value_t *typ; // the original type of V, never NULL
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    bool isimmutable; // V points to something that is definitely immutable (e.g. single-assignment, but including memory)
    MDNode *tbaa; // The related tbaa node. Non-NULL iff this is not a pointer.
    bool ispointer() const
    {
        return tbaa != nullptr;
    }
    jl_cgval_t(Value *V, Value *gcroot, bool isboxed, jl_value_t *typ) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        constant(NULL),
        gcroot(gcroot),
        typ(typ),
        isboxed(isboxed),
        isghost(false),
        isimmutable(isboxed && jl_is_immutable_datatype(typ)),
        tbaa(isboxed ? (jl_is_leaf_type(typ) ?
                        (jl_is_mutable(typ) ? tbaa_mutab : tbaa_immut) :
                        tbaa_value) : nullptr)
    {
    }
    jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        V(NULL),
        constant(((jl_datatype_t*)typ)->instance),
        gcroot(NULL),
        typ(typ),
        isboxed(false),
        isghost(true),
        isimmutable(true),
        tbaa(nullptr)
    {
        assert(jl_is_datatype(typ));
        assert(constant);
    }
    jl_cgval_t(const jl_cgval_t &v, jl_value_t *typ) : // copy constructor with new type
        V(v.V),
        constant(v.constant),
        gcroot(v.gcroot),
        typ(typ),
        isboxed(v.isboxed),
        isghost(v.isghost),
        isimmutable(v.isimmutable),
        tbaa(v.tbaa)
    {
        assert(isboxed || v.typ == typ); // expect a badly or equivalently typed version
    }
    jl_cgval_t() : // undef / unreachable / default constructor
        V(UndefValue::get(T_void)),
        constant(NULL),
        gcroot(NULL),
        typ(jl_bottom_type),
        isboxed(false),
        isghost(true),
        isimmutable(true),
        tbaa(nullptr)
    {
    }
};

// per-local-variable information
struct jl_varinfo_t {
    Value *memloc; // an address, if the var is in a jl_value_t* gc stack slot or jl_box_t* Box object (marked tbaa_const, if appropriate)
    jl_cgval_t value; // a value, if the var is unboxed or SSA (and thus memloc == NULL)
#if JL_LLVM_VERSION >= 30700
    DILocalVariable *dinfo;
#else
    DIVariable dinfo;
#endif
    // if the variable might be used undefined and is not boxed
    // this i1 flag is true when it is defined
    Value *defFlag;
    bool isSA;
    bool isVolatile;
    bool isArgument;
    bool escapes;
    bool usedUndef;
    bool used;

    jl_varinfo_t() : memloc(NULL), value(jl_cgval_t()),
#if JL_LLVM_VERSION >= 30700
                     dinfo(NULL),
#else
                     dinfo(DIVariable()),
#endif
                     defFlag(NULL),
                     isSA(false),
                     isVolatile(false), isArgument(false),
                     escapes(true), usedUndef(false), used(false)
    {
    }
};

// aggregate of array metadata
typedef struct {
    Value *dataptr;
    Value *len;
    std::vector<Value*> sizes;
    jl_value_t *ty;
} jl_arrayvar_t;

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
class jl_codectx_t {
public:
    Function *f;
    // local var info. globals are not in here.
    std::vector<jl_varinfo_t> slots;
    std::vector<jl_cgval_t> SAvalues;
    std::vector<bool> ssavalue_assigned;
    std::map<int, jl_arrayvar_t> *arrayvars;
    jl_module_t *module;
    jl_method_instance_t *linfo;
    jl_code_info_t *source;
    jl_array_t *code;
    size_t world;
    jl_array_t *roots;
    const char *name;
    StringRef file;
    ssize_t *line;
    Value *spvals_ptr;
    Value *argArray;
    Value *argCount;
    std::string funcName;
    int vaSlot;        // name of vararg argument
    bool vaStack;      // varargs stack-allocated
    bool sret;
    int nReqArgs;
    int nargs;

    CallInst *ptlsStates;
    Value *signalPage;
    Value *world_age_field;

    bool debug_enabled;
    bool is_inbounds{false};

    const jl_cgparams_t *params;

    ~jl_codectx_t() {
        assert(this->roots == NULL);
    }
};

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx);

static Value *emit_local_root(jl_codectx_t *ctx, jl_varinfo_t *vi = NULL);
static void mark_gc_use(const jl_cgval_t &v);
static Value *make_jlcall(ArrayRef<const jl_cgval_t*> args, jl_codectx_t *ctx);
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign, jl_codectx_t *ctx);
static jl_cgval_t emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol, MDNode *tbaa);
static jl_cgval_t emit_sparam(size_t i, jl_codectx_t *ctx);
static Value *emit_condition(const jl_cgval_t &condV, const std::string &msg, jl_codectx_t *ctx);
static void allocate_gc_frame(BasicBlock *b0, jl_codectx_t *ctx);
static GlobalVariable *prepare_global(GlobalVariable *G, Module *M = jl_builderModule);
static Value *prepare_call(Value *Callee);
static Value *prepare_call(IRBuilder<> &builder, Value *Callee);


template<typename T> static void push_gc_use(T &&vec, const jl_cgval_t &v)
{
    if (v.gcroot) {
        vec.push_back(v.gcroot);
    }
}

template<typename T> static void mark_gc_uses(T &&vec)
{
    auto f = prepare_call(gckill_func);
    for (auto &v: vec) {
        builder.CreateCall(f, v);
    }
}

// --- convenience functions for tagging llvm values with julia types ---

static AllocaInst *emit_static_alloca(Type *lty, int arraysize, jl_codectx_t *ctx)
{
    return new AllocaInst(lty, ConstantInt::get(T_int32, arraysize), "", /*InsertBefore=*/ctx->ptlsStates);
}
static AllocaInst *emit_static_alloca(Type *lty, jl_codectx_t *ctx)
{
    return emit_static_alloca(lty, 1, ctx);
}
static AllocaInst *emit_static_alloca(Type *lty)
{
    return new AllocaInst(lty, "",
            /*InsertBefore=*/&*builder.GetInsertBlock()->getParent()->getEntryBlock().getFirstInsertionPt());
}

static inline jl_cgval_t ghostValue(jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return jl_cgval_t(); // Undef{}
    return jl_cgval_t(typ);
}
static inline jl_cgval_t ghostValue(jl_datatype_t *typ)
{
    return ghostValue((jl_value_t*)typ);
}

static inline jl_cgval_t mark_julia_slot(Value *v, jl_value_t *typ, MDNode *tbaa)
{
    // eagerly put this back onto the stack
    assert(tbaa);
    jl_cgval_t tagval(v, NULL, false, typ);
    tagval.tbaa = tbaa;
    tagval.isimmutable = true;
    return tagval;
}

static inline jl_cgval_t mark_julia_type(Value *v, bool isboxed, jl_value_t *typ, jl_codectx_t *ctx, bool needsroot = true)
{
    Type *T = julia_type_to_llvm(typ);
    if (type_is_ghost(T)) {
        return ghostValue(typ);
    }
    if (v && T->isAggregateType() && !isboxed) {
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        Value *loc = emit_static_alloca(T);
        builder.CreateStore(v, loc);
        return mark_julia_slot(loc, typ, tbaa_stack);
    }
    Value *froot = NULL;
    if (needsroot && isboxed) {
        froot = emit_local_root(ctx);
        builder.CreateStore(v, froot);
    }
    return jl_cgval_t(v, froot, isboxed, typ);
}

static inline jl_cgval_t mark_julia_type(Value *v, bool isboxed, jl_datatype_t *typ, jl_codectx_t *ctx, bool needsroot = true)
{
    return mark_julia_type(v, isboxed, (jl_value_t*)typ, ctx, needsroot);
}

static inline jl_cgval_t remark_julia_type(const jl_cgval_t &v, jl_value_t *typ)
{
    Type *T = julia_type_to_llvm(typ);
    if (type_is_ghost(T)) {
        return ghostValue(typ);
    }
    return jl_cgval_t(v, typ);
}

static inline jl_cgval_t mark_julia_const(jl_value_t *jv)
{
    jl_value_t *typ;
    if (jl_is_type(jv))
        typ = (jl_value_t*)jl_wrap_Type(jv);
    else
        typ = jl_typeof(jv);
    if (type_is_ghost(julia_type_to_llvm(typ))) {
        return ghostValue(typ);
    }
    jl_cgval_t constant(NULL, NULL, true, typ);
    constant.constant = jv;
    return constant;
}

// --- utilities ---

static void emit_write_barrier(jl_codectx_t*, Value*, Value*);

#include "cgutils.cpp"

static void jl_rethrow_with_add(const char *fmt, ...)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_typeis(ptls->exception_in_transit, jl_errorexception_type)) {
        char *str = jl_string_data(jl_fieldref(ptls->exception_in_transit,0));
        char buf[1024];
        va_list args;
        va_start(args, fmt);
        int nc = vsnprintf(buf, sizeof(buf), fmt, args);
        va_end(args);
        nc += snprintf(buf+nc, sizeof(buf)-nc, ": %s", str);
        jl_value_t *msg = jl_pchar_to_string(buf, nc);
        JL_GC_PUSH1(&msg);
        jl_throw(jl_new_struct(jl_errorexception_type, msg));
    }
    jl_rethrow();
}

static void CreateTrap(IRBuilder<> &builder)
{
    Function *f = builder.GetInsertBlock()->getParent();
    Function *trap_func = Intrinsic::getDeclaration(
            f->getParent(),
            Intrinsic::trap);
    builder.CreateCall(trap_func);
    builder.CreateUnreachable();
    BasicBlock *newBB = BasicBlock::Create(builder.getContext(), "after_noret", f);
    builder.SetInsertPoint(newBB);
}

// --- allocating local variables ---

static bool isbits_spec(jl_value_t *jt, bool allow_unsized = true)
{
    return jl_isbits(jt) && jl_is_leaf_type(jt) && (allow_unsized ||
        ((jl_is_bitstype(jt) && jl_datatype_size(jt) > 0) ||
         (jl_is_datatype(jt) && jl_datatype_nfields(jt)>0)));
}

static bool store_unboxed_p(jl_value_t *jt)
{
    return (isbits_spec(jt,false) &&
        // don't unbox intrinsics, since inference depends on their having
        // stable addresses for table lookup.
        jt != (jl_value_t*)jl_intrinsic_type);
}

static bool store_unboxed_p(int s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->slots[s];
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->source->inferred &&
            // don't unbox vararg tuples
            s != ctx->vaSlot && store_unboxed_p(vi.value.typ));
}

static jl_sym_t *slot_symbol(int s, jl_codectx_t *ctx)
{
    return (jl_sym_t*)jl_array_ptr_ref(ctx->source->slotnames, s);
}

static void store_def_flag(const jl_varinfo_t &vi, bool val)
{
    assert(!vi.memloc); // undefinedness is null pointer for boxed things
    assert(vi.usedUndef && vi.defFlag);
    builder.CreateStore(ConstantInt::get(T_int1, val), vi.defFlag);
}

static void alloc_def_flag(jl_varinfo_t& vi, jl_codectx_t* ctx)
{
    assert(!vi.memloc); // same as above, not used for pointers
    if (vi.usedUndef) {
        vi.defFlag = emit_static_alloca(T_int1, ctx);
        store_def_flag(vi, false);
    }
}

static Value *alloc_local(int s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->slots[s];
    jl_value_t *jt = vi.value.typ;
    assert(store_unboxed_p(s,ctx));
    bool isboxed;
    Type *vtype = julia_type_to_llvm(jt, &isboxed);
    assert(!isboxed);
    if (type_is_ghost(vtype)) {
        vi.value = ghostValue(jt);
        return NULL;
    }
    // CreateAlloca is OK here because alloc_local is only called during prologue setup
    Value *lv = builder.CreateAlloca(vtype, 0, jl_symbol_name(slot_symbol(s,ctx)));
    vi.value = mark_julia_slot(lv, jt, tbaa_stack);
    // slot is not immutable if there are multiple assignments
    vi.value.isimmutable &= (vi.isSA && s >= ctx->nargs);
    alloc_def_flag(vi, ctx);
    assert(vi.value.isboxed == false);
    return lv;
}

static void maybe_alloc_arrayvar(int s, jl_codectx_t *ctx)
{
    jl_value_t *jt = ctx->slots[s].value.typ;
    if (arraytype_constshape(jt)) {
        // TODO: this optimization does not yet work with 1-d arrays, since the
        // length and data pointer can change at any time via push!
        // we could make it work by reloading the metadata when the array is
        // passed to an external function (ideally only impure functions)
        jl_arrayvar_t av;
        int ndims = jl_unbox_long(jl_tparam1(jt));
        Type *elt = julia_type_to_llvm(jl_tparam0(jt));
        if (type_is_ghost(elt))
            return;
        // CreateAlloca is OK here because maybe_alloc_arrayvar is only called in the prologue setup
        av.dataptr = builder.CreateAlloca(PointerType::get(elt,0));
        av.len = builder.CreateAlloca(T_size);
        for (int i = 0; i < ndims - 1; i++)
            av.sizes.push_back(builder.CreateAlloca(T_size));
        av.ty = jt;
        (*ctx->arrayvars)[s] = av;
    }
}

// Snooping on which functions are being compiled, and how long it takes
JL_STREAM *dump_compiles_stream = NULL;
uint64_t last_time = 0;
extern "C" JL_DLLEXPORT
void jl_dump_compiles(void *s)
{
    dump_compiles_stream = (JL_STREAM*)s;
}

// --- entry point ---
//static int n_emit=0;
static std::unique_ptr<Module> emit_function(
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        size_t world,
        jl_llvm_functions_t *declarations,
        const jl_cgparams_t *params);
void jl_add_linfo_in_flight(StringRef name, jl_method_instance_t *linfo, const DataLayout &DL);

// this generates llvm code for the lambda info
// and adds the result to the jitlayers
// (and the shadow module), but doesn't yet compile
// or generate object code for it
extern "C"
jl_llvm_functions_t jl_compile_linfo(jl_method_instance_t **pli, jl_code_info_t *src, size_t world, const jl_cgparams_t *params)
{
    JL_TIMING(CODEGEN);
    jl_method_instance_t *li = *pli;
    assert(jl_is_method_instance(li));
    jl_llvm_functions_t decls = {};

    if (params != &jl_default_cgparams /* fast path */ &&
        !compare_cgparams(params, &jl_default_cgparams) && params->cached)
        jl_error("functions compiled with custom codegen params mustn't be cached");

    // Step 1. See if it is already compiled,
    //         Get the codegen lock,
    //         And get the source
    if (li->def == NULL) {
        JL_LOCK(&codegen_lock);
        src = (jl_code_info_t*)li->inferred;
        decls = li->functionObjectsDecls;
        if (decls.functionObject != NULL || !src || !jl_is_code_info(src) || li->jlcall_api == 2) {
            JL_UNLOCK(&codegen_lock);
            return decls;
        }
    }
    else if (!src) {
        // Step 1a. If the caller didn't provide the source,
        //          try to infer it for ourself
        // first see if it is already compiled
        decls = li->functionObjectsDecls;
        if ((params->cached && decls.functionObject != NULL) || li->jlcall_api == 2) {
            if (li->min_world <= world && li->max_world >= world)
                return decls;
        }
        JL_LOCK(&codegen_lock);
        assert(li->min_world <= world && li->max_world >= world);
        decls = li->functionObjectsDecls;
        if ((params->cached && decls.functionObject != NULL) || li->jlcall_api == 2) {
            JL_UNLOCK(&codegen_lock);
            return decls;
        }

        // see if it is inferred
        src = (jl_code_info_t*)li->inferred;
        if (src) {
            if (!jl_is_code_info(src)) {
                src = jl_type_infer(pli, world, 0);
                li = *pli;
            }
            if (!src || li->jlcall_api == 2) {
                JL_UNLOCK(&codegen_lock);
                return decls;
            }
        }
        else {
            // declare a failure to compile
            JL_UNLOCK(&codegen_lock);
            return decls;
        }
    }
    else {
        // similar to above, but never returns a NULL
        // decl (unless compile fails), even if jlcall_api == 2
        decls = li->functionObjectsDecls;
        if (params->cached && decls.functionObject != NULL) {
            return decls;
        }
        JL_LOCK(&codegen_lock);
        decls = li->functionObjectsDecls;
        if (params->cached && decls.functionObject != NULL) {
            JL_UNLOCK(&codegen_lock);
            return decls;
        }
    }
    JL_GC_PUSH1(&src);
    assert(jl_is_code_info(src));

    // Step 2: setup global state
    IRBuilderBase::InsertPoint old = builder.saveAndClearIP();
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    if (!nested_compile && dump_compiles_stream != NULL)
        last_time = jl_hrtime();
    nested_compile = true;

    // Step 3. actually do the work of emitting the function
    std::unique_ptr<Module> m;
    JL_TRY {
        jl_llvm_functions_t *pdecls;
        if (!params->cached)
            pdecls = &decls;
        else if (li->min_world <= world && li->max_world >= world)
            pdecls = &li->functionObjectsDecls;
        else if (li->def == NULL)
            pdecls = &li->functionObjectsDecls;
        else
            pdecls = &decls;
        m = emit_function(li, src, world, pdecls, params);
        if (params->cached && world)
            decls = li->functionObjectsDecls;
        //n_emit++;
    }
    JL_CATCH {
        // something failed! this is very bad, since other WIP may be pointing to this function
        // but there's not much we can do now. try to clear much of the WIP anyways.
        li->functionObjectsDecls.functionObject = NULL;
        li->functionObjectsDecls.specFunctionObject = NULL;
        nested_compile = last_n_c;
        builder.restoreIP(old);
        builder.SetCurrentDebugLocation(olddl);
        JL_UNLOCK(&codegen_lock); // Might GC
        jl_rethrow_with_add("error compiling %s", jl_symbol_name(li->def ? li->def->name : anonymous_sym));
    }
    Function *f = (Function*)decls.functionObject;
    Function *specf = (Function*)decls.specFunctionObject;


    if (JL_HOOK_TEST(params, module_activation)) {
        JL_HOOK_CALL(params, module_activation, 1, jl_box_voidpointer(wrap(m.release())));
    } else {
        // Step 4. Prepare debug info to receive this function
        // record that this function name came from this linfo,
        // so we can build a reverse mapping for debug-info.
        bool toplevel = li->def == NULL;
        if (!toplevel) {
            const DataLayout &DL =
#if JL_LLVM_VERSION >= 30500
                m->getDataLayout();
#else
                *jl_data_layout;
#endif
            // but don't remember toplevel thunks because
            // they may not be rooted in the gc for the life of the program,
            // and the runtime doesn't notify us when the code becomes unreachable :(
            jl_add_linfo_in_flight((specf ? specf : f)->getName(), li, DL);
        }

        // Step 5. Add the result to the execution engine now
        jl_finalize_module(m.release(), !toplevel);
    }

    if (world && li->jlcall_api != 2) {
        // if not inlineable, code won't be needed again
        if (JL_DELETE_NON_INLINEABLE && jl_options.debug_level <= 1 &&
            li->def && li->inferred && jl_is_code_info(li->inferred) &&
            !((jl_code_info_t*)li->inferred)->inlineable &&
            li != li->def->unspecialized && !imaging_mode) {
            li->inferred = jl_nothing;
        }
    }

    // Step 6: Done compiling: Restore global state
    builder.restoreIP(old);
    builder.SetCurrentDebugLocation(olddl);
    nested_compile = last_n_c;
    JL_UNLOCK(&codegen_lock); // Might GC

    if (dump_compiles_stream != NULL) {
        uint64_t this_time = jl_hrtime();
        jl_printf(dump_compiles_stream, "%" PRIu64 "\t\"", this_time - last_time);
        jl_static_show(dump_compiles_stream, (jl_value_t*)li);
        jl_printf(dump_compiles_stream, "\"\n");
        last_time = this_time;
    }
    JL_GC_POP();
    return decls;
}

#if JL_LLVM_VERSION < 30700
static Value *getModuleFlag(Module *m, StringRef Key)
{
    SmallVector<Module::ModuleFlagEntry, 8> ModuleFlags;
    m->getModuleFlagsMetadata(ModuleFlags);
    SmallVector<Module::ModuleFlagEntry, 8>::iterator it = ModuleFlags.begin();
    for (;it != ModuleFlags.end(); ++it) {
        if (Key == it->Key->getString())
            return it->Val;
    }
    return NULL;
}
#else
#define getModuleFlag(m,str) m->getModuleFlag(str)
#endif

static void jl_setup_module(Module *m, const jl_cgparams_t *params = &jl_default_cgparams)
{
    if (JL_HOOK_TEST(params, module_setup)) {
        JL_HOOK_CALL(params, module_setup, 1, jl_box_voidpointer(wrap(m)));
        return;
    }

    // Some linkers (*cough* OS X) don't understand DWARF v4, so we use v2 in
    // imaging mode. The structure of v4 is slightly nicer for debugging JIT
    // code.
    if (!getModuleFlag(m,"Dwarf Version")) {
        int dwarf_version = 4;
#ifdef _OS_DARWIN_
        if (imaging_mode)
            dwarf_version = 2;
#endif
        m->addModuleFlag(llvm::Module::Warning, "Dwarf Version", dwarf_version);
    }
#if JL_LLVM_VERSION >= 30400
    if (!getModuleFlag(m,"Debug Info Version"))
        m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
#endif
#if JL_LLVM_VERSION >= 30700
#ifdef USE_ORCJIT
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#elif JL_LLVM_VERSION >= 30800
    m->setDataLayout(jl_ExecutionEngine->getDataLayout().getStringRepresentation());
#else
    m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
#endif
    m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
#elif JL_LLVM_VERSION >= 30600
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
}

// this ensures that llvmf has been emitted to the execution engine,
// returning the function pointer to it
extern void jl_callback_triggered_linfos(void);
static uint64_t getAddressForFunction(llvm::Function *llvmf)
{
    JL_TIMING(LLVM_EMIT);
#ifdef JL_DEBUG_BUILD
    llvm::raw_fd_ostream out(1,false);
#endif
#ifdef USE_MCJIT
    jl_finalize_function(llvmf);
    uint64_t ret = jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
    // delay executing trace callbacks until here to make sure there's no
    // recursive compilation.
    jl_callback_triggered_linfos();
    return ret;
#else
    return (uint64_t)jl_ExecutionEngine->getPointerToFunction(
            cast<Function>(shadow_output->getNamedValue(llvmf->getName())));
#endif
}

extern "C" JL_DLLEXPORT
uint64_t jl_get_llvm_fptr(llvm::Function *llvmf)
{
    uint64_t addr = getAddressForFunction(llvmf);
#ifdef USE_ORCJIT
    if (!addr)
        addr = jl_ExecutionEngine->findUnmangledSymbol(llvmf->getName()).getAddress();
#endif
    return addr;
}

static jl_method_instance_t *jl_get_unspecialized(jl_method_instance_t *method)
{
    // one unspecialized version of a function can be shared among all cached specializations
    jl_method_t *def = method->def;
    if (def->isstaged) {
        return method;
    }
    if (def->unspecialized == NULL) {
        JL_LOCK(&def->writelock);
        if (def->unspecialized == NULL) {
            def->unspecialized = jl_get_specialized(def, def->sig, jl_emptysvec);
            jl_gc_wb(def, def->unspecialized);
        }
        JL_UNLOCK(&def->writelock);
    }
    return def->unspecialized;
}

// this compiles li and emits fptr
extern "C"
jl_generic_fptr_t jl_generate_fptr(jl_method_instance_t *li, void *_F, size_t world)
{
    Function *F = (Function*)_F;
    jl_generic_fptr_t fptr;
    fptr.fptr = li->fptr;
    fptr.jlcall_api = li->jlcall_api;
    if (fptr.fptr && fptr.jlcall_api) {
        return fptr;
    }
    fptr.fptr = li->unspecialized_ducttape;
    fptr.jlcall_api = 1;
    if (!li->inferred && fptr.fptr) {
        return fptr;
    }
    JL_LOCK(&codegen_lock);
    fptr.fptr = li->fptr;
    fptr.jlcall_api = li->jlcall_api;
    if (fptr.fptr && fptr.jlcall_api) {
        JL_UNLOCK(&codegen_lock);
        return fptr;
    }
    jl_method_instance_t *unspec = NULL;
    if (li->def) {
        if (!li->def->isstaged && li->def->unspecialized) {
            unspec = li->def->unspecialized;
        }
        if (!F || !jl_can_finalize_function(F)) {
            // can't compile F in the JIT right now,
            // so instead compile an unspecialized version
            // and return its fptr instead
            if (!unspec)
                unspec = jl_get_unspecialized(li); // get-or-create the unspecialized version to cache the result
            jl_code_info_t *src = unspec->def->isstaged ? jl_code_for_staged(unspec) : unspec->def->source;
            fptr.fptr = unspec->fptr;
            fptr.jlcall_api = unspec->jlcall_api;
            if (fptr.fptr && fptr.jlcall_api) {
                JL_UNLOCK(&codegen_lock);
                return fptr;
            }
            jl_llvm_functions_t decls = unspec->functionObjectsDecls;
            if (unspec == li) {
                // temporarily clear the decls so that it will compile our unspec version of src
                unspec->functionObjectsDecls.functionObject = NULL;
                unspec->functionObjectsDecls.specFunctionObject = NULL;
            }
            assert(src);
            F = (Function*)jl_compile_linfo(&unspec, src, unspec->min_world, &jl_default_cgparams).functionObject; // this does not change unspec
            if (unspec == li) {
                unspec->functionObjectsDecls = decls;
            }
            assert(jl_can_finalize_function(F));
        }
    }
    assert(F);
    fptr.fptr = (jl_fptr_t)getAddressForFunction(F);
    fptr.jlcall_api = jl_jlcall_api(F);
    assert(fptr.fptr != NULL);
    // decide if the fptr should be cached somewhere also
    if (li->functionObjectsDecls.functionObject == F) {
        if (li->fptr) {
            // don't change fptr as that leads to race conditions
            // with the (not) simultaneous update to jlcall_api
        }
        else if (li->inferred || fptr.jlcall_api != 1) {
            li->jlcall_api = fptr.jlcall_api;
            li->fptr = fptr.fptr;
        }
        else {
            li->unspecialized_ducttape = fptr.fptr;
        }
    }
    else if (unspec) {
        if (unspec->fptr) {
            // don't change fptr as that leads to race conditions
            // with the (not) simultaneous update to jlcall_api
        }
        else if (unspec == li) {
            if (fptr.jlcall_api == 1)
                li->unspecialized_ducttape = fptr.fptr;
        }
        else if (unspec->functionObjectsDecls.functionObject == F) {
            unspec->jlcall_api = fptr.jlcall_api;
            unspec->fptr = fptr.fptr;
        }
    }
    JL_UNLOCK(&codegen_lock); // Might GC
    return fptr;
}

static Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt);
// get the address of a C-callable entry point for a function
extern "C" JL_DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_GC_PUSH1(&argt);
    if (jl_is_tuple(argt)) {
        // TODO: maybe deprecation warning, better checking
        argt = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argt), jl_nfields(argt));
    }
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    JL_GC_POP();
    return (void*)getAddressForFunction(llvmf);
}


// convenience function for debugging from gdb (pre-OrcJIT)
// it generally helps to have define KEEP_BODIES if you plan on using this
extern "C" JL_DLLEXPORT
void *jl_function_ptr_by_llvm_name(char *name) {
#ifdef JL_MSAN_ENABLED
    __msan_unpoison_string(name);
#endif
    return (void*)(intptr_t)jl_ExecutionEngine->FindFunctionNamed(name);
}

// export a C-callable entry point for a function (dllexport'ed dlsym), with a given name
extern "C" JL_DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    assert(jl_is_tuple_type(argt));
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    if (llvmf) {
        // force eager emission of the function (llvm 3.3 gets confused otherwise and tries to do recursive compilation)
        uint64_t Addr = getAddressForFunction(llvmf);

#if defined(USE_ORCJIT) || defined(USE_MCJIT)
        if (imaging_mode)
             // in the old JIT, the shadow_module aliases the engine_module,
             // otherwise, just point the alias to the declaration
#endif
            llvmf = cast<Function>(shadow_output->getNamedValue(llvmf->getName()));

        // make the alias to the shadow_module
        GlobalAlias *GA =
#if JL_LLVM_VERSION >= 30800
            GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#elif JL_LLVM_VERSION >= 30700
            GlobalAlias::create(cast<PointerType>(llvmf->getType()),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#else
            new GlobalAlias(llvmf->getType(), GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#endif

#if defined(USE_ORCJIT) || defined(USE_MCJIT)
        // make the alias name is valid for the current session
        jl_ExecutionEngine->addGlobalMapping(GA, (void*)(uintptr_t)Addr);
#else
        (void)GA; (void)Addr;
#endif
    }
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir and jl_dump_function_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT
void *jl_get_llvmf_defn(jl_method_instance_t *linfo, size_t world, bool getwrapper, bool optimize, const jl_cgparams_t params)
{
    if (linfo->def && linfo->def->source == NULL) {
        // not a generic function
        return NULL;
    }

    jl_code_info_t *src = (jl_code_info_t*)linfo->inferred;
    JL_GC_PUSH1(&src);
    if (!src || !jl_is_code_info(src)) {
        src = jl_type_infer(&linfo, world, 0);
        if (!src)
            src = linfo->def->isstaged ? jl_code_for_staged(linfo) : linfo->def->source;
    }

    // Backup the info for the nested compile
    JL_LOCK(&codegen_lock);

    IRBuilderBase::InsertPoint old = builder.saveAndClearIP();
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    // emit this function into a new module
    jl_llvm_functions_t declarations;
    std::unique_ptr<Module> m;
    JL_TRY {
        m = emit_function(linfo, src, world, &declarations, &params);
    }
    JL_CATCH {
        // something failed!
        nested_compile = last_n_c;
        builder.restoreIP(old);
        builder.SetCurrentDebugLocation(olddl);
        JL_UNLOCK(&codegen_lock); // Might GC
        jl_rethrow_with_add("error compiling %s", jl_symbol_name(linfo->def ? linfo->def->name : anonymous_sym));
    }
    // Restore the previous compile context
    builder.restoreIP(old);
    builder.SetCurrentDebugLocation(olddl);
    nested_compile = last_n_c;

    if (optimize)
        jl_globalPM->run(*m.get());
    Function *f = (llvm::Function*)declarations.functionObject;
    Function *specf = (llvm::Function*)declarations.specFunctionObject;
    // swap declarations for definitions and destroy declarations
    if (specf) {
        Function *tempf = cast<Function>(m->getNamedValue(specf->getName()));
        delete specf;
        specf = tempf;
    }
    if (f) {
        Function *tempf = cast<Function>(m->getNamedValue(f->getName()));
        delete f;
        f = tempf;
    }
    // clone the name from the runtime linfo, if it exists
    // to give the user a (false) sense of stability
    Function *specf_decl = (Function*)linfo->functionObjectsDecls.specFunctionObject;
    if (specf_decl) {
        specf->setName(specf_decl->getName());
    }
    Function *f_decl = (Function*)linfo->functionObjectsDecls.functionObject;
    if (f_decl) {
        f->setName(f_decl->getName());
    }
    m.release(); // the return object `llvmf` will be the owning pointer
    JL_UNLOCK(&codegen_lock); // Might GC
    JL_GC_POP();
    if (getwrapper || !specf)
        return f;
    else
        return specf;
}


extern "C" JL_DLLEXPORT
void *jl_get_llvmf_decl(jl_method_instance_t *linfo, size_t world, bool getwrapper, const jl_cgparams_t params)
{
    if (linfo->def && linfo->def->source == NULL) {
        // not a generic function
        return NULL;
    }

    // compile this normally
    jl_llvm_functions_t decls = jl_compile_for_dispatch(&linfo, world);

    if (decls.functionObject == NULL && linfo->jlcall_api == 2 && linfo->def) {
        // normally we don't generate native code for these functions, so need an exception here
        // This leaks a bit of memory to cache native code that we'll never actually need
        JL_LOCK(&codegen_lock);
        decls = linfo->functionObjectsDecls;
        if (decls.functionObject == NULL) {
            jl_code_info_t *src = NULL;
            src = jl_type_infer(&linfo, world, 0);
            if (!src) {
                src = linfo->def->isstaged ? jl_code_for_staged(linfo) : linfo->def->source;
            }
            decls = jl_compile_linfo(&linfo, src, world, &params);
            linfo->functionObjectsDecls = decls;
        }
        JL_UNLOCK(&codegen_lock);
    }

    if (getwrapper || !decls.specFunctionObject)
        return decls.functionObject;
    else
        return decls.specFunctionObject;
}


// print an llvm IR acquired from jl_get_llvmf
// warning: this takes ownership of, and destroys, f->getParent()
extern "C" JL_DLLEXPORT
const jl_value_t *jl_dump_function_ir(void *f, bool strip_ir_metadata, bool dump_module)
{
    std::string code;
    llvm::raw_string_ostream stream(code);

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf || (!llvmf->isDeclaration() && !llvmf->getParent()))
        jl_error("jl_dump_function_ir: Expected Function* in a temporary Module");

    JL_LOCK(&codegen_lock); // Might GC
    if (!llvmf->getParent()) {
        // print the function declaration as-is
        llvmf->print(stream);
    }
    else {
        Module *m = llvmf->getParent();
        if (strip_ir_metadata) {
            // strip metadata from all instructions in the module
            for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
                Function *f2 = &*I;
                Function::BasicBlockListType::iterator f2_bb = f2->getBasicBlockList().begin();
                // iterate over all basic blocks in the function
                for (; f2_bb != f2->getBasicBlockList().end(); ++f2_bb) {
                    BasicBlock::InstListType::iterator f2_il = (*f2_bb).getInstList().begin();
                    // iterate over instructions in basic block
                    for (; f2_il != (*f2_bb).getInstList().end(); ) {
                        Instruction *inst = &*f2_il++;
                        // remove dbg.declare and dbg.value calls
                        if (isa<DbgDeclareInst>(inst) || isa<DbgValueInst>(inst)) {
                            inst->eraseFromParent();
                            continue;
                        }

                        SmallVector<std::pair<unsigned, MDNode*>, 4> MDForInst;
                        inst->getAllMetadata(MDForInst);
                        SmallVector<std::pair<unsigned, MDNode*>, 4>::iterator md_iter = MDForInst.begin();

                        // iterate over all metadata kinds and set to NULL to remove
                        for (; md_iter != MDForInst.end(); ++md_iter) {
                            inst->setMetadata((*md_iter).first, NULL);
                        }
                    }
                }
            }
        }
        if (dump_module) {
            m->print(stream, NULL);
        }
        else {
            llvmf->print(stream);
        }
        delete m;
    }
    JL_UNLOCK(&codegen_lock); // Might GC

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// This isn't particularly fast, but it's only used for interactive mode
static uint64_t compute_obj_symsize(const object::ObjectFile *obj, uint64_t offset)
{
    // Scan the object file for the closest symbols above and below offset in the .text section
    uint64_t lo = 0;
    uint64_t hi = 0;
    bool setlo = false;
#if JL_LLVM_VERSION >= 30700
    for (const object::SectionRef &Section : obj->sections()) {
#else
    llvm::error_code err;
    for (object::section_iterator I = obj->begin_sections(), E = obj->end_sections();
            !err && I != E; I.increment(err)) {
        object::SectionRef Section = *I;
#endif
        uint64_t SAddr, SSize;
#if JL_LLVM_VERSION >= 30500
        if (!Section.isText()) continue;
#else
        bool isText;
        if (Section.isText(isText) || !isText) continue;
#endif
#if JL_LLVM_VERSION >= 30600
        SAddr = Section.getAddress();
        SSize = Section.getSize();
#else
        Section.getAddress(SAddr);
        Section.getSize(SSize);
#endif
        if (offset < SAddr || offset >= SAddr + SSize) continue;
        assert(hi == 0);

        // test for lower and upper symbol bounds relative to other symbols
        hi = SAddr + SSize;
#if JL_LLVM_VERSION >= 30700
        object::section_iterator ESection = obj->section_end();
        for (const object::SymbolRef &Sym : obj->symbols()) {
#else
        llvm::error_code err;
        object::section_iterator ESection = obj->end_sections();
        for (object::symbol_iterator I = obj->begin_symbols(), E = obj->end_symbols();
                !err && I != E; I.increment(err)) {
            object::SymbolRef Sym = *I;
#endif
            uint64_t Addr;
            object::section_iterator Sect = ESection;
#if JL_LLVM_VERSION >= 30800
            auto SectOrError = Sym.getSection();
            assert(SectOrError);
            Sect = SectOrError.get();
#else
            if (Sym.getSection(Sect)) continue;
#endif
            if (Sect == ESection) continue;
            if (Sect != Section) continue;
#if JL_LLVM_VERSION >= 30700
            auto AddrOrError = Sym.getAddress();
            assert(AddrOrError);
            Addr = AddrOrError.get();
#else
            if (Sym.getAddress(Addr)) continue;
#endif
            if (Addr <= offset && Addr >= lo) {
                // test for lower bound on symbol
                lo = Addr;
                setlo = true;
            }
            if (Addr > offset && Addr < hi) {
                // test for upper bound on symbol
                hi = Addr;
            }
        }
    }
    if (setlo)
        return hi - lo;
    return 0;
}

// print a native disassembly for f (an LLVM function)
extern "C" JL_DLLEXPORT
const jl_value_t *jl_dump_function_asm(void *f, int raw_mc, const char* asm_variant="att")
{
    jl_ptls_t ptls = jl_get_ptls_states();
    std::string code;
    llvm::raw_string_ostream stream(code);
#if JL_LLVM_VERSION < 30700
    llvm::formatted_raw_ostream fstream(stream);
#endif

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_asm: Expected Function*");

    // Dump assembly code
    uint64_t symsize = 0;
    int64_t slide = 0, section_slide = 0;
    uint64_t fptr = getAddressForFunction(llvmf);
#ifdef USE_MCJIT
    // Look in the system image as well
    if (fptr == 0)
        fptr = (uintptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(llvmf);
    llvm::DIContext *context = NULL;
    llvm::DIContext *&objcontext = context;
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> context;
    llvm::DIContext *objcontext = NULL;
#endif
    const object::ObjectFile *object = NULL;
    assert(fptr != 0);
    if (!jl_DI_for_fptr(fptr, &symsize, &slide, &section_slide, &object, &context)) {
        if (!jl_dylib_DI_for_fptr(fptr, &object, &objcontext, &slide, &section_slide, false,
            NULL, NULL, NULL, NULL)) {
                jl_printf(JL_STDERR, "WARNING: Unable to find function pointer\n");
                return jl_cstr_to_string("");
        }
    }
    if (symsize == 0 && object != NULL)
        symsize = compute_obj_symsize(object, fptr + slide + section_slide);
    if (symsize == 0) {
        jl_printf(JL_STDERR, "WARNING: Could not determine size of symbol\n");
        return jl_cstr_to_string("");
    }

    if (raw_mc) {
        return (jl_value_t*)jl_pchar_to_array((char*)fptr, symsize);
    }

    int8_t gc_state = jl_gc_safe_enter(ptls);
    jl_dump_asm_internal(fptr, symsize, slide,
#ifndef USE_MCJIT
            context,
#endif
            object, objcontext,
#if JL_LLVM_VERSION >= 30700
            stream,
#else
            fstream,
#endif
            asm_variant
            );

#if JL_LLVM_VERSION < 30700
    fstream.flush();
#endif
    jl_gc_safe_leave(ptls, gc_state);

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// Logging for code coverage and memory allocation

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< std::vector<logdata_block*> > logdata_t;

static void visitLine(std::vector<logdata_block*> &vec, int line, Value *addend, const char* name)
{
    unsigned block = line / logdata_blocksize;
    line = line % logdata_blocksize;
    if (vec.size() <= block)
        vec.resize(block + 1);
    if (vec[block] == NULL) {
        vec[block] = (logdata_block*)calloc(1, sizeof(logdata_block));
    }
    logdata_block &data = *vec[block];
    if (data[line] == 0)
        data[line] = 1;
    Value *v = ConstantExpr::getIntToPtr(
        ConstantInt::get(T_size, (uintptr_t)&data[line]),
        T_pint64);
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true, name),
                                          addend),
                        v, true); // not atomic, so this might be an underestimate,
                                  // but it's faster this way
}

// Code coverage

static logdata_t coverageData;

static void coverageVisitLine(StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    visitLine(coverageData[filename], line, ConstantInt::get(T_int64, 1), "lcnt");
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0) {
        jl_gc_sync_total_bytes();
        return;
    }
    visitLine( mallocData[filename], line,
               builder.CreateCall(prepare_call(diff_gc_total_bytes_func)
#if JL_LLVM_VERSION >= 30700
                                  , {}
#endif
                                  ),
                        "bytecnt");
}

// Resets the malloc counts. Needed to avoid including memory usage
// from JITting.
extern "C" JL_DLLEXPORT void jl_clear_malloc_data(void)
{
    logdata_t::iterator it = mallocData.begin();
    for (; it != mallocData.end(); it++) {
        std::vector<logdata_block*> &bytes = (*it).second;
        std::vector<logdata_block*>::iterator itb;
        for (itb = bytes.begin(); itb != bytes.end(); itb++) {
            if (*itb) {
                logdata_block &data = **itb;
                for (int i = 0; i < logdata_blocksize; i++) {
                    if (data[i] > 0)
                        data[i] = 1;
                }
            }
        }
    }
    jl_gc_sync_total_bytes();
}

extern "C" int isabspath(const char *in);

static void write_log_data(logdata_t &logData, const char *extension)
{
    std::string base = std::string(jl_options.julia_home);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename = it->first();
        std::vector<logdata_block*> &values = it->second;
        if (!values.empty()) {
            if (!isabspath(filename.c_str()))
                filename = base + filename;
            std::ifstream inf(filename.c_str());
            if (inf.is_open()) {
                std::string outfile = filename + extension;
                std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out);
                char line[1024];
                int l = 1;
                unsigned block = 0;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
                    if (inf.fail() && !inf.bad()) {
                        // Read through lines longer than sizeof(line)
                        inf.clear();
                        inf.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
                    }
                    logdata_block *data = NULL;
                    if (block < values.size()) {
                        data = values[block];
                    }
                    uint64_t value = data ? (*data)[l] : 0;
                    if (++l >= logdata_blocksize) {
                        l = 0;
                        block++;
                    }
                    outf.width(9);
                    if (value == 0)
                        outf << '-';
                    else
                        outf << (value - 1);
                    outf.width(0);
                    outf << " " << line << std::endl;
                }
                outf.close();
                inf.close();
            }
        }
    }
}

extern "C" int jl_getpid();
extern "C" void jl_write_coverage_data(void)
{
    std::ostringstream stm;
    stm << jl_getpid();
    std::string outf = "." + stm.str() + ".cov";
    write_log_data(coverageData, outf.c_str());
}

extern "C" void jl_write_malloc_log(void)
{
    write_log_data(mallocData, ".mem");
}

// --- constant determination ---

static void show_source_loc(JL_STREAM *out, jl_codectx_t *ctx)
{
    if (ctx == NULL) return;
    jl_printf(out, "in %s at %s", ctx->name, ctx->file.str().c_str());
}

extern "C" void jl_binding_deprecation_warning(jl_binding_t *b);

static void cg_bdw(jl_binding_t *b, jl_codectx_t *ctx)
{
    jl_binding_deprecation_warning(b);
    if (jl_options.depwarn) {
        show_source_loc(JL_STDERR, ctx);
        jl_printf(JL_STDERR, "\n");
    }
}


// try to statically evaluate, NULL if not possible
static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, int sparams=true, int allow_alloc=true)
{
    if (!JL_FEAT_TEST(ctx, static_alloc)) allow_alloc = 0;
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (jl_is_const(ctx->module, sym))
            return jl_get_global(ctx->module, sym);
        return NULL;
    }
    if (jl_is_slot(ex))
        return NULL;
    if (jl_is_ssavalue(ex)) {
        ssize_t idx = ((jl_ssavalue_t*)ex)->id;
        assert(idx >= 0);
        if (ctx->ssavalue_assigned.at(idx)) {
            return ctx->SAvalues.at(idx).constant;
        }
        return NULL;
    }
    if (jl_is_quotenode(ex))
        return jl_fieldref(ex, 0);
    if (jl_is_method_instance(ex))
        return NULL;
    jl_module_t *m = NULL;
    jl_sym_t *s = NULL;
    if (jl_is_globalref(ex)) {
        s = jl_globalref_name(ex);
        jl_binding_t *b = jl_get_binding(jl_globalref_mod(ex), s);
        if (b && b->constp) {
            if (b->deprecated) cg_bdw(b, ctx);
            return b->value;
        }
        return NULL;
    }
    if (jl_is_expr(ex)) {
        jl_expr_t *e = (jl_expr_t*)ex;
        if (e->head == call_sym) {
            jl_value_t *f = static_eval(jl_exprarg(e, 0), ctx, sparams, allow_alloc);
            if (f) {
                if (jl_array_dim0(e->args) == 3 && f==jl_builtin_getfield) {
                    m = (jl_module_t*)static_eval(jl_exprarg(e, 1), ctx, sparams, allow_alloc);
                    s = (jl_sym_t*)static_eval(jl_exprarg(e, 2), ctx, sparams, allow_alloc);
                    if (m && jl_is_module(m) && s && jl_is_symbol(s)) {
                        jl_binding_t *b = jl_get_binding(m, s);
                        if (b && b->constp) {
                            if (b->deprecated) cg_bdw(b, ctx);
                            return b->value;
                        }
                    }
                }
                else if (f==jl_builtin_tuple || f==jl_builtin_apply_type) {
                    size_t i;
                    size_t n = jl_array_dim0(e->args)-1;
                    if (n==0 && f==jl_builtin_tuple) return (jl_value_t*)jl_emptytuple;
                    if (!allow_alloc)
                        return NULL;
                    jl_value_t **v;
                    JL_GC_PUSHARGS(v, n);
                    for (i = 0; i < n; i++) {
                        v[i] = static_eval(jl_exprarg(e, i+1), ctx, sparams, allow_alloc);
                        if (v[i] == NULL) {
                            JL_GC_POP();
                            return NULL;
                        }
                    }
                    jl_value_t *result;
                    JL_TRY {
                        if (f == jl_builtin_tuple)
                            result = jl_f_tuple(NULL, v, n);
                        else
                            result = jl_f_apply_type(NULL, v, n);
                    }
                    JL_CATCH {
                        result = NULL;
                    }
                    JL_GC_POP();
                    return result;
                }
            }
        }
        else if (e->head == static_parameter_sym) {
            size_t idx = jl_unbox_long(jl_exprarg(e, 0));
            if (idx <= jl_svec_len(ctx->linfo->sparam_vals)) {
                jl_value_t *e = jl_svecref(ctx->linfo->sparam_vals, idx - 1);
                if (jl_is_typevar(e))
                    return NULL;
                return e;
            }
        }
        return NULL;
    }
    return ex;
}

static bool is_constant(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true)
{
    return static_eval(ex, ctx, sparams) != NULL;
}

static bool slot_eq(jl_value_t *e, int sl)
{
    return jl_is_slot(e) && jl_slot_number(e)-1 == sl;
}

// --- code gen for intrinsic functions ---

#include "intrinsics.cpp"

// --- find volatile variables ---

// assigned in a try block and used outside that try block

static bool local_var_occurs(jl_value_t *e, int sl)
{
    if (slot_eq(e, sl)) {
        return true;
    }
    else if (jl_is_expr(e)) {
        jl_expr_t *ex = (jl_expr_t*)e;
        size_t alength = jl_array_dim0(ex->args);
        for(int i=0; i < (int)alength; i++) {
            if (local_var_occurs(jl_exprarg(ex,i),sl))
                return true;
        }
    }
    return false;
}

static std::set<int> assigned_in_try(jl_array_t *stmts, int s, long l, int *pend)
{
    std::set<int> av;
    size_t slength = jl_array_dim0(stmts);
    for(int i=s; i < (int)slength; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == assign_sym) {
                jl_value_t *ar = jl_exprarg(st, 0);
                if (jl_is_slot(ar)) {
                    av.insert(jl_slot_number(ar)-1);
                }
            }
        }
        if (jl_is_labelnode(st)) {
            if (jl_labelnode_label(st) == l) {
                *pend = i;
                break;
            }
        }
    }
    return av;
}

static void mark_volatile_vars(jl_array_t *stmts, std::vector<jl_varinfo_t> &slots)
{
    size_t slength = jl_array_dim0(stmts);
    for(int i=0; i < (int)slength; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == enter_sym) {
                int last = (int)slength-1;
                std::set<int> as =
                    assigned_in_try(stmts, i+1,
                                    jl_unbox_long(jl_exprarg(st,0)), &last);
                for(int j=0; j < (int)slength; j++) {
                    if (j < i || j > last) {
                        std::set<int>::iterator it = as.begin();
                        for(; it != as.end(); it++) {
                            if (local_var_occurs(jl_array_ptr_ref(stmts,j), *it)) {
                                jl_varinfo_t &vi = slots[*it];
                                if (!vi.value.constant)
                                    vi.isVolatile = true;
                            }
                        }
                    }
                }
            }
        }
    }
}

// --- escape analysis ---

static bool expr_is_symbol(jl_value_t *e)
{
    return (jl_is_symbol(e) || jl_is_globalref(e));
}

// a very simple, conservative escape analysis that is sufficient for
// eliding allocation of varargs tuples.
// "esc" means "in escaping context"
static void simple_escape_analysis(jl_value_t *expr, bool esc, jl_codectx_t *ctx)
{
    if (jl_is_expr(expr)) {
        esc = true;
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i;
        if (e->head == call_sym || e->head == new_sym) {
            int alen = jl_array_dim0(e->args);
            jl_value_t *f = jl_exprarg(e,0);
            simple_escape_analysis(f, esc, ctx);
            if (expr_is_symbol(f)) {
                if (jl_value_t *fv = static_eval(f, ctx, false)) {
                    if (jl_typeis(fv, jl_intrinsic_type)) {
                        esc = false;
                    }
                    else {
                        if ((fv==jl_builtin_getfield && alen==3 &&
                             expr_type(jl_exprarg(e,2),ctx) == (jl_value_t*)jl_long_type) ||
                            fv==jl_builtin_nfields ||
                            (fv==jl_builtin__apply && alen==3)) {
                            esc = false;
                        }
                    }
                }
            }

            for (i = 1; i < (size_t)alen; i++) {
                simple_escape_analysis(jl_exprarg(e,i), esc, ctx);
            }
        }
        else if (e->head == foreigncall_sym) {
            esc = true;
            simple_escape_analysis(jl_exprarg(e, 0), esc, ctx);
            // 2nd and 3d arguments are static
            size_t alen = jl_array_dim0(e->args);
            for (i = 3; i < alen; i += 2) {
                simple_escape_analysis(jl_exprarg(e, i), esc, ctx);
            }
        }
        else if (e->head == method_sym) {
            simple_escape_analysis(jl_exprarg(e,0), esc, ctx);
            if (jl_expr_nargs(e) > 1) {
                simple_escape_analysis(jl_exprarg(e,1), esc, ctx);
                simple_escape_analysis(jl_exprarg(e,2), esc, ctx);
            }
        }
        else if (e->head == assign_sym) {
            // don't consider assignment LHS as a variable "use"
            simple_escape_analysis(jl_exprarg(e,1), esc, ctx);
        }
        else if (e->head != line_sym) {
            size_t elen = jl_array_dim0(e->args);
            for(i=0; i < elen; i++) {
                simple_escape_analysis(jl_exprarg(e,i), esc, ctx);
            }
        }
        return;
    }
    if (jl_is_slot(expr)) {
        int i = jl_slot_number(expr)-1;
        jl_varinfo_t &vi = ctx->slots[i];
        vi.escapes |= esc;
        vi.used = true;
    }
}

// --- gc root utils ---

// ---- Get Element Pointer (GEP) instructions within the GC frame ----

// Emit a gc-root slot indicator
static Value *emit_local_root(jl_codectx_t *ctx, jl_varinfo_t *vi)
{
    CallInst *newroot = CallInst::Create(prepare_call(gcroot_func), "", /*InsertBefore*/ctx->ptlsStates);
    if (vi) {
        vi->memloc->replaceAllUsesWith(newroot);
        newroot->takeName(vi->memloc);
        vi->memloc = newroot;
    }
    return newroot;
}


// Marks a use (and thus a potential kill) of a gcroot
// Note that if the operation that needs the root has terminating control flow
// (e.g. `unreachable`, `noreturn` functions) the use needs to be marked before
// the operation as well as after it.
static void mark_gc_use(const jl_cgval_t &v)
{
    if (v.gcroot)
        builder.CreateCall(prepare_call(gckill_func), v.gcroot);
}

// turn an array of arguments into a single object suitable for passing to a jlcall
static Value *make_jlcall(ArrayRef<const jl_cgval_t*> args, jl_codectx_t *ctx)
{
    // the temporary variables are after all local variables in the GC frame.
    CallInst *largs = CallInst::Create(prepare_call(jlcall_frame_func),
            ConstantInt::get(T_int32, args.size()),
            "",
            /*InsertBefore*/ctx->ptlsStates);
    int slot = 0;
    assert(args.size() > 0);
    for (ArrayRef<const jl_cgval_t*>::iterator I = args.begin(), E = args.end(); I < E; ++I, ++slot) {
        Value *arg = boxed(**I, ctx, false); // mark_gc_use isn't needed since jlcall_frame_func can take ownership of this root
        GetElementPtrInst *newroot = GetElementPtrInst::Create(LLVM37_param(NULL) largs,
                ArrayRef<Value*>(ConstantInt::get(T_int32, slot)));
        newroot->insertAfter(ctx->ptlsStates);
        builder.CreateStore(arg, newroot);
    }
    return largs;
}

static void jl_add_method_root(jl_codectx_t *ctx, jl_value_t *val)
{
    if (jl_is_leaf_type(val) || jl_is_bool(val) || jl_is_symbol(val) ||
            val == (jl_value_t*)jl_any_type || val == (jl_value_t*)jl_bottom_type)
        return;
    JL_GC_PUSH1(&val);
    if (ctx->roots == NULL) {
        ctx->roots = jl_alloc_vec_any(1);
        jl_array_ptr_set(ctx->roots, 0, val);
    }
    else {
        size_t rlen = jl_array_dim0(ctx->roots);
        for (size_t i = 0; i < rlen; i++) {
            if (jl_array_ptr_ref(ctx->roots,i) == val) {
                JL_GC_POP();
                return;
            }
        }
        jl_array_ptr_1d_push(ctx->roots, val);
    }
    JL_GC_POP();
}

// --- generating function calls ---

static jl_cgval_t emit_getfield(jl_value_t *expr, jl_sym_t *name, jl_codectx_t *ctx)
{
    if (jl_is_quotenode(expr) && jl_is_module(jl_fieldref(expr,0)))
        expr = jl_fieldref(expr,0);

    jl_value_t *static_val = static_eval(expr, ctx, true, false);
    if (static_val != NULL && jl_is_module(static_val))
        expr = static_val;

    if (jl_is_module(expr)) {
        jl_binding_t *bnd = NULL;
        Value *bp = global_binding_pointer((jl_module_t*)expr, name, &bnd, false, ctx);
        // TODO: refactor. this partially duplicates code in emit_var
        if (bnd && bnd->value != NULL) {
            if (bnd->constp) {
                return mark_julia_const(bnd->value);
            }
            return mark_julia_type(tbaa_decorate(tbaa_binding, builder.CreateLoad(bp)), true, (jl_value_t*)jl_any_type, ctx);
        }
        // todo: use type info to avoid undef check
        return emit_checked_var(bp, name, ctx, false, tbaa_binding);
    }

    jl_datatype_t *sty = (jl_datatype_t*)expr_type(expr, ctx);
    JL_GC_PUSH1(&sty);
    if (jl_is_type_type((jl_value_t*)sty) && jl_is_leaf_type(jl_tparam0(sty)))
        sty = (jl_datatype_t*)jl_typeof(jl_tparam0(sty));
    if (jl_is_structtype(sty) && sty != jl_module_type && sty->uid != 0 &&
        jl_is_leaf_type((jl_value_t*)sty)) {
        unsigned idx = jl_field_index(sty, name, 0);
        if (idx != (unsigned)-1) {
            jl_cgval_t strct = emit_expr(expr, ctx);
            jl_cgval_t fld = emit_getfield_knownidx(strct, idx, sty, ctx);
            JL_GC_POP();
            return fld;
        }
    }
    JL_GC_POP(); // kill sty
    // TODO: attempt better codegen for approximate types, if the types
    // and offsets of some fields are independent of parameters.

    // TODO: generic getfield func with more efficient calling convention
    jl_cgval_t arg1 = emit_expr(expr, ctx);
    jl_cgval_t arg2 = mark_julia_const((jl_value_t*)name);
    const jl_cgval_t* myargs_array[2] = {&arg1, &arg2};
    Value *myargs = make_jlcall(makeArrayRef(myargs_array), ctx);
#if JL_LLVM_VERSION >= 30700
    Value *result = builder.CreateCall(prepare_call(jlgetfield_func), {V_null, myargs,
                                        ConstantInt::get(T_int32,2)});
#else
    Value *result = builder.CreateCall3(prepare_call(jlgetfield_func), V_null, myargs,
                                        ConstantInt::get(T_int32,2));
#endif
    bool needsgcroot = true; // !arg1.isimmutable || !jl_is_leaf_type(arg1.typ) || !is_datatype_all_pointers((jl_datatype_t*)arg1.typ); // TODO: probably want this as a llvm pass
    jl_cgval_t ret = mark_julia_type(result, true, jl_any_type, ctx, needsgcroot); // (typ will be patched up by caller)
    return ret;
}

static Value *emit_bits_compare(const jl_cgval_t &arg1, const jl_cgval_t &arg2, jl_codectx_t *ctx)
{
    assert(jl_is_datatype(arg1.typ) && arg1.typ == arg2.typ);
    Type *at = julia_type_to_llvm(arg1.typ);

    if (at->isIntegerTy() || at->isPointerTy() || at->isFloatingPointTy()) {
        Type *at_int = INTT(at);
        Value *varg1 = emit_unbox(at_int, arg1, arg1.typ);
        Value *varg2 = emit_unbox(at_int, arg2, arg2.typ);
        return builder.CreateICmpEQ(varg1, varg2);
    }

    if (at->isVectorTy()) {
        jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
        Value *answer = ConstantInt::get(T_int1, 1);
        Value *varg1 = emit_unbox(at, arg1, arg1.typ);
        Value *varg2 = emit_unbox(at, arg2, arg2.typ);
        size_t l = jl_svec_len(types);
        for (unsigned i = 0; i < l; i++) {
            jl_value_t *fldty = jl_svecref(types, i);
            Value *subAns, *fld1, *fld2;
            fld1 = builder.CreateExtractElement(varg1, ConstantInt::get(T_int32, i)),
            fld2 = builder.CreateExtractElement(varg2, ConstantInt::get(T_int32, i)),
            subAns = emit_bits_compare(mark_julia_type(fld1, false, fldty, ctx), mark_julia_type(fld2, false, fldty, ctx), ctx);
            answer = builder.CreateAnd(answer, subAns);
        }
        return answer;
    }

    if (at->isAggregateType()) { // Struct or Array
        assert(arg1.ispointer() && arg2.ispointer());
        size_t sz = jl_datatype_size(arg1.typ);
        if (sz > 512 && !((jl_datatype_t*)arg1.typ)->layout->haspadding) {
#if JL_LLVM_VERSION >= 30700
            Value *answer = builder.CreateCall(prepare_call(memcmp_func),
                            {
                            data_pointer(arg1, ctx, T_pint8),
                            data_pointer(arg2, ctx, T_pint8),
                            ConstantInt::get(T_size, sz)
                            });
#else
            Value *answer = builder.CreateCall3(prepare_call(memcmp_func),
                    data_pointer(arg1, ctx, T_pint8),
                    data_pointer(arg2, ctx, T_pint8),
                    ConstantInt::get(T_size, sz));
#endif
            return builder.CreateICmpEQ(answer, ConstantInt::get(T_int32, 0));
        }
        else {
            Type *atp = at->getPointerTo();
            Value *varg1 = data_pointer(arg1, ctx, atp);
            Value *varg2 = data_pointer(arg2, ctx, atp);
            jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
            Value *answer = ConstantInt::get(T_int1, 1);
            size_t l = jl_svec_len(types);
            for(unsigned i=0; i < l; i++) {
                jl_value_t *fldty = jl_svecref(types, i);
                Value *subAns, *fld1, *fld2;
                fld1 = builder.CreateConstGEP2_32(LLVM37_param(at) varg1, 0, i);
                fld2 = builder.CreateConstGEP2_32(LLVM37_param(at) varg2, 0, i);
                if (type_is_ghost(fld1->getType()->getPointerElementType()))
                    continue;
                subAns = emit_bits_compare(mark_julia_slot(fld1, fldty, arg1.tbaa), mark_julia_slot(fld2, fldty, arg2.tbaa), ctx);
                answer = builder.CreateAnd(answer, subAns);
            }
            return answer;
        }
    }
    assert(0 && "what is this llvm type?");
    abort();
}

// emit code for is (===).
static Value *emit_f_is(const jl_cgval_t &arg1, const jl_cgval_t &arg2, jl_codectx_t *ctx)
{
    jl_value_t *rt1 = arg1.typ, *rt2 = arg2.typ;
    bool isleaf = jl_is_leaf_type(rt1) && jl_is_leaf_type(rt2);
    if (isleaf && rt1 != rt2 && !jl_is_type_type(rt1) && !jl_is_type_type(rt2))
        // disjoint leaf types are never equal (quick test)
        return ConstantInt::get(T_int1, 0);
    if (arg1.isghost || (isleaf && jl_is_datatype_singleton((jl_datatype_t*)rt1))) {
        if (arg2.isghost || (isleaf && jl_is_datatype_singleton((jl_datatype_t*)rt2))) {
            if (rt1 == rt2) {
                // singleton objects of the same type
                return ConstantInt::get(T_int1, 1);
            }
        }
    }

    if (jl_type_intersection(rt1, rt2) == (jl_value_t*)jl_bottom_type) // types are disjoint (exhaustive test)
        return ConstantInt::get(T_int1, 0);

    bool isbits = isleaf && jl_isbits(rt1) && jl_types_equal(rt1, rt2);
    if (isbits) { // whether this type is unique'd by value
        return emit_bits_compare(arg1, arg2, ctx);
    }

    int ptr_comparable = 0; // whether this type is unique'd by pointer
    if (rt1==(jl_value_t*)jl_sym_type || rt2==(jl_value_t*)jl_sym_type ||
        jl_is_mutable_datatype(rt1) || jl_is_mutable_datatype(rt2)) // excludes abstract types
        ptr_comparable = 1;
    if (jl_subtype(rt1, (jl_value_t*)jl_type_type) ||
        jl_subtype(rt2, (jl_value_t*)jl_type_type)) // use typeseq for datatypes
        ptr_comparable = 0;
    if ((jl_is_type_type(rt1) && jl_is_leaf_type(jl_tparam0(rt1))) ||
        (jl_is_type_type(rt2) && jl_is_leaf_type(jl_tparam0(rt2)))) // can compare leaf types by pointer
        ptr_comparable = 1;
    if (ptr_comparable) {
        assert(arg1.isboxed && arg2.isboxed); // only boxed types are valid for pointer comparison
        return builder.CreateICmpEQ(boxed(arg1, ctx), boxed(arg2, ctx));
    }

    JL_FEAT_REQUIRE(ctx, runtime);
    Value *varg1 = boxed(arg1, ctx);
    Value *varg2 = boxed(arg2, ctx, false); // potentially unrooted!
#if JL_LLVM_VERSION >= 30700
    return builder.CreateTrunc(builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
#else
    return builder.CreateTrunc(builder.CreateCall2(prepare_call(jlegal_func), varg1, varg2), T_int1);
#endif
}

static bool emit_builtin_call(jl_cgval_t *ret, jl_value_t *f, jl_value_t **args, size_t nargs,
                              jl_codectx_t *ctx, jl_value_t *expr)
// returns true if the call has been handled
{
    jl_value_t *rt1=NULL, *rt2=NULL, *rt3=NULL;
    JL_GC_PUSH3(&rt1, &rt2, &rt3);

    if (f==jl_builtin_is && nargs==2) {
        // handle simple static expressions with no side-effects
        rt1 = static_eval(args[1], ctx, true);
        if (rt1) {
            rt2 = static_eval(args[2], ctx, true);
            if (rt2) {
                *ret = mark_julia_type(ConstantInt::get(T_int8, jl_egal(rt1, rt2)), false, jl_bool_type, ctx);
                JL_GC_POP();
                return true;
            }
        }
        // emit values
        jl_cgval_t v1 = emit_expr(args[1], ctx);
        jl_cgval_t v2 = emit_expr(args[2], ctx);
        // FIXME: v.typ is roughly equiv. to expr_type, but with typeof(T) == Type{T} instead of DataType in a few cases
        if (v1.typ == (jl_value_t*)jl_datatype_type)
            v1 = remark_julia_type(v1, expr_type(args[1], ctx)); // patch up typ if necessary
        if (v2.typ == (jl_value_t*)jl_datatype_type)
            v2 = remark_julia_type(v2, expr_type(args[2], ctx)); // patch up typ if necessary
        // emit comparison test
        Value *ans = emit_f_is(v1, v2, ctx);
        mark_gc_use(v1);
        mark_gc_use(v2);
        *ret = mark_julia_type(builder.CreateZExt(ans,T_int8), false, jl_bool_type, ctx);
        JL_GC_POP();
        return true;
    }

    else if (f==jl_builtin_typeof && nargs==1) {
        jl_cgval_t arg1 = emit_expr(args[1], ctx);
        *ret = emit_typeof(arg1,ctx);
        JL_GC_POP();
        return true;
    }

    else if (f==jl_builtin_typeassert && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0)) {
                *ret = emit_expr(args[1], ctx);
                JL_GC_POP();
                return true;
            }
            if (tp0 == jl_bottom_type) {
                emit_expr(args[1], ctx);
                *ret = jl_cgval_t();
                emit_error("reached code declared unreachable", ctx);
                JL_GC_POP();
                return true;
            }
            if (!jl_is_tuple_type(tp0) && jl_is_leaf_type(tp0)) {
                *ret = emit_expr(args[1], ctx);
                emit_typecheck(*ret, tp0, "typeassert", ctx);
                if (ret->isboxed)
                    *ret = remark_julia_type(*ret, expr_type(expr, ctx));
                JL_GC_POP();
                return true;
            }
        }
        if (jl_subtype(ty, (jl_value_t*)jl_type_type)) {
            *ret = emit_expr(args[1], ctx);
            JL_FEAT_REQUIRE(ctx, runtime);
#if JL_LLVM_VERSION >= 30700
            builder.CreateCall(prepare_call(jltypeassert_func), {boxed(*ret, ctx), boxed(emit_expr(args[2], ctx), ctx)});
#else
            builder.CreateCall2(prepare_call(jltypeassert_func), boxed(*ret, ctx), boxed(emit_expr(args[2], ctx), ctx));
#endif
            JL_GC_POP();
            return true;
        }
    }

    else if (f==jl_builtin_isa && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (arg == jl_bottom_type) {
            emit_expr(args[1], ctx);
            *ret = jl_cgval_t();
            JL_GC_POP();
            return true;
        }
        if (jl_is_type_type(ty) && !jl_has_free_typevars(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0)) {
                emit_expr(args[1], ctx);  // TODO remove if no side effects
                *ret = mark_julia_type(ConstantInt::get(T_int8, 1), false, jl_bool_type, ctx);
                JL_GC_POP();
                return true;
            }
            if (!jl_subtype(tp0, (jl_value_t*)jl_type_type)) {
                if (jl_is_leaf_type(arg)) {
                    emit_expr(args[1], ctx);  // TODO remove if no side effects
                    *ret = mark_julia_type(ConstantInt::get(T_int8, 0), false, jl_bool_type, ctx);
                    JL_GC_POP();
                    return true;
                }
                if (jl_is_leaf_type(tp0)) {
                    jl_cgval_t arg1 = emit_expr(args[1], ctx);
                    *ret = mark_julia_type(builder.CreateZExt(builder.CreateICmpEQ(emit_typeof_boxed(arg1,ctx),
                                                                                   literal_pointer_val(tp0)),
                                                              T_int8),
                                           false,
                                           jl_bool_type, ctx);
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f==jl_builtin_issubtype && nargs == 2) {
        rt1 = expr_type(args[1], ctx);
        rt2 = expr_type(args[2], ctx);
        if (jl_is_type_type(rt1) && !jl_is_typevar(jl_tparam0(rt1)) &&
            jl_is_type_type(rt2) && !jl_is_typevar(jl_tparam0(rt2))) {
            int issub = jl_subtype(jl_tparam0(rt1), jl_tparam0(rt2));
            // TODO: emit args[1] and args[2] in case of side effects?
            *ret = mark_julia_type(ConstantInt::get(T_int8, issub), false, jl_bool_type, ctx);
            JL_GC_POP();
            return true;
        }
    }

    else if (f==jl_builtin__apply && nargs==2 && ctx->vaStack && slot_eq(args[2], ctx->vaSlot)) {
        // turn Core._apply(f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the vaStack allocation
        Value *theF = boxed(emit_expr(args[1], ctx), ctx);
        Value *nva = emit_n_varargs(ctx);
#ifdef _P64
        nva = builder.CreateTrunc(nva, T_int32);
#endif
        JL_FEAT_REQUIRE(ctx, runtime);
        Value *r =
#if JL_LLVM_VERSION >= 30700
            builder.CreateCall(prepare_call(jlapply2va_func), {theF,
                                builder.CreateGEP(ctx->argArray,
                                                  ConstantInt::get(T_size, ctx->nReqArgs)),
                                nva});
#else
            builder.CreateCall3(prepare_call(jlapply2va_func), theF,
                                builder.CreateGEP(ctx->argArray,
                                                  ConstantInt::get(T_size, ctx->nReqArgs)),
                                nva);
#endif
        *ret = mark_julia_type(r, true, expr_type(expr, ctx), ctx);
        JL_GC_POP();
        return true;
    }

    else if (f==jl_builtin_tuple) {
        if (nargs == 0) {
            *ret = ghostValue(jl_typeof(jl_emptytuple));
            JL_GC_POP();
            return true;
        }
        if (ctx->source->inferred) {
            rt1 = expr_type(expr, ctx);
            if (jl_is_tuple_type(rt1) && jl_is_leaf_type(rt1) && nargs == jl_datatype_nfields(rt1)) {
                *ret = emit_new_struct(rt1, nargs+1, args, ctx);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f==jl_builtin_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx), ctx, false); // rooted by throw
        raise_exception(arg1, ctx);
        *ret = jl_cgval_t();
        JL_GC_POP();
        return true;
    }

    else if (f==jl_builtin_arraysize && nargs==2) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        aty = jl_unwrap_unionall(aty);
        if (jl_is_array_type(aty) && ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ndp = jl_tparam1(aty);
            if (jl_is_long(ndp)) {
                jl_cgval_t ary = emit_expr(args[1], ctx);
                size_t ndims = jl_unbox_long(ndp);
                if (jl_is_long(args[2])) {
                    uint32_t idx = (uint32_t)jl_unbox_long(args[2]);
                    if (idx > 0 && idx <= ndims) {
                        *ret = mark_julia_type(emit_arraysize(ary, args[1], idx, ctx), false, jl_long_type, ctx);
                        JL_GC_POP();
                        return true;
                    }
                    else if (idx > ndims) {
                        *ret = mark_julia_type(ConstantInt::get(T_size, 1), false, jl_long_type, ctx);
                        JL_GC_POP();
                        return true;
                    }
                }
                else {
                    Value *idx = emit_unbox(T_size, emit_expr(args[2], ctx), ity);
                    error_unless(builder.CreateICmpSGT(idx,
                                                      ConstantInt::get(T_size,0)),
                                 "arraysize: dimension out of range", ctx);
                    BasicBlock *outBB = BasicBlock::Create(jl_LLVMContext,"outofrange",ctx->f);
                    BasicBlock *inBB = BasicBlock::Create(jl_LLVMContext,"inrange");
                    BasicBlock *ansBB = BasicBlock::Create(jl_LLVMContext,"arraysize");
                    builder.CreateCondBr(builder.CreateICmpSLE(idx,
                                                              ConstantInt::get(T_size, ndims)),
                                         inBB, outBB);
                    builder.SetInsertPoint(outBB);
                    Value *v_one = ConstantInt::get(T_size, 1);
                    builder.CreateBr(ansBB);
                    ctx->f->getBasicBlockList().push_back(inBB);
                    builder.SetInsertPoint(inBB);
                    Value *v_sz = emit_arraysize(ary, idx, ctx);
                    builder.CreateBr(ansBB);
                    ctx->f->getBasicBlockList().push_back(ansBB);
                    builder.SetInsertPoint(ansBB);
                    PHINode *result = builder.CreatePHI(T_size, 2);
                    result->addIncoming(v_one, outBB);
                    result->addIncoming(v_sz, inBB);
                    *ret = mark_julia_type(result, false, jl_long_type, ctx);
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f==jl_builtin_arrayref && nargs>=2) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        bool indexes_ok = true;
        for (size_t i=2; i <= nargs; i++) {
            if (expr_type(args[i], ctx) != (jl_value_t*)jl_long_type) {
                indexes_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(aty);
        if (jl_is_array_type(aty_dt) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            if (!jl_has_free_typevars(ety)) { // TODO: jn/foreigncall branch has a better predicate
                if (!jl_array_store_unboxed(ety))
                    ety = (jl_value_t*)jl_any_type;
                jl_value_t *ndp = jl_tparam1(aty_dt);
                if (jl_is_long(ndp) || nargs==2) {
                    jl_cgval_t ary = emit_expr(args[1], ctx);
                    ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[2], nargs-1, ctx);
                    if (jl_array_store_unboxed(ety) &&
                        jl_datatype_size(ety) == 0) {
                        assert(jl_is_datatype(ety));
                        assert(((jl_datatype_t*)ety)->instance != NULL);
                        *ret = ghostValue(ety);
                    }
                    else {
                        *ret = typed_load(emit_arrayptr(ary, args[1], ctx), idx, ety, ctx, tbaa_arraybuf);
                    }
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f==jl_builtin_arrayset && nargs>=3) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *vty = expr_type(args[2], ctx); rt2 = vty;
        bool indexes_ok = true;
        for (size_t i=3; i <= nargs; i++) {
            if (expr_type(args[i], ctx) != (jl_value_t*)jl_long_type) {
                indexes_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(aty);
        if (jl_is_array_type(aty_dt) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            if (!jl_has_free_typevars(ety) && jl_subtype(vty, ety)) { // TODO: jn/foreigncall branch has a better predicate
                if (!jl_array_store_unboxed(ety))
                    ety = (jl_value_t*)jl_any_type;
                jl_value_t *ndp = jl_tparam1(aty_dt);
                if (jl_is_long(ndp) || nargs==3) {
                    jl_cgval_t ary = emit_expr(args[1], ctx);
                    ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[3], nargs-2, ctx);
                    bool isboxed = !jl_array_store_unboxed(ety);
                    if (!isboxed && jl_datatype_size(ety) == 0) {
                        // no-op, but emit expr for possible effects
                        assert(jl_is_datatype(ety));
                        emit_expr(args[2], ctx);
                    }
                    else {
                        jl_cgval_t v = emit_expr(args[2], ctx);
                        PHINode *data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed) { // if not boxed we don't need a write barrier
                            assert(ary.isboxed);
                            Value *aryv = boxed(ary, ctx);
                            Value *flags = emit_arrayflags(ary, ctx);
                            // the owner of the data is ary itself except if ary->how == 3
                            flags = builder.CreateAnd(flags, 3);
                            Value *is_owned = builder.CreateICmpEQ(flags, ConstantInt::get(T_int16, 3));
                            BasicBlock *curBB = builder.GetInsertBlock();
                            BasicBlock *ownedBB = BasicBlock::Create(jl_LLVMContext, "array_owned", ctx->f);
                            BasicBlock *mergeBB = BasicBlock::Create(jl_LLVMContext, "merge_own", ctx->f);
                            builder.CreateCondBr(is_owned, ownedBB, mergeBB);
                            builder.SetInsertPoint(ownedBB);
                            // load owner pointer
                            Value *own_ptr;
                            if (jl_is_long(ndp)) {
                                own_ptr = tbaa_decorate(tbaa_const, builder.CreateLoad(
                                    emit_bitcast(
                                        builder.CreateConstGEP1_32(
                                            emit_bitcast(aryv, T_pint8),
                                            jl_array_data_owner_offset(nd)),
                                        T_ppjlvalue)));
                            }
                            else {
#if JL_LLVM_VERSION >= 30700
                                own_ptr = builder.CreateCall(
                                    prepare_call(jlarray_data_owner_func),
                                    {aryv});
#else
                                own_ptr = builder.CreateCall(
                                    prepare_call(jlarray_data_owner_func),
                                    aryv);
#endif
                            }
                            builder.CreateBr(mergeBB);
                            builder.SetInsertPoint(mergeBB);
                            data_owner = builder.CreatePHI(T_pjlvalue, 2);
                            data_owner->addIncoming(aryv, curBB);
                            data_owner->addIncoming(own_ptr, ownedBB);
                        }
                        typed_store(emit_arrayptr(ary,args[1],ctx), idx, v,
                                    ety, ctx, tbaa_arraybuf, data_owner, 0,
                                    false); // don't need to root the box if we had to make one since it's being stored in the array immediatly
                    }
                    *ret = ary;
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f==jl_builtin_getfield && nargs==2) {
        if (jl_is_quotenode(args[2]) && jl_is_symbol(jl_fieldref(args[2],0))) {
            *ret = emit_getfield(args[1],
                                 (jl_sym_t*)jl_fieldref(args[2],0), ctx);
            if (ret->typ == (jl_value_t*)jl_any_type) // improve the type, if known from the expr
                *ret = remark_julia_type(*ret, expr_type(expr, ctx));
            JL_GC_POP();
            return true;
        }
        jl_datatype_t *stt = (jl_datatype_t*)expr_type(args[1], ctx);
        jl_value_t *fldt   = expr_type(args[2], ctx);

        // VA tuple
        if (ctx->vaStack && slot_eq(args[1], ctx->vaSlot)) {
            Value *valen = emit_n_varargs(ctx);
            Value *idx = emit_unbox(T_size, emit_expr(args[2], ctx), fldt);
            idx = emit_bounds_check(
                    jl_cgval_t(builder.CreateGEP(ctx->argArray, ConstantInt::get(T_size, ctx->nReqArgs)), NULL, false, NULL),
                    NULL, idx, valen, ctx);
            idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
            *ret = mark_julia_type(
                tbaa_decorate(tbaa_value, builder.CreateLoad(builder.CreateGEP(ctx->argArray, idx))),
                /*boxed*/ true, expr_type(expr, ctx), ctx, /*needsgcroot*/ false);
            JL_GC_POP();
            return true;
        }

        if (fldt == (jl_value_t*)jl_long_type && jl_is_leaf_type((jl_value_t*)stt)) {
            if ((jl_is_structtype(stt) || jl_is_tuple_type(stt)) && !jl_subtype((jl_value_t*)jl_module_type, (jl_value_t*)stt)) {
                size_t nfields = jl_datatype_nfields(stt);
                jl_cgval_t strct = emit_expr(args[1], ctx);
                // integer index
                size_t idx;
                if (jl_is_long(args[2]) && (idx=jl_unbox_long(args[2])-1) < nfields) {
                    // known index
                    *ret = emit_getfield_knownidx(strct, idx, stt, ctx);
                    JL_GC_POP();
                    return true;
                }
                else {
                    // unknown index
                    Value *vidx = emit_unbox(T_size, emit_expr(args[2], ctx), (jl_value_t*)jl_long_type);
                    if (emit_getfield_unknownidx(ret, strct, vidx, stt, ctx)) {
                        if (ret->typ == (jl_value_t*)jl_any_type) // improve the type, if known from the expr
                            ret->typ = expr_type(expr, ctx);
                        JL_GC_POP();
                        return true;
                    }
                }
            }
        }
    }

    else if (f==jl_builtin_setfield && nargs==3) {
        jl_datatype_t *sty = (jl_datatype_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_structtype(sty) && sty != jl_module_type &&
            jl_is_quotenode(args[2]) && jl_is_symbol(jl_fieldref(args[2],0))) {
            size_t idx = jl_field_index(sty,
                                        (jl_sym_t*)jl_fieldref(args[2],0), 0);
            if (idx != (size_t)-1) {
                jl_value_t *ft = jl_svecref(sty->types, idx);
                jl_value_t *rhst = expr_type(args[3], ctx);
                rt2 = rhst;
                if (jl_is_leaf_type((jl_value_t*)sty) && jl_subtype(rhst, ft)) {
                    // TODO: attempt better codegen for approximate types
                    jl_cgval_t strct = emit_expr(args[1], ctx); // emit lhs
                    *ret = emit_expr(args[3], ctx);
                    emit_setfield(sty, strct, idx, *ret, ctx, true, true);
                    JL_GC_POP();
                    return true;
                }
            }
        }
        // TODO: faster code for integer index
    }

    else if (f==jl_builtin_nfields && nargs==1) {
        if (ctx->vaStack && slot_eq(args[1], ctx->vaSlot)) {
            *ret = mark_julia_type(emit_n_varargs(ctx), false, jl_long_type, ctx);
            JL_GC_POP();
            return true;
        }
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_type_type(aty)) {
            jl_value_t *tp0 = jl_tparam0(aty);
            if (jl_is_leaf_type(tp0)) {
                emit_expr(args[1], ctx);
                assert(jl_is_datatype(tp0));
                *ret = mark_julia_type(ConstantInt::get(T_size, jl_datatype_nfields(tp0)), false, jl_long_type, ctx);
                JL_GC_POP();
                return true;
            }
        }
        else if (jl_is_leaf_type(aty)) {
            jl_cgval_t arg1 = emit_expr(args[1], ctx);
            Value *sz;
            if (arg1.constant) {
                sz = ConstantInt::get(T_size, jl_datatype_nfields(arg1.typ));
            }
            else if (aty == (jl_value_t*)jl_datatype_type) {
                assert(arg1.isboxed);
                sz = emit_datatype_nfields(boxed(arg1, ctx));
            }
            else {
                assert(jl_is_datatype(aty));
                sz = ConstantInt::get(T_size, jl_datatype_nfields(aty));
            }
            *ret = mark_julia_type(sz, false, jl_long_type, ctx);
            JL_GC_POP();
            return true;
        }
    }

    else if (f==jl_builtin_fieldtype && nargs==2) {
        jl_datatype_t *sty = (jl_datatype_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_type_type((jl_value_t*)sty) || sty == jl_datatype_type) {
            rt2 = expr_type(args[2], ctx); // index argument type
            if (rt2 == (jl_value_t*)jl_long_type) {
                jl_cgval_t ty = emit_expr(args[1], ctx);
                assert(ty.isboxed);
                Value *tyv = boxed(ty, ctx);
                Value *types_svec = emit_datatype_types(tyv);
                Value *types_len = emit_datatype_nfields(tyv);
                Value *idx = emit_unbox(T_size, emit_expr(args[2], ctx), (jl_value_t*)jl_long_type);
                emit_bounds_check(ty, (jl_value_t*)jl_datatype_type, idx, types_len, ctx);
                Value *fieldtyp = tbaa_decorate(tbaa_const, builder.CreateLoad(builder.CreateGEP(emit_bitcast(types_svec, T_ppjlvalue), idx)));
                *ret = mark_julia_type(fieldtyp, true, expr_type(expr, ctx), ctx);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f==jl_builtin_sizeof && nargs == 1) {
        jl_value_t *sty = expr_type(args[1], ctx); rt1 = sty;
        sty = jl_unwrap_unionall(sty);
        if (jl_is_type_type(sty) && !jl_is_typevar(jl_tparam0(sty))) {
            sty = jl_tparam0(sty);
        }
        if (jl_is_datatype(sty) && sty != (jl_value_t*)jl_symbol_type &&
            ((jl_datatype_t*)sty)->name != jl_array_typename &&
            sty != (jl_value_t*)jl_simplevector_type && sty != (jl_value_t*)jl_string_type &&
            // exclude DataType, since each DataType has its own size, not sizeof(DataType).
            // this is issue #8798
            sty != (jl_value_t*)jl_datatype_type) {
            if (jl_is_leaf_type(sty) ||
                (((jl_datatype_t*)sty)->name->names == jl_emptysvec && jl_datatype_size(sty) > 0)) {
                *ret = mark_julia_type(ConstantInt::get(T_size, jl_datatype_size(sty)), false, jl_long_type, ctx);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f==jl_builtin_apply_type && nargs > 0) {
        size_t i;
        if (ctx->linfo->def == NULL) {
            // don't bother codegen constant-folding for toplevel
            JL_GC_POP();
            return false;
        }
        for (i=1; i <= nargs; i++) {
            if (!is_constant(args[i], ctx))
                break;
        }
        if (i > nargs) {
            jl_value_t *ty = static_eval(expr, ctx, true, true);
            if (ty!=NULL && jl_is_leaf_type(ty)) {
                if (jl_has_free_typevars(ty)) {
                    // add root for types not cached. issue #7065
                    jl_add_method_root(ctx, ty);
                }
                *ret = mark_julia_const(ty);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f == jl_builtin_isdefined && nargs == 2) {
        jl_datatype_t *stt = (jl_datatype_t*)expr_type(args[1], ctx);
        if (!jl_is_leaf_type((jl_value_t*)stt) || jl_is_array_type(stt) ||
            stt == jl_module_type) {
            JL_GC_POP();
            return false;
        }
        assert(jl_is_datatype(stt));

        ssize_t fieldidx = -1;
        if (jl_is_quotenode(args[2]) && jl_is_symbol(jl_fieldref(args[2], 0))) {
            jl_sym_t * sym = (jl_sym_t*)jl_fieldref(args[2], 0);
            fieldidx = jl_field_index(stt, sym, 0);
        }
        else if (jl_is_long(args[2])) {
            fieldidx = jl_unbox_long(args[2]) - 1;
        }
        else {
            JL_GC_POP();
            return false;
        }
        jl_cgval_t strct = emit_expr(args[1], ctx);
        if (fieldidx < 0 || fieldidx >= jl_datatype_nfields(stt)) {
            *ret = mark_julia_const(jl_false);
        }
        else if (!jl_field_isptr(stt, fieldidx) || fieldidx < stt->ninitialized) {
            *ret = mark_julia_const(jl_true);
        }
        else {
            size_t offs = jl_field_offset(stt, fieldidx);
            Value *ptr = data_pointer(strct, ctx, T_pint8);
            Value *llvm_idx = ConstantInt::get(T_size, offs);
            Value *addr = builder.CreateGEP(ptr, llvm_idx);
            Value *fldv = tbaa_decorate(strct.tbaa, builder.CreateLoad(emit_bitcast(addr, T_ppjlvalue)));
            Value *isdef = builder.CreateICmpNE(fldv, V_null);
            *ret = mark_julia_type(isdef, false, jl_bool_type, ctx);
        }
        JL_GC_POP();
        return true;
    }
    // TODO: other known builtins
    JL_GC_POP();
    return false;
}

static Value *emit_jlcall(Value *theFptr, Value *theF, jl_value_t **args,
                          size_t nargs, jl_codectx_t *ctx)
{
    // emit arguments
    Value *myargs;
    if (nargs > 0) {
        jl_cgval_t *anArg = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
        const jl_cgval_t **largs = (const jl_cgval_t**)alloca(sizeof(jl_cgval_t*) * nargs);
        for(size_t i=0; i < nargs; i++) {
            anArg[i] = emit_expr(args[i], ctx);
            largs[i] = &anArg[i];
        }
        // put into argument space
        myargs = make_jlcall(makeArrayRef(largs, nargs), ctx);
    }
    else {
        myargs = Constant::getNullValue(T_ppjlvalue);
    }
#if JL_LLVM_VERSION >= 30700
    Value *result = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                       ConstantInt::get(T_int32,nargs)});
#else
    Value *result = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
#endif
    return result;
}

static jl_cgval_t emit_call_function_object(jl_method_instance_t *li, const jl_cgval_t &theF, jl_llvm_functions_t decls,
                                            jl_value_t **args, size_t nargs, jl_value_t *callexpr, jl_codectx_t *ctx)
{
    Value *theFptr = (Value*)decls.functionObject;
    jl_value_t *inferred_retty = expr_type(callexpr, ctx);
    if (decls.specFunctionObject != NULL) {
        // emit specialized call site
        jl_value_t *jlretty = li->rettype;
        bool retboxed;
        (void)julia_type_to_llvm(jlretty, &retboxed);
        Function *cf = cast<Function>(prepare_call((Function*)decls.specFunctionObject));
        FunctionType *cft = cf->getFunctionType();
        size_t nfargs = cft->getNumParams();
        Value **argvals = (Value**) alloca(nfargs*sizeof(Value*));
        bool sret = cf->hasStructRetAttr();
        unsigned idx = 0;
        Value *result;
        if (sret) {
            assert(!retboxed);
            result = emit_static_alloca(cft->getParamType(0)->getContainedType(0), ctx);
            argvals[idx] = result;
            idx++;
        }
        SmallVector<Value*, 16> gc_uses;
        for (size_t i = 0; i < nargs + 1; i++) {
            jl_value_t *jt = jl_nth_slot_type(li->specTypes,i);
            bool isboxed;
            Type *et = julia_type_to_llvm(jt, &isboxed);
            if (type_is_ghost(et)) {
                // Still emit the expression in case it has side effects
                if (i > 0)
                    emit_expr(args[i], ctx);
                continue;
            }
            assert(idx < nfargs);
            Type *at = cft->getParamType(idx);
            if (isboxed) {
                assert(at == T_pjlvalue && et == T_pjlvalue);
                jl_cgval_t origval = i == 0 ? theF : emit_expr(args[i], ctx);
                argvals[idx] = boxed(origval, ctx);
            }
            else if (et->isAggregateType()) {
                // can lazy load on demand, no copy needed
                assert(at == PointerType::get(et, 0));
                jl_cgval_t arg = i == 0 ? theF : emit_expr(args[i], ctx);
                assert(arg.ispointer());
                argvals[idx] = data_pointer(arg, ctx, at);
                push_gc_use(gc_uses, arg);
            }
            else {
                assert(at == et);
                if (i == 0)
                    argvals[idx] = emit_unbox(et, theF, jt);
                else
                    argvals[idx] = emit_unbox(et, emit_expr(args[i], ctx), jt);
            }
            idx++;
        }
        assert(idx == nfargs);
        mark_gc_uses(gc_uses);
        CallInst *call = builder.CreateCall(prepare_call(cf), ArrayRef<Value*>(&argvals[0], nfargs));
        call->setAttributes(cf->getAttributes());
        mark_gc_uses(gc_uses);
        if (sret)
            return mark_julia_slot(result, jlretty, tbaa_stack);
        // see if codegen has a better type for the call than inference had at the time
        if (!retboxed && jlretty != inferred_retty) {
            inferred_retty = jlretty;
        }
        return mark_julia_type(call, retboxed, inferred_retty, ctx);
    }
    Value *ret = emit_jlcall(theFptr, boxed(theF, ctx), &args[1], nargs, ctx);
    return mark_julia_type(ret, true, inferred_retty, ctx);
}

static jl_cgval_t emit_invoke(jl_expr_t *ex, jl_codectx_t *ctx)
{
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 2);

    jl_cgval_t lival = emit_expr(args[0], ctx);
    if (lival.constant) {
        jl_method_instance_t *li = (jl_method_instance_t*)lival.constant;
        assert(jl_is_method_instance(li));
        jl_llvm_functions_t decls = jl_compile_linfo(&li, NULL, ctx->world, ctx->params);
        if (li->jlcall_api == 2) {
            assert(li->inferred);
            return mark_julia_const(li->inferred);
        }
        if (decls.functionObject) {
            int jlcall_api = jl_jlcall_api(decls.functionObject);
            if (jlcall_api == 1) {
                jl_cgval_t fval = emit_expr(args[1], ctx);
                jl_cgval_t result = emit_call_function_object(li, fval, decls, &args[1], nargs - 1, (jl_value_t*)ex, ctx);
                if (result.typ == jl_bottom_type)
                    CreateTrap(builder);
                return result;
            }
        }
    }
    JL_FEAT_REQUIRE(ctx, runtime);
    jl_cgval_t result = mark_julia_type(emit_jlcall(prepare_call(jlinvoke_func), boxed(lival, ctx, false),
                                                    &args[1], nargs, ctx),
                                        true, expr_type((jl_value_t*)ex, ctx), ctx);
    if (result.typ == jl_bottom_type)
        CreateTrap(builder);
    return result;
}

static jl_cgval_t emit_call(jl_expr_t *ex, jl_codectx_t *ctx)
{
    jl_value_t *expr = (jl_value_t*)ex;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 1);
    Value *theFptr = NULL;
    jl_cgval_t result;

    jl_function_t *f = (jl_function_t*)static_eval(args[0], ctx, true);
    JL_GC_PUSH1(&f);
    if (f != NULL) {
        // function is a compile-time constant
        if (jl_typeis(f, jl_intrinsic_type)) {
            result = emit_intrinsic((intrinsic)*(uint32_t*)jl_data_ptr(f), args, nargs, ctx);
            if (result.typ == (jl_value_t*)jl_any_type) // the select_value intrinsic may be missing type information
                result = remark_julia_type(result, expr_type(expr, ctx));
            JL_GC_POP();
            return result;
        }
        if (jl_isa(f, (jl_value_t*)jl_builtin_type)) {
            bool handled = emit_builtin_call(&result, (jl_value_t*)f, args, nargs, ctx, expr);
            if (handled) {
                JL_GC_POP();
                return result;
            }
        }
    }

    // special case for known builtin not handled by emit_builtin_call
    if (f && jl_isa(f, (jl_value_t*)jl_builtin_type)) {
        std::map<jl_fptr_t,Function*>::iterator it = builtin_func_map.find(jl_get_builtin_fptr(f));
        if (it != builtin_func_map.end()) {
            theFptr = (*it).second;
            result = mark_julia_type(emit_jlcall(theFptr, V_null, &args[1], nargs, ctx), true, expr_type(expr,ctx), ctx);
            JL_GC_POP();
            return result;
        }
    }

    if (!JL_FEAT_TEST(ctx, runtime)) {
        char* name = NULL;
        if (jl_is_symbol(args[0]))
            name = jl_symbol_name((jl_sym_t*)args[0]);
        if (jl_is_globalref(args[0]))
            name = jl_symbol_name(jl_globalref_name(args[0]));
        jl_errorf("generic call to %s requires the runtime language feature",
                  name!=NULL ? name : "<unknown>");
    }

    // emit function and arguments
    nargs++; // add function to nargs count
    jl_cgval_t *anArg = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    const jl_cgval_t **largs = (const jl_cgval_t**)alloca(sizeof(jl_cgval_t*) * nargs);
    for(size_t i=0; i < nargs; i++) {
        anArg[i] = emit_expr(args[i], ctx);
        largs[i] = &anArg[i];
    }
    // put into argument space
    Value *myargs = make_jlcall(makeArrayRef(largs, nargs), ctx);
#if JL_LLVM_VERSION >= 30700
    Value *callval = builder.CreateCall(prepare_call(jlapplygeneric_func),
                                 {myargs, ConstantInt::get(T_int32, nargs)});
#else
    Value *callval = builder.CreateCall2(prepare_call(jlapplygeneric_func),
                                  myargs, ConstantInt::get(T_int32, nargs));
#endif
    result = mark_julia_type(callval, true, expr_type(expr, ctx), ctx);

    JL_GC_POP();
    return result;
}

// --- accessing and assigning variables ---

static void undef_var_error_ifnot(Value *ok, jl_sym_t *name, jl_codectx_t *ctx)
{
    BasicBlock *err = BasicBlock::Create(jl_LLVMContext, "err", ctx->f);
    BasicBlock *ifok = BasicBlock::Create(jl_LLVMContext, "ok");
    builder.CreateCondBr(ok, ifok, err);
    builder.SetInsertPoint(err);
    builder.CreateCall(prepare_call(jlundefvarerror_func), literal_pointer_val((jl_value_t*)name));
    builder.CreateUnreachable();
    ctx->f->getBasicBlockList().push_back(ifok);
    builder.SetInsertPoint(ifok);
}

// returns a jl_ppvalue_t location for the global variable m.s
// if the reference currently bound or assign == true,
//   pbnd will also be assigned with the binding address
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign, jl_codectx_t *ctx)
{
    jl_binding_t *b = NULL;
    if (assign) {
        b = jl_get_binding_wr(m, s);
        assert(b != NULL);
    }
    else {
        b = jl_get_binding(m, s);
        if (b == NULL) {
            // var not found. switch to delayed lookup.
            JL_FEAT_REQUIRE(ctx, runtime);
            std::stringstream name;
            name << "delayedvar" << globalUnique++;
            Constant *initnul = ConstantPointerNull::get((PointerType*)T_pjlvalue);
            GlobalVariable *bindinggv = new GlobalVariable(*ctx->f->getParent(), T_pjlvalue,
                    false, GlobalVariable::InternalLinkage,
                    initnul, name.str());
            Value *cachedval = builder.CreateLoad(bindinggv);
            BasicBlock *have_val = BasicBlock::Create(jl_LLVMContext, "found"),
                *not_found = BasicBlock::Create(jl_LLVMContext, "notfound");
            BasicBlock *currentbb = builder.GetInsertBlock();
            builder.CreateCondBr(builder.CreateICmpNE(cachedval, initnul), have_val, not_found);
            ctx->f->getBasicBlockList().push_back(not_found);
            builder.SetInsertPoint(not_found);
#if JL_LLVM_VERSION >= 30700
            Value *bval = builder.CreateCall(prepare_call(jlgetbindingorerror_func),
                                              {literal_pointer_val((jl_value_t*)m),
                                              literal_pointer_val((jl_value_t*)s)});
#else
            Value *bval = builder.CreateCall2(prepare_call(jlgetbindingorerror_func),
                                              literal_pointer_val((jl_value_t*)m),
                                              literal_pointer_val((jl_value_t*)s));
#endif
            builder.CreateStore(bval, bindinggv);
            builder.CreateBr(have_val);
            ctx->f->getBasicBlockList().push_back(have_val);
            builder.SetInsertPoint(have_val);
            PHINode *p = builder.CreatePHI(T_pjlvalue, 2);
            p->addIncoming(cachedval, currentbb);
            p->addIncoming(bval, not_found);
            return julia_binding_gv(emit_bitcast(p, T_ppjlvalue));
        }
        if (b->deprecated) cg_bdw(b, ctx);
    }
    if (pbnd) *pbnd = b;
    return julia_binding_gv(b);
}

static jl_cgval_t emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol, MDNode *tbaa)
{
    assert(bp->getType() == T_ppjlvalue);
    Instruction *v = builder.CreateLoad(bp, isvol);
    if (tbaa)
        tbaa_decorate(tbaa, v);
    undef_var_error_ifnot(builder.CreateICmpNE(v, V_null), name, ctx);
    return mark_julia_type(v, true, jl_any_type, ctx);
}

static jl_cgval_t emit_sparam(size_t i, jl_codectx_t *ctx)
{
    if (jl_svec_len(ctx->linfo->sparam_vals) > 0) {
        jl_value_t *e = jl_svecref(ctx->linfo->sparam_vals, i);
        if (!jl_is_typevar(e)) {
            return mark_julia_const(e);
        }
    }
    assert(ctx->spvals_ptr != NULL);
    Value *bp = builder.CreateConstInBoundsGEP1_32(LLVM37_param(T_pjlvalue)
            emit_bitcast(ctx->spvals_ptr, T_ppjlvalue),
            i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
    return mark_julia_type(tbaa_decorate(tbaa_const, builder.CreateLoad(bp)), true, jl_any_type, ctx, false);
}

static jl_cgval_t emit_global(jl_sym_t *sym, jl_codectx_t *ctx)
{
    jl_binding_t *jbp=NULL;
    Value *bp = global_binding_pointer(ctx->module, sym, &jbp, false, ctx);
    assert(bp != NULL);
    if (jbp && jbp->value != NULL) {
        if (jbp->constp)
            return mark_julia_const(jbp->value);
        // double-check that a global variable is actually defined. this
        // can be a problem in parallel when a definition is missing on
        // one machine.
        return mark_julia_type(tbaa_decorate(tbaa_binding, builder.CreateLoad(bp)), true, jl_any_type, ctx);
    }
    return emit_checked_var(bp, sym, ctx, false, tbaa_binding);
}

static jl_cgval_t emit_local(jl_value_t *slotload, jl_codectx_t *ctx)
{
    size_t sl = jl_slot_number(slotload) - 1;
    jl_varinfo_t &vi = ctx->slots[sl];
    jl_sym_t *sym = slot_symbol(sl, ctx);
    if (vi.memloc) {
        Value *bp = vi.memloc;
        jl_value_t *typ;
        if (ctx->source->inferred) {
            // use the better type from inference for this load
            typ = expr_type(slotload, ctx);
            if (jl_is_typevar(typ))
                typ = ((jl_tvar_t*)typ)->ub;
        }
        else {
            // use the static type of the slot
            typ = vi.value.typ;
        }

        if (vi.isArgument || !vi.usedUndef) { // arguments are always defined
            Instruction *v = builder.CreateLoad(bp, vi.isVolatile);
            return mark_julia_type(v, true, typ, ctx,
                                   !vi.isArgument); // if an argument, doesn't need an additional root
        }
        else {
            jl_cgval_t v = emit_checked_var(bp, sym, ctx, vi.isVolatile, nullptr);
            v = remark_julia_type(v, typ); // patch up type, if possible
            return v;
        }
    }
    else {
        if (vi.usedUndef) {
            assert(vi.defFlag);
            undef_var_error_ifnot(builder.CreateLoad(vi.defFlag), sym, ctx);
        }
        if (!vi.isVolatile || vi.isArgument) {
            return vi.value;
        }
        else {
            // copy value to a non-mutable location
            Type *T = julia_type_to_llvm(vi.value.typ)->getPointerTo();
            Value *v = data_pointer(vi.value, ctx, T);
            return mark_julia_type(builder.CreateLoad(v, vi.isVolatile), false, vi.value.typ, ctx);
        }
    }
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    if (jl_is_ssavalue(l)) {
        ssize_t idx = ((jl_ssavalue_t*)l)->id;
        assert(idx >= 0);
        assert(!ctx->ssavalue_assigned.at(idx));
        jl_cgval_t slot = emit_expr(r, ctx); // slot could be a jl_value_t (unboxed) or jl_value_t* (ispointer)
        if (!slot.isboxed && !slot.isimmutable) { // emit a copy of values stored in mutable slots
            bool isboxed;
            Type *vtype = julia_type_to_llvm(slot.typ, &isboxed);
            assert(!isboxed);
            Value *dest = emit_static_alloca(vtype);
            emit_unbox(vtype, slot, slot.typ, dest);
            slot = mark_julia_slot(dest, slot.typ, tbaa_stack);
        }
        if (slot.isboxed && slot.isimmutable) {
            // see if inference had a better type for the ssavalue than the expression (after inlining getfield on a Tuple)
            jl_value_t *ssavalue_types = (jl_value_t*)ctx->source->ssavaluetypes;
            if (jl_is_array(ssavalue_types)) {
                jl_value_t *declType = jl_array_ptr_ref(ssavalue_types, idx);
                if (declType != slot.typ) {
                    slot = remark_julia_type(slot, declType);
                }
            }
        }
        ctx->SAvalues.at(idx) = slot; // now SAvalues[idx] contains the SAvalue
        ctx->ssavalue_assigned.at(idx) = true;
        return;
    }

    jl_sym_t *s = NULL;
    jl_binding_t *bnd = NULL;
    Value *bp = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_globalref(l))
        bp = global_binding_pointer(jl_globalref_mod(l), jl_globalref_name(l), &bnd, true, ctx); // now bp != NULL
    else
        assert(jl_is_slot(l));
    if (bp == NULL && s != NULL)
        bp = global_binding_pointer(ctx->module, s, &bnd, true, ctx);
    if (bp != NULL) { // it's a global
        JL_FEAT_REQUIRE(ctx, runtime);
        assert(bnd);
        Value *rval = boxed(emit_expr(r, ctx), ctx, false); // no root needed since this is about to be assigned to a global
#if JL_LLVM_VERSION >= 30700
        builder.CreateCall(prepare_call(jlcheckassign_func),
                           {literal_pointer_val(bnd),
                            rval});
#else
        builder.CreateCall2(prepare_call(jlcheckassign_func),
                           literal_pointer_val(bnd),
                            rval);
#endif
        // Global variable. Does not need debug info because the debugger knows about
        // its memory location.
        return;
    }

    int sl = jl_slot_number(l)-1;
    // it's a local variable
    jl_varinfo_t &vi = ctx->slots[sl];
    jl_cgval_t rval_info = emit_expr(r, ctx);
    if (!vi.used)
        return;

    // add info to arrayvar list
    if (rval_info.isboxed) {
        // check isboxed in case rval isn't the right type (for example, on a dead branch),
        // so we don't try to assign it to the arrayvar info
        jl_arrayvar_t *av = arrayvar_for(l, ctx);
        if (av != NULL)
            assign_arrayvar(*av, rval_info, ctx);
    }

    if (vi.memloc) {
        // boxed variables
        if (((!vi.isSA && rval_info.gcroot) || !rval_info.isboxed) && isa<AllocaInst>(vi.memloc)) {
            // rval had a gcroot, so lval needs one too: promote variable slot to a gcroot
            emit_local_root(ctx, &vi);
        }
        Value *rval = boxed(rval_info, ctx, false); // no root needed on the temporary since it is about to be assigned to the variable slot
        builder.CreateStore(rval, vi.memloc, vi.isVolatile);
    }
    else {
        if (vi.usedUndef)
            store_def_flag(vi, true);

        if (vi.value.constant) {
            // virtual store
        }
        else {
            // store unboxed
            assert(vi.value.ispointer());
            emit_unbox(julia_type_to_llvm(vi.value.typ), rval_info, vi.value.typ, vi.value.V, vi.isVolatile);
        }
    }
}

// --- convert expression to code ---

static Value *emit_condition(const jl_cgval_t &condV, const std::string &msg,
                             jl_codectx_t *ctx)
{
    if (condV.typ == (jl_value_t*)jl_bool_type) {
        Value *cond = emit_unbox(T_int8, condV, (jl_value_t*)jl_bool_type);
        assert(cond->getType() == T_int8);
        return builder.CreateXor(builder.CreateTrunc(cond,T_int1), ConstantInt::get(T_int1,1));
    }
    emit_typecheck(condV, (jl_value_t*)jl_bool_type, msg, ctx);
    if (condV.isboxed) {
        return builder.CreateICmpEQ(boxed(condV, ctx), literal_pointer_val(jl_false));
    }
    // not a boolean
    return ConstantInt::get(T_int1,0); // TODO: replace with Undef
}

static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx)
{
    return emit_condition(emit_expr(cond, ctx), msg, ctx);
}

static void emit_stmtpos(jl_value_t *expr, jl_codectx_t *ctx)
{
    if (jl_is_ssavalue(expr))
        return; // value not used, no point in attempting codegen for it
    if (jl_is_linenode(expr))
        return;
    if (jl_is_slot(expr)) {
        size_t sl = jl_slot_number(expr) - 1;
        jl_varinfo_t &vi = ctx->slots[sl];
        if (vi.usedUndef)
            (void)emit_expr(expr, ctx);
        return;
    }
    if (jl_is_newvarnode(expr)) {
        jl_value_t *var = jl_fieldref(expr, 0);
        assert(jl_is_slot(var));
        jl_varinfo_t &vi = ctx->slots[jl_slot_number(var)-1];
        Value *lv = vi.memloc;
        if (vi.usedUndef) {
            // create a new uninitialized variable
            if (lv != NULL)
                builder.CreateStore(V_null, lv);
            else
                store_def_flag(vi, false);
        }
        return;
    }
    if (!jl_is_expr(expr)) {
        (void)emit_expr(expr, ctx);
        return;
    }
    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    jl_sym_t *head = ex->head;
    if (head == line_sym || head == meta_sym || head == boundscheck_sym ||
        head == inbounds_sym) {
        // some expression types are metadata and can be ignored
        // in statement position
        return;
    }
    else if (head == leave_sym) {
        assert(jl_is_long(args[0]));
        JL_FEAT_REQUIRE(ctx, runtime);
        builder.CreateCall(prepare_call(jlleave_func),
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else {
        if (ctx->linfo->def == NULL) {
            Value *world = builder.CreateCall(prepare_call(jlgetworld_func));
            builder.CreateStore(world, ctx->world_age_field);
        }
        (void)emit_expr(expr, ctx);
    }
}

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_global(sym, ctx);
    }
    if (jl_is_slot(expr)) {
        return emit_local(expr, ctx);
    }
    if (jl_is_ssavalue(expr)) {
        ssize_t idx = ((jl_ssavalue_t*)expr)->id;
        assert(idx >= 0);
        if (!ctx->ssavalue_assigned.at(idx)) {
            ctx->ssavalue_assigned.at(idx) = true; // (assignment, not comparison test)
            return jl_cgval_t(); // dead code branch
        }
        else {
            return ctx->SAvalues.at(idx); // at this point, SAvalues[idx] actually contains the SAvalue
        }
    }
    if (jl_is_globalref(expr)) {
        return emit_getfield((jl_value_t*)jl_globalref_mod(expr), jl_globalref_name(expr), ctx);
    }
    if (jl_is_labelnode(expr)) {
        jl_error("Labelnode in value position");
    }
    if (jl_is_linenode(expr)) {
        jl_error("Linenode in value position");
    }
    if (jl_is_gotonode(expr)) {
        jl_error("Gotonode in value position");
    }
    if (!jl_is_expr(expr)) {
        int needroot = true;
        if (jl_is_quotenode(expr)) {
            expr = jl_fieldref(expr,0);
        }
        // numeric literals
        if (jl_is_int32(expr)) {
            int32_t val = jl_unbox_int32(expr);
            if ((uint32_t)(val+512) < 1024) {
                // this can be gotten from the box cache
                needroot = false;
                expr = jl_box_int32(val);
            }
        }
        else if (jl_is_int64(expr)) {
            uint64_t val = jl_unbox_uint64(expr);
            if ((uint64_t)(val+512) < 1024) {
                // this can be gotten from the box cache
                needroot = false;
                expr = jl_box_int64(val);
            }
        }
        if (needroot && ctx->linfo->def) { // toplevel exprs and some integers are already rooted
            jl_add_method_root(ctx, expr);
        }
        return mark_julia_const(expr);
    }

    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    jl_sym_t *head = ex->head;
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (head == invoke_sym) {
        return emit_invoke(ex, ctx);
    }
    else if (head == call_sym) {
        if (ctx->linfo->def) { // don't bother codegen constant-folding for toplevel
            jl_value_t *c = static_eval(expr, ctx, true, true);
            if (c) {
                jl_add_method_root(ctx, c);
                return mark_julia_const(c);
            }
        }
        jl_cgval_t res = emit_call(ex, ctx);
        // some intrinsics (e.g. typeassert) can return a wider type
        // than what's actually possible
        jl_value_t *expr_t = expr_type((jl_value_t*)ex, ctx);
        if (res.typ != expr_t && res.isboxed && !jl_is_leaf_type(res.typ)) {
            res = remark_julia_type(res, expr_t);
        }
        if (res.typ == jl_bottom_type || expr_t == jl_bottom_type) {
            CreateTrap(builder);
        }
        return res;
    }
    else if (head == foreigncall_sym) {
        return emit_ccall(args, jl_array_dim0(ex->args), ctx);
    }
    else if (head == assign_sym) {
        emit_assignment(args[0], args[1], ctx);
        return ghostValue(jl_void_type);
    }
    else if (head == static_parameter_sym) {
        return emit_sparam(jl_unbox_long(args[0])-1, ctx);
    }
    else if (head == method_sym) {
        jl_value_t *mn = args[0];
        assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(mn) || jl_is_slot(mn));
        JL_FEAT_REQUIRE(ctx, runtime);

        Value *bp = NULL, *name, *bp_owner = V_null;
        jl_binding_t *bnd = NULL;
        bool issym = jl_is_symbol(mn);
        bool isglobalref = !issym && jl_is_globalref(mn);
        if (issym || isglobalref) {
            jl_module_t *mod = ctx->module;
            if (isglobalref) {
                mod = jl_globalref_mod(mn);
                mn = (jl_value_t*)jl_globalref_name(mn);
            }
            if (jl_symbol_name((jl_sym_t*)mn)[0] == '@')
                jl_errorf("macro definition not allowed inside a local scope");
            name = literal_pointer_val(mn);
            bnd = jl_get_binding_for_method_def(mod, (jl_sym_t*)mn);
            bp = julia_binding_gv(bnd);
            bp_owner = literal_pointer_val((jl_value_t*)mod);
        }
        else if (jl_is_slot(mn)) {
            int sl = jl_slot_number(mn)-1;
            jl_varinfo_t &vi = ctx->slots[sl];
            bp = vi.memloc;
            name = literal_pointer_val((jl_value_t*)slot_symbol(sl, ctx));
        }
        if (bp) {
            Value *mdargs[4] = { name, bp, bp_owner, literal_pointer_val(bnd) };
            jl_cgval_t gf = mark_julia_type(
                    builder.CreateCall(prepare_call(jlgenericfunction_func), ArrayRef<Value*>(&mdargs[0], 4)),
                    true, jl_function_type, ctx);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }
        Value *a1 = boxed(emit_expr(args[1], ctx), ctx);
        Value *a2 = boxed(emit_expr(args[2], ctx), ctx);
        Value *mdargs[3] = { a1, a2, literal_pointer_val(args[3]) };
        builder.CreateCall(prepare_call(jlmethod_func), ArrayRef<Value*>(&mdargs[0], 3));
        return ghostValue(jl_void_type);
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx->module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (jl_is_symbol(sym)) {
            JL_FEAT_REQUIRE(ctx, runtime);
            jl_binding_t *bnd = NULL;
            (void)global_binding_pointer(mod, sym, &bnd, true, ctx); assert(bnd);
            builder.CreateCall(prepare_call(jldeclareconst_func),
                               literal_pointer_val(bnd));
        }
    }
    else if (head == new_sym) {
        jl_value_t *ty = expr_type(args[0], ctx);
        size_t nargs = jl_array_len(ex->args);
        if (jl_is_type_type(ty) &&
            jl_is_datatype(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            assert(nargs <= jl_datatype_nfields(jl_tparam0(ty))+1);
            return emit_new_struct(jl_tparam0(ty),nargs,args,ctx);
        }
        Value *typ = boxed(emit_expr(args[0], ctx), ctx);
        Value *val = emit_jlcall(jlnew_func, typ, &args[1], nargs-1, ctx);
        return mark_julia_type(val, true, ty, ctx);
    }
    else if (head == exc_sym) { // *ptls->exception_in_transit
        return mark_julia_type(builder.CreateLoad(emit_exc_in_transit(ctx),
                                                  /*isvolatile*/true),
                               true, jl_any_type, ctx);
    }
    else if (head == copyast_sym) {
        JL_FEAT_REQUIRE(ctx, runtime);
        jl_value_t *arg = args[0];
        if (jl_is_quotenode(arg)) {
            jl_value_t *arg1 = jl_fieldref(arg,0);
            if (!(jl_is_expr(arg1) || jl_typeis(arg1,jl_array_any_type) || jl_is_quotenode(arg1))) {
                // elide call to jl_copy_ast when possible
                return emit_expr(arg, ctx);
            }
        }
        jl_cgval_t ast = emit_expr(arg, ctx);
        return mark_julia_type(builder.CreateCall(prepare_call(jlcopyast_func), boxed(ast, ctx)), true, ast.typ, ctx);
    }
    else if (head == simdloop_sym) {
        llvm::annotateSimdLoop(builder.GetInsertBlock());
        return jl_cgval_t();
    }
    else if (head == goto_ifnot_sym) {
        jl_error("Expr(:goto_ifnot) in value position");
    }
    else if (head == leave_sym) {
        jl_error("Expr(:leave) in value position");
    }
    else if (head == enter_sym) {
        jl_error("Expr(:enter) in value position");
    }
    else if (head == inbounds_sym) {
        jl_error("Expr(:inbounds) in value position");
    }
    else if (head == boundscheck_sym) {
        jl_error("Expr(:boundscheck) in value position");
    }
    else {
        if (!strcmp(jl_symbol_name(head), "$"))
            jl_error("syntax: prefix \"$\" in non-quoted expression");
        if (jl_is_toplevel_only_expr(expr) &&
            ctx->linfo->def == NULL) {
            JL_FEAT_REQUIRE(ctx, runtime);
            // call interpreter to run a toplevel expr from inside a
            // compiled toplevel thunk.
            builder.CreateCall(prepare_call(jltopeval_func), literal_pointer_val(expr));
            return ghostValue(jl_void_type);
        }
        if (head == abstracttype_sym || head == compositetype_sym ||
            head == bitstype_sym) {
            jl_errorf("type definition not allowed inside a local scope");
        }
        else {
            jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                      jl_symbol_name(head), ctx->name);
        }
    }
    return jl_cgval_t();
}

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(BasicBlock *b0, jl_codectx_t *ctx)
{
    // TODO: requires the runtime, but is generated unconditionally

    // allocate a placeholder gc instruction
    ctx->ptlsStates = builder.CreateCall(prepare_call(jltls_states_func));
    int nthfield = offsetof(jl_tls_states_t, safepoint) / sizeof(void*);
    ctx->signalPage = emit_nthptr_recast(ctx->ptlsStates, nthfield, tbaa_const,
                                         PointerType::get(T_psize, 0));
}

static void emit_last_age_field(jl_codectx_t *ctx)
{
    ctx->world_age_field = builder.CreateGEP(
            builder.CreateBitCast(ctx->ptlsStates, T_psize),
            ConstantInt::get(T_size, offsetof(jl_tls_states_t, world_age) / sizeof(size_t)));
}

static Function *gen_cfun_wrapper(jl_function_t *ff, jl_value_t *jlrettype, jl_tupletype_t *argt,
                                  jl_typemap_entry_t *sf, jl_value_t *declrt, jl_tupletype_t *sigt,
                                  size_t world)
{
    // Generate a c-callable wrapper
    bool toboxed;
    Type *crt = julia_struct_to_llvm(jlrettype, NULL, &toboxed);
    if (crt == NULL)
        jl_error("cfunction: return type doesn't correspond to a C type");

    size_t nargs = jl_nparams(argt);
    function_sig_t sig(crt, jlrettype, toboxed, argt->parameters, NULL, nargs, false, CallingConv::C, false);
    if (!sig.err_msg.empty())
        jl_error(sig.err_msg.c_str());
    if (sig.fargt.size() + sig.sret != sig.fargt_sig.size())
        jl_error("va_arg syntax not allowed for cfunction argument list");

    const char *name = "cfunction";
    // try to look up this function for direct invoking
    jl_method_instance_t *lam = jl_get_specialization1((jl_tupletype_t*)sigt, world);
    jl_value_t *astrt = (jl_value_t*)jl_any_type;
    // infer it first, if necessary
    if (lam) {
        name = jl_symbol_name(lam->def->name);
        jl_code_info_t *src = NULL;
        if (!lam->inferred) // TODO: this isn't ideal to be unconditionally calling type inference from here
            src = jl_type_infer(&lam, world, 0);
        jl_compile_linfo(&lam, src, world, &jl_default_cgparams);
        if (lam->jlcall_api != 2) {
            if (lam->functionObjectsDecls.functionObject == NULL ||
                    jl_jlcall_api(lam->functionObjectsDecls.functionObject) != 1) {
                lam = NULL; // TODO: use emit_invoke framework to dispatch these
            }
        }
        if (lam) {
            astrt = lam->rettype;
            if (astrt != (jl_value_t*)jl_bottom_type &&
                jl_type_intersection(astrt, declrt) == jl_bottom_type) {
                // Do not warn if the function does not return since it is
                // occasionally required by the C API (typically error callbacks)
                // and doesn't capture the majority of the case when a function
                // may throw.
                jl_printf(JL_STDERR, "WARNING: cfunction: return type of %s does not match\n", name);
            }
        }
    }

    std::stringstream funcName;
    funcName << "jlcapi_" << name << "_" << globalUnique++;

    Module *M = new Module(name, jl_LLVMContext);
    jl_setup_module(M);
    Function *cw = Function::Create(sig.functype,
            GlobalVariable::ExternalLinkage,
            funcName.str(), M);
    jl_init_function(cw);
    cw->setAttributes(sig.attributes);
#if JL_LLVM_VERSION >= 30700
    cw->addFnAttr("no-frame-pointer-elim", "true");
#endif
    Function *cw_proto = function_proto(cw);

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", cw);
    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx = {};
    ctx.f = cw;
    ctx.linfo = lam;
    ctx.code = NULL;
    ctx.world = jl_world_counter;
    ctx.sret = false;
    ctx.spvals_ptr = NULL;
    ctx.params = &jl_default_cgparams;
    allocate_gc_frame(b0, &ctx);
    emit_last_age_field(&ctx);
    Value *dummy_world = builder.CreateAlloca(T_size);
    Value *have_tls = builder.CreateICmpNE(ctx.ptlsStates, Constant::getNullValue(ctx.ptlsStates->getType()));
    // TODO: in the future, try to initialize a full TLS context here
    // for now, just use a dummy field to avoid a branch in this function
    ctx.world_age_field = builder.CreateSelect(have_tls, ctx.world_age_field, dummy_world);
    Value *last_age = tbaa_decorate(tbaa_gcframe, builder.CreateLoad(ctx.world_age_field));
    builder.CreateStore(ConstantInt::get(T_size, world), ctx.world_age_field);

    // Save the Function object reference
    sf->func.value = jl_box_voidpointer((void*)cw_proto);
    jl_gc_wb(sf, sf->func.value);

    // See whether this function is specsig or jlcall or generic (unknown)
    bool specsig, jlfunc_sret;
    Function *theFptr;
    Value *result;
    Value *myargs;
    size_t FParamIndex = 0;
    std::vector<Value*> args;
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = sig.sret ? &*AI++ : NULL;
    if (lam == NULL) {
        theFptr = jlapplygeneric_func;
        specsig = false;
        jlfunc_sret = false;
        myargs = CallInst::Create(prepare_call(jlcall_frame_func),
            ConstantInt::get(T_int32, nargs + 1),
            "",
            /*InsertBefore*/ctx.ptlsStates);
        GetElementPtrInst *slot = GetElementPtrInst::Create(LLVM37_param(NULL) myargs,
                ArrayRef<Value*>(ConstantInt::get(T_int32, FParamIndex++)));
        slot->insertAfter(ctx.ptlsStates);
        Value *theF = literal_pointer_val((jl_value_t*)ff);
        builder.CreateStore(theF, slot);
    }
    else if (lam->functionObjectsDecls.specFunctionObject != NULL) {
        theFptr = (Function*)lam->functionObjectsDecls.specFunctionObject;
        specsig = true;
        jlfunc_sret = theFptr->hasStructRetAttr();
        if (jlfunc_sret) {
            // fuse the two sret together, or emit an alloca to hold it
            if (sig.sret)
                result = emit_bitcast(sretPtr, theFptr->getFunctionType()->getParamType(0));
            else
                result = builder.CreateAlloca(theFptr->getFunctionType()->getParamType(0)->getContainedType(0));
            args.push_back(result);
            FParamIndex++;
        }
        myargs = NULL;
    }
    else if (lam->jlcall_api == 2) {
        nargs = 0; // arguments not needed
        specsig = false;
        jlfunc_sret = false;
        myargs = NULL;
        theFptr = NULL;
    }
    else {
        theFptr = (Function*)lam->functionObjectsDecls.functionObject;
        specsig = false;
        jlfunc_sret = false;
        myargs = CallInst::Create(prepare_call(jlcall_frame_func),
            ConstantInt::get(T_int32, nargs),
            "",
            /*InsertBefore*/ctx.ptlsStates);
    }

    // first emit the arguments
    for (size_t i = 0; i < nargs; i++) {
        Value *val = &*AI++;
        jl_value_t *jargty = jl_nth_slot_type((jl_value_t*)argt, i);
        // figure out how to unpack this type
        jl_cgval_t inputarg;
        if (jl_is_abstract_ref_type(jargty)) {
            // a pointer to a value
            jargty = jl_tparam0(jargty);
            if (jargty == (jl_value_t*)jl_any_type) {
                inputarg = mark_julia_type(
                        builder.CreateLoad(builder.CreatePointerCast(val, T_ppjlvalue)),
                        true, jargty, &ctx);
            }
            else if (!jl_isbits(jargty)) {
                // must be a jl_value_t* (because it's mutable or contains gc roots)
                inputarg = mark_julia_type(builder.CreatePointerCast(val, T_pjlvalue), true, jargty, &ctx);
            }
            else {
                bool isboxed;
                Type *T = julia_type_to_llvm(jargty, &isboxed);
                assert(!isboxed);
                // a T* (of unknown origin)
                if (type_is_ghost(T)) {
                    inputarg = ghostValue(jargty);
                }
                else {
                    val = builder.CreatePointerCast(val, T->getPointerTo());
                    val = builder.CreateAlignedLoad(val, 1); // make no alignment assumption about pointer from C
                    inputarg = mark_julia_type(val, false, jargty, &ctx);
                }
            }
        }
        else {
            bool argboxed;
            (void)julia_struct_to_llvm(jargty, NULL, &argboxed);
            if (argboxed) {
                // a jl_value_t*, even when represented as a struct
                inputarg = mark_julia_type(val, true, jargty, &ctx);
            }
            else {
                // something of type T
                // undo whatever we might have done to this poor argument
                if (sig.byRefList.at(i)) {
                    assert(val->getType() == sig.fargt[i]->getPointerTo());
                    val = builder.CreateAlignedLoad(val, 1); // unknown alignment from C
                }
                else {
                    bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type);
                    val = llvm_type_rewrite(val, sig.fargt[i], issigned, &ctx);
                }
                bool isboxed;
                (void)julia_type_to_llvm(jargty, &isboxed);
                if (isboxed) {
                    // passed an unboxed T, but want something boxed
                    Value *mem = emit_allocobj(&ctx, jl_datatype_size(jargty),
                                               literal_pointer_val((jl_value_t*)jargty));
                    tbaa_decorate(jl_is_mutable(jargty) ? tbaa_mutab : tbaa_immut,
                                  builder.CreateAlignedStore(val,
                                                             emit_bitcast(mem, val->getType()->getPointerTo()),
                                                             16)); // julia's gc gives 16-byte aligned addresses
                    inputarg = mark_julia_type(mem, true, jargty, &ctx);
                }
                else {
                    // mark that this is an unboxed T
                    inputarg = mark_julia_type(val, false, jargty, &ctx);
                }
            }
        }

        // figure out how to repack this type
        if (!specsig) {
            Value *arg = boxed(inputarg, &ctx, false); // don't want a gcroot, since it's about to be put into the jlcall frame anyways
            GetElementPtrInst *slot = GetElementPtrInst::Create(LLVM37_param(NULL) myargs,
                    ArrayRef<Value*>(ConstantInt::get(T_int32, FParamIndex)));
            slot->insertAfter(ctx.ptlsStates);
            builder.CreateStore(arg, slot);
        }
        else {
            Value *arg;
            jl_value_t *spect = jl_nth_slot_type(lam->specTypes, i + 1); // +1 because argt excludes function
            bool isboxed;
            Type *T = julia_type_to_llvm(spect, &isboxed);
            if (isboxed) {
                arg = boxed(inputarg, &ctx);
            }
            else if (type_is_ghost(T)) {
                continue; // ghost types are skipped by the specsig method signature
            }
            else if (T->isAggregateType()) {
                // aggregate types are passed by pointer
                arg = data_pointer(inputarg, &ctx, T->getPointerTo());
            }
            else {
                arg = emit_unbox(T, inputarg, spect);
                assert(!isa<UndefValue>(arg));
            }

            // add to argument list
            args.push_back(arg);
        }
        FParamIndex++;
    }

    // Create the call
    jl_cgval_t retval;
    if (lam == NULL) {
        assert(theFptr);
#if JL_LLVM_VERSION >= 30700
        Value *ret = builder.CreateCall(prepare_call(theFptr), {myargs,
                                        ConstantInt::get(T_int32, nargs + 1)});
#else
        Value *ret = builder.CreateCall2(prepare_call(theFptr), myargs,
                                         ConstantInt::get(T_int32, nargs + 1));
#endif
        retval = mark_julia_type(ret, true, astrt, &ctx);
    }
    else if (specsig) {
        assert(theFptr);
        bool retboxed;
        CallInst *call = builder.CreateCall(prepare_call(theFptr), ArrayRef<Value*>(args));
        call->setAttributes(theFptr->getAttributes());
        (void)julia_type_to_llvm(astrt, &retboxed);
        retval = mark_julia_type(jlfunc_sret ? (Value*)builder.CreateLoad(result) : (Value*)call, retboxed, astrt, &ctx);
    }
    else if (lam->jlcall_api == 2) {
        retval = mark_julia_const(lam->inferred);
    }
    else {
        assert(theFptr);
        // for jlcall, we need to pass the function object even if it is a ghost.
        // here we reconstruct the function instance from its type (first elt of argt)
        Value *theF = literal_pointer_val((jl_value_t*)ff);
#if JL_LLVM_VERSION >= 30700
        Value *ret = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                        ConstantInt::get(T_int32, nargs)});
#else
        Value *ret = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                         ConstantInt::get(T_int32, nargs));
#endif
        retval = mark_julia_type(ret, true, astrt, &ctx);
    }

    if (!jl_subtype(astrt, declrt)) {
        // inline a call to typeassert here
        emit_typecheck(retval, declrt, "cfunction", &ctx);
    }

    // Prepare the return value
    Value *r;
    if (toboxed) {
        assert(!sig.sret);
        // return a jl_value_t*
        r = boxed(retval, &ctx, false); // no gcroot since this is on the return path
    }
    else if (sig.sret && jlfunc_sret) {
        // nothing to do
    }
    else if (!type_is_ghost(sig.lrt)) {
        Type *prt = sig.prt;
        if (sig.sret)
            prt = sig.fargt_sig[0]->getContainedType(0); // sret is a PointerType
        bool issigned = jl_signed_type && jl_subtype(declrt, (jl_value_t*)jl_signed_type);
        Value *v = julia_to_native(sig.lrt, toboxed, declrt, NULL, retval,
                                   false, 0, &ctx, NULL);
        r = llvm_type_rewrite(v, prt, issigned, &ctx);
        if (sig.sret)
            builder.CreateStore(r, sretPtr);
    }
    else {
        assert(type_is_ghost(sig.lrt));
        sig.sret = true;
    }

    builder.CreateStore(last_age, ctx.world_age_field);
    if (sig.sret)
        builder.CreateRetVoid();
    else
        builder.CreateRet(r);

    builder.SetCurrentDebugLocation(noDbg);
    builder.ClearInsertionPoint();

    jl_finalize_module(M, true);
    assert(!ctx.roots);

    return cw_proto;
}

const struct jl_typemap_info cfunction_cache = {
    1, &jl_voidpointer_type
};

// Get the LLVM Function* for the C-callable entry point for a certain function
// and argument types.
// here argt does not include the leading function type argument
static Function *jl_cfunction_object(jl_function_t *ff, jl_value_t *declrt, jl_tupletype_t *argt)
{
    // validate and unpack the arguments
    JL_TYPECHK(cfunction, type, declrt);
    JL_TYPECHK(cfunction, type, (jl_value_t*)argt);
    if (!jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(ff)))
        jl_error("closures are not yet c-callable");

    size_t i, nargs = jl_nparams(argt);
    jl_value_t *sigt = NULL; // type signature with Ref{} annotations removed
    jl_value_t *cfunc_sig = NULL; // type signature of the call to cfunction (for caching)
    JL_GC_PUSH2(&sigt, &cfunc_sig);
    sigt = (jl_value_t*)jl_alloc_svec(nargs + 1);
    cfunc_sig = (jl_value_t*)jl_alloc_svec(nargs + 2);

    jl_value_t *crt = declrt;
    jl_svecset(cfunc_sig, nargs + 1, declrt);
    if (jl_is_abstract_ref_type(declrt)) {
        declrt = jl_tparam0(declrt);
        if (jl_is_typevar(declrt))
            jl_error("cfunction: return type Ref should have an element type, not Ref{T}");
        if (declrt == (jl_value_t*)jl_any_type)
            jl_error("cfunction: return type Ref{Any} is invalid. Use Any or Ptr{Any} instead.");
        if (!jl_is_leaf_type(declrt))
            jl_svecset(cfunc_sig, nargs + 1, declrt); // Ref{Abstract} is the same calling convention as Abstract
        crt = (jl_value_t*)jl_any_type;
    }

    if (jl_is_type(ff))
        jl_svecset(sigt, 0, jl_wrap_Type(ff));
    else
        jl_svecset(sigt, 0, jl_typeof(ff));
    jl_svecset(cfunc_sig, 0, jl_svecref(sigt, 0));
    for (i = 0; i < nargs; i++) {
        jl_value_t *ati = jl_tparam(argt, i);
        jl_svecset(cfunc_sig, i + 1, ati);
        if (jl_is_abstract_ref_type(ati)) {
            ati = jl_tparam0(ati);
            if (jl_is_typevar(ati))
                jl_error("cfunction: argument type Ref should have an element type, not Ref{T}");
            if (ati != (jl_value_t*)jl_any_type && !jl_is_leaf_type(ati))
                jl_svecset(cfunc_sig, i + 1, ati); // Ref{Abstract} is the same calling convention as Abstract
        }
        if (jl_is_pointer(ati) && jl_is_typevar(jl_tparam0(ati)))
            jl_error("cfunction: argument type Ptr should have an element type, Ptr{T}");
        jl_svecset(sigt, i + 1, ati);
    }
    sigt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)sigt);
    cfunc_sig = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)cfunc_sig);

    // check the cache
    size_t world = jl_world_counter;
    if (jl_cfunction_list.unknown != jl_nothing) {
        jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(jl_cfunction_list, (jl_tupletype_t*)cfunc_sig, NULL, 1, 0, /*offs*/0, world);
        if (sf) {
            Function *f = (Function*)jl_unbox_voidpointer(sf->func.value);
            if (f) {
               JL_GC_POP();
               return f;
            }
        }
    }
    jl_typemap_entry_t *sf = jl_typemap_insert(&jl_cfunction_list, (jl_value_t*)jl_cfunction_list.unknown, (jl_tupletype_t*)cfunc_sig,
            jl_emptysvec, NULL, jl_emptysvec, NULL, /*offs*/0, &cfunction_cache, world, world, NULL);

    // Backup the info for the nested compile
    JL_LOCK(&codegen_lock);
    IRBuilderBase::InsertPoint old = builder.saveAndClearIP();
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    Function *f = gen_cfun_wrapper(ff, crt, (jl_tupletype_t*)argt, sf, declrt, (jl_tupletype_t*)sigt, world);
    // Restore the previous compile context
    builder.restoreIP(old);
    builder.SetCurrentDebugLocation(olddl);
    nested_compile = last_n_c;
    JL_UNLOCK(&codegen_lock); // Might GC
    JL_GC_POP();
    return f;
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_method_instance_t *lam, Function *f, const std::string &funcName, bool sret, Module *M)
{
    Function *w = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage,
                                   funcName, M);
    jl_init_function(w);
#if JL_LLVM_VERSION >= 30700
    w->addFnAttr("no-frame-pointer-elim", "true");
#endif
    Function::arg_iterator AI = w->arg_begin();
    Value *fArg = &*AI++;
    Value *argArray = &*AI++;
    /* const Argument &argCount = *AI++; */
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);

    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx = {};
    ctx.f = w;
    ctx.linfo = lam;
    ctx.code = NULL;
    ctx.world = 0;
    ctx.sret = false;
    ctx.spvals_ptr = NULL;
    ctx.params = &jl_default_cgparams;
    allocate_gc_frame(b0, &ctx);

    size_t nargs = lam->def->nargs;
    size_t nfargs = f->getFunctionType()->getNumParams();
    Value **args = (Value**) alloca(nfargs*sizeof(Value*));
    unsigned idx = 0;
    Value *result;
    if (sret) {
        result = builder.CreateAlloca(f->getFunctionType()->getParamType(0)->getContainedType(0));
        args[idx] = result;
        idx++;
    }
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed;
        Type *lty = julia_type_to_llvm(ty, &isboxed);
        if (lty != NULL && type_is_ghost(lty))
            continue;
        Value *theArg;
        if (i == 0) {
            theArg = fArg;
        }
        else {
            Value *argPtr = builder.CreateGEP(argArray, ConstantInt::get(T_size, i-1));
            theArg = builder.CreateLoad(argPtr);
        }
        if (lty != NULL && !isboxed) {
            theArg = builder.CreatePointerCast(theArg, PointerType::get(lty, 0));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = builder.CreateAlignedLoad(theArg, julia_alignment(theArg, ty, 0));
        }
        assert(dyn_cast<UndefValue>(theArg) == NULL);
        args[idx] = theArg;
        idx++;
    }
    CallInst *call = builder.CreateCall(prepare_call(f), ArrayRef<Value*>(&args[0], nfargs));
    call->setAttributes(f->getAttributes());

    jl_value_t *jlretty = lam->rettype;
    bool retboxed;
    (void)julia_type_to_llvm(jlretty, &retboxed);
    if (sret) { assert(!retboxed); }
    jl_cgval_t retval = sret ? mark_julia_slot(result, jlretty, tbaa_stack) : mark_julia_type(call, retboxed, jlretty, &ctx, /*needsroot*/false);
    builder.CreateRet(boxed(retval, &ctx, false)); // no gcroot needed since this on the return path

    assert(!ctx.roots);
    return w;
}

// Compile to LLVM IR, using a specialized signature if applicable.
static std::unique_ptr<Module> emit_function(
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        size_t world,
        jl_llvm_functions_t *declarations,
        const jl_cgparams_t *params)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(declarations && "Capturing declarations is always required");

    // step 1. unpack AST and allocate codegen context for this function
    jl_codectx_t ctx = {};
    JL_GC_PUSH2(&ctx.code, &ctx.roots);
    ctx.code = (jl_array_t*)src->code;
    if (!jl_typeis(ctx.code, jl_array_any_type))
        ctx.code = jl_uncompress_ast(lam->def, ctx.code);

    //jl_static_show(JL_STDOUT, (jl_value_t*)ast);
    //jl_printf(JL_STDOUT, "\n");
    std::map<int, jl_arrayvar_t> arrayvars;
    std::map<int, BasicBlock*> labels;
    ctx.arrayvars = &arrayvars;
    ctx.module = lam->def ? lam->def->module : ptls->current_module;
    ctx.linfo = lam;
    ctx.source = src;
    ctx.world = world;
    ctx.name = jl_symbol_name(lam->def ? lam->def->name : anonymous_sym);
    ctx.funcName = ctx.name;
    ctx.vaSlot = -1;
    ctx.vaStack = false;
    ctx.params = params;
    ctx.spvals_ptr = NULL;
    ctx.nargs = lam->def ? lam->def->nargs : 0;
    bool toplevel = lam->def == NULL;

    // step 2. process var-info lists to see what vars need boxing
    int n_ssavalues = jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_len(src->ssavaluetypes);
    size_t vinfoslen = jl_array_dim0(src->slotnames);
    ctx.slots.resize(vinfoslen);
    size_t nreq = ctx.nargs;
    int va = 0;

    assert(lam->specTypes); // the specTypes field should always be assigned

    if (nreq > 0 && lam->def->isva) {
        nreq--;
        va = 1;
        jl_sym_t *vn = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, ctx.nargs - 1);
        if (vn != unused_sym)
            ctx.vaSlot = ctx.nargs - 1;
    }
    ctx.nReqArgs = nreq;

    // create SAvalue locations for SSAValue objects
    ctx.ssavalue_assigned.assign(n_ssavalues, false);
    ctx.SAvalues.assign(n_ssavalues, jl_cgval_t());

    // step 3. some variable analysis
    size_t i;
    for(i=0; i < nreq; i++) {
        jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames,i);
        if (argname == unused_sym) continue;
        jl_varinfo_t &varinfo = ctx.slots[i];
        varinfo.isArgument = true;
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        varinfo.value = mark_julia_type((Value*)NULL, false, ty, &ctx);
    }
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &varinfo = ctx.slots[ctx.vaSlot];
        varinfo.isArgument = true;
        varinfo.value = mark_julia_type((Value*)NULL, false, jl_tuple_type, &ctx);
    }

    for(i=0; i < vinfoslen; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        uint8_t flags = jl_array_uint8_ref(src->slotflags, i);
        varinfo.escapes = false;
        varinfo.isSA = (jl_vinfo_sa(flags)!=0);
        varinfo.usedUndef = (jl_vinfo_usedundef(flags)!=0) || (!varinfo.isArgument && !src->inferred);
        if (!varinfo.isArgument) {
            jl_value_t *typ = jl_is_array(src->slottypes) ? jl_array_ptr_ref(src->slottypes,i) : (jl_value_t*)jl_any_type;
            if (!jl_is_type(typ))
                typ = (jl_value_t*)jl_any_type;
            varinfo.value = mark_julia_type((Value*)NULL, false, typ, &ctx);
        }
    }

    jl_array_t *stmts = ctx.code;
    size_t stmtslen = jl_array_dim0(stmts);

    // finish recording escape info
    for(i=0; i < stmtslen; i++)
        simple_escape_analysis(jl_array_ptr_ref(stmts,i), true, &ctx);

    // determine which vars need to be volatile
    mark_volatile_vars(stmts, ctx.slots);

    // step 4. determine function signature
    jl_value_t *jlrettype = lam->rettype;

    bool specsig = false;
    bool needsparams = lam->def ? jl_svec_len(lam->def->sparam_syms) != jl_svec_len(lam->sparam_vals) : false;
    for (i = 0; !needsparams && i < jl_svec_len(lam->sparam_vals); i++) {
        jl_value_t *e = jl_svecref(lam->sparam_vals, i);
        if (jl_is_typevar(e))
            needsparams = true;
    }
    if (!va && ctx.nargs > 0 && !needsparams && lam->specTypes != (jl_value_t*)jl_anytuple_type && src->inferred) {
        assert(jl_is_datatype(lam->specTypes));
        // not vararg, consider specialized signature
        for(size_t i=0; i < jl_nparams(lam->specTypes); i++) {
            if (isbits_spec(jl_tparam(lam->specTypes, i))) { // assumes !va
                specsig = true;
                break;
            }
        }
        if (jl_nparams(lam->specTypes) == 0)
            specsig = true;
        if (isbits_spec(jlrettype))
            specsig = true;
    }
    if (!specsig)
        ctx.nReqArgs--;  // function not part of argArray in jlcall

    Module *M = new Module(ctx.name, jl_LLVMContext);
    jl_setup_module(M, params);

    std::stringstream funcName;
    // try to avoid conflicts in the global symbol table
    if (specsig)
        funcName << "jlcall_";
    else if (needsparams)
        funcName << "japi3_";
    else
        funcName << "japi1_";
    const char* unadorned_name = ctx.name;
#if (defined(_OS_LINUX_) && JL_LLVM_VERSION < 30400)
    if (unadorned_name[0] == '@')
        unadorned_name++;
#endif
    funcName << unadorned_name << "_" << globalUnique++;

    Function *f = NULL;
    Function *fwrap = NULL;
    ctx.sret = false;

    if (specsig) { // assumes !va and !needsparams
        std::vector<Type*> fsig(0);
        Type *rt;
        bool retboxed;
        if (jlrettype == (jl_value_t*)jl_void_type) {
            rt = T_void;
            retboxed = false;
        }
        else {
            rt = julia_type_to_llvm(jlrettype, &retboxed);
        }
        if (!retboxed && rt != T_void && deserves_sret(jlrettype, rt)) {
            ctx.sret = true;
            fsig.push_back(rt->getPointerTo());
            rt = T_void;
        }
        for (size_t i = 0; i < jl_nparams(lam->specTypes); i++) {
            Type *ty = julia_type_to_llvm(jl_tparam(lam->specTypes,i));
            if (type_is_ghost(ty))
                continue;
            if (ty->isAggregateType()) // aggregate types are passed by pointer
                ty = PointerType::get(ty,0);
            fsig.push_back(ty);
        }
        std::stringstream specName;
        specName << "julia_" << unadorned_name << "_" << globalUnique;
        f = Function::Create(FunctionType::get(rt, fsig, false),
                             GlobalVariable::ExternalLinkage,
                             specName.str(), M);
        jl_init_function(f);
        if (ctx.sret) {
            f->addAttribute(1, Attribute::StructRet);
            f->addAttribute(1, Attribute::NoAlias);
        }
#if JL_LLVM_VERSION >= 30700
        f->addFnAttr("no-frame-pointer-elim", "true");
#endif
        fwrap = gen_jlcall_wrapper(lam, f, funcName.str(), ctx.sret, M);
        declarations->functionObject = function_proto(fwrap);
        declarations->specFunctionObject = function_proto(f);
    }
    else {
        f = Function::Create(needsparams ? jl_func_sig_sparams : jl_func_sig,
                             GlobalVariable::ExternalLinkage,
                             funcName.str(), M);
        jl_init_function(f);
#if JL_LLVM_VERSION >= 30700
        f->addFnAttr("no-frame-pointer-elim", "true");
#endif
        declarations->functionObject = function_proto(f);
        declarations->specFunctionObject = NULL;
    }
    if (jlrettype == (jl_value_t*)jl_bottom_type)
        f->setDoesNotReturn();
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to realign the stack to the next 16-byte boundary
    // upon entry to any function. This achieves compatibility
    // with both MinGW-GCC (which assumes an 16-byte-aligned stack) and
    // i686 Windows (which uses a 4-byte-aligned stack)
    AttrBuilder *attr = new AttrBuilder();
    attr->addStackAlignmentAttr(16);
    f->addAttributes(AttributeSet::FunctionIndex,
        AttributeSet::get(f->getContext(),
            AttributeSet::FunctionIndex,*attr));
#endif

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && JL_LLVM_VERSION >= 30500
    f->setHasUWTable(); // force NeedsWinEH
#endif

#ifdef USE_POLLY
    if (!jl_has_meta(stmts, polly_sym) || jl_options.polly == JL_OPTIONS_POLLY_OFF) {
        f->addFnAttr(polly::PollySkipFnAttr);
    }
#endif

#ifdef JL_DEBUG_BUILD
    f->addFnAttr(Attribute::StackProtectReq);
#endif
    ctx.f = f;

    // step 5. set up debug info context and create first basic block
    int coverage_mode = jl_options.code_coverage;
    int malloc_log_mode = jl_options.malloc_log;
    StringRef filename = "<missing>";
    StringRef dbgFuncName = ctx.name;
    int toplineno = -1;
    if (lam->def) {
        toplineno = lam->def->line;
        if (lam->def->file != empty_sym)
            filename = jl_symbol_name(lam->def->file);
    }
    ctx.file = filename;

    DIBuilder dbuilder(*M);
#if JL_LLVM_VERSION >= 30700
    DIFile *topfile = NULL;
    DISubprogram *SP = NULL;
#else
    DIFile topfile;
    DISubprogram SP;
#endif

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);

    // jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           jl_symbol_name(lam->name), filename.c_str(), toplineno);

    DebugLoc noDbg, topdebugloc;
    ctx.debug_enabled = true;
    if (dbgFuncName.empty()) {
        // special value: if function name is empty, disable debug info
        coverage_mode = JL_LOG_NONE;
        malloc_log_mode = JL_LOG_NONE;
        //dbgFuncName = filename; // for testing, uncomment this line
        ctx.debug_enabled = !dbgFuncName.empty();
    }
    if (jl_options.debug_level == 0)
        ctx.debug_enabled = 0;

    if (ctx.debug_enabled) {
        // TODO: Fix when moving to new LLVM version
#if JL_LLVM_VERSION < 30400
        dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
#elif JL_LLVM_VERSION >= 40000
        DIFile *difile = dbuilder.createFile(filename, ".");
        DICompileUnit *CU = dbuilder.createCompileUnit(0x01, difile, "julia", true, "", 0);
#elif JL_LLVM_VERSION >= 30700
        DICompileUnit *CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
#else
        DICompileUnit CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        assert(CU.Verify());
#endif

#if JL_LLVM_VERSION >= 30700
        DISubroutineType *subrty;
#elif JL_LLVM_VERSION >= 30600
        DISubroutineType subrty;
#else
        DICompositeType subrty;
#endif

        if (jl_options.debug_level <= 1) {
            subrty = jl_di_func_null_sig;
        }
        else if (!specsig) {
            subrty = jl_di_func_sig;
        }
        else {
#if JL_LLVM_VERSION >= 30600
            std::vector<Metadata*> ditypes(0);
#else
            std::vector<Value*> ditypes(0);
#endif
            for (size_t i = 0; i < jl_nparams(lam->specTypes); i++) { // assumes !va
                if (i < (size_t)(ctx.nargs) && ctx.slots[i].value.isghost)
                    continue;
                ditypes.push_back(julia_type_to_di(jl_tparam(lam->specTypes,i),&dbuilder,false));
            }
#if JL_LLVM_VERSION >= 30800
            subrty = dbuilder.createSubroutineType(dbuilder.getOrCreateTypeArray(ditypes));
#elif JL_LLVM_VERSION >= 30600
            subrty = dbuilder.createSubroutineType(topfile,dbuilder.getOrCreateTypeArray(ditypes));
#else
            subrty = dbuilder.createSubroutineType(topfile,dbuilder.getOrCreateArray(ditypes));
#endif
        }

        topfile = dbuilder.createFile(filename, ".");
#if JL_LLVM_VERSION < 30400
        SP = dbuilder.createFunction((DIDescriptor)dbuilder.getCU(),
#else
        SP = dbuilder.createFunction(CU,
#endif
                                    dbgFuncName,      // Name
                                    f->getName(),     // LinkageName
                                    topfile,          // File
                                    0,                // LineNo
                                    subrty,           // Ty
                                    false,            // isLocalToUnit
                                    true,             // isDefinition
                                    0,                // ScopeLine
                                    DIFlagZero,       // Flags
                                    true,             // isOptimized
#if JL_LLVM_VERSION >= 30800
                                    nullptr);         // Template Parameters
#else
                                    f);               // Function
#endif
        topdebugloc = DebugLoc::get(toplineno, 0, SP, NULL);
#if JL_LLVM_VERSION >= 30800
        f->setSubprogram(SP);
#endif
#if JL_LLVM_VERSION < 30700
        assert(SP.Verify() && SP.describes(f) && SP.getFunction() == f);
#endif
    }
    builder.SetCurrentDebugLocation(noDbg);

    if (ctx.debug_enabled && jl_options.debug_level >= 2) {
        const bool AlwaysPreserve = true;
        // Go over all arguments and local variables and initialize their debug information
        for (i = 0; i < nreq; i++) {
            jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
            if (argname == unused_sym)
                continue;
            jl_varinfo_t &varinfo = ctx.slots[i];
#if JL_LLVM_VERSION >= 30800
            varinfo.dinfo = dbuilder.createParameterVariable(
                SP,                                 // Scope (current function will be fill in later)
                jl_symbol_name(argname),            // Variable name
                ctx.sret + i + 1,                   // Argument number (1-based)
                topfile,                            // File
                toplineno == -1 ? 0 : toplineno,    // Line
                // Variable type
                julia_type_to_di(varinfo.value.typ, &dbuilder, false),
                AlwaysPreserve,                     // May be deleted if optimized out
                DIFlagZero);                        // Flags (TODO: Do we need any)
#else
            varinfo.dinfo = dbuilder.createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,    // Tag
                SP,         // Scope (current function will be fill in later)
                jl_symbol_name(argname),    // Variable name
                topfile,                    // File
                toplineno == -1 ? 0 : toplineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, &dbuilder, false), // Variable type
                AlwaysPreserve,                  // May be deleted if optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + i + 1);                   // Argument number (1-based)
#endif
        }
        if (va && ctx.vaSlot != -1) {
#if JL_LLVM_VERSION >= 30800
            ctx.slots[ctx.vaSlot].dinfo = dbuilder.createParameterVariable(
                SP,                     // Scope (current function will be fill in later)
                std::string(jl_symbol_name(slot_symbol(ctx.vaSlot, &ctx))) + "...",  // Variable name
                ctx.sret + nreq + 1,             // Argument number (1-based)
                topfile,                         // File
                toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),
                AlwaysPreserve,                  // May be deleted if optimized out
                DIFlagZero);                     // Flags (TODO: Do we need any)
#else
            ctx.slots[ctx.vaSlot].dinfo = dbuilder.createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,   // Tag
                SP,                                 // Scope (current function will be fill in later)
                std::string(jl_symbol_name(slot_symbol(ctx.vaSlot, &ctx))) + "...",  // Variable name
                topfile,                            // File
                toplineno == -1 ? 0 : toplineno,  // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),      // Variable type
                AlwaysPreserve,                  // May be deleted if optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + nreq + 1);              // Argument number (1-based)
#endif
        }
        for (i = 0; i < vinfoslen; i++) {
            jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
            jl_varinfo_t &varinfo = ctx.slots[i];
            if (varinfo.isArgument || s == compiler_temp_sym || s == unused_sym)
                continue;
            // LLVM 4.0: Assume the variable has default alignment
#if JL_LLVM_VERSION >= 30800
            varinfo.dinfo = dbuilder.createAutoVariable(
#else
            varinfo.dinfo = dbuilder.createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
#endif
                SP,                     // Scope (current function will be fill in later)
                jl_symbol_name(s),       // Variable name
                topfile,                 // File
                toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, &dbuilder, false), // Variable type
                AlwaysPreserve,          // May be deleted if optimized out
                DIFlagZero               // Flags (TODO: Do we need any)
#if JL_LLVM_VERSION < 30800
                ,0                       // Argument number (1-based)
#endif
                );
        }
    }

#if JL_LLVM_VERSION >= 30700
    std::map<jl_sym_t *, DIFile *> filescopes;
#else
    std::map<jl_sym_t *, MDNode *> filescopes;
#endif

    Value *fArg=NULL, *argArray=NULL, *pargArray=NULL, *argCount=NULL;
    if (!specsig) {
        Function::arg_iterator AI = f->arg_begin();
        if (needsparams) {
            ctx.spvals_ptr = &*AI++;
        }
        fArg = &*AI++;
        argArray = &*AI++;
        pargArray = builder.CreateAlloca(argArray->getType());
        builder.CreateStore(argArray, pargArray, true/*volatile store to prevent removal of this alloca*/);
        argCount = &*AI++;
        ctx.argArray = argArray;
        ctx.argCount = argCount;
    }

    /*
    // step 6. (optional) check for stack overflow (the slower way)
    Value *cur_sp =
        builder.CreateCall(Intrinsic::getDeclaration(M,
                                                     Intrinsic::frameaddress),
                           ConstantInt::get(T_int32, 0));
    Value *sp_ok =
        builder.CreateICmpUGT(cur_sp,
                              ConstantInt::get(T_size,
                                               (uptrint_t)jl_stack_lo));
    error_unless(sp_ok, "stack overflow", &ctx);
    */

    // step 7. set up GC frame
    allocate_gc_frame(b0, &ctx);
    Value *last_age = NULL;
    if (toplevel) {
        emit_last_age_field(&ctx);
        last_age = tbaa_decorate(tbaa_gcframe, builder.CreateLoad(ctx.world_age_field));
    }

    // step 8. allocate local variables slots
    // must be in the first basic block for the llvm mem2reg pass to work

    // get pointers for locals stored in the gc frame array (argTemp)
    for (i = 0; i < vinfoslen; i++) {
        jl_sym_t *s = slot_symbol(i, &ctx);
        if (s == unused_sym) continue;
        jl_varinfo_t &varinfo = ctx.slots[i];
        assert(!varinfo.memloc); // variables shouldn't have memory locs already
        if (varinfo.value.constant) {
            // no need to explicitly load/store a constant/ghost value
            alloc_def_flag(varinfo, &ctx);
            continue;
        }
        else if (jl_is_type_type(varinfo.value.typ) && jl_is_leaf_type(jl_tparam0(varinfo.value.typ))) {
            // replace T::Type{T} with T
            varinfo.value = mark_julia_const(jl_tparam0(varinfo.value.typ));
            alloc_def_flag(varinfo, &ctx);
            continue;
        }
        else if (store_unboxed_p(i, &ctx)) {
            if (!varinfo.isArgument) { // otherwise, just leave it in the input register
                Value *lv = alloc_local(i, &ctx); (void)lv;
#if JL_LLVM_VERSION >= 30600
                if (ctx.debug_enabled && varinfo.dinfo) {
                    assert((Metadata*)varinfo.dinfo->getType() != jl_pvalue_dillvmt);
                    dbuilder.insertDeclare(lv, varinfo.dinfo, dbuilder.createExpression(),
#if JL_LLVM_VERSION >= 30700
                                           topdebugloc,
#endif
                                           builder.GetInsertBlock());
                }
#endif
            }
            continue;
        }
        if (!varinfo.isArgument || // always need a slot if the variable is assigned
            specsig || // for arguments, give them stack slots if they aren't in `argArray` (otherwise, will use that pointer)
            (va && (int)i == ctx.vaSlot && varinfo.escapes) || // or it's the va arg tuple
            (s != unused_sym && i == 0)) { // or it is the first argument (which isn't in `argArray`)
            AllocaInst *av = new AllocaInst(T_pjlvalue, jl_symbol_name(s), /*InsertBefore*/ctx.ptlsStates);
            varinfo.memloc = av;
#if JL_LLVM_VERSION >= 30600
            if (ctx.debug_enabled && varinfo.dinfo) {
                DIExpression *expr;
                if ((Metadata*)varinfo.dinfo->getType() == jl_pvalue_dillvmt) {
                    expr = dbuilder.createExpression();
                }
                else {
                    SmallVector<uint64_t, 8> addr;
                    addr.push_back(llvm::dwarf::DW_OP_deref);
                    expr = dbuilder.createExpression(addr);
                }
                dbuilder.insertDeclare(av, varinfo.dinfo, expr,
#if JL_LLVM_VERSION >= 30700
                                            topdebugloc,
#endif
                                builder.GetInsertBlock());
            }
#endif
        }
        maybe_alloc_arrayvar(i, &ctx);
    }

    // step 9. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    if (ctx.sret)
        AI++; // skip sret slot
    for (i = 0; i < nreq; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed;
        Type *llvmArgType = julia_type_to_llvm(argType, &isboxed);
        if (s == unused_sym) {
            if (specsig && !type_is_ghost(llvmArgType)) ++AI;
            continue;
        }
        jl_varinfo_t &vi = ctx.slots[i];
        jl_cgval_t theArg;
        if (s == unused_sym || vi.value.constant) {
            assert(vi.memloc == NULL);
            if (specsig && !type_is_ghost(llvmArgType)) ++AI;
        }
        else {
            if (specsig) {
                if (type_is_ghost(llvmArgType)) { // this argument is not actually passed
                    theArg = ghostValue(argType);
                }
                else if (llvmArgType->isAggregateType()) {
                    theArg = mark_julia_slot(&*AI++, argType, tbaa_const); // this argument is by-pointer
                    theArg.isimmutable = true;
                }
                else {
                    theArg = mark_julia_type(&*AI++, isboxed, argType, &ctx, /*needsgcroot*/false);
                }
            }
            else {
                if (i == 0) {
                    // first (function) arg is separate in jlcall
                    theArg = mark_julia_type(fArg, true, vi.value.typ, &ctx, /*needsgcroot*/false);
                }
                else {
                    Value *argPtr = builder.CreateGEP(argArray, ConstantInt::get(T_size, i-1));
                    theArg = mark_julia_type(builder.CreateLoad(argPtr), true, vi.value.typ, &ctx, /*needsgcroot*/false);
#if JL_LLVM_VERSION >= 30600
                    if (ctx.debug_enabled && vi.dinfo && !vi.memloc && !vi.value.V) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus);
                        addr.push_back((i - 1) * sizeof(void*));
                        if ((Metadata*)vi.dinfo->getType() != jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        dbuilder.insertDeclare(pargArray, vi.dinfo, dbuilder.createExpression(addr),
#if JL_LLVM_VERSION >= 30700
                                        topdebugloc,
#endif
                                        builder.GetInsertBlock());
                    }
#endif
                }
            }

            if (vi.memloc == NULL) {
                if (vi.value.V) {
                    // copy theArg into its local variable slot (unboxed)
                    assert(vi.value.ispointer());
                    tbaa_decorate(vi.value.tbaa,
                                  builder.CreateStore(emit_unbox(vi.value.V->getType()->getContainedType(0),
                                                                 theArg, vi.value.typ),
                                                      vi.value.V));
                }
                else {
                    // keep track of original (possibly boxed) value to avoid re-boxing or moving
                    vi.value = theArg;
#if JL_LLVM_VERSION >= 30600
                    if (specsig && theArg.V && ctx.debug_enabled && vi.dinfo) {
                        SmallVector<uint64_t, 8> addr;
                        if ((Metadata*)vi.dinfo->getType() != jl_pvalue_dillvmt && theArg.ispointer())
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        AllocaInst *parg = dyn_cast<AllocaInst>(theArg.V);
                        if (!parg) {
                            parg = builder.CreateAlloca(theArg.V->getType(), NULL, jl_symbol_name(s));
                            builder.CreateStore(theArg.V, parg);
                        }
                        dbuilder.insertDeclare(parg, vi.dinfo, dbuilder.createExpression(addr),
#if JL_LLVM_VERSION >= 30700
                                                    topdebugloc,
#endif
                                                    builder.GetInsertBlock());
                    }
#endif
                }
            }
            else {
                Value *argp = boxed(theArg, &ctx, false); // skip the temporary gcroot since it would be folded to argp anyways
                builder.CreateStore(argp, vi.memloc);
                if (!theArg.isboxed)
                    emit_local_root(&ctx, &vi); // create a root for vi
            }
            // get arrayvar data if applicable
            if (arrayvars.find(i) != arrayvars.end()) {
                jl_arrayvar_t av = arrayvars[i];
                assign_arrayvar(av, theArg, &ctx);
            }
        }
    }

    // step 10. allocate rest argument if necessary
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &vi = ctx.slots[ctx.vaSlot];
        if (!vi.escapes) {
            ctx.vaStack = true;
        }
        else if (!vi.value.constant) {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            if (vi.memloc != NULL) {
#if JL_LLVM_VERSION >= 30700
                Value *restTuple =
                    builder.CreateCall(prepare_call(jltuple_func), {V_null,
                                        builder.CreateGEP(argArray,
                                                          ConstantInt::get(T_size,nreq-1)),
                                        builder.CreateSub(argCount,
                                                          ConstantInt::get(T_int32,nreq-1))});
#else
                Value *restTuple =
                    builder.CreateCall3(prepare_call(jltuple_func), V_null,
                                        builder.CreateGEP(argArray,
                                                          ConstantInt::get(T_size,nreq-1)),
                                        builder.CreateSub(argCount,
                                                          ConstantInt::get(T_int32,nreq-1)));
#endif
                builder.CreateStore(restTuple, vi.memloc);
                emit_local_root(&ctx, &vi); // create a root for vi
            }
            else {
                // TODO: Perhaps allow this in the future, but for now since varargs
                // are always unspecialized we don't
                assert(false);
            }
        }
        else {
            assert(vi.memloc == NULL);
        }
    }

    // step 11. Compute properties for each statements
    //     This needs to be computed by iterating in the IR order
    //     instead of control flow order.
    auto in_user_mod = [] (jl_module_t *mod) {
        return (!jl_is_submodule(mod, jl_base_module) &&
                !jl_is_submodule(mod, jl_core_module));
    };
    struct DbgState {
        DebugLoc loc;
#if JL_LLVM_VERSION >= 30700
        DISubprogram *sp;
#else
        DISubprogram sp;
#endif
        StringRef file;
        ssize_t line;
        bool in_user_code;
    };
    struct StmtProp {
        DebugLoc loc;
        StringRef file;
        ssize_t line;
        bool is_inbounds;
        bool loc_changed;
        bool is_poploc;
        bool in_user_code;
    };
    std::vector<StmtProp> stmtprops(stmtslen);
    std::vector<DbgState> DI_stack;
    std::vector<bool> inbounds_stack{false};
    auto is_inbounds = [&] () {
        // inbounds rule is either of top two values on inbounds stack are true
        size_t sz = inbounds_stack.size();
        bool inbounds = sz && inbounds_stack.back();
        if (sz > 1)
            inbounds |= inbounds_stack[sz - 2];
        return inbounds;
    };
    StmtProp cur_prop{topdebugloc, filename, toplineno,
            false, true, false, false};
    ctx.line = &cur_prop.line;
    if (coverage_mode != JL_LOG_NONE || malloc_log_mode) {
        cur_prop.in_user_code = (!jl_is_submodule(ctx.module, jl_base_module) &&
                                 !jl_is_submodule(ctx.module, jl_core_module));
    }
    for (i = 0; i < stmtslen; i++) {
        cur_prop.loc_changed = false;
        cur_prop.is_poploc = false;
        jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
        jl_expr_t *expr = jl_is_expr(stmt) ? (jl_expr_t*)stmt : nullptr;
#ifndef NDEBUG
        if (jl_is_labelnode(stmt)) {
            size_t lname = jl_labelnode_label(stmt);
            if (lname != i + 1) {
                jl_safe_printf("Label number mismatch.\n");
                jl_(stmts);
                abort();
            }
        }
#endif
        if (jl_is_linenode(stmt) || (expr && expr->head == line_sym)) {
            ssize_t lno = -1;
            if (jl_is_linenode(stmt)) {
                lno = jl_linenode_line(stmt);
            }
            else {
                lno = jl_unbox_long(jl_exprarg(stmt,0));
            }
            MDNode *inlinedAt = NULL;
            if (DI_stack.size() > 0) {
#if JL_LLVM_VERSION >= 30700
                inlinedAt = DI_stack.back().loc;
#else
                inlinedAt = DI_stack.back().loc.getAsMDNode(jl_LLVMContext);
#endif
            }
            if (ctx.debug_enabled)
                cur_prop.loc = DebugLoc::get(lno, 0, SP, inlinedAt);
            cur_prop.line = lno;
            cur_prop.loc_changed = true;
        }
        else if (expr && expr->head == meta_sym &&
                 jl_array_len(expr->args) >= 1) {
            jl_value_t *meta_arg = jl_exprarg(expr, 0);
            if (meta_arg == (jl_value_t*)jl_symbol("push_loc")) {
                const char *new_filename = "<missing>";
                assert(jl_array_len(expr->args) > 1);
                jl_sym_t *filesym = (jl_sym_t*)jl_exprarg(expr, 1);
                if (filesym != empty_sym)
                    new_filename = jl_symbol_name(filesym);
#if JL_LLVM_VERSION >= 30700
                DIFile *new_file = nullptr;
#else
                DIFile new_file;
#endif
                if (ctx.debug_enabled)
                    new_file = dbuilder.createFile(new_filename, ".");
                DI_stack.push_back(DbgState{cur_prop.loc, SP,
                            cur_prop.file, cur_prop.line,
                            cur_prop.in_user_code});
                const char *inl_name = "";
                int inlined_func_lineno = 0;
                if (jl_array_len(expr->args) > 2) {
                    for (size_t ii = 2; ii < jl_array_len(expr->args); ii++) {
                        jl_value_t *arg = jl_exprarg(expr, ii);
                        if (jl_is_symbol(arg))
                            inl_name = jl_symbol_name((jl_sym_t*)arg);
                        else if (jl_is_int32(arg))
                            inlined_func_lineno = jl_unbox_int32(arg);
                        else if (jl_is_int64(arg))
                            inlined_func_lineno = jl_unbox_int64(arg);
                        else if (jl_is_module(arg)) {
                            jl_module_t *mod = (jl_module_t*)arg;
                            cur_prop.in_user_code = in_user_mod(mod);
                        }
                    }
                }
                else {
                    inl_name = "macro expansion";
                }
                if (ctx.debug_enabled) {
                    SP = dbuilder.createFunction(new_file,
                                                 std::string(inl_name) + ";",
                                                 inl_name,
                                                 new_file,
                                                 0,
                                                 jl_di_func_null_sig,
                                                 false,
                                                 true,
                                                 0,
                                                 DIFlagZero,
                                                 true,
                                                 nullptr);
                    MDNode *inlinedAt = NULL;
#if JL_LLVM_VERSION >= 30700
                    inlinedAt = cur_prop.loc;
#else
                    inlinedAt = cur_prop.loc.getAsMDNode(jl_LLVMContext);
#endif
                    cur_prop.loc = DebugLoc::get(inlined_func_lineno,
                                                 0, SP, inlinedAt);
                }
                cur_prop.file = new_filename;
                cur_prop.line = inlined_func_lineno;
                cur_prop.loc_changed = true;
            }
            else if (meta_arg == (jl_value_t*)jl_symbol("pop_loc")) {
                cur_prop.is_poploc = true;
                auto &DI = DI_stack.back();
                SP = DI.sp;
                cur_prop.loc = DI.loc;
                cur_prop.file = DI.file;
                cur_prop.line = DI.line;
                cur_prop.in_user_code = DI.in_user_code;
                DI_stack.pop_back();
                cur_prop.loc_changed = true;
            }
        }
        if (expr) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);
            if (expr->head == inbounds_sym) {
                // manipulate inbounds stack
                if (jl_array_len(expr->args) > 0) {
                    jl_value_t *arg = args[0];
                    if (arg == jl_true) {
                        inbounds_stack.push_back(true);
                    }
                    else if (arg == jl_false) {
                        inbounds_stack.push_back(false);
                    }
                    else if (!inbounds_stack.empty()) {
                        inbounds_stack.pop_back();
                    }
                }
            }
        }
        cur_prop.is_inbounds = is_inbounds();
        stmtprops[i] = cur_prop;
    }
    DI_stack.clear();
    inbounds_stack.clear();

    // step 12. Do codegen in control flow order
    std::vector<std::pair<int,BasicBlock*>> workstack;
    int cursor = 0;
    // Whether we are doing codegen in statement order.
    // We need to update debug location if this is false even if
    // `loc_changed` is false.
    bool linear_codegen = true;
    auto find_next_stmt = [&] (int seq_next) {
        // `seq_next` is the next statement we want to emit
        // i.e. if it exists, it's the next one following control flow and
        // should be emitted into the current insert point.
        if (seq_next >= 0 && (unsigned)seq_next < stmtslen) {
            linear_codegen = (seq_next - cursor) == 1;
            cursor = seq_next;
            return;
        }
        if (!builder.GetInsertBlock()->getTerminator())
            builder.CreateUnreachable();
        if (workstack.empty()) {
            cursor = -1;
            linear_codegen = false;
            return;
        }
        auto &item = workstack.back();
        builder.SetInsertPoint(item.second);
        linear_codegen = (item.first - cursor) == 1;
        cursor = item.first;
        workstack.pop_back();
    };
    auto add_to_list = [&] (unsigned pos, BasicBlock *bb) {
        if (pos >= stmtslen)
            return;
        workstack.push_back({pos, bb});
    };
    // returns the corresponding basic block.
    // if `unconditional` a unconditional branch is created to the target
    // label and the cursor is set to the next statement to process
    auto handle_label = [&] (int lname, bool unconditional) {
        auto &bb = labels[lname];
        BasicBlock *cur_bb = builder.GetInsertBlock();
        // Check if we've already visited this label
        if (bb) {
            // Already in the work list
            // branch to it and pop one from the work list
            if (unconditional) {
                if (!cur_bb->getTerminator())
                    builder.CreateBr(bb);
                find_next_stmt(-1);
            }
            return bb;
        }
        // If this is a label node in an empty bb
        if (lname == cursor + 1 && cur_bb->begin() == cur_bb->end()) {
            assert(unconditional);
            // Use this bb as the one for the new label.
            bb = cur_bb;
        }
        else {
            // Otherwise, create a new BB
            // use the label name as the BB name.
            bb = BasicBlock::Create(jl_LLVMContext,
                                    "L" + std::to_string(lname), f);
            if (unconditional) {
                if (!cur_bb->getTerminator())
                    builder.CreateBr(bb);
                builder.SetInsertPoint(bb);
            }
            else {
                add_to_list(lname, bb);
            }
        }
        if (unconditional)
            find_next_stmt(lname);
        return bb;
    };

    auto do_coverage = [&] (bool in_user_code) {
        if (!JL_FEAT_TEST(&ctx, code_coverage)) return false;
        return (coverage_mode == JL_LOG_ALL ||
                (coverage_mode == JL_LOG_USER && in_user_code));
    };
    auto do_malloc_log = [&] (bool in_user_code) {
        if (!JL_FEAT_TEST(&ctx, track_allocations)) return false;
        return (malloc_log_mode == JL_LOG_ALL ||
                (malloc_log_mode == JL_LOG_USER && in_user_code));
    };

    // Handle the implicit first line number node.
    if (ctx.debug_enabled)
        builder.SetCurrentDebugLocation(topdebugloc);
    if (coverage_mode != JL_LOG_NONE && do_coverage(in_user_mod(ctx.module)))
        coverageVisitLine(filename, toplineno);
    while (cursor != -1) {
        auto &props = stmtprops[cursor];
        if ((props.loc_changed || !linear_codegen) && ctx.debug_enabled)
            builder.SetCurrentDebugLocation(props.loc);
        // Disable coverage for pop_loc, it doesn't start a new expression
        if (props.loc_changed && do_coverage(props.in_user_code) &&
            !props.is_poploc) {
            coverageVisitLine(props.file, props.line);
        }
        ctx.is_inbounds = props.is_inbounds;
        jl_value_t *stmt = jl_array_ptr_ref(stmts, cursor);
        jl_expr_t *expr = jl_is_expr(stmt) ? (jl_expr_t*)stmt : nullptr;
        if (jl_is_labelnode(stmt)) {
            // Label node
            int lname = jl_labelnode_label(stmt);
            handle_label(lname, true);
            continue;
        }
        if (expr && expr->head == return_sym) {
            bool retboxed = false;
            Type *retty;
            if (specsig) {
                retty = julia_type_to_llvm(jlrettype, &retboxed);
            }
            else {
                retty = T_pjlvalue;
                retboxed = true;
            }
            jl_cgval_t retvalinfo = emit_expr(jl_exprarg(expr, 0), &ctx);
            Value *retval;
            if (retboxed) {
                retval = boxed(retvalinfo, &ctx, false); // skip the gcroot on the return path
                assert(!ctx.sret);
            }
            else if (!type_is_ghost(retty)) {
                retval = emit_unbox(retty, retvalinfo, jlrettype,
                                    ctx.sret ? &*ctx.f->arg_begin() : NULL);
            }
            else { // undef return type
                retval = NULL;
            }
            if (do_malloc_log(props.in_user_code) && props.line != -1)
                mallocVisitLine(props.file, props.line);
            if (toplevel)
                builder.CreateStore(last_age, ctx.world_age_field);
            if (type_is_ghost(retty) || ctx.sret)
                builder.CreateRetVoid();
            else
                builder.CreateRet(retval);
            find_next_stmt(-1);
            continue;
        }
        if (jl_is_gotonode(stmt)) {
            int lname = jl_gotonode_label(stmt);
            handle_label(lname, true);
            continue;
        }
        if (expr && expr->head == goto_ifnot_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);
            jl_value_t *cond = args[0];
            int lname = jl_unbox_long(args[1]);
            Value *isfalse = emit_condition(cond, "if", &ctx);
            if (do_malloc_log(props.in_user_code) && props.line != -1)
                mallocVisitLine(props.file, props.line);
            BasicBlock *ifso = BasicBlock::Create(jl_LLVMContext, "if", f);
            BasicBlock *ifnot = handle_label(lname, false);
            // Any branches treated as constant in type inference should be
            // eliminated before running
            builder.CreateCondBr(isfalse, ifnot, ifso);
            builder.SetInsertPoint(ifso);
        }
        else if (expr && expr->head == enter_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);
            assert(jl_is_long(args[0]));
            int lname = jl_unbox_long(args[0]);
            CallInst *sj = builder.CreateCall(prepare_call(except_enter_func));
            // We need to mark this on the call site as well. See issue #6757
            sj->setCanReturnTwice();
            Value *isz = builder.CreateICmpEQ(sj, ConstantInt::get(T_int32, 0));
            BasicBlock *tryblk = BasicBlock::Create(jl_LLVMContext, "try", f);
            BasicBlock *handlr = handle_label(lname, false);
#ifdef _OS_WINDOWS_
            BasicBlock *cond_resetstkoflw_blk = BasicBlock::Create(jl_LLVMContext, "cond_resetstkoflw", f);
            BasicBlock *resetstkoflw_blk = BasicBlock::Create(jl_LLVMContext, "resetstkoflw", f);
            builder.CreateCondBr(isz, tryblk, cond_resetstkoflw_blk);
            builder.SetInsertPoint(cond_resetstkoflw_blk);
            builder.CreateCondBr(builder.CreateICmpEQ(
                                     literal_pointer_val(jl_stackovf_exception),
                                     builder.CreateLoad(emit_exc_in_transit(&ctx), true)),
                                 resetstkoflw_blk, handlr);
            builder.SetInsertPoint(resetstkoflw_blk);
            builder.CreateCall(prepare_call(resetstkoflw_func)
#                          if JL_LLVM_VERSION >= 30700
                               , {}
#                          endif
                );
            builder.CreateBr(handlr);
#else
            builder.CreateCondBr(isz, tryblk, handlr);
#endif
            builder.SetInsertPoint(tryblk);
        }
        else {
            emit_stmtpos(stmt, &ctx);
            if (do_malloc_log(props.in_user_code) && props.line != -1) {
                mallocVisitLine(props.file, props.line);
            }
        }
        find_next_stmt(cursor + 1);
    }
    builder.SetCurrentDebugLocation(noDbg);
    builder.ClearInsertionPoint();

    // step 13. Perform any delayed instantiations
    if (ctx.debug_enabled) {
        dbuilder.finalize();
    }

    // copy ctx.roots into m->roots
    // if we created any new roots during codegen
    if (ctx.roots) {
        jl_method_t *m = lam->def;
        JL_LOCK(&m->writelock);
        if (m->roots == NULL) {
            m->roots = ctx.roots;
            jl_gc_wb(m, m->roots);
        }
        else {
            size_t i, ilen = jl_array_dim0(ctx.roots);
            size_t j, jlen = jl_array_dim0(m->roots);
            for (i = 0; i < ilen; i++) {
                jl_value_t *ival = jl_array_ptr_ref(ctx.roots, i);
                for (j = 0; j < jlen; j++) {
                    jl_value_t *jval = jl_array_ptr_ref(m->roots, j);
                    if (ival == jval)
                        break;
                }
                if (j == jlen) // not found - add to array
                    jl_array_ptr_1d_push(m->roots, ival);
            }
        }
        ctx.roots = NULL;
        JL_UNLOCK(&m->writelock);
    }

    JL_GC_POP();
    return std::unique_ptr<Module>(M);
}

// --- initialization ---

std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false)
{
    static MDBuilder *mbuilder = new MDBuilder(jl_LLVMContext);
    static MDNode *tbaa_root = mbuilder->createTBAARoot("jtbaa");
    if (!parent)
        parent = tbaa_root;
#if JL_LLVM_VERSION >= 30700
    MDNode *scalar = mbuilder->createTBAAScalarTypeNode(name, parent);
    MDNode *n = mbuilder->createTBAAStructTagNode(scalar, scalar, 0, isConstant);
#else
    MDNode *n = mbuilder->createTBAANode(name, parent, isConstant);
    MDNode *scalar = n;
#if JL_LLVM_VERSION < 30600
#if JL_LLVM_VERSION >= 30500
    n->setValueName(ValueName::Create(name));
#else
    n->setValueName(ValueName::Create(name, name + strlen(name)));
#endif
#endif
#endif
    return std::make_pair(n, scalar);
}

static GlobalVariable *global_to_llvm(const std::string &cname, void *addr, Module *m)
{
    GlobalVariable *gv =
        new GlobalVariable(*m, T_pjlvalue, true,
                           GlobalVariable::ExternalLinkage, NULL, cname);
    add_named_global(gv, addr);
    return gv;
}
llvm::SmallVector<std::pair<jl_value_t**, GlobalVariable*>, 16> gv_for_global;
static GlobalVariable *global_jlvalue_to_llvm(const std::string &cname, jl_value_t **addr, Module *m)
{
    GlobalVariable *gv = global_to_llvm(cname, (void*)addr, m);
    gv_for_global.push_back(std::make_pair(addr, gv));
    return gv;
}
static GlobalVariable *julia_const_gv(jl_value_t *val)
{
    for (auto& kv : gv_for_global) {
        if (*kv.first == val)
            return kv.second;
    }
    return nullptr;
}

static Function *jlcall_func_to_llvm(const std::string &cname, jl_fptr_t addr, Module *m)
{
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage, cname, m);
    add_named_global(f, addr);
    return f;
}

extern "C" void jl_fptr_to_llvm(jl_fptr_t fptr, jl_method_instance_t *lam, int specsig)
{
    if (imaging_mode) {
        if (!specsig) {
            lam->fptr = fptr; // in imaging mode, it's fine to use the fptr, but we don't want it in the shadow_module
        }
    }
    else {
        // this assigns a function pointer (from loading the system image), to the function object
        std::stringstream funcName;
        if (specsig)
            funcName << "jlsys_"; // the specsig implementation
        else if (lam->functionObjectsDecls.specFunctionObject)
            funcName << "jlsysw_"; // it's a specsig wrapper
        else if (lam->jlcall_api == 1)
            funcName << "jsys1_"; // it's a jlcall without a specsig
        const char* unadorned_name = jl_symbol_name(lam->def->name);
#if (defined(_OS_LINUX_) && JL_LLVM_VERSION < 30400)
        if (unadorned_name[0] == '@')
            unadorned_name++;
#endif
        funcName << unadorned_name << "_" << globalUnique++;
        if (specsig) { // assumes !va
            SmallVector<Type*, 8> fsig;
            jl_value_t *jlrettype = lam->rettype;
            bool retboxed;
            Type *rt;
            if (jlrettype == (jl_value_t*)jl_void_type) {
                rt = T_void;
                retboxed = false;
            }
            else {
                rt = julia_type_to_llvm(jlrettype, &retboxed);
            }
            bool sret = false;
            if (!retboxed && rt != T_void && deserves_sret(jlrettype, rt)) {
                sret = true;
                fsig.push_back(rt->getPointerTo());
                rt = T_void;
            }
            for (size_t i = 0; i < jl_nparams(lam->specTypes); i++) {
                Type *ty = julia_type_to_llvm(jl_tparam(lam->specTypes,i));
                if (type_is_ghost(ty))
                    continue;
                if (ty->isAggregateType()) // aggregate types are passed by pointer
                    ty = PointerType::get(ty,0);
                fsig.push_back(ty);
            }
            Function *f = Function::Create(FunctionType::get(rt, fsig, false), Function::ExternalLinkage, funcName.str(), shadow_output);
            if (sret)
                f->addAttribute(1, Attribute::StructRet);

            if (lam->functionObjectsDecls.specFunctionObject == NULL) {
                lam->functionObjectsDecls.specFunctionObject = (void*)f;
            }
            add_named_global(f, fptr);
        }
        else {
            if (lam->jlcall_api != 1) { // jl_func_sig_sparams -- don't bother emitting the FunctionObject (since can't be used right now)
                assert(lam->fptr == NULL);
                lam->fptr = fptr;
            }
            else {
                Function *f = jlcall_func_to_llvm(funcName.str(), fptr, shadow_output);
                if (lam->functionObjectsDecls.functionObject == NULL) {
                    lam->functionObjectsDecls.functionObject = (void*)f;
                    assert(lam->fptr == NULL);
                    lam->fptr = fptr;
                }
            }
        }
    }
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 3 && defined(SYSTEM_LLVM)
#define INSTCOMBINE_BUG
#define V128_BUG
#endif

static void init_julia_llvm_meta(void)
{
    tbaa_gcframe = tbaa_make_child("jtbaa_gcframe").first;
    tbaa_stack = tbaa_make_child("jtbaa_stack").first;
    MDNode *tbaa_data_scalar;
    std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child("jtbaa_data");
    tbaa_tag = tbaa_make_child("jtbaa_tag", tbaa_data_scalar).first;
    tbaa_binding = tbaa_make_child("jtbaa_binding", tbaa_data_scalar).first;
    MDNode *tbaa_value_scalar;
    std::tie(tbaa_value, tbaa_value_scalar) =
        tbaa_make_child("jtbaa_value", tbaa_data_scalar);
    tbaa_mutab = tbaa_make_child("jtbaa_mutab", tbaa_value_scalar).first;
    tbaa_immut = tbaa_make_child("jtbaa_immut", tbaa_value_scalar).first;
    tbaa_arraybuf = tbaa_make_child("jtbaa_arraybuf", tbaa_data_scalar).first;
    MDNode *tbaa_array_scalar;
    std::tie(tbaa_array, tbaa_array_scalar) = tbaa_make_child("jtbaa_array");
    tbaa_arrayptr = tbaa_make_child("jtbaa_arrayptr", tbaa_array_scalar).first;
    tbaa_arraysize = tbaa_make_child("jtbaa_arraysize", tbaa_array_scalar).first;
    tbaa_arraylen = tbaa_make_child("jtbaa_arraylen", tbaa_array_scalar).first;
    tbaa_arrayflags = tbaa_make_child("jtbaa_arrayflags", tbaa_array_scalar).first;
    tbaa_const = tbaa_make_child("jtbaa_const", nullptr, true).first;
}

static void init_julia_llvm_env(Module *m)
{
    // every variable or function mapped in this function must be
    // exported from libjulia, to support static compilation
    T_int1  = Type::getInt1Ty(jl_LLVMContext);
    T_int8  = Type::getInt8Ty(jl_LLVMContext);
    T_pint8 = PointerType::get(T_int8, 0);
    T_ppint8 = PointerType::get(T_pint8, 0);
    T_pppint8 = PointerType::get(T_ppint8, 0);
    T_int16 = Type::getInt16Ty(jl_LLVMContext);
    T_pint16 = PointerType::get(T_int16, 0);
    T_int32 = Type::getInt32Ty(jl_LLVMContext);
    T_char = Type::getInt32Ty(jl_LLVMContext);
    T_pint32 = PointerType::get(T_int32, 0);
    T_int64 = Type::getInt64Ty(jl_LLVMContext);
    T_pint64 = PointerType::get(T_int64, 0);
    T_uint8 = T_int8;   T_uint16 = T_int16;
    T_uint32 = T_int32; T_uint64 = T_int64;
    if (sizeof(size_t) == 8)
        T_size = T_uint64;
    else
        T_size = T_uint32;
    T_sigatomic = Type::getIntNTy(jl_LLVMContext, sizeof(sig_atomic_t) * 8);
    T_psize = PointerType::get(T_size, 0);
    T_float16 = Type::getHalfTy(jl_LLVMContext);
    T_float32 = Type::getFloatTy(jl_LLVMContext);
    T_pfloat32 = PointerType::get(T_float32, 0);
    T_float64 = Type::getDoubleTy(jl_LLVMContext);
    T_pfloat64 = PointerType::get(T_float64, 0);
    T_float128 = Type::getFP128Ty(jl_LLVMContext);
    T_void = Type::getVoidTy(jl_LLVMContext);
    T_pvoidfunc = FunctionType::get(T_void, /*isVarArg*/false)->getPointerTo();

    // This type is used to create undef Values for use in struct declarations to skip indices
    NoopType = ArrayType::get(T_int1, 0);

    // add needed base debugging definitions to our LLVM environment
    DIBuilder dbuilder(*m);
#if JL_LLVM_VERSION >= 30700
    DIFile *julia_h = dbuilder.createFile("julia.h","");
    jl_value_dillvmt = dbuilder.createStructType(nullptr,
#else
    DIFile julia_h = dbuilder.createFile("julia.h","");
    jl_value_dillvmt = dbuilder.createStructType(DIDescriptor(),
#endif
        "jl_value_t",
        julia_h,
        71, // At the time of this writing. Not sure if it's worth it to keep this in sync
        0 * 8, // sizeof(jl_value_t) * 8,
        __alignof__(void*) * 8, // __alignof__(jl_value_t) * 8,
        DIFlagZero, // Flags
#if JL_LLVM_VERSION >= 30700
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later
#else
        DIType(), // Derived from
        DIArray()); // Elements - will be corrected later
#endif

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt, sizeof(jl_value_t*) * 8,
                                                   __alignof__(jl_value_t*) * 8);

#if JL_LLVM_VERSION >= 30600
    SmallVector<llvm::Metadata *, 1> Elts;
    std::vector<Metadata*> diargs(0);
    Elts.push_back(jl_pvalue_dillvmt);
    dbuilder.replaceArrays(jl_value_dillvmt,
       dbuilder.getOrCreateArray(Elts));
#else
    SmallVector<llvm::Value *, 1> Elts;
    std::vector<Value*> diargs(0);
    Elts.push_back(jl_pvalue_dillvmt);
    jl_value_dillvmt.setTypeArray(dbuilder.getOrCreateArray(Elts));
#endif

    jl_ppvalue_dillvmt = dbuilder.createPointerType(jl_pvalue_dillvmt,sizeof(jl_value_t**)*8,
                                                    __alignof__(jl_value_t**)*8);

    diargs.push_back(jl_pvalue_dillvmt);    // Return Type (ret value)
    diargs.push_back(jl_pvalue_dillvmt);    // First Argument (function)
    diargs.push_back(jl_ppvalue_dillvmt);   // Second Argument (argv)
    // Third argument (length(argv))
    diargs.push_back(julia_type_to_di((jl_value_t*)jl_int32_type,&dbuilder,false));

#if JL_LLVM_VERSION >= 30800
    jl_di_func_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(None));
#elif JL_LLVM_VERSION >= 30600
    jl_di_func_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateTypeArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateTypeArray(None));
#else
    jl_di_func_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateArray(ArrayRef<Value*>()));
#endif

    T_pjlvalue = T_ppint8;
    T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
    two_pvalue_llvmt.push_back(T_pjlvalue);
    two_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    four_pvalue_llvmt.push_back(T_pjlvalue);
    four_pvalue_llvmt.push_back(T_pjlvalue);
    four_pvalue_llvmt.push_back(T_pjlvalue);
    four_pvalue_llvmt.push_back(T_pjlvalue);
    V_null = Constant::getNullValue(T_pjlvalue);
    jl_init_jit(T_pjlvalue);

    std::vector<Type*> ftargs(0);
    ftargs.push_back(T_pjlvalue);  // linfo->sparam_vals
    ftargs.push_back(T_pjlvalue);  // function
    ftargs.push_back(T_ppjlvalue); // args[]
    ftargs.push_back(T_int32);     // nargs
    jl_func_sig_sparams = FunctionType::get(T_pjlvalue, ftargs, false);
    assert(jl_func_sig_sparams != NULL);
    ftargs.erase(ftargs.begin());  // drop linfo->sparams_vals argument
    jl_func_sig = FunctionType::get(T_pjlvalue, ftargs, false);
    assert(jl_func_sig != NULL);

    Type *vaelts[] = {T_pint8
#ifdef STORE_ARRAY_LEN
                      , T_size
#endif
                      , T_int16
    };
    static_assert(sizeof(jl_array_flags_t) == sizeof(int16_t),
                  "Size of jl_array_flags_t is not the same as int16_t");
    Type *jl_array_llvmt =
        StructType::create(jl_LLVMContext,
                           ArrayRef<Type*>(vaelts,sizeof(vaelts)/sizeof(vaelts[0])),
                           "jl_array_t");
    jl_parray_llvmt = PointerType::get(jl_array_llvmt,0);

    global_to_llvm("__stack_chk_guard", (void*)&__stack_chk_guard, m);
    Function *jl__stack_chk_fail =
        Function::Create(FunctionType::get(T_void, false),
                         Function::ExternalLinkage,
                         "__stack_chk_fail", m);
    jl__stack_chk_fail->setDoesNotReturn();
    add_named_global(jl__stack_chk_fail, &__stack_chk_fail);

    global_jlvalue_to_llvm("jl_true", &jl_true, m);
    global_jlvalue_to_llvm("jl_false", &jl_false, m);
    global_jlvalue_to_llvm("jl_emptysvec", (jl_value_t**)&jl_emptysvec, m);
    global_jlvalue_to_llvm("jl_emptytuple", &jl_emptytuple, m);
    global_jlvalue_to_llvm("jl_diverror_exception", &jl_diverror_exception, m);
    global_jlvalue_to_llvm("jl_undefref_exception", &jl_undefref_exception, m);
    global_jlvalue_to_llvm("jl_domain_exception", &jl_domain_exception, m);
    global_jlvalue_to_llvm("jl_overflow_exception", &jl_overflow_exception, m);
    global_jlvalue_to_llvm("jl_inexact_exception", &jl_inexact_exception, m);

    jlRTLD_DEFAULT_var =
        new GlobalVariable(*m, T_pint8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_RTLD_DEFAULT_handle");
    add_named_global(jlRTLD_DEFAULT_var, &jl_RTLD_DEFAULT_handle);
#ifdef _OS_WINDOWS_
    jlexe_var =
        new GlobalVariable(*m, T_pint8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_exe_handle");
    add_named_global(jlexe_var, &jl_exe_handle);
    jldll_var =
        new GlobalVariable(*m, T_pint8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_dl_handle");
    add_named_global(jldll_var, &jl_dl_handle);
#endif

#ifndef JULIA_ENABLE_THREADING
    // For non-threading, we use the address of the global variable directly
    jltls_states_var =
        new GlobalVariable(*m, T_ppjlvalue,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, "jl_tls_states");
    add_named_global(jltls_states_var, &jl_tls_states);
    // placeholder function for keeping track of the end of the gcframe
    jltls_states_func = Function::Create(FunctionType::get(jltls_states_var->getType(), false),
                                         Function::ExternalLinkage,
                                         "jl_get_ptls_states", m);
    add_named_global(jltls_states_func, (void*)NULL, /*dllimport*/false);
#else
    // For threading, we emit a call to the getter function.
    // In non-imaging mode, (i.e. the code will not be saved to disk), we
    // use the address of the actual getter function directly
    // (`jl_tls_states_cb` returned by `jl_get_ptls_states_getter()`)
    // (Alternatively if we know how to generate the tls address directly
    // we will inline the assembly, see `finalize_gc_frame(Function*)`)
    // In imaging mode, we emit the function address as a load of a static
    // variable to be filled (in `dump.c`) at initialization time of the sysimg.
    // This way we can by pass the extra indirection in `jl_get_ptls_states`
    // since we don't know which getter function to use ahead of time.
    jltls_states_func = Function::Create(FunctionType::get(PointerType::get(T_ppjlvalue, 0), false),
                                         Function::ExternalLinkage,
                                         "jl_get_ptls_states", m);
    add_named_global(jltls_states_func, jl_get_ptls_states_getter());
    if (imaging_mode) {
        PointerType *pfunctype = jltls_states_func->getFunctionType()->getPointerTo();
        jltls_states_func_ptr =
            jl_emit_sysimg_slot(m, pfunctype, "jl_get_ptls_states.ptr",
                                (uintptr_t)jl_get_ptls_states_getter(),
                                jltls_states_func_idx);
    }
#endif

    std::vector<Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", m);
    jlerror_func->setDoesNotReturn();
    add_named_global(jlerror_func, &jl_error);

    std::vector<Type*> args1_(0);
    args1_.push_back(T_pjlvalue);
    jlthrow_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_throw", m);
    jlthrow_func->setDoesNotReturn();
    add_named_global(jlthrow_func, &jl_throw);

    jlundefvarerror_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_undefined_var_error", m);
    jlundefvarerror_func->setDoesNotReturn();
    add_named_global(jlundefvarerror_func, &jl_undefined_var_error);

    std::vector<Type*> args2_boundserrorv(0);
    args2_boundserrorv.push_back(T_pjlvalue);
    args2_boundserrorv.push_back(T_psize);
    args2_boundserrorv.push_back(T_size);
    jlboundserrorv_func =
        Function::Create(FunctionType::get(T_void, args2_boundserrorv, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_ints", m);
    jlboundserrorv_func->setDoesNotReturn();
    add_named_global(jlboundserrorv_func, &jl_bounds_error_ints);

    std::vector<Type*> args2_boundserror(0);
    args2_boundserror.push_back(T_pjlvalue);
    args2_boundserror.push_back(T_size);
    jlboundserror_func =
        Function::Create(FunctionType::get(T_void, args2_boundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_int", m);
    jlboundserror_func->setDoesNotReturn();
    add_named_global(jlboundserror_func, &jl_bounds_error_int);

    std::vector<Type*> args3_vboundserror(0);
    args3_vboundserror.push_back(T_ppjlvalue);
    args3_vboundserror.push_back(T_size);
    args3_vboundserror.push_back(T_size);
    jlvboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_vboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_tuple_int", m);
    jlvboundserror_func->setDoesNotReturn();
    add_named_global(jlvboundserror_func, &jl_bounds_error_tuple_int);

    std::vector<Type*> args3_uboundserror(0);
    args3_uboundserror.push_back(T_pint8);
    args3_uboundserror.push_back(T_pjlvalue);
    args3_uboundserror.push_back(T_size);
    jluboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_uboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_unboxed_int", m);
    jluboundserror_func->setDoesNotReturn();
    add_named_global(jluboundserror_func, &jl_bounds_error_unboxed_int);

    jlnew_func =
        Function::Create(jl_func_sig, Function::ExternalLinkage,
                         "jl_new_structv", m);
    add_named_global(jlnew_func, &jl_new_structv);

    std::vector<Type*> args2(0);
    args2.push_back(T_pint8);
#ifndef _OS_WINDOWS_
    args2.push_back(T_int32);
#endif
    setjmp_func =
        Function::Create(FunctionType::get(T_int32, args2, false),
                         Function::ExternalLinkage, jl_setjmp_name, m);
    setjmp_func->addFnAttr(Attribute::ReturnsTwice);
    add_named_global(setjmp_func, &jl_setjmp_f);

    std::vector<Type*> args_memcmp(0);
    args_memcmp.push_back(T_pint8);
    args_memcmp.push_back(T_pint8);
    args_memcmp.push_back(T_size);
    memcmp_func =
        Function::Create(FunctionType::get(T_int32, args_memcmp, false),
                         Function::ExternalLinkage, "memcmp", m);
    add_named_global(memcmp_func, &memcmp);

    std::vector<Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pjlvalue);
    te_args.push_back(T_pjlvalue);
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error_rt", m);
    jltypeerror_func->setDoesNotReturn();
    add_named_global(jltypeerror_func, &jl_type_error_rt);

    std::vector<Type *> args_2ptrs(0);
    args_2ptrs.push_back(T_pjlvalue);
    args_2ptrs.push_back(T_pjlvalue);
    jlcheckassign_func =
        Function::Create(FunctionType::get(T_void, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_checked_assignment", m);
    add_named_global(jlcheckassign_func, &jl_checked_assignment);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(T_pjlvalue);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1ptr, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", m);
    add_named_global(jldeclareconst_func, &jl_declare_constant);

    jlgetbindingorerror_func =
        Function::Create(FunctionType::get(T_pjlvalue, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_get_binding_or_error", m);
    add_named_global(jlgetbindingorerror_func, &jl_get_binding_or_error);

    builtin_func_map[jl_f_is] = jlcall_func_to_llvm("jl_f_is", &jl_f_is, m);
    builtin_func_map[jl_f_typeof] = jlcall_func_to_llvm("jl_f_typeof", &jl_f_typeof, m);
    builtin_func_map[jl_f_sizeof] = jlcall_func_to_llvm("jl_f_sizeof", &jl_f_sizeof, m);
    builtin_func_map[jl_f_issubtype] = jlcall_func_to_llvm("jl_f_issubtype", &jl_f_issubtype, m);
    builtin_func_map[jl_f_isa] = jlcall_func_to_llvm("jl_f_isa", &jl_f_isa, m);
    builtin_func_map[jl_f_typeassert] = jlcall_func_to_llvm("jl_f_typeassert", &jl_f_typeassert, m);
    builtin_func_map[jl_f__apply] = jlcall_func_to_llvm("jl_f__apply", &jl_f__apply, m);
    builtin_func_map[jl_f__apply_pure] = jlcall_func_to_llvm("jl_f__apply_pure", &jl_f__apply_pure, m);
    builtin_func_map[jl_f_throw] = jlcall_func_to_llvm("jl_f_throw", &jl_f_throw, m);
    builtin_func_map[jl_f_tuple] = jlcall_func_to_llvm("jl_f_tuple", &jl_f_tuple, m);
    builtin_func_map[jl_f_svec] = jlcall_func_to_llvm("jl_f_svec", &jl_f_svec, m);
    builtin_func_map[jl_f_applicable] = jlcall_func_to_llvm("jl_f_applicable", &jl_f_applicable, m);
    builtin_func_map[jl_f_invoke] = jlcall_func_to_llvm("jl_f_invoke", &jl_f_invoke, m);
    builtin_func_map[jl_f_isdefined] = jlcall_func_to_llvm("jl_f_isdefined", &jl_f_isdefined, m);
    builtin_func_map[jl_f_getfield] = jlcall_func_to_llvm("jl_f_getfield", &jl_f_getfield, m);
    builtin_func_map[jl_f_setfield] = jlcall_func_to_llvm("jl_f_setfield", &jl_f_setfield, m);
    builtin_func_map[jl_f_fieldtype] = jlcall_func_to_llvm("jl_f_fieldtype", &jl_f_fieldtype, m);
    builtin_func_map[jl_f_nfields] = jlcall_func_to_llvm("jl_f_nfields", &jl_f_nfields, m);
    builtin_func_map[jl_f__expr] = jlcall_func_to_llvm("jl_f__expr", &jl_f__expr, m);
    builtin_func_map[jl_f_arrayref] = jlcall_func_to_llvm("jl_f_arrayref", &jl_f_arrayref, m);
    builtin_func_map[jl_f_arrayset] = jlcall_func_to_llvm("jl_f_arrayset", &jl_f_arrayset, m);
    builtin_func_map[jl_f_arraysize] = jlcall_func_to_llvm("jl_f_arraysize", &jl_f_arraysize, m);
    builtin_func_map[jl_f_apply_type] = jlcall_func_to_llvm("jl_f_apply_type", &jl_f_apply_type, m);
    jltuple_func = builtin_func_map[jl_f_tuple];
    jlgetfield_func = builtin_func_map[jl_f_getfield];

    jlapply2va_func = jlcall_func_to_llvm("jl_apply_2va", &jl_apply_2va, m);

    jltypeassert_func = Function::Create(FunctionType::get(T_void, two_pvalue_llvmt, false),
                                        Function::ExternalLinkage,
                                        "jl_typeassert", m);
    add_named_global(jltypeassert_func, &jl_typeassert);

    queuerootfun = Function::Create(FunctionType::get(T_void, args_1ptr, false),
                                    Function::ExternalLinkage,
                                    "jl_gc_queue_root", m);
    add_named_global(queuerootfun, &jl_gc_queue_root);

    std::vector<Type *> agargs(0);
    agargs.push_back(T_ppjlvalue);
    agargs.push_back(T_uint32);
    jlapplygeneric_func = Function::Create(FunctionType::get(T_pjlvalue, agargs, false),
                                           Function::ExternalLinkage,
                                           "jl_apply_generic", m);
    add_named_global(jlapplygeneric_func, &jl_apply_generic);

    std::vector<Type *> invokeargs(0);
    invokeargs.push_back(T_pjlvalue);
    invokeargs.push_back(T_ppjlvalue);
    invokeargs.push_back(T_uint32);
    jlinvoke_func = Function::Create(FunctionType::get(T_pjlvalue, invokeargs, false),
                                     Function::ExternalLinkage,
                                     "jl_invoke", m);
    add_named_global(jlinvoke_func, &jl_invoke);

    std::vector<Type *> exp_args(0);
    exp_args.push_back(T_int1);
    expect_func = Intrinsic::getDeclaration(m, Intrinsic::expect, exp_args);

    std::vector<Type*> args3(0);
    args3.push_back(T_pjlvalue);
    jltopeval_func =
        Function::Create(FunctionType::get(T_pjlvalue, args3, false),
                         Function::ExternalLinkage,
                         "jl_toplevel_eval", m);
    add_named_global(jltopeval_func, &jl_toplevel_eval);

    jlcopyast_func =
        Function::Create(FunctionType::get(T_pjlvalue, args3, false),
                         Function::ExternalLinkage,
                         "jl_copy_ast", m);
    add_named_global(jlcopyast_func, &jl_copy_ast);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlnsvec_func =
        Function::Create(FunctionType::get(T_pjlvalue, args5, true),
                         Function::ExternalLinkage,
                         "jl_svec", m);
    add_named_global(jlnsvec_func, &jl_svec);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    jlmethod_func =
        Function::Create(FunctionType::get(T_void, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", m);
    add_named_global(jlmethod_func, &jl_method_def);

    std::vector<Type*> funcdefargs(0);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_ppjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    jlgenericfunction_func =
        Function::Create(FunctionType::get(T_pjlvalue, funcdefargs, false),
                         Function::ExternalLinkage,
                         "jl_generic_function_def", m);
    add_named_global(jlgenericfunction_func, &jl_generic_function_def);

    std::vector<Type*> ehargs(0);
    ehargs.push_back(T_pint8);
    jlenter_func =
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage,
                         "jl_enter_handler", m);
    add_named_global(jlenter_func, &jl_enter_handler);

#ifdef _OS_WINDOWS_
    resetstkoflw_func = Function::Create(FunctionType::get(T_int32, false),
            Function::ExternalLinkage, "_resetstkoflw", m);
    add_named_global(resetstkoflw_func, &_resetstkoflw);
#if defined(_CPU_X86_64_)
    juliapersonality_func = Function::Create(FunctionType::get(T_int32, true),
            Function::ExternalLinkage, "__julia_personality", m);
    add_named_global(juliapersonality_func, &__julia_personality);
#endif
#ifndef FORCE_ELF
#if defined(_CPU_X86_64_)
#if defined(_COMPILER_MINGW_)
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "___chkstk_ms", m);
    add_named_global(chkstk_func, &___chkstk_ms, /*dllimport*/false);
#else
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "__chkstk", m);
    add_named_global(chkstk_func, &__chkstk, /*dllimport*/false);
#endif
#else
#if defined(_COMPILER_MINGW_)
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_alloca", m);
    add_named_global(chkstk_func, &_alloca, /*dllimport*/false);
#else
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_chkstk", m);
    add_named_global(chkstk_func, &_chkstk, /*dllimport*/false);
#endif
#endif
#endif
#endif

    std::vector<Type*> lhargs(0);
    lhargs.push_back(T_int32);
    jlleave_func =
        Function::Create(FunctionType::get(T_void, lhargs, false),
                         Function::ExternalLinkage,
                         "jl_pop_handler", m);
    add_named_global(jlleave_func, &jl_pop_handler);

    std::vector<Type *> args_2vals(0);
    args_2vals.push_back(T_pjlvalue);
    args_2vals.push_back(T_pjlvalue);
    jlegal_func =
        Function::Create(FunctionType::get(T_int32, args_2vals, false),
                         Function::ExternalLinkage,
                         "jl_egal", m);
    add_named_global(jlegal_func, &jl_egal);

    std::vector<Type *> isa_args(0);
    isa_args.push_back(T_pjlvalue);
    isa_args.push_back(T_pjlvalue);
    jlisa_func =
        Function::Create(FunctionType::get(T_int32, isa_args, false),
                         Function::ExternalLinkage,
                         "jl_isa", m);
    add_named_global(jlisa_func, &jl_isa);

    std::vector<Type *> applytype_args(0);
    applytype_args.push_back(T_pjlvalue);
    applytype_args.push_back(T_pjlvalue);
    applytype_args.push_back(T_ppjlvalue);
    jlapplytype_func =
        Function::Create(FunctionType::get(T_pjlvalue, applytype_args, false),
                         Function::ExternalLinkage,
                         "jl_instantiate_type_in_env", m);
    add_named_global(jlapplytype_func, &jl_instantiate_type_in_env);

    std::vector<Type*> alloc_pool_args(0);
    alloc_pool_args.push_back(T_pint8);
    alloc_pool_args.push_back(T_int32);
    alloc_pool_args.push_back(T_int32);
    jlalloc_pool_func =
        Function::Create(FunctionType::get(T_pjlvalue, alloc_pool_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_pool_alloc", m);
    add_named_global(jlalloc_pool_func, &jl_gc_pool_alloc);

    std::vector<Type*> alloc_big_args(0);
    alloc_big_args.push_back(T_pint8);
    alloc_big_args.push_back(T_size);
    jlalloc_big_func =
        Function::Create(FunctionType::get(T_pjlvalue, alloc_big_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_big_alloc", m);
    add_named_global(jlalloc_big_func, &jl_gc_big_alloc);

    std::vector<Type *> dlsym_args(0);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(PointerType::get(T_pint8,0));
    jldlsym_func =
        Function::Create(FunctionType::get(T_pvoidfunc, dlsym_args, false),
                         Function::ExternalLinkage,
                         "jl_load_and_lookup", m);
    add_named_global(jldlsym_func, &jl_load_and_lookup);

    std::vector<Type *> newbits_args(0);
    newbits_args.push_back(T_pjlvalue);
    newbits_args.push_back(T_pint8);
    jlnewbits_func =
        Function::Create(FunctionType::get(T_pjlvalue, newbits_args, false),
                         Function::ExternalLinkage,
                         "jl_new_bits", m);
    add_named_global(jlnewbits_func, &jl_new_bits);

    std::vector<Type *> getnthfld_args(0);
    getnthfld_args.push_back(T_pjlvalue);
    getnthfld_args.push_back(T_size);
    jlgetnthfieldchecked_func =
        Function::Create(FunctionType::get(T_pjlvalue, getnthfld_args, false),
                         Function::ExternalLinkage,
                         "jl_get_nth_field_checked", m);
    add_named_global(jlgetnthfieldchecked_func, *jl_get_nth_field_checked);

    diff_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, false),
                         Function::ExternalLinkage,
                         "jl_gc_diff_total_bytes", m);
    add_named_global(diff_gc_total_bytes_func, *jl_gc_diff_total_bytes);

#if JL_LLVM_VERSION < 30600
    Type *powf_type[2] = { T_float32, T_float32 };
    jlpowf_func = Function::Create(FunctionType::get(T_float32, powf_type, false),
                                   Function::ExternalLinkage,
                                   "powf", m);
    add_named_global(jlpowf_func, &powf, false);

    Type *pow_type[2] = { T_float64, T_float64 };
    jlpow_func = Function::Create(FunctionType::get(T_float64, pow_type, false),
                                  Function::ExternalLinkage,
                                  "pow", m);
    add_named_global(jlpow_func,
#ifdef _COMPILER_MICROSOFT_
        static_cast<double (*)(double, double)>(&pow),
#else
        &pow,
#endif
        false);
#endif
    std::vector<Type*> array_owner_args(0);
    array_owner_args.push_back(T_pjlvalue);
    jlarray_data_owner_func =
        Function::Create(FunctionType::get(T_pjlvalue, array_owner_args, false),
                         Function::ExternalLinkage,
                         "jl_array_data_owner", m);
    jlarray_data_owner_func->setAttributes(
        jlarray_data_owner_func->getAttributes()
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::ReadOnly)
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::NoUnwind));
    add_named_global(jlarray_data_owner_func, jl_array_data_owner);

    gcroot_func =
        Function::Create(FunctionType::get(T_ppjlvalue, false),
                     Function::ExternalLinkage,
                     "julia.gc_root_decl");
    add_named_global(gcroot_func, (void*)NULL, /*dllimport*/false);

    gckill_func =
        Function::Create(FunctionType::get(T_void, ArrayRef<Type*>(T_ppjlvalue), false),
                     Function::ExternalLinkage,
                     "julia.gc_root_kill");
    add_named_global(gckill_func, (void*)NULL, /*dllimport*/false);

    jlcall_frame_func =
        Function::Create(FunctionType::get(T_ppjlvalue, ArrayRef<Type*>(T_int32), false),
                     Function::ExternalLinkage,
                     "julia.jlcall_frame_decl");
    add_named_global(jlcall_frame_func, (void*)NULL, /*dllimport*/false);

    gcroot_flush_func = Function::Create(FunctionType::get(T_void, false),
                                         Function::ExternalLinkage,
                                         "julia.gcroot_flush");
    add_named_global(gcroot_flush_func, (void*)NULL, /*dllimport*/false);

    except_enter_func = Function::Create(FunctionType::get(T_int32, false),
                                         Function::ExternalLinkage,
                                         "julia.except_enter");
    except_enter_func->addFnAttr(Attribute::ReturnsTwice);
    add_named_global(except_enter_func, (void*)NULL, /*dllimport*/false);

    jlgetworld_func =
        Function::Create(FunctionType::get(T_size, ArrayRef<Type*>(), false),
                         Function::ExternalLinkage,
                         "jl_get_world_counter", m);
    jlgetworld_func->addFnAttr(Attribute::ReadOnly);
    add_named_global(jlgetworld_func, jl_get_world_counter);

    // set up optimization passes
#if JL_LLVM_VERSION >= 30700
// No DataLayout pass needed anymore.
#elif JL_LLVM_VERSION >= 30600
    jl_data_layout = new llvm::DataLayoutPass();
#elif JL_LLVM_VERSION >= 30500
    jl_data_layout = new llvm::DataLayoutPass(*jl_ExecutionEngine->getDataLayout());
#else
    jl_data_layout = new DataLayout(*jl_ExecutionEngine->getDataLayout());
#endif

#if JL_LLVM_VERSION >= 30700
    jl_globalPM = new legacy::PassManager();
#else
    jl_globalPM = new PassManager();
#endif
#if JL_LLVM_VERSION < 30700
    jl_globalPM->add(new TargetLibraryInfo(Triple(jl_TargetMachine->getTargetTriple())));
#else
    jl_globalPM->add(new TargetLibraryInfoWrapperPass(Triple(jl_TargetMachine->getTargetTriple())));
#endif
#if JL_LLVM_VERSION < 30700
    jl_globalPM->add(jl_data_layout);
#endif
    addOptimizationPasses(jl_globalPM);
}

static inline std::string getNativeTarget()
{
    std::string cpu = sys::getHostCPUName();
#if defined(_CPU_ARM_)
    // Try slightly harder than LLVM at determining the CPU architecture.
    if (cpu == "generic") {
        // This is the most reliable way I can find
        // `/proc/cpuinfo` changes between kernel versions
        struct utsname name;
        if (uname(&name) >= 0) {
            // name.machine is the elf_platform in the kernel.
            if (strcmp(name.machine, "armv6l") == 0) {
                return "armv6";
            }
            if (strcmp(name.machine, "armv7l") == 0) {
                return "armv7";
            }
            if (strcmp(name.machine, "armv7ml") == 0) {
                // Thumb
                return "armv7-m";
            }
            if (strcmp(name.machine, "armv8l") == 0 ||
                strcmp(name.machine, "aarch64") == 0) {
                return "armv8";
            }
        }
    }
#endif
    return cpu;
}

#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
// Check if the cpu name is a ARM/AArch64 arch name and return a
// string that can be used as LLVM feature name
static inline void checkARMArchFeature(std::string &cpu,
                                       StringMap<bool> &HostFeatures)
{
#if defined(_CPU_ARM_)
    if (cpu == "generic") {
        HostFeatures["neon"] = false;
        return;
    }
#endif
    StringRef cpu_s = cpu;
    if (!cpu_s.startswith("armv"))
        return;
    // Generic names
#if defined(_CPU_ARM_)
    if (!cpu_s.startswith("armv8")) {
        // Turn off `neon` for generic archs on ARM
        // since LLVM seems to enable it for all armv7-a processors.
        HostFeatures["neon"] = false;
    }
    // "v7" and "v8" are not available in the form of `armv*`
    // in the feature list
    if (cpu == "armv7") {
        HostFeatures["v7"] = true;
    }
    else if (cpu == "armv8") {
        HostFeatures["v8"] = true;
    }
    else {
        HostFeatures[cpu] = true;
    }
#else
    HostFeatures[cpu.substr(3)] = true;
#endif
    cpu = "generic";
}
#endif

// Helper to figure out what features to set for the LLVM target
// If the user specifies native (or does not specify) we default
// using the API provided by LLVM
static inline SmallVector<std::string,10> getTargetFeatures(std::string &cpu)
{
    StringMap<bool> HostFeatures;
    if (!strcmp(jl_options.cpu_target,"native")) {
        // On earlier versions of LLVM this is empty
        llvm::sys::getHostCPUFeatures(HostFeatures);
    }

    // Platform specific overides follow
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#ifndef USE_MCJIT
    // Temporarily disable Haswell BMI2 features due to LLVM bug.
    HostFeatures["bmi2"] = false;
    HostFeatures["avx2"] = false;
#endif
#ifdef V128_BUG
    HostFeatures["avx"] = false;
#endif
#endif
#if defined(_CPU_X86_64_) && JL_LLVM_VERSION >= 30600
    // Require cx16 (cmpxchg16b)
    // We need this for 128-bit atomic operations. We only need this
    // when threading is enabled; however, to test whether this
    // excludes important systems, we require this even when threading
    // is disabled.
    HostFeatures["cx16"] = true;
#endif

    // Figure out if we know the cpu_target
    cpu = (strcmp(jl_options.cpu_target,"native") ? jl_options.cpu_target :
           getNativeTarget());
#if defined(_CPU_ARM_)
    // Figure out what we are compiling against from the C defines.
    // This might affect ABI but is fine since
    // 1. We define the C ABI explicitly.
    // 2. This does not change when running the same binary on different
    //    machines.
    // This shouldn't affect making generic binaries since that requires a
    // generic C -march anyway.
    HostFeatures["vfp2"] = true;

    // Arch version
#if __ARM_ARCH >= 8
#  if defined(__ARM_ARCH_PROFILE) && __ARM_ARCH_PROFILE == 'A'
    HostFeatures["armv8-a"] = true;
#  else
    HostFeatures["v8"] = true;
#  endif
#elif __ARM_ARCH >= 7
    // v7 + aclass emits slightly different code than armv7-a
    // In particular LLVM does not use the armv7-a instruction for barrier
    // with v7 + aclass.
#  if defined(__ARM_ARCH_PROFILE) && __ARM_ARCH_PROFILE == 'A'
    HostFeatures["armv7-a"] = true;
#  elif defined(__ARM_ARCH_PROFILE) && __ARM_ARCH_PROFILE == 'R'
    HostFeatures["armv7-r"] = true;
#  elif defined(__ARM_ARCH_PROFILE) && __ARM_ARCH_PROFILE == 'M'
    // Thumb
    HostFeatures["armv7-m"] = true;
#  else
    HostFeatures["v7"] = true;
#  endif
#else
    // minimum requirement
    HostFeatures["v6"] = true;
#endif

    // ARM profile
    // Only do this on ARM and not AArch64 since LLVM aarch64 backend
    // doesn't support setting profiles.
    // AFAIK there's currently no 64bit R and M profile either
    // (v8r and v8m are both 32bit)
#if defined(__ARM_ARCH_PROFILE)
#  if __ARM_ARCH_PROFILE == 'A'
    HostFeatures["aclass"] = true;
#  elif __ARM_ARCH_PROFILE == 'R'
    HostFeatures["rclass"] = true;
#  elif __ARM_ARCH_PROFILE == 'M'
    // Thumb
    HostFeatures["mclass"] = true;
#  endif
#endif
#endif // _CPU_ARM_

    // On ARM and AArch64, allow using cpu_target to specify a CPU architecture
    // which is specified in the feature set in LLVM.
#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
    // Supported ARM arch names on LLVM 3.8:
    //   armv6, armv6-m, armv6j, armv6k, armv6kz, armv6s-m, armv6t2,
    //   armv7, armv7-a, armv7-m, armv7-r, armv7e-m, armv7k, armv7s,
    //   armv8, armv8-a, armv8.1-a, armv8.2-a
    // Additional ARM arch names on LLVM 3.9:
    //   armv8-m.base, armv8-m.main
    //
    // Supported AArch64 arch names on LLVM 3.8:
    //   armv8.1a, armv8.2a
    checkARMArchFeature(cpu, HostFeatures);
#endif

    SmallVector<std::string,10> attr;
    for (auto it = HostFeatures.begin(); it != HostFeatures.end(); it++) {
        if (it->getValue()) {
            attr.append(1, it->getKey().str());
        }
    }
    // Explicitly disabled features need to be added at the end so that
    // they are not reenabled by other features that implies them by default.
    for (auto it = HostFeatures.begin(); it != HostFeatures.end(); it++) {
        if (!it->getValue()) {
            attr.append(1, std::string("-") + it->getKey().str());
        }
    }
    return attr;
}

extern "C" void jl_init_codegen(void)
{
    const char *const argv_tailmerge[] = {"", "-enable-tail-merge=0"}; // NOO TOUCHIE; NO TOUCH! See #922
    cl::ParseCommandLineOptions(sizeof(argv_tailmerge)/sizeof(argv_tailmerge[0]), argv_tailmerge, "disable-tail-merge\n");
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    const char *const argv_copyprop[] = {"", "-disable-copyprop"}; // llvm bug 21743
    cl::ParseCommandLineOptions(sizeof(argv_copyprop)/sizeof(argv_copyprop[0]), argv_copyprop, "disable-copyprop\n");
#endif
#ifdef JL_DEBUG_BUILD
    cl::ParseEnvironmentOptions("Julia", "JULIA_LLVM_ARGS");
#endif

    imaging_mode = jl_generating_output();
    jl_init_debuginfo();
    jl_init_runtime_ccall();

#if JL_LLVM_VERSION < 30400
    // this option disables LLVM's signal handlers
    llvm::DisablePrettyStackTrace = true;
#endif

#ifdef USE_POLLY
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    polly::initializePollyPasses(Registry);
    initializeAnalysis(Registry);
#endif

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetDisassembler();

    Module *m, *engine_module;
    engine_module = new Module("julia", jl_LLVMContext);
#ifdef USE_MCJIT
    m = new Module("julia", jl_LLVMContext);
#else
    m = engine_module;
#endif
    shadow_output = m;

    TargetOptions options = TargetOptions();
    //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#if defined(JL_DEBUG_BUILD) && JL_LLVM_VERSION < 30700
    options.JITEmitDebugInfo = true;
#endif
#if JL_LLVM_VERSION < 30700
    options.NoFramePointerElim = true;
#endif
#if JL_LLVM_VERSION < 30400
    options.NoFramePointerElimNonLeaf = true;
#endif
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to assume the stack is always 16-byte aligned,
    // and to ensure that it is 16-byte aligned for out-going calls,
    // to ensure compatibility with GCC codes
    options.StackAlignmentOverride = 16;
#endif
#if defined(__APPLE__) && JL_LLVM_VERSION < 30400
    // turn on JIT support for libunwind to walk the stack
    options.JITExceptionHandling = 1;
#endif

#if JL_LLVM_VERSION >= 30600
    EngineBuilder eb((std::unique_ptr<Module>(engine_module)));
#else
    EngineBuilder eb(engine_module);
#endif
    std::string ErrorStr;
    eb  .setEngineKind(EngineKind::JIT)
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && !defined(USE_MCJIT)
        .setJITMemoryManager(createJITMemoryManagerWin())
#elif defined(USE_MCJIT)
        .setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>{createRTDyldMemoryManager()})
#endif
        .setTargetOptions(options)
#if (defined(_OS_LINUX_) && defined(_CPU_X86_64_))
        .setRelocationModel(Reloc::PIC_)
#elif JL_LLVM_VERSION < 30900
        .setRelocationModel(Reloc::Default)
#endif
#ifdef _P64
        .setCodeModel(CodeModel::Large)
#else
        .setCodeModel(CodeModel::JITDefault)
#endif
#ifdef DISABLE_OPT
        .setOptLevel(CodeGenOpt::None)
#else
        .setOptLevel(jl_options.opt_level == 0 ? CodeGenOpt::None : CodeGenOpt::Aggressive)
#endif
#if defined(USE_MCJIT) && JL_LLVM_VERSION < 30600
        .setUseMCJIT(true)
#endif
#ifdef USE_ORCMCJIT
        .setUseOrcMCJITReplacement(true)
#endif
    ;
    Triple TheTriple(sys::getProcessTriple());
#if defined(FORCE_ELF)
#if JL_LLVM_VERSION >= 30500
    TheTriple.setObjectFormat(Triple::ELF);
#else
    TheTriple.setEnvironment(Triple::ELF);
#endif
#endif
    std::string TheCPU;
    SmallVector<std::string, 10> targetFeatures = getTargetFeatures(TheCPU);
    jl_TargetMachine = eb.selectTarget(
            TheTriple,
            "",
            TheCPU,
            targetFeatures);
    assert(jl_TargetMachine && "Failed to select target machine -"
                               " Is the LLVM backend for this CPU enabled?");
#if defined(USE_MCJIT) && (!defined(_CPU_ARM_) && !defined(_CPU_PPC64_))
    // FastISel seems to be buggy for ARM. Ref #13321
    if (jl_options.opt_level < 2)
        jl_TargetMachine->setFastISel(true);
#endif

    init_julia_llvm_meta();

#ifdef USE_ORCJIT
    jl_ExecutionEngine = new JuliaOJIT(*jl_TargetMachine);
#else
    jl_ExecutionEngine = eb.create(jl_TargetMachine);
    //jl_printf(JL_STDERR,"%s\n",jl_ExecutionEngine->getDataLayout()->getStringRepresentation().c_str());
    if (!jl_ExecutionEngine) {
        jl_printf(JL_STDERR, "Critical error initializing llvm: %s\n",
                  ErrorStr.c_str());
        exit(1);
    }
#if JL_LLVM_VERSION >= 30500 && !defined(USE_ORCMCJIT)
    jl_ExecutionEngine->setProcessAllSections(true);
#endif
    jl_ExecutionEngine->DisableLazyCompilation();
#endif

    // Now that the execution engine exists, initialize all modules
    jl_setup_module(engine_module);
    jl_setup_module(m);
    init_julia_llvm_env(m);

#ifndef USE_ORCJIT
    jl_ExecutionEngine->RegisterJITEventListener(CreateJuliaJITEventListener());
#ifdef JL_USE_INTEL_JITEVENTS
    if (jl_using_intel_jitevents)
        jl_ExecutionEngine->RegisterJITEventListener(
            JITEventListener::createIntelJITEventListener());
#endif // JL_USE_INTEL_JITEVENTS

#ifdef JL_USE_OPROFILE_JITEVENTS
    if (jl_using_oprofile_jitevents)
        jl_ExecutionEngine->RegisterJITEventListener(
            JITEventListener::createOProfileJITEventListener());
#endif // JL_USE_OPROFILE_JITEVENTS
#endif

    BOX_F(int8,int8);  UBOX_F(uint8,uint8);
    BOX_F(int16,int16); UBOX_F(uint16,uint16);
    BOX_F(int32,int32); UBOX_F(uint32,uint32);
    BOX_F(int64,int64); UBOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);
    UBOX_F(ssavalue,size);

    box8_func  = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int8),
                              "jl_box8", &jl_box8, m);
    box16_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int16),
                              "jl_box16", &jl_box16, m);
    box32_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int32),
                              "jl_box32", &jl_box32, m);
    box64_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int64),
                              "jl_box64", &jl_box64, m);
    jl_init_intrinsic_functions_codegen(m);
}

// for debugging from gdb
extern "C" void jl_dump_llvm_value(void *v)
{
    ((Value*)v)->dump();
}
extern "C" void jl_dump_llvm_type(void *v)
{
    ((Type*)v)->dump(); putchar('\n');
}
