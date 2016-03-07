// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#if defined(_OS_WINDOWS_)
// trick pre-llvm36 into skipping the generation of _chkstk calls
//   since it has some codegen issues associated with them:
//   (a) assumed to be within 32-bit offset
//   (b) bad asm is generated for certain code patterns:
//       see https://github.com/JuliaLang/julia/pull/11644#issuecomment-112276813
// also MCJIT debugging support on windows needs ELF (currently)
#define FORCE_ELF
#endif
#if defined(_CPU_X86_)
#define JL_NEED_FLOATTEMP_VAR 1
#endif

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/IR/IntrinsicInst.h>
#ifdef LLVM38
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/TypeBasedAliasAnalysis.h>
#endif
#ifdef LLVM37
#include "llvm/IR/LegacyPassManager.h"
#else
#include <llvm/PassManager.h>
#endif
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Bitcode/ReaderWriter.h>
#ifdef LLVM37
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Object/SymbolSize.h>
#else
#include <llvm/Target/TargetLibraryInfo.h>
#endif
#ifdef LLVM35
#include <llvm/IR/Verifier.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/AsmParser/Parser.h>
#else
#include <llvm/Assembly/Parser.h>
#include <llvm/Analysis/Verifier.h>
#endif
#include <llvm/DebugInfo/DIContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Value.h>
#ifndef LLVM35
#include <llvm/DebugInfo.h>
#include <llvm/DIBuilder.h>
#endif
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Instrumentation.h>
#include <llvm/Transforms/Vectorize.h>
#ifdef LLVM39
#include <llvm/Transforms/Scalar/GVN.h>
#endif
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Transforms/Utils/Cloning.h>

#if defined(USE_ORCJIT)
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/LazyEmittingLayer.h"
#include "llvm/ExecutionEngine/Orc/ObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/ObjectMemoryBuffer.h"
#elif defined(USE_MCJIT)
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/Object/ObjectFile.h>
#else
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#endif

using namespace llvm;

#if defined(_OS_WINDOWS_) && !defined(NOMINMAX)
#define NOMINMAX
#endif

#include "julia.h"
#include "julia_internal.h"
#include "codegen_internal.h"

#include <setjmp.h>

#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <set>
#include <cstdio>
#include <cassert>

// LLVM version compatibility macros
#ifdef LLVM37
using namespace llvm::legacy;
#define LLVM37_param(x) (x),
#else
#define LLVM37_param(x)
#endif

#ifndef LLVM35
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
JL_DLLEXPORT LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static bool nested_compile = false;
static TargetMachine *jl_TargetMachine;

extern JITEventListener *CreateJuliaJITEventListener();

namespace llvm {
    extern Pass *createLowerSimdLoopPass();
    extern bool annotateSimdLoop(BasicBlock *latch);
}

// for image reloading
static bool imaging_mode = false;

static Module *shadow_output;
#define jl_Module ctx->f->getParent()
#define jl_builderModule builder.GetInsertBlock()->getParent()->getParent()
static MDBuilder *mbuilder;
static std::map<int, std::string> argNumberStrings;
#ifndef USE_ORCJIT
#ifdef LLVM38
static legacy::PassManager *PM;
#else
static PassManager *PM;
#endif
#endif

#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif defined(LLVM35)
static DataLayoutPass *jl_data_layout;
#else
static DataLayout *jl_data_layout;
#endif

// types
static Type *T_jlvalue;
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
static MDNode *tbaa_user;           // User data that is mutable
static MDNode *tbaa_immut;          // User data inside a heap-allocated immutable
static MDNode *tbaa_value;          // Julia value
static MDNode *tbaa_array;              // Julia array
static MDNode *tbaa_arrayptr;               // The pointer inside a jl_array_t
static MDNode *tbaa_arraysize;              // A size in a jl_array_t
static MDNode *tbaa_arraylen;               // The len in a jl_array_t
static MDNode *tbaa_arrayflags;             // The flags in a jl_array_t
static MDNode *tbaa_sveclen;            // The len in a jl_svec_t
static MDNode *tbaa_func;               // A jl_function_t
static MDNode *tbaa_datatype;           // A jl_datatype_t
static MDNode *tbaa_const;          // Memory that is immutable by the time LLVM can see it

// Basic DITypes
#ifdef LLVM37
static DICompositeType *jl_value_dillvmt;
static DIDerivedType *jl_pvalue_dillvmt;
static DIDerivedType *jl_ppvalue_dillvmt;
static DISubroutineType *jl_di_func_sig;
#else
static DICompositeType jl_value_dillvmt;
static DIDerivedType jl_pvalue_dillvmt;
static DIDerivedType jl_ppvalue_dillvmt;
#ifdef LLVM36
DISubroutineType jl_di_func_sig;
#else
DICompositeType jl_di_func_sig;
#endif
#endif

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
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlemptysvec_var;
static GlobalVariable *jlemptytuple_var;
static GlobalVariable *jldiverr_var;
static GlobalVariable *jlundeferr_var;
static GlobalVariable *jldomerr_var;
static GlobalVariable *jlovferr_var;
static GlobalVariable *jlinexacterr_var;
static GlobalVariable *jlRTLD_DEFAULT_var;
#ifdef _OS_WINDOWS_
static GlobalVariable *jlexe_var;
static GlobalVariable *jldll_var;
#if defined(_CPU_X86_64_) && !defined(USE_MCJIT)
extern JITMemoryManager *createJITMemoryManagerWin();
#endif
#endif //_OS_WINDOWS_
#if defined(_OS_DARWIN_) && defined(LLVM37) && defined(LLVM_SHLIB)
#define CUSTOM_MEMORY_MANAGER createRTDyldMemoryManagerOSX
extern RTDyldMemoryManager *createRTDyldMemoryManagerOSX();
#elif defined(_OS_LINUX_) && defined(LLVM37) && defined(JL_UNW_HAS_FORMAT_IP)
#define CUSTOM_MEMORY_MANAGER createRTDyldMemoryManagerUnix
extern RTDyldMemoryManager *createRTDyldMemoryManagerUnix();
#endif

static Function *jltls_states_func;
#ifndef JULIA_ENABLE_THREADING
static GlobalVariable *jltls_states_var;
#else
// Imaging mode only
static GlobalVariable *jltls_states_func_ptr = NULL;
static size_t jltls_states_func_idx = 0;
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
static Function *jlpref_func;
static Function *jlpset_func;
static Function *jltopeval_func;
static Function *jlcopyast_func;
static Function *jltuple_func;
static Function *jlnsvec_func;
static Function *jlapplygeneric_func;
static Function *jlgetfield_func;
static Function *jlmethod_func;
static Function *jlgenericfunction_func;
static Function *jlenter_func;
static Function *jlleave_func;
static Function *jlegal_func;
static Function *jlallocobj_func;
static Function *jlalloc1w_func;
static Function *jlalloc2w_func;
static Function *jlalloc3w_func;
static Function *jl_alloc_svec_func;
static Function *jlsubtype_func;
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
static Function *box_gensym_func;
static Function *box8_func;
static Function *box16_func;
static Function *box32_func;
static Function *box64_func;
static Function *queuerootfun;
static Function *expect_func;
static Function *jldlsym_func;
static Function *jlnewbits_func;
static Function *jltypeassert_func;
#ifndef LLVM36
static Function *jlpow_func;
static Function *jlpowf_func;
#endif
//static Function *jlgetnthfield_func;
static Function *jlgetnthfieldchecked_func;
//static Function *jlsetnthfield_func;
#ifdef _OS_WINDOWS_
static Function *resetstkoflw_func;
#endif
static Function *diff_gc_total_bytes_func;
static Function *jlarray_data_owner_func;

// placeholder functions
static Function *gcroot_func;
static Function *gcstore_func;
static Function *gckill_func;
static Function *jlcall_frame_func;

static std::vector<Type *> two_pvalue_llvmt;
static std::vector<Type *> three_pvalue_llvmt;

static std::map<jl_fptr_t, Function*> builtin_func_map;

// --- code generation ---
extern "C" {
    int globalUnique = 0;
}

#include "jitlayers.cpp"

// metadata tracking for a llvm Value* during codegen
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    Value *gcroot; // the gcroot associated with V (if it has one)
    jl_value_t *typ; // the original type of V, never NULL
    //Type *T; // cached result of julia_type_to_llvm(typ)
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    bool ispointer; // whether this value is actually pointer to the value
    bool isimmutable; // V points to something that is definitely immutable (e.g. single-assignment, but including memory)
    jl_cgval_t(Value *V, Value *gcroot, bool isboxed, jl_value_t *typ) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        constant(NULL),
        gcroot(gcroot),
        typ(typ),
        //T(julia_type_to_llvm(typ)),
        isboxed(isboxed),
        isghost(false),
        ispointer(isboxed),
        isimmutable(isboxed && jl_is_immutable_datatype(typ))
    {
    }
    jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        V(NULL),
        constant(((jl_datatype_t*)typ)->instance),
        gcroot(NULL),
        typ(typ),
        //T(T_void),
        isboxed(false),
        isghost(true),
        ispointer(false),
        isimmutable(true)
    {
        assert(jl_is_datatype(typ));
        assert(constant);
    }
    jl_cgval_t(const jl_cgval_t &v, jl_value_t *typ) : // copy constructor with new type
        V(v.V),
        constant(v.constant),
        gcroot(v.gcroot),
        typ(typ),
        //T(V.T),
        isboxed(v.isboxed),
        isghost(v.isghost),
        ispointer(v.ispointer),
        isimmutable(v.isimmutable)
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
        ispointer(false),
        isimmutable(true)
    {
    }
};

// per-local-variable information
struct jl_varinfo_t {
    Value *memloc; // an address, if the var is in a jl_value_t* gc stack slot or jl_box_t* Box object (marked tbaa_const, if appropriate)
    jl_cgval_t value; // a value, if the var is unboxed or SSA (and thus memloc == NULL)
#ifdef LLVM37
    DILocalVariable *dinfo;
#else
    DIVariable dinfo;
#endif
    bool isAssigned;
    bool isSA;
    bool isVolatile;
    bool isArgument;
    bool escapes;
    bool usedUndef;
    bool used;

    jl_varinfo_t() : memloc(NULL), value(jl_cgval_t()),
#ifdef LLVM37
                     dinfo(NULL),
#else
                     dinfo(DIVariable()),
#endif
                     isAssigned(true), isSA(false),
                     isVolatile(false), isArgument(false),
                     escapes(true), usedUndef(false), used(false)
    {
    }
};

// --- helpers for reloading IR image
static void jl_dump_shadow(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len, bool dump_as_bc);

extern "C"
void jl_dump_bitcode(char *fname, const char *sysimg_data, size_t sysimg_len)
{
    jl_dump_shadow(fname, 0, sysimg_data, sysimg_len, true);
}

extern "C"
void jl_dump_objfile(char *fname, int jit_model, const char *sysimg_data, size_t sysimg_len)
{
    jl_dump_shadow(fname, jit_model, sysimg_data, sysimg_len, false);
}


// aggregate of array metadata
typedef struct {
    Value *dataptr;
    Value *len;
    std::vector<Value*> sizes;
    jl_value_t *ty;
} jl_arrayvar_t;

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    // local var info. globals are not in here.
    std::vector<jl_varinfo_t> slots;
    std::vector<jl_cgval_t> gensym_SAvalues;
    std::vector<bool> gensym_assigned;
    std::map<int, jl_arrayvar_t> *arrayvars;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *handlers;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_lambda_info_t *linfo;
    Value *spvals_ptr;
    Value *argArray;
    Value *argCount;
    std::string funcName;
    int vaSlot;        // name of vararg argument
    bool vaStack;      // varargs stack-allocated
    bool sret;
    int nReqArgs;
    std::vector<bool> boundsCheck;
    std::vector<bool> inbounds;

    CallInst *ptlsStates;

    llvm::DIBuilder *dbuilder;
    bool debug_enabled;
    std::vector<CallInst*> to_inline;
} jl_codectx_t;

typedef struct {
    int64_t isref;
    Function *f;
} cFunction_t;

typedef struct {
    size_t len;
    // cFunction_t data[];
    cFunction_t *data()
    {
        return (cFunction_t*)((char*)this + sizeof(*this));
    }
} cFunctionList_t;

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx);

static Value *emit_local_root(jl_codectx_t *ctx, jl_varinfo_t *vi = NULL);
static void mark_gc_use(const jl_cgval_t &v);
static Value *make_jlcall(ArrayRef<const jl_cgval_t*> args, jl_codectx_t *ctx);
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign, jl_codectx_t *ctx);
static jl_cgval_t emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol=false);
static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx);
static void allocate_gc_frame(BasicBlock *b0, jl_codectx_t *ctx);
static void jl_finalize_module(std::unique_ptr<Module> m);
static GlobalVariable *prepare_global(GlobalVariable *G);

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

static inline jl_cgval_t mark_julia_slot(Value *v, jl_value_t *typ)
{
    // eagerly put this back onto the stack
    assert(v->getType() != T_pjlvalue);
    jl_cgval_t tagval(v, NULL, false, typ);
    tagval.ispointer = true;
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
        assert(v->getType() != T_pjlvalue);
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        Value *loc = emit_static_alloca(T);
        builder.CreateStore(v, loc);
        return mark_julia_slot(loc, typ);
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
    if (jl_is_datatype(jv) || jl_is_uniontype(jv) || jl_is_typector(jv))
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
    if (jl_typeis(jl_exception_in_transit, jl_errorexception_type)) {
        char *str = jl_string_data(jl_fieldref(jl_exception_in_transit,0));
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
    return (ctx->linfo->inferred && !vi.usedUndef &&
            // don't unbox vararg tuples
            s != ctx->vaSlot && store_unboxed_p(vi.value.typ));
}

static jl_sym_t *slot_symbol(int s, jl_codectx_t *ctx)
{
    return (jl_sym_t*)jl_cellref(jl_cellref(jl_lam_vinfo(ctx->ast), s), 0);
}

static Value *alloc_local(int s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->slots[s];
    jl_value_t *jt = vi.value.typ;
    assert(store_unboxed_p(s,ctx));
    Type *vtype = julia_type_to_llvm(jt);
    assert(vtype != T_pjlvalue);
    if (type_is_ghost(vtype)) {
        vi.value = ghostValue(jt);
        return NULL;
    }
    // CreateAlloca is OK here because alloc_local is only called during prologue setup
    Value *lv = builder.CreateAlloca(vtype, 0, jl_symbol_name(slot_symbol(s,ctx)));
    vi.value = mark_julia_slot(lv, jt);
    vi.value.isimmutable &= vi.isSA; // slot is not immutable if there are multiple assignments
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
        for(int i=0; i < ndims-1; i++)
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
static std::unique_ptr<Module> emit_function(jl_lambda_info_t *lam, jl_llvm_functions_t *declarations);
void jl_add_linfo_in_flight(StringRef name, jl_lambda_info_t *linfo, const DataLayout &DL);

// this is the implementation component of jl_compile_linfo
// which compiles li and adds the result to the jitlayers
static void to_function(jl_lambda_info_t *li)
{
    // setup global state
    JL_LOCK(codegen);
    JL_SIGATOMIC_BEGIN();
    assert(!li->inInference);
    li->inCompile = 1;
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    if (!nested_compile && dump_compiles_stream != NULL)
        last_time = jl_hrtime();
    nested_compile = true;
    jl_gc_inhibit_finalizers(nested_compile);
    std::unique_ptr<Module> m;
    Function *f = NULL, *specf = NULL;
    // actually do the work of emitting the function
    JL_TRY {
        m = emit_function(li, &li->functionObjects);
        f = (Function*)li->functionObjects.functionObject;
        specf = (Function*)li->functionObjects.specFunctionObject;
        //n_emit++;
    }
    JL_CATCH {
        // something failed! this is very bad, since other WIP may be pointing to this function
        // but there's not much we can do now. try to clear much of the WIP anyways.
        li->functionObjects.functionObject = NULL;
        li->functionObjects.specFunctionObject = NULL;
        li->functionObjects.cFunctionList = NULL;
        nested_compile = last_n_c;
        if (old != NULL) {
            builder.SetInsertPoint(old);
            builder.SetCurrentDebugLocation(olddl);
        }
        li->inCompile = 0;
        JL_SIGATOMIC_END();
        JL_UNLOCK(codegen);
        jl_rethrow_with_add("error compiling %s", jl_symbol_name(li->name));
    }
    // record that this function name came from this linfo,
    // so we can build a reverse mapping for debug-info.
    if (li->name != anonymous_sym) {
        const DataLayout &DL =
#ifdef LLVM35
            m->getDataLayout();
#else
            *jl_data_layout;
#endif
        // but don't remember anonymous symbols because
        // they may not be rooted in the gc for the life of the program,
        // and the runtime doesn't notify us when the code becomes unreachable :(
        jl_add_linfo_in_flight((specf ? specf : f)->getName(), li, DL);
    }

    // success. add the result to the execution engine now
    jl_finalize_module(std::move(m));
    li->functionID = jl_assign_functionID(f, 0);
    if (specf)
        li->specFunctionID = jl_assign_functionID(specf, 1);
    if (f->getFunctionType() != jl_func_sig)
        // mark the pointer as jl_fptr_sparam_t calling convention
        li->jlcall_api = 1;

    // done compiling: restore global state
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    li->inCompile = 0;
    nested_compile = last_n_c;
    jl_gc_inhibit_finalizers(nested_compile);
    JL_UNLOCK(codegen);
    JL_SIGATOMIC_END();
    if (dump_compiles_stream != NULL) {
        uint64_t this_time = jl_hrtime();
        jl_printf(dump_compiles_stream, "%" PRIu64 "\t\"", this_time - last_time);
        jl_static_show(dump_compiles_stream, (jl_value_t*)li);
        jl_printf(dump_compiles_stream, "\"\n");
        last_time = this_time;
    }
}

#ifndef LLVM37
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

static void jl_setup_module(Module *m)
{
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
#ifdef LLVM34
    if (!getModuleFlag(m,"Debug Info Version"))
        m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
#endif
#ifdef LLVM37
#ifdef USE_ORCJIT
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#elif defined(LLVM38)
    m->setDataLayout(jl_ExecutionEngine->getDataLayout().getStringRepresentation());
#else
    m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
#endif
    m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
#elif defined(LLVM36)
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
}

// this takes ownership of a module after code emission is complete
// and will add it to the execution engine when required (by jl_finalize_function)
static void finalize_gc_frame(Module *m);
static void jl_finalize_module(std::unique_ptr<Module> uniquem)
{
    Module *m = uniquem.release(); // unique_ptr won't be able track what we do with this (the invariant is recovered by jl_finalize_function)
    finalize_gc_frame(m);
#if !defined(USE_ORCJIT)
#ifdef LLVM33
#ifdef JL_DEBUG_BUILD
    if (verifyModule(*m, PrintMessageAction)) {
        m->dump();
        gc_debug_critical_error();
        abort();
    }
#endif
#endif
    PM->run(*m);
#endif
#ifdef USE_MCJIT
    // record the function names that are part of this Module
    // so it can be added to the JIT when needed
    for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
        Function *F = &*I;
        if (!F->isDeclaration())
            module_for_fname[F->getName()] = m;
    }
#endif
    jl_add_to_shadow(m);
}

// this ensures that llvmf has been emitted to the execution engine,
// returning the function pointer to it
static uint64_t getAddressForFunction(llvm::Function *llvmf)
{
#ifdef JL_DEBUG_BUILD
    llvm::raw_fd_ostream out(1,false);
#endif
#ifdef USE_MCJIT
    jl_finalize_function(llvmf, NULL);
    return jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
#else
    return (uint64_t)jl_ExecutionEngine->getPointerToFunction(
            cast<Function>(shadow_output->getNamedValue(llvmf->getName())));
#endif
}

// this assumes that jl_compile_linfo has already been called
// and forces compilation of the lambda info
extern "C" void jl_generate_fptr(jl_lambda_info_t *li)
{
    JL_LOCK(codegen);
    // objective: assign li->fptr
    assert(li->functionObjects.functionObject);
    assert(!li->inCompile);
    if (li->fptr == NULL) {
        JL_SIGATOMIC_BEGIN();
        li->fptr = (jl_fptr_t)getAddressForFunction((Function*)li->functionObjects.functionObject);
        assert(li->fptr != NULL);
        JL_SIGATOMIC_END();
    }
    JL_UNLOCK(codegen);
}

// this generates llvm code for the lambda info
// (and adds it to the shadow module), but doesn't yet compile
// or generate object code for it
extern "C" void jl_compile_linfo(jl_lambda_info_t *li)
{
    if (li->functionObjects.functionObject == NULL) {
        // objective: assign li->functionObject
        to_function(li);
    }
}

// Get the LLVM Function* for the C-callable entry point for a certain function
// and argument types. If rt is NULL then whatever return type is present is
// accepted.
static Function *gen_cfun_wrapper(jl_lambda_info_t *ff, jl_function_t *f, jl_value_t *jlrettype, jl_tupletype_t *argt, int64_t isref);
static Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt)
{
    if (rt) {
        JL_TYPECHK(cfunction, type, rt);
    }
    JL_TYPECHK(cfunction, type, (jl_value_t*)argt);
    if (!jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(f)))
        jl_error("closures are not yet c-callable");

    size_t i, nargs = jl_nparams(argt);
    if (nargs >= 64)
        jl_error("only functions with less than 64 arguments are c-callable");

    uint64_t isref = 0; // bit vector of which argument types are a subtype of Type{Ref{T}}
    jl_value_t *sigt = NULL; // type signature with Ref{} annotations removed
    JL_GC_PUSH1(&sigt);
    sigt = (jl_value_t*)jl_alloc_svec(nargs+1);
    if (jl_is_type(f))
        jl_svecset(sigt, 0, jl_wrap_Type(f));
    else
        jl_svecset(sigt, 0, jl_typeof(f));
    for (i = 0; i < nargs; i++) {
        jl_value_t *ati = jl_tparam(argt, i);
        if (jl_is_abstract_ref_type(ati)) {
            ati = jl_tparam0(ati);
            if (jl_is_typevar(ati))
                jl_error("cfunction: argument type Ref should have an element type, not Ref{T}");
            isref |= (2<<i);
        }
        else if (ati != (jl_value_t*)jl_any_type && !jl_is_leaf_type(ati)) {
            jl_error("cfunction: type signature must only contain leaf types");
        }
        jl_svecset(sigt, i+1, ati);
    }
    sigt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)sigt);

    if (rt != NULL) {
        if (jl_is_abstract_ref_type(rt)) {
            rt = jl_tparam0(rt);
            if (jl_is_typevar(rt))
                jl_error("cfunction: return type Ref should have an element type, not Ref{T}");
            if (rt == (jl_value_t*)jl_any_type)
                jl_error("cfunction: return type Ref{Any} is invalid. Use Any or Ptr{Any} instead.");
            isref |= 1;
        }
        else if (!jl_is_leaf_type(rt)) {
            isref |= 1;
        }
    }

    jl_lambda_info_t *li = jl_get_specialization1((jl_tupletype_t*)sigt);
    if (li != NULL) {
        for(i=1; i < nargs+1; i++) {
            jl_value_t *speci = jl_nth_slot_type(li->specTypes, i);
            jl_value_t *sigi = jl_nth_slot_type((jl_tupletype_t*)sigt, i);
            if ((isref & (2<<(i-1))) && speci == (jl_value_t*)jl_any_type) {
                // specialized for Any => can accept any Ref
            }
            else if (!jl_types_equal(speci, sigi)) {
                jl_errorf("cfunction: type signature of %s does not match specification",
                          jl_symbol_name(li->name));
            }
        }
        jl_value_t *astrt = li->rettype;
        if (rt != NULL) {
            if (astrt == (jl_value_t*)jl_bottom_type) {
                if (rt != (jl_value_t*)jl_void_type) {
                    // a function that doesn't return can be passed to C as void
                    jl_errorf("cfunction: %s does not return",
                              jl_symbol_name(li->name));
                }
            }
            else if (!jl_subtype(astrt, rt, 0)) {
                jl_errorf("cfunction: return type of %s does not match",
                          jl_symbol_name(li->name));
            }
        }
        JL_GC_POP(); // kill list: sigt
        return gen_cfun_wrapper(li, f, astrt, argt, isref);
    }
    jl_error("cfunction: no method exactly matched the required type signature (function not yet c-callable)");
}

// get the address of a C-callable entry point for a function
extern "C" JL_DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_GC_PUSH1(&argt);
    if (jl_is_tuple(argt)) {
        // TODO: maybe deprecation warning, better checking
        argt = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argt), jl_nfields(argt));
    }
    assert(jl_is_tuple_type(argt));
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    JL_GC_POP();
    return (void*)getAddressForFunction(llvmf);
}


// convenience function for debugging from gdb (pre-OrcJIT)
// it generally helps to have define KEEP_BODIES if you plan on using this
extern "C" JL_DLLEXPORT
void *jl_function_ptr_by_llvm_name(char *name) {
#ifdef __has_feature
#if __has_feature(memory_sanitizer)
    __msan_unpoison_string(name);
#endif
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
        uint64_t Addr = getAddressForFunction(llvmf); (void)Addr;
        // emit the function pointer and set up an alias in the execution engine
#if defined(USE_ORCJIT) || defined(USE_MCJIT)
        jl_ExecutionEngine->addGlobalMapping(name, Addr);
        if (imaging_mode)
             // in the old JIT, the shadow_module aliases the engine_module,
             // otherwise, adding it as a global mapping is needed unconditionally
#endif
        {
            llvmf = cast<Function>(shadow_output->getNamedValue(llvmf->getName()));
            // in imaging_mode, also need to add the alias to the shadow_module
#if defined(LLVM38)
            GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#elif defined(LLVM37)
            GlobalAlias::create(cast<PointerType>(llvmf->getType()),
                                GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#else
            new GlobalAlias(llvmf->getType(), GlobalValue::ExternalLinkage, name, llvmf, shadow_output);
#endif
        }
    }
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir and jl_dump_function_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT
void *jl_get_llvmf(jl_function_t *f, jl_tupletype_t *tt, bool getwrapper, bool getdeclarations)
{
    jl_lambda_info_t *linfo = NULL;
    JL_GC_PUSH2(&linfo, &tt);
    if (tt != NULL) {
        linfo = jl_get_specialization1(tt);
        if (linfo == NULL) {
            linfo = jl_method_lookup_by_type(jl_gf_mtable(f), tt, 0, 0);
            if (linfo == NULL) {
                JL_GC_POP();
                return NULL;
            }
        }
    }
    if (linfo == NULL) {
        JL_GC_POP();
        return NULL;
    }
    if (!linfo->specTypes) {
        jl_printf(JL_STDERR, "WARNING: Returned code may not match what actually runs.\n");
        linfo = jl_get_unspecialized(linfo);
    }

    if (!getdeclarations) {
        // emit this function into a new module
        Function *f, *specf;
        jl_llvm_functions_t declarations;
        std::unique_ptr<Module> m = emit_function(linfo, &declarations);
        f = (llvm::Function*)declarations.functionObject;
        specf = (llvm::Function*)declarations.specFunctionObject;
        // swap declarations for definitions and destroy declarations
        if (specf) {
            Function *temp = cast<Function>(m->getNamedValue(specf->getName()));
            delete specf;
            specf = temp;
        }
        if (f) {
            Function *temp = cast<Function>(m->getNamedValue(f->getName()));
            delete f;
            f = temp;
        }
        Function *specf_decl = (Function*)linfo->functionObjects.specFunctionObject;
        if (specf_decl) {
            specf->setName(specf_decl->getName());
        }
        Function *f_decl = (Function*)linfo->functionObjects.functionObject;
        if (f_decl) {
            f->setName(f_decl->getName());
        }
        finalize_gc_frame(m.release()); // the return object `llvmf` will be the owning pointer
        JL_GC_POP();
        if (getwrapper || !specf) {
            return f;
        }
        else {
            return specf;
        }
    }
    jl_compile_linfo(linfo);
    Function *llvmf;
    if (!getwrapper && linfo->functionObjects.specFunctionObject != NULL) {
        llvmf = (Function*)linfo->functionObjects.specFunctionObject;
    }
    else {
        llvmf = (Function*)linfo->functionObjects.functionObject;
    }
    JL_GC_POP();
    return llvmf;
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

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// This isn't particularly fast, but it's only used for interactive mode
static uint64_t compute_obj_symsize(const object::ObjectFile *obj, uint64_t offset)
{
    // Scan the object file for the closest symbols above and below offset in the .text section
    uint64_t lo = 0;
    uint64_t hi = 0;
    bool setlo = false;
#ifdef LLVM37
    for (const object::SectionRef &Section : obj->sections()) {
#else
    llvm::error_code err;
    for (object::section_iterator I = obj->begin_sections(), E = obj->end_sections();
            !err && I != E; I.increment(err)) {
        object::SectionRef Section = *I;
#endif
        uint64_t SAddr, SSize;
#ifdef LLVM36
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
#ifdef LLVM37
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
#ifdef LLVM38
            Sect = Sym.getSection().get();
#else
            if (Sym.getSection(Sect)) continue;
#endif
            if (Sect == ESection) continue;
            if (Sect != Section) continue;
#ifdef LLVM37
            Addr = Sym.getAddress().get();
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
const jl_value_t *jl_dump_function_asm(void *f, int raw_mc)
{
    std::string code;
    llvm::raw_string_ostream stream(code);
#ifndef LLVM37
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
        fptr = (uintptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(
            jl_ExecutionEngine->getMangledName(llvmf));
    llvm::DIContext *context = NULL;
    llvm::DIContext *&objcontext = context;
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> context;
    llvm::DIContext *objcontext = NULL;
#endif
    const object::ObjectFile *object = NULL;
    assert(fptr != 0);
    bool isJIT = true;
    if (!jl_DI_for_fptr(fptr, &symsize, &slide, &section_slide, &object, &context)) {
        isJIT = false;
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
#ifdef LLVM37
        jl_cleanup_DI(context);
#endif
        return (jl_value_t*)jl_pchar_to_array((char*)fptr, symsize);
    }

    jl_dump_asm_internal(fptr, symsize, slide,
#ifndef USE_MCJIT
            context,
#endif
            objcontext,
#ifdef LLVM37
            stream
#else
            fstream
#endif
            );

#ifdef LLVM37
    if (isJIT)
        jl_cleanup_DI(context);
#else
    fstream.flush();
#endif

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// Code coverage

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< std::vector<logdata_block*> > logdata_t;
static logdata_t coverageData;

static void coverageVisitLine(StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || line < 0)
        return;
    std::vector<logdata_block*> &vec = coverageData[filename];
    int block = line / logdata_blocksize;
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
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true, "lcnt"),
                                          ConstantInt::get(T_int64, 1)),
                        v, true); // not atomic, so this might be an underestimate,
                                  // but it's faster this way
}


// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || line < 0) {
        jl_gc_sync_total_bytes();
        return;
    }
    std::vector<logdata_block*> &vec = mallocData[filename];
    int block = line / logdata_blocksize;
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
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true, "bytecnt"),
                                          builder.CreateCall(prepare_call(diff_gc_total_bytes_func)
#ifdef LLVM37
                                            , {}
#endif
                                              )),
                        v, true); // not atomic, so this might be an underestimate,
                                  // but it's faster this way
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
                for (size_t i = 0; i < logdata_blocksize; i++) {
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
                int block = 0;
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


// --- code gen for intrinsic functions ---

#include "intrinsics.cpp"

// --- constant determination ---

static void show_source_loc(JL_STREAM *out, jl_codectx_t *ctx)
{
    if (ctx == NULL) return;
    jl_printf(out, "in %s at %s", jl_symbol_name(ctx->linfo->name),
              jl_symbol_name(ctx->linfo->file));
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
extern "C"
jl_value_t *jl_static_eval(jl_value_t *ex, void *ctx_, jl_module_t *mod,
                           jl_lambda_info_t *linfo, int sparams, int allow_alloc)
{
    jl_codectx_t *ctx = (jl_codectx_t*)ctx_;
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (jl_is_const(mod, sym))
            return jl_get_global(mod, sym);
        return NULL;
    }
    if (jl_typeis(ex,jl_slot_type))
        return NULL;
    if (jl_is_gensym(ex)) {
        ssize_t idx = ((jl_gensym_t*)ex)->id;
        assert(idx >= 0);
        if (ctx != NULL && ctx->gensym_assigned.at(idx)) {
            return ctx->gensym_SAvalues.at(idx).constant;
        }
        return NULL;
    }
    if (jl_is_topnode(ex)) {
        jl_binding_t *b = jl_get_binding(jl_base_relative_to(mod),
                                         (jl_sym_t*)jl_fieldref(ex,0));
        if (b == NULL) return NULL;
        if (b->constp)
            return b->value;
        return NULL;
    }
    if (jl_is_quotenode(ex))
        return jl_fieldref(ex,0);
    if (jl_is_lambda_info(ex))
        return NULL;
    jl_module_t *m = NULL;
    jl_sym_t *s = NULL;
    if (jl_is_globalref(ex)) {
        s = (jl_sym_t*)jl_globalref_name(ex);
        if (s && jl_is_symbol(s)) {
            jl_binding_t *b = jl_get_binding(jl_globalref_mod(ex), s);
            if (b && b->constp) {
                if (b->deprecated) cg_bdw(b, ctx);
                return b->value;
            }
        }
        return NULL;
    }
    if (jl_is_expr(ex)) {
        jl_expr_t *e = (jl_expr_t*)ex;
        if (e->head == call_sym) {
            jl_value_t *f = jl_static_eval(jl_exprarg(e,0),ctx,mod,linfo,sparams,allow_alloc);
            if (f) {
                if (jl_array_dim0(e->args) == 3 && f==jl_builtin_getfield) {
                    m = (jl_module_t*)jl_static_eval(jl_exprarg(e,1),ctx,mod,linfo,sparams,allow_alloc);
                    s = (jl_sym_t*)jl_static_eval(jl_exprarg(e,2),ctx,mod,linfo,sparams,allow_alloc);
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
                        v[i] = jl_static_eval(jl_exprarg(e,i+1),ctx,mod,linfo,sparams,allow_alloc);
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
        return NULL;
    }
    return ex;
}

static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, bool sparams,
                               bool allow_alloc)
{
    return jl_static_eval(ex, ctx, ctx->module, ctx->linfo, sparams, allow_alloc);
}

static bool is_constant(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true)
{
    return static_eval(ex,ctx,sparams) != NULL;
}

static bool slot_eq(jl_value_t *e, int sl)
{
    return jl_is_slot(e) && jl_slot_number(e)-1 == sl;
}

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
        jl_value_t *st = jl_cellref(stmts,i);
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
        jl_value_t *st = jl_cellref(stmts,i);
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
                            if (local_var_occurs(jl_cellref(stmts,j), *it)) {
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
    return (jl_is_symbol(e) || jl_is_topnode(e) || jl_is_globalref(e));
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
                if (is_constant(f, ctx, false)) {
                    jl_value_t *fv = jl_interpret_toplevel_expr_in(ctx->module, f, NULL);
                    if (jl_typeis(fv, jl_intrinsic_type)) {
                        esc = false;
                        JL_I::intrinsic fi = (JL_I::intrinsic)jl_unbox_int32(fv);
                        if (fi == JL_I::ccall) {
                            esc = true;
                            simple_escape_analysis(jl_exprarg(e,1), esc, ctx);
                            // 2nd and 3d arguments are static
                            for(i=4; i < (size_t)alen; i+=2) {
                                simple_escape_analysis(jl_exprarg(e,i), esc, ctx);
                            }
                            return;
                        }
                    }
                    else {
                        if ((fv==jl_builtin_getfield && alen==3 &&
                             expr_type(jl_exprarg(e,2),ctx) == (jl_value_t*)jl_long_type) ||
                            fv==jl_builtin_nfields /*||   // TODO jb/functions
                            (fv==jl_builtin__apply && alen==3 &&
                            expr_type(jl_exprarg(e,2),ctx) == (jl_value_t*)jl_function_type)*/) {
                            esc = false;
                        }
                    }
                }
            }

            for(i=1; i < (size_t)alen; i++) {
                simple_escape_analysis(jl_exprarg(e,i), esc, ctx);
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

static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val)
{
    if (jl_is_leaf_type(val))
        return;
    JL_GC_PUSH1(&val);
    li = li->def;
    if (li->roots == NULL) {
        li->roots = jl_alloc_cell_1d(1);
        jl_gc_wb(li, li->roots);
        jl_cellset(li->roots, 0, val);
    }
    else {
        size_t rlen = jl_array_dim0(li->roots);
        for(size_t i=0; i < rlen; i++) {
            if (jl_cellref(li->roots,i) == val) {
                JL_GC_POP();
                return;
            }
        }
        jl_cell_1d_push(li->roots, val);
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
            return mark_julia_type(builder.CreateLoad(bp), true, (jl_value_t*)jl_any_type, ctx);
        }
        // todo: use type info to avoid undef check
        return emit_checked_var(bp, name, ctx);
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
#ifdef LLVM37
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
        Value *varg1 = emit_unbox(at, arg1, arg1.typ);
        Value *varg2 = emit_unbox(at, arg2, arg2.typ);
        return builder.CreateICmpEQ(JL_INT(varg1),JL_INT(varg2));
    }

    if (at->isVectorTy()) {
        jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
        Value *answer = ConstantInt::get(T_int1, 1);
        Value *varg1 = emit_unbox(at, arg1, arg1.typ);
        Value *varg2 = emit_unbox(at, arg2, arg2.typ);
        size_t l = jl_svec_len(types);
        for(unsigned i=0; i < l; i++) {
            jl_value_t *fldty = jl_svecref(types,i);
            Value *subAns, *fld1, *fld2;
            fld1 = builder.CreateExtractElement(varg1, ConstantInt::get(T_int32,i)),
            fld2 = builder.CreateExtractElement(varg2, ConstantInt::get(T_int32,i)),
            subAns = emit_bits_compare(mark_julia_type(fld1, false, fldty, ctx), mark_julia_type(fld2, false, fldty, ctx), ctx);
            answer = builder.CreateAnd(answer, subAns);
        }
        return answer;
    }

    if (at->isAggregateType()) { // Struct or Array
        assert(arg1.ispointer && arg2.ispointer);
        size_t sz = jl_datatype_size(arg1.typ);
        if (sz > 512 && !((jl_datatype_t*)arg1.typ)->haspadding) {
#ifdef LLVM37
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
                subAns = emit_bits_compare(mark_julia_slot(fld1, fldty), mark_julia_slot(fld2, fldty), ctx);
                answer = builder.CreateAnd(answer, subAns);
            }
            return answer;
        }
    }
    assert(0 && "what is this llvm type?");
    return 0;
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
    if (jl_subtype(rt1, (jl_value_t*)jl_type_type, 0) ||
        jl_subtype(rt2, (jl_value_t*)jl_type_type, 0)) // use typeseq for datatypes
        ptr_comparable = 0;
    if ((jl_is_type_type(rt1) && jl_is_leaf_type(jl_tparam0(rt1))) ||
        (jl_is_type_type(rt2) && jl_is_leaf_type(jl_tparam0(rt2)))) // can compare leaf types by pointer
        ptr_comparable = 1;
    if (ptr_comparable) {
        assert(arg1.isboxed && arg2.isboxed); // only boxed types are valid for pointer comparison
        return builder.CreateICmpEQ(boxed(arg1, ctx), boxed(arg2, ctx));
    }

    Value *varg1 = boxed(arg1, ctx);
    Value *varg2 = boxed(arg2, ctx, false); // potentially unrooted!
#ifdef LLVM37
    return builder.CreateTrunc(builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
#else
    return builder.CreateTrunc(builder.CreateCall2(prepare_call(jlegal_func), varg1, varg2), T_int1);
#endif
}

static jl_svec_t *call_arg_types(jl_value_t **args, size_t n, jl_codectx_t *ctx)
{
    jl_svec_t *t = jl_alloc_svec(n);
    JL_GC_PUSH1(&t);
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *ty = expr_type(args[i], ctx);
        if (!jl_is_leaf_type(ty)) {
            t = NULL;
            break;
        }
        jl_svecset(t, i, ty);
    }
    JL_GC_POP();
    return t;
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
                *ret = mark_julia_type(ConstantInt::get(T_int1, jl_egal(rt1, rt2)), false, jl_bool_type, ctx);
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
        *ret = mark_julia_type(ans, false, jl_bool_type, ctx);
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
            if (jl_subtype(arg, tp0, 0)) {
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
        if (jl_subtype(ty, (jl_value_t*)jl_type_type, 0)) {
            *ret = emit_expr(args[1], ctx);
#ifdef LLVM37
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
        if (jl_is_type_type(ty) && !jl_has_typevars(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0, 0)) {
                emit_expr(args[1], ctx);  // TODO remove if no side effects
                *ret = mark_julia_type(ConstantInt::get(T_int1, 1), false, jl_bool_type, ctx);
                JL_GC_POP();
                return true;
            }
            if (!jl_subtype(tp0, (jl_value_t*)jl_type_type, 0)) {
                if (jl_is_leaf_type(arg)) {
                    emit_expr(args[1], ctx);  // TODO remove if no side effects
                    *ret = mark_julia_type(ConstantInt::get(T_int1, 0), false, jl_bool_type, ctx);
                    JL_GC_POP();
                    return true;
                }
                if (jl_is_leaf_type(tp0)) {
                    jl_cgval_t arg1 = emit_expr(args[1], ctx);
                    *ret = mark_julia_type(
                            builder.CreateICmpEQ(emit_typeof_boxed(arg1,ctx),
                                                 literal_pointer_val(tp0)),
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
            int issub = jl_subtype(jl_tparam0(rt1), jl_tparam0(rt2), 0);
            // TODO: emit args[1] and args[2] in case of side effects?
            *ret = mark_julia_type(ConstantInt::get(T_int1, issub), false, jl_bool_type, ctx);
            JL_GC_POP();
            return true;
        }
    }

    // TODO jb/functions
    /*
    else if (f==jl_builtin__apply && nargs==2 && ctx->vaStack &&
             symbol_eq(args[2], ctx->vaName)) {
        // turn Core._apply(f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the vaStack allocation
        Value *theF = boxed(emit_expr(args[1], ctx), ctx);
        Value *nva = emit_n_varargs(ctx);
#ifdef _P64
        nva = builder.CreateTrunc(nva, T_int32);
#endif
        Value *r =
#ifdef LLVM37
            builder.CreateCall(prepare_call(theFptr), {theF,
                                builder.CreateGEP(ctx->argArray,
                                                  ConstantInt::get(T_size, ctx->nReqArgs)),
                                nva});
#else
            builder.CreateCall3(prepare_call(theFptr), theF,
                                builder.CreateGEP(ctx->argArray,
                                                  ConstantInt::get(T_size, ctx->nReqArgs)),
                                nva);
#endif
        *ret = mark_julia_type(r, true, expr_type(expr, ctx), ctx);
        JL_GC_POP();
        return true;
    }
    */

    else if (f==jl_builtin_tuple) {
        if (nargs == 0) {
            *ret = ghostValue(jl_typeof(jl_emptytuple));
            JL_GC_POP();
            return true;
        }
        if (ctx->linfo->inferred) {
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
        // emit a "conditional" throw so that codegen does't end up trying to emit code after an "unreachable" terminator
        raise_exception_unless(ConstantInt::get(T_int1,0), arg1, ctx);
        *ret = jl_cgval_t();
        JL_GC_POP();
        return true;
    }

    else if (f==jl_builtin_arraysize && nargs==2) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
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
                    BasicBlock *outBB = BasicBlock::Create(getGlobalContext(),"outofrange",ctx->f);
                    BasicBlock *inBB = BasicBlock::Create(getGlobalContext(),"inrange");
                    BasicBlock *ansBB = BasicBlock::Create(getGlobalContext(),"arraysize");
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
                indexes_ok = false; break;
            }
        }
        if (jl_is_array_type(aty) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty);
            if (!jl_is_typevar(ety)) {
                if (!jl_array_store_unboxed(ety))
                    ety = (jl_value_t*)jl_any_type;
                jl_value_t *ndp = jl_tparam1(aty);
                if (jl_is_long(ndp) || nargs==2) {
                    jl_cgval_t ary = emit_expr(args[1], ctx);
                    size_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : 1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[2], nargs-1, ctx);
                    if (jl_array_store_unboxed(ety) &&
                        ((jl_datatype_t*)ety)->size == 0) {
                        assert(jl_is_datatype(ety));
                        assert(((jl_datatype_t*)ety)->instance != NULL);
                        *ret = ghostValue(ety);
                    }
                    else {
                        *ret = typed_load(emit_arrayptr(ary, args[1], ctx), idx, ety, ctx, tbaa_user);
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
                indexes_ok = false; break;
            }
        }
        if (jl_is_array_type(aty) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty);
            if (!jl_is_typevar(ety) && jl_subtype(vty, ety, 0)) {
                if (!jl_array_store_unboxed(ety))
                    ety = (jl_value_t*)jl_any_type;
                jl_value_t *ndp = jl_tparam1(aty);
                if (jl_is_long(ndp) || nargs==3) {
                    jl_cgval_t ary = emit_expr(args[1], ctx);
                    size_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : 1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[3], nargs-2, ctx);
                    bool isboxed = !jl_array_store_unboxed(ety);
                    if (!isboxed && ((jl_datatype_t*)ety)->size == 0) {
                        // no-op, but emit expr for possible effects
                        assert(jl_is_datatype(ety));
                        emit_expr(args[2], ctx);
                    }
                    else {
                        jl_cgval_t v = (ety == (jl_value_t*)jl_any_type ? emit_expr(args[2], ctx) : emit_expr(args[2], ctx));
                        PHINode *data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed) { // if not boxed we don't need a write barrier
                            assert(ary.isboxed);
                            Value *aryv = boxed(ary, ctx);
                            Value *flags = emit_arrayflags(ary, ctx);
                            // the owner of the data is ary itself except if ary->how == 3
                            flags = builder.CreateAnd(flags, 3);
                            Value *is_owned = builder.CreateICmpEQ(flags, ConstantInt::get(T_int16, 3));
                            BasicBlock *curBB = builder.GetInsertBlock();
                            BasicBlock *ownedBB = BasicBlock::Create(getGlobalContext(), "array_owned", ctx->f);
                            BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(), "merge_own", ctx->f);
                            builder.CreateCondBr(is_owned, ownedBB, mergeBB);
                            builder.SetInsertPoint(ownedBB);
                            // load owner pointer
                            Value *own_ptr;
                            if (jl_is_long(ndp)) {
                                own_ptr = builder.CreateLoad(
                                    builder.CreateBitCast(
                                        builder.CreateConstGEP1_32(
                                            builder.CreateBitCast(aryv, T_pint8),
                                            jl_array_data_owner_offset(nd)),
                                        T_ppjlvalue));
                            }
                            else {
#ifdef LLVM37
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
                                    ety, ctx, tbaa_user, data_owner);
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
                    tbaa_decorate(tbaa_user, builder.CreateLoad(builder.CreateGEP(ctx->argArray, idx))),
                    /*boxed*/ true,
                    expr_type(expr, ctx),
                    ctx,
                    /*needsgcroot*/ false);
            JL_GC_POP();
            return true;
        }

        if (fldt == (jl_value_t*)jl_long_type && jl_is_leaf_type((jl_value_t*)stt)) {
            if ((jl_is_structtype(stt) || jl_is_tuple_type(stt)) && !jl_subtype((jl_value_t*)jl_module_type, (jl_value_t*)stt, 0)) {
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
                if (jl_is_leaf_type((jl_value_t*)sty) && jl_subtype(rhst, ft, 0)) {
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
        if (ctx->vaStack && slot_eq(args[1], ctx->vaSlot) && !ctx->slots[ctx->vaSlot].isAssigned) {
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
                Value *fieldtyp = builder.CreateLoad(builder.CreateGEP(builder.CreateBitCast(types_svec, T_ppjlvalue), idx));
                *ret = mark_julia_type(fieldtyp, true, expr_type(expr, ctx), ctx);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f==jl_builtin_sizeof && nargs == 1) {
        jl_datatype_t *sty = (jl_datatype_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_type_type((jl_value_t*)sty) && !jl_is_typevar(jl_tparam0(sty))) {
            sty = (jl_datatype_t*)jl_tparam0(sty);
        }
        if (jl_is_datatype(sty) && sty != jl_symbol_type && sty->name != jl_array_typename &&
            sty != jl_simplevector_type &&
            // exclude DataType, since each DataType has its own size, not sizeof(DataType).
            // this is issue #8798
            sty != jl_datatype_type) {
            if (jl_is_leaf_type((jl_value_t*)sty) ||
                (sty->name->names == jl_emptysvec && sty->size > 0)) {
                *ret = mark_julia_type(ConstantInt::get(T_size, sty->size), false, jl_long_type, ctx);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f==jl_builtin_apply_type && nargs > 0) {
        size_t i;
        for(i=1; i <= nargs; i++) {
            if (!is_constant(args[i], ctx))
                break;
        }
        if (i > nargs) {
            jl_value_t *ty = static_eval(expr, ctx, true, true);
            if (ty!=NULL && jl_is_leaf_type(ty)) {
                if (jl_has_typevars(ty)) {
                    // add root for types not cached. issue #7065
                    jl_add_linfo_root(ctx->linfo, ty);
                }
                *ret = mark_julia_const(ty);
                JL_GC_POP();
                return true;
            }
        }
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
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                       ConstantInt::get(T_int32,nargs)});
#else
    Value *result = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
#endif
    return result;
}

static jl_cgval_t emit_call_function_object(jl_lambda_info_t *li, const jl_cgval_t &theF, Value *theFptr,
                                            jl_value_t **args, size_t nargs, jl_value_t *callexpr, jl_codectx_t *ctx)
{
    if (li->functionObjects.specFunctionObject != NULL) {
        // emit specialized call site
        jl_value_t *jlretty = li->rettype;
        bool retboxed;
        (void)julia_type_to_llvm(jlretty, &retboxed);
        Function *cf = cast<Function>(prepare_call((Function*)li->functionObjects.specFunctionObject));
        FunctionType *cft = cf->getFunctionType();
        size_t nfargs = cft->getNumParams();
        Value **argvals = (Value**) alloca(nfargs*sizeof(Value*));
        bool sret = cf->hasStructRetAttr();
        unsigned idx = 0;
        Value *result;
        if (sret) {
            result = emit_static_alloca(cft->getParamType(0)->getContainedType(0), ctx);
            argvals[idx] = result;
            idx++;
        }
        for(size_t i=0; i < nargs+1; i++) {
            Type *at = cft->getParamType(idx);
            jl_value_t *jt = jl_nth_slot_type(li->specTypes,i);
            bool isboxed;
            Type *et = julia_type_to_llvm(jt, &isboxed);
            if (type_is_ghost(et)) {
                // Still emit the expression in case it has side effects
                if (i>0) emit_expr(args[i], ctx);
                continue;
            }
            if (isboxed) {
                assert(at == T_pjlvalue && et == T_pjlvalue);
                jl_cgval_t origval = i==0 ? theF : emit_expr(args[i], ctx);
                argvals[idx] = boxed(origval, ctx);
                assert(!isa<UndefValue>(argvals[idx]));
            }
            else if (et->isAggregateType()) {
                assert(at == PointerType::get(et, 0));
                jl_cgval_t arg = i==0 ? theF : emit_expr(args[i], ctx);
                if (arg.ispointer) {
                    // can lazy load on demand, no copy needed
                    argvals[idx] = data_pointer(arg, ctx, at);
                    mark_gc_use(arg); // TODO: must be after the jlcall
                }
                else {
                    Value *v = emit_unbox(et, arg, jt);
                    Value *p = emit_static_alloca(v->getType(), ctx);
                    builder.CreateStore(v, p);
                    argvals[idx] = p;
                }
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
        CallInst *call = builder.CreateCall(prepare_call(cf), ArrayRef<Value*>(&argvals[0], nfargs));
        call->setAttributes(cf->getAttributes());
        return sret ? mark_julia_slot(result, jlretty) : mark_julia_type(call, retboxed, jlretty, ctx);
    }
    return mark_julia_type(emit_jlcall(theFptr, boxed(theF,ctx), &args[1], nargs, ctx), true,
                           expr_type(callexpr, ctx), ctx);
}

static jl_cgval_t emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx, jl_value_t *expr)
{
    size_t nargs = arglen-1;
    Value *theFptr = NULL;
    jl_cgval_t result;
    jl_value_t *aty = NULL;

    jl_function_t *f = (jl_function_t*)static_eval(args[0], ctx, true);
    JL_GC_PUSH2(&f, &aty);
    if (f != NULL) {
        // function is a compile-time constant
        if (jl_typeis(f, jl_intrinsic_type)) {
            result = emit_intrinsic((intrinsic)*(uint32_t*)jl_data_ptr(f), args, nargs, ctx);
            if (result.typ == (jl_value_t*)jl_any_type) // the select_value intrinsic may be missing type information
                result = remark_julia_type(result, expr_type(expr, ctx));
            JL_GC_POP();
            return result;
        }
        if (jl_subtype(f, (jl_value_t*)jl_builtin_type, 1)) {
            bool handled = emit_builtin_call(&result, (jl_value_t*)f, args, nargs, ctx, expr);
            if (handled) {
                JL_GC_POP();
                return result;
            }
        }
    }

    // special case for known builtin not handled by emit_builtin_call
    if (f && jl_subtype(f, (jl_value_t*)jl_builtin_type, 1)) {
        std::map<jl_fptr_t,Function*>::iterator it = builtin_func_map.find(jl_get_builtin_fptr(f));
        if (it != builtin_func_map.end()) {
            theFptr = (*it).second;
            result = mark_julia_type(emit_jlcall(theFptr, V_null, &args[1], nargs, ctx), true, expr_type(expr,ctx), ctx);
            JL_GC_POP();
            return result;
        }
    }

    if (ctx->linfo->inferred) {
        aty = (jl_value_t*)call_arg_types(args, arglen, ctx);
        // attempt compile-time specialization for inferred types
        if (aty != NULL) {
            aty = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)aty);
            /*if (trace) {
                  jl_printf(JL_STDOUT, "call %s%s\n",
                  jl_sprint(args[0]),
                  jl_sprint((jl_value_t*)aty));
              }*/
            jl_lambda_info_t *li = jl_get_specialization1((jl_tupletype_t*)aty);
            if (li != NULL) {
                assert(li->functionObjects.functionObject != NULL);
                theFptr = (Value*)li->functionObjects.functionObject;
                jl_cgval_t fval;
                if (f != NULL) {
                    // TODO jb/functions: avoid making too many roots here
                    if (!jl_is_globalref(args[0]) && !jl_is_symbol(args[0]) &&
                        !jl_is_leaf_type(f)) {
                        jl_add_linfo_root(ctx->linfo, f);
                    }
                    fval = mark_julia_const((jl_value_t*)f);
                }
                else {
                    fval = emit_expr(args[0], ctx);
                }
                jl_add_linfo_root(ctx->linfo, (jl_value_t*)li);
                result = emit_call_function_object(li, fval, theFptr, args, nargs, expr, ctx);
                JL_GC_POP();
                return result;
            }
        }
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
#ifdef LLVM37
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

static void undef_var_error_if_null(Value *v, jl_sym_t *name, jl_codectx_t *ctx)
{
    Value *ok = builder.CreateICmpNE(v, V_null);
    BasicBlock *err = BasicBlock::Create(getGlobalContext(), "err", ctx->f);
    BasicBlock *ifok = BasicBlock::Create(getGlobalContext(), "ok");
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
            std::stringstream name;
            name << "delayedvar" << globalUnique++;
            Constant *initnul = ConstantPointerNull::get((PointerType*)T_pjlvalue);
            GlobalVariable *bindinggv = new GlobalVariable(*ctx->f->getParent(), T_pjlvalue,
                    false, GlobalVariable::PrivateLinkage,
                    initnul, name.str());
            Value *cachedval = builder.CreateLoad(bindinggv);
            BasicBlock *have_val = BasicBlock::Create(jl_LLVMContext, "found"),
                *not_found = BasicBlock::Create(jl_LLVMContext, "notfound");
            BasicBlock *currentbb = builder.GetInsertBlock();
            builder.CreateCondBr(builder.CreateICmpNE(cachedval, initnul), have_val, not_found);
            ctx->f->getBasicBlockList().push_back(not_found);
            builder.SetInsertPoint(not_found);
#ifdef LLVM37
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
            return julia_binding_gv(builder.CreateBitCast(p, T_ppjlvalue));
        }
        if (b->deprecated) cg_bdw(b, ctx);
    }
    if (pbnd) *pbnd = b;
    return julia_binding_gv(b);
}

static jl_cgval_t emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol)
{
    assert(bp->getType() == T_ppjlvalue);
    Value *v = builder.CreateLoad(bp, isvol);
    undef_var_error_if_null(v, name, ctx);
    return mark_julia_type(v, true, jl_any_type, ctx);
}

static jl_cgval_t emit_sparam(size_t i, jl_codectx_t *ctx)
{
    if (jl_svec_len(ctx->linfo->sparam_vals) > 0) {
        return mark_julia_const(jl_svecref(ctx->linfo->sparam_vals, i));
    }
    else {
        assert(ctx->spvals_ptr != NULL);
        Value *bp = builder.CreateConstInBoundsGEP1_32(LLVM37_param(T_pjlvalue)
                builder.CreateBitCast(ctx->spvals_ptr, T_ppjlvalue),
                i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
        return mark_julia_type(tbaa_decorate(tbaa_const, builder.CreateLoad(bp)), true, jl_any_type, ctx);
    }
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
        return mark_julia_type(builder.CreateLoad(bp), true, jl_any_type, ctx);
    }
    return emit_checked_var(bp, sym, ctx);
}

static jl_cgval_t emit_local(int sl, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->slots[sl];
    jl_sym_t *sym = slot_symbol(sl, ctx);
    if (!vi.isArgument && !vi.isAssigned) {
        undef_var_error_if_null(V_null, sym, ctx);
        return jl_cgval_t();
    }
    if (vi.memloc) {
        Value *bp = vi.memloc;
        if (vi.isArgument ||  // arguments are always defined
            (!vi.isAssigned && !vi.usedUndef)) {
            // if no undef usage was found by inference, and it's either not assigned or not in env: it must be always defined
            Instruction *v = builder.CreateLoad(bp, vi.isVolatile);
            return mark_julia_type(v, true, vi.value.typ, ctx);
        }
        else {
            jl_cgval_t v = emit_checked_var(bp, sym, ctx, vi.isVolatile);
            v = remark_julia_type(v, vi.value.typ); // patch up typ, is possible
            return v;
        }
    }
    else if (!vi.isVolatile || !vi.isAssigned) {
        return vi.value;
    }
    else {
        // copy value to a non-mutable location
        Type *T = julia_type_to_llvm(vi.value.typ)->getPointerTo();
        Value *v = data_pointer(vi.value, ctx, T);
        return mark_julia_type(builder.CreateLoad(v, vi.isVolatile), false, vi.value.typ, ctx);
    }
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    if (jl_is_gensym(l)) {
        ssize_t idx = ((jl_gensym_t*)l)->id;
        assert(idx >= 0);
        assert(!ctx->gensym_assigned.at(idx));
        jl_cgval_t slot = emit_expr(r, ctx); // slot could be a jl_value_t (unboxed) or jl_value_t* (ispointer)
        if (!slot.isboxed && !slot.isimmutable) { // emit a copy of values stored in mutable slots
            Type *vtype = julia_type_to_llvm(slot.typ);
            assert(vtype != T_pjlvalue);
            slot = mark_julia_type(
                    emit_unbox(vtype, slot, slot.typ),
                    false, slot.typ, ctx);
        }
        if (slot.isboxed && slot.isimmutable) {
            // see if inference had a better type for the gensym than the expression (after inlining getfield on a Tuple)
            jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
            if (jl_is_array(gensym_types)) {
                jl_value_t *declType = jl_cellref(gensym_types, idx);
                if (declType != slot.typ) {
                    slot = remark_julia_type(slot, declType);
                }
            }
        }
        ctx->gensym_SAvalues.at(idx) = slot; // now gensym_SAvalues[idx] contains the SAvalue
        ctx->gensym_assigned.at(idx) = true;
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
        assert(bnd);
        Value *rval = boxed(emit_expr(r, ctx), ctx, false); // no root needed since this is about to be assigned to a global
#ifdef LLVM37
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

    assert(vi.isAssigned);
    if (vi.memloc) {
        // boxed variables
        if (((!vi.isSA && rval_info.gcroot) || !rval_info.isboxed) && isa<AllocaInst>(vi.memloc)) {
            // rval had a gcroot, so lval needs one too: promote variable slot to a gcroot
            emit_local_root(ctx, &vi);
        }
        Value *rval = boxed(rval_info, ctx, false); // no root needed on the temporary since it is about to be assigned to the variable slot
        builder.CreateStore(rval, vi.memloc, vi.isVolatile);
    }
    else if (vi.value.constant) {
        // virtual store
    }
    else {
        // store unboxed
        assert(vi.value.ispointer);
        builder.CreateStore(
                emit_unbox(julia_type_to_llvm(vi.value.typ), rval_info, vi.value.typ),
                vi.value.V, vi.isVolatile);
    }
}

// --- convert expression to code ---

static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx)
{
    jl_cgval_t condV = emit_expr(cond, ctx);
    if (condV.typ == (jl_value_t*)jl_bool_type) {
        Value *cond = emit_unbox(T_int1, condV, (jl_value_t*)jl_bool_type);
        assert(cond->getType() == T_int1);
        return builder.CreateXor(cond, ConstantInt::get(T_int1,1));
    }
    emit_typecheck(condV, (jl_value_t*)jl_bool_type, msg, ctx);
    if (condV.isboxed) {
        return builder.CreateICmpEQ(boxed(condV, ctx), tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var))));
    }
    // not a boolean
    return ConstantInt::get(T_int1,0); // TODO: replace with Undef
}

static void emit_stmtpos(jl_value_t *expr, jl_codectx_t *ctx)
{
    if (jl_is_symbol(expr) || jl_is_slot(expr))
        return; // value not used, no point in attempting codegen for it
    if (jl_is_gensym(expr))
        return; // value not used, no point in attempting codegen for it
    if (jl_is_linenode(expr))
        return;
    if (jl_is_newvarnode(expr)) {
        jl_value_t *var = jl_fieldref(expr, 0);
        assert(jl_is_slot(var));
        jl_varinfo_t &vi = ctx->slots[jl_slot_number(var)-1];
        Value *lv = vi.memloc;
        if (lv != NULL) {
            // create a new uninitialized variable
            if (vi.usedUndef)
                builder.CreateStore(V_null, lv);
        }
        return;
    }
    if (jl_is_expr(expr)) {
        jl_sym_t *head = ((jl_expr_t*)expr)->head;
        // some expression types are metadata and can be ignored in statement position
        if (head == line_sym || head == type_goto_sym || head == meta_sym)
            return;
        // fall-through
    }
    (void)emit_expr(expr, ctx);
}

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_global(sym, ctx);
    }
    if (jl_is_slot(expr)) {
        size_t i = jl_slot_number(expr);
        jl_value_t *typ = jl_slot_get_type(expr);
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
        jl_cgval_t val = emit_local(i-1, ctx);
        if (val.isboxed)
            val = remark_julia_type(val, typ); // patch up typ to match slot.typ
        return val;
    }
    if (jl_is_gensym(expr)) {
        ssize_t idx = ((jl_gensym_t*)expr)->id;
        assert(idx >= 0);
        if (!ctx->gensym_assigned.at(idx)) {
            ctx->gensym_assigned.at(idx) = true; // (assignment, not comparison test)
            return jl_cgval_t(); // dead code branch
        }
        else {
            return ctx->gensym_SAvalues.at(idx); // at this point, gensym_SAvalues[idx] actually contains the SAvalue
        }
    }
    if (jl_is_labelnode(expr)) {
        int labelname = jl_labelnode_label(expr);
        BasicBlock *bb = (*ctx->labels)[labelname];
        assert(bb);
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            builder.CreateBr(bb); // all BasicBlocks must exit explicitly
        }
        builder.SetInsertPoint(bb);
        return jl_cgval_t();
    }
    if (jl_is_linenode(expr)) {
        jl_error("Linenode in value position");
    }
    if (jl_is_gotonode(expr)) {
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            int labelname = jl_gotonode_label(expr);
            BasicBlock *bb = (*ctx->labels)[labelname];
            assert(bb);
            builder.CreateBr(bb);
            BasicBlock *after = BasicBlock::Create(getGlobalContext(),
                                                   "br", ctx->f);
            builder.SetInsertPoint(after);
        }
        return jl_cgval_t();
    }
    if (jl_is_globalref(expr)) {
        return emit_getfield((jl_value_t*)jl_globalref_mod(expr), jl_globalref_name(expr), ctx);
    }
    if (jl_is_topnode(expr)) {
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
        jl_module_t *mod = topmod(ctx);
        jl_binding_t *b = jl_get_binding(mod, var);
        if (b == NULL)
            b = jl_get_binding_wr(mod, var);
        if (b->constp && b->value != NULL) {
            return mark_julia_const(b->value);
        }
        return emit_checked_var(julia_binding_gv(b), var, ctx);
    }
    if (!jl_is_expr(expr)) {
        int needroot = true;
        if (jl_is_quotenode(expr)) {
            expr = jl_fieldref(expr,0);
            if (jl_is_symbol(expr)) {
                needroot = false;
            }
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
        if (needroot) {
            jl_add_linfo_root(ctx->linfo, expr);
        }
        return mark_julia_const(expr);
    }

    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    jl_sym_t *head = ex->head;
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (head == goto_ifnot_sym) {
        jl_value_t *cond = args[0];
        int labelname = jl_unbox_long(args[1]);
        BasicBlock *ifso = BasicBlock::Create(getGlobalContext(), "if", ctx->f);
        BasicBlock *ifnot = (*ctx->labels)[labelname];
        assert(ifnot);
        // NOTE: if type inference sees a constant condition it behaves as if
        // the branch weren't there. But LLVM will not see constant conditions
        // this way until a later optimization pass, so it might see one of our
        // SSA vars as not dominating all uses. see issue #6068
        // Work around this by generating unconditional branches.
        if (cond == jl_true) {
            builder.CreateBr(ifso);
        }
        else if (cond == jl_false) {
            builder.CreateBr(ifnot);
        }
        else {
            Value *isfalse = emit_condition(cond, "if", ctx);
            builder.CreateCondBr(isfalse, ifnot, ifso);
        }
        builder.SetInsertPoint(ifso);
    }
    else if (head == call_sym) {
        jl_value_t *c = static_eval(expr, ctx, true, true);
        if (c) {
            jl_add_linfo_root(ctx->linfo, c);
            return mark_julia_const(c);
        }
        return emit_call(args, jl_array_dim0(ex->args), ctx, (jl_value_t*)ex);
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

        Value *bp = NULL, *name, *bp_owner = V_null;
        jl_binding_t *bnd = NULL;
        if (jl_is_symbol(mn)) {
            if (jl_symbol_name((jl_sym_t*)mn)[0] == '@')
                jl_errorf("macro definition not allowed inside a local scope");
            name = literal_pointer_val(mn);
            bnd = jl_get_binding_for_method_def(ctx->module, (jl_sym_t*)mn);
            bp = julia_binding_gv(bnd);
            bp_owner = literal_pointer_val((jl_value_t*)ctx->module);
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
        if (jl_is_symbol(sym)) {
            jl_binding_t *bnd = NULL;
            (void)global_binding_pointer(ctx->module, sym, &bnd, true, ctx); assert(bnd);
            builder.CreateCall(prepare_call(jldeclareconst_func),
                               literal_pointer_val(bnd));
        }
    }
    else if (head == null_sym) {
        return ghostValue(jl_void_type);
    }
    else if (head == static_typeof_sym) {
        jl_value_t *extype = expr_type((jl_value_t*)ex, ctx);
        if (jl_is_type_type(extype)) {
            extype = jl_tparam0(extype);
            if (jl_is_typevar(extype))
                extype = ((jl_tvar_t*)extype)->ub;
        }
        else {
            extype = (jl_value_t*)jl_any_type;
        }
        if (jl_is_tuple_type(extype))
            jl_add_linfo_root(ctx->linfo, extype);
        return mark_julia_const(extype);
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
    else if (head == exc_sym) { // *jl_exception_in_transit
        return mark_julia_type(builder.CreateLoad(emit_exc_in_transit(ctx),
                                                  /*isvolatile*/true),
                               true, jl_any_type, ctx);
    }
    else if (head == leave_sym) {
        assert(jl_is_long(args[0]));
        builder.CreateCall(prepare_call(jlleave_func),
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else if (head == enter_sym) {
        assert(jl_is_long(args[0]));
        int labl = jl_unbox_long(args[0]);
        Value *jbuf = builder.CreateGEP((*ctx->handlers)[labl],
                                        ConstantInt::get(T_size,0));
        builder.CreateCall(prepare_call(jlenter_func), jbuf);
#ifndef _OS_WINDOWS_
#ifdef LLVM37
        CallInst *sj = builder.CreateCall(prepare_call(setjmp_func), { jbuf, ConstantInt::get(T_int32,0) });
#else
        CallInst *sj = builder.CreateCall2(prepare_call(setjmp_func), jbuf, ConstantInt::get(T_int32,0));
#endif
#else
        CallInst *sj = builder.CreateCall(prepare_call(setjmp_func), jbuf);
#endif
        // We need to mark this on the call site as well. See issue #6757
        sj->setCanReturnTwice();
        Value *isz = builder.CreateICmpEQ(sj, ConstantInt::get(T_int32,0));
        BasicBlock *tryblk = BasicBlock::Create(getGlobalContext(), "try",
                                                ctx->f);
        BasicBlock *handlr = (*ctx->labels)[labl];
        assert(handlr);
#ifdef _OS_WINDOWS_
        BasicBlock *cond_resetstkoflw_blk = BasicBlock::Create(getGlobalContext(), "cond_resetstkoflw", ctx->f);
        BasicBlock *resetstkoflw_blk = BasicBlock::Create(getGlobalContext(), "resetstkoflw", ctx->f);
        builder.CreateCondBr(isz, tryblk, cond_resetstkoflw_blk);
        builder.SetInsertPoint(cond_resetstkoflw_blk);
        builder.CreateCondBr(builder.CreateICmpEQ(
                    literal_pointer_val(jl_stackovf_exception),
                    builder.CreateLoad(emit_exc_in_transit(ctx), true)),
                resetstkoflw_blk, handlr);
        builder.SetInsertPoint(resetstkoflw_blk);
        builder.CreateCall(prepare_call(resetstkoflw_func)
#                          ifdef LLVM37
                           , {}
#                          endif
                           );
        builder.CreateBr(handlr);
#else
        builder.CreateCondBr(isz, tryblk, handlr);
#endif
        builder.SetInsertPoint(tryblk);
    }
    else if (head == inbounds_sym) {
        // manipulate inbounds stack
        // note that when entering an inbounds context, we must also update
        // the boundsCheck context to be false
        if (jl_array_len(ex->args) > 0) {
            jl_value_t *arg = args[0];
            if (arg == jl_true) {
                ctx->inbounds.push_back(true);
                ctx->boundsCheck.push_back(false);
            }
            else if (arg == jl_false) {
                ctx->inbounds.push_back(false);
                ctx->boundsCheck.push_back(false);
            }
            else {
                if (!ctx->inbounds.empty())
                    ctx->inbounds.pop_back();
                if (!ctx->boundsCheck.empty())
                    ctx->boundsCheck.pop_back();
            }
        }
        return ghostValue(jl_void_type);
    }
    else if (head == boundscheck_sym) {
        if (jl_array_len(ex->args) > 0) {
            jl_value_t *arg = args[0];
            if (arg == jl_true) {
                ctx->boundsCheck.push_back(true);
            }
            else if (arg == jl_false) {
                ctx->boundsCheck.push_back(false);
            }
            else {
                if (!ctx->boundsCheck.empty())
                    ctx->boundsCheck.pop_back();
            }
        }
        return ghostValue(jl_void_type);
    }
    else if (head == copyast_sym) {
        jl_value_t *arg = args[0];
        if (jl_is_quotenode(arg)) {
            jl_value_t *arg1 = jl_fieldref(arg,0);
            if (!((jl_is_expr(arg1) && ((jl_expr_t*)arg1)->head!=null_sym) ||
                  jl_typeis(arg1,jl_array_any_type) || jl_is_quotenode(arg1))) {
                // elide call to jl_copy_ast when possible
                return emit_expr(arg, ctx);
            }
        }
        jl_cgval_t ast = emit_expr(arg, ctx);
        return mark_julia_type(builder.CreateCall(prepare_call(jlcopyast_func), boxed(ast, ctx)), true, ast.typ, ctx);
    }
    else if (head == simdloop_sym) {
        if (!llvm::annotateSimdLoop(builder.GetInsertBlock()))
            jl_printf(JL_STDERR, "WARNING: could not attach metadata for @simd loop.\n");
        return jl_cgval_t();
    }
    else {
        if (!strcmp(jl_symbol_name(head), "$"))
            jl_error("syntax: prefix \"$\" in non-quoted expression");
        if (jl_is_toplevel_only_expr(expr) &&
            ctx->linfo->name == anonymous_sym &&
            (ctx->slots.empty() || ((jl_expr_t*)expr)->head == thunk_sym) &&
            ctx->linfo->module == jl_current_module) {
            // call interpreter to run a toplevel expr from inside a
            // compiled toplevel thunk.
            builder.CreateCall(prepare_call(jltopeval_func), literal_pointer_val(expr));
            jl_add_linfo_root(ctx->linfo, expr);
            return ghostValue(jl_void_type);
        }
        if (head == abstracttype_sym || head == compositetype_sym ||
            head == bitstype_sym) {
            jl_errorf("type definition not allowed inside a local scope");
        }
        else {
            jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                      jl_symbol_name(head),
                      jl_symbol_name(ctx->linfo->name));
        }
    }
    return jl_cgval_t();
}

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(BasicBlock *b0, jl_codectx_t *ctx)
{
    // allocate a placeholder gc instruction
    ctx->ptlsStates = builder.CreateCall(prepare_call(jltls_states_func));
}

void jl_codegen_finalize_temp_arg(CallInst *ptlsStates, Type *T_pjlvalue);
static void finalize_gc_frame(Function *F)
{
    Module *M = F->getParent();
    M->getOrInsertFunction(gcroot_func->getName(), gcroot_func->getFunctionType());
    M->getOrInsertFunction(gckill_func->getName(), gckill_func->getFunctionType());
    M->getOrInsertFunction(gcstore_func->getName(), gcstore_func->getFunctionType());
    M->getOrInsertFunction(jlcall_frame_func->getName(), jlcall_frame_func->getFunctionType());
    Function *jl_get_ptls_states = M->getFunction("jl_get_ptls_states");

    CallInst *ptlsStates = NULL;
    for (BasicBlock::iterator i = F->getEntryBlock().begin(), e = F->getEntryBlock().end(); i != e; ++i) {
        if (CallInst *callInst = dyn_cast<CallInst>(&*i)) {
            if (callInst->getCalledFunction() == jl_get_ptls_states) {
                ptlsStates = callInst;
                break;
            }
        }
    }
    if (!ptlsStates)
        return;

    jl_codegen_finalize_temp_arg(ptlsStates, T_pjlvalue);

#ifdef JULIA_ENABLE_THREADING
    if (imaging_mode) {
        Value *getter;
        if (GlobalVariable *GV = M->getGlobalVariable(jltls_states_func_ptr->getName(), true /* AllowLocal */)) {
            getter = tbaa_decorate(tbaa_const, new LoadInst(GV, "", ptlsStates));
        }
        else {
            // If the global variable doesn't exist, we are coping code out of
            // the shadow_module to run, in which case, we just use the
            // function directly.
            getter = ConstantExpr::getIntToPtr(
                ConstantInt::get(T_size, (intptr_t)jl_get_ptls_states_getter()),
                jltls_states_func->getFunctionType()->getPointerTo());
        }
        ptlsStates->setCalledFunction(getter);
    }
    ptlsStates->setAttributes(jltls_states_func->getAttributes());
#else
    Module *destModule = F->getParent();
    GlobalVariable *GV = destModule->getGlobalVariable(jltls_states_var->getName());
    if (GV == NULL) {
        GV = new GlobalVariable(*destModule,
            jltls_states_var->getType()->getElementType(),
            jltls_states_var->isConstant(),
            GlobalVariable::ExternalLinkage,
            NULL,
            jltls_states_var->getName());
        GV->copyAttributesFrom(jltls_states_var);
    }
    ptlsStates->replaceAllUsesWith(GV);
    ptlsStates->eraseFromParent();
#endif
}

static void finalize_gc_frame(Module *m)
{
    for (Module::iterator I = m->begin(), E = m->end(); I != E; ++I) {
        Function *F = &*I;
        if (F->isDeclaration())
            continue;
        finalize_gc_frame(F);
    }
#ifndef JULIA_ENABLE_THREADING
    m->getFunction("jl_get_ptls_states")->eraseFromParent();
#endif
    m->getFunction("julia.gc_root_decl")->eraseFromParent();
    m->getFunction("julia.gc_root_kill")->eraseFromParent();
    m->getFunction("julia.gc_store")->eraseFromParent();
    m->getFunction("julia.jlcall_frame_decl")->eraseFromParent();
}

// here argt does not include the leading function type argument
static Function *gen_cfun_wrapper(jl_lambda_info_t *lam, jl_function_t *ff, jl_value_t *jlrettype, jl_tupletype_t *argt, int64_t isref)
{
    cFunctionList_t *list = (cFunctionList_t*)lam->functionObjects.cFunctionList;
    if (list != NULL) {
        size_t i;
        for (i = 0; i < list->len; i++) {
            if (list->data()[i].isref == isref) {
                return list->data()[i].f;
            }
        }
    }
    // Generate a c-callable wrapper
    bool toboxed;
    Type *crt = ((isref & 1) ? T_pjlvalue : julia_struct_to_llvm(jlrettype, &toboxed));
    if (crt == NULL)
        jl_error("cfunction: return type doesn't correspond to a C type");
    size_t i;
    size_t nargs = jl_nparams(argt);
    for(i=1; i < nargs+1; i++) {
        jl_value_t *tti = jl_nth_slot_type(lam->specTypes,i);
        if (tti == (jl_value_t*)jl_pointer_type) {
            jl_error("cfunction: argument type Ptr should have an element type, Ptr{T}");
        }
    }

    std::vector<Type*> fargt(0);
    std::vector<bool> fargt_isboxed(0);
    std::vector<Type*> fargt_sig(0);
    Type *fargt_vasig;
    std::vector<bool> inRegList(0);
    std::vector<bool> byRefList(0);
    attr_type attrs;
    Type *prt = NULL;
    int sret = 0;
    std::string err_msg = generate_func_sig(&crt, &prt, sret, fargt, fargt_isboxed, fargt_sig, fargt_vasig, inRegList, byRefList, attrs,
                                            ((isref&1) ? (jl_value_t*)jl_any_type : jlrettype), argt->parameters, nargs);
    if (!err_msg.empty())
        jl_error(err_msg.c_str());
    if (fargt.size() + sret != fargt_sig.size())
        jl_error("va_arg syntax not allowed for cfunction argument list");

    jl_compile_linfo(lam);
    if (!lam->functionObjects.functionObject) {
        jl_errorf("error compiling %s while creating cfunction",
                  jl_symbol_name(lam->name));
    }

    std::stringstream funcName;
    funcName << "jlcapi_" << jl_symbol_name(lam->name) << "_" << globalUnique++;

    // Backup the info for the nested compile
    JL_SIGATOMIC_BEGIN(); // no errors expected beyond this point
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    jl_gc_inhibit_finalizers(nested_compile); // no allocations expected between the top of this function (when last scanned lam->cFunctionList) and here, which might have triggered running julia code

    Module *M = new Module(jl_symbol_name(lam->name), jl_LLVMContext);
    jl_setup_module(M);
    Function *cw = Function::Create(FunctionType::get(sret ? T_void : prt, fargt_sig, false),
            GlobalVariable::ExternalLinkage,
            funcName.str(), M);
    addComdat(cw);
    cw->setAttributes(attrs);
#ifdef LLVM37
    cw->addFnAttr("no-frame-pointer-elim", "true");
#endif
    Function *cw_proto = function_proto(cw);

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", cw);
    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx;
    ctx.f = cw;
    ctx.linfo = lam;
    ctx.sret = false;
    ctx.spvals_ptr = NULL;
    allocate_gc_frame(b0, &ctx);

    // Save the Function object reference
    int len = (list ? list->len : 0) + 1;
    cFunctionList_t *list2 = (cFunctionList_t*)realloc(list, sizeof(cFunctionList_t)+sizeof(list->data()[0])*len);
    if (!list2)
        jl_throw(jl_memory_exception);
    list2->len = len;
    list2->data()[len-1].isref = isref;
    list2->data()[len-1].f = cw_proto;
    lam->functionObjects.cFunctionList = list2;

    // See whether this function is specsig or jlcall
    bool specsig, jlfunc_sret;
    Function *theFptr;
    Value *myargs;
    if (lam->functionObjects.specFunctionObject != NULL) {
        theFptr = (Function*)lam->functionObjects.specFunctionObject;
        specsig = true;
        jlfunc_sret = theFptr->hasStructRetAttr();
        myargs = NULL;
    }
    else {
        theFptr = (Function*)lam->functionObjects.functionObject;
        specsig = false;
        jlfunc_sret = false;
        myargs = builder.CreateCall(prepare_call(jlcall_frame_func), ConstantInt::get(T_int32, nargs));
    }
    assert(theFptr);

    // Alright, let's do this!
    // let's first emit the arguments
    std::vector<Value*> args;
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = NULL;
    if (sret)
        sretPtr = &*AI++;

    Value *result;
    size_t FParamIndex = 0;
    if (jlfunc_sret) {
        if (sret)
            result = builder.CreateBitCast(sretPtr, theFptr->getFunctionType()->getParamType(0));
        else
            result = builder.CreateAlloca(theFptr->getFunctionType()->getParamType(0)->getContainedType(0));
        args.push_back(result);
        FParamIndex++;
    }

    for (size_t i = 0; i < nargs; i++) {
        Value *val = &*AI++;
        jl_value_t *jargty = jl_nth_slot_type(lam->specTypes, i+1);  // +1 because argt excludes function
        bool isboxed, argboxed;
        Type *t = julia_type_to_llvm(jargty, &isboxed);
        (void)julia_struct_to_llvm(jargty, &argboxed);
        jl_cgval_t inputarg;

        // figure out how to unpack this type
        if (isref & (2<<i)) {
            if (!jl_isbits(jargty)) {
                inputarg = mark_julia_type(builder.CreatePointerCast(val, T_pjlvalue), true, jargty, &ctx);
            }
            else {
                if (type_is_ghost(t)) {
                    if (specsig) {
                        continue; // ghost types are skipped by the specsig method signature
                    }
                    else {
                        inputarg = ghostValue(jargty);
                    }
                }
                else {
                    val = builder.CreatePointerCast(val, t->getPointerTo());
                    val = builder.CreateAlignedLoad(val, 1); // make no alignment assumption about pointer from C
                    inputarg = mark_julia_type(val, false, jargty, &ctx);
                }
            }
        }
        else if (argboxed) {
            inputarg = mark_julia_type(val, true, jargty, &ctx);
        }
        else {
            // undo whatever we might have done to this poor argument
            bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type, 0);
            val = llvm_type_rewrite(val, val->getType(), fargt[i], true, byRefList[i], issigned, &ctx);
            if (isboxed) {
                Value *mem = emit_allocobj(jl_datatype_size(jargty));
                builder.CreateStore(literal_pointer_val((jl_value_t*)jargty),
                                    emit_typeptr_addr(mem));
                builder.CreateAlignedStore(val,
                        builder.CreateBitCast(mem, val->getType()->getPointerTo()),
                        16); // julia's gc gives 16-byte aligned addresses
                inputarg = mark_julia_type(mem, true, jargty, &ctx);
            }
            else {
                inputarg = mark_julia_type(val, false, jargty, &ctx);
            }
        }

        // figure out how to repack this type
        if (!specsig) {
            Value *arg = boxed(inputarg, &ctx, false); // don't want a gcroot, since it's about to be but into the jlcall frame anyways
            Value *slot = builder.CreateGEP(myargs, ConstantInt::get(T_int32, FParamIndex++));
            builder.CreateStore(arg, slot);
        }
        else {
            Value *arg;
            FParamIndex++;
            if (isboxed) {
                arg = boxed(inputarg, &ctx);
            }
            else {
                arg = emit_unbox(t, inputarg, jargty);
                assert(!isa<UndefValue>(arg));
                if (t->isAggregateType()) {
#ifndef NDEBUG
                    Type *at = theFptr->getFunctionType()->getParamType(FParamIndex-1);
#endif
                    assert(at->isPointerTy() && at->getContainedType(0) == t);
                    // aggregate types are passed by pointer
                    Value *loc = emit_static_alloca(t, &ctx);
                    builder.CreateStore(arg, loc);
                    arg = loc;
                }
            }

            // add to argument list
            args.push_back(arg);
        }
    }

    // Create the call
    jl_cgval_t retval;
    if (specsig) {
        bool retboxed;
        CallInst *call = builder.CreateCall(prepare_call(theFptr), ArrayRef<Value*>(args));
        call->setAttributes(theFptr->getAttributes());
        (void)julia_type_to_llvm(jlrettype, &retboxed);
        retval = mark_julia_type(jlfunc_sret ? (Value*)builder.CreateLoad(result) : (Value*)call, retboxed, jlrettype, &ctx);
    }
    else {
        assert(nargs > 0);
        // for jlcall, we need to pass the function object even if it is a ghost.
        // here we reconstruct the function instance from its type (first elt of argt)
        Value *theF = literal_pointer_val((jl_value_t*)ff);
#ifdef LLVM37
        Value *ret = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                        ConstantInt::get(T_int32, nargs)});
#else
        Value *ret = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                         ConstantInt::get(T_int32, nargs));
#endif
        retval = mark_julia_type(ret, true, jlrettype, &ctx);
    }

    // Prepare the return value
    Value *r;
    if (isref & 1) {
        assert(!sret);
        // return a jl_value_t*
        r = boxed(retval, &ctx, false); // no gcroot since this is on the return path
    }
    else if (sret && jlfunc_sret) {
        // nothing to do
    }
    else if (!type_is_ghost(crt)) {
        if (sret)
            prt = fargt_sig[0]->getContainedType(0); // sret is a PointerType
        bool issigned = jl_signed_type && jl_subtype(jlrettype, (jl_value_t*)jl_signed_type, 0);
        Value *v = julia_to_native(crt, toboxed, jlrettype, retval,
                false, false, false, false, false, 0, &ctx, NULL);
        r = llvm_type_rewrite(v, crt, prt, false, false, issigned, &ctx);
        if (sret)
            builder.CreateStore(r, sretPtr);
    }
    else {
        assert(type_is_ghost(prt));
        sret = true;
    }

    if (sret)
        builder.CreateRetVoid();
    else
        builder.CreateRet(r);

    jl_finalize_module(std::unique_ptr<Module>(M));

    // Restore the previous compile context
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    nested_compile = last_n_c;
    jl_gc_inhibit_finalizers(nested_compile);
    JL_SIGATOMIC_END();

    return cw_proto;
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_lambda_info_t *lam, jl_expr_t *ast, Function *f, bool sret, Module *M)
{
    std::stringstream funcName;
    const std::string &fname = f->getName().str();
    funcName << "jlcall_";
    if (fname.compare(0, 6, "julia_") == 0)
        funcName << fname.substr(6);
    else
        funcName << fname;

    Function *w = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage,
                                   funcName.str(), M);
    addComdat(w);
#ifdef LLVM37
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

    jl_codectx_t ctx;
    ctx.f = w;
    ctx.linfo = lam;
    ctx.sret = false;
    ctx.spvals_ptr = NULL;
    allocate_gc_frame(b0, &ctx);

    size_t nargs = jl_array_dim0(jl_lam_args(ast));
    size_t nfargs = f->getFunctionType()->getNumParams();
    Value **args = (Value**) alloca(nfargs*sizeof(Value*));
    unsigned idx = 0;
    Value *result;
    if (sret) {
        result = builder.CreateAlloca(f->getFunctionType()->getParamType(0)->getContainedType(0));
        args[idx] = result;
        idx++;
    }
    for(size_t i=0; i < nargs; i++) {
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
            theArg = builder.CreatePointerCast(theArg, PointerType::get(lty,0));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = builder.CreateLoad(theArg);
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
    jl_cgval_t retval = sret ? mark_julia_slot(result, jlretty) : mark_julia_type(call, retboxed, jlretty, &ctx);
    builder.CreateRet(boxed(retval, &ctx, false)); // no gcroot needed since this on the return path

    return w;
}

// Compile to LLVM IR, using a specialized signature if applicable.
static std::unique_ptr<Module> emit_function(jl_lambda_info_t *lam, jl_llvm_functions_t *declarations)
{
    assert(declarations && "Capturing declarations is always required");

    // step 1. unpack AST and allocate codegen context for this function
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    JL_GC_PUSH1(&ast);
    if (!jl_is_expr(ast)) {
        ast = (jl_expr_t*)jl_uncompress_ast(lam, (jl_value_t*)ast);
    }
    assert(jl_is_expr(ast));
    //jl_static_show(JL_STDOUT, (jl_value_t*)ast);
    //jl_printf(JL_STDOUT, "\n");
    std::map<int, jl_arrayvar_t> arrayvars;
    std::map<int, BasicBlock*> labels;
    std::map<int, Value*> handlers;
    jl_codectx_t ctx;
    ctx.arrayvars = &arrayvars;
    ctx.labels = &labels;
    ctx.handlers = &handlers;
    ctx.module = lam->module;
    ctx.ast = ast;
    ctx.linfo = lam;
    ctx.funcName = jl_symbol_name(lam->name);
    ctx.vaSlot = -1;
    ctx.vaStack = false;
    ctx.inbounds.push_back(false);
    ctx.boundsCheck.push_back(false);
    ctx.spvals_ptr = NULL;

    // step 2. process var-info lists to see what vars need boxing
    jl_value_t *gensym_types = jl_lam_gensyms(ast);
    int n_gensyms = (jl_is_array(gensym_types) ? jl_array_len(gensym_types) : jl_unbox_gensym(gensym_types));
    jl_array_t *largs = jl_lam_args(ast);
    size_t largslen = jl_array_dim0(largs);
    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t vinfoslen = jl_array_dim0(vinfos);
    ctx.slots.resize(vinfoslen);
    size_t nreq = largslen;
    int va = 0;

    assert(lam->specTypes); // this could happen if the user tries to compile a generic-function
                            // without specializing (or unspecializing) it first
                            // compiling this would cause all specializations to inherit
                            // this code and could create an broken compile / function cache

    if (nreq > 0 && jl_is_rest_arg(jl_cellref(largs,largslen-1))) {
        nreq--;
        va = 1;
        jl_sym_t *vn = jl_decl_var(jl_cellref(largs,largslen-1));
        if (vn != unused_sym)
            ctx.vaSlot = largslen-1;
    }
    ctx.nReqArgs = nreq;

    // create SAvalue locations for GenSym objects
    ctx.gensym_assigned.assign(n_gensyms, false);
    ctx.gensym_SAvalues.assign(n_gensyms, jl_cgval_t());

    // step 3. some variable analysis
    size_t i;
    for(i=0; i < nreq; i++) {
        jl_value_t *arg = jl_cellref(largs,i);
        jl_sym_t *argname = jl_decl_var(arg);
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
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
        assert(jl_is_symbol(vname));
        jl_varinfo_t &varinfo = ctx.slots[i];
        varinfo.isAssigned = (jl_vinfo_assigned(vi)!=0);
        varinfo.escapes = false;
        varinfo.isSA = (jl_vinfo_sa(vi)!=0);
        varinfo.usedUndef = (jl_vinfo_usedundef(vi)!=0) || (!varinfo.isArgument && !lam->inferred);
        if (!varinfo.isArgument || varinfo.isAssigned) {
            jl_value_t *typ = jl_cellref(vi,1);
            if (!jl_is_type(typ))
                typ = (jl_value_t*)jl_any_type;
            varinfo.value = mark_julia_type((Value*)NULL, false, typ, &ctx);
        }
    }

    // finish recording escape info
    simple_escape_analysis((jl_value_t*)ast, true, &ctx);

    // determine which vars need to be volatile
    jl_array_t *stmts = jl_lam_body(ast)->args;
    mark_volatile_vars(stmts, ctx.slots);

    // step 4. determine function signature
    jl_value_t *jlrettype = lam->rettype;
    Function *f = NULL;

    bool specsig = false;
    bool needsparams = jl_svec_len(lam->sparam_syms) != jl_svec_len(lam->sparam_vals);
    if (!va && !needsparams && lam->specTypes != jl_anytuple_type && lam->inferred) {
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

    std::stringstream funcName;
    // try to avoid conflicts in the global symbol table
    funcName << "julia_" << jl_symbol_name(lam->name);

    Function *fwrap = NULL;
    funcName << "_" << globalUnique++;

    ctx.sret = false;
    Module *M = new Module(jl_symbol_name(lam->name), jl_LLVMContext);
    jl_setup_module(M);
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
        for(size_t i=0; i < jl_nparams(lam->specTypes); i++) {
            Type *ty = julia_type_to_llvm(jl_tparam(lam->specTypes,i));
            if (type_is_ghost(ty))
                continue;
            if (ty->isAggregateType()) // aggregate types are passed by pointer
                ty = PointerType::get(ty,0);
            fsig.push_back(ty);
        }
        f = Function::Create(FunctionType::get(rt, fsig, false),
                             GlobalVariable::ExternalLinkage,
                             funcName.str(), M);
        if (ctx.sret)
            f->addAttribute(1, Attribute::StructRet);
        addComdat(f);
#ifdef LLVM37
        f->addFnAttr("no-frame-pointer-elim", "true");
#endif
        fwrap = gen_jlcall_wrapper(lam, ast, f, ctx.sret, M);
        declarations->functionObject = function_proto(fwrap);
        declarations->specFunctionObject = function_proto(f);
    }
    else {
        f = Function::Create(needsparams ? jl_func_sig_sparams : jl_func_sig,
                             GlobalVariable::ExternalLinkage,
                             funcName.str(), M);
        addComdat(f);
#ifdef LLVM37
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

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && defined(LLVM35)
    f->setHasUWTable(); // force NeedsWinEH
#endif

#ifdef JL_DEBUG_BUILD
    f->addFnAttr(Attribute::StackProtectReq);
#endif
    ctx.f = f;

    // step 5. set up debug info context and create first basic block
    bool in_user_code = !jl_is_submodule(lam->module, jl_base_module) && !jl_is_submodule(lam->module, jl_core_module);
    bool do_coverage = jl_options.code_coverage == JL_LOG_ALL || (jl_options.code_coverage == JL_LOG_USER && in_user_code);
    bool do_malloc_log = jl_options.malloc_log  == JL_LOG_ALL || (jl_options.malloc_log    == JL_LOG_USER && in_user_code);
    jl_value_t *stmt = skip_meta(stmts);
    StringRef filename = "<no file>";
    StringRef dbgFuncName = jl_symbol_name(lam->name);
    int lno = -1;
    // look for initial (line num filename [funcname]) node, [funcname] for kwarg methods.
    if (jl_is_linenode(stmt)) {
        lno = jl_linenode_line(stmt);
        filename = jl_symbol_name(jl_linenode_file(stmt));
    }
    else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym &&
             jl_array_dim0(((jl_expr_t*)stmt)->args) > 0) {
        jl_value_t *a1 = jl_exprarg(stmt,0);
        if (jl_is_long(a1))
            lno = jl_unbox_long(a1);
        if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
            a1 = jl_exprarg(stmt,1);
            if (jl_is_symbol(a1))
                filename = jl_symbol_name((jl_sym_t*)a1);
            if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 2) {
                a1 = jl_exprarg(stmt,2);
                if (jl_is_symbol(a1))
                    dbgFuncName = jl_symbol_name((jl_sym_t*)a1);
            }
        }
    }
    if (filename.empty())
        filename = "<missing>";
    int toplineno = lno;

    DIBuilder dbuilder(*M);
    ctx.dbuilder = &dbuilder;
#ifdef LLVM37
    DIFile *topfile = NULL;
    DISubprogram *SP = NULL;
    DICompileUnit *CU;
#else
    DIFile topfile;
    DISubprogram SP;
#endif
    DebugLoc inlineLoc;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);

    // jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           jl_symbol_name(lam->name), filename.c_str(), lno);

    DebugLoc noDbg;
    ctx.debug_enabled = true;
    if (dbgFuncName.empty()) {
        // special value: if function name is empty, disable debug info
        do_coverage = false;
        do_malloc_log = false;
        //dbgFuncName = filename; // for testing, uncomment this line
        ctx.debug_enabled = !dbgFuncName.empty();
    }

    if (ctx.debug_enabled) {
        // TODO: Fix when moving to new LLVM version
        #ifndef LLVM34
        dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        #elif defined(LLVM37)
        CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        #else
        DICompileUnit CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        assert(CU.Verify());
        #endif

#ifdef LLVM37
        DISubroutineType *subrty;
#elif defined(LLVM36)
        DISubroutineType subrty;
#else
        DICompositeType subrty;
#endif

        if (!specsig) {
            subrty = jl_di_func_sig;
        }
        else {
#ifdef LLVM36
            std::vector<Metadata*> ditypes(0);
#else
            std::vector<Value*> ditypes(0);
#endif
            for(size_t i=0; i < jl_nparams(lam->specTypes); i++) { // assumes !va
                if (i < largslen && ctx.slots[i].value.isghost)
                    continue;
                ditypes.push_back(julia_type_to_di(jl_tparam(lam->specTypes,i),ctx.dbuilder,false));
            }
#ifdef LLVM38
            subrty = ctx.dbuilder->createSubroutineType(ctx.dbuilder->getOrCreateTypeArray(ditypes));
#elif defined(LLVM36)
            subrty = ctx.dbuilder->createSubroutineType(topfile,ctx.dbuilder->getOrCreateTypeArray(ditypes));
#else
            subrty = ctx.dbuilder->createSubroutineType(topfile,ctx.dbuilder->getOrCreateArray(ditypes));
#endif
        }

        topfile = dbuilder.createFile(filename, ".");
        #ifndef LLVM34
        SP = dbuilder.createFunction((DIDescriptor)dbuilder.getCU(),
        #else
        SP = dbuilder.createFunction(CU,
        #endif
                                    dbgFuncName,  // Name
                                    f->getName(), // LinkageName
                                    topfile,       // File
                                    0,            // LineNo
                                    subrty,       // Ty
                                    false,        // isLocalToUnit
                                    true,         // isDefinition
                                    0,            // ScopeLine
                                    0,            // Flags
                                    true,         // isOptimized
        #ifdef LLVM38
                                    nullptr);       // Template Parameters
        #else
                                    f);             // Function
        #endif
        // set initial line number
        inlineLoc = DebugLoc::get(lno, 0, (MDNode*)SP, NULL);
        #ifdef LLVM38
        f->setSubprogram(SP);
        #endif
        #ifndef LLVM37
        assert(SP.Verify() && SP.describes(f) && SP.getFunction() == f);
        #endif
    }
    builder.SetCurrentDebugLocation(noDbg);

    if (ctx.debug_enabled) {
        const bool AlwaysPreserve = true;
        // Go over all arguments and local variables and initialize their debug information
        for(i=0; i < nreq; i++) {
            jl_sym_t *argname = jl_decl_var(jl_cellref(largs,i));
            if (argname == unused_sym) continue;
            jl_varinfo_t &varinfo = ctx.slots[i];
#ifdef LLVM38
            varinfo.dinfo = ctx.dbuilder->createParameterVariable(
                SP,                                 // Scope (current function will be fill in later)
                jl_symbol_name(argname),            // Variable name
                ctx.sret + i + 1,                   // Argument number (1-based)
                topfile,                            // File
                toplineno == -1 ? 0 : toplineno,  // Line
                // Variable type
                julia_type_to_di(varinfo.value.typ,ctx.dbuilder,false),
                AlwaysPreserve,                  // May be deleted if optimized out
                0);                     // Flags (TODO: Do we need any)
#else
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,    // Tag
                SP,         // Scope (current function will be fill in later)
                jl_symbol_name(argname),    // Variable name
                topfile,                    // File
                toplineno == -1 ? 0 : toplineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, ctx.dbuilder,false), // Variable type
                AlwaysPreserve,                  // May be deleted if optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + i + 1);                   // Argument number (1-based)
#endif
        }
        if (va && ctx.vaSlot != -1) {
#ifdef LLVM38
            ctx.slots[ctx.vaSlot].dinfo = ctx.dbuilder->createParameterVariable(
                SP,                     // Scope (current function will be fill in later)
                std::string(jl_symbol_name(slot_symbol(ctx.vaSlot, &ctx))) + "...",  // Variable name
                ctx.sret + nreq + 1,               // Argument number (1-based)
                topfile,                    // File
                toplineno == -1 ? 0 : toplineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.slots[ctx.vaSlot].value.typ, ctx.dbuilder, false),
                AlwaysPreserve,                  // May be deleted if optimized out
                0);                     // Flags (TODO: Do we need any)
#else
            ctx.slots[ctx.vaSlot].dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,   // Tag
                SP,                                 // Scope (current function will be fill in later)
                std::string(jl_symbol_name(slot_symbol(ctx.vaSlot, &ctx))) + "...",  // Variable name
                topfile,                            // File
                toplineno == -1 ? 0 : toplineno,  // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.slots[ctx.vaSlot].value.typ, ctx.dbuilder, false),      // Variable type
                AlwaysPreserve,                  // May be deleted if optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + nreq + 1);              // Argument number (1-based)
#endif
        }
        for(i=0; i < vinfoslen; i++) {
            jl_sym_t *s = (jl_sym_t*)jl_cellref(jl_cellref(vinfos,i),0);
            jl_varinfo_t &varinfo = ctx.slots[i];
            if (varinfo.isArgument)
                continue;
#ifdef LLVM38
            varinfo.dinfo = ctx.dbuilder->createAutoVariable(
#else
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
#endif
                SP,                     // Scope (current function will be fill in later)
                jl_symbol_name(s),       // Variable name
                topfile,                 // File
                toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, ctx.dbuilder, false), // Variable type
                AlwaysPreserve,                  // May be deleted if optimized out
                0                       // Flags (TODO: Do we need any)
#ifndef LLVM38
                ,0                      // Argument number (1-based)
#endif
                );
        }
    }

#ifdef LLVM37
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

    // step 8. allocate space for exception handler contexts
    size_t stmtslen = jl_array_dim0(stmts);
    for(i=0; i < stmtslen; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == enter_sym) {
            int labl = jl_unbox_long(jl_exprarg(stmt,0));
            AllocaInst *handlr =
                builder.CreateAlloca(T_int8,
                                     ConstantInt::get(T_int32,
                                                      sizeof(jl_handler_t)));
            handlr->setAlignment(16);
            handlers[labl] = handlr;
        }
    }

    // step 9. allocate local variables slots
    // must be in the first basic block for the llvm mem2reg pass to work

    // get pointers for locals stored in the gc frame array (argTemp)
    for(i=0; i < vinfoslen; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_cellref(jl_cellref(vinfos,i),0);
        if (s == unused_sym) continue;
        jl_varinfo_t &varinfo = ctx.slots[i];
        assert(!varinfo.memloc); // variables shouldn't also have memory locs already
        if (!varinfo.usedUndef) {
            if (varinfo.value.constant) {
                // no need to explicitly load/store a constant/ghost value
                continue;
            }
            else if (jl_is_type_type(varinfo.value.typ) && jl_is_leaf_type(jl_tparam0(varinfo.value.typ))) {
                // replace T::Type{T} with T
                varinfo.value = mark_julia_const(jl_tparam0(varinfo.value.typ));
                continue;
            }
            else if (store_unboxed_p(i, &ctx)) {
                if (varinfo.isAssigned) { // otherwise, just leave it in the input register
                    Value *lv = alloc_local(i, &ctx); (void)lv;
#ifdef LLVM36
                    if (ctx.debug_enabled) {
                        assert(varinfo.dinfo->getType() != jl_pvalue_dillvmt);
                        ctx.dbuilder->insertDeclare(lv, varinfo.dinfo, ctx.dbuilder->createExpression(),
#ifdef LLVM37
                                inlineLoc,
#endif
                                builder.GetInsertBlock());
                    }
#endif
                }
                continue;
            }
        }
        if (varinfo.isAssigned || // always need a slot if the variable is assigned
            specsig || // for arguments, give them stack slots if then aren't in `argArray` (otherwise, will use that pointer)
            (va && (int)i == ctx.vaSlot && varinfo.escapes) || // or it's the va arg tuple
            (s != unused_sym && s == jl_decl_var(jl_cellref(largs, 0)))) { // or it is the first argument (which isn't in `argArray`)
            AllocaInst *av = new AllocaInst(T_pjlvalue, jl_symbol_name(s), /*InsertBefore*/ctx.ptlsStates);
            varinfo.memloc = av;
#ifdef LLVM36
            if (ctx.debug_enabled) {
                DIExpression *expr;
                if (varinfo.dinfo->getType() == jl_pvalue_dillvmt) {
                    expr = ctx.dbuilder->createExpression();
                }
                else {
                    SmallVector<uint64_t, 8> addr;
                    addr.push_back(llvm::dwarf::DW_OP_deref);
                    expr = ctx.dbuilder->createExpression(addr);
                }
                ctx.dbuilder->insertDeclare(av, varinfo.dinfo, expr,
#ifdef LLVM37
                                inlineLoc,
#endif
                                builder.GetInsertBlock());
            }
#endif
        }
        maybe_alloc_arrayvar(i, &ctx);
    }

    // step 10. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    if (ctx.sret)
        AI++; // skip sret slot
    for(i=0; i < nreq; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
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
                    theArg = mark_julia_slot(&*AI++, argType); // this argument is by-pointer
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
#ifdef LLVM36
                    if (ctx.debug_enabled && !vi.memloc && !vi.value.V) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus);
                        addr.push_back((i - 1) * sizeof(void*));
                        if (vi.dinfo->getType() != jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        ctx.dbuilder->insertDeclare(pargArray, vi.dinfo, ctx.dbuilder->createExpression(addr),
#ifdef LLVM37
                                        inlineLoc,
#endif
                                        builder.GetInsertBlock());
                    }
#endif
                }
            }

            if (vi.memloc == NULL) {
                if (vi.value.V) {
                    // copy theArg into its local variable slot (unboxed)
                    assert(vi.isAssigned && vi.value.ispointer);
                    builder.CreateStore(emit_unbox(vi.value.V->getType()->getContainedType(0),
                                                   theArg, vi.value.typ),
                                        vi.value.V);
                }
                else {
                    // keep track of original (possibly boxed) value to avoid re-boxing or moving
                    assert(!vi.isAssigned || vi.value.constant);
                    vi.value = theArg;
#ifdef LLVM36
                    if (specsig && theArg.V && ctx.debug_enabled) {
                        SmallVector<uint64_t, 8> addr;
                        if (vi.dinfo->getType() != jl_pvalue_dillvmt && theArg.ispointer)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        AllocaInst *parg = dyn_cast<AllocaInst>(theArg.V);
                        if (!parg) {
                            parg = builder.CreateAlloca(theArg.V->getType(), NULL, jl_symbol_name(s));
                            builder.CreateStore(theArg.V, parg);
                        }
                        ctx.dbuilder->insertDeclare(parg, vi.dinfo, ctx.dbuilder->createExpression(addr),
#ifdef LLVM37
                                        inlineLoc,
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

    // step 11. allocate rest argument if necessary
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &vi = ctx.slots[ctx.vaSlot];
        if (!vi.escapes && !vi.isAssigned) {
            ctx.vaStack = true;
        }
        else if (!vi.value.constant) {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            if (vi.memloc != NULL) {
#ifdef LLVM37
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

    // step 12. associate labels with basic blocks to resolve forward jumps
    BasicBlock *prev=NULL;
    for(i=0; i < stmtslen; i++) {
        jl_value_t *ex = jl_cellref(stmts,i);
        if (jl_is_labelnode(ex)) {
            int lname = jl_labelnode_label(ex);
            if (prev != NULL) {
                // fuse consecutive labels
                labels[lname] = prev;
            }
            else {
                prev = BasicBlock::Create(getGlobalContext(), "L", f);
                labels[lname] = prev;
            }
        }
        else {
            prev = NULL;
        }
    }

    // step 13. compile body statements
    bool prevlabel = false;
    lno = -1;
    int prevlno = -1;
    if (ctx.debug_enabled)
        builder.SetCurrentDebugLocation(inlineLoc);
    for(i=0; i < stmtslen; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_linenode(stmt) ||
            (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym)) {

            jl_sym_t *file = NULL;
            if (jl_is_linenode(stmt)) {
                lno = jl_linenode_line(stmt);
                file = jl_linenode_file(stmt);
            }
            else if (jl_is_expr(stmt)) {
                lno = jl_unbox_long(jl_exprarg(stmt,0));
                if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
                    jl_value_t *a1 = jl_exprarg(stmt,1);
                    if (jl_is_symbol(a1)) {
                        file = (jl_sym_t*)a1;
                    }
                }
            }
            assert(jl_symbol_name(file));

#           ifdef LLVM37
            DIFile *dfil = NULL;
#           else
            MDNode *dfil = NULL;
#           endif

            // If the string is not empty
            if (*jl_symbol_name(file) != '\0') {
#               ifdef LLVM37
                std::map<jl_sym_t *, DIFile *>::iterator it = filescopes.find(file);
#               else
                std::map<jl_sym_t *, MDNode *>::iterator it = filescopes.find(file);
#               endif
                if (it != filescopes.end()) {
                    dfil = it->second;
                }
                else {
#                   ifdef LLVM37
                    dfil = (DIFile*)dbuilder.createFile(jl_symbol_name(file),
                                                        ".");
#                   else
                    dfil = (MDNode*)dbuilder.createFile(jl_symbol_name(file),
                                                        ".");
#                   endif
                }
            }
            DebugLoc loc;
            if (ctx.debug_enabled) {
                MDNode *scope;
                if ((dfil == topfile || dfil == NULL) &&
                    lno >= toplineno)
                    {
                    // for sequentially-defined code,
                    // set location to line in top file.
                    // TODO: improve handling of nested inlines
                    loc = inlineLoc = DebugLoc::get(lno, 1, SP, NULL);
                }
                else {
                    // otherwise, we are compiling inlined code,
                    // so set the DebugLoc "inlinedAt" parameter
                    // to the current line, then use source loc.
#ifdef LLVM37
                    scope = (MDNode*)dbuilder.createLexicalBlockFile(SP,dfil);
                    MDNode *inlineLocMd = inlineLoc.getAsMDNode();
#else
                    scope = (MDNode*)dbuilder.createLexicalBlockFile(SP,DIFile(dfil));
                    MDNode *inlineLocMd = inlineLoc.getAsMDNode(jl_LLVMContext);
#endif
                    loc = DebugLoc::get(lno, 1, scope, inlineLocMd);
                }
                builder.SetCurrentDebugLocation(loc);
            }
            if (do_coverage)
                coverageVisitLine(filename, lno);
        }
        if (jl_is_labelnode(stmt)) {
            if (prevlabel) continue;
            prevlabel = true;
        }
        else {
            prevlabel = false;
        }
        if (do_malloc_log) {
            // Check memory allocation after finishing a line or hitting the next branch
            if (lno != prevlno ||
                (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == goto_ifnot_sym) ||
                jl_is_gotonode(stmt)) {
                if (prevlno != -1)
                    mallocVisitLine(filename, prevlno);
                prevlno = lno;
            }
        }
        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == return_sym) {
            jl_expr_t *ex = (jl_expr_t*)stmt;
            Value *retval;
            bool retboxed;
            Type *retty;
            if (specsig) {
                retty = julia_type_to_llvm(jlrettype, &retboxed);
            }
            else {
                retty = T_pjlvalue;
                retboxed = true;
            }
            jl_cgval_t retvalinfo = emit_expr(jl_exprarg(ex,0), &ctx);
            if (retboxed)
                retval = boxed(retvalinfo, &ctx, false); // skip the gcroot on the return path
            else if (!type_is_ghost(retty))
                retval = emit_unbox(retty, retvalinfo, jlrettype);
            else // undef return type
                retval = NULL;
            if (do_malloc_log && lno != -1)
                mallocVisitLine(filename, lno);
            if (ctx.sret)
                builder.CreateStore(retval, &*ctx.f->arg_begin());
            if (type_is_ghost(retty) || ctx.sret)
                builder.CreateRetVoid();
            else
                builder.CreateRet(retval);
            if (i != stmtslen-1) {
                BasicBlock *bb =
                    BasicBlock::Create(getGlobalContext(), "ret", ctx.f);
                builder.SetInsertPoint(bb);
            }
        }
        else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == boundscheck_sym) {
            // always emit expressions that update the boundscheck stack
            emit_stmtpos(stmt, &ctx);
        }
        else if (is_inbounds(&ctx) && is_bounds_check_block(&ctx)) {
            // elide bounds check blocks
        }
        else {
            emit_stmtpos(stmt, &ctx);
        }
    }

    builder.SetCurrentDebugLocation(noDbg);

    // sometimes we have dangling labels after the end
    if (builder.GetInsertBlock()->getTerminator() == NULL) {
        builder.CreateUnreachable();
    }

    // patch up dangling BasicBlocks from skipped labels
    for (std::map<int,BasicBlock*>::iterator it = labels.begin(); it != labels.end(); ++it) {
        if (it->second->getTerminator() == NULL) {
            builder.SetInsertPoint(it->second);
            builder.CreateUnreachable();
        }
    }

    // step 14, Apply LLVM level inlining
    for(std::vector<CallInst*>::iterator it = ctx.to_inline.begin(); it != ctx.to_inline.end(); ++it) {
        Function *inlinef = (*it)->getCalledFunction();
        InlineFunctionInfo info;
        if (!InlineFunction(*it,info))
            jl_error("Inlining Pass failed");
        if (inlinef->getParent())
            inlinef->eraseFromParent();
        else {
            inlinef->dropAllReferences();
            delete inlinef;
        }
    }

    // step 15. Perform any delayed instantiations
    if (ctx.debug_enabled) {
        ctx.dbuilder->finalize();
    }

    JL_GC_POP();

    return std::unique_ptr<Module>(M);
}

// --- initialization ---

static MDNode *tbaa_make_child( const char *name, MDNode *parent, bool isConstant=false )
{
    MDNode *n = mbuilder->createTBAANode(name,parent,isConstant);
#ifndef LLVM36
#ifdef LLVM35
    n->setValueName( ValueName::Create(name));
#else
    n->setValueName( ValueName::Create(name, name+strlen(name)));
#endif
#endif
    return n;
}

static GlobalVariable *global_to_llvm(const std::string &cname, void *addr, Module *m)
{
    GlobalVariable *gv =
        new GlobalVariable(*m, T_pjlvalue, true,
                           GlobalVariable::ExternalLinkage, NULL, cname);
    add_named_global(gv, addr);
    return gv;
}

static Function *jlcall_func_to_llvm(const std::string &cname, jl_fptr_t addr, Module *m)
{
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage, cname, m);
    add_named_global(f, addr);
    return f;
}

extern "C" void jl_fptr_to_llvm(jl_fptr_t fptr, jl_lambda_info_t *lam, int specsig)
{
    if (imaging_mode) {
        if (!specsig) {
            lam->fptr = fptr; // in imaging mode, it's fine to use the fptr, but we don't want it in the shadow_module
        }
    }
    else {
        // this assigns a function pointer (from loading the system image), to the function object
        std::stringstream funcName;
        funcName << "jlsys_" << jl_symbol_name(lam->name) << "_" << globalUnique++;
        if (specsig) { // assumes !va
            std::vector<Type*> fsig(0);
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
            for (size_t i=0; i < jl_nparams(lam->specTypes); i++) {
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

            if (lam->functionObjects.specFunctionObject == NULL) {
                lam->functionObjects.specFunctionObject = (void*)f;
            }
            add_named_global(f, fptr);
        }
        else {
            if (lam->jlcall_api == 1) { // jl_func_sig_sparams -- don't bother emitting the FunctionObject (since can't be used right now)
                assert(lam->fptr == NULL);
                lam->fptr = fptr;
            }
            else {
                Function *f = jlcall_func_to_llvm(funcName.str(), fptr, shadow_output);
                if (lam->functionObjects.functionObject == NULL) {
                    lam->functionObjects.functionObject = (void*)f;
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

static void init_julia_llvm_env(Module *m)
{
    MDNode *tbaa_root = mbuilder->createTBAARoot("jtbaa");
    tbaa_user = tbaa_make_child("jtbaa_user",tbaa_root);
    tbaa_value = tbaa_make_child("jtbaa_value",tbaa_root);
    tbaa_immut = tbaa_make_child("jtbaa_immut",tbaa_root);
    tbaa_array = tbaa_make_child("jtbaa_array",tbaa_value);
    tbaa_arrayptr = tbaa_make_child("jtbaa_arrayptr",tbaa_array);
    tbaa_arraysize = tbaa_make_child("jtbaa_arraysize",tbaa_array);
    tbaa_arraylen = tbaa_make_child("jtbaa_arraylen",tbaa_array);
    tbaa_arrayflags = tbaa_make_child("jtbaa_arrayflags",tbaa_array);
    tbaa_sveclen = tbaa_make_child("jtbaa_sveclen",tbaa_value);
    tbaa_func = tbaa_make_child("jtbaa_func",tbaa_value);
    tbaa_datatype = tbaa_make_child("jtbaa_datatype",tbaa_value);
    tbaa_const = tbaa_make_child("jtbaa_const",tbaa_root,true);

    // every variable or function mapped in this function must be
    // exported from libjulia, to support static compilation
    T_int1  = Type::getInt1Ty(getGlobalContext());
    T_int8  = Type::getInt8Ty(getGlobalContext());
    T_pint8 = PointerType::get(T_int8, 0);
    T_ppint8 = PointerType::get(T_pint8, 0);
    T_pppint8 = PointerType::get(T_ppint8, 0);
    T_int16 = Type::getInt16Ty(getGlobalContext());
    T_pint16 = PointerType::get(T_int16, 0);
    T_int32 = Type::getInt32Ty(getGlobalContext());
    T_char = Type::getInt32Ty(getGlobalContext());
    T_pint32 = PointerType::get(T_int32, 0);
    T_int64 = Type::getInt64Ty(getGlobalContext());
    T_pint64 = PointerType::get(T_int64, 0);
    T_uint8 = T_int8;   T_uint16 = T_int16;
    T_uint32 = T_int32; T_uint64 = T_int64;
    if (sizeof(size_t) == 8)
        T_size = T_uint64;
    else
        T_size = T_uint32;
    T_psize = PointerType::get(T_size, 0);
    T_float16 = Type::getHalfTy(getGlobalContext());
    T_float32 = Type::getFloatTy(getGlobalContext());
    T_pfloat32 = PointerType::get(T_float32, 0);
    T_float64 = Type::getDoubleTy(getGlobalContext());
    T_pfloat64 = PointerType::get(T_float64, 0);
    T_float128 = Type::getFP128Ty(getGlobalContext());
    T_void = Type::getVoidTy(jl_LLVMContext);
    T_pvoidfunc = FunctionType::get(T_void, /*isVarArg*/false)->getPointerTo();

    // This type is used to create undef Values for use in struct declarations to skip indices
    NoopType = ArrayType::get(T_int1, 0);

    // add needed base definitions to our LLVM environment
    StructType *valueSt = StructType::create(getGlobalContext(), "jl_value_t");
    Type *valueStructElts[1] = { PointerType::getUnqual(valueSt) };
    ArrayRef<Type*> vselts(valueStructElts);
    valueSt->setBody(vselts);
    T_jlvalue = valueSt;

    DIBuilder dbuilder(*m);
#ifdef LLVM37
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
        0, // Flags
#ifdef LLVM37
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later
#else
        DIType(), // Derived from
        DIArray()); // Elements - will be corrected later
#endif

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt, sizeof(jl_value_t*) * 8,
                                                   __alignof__(jl_value_t*) * 8);

#ifdef LLVM36
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

#ifdef LLVM38
    jl_di_func_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(diargs));
#elif defined(LLVM36)
    jl_di_func_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateTypeArray(diargs));
#else
    jl_di_func_sig = dbuilder.createSubroutineType(julia_h,
        dbuilder.getOrCreateArray(diargs));
#endif

    T_pjlvalue = PointerType::get(T_jlvalue, 0);
    T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
    two_pvalue_llvmt.push_back(T_pjlvalue);
    two_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    three_pvalue_llvmt.push_back(T_pjlvalue);
    V_null = Constant::getNullValue(T_pjlvalue);

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

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true, m);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false, m);
    jlemptysvec_var = global_to_llvm("jl_emptysvec", (void*)&jl_emptysvec, m);
    jlemptytuple_var = global_to_llvm("jl_emptytuple", (void*)&jl_emptytuple, m);
    jldiverr_var = global_to_llvm("jl_diverror_exception",
                                  (void*)&jl_diverror_exception, m);
    jlundeferr_var = global_to_llvm("jl_undefref_exception",
                                    (void*)&jl_undefref_exception, m);
    jldomerr_var = global_to_llvm("jl_domain_exception",
                                  (void*)&jl_domain_exception, m);
    jlovferr_var = global_to_llvm("jl_overflow_exception",
                                  (void*)&jl_overflow_exception, m);
    jlinexacterr_var = global_to_llvm("jl_inexact_exception",
                                      (void*)&jl_inexact_exception, m);

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
    // In imaging mode, we emit the function address as a load of a static
    // variable to be filled (in `dump.c`) at initialization time of the sysimg.
    // This way we can by pass the extra indirection in `jl_get_ptls_states`
    // since we don't know which getter function to use ahead of time.
    jltls_states_func = Function::Create(FunctionType::get(PointerType::get(T_ppjlvalue, 0), false),
                                         Function::ExternalLinkage,
                                         "jl_get_ptls_states", m);
    jltls_states_func->setAttributes(
        jltls_states_func->getAttributes()
        .addAttribute(jltls_states_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::ReadNone)
        .addAttribute(jltls_states_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::NoUnwind));
    add_named_global(jltls_states_func, jl_get_ptls_states_getter());
    if (imaging_mode) {
        PointerType *pfunctype = jltls_states_func->getFunctionType()->getPointerTo();
        jltls_states_func_ptr =
            new GlobalVariable(*m, pfunctype,
                               false, GlobalVariable::ExternalLinkage,
                               NULL, "jl_get_ptls_states.ptr");
        addComdat(jltls_states_func_ptr);
        void **p = (void**)jl_emit_and_add_to_shadow(jltls_states_func_ptr);
        *p = (void*)jl_get_ptls_states_getter();
        jl_sysimg_gvars.push_back(ConstantExpr::getBitCast(jltls_states_func_ptr,
                                                           T_psize));
        jltls_states_func_idx = jl_sysimg_gvars.size();
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

    jlpref_func = Function::Create(FunctionType::get(T_pjlvalue, two_pvalue_llvmt, false),
                            Function::ExternalLinkage,
                            "jl_pointerref", m);

    jlpset_func = Function::Create(FunctionType::get(T_pjlvalue, three_pvalue_llvmt, false),
                            Function::ExternalLinkage,
                            "jl_pointerset", m);


    builtin_func_map[jl_f_is] = jlcall_func_to_llvm("jl_f_is", &jl_f_is, m);
    builtin_func_map[jl_f_typeof] = jlcall_func_to_llvm("jl_f_typeof", &jl_f_typeof, m);
    builtin_func_map[jl_f_sizeof] = jlcall_func_to_llvm("jl_f_sizeof", &jl_f_sizeof, m);
    builtin_func_map[jl_f_issubtype] = jlcall_func_to_llvm("jl_f_issubtype", &jl_f_issubtype, m);
    builtin_func_map[jl_f_isa] = jlcall_func_to_llvm("jl_f_isa", &jl_f_isa, m);
    builtin_func_map[jl_f_typeassert] = jlcall_func_to_llvm("jl_f_typeassert", &jl_f_typeassert, m);
    builtin_func_map[jl_f__apply] = jlcall_func_to_llvm("jl_f__apply", &jl_f__apply, m);
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

    std::vector<Type *> subt_args(0);
    subt_args.push_back(T_pjlvalue);
    subt_args.push_back(T_pjlvalue);
    subt_args.push_back(T_int32);
    jlsubtype_func =
        Function::Create(FunctionType::get(T_int32, subt_args, false),
                         Function::ExternalLinkage,
                         "jl_subtype", m);
    add_named_global(jlsubtype_func, &jl_subtype);

    std::vector<Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(T_pjlvalue, aoargs, false),
                         Function::ExternalLinkage,
                         "jl_gc_allocobj", m);
    add_named_global(jlallocobj_func, &jl_gc_allocobj);

    std::vector<Type*> empty_args(0);
    jlalloc1w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_1w", m);
    add_named_global(jlalloc1w_func, &jl_gc_alloc_1w);

    jlalloc2w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_2w", m);
    add_named_global(jlalloc2w_func, &jl_gc_alloc_2w);

    jlalloc3w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_3w", m);
    add_named_global(jlalloc3w_func, &jl_gc_alloc_3w);

    std::vector<Type*> atargs(0);
    atargs.push_back(T_size);
    jl_alloc_svec_func =
        Function::Create(FunctionType::get(T_pjlvalue, atargs, false),
                         Function::ExternalLinkage,
                         "jl_alloc_svec", m);
    add_named_global(jl_alloc_svec_func, &jl_alloc_svec);

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

#ifndef LLVM36
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
                     "julia.gc_root_decl", m);
    add_named_global(gcroot_func, (void*)NULL, /*dllimport*/false);

    gckill_func =
        Function::Create(FunctionType::get(T_void, ArrayRef<Type*>(T_ppjlvalue), false),
                     Function::ExternalLinkage,
                     "julia.gc_root_kill", m);
    add_named_global(gckill_func, (void*)NULL, /*dllimport*/false);

    Type* gc_store_args[2] = { T_ppjlvalue, T_pjlvalue }; // [1] <= [2]
    gcstore_func =
        Function::Create(FunctionType::get(T_void, makeArrayRef(gc_store_args),  false),
                     Function::ExternalLinkage,
                     "julia.gc_store", m);
    add_named_global(gcstore_func, (void*)NULL, /*dllimport*/false);

    jlcall_frame_func =
        Function::Create(FunctionType::get(T_ppjlvalue, ArrayRef<Type*>(T_int32), false),
                     Function::ExternalLinkage,
                     "julia.jlcall_frame_decl", m);
    add_named_global(jlcall_frame_func, (void*)NULL, /*dllimport*/false);

    // set up optimization passes
#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif defined(LLVM36)
    jl_data_layout = new llvm::DataLayoutPass();
#elif defined(LLVM35)
    jl_data_layout = new llvm::DataLayoutPass(*jl_ExecutionEngine->getDataLayout());
#else
    jl_data_layout = new DataLayout(*jl_ExecutionEngine->getDataLayout());
#endif

#ifndef USE_ORCJIT
#ifdef LLVM38
    PM = new legacy::PassManager();
#else
    PM = new PassManager();
#endif
#ifndef LLVM37
    PM->add(new TargetLibraryInfo(Triple(jl_TargetMachine->getTargetTriple())));
#else
    PM->add(new TargetLibraryInfoWrapperPass(Triple(jl_TargetMachine->getTargetTriple())));
#endif
#ifndef LLVM37
    PM->add(jl_data_layout);
#endif
    addOptimizationPasses(PM);
#endif
}

// Helper to figure out what features to set for the LLVM target
// If the user specifies native (or does not specify) we default
// using the API provided by LLVM
static inline SmallVector<std::string,10> getTargetFeatures() {
    StringMap<bool> HostFeatures;
    if (!strcmp(jl_options.cpu_target,"native"))
    {
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
#if defined(_CPU_X86_64_) && defined(LLVM36)
    // Require cx16 (cmpxchg16b)
    // We need this for 128-bit atomic operations. We only need this
    // when threading is enabled; however, to test whether this
    // excludes important systems, we require this even when threading
    // is disabled.
    HostFeatures["cx16"] = true;
#endif

    // Figure out if we know the cpu_target
    std::string cpu = strcmp(jl_options.cpu_target,"native") ? jl_options.cpu_target : sys::getHostCPUName();
    if (cpu.empty() || cpu == "generic") {
        jl_printf(JL_STDERR, "WARNING: unable to determine host cpu name.\n");
#if defined(_CPU_ARM_) && defined(__ARM_PCS_VFP)
        // Check if this is required when you have read the features directly from the processor
        // This affects the platform calling convention.
        // TODO: enable vfp3 for ARMv7+ (but adapt the ABI)
        HostFeatures["vfp2"] = true;
#endif
    }

    SmallVector<std::string,10> attr;
    for (StringMap<bool>::const_iterator it = HostFeatures.begin(); it != HostFeatures.end(); it++)
    {
        std::string att = it->getValue() ? it->getKey().str() :
                          std::string("-") + it->getKey().str();
        attr.append(1, att);
    }
    return attr;
}

extern "C" void jl_init_debuginfo(void);

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

#if defined(_CPU_PPC_) || defined(_CPU_PPC64_)
    imaging_mode = true; // LLVM seems to JIT bad TOC tables for the optimizations we attempt in non-imaging_mode
#else
    imaging_mode = jl_generating_output();
#endif
    jl_init_debuginfo();

#ifndef LLVM34
    // this option disables LLVM's signal handlers
    llvm::DisablePrettyStackTrace = true;
#endif

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

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
#if defined(JL_DEBUG_BUILD) && !defined(LLVM37)
    options.JITEmitDebugInfo = true;
#endif
#ifndef LLVM37
    options.NoFramePointerElim = true;
#endif
#ifndef LLVM34
    options.NoFramePointerElimNonLeaf = true;
#endif
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to assume the stack is always 16-byte aligned,
    // and to ensure that it is 16-byte aligned for out-going calls,
    // to ensure compatibility with GCC codes
    options.StackAlignmentOverride = 16;
#endif
#if defined(__APPLE__) && !defined(LLVM34)
    // turn on JIT support for libunwind to walk the stack
    options.JITExceptionHandling = 1;
#endif

#ifdef LLVM36
    EngineBuilder eb((std::unique_ptr<Module>(engine_module)));
#else
    EngineBuilder eb(engine_module);
#endif
    std::string ErrorStr;
    eb  .setEngineKind(EngineKind::JIT)
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && !defined(USE_MCJIT)
        .setJITMemoryManager(createJITMemoryManagerWin())
#elif defined(CUSTOM_MEMORY_MANAGER)
        .setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>{CUSTOM_MEMORY_MANAGER()})
#elif defined(USE_ORCMCJIT) // ORCJIT forgets to create one if one isn't created for it
        .setMCJITMemoryManager(std::unique_ptr<RTDyldMemoryManager>{new SectionMemoryManager()})
#endif
        .setTargetOptions(options)
#if (defined(_OS_LINUX_) && defined(_CPU_X86_64_)) || defined(CODEGEN_TLS)
        .setRelocationModel(Reloc::PIC_)
#else
        .setRelocationModel(Reloc::Default)
#endif
#ifdef CODEGEN_TLS
        .setCodeModel(CodeModel::Small)
#else
        .setCodeModel(CodeModel::JITDefault)
#endif
#ifdef DISABLE_OPT
        .setOptLevel(CodeGenOpt::None)
#else
        .setOptLevel(CodeGenOpt::Aggressive)
#endif
#if defined(USE_MCJIT) && !defined(LLVM36)
        .setUseMCJIT(true)
#endif
#ifdef USE_ORCMCJIT
        .setUseOrcMCJITReplacement(true)
#endif
    ;
    Triple TheTriple(sys::getProcessTriple());
#if defined(FORCE_ELF)
#ifdef LLVM35
    TheTriple.setObjectFormat(Triple::ELF);
#else
    TheTriple.setEnvironment(Triple::ELF);
#endif
#endif
    std::string TheCPU = strcmp(jl_options.cpu_target,"native") ? jl_options.cpu_target : sys::getHostCPUName();
    SmallVector<std::string, 10>  targetFeatures = getTargetFeatures( );
    jl_TargetMachine = eb.selectTarget(
            TheTriple,
            "",
            TheCPU,
            targetFeatures);
    assert(jl_TargetMachine && "Failed to select target machine -"
                               " Is the LLVM backend for this CPU enabled?");
#if defined(USE_MCJIT) && !defined(_CPU_ARM_)
    // FastISel seems to be buggy for ARM. Ref #13321
    jl_TargetMachine->setFastISel(true);
#endif

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
#if defined(LLVM35) && !defined(USE_ORCMCJIT)
    jl_ExecutionEngine->setProcessAllSections(true);
#endif
    jl_ExecutionEngine->DisableLazyCompilation();
#endif

    mbuilder = new MDBuilder(getGlobalContext());

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
    UBOX_F(gensym,size);

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
