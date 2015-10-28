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
#ifdef USE_MCJIT
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/ADT/DenseMapInfo.h>
#include <llvm/Object/ObjectFile.h>
#else
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/JITMemoryManager.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#endif
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
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Transforms/Utils/Cloning.h>

#if defined(_OS_WINDOWS_) && !defined(NOMINMAX)
#define NOMINMAX
#endif

#include "julia.h"
#include "julia_internal.h"

#include <setjmp.h>

#include <string>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <set>
#include <cstdio>
#include <cassert>
using namespace llvm;

// LLVM version compatibility macros
#if LLVM37
using namespace llvm::legacy;
#define LLVM37_param(x) (x),
#else
#define LLVM37_param(x)
#endif

#ifndef LLVM35
#define AddrSpaceCastInst BitCastInst
#endif

extern "C" {

#include "builtin_proto.h"

#ifdef HAVE_SSP
extern uintptr_t __stack_chk_guard;
extern void __stack_chk_fail();
#else
DLLEXPORT uintptr_t __stack_chk_guard = (uintptr_t)0xBAD57ACCBAD67ACC; // 0xBADSTACKBADSTACK
DLLEXPORT void __stack_chk_fail()
{
    /* put your panic function or similar in here */
    fprintf(stderr, "fatal error: stack corruption detected\n");
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
DLLEXPORT LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static bool nested_compile=false;
DLLEXPORT ExecutionEngine *jl_ExecutionEngine;
DLLEXPORT TargetMachine *jl_TargetMachine;
#ifdef USE_MCJIT
static Module *shadow_module;
DLLEXPORT RTDyldMemoryManager *jl_mcjmm;
#define jl_Module (builder.GetInsertBlock()->getParent()->getParent())
#else
static Module *jl_Module;
#define shadow_module jl_Module
#endif
static MDBuilder *mbuilder;
static std::map<int, std::string> argNumberStrings;
#ifdef LLVM38
static legacy::FunctionPassManager *FPM;
#else
static FunctionPassManager *FPM;
#endif

#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif LLVM35
static DataLayoutPass *jl_data_layout;
#else
static DataLayout *jl_data_layout;
#endif

// for image reloading
static bool imaging_mode = false;

// types
static Type *T_jlvalue;
static Type *T_pjlvalue;
static Type *T_ppjlvalue;
static Type* jl_parray_llvmt;
static FunctionType *jl_func_sig;
static Type *jl_pfptr_llvmt;

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

static Type *T_float32;
static Type *T_float64;

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
static MDNode* tbaa_user;           // User data that is mutable
static MDNode* tbaa_immut;          // User data inside a heap-allocated immutable
static MDNode* tbaa_value;          // Julia value
static MDNode* tbaa_array;              // Julia array
static MDNode* tbaa_arrayptr;               // The pointer inside a jl_array_t
static MDNode* tbaa_arraysize;              // A size in a jl_array_t
static MDNode* tbaa_arraylen;               // The len in a jl_array_t
static MDNode* tbaa_sveclen;           // The len in a jl_svec_t
static MDNode* tbaa_func;           // A jl_function_t
static MDNode* tbaa_datatype;       // A jl_datatype_t
static MDNode* tbaa_const;          // Memory that is immutable by the time LLVM can see it

namespace llvm {
    extern Pass *createLowerSimdLoopPass();
    extern bool annotateSimdLoop( BasicBlock* latch );
}

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
DLLEXPORT Type *julia_type_to_llvm(jl_value_t *jt, bool *isboxed=NULL);
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
#if defined(_CPU_X86_)
#define JL_NEED_FLOATTEMP_VAR 1
#endif
#if JL_NEED_FLOATTEMP_VAR
static GlobalVariable *jlfloattemp_var;
#endif
static GlobalVariable *jlpgcstack_var;
static GlobalVariable *jlexc_var;
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
extern JITMemoryManager* createJITMemoryManagerWin();
#endif
#endif //_OS_WINDOWS_
#if defined(_OS_DARWIN_) && defined(LLVM37) && defined(LLVM_SHLIB)
#define CUSTOM_MEMORY_MANAGER 1
extern RTDyldMemoryManager* createRTDyldMemoryManagerOSX();
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
static Function *jlgetfield_func;
static Function *jlbox_func;
static Function *jlclosure_func;
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
static Function *wbfunc;
static Function *queuerootfun;
static Function *expect_func;
static Function *jldlsym_func;
static Function *jlnewbits_func;
//static Function *jlgetnthfield_func;
static Function *jlgetnthfieldchecked_func;
//static Function *jlsetnthfield_func;
#ifdef _OS_WINDOWS_
static Function *resetstkoflw_func;
#endif
static Function *diff_gc_total_bytes_func;

static std::vector<Type *> two_pvalue_llvmt;
static std::vector<Type *> three_pvalue_llvmt;

static std::map<jl_fptr_t, Function*> builtin_func_map;

extern "C" DLLEXPORT void jl_gc_wb_slow(jl_value_t* parent, jl_value_t* ptr)
{
    jl_gc_wb(parent, ptr);
}

// --- code generation ---

// metadata tracking for a llvm Value* during codegen
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    jl_value_t *typ; // the original type of V, never NULL
    //Type *T; // cached result of julia_type_to_llvm(typ)
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    bool ispointer; // whether this value is actually pointer to the value
    bool isimmutable; // V points to something that is definitely immutable (e.g. not stack allocated)
    //bool isstack; // points to stack-allocated memory
    //bool isarg; // derived from an argument
    mutable bool needsgcroot; // this value needs a gcroot
    jl_cgval_t(Value *V, bool isboxed, jl_value_t *typ) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        typ(typ),
        //T(julia_type_to_llvm(typ)),
        isboxed(isboxed),
        isghost(false),
        ispointer(this->isboxed),
        isimmutable(this->isboxed && jl_is_immutable_datatype(typ)),
        needsgcroot(this->isboxed)
    {
    }
    jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        V(NULL),
        typ(typ),
        //T(T_void),
        isboxed(false),
        isghost(true),
        ispointer(false),
        isimmutable(true),
        needsgcroot(false)
    {
    }
    jl_cgval_t(const jl_cgval_t &v, jl_value_t *typ) : // copy constructor with new type
        V(v.V),
        typ(typ),
        //T(V.T),
        isboxed(v.isboxed),
        isghost(v.isghost),
        ispointer(v.ispointer),
        isimmutable(v.isimmutable),
        needsgcroot(v.needsgcroot)
    {
        assert(isboxed || v.typ == typ); // expect a badly or equivalently typed version
    }
    jl_cgval_t() : // undef / unreachable / default constructor
        V(UndefValue::get(T_void)),
        typ(jl_bottom_type), // TODO: jl_bottom_type?
        isboxed(false),
        isghost(true),
        ispointer(false),
        isimmutable(true),
        needsgcroot(false)
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
    int closureidx;   // index in closure env, or -1
    bool isAssigned;
    bool isCaptured;
    bool isSA;
    bool isVolatile;
    bool isArgument;
    bool isBox;
    bool hasGCRoot;
    bool escapes;
    bool usedUndef;
    bool used;

    jl_varinfo_t() : memloc(NULL), value(jl_cgval_t()),
#ifdef LLVM37
                     dinfo(NULL),
#else
                     dinfo(DIVariable()),
#endif
                     closureidx(-1), isAssigned(true), isCaptured(false), isSA(false),
                     isVolatile(false), isArgument(false), isBox(false), hasGCRoot(false),
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

struct jl_gcinfo_t {
    AllocaInst *gcframe;
    Value *argSlot;
    GetElementPtrInst *tempSlot;
    int argDepth;
    int maxDepth;
    int argSpaceSize;
    BasicBlock::iterator first_gcframe_inst;
    BasicBlock::iterator last_gcframe_inst;
};

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    // local var info. globals are not in here.
    // NOTE: you must be careful not to access vars[s] before you are sure "s" is
    // a local, since otherwise this will add it to the map.
    std::map<jl_sym_t*, jl_varinfo_t> vars;
    std::vector<jl_cgval_t> gensym_SAvalues;
    std::vector<bool> gensym_assigned;
    std::map<jl_sym_t*, jl_arrayvar_t> *arrayvars;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *handlers;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_svec_t *sp;
    jl_lambda_info_t *linfo;
    Value *argArray;
    Value *argCount;
    std::string funcName;
    jl_sym_t *vaName;  // name of vararg argument
    bool vaStack;      // varargs stack-allocated
    bool sret;
    int nReqArgs;
    std::vector<bool> boundsCheck;

    jl_gcinfo_t gc;

    llvm::DIBuilder *dbuilder;
    bool debug_enabled;
    std::vector<CallInst*> to_inline;
} jl_codectx_t;

typedef struct {
    size_t len;
    struct {
        int64_t isref;
        Function *f;
    } data[];
} cFunctionList_t;

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool boxed=true,
                        bool valuepos=true);
static jl_cgval_t emit_unboxed(jl_value_t *e, jl_codectx_t *ctx);
static int is_global(jl_sym_t *s, jl_codectx_t *ctx);

static Value *make_gcroot(Value *v, jl_codectx_t *ctx);
static jl_cgval_t emit_boxed_rooted(jl_value_t *e, jl_codectx_t *ctx);
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign, jl_codectx_t *ctx);
static jl_cgval_t emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol=false);
static bool might_need_root(jl_value_t *ex);
static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx);
static void allocate_gc_frame(size_t n_roots, BasicBlock *b0, jl_codectx_t *ctx);
static void finalize_gc_frame(jl_codectx_t *ctx);

// --- convenience functions for tagging llvm values with julia types ---

static AllocaInst *emit_static_alloca(Type *lty, int arraysize, jl_codectx_t *ctx)
{
    return new AllocaInst(lty, ConstantInt::get(T_int32, arraysize), "", /*InsertBefore=*/ctx->gc.gcframe);
}
static AllocaInst *emit_static_alloca(Type *lty, jl_codectx_t *ctx)
{
    return emit_static_alloca(lty, 1, ctx);
}
static AllocaInst *emit_static_alloca(Type *lty)
{
    return new AllocaInst(lty, "",
            /*InsertBefore=*/builder.GetInsertBlock()->getParent()->getEntryBlock().getFirstInsertionPt());
}

static inline jl_cgval_t ghostValue(jl_value_t *typ)
{
    assert(typ == jl_bottom_type || (jl_is_datatype(typ) && jl_datatype_size(typ) == 0));
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
    jl_cgval_t tagval(v, false, typ);
    tagval.ispointer = true;
    return tagval;
}

static inline jl_cgval_t mark_julia_type(Value *v, bool isboxed, jl_value_t *typ)
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
    return jl_cgval_t(v, isboxed, typ);
}
static inline jl_cgval_t mark_julia_type(Value *v, bool isboxed, jl_datatype_t *typ)
{
    return mark_julia_type(v, isboxed, (jl_value_t*)typ);
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
    jl_cgval_t constant(literal_pointer_val(jv), true, typ);
    constant.needsgcroot = false;
    return constant;
}


// --- utilities ---

extern "C" {
    int globalUnique = 0;
}

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

static bool store_unboxed_p(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->vars[s];
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->linfo->inferred && !vi.isCaptured && !vi.usedUndef &&
            // don't unbox vararg tuples
            s != ctx->vaName && store_unboxed_p(vi.value.typ));
}

static void alloc_local(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->vars[s];
    jl_value_t *jt = vi.value.typ;
    assert(store_unboxed_p(s,ctx));
    Type *vtype = julia_type_to_llvm(jt);
    assert(vtype != T_pjlvalue);
    if (type_is_ghost(vtype)) {
        vi.value = ghostValue(jt);
        return;
    }
    // CreateAlloca is OK here because alloc_local is only called during prologue setup
    Value *lv = builder.CreateAlloca(vtype, 0, s->name);
    vi.value = mark_julia_slot(lv, jt);
    vi.value.isimmutable &= vi.isSA; // slot is not immutable if there are multiple assignments
    assert(vi.value.isboxed == false);
#ifdef LLVM36
    if (ctx->debug_enabled) {
#ifdef LLVM37
        ctx->dbuilder->insertDeclare(lv, vi.dinfo, ctx->dbuilder->createExpression(),
                builder.getCurrentDebugLocation().get(), builder.GetInsertBlock());
#else
        ctx->dbuilder->insertDeclare(lv, vi.dinfo, ctx->dbuilder->createExpression(),
                builder.GetInsertBlock());
#endif
    }
#endif
}

static void maybe_alloc_arrayvar(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_value_t *jt = ctx->vars[s].value.typ;
    if (jl_is_array_type(jt) && jl_is_leaf_type(jt) && jl_is_long(jl_tparam1(jt)) &&
        jl_unbox_long(jl_tparam1(jt)) != 1) {
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

JL_DEFINE_MUTEX_EXT(codegen)

// Snooping on which functions are being compiled, and how long it takes
JL_STREAM *dump_compiles_stream = NULL;
uint64_t last_time = 0;
extern "C" DLLEXPORT
void jl_dump_compiles(void *s)
{
    dump_compiles_stream = (JL_STREAM*)s;
}

// --- entry point ---
//static int n_emit=0;
static Function *emit_function(jl_lambda_info_t *lam);
//static int n_compile=0;
static Function *to_function(jl_lambda_info_t *li)
{
    JL_LOCK(codegen)
    JL_SIGATOMIC_BEGIN();
    assert(!li->inInference);
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    if (!nested_compile && dump_compiles_stream != NULL)
        last_time = jl_hrtime();
    nested_compile = true;
    jl_gc_inhibit_finalizers(nested_compile);
    Function *f = NULL;
    JL_TRY {
        f = emit_function(li);
        //n_emit++;
    }
    JL_CATCH {
        li->functionObject = NULL;
        li->specFunctionObject = NULL;
        li->cFunctionList = NULL;
        nested_compile = last_n_c;
        if (old != NULL) {
            builder.SetInsertPoint(old);
            builder.SetCurrentDebugLocation(olddl);
        }
        JL_SIGATOMIC_END();
        JL_UNLOCK(codegen)
        jl_rethrow_with_add("error compiling %s", li->name->name);
    }
    assert(f != NULL);
#ifdef JL_DEBUG_BUILD
#ifdef LLVM35
    llvm::raw_fd_ostream out(1,false);
#endif
    if (
#ifdef LLVM35
        verifyFunction(*f,&out)
#else
        verifyFunction(*f,PrintMessageAction)
#endif
        ) {
        f->dump();
        abort();
    }
#endif
    FPM->run(*f);
    //n_compile++;
    // print out the function's LLVM code
    //jl_static_show(JL_STDERR, (jl_value_t*)li);
    //jl_printf(JL_STDERR, "%s:%d\n",
    //           ((jl_sym_t*)li->file)->name, li->line);
    //f->dump();
    //if (verifyFunction(*f,PrintMessageAction)) {
    //    f->dump();
    //    abort();
    //}
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    nested_compile = last_n_c;
    jl_gc_inhibit_finalizers(nested_compile);
    JL_UNLOCK(codegen)
    JL_SIGATOMIC_END();
    if (dump_compiles_stream != NULL) {
        uint64_t this_time = jl_hrtime();
        jl_printf(dump_compiles_stream, "%llu\t\"", (unsigned long long)(this_time-last_time));
        jl_static_show(dump_compiles_stream, (jl_value_t*)li);
        jl_printf(dump_compiles_stream, "\"\n");
        last_time = this_time;
    }
    return f;
}

static void jl_setup_module(Module *m, bool add)
{
    m->addModuleFlag(llvm::Module::Warning, "Dwarf Version",2);
#ifdef LLVM34
    m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
        llvm::DEBUG_METADATA_VERSION);
#endif
#ifdef LLVM37
    if (jl_ExecutionEngine) {
#ifdef LLVM38
        m->setDataLayout(jl_ExecutionEngine->getDataLayout().getStringRepresentation());
#else
        m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
#endif
        m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
    }
#elif LLVM36
    if (jl_ExecutionEngine)
        m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
    if (add) {
        assert(jl_ExecutionEngine);
#ifdef LLVM36
        jl_ExecutionEngine->addModule(std::unique_ptr<Module>(m));
#else
        jl_ExecutionEngine->addModule(m);
#endif
#if defined(_CPU_X86_64_) && defined(_OS_WINDOWS_) && defined(USE_MCJIT)
        ArrayType *atype = ArrayType::get(T_uint32,3); // want 4-byte alignment of 12-bytes of data
        (new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__UnwindData"))->setSection(".text");
        (new GlobalVariable(*m, atype,
            false, GlobalVariable::InternalLinkage,
            ConstantAggregateZero::get(atype), "__catchjmp"))->setSection(".text");
#endif
    }
}

extern "C" void jl_generate_fptr(jl_function_t *f)
{
    JL_LOCK(codegen)
    // objective: assign li->fptr
    jl_lambda_info_t *li = f->linfo;
    assert(li->functionObject);
    if (li->fptr == &jl_trampoline) {
        JL_SIGATOMIC_BEGIN();
        #ifdef USE_MCJIT
        if (imaging_mode) {
            // Copy the function out of the shadow module
            Module *m = new Module("julia", jl_LLVMContext);
            jl_setup_module(m, true);
            FunctionMover mover(m, shadow_module);
            li->functionObject = mover.CloneFunction((Function*)li->functionObject);
            if (li->specFunctionObject != NULL)
                li->specFunctionObject = mover.CloneFunction((Function*)li->specFunctionObject);
            if (li->cFunctionList != NULL) {
                size_t i;
                cFunctionList_t *list = (cFunctionList_t*)li->cFunctionList;
                for (i = 0; i < list->len; i++) {
                    list->data[i].f = mover.CloneFunction(list->data[i].f);
                }
            }
        }
        #endif

        Function *llvmf = (Function*)li->functionObject;
#ifdef USE_MCJIT
        li->fptr = (jl_fptr_t)(intptr_t)jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
#else
        li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
#endif
        assert(li->fptr != NULL);
#ifndef KEEP_BODIES
        if (!imaging_mode)
            llvmf->deleteBody();
#endif

        if (li->cFunctionList != NULL) {
            size_t i;
            cFunctionList_t *list = (cFunctionList_t*)li->cFunctionList;
            for (i = 0; i < list->len; i++) {
#ifdef USE_MCJIT
                (void)jl_ExecutionEngine->getFunctionAddress(list->data[i].f->getName());
#else
                (void)jl_ExecutionEngine->getPointerToFunction(list->data[i].f);
#endif
#ifndef KEEP_BODIES
                if (!imaging_mode) {
                    list->data[i].f->deleteBody();
                }
#endif
            }
        }

        if (li->specFunctionObject != NULL) {
#ifdef USE_MCJIT
            (void)jl_ExecutionEngine->getFunctionAddress(((Function*)li->specFunctionObject)->getName());
#else
            (void)jl_ExecutionEngine->getPointerToFunction((Function*)li->specFunctionObject);
#endif
#ifndef KEEP_BODIES
            if (!imaging_mode)
                ((Function*)li->specFunctionObject)->deleteBody();
#endif
        }
        JL_SIGATOMIC_END();
    }
    f->fptr = li->fptr;
    JL_UNLOCK(codegen)
}

extern "C" void jl_compile(jl_function_t *f)
{
    jl_lambda_info_t *li = f->linfo;
    if (li->functionObject == NULL) {
        // objective: assign li->functionObject
        li->inCompile = 1;
        (void)to_function(li);
        li->inCompile = 0;
    }
}

// Get the LLVM Function* for the C-callable entry point for a certain function
// and argument types. If rt is NULL then whatever return type is present is
// accepted.
static Function *gen_cfun_wrapper(jl_function_t *ff, jl_value_t *jlrettype, jl_tupletype_t *argt, int64_t isref);
static Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_tupletype_t *argt)
{
    if (rt) {
        JL_TYPECHK(cfunction, type, rt);
    }
    JL_TYPECHK(cfunction, type, (jl_value_t*)argt);
    JL_TYPECHK(cfunction, function, (jl_value_t*)f);
    if (!jl_is_gf(f))
        jl_error("only generic functions are currently c-callable");

    size_t i, nargs = jl_nparams(argt);
    if (nargs >= 64)
        jl_error("only functions with less than 64 arguments are c-callable");

    uint64_t isref = 0; // bit vector of which argument types are a subtype of Type{Ref{T}}
    jl_value_t *sigt = NULL; // type signature with Ref{} annotations removed
    JL_GC_PUSH1(&sigt);
    sigt = (jl_value_t*)jl_alloc_svec(nargs);
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
        jl_svecset(sigt, i, ati);
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

    jl_function_t *ff = jl_get_specialization(f, (jl_tupletype_t*)sigt);
    if (ff != NULL && ff->env==(jl_value_t*)jl_emptysvec && ff->linfo != NULL) {
        jl_lambda_info_t *li = ff->linfo;
        if (!jl_types_equal((jl_value_t*)li->specTypes, sigt)) {
            jl_errorf("cfunction: type signature of %s does not match specification",
                      li->name->name);
        }
        jl_value_t *astrt = jl_ast_rettype(li, li->ast);
        if (rt != NULL) {
            if (astrt == (jl_value_t*)jl_bottom_type) {
                if (rt != (jl_value_t*)jl_void_type) {
                    // a function that doesn't return can be passed to C as void
                    jl_errorf("cfunction: %s does not return", li->name->name);
                }
            }
            else if (!jl_subtype(astrt, rt, 0)) {
                jl_errorf("cfunction: return type of %s does not match",
                          li->name->name);
            }
        }
        JL_GC_POP(); // kill list: sigt
        return gen_cfun_wrapper(ff, astrt, argt, isref);
    }
    jl_error("cfunction: no method exactly matched the required type signature (function not yet c-callable)");
}

// get the address of a C-callable entry point for a function
extern "C" DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_GC_PUSH1(&argt);
    if (jl_is_tuple(argt)) {
        // TODO: maybe deprecation warning, better checking
        argt = (jl_value_t*)jl_apply_tuple_type_v((jl_value_t**)jl_data_ptr(argt), jl_nfields(argt));
    }
    assert(jl_is_tuple_type(argt));
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    assert(llvmf);
    JL_GC_POP();
#ifdef USE_MCJIT
    return (void*)(intptr_t)jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
#else
    return jl_ExecutionEngine->getPointerToFunction(llvmf);
#endif
}


extern "C" DLLEXPORT
void *jl_function_ptr_by_llvm_name(char* name) {
#ifdef __has_feature
#if __has_feature(memory_sanitizer)
    __msan_unpoison_string(name);
#endif
#endif
    return (void*)(intptr_t)jl_ExecutionEngine->FindFunctionNamed(name);
}

// export a C-callable entry point for a function, with a given name
extern "C" DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    assert(jl_is_tuple_type(argt));
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    if (llvmf) {
        #ifndef LLVM35
        new GlobalAlias(llvmf->getType(), GlobalValue::ExternalLinkage, name, llvmf, llvmf->getParent());
        #elif defined(LLVM37) && !defined(LLVM38)
        GlobalAlias::create(cast<PointerType>(llvmf->getType()),
                            GlobalValue::ExternalLinkage, name, llvmf, llvmf->getParent());
        #else
        GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                            GlobalValue::ExternalLinkage, name, llvmf, llvmf->getParent());
        #endif
    }
}

// --- native code info, and dump function to IR and ASM ---
extern void RegisterJuliaJITEventListener();

extern int jl_get_llvmf_info(uint64_t fptr, uint64_t *symsize, uint64_t *slide,
#ifdef USE_MCJIT
    const object::ObjectFile **object
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines
#endif
    );


// Get pointer to llvm::Function instance, compiling if necessary
extern "C" DLLEXPORT
void *jl_get_llvmf(jl_function_t *f, jl_tupletype_t *tt, bool getwrapper)
{
    jl_function_t *sf = f;
    if (tt != NULL) {
        if (!jl_is_function(f) || !jl_is_gf(f)) {
            return NULL;
        }
        sf = jl_get_specialization(f, tt);
    }
    if (sf == NULL || sf->linfo == NULL) {
        sf = jl_method_lookup_by_type(jl_gf_mtable(f), tt, 0, 0);
        if (sf == jl_bottom_func) {
            return NULL;
        }
        jl_printf(JL_STDERR,
                  "WARNING: Returned code may not match what actually runs.\n");
    }
    if (sf->linfo->specFunctionObject != NULL) {
        // found in the system image: force a recompile
        Function *llvmf = (Function*)sf->linfo->specFunctionObject;
        if (llvmf->isDeclaration()) {
            sf->linfo->specFunctionObject = NULL;
            sf->linfo->functionObject = NULL;
        }
    }
    if (sf->linfo->functionObject != NULL) {
        // found in the system image: force a recompile
        Function *llvmf = (Function*)sf->linfo->functionObject;
        if (llvmf->isDeclaration()) {
            sf->linfo->specFunctionObject = NULL;
            sf->linfo->functionObject = NULL;
        }
    }
    if (sf->linfo->functionObject == NULL && sf->linfo->specFunctionObject == NULL) {
        jl_compile(sf);
    }
    if (!getwrapper && sf->linfo->specFunctionObject != NULL)
        return (Function*)sf->linfo->specFunctionObject;
    else
        return (Function*)sf->linfo->functionObject;
}

Function* CloneFunctionToModule(Function *F, Module *destModule)
{
    ValueToValueMapTy VMap;
    Function *NewF = Function::Create(F->getFunctionType(),
                                      Function::ExternalLinkage,
                                      F->getName(),
                                      destModule);
    VMap[F] = NewF;

    Function::arg_iterator DestI = NewF->arg_begin();
    for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end(); I != E; ++I) {
        DestI->setName(I->getName());    // Copy the name over...
        VMap[I] = DestI++;        // Add mapping to VMap
    }

    SmallVector<ReturnInst*, 8> Returns;
    llvm::CloneFunctionInto(NewF, F, VMap, true, Returns, "", NULL, NULL);
    return NewF;
}

extern "C" DLLEXPORT
const jl_value_t *jl_dump_function_ir(void *f, bool strip_ir_metadata, bool dump_module)
{
    std::string code;
    llvm::raw_string_ostream stream(code);

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_ir: Expected Function*");

    if (llvmf->isDeclaration()) {
        // print the function declaration plain
        llvmf->print(stream);
    }
    else {
        // make a copy of the function with all module metadata
        Module *m = new Module(llvmf->getName(), jl_LLVMContext);
        jl_setup_module(m, false);
        Function *f2 = CloneFunctionToModule(llvmf, m);
        if (strip_ir_metadata) {
            // strip metadata from the copy
            Function::BasicBlockListType::iterator f2_bb = f2->getBasicBlockList().begin();
            // iterate over all basic blocks in the function
            for (; f2_bb != f2->getBasicBlockList().end(); ++f2_bb) {
                BasicBlock::InstListType::iterator f2_il = (*f2_bb).getInstList().begin();
                // iterate over instructions in basic block
                for (; f2_il != (*f2_bb).getInstList().end(); ) {
                    Instruction *inst = f2_il++;
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
        if (dump_module)
            m->print(stream, NULL);
        else
            f2->print(stream);
        f2->eraseFromParent();
        delete m;
    }

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// Pre-declaration. Definition in disasm.cpp
extern "C"
void jl_dump_asm_internal(uintptr_t Fptr, size_t Fsize, size_t slide,
#ifdef USE_MCJIT
                          const object::ObjectFile *objectfile,
#else
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#endif
                          formatted_raw_ostream &stream);

extern "C" DLLEXPORT
const jl_value_t *jl_dump_function_asm(void *f, int raw_mc)
{
    std::string code;
    llvm::raw_string_ostream stream(code);
    llvm::formatted_raw_ostream fstream(stream);

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_asm: Expected Function*");

    // Dump assembly code
    uint64_t symsize, slide;
#ifdef USE_MCJIT
    uint64_t fptr = jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
    const object::ObjectFile *object;
#else
    uint64_t fptr = (uintptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> object;
#endif
    assert(fptr != 0);
    if (jl_get_llvmf_info(fptr, &symsize, &slide, &object)) {
        if (raw_mc)
            return (jl_value_t*)jl_pchar_to_array((char*)fptr, symsize);
        jl_dump_asm_internal(fptr, symsize, slide, object, fstream);
    }
    else {
        jl_printf(JL_STDERR, "WARNING: Unable to find function pointer\n");
    }
    fstream.flush();

    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

// Code coverage

typedef std::map<std::string,std::vector<GlobalVariable*> > logdata_t;
static logdata_t coverageData;

static void coverageVisitLine(std::string filename, int line)
{
    if (filename == "" || filename == "none" || filename == "no file")
        return;
    logdata_t::iterator it = coverageData.find(filename);
    if (it == coverageData.end()) {
        coverageData[filename] = std::vector<GlobalVariable*>(0);
    }
    std::vector<GlobalVariable*> &vec = coverageData[filename];
    if (vec.size() <= (size_t)line)
        vec.resize(line+1, NULL);
    if (vec[line] == NULL) {
        vec[line] = addComdat(new GlobalVariable(*jl_Module, T_int64, false,
                                                 GlobalVariable::InternalLinkage,
                                                 ConstantInt::get(T_int64,0), "lcnt"));
    }
    GlobalVariable *v = prepare_global(vec[line]);
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true),
                                          ConstantInt::get(T_int64,1)),
                        v, true);
}

extern "C" int isabspath(const char *in);

void write_log_data(logdata_t logData, const char *extension)
{
    std::string base = std::string(jl_options.julia_home);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename = (*it).first;
        std::vector<GlobalVariable*> &values = (*it).second;
        if (values.size() > 1) {
            if (!isabspath(filename.c_str()))
                filename = base + filename;
            std::ifstream inf(filename.c_str());
            if (inf.is_open()) {
                std::string outfile = filename + extension;
                std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out);
                char line[1024];
                int l = 1;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
                    if (inf.fail() && !inf.bad()) {
                        // Read through lines longer than sizeof(line)
                        inf.clear();
                        inf.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
                    }
                    int value = -1;
                    if ((size_t)l < values.size()) {
                        GlobalVariable *gv = values[l];
                        if (gv) {
#ifdef USE_MCJIT
                            int *p = (int*)(intptr_t)jl_ExecutionEngine->getGlobalValueAddress(gv->getName());
#else
                            int *p = (int*)jl_ExecutionEngine->getPointerToGlobal(gv);
#endif
                            value = *p;
                        }
                    }
                    outf.width(9);
                    if (value == -1)
                        outf<<'-';
                    else
                        outf<<value;
                    outf.width(0);
                    outf<<" "<<line<<std::endl;
                    l++;
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

// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(std::string filename, int line)
{
    if (filename == "" || filename == "none" || filename == "no file") {
        jl_gc_sync_total_bytes();
        return;
    }
    logdata_t::iterator it = mallocData.find(filename);
    if (it == mallocData.end()) {
        mallocData[filename] = std::vector<GlobalVariable*>(0);
    }
    std::vector<GlobalVariable*> &vec = mallocData[filename];
    if (vec.size() <= (size_t)line)
        vec.resize(line+1, NULL);
    if (vec[line] == NULL) {
        vec[line] = addComdat(new GlobalVariable(*jl_Module, T_int64, false,
                                                 GlobalVariable::InternalLinkage,
                                                 ConstantInt::get(T_int64,0), "bytecnt"));
    }
    GlobalVariable *v = prepare_global(vec[line]);
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true),
                                          builder.CreateCall(prepare_call(diff_gc_total_bytes_func)
#ifdef LLVM37
                                            , {}
#endif
                                            )),
                        v, true);
}

// Resets the malloc counts. Needed to avoid including memory usage
// from JITting.
extern "C" DLLEXPORT void jl_clear_malloc_data(void)
{
    logdata_t::iterator it = mallocData.begin();
    for (; it != mallocData.end(); it++) {
        std::vector<GlobalVariable*> &bytes = (*it).second;
        std::vector<GlobalVariable*>::iterator itb;
        for (itb = bytes.begin(); itb != bytes.end(); itb++) {
            if (*itb) {
#ifdef USE_MCJIT
                int *p = (int*)(intptr_t)jl_ExecutionEngine->getGlobalValueAddress((*itb)->getName());
#else
                int *p = (int*)jl_ExecutionEngine->getPointerToGlobal(*itb);
#endif
                *p = 0;
            }
        }
    }
    jl_gc_sync_total_bytes();
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
    jl_printf(out, "in %s at %s", ctx->linfo->name->name, ctx->linfo->file->name);
}

extern "C" void jl_binding_deprecation_warning(jl_binding_t *b);

static void cg_bdw(jl_binding_t *b, jl_codectx_t *ctx)
{
    jl_binding_deprecation_warning(b);
    show_source_loc(JL_STDERR, ctx);
    jl_printf(JL_STDERR, "\n");
}

// try to statically evaluate, NULL if not possible
extern "C"
jl_value_t *jl_static_eval(jl_value_t *ex, void *ctx_, jl_module_t *mod,
                           jl_value_t *sp, jl_expr_t *ast, int sparams, int allow_alloc)
{
    jl_codectx_t *ctx = (jl_codectx_t*)ctx_;
    if (jl_is_symbolnode(ex))
        ex = (jl_value_t*)jl_symbolnode_sym(ex);
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        bool isglob = true;
        if (ctx) {
            isglob = is_global(sym, ctx);
        }
        else if (ast) {
            isglob = !jl_local_in_ast(ast, sym);
        }
        if (isglob) {
            size_t i;
            if (sparams) {
                for(i=0; i < jl_svec_len(sp); i+=2) {
                    if (sym == (jl_sym_t*)jl_svecref(sp, i)) {
                        // static parameter
                        return jl_svecref(sp, i+1);
                    }
                }
            }
            if (jl_is_const(mod, sym))
                return jl_get_global(mod, sym);
        }
        return NULL;
    }
    if (jl_is_gensym(ex))
        return NULL;
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
            jl_value_t *f = jl_static_eval(jl_exprarg(e,0),ctx,mod,sp,ast,sparams,allow_alloc);
            if (f && jl_is_function(f)) {
                jl_fptr_t fptr = ((jl_function_t*)f)->fptr;
                if (jl_array_dim0(e->args) == 3 && fptr == &jl_f_get_field) {
                    m = (jl_module_t*)jl_static_eval(jl_exprarg(e,1),ctx,mod,sp,ast,sparams,allow_alloc);
                    s = (jl_sym_t*)jl_static_eval(jl_exprarg(e,2),ctx,mod,sp,ast,sparams,allow_alloc);
                    if (m && jl_is_module(m) && s && jl_is_symbol(s)) {
                        jl_binding_t *b = jl_get_binding(m, s);
                        if (b && b->constp) {
                            if (b->deprecated) cg_bdw(b, ctx);
                            return b->value;
                        }
                    }
                }
                else if (fptr == &jl_f_tuple || fptr == &jl_f_instantiate_type) {
                    size_t i;
                    size_t n = jl_array_dim0(e->args)-1;
                    if (n==0 && fptr == &jl_f_tuple) return (jl_value_t*)jl_emptytuple;
                    if (!allow_alloc)
                        return NULL;
                    jl_value_t **v;
                    JL_GC_PUSHARGS(v, n);
                    for (i = 0; i < n; i++) {
                        v[i] = jl_static_eval(jl_exprarg(e,i+1),ctx,mod,sp,ast,sparams,allow_alloc);
                        if (v[i] == NULL) {
                            JL_GC_POP();
                            return NULL;
                        }
                    }
                    jl_value_t *result;
                    JL_TRY {
                        result = fptr(f, v, n);
                    }
                    JL_CATCH {
                        result = NULL;
                    }
                    JL_GC_POP();
                    return result;
                }
            }
        // The next part is probably valid, but it is untested
        //} else if (e->head == tuple_sym) {
        //  size_t i;
        //  for (i = 0; i < jl_array_dim0(e->args); i++)
        //        if (jl_static_eval(jl_exprarg(e,i),ctx,mod,sp,ast,sparams,allow_alloc) == NULL)
        //          return NULL;
        //  return ex;
        }
        return NULL;
    }
    return ex;
}

static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, bool sparams,
                               bool allow_alloc)
{
    return jl_static_eval(ex, ctx, ctx->module, (jl_value_t*)ctx->sp, ctx->ast,
                          sparams, allow_alloc);
}

static bool is_constant(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true)
{
    return static_eval(ex,ctx,sparams) != NULL;
}

static bool symbol_eq(jl_value_t *e, jl_sym_t *sym)
{
    return ((jl_is_symbol(e) && ((jl_sym_t*)e)==sym) ||
            (jl_is_symbolnode(e) && jl_symbolnode_sym(e)==sym));
}

// --- find volatile variables ---

// assigned in a try block and used outside that try block

static bool local_var_occurs(jl_value_t *e, jl_sym_t *s)
{
    if (jl_is_symbol(e) || jl_is_symbolnode(e)) {
        if (symbol_eq(e, s))
            return true;
    }
    else if (jl_is_expr(e)) {
        jl_expr_t *ex = (jl_expr_t*)e;
        size_t alength = jl_array_dim0(ex->args);
        for(int i=0; i < (int)alength; i++) {
            if (local_var_occurs(jl_exprarg(ex,i),s))
                return true;
        }
    }
    return false;
}

static std::set<jl_sym_t*> assigned_in_try(jl_array_t *stmts, int s, long l, int *pend)
{
    std::set<jl_sym_t*> av;
    size_t slength = jl_array_dim0(stmts);
    for(int i=s; i < (int)slength; i++) {
        jl_value_t *st = jl_cellref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == assign_sym) {
                jl_value_t *ar = jl_exprarg(st, 0);
                if (jl_is_symbolnode(ar)) {
                    ar = (jl_value_t*)jl_symbolnode_sym(ar);
                }
                if (jl_is_symbol(ar)) {
                    av.insert((jl_sym_t*)ar);
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

static void mark_volatile_vars(jl_array_t *stmts, std::map<jl_sym_t*,jl_varinfo_t> &vars)
{
    size_t slength = jl_array_dim0(stmts);
    for(int i=0; i < (int)slength; i++) {
        jl_value_t *st = jl_cellref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == enter_sym) {
                int last = (int)slength-1;
                std::set<jl_sym_t*> as =
                    assigned_in_try(stmts, i+1,
                                    jl_unbox_long(jl_exprarg(st,0)), &last);
                for(int j=0; j < (int)slength; j++) {
                    if (j < i || j > last) {
                        std::set<jl_sym_t*>::iterator it = as.begin();
                        for(; it != as.end(); it++) {
                            if (vars.find(*it) != vars.end() &&
                                local_var_occurs(jl_cellref(stmts,j), *it)) {
                                vars[*it].isVolatile = true;
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
    return (jl_is_symbol(e) || jl_is_symbolnode(e) || jl_is_topnode(e) || jl_is_globalref(e));
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
                    jl_value_t *fv =
                        jl_interpret_toplevel_expr_in(ctx->module, f, NULL, 0);
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
                    else if (jl_is_function(fv)) {
                        jl_function_t *ff = (jl_function_t*)fv;
                        if ((ff->fptr == jl_f_get_field && alen==3 &&
                             expr_type(jl_exprarg(e,2),ctx) == (jl_value_t*)jl_long_type) ||
                            ff->fptr == jl_f_nfields ||
                            (ff->fptr == jl_f_apply && alen==4 &&
                             expr_type(jl_exprarg(e,2),ctx) == (jl_value_t*)jl_function_type)) {
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
    if (jl_is_symbolnode(expr)) {
        expr = (jl_value_t*)jl_symbolnode_sym(expr);
    }
    if (jl_is_symbol(expr)) {
        jl_sym_t *vname = ((jl_sym_t*)expr);
        if (ctx->vars.find(vname) != ctx->vars.end()) {
            jl_varinfo_t &vi = ctx->vars[vname];
            vi.escapes |= esc;
            vi.used = true;
        }
    }
}

// --- gc root utils ---

// ---- Get Element Pointer (GEP) instructions within the GC frame ----

// Emit GEP for the @slot-th slot in the GC frame
static Value*
emit_local_slot(int slot, jl_codectx_t *ctx)
{
    Value *idx = ConstantInt::get(T_int32, slot);
    return builder.CreateGEP(ctx->gc.argSlot, idx);
}

// Emit GEP for the @slot-th temporary variable in the GC frame.
// The temporary variables are after all local variables in the GC frame.
static Value*
emit_temp_slot(int slot, jl_codectx_t *ctx)
{
    Value *idx = ConstantInt::get(T_int32, slot);
    return builder.CreateGEP(ctx->gc.tempSlot, idx);
}

static Value *make_gcroot(Value *v, jl_codectx_t *ctx)
{
    Value *froot = emit_temp_slot(ctx->gc.argDepth, ctx);
    builder.CreateStore(v, froot);
    ctx->gc.argDepth++;
    if (ctx->gc.argDepth > ctx->gc.maxDepth)
        ctx->gc.maxDepth = ctx->gc.argDepth;
    return froot;
}

// test whether getting a field from the given type using the given
// field expression would not allocate memory
static bool is_getfield_nonallocating(jl_datatype_t *ty, jl_value_t *fld)
{
    if (!jl_is_leaf_type((jl_value_t*)ty))
        return false;
    jl_sym_t *name = NULL;
    if (jl_is_quotenode(fld) && jl_is_symbol(jl_fieldref(fld,0)))
        name = (jl_sym_t*)jl_fieldref(fld,0);
    int idx = -1;
    if (name)
        idx = jl_field_index(ty, name, 0);
    else if (jl_is_long(fld))
        idx = jl_unbox_long(fld)-1;
    else if (jl_is_quotenode(fld) && jl_is_long(jl_fieldref(fld,0)))
        idx = jl_unbox_long(jl_fieldref(fld,0))-1;
    for(size_t i=0; i < jl_svec_len(ty->types); i++) {
        if (!(jl_field_isptr(ty,i) || (idx >= 0 && (size_t)idx != i)))
            return false;
    }
    return true;
}

// does "ex" compute something that doesn't need a root over the whole function?
static bool is_stable_expr(jl_value_t *ex, jl_codectx_t *ctx)
{
    if (jl_is_symbolnode(ex))
        ex = (jl_value_t*)jl_symbolnode_sym(ex);
    if (jl_is_symbol(ex)) {
        if (ctx->vars.find((jl_sym_t*)ex) != ctx->vars.end()) {
            // arguments and SSA vars are stable
            jl_varinfo_t &rhs = ctx->vars[(jl_sym_t*)ex];
            if ((rhs.isArgument && !rhs.isAssigned) || rhs.isSA)
                return true;
        }
    }
    if (jl_is_gensym(ex))
        return true;
    if (static_eval(ex, ctx, true, false) != NULL)
        return true;
    if (jl_is_expr(ex)) {
        jl_expr_t *e = (jl_expr_t*)ex;
        if (e->head == call_sym) {
            jl_value_t *f = static_eval(jl_exprarg(e,0),ctx,true,false);
            if (f && jl_is_function(f)) {
                jl_fptr_t fptr = ((jl_function_t*)f)->fptr;
                // something reached via getfield from a stable value is also stable.
                if (jl_array_dim0(e->args) == 3) {
                    jl_value_t *ty = expr_type(jl_exprarg(e,1), ctx);
                    if ((fptr == &jl_f_get_field && jl_is_immutable_datatype(ty) &&
                         is_getfield_nonallocating((jl_datatype_t*)ty, jl_exprarg(e,2)))) {
                        if (is_stable_expr(jl_exprarg(e,1), ctx))
                            return true;
                    }
                }
            }
        }
    }
    return false;
}

// classify exprs that might need temporary rooting.
static bool might_need_root(jl_value_t *ex)
{
    return (!jl_is_symbol(ex) && !jl_is_symbolnode(ex) && !jl_is_gensym(ex) &&
            !jl_is_bool(ex) && !jl_is_quotenode(ex) && !jl_is_byte_string(ex) &&
            !jl_is_globalref(ex));
}

static jl_cgval_t emit_boxed_rooted(jl_value_t *e, jl_codectx_t *ctx) // TODO: make this return a Value*
{
    jl_cgval_t v = emit_expr(e, ctx);
    if (!v.isboxed) {
        Value *vbox = boxed(v, ctx);
        make_gcroot(vbox, ctx);
        v = jl_cgval_t(vbox, true, v.typ); // XXX: bypasses the normal auto-unbox behavior for isghost!
    }
    else if (might_need_root(e)) { // TODO: v.needsgcroot
        make_gcroot(v.V, ctx);
    }
    return v;
}

// --- lambda ---

static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val)
{
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

static Value *emit_lambda_closure(jl_value_t *expr, jl_codectx_t *ctx)
{
    assert(jl_is_lambda_info(expr));
    size_t i;
    jl_value_t *ast = ((jl_lambda_info_t*)expr)->ast;
    jl_array_t *capt;
    if (jl_is_expr(ast))
        capt = jl_lam_capt((jl_expr_t*)ast);
    else
        capt = (jl_array_t*)((jl_lambda_info_t*)expr)->capt;
    if (capt == NULL || jl_array_dim0(capt) == 0) {
        // no captured vars; lift
        jl_value_t *fun =
            (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_emptysvec,
                                        (jl_lambda_info_t*)expr);
        jl_add_linfo_root(ctx->linfo, fun);
        return literal_pointer_val(fun);
    }

    int argStart = ctx->gc.argDepth;
    size_t clen = jl_array_dim0(capt);
    Value **captured = (Value**) alloca((1 + clen) * sizeof(Value*));
    captured[0] = ConstantInt::get(T_size, clen);
    for(i=0; i < clen; i++) {
        Value *val;
        jl_array_t *vi = (jl_array_t*)jl_cellref(capt, i);
        assert(jl_is_array(vi));
        jl_sym_t *s = (jl_sym_t*)jl_cellref(vi,0);
        assert(jl_is_symbol(s));
        jl_varinfo_t &vari = ctx->vars[s];
        if (vari.memloc) {
            val = builder.CreateLoad(vari.memloc, vari.isVolatile);
        }
        else {
            assert(!vari.isAssigned || vari.value.isghost); // make sure there wasn't an inference / codegen error earlier
            val = boxed(vari.value, ctx);
            if (!vari.value.isghost)
                make_gcroot(val, ctx);
        }
        captured[i+1] = val;
    }
    Value *env_tuple = builder.CreateCall(prepare_call(jlnsvec_func),
                                   ArrayRef<Value*>(&captured[0], 1+clen));
    ctx->gc.argDepth = argStart; // remove arguments roots from the implicit gc stack
    make_gcroot(env_tuple, ctx);
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(jlclosure_func),
                                        {Constant::getNullValue(T_pint8),
                                        env_tuple, literal_pointer_val(expr)});
#else
    Value *result = builder.CreateCall3(prepare_call(jlclosure_func),
                                        Constant::getNullValue(T_pint8),
                                        env_tuple, literal_pointer_val(expr));
#endif
    ctx->gc.argDepth--; // pop env_tuple from the implicit gc stack
    return result;
}

// --- generating function calls ---

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
                if (jl_isbits(jl_typeof(bnd->value)))
                    return emit_unboxed(bnd->value, ctx);
                else
                    return mark_julia_const(bnd->value);
            }
            return mark_julia_type(builder.CreateLoad(bp), true, (jl_value_t*)jl_any_type);
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
            jl_cgval_t strct = emit_expr(expr, ctx, false);
            jl_cgval_t fld = emit_getfield_knownidx(strct, idx, sty, ctx);
            JL_GC_POP();
            return fld;
        }
    }
    JL_GC_POP(); // kill sty
    // TODO: attempt better codegen for approximate types, if the types
    // and offsets of some fields are independent of parameters.

    int argStart = ctx->gc.argDepth;
    Value *arg1 = boxed(emit_expr(expr,ctx), ctx, expr_type(expr,ctx));
    // TODO: generic getfield func with more efficient calling convention
    make_gcroot(arg1, ctx);
    Value *arg2 = literal_pointer_val((jl_value_t*)name);
    make_gcroot(arg2, ctx);
    Value *myargs = emit_temp_slot(argStart, ctx);
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(jlgetfield_func), {V_null, myargs,
                                        ConstantInt::get(T_int32,2)});
#else
    Value *result = builder.CreateCall3(prepare_call(jlgetfield_func), V_null, myargs,
                                        ConstantInt::get(T_int32,2));
#endif
    ctx->gc.argDepth = argStart;
    jl_cgval_t ret = mark_julia_type(result, true, jl_any_type); // (typ will be patched up by caller)
    //ret.needsgcroot = arg1.needsgcroot || !arg1.isimmutable || !jl_is_leaf_type(arg1.typ) || !is_datatype_all_pointers((jl_datatype_t*)arg1.typ);
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
            subAns = emit_bits_compare(mark_julia_type(fld1, false, fldty), mark_julia_type(fld2, false, fldty), ctx);
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
                            builder.CreatePointerCast(arg1.V, T_pint8),
                            builder.CreatePointerCast(arg2.V, T_pint8),
                            ConstantInt::get(T_size, sz)
                            });
#else
            Value *answer = builder.CreateCall3(prepare_call(memcmp_func),
                    builder.CreatePointerCast(arg1.V, T_pint8),
                    builder.CreatePointerCast(arg2.V, T_pint8),
                    ConstantInt::get(T_size, sz));
#endif
            return builder.CreateICmpEQ(answer, ConstantInt::get(T_int32, 0));
        }
        else {
            Type *atp = at->getPointerTo();
            Value *varg1 = arg1.V;
            if (varg1->getType() != atp)
                varg1 = builder.CreatePointerCast(varg1, atp);
            Value *varg2 = arg2.V;
            if (varg2->getType() != atp)
                varg2 = builder.CreatePointerCast(varg2, atp);
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
        return builder.CreateICmpEQ(arg1.V, arg2.V);
    }

    if (arg2.isboxed && arg2.needsgcroot)
        make_gcroot(arg2.V, ctx);
    Value *varg1 = boxed(arg1, ctx);
    if (!arg1.isboxed)
        make_gcroot(varg1, ctx);
    Value *varg2 = boxed(arg2, ctx); // unrooted!
#ifdef LLVM37
    return builder.CreateTrunc(builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
#else
    return builder.CreateTrunc(builder.CreateCall2(prepare_call(jlegal_func), varg1, varg2), T_int1);
#endif
}

static bool emit_known_call(jl_cgval_t *ret, jl_value_t *ff,
                            jl_value_t **args, size_t nargs,
                            jl_codectx_t *ctx,
                            Value **theFptr, jl_function_t **theF,
                            jl_value_t *expr)
// returns true if the call has been handled
{
    if (jl_typeis(ff, jl_intrinsic_type)) {
        *ret = emit_intrinsic((intrinsic)*(uint32_t*)jl_data_ptr(ff),
                              args, nargs, ctx);
        if (ret->typ == (jl_value_t*)jl_any_type) // the select_value intrinsic may be missing type information
            *ret = remark_julia_type(*ret, expr_type(expr, ctx));
        return true;
    }
    if (!jl_is_func(ff)) {
        return false;
    }
    jl_value_t *rt1=NULL, *rt2=NULL, *rt3=NULL;
    JL_GC_PUSH3(&rt1, &rt2, &rt3);
    jl_function_t *f = (jl_function_t*)ff;

    if (f->fptr == &jl_apply_generic) {
        *theFptr = jlapplygeneric_func;
        *theF = f;
        if (ctx->linfo->inferred) {
            jl_svec_t *aty = call_arg_types(&args[1], nargs, ctx);
            rt1 = (jl_value_t*)aty;
            // attempt compile-time specialization for inferred types
            if (aty != NULL) {
                rt1 = (jl_value_t*)jl_apply_tuple_type(aty);
                /*
                  if (trace) {
                      jl_printf(JL_STDOUT, "call %s%s\n",
                      jl_sprint(args[0]),
                      jl_sprint((jl_value_t*)aty));
                  }
                */
                f = jl_get_specialization(f, (jl_tupletype_t*)rt1);
                if (f != NULL) {
                    assert(f->linfo->functionObject != NULL);
                    *theFptr = (Value*)f->linfo->functionObject;
                    *theF = f;
                }
            }
        }
        JL_GC_POP();
        return false;
    }

    else if (f->fptr == &jl_f_is && nargs==2) {
        // handle simple static expressions with no side-effects
        rt1 = static_eval(args[1], ctx, true);
        if (rt1) {
            rt2 = static_eval(args[2], ctx, true);
            if (rt2) {
                *ret = mark_julia_type(ConstantInt::get(T_int1, jl_egal(rt1, rt2)), false, jl_bool_type);
                JL_GC_POP();
                return true;
            }
        }
        // emit values
        int last_depth = ctx->gc.argDepth;
        jl_cgval_t v1 = emit_unboxed(args[1], ctx);
        if (v1.isboxed && v1.needsgcroot && might_need_root(args[1]))
            make_gcroot(v1.V, ctx);
        jl_cgval_t v2 = emit_unboxed(args[2], ctx); // unrooted!
        // FIXME: v.typ is roughly equiv. to expr_type, but with typeof(T) == Type{T} instead of DataType in a few cases
        if (v1.typ == (jl_value_t*)jl_datatype_type)
            v1 = remark_julia_type(v1, expr_type(args[1], ctx)); // patch up typ if necessary
        if (v2.typ == (jl_value_t*)jl_datatype_type)
            v2 = remark_julia_type(v2, expr_type(args[2], ctx)); // patch up typ if necessary
        // emit comparison test
        Value *ans = emit_f_is(v1, v2, ctx);
        ctx->gc.argDepth = last_depth;
        *ret = mark_julia_type(ans, false, jl_bool_type);
        JL_GC_POP();
        return true;
    }

    else if (f->fptr == &jl_f_typeof && nargs==1) {
        jl_cgval_t arg1 = emit_expr(args[1], ctx);
        Value *lty = emit_typeof(arg1);
        *ret = mark_julia_type(lty, true, jl_datatype_type);
        JL_GC_POP();
        return true;
    }

    else if (f->fptr == &jl_f_typeassert && nargs==2) {
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
            FunctionType *ft = FunctionType::get(T_void, two_pvalue_llvmt, false); // TODO: move this to the codegen init section
            Value *typeassert = jl_Module->getOrInsertFunction("jl_typeassert", ft);
            int ldepth = ctx->gc.argDepth;
            *ret = emit_expr(args[1], ctx);
            Value *V = boxed(*ret, ctx);
            make_gcroot(V, ctx);
#ifdef LLVM37
            builder.CreateCall(prepare_call(typeassert), {V, boxed(emit_expr(args[2], ctx),ctx)});
#else
            builder.CreateCall2(prepare_call(typeassert), V, boxed(emit_expr(args[2], ctx),ctx));
#endif
            ctx->gc.argDepth = ldepth;
            JL_GC_POP();
            return true;
        }
    }

    else if (f->fptr == &jl_f_isa && nargs==2) {
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
                *ret = mark_julia_type(ConstantInt::get(T_int1, 1), false, jl_bool_type);
                JL_GC_POP();
                return true;
            }
            if (!jl_subtype(tp0, (jl_value_t*)jl_type_type, 0)) {
                if (jl_is_leaf_type(arg)) {
                    *ret = mark_julia_type(ConstantInt::get(T_int1, 0), false, jl_bool_type);
                    JL_GC_POP();
                    return true;
                }
                if (jl_is_leaf_type(tp0)) {
                    jl_cgval_t arg1 = emit_expr(args[1], ctx);
                    *ret = mark_julia_type(
                            builder.CreateICmpEQ(emit_typeof(arg1),
                                                literal_pointer_val(tp0)),
                            false,
                            jl_bool_type);
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f->fptr == &jl_f_subtype && nargs == 2) {
        rt1 = expr_type(args[1], ctx);
        rt2 = expr_type(args[2], ctx);
        if (jl_is_type_type(rt1) && !jl_is_typevar(jl_tparam0(rt1)) &&
            jl_is_type_type(rt2) && !jl_is_typevar(jl_tparam0(rt2))) {
            int issub = jl_subtype(jl_tparam0(rt1), jl_tparam0(rt2), 0);
            // TODO: emit args[1] and args[2] in case of side effects?
            *ret = mark_julia_type(ConstantInt::get(T_int1, issub), false, jl_bool_type);
            JL_GC_POP();
            return true;
        }
    }

    else if (f->fptr == &jl_f_apply && nargs==3 && ctx->vaStack &&
             symbol_eq(args[3], ctx->vaName) && expr_type(args[2],ctx) == (jl_value_t*)jl_function_type) {
        // turn Core._apply(f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the vaStack allocation
        Value *theF = boxed(emit_expr(args[2], ctx), ctx);
        Value *theFptr = emit_nthptr_recast(
                theF,
                (ssize_t)(offsetof(jl_function_t,fptr)/sizeof(void*)),
                tbaa_func,
                jl_pfptr_llvmt);
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
        *ret = mark_julia_type(r, true, expr_type(expr, ctx));
        JL_GC_POP();
        return true;
    }

    else if (f->fptr == &jl_f_tuple) {
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

    else if (f->fptr == &jl_f_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx), ctx);
        // emit a "conditional" throw so that codegen does't end up trying to emit code after an "unreachable" terminator
        raise_exception_unless(ConstantInt::get(T_int1,0), arg1, ctx);
        *ret = jl_cgval_t();
        JL_GC_POP();
        return true;
    }

    else if (f->fptr == &jl_f_arraylen && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_array_type(aty)) {
            // todo: also allow e.g. Union of several array types
            jl_cgval_t arg1 = emit_expr(args[1], ctx);
            *ret = mark_julia_type(emit_arraylen(arg1, args[1], ctx), false, jl_long_type);
            JL_GC_POP();
            return true;
        }
    }

    else if (f->fptr == &jl_f_arraysize && nargs==2) {
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
                        *ret = mark_julia_type(emit_arraysize(ary, args[1], idx, ctx), false, jl_long_type);
                        JL_GC_POP();
                        return true;
                    }
                    else if (idx > ndims) {
                        *ret = mark_julia_type(ConstantInt::get(T_size, 1), false, jl_long_type);
                        JL_GC_POP();
                        return true;
                    }
                }
                else {
                    Value *idx = emit_unbox(T_size,
                                            emit_unboxed(args[2], ctx), ity);
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
                    Value *v_sz = emit_arraysize(ary, idx);
                    builder.CreateBr(ansBB);
                    ctx->f->getBasicBlockList().push_back(ansBB);
                    builder.SetInsertPoint(ansBB);
                    PHINode *result = builder.CreatePHI(T_size, 2);
                    result->addIncoming(v_one, outBB);
                    result->addIncoming(v_sz, inBB);
                    *ret = mark_julia_type(result, false, jl_long_type);
                    JL_GC_POP();
                    return true;
                }
            }
        }
    }

    else if (f->fptr == &jl_f_arrayref && nargs>=2) {
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

    else if (f->fptr == &jl_f_arrayset && nargs>=3) {
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
                        emit_expr(args[2],ctx,false);
                    }
                    else {
                        jl_cgval_t v = (ety == (jl_value_t*)jl_any_type ? emit_expr(args[2],ctx) : emit_unboxed(args[2],ctx));
                        PHINode* data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed) { // if not boxed we don't need a write barrier
                            assert(ary.isboxed);
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
                            Value *own_ptr = builder.CreateLoad(
                                builder.CreateBitCast(builder.CreateConstGEP1_32(
                                    builder.CreateBitCast(ary.V,T_pint8), jl_array_data_owner_offset(nd)),
                                    T_ppjlvalue));
                            builder.CreateBr(mergeBB);
                            builder.SetInsertPoint(mergeBB);
                            data_owner = builder.CreatePHI(T_pjlvalue, 2);
                            data_owner->addIncoming(ary.V, curBB);
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

    else if (f->fptr == &jl_f_get_field && nargs==2) {
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
        if (ctx->vaStack && symbol_eq(args[1], ctx->vaName)) {
            Value *valen = emit_n_varargs(ctx);
            Value *idx = emit_unbox(T_size,
                                    emit_unboxed(args[2], ctx), fldt);
            idx = emit_bounds_check(
                    jl_cgval_t(builder.CreateGEP(ctx->argArray, ConstantInt::get(T_size, ctx->nReqArgs)), false, NULL),
                    NULL, idx, valen, ctx);
            idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
            *ret = mark_julia_type(
                    tbaa_decorate(tbaa_user, builder.CreateLoad(builder.CreateGEP(ctx->argArray, idx))),
                    true,
                    expr_type(expr, ctx));
            ret->needsgcroot = false;
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
                    Value *vidx = emit_unbox(T_size, emit_unboxed(args[2], ctx), (jl_value_t*)jl_long_type);
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

    else if (f->fptr == &jl_f_set_field && nargs==3) {
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
                    if (jl_field_isptr(sty, idx)) // emit rhs
                        *ret = emit_expr(args[3], ctx);
                    else
                        *ret = emit_unboxed(args[3], ctx);
                    emit_setfield(sty, strct, idx, *ret, ctx, true, true);
                    JL_GC_POP();
                    return true;
                }
            }
        }
        // TODO: faster code for integer index
    }

    else if (f->fptr == &jl_f_nfields && nargs==1) {
        if (ctx->vaStack && symbol_eq(args[1], ctx->vaName) && !ctx->vars[ctx->vaName].isAssigned) {
            *ret = mark_julia_type(emit_n_varargs(ctx), false, jl_long_type);
            JL_GC_POP();
            return true;
        }
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_type_type(aty)) {
            jl_value_t *tp0 = jl_tparam0(aty);
            if (jl_is_leaf_type(tp0)) {
                emit_expr(args[1], ctx);
                assert(jl_is_datatype(tp0));
                *ret = mark_julia_type(ConstantInt::get(T_size, jl_datatype_nfields(tp0)), false, jl_long_type);
                JL_GC_POP();
                return true;
            }
        }
        else if (jl_is_leaf_type(aty)) {
            jl_cgval_t arg1 = emit_expr(args[1], ctx);
            Value *sz;
            if (aty == (jl_value_t*)jl_datatype_type) {
                assert(arg1.isboxed);
                sz = emit_datatype_nfields(arg1.V);
            }
            else {
                sz = ConstantInt::get(T_size, jl_datatype_nfields(aty));
            }
            *ret = mark_julia_type(sz, false, jl_long_type);
            JL_GC_POP();
            return true;
        }
    }

    else if (f->fptr == &jl_f_field_type && nargs==2) {
        jl_datatype_t *sty = (jl_datatype_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_type_type((jl_value_t*)sty) || sty == jl_datatype_type) {
            rt2 = expr_type(args[2], ctx); // index argument type
            if (rt2 == (jl_value_t*)jl_long_type) {
                jl_cgval_t ty = emit_expr(args[1], ctx);
                assert(ty.isboxed);
                Value *types_svec = emit_datatype_types(ty.V);
                Value *types_len = emit_datatype_nfields(ty.V);
                Value *idx = emit_unbox(T_size, emit_unboxed(args[2], ctx), (jl_value_t*)jl_long_type);
                emit_bounds_check(ty, (jl_value_t*)jl_datatype_type, idx, types_len, ctx);
                Value *fieldtyp = builder.CreateLoad(builder.CreateGEP(builder.CreateBitCast(types_svec, T_ppjlvalue), idx));
                *ret = mark_julia_type(fieldtyp, true, expr_type(expr, ctx));
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f->fptr == &jl_f_sizeof && nargs == 1) {
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
                *ret = mark_julia_type(ConstantInt::get(T_size, sty->size), false, jl_long_type);
                JL_GC_POP();
                return true;
            }
        }
    }

    else if (f->fptr == &jl_f_instantiate_type && nargs > 0) {
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

static Value *emit_jlcall(Value *theFptr, Value *theF, int argStart,
                          size_t nargs, jl_codectx_t *ctx)
{
    // call
    Value *myargs;
    if (nargs > 0)
        myargs = emit_temp_slot(argStart, ctx);
    else
        myargs = Constant::getNullValue(T_ppjlvalue);
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                        ConstantInt::get(T_int32,nargs)});
#else
    Value *result = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
#endif
    ctx->gc.argDepth = argStart; // clear the args from the gcstack
    return result;
}

static Value *emit_jlcall(Value *theFptr, Value *theF, jl_value_t **args,
                          size_t nargs, jl_codectx_t *ctx)
{
    // emit arguments
    int argStart = ctx->gc.argDepth;
    for(size_t i=0; i < nargs; i++) {
        jl_cgval_t anArg = emit_expr(args[i], ctx, true, true);
        // put into argument space
        make_gcroot(boxed(anArg, ctx, expr_type(args[i],ctx)), ctx);
    }
    return emit_jlcall(theFptr, theF, argStart, nargs, ctx);
}

static jl_cgval_t emit_call_function_object(jl_function_t *f, Value *theF, Value *theFptr,
                                        bool specialized,
                                        jl_value_t **args, size_t nargs,
                                        jl_codectx_t *ctx)
{
    if (f!=NULL && specialized && f->linfo!=NULL && f->linfo->specFunctionObject != NULL) {
        // emit specialized call site
        jl_value_t *jlretty = jl_ast_rettype(f->linfo, f->linfo->ast);
        bool retboxed;
        (void)julia_type_to_llvm(jlretty, &retboxed);
        Function *cf = (Function*)f->linfo->specFunctionObject;
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
        for(size_t i=0; i < nargs; i++) {
            Type *at = cft->getParamType(idx);
            jl_value_t *jt = jl_nth_slot_type(f->linfo->specTypes,i);
            bool isboxed;
            Type *et = julia_type_to_llvm(jt, &isboxed);
            if (type_is_ghost(et)) {
                // Still emit the expression in case it has side effects
                emit_expr(args[i+1], ctx);
                continue;
            }
            if (isboxed) {
                assert(at == T_pjlvalue && et == T_pjlvalue);
                jl_cgval_t origval = emit_expr(args[i+1], ctx);
                argvals[idx] = boxed(origval, ctx,expr_type(args[i+1],ctx));
                assert(dyn_cast<UndefValue>(argvals[idx]) == 0);
                // TODO: there should be a function emit_rooted that handles this, leaving
                // the value rooted if it was already, to avoid redundant stores.
                if (!origval.isboxed ||
                    (might_need_root(args[i+1]) && !is_stable_expr(args[i+1], ctx))) {
                    make_gcroot(argvals[idx], ctx);
                }
            }
            else if (et->isAggregateType()) {
                assert(at == PointerType::get(et, 0));
                jl_cgval_t arg = emit_unboxed(args[i+1], ctx);
                if (arg.isimmutable && !arg.needsgcroot && arg.ispointer) {
                    // can lazy load on demand, no copy needed
                    Value *argv = arg.V;
                    if (argv->getType() != at)
                        argv = builder.CreatePointerCast(argv, at);
                    argvals[idx] = argv;
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
                argvals[idx] = emit_unbox(et, emit_unboxed(args[i+1], ctx), jt);
            }
            idx++;
        }
        assert(idx == nfargs);
        CallInst *call = builder.CreateCall(prepare_call(cf), ArrayRef<Value*>(&argvals[0], nfargs));
        call->setAttributes(cf->getAttributes());
        return sret ? mark_julia_slot(result, jlretty) : mark_julia_type(call, retboxed, jlretty);
    }
    return mark_julia_type(emit_jlcall(theFptr, theF, &args[1], nargs, ctx), true, jl_any_type); // (typ will be patched up by caller)
}

static Value *emit_is_function(Value *x, jl_codectx_t *ctx)
{
    Value *xty = emit_typeof(x);
    Value *isfunc =
        builder.CreateICmpEQ(xty, literal_pointer_val((jl_value_t*)jl_function_type));
    return isfunc;
}

static jl_cgval_t emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx, jl_value_t *expr)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theF=NULL;
    jl_value_t *a0 = args[0];
    jl_value_t *hdtype;
    bool headIsGlobal = false;
    bool definitely_function = false;
    bool definitely_not_function = false;
    jl_cgval_t result;

    jl_function_t *f = (jl_function_t*)static_eval(a0, ctx, true);
    JL_GC_PUSH1(&f);
    if (f != NULL) {
        // function is a compile-time constant
        headIsGlobal = true;
        definitely_function = jl_is_func(f);
        definitely_not_function = !definitely_function;
        bool handled;
        if (jl_typeis(f, jl_intrinsic_type) || jl_is_func(f)) {
            handled = emit_known_call(&result, (jl_value_t*)f, args, nargs, ctx, &theFptr, &f, expr);
            assert(!jl_typeis(f,jl_intrinsic_type) || handled);
        }
        else {
            handled = emit_known_call(&result, (jl_value_t*)jl_module_call_func(ctx->module),
                                      args-1, nargs+1, ctx, &theFptr, &f, expr);
        }
        if (handled) {
            JL_GC_POP();
            return result;
        }
    }

    hdtype = expr_type(a0, ctx);
    definitely_function |= (hdtype == (jl_value_t*)jl_function_type);
    definitely_not_function |= (jl_is_leaf_type(hdtype) && !definitely_function);

    assert(!(definitely_function && definitely_not_function));

    int last_depth = ctx->gc.argDepth;

    if (definitely_not_function) {
        f = jl_module_call_func(ctx->module);
        bool handled = emit_known_call(NULL, (jl_value_t*)f, args-1, nargs+1, ctx, &theFptr, &f, expr);
        assert(!handled); (void)handled;
        if (theFptr == NULL) {
            just_emit_error("\"call\" is not a generic function", ctx);
            result = jl_cgval_t();
        }
        else {
            theF = literal_pointer_val((jl_value_t*)f);
            result = emit_call_function_object(f, theF, theFptr, true, args-1, nargs+1, ctx);
        }
    }
    else if (definitely_function) {
        bool specialized = true;
        if (theFptr == NULL) {
            specialized = false;
            if (f != NULL) {
                // builtin functions don't need the function object passed and are constant
                std::map<jl_fptr_t,Function*>::iterator it = builtin_func_map.find(f->fptr);
                if (it != builtin_func_map.end()) {
                    theFptr = (*it).second;
                    theF = V_null;
                }
            }
            if (theFptr == NULL) {
                Value *theFunc = boxed(emit_expr(args[0], ctx), ctx);
                if (!headIsGlobal && (jl_is_expr(a0) || jl_is_lambda_info(a0)))
                    make_gcroot(theFunc, ctx);
                // extract pieces of the function object
                // TODO: try extractvalue instead
                theFptr = emit_nthptr_recast(theFunc, (ssize_t)(offsetof(jl_function_t,fptr)/sizeof(void*)), tbaa_func, jl_pfptr_llvmt);
                theF = theFunc;
            }
        }
        else {
            theF = literal_pointer_val((jl_value_t*)f);
        }
        result = emit_call_function_object(f, theF, theFptr, specialized, args, nargs, ctx);
    }
    else {
        // either direct function, or use call(), based on run-time branch

        // emit "function" and arguments
        int argStart = ctx->gc.argDepth;
        Value *theFunc = boxed(emit_expr(args[0], ctx), ctx);
        make_gcroot(theFunc, ctx);
        for(size_t i=0; i < nargs; i++) {
            jl_cgval_t anArg = emit_expr(args[i+1], ctx);
            // put into argument space
            make_gcroot(boxed(anArg, ctx, expr_type(args[i+1],ctx)), ctx);
        }

        Value *isfunc = emit_is_function(theFunc, ctx);
        BasicBlock *funcBB1 = BasicBlock::Create(getGlobalContext(),"isf", ctx->f);
        BasicBlock *elseBB1 = BasicBlock::Create(getGlobalContext(),"notf");
        BasicBlock *mergeBB1 = BasicBlock::Create(getGlobalContext(),"mergef");
        builder.CreateCondBr(isfunc, funcBB1, elseBB1);

        builder.SetInsertPoint(funcBB1);
        // is function
        Value *myargs;
        if (nargs > 0)
            myargs = emit_temp_slot(argStart + 1, ctx); // argStart holds theFunc, argStart + 1 holds the start of the argument list
        else
            myargs = Constant::getNullValue(T_ppjlvalue); // no arguments
        theFptr = emit_nthptr_recast(theFunc, (ssize_t)(offsetof(jl_function_t,fptr)/sizeof(void*)), tbaa_func, jl_pfptr_llvmt);
#ifdef LLVM37
        Value *r1 = builder.CreateCall(prepare_call(theFptr), {theFunc, myargs,
                                        ConstantInt::get(T_int32,nargs)});
#else
        Value *r1 = builder.CreateCall3(prepare_call(theFptr), theFunc, myargs,
                                        ConstantInt::get(T_int32,nargs));
#endif
        builder.CreateBr(mergeBB1);
        ctx->f->getBasicBlockList().push_back(elseBB1);
        builder.SetInsertPoint(elseBB1);
        // not function
        myargs = emit_temp_slot(argStart, ctx);
        jl_value_t *call_func = (jl_value_t*)jl_module_call_func(ctx->module);
        Value *r2;
        if (!jl_is_gf(call_func)) {
            just_emit_error("\"call\" is not a generic function", ctx);
            r2 = UndefValue::get(T_pjlvalue);
        }
        else {
#ifdef LLVM37
            r2 = builder.CreateCall(prepare_call(jlapplygeneric_func),
                                    {literal_pointer_val(call_func),
                                     myargs,
                                     ConstantInt::get(T_int32, nargs + 1)});
#else
            r2 = builder.CreateCall3(prepare_call(jlapplygeneric_func),
                                     literal_pointer_val(call_func),
                                     myargs,
                                     ConstantInt::get(T_int32, nargs + 1));
#endif
        }
        builder.CreateBr(mergeBB1);
        ctx->f->getBasicBlockList().push_back(mergeBB1);
        builder.SetInsertPoint(mergeBB1);
        PHINode *ph = builder.CreatePHI(T_pjlvalue, 2);
        ph->addIncoming(r1, funcBB1);
        ph->addIncoming(r2, elseBB1);
        result = mark_julia_type(ph, true, jl_any_type);
    }
    if (result.typ == (jl_value_t*)jl_any_type)
        result = remark_julia_type(result, expr_type(expr, ctx)); // patch up typ if necessary

    ctx->gc.argDepth = last_depth; // remove the arguments from the gc stack
    JL_GC_POP();
    return result;
}

// --- accessing and assigning variables ---

static int is_global(jl_sym_t *s, jl_codectx_t *ctx)
{
    std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find(s);
    return (it == ctx->vars.end());
}

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
            Constant *initnul = ConstantPointerNull::get((PointerType*)T_pjlvalue);
            GlobalVariable *bindinggv =
                new GlobalVariable(*jl_Module, T_pjlvalue,
                                   false, GlobalVariable::PrivateLinkage,
                                   initnul, "delayedvar");
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
    return mark_julia_type(v, true, jl_any_type);
}

static jl_cgval_t emit_var(jl_sym_t *sym, jl_codectx_t *ctx, bool isboxed)
{
    bool isglobal = is_global(sym, ctx);
    if (isglobal) {
        // look for static parameter
        for(size_t i=0; i < jl_svec_len(ctx->sp); i+=2) {
            assert(jl_is_symbol(jl_svecref(ctx->sp, i)));
            if (sym == (jl_sym_t*)jl_svecref(ctx->sp, i)) {
                jl_value_t *sp = jl_svecref(ctx->sp, i+1);
                return mark_julia_const(sp);
            }
        }
        jl_binding_t *jbp=NULL;
        Value *bp = global_binding_pointer(ctx->module, sym, &jbp, false, ctx);
        assert(bp != NULL);
        if (jbp && jbp->value != NULL) {
            if (jbp->constp) {
                if (!isboxed && jl_isbits(jl_typeof(jbp->value)))
                    return emit_unboxed(jbp->value, ctx);
                else
                    return mark_julia_const(jbp->value);
            }
            // double-check that a global variable is actually defined. this
            // can be a problem in parallel when a definition is missing on
            // one machine.
            return mark_julia_type(builder.CreateLoad(bp), true, jl_any_type);
        }
        return emit_checked_var(bp, sym, ctx);
    }

    jl_varinfo_t &vi = ctx->vars[sym];
    if (vi.memloc) {
        Value *bp = vi.memloc;
        if (vi.isBox) {
            Instruction *load = builder.CreateLoad(bp);
            if (vi.closureidx != -1) {
                // if the jl_box_t in the closure env, it will be const in the function
                load = tbaa_decorate(tbaa_const, load);
            }
            bp = builder.CreatePointerCast(load, T_ppjlvalue);
        }
        if (vi.isArgument ||  // arguments are always defined
            ((vi.closureidx == -1 || !vi.isAssigned) && !vi.usedUndef)) {
            // if no undef usage was found by inference, and it's either not assigned or not in env it must be always defined
            Instruction *v = builder.CreateLoad(bp, vi.isVolatile);
            if (vi.closureidx != -1 && !vi.isAssigned) {
                // if it's in the closure env, but only assigned by the parent function, it will be const while in the child function
                v = tbaa_decorate(tbaa_const, v);
            }
            return mark_julia_type(v, true, vi.value.typ);
        }
        else {
            jl_cgval_t v = emit_checked_var(bp, sym, ctx, vi.isVolatile);
            v = remark_julia_type(v, vi.value.typ); // patch up typ, is possible
            return v;
        }
    }
    else if (vi.isVolatile && !vi.value.isghost) {
        // copy value to a non-volatile location
        assert(vi.value.ispointer);
        Type *T = julia_type_to_llvm(vi.value.typ)->getPointerTo();
        Value *v = vi.value.V;
        if (v->getType() != T)
            v = builder.CreatePointerCast(v, T);
        return mark_julia_type(builder.CreateLoad(v, true), false, vi.value.typ);
    }
    else {
        return vi.value;
    }
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    if (jl_is_gensym(l)) {
        ssize_t idx = ((jl_gensym_t*)l)->id;
        assert(idx >= 0);
        assert(!ctx->gensym_assigned.at(idx));
        jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
        jl_value_t *declType = (jl_is_array(gensym_types) ? jl_cellref(gensym_types, idx) : (jl_value_t*)jl_any_type);
        jl_cgval_t slot; // slot is a jl_value_t or jl_value_t*
        if (store_unboxed_p(declType)) {
            Type *vtype = julia_type_to_llvm(declType);
            assert(vtype != T_pjlvalue);
            if (type_is_ghost(vtype)) {
                slot = emit_expr(r, ctx);
            }
            else {
                slot = emit_unboxed(r, ctx);
                if (slot.ispointer) { // emit a copy of boxed isbits values. TODO: elid this copy if unnecessary
                    slot = mark_julia_type(
                            emit_unbox(julia_type_to_llvm(declType), slot, declType),
                            false,
                            declType);
                }
            }
        }
        else {
            Value *rval = boxed(emit_expr(r, ctx, true), ctx);
            if (!is_stable_expr(r, ctx)) {
                // add a gc root for this GenSym node
                Value *bp = emit_local_slot(ctx->gc.argSpaceSize++, ctx);
                builder.CreateStore(rval, bp);
            }
            slot = mark_julia_type(rval, true, declType);
        }
        ctx->gensym_SAvalues.at(idx) = slot; // now gensym_SAvalues[idx] contains the SAvalue
        assert(ctx->gensym_assigned.at(idx) = true); // (assignment, not comparison test)
        return;
    }

    jl_sym_t *s = NULL;
    jl_binding_t *bnd = NULL;
    Value *bp = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_symbolnode(l))
        s = jl_symbolnode_sym(l);
    else if (jl_is_globalref(l))
        bp = global_binding_pointer(jl_globalref_mod(l), jl_globalref_name(l), &bnd, true, ctx); // now bp != NULL
    else
        assert(false);
    if (bp == NULL && is_global(s, ctx)) {
        bp = global_binding_pointer(ctx->module, s, &bnd, true, ctx); // now bp != NULL
    }
    if (bp != NULL) { // it's a global
        assert(bnd);
        Value *rval = boxed(emit_expr(r, ctx, true),ctx);
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

    // it's a local variable or closure variable
    jl_varinfo_t &vi = ctx->vars[s];
    if (!vi.memloc && !vi.hasGCRoot && vi.used
            && !vi.isArgument && !is_stable_expr(r, ctx)) {
        Instruction *newroot = cast<Instruction>(emit_local_slot(ctx->gc.argSpaceSize++, ctx));
        newroot->removeFromParent(); // move it to the gc frame basic block so it can be reused as needed
        newroot->insertAfter(ctx->gc.last_gcframe_inst);
        vi.memloc = newroot;
        vi.hasGCRoot = true; // this has been discovered to need a gc root, add it now
        //TODO: move this logic after the emit_expr
    }

    if (vi.memloc || !vi.hasGCRoot) {
        // boxed or unused variables
        jl_cgval_t rval_info = emit_expr(r, ctx, true);
        if (!vi.used)
            return;
        Value *rval = boxed(rval_info, ctx);
        if (vi.memloc) {
            Value *bp = vi.memloc;
            if ((jl_is_symbol(r) || jl_is_symbolnode(r)) && (!rval_info.typ || rval_info.typ == jl_bottom_type)) {
                if (vi.usedUndef) {
                    // sometimes x = y::Union{} occurs
                    jl_sym_t *s;
                    if (jl_is_symbolnode(r))
                        s = jl_symbolnode_sym(r);
                    else
                        s = (jl_sym_t*)r;
                    builder.CreateCall(prepare_call(jlundefvarerror_func), literal_pointer_val((jl_value_t*)s));
                }
                return;
            }
            if (vi.isBox) {
                bp = builder.CreatePointerCast(builder.CreateLoad(bp), T_ppjlvalue);
            }
            builder.CreateStore(rval, bp, vi.isVolatile);
            if (vi.isBox) {
                // bp is a jl_box_t*
                emit_write_barrier(ctx, bp, rval);
            }
        }
        else {
            // SSA variable w/o gcroot, just track the value info
            assert(vi.isSA);
            if (store_unboxed_p(vi.value.typ) && rval_info.ispointer) { // emit a copy of boxed isbits values. TODO: elid this copy if unnecessary
                rval_info = mark_julia_type(
                        emit_unbox(julia_type_to_llvm(vi.value.typ), rval_info, vi.value.typ),
                        false,
                        vi.value.typ);
            }
            vi.value = rval_info;
        }
        // add info to arravar list
        if (rval && !isa<UndefValue>(rval) && rval_info.isboxed) {
            // check isboxed in case rval isn't the right type (for example, on a dead branch),
            // so we don't try to assign it to the arrayvar info
            jl_arrayvar_t *av = arrayvar_for(l, ctx);
            if (av != NULL) {
                assign_arrayvar(*av, rval_info);
            }
        }
    }
    else if (vi.value.isghost) {
        // virtual store
        (void)emit_expr(r, ctx);
    }
    else {
        // store unboxed
        assert(vi.value.ispointer);
        builder.CreateStore(emit_unbox(
                    julia_type_to_llvm(vi.value.typ),
                    emit_unboxed(r, ctx),
                    vi.value.typ),
                vi.value.V, vi.isVolatile);
    }
}

// --- convert expression to code ---

static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx)
{
    jl_cgval_t condV = emit_unboxed(cond, ctx);
    if (condV.typ == (jl_value_t*)jl_bool_type) {
        Value *cond = emit_unbox(T_int1, condV, (jl_value_t*)jl_bool_type);
        assert(cond->getType() == T_int1);
        return builder.CreateXor(cond, ConstantInt::get(T_int1,1));
    }
    emit_typecheck(condV, (jl_value_t*)jl_bool_type, msg, ctx);
    if (condV.isboxed) {
        return builder.CreateICmpEQ(condV.V, tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var))));
    }
    // not a boolean
    return ConstantInt::get(T_int1,0); // TODO: replace with Undef
}

static jl_cgval_t emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool isboxed, bool valuepos)
{
    if (jl_is_symbol(expr)) {
        if (!valuepos) return jl_cgval_t(); // value not used, no point in doing codegen for it
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_var(sym, ctx, isboxed);
    }
    if (jl_is_symbolnode(expr)) {
        if (!valuepos) return jl_cgval_t(); // value not used, no point in doing codegen for it
        jl_sym_t *sym = jl_symbolnode_sym(expr);
        jl_value_t *typ = jl_symbolnode_type(expr);
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
        jl_cgval_t val = emit_var(sym, ctx, isboxed);
        if (val.isboxed)
            val = remark_julia_type(val, typ); // patch up typ to match SymbolNode.type
        return val; // patch up typ to match SymbolNode.type
    }
    if (jl_is_gensym(expr)) {
        if (!valuepos) return jl_cgval_t(); // value not used, no point in doing codegen for it
        ssize_t idx = ((jl_gensym_t*)expr)->id;
        assert(idx >= 0);
        //assert(ctx->gensym_assigned.at(idx));
        jl_cgval_t val = ctx->gensym_SAvalues.at(idx); // at this point, gensym_SAvalues[idx] actually contains the SAvalue
        return val;
    }
    if (jl_is_labelnode(expr)) {
        int labelname = jl_labelnode_label(expr);
        BasicBlock *bb = (*ctx->labels)[labelname];
        assert(bb);
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            builder.CreateBr(bb); // all BasicBlocks must exit explicitly
        }
        ctx->f->getBasicBlockList().push_back(bb);
        builder.SetInsertPoint(bb);
        return jl_cgval_t();
    }
    if (jl_is_linenode(expr)) {
        if (valuepos)
            jl_error("Linenode in value position");
        return jl_cgval_t();
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
    if (jl_is_newvarnode(expr)) {
        assert(!valuepos);
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
        assert(!jl_is_gensym(var));
        assert(jl_is_symbol(var));
        jl_varinfo_t &vi = ctx->vars[var];
        Value *lv = vi.memloc;
        if (lv != NULL) {
            // create a new uninitialized variable
            if (vi.isBox) {
                builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), V_null), lv);
            }
            else if (vi.usedUndef) {
                builder.CreateStore(V_null, lv);
            }
        }
        return jl_cgval_t();
    }
    if (jl_is_lambda_info(expr)) {
        return mark_julia_type(emit_lambda_closure(expr, ctx), true, jl_function_type);
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
        return emit_call(args, jl_array_dim0(ex->args), ctx, (jl_value_t*)ex);
    }
    else if (head == assign_sym) {
        emit_assignment(args[0], args[1], ctx);
        if (valuepos) {
            return ghostValue(jl_void_type);
        }
    }
    else if (head == method_sym) {
        jl_value_t *mn = args[0];
        bool iskw = false;
        Value *theF = NULL;
        if (jl_is_expr(mn) || jl_is_globalref(mn)) {
            if (jl_is_expr(mn) && ((jl_expr_t*)mn)->head == kw_sym) {
                iskw = true;
                mn = jl_exprarg(mn,0);
            }
            theF = boxed(emit_expr(mn, ctx), ctx);
            if (!iskw) {
                mn = jl_fieldref(jl_exprarg(mn, 2), 0);
            }
        }
        if (jl_is_symbolnode(mn)) {
            mn = (jl_value_t*)jl_symbolnode_sym(mn);
        }
        assert(jl_is_symbol(mn));
        int last_depth = ctx->gc.argDepth;
        Value *name = literal_pointer_val(mn);
        jl_binding_t *bnd = NULL;
        Value *bp, *bp_owner = V_null;
        if (theF != NULL) {
            bp = make_gcroot(theF, ctx);
        }
        else {
            if (is_global((jl_sym_t*)mn, ctx)) {
                bnd = jl_get_binding_for_method_def(ctx->module, (jl_sym_t*)mn);
                bp = julia_binding_gv(bnd);
                bp_owner = literal_pointer_val((jl_value_t*)ctx->module);
            }
            else {
                jl_sym_t *s = (jl_sym_t*)mn;
                jl_varinfo_t &vi = ctx->vars[s];
                bp = vi.memloc;
                if (vi.isBox) {
                    // bp is a jl_box_t*
                    bp = builder.CreatePointerCast(builder.CreateLoad(bp), T_ppjlvalue);
                    bp_owner = builder.CreateBitCast(bp, T_pjlvalue);
                }
            }
        }
        if (jl_expr_nargs(ex) == 1) {
            Value *mdargs[4] = { name, bp, bp_owner, literal_pointer_val(bnd) };
            return mark_julia_type(
                    builder.CreateCall(prepare_call(jlgenericfunction_func), ArrayRef<Value*>(&mdargs[0], 4)),
                    true,
                    jl_function_type);
        }
        else {
            Value *a1 = boxed(emit_expr(args[1], ctx),ctx);
            make_gcroot(a1, ctx);
            Value *a2 = boxed(emit_expr(args[2], ctx),ctx);
            make_gcroot(a2, ctx);
            Value *mdargs[9] =
                { name, bp, bp_owner, literal_pointer_val(bnd), a1, a2, literal_pointer_val(args[3]),
                  literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
                  ConstantInt::get(T_int32, (int)iskw) };
            ctx->gc.argDepth = last_depth;
            return mark_julia_type(
                    builder.CreateCall(prepare_call(jlmethod_func), ArrayRef<Value*>(&mdargs[0], 9)),
                    true,
                    jl_function_type);
        }
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        assert(jl_is_symbol(sym));
        jl_binding_t *bnd = NULL;
        if (is_global(sym, ctx)) {
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
            return emit_new_struct(jl_tparam0(ty),nargs,args,ctx);
        }
        Value *typ = boxed(emit_expr(args[0], ctx), ctx);
        Value *val = emit_jlcall(jlnew_func, typ, &args[1], nargs-1, ctx);
        return mark_julia_type(val, true, ty);
    }
    else if (head == exc_sym) { // *jl_exception_in_transit
        return mark_julia_type(builder.CreateLoad(prepare_global(jlexc_var), /*isvolatile*/true), true, jl_any_type);
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
                    builder.CreateLoad(prepare_global(jlexc_var), true)),
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
    else if (head == boundscheck_sym) {
        if (jl_array_len(ex->args) > 0 &&
            jl_options.check_bounds == JL_OPTIONS_CHECK_BOUNDS_DEFAULT) {
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
        return mark_julia_type(builder.CreateCall(prepare_call(jlcopyast_func), boxed(ast, ctx)), true, ast.typ);
    }
    else if (head == simdloop_sym) {
        if (!llvm::annotateSimdLoop(builder.GetInsertBlock()))
            jl_printf(JL_STDERR, "WARNING: could not attach metadata for @simd loop.\n");
        return jl_cgval_t();
    }
    else if (head == meta_sym) {
        return ghostValue(jl_void_type);  // will change as new metadata gets added
    }
    else {
        if (!strcmp(head->name, "$"))
            jl_error("syntax: prefix \"$\" in non-quoted expression");
        if (jl_is_toplevel_only_expr(expr) &&
            ctx->linfo->name == anonymous_sym && ctx->vars.empty() &&
            ctx->linfo->module == jl_current_module) {
            // call interpreter to run a toplevel expr from inside a
            // compiled toplevel thunk.
            builder.CreateCall(prepare_call(jltopeval_func), literal_pointer_val(expr));
            jl_add_linfo_root(ctx->linfo, expr);
            return ghostValue(jl_void_type);
        }
        // some expression types are metadata and can be ignored
        if (valuepos || !(head == line_sym || head == type_goto_sym)) {
            if (head == abstracttype_sym || head == compositetype_sym ||
                head == bitstype_sym) {
                jl_errorf("type definition not allowed inside a local scope");
            }
            else if (head == macro_sym) {
                jl_errorf("macro definition not allowed inside a local scope");
            }
            else {
                jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                          head->name, ctx->linfo->name->name);
            }
        }
    }
    return jl_cgval_t();
}

// --- generate function bodies ---

extern "C" jl_svec_t *jl_svec_tvars_to_symbols(jl_svec_t *t);

// gc frame emission
static void allocate_gc_frame(size_t n_roots, BasicBlock *b0, jl_codectx_t *ctx)
{
    // allocate a placeholder gc frame
    jl_gcinfo_t *gc = &ctx->gc;
    gc->argSpaceSize = n_roots;
    gc->argDepth = 0;
    gc->maxDepth = 0;

    gc->gcframe = builder.CreateAlloca(T_pjlvalue, ConstantInt::get(T_int32, 0));
#ifdef JL_DEBUG_BUILD
    gc->gcframe->setName("gcrootframe");
#endif
    gc->first_gcframe_inst = BasicBlock::iterator(gc->gcframe);
    gc->argSlot = builder.CreateConstGEP1_32(gc->gcframe, 2);
#ifdef JL_DEBUG_BUILD
    gc->argSlot->setName("locals");
#endif
    gc->tempSlot = (GetElementPtrInst*)builder.CreateConstGEP1_32(gc->gcframe, 2);
#ifdef JL_DEBUG_BUILD
    gc->tempSlot->setName("temproots");
#endif
    gc->last_gcframe_inst = BasicBlock::iterator((Instruction*)gc->tempSlot);
}

static void clear_gc_frame(jl_gcinfo_t *gc)
{
    // replace instruction uses with Undef first to avoid LLVM assertion failures
    BasicBlock::iterator bbi = gc->first_gcframe_inst;
    while (1) {
        Instruction &iii = *bbi;
        Type *ty = iii.getType();
        if (ty != T_void)
            iii.replaceAllUsesWith(UndefValue::get(ty));
        if (bbi == gc->last_gcframe_inst) break;
        bbi++;
    }
    // Remove GC frame creation
    // (instructions from gc->gcframe to gc->last_gcframe_inst)
    BasicBlock::InstListType &il = gc->gcframe->getParent()->getInstList();
    il.erase(gc->first_gcframe_inst, gc->last_gcframe_inst);
    // erase() erases up *to* the end point; erase last inst too
    il.erase(gc->last_gcframe_inst);
}

static void
emit_gcpops(jl_codectx_t *ctx)
{
    Function *F = ctx->f;
    for(Function::iterator I = F->begin(), E = F->end(); I != E; ++I) {
        if (isa<ReturnInst>(I->getTerminator())) {
            builder.SetInsertPoint(I->getTerminator()); // set insert *before* Ret
            Instruction *gcpop =
                (Instruction*)builder.CreateConstGEP1_32(ctx->gc.gcframe, 1);
            builder.CreateStore(builder.CreatePointerCast(builder.CreateLoad(gcpop),
                                                      T_ppjlvalue),
                                prepare_global(jlpgcstack_var));
        }
    }
}

static void finalize_gc_frame(jl_codectx_t *ctx)
{
    jl_gcinfo_t *gc = &ctx->gc;
    if (gc->argSpaceSize + gc->maxDepth == 0) {
        // 0 roots; remove gc frame entirely
        clear_gc_frame(gc);
        return;
    }
    BasicBlock::iterator bbi(gc->gcframe);
    AllocaInst *newgcframe = gc->gcframe;
    builder.SetInsertPoint(++gc->last_gcframe_inst); // set insert *before* point, e.g. after the gcframe
    // Allocate the real GC frame
    // n_frames++;
    newgcframe->setOperand(0, ConstantInt::get(T_int32, 2 + gc->argSpaceSize + gc->maxDepth)); // fix up the size of the gc frame
    gc->tempSlot->setOperand(1, ConstantInt::get(T_int32, 2 + gc->argSpaceSize)); // fix up the offset to the temp slot space
    builder.CreateStore(ConstantInt::get(T_size, (gc->argSpaceSize + gc->maxDepth) << 1),
                        builder.CreateBitCast(builder.CreateConstGEP1_32(newgcframe, 0), T_psize));
    builder.CreateStore(builder.CreateLoad(prepare_global(jlpgcstack_var)),
                        builder.CreatePointerCast(builder.CreateConstGEP1_32(newgcframe, 1), PointerType::get(T_ppjlvalue,0)));
    builder.CreateStore(newgcframe, prepare_global(jlpgcstack_var));
    // Initialize the slots for temporary variables to NULL
    for (int i = 0; i < gc->argSpaceSize; i++) {
        Value *argTempi = emit_local_slot(i, ctx);
        builder.CreateStore(V_null, argTempi);
    }
    for (int i = 0; i < gc->maxDepth; i++) {
        Value *argTempi = emit_temp_slot(i, ctx);
        builder.CreateStore(V_null, argTempi);
    }
    emit_gcpops(ctx);
}

static Function *gen_cfun_wrapper(jl_function_t *ff, jl_value_t *jlrettype, jl_tupletype_t *argt, int64_t isref)
{
    jl_lambda_info_t *lam = ff->linfo;
    cFunctionList_t *list = (cFunctionList_t*)lam->cFunctionList;
    if (list != NULL) {
        size_t i;
        for (i = 0; i < list->len; i++) {
            if (list->data[i].isref == isref) {
                return list->data[i].f;
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
    for(i=0; i < nargs; i++) {
        jl_value_t *tti = jl_nth_slot_type(lam->specTypes,i);
        if (tti == (jl_value_t*)jl_pointer_type) {
            jl_error("cfunction: argument type Ptr should have an element type, Ptr{T}");
        }
    }

    std::vector<Type*> fargt(0);
    std::vector<bool> fargt_isboxed(0);
    std::vector<Type*> fargt_sig(0);
    Type* fargt_vasig;
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

    jl_compile(ff);
    if (!lam->functionObject) {
        jl_errorf("error compiling %s while creating cfunction", lam->name->name);
    }

    std::stringstream funcName;
    funcName << "jlcapi_" << lam->name->name << "_" << globalUnique++;

    // Backup the info for the nested compile
    JL_SIGATOMIC_BEGIN(); // no errors expected beyond this point
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    jl_gc_inhibit_finalizers(nested_compile); // no allocations expected between the top of this function (when last scanned lam->cFunctionList) and here, which might have triggered running julia code

    // Create the Function stub
    Module *m;
#ifdef USE_MCJIT
    if (!imaging_mode) {
        m = new Module(funcName.str(), jl_LLVMContext);
        jl_setup_module(m,true);
    }
    else {
        m = shadow_module;
    }
#else
    m = jl_Module;
#endif

    Function *cw = Function::Create(FunctionType::get(sret ? T_void : prt, fargt_sig, false),
            imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
            funcName.str(), m);
    cw->setAttributes(attrs);
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", cw);
    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx;
    ctx.f = cw;
    ctx.linfo = lam;
    ctx.sret = false;
    allocate_gc_frame(0, b0, &ctx);

    // Save the Function object reference
    int len = (list ? list->len : 0) + 1;
    cFunctionList_t *list2 = (cFunctionList_t*)realloc(list, sizeof(*list)+sizeof(list->data[0])*len);
    if (!list2)
        jl_throw(jl_memory_exception);
    list2->len = len;
    list2->data[len-1].isref = isref;
    list2->data[len-1].f = cw;
    lam->cFunctionList = list2;

    // See whether this function is specsig or jlcall
    bool specsig, jlfunc_sret;
    Function *theFptr;
    if (lam->specFunctionObject != NULL) {
        theFptr = (Function*)lam->specFunctionObject;
        specsig = true;
        jlfunc_sret = theFptr->hasStructRetAttr();
    }
    else {
        theFptr = (Function*)lam->functionObject;
        specsig = false;
        jlfunc_sret = false;
    }
    assert(theFptr);

    // Alright, let's do this!
    // let's first emit the arguments
    std::vector<Value*> args;
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = NULL;
    if (sret)
        sretPtr = AI++;

    Value *result;
    size_t FParamIndex = 0;
    if (jlfunc_sret) {
        if (sret)
            result = sretPtr;
        else
            result = builder.CreateAlloca(theFptr->getFunctionType()->getParamType(0)->getContainedType(0));
        args.push_back(result);
        FParamIndex++;
    }

    for (size_t i = 0; i < nargs; i++) {
        Value *val = AI++;
        jl_value_t *jargty = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed, argboxed;
        Type *t = julia_type_to_llvm(jargty, &isboxed);
        (void)julia_struct_to_llvm(jargty, &argboxed);
        jl_cgval_t inputarg;

        // figure out how to unpack this type
        if (isref & (2<<i)) {
            if (!jl_isbits(jargty)) {
                inputarg = mark_julia_type(builder.CreatePointerCast(val, T_pjlvalue), true, jargty);
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
                    inputarg = mark_julia_type(val, false, jargty);
                }
            }
        }
        else if (argboxed) {
            inputarg = mark_julia_type(val, true, jargty);
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
                inputarg = mark_julia_type(mem, true, jargty);
            }
            else {
                inputarg = mark_julia_type(val, false, jargty);
            }
        }

        // figure out how to repack this type
        if (!specsig) {
            Value *arg = boxed(inputarg, &ctx);
            make_gcroot(arg, &ctx);
        }
        else {
            Value *arg;
            FParamIndex++;
            if (isboxed) {
                arg = boxed(inputarg, &ctx);
                make_gcroot(arg, &ctx);
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
        retval = mark_julia_type(jlfunc_sret ? (Value*)builder.CreateLoad(result) : (Value*)call, retboxed, jlrettype);
    }
    else {
        Value *ret = emit_jlcall(theFptr, literal_pointer_val((jl_value_t*)ff), 0, nargs, &ctx);
        retval = mark_julia_type(ret, true, jlrettype);
    }

    // Prepare the return value
    Value *r;
    if (isref & 1) {
        assert(!sret);
        // return a jl_value_t*
        r = boxed(retval, &ctx);
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
    finalize_gc_frame(&ctx);

#ifdef JL_DEBUG_BUILD
#ifdef LLVM35
    llvm::raw_fd_ostream out(1,false);
#endif
    if (
#ifdef LLVM35
        verifyFunction(*cw,&out)
#else
        verifyFunction(*cw,PrintMessageAction)
#endif
    ) {
        cw->dump();
        abort();
    }
#endif

#ifdef USE_MCJIT
    if (imaging_mode) {
        // Copy the function out of the shadow module
        Module *m = new Module("julia", jl_LLVMContext);
        jl_setup_module(m, true);
        FunctionMover mover(m, shadow_module);
        Function *clone = mover.CloneFunction(cw);
        FPM->run(*clone);
    }
    else {
        FPM->run(*cw);
    }
#endif

    // Restore the previous compile context
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    nested_compile = last_n_c;
    jl_gc_inhibit_finalizers(nested_compile);
    JL_SIGATOMIC_END();

    return cw;
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_lambda_info_t *lam, jl_expr_t *ast, Function *f, bool sret)
{
    std::stringstream funcName;
    const std::string &fname = f->getName().str();
    funcName << "jlcall_";
    if (fname.compare(0, 6, "julia_") == 0)
        funcName << fname.substr(6);
    else
        funcName << fname;

    Function *w = Function::Create(jl_func_sig, imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                                   funcName.str(), f->getParent());
    addComdat(w);
    Function::arg_iterator AI = w->arg_begin();
    /* const Argument &fArg = */ *AI++;
    Value *argArray = AI++;
    /* const Argument &argCount = *AI++; */
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);

    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx;
    ctx.f = w;
    ctx.linfo = lam;
    ctx.sret = false;
    allocate_gc_frame(0, b0, &ctx);

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
        Value *argPtr = builder.CreateGEP(argArray,
                                          ConstantInt::get(T_size, i));
        Value *theArg = builder.CreateLoad(argPtr);
        if (lty != NULL && !isboxed) {
            theArg = builder.CreatePointerCast(theArg, PointerType::get(lty,0));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = builder.CreateLoad(theArg);
        }
        assert(dyn_cast<UndefValue>(theArg) == NULL);
        args[idx] = theArg;
        idx++;
    }
    // TODO: consider pulling the function pointer out of fArg so these
    // wrappers can be reused for different functions of the same type.
    CallInst *call = builder.CreateCall(prepare_call(f), ArrayRef<Value*>(&args[0], nfargs));
    call->setAttributes(f->getAttributes());

    jl_value_t *jlretty = jl_ast_rettype(lam, (jl_value_t*)ast);
    bool retboxed;
    (void)julia_type_to_llvm(jlretty, &retboxed);
    if (sret) { assert(!retboxed); }
    jl_cgval_t retval = sret ? mark_julia_slot(result, jlretty) : mark_julia_type(call, retboxed, jlretty);
    builder.CreateRet(boxed(retval, &ctx));
    finalize_gc_frame(&ctx);

    FPM->run(*w);

    return w;
}

// Compile to LLVM IR, using a specialized signature if applicable.
static Function *emit_function(jl_lambda_info_t *lam)
{
    // step 1. unpack AST and allocate codegen context for this function
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_svec_t *sparams = NULL;
    JL_GC_PUSH2(&ast, &sparams);
    if (!jl_is_expr(ast)) {
        ast = (jl_expr_t*)jl_uncompress_ast(lam, (jl_value_t*)ast);
    }
    assert(jl_is_expr(ast));
    sparams = jl_svec_tvars_to_symbols(lam->sparams);
    //jl_printf((jl_value_t*)ast);
    //jl_printf(JL_STDOUT, "\n");
    std::map<jl_sym_t*, jl_arrayvar_t> arrayvars;
    std::map<int, BasicBlock*> labels;
    std::map<int, Value*> handlers;
    jl_codectx_t ctx;
    ctx.arrayvars = &arrayvars;
    ctx.labels = &labels;
    ctx.handlers = &handlers;
    ctx.module = lam->module;
    ctx.ast = ast;
    ctx.sp = sparams;
    ctx.linfo = lam;
    ctx.funcName = lam->name->name;
    ctx.vaName = NULL;
    ctx.vaStack = false;
    ctx.boundsCheck.push_back(true);

    // step 2. process var-info lists to see what vars are captured, need boxing
    jl_value_t *gensym_types = jl_lam_gensyms(ast);
    int n_gensyms = (jl_is_array(gensym_types) ? jl_array_len(gensym_types) : jl_unbox_gensym(gensym_types));
    jl_array_t *largs = jl_lam_args(ast);
    size_t largslen = jl_array_dim0(largs);
    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t vinfoslen = jl_array_dim0(vinfos);
    jl_array_t *captvinfos = jl_lam_capt(ast);
    size_t captvinfoslen = jl_array_dim0(captvinfos);
    size_t nreq = largslen;
    int va = 0;
    if (!lam->specTypes)
        lam->specTypes = jl_anytuple_type;
    if (nreq > 0 && jl_is_rest_arg(jl_cellref(largs,nreq-1))) {
        nreq--;
        va = 1;
        ctx.vaName = jl_decl_var(jl_cellref(largs,nreq));
    }
    ctx.nReqArgs = nreq;

    size_t i;
    for(i=0; i < nreq; i++) {
        jl_value_t *arg = jl_cellref(largs,i);
        jl_sym_t *argname = jl_decl_var(arg);
        jl_varinfo_t &varinfo = ctx.vars[argname];
        varinfo.isArgument = true;
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        varinfo.value = mark_julia_type((Value*)NULL, false, ty);
    }
    if (va) {
        jl_varinfo_t &varinfo = ctx.vars[ctx.vaName];
        varinfo.isArgument = true;
        varinfo.value = mark_julia_type((Value*)NULL, false, jl_tuple_type);
    }

    for(i=0; i < vinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
        assert(jl_is_symbol(vname));
        jl_varinfo_t &varinfo = ctx.vars[vname];
        varinfo.isAssigned = (jl_vinfo_assigned(vi)!=0);
        varinfo.isCaptured = (jl_vinfo_capt(vi)!=0);
        if (varinfo.isCaptured && varinfo.isAssigned)
            varinfo.isBox = true;
        varinfo.escapes = varinfo.isCaptured;
        if (varinfo.isCaptured)
            varinfo.used = true;
        varinfo.isSA = (jl_vinfo_sa(vi)!=0);
        varinfo.usedUndef = (jl_vinfo_usedundef(vi)!=0) || (!varinfo.isArgument && !lam->inferred);
        jl_value_t *typ = jl_cellref(vi,1);
        if (!jl_is_type(typ))
            typ = (jl_value_t*)jl_any_type;
        varinfo.value = mark_julia_type((Value*)NULL, false, typ);
    }
    bool hasCapt = (captvinfoslen > 0);
    for(i=0; i < captvinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(captvinfos, i);
        assert(jl_is_array(vi));
        jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
        assert(jl_is_symbol(vname));
        jl_varinfo_t &varinfo = ctx.vars[vname];
        varinfo.closureidx = i;
        varinfo.isAssigned = (jl_vinfo_assigned(vi)!=0);
        varinfo.isCaptured = true;
        if (varinfo.isAssigned)
            varinfo.isBox = true;
        varinfo.escapes = true;
        varinfo.used = true;
        varinfo.usedUndef = (jl_vinfo_usedundef(vi)!=0) || !lam->inferred;
        jl_value_t *typ = jl_cellref(vi,1);
        if (!jl_is_type(typ))
            typ = (jl_value_t*)jl_any_type;
        varinfo.value = mark_julia_type((Value*)NULL, false, typ);
        varinfo.hasGCRoot = true;
    }

    // step 3. some variable analysis

    // finish recording escape info
    simple_escape_analysis((jl_value_t*)ast, true, &ctx);

    // determine which vars need to be volatile
    jl_array_t *stmts = jl_lam_body(ast)->args;
    mark_volatile_vars(stmts, ctx.vars);

    // step 4. determine function signature
    jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)ast);
    Function *f = NULL;

    bool specsig = false;
    if (!va && !hasCapt && lam->specTypes != jl_anytuple_type && lam->inferred) {
        // no captured vars and not vararg
        // consider specialized signature
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

    std::stringstream funcName;
    // try to avoid conflicts in the global symbol table
    funcName << "julia_" << lam->name->name;

    Module *m;
#ifdef USE_MCJIT
    if (!imaging_mode) {
        m = new Module(funcName.str(), jl_LLVMContext);
        jl_setup_module(m,true);
    }
    else {
        m = shadow_module;
    }
#else
    m = jl_Module;
#endif
    funcName << "_" << globalUnique++;

    ctx.sret = false;
    if (specsig) { // assumes !va
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
                             imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                             funcName.str(), m);
        if (ctx.sret)
            f->addAttribute(1, Attribute::StructRet);
        addComdat(f);
        if (lam->specFunctionObject == NULL) {
            lam->specFunctionObject = (void*)f;
            lam->specFunctionID = jl_assign_functionID(f);
        }
        if (lam->functionObject == NULL) {
            Function *fwrap = gen_jlcall_wrapper(lam, ast, f, ctx.sret);
            lam->functionObject = (void*)fwrap;
            lam->functionID = jl_assign_functionID(fwrap);
        }
    }
    else {
        f = Function::Create(jl_func_sig, imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                             funcName.str(), m);
        addComdat(f);
        if (lam->functionObject == NULL) {
            lam->functionObject = (void*)f;
            lam->functionID = jl_assign_functionID(f);
        }
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

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && LLVM35
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
    std::string filename = "no file";
    char *dbgFuncName = lam->name->name;
    int lno = -1;
    // look for initial (line num filename [funcname]) node, [funcname] for kwarg methods.
    if (jl_is_linenode(stmt)) {
        lno = jl_linenode_line(stmt);
        filename = jl_linenode_file(stmt)->name;
    }
    else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym &&
             jl_array_dim0(((jl_expr_t*)stmt)->args) > 0) {
        jl_value_t *a1 = jl_exprarg(stmt,0);
        if (jl_is_long(a1))
            lno = jl_unbox_long(a1);
        if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
            a1 = jl_exprarg(stmt,1);
            if (jl_is_symbol(a1))
                filename = ((jl_sym_t*)a1)->name;
            if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 2) {
                a1 = jl_exprarg(stmt,2);
                if (jl_is_symbol(a1))
                    dbgFuncName = ((jl_sym_t*)a1)->name;
            }
        }
    }
    int toplineno = lno;

    DIBuilder dbuilder(*m);
    ctx.dbuilder = &dbuilder;
#ifdef LLVM37
    DIFile *topfile = NULL;
    DISubprogram *SP;
#else
    DIFile topfile;
    DISubprogram SP;
#endif
    DebugLoc inlineLoc;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);

    //jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           lam->name->name, filename.c_str(), lno);

    DebugLoc noDbg;
    ctx.debug_enabled = true;
    if (dbgFuncName[0] == 0) {
        // special value: if function name is empty, disable debug info
        builder.SetCurrentDebugLocation(noDbg);
        ctx.debug_enabled = false;
        do_coverage = false;
        do_malloc_log = false;
    }
    else {
        // TODO: Fix when moving to new LLVM version
        #ifndef LLVM34
        dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        #elif LLVM37
        DICompileUnit *CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        #else
        DICompileUnit CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        assert(CU.Verify());
        #endif

#ifdef LLVM37
        DISubroutineType *subrty;
#elif LLVM36
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
                if (ctx.vars[jl_decl_var(jl_cellref(largs,i))].value.isghost)
                    continue;
                ditypes.push_back(julia_type_to_di(jl_tparam(lam->specTypes,i),ctx.dbuilder,false));
            }
#ifdef LLVM36
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
                                    f);           // Fn
        // set initial line number
        inlineLoc = DebugLoc::get(lno, 0, (MDNode*)SP, NULL);
        builder.SetCurrentDebugLocation(inlineLoc);
        #ifndef LLVM37
        assert(SP.Verify() && SP.describes(f) && SP.getFunction() == f);
        #endif
    }

    if (ctx.debug_enabled) {
        // Go over all arguments and local variables and initialize their debug information
        for(i=0; i < nreq; i++) {
            jl_sym_t *argname = jl_decl_var(jl_cellref(largs,i));
            jl_varinfo_t &varinfo = ctx.vars[argname];
#ifdef LLVM38
            varinfo.dinfo = ctx.dbuilder->createParameterVariable(
                SP,                                 // Scope (current function will be fill in later)
                argname->name,                      // Variable name
                ctx.sret + i + 1,                                // Argument number (1-based)
                topfile,                            // File
                toplineno == -1 ? 0 : toplineno,  // Line
                // Variable type
                julia_type_to_di(varinfo.value.typ,ctx.dbuilder,specsig));
#else
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,    // Tag
                SP,         // Scope (current function will be fill in later)
                argname->name,    // Variable name
                topfile,                    // File
                toplineno == -1 ? 0 : toplineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, ctx.dbuilder, specsig), // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + i + 1);                   // Argument number (1-based)
#endif
        }
        if (va) {
#ifdef LLVM38
            ctx.vars[ctx.vaName].dinfo = ctx.dbuilder->createParameterVariable(
                SP,                     // Scope (current function will be fill in later)
                ctx.vaName->name,       // Variable name
                ctx.sret + nreq + 1,               // Argument number (1-based)
                topfile,                    // File
                toplineno == -1 ? 0 : toplineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.vars[ctx.vaName].value.typ, ctx.dbuilder, false));
#else
            ctx.vars[ctx.vaName].dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,   // Tag
                SP,                                 // Scope (current function will be fill in later)
                ctx.vaName->name,                   // Variable name
                topfile,                             // File
                toplineno == -1 ? 0 : toplineno,  // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.vars[ctx.vaName].value.typ, ctx.dbuilder, false),      // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                ctx.sret + nreq + 1);              // Argument number (1-based)
#endif
        }
        for(i=0; i < vinfoslen; i++) {
            jl_sym_t *s = (jl_sym_t*)jl_cellref(jl_cellref(vinfos,i),0);
            jl_varinfo_t &varinfo = ctx.vars[s];
            if (varinfo.isArgument)
                continue;
#ifdef LLVM38
            varinfo.dinfo = ctx.dbuilder->createAutoVariable(
#else
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
#endif
                SP,                     // Scope (current function will be fill in later)
                s->name,                // Variable name
                topfile,                 // File
                toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, ctx.dbuilder, specsig), // Variable type
                false,                  // May be optimized out
                0                       // Flags (TODO: Do we need any)
#ifndef LLVM38
                ,0                      // Argument number (1-based)
#endif
                );
        }
        for(i=0; i < captvinfoslen; i++) {
            jl_array_t *vi = (jl_array_t*)jl_cellref(captvinfos, i);
            assert(jl_is_array(vi));
            jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
            assert(jl_is_symbol(vname));
            jl_varinfo_t &varinfo = ctx.vars[vname];
#ifdef LLVM38
            varinfo.dinfo = ctx.dbuilder->createAutoVariable(
#else
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
#endif
                SP,                     // Scope (current function will be filled in later)
                vname->name,            // Variable name
                topfile,                 // File
                toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.value.typ, ctx.dbuilder, specsig), // Variable type
                false,                  // May be optimized out
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

    Value *fArg=NULL, *argArray=NULL, *argCount=NULL;
    if (!specsig) {
        Function::arg_iterator AI = f->arg_begin();
        fArg = AI++;
        argArray = AI++;
        argCount = AI++;
        ctx.argArray = argArray;
        ctx.argCount = argCount;

#ifdef LLVM36
        // Declare arguments early so llvm in case any of the below emits basic blocks
        // before we get to loading local variables
        for(i=0; i < nreq; i++) {
            jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
            if (ctx.vars[s].dinfo != (MDNode*)NULL) {
                SmallVector<int64_t, 9> addr;
                addr.push_back(llvm::dwarf::DW_OP_plus);
                addr.push_back(i * sizeof(void*));
                //addr.push_back(llvm::dwarf::DW_OP_deref);
#ifdef LLVM37
                ctx.dbuilder->insertDbgValueIntrinsic(argArray, 0, ctx.vars[s].dinfo,
                ctx.dbuilder->createExpression(addr),
                builder.getCurrentDebugLocation().get(), builder.GetInsertBlock());
#else
                ctx.dbuilder->insertDbgValueIntrinsic(argArray, 0, ctx.vars[s].dinfo,
                ctx.dbuilder->createExpression(addr), builder.GetInsertBlock());
#endif
            }
        }
#endif
    }

    /*
    // step 6. (optional) check for stack overflow (the slower way)
    Value *cur_sp =
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::frameaddress),
                           ConstantInt::get(T_int32, 0));
    Value *sp_ok =
        builder.CreateICmpUGT(cur_sp,
                              ConstantInt::get(T_size,
                                               (uptrint_t)jl_stack_lo));
    error_unless(sp_ok, "stack overflow", &ctx);
    */

    // step 7. allocate local variables
    // must be in the first basic block for the llvm mem2reg pass to work
    int n_roots = 0;
    for(i=0; i < largslen; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        jl_varinfo_t &varinfo = ctx.vars[s];
        if (varinfo.value.isghost) {
            // no need to explicitly load/store a ghost value
            continue;
        }
        if (store_unboxed_p(s, &ctx)) {
            if (varinfo.isAssigned) // otherwise, just leave it in the input register
                alloc_local(s, &ctx);
        }
        else if (varinfo.isAssigned || (va && i==largslen-1 && varinfo.escapes)) {
            n_roots++;
        }
        maybe_alloc_arrayvar(s, &ctx);
    }
    for(i=0; i < vinfoslen; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_cellref(jl_cellref(vinfos,i),0);
        assert(jl_is_symbol(s));
        jl_varinfo_t &vi = ctx.vars[s];
        if (vi.isArgument)
            continue;
        if (vi.value.isghost && !vi.usedUndef) {
            // no need to explicitly load/store a ghost value
            continue;
        }
        if (store_unboxed_p(s, &ctx)) {
            alloc_local(s, &ctx);
        }
        else {
            if (!vi.used) {
                vi.hasGCRoot = false;
                continue;
            }
            if (vi.isSA && !vi.isVolatile && !vi.isCaptured && !vi.usedUndef) {
                vi.hasGCRoot = false; // so far...
            }
            else {
                vi.hasGCRoot = true;
                n_roots++;
            }
        }
        maybe_alloc_arrayvar(s, &ctx);
    }

    // create SAvalue locations for GenSym objects
    ctx.gensym_assigned.assign(n_gensyms, false);
    ctx.gensym_SAvalues.assign(n_gensyms, jl_cgval_t());

    // fetch env out of function object if we need it
    if (hasCapt) {
        Value *envArg = emit_nthptr(fArg, offsetof(jl_function_t,env)/sizeof(jl_value_t*), tbaa_const);
        for(i=0; i < captvinfoslen; i++) {
            jl_array_t *vi = (jl_array_t*)jl_cellref(captvinfos, i);
            assert(jl_is_array(vi));
            jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
            assert(jl_is_symbol(vname));
            jl_varinfo_t &varinfo = ctx.vars[vname];
            if (!varinfo.value.isghost) {
                varinfo.memloc = builder.CreatePointerCast(
                        emit_nthptr_addr(envArg, i + offsetof(jl_svec_t,data) / sizeof(void*)),
                        T_ppjlvalue);
            }
        }
    }

    // step 8. set up GC frame
    allocate_gc_frame(n_roots, b0, &ctx);

    // get pointers for locals stored in the gc frame array (argTemp)
    int varnum = 0;
    for(i=0; i < largslen; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        jl_varinfo_t &varinfo = ctx.vars[s];
        assert(!varinfo.memloc); // arguments shouldn't also be in the closure env
        if (varinfo.value.isghost) {
            // no need to explicitly load/store a ghost value
            varinfo.hasGCRoot = true;
            continue;
        }
        if (store_unboxed_p(s, &ctx)) {
            varinfo.hasGCRoot = true;
        }
        else if (varinfo.isAssigned || (va && i==largslen-1 && varinfo.escapes)) {
            Value *av = emit_local_slot(varnum, &ctx);
            varnum++;
            varinfo.memloc = av;
        }
    }
    for(i=0; i < vinfoslen; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_cellref(jl_cellref(vinfos,i),0);
        jl_varinfo_t &varinfo = ctx.vars[s];
        if (varinfo.memloc || varinfo.isArgument)
            continue; // gc root already created
        if (store_unboxed_p(s, &ctx) ||
                (varinfo.value.isghost && !varinfo.usedUndef)) {
            varinfo.hasGCRoot = true; // will never need a gc-root for this
        }
        else if (varinfo.hasGCRoot) {
            Value *lv = emit_local_slot(varnum, &ctx);
            varnum++;
            varinfo.memloc = lv;
        }
    }
    assert(varnum == ctx.gc.argSpaceSize);

    // step 9. create boxes for boxed locals
    // now handled by explicit :newvar nodes

    // step 10. allocate space for exception handler contexts
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

    // step 11. check arg count
    if (jl_is_va_tuple(ctx.linfo->specTypes)) {
        std::string msg;
        Value *enough;
        if (va) {
            msg = "too few arguments";
            enough = builder.CreateICmpUGE(argCount,
                                      ConstantInt::get(T_int32, nreq));
        }
        else {
            msg = "wrong number of arguments";
            enough =
                builder.CreateICmpEQ(argCount,
                                     ConstantInt::get(T_int32, nreq));
        }
        BasicBlock *elseBB =
            BasicBlock::Create(getGlobalContext(), "else", f);
        BasicBlock *mergeBB =
            BasicBlock::Create(getGlobalContext(), "ifcont");
        builder.CreateCondBr(enough, mergeBB, elseBB);
        builder.SetInsertPoint(elseBB);
        just_emit_error(msg, &ctx);
        builder.CreateUnreachable();
        f->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
    }

    // step 12. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    if (ctx.sret)
        AI++; // skip sret slot
    for(i=0; i < nreq; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        jl_varinfo_t &vi = ctx.vars[s];
        jl_cgval_t theArg;
        if (!vi.value.isghost) {
            if (specsig) {
                jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
                bool isboxed;
                Type *llvmArgType = julia_type_to_llvm(argType, &isboxed);
                if (type_is_ghost(llvmArgType)) // this argument is not actually passed
                    theArg = ghostValue(argType);
                else if (llvmArgType->isAggregateType())
                    theArg = mark_julia_slot(AI++, argType); // this argument is by-pointer
                else
                    theArg = mark_julia_type(AI++, isboxed, argType);
            }
            else {
                Value *argPtr = builder.CreateGEP(argArray, ConstantInt::get(T_size, i));
                theArg = mark_julia_type(builder.CreateLoad(argPtr), true, vi.value.typ);
            }
            theArg.needsgcroot = false;

            Value *lv = vi.memloc;
            if (lv == NULL) {
                if (vi.value.V) {
                    // copy theArg into its local variable slot (unboxed)
                    assert(vi.isAssigned);
                    assert(vi.value.ispointer);
                    builder.CreateStore(
                            emit_unbox(vi.value.V->getType()->getContainedType(0),
                                theArg, vi.value.typ),
                            vi.value.V);
                }
                else {
                    // keep track of original (boxed) value to avoid re-boxing or moving
                    vi.value = theArg;
                }
            }
            else {
                Value *argp = boxed(theArg, &ctx);
                if (vi.isBox) {
                    if (!theArg.isboxed) {
                        builder.CreateStore(argp, lv); // temporarily root
                    }
                    builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), argp), lv);
                }
                else {
                    builder.CreateStore(argp, lv);
                }
            }
            // get arrayvar data if applicable
            if (arrayvars.find(s) != arrayvars.end()) {
                jl_arrayvar_t av = arrayvars[s];
                assign_arrayvar(av, theArg);
            }
        }
        else if (vi.isBox) {
            // boxed ghost value -- TODO: kill this
            Value *lv = vi.memloc;
            assert(lv);
            jl_value_t *inst = static_void_instance(vi.value.typ);
            assert(inst);
            builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), literal_pointer_val(inst)), lv);
        }
        else {
            assert(vi.memloc == NULL);
        }
    }

    // step 13. allocate rest argument if necessary
    if (va) {
        jl_sym_t *argname = ctx.vaName;
        jl_varinfo_t &vi = ctx.vars[argname];
        if (!vi.escapes && !vi.isAssigned) {
            ctx.vaStack = true;
        }
        else if (!vi.value.isghost) {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            Value *lv = vi.memloc;
            if (lv != NULL) {
#ifdef LLVM37
                Value *restTuple =
                    builder.CreateCall(prepare_call(jltuple_func), {V_null,
                                        builder.CreateGEP(argArray,
                                                          ConstantInt::get(T_size,nreq)),
                                        builder.CreateSub(argCount,
                                                          ConstantInt::get(T_int32,nreq))});
#else
                Value *restTuple =
                    builder.CreateCall3(prepare_call(jltuple_func), V_null,
                                        builder.CreateGEP(argArray,
                                                          ConstantInt::get(T_size,nreq)),
                                        builder.CreateSub(argCount,
                                                          ConstantInt::get(T_int32,nreq)));
#endif
                if (vi.isBox)
                    builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), restTuple), lv);
                else
                    builder.CreateStore(restTuple, lv);
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

    // step 14. associate labels with basic blocks to resolve forward jumps
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
                prev = BasicBlock::Create(getGlobalContext(), "L");
                labels[lname] = prev;
            }
        }
        else {
            prev = NULL;
        }
    }

    // step 15. compile body statements
    bool prevlabel = false;
    lno = -1;
    int prevlno = -1;
    for(i=0; i < stmtslen; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_linenode(stmt) ||
            (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym)) {

            jl_sym_t *file = NULL;
            if (jl_is_linenode(stmt)) {
                lno = jl_linenode_line(stmt);
                file = jl_linenode_file(stmt);
            } else if (jl_is_expr(stmt)) {
                lno = jl_unbox_long(jl_exprarg(stmt,0));
                if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
                    jl_value_t *a1 = jl_exprarg(stmt,1);
                    if (jl_is_symbol(a1)) {
                        file = (jl_sym_t*)a1;
                    }
                }
            }
            assert(file->name);

#           ifdef LLVM37
            DIFile *dfil = NULL;
#           else
            MDNode *dfil = NULL;
#           endif

            // If the string is not empty
            if (*file->name != '\0') {
#               ifdef LLVM37
                std::map<jl_sym_t *, DIFile *>::iterator it = filescopes.find(file);
#               else
                std::map<jl_sym_t *, MDNode *>::iterator it = filescopes.find(file);
#               endif
                if (it != filescopes.end()) {
                    dfil = it->second;
                } else {
#                   ifdef LLVM37
                    dfil = (DIFile*)dbuilder.createFile(file->name, ".");
#                   else
                    dfil = (MDNode*)dbuilder.createFile(file->name, ".");
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
                } else {
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
            if (retboxed) {
                retval = boxed(emit_expr(jl_exprarg(ex,0), &ctx, true), &ctx, expr_type(stmt, &ctx));
            }
            else if (!type_is_ghost(retty)) {
                retval = emit_unbox(retty,
                                    emit_unboxed(jl_exprarg(ex,0), &ctx), jlrettype);
            }
            else { // undef return type
                emit_expr(jl_exprarg(ex,0), &ctx, false);
                retval = NULL;
            }
            if (do_malloc_log && lno != -1)
                mallocVisitLine(filename, lno);
            if (ctx.sret)
                builder.CreateStore(retval, ctx.f->arg_begin());
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
        else {
            (void)emit_expr(stmt, &ctx, false, false);
        }
    }

    builder.SetCurrentDebugLocation(noDbg);

    // sometimes we have dangling labels after the end
    if (builder.GetInsertBlock()->getTerminator() == NULL) {
        builder.CreateUnreachable();
    }

    // step 16. fix up size of stack root list
    finalize_gc_frame(&ctx);

    // step 17, Apply LLVM level inlining
    for(std::vector<CallInst*>::iterator it = ctx.to_inline.begin(); it != ctx.to_inline.end(); ++it) {
        Function *inlinef = (*it)->getCalledFunction();
        InlineFunctionInfo info;
        if (!InlineFunction(*it,info))
            jl_error("Inlining Pass failed");
        inlinef->eraseFromParent();
    }

    // step 18. Perform any delayed instantiations
    if (ctx.debug_enabled)
        ctx.dbuilder->finalize();

    JL_GC_POP();

    return f;
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

static Function *jlcall_func_to_llvm(const std::string &cname, void *addr, Module *m)
{
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage, cname, m);
    add_named_global(f, addr);
    return f;
}

extern "C" void jl_fptr_to_llvm(void *fptr, jl_lambda_info_t *lam, int specsig)
{
    if (imaging_mode) {
        if (!specsig) {
            lam->fptr = (jl_fptr_t)fptr; // in imaging mode, it's fine to use the fptr, but we don't want it in the shadow_module
        }
    }
    else {
        // this assigns a function pointer (from loading the system image), to the function object
        std::string funcName = lam->name->name;
        funcName = "julia_" + funcName;
        if (specsig) { // assumes !va
            std::vector<Type*> fsig(0);
            jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)lam->ast);
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
            Function *f = Function::Create(FunctionType::get(rt, fsig, false), Function::ExternalLinkage, funcName,
                                           shadow_module);
            if (sret)
                f->addAttribute(1, Attribute::StructRet);

        if (lam->specFunctionObject == NULL) {
            lam->specFunctionObject = (void*)f;
            lam->specFunctionID = jl_assign_functionID(f);
            }
            add_named_global(f, (void*)fptr);
        }
        else {
            Function *f = jlcall_func_to_llvm(funcName, fptr, shadow_module);
            if (lam->functionObject == NULL) {
                lam->functionObject = (void*)f;
                lam->functionID = jl_assign_functionID(f);
                assert(lam->fptr == &jl_trampoline);
                lam->fptr = (jl_fptr_t)fptr;
            }
        }
    }
}

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 3 && SYSTEM_LLVM
#define INSTCOMBINE_BUG
#define V128_BUG
#endif

static void init_julia_llvm_env(Module *m)
{
    MDNode* tbaa_root = mbuilder->createTBAARoot("jtbaa");
    tbaa_user = tbaa_make_child("jtbaa_user",tbaa_root);
    tbaa_value = tbaa_make_child("jtbaa_value",tbaa_root);
    tbaa_immut = tbaa_make_child("jtbaa_immut",tbaa_root);
    tbaa_array = tbaa_make_child("jtbaa_array",tbaa_value);
    tbaa_arrayptr = tbaa_make_child("jtbaa_arrayptr",tbaa_array);
    tbaa_arraysize = tbaa_make_child("jtbaa_arraysize",tbaa_array);
    tbaa_arraylen = tbaa_make_child("jtbaa_arraylen",tbaa_array);
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
    T_float32 = Type::getFloatTy(getGlobalContext());
    T_pfloat32 = PointerType::get(T_float32, 0);
    T_float64 = Type::getDoubleTy(getGlobalContext());
    T_pfloat64 = PointerType::get(T_float64, 0);
    T_void = Type::getVoidTy(jl_LLVMContext);

    // This type is used to create undef Values for use in struct declarations to skip indices
    NoopType = ArrayType::get(T_int1,0);

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
        sizeof(jl_value_t)*8,
        __alignof__(jl_value_t)*8,
        0, // Flags
#ifdef LLVM37
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later
#else
        DIType(), // Derived from
        DIArray()); // Elements - will be corrected later
#endif

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt,sizeof(jl_value_t*)*8,
                                                   __alignof__(jl_value_t*)*8);

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

#ifdef LLVM36
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
    ftargs.push_back(T_pjlvalue);
    ftargs.push_back(T_ppjlvalue);
    ftargs.push_back(T_int32);
    jl_func_sig = FunctionType::get(T_pjlvalue, ftargs, false);
    assert(jl_func_sig != NULL);
    jl_pfptr_llvmt = PointerType::get(PointerType::get(jl_func_sig, 0), 0);

    Type* vaelts[] = {T_pint8
#ifdef STORE_ARRAY_LEN
                      , T_size
#endif
                      , T_int16
    };
    Type* jl_array_llvmt =
        StructType::create(jl_LLVMContext,
                           ArrayRef<Type*>(vaelts,sizeof(vaelts)/sizeof(vaelts[0])),
                           "jl_array_t");
    jl_parray_llvmt = PointerType::get(jl_array_llvmt,0);

#ifdef JULIA_ENABLE_THREADING
#define JL_THREAD_MODEL ,GlobalValue::GeneralDynamicTLSModel
#else
#define JL_THREAD_MODEL
#endif
    jlpgcstack_var =
        new GlobalVariable(*m, T_ppjlvalue,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, "jl_pgcstack", NULL JL_THREAD_MODEL);
    add_named_global(jlpgcstack_var, jl_dlsym(jl_dl_handle, "jl_pgcstack"));

    jlexc_var =
        new GlobalVariable(*m, T_pjlvalue,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, "jl_exception_in_transit", NULL JL_THREAD_MODEL);
    add_named_global(jlexc_var, jl_dlsym(jl_dl_handle, "jl_exception_in_transit"));

    global_to_llvm("__stack_chk_guard", (void*)&__stack_chk_guard, m);
    Function *jl__stack_chk_fail =
        Function::Create(FunctionType::get(T_void, false),
                         Function::ExternalLinkage,
                         "__stack_chk_fail", m);
    jl__stack_chk_fail->setDoesNotReturn();
    add_named_global(jl__stack_chk_fail, (void*)&__stack_chk_fail);

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
    add_named_global(jlRTLD_DEFAULT_var, (void*)&jl_RTLD_DEFAULT_handle);
#ifdef _OS_WINDOWS_
    jlexe_var =
        new GlobalVariable(*m, T_pint8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_exe_handle");
    add_named_global(jlexe_var, (void*)&jl_exe_handle);
    jldll_var =
        new GlobalVariable(*m, T_pint8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_dl_handle");
    add_named_global(jldll_var, (void*)&jl_dl_handle);
#endif
#if JL_NEED_FLOATTEMP_VAR
    // Has to be big enough for the biggest LLVM-supported float type
    jlfloattemp_var =
        addComdat(new GlobalVariable(*m, IntegerType::get(jl_LLVMContext,128),
                                     false, GlobalVariable::ExternalLinkage,
                                     ConstantInt::get(IntegerType::get(jl_LLVMContext,128),0),
                                     "jl_float_temp"));
#endif

    std::vector<Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", m);
    jlerror_func->setDoesNotReturn();
    add_named_global(jlerror_func, (void*)&jl_error);

    std::vector<Type*> args1_(0);
    args1_.push_back(T_pjlvalue);
    jlthrow_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_throw", m);
    jlthrow_func->setDoesNotReturn();
    add_named_global(jlthrow_func, (void*)&jl_throw);

    jlundefvarerror_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_undefined_var_error", m);
    jlundefvarerror_func->setDoesNotReturn();
    add_named_global(jlundefvarerror_func, (void*)&jl_undefined_var_error);

    std::vector<Type*> args2_boundserrorv(0);
    args2_boundserrorv.push_back(T_pjlvalue);
    args2_boundserrorv.push_back(T_psize);
    args2_boundserrorv.push_back(T_size);
    jlboundserrorv_func =
        Function::Create(FunctionType::get(T_void, args2_boundserrorv, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_ints", m);
    jlboundserrorv_func->setDoesNotReturn();
    add_named_global(jlboundserrorv_func, (void*)&jl_bounds_error_ints);

    std::vector<Type*> args2_boundserror(0);
    args2_boundserror.push_back(T_pjlvalue);
    args2_boundserror.push_back(T_size);
    jlboundserror_func =
        Function::Create(FunctionType::get(T_void, args2_boundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_int", m);
    jlboundserror_func->setDoesNotReturn();
    add_named_global(jlboundserror_func, (void*)&jl_bounds_error_int);

    std::vector<Type*> args3_vboundserror(0);
    args3_vboundserror.push_back(T_ppjlvalue);
    args3_vboundserror.push_back(T_size);
    args3_vboundserror.push_back(T_size);
    jlvboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_vboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_tuple_int", m);
    jlvboundserror_func->setDoesNotReturn();
    add_named_global(jlvboundserror_func, (void*)&jl_bounds_error_tuple_int);

    std::vector<Type*> args3_uboundserror(0);
    args3_uboundserror.push_back(T_pint8);
    args3_uboundserror.push_back(T_pjlvalue);
    args3_uboundserror.push_back(T_size);
    jluboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_uboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_unboxed_int", m);
    jluboundserror_func->setDoesNotReturn();
    add_named_global(jluboundserror_func, (void*)&jl_bounds_error_unboxed_int);

    jlnew_func =
        Function::Create(jl_func_sig, Function::ExternalLinkage,
                         "jl_new_structv", m);
    add_named_global(jlnew_func, (void*)&jl_new_structv);

    std::vector<Type*> args2(0);
    args2.push_back(T_pint8);
#ifndef _OS_WINDOWS_
    args2.push_back(T_int32);
#endif
    setjmp_func =
        Function::Create(FunctionType::get(T_int32, args2, false),
                         Function::ExternalLinkage, jl_setjmp_name, m);
    setjmp_func->addFnAttr(Attribute::ReturnsTwice);
    add_named_global(setjmp_func, (void*)&jl_setjmp_f);

    std::vector<Type*> args_memcmp(0);
    args_memcmp.push_back(T_pint8);
    args_memcmp.push_back(T_pint8);
    args_memcmp.push_back(T_size);
    memcmp_func =
        Function::Create(FunctionType::get(T_int32, args_memcmp, false),
                         Function::ExternalLinkage, "memcmp", m);
    add_named_global(memcmp_func, (void*)&memcmp);

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
    add_named_global(jltypeerror_func, (void*)&jl_type_error_rt);

    std::vector<Type *> args_2ptrs(0);
    args_2ptrs.push_back(T_pjlvalue);
    args_2ptrs.push_back(T_pjlvalue);
    jlcheckassign_func =
        Function::Create(FunctionType::get(T_void, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_checked_assignment", m);
    add_named_global(jlcheckassign_func, (void*)&jl_checked_assignment);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(T_pjlvalue);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1ptr, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", m);
    add_named_global(jldeclareconst_func, (void*)&jl_declare_constant);

    jlgetbindingorerror_func =
        Function::Create(FunctionType::get(T_pjlvalue, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_get_binding_or_error", m);
    add_named_global(jlgetbindingorerror_func, (void*)&jl_get_binding_or_error);

    builtin_func_map[jl_f_is] = jlcall_func_to_llvm("jl_f_is", (void*)&jl_f_is, m);
    builtin_func_map[jl_f_typeof] = jlcall_func_to_llvm("jl_f_typeof", (void*)&jl_f_typeof, m);
    builtin_func_map[jl_f_sizeof] = jlcall_func_to_llvm("jl_f_sizeof", (void*)&jl_f_sizeof, m);
    builtin_func_map[jl_f_subtype] = jlcall_func_to_llvm("jl_f_subtype", (void*)&jl_f_subtype, m);
    builtin_func_map[jl_f_isa] = jlcall_func_to_llvm("jl_f_isa", (void*)&jl_f_isa, m);
    builtin_func_map[jl_f_typeassert] = jlcall_func_to_llvm("jl_f_typeassert", (void*)&jl_f_typeassert, m);
    builtin_func_map[jl_f_apply] = jlcall_func_to_llvm("jl_f_apply", (void*)&jl_f_apply, m);
    builtin_func_map[jl_f_kwcall] = jlcall_func_to_llvm("jl_f_kwcall", (void*)&jl_f_kwcall, m);
    builtin_func_map[jl_f_throw] = jlcall_func_to_llvm("jl_f_throw", (void*)&jl_f_throw, m);
    builtin_func_map[jl_f_tuple] = jlcall_func_to_llvm("jl_f_tuple", (void*)&jl_f_tuple, m);
    builtin_func_map[jl_f_svec] = jlcall_func_to_llvm("jl_f_svec", (void*)&jl_f_svec, m);
    builtin_func_map[jl_f_methodexists] = jlcall_func_to_llvm("jl_f_methodexists", (void*)&jl_f_methodexists, m);
    builtin_func_map[jl_f_applicable] = jlcall_func_to_llvm("jl_f_applicable", (void*)&jl_f_applicable, m);
    builtin_func_map[jl_f_invoke] = jlcall_func_to_llvm("jl_f_invoke", (void*)&jl_f_invoke, m);
    builtin_func_map[jl_f_top_eval] = jlcall_func_to_llvm("jl_f_top_eval", (void*)&jl_f_top_eval, m);
    builtin_func_map[jl_f_isdefined] = jlcall_func_to_llvm("jl_f_isdefined", (void*)&jl_f_isdefined, m);
    builtin_func_map[jl_f_get_field] = jlcall_func_to_llvm("jl_f_get_field", (void*)&jl_f_get_field, m);
    builtin_func_map[jl_f_set_field] = jlcall_func_to_llvm("jl_f_set_field", (void*)&jl_f_set_field, m);
    builtin_func_map[jl_f_field_type] = jlcall_func_to_llvm("jl_f_field_type", (void*)&jl_f_field_type, m);
    builtin_func_map[jl_f_nfields] = jlcall_func_to_llvm("jl_f_nfields", (void*)&jl_f_nfields, m);
    builtin_func_map[jl_f_new_expr] = jlcall_func_to_llvm("jl_f_new_expr", (void*)&jl_f_new_expr, m);
    builtin_func_map[jl_f_arraylen] = jlcall_func_to_llvm("jl_f_arraylen", (void*)&jl_f_arraylen, m);
    builtin_func_map[jl_f_arrayref] = jlcall_func_to_llvm("jl_f_arrayref", (void*)&jl_f_arrayref, m);
    builtin_func_map[jl_f_arrayset] = jlcall_func_to_llvm("jl_f_arrayset", (void*)&jl_f_arrayset, m);
    builtin_func_map[jl_f_arraysize] = jlcall_func_to_llvm("jl_f_arraysize", (void*)&jl_f_arraysize, m);
    builtin_func_map[jl_f_instantiate_type] = jlcall_func_to_llvm("jl_f_instantiate_type", (void*)&jl_f_instantiate_type, m);
    jltuple_func = builtin_func_map[jl_f_tuple];
    jlgetfield_func = builtin_func_map[jl_f_get_field];
    jlapplygeneric_func = jlcall_func_to_llvm("jl_apply_generic", (void*)&jl_apply_generic, m);

    queuerootfun = Function::Create(FunctionType::get(T_void, args_1ptr, false),
                                    Function::ExternalLinkage,
                                    "jl_gc_queue_root", m);
    add_named_global(queuerootfun, (void*)&jl_gc_queue_root);

    std::vector<Type *> wbargs(0);
    wbargs.push_back(T_pjlvalue);
    wbargs.push_back(T_pjlvalue);
    wbfunc = Function::Create(FunctionType::get(T_void, wbargs, false),
                              Function::ExternalLinkage,
                              "jl_gc_wb_slow", m);
    add_named_global(wbfunc, (void*)&jl_gc_wb_slow);

    std::vector<Type *> exp_args(0);
    exp_args.push_back(T_int1);
    expect_func = Intrinsic::getDeclaration(m, Intrinsic::expect, exp_args);

    std::vector<Type*> args3(0);
    args3.push_back(T_pjlvalue);
    jlbox_func =
        Function::Create(FunctionType::get(T_pjlvalue, args3, false),
                         Function::ExternalLinkage,
                         "jl_new_box", m);
    add_named_global(jlbox_func, (void*)&jl_new_box);

    jltopeval_func =
        Function::Create(FunctionType::get(T_pjlvalue, args3, false),
                         Function::ExternalLinkage,
                         "jl_toplevel_eval", m);
    add_named_global(jltopeval_func, (void*)&jl_toplevel_eval);

    jlcopyast_func =
        Function::Create(FunctionType::get(T_pjlvalue, args3, false),
                         Function::ExternalLinkage,
                         "jl_copy_ast", m);
    add_named_global(jlcopyast_func, (void*)&jl_copy_ast);

    std::vector<Type*> args4(0);
    args4.push_back(T_pint8);
    args4.push_back(T_pjlvalue);
    args4.push_back(T_pjlvalue);
    jlclosure_func =
        Function::Create(FunctionType::get(T_pjlvalue, args4, false),
                         Function::ExternalLinkage,
                         "jl_new_closure", m);
    add_named_global(jlclosure_func, (void*)&jl_new_closure);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlnsvec_func =
        Function::Create(FunctionType::get(T_pjlvalue, args5, true),
                         Function::ExternalLinkage,
                         "jl_svec", m);
    add_named_global(jlnsvec_func, (void*)&jl_svec);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_ppjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_pjlvalue);
    mdargs.push_back(T_int32);
    jlmethod_func =
        Function::Create(FunctionType::get(T_pjlvalue, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", m);
    add_named_global(jlmethod_func, (void*)&jl_method_def);

    std::vector<Type*> funcdefargs(0);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_ppjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    jlgenericfunction_func =
        Function::Create(FunctionType::get(T_pjlvalue, funcdefargs, false),
                         Function::ExternalLinkage,
                         "jl_generic_function_def", m);
    add_named_global(jlgenericfunction_func, (void*)&jl_generic_function_def);

    std::vector<Type*> ehargs(0);
    ehargs.push_back(T_pint8);
    jlenter_func =
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage,
                         "jl_enter_handler", m);
    add_named_global(jlenter_func, (void*)&jl_enter_handler);

#ifdef _OS_WINDOWS_
    resetstkoflw_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_resetstkoflw", m);
    add_named_global(resetstkoflw_func, (void*)&_resetstkoflw);
#if defined(_CPU_X86_64_)
#if defined(_COMPILER_MINGW_)
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "___chkstk_ms", m);
    add_named_global(chkstk_func, (void*)&___chkstk_ms);
#else
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "__chkstk", m);
    add_named_global(chkstk_func, (void*)&__chkstk);
#endif
#else
#if defined(_COMPILER_MINGW_)
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_alloca", m);
    add_named_global(chkstk_func, (void*)&_alloca);
#else
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_chkstk", m);
    add_named_global(chkstk_func, (void*)&_chkstk);
#endif
#endif
#endif

    std::vector<Type*> lhargs(0);
    lhargs.push_back(T_int32);
    jlleave_func =
        Function::Create(FunctionType::get(T_void, lhargs, false),
                         Function::ExternalLinkage,
                         "jl_pop_handler", m);
    add_named_global(jlleave_func, (void*)&jl_pop_handler);

    std::vector<Type *> args_2vals(0);
    args_2vals.push_back(T_pjlvalue);
    args_2vals.push_back(T_pjlvalue);
    jlegal_func =
        Function::Create(FunctionType::get(T_int32, args_2vals, false),
                         Function::ExternalLinkage,
                         "jl_egal", m);
    add_named_global(jlegal_func, (void*)&jl_egal);

    std::vector<Type *> subt_args(0);
    subt_args.push_back(T_pjlvalue);
    subt_args.push_back(T_pjlvalue);
    subt_args.push_back(T_int32);
    jlsubtype_func =
        Function::Create(FunctionType::get(T_int32, subt_args, false),
                         Function::ExternalLinkage,
                         "jl_subtype", m);
    add_named_global(jlsubtype_func, (void*)&jl_subtype);

    std::vector<Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(T_pjlvalue, aoargs, false),
                         Function::ExternalLinkage,
                         "jl_gc_allocobj", m);
    add_named_global(jlallocobj_func, (void*)&jl_gc_allocobj);

    std::vector<Type*> empty_args(0);
    jlalloc1w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_1w", m);
    add_named_global(jlalloc1w_func, (void*)&jl_gc_alloc_1w);

    jlalloc2w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_2w", m);
    add_named_global(jlalloc2w_func, (void*)&jl_gc_alloc_2w);

    jlalloc3w_func =
        Function::Create(FunctionType::get(T_pjlvalue, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_alloc_3w", m);
    add_named_global(jlalloc3w_func, (void*)&jl_gc_alloc_3w);

    std::vector<Type*> atargs(0);
    atargs.push_back(T_size);
    jl_alloc_svec_func =
        Function::Create(FunctionType::get(T_pjlvalue, atargs, false),
                         Function::ExternalLinkage,
                         "jl_alloc_svec", m);
    add_named_global(jl_alloc_svec_func, (void*)&jl_alloc_svec);

    std::vector<Type *> dlsym_args(0);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(PointerType::get(T_pint8,0));
    jldlsym_func =
        Function::Create(FunctionType::get(T_pint8, dlsym_args, false),
                         Function::ExternalLinkage,
                         "jl_load_and_lookup", m);
    add_named_global(jldlsym_func, (void*)&jl_load_and_lookup);

    std::vector<Type *> newbits_args(0);
    newbits_args.push_back(T_pjlvalue);
    newbits_args.push_back(T_pint8);
    jlnewbits_func =
        Function::Create(FunctionType::get(T_pjlvalue, newbits_args, false),
                         Function::ExternalLinkage,
                         "jl_new_bits", m);
    add_named_global(jlnewbits_func, (void*)&jl_new_bits);

    std::vector<Type *> getnthfld_args(0);
    getnthfld_args.push_back(T_pjlvalue);
    getnthfld_args.push_back(T_size);
    jlgetnthfieldchecked_func =
        Function::Create(FunctionType::get(T_pjlvalue, getnthfld_args, false),
                         Function::ExternalLinkage,
                         "jl_get_nth_field_checked", m);
    add_named_global(jlgetnthfieldchecked_func, (void*)*jl_get_nth_field_checked);

    diff_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, false),
                         Function::ExternalLinkage,
                         "jl_gc_diff_total_bytes", m);
    add_named_global(diff_gc_total_bytes_func, (void*)*jl_gc_diff_total_bytes);

    // set up optimization passes
#ifdef LLVM38
    FPM = new legacy::FunctionPassManager(m);
#else
    FPM = new FunctionPassManager(m);
#endif

#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif LLVM36
    jl_data_layout = new llvm::DataLayoutPass();
#elif LLVM35
    jl_data_layout = new llvm::DataLayoutPass(*jl_ExecutionEngine->getDataLayout());
#else
    jl_data_layout = new DataLayout(*jl_ExecutionEngine->getDataLayout());
#endif

#ifndef LLVM37
    FPM->add(jl_data_layout);
#endif

#ifdef __has_feature
#   if __has_feature(address_sanitizer)
    FPM->add(createAddressSanitizerFunctionPass());
#   endif
#   if __has_feature(memory_sanitizer)
    FPM->add(llvm::createMemorySanitizerPass(true));
#   endif
#endif
#ifdef LLVM37
    FPM->add(createTargetTransformInfoWrapperPass(jl_TargetMachine->getTargetIRAnalysis()));
#else
    jl_TargetMachine->addAnalysisPasses(*FPM);
#endif
#ifdef LLVM38
    FPM->add(createTypeBasedAAWrapperPass());
#else
    FPM->add(createTypeBasedAliasAnalysisPass());
#endif
    if (jl_options.opt_level>=1) {
#ifdef LLVM38
        FPM->add(createBasicAAWrapperPass());
#else
        FPM->add(createBasicAliasAnalysisPass());
#endif
    }
    // list of passes from vmkit
    FPM->add(createCFGSimplificationPass()); // Clean up disgusting code
    FPM->add(createPromoteMemoryToRegisterPass());// Kill useless allocas

#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
#endif
    FPM->add(createScalarReplAggregatesPass()); // Break up aggregate allocas
#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
#endif
    FPM->add(createJumpThreadingPass());        // Thread jumps.
    // NOTE: CFG simp passes after this point seem to hurt native codegen.
    // See issue #6112. Should be re-evaluated when we switch to MCJIT.
    //FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass()); // Combine silly seq's
#endif

    //FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
    FPM->add(createReassociatePass());          // Reassociate expressions

    // this has the potential to make some things a bit slower
    //FPM->add(createBBVectorizePass());

    FPM->add(createEarlyCSEPass()); //// ****

    FPM->add(createLoopIdiomPass()); //// ****
    FPM->add(createLoopRotatePass());           // Rotate loops.
    // LoopRotate strips metadata from terminator, so run LowerSIMD afterwards
    FPM->add(createLowerSimdLoopPass());        // Annotate loop marked with "simdloop" as LLVM parallel loop
    FPM->add(createLICMPass());                 // Hoist loop invariants
    FPM->add(createLoopUnswitchPass());         // Unswitch loops.
    // Subsequent passes not stripping metadata from terminator
#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass());
#endif
    FPM->add(createIndVarSimplifyPass());       // Canonicalize indvars
    FPM->add(createLoopDeletionPass());         // Delete dead loops
#if LLVM35
    FPM->add(createSimpleLoopUnrollPass());     // Unroll small loops
#else
    FPM->add(createLoopUnrollPass());           // Unroll small loops
#endif
#if !LLVM35 && !defined(INSTCOMBINE_BUG)
    FPM->add(createLoopVectorizePass());        // Vectorize loops
#endif
    //FPM->add(createLoopStrengthReducePass());   // (jwb added)

#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass()); // Clean up after the unroller
#endif
    FPM->add(createGVNPass());                  // Remove redundancies
    //FPM->add(createMemCpyOptPass());            // Remove memcpy / form memset
    FPM->add(createSCCPPass());                 // Constant prop with SCCP

    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    FPM->add(createSinkingPass()); ////////////// ****
    FPM->add(createInstructionSimplifierPass());///////// ****
#ifndef INSTCOMBINE_BUG
    FPM->add(createInstructionCombiningPass());
#endif
    FPM->add(createJumpThreadingPass());         // Thread jumps
    FPM->add(createDeadStoreEliminationPass());  // Delete dead stores
#if !defined(INSTCOMBINE_BUG)
    if (jl_options.opt_level>=1)
        FPM->add(createSLPVectorizerPass());     // Vectorize straight-line code
#endif

    FPM->add(createAggressiveDCEPass());         // Delete dead instructions
#if !defined(INSTCOMBINE_BUG)
    if (jl_options.opt_level>=1)
        FPM->add(createInstructionCombiningPass());   // Clean up after SLP loop vectorizer
#endif
#if LLVM35
    FPM->add(createLoopVectorizePass());         // Vectorize loops
    FPM->add(createInstructionCombiningPass());  // Clean up after loop vectorizer
#endif
    //FPM->add(createCFGSimplificationPass());     // Merge & remove BBs

    FPM->doInitialization();
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

#if defined(_CPU_PPC_) || defined(_CPU_PPC64_)
    imaging_mode = true; // LLVM seems to JIT bad TOC tables for the optimizations we attempt in non-imaging_mode
#else
    imaging_mode = jl_generating_output();
#endif

#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3
    // this option disables LLVM's signal handlers
    llvm::DisablePrettyStackTrace = true;
#endif

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();

    Module *m, *engine_module;

#ifdef USE_MCJIT
    m = shadow_module = new Module("shadow", jl_LLVMContext);
    jl_setup_module(shadow_module,false);
    if (imaging_mode) {
        engine_module = new Module("engine_module", jl_LLVMContext);
        jl_setup_module(engine_module,false);
    }
    else {
        engine_module = m;
    }
#else
    engine_module = m = jl_Module = new Module("julia", jl_LLVMContext);
    jl_setup_module(engine_module,false);
#endif

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
#ifdef USE_MCJIT
    jl_mcjmm = new SectionMemoryManager();
#endif
    const char *mattr[] = {
#if defined(_CPU_X86_64_) || defined(_CPU_X86_)
#ifndef USE_MCJIT
        // Temporarily disable Haswell BMI2 features due to LLVM bug.
        "-bmi2", "-avx2",
#endif
#ifdef V128_BUG
        "-avx",
#endif
#endif
        "", // padding to make sure this isn't an empty array (ref #11817)
    };
    SmallVector<std::string, 4> MAttrs(mattr, mattr+sizeof(mattr)/sizeof(mattr[0]));
#ifdef LLVM36
    EngineBuilder eb(std::move(std::unique_ptr<Module>(engine_module)));
#else
    EngineBuilder eb(engine_module);
#endif
    std::string ErrorStr;
    eb  .setEngineKind(EngineKind::JIT)
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && !defined(USE_MCJIT)
        .setJITMemoryManager(createJITMemoryManagerWin())
#elif defined(CUSTOM_MEMORY_MANAGER)
        .setMCJITMemoryManager(std::move(std::unique_ptr<RTDyldMemoryManager>{createRTDyldMemoryManagerOSX()}))
#elif defined(USE_ORCJIT) // ORCJIT forgets to create one if one isn't created for it
        .setMCJITMemoryManager(std::move(std::unique_ptr<RTDyldMemoryManager>{new SectionMemoryManager()}))
#endif
        .setTargetOptions(options)
#if defined(_OS_LINUX_) && defined(_CPU_X86_64_)
        .setRelocationModel(Reloc::PIC_)
#else
        .setRelocationModel(Reloc::Default)
#endif
        .setCodeModel(CodeModel::JITDefault)
#ifdef DISABLE_OPT
        .setOptLevel(CodeGenOpt::None)
#else
        .setOptLevel(CodeGenOpt::Aggressive)
#endif
#if defined(USE_MCJIT) && !defined(LLVM36)
        .setUseMCJIT(true)
#endif
#ifdef USE_ORCJIT
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
    if (TheCPU.empty() || TheCPU == "generic") {
        jl_printf(JL_STDERR, "WARNING: unable to determine host cpu name.\n");
#ifdef _CPU_ARM_
        MAttrs.append(1, "+vfp2"); // the processors that don't have VFP are old and (hopefully) rare. this affects the platform calling convention.
#endif
    }
    jl_TargetMachine = eb.selectTarget(
            TheTriple,
            "",
            TheCPU,
            MAttrs);
    assert(jl_TargetMachine && "Failed to select target machine -"
                            " Is the LLVM backend for this CPU enabled?");
#if defined(USE_MCJIT) && !defined(_CPU_ARM_)
    // FastISel seems to be buggy for ARM. Ref #13321
    jl_TargetMachine->setFastISel(true);
#endif

#if defined(LLVM38)
    engine_module->setDataLayout(jl_TargetMachine->createDataLayout());
#elif defined(LLVM36) && !defined(LLVM37)
    engine_module->setDataLayout(jl_TargetMachine->getSubtargetImpl()->getDataLayout());
#elif defined(LLVM35) && !defined(LLVM37)
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout());
#else
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout()->getStringRepresentation());
#endif
    jl_ExecutionEngine = eb.create(jl_TargetMachine);
    //jl_printf(JL_STDERR,"%s\n",jl_ExecutionEngine->getDataLayout()->getStringRepresentation().c_str());
    if (!jl_ExecutionEngine) {
        jl_printf(JL_STDERR, "Critical error initializing llvm: %s\n",
                  ErrorStr.c_str());
        exit(1);
    }
#if defined(LLVM35) && !defined(USE_ORCJIT)
    jl_ExecutionEngine->setProcessAllSections(true);
#endif
    jl_ExecutionEngine->DisableLazyCompilation();
    mbuilder = new MDBuilder(getGlobalContext());

#ifdef LLVM37
#ifdef LLVM38
    m->setDataLayout(jl_ExecutionEngine->getDataLayout().getStringRepresentation());
    engine_module->setDataLayout(jl_ExecutionEngine->getDataLayout().getStringRepresentation());
#else
    m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
    engine_module->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
#endif
    m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
    engine_module->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
#elif LLVM36
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
    engine_module->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
    init_julia_llvm_env(m);

    RegisterJuliaJITEventListener();
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

    BOX_F(int8,int8);  UBOX_F(uint8,uint8);
    BOX_F(int16,int16); UBOX_F(uint16,uint16);
    BOX_F(int32,int32); UBOX_F(uint32,uint32);
    BOX_F(int64,int64); UBOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);
    UBOX_F(gensym,size);

    box8_func  = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int8),
                              "jl_box8", (void*)&jl_box8, m);
    box16_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int16),
                              "jl_box16", (void*)&jl_box16, m);
    box32_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int32),
                              "jl_box32", (void*)&jl_box32, m);
    box64_func = boxfunc_llvm(ft2arg(T_pjlvalue, T_pjlvalue, T_int64),
                              "jl_box64", (void*)&jl_box64, m);
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
