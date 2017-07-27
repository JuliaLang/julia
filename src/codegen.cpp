// This file is a part of Julia. License is MIT: https://julialang.org/license

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
#include <functional>

// target machine computation
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Object/SymbolSize.h>

// IR building
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/AsmParser/Parser.h>
#include <llvm/DebugInfo/DIContext.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>

// support
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/ADT/Optional.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/SourceMgr.h> // for llvmcall
#include <llvm/Transforms/Utils/Cloning.h> // for llvmcall inlining
#include <llvm/IR/Verifier.h> // for llvmcall validation
#if JL_LLVM_VERSION >= 40000
#  include <llvm/Bitcode/BitcodeWriter.h>
#else
#  include <llvm/Bitcode/ReaderWriter.h>
#endif

// C API
#include <llvm-c/Types.h>

// for configuration options
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/CommandLine.h>

#include <llvm/IR/InlineAsm.h>
#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
#  include <sys/utsname.h>
#endif
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/ScopDetection.h>
#endif
#include "fix_llvm_assert.h"

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
#include "codegen_shared.h"

// LLVM version compatibility macros
legacy::PassManager *jl_globalPM;

#if JL_LLVM_VERSION >= 40000
#define DIFlagZero (DINode::FlagZero)
#else
#define DIFlagZero (0)
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
JL_DLLEXPORT LLVMContext jl_LLVMContext;
static bool nested_compile = false;
TargetMachine *jl_TargetMachine;

extern JITEventListener *CreateJuliaJITEventListener();

// for image reloading
bool imaging_mode = false;

Module *shadow_output;
#define jl_Module ctx.f->getParent()
#define jl_builderModule(builder) (builder).GetInsertBlock()->getParent()->getParent()

static DataLayout jl_data_layout("");

// types
static Type *T_jlvalue;
static Type *T_pjlvalue;
static Type *T_prjlvalue;
static Type *T_ppjlvalue;
static Type *T_pprjlvalue;
static Type *jl_array_llvmt;
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
static MDNode *tbaa_ptrarraybuf;    // Data in an array of boxed values
static MDNode *tbaa_arraybuf;       // Data in an array of POD
static MDNode *tbaa_array;      // jl_array_t
static MDNode *tbaa_arrayptr;       // The pointer inside a jl_array_t
static MDNode *tbaa_arraysize;      // A size in a jl_array_t
static MDNode *tbaa_arraylen;       // The len in a jl_array_t
static MDNode *tbaa_arrayflags;     // The flags in a jl_array_t
static MDNode *tbaa_const;      // Memory that is immutable by the time LLVM can see it

// Basic DITypes
static DICompositeType *jl_value_dillvmt;
static DIDerivedType *jl_pvalue_dillvmt;
static DIDerivedType *jl_ppvalue_dillvmt;
static DISubroutineType *jl_di_func_sig;
static DISubroutineType *jl_di_func_null_sig;

extern "C"
int32_t jl_jlcall_api(const char *fname)
{
    // give the function an index in the constant lookup table
    if (fname == NULL)
        return 0;
    StringRef Name(fname);
    if (Name.startswith("japi3_")) // jlcall abi 3 from JIT
        return 3;
    assert(Name.startswith("japi1_") || // jlcall abi 1 from JIT
           Name.startswith("jsys1_") || // jlcall abi 1 from sysimg
           Name.startswith("jlcall_") || // jlcall abi 1 from JIT wrapping a specsig method
           Name.startswith("jlsysw_")); // jlcall abi 1 from sysimg wrapping a specsig method
    return 1;
}


// constants
static Constant *V_null;
static Type *NoopType;
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
#endif //_OS_WINDOWS_

static Function *jltls_states_func;
#ifndef JULIA_ENABLE_THREADING
static GlobalVariable *jltls_states_var;
#else
// Imaging mode only
size_t jltls_states_func_idx = 0;
size_t jltls_offset_idx = 0;
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
static Function *jlboundp_func;
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
static Function *jlsubtype_func;
static Function *jlapplytype_func;
static Function *setjmp_func;
static Function *memcmp_derived_func;
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
static Function *jldepwarnpi_func;
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
static GlobalVariable *jlgetworld_global;

// placeholder functions
static Function *gcroot_flush_func;
static Function *except_enter_func;
static Function *pointer_from_objref_func;

static std::vector<Type *> two_pvalue_llvmt;
static std::vector<Type *> three_pvalue_llvmt;
static std::vector<Type *> four_pvalue_llvmt;

static std::map<jl_fptr_t, Function*> builtin_func_map;

// --- code generation ---
extern "C" {
    int globalUnique = 0;
}

static bool isbits_spec(jl_value_t *jt, bool allow_singleton = true)
{
    return jl_isbits(jt) && jl_is_leaf_type(jt) &&
        (allow_singleton || (jl_datatype_size(jt) > 0) || (jl_datatype_nfields(jt) > 0));
}

static MDNode *best_tbaa(jl_value_t *jt) {
    jt = jl_unwrap_unionall(jt);
    if (!jl_is_datatype(jt))
        return tbaa_value;
    if (jl_is_abstracttype(jt))
        return tbaa_value;
    // If we're here, we know all subtypes are (im)mutable, even if we
    // don't know what the exact type is
    return jl_is_mutable(jt) ? tbaa_mutab : tbaa_immut;
}

// metadata tracking for a llvm Value* during codegen
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    Value *TIndex; // if `V` is an unboxed (tagged) Union described by `typ`, this gives the DataType index (1-based, small int) as an i8
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    Value *gcroot; // the gcroot associated with V (if it has one)
    jl_value_t *typ; // the original type of V, never NULL
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    bool isimmutable; // V points to something that is definitely immutable (e.g. single-assignment, but including memory)
    MDNode *tbaa; // The related tbaa node. Non-NULL iff this holds an address.
    bool ispointer() const
    {
        // whether this value is compatible with `data_pointer`
        return tbaa != nullptr;
    }
    //bool isvalue() const
    //{
    //    // whether this value is compatible with loading into registers (`emit_unbox` without an explicit type)
    //    return isbits_spec(typ) && (!ispointer() || constant);
    //}
    jl_cgval_t(Value *V, Value *gcroot, bool isboxed, jl_value_t *typ, Value *tindex) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        TIndex(tindex),
        constant(NULL),
        gcroot(gcroot),
        typ(typ),
        isboxed(isboxed),
        isghost(false),
        isimmutable(isboxed && jl_is_immutable_datatype(typ)),
        tbaa(isboxed ? best_tbaa(typ) : nullptr)
    {
        assert(!(isboxed && TIndex != NULL));
        assert(TIndex == NULL || TIndex->getType() == T_int8);
    }
    jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        V(NULL),
        TIndex(NULL),
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
    jl_cgval_t(const jl_cgval_t &v, jl_value_t *typ, Value *tindex) : // copy constructor with new type
        V(v.V),
        TIndex(tindex),
        constant(v.constant),
        gcroot(v.gcroot),
        typ(typ),
        isboxed(v.isboxed),
        isghost(v.isghost),
        isimmutable(v.isimmutable),
        tbaa(v.tbaa)
    {
        // this constructor expects we had a badly or equivalently typed version
        // make sure we aren't discarding the actual type information
        if (v.TIndex) {
            assert((TIndex == NULL) == jl_is_leaf_type(typ));
        }
        else {
            assert(isboxed || v.typ == typ || tindex);
        }
    }
    jl_cgval_t() : // undef / unreachable / default constructor
        V(UndefValue::get(T_void)),
        TIndex(NULL),
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
    Instruction *boxroot; // an address, if the var might be in a jl_value_t** stack slot (marked tbaa_const, if appropriate)
    jl_cgval_t value; // a stack slot or constant value
    Value *pTIndex; // i8* stack slot for the value.TIndex tag describing `value.V`
    DILocalVariable *dinfo;
    // if the variable might be used undefined and is not boxed
    // this i1 flag is true when it is defined
    Value *defFlag;
    bool isSA;
    bool isVolatile;
    bool isArgument;
    bool usedUndef;
    bool used;

    jl_varinfo_t() : boxroot(NULL),
                     value(jl_cgval_t()),
                     pTIndex(NULL),
                     dinfo(NULL),
                     defFlag(NULL),
                     isSA(false),
                     isVolatile(false),
                     isArgument(false),
                     usedUndef(false),
                     used(false)
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

struct jl_returninfo_t {
    Function *decl;
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
};

static jl_returninfo_t get_specsig_function(Module *M, const std::string &name, jl_value_t *sig, jl_value_t *jlrettype);

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
class jl_codectx_t {
public:
    IRBuilder<> builder;
    Function *f = NULL;
    // local var info. globals are not in here.
    std::vector<jl_varinfo_t> slots;
    std::vector<jl_cgval_t> SAvalues;
    std::vector<bool> ssavalue_assigned;
    std::map<int, jl_arrayvar_t> *arrayvars = NULL;
    jl_module_t *module = NULL;
    jl_method_instance_t *linfo = NULL;
    jl_code_info_t *source = NULL;
    jl_array_t *code = NULL;
    size_t world = 0;
    jl_array_t *roots = NULL;
    const char *name = NULL;
    StringRef file{};
    ssize_t *line = NULL;
    Value *spvals_ptr = NULL;
    Value *argArray = NULL;
    Value *argCount = NULL;
    std::string funcName;
    int vaSlot = -1;        // name of vararg argument
    bool vaStack = false;      // varargs stack-allocated
    bool has_sret = false;
    int nReqArgs = 0;
    int nargs = 0;

    CallInst *ptlsStates = NULL;
    Value *signalPage = NULL;
    Value *world_age_field = NULL;

    bool debug_enabled = false;
    bool is_inbounds = false;

    const jl_cgparams_t *params = NULL;

    jl_codectx_t(LLVMContext &llvmctx)
      : builder(llvmctx) { }

    ~jl_codectx_t() {
        assert(this->roots == NULL);
    }
};

static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr);
static Value *emit_local_root(jl_codectx_t &ctx, jl_varinfo_t *vi = NULL);
static Value *global_binding_pointer(jl_codectx_t &ctx, jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign);
static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, bool isvol, MDNode *tbaa);
static jl_cgval_t emit_sparam(jl_codectx_t &ctx, size_t i);
static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const std::string &msg);
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0);
static void CreateTrap(IRBuilder<> &irbuilder);
static Value *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                          jl_cgval_t *args, size_t nargs);

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p);
static GlobalVariable *prepare_global_in(Module *M, GlobalVariable *G);
#define prepare_global(G) prepare_global_in(jl_Module, (G))

// --- convenience functions for tagging llvm values with julia types ---

static GlobalVariable *get_pointer_to_constant(Constant *val, StringRef name, Module &M)
{
    GlobalVariable *gv = new GlobalVariable(
            M,
            val->getType(),
            true,
            GlobalVariable::PrivateLinkage,
            val,
            name);
    gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    return gv;
}

static AllocaInst *emit_static_alloca(jl_codectx_t &ctx, Type *lty, int arraysize=1)
{
    return new AllocaInst(lty,
#if JL_LLVM_VERSION >= 50000
            0,
#endif
            ConstantInt::get(T_int32, arraysize), "", /*InsertBefore=*/ctx.ptlsStates);
}

static inline jl_cgval_t ghostValue(jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return jl_cgval_t(); // Undef{}
    if (typ == (jl_value_t*)jl_typeofbottom_type) {
        // normalize TypeofBottom to Type{Union{}}
        typ = (jl_value_t*)jl_wrap_Type(jl_bottom_type);
    }
    if (jl_is_type_type(typ)) {
        // replace T::Type{T} with T, by assuming that T must be a leaftype of some sort
        jl_cgval_t constant(NULL, NULL, true, typ, NULL);
        constant.constant = jl_tparam0(typ);
        return constant;
    }
    return jl_cgval_t(typ);
}
static inline jl_cgval_t ghostValue(jl_datatype_t *typ)
{
    return ghostValue((jl_value_t*)typ);
}

static inline jl_cgval_t mark_julia_const(jl_value_t *jv)
{
    jl_value_t *typ;
    if (jl_is_type(jv)) {
        typ = (jl_value_t*)jl_wrap_Type(jv); // TODO: gc-root this?
    }
    else {
        typ = jl_typeof(jv);
        if (type_is_ghost(julia_type_to_llvm(typ))) {
            return ghostValue(typ);
        }
    }
    jl_cgval_t constant(NULL, NULL, true, typ, NULL);
    constant.constant = jv;
    return constant;
}


static inline jl_cgval_t mark_julia_slot(Value *v, jl_value_t *typ, Value *tindex, MDNode *tbaa)
{
    // this enables lazy-copying of immutable values and stack or argument slots
    assert(tbaa);
    jl_cgval_t tagval(v, NULL, false, typ, tindex);
    tagval.tbaa = tbaa;
    tagval.isimmutable = true;
    return tagval;
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_value_t *typ, bool needsroot = true)
{
    if (jl_is_datatype(typ) && jl_is_datatype_singleton((jl_datatype_t*)typ)) {
        // no need to explicitly load/store a constant/ghost value
        return ghostValue(typ);
    }
    if (jl_is_type_type(typ)) {
        jl_value_t *tp0 = jl_tparam0(typ);
        if (jl_is_leaf_type(tp0) || tp0 == jl_bottom_type) {
            // replace T::Type{T} with T
            return ghostValue(typ);
        }
    }
    Type *T = julia_type_to_llvm(typ);
    if (type_is_ghost(T)) {
        return ghostValue(typ);
    }
    if (v && T->isAggregateType() && !isboxed) {
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        Value *loc;
        if (Constant *cv = dyn_cast<Constant>(v)) {
            loc = get_pointer_to_constant(cv, "", *jl_Module);
        }
        else {
            loc = emit_static_alloca(ctx, T);
            ctx.builder.CreateStore(v, loc);
        }
        return mark_julia_slot(loc, typ, NULL, tbaa_stack);
    }
    Value *froot = NULL;
    if (needsroot && isboxed) {
        froot = emit_local_root(ctx);
        ctx.builder.CreateStore(v, froot);
    }
    return jl_cgval_t(v, froot, isboxed, typ, NULL);
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_datatype_t *typ, bool needsroot = true)
{
    return mark_julia_type(ctx, v, isboxed, (jl_value_t*)typ, needsroot);
}

// see if it might be profitable (and cheap) to change the type of v to typ
static inline jl_cgval_t update_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    if (v.typ == typ || v.typ == jl_bottom_type || v.constant || typ == (jl_value_t*)jl_any_type || jl_egal(v.typ, typ))
        return v; // fast-path
    if (jl_is_leaf_type(v.typ) && !jl_is_kind(v.typ)) {
        if (jl_is_leaf_type(typ) && !jl_is_kind(typ) && !((jl_datatype_t*)typ)->abstract && !((jl_datatype_t*)v.typ)->abstract) {
            // type mismatch: changing from one leaftype to another
            CreateTrap(ctx.builder);
            return jl_cgval_t();
        }
        return v; // doesn't improve type info
    }
    if (v.TIndex) {
        if (!jl_is_leaf_type(typ))
            return v; // not worth trying to improve type info
        if (!isbits_spec(typ)) {
            // discovered that this union-split type must actually be isboxed
            if (v.V) {
                return jl_cgval_t(v.V, v.gcroot, true, typ, NULL);
            }
            else {
                // type mismatch (there weren't any boxed values in the union)
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
    }
    Type *T = julia_type_to_llvm(typ);
    if (type_is_ghost(T))
        return ghostValue(typ);
    return jl_cgval_t(v, typ, NULL);
}

// --- allocating local variables ---

static jl_sym_t *slot_symbol(jl_codectx_t &ctx, int s)
{
    return (jl_sym_t*)jl_array_ptr_ref(ctx.source->slotnames, s);
}

static void store_def_flag(jl_codectx_t &ctx, const jl_varinfo_t &vi, bool val)
{
    assert((!vi.boxroot || vi.pTIndex) && "undef check is null pointer for boxed things");
    assert(vi.usedUndef && vi.defFlag && "undef flag codegen corrupted");
    ctx.builder.CreateStore(ConstantInt::get(T_int1, val), vi.defFlag, vi.isVolatile);
}

static void alloc_def_flag(jl_codectx_t &ctx, jl_varinfo_t& vi)
{
    assert((!vi.boxroot || vi.pTIndex) && "undef check is null pointer for boxed things");
    if (vi.usedUndef) {
        vi.defFlag = emit_static_alloca(ctx, T_int1);
        store_def_flag(ctx, vi, false);
    }
}


// --- utilities ---

static void CreateTrap(IRBuilder<> &irbuilder)
{
    Function *f = irbuilder.GetInsertBlock()->getParent();
    Function *trap_func = Intrinsic::getDeclaration(
            f->getParent(),
            Intrinsic::trap);
    irbuilder.CreateCall(trap_func);
    irbuilder.CreateUnreachable();
    BasicBlock *newBB = BasicBlock::Create(irbuilder.getContext(), "after_noret", f);
    irbuilder.SetInsertPoint(newBB);
}

#if 0 // this code is likely useful, but currently unused
#ifndef JL_NDEBUG
static void CreateConditionalAbort(IRBuilder<> &irbuilder, Value *test)
{
    Function *f = irbuilder.GetInsertBlock()->getParent();
    BasicBlock *abortBB = BasicBlock::Create(jl_LLVMContext, "debug_abort", f);
    BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_abort", f);
    irbuilder.CreateCondBr(test, abortBB, postBB);
    irbuilder.SetInsertPoint(abortBB);
    Function *trap_func = Intrinsic::getDeclaration(
            f->getParent(),
            Intrinsic::trap);
    irbuilder.CreateCall(trap_func);
    irbuilder.CreateUnreachable();
    irbuilder.SetInsertPoint(postBB);
}
#endif
#endif

static void emit_write_barrier(jl_codectx_t&, Value*, Value*);

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

// given a value marked with type `v.typ`, compute the mapping and/or boxing to return a value of type `typ`
static jl_cgval_t convert_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ, bool needsroot = true)
{
    if (typ == (jl_value_t*)jl_typeofbottom_type)
        return ghostValue(typ); // normalize TypeofBottom to Type{Union{}}
    if (v.typ == typ || v.typ == jl_bottom_type || jl_egal(v.typ, typ))
        return v; // fast-path
    Type *T = julia_type_to_llvm(typ);
    if (type_is_ghost(T))
        return ghostValue(typ);
    Value *new_tindex = NULL;
    if (jl_is_leaf_type(typ)) {
        if (v.TIndex && !isbits_spec(typ)) {
            // discovered that this union-split type must actually be isboxed
            if (v.V) {
                return jl_cgval_t(v.V, v.gcroot, true, typ, NULL);
            }
            else {
                // type mismatch: there weren't any boxed values in the union
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
        if (jl_is_leaf_type(v.typ) && !jl_is_kind(v.typ) && !((jl_datatype_t*)v.typ)->abstract) {
            if (jl_is_leaf_type(typ) && !jl_is_kind(typ) && !((jl_datatype_t*)typ)->abstract) {
                // type mismatch: changing from one leaftype to another
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
    }
    else {
        bool makeboxed = false;
        if (v.TIndex) {
            // previous value was a split union, compute new index, or box
            new_tindex = ConstantInt::get(T_int8, 0x80);
            SmallBitVector skip_box(1, true);
            Value *tindex = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(T_int8, 0x7f));
            if (jl_is_uniontype(typ)) {
                // compute the TIndex mapping from v.typ -> typ
                unsigned counter = 0;
                for_each_uniontype_small(
                        // for each old union-split value
                        [&](unsigned idx, jl_datatype_t *jt) {
                            unsigned new_idx = get_box_tindex(jt, typ);
                            bool t;
                            if (new_idx) {
                                // found a matching element,
                                // match it against either the unboxed index
                                Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(T_int8, idx));
                                new_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, new_idx), new_tindex);
                                t = true;
                            }
                            else if (!jl_subtype((jl_value_t*)jt, typ)) {
                                // new value doesn't need to be boxed
                                // since it isn't part of the new union
                                t = true;
                            }
                            else {
                                // will actually need to box this element
                                // since it appeared as a leaftype in the original type
                                // but not in the remark type
                                t = false;
                            }
                            skip_box.resize(idx + 1, t);
                        },
                        v.typ,
                        counter);
            }

            // some of the values are still unboxed
            if (!isa<Constant>(new_tindex)) {
                Value *wasboxed = NULL;
                // check if some of the old values might have been boxed
                // and copy that information over into the new tindex
                if (v.ispointer() && v.V && !isa<AllocaInst>(v.V)) {
                    wasboxed = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(T_int8, 0x80));
                    new_tindex = ctx.builder.CreateOr(wasboxed, new_tindex);
                    wasboxed = ctx.builder.CreateICmpNE(wasboxed, ConstantInt::get(T_int8, 0));

                    // may need to handle compute_box_tindex for some of the values
                    BasicBlock *currBB = ctx.builder.GetInsertBlock();
                    Value *union_box_dt = NULL;
                    Value *union_box_tindex = ConstantInt::get(T_int8, 0x80);
                    unsigned counter = 0;
                    for_each_uniontype_small(
                            // for each new union-split value
                            [&](unsigned idx, jl_datatype_t *jt) {
                                unsigned old_idx = get_box_tindex(jt, v.typ);
                                if (old_idx == 0) {
                                    if (!union_box_dt) {
                                        BasicBlock *isaBB = BasicBlock::Create(jl_LLVMContext, "union_isa", ctx.f);
                                        ctx.builder.SetInsertPoint(isaBB);
                                        union_box_dt = emit_typeof(ctx, v.V);
                                    }
                                    // didn't handle this item before, select its new union index
                                    Value *cmp = ctx.builder.CreateICmpEQ(maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jt)), union_box_dt);
                                    union_box_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, 0x80 | idx), union_box_tindex);
                                }
                            },
                            typ,
                            counter);
                    if (union_box_dt) {
                        BasicBlock *isaBB = ctx.builder.GetInsertBlock();
                        BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_union_isa", ctx.f);
                        ctx.builder.CreateBr(postBB);
                        ctx.builder.SetInsertPoint(currBB);
                        Value *wasunknown = ctx.builder.CreateICmpEQ(v.TIndex, ConstantInt::get(T_int8, 0x80));
                        ctx.builder.CreateCondBr(wasunknown, isaBB, postBB);
                        ctx.builder.SetInsertPoint(postBB);
                        PHINode *tindex_phi = ctx.builder.CreatePHI(T_int8, 2);
                        tindex_phi->addIncoming(new_tindex, currBB);
                        tindex_phi->addIncoming(union_box_tindex, isaBB);
                        new_tindex = tindex_phi;
                    }

                }

                if (!skip_box.all()) {
                    // some values weren't unboxed in the new union
                    // box them now (tindex above already selected 0x80 = box for them)
                    // root the result, and return a new mark_julia_slot over the result
                    Value *boxv = box_union(ctx, v, skip_box);
                    Value *froot = NULL;
                    if (needsroot) {
                        // build a new gc-root, as needed
                        froot = emit_local_root(ctx);
                        Value *newroot = boxv;
                        if (wasboxed || v.gcroot) { // oldbox might be all ghost values (which don't need roots)
                            // store either the old box or the new box into the gc-root (skip_box ensures these are mutually-exclusive)
                            // need to clone the value from `v.gcroot` if this isn't a new box
                            Value *oldroot;
                            if (v.gcroot)
                                oldroot = ctx.builder.CreateLoad(v.gcroot);
                            else
                                oldroot = v.V;
                            newroot = ctx.builder.CreateSelect(wasboxed, emit_bitcast(ctx, oldroot, boxv->getType()), newroot);
                        }
                        ctx.builder.CreateStore(newroot, froot);
                    }
                    if (v.V == NULL) {
                        // v.V might be NULL if it was all ghost objects before
                        return jl_cgval_t(boxv, froot, false, typ, new_tindex);
                    }
                    else {
                        Value *isboxv = ctx.builder.CreateIsNotNull(boxv);
                        Value *slotv;
                        MDNode *tbaa;
                        bool isimmutable;
                        if (v.ispointer()) {
                            slotv = v.V;
                            tbaa = v.tbaa;
                            isimmutable = v.isimmutable;
                        }
                        else {
                            slotv = emit_static_alloca(ctx, v.V->getType());
                            ctx.builder.CreateStore(v.V, slotv);
                            tbaa = tbaa_stack;
                            isimmutable = true;
                        }
                        slotv = ctx.builder.CreateSelect(isboxv,
                            decay_derived(boxv), emit_bitcast(ctx, slotv, boxv->getType()));
                        jl_cgval_t newv = jl_cgval_t(slotv, froot, false, typ, new_tindex);
                        newv.tbaa = tbaa;
                        newv.isimmutable = isimmutable;
                        return newv;
                    }
                }
            }
            else {
                new_tindex = NULL;
                makeboxed = true;
            }
        }
        else if (!v.isboxed && jl_is_uniontype(typ)) {
            // previous value was unboxed (leaftype), statically compute union tindex
            assert(jl_is_leaf_type(v.typ));
            unsigned new_idx = get_box_tindex((jl_datatype_t*)v.typ, typ);
            if (new_idx) {
                new_tindex = ConstantInt::get(T_int8, new_idx);
                if (v.V && !v.ispointer()) {
                    // TODO: remove this branch once all consumers of v.TIndex understand how to handle a non-ispointer value
                    Value *slotv = emit_static_alloca(ctx, v.V->getType());
                    ctx.builder.CreateStore(v.V, slotv);
                    jl_cgval_t newv = jl_cgval_t(slotv, NULL, false, typ, new_tindex);
                    newv.tbaa = tbaa_stack;
                    newv.isimmutable = true;
                    return newv;
                }
            }
            else if (jl_subtype(v.typ, typ)) {
                makeboxed = true;
            }
            else {
                // unreachable
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
        else if (!v.isboxed) {
            makeboxed = true;
        }
        if (makeboxed) {
            // convert to a simple isboxed value
            Value *boxv = boxed(ctx, v, false);
            Value *froot = NULL;
            if (needsroot) {
                froot = emit_local_root(ctx);
                ctx.builder.CreateStore(maybe_decay_untracked(boxv), froot);
            }
            return jl_cgval_t(boxv, froot, true, typ, NULL);
        }
    }
    return jl_cgval_t(v, typ, new_tindex);
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
    // N.B.: `src` may have not been rooted by the caller.
    JL_TIMING(CODEGEN);
    jl_method_instance_t *li = *pli;
    assert(jl_is_method_instance(li));
    jl_llvm_functions_t decls = {};

    if (params != &jl_default_cgparams /* fast path */ &&
        !compare_cgparams(params, &jl_default_cgparams) && params->cached)
        jl_error("functions compiled with custom codegen params mustn't be cached");

    // Fast path for the already-compiled case
    if (jl_is_method(li->def.method)) {
        decls = li->functionObjectsDecls;
        bool already_compiled = params->cached && decls.functionObject != NULL;
        if (!src) {
            if ((already_compiled || li->jlcall_api == 2) &&
                (li->min_world <= world && li->max_world >= world)) {
                return decls;
            }
        } else if (already_compiled) {
            return decls;
        }
    }

    JL_GC_PUSH1(&src);
    JL_LOCK(&codegen_lock);
    decls = li->functionObjectsDecls;

    // Codegen lock held in this block
    {
        // Step 1: Re-check if this was already compiled (it may have been while
        // we waited at the lock).
        if (!jl_is_method(li->def.method)) {
            src = (jl_code_info_t*)li->inferred;
            if (decls.functionObject != NULL || !src || !jl_is_code_info(src) || li->jlcall_api == 2) {
                goto locked_out;
            }
        }
        else if (!src) {
            // If the caller didn't provide the source,
            // try to infer it for ourself, but first, re-check if it's already compiled.
            assert(li->min_world <= world && li->max_world >= world);
            if ((params->cached && decls.functionObject != NULL) || li->jlcall_api == 2)
                goto locked_out;

            // see if it is inferred
            src = (jl_code_info_t*)li->inferred;
            if (src) {
                if ((jl_value_t*)src != jl_nothing)
                    src = jl_uncompress_ast(li->def.method, (jl_array_t*)src);
                if (!jl_is_code_info(src)) {
                    src = jl_type_infer(pli, world, 0);
                    li = *pli;
                }
                if (!src || li->jlcall_api == 2)
                    goto locked_out;
            }
            else {
                // declare a failure to compile
                goto locked_out;
            }
        }
        else if (params->cached && decls.functionObject != NULL) {
            // similar to above, but never returns a NULL
            // decl (unless compile fails), even if jlcall_api == 2
            goto locked_out;
        }
        else {
            if ((jl_value_t*)src != jl_nothing)
                src = jl_uncompress_ast(li->def.method, (jl_array_t*)src);
        }
        assert(jl_is_code_info(src));

        // Step 2: setup global state
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
            else if (!jl_is_method(li->def.method)) // toplevel thunk
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
            JL_UNLOCK(&codegen_lock); // Might GC
            const char *mname = jl_symbol_name(jl_is_method(li->def.method) ? li->def.method->name : anonymous_sym);
            jl_rethrow_with_add("error compiling %s", mname);
        }
        const char *f = decls.functionObject;
        const char *specf = decls.specFunctionObject;

        if (JL_HOOK_TEST(params, module_activation)) {
            JL_HOOK_CALL(params, module_activation, 1, jl_box_voidpointer(wrap(m.release())));
        } else {
            // Step 4. Prepare debug info to receive this function
            // record that this function name came from this linfo,
            // so we can build a reverse mapping for debug-info.
            bool toplevel = !jl_is_method(li->def.method);
            if (!toplevel) {
                const DataLayout &DL = m->getDataLayout();
                // but don't remember toplevel thunks because
                // they may not be rooted in the gc for the life of the program,
                // and the runtime doesn't notify us when the code becomes unreachable :(
                jl_add_linfo_in_flight(StringRef(specf ? specf : f), li, DL);
            }

            // Step 5. Add the result to the execution engine now
            jl_finalize_module(m.release(), !toplevel);
        }

        // if not inlineable, code won't be needed again
        if (JL_DELETE_NON_INLINEABLE &&
                // don't delete code when debugging level >= 2
                jl_options.debug_level <= 1 &&
                // don't delete toplevel code
                jl_is_method(li->def.method) &&
                // don't change inferred state
                li->inferred &&
                // and there is something to delete (test this before calling jl_ast_flag_inlineable)
                li->inferred != jl_nothing &&
                // don't delete the code for the generator
                li != li->def.method->generator &&
                // don't delete inlineable code, unless it is constant
                (li->jlcall_api == 2 || !jl_ast_flag_inlineable((jl_array_t*)li->inferred)) &&
                // don't delete code when generating a precompile file
                !imaging_mode &&
                // don't delete code when it's not actually directly being used
                world) {
            li->inferred = jl_nothing;
        }

        // Step 6: Done compiling: Restore global state
        nested_compile = last_n_c;
    }

    JL_UNLOCK(&codegen_lock); // Might GC

    // If logging of the compilation stream is enabled then dump the function to the stream
    // ... unless li->def isn't defined here meaning the function is a toplevel thunk and
    // would have its CodeInfo printed in the stream, which might contain double-quotes that
    // would not be properly escaped given the double-quotes added to the stream below.
    if (dump_compiles_stream != NULL && jl_is_method(li->def.method)) {
        uint64_t this_time = jl_hrtime();
        jl_printf(dump_compiles_stream, "%" PRIu64 "\t\"", this_time - last_time);
        jl_static_show(dump_compiles_stream, (jl_value_t*)li);
        jl_printf(dump_compiles_stream, "\"\n");
        last_time = this_time;
    }
    JL_GC_POP();
    return decls;

locked_out:
    JL_UNLOCK(&codegen_lock);
    JL_GC_POP();
    return decls;
}

#define getModuleFlag(m,str) m->getModuleFlag(str)

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
    if (!getModuleFlag(m,"Debug Info Version"))
        m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
#if JL_LLVM_VERSION >= 40000
    m->setDataLayout(jl_data_layout);
#else
    m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
    m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());

}

// this ensures that llvmf has been emitted to the execution engine,
// returning the function pointer to it
extern void jl_callback_triggered_linfos(void);
static uint64_t getAddressForFunction(StringRef fname)
{
    JL_TIMING(LLVM_EMIT);
#ifdef JL_DEBUG_BUILD
    llvm::raw_fd_ostream out(1, false);
#endif
    jl_finalize_function(fname);
    uint64_t ret = jl_ExecutionEngine->getFunctionAddress(fname);
    // delay executing trace callbacks until here to make sure there's no
    // recursive compilation.
    jl_callback_triggered_linfos();
    return ret;
}

// convenience helper exported for usage from gdb
extern "C" JL_DLLEXPORT
uint64_t jl_get_llvm_fptr(void *function)
{
    Function *F = (Function*)function;
    uint64_t addr = getAddressForFunction(F->getName());
    if (!addr)
        addr = jl_ExecutionEngine->findUnmangledSymbol(F->getName()).getAddress();
    return addr;
}

static jl_method_instance_t *jl_get_unspecialized(jl_method_instance_t *method)
{
    // one unspecialized version of a function can be shared among all cached specializations
    jl_method_t *def = method->def.method;
    if (def->source == NULL) {
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
jl_generic_fptr_t jl_generate_fptr(jl_method_instance_t *li, const char *F, size_t world)
{
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
    if (jl_is_method(li->def.method)) {
        if (li->def.method->unspecialized) {
            unspec = li->def.method->unspecialized;
        }
        if (!F || !jl_can_finalize_function(F)) {
            // can't compile F in the JIT right now,
            // so instead compile an unspecialized version
            // and return its fptr instead
            if (!unspec)
                unspec = jl_get_unspecialized(li); // get-or-create the unspecialized version to cache the result
            jl_code_info_t *src = (jl_code_info_t*)unspec->def.method->source;
            if (src == NULL) {
                assert(unspec->def.method->generator);
                src = jl_code_for_staged(unspec);
            }
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
            F = jl_compile_linfo(&unspec, src, unspec->min_world, &jl_default_cgparams).functionObject; // this does not change unspec
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
    JL_LOCK(&codegen_lock);
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    JL_GC_POP();
    void *ptr = (void*)getAddressForFunction(llvmf->getName());
    JL_UNLOCK(&codegen_lock);
    return ptr;
}


// convenience function for debugging from gdb (pre-OrcJIT)
// it generally helps to have define KEEP_BODIES if you plan on using this
extern "C" JL_DLLEXPORT
void *jl_function_ptr_by_llvm_name(char *name) {
#ifdef JL_MSAN_ENABLED
    __msan_unpoison_string(name);
#endif
    return (void*)jl_ExecutionEngine->FindFunctionNamed(name); // returns an llvm::Function*
}

// export a C-callable entry point for a function (dllexport'ed dlsym), with a given name
extern "C" JL_DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    assert(jl_is_tuple_type(argt));
    JL_LOCK(&codegen_lock);
    Function *llvmf = jl_cfunction_object(f, rt, (jl_tupletype_t*)argt);
    // force eager emission of the function (llvm 3.3 gets confused otherwise and tries to do recursive compilation)
    uint64_t Addr = getAddressForFunction(llvmf->getName());

    if (imaging_mode)
        llvmf = cast<Function>(shadow_output->getNamedValue(llvmf->getName()));

    // make the alias to the shadow_module
    GlobalAlias *GA =
        GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                            GlobalValue::ExternalLinkage, name, llvmf, shadow_output);

    // make sure the alias name is valid for the current session
    jl_ExecutionEngine->addGlobalMapping(GA, (void*)(uintptr_t)Addr);
    JL_UNLOCK(&codegen_lock);
}

// --- native code info, and dump function to IR and ASM ---
// Get pointer to llvm::Function instance, compiling if necessary
// for use in reflection from Julia.
// this is paired with jl_dump_function_ir and jl_dump_function_asm in particular ways:
// misuse will leak memory or cause read-after-free
extern "C" JL_DLLEXPORT
void *jl_get_llvmf_defn(jl_method_instance_t *linfo, size_t world, bool getwrapper, bool optimize, const jl_cgparams_t params)
{
    if (jl_is_method(linfo->def.method) && linfo->def.method->source == NULL &&
        linfo->def.method->generator == NULL) {
        // not a generic function
        return NULL;
    }

    jl_code_info_t *src = (jl_code_info_t*)linfo->inferred;
    JL_GC_PUSH1(&src);
    if (!src || (jl_value_t*)src == jl_nothing) {
        src = jl_type_infer(&linfo, world, 0);
        if (!src && jl_is_method(linfo->def.method))
            src = linfo->def.method->generator ? jl_code_for_staged(linfo) : (jl_code_info_t*)linfo->def.method->source;
    }
    if ((jl_value_t*)src == jl_nothing)
        src = NULL;
    if (src && !jl_is_code_info(src) && jl_is_method(linfo->def.method))
        src = jl_uncompress_ast(linfo->def.method, (jl_array_t*)src);
    if (src && !jl_is_code_info(src))
        src = NULL;
    if (!src)
        jl_error("source not found for function");

    // Backup the info for the nested compile
    JL_LOCK(&codegen_lock);

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
        JL_UNLOCK(&codegen_lock); // Might GC
        const char *mname = jl_symbol_name(jl_is_method(linfo->def.method) ? linfo->def.method->name : anonymous_sym);
        jl_rethrow_with_add("error compiling %s", mname);
    }
    // Restore the previous compile context
    nested_compile = last_n_c;

    if (optimize)
        jl_globalPM->run(*m.get());
    const char *fname = declarations.functionObject;
    const char *specfname = declarations.specFunctionObject;
    Function *f = NULL;
    Function *specf = NULL;
    // swap declarations for definitions and destroy declarations
    if (specfname) {
        specf = cast<Function>(m->getNamedValue(specfname));
        free(const_cast<char*>(specfname));
    }
    if (fname) {
        f = cast<Function>(m->getNamedValue(fname));
        free(const_cast<char*>(fname));
    }
    // clone the name from the runtime linfo, if it exists
    // to give the user a (false) sense of stability
    specfname = linfo->functionObjectsDecls.specFunctionObject;
    if (specfname) {
        specf->setName(specfname);
    }
    fname = linfo->functionObjectsDecls.functionObject;
    if (fname) {
        f->setName(fname);
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
    if (jl_is_method(linfo->def.method) && linfo->def.method->source == NULL &&
        linfo->def.method->generator == NULL) {
        // not a generic function
        return NULL;
    }

    // compile this normally
    jl_llvm_functions_t decls = jl_compile_for_dispatch(&linfo, world);

    if (decls.functionObject == NULL && linfo->jlcall_api == 2 && jl_is_method(linfo->def.method)) {
        // normally we don't generate native code for these functions, so need an exception here
        // This leaks a bit of memory to cache native code that we'll never actually need
        JL_LOCK(&codegen_lock);
        decls = linfo->functionObjectsDecls;
        if (decls.functionObject == NULL) {
            jl_code_info_t *src = NULL;
            src = jl_type_infer(&linfo, world, 0);
            if (!src) {
                src = linfo->def.method->generator ? jl_code_for_staged(linfo) : (jl_code_info_t*)linfo->def.method->source;
            }
            decls = jl_compile_linfo(&linfo, src, world, &params);
            linfo->functionObjectsDecls = decls;
        }
        JL_UNLOCK(&codegen_lock);
    }

    if (getwrapper || !decls.specFunctionObject) {
        return Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage, decls.functionObject);
    }
    else {
        jl_returninfo_t returninfo = get_specsig_function(NULL, decls.specFunctionObject, linfo->specTypes, linfo->rettype);
        return returninfo.decl;
    }
}

// get a native disassembly for f (an LLVM function)
// warning: this takes ownership of, and destroys, f
extern "C" JL_DLLEXPORT
const jl_value_t *jl_dump_function_asm(void *f, int raw_mc, const char* asm_variant)
{
    Function *llvmf = dyn_cast_or_null<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_asm: Expected Function*");
    uint64_t fptr = getAddressForFunction(llvmf->getName());
    // Look in the system image as well
    if (fptr == 0)
        fptr = (uintptr_t)jl_ExecutionEngine->getPointerToGlobalIfAvailable(llvmf);
    delete llvmf;
    return jl_dump_fptr_asm(fptr, raw_mc, asm_variant);
}

// Logging for code coverage and memory allocation

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< std::vector<logdata_block*> > logdata_t;

static void visitLine(jl_codectx_t &ctx, std::vector<logdata_block*> &vec, int line, Value *addend, const char* name)
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
    ctx.builder.CreateStore(ctx.builder.CreateAdd(ctx.builder.CreateLoad(v, true, name),
                                          addend),
                        v, true); // not atomic, so this might be an underestimate,
                                  // but it's faster this way
}

// Code coverage

static logdata_t coverageData;

static void coverageVisitLine(jl_codectx_t &ctx, StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    visitLine(ctx, coverageData[filename], line, ConstantInt::get(T_int64, 1), "lcnt");
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(jl_codectx_t &ctx, StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0) {
        jl_gc_sync_total_bytes();
        return;
    }
    Value *addend = ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    visitLine(ctx, mallocData[filename], line, addend, "bytecnt");
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

static void show_source_loc(jl_codectx_t &ctx, JL_STREAM *out)
{
    jl_printf(out, "in %s at %s", ctx.name, ctx.file.str().c_str());
}

extern "C" void jl_binding_deprecation_warning(jl_binding_t *b);

static void cg_bdw(jl_codectx_t &ctx, jl_binding_t *b)
{
    jl_binding_deprecation_warning(b);
    if (b->deprecated == 1 && jl_options.depwarn) {
        show_source_loc(ctx, JL_STDERR);
        jl_printf(JL_STDERR, "\n");
    }
}

static jl_value_t *static_apply_type(jl_codectx_t &ctx, const jl_cgval_t *args, size_t nargs)
{
    jl_value_t **v = (jl_value_t**)alloca(sizeof(jl_value_t*) * nargs);
    for (size_t i = 0; i < nargs; i++) {
        if (!args[i].constant)
            return NULL;
        v[i] = args[i].constant;
    }
    assert(v[0] == jl_builtin_apply_type);
    size_t last_age = jl_get_ptls_states()->world_age;
    // call apply_type, but ignore errors. we know that will work in world 1.
    jl_get_ptls_states()->world_age = 1;
    jl_value_t *result = jl_apply_with_saved_exception_state(v, nargs, 1);
    jl_get_ptls_states()->world_age = last_age;
    return result;
}

// try to statically evaluate, NULL if not possible
static jl_value_t *static_eval(jl_codectx_t &ctx, jl_value_t *ex, int sparams=true, int allow_alloc=true)
{
    if (!JL_FEAT_TEST(ctx, static_alloc)) allow_alloc = 0;
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (jl_is_const(ctx.module, sym))
            return jl_get_global(ctx.module, sym);
        return NULL;
    }
    if (jl_is_slot(ex))
        return NULL;
    if (jl_is_ssavalue(ex)) {
        ssize_t idx = ((jl_ssavalue_t*)ex)->id;
        assert(idx >= 0);
        if (ctx.ssavalue_assigned.at(idx)) {
            return ctx.SAvalues.at(idx).constant;
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
            if (b->deprecated)
                cg_bdw(ctx, b);
            return b->value;
        }
        return NULL;
    }
    if (jl_is_expr(ex)) {
        jl_expr_t *e = (jl_expr_t*)ex;
        if (e->head == call_sym) {
            jl_value_t *f = static_eval(ctx, jl_exprarg(e, 0), sparams, allow_alloc);
            if (f) {
                if (jl_array_dim0(e->args) == 3 && f==jl_builtin_getfield) {
                    m = (jl_module_t*)static_eval(ctx, jl_exprarg(e, 1), sparams, allow_alloc);
                    // Check the tag before evaluating `s` so that a value of random
                    // type won't be corrupted.
                    if (!m || !jl_is_module(m))
                        return NULL;
                    // Assumes that the module is rooted somewhere.
                    s = (jl_sym_t*)static_eval(ctx, jl_exprarg(e, 2), sparams, allow_alloc);
                    if (s && jl_is_symbol(s)) {
                        jl_binding_t *b = jl_get_binding(m, s);
                        if (b && b->constp) {
                            if (b->deprecated)
                                cg_bdw(ctx, b);
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
                    JL_GC_PUSHARGS(v, n+1);
                    v[0] = f;
                    for (i = 0; i < n; i++) {
                        v[i+1] = static_eval(ctx, jl_exprarg(e, i+1), sparams, allow_alloc);
                        if (v[i+1] == NULL) {
                            JL_GC_POP();
                            return NULL;
                        }
                    }
                    size_t last_age = jl_get_ptls_states()->world_age;
                    // here we know we're calling specific builtin functions that work in world 1.
                    jl_get_ptls_states()->world_age = 1;
                    jl_value_t *result = jl_apply_with_saved_exception_state(v, n+1, 1);
                    jl_get_ptls_states()->world_age = last_age;
                    JL_GC_POP();
                    return result;
                }
            }
        }
        else if (e->head == static_parameter_sym) {
            size_t idx = jl_unbox_long(jl_exprarg(e, 0));
            if (idx <= jl_svec_len(ctx.linfo->sparam_vals)) {
                jl_value_t *e = jl_svecref(ctx.linfo->sparam_vals, idx - 1);
                if (jl_is_typevar(e))
                    return NULL;
                return e;
            }
        }
        return NULL;
    }
    return ex;
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
    for (int i = 0; i < (int)slength; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == enter_sym) {
                int last = (int)slength - 1;
                std::set<int> as =
                    assigned_in_try(stmts, i + 1,
                                    jl_unbox_long(jl_exprarg(st, 0)), &last);
                for (int j = 0; j < (int)slength; j++) {
                    if (j < i || j > last) {
                        std::set<int>::iterator it = as.begin();
                        for (; it != as.end(); it++) {
                            if (local_var_occurs(jl_array_ptr_ref(stmts, j), *it)) {
                                jl_varinfo_t &vi = slots[*it];
                                vi.isVolatile = true;
                            }
                        }
                    }
                }
            }
        }
    }
}

// --- use analysis ---

// a very simple, conservative use analysis
// to eagerly remove slot assignments that are never read from
static void simple_use_analysis(jl_codectx_t &ctx, jl_value_t *expr)
{
    if (jl_is_slot(expr)) {
        int i = jl_slot_number(expr) - 1;
        ctx.slots[i].used = true;
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i;
        if (e->head == method_sym) {
            simple_use_analysis(ctx, jl_exprarg(e, 0));
            if (jl_expr_nargs(e) > 1) {
                simple_use_analysis(ctx, jl_exprarg(e, 1));
                simple_use_analysis(ctx, jl_exprarg(e, 2));
            }
        }
        else if (e->head == assign_sym) {
            // don't consider assignment LHS as a variable "use"
            simple_use_analysis(ctx, jl_exprarg(e, 1));
        }
        else if (e->head != line_sym) {
            size_t elen = jl_array_dim0(e->args);
            for (i = 0; i < elen; i++) {
                simple_use_analysis(ctx, jl_exprarg(e, i));
            }
        }
    }
}

// --- gc root utils ---

// ---- Get Element Pointer (GEP) instructions within the GC frame ----

// Emit a gc-root slot indicator
static Value *emit_local_root(jl_codectx_t &ctx, jl_varinfo_t *vi)
{
    Instruction *newroot = new AllocaInst(T_prjlvalue, 0, "gcroot", /*InsertBefore*/ctx.ptlsStates);
    if (vi) {
        vi->boxroot->replaceAllUsesWith(newroot);
        newroot->takeName(vi->boxroot);
        vi->boxroot->eraseFromParent();
        vi->boxroot = newroot;
    }

    return newroot;
}

static void jl_add_method_root(jl_codectx_t &ctx, jl_value_t *val)
{
    if (jl_is_leaf_type(val) || jl_is_bool(val) || jl_is_symbol(val) ||
            val == (jl_value_t*)jl_any_type || val == (jl_value_t*)jl_bottom_type)
        return;
    JL_GC_PUSH1(&val);
    if (ctx.roots == NULL) {
        ctx.roots = jl_alloc_vec_any(1);
        jl_array_ptr_set(ctx.roots, 0, val);
    }
    else {
        size_t rlen = jl_array_dim0(ctx.roots);
        for (size_t i = 0; i < rlen; i++) {
            if (jl_array_ptr_ref(ctx.roots,i) == val) {
                JL_GC_POP();
                return;
            }
        }
        jl_array_ptr_1d_push(ctx.roots, val);
    }
    JL_GC_POP();
}

// --- generating function calls ---

static jl_cgval_t emit_globalref(jl_codectx_t &ctx, jl_module_t *mod, jl_sym_t *name)
{
    jl_binding_t *bnd = NULL;
    Value *bp = global_binding_pointer(ctx, mod, name, &bnd, false);
    // TODO: refactor. this partially duplicates code in emit_var
    if (bnd && bnd->value != NULL) {
        if (bnd->constp) {
            return mark_julia_const(bnd->value);
        }
        return mark_julia_type(ctx, tbaa_decorate(tbaa_binding, ctx.builder.CreateLoad(bp)), true, (jl_value_t*)jl_any_type);
    }
    // todo: use type info to avoid undef check
    return emit_checked_var(ctx, bp, name, false, tbaa_binding);
}

static jl_cgval_t emit_getfield(jl_codectx_t &ctx, const jl_cgval_t &strct, jl_sym_t *name)
{
    if (strct.constant && jl_is_module(strct.constant))
        return emit_globalref(ctx, (jl_module_t*)strct.constant, name);

    jl_datatype_t *sty = (jl_datatype_t*)strct.typ;
    if (jl_is_type_type((jl_value_t*)sty) && jl_is_leaf_type(jl_tparam0(sty)))
        sty = (jl_datatype_t*)jl_typeof(jl_tparam0(sty));
    sty = (jl_datatype_t*)jl_unwrap_unionall((jl_value_t*)sty);
    if (jl_is_structtype(sty) && sty != jl_module_type && sty->layout) {
        unsigned idx = jl_field_index(sty, name, 0);
        if (idx != (unsigned)-1) {
            return emit_getfield_knownidx(ctx, strct, idx, sty);
        }
    }
    // TODO: attempt better codegen for approximate types, if the types
    // and offsets of some fields are independent of parameters.

    // TODO: generic getfield func with more efficient calling convention
    jl_cgval_t myargs_array[2] = {
        strct,
        mark_julia_const((jl_value_t*)name)
    };
    Value *result = emit_jlcall(ctx, jlgetfield_func, maybe_decay_untracked(V_null), myargs_array, 2);
    bool needsgcroot = true; // !arg1.isimmutable || !jl_is_leaf_type(arg1.typ) || !is_datatype_all_pointers((jl_datatype_t*)arg1.typ); // TODO: probably want this as a llvm pass
    return mark_julia_type(ctx, result, true, jl_any_type, needsgcroot);
}

static Value *emit_bits_compare(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2)
{
    assert(jl_is_datatype(arg1.typ) && arg1.typ == arg2.typ);
    Type *at = julia_type_to_llvm(arg1.typ);

    if (at->isIntegerTy() || at->isPointerTy() || at->isFloatingPointTy()) {
        Type *at_int = INTT(at);
        Value *varg1 = emit_unbox(ctx, at_int, arg1, arg1.typ);
        Value *varg2 = emit_unbox(ctx, at_int, arg2, arg2.typ);
        return ctx.builder.CreateICmpEQ(varg1, varg2);
    }

    if (at->isVectorTy()) {
        jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
        Value *answer = ConstantInt::get(T_int1, 1);
        Value *varg1 = emit_unbox(ctx, at, arg1, arg1.typ);
        Value *varg2 = emit_unbox(ctx, at, arg2, arg2.typ);
        for (size_t i = 0, l = jl_svec_len(types); i < l; i++) {
            jl_value_t *fldty = jl_svecref(types, i);
            Value *subAns, *fld1, *fld2;
            fld1 = ctx.builder.CreateExtractElement(varg1, ConstantInt::get(T_int32, i)),
            fld2 = ctx.builder.CreateExtractElement(varg2, ConstantInt::get(T_int32, i)),
            subAns = emit_bits_compare(ctx,
                    mark_julia_type(ctx, fld1, false, fldty),
                    mark_julia_type(ctx, fld2, false, fldty));
            answer = ctx.builder.CreateAnd(answer, subAns);
        }
        return answer;
    }

    if (at->isAggregateType()) { // Struct or Array
        assert(arg1.ispointer() && arg2.ispointer());
        size_t sz = jl_datatype_size(arg1.typ);
        if (sz > 512 && !((jl_datatype_t*)arg1.typ)->layout->haspadding) {
            Value *answer = ctx.builder.CreateCall(prepare_call(memcmp_derived_func),
                            {
                            data_pointer(ctx, arg1, T_pint8),
                            data_pointer(ctx, arg2, T_pint8),
                            ConstantInt::get(T_size, sz)
                            });
            return ctx.builder.CreateICmpEQ(answer, ConstantInt::get(T_int32, 0));
        }
        else {
            Type *atp = at->getPointerTo();
            Value *varg1 = data_pointer(ctx, arg1, atp);
            Value *varg2 = data_pointer(ctx, arg2, atp);
            jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
            Value *answer = ConstantInt::get(T_int1, 1);
            for (size_t i = 0, l = jl_svec_len(types); i < l; i++) {
                jl_value_t *fldty = jl_svecref(types, i);
                Value *subAns, *fld1, *fld2;
                fld1 = ctx.builder.CreateConstGEP2_32(at, varg1, 0, i);
                fld2 = ctx.builder.CreateConstGEP2_32(at, varg2, 0, i);
                if (type_is_ghost(fld1->getType()->getPointerElementType()))
                    continue;
                subAns = emit_bits_compare(ctx,
                        mark_julia_slot(fld1, fldty, NULL, arg1.tbaa),
                        mark_julia_slot(fld2, fldty, NULL, arg2.tbaa));
                answer = ctx.builder.CreateAnd(answer, subAns);
            }
            return answer;
        }
    }
    assert(0 && "what is this llvm type?");
    abort();
}

// emit code for is (===).
static Value *emit_f_is(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2)
{
    jl_value_t *rt1 = arg1.typ, *rt2 = arg2.typ;
    bool istypes = false;
    if (jl_subtype(rt1, (jl_value_t*)jl_type_type) ||
        jl_subtype(rt2, (jl_value_t*)jl_type_type)) {
        // need to use typeseq for datatypes
        istypes = true;
    }
    if (!istypes && jl_is_leaf_type(rt1) && jl_is_leaf_type(rt2) && rt1 != rt2)
        // disjoint concrete leaf types are never equal (quick test)
        return ConstantInt::get(T_int1, 0);

    if (arg1.isghost || arg2.isghost) {
        // comparing to a singleton object
        if (arg1.TIndex)
            return emit_isa(ctx, arg1, rt2, NULL).first; // rt2 is a singleton type
        if (arg2.TIndex)
            return emit_isa(ctx, arg2, rt1, NULL).first; // rt1 is a singleton type
        // rooting these values isn't needed since we won't load this pointer
        // and we know at least one of them is a unique Singleton
        // which is already enough to ensure pointer uniqueness for this test
        // even if the other pointer managed to get garbage collected
        return ctx.builder.CreateICmpEQ(
            mark_callee_rooted(boxed(ctx, arg1, false)),
            mark_callee_rooted(boxed(ctx, arg2, false)));
    }

    if (jl_type_intersection(rt1, rt2) == (jl_value_t*)jl_bottom_type) // types are disjoint (exhaustive test)
        return ConstantInt::get(T_int1, 0);

    bool isbits = jl_isbits(rt1) || jl_isbits(rt2);
    if (isbits) { // whether this type is unique'd by value
        jl_value_t *typ = jl_isbits(rt1) ? rt1 : rt2;
        if (rt1 == rt2)
            return emit_bits_compare(ctx, arg1, arg2);
        Value *same_type = (typ == rt2) ? emit_isa(ctx, arg1, typ, NULL).first : emit_isa(ctx, arg2, typ, NULL).first;
        BasicBlock *currBB = ctx.builder.GetInsertBlock();
        BasicBlock *isaBB = BasicBlock::Create(jl_LLVMContext, "is", ctx.f);
        BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_is", ctx.f);
        ctx.builder.CreateCondBr(same_type, isaBB, postBB);
        ctx.builder.SetInsertPoint(isaBB);
        Value *bitcmp = emit_bits_compare(ctx,
                jl_cgval_t(arg1, typ, NULL),
                jl_cgval_t(arg2, typ, NULL));
        ctx.builder.CreateBr(postBB);
        ctx.builder.SetInsertPoint(postBB);
        PHINode *cmp = ctx.builder.CreatePHI(T_int1, 2);
        cmp->addIncoming(ConstantInt::get(T_int1, 0), currBB);
        cmp->addIncoming(bitcmp, isaBB);
        return cmp;
    }

    int ptr_comparable = 0; // whether this type is unique'd by pointer
    if (rt1 == (jl_value_t*)jl_sym_type || rt2 == (jl_value_t*)jl_sym_type)
        ptr_comparable = 1;
    if (jl_is_mutable_datatype(rt1) || jl_is_mutable_datatype(rt2)) // excludes abstract types
        ptr_comparable = 1;
    if (istypes) // need to use typeseq for datatypes
        ptr_comparable = 0;
    if ((jl_is_type_type(rt1) && jl_is_leaf_type(jl_tparam0(rt1))) ||
        (jl_is_type_type(rt2) && jl_is_leaf_type(jl_tparam0(rt2)))) // can compare leaf types by pointer
        ptr_comparable = 1;
    if ((rt1 == (jl_value_t*)jl_string_type && rt2 == (jl_value_t*)jl_string_type) ||
        (rt1 == (jl_value_t*)jl_simplevector_type && rt2 == (jl_value_t*)jl_simplevector_type))
        ptr_comparable = 0; // technically mutable, but compared by contents
    if (ptr_comparable) {
        Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : arg1.V;
        Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : arg2.V;
        assert(varg1 && varg2 && (arg1.isboxed || arg1.TIndex) && (arg2.isboxed || arg2.TIndex) &&
                "Only boxed types are valid for pointer comparison.");
        return ctx.builder.CreateICmpEQ(decay_derived(varg1),
                                        decay_derived(varg2));
    }

    JL_FEAT_REQUIRE(ctx, runtime);
    Value *varg1 = mark_callee_rooted(boxed(ctx, arg1));
    Value *varg2 = mark_callee_rooted(boxed(ctx, arg2, false)); // potentially unrooted!
    return ctx.builder.CreateTrunc(ctx.builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
}

static bool emit_builtin_call(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                              const jl_cgval_t *argv, size_t nargs, jl_value_t *rt,
                              jl_expr_t *ex)
// returns true if the call has been handled
{
    if (f == jl_builtin_is && nargs == 2) {
        // emit values
        const jl_cgval_t &v1 = argv[1];
        const jl_cgval_t &v2 = argv[2];
        // handle simple static expressions with no side-effects
        if (v1.constant) {
            if (v2.constant) {
                *ret = mark_julia_type(ctx, ConstantInt::get(T_int8, jl_egal(v1.constant, v2.constant)), false, jl_bool_type);
                return true;
            }
        }
        // emit comparison test
        Value *ans = emit_f_is(ctx, v1, v2);
        *ret = mark_julia_type(ctx, ctx.builder.CreateZExt(ans, T_int8), false, jl_bool_type);
        return true;
    }

    else if (f == jl_builtin_typeof && nargs == 1) {
        *ret = emit_typeof(ctx, argv[1]);
        return true;
    }

    else if (f == jl_builtin_typeassert && nargs == 2) {
        const jl_cgval_t &arg = argv[1];
        const jl_cgval_t &ty = argv[2];
        if (jl_is_type_type(ty.typ) && !jl_has_free_typevars(ty.typ)) {
            jl_value_t *tp0 = jl_tparam0(ty.typ);
            emit_typecheck(ctx, arg, tp0, "typeassert");
            *ret = arg;
            return true;
        }
        if (jl_subtype(ty.typ, (jl_value_t*)jl_type_type)) {
            Value *rt_arg = boxed(ctx, arg);
            Value *rt_ty = boxed(ctx, ty);
            JL_FEAT_REQUIRE(ctx, runtime);
            ctx.builder.CreateCall(prepare_call(jltypeassert_func), {rt_arg, rt_ty});
            *ret = arg;
            return true;
        }
    }

    else if (f == jl_builtin_isa && nargs == 2) {
        const jl_cgval_t &arg = argv[1];
        const jl_cgval_t &ty = argv[2];
        if (jl_is_type_type(ty.typ) && !jl_has_free_typevars(ty.typ)) {
            jl_value_t *tp0 = jl_tparam0(ty.typ);
            Value *isa_result = emit_isa(ctx, arg, tp0, NULL).first;
            if (isa_result->getType() == T_int1)
                isa_result = ctx.builder.CreateZExt(isa_result, T_int8);
            *ret = mark_julia_type(ctx, isa_result, false, jl_bool_type);
            return true;
        }
    }

    else if (f == jl_builtin_issubtype && nargs == 2) {
        const jl_cgval_t &ta = argv[1];
        const jl_cgval_t &tb = argv[2];
        if (jl_is_type_type(ta.typ) && !jl_has_free_typevars(ta.typ) &&
            jl_is_type_type(tb.typ) && !jl_has_free_typevars(tb.typ)) {
            int issub = jl_subtype(jl_tparam0(ta.typ), jl_tparam0(tb.typ));
            *ret = mark_julia_type(ctx, ConstantInt::get(T_int8, issub), false, jl_bool_type);
            return true;
        }
    }

    else if (f == jl_builtin__apply && nargs == 2 && ctx.vaSlot > 0) {
        // turn Core._apply(f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the vaStack allocation
        if (LoadInst *load = dyn_cast_or_null<LoadInst>(argv[2].V)) {
            if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot) {
                Value *theF = maybe_decay_untracked(boxed(ctx, argv[1]));
                Value *nva = emit_n_varargs(ctx);
#ifdef _P64
                nva = ctx.builder.CreateTrunc(nva, T_int32);
#endif
                Value *theArgs = ctx.builder.CreateGEP(ctx.argArray, ConstantInt::get(T_size, ctx.nReqArgs));
                JL_FEAT_REQUIRE(ctx, runtime);
                Value *r = ctx.builder.CreateCall(prepare_call(jlapply2va_func), { theF, theArgs, nva });
                *ret = mark_julia_type(ctx, r, true, jl_any_type);
                return true;
            }
        }
    }

    else if (f == jl_builtin_tuple) {
        if (nargs == 0) {
            *ret = ghostValue(jl_emptytuple_type);
            return true;
        }
        if (jl_is_tuple_type(rt) && jl_is_leaf_type(rt) && nargs == jl_datatype_nfields(rt)) {
            *ret = emit_new_struct(ctx, rt, nargs + 1, argv);
            return true;
        }
    }

    else if (f == jl_builtin_throw && nargs == 1) {
        Value *arg1 = boxed(ctx, argv[1], false); // rooted by throw
        raise_exception(ctx, arg1);
        *ret = jl_cgval_t();
        return true;
    }

    else if (f == jl_builtin_arraysize && nargs == 2) {
        const jl_cgval_t &ary = argv[1];
        const jl_cgval_t &idx = argv[2];
        jl_value_t *aty = jl_unwrap_unionall(ary.typ);
        if (jl_is_array_type(aty) && idx.typ == (jl_value_t*)jl_long_type) {
            jl_value_t *ndp = jl_tparam1(aty);
            if (jl_is_long(ndp)) {
                size_t ndims = jl_unbox_long(ndp);
                if (idx.constant) {
                    uint32_t idx_const = (uint32_t)jl_unbox_long(idx.constant);
                    if (idx_const > 0 && idx_const <= ndims) {
                        jl_value_t *ary_ex = jl_exprarg(ex, 1);
                        *ret = mark_julia_type(ctx, emit_arraysize(ctx, ary, ary_ex, idx_const), false, jl_long_type);
                        return true;
                    }
                    else if (idx_const > ndims) {
                        *ret = mark_julia_type(ctx, ConstantInt::get(T_size, 1), false, jl_long_type);
                        return true;
                    }
                }
                else {
                    Value *idx_dyn = emit_unbox(ctx, T_size, idx, (jl_value_t*)jl_long_type);
                    error_unless(ctx, ctx.builder.CreateICmpSGT(idx_dyn, ConstantInt::get(T_size, 0)),
                                 "arraysize: dimension out of range");
                    BasicBlock *outBB = BasicBlock::Create(jl_LLVMContext, "outofrange", ctx.f);
                    BasicBlock *inBB = BasicBlock::Create(jl_LLVMContext, "inrange");
                    BasicBlock *ansBB = BasicBlock::Create(jl_LLVMContext, "arraysize");
                    ctx.builder.CreateCondBr(ctx.builder.CreateICmpSLE(idx_dyn,
                                ConstantInt::get(T_size, ndims)),
                            inBB, outBB);
                    ctx.builder.SetInsertPoint(outBB);
                    Value *v_one = ConstantInt::get(T_size, 1);
                    ctx.builder.CreateBr(ansBB);
                    ctx.f->getBasicBlockList().push_back(inBB);
                    ctx.builder.SetInsertPoint(inBB);
                    Value *v_sz = emit_arraysize(ctx, ary, idx_dyn);
                    ctx.builder.CreateBr(ansBB);
                    ctx.f->getBasicBlockList().push_back(ansBB);
                    ctx.builder.SetInsertPoint(ansBB);
                    PHINode *result = ctx.builder.CreatePHI(T_size, 2);
                    result->addIncoming(v_one, outBB);
                    result->addIncoming(v_sz, inBB);
                    *ret = mark_julia_type(ctx, result, false, jl_long_type);
                    return true;
                }
            }
        }
    }

    else if (f == jl_builtin_arrayref && nargs >= 2) {
        const jl_cgval_t &ary = argv[1];
        bool indexes_ok = true;
        for (size_t i = 2; i <= nargs; i++) {
            if (argv[i].typ != (jl_value_t*)jl_long_type) {
                indexes_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(ary.typ);
        if (jl_is_array_type(aty_dt) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            jl_value_t *ndp = jl_tparam1(aty_dt);
            if (!jl_has_free_typevars(ety) && (jl_is_long(ndp) || nargs == 2)) {
                jl_value_t *ary_ex = jl_exprarg(ex, 1);
                if (!jl_array_store_unboxed(ety))
                    ety = (jl_value_t*)jl_any_type;
                ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[2], nargs - 1);
                if (jl_array_store_unboxed(ety) && jl_datatype_size(ety) == 0) {
                    assert(jl_is_datatype(ety));
                    assert(((jl_datatype_t*)ety)->instance != NULL);
                    *ret = ghostValue(ety);
                }
                else {
                    *ret = typed_load(ctx, emit_arrayptr(ctx, ary, ary_ex), idx, ety,
                        jl_array_store_unboxed(ety) ? tbaa_arraybuf : tbaa_ptrarraybuf);
                }
                return true;
            }
        }
    }

    else if (f == jl_builtin_arrayset && nargs >= 3) {
        const jl_cgval_t &ary = argv[1];
        const jl_cgval_t &val = argv[2];
        bool indexes_ok = true;
        for (size_t i = 3; i <= nargs; i++) {
            if (argv[i].typ != (jl_value_t*)jl_long_type) {
                indexes_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(ary.typ);
        if (jl_is_array_type(aty_dt) && indexes_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            jl_value_t *ndp = jl_tparam1(aty_dt);
            if (!jl_has_free_typevars(ety) && (jl_is_long(ndp) || nargs == 3)) {
                if (jl_subtype(val.typ, ety)) { // TODO: probably should just convert this to a type-assert
                    bool isboxed = !jl_array_store_unboxed(ety);
                    if (isboxed)
                        ety = (jl_value_t*)jl_any_type;
                    jl_value_t *ary_ex = jl_exprarg(ex, 1);
                    ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                    Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[3], nargs - 2);
                    if (!isboxed && jl_datatype_size(ety) == 0) {
                        assert(jl_is_datatype(ety));
                    }
                    else {
                        PHINode *data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed) { // if not boxed we don't need a write barrier
                            assert(ary.isboxed);
                            Value *aryv = maybe_decay_untracked(boxed(ctx, ary));
                            Value *flags = emit_arrayflags(ctx, ary);
                            // the owner of the data is ary itself except if ary->how == 3
                            flags = ctx.builder.CreateAnd(flags, 3);
                            Value *is_owned = ctx.builder.CreateICmpEQ(flags, ConstantInt::get(T_int16, 3));
                            BasicBlock *curBB = ctx.builder.GetInsertBlock();
                            BasicBlock *ownedBB = BasicBlock::Create(jl_LLVMContext, "array_owned", ctx.f);
                            BasicBlock *mergeBB = BasicBlock::Create(jl_LLVMContext, "merge_own", ctx.f);
                            ctx.builder.CreateCondBr(is_owned, ownedBB, mergeBB);
                            ctx.builder.SetInsertPoint(ownedBB);
                            // load owner pointer
                            Value *own_ptr;
                            if (jl_is_long(ndp)) {
                                own_ptr = tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(
                                    emit_bitcast(ctx,
                                        ctx.builder.CreateConstGEP1_32(
                                            emit_bitcast(ctx, decay_derived(aryv), T_pint8),
                                            jl_array_data_owner_offset(nd)),
                                        T_pprjlvalue)));
                            }
                            else {
                                own_ptr = ctx.builder.CreateCall(
                                    prepare_call(jlarray_data_owner_func),
                                    {aryv});
                            }
                            ctx.builder.CreateBr(mergeBB);
                            ctx.builder.SetInsertPoint(mergeBB);
                            data_owner = ctx.builder.CreatePHI(T_prjlvalue, 2);
                            data_owner->addIncoming(aryv, curBB);
                            data_owner->addIncoming(own_ptr, ownedBB);
                        }
                        typed_store(ctx,
                                    emit_arrayptr(ctx, ary, ary_ex, isboxed),
                                    idx, val, ety,
                                    !isboxed ? tbaa_arraybuf : tbaa_ptrarraybuf,
                                    data_owner, 0,
                                    false); // don't need to root the box if we had to make one since it's being stored in the array immediatly
                    }
                    *ret = ary;
                    return true;
                }
            }
        }
    }

    else if (f == jl_builtin_getfield && nargs == 2) {
        const jl_cgval_t &obj = argv[1];
        const jl_cgval_t &fld = argv[2];
        if (fld.constant && fld.typ == (jl_value_t*)jl_symbol_type) {
            *ret = emit_getfield(ctx, argv[1], (jl_sym_t*)fld.constant);
            return true;
        }

        if (fld.typ == (jl_value_t*)jl_long_type) {
            if (ctx.vaSlot > 0) {
                // optimize VA tuple
                if (LoadInst *load = dyn_cast_or_null<LoadInst>(obj.V)) {
                    if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot) {
                        Value *valen = emit_n_varargs(ctx);
                        jl_cgval_t va_ary( // fake instantiation of a cgval, in order to call emit_bounds_check
                                ctx.builder.CreateGEP(ctx.argArray, ConstantInt::get(T_size, ctx.nReqArgs)),
                                NULL, false, NULL, NULL);
                        Value *idx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                        idx = emit_bounds_check(ctx, va_ary, NULL, idx, valen);
                        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(T_size, ctx.nReqArgs));
                        Value *v = tbaa_decorate(tbaa_value, ctx.builder.CreateLoad(ctx.builder.CreateGEP(ctx.argArray, idx)));
                        *ret = mark_julia_type(ctx, v, /*boxed*/ true, jl_any_type, /*needsgcroot*/ false);
                        return true;
                    }
                }
            }

            jl_datatype_t *utt = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
            if (jl_is_datatype(utt) && utt->layout) {
                if ((jl_is_structtype(utt) || jl_is_tuple_type(utt)) && !jl_subtype((jl_value_t*)jl_module_type, obj.typ)) {
                    size_t nfields = jl_datatype_nfields(utt);
                    // integer index
                    size_t idx;
                    if (fld.constant && (idx = jl_unbox_long(fld.constant) - 1) < nfields) {
                        // known index
                        *ret = emit_getfield_knownidx(ctx, obj, idx, utt);
                        return true;
                    }
                    else {
                        // unknown index
                        Value *vidx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                        if (emit_getfield_unknownidx(ctx, ret, obj, vidx, utt)) {
                            return true;
                        }
                    }
                }
            }
            else {
                if (jl_is_tuple_type(utt) && is_tupletype_homogeneous(utt->types, true)) {
                    // For tuples, we can emit code even if we don't know the exact
                    // type (e.g. because we don't know the length). This is possible
                    // as long as we know that all elements are of the same (leaf) type.
                    if (obj.ispointer()) {
                        // Determine which was the type that was homogenous
                        jl_value_t *jt = jl_tparam0(utt);
                        if (jl_is_vararg_type(jt))
                            jt = jl_unwrap_vararg(jt);
                        Value *vidx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                        // This is not necessary for correctness, but allows to omit
                        // the extra code for getting the length of the tuple
                        if (!bounds_check_enabled(ctx)) {
                            vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(T_size, 1));
                        } else {
                            vidx = emit_bounds_check(ctx, obj, (jl_value_t*)obj.typ, vidx,
                                emit_datatype_nfields(ctx, emit_typeof_boxed(ctx, obj)));
                        }
                        Value *ptr = data_pointer(ctx, obj);
                        *ret = typed_load(ctx, ptr, vidx, jt, obj.tbaa, false);
                        return true;
                    }
                }
            }
        }
    }

    else if (f == jl_builtin_setfield && nargs == 3) {
        const jl_cgval_t &obj = argv[1];
        const jl_cgval_t &fld = argv[2];
        const jl_cgval_t &val = argv[3];

        jl_datatype_t *uty = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
        if (jl_is_structtype(uty) && uty != jl_module_type && uty->layout) {
            size_t idx = (size_t)-1;
            if (fld.constant && fld.typ == (jl_value_t*)jl_symbol_type) {
                idx = jl_field_index(uty, (jl_sym_t*)fld.constant, 0);
            }
            else if (fld.constant && fld.typ == (jl_value_t*)jl_long_type) {
                ssize_t i = jl_unbox_long(fld.constant);
                if (i > 0 && i <= jl_datatype_nfields(uty))
                    idx = i - 1;
            }
            if (idx != (size_t)-1) {
                jl_value_t *ft = jl_svecref(uty->types, idx);
                if (jl_subtype(val.typ, ft)) {
                    // TODO: attempt better codegen for approximate types
                    emit_setfield(ctx, uty, obj, idx, val, true, true);
                    *ret = val;
                    return true;
                }
            }
        }
    }

    else if (f == jl_builtin_nfields && nargs == 1) {
        const jl_cgval_t &obj = argv[1];
        if (ctx.vaSlot > 0) {
            // optimize VA tuple
            if (LoadInst *load = dyn_cast_or_null<LoadInst>(obj.V)) {
                if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot) {
                    *ret = mark_julia_type(ctx, emit_n_varargs(ctx), false, jl_long_type);
                    return true;
                }
            }
        }
        if (jl_is_type_type(obj.typ)) {
            jl_value_t *tp0 = jl_tparam0(obj.typ);
            if (jl_is_leaf_type(tp0)) {
                *ret = mark_julia_type(ctx, ConstantInt::get(T_size, jl_datatype_nfields(tp0)), false, jl_long_type);
                return true;
            }
        }
        else if (jl_is_leaf_type(obj.typ) || obj.constant) {
            Value *sz;
            if (obj.constant) {
                if (jl_typeof(obj.constant) == (jl_value_t*)jl_datatype_type)
                    sz = ConstantInt::get(T_size, jl_datatype_nfields(obj.constant));
                else
                    sz = ConstantInt::get(T_size, jl_datatype_nfields(obj.typ));
            }
            else if (obj.typ == (jl_value_t*)jl_datatype_type) {
                sz = emit_datatype_nfields(ctx, boxed(ctx, obj));
            }
            else {
                assert(jl_is_datatype(obj.typ));
                sz = ConstantInt::get(T_size, jl_datatype_nfields(obj.typ));
            }
            *ret = mark_julia_type(ctx, sz, false, jl_long_type);
            return true;
        }
    }

    else if (f == jl_builtin_fieldtype && nargs == 2) {
        const jl_cgval_t &typ = argv[1];
        const jl_cgval_t &fld = argv[2];
        if ((jl_is_type_type(typ.typ) && jl_is_leaf_type(jl_tparam0(typ.typ))) ||
                (typ.constant && jl_is_datatype(typ.constant)) ||
                typ.typ == (jl_value_t*)jl_datatype_type) {
            if (fld.typ == (jl_value_t*)jl_long_type) {
                assert(typ.isboxed);
                Value *tyv = boxed(ctx, typ);
                Value *types_svec = emit_datatype_types(ctx, tyv);
                Value *types_len = emit_datatype_nfields(ctx, tyv);
                Value *idx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                emit_bounds_check(ctx, typ, (jl_value_t*)jl_datatype_type, idx, types_len);
                Value *fieldtyp_p = ctx.builder.CreateGEP(decay_derived(emit_bitcast(ctx, types_svec, T_pprjlvalue)), idx);
                Value *fieldtyp = tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(fieldtyp_p));
                *ret = mark_julia_type(ctx, fieldtyp, true, (jl_value_t*)jl_type_type);
                return true;
            }
        }
    }

    else if (f == jl_builtin_sizeof && nargs == 1) {
        const jl_cgval_t &obj = argv[1];
        jl_datatype_t *sty = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
        assert(jl_string_type->mutabl);
        if (sty == jl_string_type || sty == jl_simplevector_type) {
            // String and SimpleVector's length fields have the same layout
            auto ptr = emit_bitcast(ctx, boxed(ctx, obj), T_psize);
            Value *len = tbaa_decorate(tbaa_mutab, ctx.builder.CreateLoad(ptr));
            if (sty == jl_simplevector_type) {
                len = ctx.builder.CreateMul(len, ConstantInt::get(T_size, sizeof(void*)));
                len = ctx.builder.CreateAdd(len, ConstantInt::get(T_size, sizeof(void*)));
            }
            *ret = mark_julia_type(ctx, len, false, jl_long_type);
            return true;
        }
        else if (jl_is_datatype(sty) && sty->name == jl_array_typename) {
            jl_value_t *ary_ex = jl_exprarg(ex, 1);
            auto len = emit_arraylen(ctx, obj, ary_ex);
            jl_value_t *ety = jl_tparam0(sty);
            Value *elsize;
            if (!jl_has_free_typevars(ety)) {
                if (!jl_array_store_unboxed(ety)) {
                    elsize = ConstantInt::get(T_size, sizeof(void*));
                }
                else {
                    elsize = ConstantInt::get(T_size, jl_datatype_size(ety));
                }
            }
            else {
                elsize = ctx.builder.CreateZExt(emit_arrayelsize(ctx, obj), T_size);
            }
            *ret = mark_julia_type(ctx, ctx.builder.CreateMul(len, elsize), false, jl_long_type);
            return true;
        }
        if (jl_is_type_type((jl_value_t*)sty) && !jl_is_typevar(jl_tparam0(sty))) {
            sty = (jl_datatype_t*)jl_tparam0(sty);
        }
        if (jl_is_datatype(sty) && sty != jl_symbol_type &&
                sty->name != jl_array_typename &&
                sty != jl_simplevector_type && sty != jl_string_type &&
                // exclude DataType, since each DataType has its own size, not sizeof(DataType).
                // this is issue #8798
                sty != jl_datatype_type) {
            if (jl_is_leaf_type((jl_value_t*)sty) ||
                    (jl_field_names(sty) == jl_emptysvec && jl_datatype_size(sty) > 0)) {
                *ret = mark_julia_type(ctx, ConstantInt::get(T_size, jl_datatype_size(sty)), false, jl_long_type);
                return true;
            }
        }
    }

    else if (f == jl_builtin_apply_type && nargs > 0) {
        if (jl_is_method(ctx.linfo->def.method)) {
            // don't bother codegen constant-folding for toplevel.
            jl_value_t *ty = static_apply_type(ctx, argv, nargs + 1);
            if (ty != NULL) {
                jl_add_method_root(ctx, ty);
                *ret = mark_julia_const(ty);
                return true;
            }
        }
    }

    else if (f == jl_builtin_isdefined && nargs == 2) {
        const jl_cgval_t &obj = argv[1];
        const jl_cgval_t &fld = argv[2];
        jl_datatype_t *stt = (jl_datatype_t*)obj.typ;
        if (!jl_is_leaf_type((jl_value_t*)stt) || jl_is_array_type(stt) ||
            stt == jl_module_type) { // TODO: use ->layout here instead of leaf_type
            return false;
        }
        assert(jl_is_datatype(stt));

        ssize_t fieldidx = -1;
        if (fld.constant && fld.typ == (jl_value_t*)jl_symbol_type) {
            jl_sym_t *sym = (jl_sym_t*)fld.constant;
            fieldidx = jl_field_index(stt, sym, 0);
        }
        else if (fld.constant && fld.typ == (jl_value_t*)jl_long_type) {
            fieldidx = jl_unbox_long(fld.constant) - 1;
        }
        else {
            return false;
        }
        if (fieldidx < 0 || fieldidx >= jl_datatype_nfields(stt)) {
            *ret = mark_julia_const(jl_false);
        }
        else if (!jl_field_isptr(stt, fieldidx) || fieldidx < stt->ninitialized) {
            *ret = mark_julia_const(jl_true);
        }
        else {
            size_t offs = jl_field_offset(stt, fieldidx);
            Value *ptr = data_pointer(ctx, obj, T_pint8);
            Value *llvm_idx = ConstantInt::get(T_size, offs);
            Value *addr = ctx.builder.CreateGEP(ptr, llvm_idx);
            Value *fldv = tbaa_decorate(obj.tbaa, ctx.builder.CreateLoad(emit_bitcast(ctx, addr, T_ppjlvalue)));
            Value *isdef = ctx.builder.CreateICmpNE(fldv, V_null);
            *ret = mark_julia_type(ctx, isdef, false, jl_bool_type);
        }
        return true;
    }
    return false;
}

static Value *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                          jl_cgval_t *argv, size_t nargs)
{
    // emit arguments
    SmallVector<Value*, 3> theArgs;
    if (theF)
        theArgs.push_back(theF);
    for (size_t i = 0; i < nargs; i++) {
        Value *arg = maybe_decay_untracked(boxed(ctx, argv[i], false));
        theArgs.push_back(arg);
    }
    SmallVector<Type *, 3> argsT;
    for (size_t i = 0; i < nargs + (theF != nullptr); i++) {
        argsT.push_back(T_prjlvalue);
    }
    FunctionType *FTy = FunctionType::get(T_prjlvalue, argsT, false);
    CallInst *result = ctx.builder.CreateCall(FTy,
        ctx.builder.CreateBitCast(prepare_call(theFptr), FTy->getPointerTo()),
        theArgs);
    if (theF)
        result->setCallingConv(JLCALL_F_CC);
    else
        result->setCallingConv(JLCALL_CC);
    return result;
}


static jl_cgval_t emit_call_function_object(jl_method_instance_t *li, jl_llvm_functions_t decls,
                                            jl_cgval_t *argv, size_t nargs, jl_value_t *inferred_retty, jl_codectx_t &ctx)
{
    if (decls.specFunctionObject != NULL) {
        // emit specialized call site
        jl_value_t *jlretty = li->rettype;
        jl_returninfo_t returninfo = get_specsig_function(jl_Module, decls.specFunctionObject, li->specTypes, jlretty);
        FunctionType *cft = returninfo.decl->getFunctionType();

        size_t nfargs = cft->getNumParams();
        Value **argvals = (Value**)alloca(nfargs * sizeof(Value*));
        unsigned idx = 0;
        AllocaInst *result;
        switch (returninfo.cc) {
        case jl_returninfo_t::Boxed:
        case jl_returninfo_t::Register:
        case jl_returninfo_t::Ghosts:
            break;
        case jl_returninfo_t::SRet:
            result = emit_static_alloca(ctx, cft->getParamType(0)->getContainedType(0));
            argvals[idx] = decay_derived(result);
            idx++;
            break;
        case jl_returninfo_t::Union:
            result = emit_static_alloca(ctx, ArrayType::get(T_int8, returninfo.union_bytes));
            if (returninfo.union_align > 1)
                result->setAlignment(returninfo.union_align);
            argvals[idx] = result;
            idx++;
            break;
        }

        for (size_t i = 0; i < nargs; i++) {
            jl_value_t *jt = jl_nth_slot_type(li->specTypes, i);
            bool isboxed;
            Type *et = julia_type_to_llvm(jt, &isboxed);
            if (type_is_ghost(et))
                continue;
            assert(idx < nfargs);
            Type *at = cft->getParamType(idx);
            const jl_cgval_t &arg = argv[i];
            if (isboxed) {
                assert(at == T_prjlvalue && (et == T_pjlvalue || et == T_prjlvalue));
                argvals[idx] = maybe_decay_untracked(boxed(ctx, arg));
            }
            else if (et->isAggregateType()) {
                // can lazy load on demand, no copy needed
                assert(at == PointerType::get(et, AddressSpace::Derived));
                assert(arg.ispointer());
                argvals[idx] = decay_derived(data_pointer(ctx, arg, at));
            }
            else {
                assert(at == et);
                argvals[idx] = emit_unbox(ctx, et, arg, jt);
            }
            idx++;
        }
        assert(idx == nfargs);
        CallInst *call = ctx.builder.CreateCall(returninfo.decl, ArrayRef<Value*>(&argvals[0], nfargs));
        call->setAttributes(returninfo.decl->getAttributes());

        jl_cgval_t retval;
        switch (returninfo.cc) {
            case jl_returninfo_t::Boxed:
                retval = mark_julia_type(ctx, call, true, inferred_retty);
                break;
            case jl_returninfo_t::Register:
                retval = mark_julia_type(ctx, call, false, jlretty);
                break;
            case jl_returninfo_t::SRet:
                retval = mark_julia_slot(result, jlretty, NULL, tbaa_stack);
                break;
            case jl_returninfo_t::Union: {
                Value *box = ctx.builder.CreateExtractValue(call, 0);
                Value *tindex = ctx.builder.CreateExtractValue(call, 1);
                Value *derived = ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x80)),
                            ConstantInt::get(T_int8, 0)),
                    decay_derived(ctx.builder.CreateBitCast(argvals[0], T_pjlvalue)),
                    decay_derived(box)
                );
                retval = mark_julia_slot(derived,
                                         jlretty,
                                         tindex,
                                         tbaa_stack);
                // root this, if the return value was a box (tindex & 0x80) != 0
                retval.gcroot = emit_local_root(ctx);
                ctx.builder.CreateStore(box, retval.gcroot);
                break;
            }
            case jl_returninfo_t::Ghosts:
                retval = mark_julia_slot(NULL, jlretty, call, tbaa_stack);
                break;
        }
        // see if inference has a different / better type for the call than the lambda
        if (inferred_retty != retval.typ)
            retval = update_julia_type(ctx, retval, inferred_retty);
        return retval;
    }
    Value *theFptr = jl_Module->getOrInsertFunction(decls.functionObject, jl_func_sig);
    Value *ret = emit_jlcall(ctx, theFptr, boxed(ctx, argv[0]), &argv[1], nargs - 1);
    return mark_julia_type(ctx, ret, true, inferred_retty);
}

static jl_cgval_t emit_invoke(jl_codectx_t &ctx, jl_expr_t *ex)
{
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    jl_value_t *rt = ex->etype;
    assert(arglen >= 2);

    jl_cgval_t lival = emit_expr(ctx, args[0]);
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
    }

    if (lival.constant) {
        jl_method_instance_t *li = (jl_method_instance_t*)lival.constant;
        assert(jl_is_method_instance(li));
        jl_llvm_functions_t decls = jl_compile_linfo(&li, NULL, ctx.world, ctx.params);
        if (li->jlcall_api == 2) {
            assert(li->inferred_const);
            return mark_julia_const(li->inferred_const);
        }
        if (decls.functionObject) {
            int jlcall_api = jl_jlcall_api(decls.functionObject);
            if (jlcall_api == 1) {
                jl_cgval_t result = emit_call_function_object(li, decls, argv, nargs, rt, ctx);
                if (result.typ == jl_bottom_type)
                    CreateTrap(ctx.builder);
                return result;
            }
        }
    }
    JL_FEAT_REQUIRE(ctx, runtime);
    jl_cgval_t result = mark_julia_type(ctx,
            emit_jlcall(
                ctx,
                prepare_call(jlinvoke_func),
                boxed(ctx, lival, false),
                argv, nargs),
            true,
            rt);
    if (result.typ == jl_bottom_type)
        CreateTrap(ctx.builder);
    return result;
}

static jl_cgval_t emit_call(jl_codectx_t &ctx, jl_expr_t *ex)
{
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_dim0(ex->args);
    assert(nargs >= 1);
    jl_value_t *rt = ex->etype;
    jl_cgval_t f = emit_expr(ctx, args[0]);

    if (f.constant && jl_typeis(f.constant, jl_intrinsic_type)) {
        return emit_intrinsic(ctx, (intrinsic)*(uint32_t*)jl_data_ptr(f.constant), args, nargs - 1);
    }

    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    argv[0] = f;
    for (size_t i = 1; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t(); // anything past here is unreachable
    }

    if (f.constant && jl_isa(f.constant, (jl_value_t*)jl_builtin_type)) {
        jl_cgval_t result;
        bool handled = emit_builtin_call(ctx, &result, f.constant, argv, nargs - 1, rt, ex);
        if (handled) {
            return result;
        }
    }

    // special case for known builtin not handled by emit_builtin_call
    if (f.constant && jl_isa(f.constant, (jl_value_t*)jl_builtin_type)) {
        std::map<jl_fptr_t, Function*>::iterator it = builtin_func_map.find(jl_get_builtin_fptr(f.constant));
        if (it != builtin_func_map.end()) {
            Value *theFptr = it->second;
            Value *ret = emit_jlcall(ctx, theFptr, maybe_decay_untracked(V_null), &argv[1], nargs - 1);
            return mark_julia_type(ctx, ret, true, rt);
        }
    }

    if (!JL_FEAT_TEST(ctx, runtime)) {
        char* name = NULL;
        if (jl_is_symbol(args[0]))
            name = jl_symbol_name((jl_sym_t*)args[0]);
        if (jl_is_globalref(args[0]))
            name = jl_symbol_name(jl_globalref_name(args[0]));
        jl_errorf("generic call to %s requires the runtime language feature",
                  name ? name : "<unknown>");
    }

    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, nargs);
    return mark_julia_type(ctx, callval, true, rt);
}

// --- accessing and assigning variables ---

static void undef_var_error_ifnot(jl_codectx_t &ctx, Value *ok, jl_sym_t *name)
{
    BasicBlock *err = BasicBlock::Create(jl_LLVMContext, "err", ctx.f);
    BasicBlock *ifok = BasicBlock::Create(jl_LLVMContext, "ok");
    ctx.builder.CreateCondBr(ok, ifok, err);
    ctx.builder.SetInsertPoint(err);
    ctx.builder.CreateCall(prepare_call(jlundefvarerror_func),
        mark_callee_rooted(literal_pointer_val(ctx, (jl_value_t*)name)));
    ctx.builder.CreateUnreachable();
    ctx.f->getBasicBlockList().push_back(ifok);
    ctx.builder.SetInsertPoint(ifok);
}

// returns a jl_ppvalue_t location for the global variable m.s
// if the reference currently bound or assign == true,
//   pbnd will also be assigned with the binding address
static Value *global_binding_pointer(jl_codectx_t &ctx, jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign)
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
            Constant *initnul = V_null;
            GlobalVariable *bindinggv = new GlobalVariable(*ctx.f->getParent(), T_pjlvalue,
                    false, GlobalVariable::InternalLinkage,
                    initnul, name.str());
            Value *cachedval = ctx.builder.CreateLoad(bindinggv);
            BasicBlock *have_val = BasicBlock::Create(jl_LLVMContext, "found"),
                *not_found = BasicBlock::Create(jl_LLVMContext, "notfound");
            BasicBlock *currentbb = ctx.builder.GetInsertBlock();
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpNE(cachedval, initnul), have_val, not_found);
            ctx.f->getBasicBlockList().push_back(not_found);
            ctx.builder.SetInsertPoint(not_found);
            Value *bval = ctx.builder.CreateCall(prepare_call(jlgetbindingorerror_func),
                                              {maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)m)),
                                              literal_pointer_val(ctx, (jl_value_t*)s)});
            ctx.builder.CreateStore(bval, bindinggv);
            ctx.builder.CreateBr(have_val);
            ctx.f->getBasicBlockList().push_back(have_val);
            ctx.builder.SetInsertPoint(have_val);
            PHINode *p = ctx.builder.CreatePHI(T_pjlvalue, 2);
            p->addIncoming(cachedval, currentbb);
            p->addIncoming(bval, not_found);
            return julia_binding_gv(ctx, emit_bitcast(ctx, p, T_pprjlvalue));
        }
        if (b->deprecated)
            cg_bdw(ctx, b);
    }
    if (pbnd)
        *pbnd = b;
    return julia_binding_gv(ctx, b);
}

static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, bool isvol, MDNode *tbaa)
{
    assert(bp->getType() == T_pprjlvalue);
    Instruction *v = ctx.builder.CreateLoad(bp, isvol);
    if (tbaa)
        tbaa_decorate(tbaa, v);
    undef_var_error_ifnot(ctx, ctx.builder.CreateICmpNE(v, maybe_decay_untracked(V_null)), name);
    return mark_julia_type(ctx, v, true, jl_any_type);
}

static jl_cgval_t emit_sparam(jl_codectx_t &ctx, size_t i)
{
    if (jl_svec_len(ctx.linfo->sparam_vals) > 0) {
        jl_value_t *e = jl_svecref(ctx.linfo->sparam_vals, i);
        if (!jl_is_typevar(e)) {
            return mark_julia_const(e);
        }
    }
    assert(ctx.spvals_ptr != NULL);
    Value *bp = ctx.builder.CreateConstInBoundsGEP1_32(
            T_prjlvalue,
            ctx.spvals_ptr,
            i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
    return mark_julia_type(ctx, tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(bp)), true, jl_any_type, false);
}

static jl_cgval_t emit_global(jl_codectx_t &ctx, jl_sym_t *sym)
{
    jl_binding_t *jbp = NULL;
    Value *bp = global_binding_pointer(ctx, ctx.module, sym, &jbp, false);
    assert(bp != NULL);
    if (jbp && jbp->value != NULL) {
        if (jbp->constp)
            return mark_julia_const(jbp->value);
        // double-check that a global variable is actually defined. this
        // can be a problem in parallel when a definition is missing on
        // one machine.
        return mark_julia_type(ctx, tbaa_decorate(tbaa_binding, ctx.builder.CreateLoad(bp)), true, jl_any_type);
    }
    return emit_checked_var(ctx, bp, sym, false, tbaa_binding);
}

static jl_cgval_t emit_isdefined(jl_codectx_t &ctx, jl_value_t *sym)
{
    Value *isnull;
    if (jl_is_slot(sym)) {
        size_t sl = jl_slot_number(sym) - 1;
        jl_varinfo_t &vi = ctx.slots[sl];
        if (!vi.usedUndef)
            return mark_julia_const(jl_true);
        if (vi.boxroot == NULL || vi.pTIndex != NULL) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateLoad(vi.defFlag, vi.isVolatile);
        }
        if (vi.boxroot != NULL) {
            Value *boxed = ctx.builder.CreateLoad(vi.boxroot, vi.isVolatile);
            Value *box_isnull = ctx.builder.CreateICmpNE(boxed, maybe_decay_untracked(V_null));
            if (vi.pTIndex) {
                // value is either boxed in the stack slot, or unboxed in value
                // as indicated by testing (pTIndex & 0x80)
                Value *tindex = ctx.builder.CreateLoad(vi.pTIndex, vi.isVolatile);
                Value *load_unbox = ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x80)),
                            ConstantInt::get(T_int8, 0));
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            }
            else {
                isnull = box_isnull;
            }
        }
    }
    else {
        jl_module_t *modu;
        jl_sym_t *name;
        if (jl_is_globalref(sym)) {
            modu = jl_globalref_mod(sym);
            name = jl_globalref_name(sym);
        }
        else {
            assert(jl_is_symbol(sym) && "malformed isdefined expression");
            modu = ctx.module;
            name = (jl_sym_t*)sym;
        }
        jl_binding_t *bnd = jl_get_binding(modu, name);
        if (bnd) {
            if (bnd->value != NULL)
                return mark_julia_const(jl_true);
            Value *bp = julia_binding_gv(ctx, bnd);
            Instruction *v = ctx.builder.CreateLoad(bp);
            tbaa_decorate(tbaa_binding, v);
            isnull = ctx.builder.CreateICmpNE(v, V_null);
        }
        else {
            Value *v = ctx.builder.CreateCall(prepare_call(jlboundp_func), {
                    literal_pointer_val(ctx, (jl_value_t*)modu),
                    literal_pointer_val(ctx, (jl_value_t*)name)
                });
            isnull = ctx.builder.CreateICmpNE(v, ConstantInt::get(T_int32, 0));
        }
    }
    return mark_julia_type(ctx, isnull, false, jl_bool_type);
}

static jl_cgval_t emit_local(jl_codectx_t &ctx, jl_value_t *slotload)
{
    size_t sl = jl_slot_number(slotload) - 1;
    jl_varinfo_t &vi = ctx.slots[sl];
    assert(!(ctx.vaStack && (int)sl == ctx.vaSlot && "error in escape analysis"));
    jl_sym_t *sym = slot_symbol(ctx, sl);
    jl_value_t *typ;
    if (jl_typeis(slotload, jl_typedslot_type)) {
        // use the better type from inference for this load
        typ = jl_typedslot_get_type(slotload);
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
    }
    else {
        // use the static type of the slot
        typ = vi.value.typ;
    }

    jl_cgval_t v;
    Value *isnull = NULL;
    if (vi.boxroot == NULL || vi.pTIndex != NULL) {
        if (!vi.isVolatile || vi.value.constant || !vi.value.V) {
            v = vi.value;
            if (vi.pTIndex)
                v.TIndex = ctx.builder.CreateLoad(vi.pTIndex);
        }
        else {
            // copy value to a non-volatile location
            AllocaInst *volatile_slot = cast<AllocaInst>(vi.value.V);
            Type *T = volatile_slot->getAllocatedType();
            assert(!volatile_slot->isArrayAllocation() && "variables not expected to be VLA");
            AllocaInst *slot = emit_static_alloca(ctx, T);
            // TODO: emit memcpy instead
            Value *unbox = ctx.builder.CreateLoad(vi.value.V, /*volatile*/true);
            ctx.builder.CreateStore(unbox, slot);
            Value *tindex = NULL;
            if (vi.pTIndex)
                tindex = ctx.builder.CreateLoad(vi.pTIndex, /*volatile*/true);
            v = mark_julia_slot(slot, vi.value.typ, tindex, tbaa_stack);
        }
        if (vi.boxroot == NULL)
            v = update_julia_type(ctx, v, typ);
        if (vi.usedUndef) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateLoad(vi.defFlag, vi.isVolatile);
        }
    }
    if (vi.boxroot != NULL) {
        Value *boxed = ctx.builder.CreateLoad(vi.boxroot, vi.isVolatile);
        Value *box_isnull;
        v.gcroot = vi.boxroot;
        if (vi.usedUndef)
            box_isnull = ctx.builder.CreateICmpNE(boxed, maybe_decay_untracked(V_null));
        if (vi.pTIndex) {
            // value is either boxed in the stack slot, or unboxed in value
            // as indicated by testing (pTIndex & 0x80)
            Value *load_unbox = ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(T_int8, 0x80)),
                        ConstantInt::get(T_int8, 0));
            if (vi.usedUndef)
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            if (v.V) { // v.V will be null if it is a union of all ghost values
                boxed = decay_derived(boxed);
                v.V = ctx.builder.CreateSelect(load_unbox, emit_bitcast(ctx,
                    decay_derived(v.V), boxed->getType()), boxed);
            } else
                v.V = boxed;
            v = update_julia_type(ctx, v, typ);
        }
        else {
            v = mark_julia_type(ctx, boxed, true, typ,
                                /*gc-root*/!vi.isArgument); // if an argument, doesn't need an additional root
            if (vi.usedUndef)
                isnull = box_isnull;
        }
    }
    if (isnull)
        undef_var_error_ifnot(ctx, isnull, sym);
    return v;
}


static void union_alloca_type(jl_uniontype_t *ut,
        bool &allunbox, size_t &nbytes, size_t &align, size_t &min_align)
{
    nbytes = 0;
    align = 0;
    min_align = MAX_ALIGN;
    // compute the size of the union alloca that could hold this type
    unsigned counter = 0;
    allunbox = for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (!jl_is_datatype_singleton(jt)) {
                    size_t nb1 = jl_datatype_size(jt);
                    size_t align1 = jl_datatype_align(jt);
                    if (nb1 > nbytes)
                        nbytes = nb1;
                    if (align1 > align)
                        align = align1;
                    if (align1 < min_align)
                        min_align = align1;
                }
            },
            (jl_value_t*)ut,
            counter);
}

static Value *try_emit_union_alloca(jl_codectx_t &ctx, jl_uniontype_t *ut, bool &allunbox, size_t &min_align)
{
    size_t nbytes, align;
    union_alloca_type(ut, allunbox, nbytes, align, min_align);
    if (nbytes > 0) {
        // at least some of the values can live on the stack
        // try to pick an Integer type size such that SROA will emit reasonable code
        Type *AT = ArrayType::get(IntegerType::get(jl_LLVMContext, 8 * min_align), (nbytes + min_align - 1) / min_align);
        AllocaInst *lv = emit_static_alloca(ctx, AT);
        if (align > 1)
            lv->setAlignment(align);
        return lv;
    }
    return NULL;
}

static Value *compute_box_tindex(jl_codectx_t &ctx, Value *datatype, jl_value_t *supertype, jl_value_t *ut)
{
    Value *tindex = ConstantInt::get(T_int8, 0);
    unsigned counter = 0;
    for_each_uniontype_small(
            [&](unsigned idx, jl_datatype_t *jt) {
                if (jl_subtype((jl_value_t*)jt, supertype)) {
                    Value *cmp = ctx.builder.CreateICmpEQ(maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jt)), datatype);
                    tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, idx), tindex);
                }
            },
            ut,
            counter);
    return tindex;
}

// get the runtime tindex value
static Value *compute_tindex_unboxed(jl_codectx_t &ctx, const jl_cgval_t &val, jl_value_t *typ)
{
    if (val.constant)
        return ConstantInt::get(T_int8, get_box_tindex((jl_datatype_t*)jl_typeof(val.constant), typ));
    if (val.isboxed)
        return compute_box_tindex(ctx, emit_typeof_boxed(ctx, val), val.typ, typ);
    assert(val.TIndex);
    return ctx.builder.CreateAnd(val.TIndex, ConstantInt::get(T_int8, 0x7f));
}

static void emit_assignment(jl_codectx_t &ctx, jl_value_t *l, jl_value_t *r)
{
    if (jl_is_ssavalue(l)) {
        ssize_t idx = ((jl_ssavalue_t*)l)->id;
        assert(idx >= 0);
        assert(!ctx.ssavalue_assigned.at(idx));
        jl_cgval_t slot = emit_expr(ctx, r); // slot could be a jl_value_t (unboxed) or jl_value_t* (ispointer)
        if (slot.isboxed || slot.TIndex) {
            // see if inference suggested a different type for the ssavalue than the expression
            // e.g. sometimes the information is inconsistent after inlining getfield on a Tuple
            jl_value_t *ssavalue_types = (jl_value_t*)ctx.source->ssavaluetypes;
            if (jl_is_array(ssavalue_types)) {
                jl_value_t *declType = jl_array_ptr_ref(ssavalue_types, idx);
                if (declType != slot.typ) {
                    slot = update_julia_type(ctx, slot, declType);
                }
            }
        }
        if (!slot.isboxed && !slot.isimmutable) {
            // emit a copy of values stored in mutable slots
            Value *dest;
            jl_value_t *jt = slot.typ;
            if (jl_is_uniontype(jt)) {
                assert(slot.TIndex && "Unboxed union must have a type-index.");
                bool allunbox;
                size_t min_align;
                dest = try_emit_union_alloca(ctx, ((jl_uniontype_t*)jt), allunbox, min_align);
                Value *isboxed = NULL;
                if (slot.ispointer() && slot.V != NULL && !isa<AllocaInst>(slot.V)) {
                    isboxed = ctx.builder.CreateICmpNE(
                            ctx.builder.CreateAnd(slot.TIndex, ConstantInt::get(T_int8, 0x80)),
                            ConstantInt::get(T_int8, 0));
                }
                if (dest)
                    emit_unionmove(ctx, dest, slot, isboxed, false, NULL);
                Value *gcroot = NULL;
                if (isboxed) {
                    Value *box;
                    if (slot.gcroot) {
                        gcroot = emit_local_root(ctx);
                        // This might load the wrong object in general, but if it gets selected, below,
                        // we know that it was in fact the one we wanted.
                        box = ctx.builder.CreateLoad(slot.gcroot);
                    } else {
                        gcroot = emit_static_alloca(ctx, T_pjlvalue);
                        box = V_null;
                    }
                    ctx.builder.CreateStore(box, gcroot);
                    if (dest) // might be all ghost values
                        dest = ctx.builder.CreateSelect(isboxed,
                            decay_derived(box),
                            emit_bitcast(ctx, decay_derived(dest), box->getType()));
                    else
                        dest = box;
                }
                else {
                    assert(allunbox && "Failed to allocate correct union-type storage.");
                }
                slot = mark_julia_slot(dest, slot.typ, slot.TIndex, tbaa_stack);
                slot.gcroot = gcroot;
            }
            else {
                bool isboxed;
                Type *vtype = julia_type_to_llvm(slot.typ, &isboxed);
                assert(!isboxed);
                dest = emit_static_alloca(ctx, vtype);
                emit_unbox(ctx, vtype, slot, slot.typ, dest);
                slot = mark_julia_slot(dest, slot.typ, NULL, tbaa_stack);
            }
        }
        ctx.SAvalues.at(idx) = slot; // now SAvalues[idx] contains the SAvalue
        ctx.ssavalue_assigned.at(idx) = true;
        return;
    }

    jl_sym_t *s = NULL;
    jl_binding_t *bnd = NULL;
    Value *bp = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_globalref(l))
        bp = global_binding_pointer(ctx, jl_globalref_mod(l), jl_globalref_name(l), &bnd, true); // now bp != NULL
    else
        assert(jl_is_slot(l));
    if (bp == NULL && s != NULL)
        bp = global_binding_pointer(ctx, ctx.module, s, &bnd, true);
    if (bp != NULL) { // it's a global
        JL_FEAT_REQUIRE(ctx, runtime);
        assert(bnd);
        Value *rval = mark_callee_rooted(boxed(ctx, emit_expr(ctx, r), false)); // no root needed since this is about to be assigned to a global
        ctx.builder.CreateCall(prepare_call(jlcheckassign_func),
                           { literal_pointer_val(ctx, bnd),
                             rval });
        // Global variable. Does not need debug info because the debugger knows about
        // its memory location.
        return;
    }

    int sl = jl_slot_number(l) - 1;
    // it's a local variable
    jl_varinfo_t &vi = ctx.slots[sl];
    jl_cgval_t rval_info = emit_expr(ctx, r);
    if (!vi.used)
        return;

    bool needs_root = false;
    if ((!vi.isSA && rval_info.gcroot) || !rval_info.isboxed)
        // rval needed a gcroot, so lval will need one too
        needs_root = true;

    // convert rval-type to lval-type
    jl_value_t *slot_type = vi.value.typ;
    rval_info = convert_julia_type(ctx, rval_info, slot_type, /*needs-root*/true);
    if (rval_info.typ == jl_bottom_type)
        return;

    // add info to arrayvar list
    if (rval_info.isboxed) {
        // check isboxed in case rval isn't the right type (for example, on a dead branch),
        // so we don't try to assign it to the arrayvar info
        jl_arrayvar_t *av = arrayvar_for(ctx, l);
        if (av != NULL)
            assign_arrayvar(ctx, *av, rval_info);
    }

    // compute / store tindex info
    if (vi.pTIndex) {
        Value *tindex;
        if (rval_info.TIndex) {
            tindex = rval_info.TIndex;
            if (!vi.boxroot)
                tindex = ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x7f));
        }
        else {
            assert(rval_info.isboxed || rval_info.constant);
            tindex = compute_tindex_unboxed(ctx, rval_info, vi.value.typ);
            if (vi.boxroot)
                tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
            if (!vi.boxroot)
                rval_info.TIndex = tindex;
        }
        ctx.builder.CreateStore(tindex, vi.pTIndex, vi.isVolatile);
    }

    // store boxed variables
    Value *isboxed = NULL;
    if (vi.boxroot) {
        if (isa<AllocaInst>(vi.boxroot) && needs_root)
            emit_local_root(ctx, &vi); // promote variable slot to a gcroot
        Value *rval;
        if (vi.pTIndex && rval_info.TIndex) {
            ctx.builder.CreateStore(rval_info.TIndex, vi.pTIndex, vi.isVolatile);
            isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(rval_info.TIndex, ConstantInt::get(T_int8, 0x80)),
                    ConstantInt::get(T_int8, 0));
            rval = maybe_decay_untracked(V_null);
            if (rval_info.ispointer() && rval_info.V != NULL && !isa<AllocaInst>(rval_info.V) &&
                !(isa<Constant>(isboxed) && cast<ConstantInt>(isboxed)->isZero())) // might be all ghost values or otherwise definitely not boxed
                rval = ctx.builder.CreateLoad(rval_info.gcroot);
            assert(!vi.value.constant);
        }
        else {
            assert(!vi.pTIndex || rval_info.isboxed || rval_info.constant);
            rval = maybe_decay_untracked(boxed(ctx, rval_info, false));
        }
        ctx.builder.CreateStore(maybe_decay_untracked(rval), vi.boxroot, vi.isVolatile);
    }

    // store unboxed variables
    if (!vi.boxroot || (vi.pTIndex && rval_info.TIndex)) {
        if (vi.usedUndef)
            store_def_flag(ctx, vi, true);

        if (!vi.value.constant) { // check that this is not a virtual store
            assert(vi.value.ispointer() || (vi.pTIndex && vi.value.V == NULL));
            // store value
            if (vi.value.V == NULL) {
                // all ghost values in destination - nothing to copy or store
            }
            else if (rval_info.constant || !rval_info.ispointer()) {
                if (rval_info.isghost) {
                    // all ghost values in source - nothing to copy or store
                }
                else {
                    if (rval_info.typ != vi.value.typ && !vi.pTIndex && !rval_info.TIndex) {
                        // isbits cast-on-assignment is invalid. this branch should be dead-code.
                        CreateTrap(ctx.builder);
                    }
                    else {
                        Value *dest = vi.value.V;
                        if (vi.pTIndex)
                            ctx.builder.CreateStore(UndefValue::get(cast<AllocaInst>(vi.value.V)->getAllocatedType()), vi.value.V);
                        Type *store_ty = julia_type_to_llvm(rval_info.constant ? jl_typeof(rval_info.constant) : rval_info.typ);
                        Type *dest_ty = store_ty->getPointerTo();
                        if (dest_ty != dest->getType())
                            dest = emit_bitcast(ctx, dest, dest_ty);
                        tbaa_decorate(tbaa_stack, ctx.builder.CreateStore(
                                          emit_unbox(ctx, store_ty, rval_info, rval_info.typ),
                                          dest,
                                          vi.isVolatile));
                    }
                }
            }
            else {
                MDNode *tbaa = rval_info.tbaa;
                // the memcpy intrinsic does not allow to specify different alias tags
                // for the load part (x.tbaa) and the store part (tbaa_stack).
                // since the tbaa lattice has to be a tree we have unfortunately
                // x.tbaa ∪ tbaa_stack = tbaa_root if x.tbaa != tbaa_stack
                if (tbaa != tbaa_stack)
                    tbaa = NULL;
                if (vi.pTIndex == NULL) {
                    assert(jl_is_leaf_type(vi.value.typ));
                    Value *copy_bytes = ConstantInt::get(T_int32, jl_datatype_size(vi.value.typ));
                    ctx.builder.CreateMemCpy(vi.value.V,
                                         data_pointer(ctx, rval_info, T_pint8),
                                         copy_bytes,
                                         jl_datatype_align(rval_info.typ),
                                         vi.isVolatile,
                                         tbaa);
                }
                else {
                    emit_unionmove(ctx, vi.value.V, rval_info, isboxed, vi.isVolatile, tbaa);
                }
            }
        }
        else {
            assert(vi.pTIndex == NULL);
        }
    }
}

// --- convert expression to code ---

static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const std::string &msg)
{
    bool isbool = (condV.typ == (jl_value_t*)jl_bool_type);
    if (!isbool) {
        if (condV.TIndex) {
            // check whether this might be bool
            isbool = jl_subtype((jl_value_t*)jl_bool_type, condV.typ);
        }
        emit_typecheck(ctx, condV, (jl_value_t*)jl_bool_type, msg);
    }
    if (isbool) {
        Value *cond = emit_unbox(ctx, T_int8, condV, (jl_value_t*)jl_bool_type);
        assert(cond->getType() == T_int8);
        return ctx.builder.CreateXor(ctx.builder.CreateTrunc(cond, T_int1), ConstantInt::get(T_int1, 1));
    }
    if (condV.isboxed) {
        return ctx.builder.CreateICmpEQ(boxed(ctx, condV),
            maybe_decay_untracked(literal_pointer_val(ctx, jl_false)));
    }
    // not a boolean
    return ConstantInt::get(T_int1, 0); // TODO: replace with Undef
}

static Value *emit_condition(jl_codectx_t &ctx, jl_value_t *cond, const std::string &msg)
{
    return emit_condition(ctx, emit_expr(ctx, cond), msg);
}

static void emit_stmtpos(jl_codectx_t &ctx, jl_value_t *expr)
{
    if (jl_is_ssavalue(expr))
        return; // value not used, no point in attempting codegen for it
    if (jl_is_linenode(expr))
        return;
    if (jl_is_slot(expr)) {
        size_t sl = jl_slot_number(expr) - 1;
        jl_varinfo_t &vi = ctx.slots[sl];
        if (vi.usedUndef)
            (void)emit_expr(ctx, expr);
        return;
    }
    if (jl_is_newvarnode(expr)) {
        jl_value_t *var = jl_fieldref(expr, 0);
        assert(jl_is_slot(var));
        jl_varinfo_t &vi = ctx.slots[jl_slot_number(var)-1];
        if (vi.usedUndef) {
            // create a new uninitialized variable
            Value *lv = vi.boxroot;
            if (lv != NULL)
                ctx.builder.CreateStore(maybe_decay_untracked(V_null), lv);
            if (lv == NULL || vi.pTIndex != NULL)
                store_def_flag(ctx, vi, false);
        }
        return;
    }
    if (!jl_is_expr(expr)) {
        (void)emit_expr(ctx, expr);
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
        ctx.builder.CreateCall(prepare_call(jlleave_func),
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else {
        if (!jl_is_method(ctx.linfo->def.method)) {
            // TODO: inference is invalid if this has an effect
            Value *world = ctx.builder.CreateLoad(prepare_global(jlgetworld_global));
            ctx.builder.CreateStore(world, ctx.world_age_field);
        }
        (void)emit_expr(ctx, expr);
    }
}

static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_global(ctx, sym);
    }
    if (jl_is_slot(expr)) {
        return emit_local(ctx, expr);
    }
    if (jl_is_ssavalue(expr)) {
        ssize_t idx = ((jl_ssavalue_t*)expr)->id;
        assert(idx >= 0);
        if (!ctx.ssavalue_assigned.at(idx)) {
            ctx.ssavalue_assigned.at(idx) = true; // (assignment, not comparison test)
            return jl_cgval_t(); // dead code branch
        }
        else {
            return ctx.SAvalues.at(idx); // at this point, SAvalues[idx] actually contains the SAvalue
        }
    }
    if (jl_is_globalref(expr)) {
        return emit_globalref(ctx, jl_globalref_mod(expr), jl_globalref_name(expr));
    }
    if (jl_is_labelnode(expr)) {
        jl_error("LabelNode in value position");
    }
    if (jl_is_linenode(expr)) {
        jl_error("LineNumberNode in value position");
    }
    if (jl_is_gotonode(expr)) {
        jl_error("GotoNode in value position");
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
        if (needroot && jl_is_method(ctx.linfo->def.method)) { // toplevel exprs and some integers are already rooted
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
    if (head == isdefined_sym) {
        return emit_isdefined(ctx, args[0]);
    }
    else if (head == invoke_sym) {
        return emit_invoke(ctx, ex);
    }
    else if (head == call_sym) {
        jl_cgval_t res = emit_call(ctx, ex);
        // some intrinsics (e.g. typeassert) can return a wider type
        // than what's actually possible
        jl_value_t *expr_t = ex->etype;
        res = update_julia_type(ctx, res, expr_t);
        if (res.typ == jl_bottom_type || expr_t == jl_bottom_type) {
            CreateTrap(ctx.builder);
        }
        return res;
    }
    else if (head == foreigncall_sym) {
        return emit_ccall(ctx, args, jl_array_dim0(ex->args));
    }
    else if (head == assign_sym) {
        emit_assignment(ctx, args[0], args[1]);
        return ghostValue(jl_void_type);
    }
    else if (head == static_parameter_sym) {
        return emit_sparam(ctx, jl_unbox_long(args[0]) - 1);
    }
    else if (head == method_sym) {
        jl_value_t *mn = args[0];
        assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(mn) || jl_is_slot(mn));
        JL_FEAT_REQUIRE(ctx, runtime);

        Value *bp = NULL, *name, *bp_owner = V_null;
        jl_binding_t *bnd = NULL;
        bool issym = jl_is_symbol(mn);
        bool isglobalref = !issym && jl_is_globalref(mn);
        jl_module_t *mod = ctx.module;
        if (issym || isglobalref) {
            if (isglobalref) {
                mod = jl_globalref_mod(mn);
                mn = (jl_value_t*)jl_globalref_name(mn);
            }
            if (jl_symbol_name((jl_sym_t*)mn)[0] == '@')
                jl_errorf("macro definition not allowed inside a local scope");
            name = literal_pointer_val(ctx, mn);
            bnd = jl_get_binding_for_method_def(mod, (jl_sym_t*)mn);
            bp = julia_binding_gv(ctx, bnd);
            bp_owner = literal_pointer_val(ctx, (jl_value_t*)mod);
        }
        else if (jl_is_slot(mn)) {
            int sl = jl_slot_number(mn)-1;
            jl_varinfo_t &vi = ctx.slots[sl];
            bp = vi.boxroot;
            name = literal_pointer_val(ctx, (jl_value_t*)slot_symbol(ctx, sl));
        }
        if (bp) {
            Value *mdargs[5] = { name, literal_pointer_val(ctx, (jl_value_t*)mod), bp,
                                 maybe_decay_untracked(bp_owner), literal_pointer_val(ctx, bnd) };
            jl_cgval_t gf = mark_julia_type(
                    ctx,
                    ctx.builder.CreateCall(prepare_call(jlgenericfunction_func), makeArrayRef(mdargs)),
                    true,
                    jl_function_type);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }
        Value *a1 = maybe_decay_untracked(boxed(ctx, emit_expr(ctx, args[1])));
        Value *a2 = maybe_decay_untracked(boxed(ctx, emit_expr(ctx, args[2])));
        Value *mdargs[4] = {
            /*argdata*/a1,
            /*code*/a2,
            /*module*/literal_pointer_val(ctx, (jl_value_t*)ctx.module),
            /*isstaged*/literal_pointer_val(ctx, args[3])
        };
        ctx.builder.CreateCall(prepare_call(jlmethod_func), makeArrayRef(mdargs));
        return ghostValue(jl_void_type);
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx.module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (jl_is_symbol(sym)) {
            JL_FEAT_REQUIRE(ctx, runtime);
            jl_binding_t *bnd = NULL;
            (void)global_binding_pointer(ctx, mod, sym, &bnd, true); assert(bnd);
            ctx.builder.CreateCall(prepare_call(jldeclareconst_func),
                               literal_pointer_val(ctx, bnd));
        }
    }
    else if (head == new_sym) {
        size_t nargs = jl_array_len(ex->args);
        jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        jl_value_t *ty = argv[0].typ;
        if (jl_is_type_type(ty) &&
                jl_is_datatype(jl_tparam0(ty)) &&
                jl_is_leaf_type(jl_tparam0(ty))) {
            assert(nargs <= jl_datatype_nfields(jl_tparam0(ty)) + 1);
            return emit_new_struct(ctx, jl_tparam0(ty), nargs, argv);
        }
        Value *typ = boxed(ctx, argv[0]);
        Value *val = emit_jlcall(ctx, jlnew_func, typ, &argv[1], nargs - 1);
        return mark_julia_type(ctx, val, true, ty);
    }
    else if (head == exc_sym) { // *ptls->exception_in_transit
        return mark_julia_type(ctx,
                ctx.builder.CreateLoad(emit_exc_in_transit(ctx), /*isvolatile*/true),
                true, jl_any_type);
    }
    else if (head == copyast_sym) {
        JL_FEAT_REQUIRE(ctx, runtime);
        jl_value_t *arg = args[0];
        if (jl_is_quotenode(arg)) {
            jl_value_t *arg1 = jl_fieldref(arg, 0);
            if (!(jl_is_expr(arg1) || jl_typeis(arg1, jl_array_any_type) || jl_is_quotenode(arg1))) {
                // elide call to jl_copy_ast when possible
                return emit_expr(ctx, arg);
            }
        }
        jl_cgval_t ast = emit_expr(ctx, arg);
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jlcopyast_func),
                    maybe_decay_untracked(boxed(ctx, ast))), true, ast.typ);
    }
    else if (head == simdloop_sym) {
        llvm::annotateSimdLoop(ctx.builder.GetInsertBlock());
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
            !jl_is_method(ctx.linfo->def.method)) {
            JL_FEAT_REQUIRE(ctx, runtime);
            // call interpreter to run a toplevel expr from inside a
            // compiled toplevel thunk.
            Value *args[2] = {
                literal_pointer_val(ctx, (jl_value_t*)ctx.module),
                literal_pointer_val(ctx, expr)
            };
            ctx.builder.CreateCall(prepare_call(jltopeval_func), args);
            return ghostValue(jl_void_type);
        }
        if (head == abstracttype_sym || head == compositetype_sym ||
            head == bitstype_sym) {
            jl_errorf("type definition not allowed inside a local scope");
        }
        else {
            jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                      jl_symbol_name(head), ctx.name);
        }
    }
    return jl_cgval_t();
}

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0)
{
    // TODO: requires the runtime, but is generated unconditionally

    // allocate a placeholder gc instruction
    ctx.ptlsStates = ctx.builder.CreateCall(prepare_call(jltls_states_func));
    int nthfield = offsetof(jl_tls_states_t, safepoint) / sizeof(void*);
    ctx.signalPage = emit_nthptr_recast(ctx, ctx.ptlsStates, nthfield, tbaa_const,
                                        PointerType::get(T_psize, 0), false);
}

static void emit_last_age_field(jl_codectx_t &ctx)
{
    ctx.world_age_field = ctx.builder.CreateGEP(
            ctx.builder.CreateBitCast(ctx.ptlsStates, T_psize),
            ConstantInt::get(T_size, offsetof(jl_tls_states_t, world_age) / sizeof(size_t)));
}

static void emit_cfunc_invalidate(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc,
        jl_method_instance_t *lam, size_t nargs, size_t world)
{
    jl_codectx_t ctx(jl_LLVMContext);
    ctx.f = gf_thunk;
    ctx.world = world;
    ctx.params = &jl_default_cgparams;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", gf_thunk);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    Function::arg_iterator AI = gf_thunk->arg_begin();
    Value *myargs = new AllocaInst(T_prjlvalue,
#if JL_LLVM_VERSION >= 50000
        0,
#endif
        ConstantInt::get(T_int32, nargs), "jlcall", ctx.ptlsStates);
    if (cc == jl_returninfo_t::SRet || cc == jl_returninfo_t::Union)
        ++AI;
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed;
        Type *et = julia_type_to_llvm(jt, &isboxed);
        Value *arg_box;
        if (type_is_ghost(et)) {
            assert(jl_is_datatype(jt) && ((jl_datatype_t*)jt)->instance);
            arg_box = literal_pointer_val(ctx, ((jl_datatype_t*)jt)->instance);
        }
        else {
            Value *arg_v = &*AI;
            ++AI;
            Type *at = arg_v->getType();
            if (isboxed) {
                assert(at == T_prjlvalue && et == T_pjlvalue);
                arg_box = arg_v;
            }
            else if (et->isAggregateType()) {
                arg_box = boxed(ctx, mark_julia_slot(arg_v, jt, NULL, tbaa_const), false);
            }
            else {
                assert(at == et);
                arg_box = boxed(ctx, mark_julia_type(ctx, arg_v, false, jt), false);
            }
            (void)at;
        }
        Value *argn = ctx.builder.CreateConstInBoundsGEP1_32(T_prjlvalue, myargs, i);
        ctx.builder.CreateStore(maybe_decay_untracked(arg_box), argn);
    }
    assert(AI == gf_thunk->arg_end());
    Value *nargs_v = ConstantInt::get(T_int32, nargs);
    Value *gf_ret = ctx.builder.CreateCall(prepare_call(jlapplygeneric_func), { myargs, nargs_v });
    jl_cgval_t gf_retbox = mark_julia_type(ctx, gf_ret, true, jl_any_type, /*needsroot*/false);
    jl_value_t *astrt = lam->rettype;
    if (cc != jl_returninfo_t::Boxed) {
        emit_typecheck(ctx, gf_retbox, astrt, "cfunction");
    }

    switch (cc) {
    case jl_returninfo_t::Boxed:
        ctx.builder.CreateRet(gf_ret);
        break;
    case jl_returninfo_t::Register: {
        Type *gfrt = gf_thunk->getReturnType();
        if (gfrt->isVoidTy()) {
            ctx.builder.CreateRetVoid();
        }
        else {
            gf_ret = emit_bitcast(ctx, gf_ret, gfrt->getPointerTo());
            ctx.builder.CreateRet(ctx.builder.CreateLoad(gf_ret));
        }
        break;
    }
    case jl_returninfo_t::SRet: {
        unsigned sret_nbytes = jl_datatype_size(astrt);
        ctx.builder.CreateMemCpy(&*gf_thunk->arg_begin(), gf_ret, sret_nbytes, jl_alignment(sret_nbytes));
        ctx.builder.CreateRetVoid();
        break;
    }
    case jl_returninfo_t::Union: {
        Type *retty = gf_thunk->getReturnType();
        Value *gf_retval = UndefValue::get(retty);
        Value *tindex = compute_box_tindex(ctx, gf_ret, (jl_value_t*)jl_any_type, astrt);
        tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
        gf_retval = ctx.builder.CreateInsertValue(gf_retval, gf_ret, 0);
        gf_retval = ctx.builder.CreateInsertValue(gf_retval, tindex, 1);
        ctx.builder.CreateRet(gf_retval);
        break;
    }
    case jl_returninfo_t::Ghosts: {
        Value *gf_retval = compute_tindex_unboxed(ctx, gf_retbox, astrt);
        ctx.builder.CreateRet(gf_retval);
        break;
    }
    }
}

static Function *gen_cfun_wrapper(jl_function_t *ff, jl_value_t *jlrettype, jl_tupletype_t *argt,
                                  jl_typemap_entry_t *sf, jl_value_t *declrt, jl_tupletype_t *sigt)
{
    // Generate a c-callable wrapper
    bool toboxed;
    Type *crt = julia_struct_to_llvm(jlrettype, NULL, &toboxed);
    if (crt == NULL)
        jl_error("cfunction: return type doesn't correspond to a C type");
    else if (toboxed)
        crt = T_prjlvalue;

    size_t nargs = jl_nparams(argt);
    function_sig_t sig(crt, jlrettype, toboxed, argt->parameters, NULL, nargs, false, CallingConv::C, false);
    if (!sig.err_msg.empty())
        jl_error(sig.err_msg.c_str());
    if (sig.fargt.size() + sig.sret != sig.fargt_sig.size())
        jl_error("va_arg syntax not allowed for cfunction argument list");

    const char *name = "cfunction";
    size_t world = jl_world_counter;
    // try to look up this function for direct invoking
    jl_method_instance_t *lam = jl_get_specialization1((jl_tupletype_t*)sigt, world);
    jl_value_t *astrt = (jl_value_t*)jl_any_type;
    // infer it first, if necessary
    if (lam) {
        name = jl_symbol_name(lam->def.method->name);
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
    cw->addFnAttr("no-frame-pointer-elim", "true");
    Function *cw_proto = function_proto(cw);

    jl_codectx_t ctx(jl_LLVMContext);
    ctx.f = cw;
    ctx.linfo = lam;
    ctx.world = world;
    ctx.params = &jl_default_cgparams;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", cw);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);
    emit_last_age_field(ctx);

    Value *dummy_world = ctx.builder.CreateAlloca(T_size);
    Value *have_tls = ctx.builder.CreateIsNotNull(ctx.ptlsStates);
    // TODO: in the future, try to initialize a full TLS context here
    // for now, just use a dummy field to avoid a branch in this function
    ctx.world_age_field = ctx.builder.CreateSelect(have_tls, ctx.world_age_field, dummy_world);
    Value *last_age = tbaa_decorate(tbaa_gcframe, ctx.builder.CreateLoad(ctx.world_age_field));
    have_tls = ctx.builder.CreateAnd(have_tls, ctx.builder.CreateIsNotNull(last_age));
    Value *world_v = ctx.builder.CreateLoad(prepare_global(jlgetworld_global));

    Value *age_ok = NULL;
    if (lam) {
        Value *lam_max = ctx.builder.CreateLoad(
                ctx.builder.CreateConstInBoundsGEP1_32(
                    T_size,
                    emit_bitcast(ctx, decay_derived(literal_pointer_val(ctx, (jl_value_t*)lam)), T_psize),
                    offsetof(jl_method_instance_t, max_world) / sizeof(size_t)));
        // XXX: age is always OK if we don't have a TLS. This is a hack required due to `@threadcall` abuse.
        // and adds quite a bit of complexity here, even though it's still wrong
        // (anything that tries to interact with the runtime will fault)
        age_ok = ctx.builder.CreateICmpUGE(lam_max, world_v);
        world_v = ctx.builder.CreateSelect(ctx.builder.CreateOr(have_tls, age_ok), world_v, lam_max);
        age_ok = ctx.builder.CreateOr(ctx.builder.CreateNot(have_tls), age_ok);
    }
    ctx.builder.CreateStore(world_v, ctx.world_age_field);
    // Save the Function object reference
    sf->func.value = jl_box_voidpointer((void*)cw_proto);
    jl_gc_wb(sf, sf->func.value);

    // first emit code to record the arguments
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = sig.sret ? &*AI++ : NULL;
    jl_cgval_t *inputargs = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * (nargs + 1));
    inputargs[0] = mark_julia_const(ff); // we need to pass the function object even if (even though) it is a ghost
    for (size_t i = 0; i < nargs; ++i, ++AI) {
        Value *val = &*AI;
        jl_value_t *jargty = jl_nth_slot_type((jl_value_t*)argt, i);
        // figure out how to unpack this type
        jl_cgval_t &inputarg = inputargs[i + 1];
        if (jl_is_abstract_ref_type(jargty)) {
            // a pointer to a value
            jargty = jl_tparam0(jargty);
            if (jargty == (jl_value_t*)jl_any_type) {
                inputarg = mark_julia_type(ctx,
                        ctx.builder.CreateLoad(emit_bitcast(ctx, val, T_pprjlvalue)),
                        true, jargty);
            }
            else if (!jl_isbits(jargty)) {
                // must be a jl_value_t* (because it's mutable or contains gc roots)
                inputarg = mark_julia_type(ctx, maybe_decay_untracked(emit_bitcast(ctx, val, T_prjlvalue)), true, jargty);
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
                    val = emit_bitcast(ctx, val, T->getPointerTo());
                    val = ctx.builder.CreateAlignedLoad(val, 1); // make no alignment assumption about pointer from C
                    inputarg = mark_julia_type(ctx, val, false, jargty);
                }
            }
        }
        else {
            bool argboxed;
            (void)julia_struct_to_llvm(jargty, NULL, &argboxed);
            if (argboxed) {
                // a jl_value_t*, even when represented as a struct
                inputarg = mark_julia_type(ctx, val, true, jargty);
            }
            else {
                // something of type T
                // undo whatever we might have done to this poor argument
                if (sig.byRefList.at(i)) {
                    assert(cast<PointerType>(val->getType())->getElementType() == sig.fargt[i]);
                    val = ctx.builder.CreateAlignedLoad(val, 1); // unknown alignment from C
                }
                else {
                    bool issigned = jl_signed_type && jl_subtype(jargty, (jl_value_t*)jl_signed_type);
                    val = llvm_type_rewrite(ctx, val, sig.fargt[i], issigned);
                }
                bool isboxed;
                (void)julia_type_to_llvm(jargty, &isboxed);
                if (isboxed) {
                    // passed an unboxed T, but want something boxed
                    Value *mem = emit_allocobj(ctx, jl_datatype_size(jargty),
                                               literal_pointer_val(ctx, (jl_value_t*)jargty));
                    tbaa_decorate(jl_is_mutable(jargty) ? tbaa_mutab : tbaa_immut,
                                  ctx.builder.CreateAlignedStore(val,
                                                             emit_bitcast(ctx, mem, val->getType()->getPointerTo()),
                                                             16)); // julia's gc gives 16-byte aligned addresses
                    inputarg = mark_julia_type(ctx, mem, true, jargty);
                }
                else {
                    // mark that this is an unboxed T
                    inputarg = mark_julia_type(ctx, val, false, jargty);
                }
            }
        }
    }
    assert(AI == cw->arg_end());

    // Create the call
    bool jlfunc_sret;
    jl_cgval_t retval;
    if (lam && lam->jlcall_api == 2) {
        nargs = 0; // arguments not needed -- TODO: not really true, should emit an age_ok test and jlcall
        jlfunc_sret = false;
        retval = mark_julia_const(lam->inferred_const);
    }
    else if (lam && lam->functionObjectsDecls.specFunctionObject != NULL) {
        // emit a specsig call
        const char *protoname = lam->functionObjectsDecls.specFunctionObject;
        jl_returninfo_t returninfo = get_specsig_function(M, protoname, lam->specTypes, lam->rettype);
        FunctionType *cft = returninfo.decl->getFunctionType();
        jlfunc_sret = (returninfo.cc == jl_returninfo_t::SRet);

        std::vector<Value*> args;
        Value *result;
        if (jlfunc_sret || returninfo.cc == jl_returninfo_t::Union) {
            // fuse the two sret together, or emit an alloca to hold it
            if (sig.sret && jlfunc_sret)
                result = emit_bitcast(ctx, sretPtr, cft->getParamType(0));
            else
                result = decay_derived(emit_static_alloca(ctx, cft->getParamType(0)->getContainedType(0)));
            args.push_back(result);
        }
        for (size_t i = 0; i < nargs + 1; i++) {
            // figure out how to repack the arguments
            const jl_cgval_t &inputarg = inputargs[i];
            Value *arg;
            jl_value_t *spect = jl_nth_slot_type(lam->specTypes, i);
            bool isboxed;
            Type *T = julia_type_to_llvm(spect, &isboxed);
            if (isboxed) {
                arg = boxed(ctx, inputarg);
            }
            else if (type_is_ghost(T)) {
                continue; // ghost types are skipped by the specsig method signature
            }
            else if (T->isAggregateType()) {
                // aggregate types are passed by pointer
                arg = data_pointer(ctx, inputarg, T->getPointerTo());
            }
            else {
                arg = emit_unbox(ctx, T, inputarg, spect);
                assert(!isa<UndefValue>(arg));
            }

            // add to argument list
            args.push_back(arg);
        }
        Value *theFptr = returninfo.decl;
        assert(theFptr);
        if (age_ok) {
            funcName << "_gfthunk";
            Function *gf_thunk = Function::Create(returninfo.decl->getFunctionType(),
                    GlobalVariable::InternalLinkage, funcName.str(), M);
            jl_init_function(gf_thunk);
            gf_thunk->setAttributes(returninfo.decl->getAttributes());
            gf_thunk->addFnAttr("no-frame-pointer-elim", "true");
            // build a  specsig -> jl_apply_generic converter thunk
            // this builds a method that calls jl_apply_generic (as a closure over a singleton function pointer),
            // but which has the signature of a specsig
            emit_cfunc_invalidate(gf_thunk, returninfo.cc, lam, nargs + 1, world);
            theFptr = ctx.builder.CreateSelect(age_ok, theFptr, gf_thunk);
        }
        CallInst *call = ctx.builder.CreateCall(theFptr, ArrayRef<Value*>(args));
        call->setAttributes(returninfo.decl->getAttributes());
        switch (returninfo.cc) {
            case jl_returninfo_t::Boxed:
                retval = mark_julia_type(ctx, call, true, astrt);
                break;
            case jl_returninfo_t::Register:
                retval = mark_julia_type(ctx, call, false, astrt);
                break;
            case jl_returninfo_t::SRet:
                retval = mark_julia_slot(result, astrt, NULL, tbaa_stack);
                break;
            case jl_returninfo_t::Union:
                retval = mark_julia_slot(ctx.builder.CreateExtractValue(call, 0),
                                         astrt,
                                         ctx.builder.CreateExtractValue(call, 1),
                                         tbaa_stack);
                // note that the value may not be rooted here (on the return path)
                break;
            case jl_returninfo_t::Ghosts:
                retval = mark_julia_slot(NULL, astrt, call, tbaa_stack);
                break;
        }
    }
    else {
        // emit a jlcall
        jlfunc_sret = false;
        Function *theFptr = NULL;
        if (lam) {
            const char *fname = lam->functionObjectsDecls.functionObject;
            if (fname) {
                theFptr = cast_or_null<Function>(jl_Module->getNamedValue(fname));
                if (!theFptr) {
                    theFptr = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage,
                                               fname, jl_Module);
                }
                else {
                    assert(theFptr->getFunctionType() == jl_func_sig);
                }
            }
        }
        BasicBlock *b_generic, *b_jlcall, *b_after;
        Value *ret_jlcall;
        if (age_ok) {
            assert(theFptr);
            b_generic = BasicBlock::Create(jl_LLVMContext, "generic", cw);
            b_jlcall = BasicBlock::Create(jl_LLVMContext, "apply", cw);
            b_after = BasicBlock::Create(jl_LLVMContext, "after", cw);
            ctx.builder.CreateCondBr(age_ok, b_jlcall, b_generic);
            ctx.builder.SetInsertPoint(b_jlcall);
            // for jlcall, we need to pass the function object even if it is a ghost.
            Value *theF = boxed(ctx, inputargs[0]);
            assert(theF);
            ret_jlcall = emit_jlcall(ctx, theFptr, theF, &inputargs[1], nargs);
            ctx.builder.CreateBr(b_after);
            ctx.builder.SetInsertPoint(b_generic);
        }
        Value *ret = emit_jlcall(ctx, prepare_call(jlapplygeneric_func), NULL, inputargs, nargs + 1);
        if (age_ok) {
            ctx.builder.CreateBr(b_after);
            ctx.builder.SetInsertPoint(b_after);
            PHINode *retphi = ctx.builder.CreatePHI(T_prjlvalue, 2);
            retphi->addIncoming(ret_jlcall, b_jlcall);
            retphi->addIncoming(ret, b_generic);
            ret = retphi;
        }
        retval = mark_julia_type(ctx, ret, true, astrt);
    }

    // inline a call to typeassert here
    emit_typecheck(ctx, retval, declrt, "cfunction");

    // Prepare the return value
    Value *r;
    if (toboxed) {
        assert(!sig.sret);
        // return a jl_value_t*
        r = boxed(ctx, retval, false); // no gcroot since this is on the return path
    }
    else if (sig.sret && jlfunc_sret) {
        // nothing to do
        r = NULL;
    }
    else if (!type_is_ghost(sig.lrt)) {
        Type *prt = sig.prt;
        if (sig.sret)
            prt = sig.fargt_sig[0]->getContainedType(0); // sret is a PointerType
        bool issigned = jl_signed_type && jl_subtype(declrt, (jl_value_t*)jl_signed_type);
        Value *v = julia_to_native(ctx, sig.lrt, toboxed, declrt, NULL, retval,
                                   false, 0, NULL);
        r = llvm_type_rewrite(ctx, v, prt, issigned);
        if (sig.sret) {
            ctx.builder.CreateStore(r, sretPtr);
            r = NULL;
        }
    }
    else {
        assert(type_is_ghost(sig.lrt));
        sig.sret = true;
        r = NULL;
    }

    ctx.builder.CreateStore(last_age, ctx.world_age_field);
    ctx.builder.CreateRet(r);

    ctx.builder.SetCurrentDebugLocation(noDbg);
    ctx.builder.ClearInsertionPoint();

    jl_finalize_module(M, true);

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
    // Assumes the codegen lock is acquired. The caller is responsible for that.

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
    jl_typemap_entry_t *sf = NULL;
    if (jl_cfunction_list.unknown != jl_nothing) {
        sf = jl_typemap_assoc_by_type(jl_cfunction_list, (jl_tupletype_t*)cfunc_sig, NULL, 1, /*subtype*/0, /*offs*/0, /*world*/1);
        if (sf != NULL) {
            jl_value_t *v = sf->func.value;
            if (v != NULL) {
                if (jl_is_svec(v))
                    v = jl_svecref(v, 0);
                Function *f = (Function*)jl_unbox_voidpointer(v);
                JL_GC_POP();
                return f;
            }
        }
    }
    if (sf == NULL) {
        sf = jl_typemap_insert(&jl_cfunction_list, (jl_value_t*)jl_cfunction_list.unknown, (jl_tupletype_t*)cfunc_sig,
            NULL, jl_emptysvec, NULL, /*offs*/0, &cfunction_cache, 1, ~(size_t)0, NULL);
    }

    // Backup the info for the nested compile
    bool last_n_c = nested_compile;
    nested_compile = true;
    Function *f;
    JL_TRY {
        f = gen_cfun_wrapper(ff, crt, (jl_tupletype_t*)argt, sf, declrt, (jl_tupletype_t*)sigt);
    }
    JL_CATCH {
        f = NULL;
    }
    // Restore the previous compile context
    nested_compile = last_n_c;
    JL_GC_POP();
    if (f == NULL)
        jl_rethrow();
    return f;
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_method_instance_t *lam, const jl_returninfo_t &f, const std::string &funcName, Module *M)
{
    Function *w = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage,
                                   funcName, M);
    jl_init_function(w);
    w->addFnAttr("no-frame-pointer-elim", "true");
    Function::arg_iterator AI = w->arg_begin();
    Value *fArg = &*AI++;
    Value *argArray = &*AI++;
    /* const Argument &argCount = *AI++; */

    jl_codectx_t ctx(jl_LLVMContext);
    ctx.f = w;
    ctx.linfo = lam;
    ctx.world = 0;
    ctx.params = &jl_default_cgparams;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    FunctionType *ftype = f.decl->getFunctionType();
    size_t nargs = lam->def.method->nargs;
    size_t nfargs = ftype->getNumParams();
    Value **args = (Value**) alloca(nfargs*sizeof(Value*));
    unsigned idx = 0;
    AllocaInst *result;
    switch (f.cc) {
    case jl_returninfo_t::Boxed:
    case jl_returninfo_t::Register:
    case jl_returninfo_t::Ghosts:
        break;
    case jl_returninfo_t::SRet:
        result = ctx.builder.CreateAlloca(ftype->getParamType(0)->getContainedType(0));
        args[idx] = decay_derived(result);
        idx++;
        break;
    case jl_returninfo_t::Union:
        result = ctx.builder.CreateAlloca(ArrayType::get(T_int8, f.union_bytes));
        if (f.union_align > 1)
            result->setAlignment(f.union_align);
        args[idx] = result;
        idx++;
        break;
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
            Value *argPtr = ctx.builder.CreateGEP(argArray, ConstantInt::get(T_size, i-1));
            theArg = ctx.builder.CreateLoad(argPtr);
        }
        if (lty != NULL && !isboxed) {
            theArg = decay_derived(emit_bitcast(ctx, theArg, PointerType::get(lty, 0)));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = ctx.builder.CreateAlignedLoad(theArg, julia_alignment(theArg, ty, 0));
        }
        assert(dyn_cast<UndefValue>(theArg) == NULL);
        args[idx] = theArg;
        idx++;
    }
    CallInst *call = ctx.builder.CreateCall(f.decl, ArrayRef<Value*>(&args[0], nfargs));
    call->setAttributes(f.decl->getAttributes());

    jl_value_t *jlretty = lam->rettype;
    jl_cgval_t retval;
    switch (f.cc) {
    case jl_returninfo_t::Boxed:
        retval = mark_julia_type(ctx, call, true, jlretty, /*needsroot*/false);
        break;
    case jl_returninfo_t::Register:
        retval = mark_julia_type(ctx, call, false, jlretty, /*needsroot*/false);
        break;
    case jl_returninfo_t::SRet:
        retval = mark_julia_slot(result, jlretty, NULL, tbaa_stack);
        break;
    case jl_returninfo_t::Union:
        // result is technically not right here, but we only need to look at it
        // for the unboxed values, so it's ok.
        retval = mark_julia_slot(result,
                                 jlretty,
                                 ctx.builder.CreateExtractValue(call, 1),
                                 tbaa_stack);
        retval.gcroot = emit_local_root(ctx);
        ctx.builder.CreateStore(ctx.builder.CreateExtractValue(call, 0), retval.gcroot);
        break;
    case jl_returninfo_t::Ghosts:
        retval = mark_julia_slot(NULL, jlretty, call, tbaa_stack);
        break;
    }
    ctx.builder.CreateRet(boxed(ctx, retval, false)); // no gcroot needed since this on the return path
    assert(!ctx.roots);
    return w;
}

static bool uses_specsig(jl_value_t *sig, jl_value_t *rettype, bool needsparam, bool va, jl_code_info_t *src)
{
    if (va || needsparam)
        return false;
    if (!src || !jl_ast_flag_inferred((jl_array_t*)src))
        return false;
    if (sig == (jl_value_t*)jl_anytuple_type)
        return false;
    if (!jl_is_datatype(sig))
        return false;
    if (jl_nparams(sig) == 0)
        return false;
    // not invalid, consider if specialized signature is worthwhile
    if (isbits_spec(rettype, false))
        return true;
    if (jl_is_uniontype(rettype)) {
        bool allunbox;
        size_t nbytes, align, min_align;
        union_alloca_type((jl_uniontype_t*)rettype, allunbox, nbytes, align, min_align);
        if (nbytes > 0)
            return true; // some elements of the union could be returned unboxed avoiding allocation
    }
    for (size_t i = 0; i < jl_nparams(sig); i++) {
        if (isbits_spec(jl_tparam(sig, i), false)) { // assumes !va
            return true;
        }
    }
    return false; // jlcall sig won't require any box allocations
}

static jl_returninfo_t get_specsig_function(Module *M, const std::string &name, jl_value_t *sig, jl_value_t *jlrettype)
{
    jl_returninfo_t props = {};
    SmallVector<Type*, 8> fsig;
    Type *rt;
    if (jlrettype == (jl_value_t*)jl_void_type) {
        rt = T_void;
        props.cc = jl_returninfo_t::Register;
    }
    else if (jl_is_uniontype(jlrettype)) {
        bool allunbox;
        union_alloca_type((jl_uniontype_t*)jlrettype, allunbox, props.union_bytes, props.union_align, props.union_minalign);
        if (props.union_bytes) {
            props.cc = jl_returninfo_t::Union;
            Type *AT = ArrayType::get(T_int8, props.union_bytes);
            fsig.push_back(AT->getPointerTo());
            Type *pair[] = { T_prjlvalue, T_int8 };
            rt = StructType::get(jl_LLVMContext, makeArrayRef(pair));
        }
        else if (allunbox) {
            props.cc = jl_returninfo_t::Ghosts;
            rt = T_int8;
        }
        else {
            rt = T_prjlvalue;
        }
    }
    else {
        bool retboxed;
        rt = julia_type_to_llvm(jlrettype, &retboxed);
        if (!retboxed) {
            if (rt != T_void && deserves_sret(jlrettype, rt)) {
                props.cc = jl_returninfo_t::SRet;
                fsig.push_back(rt->getPointerTo(AddressSpace::Derived));
                rt = T_void;
            }
            else {
                props.cc = jl_returninfo_t::Register;
            }
        } else {
            rt = T_prjlvalue;
        }
    }
#if JL_LLVM_VERSION >= 50000
    AttributeList attributes; // function declaration attributes
#else
    AttributeSet attributes; // function declaration attributes
#endif
    if (props.cc == jl_returninfo_t::SRet) {
        attributes = attributes.addAttribute(jl_LLVMContext, 1, Attribute::StructRet);
        attributes = attributes.addAttribute(jl_LLVMContext, 1, Attribute::NoAlias);
        attributes = attributes.addAttribute(jl_LLVMContext, 1, Attribute::NoCapture);
    }
    if (props.cc == jl_returninfo_t::Union) {
        attributes = attributes.addAttribute(jl_LLVMContext, 1, Attribute::NoAlias);
        attributes = attributes.addAttribute(jl_LLVMContext, 1, Attribute::NoCapture);
    }
    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        bool isboxed;
        Type *ty = julia_type_to_llvm(jt, &isboxed);
        if (type_is_ghost(ty))
            continue;
        if (ty->isAggregateType()) { // aggregate types are passed by pointer
            attributes = attributes.addAttribute(jl_LLVMContext, fsig.size() + 1, Attribute::NoCapture);
            attributes = attributes.addAttribute(jl_LLVMContext, fsig.size() + 1, Attribute::ReadOnly);
            ty = PointerType::get(ty, AddressSpace::Derived);
        }
        if (isboxed)
            ty = PointerType::get(cast<PointerType>(ty)->getElementType(), AddressSpace::Tracked);
        fsig.push_back(ty);
    }
    FunctionType *ftype = FunctionType::get(rt, fsig, false);
    Function *f = M ? cast_or_null<Function>(M->getNamedValue(name)) : NULL;
    if (f == NULL) {
        f = Function::Create(ftype, GlobalVariable::ExternalLinkage, name, M);
        f->setAttributes(attributes);
    }
    else {
        assert(f->getFunctionType() == ftype);
    }
    props.decl = f;
    return props;
}

static DISubroutineType *
get_specsig_di(jl_value_t *rt, jl_value_t *sig, DIFile *topfile, DIBuilder &dbuilder)
{
    std::vector<Metadata*> ditypes(0);
    Type *ty = julia_type_to_llvm(rt);
    if (type_is_ghost(ty))
        ditypes.push_back(nullptr);
    else
        ditypes.push_back(julia_type_to_di(rt, &dbuilder, false));
    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        Type *ty = julia_type_to_llvm(jt);
        if (type_is_ghost(ty))
            continue;
        ditypes.push_back(julia_type_to_di(jt, &dbuilder, false));
    }
    return dbuilder.createSubroutineType(dbuilder.getOrCreateTypeArray(ditypes));
}


// Compile to LLVM IR, using a specialized signature if applicable.
static std::unique_ptr<Module> emit_function(
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        size_t world,
        jl_llvm_functions_t *declarations,
        const jl_cgparams_t *params)
{
    assert(declarations && "Capturing declarations is always required");

    // step 1. unpack AST and allocate codegen context for this function
    jl_codectx_t ctx(jl_LLVMContext);
    JL_GC_PUSH2(&ctx.code, &ctx.roots);
    ctx.code = (jl_array_t*)src->code;

    //jl_static_show(JL_STDOUT, (jl_value_t*)ast);
    //jl_printf(JL_STDOUT, "\n");
    std::map<int, jl_arrayvar_t> arrayvars;
    std::map<int, BasicBlock*> labels;
    ctx.arrayvars = &arrayvars;
    ctx.module = jl_is_method(lam->def.method) ? lam->def.method->module : lam->def.module;
    ctx.linfo = lam;
    ctx.source = src;
    ctx.world = world;
    ctx.name = jl_symbol_name(jl_is_method(lam->def.method) ? lam->def.method->name : anonymous_sym);
    ctx.funcName = ctx.name;
    ctx.vaStack = false;
    ctx.params = params;
    ctx.spvals_ptr = NULL;
    ctx.nargs = jl_is_method(lam->def.method) ? lam->def.method->nargs : 0;
    bool toplevel = !jl_is_method(lam->def.method);

    // step 1b. unpack debug information
    int coverage_mode = jl_options.code_coverage;
    int malloc_log_mode = jl_options.malloc_log;
    StringRef filename = "<missing>";
    StringRef dbgFuncName = ctx.name;
    int toplineno = -1;
    if (jl_is_method(lam->def.method)) {
        toplineno = lam->def.method->line;
        if (lam->def.method->file != empty_sym)
            filename = jl_symbol_name(lam->def.method->file);
    }
    ctx.file = filename;
    // jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           jl_symbol_name(ctx.name), filename.str().c_str(), toplineno);

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

    // step 2. process var-info lists to see what vars need boxing
    int n_ssavalues = jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_len(src->ssavaluetypes);
    size_t vinfoslen = jl_array_dim0(src->slotnames);
    ctx.slots.resize(vinfoslen);
    size_t nreq = ctx.nargs;
    int va = 0;

    assert(lam->specTypes); // the specTypes field should always be assigned

    if (nreq > 0 && lam->def.method->isva) {
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
    for (i = 0; i < nreq; i++) {
        jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        if (argname == unused_sym)
            continue;
        jl_varinfo_t &varinfo = ctx.slots[i];
        varinfo.isArgument = true;
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, ty);
    }
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &varinfo = ctx.slots[ctx.vaSlot];
        varinfo.isArgument = true;
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, jl_tuple_type);
    }

    for (i = 0; i < vinfoslen; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        uint8_t flags = jl_array_uint8_ref(src->slotflags, i);
        varinfo.isSA = (jl_vinfo_sa(flags) != 0);
        varinfo.usedUndef = (jl_vinfo_usedundef(flags) != 0) || (!varinfo.isArgument && !src->inferred);
        if (!varinfo.isArgument) {
            jl_value_t *typ = jl_is_array(src->slottypes) ? jl_array_ptr_ref(src->slottypes, i) : (jl_value_t*)jl_any_type;
            if (!jl_is_type(typ))
                typ = (jl_value_t*)jl_any_type;
            varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, typ);
        }
    }

    jl_array_t *stmts = ctx.code;
    size_t stmtslen = jl_array_dim0(stmts);

    // finish recording variable use info
    for (i = 0; i < stmtslen; i++)
        simple_use_analysis(ctx, jl_array_ptr_ref(stmts, i));

    // determine which vars need to be volatile
    mark_volatile_vars(stmts, ctx.slots);

    // step 4. determine function signature
    bool needsparams = jl_is_method(lam->def.method)
        ? jl_svec_len(lam->def.method->sparam_syms) != jl_svec_len(lam->sparam_vals)
        : false;
    for (i = 0; !needsparams && i < jl_svec_len(lam->sparam_vals); i++) {
        jl_value_t *e = jl_svecref(lam->sparam_vals, i);
        if (jl_is_typevar(e))
            needsparams = true;
    }

    jl_value_t *jlrettype = lam->rettype;
    bool specsig = uses_specsig(lam->specTypes, jlrettype, needsparams, va, src);
    if (!specsig)
        ctx.nReqArgs--;  // function not part of argArray in jlcall

    std::stringstream funcName;
    // try to avoid conflicts in the global symbol table
    if (specsig)
        funcName << "jlcall_";
    else if (needsparams)
        funcName << "japi3_";
    else
        funcName << "japi1_";
    const char* unadorned_name = ctx.name;
#if defined(_OS_LINUX_)
    if (unadorned_name[0] == '@')
        unadorned_name++;
#endif
    funcName << unadorned_name << "_" << globalUnique++;

    // allocate Function declarations and wrapper objects
    Module *M = new Module(ctx.name, jl_LLVMContext);
    jl_setup_module(M);
    jl_returninfo_t returninfo = {};
    Function *f = NULL;
    Function *fwrap = NULL;
    if (specsig) { // assumes !va and !needsparams
        std::stringstream specName;
        specName << "julia_" << unadorned_name << "_" << globalUnique;
        returninfo = get_specsig_function(M, specName.str(), lam->specTypes, jlrettype);
        f = returninfo.decl;
        ctx.has_sret = (returninfo.cc == jl_returninfo_t::SRet || returninfo.cc == jl_returninfo_t::Union);
        jl_init_function(f);

        fwrap = gen_jlcall_wrapper(lam, returninfo, funcName.str(), M);
        declarations->functionObject = strdup(fwrap->getName().str().c_str());
        declarations->specFunctionObject = strdup(f->getName().str().c_str());
    }
    else {
        f = Function::Create(needsparams ? jl_func_sig_sparams : jl_func_sig,
                             GlobalVariable::ExternalLinkage,
                             funcName.str(), M);
        returninfo.decl = f;
        jl_init_function(f);
        declarations->functionObject = strdup(f->getName().str().c_str());
        declarations->specFunctionObject = NULL;
    }

    f->addFnAttr("no-frame-pointer-elim", "true");
    if (jlrettype == (jl_value_t*)jl_bottom_type)
        f->setDoesNotReturn();
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to realign the stack to the next 16-byte boundary
    // upon entry to any function. This achieves compatibility
    // with both MinGW-GCC (which assumes an 16-byte-aligned stack) and
    // i686 Windows (which uses a 4-byte-aligned stack)
    AttrBuilder *attr = new AttrBuilder();
    attr->addStackAlignmentAttr(16);
#if JL_LLVM_VERSION >= 50000
    f->addAttributes(AttributeList::FunctionIndex,
        AttributeList::get(f->getContext(),
            AttributeList::FunctionIndex, *attr));
#else
    f->addAttributes(AttributeSet::FunctionIndex,
        AttributeSet::get(f->getContext(),
            AttributeSet::FunctionIndex, *attr));
#endif
#endif
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    f->setHasUWTable(); // force NeedsWinEH
#endif

#ifdef USE_POLLY
    if (!jl_has_meta(stmts, polly_sym) || jl_options.polly == JL_OPTIONS_POLLY_OFF) {
        f->addFnAttr(polly::PollySkipFnAttr);
    }
#endif

#ifdef JL_DEBUG_BUILD
    f->addFnAttr(Attribute::StackProtectStrong);
#endif
    ctx.f = f;

    // Step 4b. determine debug info signature and other type info for locals
    DIBuilder dbuilder(*M);
    DIFile *topfile = NULL;
    DISubprogram *SP = NULL;
    DebugLoc noDbg, topdebugloc;
    if (ctx.debug_enabled) {
        // TODO: Fix when moving to new LLVM version
        topfile = dbuilder.createFile(filename, ".");
#if JL_LLVM_VERSION >= 40000
        DICompileUnit *CU = dbuilder.createCompileUnit(0x01, topfile, "julia", true, "", 0);
#else
        DICompileUnit *CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
#endif

        DISubroutineType *subrty;
        if (jl_options.debug_level <= 1) {
            subrty = jl_di_func_null_sig;
        }
        else if (!specsig) {
            subrty = jl_di_func_sig;
        }
        else {
            subrty = get_specsig_di(lam->rettype, lam->specTypes, topfile, dbuilder);
        }
        SP = dbuilder.createFunction(CU,
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
                                     nullptr);         // Template Parameters
        topdebugloc = DebugLoc::get(toplineno, 0, SP, NULL);
        f->setSubprogram(SP);
        if (jl_options.debug_level >= 2) {
            const bool AlwaysPreserve = true;
            // Go over all arguments and local variables and initialize their debug information
            for (i = 0; i < nreq; i++) {
                jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
                if (argname == unused_sym)
                    continue;
                jl_varinfo_t &varinfo = ctx.slots[i];
                varinfo.dinfo = dbuilder.createParameterVariable(
                    SP,                                 // Scope (current function will be fill in later)
                    jl_symbol_name(argname),            // Variable name
                    ctx.has_sret + i + 1,               // Argument number (1-based)
                    topfile,                            // File
                    toplineno == -1 ? 0 : toplineno,    // Line
                    // Variable type
                    julia_type_to_di(varinfo.value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DIFlagZero);                        // Flags (TODO: Do we need any)
            }
            if (va && ctx.vaSlot != -1) {
                ctx.slots[ctx.vaSlot].dinfo = dbuilder.createParameterVariable(
                    SP,                                 // Scope (current function will be fill in later)
                    std::string(jl_symbol_name(slot_symbol(ctx, ctx.vaSlot))) + "...",  // Variable name
                    ctx.has_sret + nreq + 1,            // Argument number (1-based)
                    topfile,                            // File
                    toplineno == -1 ? 0 : toplineno,    // Line (for now, use lineno of the function)
                    julia_type_to_di(ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DIFlagZero);                        // Flags (TODO: Do we need any)
            }
            for (i = 0; i < vinfoslen; i++) {
                jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
                jl_varinfo_t &varinfo = ctx.slots[i];
                if (varinfo.isArgument || s == compiler_temp_sym || s == unused_sym)
                    continue;
                // LLVM 4.0: Assume the variable has default alignment
                varinfo.dinfo = dbuilder.createAutoVariable(
                    SP,                     // Scope (current function will be fill in later)
                    jl_symbol_name(s),       // Variable name
                    topfile,                 // File
                    toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                    julia_type_to_di(varinfo.value.typ, &dbuilder, false), // Variable type
                    AlwaysPreserve,          // May be deleted if optimized out
                    DIFlagZero               // Flags (TODO: Do we need any)
                    );
            }
        }
    }

    // step 5. create first basic block
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    ctx.builder.SetInsertPoint(b0);
    ctx.builder.SetCurrentDebugLocation(noDbg);

    // spill arguments into stack slots
    // so it is more likely to be possible to find them when debugging
    Value *fArg=NULL, *argArray=NULL, *pargArray=NULL, *argCount=NULL;
    if (!specsig) {
        Function::arg_iterator AI = f->arg_begin();
        if (needsparams) {
            ctx.spvals_ptr = &*AI;
            ++AI;
        }
        fArg = &*AI++;
        argArray = &*AI++;
        pargArray = ctx.builder.CreateAlloca(argArray->getType());
        ctx.builder.CreateStore(argArray, pargArray, true/*volatile store to prevent removal of this alloca*/);
        argCount = &*AI++;
        ctx.argArray = argArray;
        ctx.argCount = argCount;
    }

    /*
    // step 6. (optional) check for stack overflow (the slower way)
    Value *cur_sp =
        ctx.builder.CreateCall(Intrinsic::getDeclaration(M,
                                                     Intrinsic::frameaddress),
                           ConstantInt::get(T_int32, 0));
    Value *sp_ok =
        ctx.builder.CreateICmpUGT(cur_sp,
                              ConstantInt::get(T_size,
                                               (uptrint_t)jl_stack_lo));
    error_unless(ctx, sp_ok, "stack overflow");
    */

    // step 7. set up GC frame
    allocate_gc_frame(ctx, b0);
    Value *last_age = NULL;
    if (toplevel) {
        emit_last_age_field(ctx);
        last_age = tbaa_decorate(tbaa_gcframe, ctx.builder.CreateLoad(ctx.world_age_field));
    }

    // step 8. allocate local variables slots
    // must be in the first basic block for the llvm mem2reg pass to work

    // get pointers for locals stored in the gc frame array (argTemp)
    for (i = 0; i < vinfoslen; i++) {
        jl_sym_t *s = slot_symbol(ctx, i);
        if (s == unused_sym)
            continue;
        jl_varinfo_t &varinfo = ctx.slots[i];
        if (!varinfo.used) {
            varinfo.usedUndef = false;
            continue;
        }
        jl_value_t *jt = varinfo.value.typ;
        assert(!varinfo.boxroot); // variables shouldn't have memory locs already
        if (varinfo.value.constant) {
            // no need to explicitly load/store a constant/ghost value
            alloc_def_flag(ctx, varinfo);
            continue;
        }
        else if (varinfo.isArgument) {
            // if we can unbox it, just use the input pointer
            if (i != (size_t)ctx.vaSlot && isbits_spec(jt, false))
                continue;
        }
        else if (jl_is_uniontype(jt)) {
            bool allunbox;
            size_t align;
            Value *lv = try_emit_union_alloca(ctx, (jl_uniontype_t*)jt, allunbox, align);
            if (lv) {
                lv->setName(jl_symbol_name(s));
                varinfo.value = mark_julia_slot(lv, jt, NULL, tbaa_stack);
                varinfo.pTIndex = emit_static_alloca(ctx, T_int8);
                // the slot is not immutable if there are multiple assignments
                varinfo.value.isimmutable &= varinfo.isSA;
            }
            else if (allunbox) {
                // all ghost values just need a selector allocated
                AllocaInst *lv = emit_static_alloca(ctx, T_int8);
                lv->setName(jl_symbol_name(s));
                varinfo.pTIndex = lv;
                varinfo.value.tbaa = NULL;
                varinfo.value.isboxed = false;
                varinfo.value.isimmutable = true;
            }
            if (lv || allunbox)
                alloc_def_flag(ctx, varinfo);
            if (allunbox)
                continue;
        }
        else if (isbits_spec(jt, false)) {
            bool isboxed;
            Type *vtype = julia_type_to_llvm(jt, &isboxed);
            assert(!isboxed);
            assert(!type_is_ghost(vtype) && "constants should already be handled");
            // CreateAlloca is OK during prologue setup
            Value *lv = ctx.builder.CreateAlloca(vtype, NULL, jl_symbol_name(s));
            varinfo.value = mark_julia_slot(lv, jt, NULL, tbaa_stack);
            // slot is not immutable if there are multiple assignments
            varinfo.value.isimmutable &= varinfo.isSA;
            alloc_def_flag(ctx, varinfo);
            if (ctx.debug_enabled && varinfo.dinfo) {
                assert((Metadata*)varinfo.dinfo->getType() != jl_pvalue_dillvmt);
                dbuilder.insertDeclare(lv, varinfo.dinfo, dbuilder.createExpression(),
                                       topdebugloc,
                                       ctx.builder.GetInsertBlock());
            }
            continue;
        }
        if (!varinfo.isArgument || // always need a slot if the variable is assigned
            specsig || // for arguments, give them stack slots if they aren't in `argArray` (otherwise, will use that pointer)
            (va && (int)i == ctx.vaSlot) || // or it's the va arg tuple
            i == 0) { // or it is the first argument (which isn't in `argArray`)
#if JL_LLVM_VERSION >= 50000
            AllocaInst *av = new AllocaInst(T_prjlvalue, 0,
#else
            AllocaInst *av = new AllocaInst(T_prjlvalue,
#endif
                jl_symbol_name(s), /*InsertBefore*/ctx.ptlsStates);
            StoreInst *SI = new StoreInst(
                ConstantPointerNull::get(cast<PointerType>(T_prjlvalue)), av,
                false);
            SI->insertAfter(ctx.ptlsStates);
            varinfo.boxroot = av;
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
                                            topdebugloc,
                                ctx.builder.GetInsertBlock());
            }
        }
        maybe_alloc_arrayvar(ctx, i);
    }

    // step 9. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    if (ctx.has_sret)
        AI++; // skip sret slot
    for (i = 0; i < nreq; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed;
        Type *llvmArgType = julia_type_to_llvm(argType, &isboxed);
        if (s == unused_sym) {
            if (specsig && !type_is_ghost(llvmArgType))
                ++AI;
            continue;
        }
        jl_varinfo_t &vi = ctx.slots[i];
        jl_cgval_t theArg;
        if (s == unused_sym || vi.value.constant) {
            assert(vi.boxroot == NULL);
            if (specsig && !type_is_ghost(llvmArgType))
                ++AI;
        }
        else {
            if (specsig) {
                if (type_is_ghost(llvmArgType)) { // this argument is not actually passed
                    theArg = ghostValue(argType);
                }
                else if (llvmArgType->isAggregateType()) {
                    Argument *Arg = &*AI++;
                    maybe_mark_argument_dereferenceable(Arg, argType);
                    theArg = mark_julia_slot(Arg, argType, NULL, tbaa_const); // this argument is by-pointer
                    theArg.isimmutable = true;
                }
                else {
                    Argument *Arg = &*AI++;
                    if (isboxed)
                        maybe_mark_argument_dereferenceable(Arg, argType);
                    theArg = mark_julia_type(ctx, Arg, isboxed, argType, /*needsgcroot*/false);
                }
            }
            else {
                if (i == 0) {
                    // first (function) arg is separate in jlcall
                    theArg = mark_julia_type(ctx, fArg, true, vi.value.typ, /*needsgcroot*/false);
                }
                else {
                    Value *argPtr = ctx.builder.CreateGEP(argArray, ConstantInt::get(T_size, i-1));
                    theArg = mark_julia_type(ctx, ctx.builder.CreateLoad(argPtr), true, vi.value.typ, /*needsgcroot*/false);
                    if (ctx.debug_enabled && vi.dinfo && !vi.boxroot && !vi.value.V) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus);
                        addr.push_back((i - 1) * sizeof(void*));
                        if ((Metadata*)vi.dinfo->getType() != jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        dbuilder.insertDeclare(pargArray, vi.dinfo, dbuilder.createExpression(addr),
                                        topdebugloc,
                                        ctx.builder.GetInsertBlock());
                    }
                }
            }

            if (vi.boxroot == NULL) {
                assert(vi.value.V == NULL && "unexpected variable slot created for argument");
                // keep track of original (possibly boxed) value to avoid re-boxing or moving
                vi.value = theArg;
                if (specsig && theArg.V && ctx.debug_enabled && vi.dinfo) {
                    SmallVector<uint64_t, 8> addr;
                    if ((Metadata*)vi.dinfo->getType() != jl_pvalue_dillvmt && theArg.ispointer())
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                    AllocaInst *parg = dyn_cast<AllocaInst>(theArg.V);
                    if (!parg) {
                        parg = ctx.builder.CreateAlloca(theArg.V->getType(), NULL, jl_symbol_name(s));
                        ctx.builder.CreateStore(theArg.V, parg);
                    }
                    dbuilder.insertDeclare(parg, vi.dinfo, dbuilder.createExpression(addr),
                                                topdebugloc,
                                                ctx.builder.GetInsertBlock());
                }
            }
            else {
                Value *argp = boxed(ctx, theArg, false); // skip the temporary gcroot since it would be folded to argp anyways
                ctx.builder.CreateStore(argp, vi.boxroot);
                if (!theArg.isboxed)
                    emit_local_root(ctx, &vi); // create a root for vi
            }
            // get arrayvar data if applicable
            if (arrayvars.find(i) != arrayvars.end()) {
                jl_arrayvar_t av = arrayvars[i];
                assign_arrayvar(ctx, av, theArg);
            }
        }
    }

    // step 10. allocate rest argument
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &vi = ctx.slots[ctx.vaSlot];
        if (vi.value.constant || !vi.used) {
            assert(vi.boxroot == NULL);
        }
        else {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs - nreq)
            CallInst *restTuple =
                ctx.builder.CreateCall(prepare_call(jltuple_func),
                        { maybe_decay_untracked(V_null),
                          ctx.builder.CreateGEP(argArray,
                                  ConstantInt::get(T_size, nreq - 1)),
                          ctx.builder.CreateSub(argCount,
                                  ConstantInt::get(T_int32, nreq - 1)) });
            restTuple->setAttributes(jltuple_func->getAttributes());
            ctx.builder.CreateStore(restTuple, vi.boxroot);
            emit_local_root(ctx, &vi); // create a root for vi
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
        DISubprogram *sp;
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
#ifndef JL_NDEBUG
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
                inlinedAt = DI_stack.back().loc;
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
                DIFile *new_file = nullptr;
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
                    inlinedAt = cur_prop.loc;
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
        if (!ctx.builder.GetInsertBlock()->getTerminator())
            ctx.builder.CreateUnreachable();
        if (workstack.empty()) {
            cursor = -1;
            linear_codegen = false;
            return;
        }
        auto &item = workstack.back();
        ctx.builder.SetInsertPoint(item.second);
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
        BasicBlock *cur_bb = ctx.builder.GetInsertBlock();
        // Check if we've already visited this label
        if (bb) {
            // Already in the work list
            // branch to it and pop one from the work list
            if (unconditional) {
                if (!cur_bb->getTerminator())
                    ctx.builder.CreateBr(bb);
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
                    ctx.builder.CreateBr(bb);
                ctx.builder.SetInsertPoint(bb);
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
        if (!JL_FEAT_TEST(ctx, code_coverage)) return false;
        return (coverage_mode == JL_LOG_ALL ||
                (coverage_mode == JL_LOG_USER && in_user_code));
    };
    auto do_malloc_log = [&] (bool in_user_code) {
        if (!JL_FEAT_TEST(ctx, track_allocations)) return false;
        return (malloc_log_mode == JL_LOG_ALL ||
                (malloc_log_mode == JL_LOG_USER && in_user_code));
    };

    // Handle the implicit first line number node.
    if (ctx.debug_enabled)
        ctx.builder.SetCurrentDebugLocation(topdebugloc);
    if (coverage_mode != JL_LOG_NONE && do_coverage(in_user_mod(ctx.module)))
        coverageVisitLine(ctx, filename, toplineno);
    while (cursor != -1) {
        auto &props = stmtprops[cursor];
        if ((props.loc_changed || !linear_codegen) && ctx.debug_enabled)
            ctx.builder.SetCurrentDebugLocation(props.loc);
        // Disable coverage for pop_loc, it doesn't start a new expression
        if (props.loc_changed && do_coverage(props.in_user_code) &&
            !props.is_poploc) {
            coverageVisitLine(ctx, props.file, props.line);
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
            // this is basically a copy of emit_assignment,
            // but where the assignment slot is the retval
            jl_cgval_t retvalinfo = emit_expr(ctx, jl_exprarg(expr, 0));
            retvalinfo = convert_julia_type(ctx, retvalinfo, jlrettype, /*needs-root*/false);
            if (retvalinfo.typ == jl_bottom_type) {
                ctx.builder.CreateUnreachable();
                find_next_stmt(-1);
                continue;
            }

            Value *isboxed_union = NULL;
            Value *retval;
            Value *sret = ctx.has_sret ? &*f->arg_begin() : NULL;
            Type *retty = f->getReturnType();
            switch (returninfo.cc) {
            case jl_returninfo_t::Boxed:
                retval = boxed(ctx, retvalinfo, false); // skip the gcroot on the return path
                break;
            case jl_returninfo_t::Register:
                if (type_is_ghost(retty))
                    retval = NULL;
                else
                    retval = emit_unbox(ctx, retty, retvalinfo, jlrettype);
                break;
            case jl_returninfo_t::SRet:
                retval = NULL;
                break;
            case jl_returninfo_t::Union: {
                Value *data, *tindex;
                if (retvalinfo.TIndex) {
                    tindex = retvalinfo.TIndex;
                    if (retvalinfo.V == NULL) {
                        // treat this as a simple Ghosts
                        data = maybe_decay_untracked(V_null);
                        sret = NULL;
                    }
                    else {
                        data = maybe_decay_untracked(V_null);
                        if (retvalinfo.ispointer() && !isa<AllocaInst>(retvalinfo.V)) {
                            // also need to account for the possibility the return object is boxed
                            // and avoid / skip copying it to the stack
                            isboxed_union = ctx.builder.CreateICmpNE(
                                    ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x80)),
                                    ConstantInt::get(T_int8, 0));
                            // Lift the select, because gcroot may be NULL if
                            // there's no boxed value.
                            if (isa<Constant>(isboxed_union))
                                data = cast<ConstantInt>(isboxed_union)->isZero() ? data : ctx.builder.CreateLoad(retvalinfo.gcroot);
                            else
                                data = ctx.builder.CreateSelect(isboxed_union,
                                    ctx.builder.CreateLoad(retvalinfo.gcroot),
                                    data);
                        }
                    }
                }
                else {
                    // treat this as a simple boxed returninfo
                    //assert(retvalinfo.isboxed);
                    tindex = compute_tindex_unboxed(ctx, retvalinfo, jlrettype);
                    tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
                    data = maybe_decay_untracked(boxed(ctx, retvalinfo, false)); // skip the gcroot on the return path
                    sret = NULL;
                }
                retval = UndefValue::get(retty);
                retval = ctx.builder.CreateInsertValue(retval, data, 0);
                retval = ctx.builder.CreateInsertValue(retval, tindex, 1);
                break;
            }
            case jl_returninfo_t::Ghosts:
                retval = compute_tindex_unboxed(ctx, retvalinfo, jlrettype);
                break;
            }
            if (sret) {
                if (retvalinfo.ispointer()) {
                    if (returninfo.cc == jl_returninfo_t::SRet) {
                        assert(jl_is_leaf_type(jlrettype));
                        Value *copy_bytes = ConstantInt::get(T_int32, jl_datatype_size(jlrettype));
                        ctx.builder.CreateMemCpy(sret,
                                             data_pointer(ctx, retvalinfo, T_pint8),
                                             copy_bytes,
                                             returninfo.union_minalign);
                    }
                    else {
                        emit_unionmove(ctx, sret, retvalinfo, isboxed_union, false, NULL);
                    }
                }
                else {
                    Type *store_ty = julia_type_to_llvm(retvalinfo.typ);
                    Type *dest_ty = store_ty->getPointerTo();
                    if (dest_ty != sret->getType())
                        sret = emit_bitcast(ctx, sret, dest_ty);
                    ctx.builder.CreateStore(emit_unbox(ctx, store_ty, retvalinfo, retvalinfo.typ), sret);
                }
            }

            if (do_malloc_log(props.in_user_code) && props.line != -1)
                mallocVisitLine(ctx, props.file, props.line);
            if (toplevel)
                ctx.builder.CreateStore(last_age, ctx.world_age_field);
            assert(type_is_ghost(retty) || returninfo.cc == jl_returninfo_t::SRet ||
                retval->getType() == ctx.f->getReturnType());
            ctx.builder.CreateRet(retval);
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
            Value *isfalse = emit_condition(ctx, cond, "if");
            if (do_malloc_log(props.in_user_code) && props.line != -1)
                mallocVisitLine(ctx, props.file, props.line);
            BasicBlock *ifso = BasicBlock::Create(jl_LLVMContext, "if", f);
            BasicBlock *ifnot = handle_label(lname, false);
            // Any branches treated as constant in type inference should be
            // eliminated before running
            ctx.builder.CreateCondBr(isfalse, ifnot, ifso);
            ctx.builder.SetInsertPoint(ifso);
        }
        else if (expr && expr->head == enter_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);
            assert(jl_is_long(args[0]));
            int lname = jl_unbox_long(args[0]);
            CallInst *sj = ctx.builder.CreateCall(prepare_call(except_enter_func));
            // We need to mark this on the call site as well. See issue #6757
            sj->setCanReturnTwice();
            Value *isz = ctx.builder.CreateICmpEQ(sj, ConstantInt::get(T_int32, 0));
            BasicBlock *tryblk = BasicBlock::Create(jl_LLVMContext, "try", f);
            BasicBlock *handlr = handle_label(lname, false);
#ifdef _OS_WINDOWS_
            BasicBlock *cond_resetstkoflw_blk = BasicBlock::Create(jl_LLVMContext, "cond_resetstkoflw", f);
            BasicBlock *resetstkoflw_blk = BasicBlock::Create(jl_LLVMContext, "resetstkoflw", f);
            ctx.builder.CreateCondBr(isz, tryblk, cond_resetstkoflw_blk);
            ctx.builder.SetInsertPoint(cond_resetstkoflw_blk);
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpEQ(
                                     literal_pointer_val(ctx, jl_stackovf_exception),
                                     ctx.builder.CreateLoad(emit_exc_in_transit(ctx), true)),
                                 resetstkoflw_blk, handlr);
            ctx.builder.SetInsertPoint(resetstkoflw_blk);
            ctx.builder.CreateCall(prepare_call(resetstkoflw_func), {});
            ctx.builder.CreateBr(handlr);
#else
            ctx.builder.CreateCondBr(isz, tryblk, handlr);
#endif
            ctx.builder.SetInsertPoint(tryblk);
        }
        else {
            emit_stmtpos(ctx, stmt);
            if (do_malloc_log(props.in_user_code) && props.line != -1) {
                mallocVisitLine(ctx, props.file, props.line);
            }
        }
        find_next_stmt(cursor + 1);
    }
    ctx.builder.SetCurrentDebugLocation(noDbg);
    ctx.builder.ClearInsertionPoint();

    // step 13. Perform any delayed instantiations
    if (ctx.debug_enabled) {
        dbuilder.finalize();
    }

    if (ctx.vaSlot > 0) {
        // remove VA allocation if we never referenced it
        Instruction *root = cast_or_null<Instruction>(ctx.slots[ctx.vaSlot].boxroot);
        if (root) {
            Instruction *store_value = NULL;
            bool have_real_use = false;
            for (Use &U : root->uses()) {
                User *RU = U.getUser();
                if (StoreInst *SRU = dyn_cast<StoreInst>(RU)) {
                    if (!store_value)
                        store_value = dyn_cast<Instruction>(SRU->getValueOperand());
                }
                else if (isa<DbgInfoIntrinsic>(RU)) {
                }
                else if (isa<LoadInst>(RU) && RU->use_empty()) {
                }
                else {
                    have_real_use = true;
                    break;
                }
            }
            if (!have_real_use) {
                for (Use &U : root->uses()) {
                    User *RU = U.getUser();
                    cast<Instruction>(RU)->eraseFromParent();
                }
                root->eraseFromParent();
                if (store_value)
                    store_value->eraseFromParent();
            }
        }
    }

    // copy ctx.roots into m->roots
    // if we created any new roots during codegen
    if (ctx.roots) {
        jl_method_t *m = lam->def.method;
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
    MDNode *scalar = mbuilder->createTBAAScalarTypeNode(name, parent);
    MDNode *n = mbuilder->createTBAAStructTagNode(scalar, scalar, 0, isConstant);
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
        const char* unadorned_name = jl_symbol_name(lam->def.method->name);
        funcName << unadorned_name << "_" << globalUnique++;
        Function *f = jlcall_func_to_llvm(funcName.str(), fptr, NULL);
        if (specsig) {
            if (lam->functionObjectsDecls.specFunctionObject == NULL) {
                lam->functionObjectsDecls.specFunctionObject = strdup(f->getName().str().c_str());
            }
        }
        else {
            assert(lam->fptr == NULL);
            lam->fptr = fptr;
            if (lam->jlcall_api == 1) {
                if (lam->functionObjectsDecls.functionObject == NULL) {
                    lam->functionObjectsDecls.functionObject = strdup(f->getName().str().c_str());
                }
            }
        }
        delete f;
    }
}

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
    tbaa_ptrarraybuf = tbaa_make_child("jtbaa_ptrarraybuf", tbaa_data_scalar).first;
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

    auto T_pint8_derived = PointerType::get(T_int8, AddressSpace::Derived);

    // This type is used to create undef Values for use in struct declarations to skip indices
    NoopType = ArrayType::get(T_int1, 0);

    // add needed base debugging definitions to our LLVM environment
    DIBuilder dbuilder(*m);
    DIFile *julia_h = dbuilder.createFile("julia.h","");
    jl_value_dillvmt = dbuilder.createStructType(nullptr,
        "jl_value_t",
        julia_h,
        71, // At the time of this writing. Not sure if it's worth it to keep this in sync
        0 * 8, // sizeof(jl_value_t) * 8,
        __alignof__(void*) * 8, // __alignof__(jl_value_t) * 8,
        DIFlagZero, // Flags
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt, sizeof(jl_value_t*) * 8,
                                                   __alignof__(jl_value_t*) * 8);

    SmallVector<llvm::Metadata *, 1> Elts;
    std::vector<Metadata*> diargs(0);
    Elts.push_back(jl_pvalue_dillvmt);
    dbuilder.replaceArrays(jl_value_dillvmt,
       dbuilder.getOrCreateArray(Elts));

    jl_ppvalue_dillvmt = dbuilder.createPointerType(jl_pvalue_dillvmt,sizeof(jl_value_t**)*8,
                                                    __alignof__(jl_value_t**)*8);

    diargs.push_back(jl_pvalue_dillvmt);    // Return Type (ret value)
    diargs.push_back(jl_pvalue_dillvmt);    // First Argument (function)
    diargs.push_back(jl_ppvalue_dillvmt);   // Second Argument (argv)
    // Third argument (length(argv))
    diargs.push_back(julia_type_to_di((jl_value_t*)jl_int32_type,&dbuilder,false));

    jl_di_func_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(None));

    T_jlvalue = StructType::create(jl_LLVMContext, "jl_value_t");
    T_pjlvalue = PointerType::get(T_jlvalue, 0);
    T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
    T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
    T_pprjlvalue = PointerType::get(T_prjlvalue, 0);
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
    ftargs.push_back(T_pprjlvalue); // linfo->sparam_vals
    ftargs.push_back(T_prjlvalue);  // function
    ftargs.push_back(T_pprjlvalue); // args[]
    ftargs.push_back(T_int32);      // nargs
    jl_func_sig_sparams = FunctionType::get(T_prjlvalue, ftargs, false);
    assert(jl_func_sig_sparams != NULL);
    ftargs.erase(ftargs.begin());  // drop linfo->sparams_vals argument
    jl_func_sig = FunctionType::get(T_prjlvalue, ftargs, false);
    assert(jl_func_sig != NULL);

    Type *vaelts[] = {T_pint8
#ifdef STORE_ARRAY_LEN
                      , T_size
#endif
                      , T_int16
                      , T_int16
    };
    static_assert(sizeof(jl_array_flags_t) == sizeof(int16_t),
                  "Size of jl_array_flags_t is not the same as int16_t");
    jl_array_llvmt =
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
    global_jlvalue_to_llvm("jl_overflow_exception", &jl_overflow_exception, m);

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
        jl_emit_sysimg_slot(m, pfunctype, "jl_get_ptls_states.ptr",
                            (uintptr_t)jl_get_ptls_states_getter(),
                            jltls_states_func_idx);
        jl_emit_sysimg_slot(m, T_size, "jl_tls_offset.val",
                            (uintptr_t)(jl_tls_offset == -1 ? 0 : jl_tls_offset),
                            jltls_offset_idx);
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
    args1_.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    jlthrow_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_throw", m);
    jlthrow_func->setDoesNotReturn();
    add_named_global(jlthrow_func, &jl_throw);

    // Symbols are not gc-tracked, but we'll treat them as callee rooted anyway,
    // because they may come from a gc-rooted location
    jlundefvarerror_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_undefined_var_error", m);
    jlundefvarerror_func->setDoesNotReturn();
    add_named_global(jlundefvarerror_func, &jl_undefined_var_error);

    std::vector<Type*> args2_boundserrorv(0);
    args2_boundserrorv.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    args2_boundserrorv.push_back(T_psize);
    args2_boundserrorv.push_back(T_size);
    jlboundserrorv_func =
        Function::Create(FunctionType::get(T_void, args2_boundserrorv, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_ints", m);
    jlboundserrorv_func->setDoesNotReturn();
    add_named_global(jlboundserrorv_func, &jl_bounds_error_ints);

    std::vector<Type*> args2_boundserror(0);
    args2_boundserror.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    args2_boundserror.push_back(T_size);
    jlboundserror_func =
        Function::Create(FunctionType::get(T_void, args2_boundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_int", m);
    jlboundserror_func->setDoesNotReturn();
    add_named_global(jlboundserror_func, &jl_bounds_error_int);

    std::vector<Type*> args3_vboundserror(0);
    args3_vboundserror.push_back(T_pprjlvalue);
    args3_vboundserror.push_back(T_size);
    args3_vboundserror.push_back(T_size);
    jlvboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_vboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_tuple_int", m);
    jlvboundserror_func->setDoesNotReturn();
    add_named_global(jlvboundserror_func, &jl_bounds_error_tuple_int);

    std::vector<Type*> args3_uboundserror(0);
    args3_uboundserror.push_back(T_pint8_derived);
    args3_uboundserror.push_back(T_prjlvalue);
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
    args_memcmp.push_back(T_pint8_derived);
    args_memcmp.push_back(T_pint8_derived);
    args_memcmp.push_back(T_size);
    memcmp_derived_func =
        Function::Create(FunctionType::get(T_int32, args_memcmp, false),
                         Function::ExternalLinkage, "memcmp", m);
    add_named_global(memcmp_derived_func, &memcmp);

    std::vector<Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pint8);
    te_args.push_back(T_prjlvalue);
    te_args.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error_rt", m);
    jltypeerror_func->setDoesNotReturn();
    add_named_global(jltypeerror_func, &jl_type_error_rt);

    std::vector<Type *> args_2ptrs(0);
    args_2ptrs.push_back(T_pjlvalue);
    args_2ptrs.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    jlcheckassign_func =
        Function::Create(FunctionType::get(T_void, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_checked_assignment", m);
    add_named_global(jlcheckassign_func, &jl_checked_assignment);

    std::vector<Type *> args_1binding(0);
    args_1binding.push_back(T_pjlvalue);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1binding, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", m);
    add_named_global(jldeclareconst_func, &jl_declare_constant);

    std::vector<Type *> args_2ptrs_(0);
    args_2ptrs_.push_back(T_prjlvalue);
    args_2ptrs_.push_back(T_prjlvalue);
    jlgetbindingorerror_func =
        Function::Create(FunctionType::get(T_pjlvalue, args_2ptrs_, false),
                         Function::ExternalLinkage,
                         "jl_get_binding_or_error", m);
    add_named_global(jlgetbindingorerror_func, &jl_get_binding_or_error);

    jlboundp_func =
        Function::Create(FunctionType::get(T_int32, args_2ptrs_, false),
                         Function::ExternalLinkage,
                         "jl_boundp", m);
    add_named_global(jlboundp_func, &jl_boundp);

    builtin_func_map[jl_f_is] = jlcall_func_to_llvm("jl_f_is", &jl_f_is, m);
    builtin_func_map[jl_f_typeof] = jlcall_func_to_llvm("jl_f_typeof", &jl_f_typeof, m);
    builtin_func_map[jl_f_sizeof] = jlcall_func_to_llvm("jl_f_sizeof", &jl_f_sizeof, m);
    builtin_func_map[jl_f_issubtype] = jlcall_func_to_llvm("jl_f_issubtype", &jl_f_issubtype, m);
    builtin_func_map[jl_f_isa] = jlcall_func_to_llvm("jl_f_isa", &jl_f_isa, m);
    builtin_func_map[jl_f_typeassert] = jlcall_func_to_llvm("jl_f_typeassert", &jl_f_typeassert, m);
    builtin_func_map[jl_f__apply] = jlcall_func_to_llvm("jl_f__apply", &jl_f__apply, m);
    builtin_func_map[jl_f__apply_pure] = jlcall_func_to_llvm("jl_f__apply_pure", &jl_f__apply_pure, m);
    builtin_func_map[jl_f__apply_latest] = jlcall_func_to_llvm("jl_f__apply_latest", &jl_f__apply_latest, m);
    builtin_func_map[jl_f_throw] = jlcall_func_to_llvm("jl_f_throw", &jl_f_throw, m);
    builtin_func_map[jl_f_tuple] = jlcall_func_to_llvm("jl_f_tuple", &jl_f_tuple, m);
    builtin_func_map[jl_f_svec] = jlcall_func_to_llvm("jl_f_svec", &jl_f_svec, m);
    builtin_func_map[jl_f_applicable] = jlcall_func_to_llvm("jl_f_applicable", &jl_f_applicable, m);
    builtin_func_map[jl_f_invoke] = jlcall_func_to_llvm("jl_f_invoke", &jl_f_invoke, m);
    builtin_func_map[jl_f_invoke_kwsorter] = jlcall_func_to_llvm("jl_f_invoke_kwsorter", &jl_f_invoke_kwsorter, m);
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

    std::vector<Type*> argsdepwarnpi(0);
    argsdepwarnpi.push_back(T_size);
    jldepwarnpi_func = Function::Create(FunctionType::get(T_void, argsdepwarnpi, false),
                                        Function::ExternalLinkage,
                                        "jl_depwarn_partial_indexing", m);
    add_named_global(jldepwarnpi_func, &jl_depwarn_partial_indexing);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(T_prjlvalue);
    queuerootfun = Function::Create(FunctionType::get(T_void, args_1ptr, false),
                                    Function::ExternalLinkage,
                                    "jl_gc_queue_root", m);
    add_named_global(queuerootfun, &jl_gc_queue_root);

    std::vector<Type *> agargs(0);
    agargs.push_back(T_pprjlvalue);
    agargs.push_back(T_uint32);
    jlapplygeneric_func = Function::Create(FunctionType::get(T_prjlvalue, agargs, false),
                                           Function::ExternalLinkage,
                                           "jl_apply_generic", m);
    add_named_global(jlapplygeneric_func, &jl_apply_generic);

    std::vector<Type *> invokeargs(0);
    invokeargs.push_back(T_prjlvalue);
    invokeargs.push_back(T_pprjlvalue);
    invokeargs.push_back(T_uint32);
    jlinvoke_func = Function::Create(FunctionType::get(T_prjlvalue, invokeargs, false),
                                     Function::ExternalLinkage,
                                     "jl_invoke", m);
    add_named_global(jlinvoke_func, &jl_invoke);

    std::vector<Type *> exp_args(0);
    exp_args.push_back(T_int1);
    expect_func = Intrinsic::getDeclaration(m, Intrinsic::expect, exp_args);

    std::vector<Type*> args_topeval(0);
    args_topeval.push_back(T_prjlvalue);
    args_topeval.push_back(T_prjlvalue);
    jltopeval_func =
        Function::Create(FunctionType::get(T_pjlvalue, args_topeval, false),
                         Function::ExternalLinkage,
                         "jl_toplevel_eval", m);
    add_named_global(jltopeval_func, &jl_toplevel_eval);

    std::vector<Type*> args_copyast(0);
    args_copyast.push_back(T_prjlvalue);
    jlcopyast_func =
        Function::Create(FunctionType::get(T_prjlvalue, args_copyast, false),
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
    mdargs.push_back(T_prjlvalue);
    mdargs.push_back(T_prjlvalue);
    mdargs.push_back(T_prjlvalue);
    mdargs.push_back(T_prjlvalue);
    jlmethod_func =
        Function::Create(FunctionType::get(T_void, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", m);
    add_named_global(jlmethod_func, &jl_method_def);

    std::vector<Type*> funcdefargs(0);
    funcdefargs.push_back(T_prjlvalue);
    funcdefargs.push_back(T_prjlvalue);
    funcdefargs.push_back(T_pprjlvalue);
    funcdefargs.push_back(T_prjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    jlgenericfunction_func =
        Function::Create(FunctionType::get(T_prjlvalue, funcdefargs, false),
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

    std::vector<Type *> args_2vals_callee_rooted(0);
    args_2vals_callee_rooted.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    args_2vals_callee_rooted.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    jlegal_func =
        Function::Create(FunctionType::get(T_int32, args_2vals_callee_rooted, false),
                         Function::ExternalLinkage,
                         "jl_egal", m);
    add_named_global(jlegal_func, &jl_egal);

    std::vector<Type *> args_2vals_tracked(0);
    args_2vals_tracked.push_back(T_prjlvalue);
    args_2vals_tracked.push_back(T_prjlvalue);
    jlisa_func =
        Function::Create(FunctionType::get(T_int32, args_2vals_tracked, false),
                         Function::ExternalLinkage,
                         "jl_isa", m);
    add_named_global(jlisa_func, &jl_isa);

    jlsubtype_func =
        Function::Create(FunctionType::get(T_int32, args_2vals_tracked, false),
                         Function::ExternalLinkage,
                         "jl_subtype", m);
    add_named_global(jlsubtype_func, &jl_subtype);

    jltypeassert_func = Function::Create(FunctionType::get(T_void, args_2vals_tracked, false),
                                        Function::ExternalLinkage,
                                        "jl_typeassert", m);
    add_named_global(jltypeassert_func, &jl_typeassert);

    std::vector<Type *> applytype_args(0);
    applytype_args.push_back(T_prjlvalue);
    applytype_args.push_back(T_prjlvalue);
    applytype_args.push_back(T_pprjlvalue);
    jlapplytype_func =
        Function::Create(FunctionType::get(T_prjlvalue, applytype_args, false),
                         Function::ExternalLinkage,
                         "jl_instantiate_type_in_env", m);
    add_named_global(jlapplytype_func, &jl_instantiate_type_in_env);

    std::vector<Type*> alloc_pool_args(0);
    alloc_pool_args.push_back(T_pint8);
    alloc_pool_args.push_back(T_int32);
    alloc_pool_args.push_back(T_int32);
    jlalloc_pool_func =
        Function::Create(FunctionType::get(T_prjlvalue, alloc_pool_args, false),
                         Function::ExternalLinkage,
                         "jl_gc_pool_alloc", m);
    add_named_global(jlalloc_pool_func, &jl_gc_pool_alloc);

    std::vector<Type*> alloc_big_args(0);
    alloc_big_args.push_back(T_pint8);
    alloc_big_args.push_back(T_size);
    jlalloc_big_func =
        Function::Create(FunctionType::get(T_prjlvalue, alloc_big_args, false),
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
    getnthfld_args.push_back(T_prjlvalue);
    getnthfld_args.push_back(T_size);
    jlgetnthfieldchecked_func =
        Function::Create(FunctionType::get(T_prjlvalue, getnthfld_args, false),
                         Function::ExternalLinkage,
                         "jl_get_nth_field_checked", m);
    add_named_global(jlgetnthfieldchecked_func, *jl_get_nth_field_checked);

    diff_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, false),
                         Function::ExternalLinkage,
                         "jl_gc_diff_total_bytes", m);
    add_named_global(diff_gc_total_bytes_func, *jl_gc_diff_total_bytes);

    std::vector<Type*> array_owner_args(0);
    array_owner_args.push_back(T_prjlvalue);
    jlarray_data_owner_func =
        Function::Create(FunctionType::get(T_prjlvalue, array_owner_args, false),
                         Function::ExternalLinkage,
                         "jl_array_data_owner", m);
    jlarray_data_owner_func->setAttributes(
        jlarray_data_owner_func->getAttributes()
#if JL_LLVM_VERSION >= 50000
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeList::FunctionIndex, Attribute::ReadOnly)
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeList::FunctionIndex, Attribute::NoUnwind));
#else
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::ReadOnly)
        .addAttribute(jlarray_data_owner_func->getContext(),
                      AttributeSet::FunctionIndex, Attribute::NoUnwind));
#endif
    add_named_global(jlarray_data_owner_func, jl_array_data_owner);

    gcroot_flush_func = Function::Create(FunctionType::get(T_void, false),
                                         Function::ExternalLinkage,
                                         "julia.gcroot_flush");
    add_named_global(gcroot_flush_func, (void*)NULL, /*dllimport*/false);

    pointer_from_objref_func = Function::Create(FunctionType::get(T_pjlvalue,
                                         ArrayRef<Type*>(PointerType::get(T_jlvalue, AddressSpace::Derived)), false),
                                         Function::ExternalLinkage,
                                         "julia.pointer_from_objref");
    pointer_from_objref_func->addFnAttr(Attribute::ReadNone);
    add_named_global(pointer_from_objref_func, (void*)NULL, /*dllimport*/false);

    except_enter_func = Function::Create(FunctionType::get(T_int32, false),
                                         Function::ExternalLinkage,
                                         "julia.except_enter");
    except_enter_func->addFnAttr(Attribute::ReturnsTwice);
    add_named_global(except_enter_func, (void*)NULL, /*dllimport*/false);

    jlgetworld_global =
        new GlobalVariable(*m, T_size,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, "jl_world_counter");
    add_named_global(jlgetworld_global, &jl_world_counter);

    jl_globalPM = new legacy::PassManager();
    jl_globalPM->add(new TargetLibraryInfoWrapperPass(Triple(jl_TargetMachine->getTargetTriple())));
    addOptimizationPasses(jl_globalPM, jl_options.opt_level);
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
    // These two are allowed on 32bit. Allow them on 64bits too for consistency since
    // they basically mean "generic" on aarch64.
    // In particular, "armv8-a" is the generic value for the GCC `-march` option.
    if (cpu != "armv8" && cpu != "armv8-a")
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
    if (jl_options.cpu_target && !strcmp(jl_options.cpu_target,"native")) {
        // On earlier versions of LLVM this is empty
        llvm::sys::getHostCPUFeatures(HostFeatures);
    }

    // Platform specific overides follow
#if defined(_CPU_X86_64_)
    // Require cx16 (cmpxchg16b)
    // We need this for 128-bit atomic operations. We only need this
    // when threading is enabled; however, to test whether this
    // excludes important systems, we require this even when threading
    // is disabled.
    HostFeatures["cx16"] = true;
#endif

    // Figure out if we know the cpu_target
    cpu = ((jl_options.cpu_target && strcmp(jl_options.cpu_target,"native")) ?
            jl_options.cpu_target : getNativeTarget());
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

extern "C" void *jl_init_llvm(void)
{
    const char *const argv_tailmerge[] = {"", "-enable-tail-merge=0"}; // NOO TOUCHIE; NO TOUCH! See #922
    cl::ParseCommandLineOptions(sizeof(argv_tailmerge)/sizeof(argv_tailmerge[0]), argv_tailmerge, "disable-tail-merge\n");
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    const char *const argv_copyprop[] = {"", "-disable-copyprop"}; // llvm bug 21743
    cl::ParseCommandLineOptions(sizeof(argv_copyprop)/sizeof(argv_copyprop[0]), argv_copyprop, "disable-copyprop\n");
#endif
#if defined(JL_DEBUG_BUILD) || defined(USE_POLLY)
    cl::ParseEnvironmentOptions("Julia", "JULIA_LLVM_ARGS");
#endif

    jl_page_size = jl_getpagesize();
    imaging_mode = jl_generating_output();
    jl_init_debuginfo();
    jl_init_runtime_ccall();

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
    m = new Module("julia", jl_LLVMContext);
    shadow_output = m;

    TargetOptions options = TargetOptions();
    //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to assume the stack is always 16-byte aligned,
    // and to ensure that it is 16-byte aligned for out-going calls,
    // to ensure compatibility with GCC codes
    options.StackAlignmentOverride = 16;
#endif
    EngineBuilder eb((std::unique_ptr<Module>(engine_module)));
    std::string ErrorStr;
    eb  .setEngineKind(EngineKind::JIT)
        .setTargetOptions(options)
        // Generate simpler code for JIT
        .setRelocationModel(Reloc::Static)
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
    ;
    Triple TheTriple(sys::getProcessTriple());
#if defined(FORCE_ELF)
    TheTriple.setObjectFormat(Triple::ELF);
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
    #if (!defined(_CPU_ARM_) && !defined(_CPU_PPC64_))
    // FastISel seems to be buggy for ARM. Ref #13321
    if (jl_options.opt_level < 2)
        jl_TargetMachine->setFastISel(true);
    #endif

    init_julia_llvm_meta();
    jl_ExecutionEngine = new JuliaOJIT(*jl_TargetMachine);

// Mark our address spaces as non-integral
#if JL_LLVM_VERSION >= 40000
    jl_data_layout = jl_ExecutionEngine->getDataLayout();
    std::string DL = jl_data_layout.getStringRepresentation() + "-ni:10:11:12";
    jl_data_layout.reset(DL);
#endif

    // Now that the execution engine exists, initialize all modules
    jl_setup_module(engine_module);
    jl_setup_module(m);
    return (void*)m;
}

extern "C" void jl_init_codegen(void)
{
    Module *m = (Module *)jl_init_llvm();
    init_julia_llvm_env(m);

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
#if JL_LLVM_VERSION >= 50000
    ((Value*)v)->print(llvm::dbgs(), true);
#else
    ((Value*)v)->dump();
#endif
}
extern "C" void jl_dump_llvm_inst_function(void *v)
{
#if JL_LLVM_VERSION >= 50000
    cast<Instruction>(((Value*)v))->getParent()->getParent()->print(llvm::dbgs(), nullptr, true);
#else
    cast<Instruction>(((Value*)v))->getParent()->getParent()->dump();
#endif
}
extern "C" void jl_dump_llvm_type(void *v)
{
#if JL_LLVM_VERSION >= 50000
    ((Type*)v)->print(llvm::dbgs(), true);
#else
    ((Type*)v)->dump();
#endif
    putchar('\n');
}

extern void jl_write_bitcode_func(void *F, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::F_None);
    llvm::WriteBitcodeToFile(((llvm::Function*)F)->getParent(), OS);
}

extern void jl_write_bitcode_module(void *M, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::F_None);
    llvm::WriteBitcodeToFile((llvm::Module*)M, OS);
}
