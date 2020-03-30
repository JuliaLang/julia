// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "llvm-version.h"
#include "platform.h"
#include "options.h"
#if defined(_OS_WINDOWS_)
// use ELF because RuntimeDyld COFF i686 support didn't exist
// use ELF because RuntimeDyld COFF X86_64 doesn't seem to work (fails to generate function pointers)?
#define FORCE_ELF
#endif
#if defined(_CPU_X86_)
#define JL_NEED_FLOATTEMP_VAR 1
#endif
#if defined(_OS_WINDOWS_) || defined(_OS_FREEBSD_)
#define JL_DISABLE_FPO
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
#include <iostream>
#include <functional>

// target machine computation
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
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
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/IR/Verifier.h> // for llvmcall validation
#include <llvm/Bitcode/BitcodeWriter.h>

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
#include <llvm/Target/TargetMachine.h>

using namespace llvm;

typedef Instruction TerminatorInst;

#if defined(_OS_WINDOWS_) && !defined(NOMINMAX)
#define NOMINMAX
#endif

#include "julia.h"
#include "julia_internal.h"
#include "jitlayers.h"
#include "codegen_shared.h"
#include "processor.h"
#include "julia_assert.h"

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
#if defined(_COMPILER_GCC_)
extern void ___chkstk_ms(void);
#else
extern void __chkstk(void);
#endif
#else
#if defined(_COMPILER_GCC_)
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
extern JITEventListener *CreateJuliaJITEventListener();

// for image reloading
bool imaging_mode = false;

// shared llvm state
JL_DLLEXPORT LLVMContext &jl_LLVMContext = *(new LLVMContext());
TargetMachine *jl_TargetMachine;
Module *shadow_output;
static DataLayout &jl_data_layout = *(new DataLayout(""));
#define jl_Module ctx.f->getParent()
#define jl_builderModule(builder) (builder).GetInsertBlock()->getParent()->getParent()

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
static MDNode *tbaa_root;     // Everything
static MDNode *tbaa_gcframe;    // GC frame
// LLVM should have enough info for alias analysis of non-gcframe stack slot
// this is mainly a place holder for `jl_cgval_t::tbaa`
static MDNode *tbaa_stack;      // stack slot
static MDNode *tbaa_unionselbyte;   // a selector byte in isbits Union struct fields
static MDNode *tbaa_data;       // Any user data that `pointerset/ref` are allowed to alias
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
static MDNode *tbaa_arrayoffset;     // The offset in a jl_array_t
static MDNode *tbaa_arrayselbyte;   // a selector byte in a isbits Union jl_array_t
static MDNode *tbaa_const;      // Memory that is immutable by the time LLVM can see it

static Attribute Thunk;

// Basic DITypes
static DICompositeType *jl_value_dillvmt;
static DIDerivedType *jl_pvalue_dillvmt;
static DIDerivedType *jl_ppvalue_dillvmt;
static DISubroutineType *jl_di_func_sig;
static DISubroutineType *jl_di_func_null_sig;


// constants
static Constant *V_null;
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

// important functions
static Function *jlnew_func;
static Function *jlsplatnew_func;
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
static Function *jlgetfield_func;
static Function *jlmethod_func;
static Function *jlgenericfunction_func;
static Function *jlenter_func;
static Function *jl_current_exception_func;
static Function *jlleave_func;
static Function *jl_restore_excstack_func;
static Function *jl_excstack_state_func;
static Function *jlegal_func;
static Function *jl_alloc_obj_func;
static Function *jl_newbits_func;
static Function *jl_typeof_func;
static Function *jl_loopinfo_marker_func;
static Function *jl_write_barrier_func;
static Function *jlisa_func;
static Function *jlsubtype_func;
static Function *jlapplytype_func;
static Function *jl_object_id__func;
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
static Function *expect_func;
static Function *jldlsym_func;
static Function *jltypeassert_func;
//static Function *jlgetnthfield_func;
static Function *jlgetnthfieldchecked_func;
//static Function *jlsetnthfield_func;
static Function *jlgetcfunctiontrampoline_func;
static Function *diff_gc_total_bytes_func;
static Function *sync_gc_total_bytes_func;
static Function *jlarray_data_owner_func;
static GlobalVariable *jlgetworld_global;

// placeholder functions
static Function *gcroot_flush_func;
static Function *gc_preserve_begin_func;
static Function *gc_preserve_end_func;
static Function *except_enter_func;
static Function *pointer_from_objref_func;

static std::map<jl_fptr_args_t, Function*> builtin_func_map;

// --- code generation ---
extern "C" {
    int globalUnique = 0;
    int jl_default_debug_info_kind = (int) DICompileUnit::DebugEmissionKind::FullDebug;
    jl_cgparams_t jl_default_cgparams = {1, 1, 1, 0,
#ifdef _OS_WINDOWS_
        0,
#else
        1,
#endif
        jl_default_debug_info_kind, NULL, NULL, NULL, NULL, NULL};
}

template<typename T>
static void add_return_attr(T *f, Attribute::AttrKind Kind)
{
    f->addAttribute(AttributeList::ReturnIndex, Kind);
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

// tracks whether codegen is currently able to simply stack-allocate this type
// note that this includes jl_isbits, although codegen should work regardless
static bool jl_is_concrete_immutable(jl_value_t* t)
{
    return jl_is_immutable_datatype(t) && ((jl_datatype_t*)t)->layout;
}

static bool jl_is_pointerfree(jl_value_t* t)
{
    if (!jl_is_immutable_datatype(t))
        return 0;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)t)->layout;
    return layout && layout->npointers == 0;
}

// these queries are usually related, but we split them out here
// for convenience and clarity (and because it changes the calling convention)
static bool deserves_stack(jl_value_t* t, bool pointerfree=false)
{
    if (!jl_is_concrete_immutable(t))
        return false;
    return ((jl_datatype_t*)t)->isinlinealloc;
}
static bool deserves_argbox(jl_value_t* t)
{
    return !deserves_stack(t);
}
static bool deserves_retbox(jl_value_t* t)
{
    return deserves_argbox(t);
}
static bool deserves_sret(jl_value_t *dt, Type *T)
{
    assert(jl_is_datatype(dt));
    return (size_t)jl_datatype_size(dt) > sizeof(void*) && !T->isFloatingPointTy() && !T->isVectorTy();
}


// metadata tracking for a llvm Value* during codegen
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    // For unions, we may need to keep a reference to the boxed part individually.
    // If this is non-NULL, then, at runtime, we satisfy the invariant that (for the corresponding
    // runtime values) if `(TIndex | 0x80) != 0`, then `Vboxed == V` (by value).
    // For convenience, we also set this value of isboxed values, in which case
    // it is equal (at compile time) to V.
    Value *Vboxed;
    Value *TIndex; // if `V` is an unboxed (tagged) Union described by `typ`, this gives the DataType index (1-based, small int) as an i8
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    jl_value_t *typ; // the original type of V, never NULL
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    MDNode *tbaa; // The related tbaa node. Non-NULL iff this holds an address.
    bool ispointer() const
    {
        // whether this value is compatible with `data_pointer`
        return tbaa != nullptr;
    }
    jl_cgval_t(Value *V, Value *gcroot, bool isboxed, jl_value_t *typ, Value *tindex) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        Vboxed(isboxed ? V : nullptr),
        TIndex(tindex),
        constant(NULL),
        typ(typ),
        isboxed(isboxed),
        isghost(false),
        tbaa(isboxed ? best_tbaa(typ) : nullptr)
    {
        assert(gcroot == nullptr);
        assert(!(isboxed && TIndex != NULL));
        assert(TIndex == NULL || TIndex->getType() == T_int8);
    }
    explicit jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        // mark explicit to avoid being used implicitly for conversion from NULL (use jl_cgval_t() instead)
        V(NULL),
        Vboxed(NULL),
        TIndex(NULL),
        constant(((jl_datatype_t*)typ)->instance),
        typ(typ),
        isboxed(false),
        isghost(true),
        tbaa(nullptr)
    {
        assert(jl_is_datatype(typ));
        assert(constant);
    }
    jl_cgval_t(const jl_cgval_t &v, jl_value_t *typ, Value *tindex) : // copy constructor with new type
        V(v.V),
        Vboxed(v.Vboxed),
        TIndex(tindex),
        constant(v.constant),
        typ(typ),
        isboxed(v.isboxed),
        isghost(v.isghost),
        tbaa(v.tbaa)
    {
        // this constructor expects we had a badly or equivalently typed version
        // make sure we aren't discarding the actual type information
        if (v.TIndex) {
            assert((TIndex == NULL) == jl_is_concrete_type(typ));
        }
        else {
            assert(isboxed || v.typ == typ || tindex);
        }
    }
    jl_cgval_t() : // undef / unreachable / default constructor
        V(UndefValue::get(T_void)),
        Vboxed(NULL),
        TIndex(NULL),
        constant(NULL),
        typ(jl_bottom_type),
        isboxed(false),
        isghost(true),
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
    bool isSA; // whether all stores dominate all uses
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

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
class jl_codectx_t {
public:
    IRBuilder<> builder;
    jl_codegen_params_t &emission_context;
    jl_codegen_call_targets_t &call_targets;
    std::map<void*, GlobalVariable*> &global_targets;
    Function *f = NULL;
    // local var info. globals are not in here.
    std::vector<jl_varinfo_t> slots;
    std::map<int, jl_varinfo_t> phic_slots;
    std::vector<jl_cgval_t> SAvalues;
    std::vector<std::tuple<jl_cgval_t, BasicBlock *, AllocaInst *, PHINode *, jl_value_t *>> PhiNodes;
    std::vector<bool> ssavalue_assigned;
    jl_module_t *module = NULL;
    jl_method_instance_t *linfo = NULL;
    jl_value_t *rettype = NULL;
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
    MDNode *aliasscope = NULL;
    std::string funcName;
    int vaSlot = -1;        // name of vararg argument
    int nReqArgs = 0;
    int nargs = 0;
    int nvargs = -1;

    CallInst *ptlsStates = NULL;
    Value *signalPage = NULL;
    Value *world_age_field = NULL;

    bool debug_enabled = false;
    bool use_cache = false;
    const jl_cgparams_t *params = NULL;

    jl_codectx_t(LLVMContext &llvmctx, jl_codegen_params_t &params)
      : builder(llvmctx),
        emission_context(params),
        call_targets(params.workqueue),
        global_targets(params.globals),
        world(params.world),
        use_cache(params.cache),
        params(params.params) { }

    ~jl_codectx_t() {
        assert(this->roots == NULL);
    }
};

static Type *julia_type_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed = NULL);
static jl_returninfo_t get_specsig_function(jl_codectx_t &ctx, Module *M, StringRef name, jl_value_t *sig, jl_value_t *jlrettype);
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaval = -1);
static Value *global_binding_pointer(jl_codectx_t &ctx, jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign);
static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, bool isvol, MDNode *tbaa);
static jl_cgval_t emit_sparam(jl_codectx_t &ctx, size_t i);
static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const std::string &msg);
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0);
static void CreateTrap(IRBuilder<> &irbuilder);
static CallInst *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                             jl_cgval_t *args, size_t nargs, CallingConv::ID cc);

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p);
static GlobalVariable *prepare_global_in(Module *M, GlobalVariable *G);
#define prepare_global(G) prepare_global_in(jl_Module, (G))
static Instruction *tbaa_decorate(MDNode *md, Instruction *load_or_store);

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

static AllocaInst *emit_static_alloca(jl_codectx_t &ctx, Type *lty)
{
    return new AllocaInst(lty, 0, "", /*InsertBefore=*/ctx.ptlsStates);
}

static void undef_derived_strct(IRBuilder<> &irbuilder, Value *ptr, jl_datatype_t *sty, MDNode *tbaa)
{
    assert(ptr->getType()->getPointerAddressSpace() != AddressSpace::Tracked);
    size_t i, np = sty->layout->npointers;
    if (np == 0)
        return;
    ptr = irbuilder.CreateBitCast(ptr, T_prjlvalue->getPointerTo(ptr->getType()->getPointerAddressSpace()));
    Value *V_null = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
    for (i = 0; i < np; i++) {
        Value *fld = irbuilder.CreateConstInBoundsGEP1_32(T_prjlvalue, ptr, jl_ptr_offset(sty, i));
        tbaa_decorate(tbaa, irbuilder.CreateStore(V_null, fld));
    }
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
        if (jl_is_datatype_singleton((jl_datatype_t*)typ))
            return ghostValue(typ);
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
    return tagval;
}

static bool valid_as_globalinit(const Value *v) {
    if (isa<ConstantExpr>(v)) {
        // llvm can't handle all the things that could be inside a ConstantExpr
        // (such as addrspacecast), and we don't really mind losing this optimization
        return false;
    }
    if (const auto *CC = dyn_cast<ConstantAggregate>(v)) {
        for (const Value *elem : CC->operand_values())
            if (!valid_as_globalinit(elem))
                return false;
    }
    return isa<Constant>(v);
}

static inline jl_cgval_t value_to_pointer(jl_codectx_t &ctx, Value *v, jl_value_t *typ, Value *tindex)
{
    Value *loc;
    if (valid_as_globalinit(v)) { // llvm can't handle all the things that could be inside a ConstantExpr
        loc = get_pointer_to_constant(cast<Constant>(v), "", *jl_Module);
    }
    else {
        loc = emit_static_alloca(ctx, v->getType());
        ctx.builder.CreateStore(v, loc);
    }
    return mark_julia_slot(loc, typ, tindex, tbaa_stack);
}
static inline jl_cgval_t value_to_pointer(jl_codectx_t &ctx, const jl_cgval_t &v)
{
    if (v.ispointer())
        return v;
    return value_to_pointer(ctx, v.V, v.typ, v.TIndex);
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_value_t *typ)
{
    if (jl_is_datatype(typ) && jl_is_datatype_singleton((jl_datatype_t*)typ)) {
        // no need to explicitly load/store a constant/ghost value
        return ghostValue(typ);
    }
    if (jl_is_type_type(typ)) {
        jl_value_t *tp0 = jl_tparam0(typ);
        if (jl_is_concrete_type(tp0) || tp0 == jl_bottom_type) {
            // replace T::Type{T} with T
            return ghostValue(typ);
        }
    }
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T)) {
        return ghostValue(typ);
    }
    if (v && !isboxed && v->getType()->isAggregateType() && !jl_is_vecelement_type(typ) && CountTrackedPointers(v->getType()).count == 0) {
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        return value_to_pointer(ctx, v, typ, NULL);
    }
    return jl_cgval_t(v, NULL, isboxed, typ, NULL);
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_datatype_t *typ)
{
    return mark_julia_type(ctx, v, isboxed, (jl_value_t*)typ);
}

// see if it might be profitable (and cheap) to change the type of v to typ
static inline jl_cgval_t update_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    if (v.typ == typ || v.typ == jl_bottom_type || v.constant || typ == (jl_value_t*)jl_any_type || jl_egal(v.typ, typ))
        return v; // fast-path
    if (jl_is_concrete_type(v.typ) && !jl_is_kind(v.typ)) {
        if (jl_is_concrete_type(typ) && !jl_is_kind(typ)) {
            // type mismatch: changing from one leaftype to another
            CreateTrap(ctx.builder);
            return jl_cgval_t();
        }
        return v; // doesn't improve type info
    }
    if (v.TIndex) {
        jl_value_t *utyp = jl_unwrap_unionall(typ);
        if (jl_is_datatype(utyp)) {
            bool alwaysboxed;
            if (jl_is_concrete_type(utyp))
                alwaysboxed = !jl_is_pointerfree(utyp);
            else
                alwaysboxed = !((jl_datatype_t*)utyp)->abstract && ((jl_datatype_t*)utyp)->mutabl;
            if (alwaysboxed) {
                // discovered that this union-split type must actually be isboxed
                if (v.Vboxed) {
                    return jl_cgval_t(v.Vboxed, nullptr, true, typ, NULL);
                }
                else {
                    // type mismatch (there weren't any boxed values in the union)
                    CreateTrap(ctx.builder);
                    return jl_cgval_t();
                }
            }
        }
        if (!jl_is_concrete_type(typ))
            return v; // not generally worth trying to change type info (which would require recomputing tindex)
    }
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T))
        return ghostValue(typ);
    return jl_cgval_t(v, typ, NULL);
}

static jl_cgval_t convert_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ);

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

#include "cgutils.cpp"

static jl_cgval_t convert_julia_type_union(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    // previous value was a split union, compute new index, or box
    Value *new_tindex = ConstantInt::get(T_int8, 0x80);
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
        // If the old value was boxed and unknown (type tag 0x80),
        // it is possible that the tag was actually one of the types
        // that are now explicitly represented. To find out, we need
        // to compare typeof(v.Vboxed) (i.e. the type of the unknown
        // value) against all the types that are now explicitly
        // selected and select the appropriate one as our new tindex.
        if (v.Vboxed) {
            wasboxed = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(T_int8, 0x80));
            new_tindex = ctx.builder.CreateOr(wasboxed, new_tindex);
            wasboxed = ctx.builder.CreateICmpNE(wasboxed, ConstantInt::get(T_int8, 0));

            BasicBlock *currBB = ctx.builder.GetInsertBlock();

            // We lazily create a BB for this, once we decide that we
            // actually need it.
            Value *union_box_dt = NULL;
            BasicBlock *union_isaBB = NULL;
            auto maybe_setup_union_isa = [&]() {
                if (!union_isaBB) {
                    union_isaBB = BasicBlock::Create(jl_LLVMContext, "union_isa", ctx.f);
                    ctx.builder.SetInsertPoint(union_isaBB);
                    union_box_dt = emit_typeof(ctx, v.Vboxed);
                }
            };

            // If we don't find a match. The type remains unknown
            // (0x80). We could use `v.Tindex`, here, since we know
            // it has to be 0x80, but it seems likely the backend
            // will like the explicit constant better.
            Value *union_box_tindex = ConstantInt::get(T_int8, 0x80);
            unsigned counter = 0;
            for_each_uniontype_small(
                // for each new union-split value
                [&](unsigned idx, jl_datatype_t *jt) {
                    unsigned old_idx = get_box_tindex(jt, v.typ);
                    if (old_idx == 0) {
                        // didn't handle this item before, select its new union index
                        maybe_setup_union_isa();
                        Value *cmp = ctx.builder.CreateICmpEQ(maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jt)), union_box_dt);
                        union_box_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(T_int8, 0x80 | idx), union_box_tindex);
                    }
                },
                typ,
                counter);
            if (union_box_dt) {
                BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_union_isa", ctx.f);
                ctx.builder.CreateBr(postBB);
                ctx.builder.SetInsertPoint(currBB);
                Value *wasunknown = ctx.builder.CreateICmpEQ(v.TIndex, ConstantInt::get(T_int8, 0x80));
                ctx.builder.CreateCondBr(wasunknown, union_isaBB, postBB);
                ctx.builder.SetInsertPoint(postBB);
                PHINode *tindex_phi = ctx.builder.CreatePHI(T_int8, 2);
                tindex_phi->addIncoming(new_tindex, currBB);
                tindex_phi->addIncoming(union_box_tindex, union_isaBB);
                new_tindex = tindex_phi;
            }
        }
        if (!skip_box.all()) {
            // some values weren't unboxed in the new union
            // box them now (tindex above already selected 0x80 = box for them)
            Value *boxv = box_union(ctx, v, skip_box);
            if (v.Vboxed) {
                // If the value is boxed both before and after, we don't need
                // to touch it at all. Otherwise we're either transitioning
                // unboxed->boxed, or leaving an unboxed value in place.
                Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(new_tindex, ConstantInt::get(T_int8, 0x80)),
                    ConstantInt::get(T_int8, 0));
                boxv = ctx.builder.CreateSelect(
                    ctx.builder.CreateAnd(wasboxed, isboxed), v.Vboxed, boxv);
            }
            if (v.V == NULL) {
                // v.V might be NULL if it was all ghost objects before
                return jl_cgval_t(boxv, NULL, false, typ, new_tindex);
            } else {
                Value *isboxv = ctx.builder.CreateIsNotNull(boxv);
                Value *slotv;
                MDNode *tbaa;
                if (v.ispointer()) {
                    slotv = v.V;
                    tbaa = v.tbaa;
                }
                else {
                    slotv = emit_static_alloca(ctx, v.V->getType());
                    ctx.builder.CreateStore(v.V, slotv);
                    tbaa = tbaa_stack;
                }
                slotv = ctx.builder.CreateSelect(isboxv,
                            decay_derived(boxv),
                            decay_derived(emit_bitcast(ctx, slotv, boxv->getType())));
                jl_cgval_t newv = jl_cgval_t(slotv, NULL, false, typ, new_tindex);
                newv.Vboxed = boxv;
                newv.tbaa = tbaa;
                return newv;
            }
        }
    }
    else {
        return jl_cgval_t(boxed(ctx, v), NULL, true, typ, NULL);
    }
    return jl_cgval_t(v, typ, new_tindex);
}

// given a value marked with type `v.typ`, compute the mapping and/or boxing to return a value of type `typ`
// TODO: should this set TIndex when trivial (such as 0x80 or concrete types) ?
static jl_cgval_t convert_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    if (typ == (jl_value_t*)jl_typeofbottom_type)
        return ghostValue(typ); // normalize TypeofBottom to Type{Union{}}
    if (v.typ == typ || v.typ == jl_bottom_type || jl_egal(v.typ, typ))
        return v; // fast-path
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T))
        return ghostValue(typ);
    Value *new_tindex = NULL;
    if (jl_is_concrete_type(typ)) {
        if (v.TIndex && !jl_is_pointerfree(typ)) {
            // discovered that this union-split type must actually be isboxed
            if (v.Vboxed) {
                return jl_cgval_t(v.Vboxed, nullptr, true, typ, NULL);
            }
            else {
                // type mismatch: there weren't any boxed values in the union
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
        if (jl_is_concrete_type(v.typ) && !jl_is_kind(v.typ)) {
            if (jl_is_concrete_type(typ) && !jl_is_kind(typ)) {
                // type mismatch: changing from one leaftype to another
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
        }
    }
    else {
        bool makeboxed = false;
        if (v.TIndex) {
            return convert_julia_type_union(ctx, v, typ);
        }
        else if (!v.isboxed && jl_is_uniontype(typ)) {
            // previous value was unboxed (leaftype), statically compute union tindex
            assert(jl_is_concrete_type(v.typ));
            unsigned new_idx = get_box_tindex((jl_datatype_t*)v.typ, typ);
            if (new_idx) {
                new_tindex = ConstantInt::get(T_int8, new_idx);
                if (v.V && !v.ispointer()) {
                    // TODO: remove this branch once all consumers of v.TIndex understand how to handle a non-ispointer value
                    Value *slotv = emit_static_alloca(ctx, v.V->getType());
                    ctx.builder.CreateStore(v.V, slotv);
                    jl_cgval_t newv = jl_cgval_t(slotv, NULL, false, typ, new_tindex);
                    newv.tbaa = tbaa_stack;
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
            return jl_cgval_t(boxed(ctx, v), NULL, true, typ, NULL);
        }
    }
    return jl_cgval_t(v, typ, new_tindex);
}

static void jl_setup_module(Module *m, const jl_cgparams_t *params = &jl_default_cgparams)
{
    if (JL_HOOK_TEST(params, module_setup)) {
        JL_HOOK_CALL(params, module_setup, 1, jl_box_voidpointer(wrap(m)));
        return;
    }

    // Some linkers (*cough* OS X) don't understand DWARF v4, so we use v2 in
    // imaging mode. The structure of v4 is slightly nicer for debugging JIT
    // code.
    if (!m->getModuleFlag("Dwarf Version")) {
        int dwarf_version = 4;
#ifdef _OS_DARWIN_
        if (imaging_mode)
            dwarf_version = 2;
#endif
        m->addModuleFlag(llvm::Module::Warning, "Dwarf Version", dwarf_version);
    }
    if (!m->getModuleFlag("Debug Info Version"))
        m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
    m->setDataLayout(jl_data_layout);
    m->setTargetTriple(jl_TargetMachine->getTargetTriple().str());
}

static void jl_init_function(Function *F)
{
    // set any attributes that *must* be set on all functions
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to realign the stack to the next 16-byte boundary
    // upon entry to any function. This achieves compatibility
    // with both MinGW-GCC (which assumes an 16-byte-aligned stack) and
    // i686 Windows (which uses a 4-byte-aligned stack)
    AttrBuilder attr;
    attr.addStackAlignmentAttr(16);
    F->addAttributes(AttributeList::FunctionIndex, attr);
#endif
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    F->setHasUWTable(); // force NeedsWinEH
#endif
#ifdef JL_DISABLE_FPO
#if LLVM_VERSION_MAJOR >= 8
    F->addFnAttr("frame-pointer", "all");
#else
    F->addFnAttr("no-frame-pointer-elim", "true");
#endif
#endif
}

static std::pair<bool, bool> uses_specsig(jl_method_instance_t *lam, jl_value_t *rettype, bool prefer_specsig)
{
    size_t nreq = jl_is_method(lam->def.method) ? lam->def.method->nargs : 0;
    int va = 0;
    if (nreq > 0 && lam->def.method->isva) {
        nreq--;
        va = 1;
    }
    jl_value_t *sig = lam->specTypes;
    bool needsparams = false;
    if (jl_is_method(lam->def.method)) {
        if ((size_t)jl_subtype_env_size(lam->def.method->sig) != jl_svec_len(lam->sparam_vals))
            needsparams = true;
        for (size_t i = 0; i < jl_svec_len(lam->sparam_vals); ++i) {
            if (jl_is_typevar(jl_svecref(lam->sparam_vals, i)))
                needsparams = true;
        }
    }
    if (needsparams)
        return std::make_pair(false, true);
    if (sig == (jl_value_t*)jl_anytuple_type)
        return std::make_pair(false, false);
    if (!jl_is_datatype(sig))
        return std::make_pair(false, false);
    if (jl_nparams(sig) == 0)
        return std::make_pair(false, false);
    if (va) {
        if (jl_is_vararg_type(jl_tparam(sig, jl_nparams(sig) - 1)))
            return std::make_pair(false, false);
    }
    // not invalid, consider if specialized signature is worthwhile
    if (prefer_specsig)
        return std::make_pair(true, false);
    if (!deserves_retbox(rettype) && !jl_is_datatype_singleton((jl_datatype_t*)rettype))
        return std::make_pair(true, false);
    if (jl_is_uniontype(rettype)) {
        bool allunbox;
        size_t nbytes, align, min_align;
        union_alloca_type((jl_uniontype_t*)rettype, allunbox, nbytes, align, min_align);
        if (nbytes > 0)
            return std::make_pair(true, false); // some elements of the union could be returned unboxed avoiding allocation
    }
    bool allSingleton = true;
    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *sigt = jl_tparam(sig, i);
        bool issing = jl_is_datatype(sigt) && jl_is_datatype_singleton((jl_datatype_t*)sigt);
        allSingleton &= issing;
        if (!deserves_argbox(sigt) && !issing) {
            return std::make_pair(true, false);
        }
    }
    if (allSingleton)
        return std::make_pair(true, false);
    return std::make_pair(false, false); // jlcall sig won't require any box allocations
}


// Logging for code coverage and memory allocation

const int logdata_blocksize = 32; // target getting nearby lines in the same general cache area and reducing calls to malloc by chunking
typedef uint64_t logdata_block[logdata_blocksize];
typedef StringMap< std::vector<logdata_block*> > logdata_t;

static uint64_t *allocLine(std::vector<logdata_block*> &vec, int line)
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
    return &data[line];
}

static void visitLine(jl_codectx_t &ctx, std::vector<logdata_block*> &vec, int line, Value *addend, const char* name)
{
    uint64_t *ptr = allocLine(vec, line);
    Value *pv = ConstantExpr::getIntToPtr(
        ConstantInt::get(T_size, (uintptr_t)ptr),
        T_pint64);
    Value *v = ctx.builder.CreateLoad(pv, true, name);
    v = ctx.builder.CreateAdd(v, addend);
    ctx.builder.CreateStore(v, pv, true); // volatile, not atomic, so this might be an underestimate,
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

static void coverageAllocLine(StringRef filename, int line)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    allocLine(coverageData[filename], line);
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(jl_codectx_t &ctx, StringRef filename, int line, Value *sync)
{
    assert(!imaging_mode);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    Value *addend = sync
        ? ctx.builder.CreateCall(prepare_call(sync_gc_total_bytes_func), {sync})
        : ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    visitLine(ctx, mallocData[filename], line, addend, "bytecnt");
}

// Resets the malloc counts.
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
    jl_gc_sync_total_bytes(0);
}

extern "C" int isabspath(const char *in);

static void write_log_data(logdata_t &logData, const char *extension)
{
    std::string base = std::string(jl_options.julia_bindir);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename(it->first());
        std::vector<logdata_block*> &values = it->second;
        if (!values.empty()) {
            if (!isabspath(filename.c_str()))
                filename = base + filename;
            std::ifstream inf(filename.c_str());
            if (!inf.is_open())
                continue;
            std::string outfile = filename + extension;
            std::ofstream outf(outfile.c_str(), std::ofstream::trunc | std::ofstream::out | std::ofstream::binary);
            if (outf.is_open()) {
                inf.exceptions(std::ifstream::badbit);
                outf.exceptions(std::ifstream::failbit | std::ifstream::badbit);
                char line[1024];
                int l = 1;
                unsigned block = 0;
                while (!inf.eof()) {
                    inf.getline(line, sizeof(line));
                    if (inf.fail()) {
                        if (inf.eof())
                            break; // no content on trailing line
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
                    outf << " " << line << '\n';
                }
                outf.close();
            }
            inf.close();
        }
    }
}

extern "C" int jl_getpid();

static void write_lcov_data(logdata_t &logData, const std::string &outfile)
{
    std::ofstream outf(outfile.c_str(), std::ofstream::ate | std::ofstream::out | std::ofstream::binary);
    //std::string base = std::string(jl_options.julia_bindir);
    //base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        StringRef filename = it->first();
        const std::vector<logdata_block*> &values = it->second;
        if (!values.empty()) {
            outf << "SF:" << filename.str() << '\n';
            size_t n_covered = 0;
            size_t n_instrumented = 0;
            size_t lno = 0;
            for (auto &itv : values) {
                if (itv) {
                    logdata_block &data = *itv;
                    for (int i = 0; i < logdata_blocksize; i++) {
                        auto cov = data[i];
                        if (cov > 0) {
                            n_instrumented++;
                            if (cov > 1)
                                n_covered++;
                            outf << "DA:" << lno << ',' << (cov - 1) << '\n';
                        }
                        lno++;
                    }
                }
                else {
                    lno += logdata_blocksize;
                }
            }
            outf << "LH:" << n_covered << '\n';
            outf << "LF:" << n_instrumented << '\n';
            outf << "end_of_record\n";
        }
    }
    outf.close();
}

extern "C" void jl_write_coverage_data(const char *output)
{
    if (output) {
        StringRef output_pattern(output);
        if (output_pattern.endswith(".info"))
            write_lcov_data(coverageData, jl_format_filename(output_pattern));
    }
    else {
        std::ostringstream stm;
        stm << "." << jl_getpid() << ".cov";
        write_log_data(coverageData, stm.str().c_str());
    }
}

extern "C" void jl_write_malloc_log(void)
{
    std::ostringstream stm;
    stm << "." << jl_getpid() << ".mem";
    write_log_data(mallocData, stm.str().c_str());
}

// --- constant determination ---

static void show_source_loc(jl_codectx_t &ctx, JL_STREAM *out)
{
    jl_printf(out, "in %s at %s", ctx.name, ctx.file.str().c_str());
}

extern "C" void jl_binding_deprecation_warning(jl_module_t *m, jl_binding_t *b);

static void cg_bdw(jl_codectx_t &ctx, jl_binding_t *b)
{
    jl_binding_deprecation_warning(ctx.module, b);
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
    jl_value_t *result;
    JL_TRY {
        result = jl_apply(v, nargs);
    }
    JL_CATCH {
        result = NULL;
    }
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
        ssize_t idx = ((jl_ssavalue_t*)ex)->id - 1;
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
                if (jl_array_dim0(e->args) == 3 && f == jl_builtin_getfield) {
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
                    jl_value_t *result;
                    JL_TRY {
                        result = jl_apply(v, n+1);
                    }
                    JL_CATCH {
                        result = NULL;
                    }
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

static std::set<int> assigned_in_try(jl_array_t *stmts, int s, long l)
{
    std::set<int> av;
    for(int i=s; i <= l; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == assign_sym) {
                jl_value_t *ar = jl_exprarg(st, 0);
                if (jl_is_slot(ar)) {
                    av.insert(jl_slot_number(ar)-1);
                }
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
                int last = jl_unbox_long(jl_exprarg(st, 0));
                std::set<int> as = assigned_in_try(stmts, i + 1, last);
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
        else {
            size_t i, elen = jl_array_dim0(e->args);
            for (i = 0; i < elen; i++) {
                simple_use_analysis(ctx, jl_exprarg(e, i));
            }
        }
    }
    else if (jl_is_pinode(expr)) {
        simple_use_analysis(ctx, jl_fieldref_noalloc(expr, 0));
    }
    else if (jl_is_upsilonnode(expr)) {
        jl_value_t *val = jl_fieldref_noalloc(expr, 0);
        if (val)
            simple_use_analysis(ctx, val);
    }
    else if (jl_is_phicnode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 0);
        size_t i, elen = jl_array_len(values);
        for (i = 0; i < elen; i++) {
            jl_value_t *v = jl_array_ptr_ref(values, i);
            simple_use_analysis(ctx, v);
        }
    }
    else if (jl_is_phinode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 1);
        size_t i, elen = jl_array_len(values);
        for (i = 0; i < elen; i++) {
            jl_value_t *v = jl_array_ptr_ref(values, i);
            if (v)
                simple_use_analysis(ctx, v);
        }
    }
}

// --- gc root utils ---

// ---- Get Element Pointer (GEP) instructions within the GC frame ----

static void jl_add_method_root(jl_codectx_t &ctx, jl_value_t *val)
{
    if (jl_is_concrete_type(val) || jl_is_bool(val) || jl_is_symbol(val) || val == jl_nothing ||
            val == (jl_value_t*)jl_any_type || val == (jl_value_t*)jl_bottom_type || val == (jl_value_t*)jl_core_module)
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
    if (jl_is_type_type((jl_value_t*)sty) && jl_is_concrete_type(jl_tparam0(sty)))
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
    Value *result = emit_jlcall(ctx, jlgetfield_func, maybe_decay_untracked(V_null), myargs_array, 2, JLCALL_F_CC);
    return mark_julia_type(ctx, result, true, jl_any_type);
}

static Value *emit_box_compare(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2)
{
    if (jl_pointer_egal(arg1.typ) || jl_pointer_egal(arg2.typ)) {
        Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : arg1.V;
        Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : arg2.V;
        assert(varg1 && varg2 && (arg1.isboxed || arg1.TIndex) && (arg2.isboxed || arg2.TIndex) &&
                "Only boxed types are valid for pointer comparison.");
        varg1 = maybe_decay_tracked(varg1);
        varg2 = maybe_decay_tracked(varg2);
        if (cast<PointerType>(varg1->getType())->getAddressSpace() != cast<PointerType>(varg2->getType())->getAddressSpace()) {
            varg1 = decay_derived(varg1);
            varg2 = decay_derived(varg2);
        }
        return ctx.builder.CreateICmpEQ(emit_bitcast(ctx, varg1, T_pint8),
                                        emit_bitcast(ctx, varg2, T_pint8));
    }

    Value *varg1 = mark_callee_rooted(boxed(ctx, arg1));
    Value *varg2 = mark_callee_rooted(boxed(ctx, arg2));
    return ctx.builder.CreateTrunc(ctx.builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
}

static Value *emit_bits_compare(jl_codectx_t &ctx, jl_cgval_t arg1, jl_cgval_t arg2);

static Value *emit_bitsunion_compare(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2)
{
    assert(arg1.typ == arg2.typ && arg1.TIndex && arg2.TIndex && jl_is_uniontype(arg1.typ) && "unimplemented");
    Value *tindex = arg1.TIndex;
    BasicBlock *defaultBB = BasicBlock::Create(jl_LLVMContext, "unionbits_is_boxed", ctx.f);
    SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
    BasicBlock *postBB = BasicBlock::Create(jl_LLVMContext, "post_unionbits_is", ctx.f);
    ctx.builder.SetInsertPoint(postBB);
    PHINode *phi = ctx.builder.CreatePHI(T_int1, 2);
    unsigned counter = 0;
    for_each_uniontype_small(
        [&](unsigned idx, jl_datatype_t *jt) {
            BasicBlock *tempBB = BasicBlock::Create(jl_LLVMContext, "unionbits_is", ctx.f);
            ctx.builder.SetInsertPoint(tempBB);
            switchInst->addCase(ConstantInt::get(T_int8, idx), tempBB);
            jl_cgval_t sel_arg1(arg1, (jl_value_t*)jt, NULL);
            jl_cgval_t sel_arg2(arg2, (jl_value_t*)jt, NULL);
            Value *cmp = emit_bits_compare(ctx, sel_arg1, sel_arg2);
            tempBB = ctx.builder.GetInsertBlock(); // could have changed
            phi->addIncoming(cmp, tempBB);
            ctx.builder.CreateBr(postBB);
        },
        arg1.typ,
        counter);
    ctx.builder.SetInsertPoint(defaultBB);
    Function *trap_func = Intrinsic::getDeclaration(
        ctx.f->getParent(),
        Intrinsic::trap);
    ctx.builder.CreateCall(trap_func);
    ctx.builder.CreateUnreachable();
    ctx.builder.SetInsertPoint(postBB);
    return ctx.builder.CreateAnd(phi, ctx.builder.CreateICmpEQ(arg1.TIndex, arg2.TIndex));
}

static Value *emit_bits_compare(jl_codectx_t &ctx, jl_cgval_t arg1, jl_cgval_t arg2)
{
    bool isboxed;
    Type *at = julia_type_to_llvm(ctx, arg1.typ, &isboxed);
    assert(jl_is_datatype(arg1.typ) && arg1.typ == arg2.typ && !isboxed);

    if (type_is_ghost(at))
        return ConstantInt::get(T_int1, 1);

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
        jl_datatype_t *sty = (jl_datatype_t*)arg1.typ;
        size_t sz = jl_datatype_size(sty);
        Value *varg1 = arg1.ispointer() ? maybe_decay_tracked(data_pointer(ctx, arg1)) : arg1.V;
        Value *varg2 = arg2.ispointer() ? maybe_decay_tracked(data_pointer(ctx, arg2)) : arg2.V;
        if (sz > 512 && !sty->layout->haspadding) {
            if (!arg1.ispointer())
                varg1 = value_to_pointer(ctx, arg1).V;
            if (!arg2.ispointer())
                varg2 = value_to_pointer(ctx, arg2).V;
            varg1 = emit_pointer_from_objref(ctx, varg1);
            varg2 = emit_pointer_from_objref(ctx, varg2);
            Value *gc_uses[2];
            int nroots = 0;
            if ((gc_uses[nroots] = get_gc_root_for(arg1)))
                nroots++;
            if ((gc_uses[nroots] = get_gc_root_for(arg2)))
                nroots++;
            OperandBundleDef OpBundle("jl_roots", gc_uses);
            Value *answer = ctx.builder.CreateCall(prepare_call(memcmp_func), {
                        ctx.builder.CreateBitCast(varg1, T_pint8),
                        ctx.builder.CreateBitCast(varg2, T_pint8),
                        ConstantInt::get(T_size, sz) },
                    ArrayRef<OperandBundleDef>(&OpBundle, nroots ? 1 : 0));
            return ctx.builder.CreateICmpEQ(answer, ConstantInt::get(T_int32, 0));
        }
        else {
            Type *atp = at->getPointerTo();
            if (arg1.ispointer())
                varg1 = maybe_bitcast(ctx, varg1, atp);
            if (arg2.ispointer())
                varg2 = maybe_bitcast(ctx, varg2, atp);
            jl_svec_t *types = sty->types;
            Value *answer = ConstantInt::get(T_int1, 1);
            for (size_t i = 0, l = jl_svec_len(types); i < l; i++) {
                // TODO: should we replace this with `subAns = emit_f_is(emit_getfield_knownidx(ctx, arg1, i, arg1.typ), emit_getfield_knownidx(ctx, arg2, i, arg2.typ))`
                // or is there any value in all this extra effort??
                jl_value_t *fldty = jl_svecref(types, i);
                if (type_is_ghost(julia_type_to_llvm(ctx, fldty)))
                    continue;
                unsigned byte_offset = jl_field_offset(sty, i);
                Value *subAns, *fld1, *fld2;
                unsigned llvm_idx = (i > 0 && isa<StructType>(at)) ? convert_struct_offset(ctx, at, byte_offset) : i;
                if (arg1.ispointer())
                    fld1 = ctx.builder.CreateConstInBoundsGEP2_32(at, varg1, 0, llvm_idx);
                else
                    fld1 = ctx.builder.CreateExtractValue(varg1, llvm_idx);
                if (arg2.ispointer())
                    fld2 = ctx.builder.CreateConstInBoundsGEP2_32(at, varg2, 0, llvm_idx);
                else
                    fld2 = ctx.builder.CreateExtractValue(varg2, llvm_idx);
                if (jl_field_isptr(sty, i)) {
                    if (arg1.ispointer())
                        fld1 = ctx.builder.CreateLoad(T_prjlvalue, fld1);
                    if (arg2.ispointer())
                        fld2 = ctx.builder.CreateLoad(T_prjlvalue, fld2);
                    subAns = emit_box_compare(ctx,
                            mark_julia_type(ctx, fld1, true, fldty),
                            mark_julia_type(ctx, fld2, true, fldty));
                }
                else if (jl_is_uniontype(fldty)) {
                    unsigned tindex_offset = byte_offset + jl_field_size(sty, i) - 1;
                    jl_cgval_t fld1_info;
                    jl_cgval_t fld2_info;
                    if (arg1.ispointer()) {
                        Value *tindex1 = ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1),
                                ctx.builder.CreateLoad(T_int8, emit_struct_gep(ctx, at, varg1, tindex_offset)));
                        fld1_info = mark_julia_slot(fld1, fldty, tindex1, arg1.tbaa);
                    }
                    else {
                        fld1_info = emit_getfield_knownidx(ctx, arg1, i, sty);
                    }
                    if (arg2.ispointer()) {
                        Value *tindex2 = ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1),
                                ctx.builder.CreateLoad(T_int8, emit_struct_gep(ctx, at, varg2, tindex_offset)));
                        fld2_info = mark_julia_slot(fld2, fldty, tindex2, arg2.tbaa);
                    }
                    else {
                        fld2_info = emit_getfield_knownidx(ctx, arg2, i, sty);
                    }
                    subAns = emit_bitsunion_compare(ctx, fld1_info, fld2_info);
                }
                else {
                    assert(jl_is_concrete_type(fldty));
                    jl_cgval_t fld1_info = arg1.ispointer() ? mark_julia_slot(fld1, fldty, NULL, arg1.tbaa) : mark_julia_type(ctx, fld1, false, fldty);
                    jl_cgval_t fld2_info = arg2.ispointer() ? mark_julia_slot(fld2, fldty, NULL, arg2.tbaa) : mark_julia_type(ctx, fld2, false, fldty);
                    subAns = emit_bits_compare(ctx, fld1_info, fld2_info);
                }
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
    jl_value_t *rt1 = arg1.typ;
    jl_value_t *rt2 = arg2.typ;
    if (jl_is_concrete_type(rt1) && jl_is_concrete_type(rt2) && !jl_is_kind(rt1) && !jl_is_kind(rt2) && rt1 != rt2) {
        // disjoint concrete leaf types are never equal (quick test)
        return ConstantInt::get(T_int1, 0);
    }

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
            mark_callee_rooted(boxed(ctx, arg1)),
            mark_callee_rooted(boxed(ctx, arg2)));
    }

    if (jl_type_intersection(rt1, rt2) == (jl_value_t*)jl_bottom_type) // types are disjoint (exhaustive test)
        return ConstantInt::get(T_int1, 0);

    bool justbits1 = jl_is_concrete_immutable(rt1);
    bool justbits2 = jl_is_concrete_immutable(rt2);
    if (justbits1 || justbits2) { // whether this type is unique'd by value
        jl_value_t *typ = justbits1 ? rt1 : rt2;
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
        isaBB = ctx.builder.GetInsertBlock(); // might have changed
        ctx.builder.CreateBr(postBB);
        ctx.builder.SetInsertPoint(postBB);
        PHINode *cmp = ctx.builder.CreatePHI(T_int1, 2);
        cmp->addIncoming(ConstantInt::get(T_int1, 0), currBB);
        cmp->addIncoming(bitcmp, isaBB);
        return cmp;
    }

    // if (arg1.tindex || arg2.tindex)
    //   TODO: handle with emit_bitsunion_compare

    return emit_box_compare(ctx, arg1, arg2);
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

    else if (((f == jl_builtin__apply && nargs == 2) ||
              (f == jl_builtin__apply_iterate && nargs == 3)) && ctx.vaSlot > 0) {
        int arg_start = f == jl_builtin__apply ? 2 : 3;
        // turn Core._apply(f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the va allocation
        if (LoadInst *load = dyn_cast_or_null<LoadInst>(argv[arg_start].V)) {
            if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot && ctx.argArray) {
                Value *theF = maybe_decay_untracked(boxed(ctx, argv[arg_start-1]));
                Value *nva = emit_n_varargs(ctx);
#ifdef _P64
                nva = ctx.builder.CreateTrunc(nva, T_int32);
#endif
                Value *theArgs = ctx.builder.CreateInBoundsGEP(T_prjlvalue, ctx.argArray, ConstantInt::get(T_size, ctx.nReqArgs));
                Value *r = ctx.builder.CreateCall(prepare_call(jlapplygeneric_func), { theF, theArgs, nva });
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
        if (jl_is_tuple_type(rt) && jl_is_concrete_type(rt) && nargs == jl_datatype_nfields(rt)) {
            *ret = emit_new_struct(ctx, rt, nargs, &argv[1]);
            return true;
        }
    }

    else if (f == jl_builtin_throw && nargs == 1) {
        Value *arg1 = boxed(ctx, argv[1]);
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
                    inBB = ctx.builder.GetInsertBlock(); // could have changed
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

    else if ((f == jl_builtin_arrayref || f == jl_builtin_const_arrayref) && nargs >= 3) {
        const jl_cgval_t &ary = argv[2];
        bool indices_ok = true;
        for (size_t i = 3; i <= nargs; i++) {
            if (argv[i].typ != (jl_value_t*)jl_long_type) {
                indices_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(ary.typ);
        if (jl_is_array_type(aty_dt) && indices_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            jl_value_t *ndp = jl_tparam1(aty_dt);
            if (!jl_has_free_typevars(ety) && (jl_is_long(ndp) || nargs == 3)) {
                jl_value_t *ary_ex = jl_exprarg(ex, 2);
                size_t elsz = 0, al = 0;
                int union_max = jl_islayout_inline(ety, &elsz, &al);
                bool isboxed = (union_max == 0);
                if (isboxed)
                    ety = (jl_value_t*)jl_any_type;
                ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                jl_value_t *boundscheck = argv[1].constant;
                Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[3], nargs - 2, boundscheck);
                if (!isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
                    assert(((jl_datatype_t*)ety)->instance != NULL);
                    *ret = ghostValue(ety);
                }
                else if (!isboxed && jl_is_uniontype(ety)) {
                    Type *AT = ArrayType::get(IntegerType::get(jl_LLVMContext, 8 * al), (elsz + al - 1) / al);
                    Value *data = emit_bitcast(ctx, emit_arrayptr(ctx, ary, ary_ex), AT->getPointerTo());
                    // isbits union selector bytes are stored after a->maxsize
                    Value *ndims = (nd == -1 ? emit_arrayndims(ctx, ary) : ConstantInt::get(T_int16, nd));
                    Value *is_vector = ctx.builder.CreateICmpEQ(ndims, ConstantInt::get(T_int16, 1));
                    Value *offset = emit_arrayoffset(ctx, ary, nd);
                    Value *selidx_v = ctx.builder.CreateSub(emit_vectormaxsize(ctx, ary), ctx.builder.CreateZExt(offset, T_size));
                    Value *selidx_m = emit_arraylen(ctx, ary);
                    Value *selidx = ctx.builder.CreateSelect(is_vector, selidx_v, selidx_m);
                    Value *ptindex = ctx.builder.CreateInBoundsGEP(AT, data, selidx);
                    ptindex = emit_bitcast(ctx, ptindex, T_pint8);
                    ptindex = ctx.builder.CreateInBoundsGEP(T_int8, ptindex, offset);
                    ptindex = ctx.builder.CreateInBoundsGEP(T_int8, ptindex, idx);
                    Instruction *tindex = tbaa_decorate(tbaa_arrayselbyte, ctx.builder.CreateLoad(T_int8, ptindex));
                    tindex->setMetadata(LLVMContext::MD_range, MDNode::get(jl_LLVMContext, {
                        ConstantAsMetadata::get(ConstantInt::get(T_int8, 0)),
                        ConstantAsMetadata::get(ConstantInt::get(T_int8, union_max)) }));
                    AllocaInst *lv = emit_static_alloca(ctx, AT);
                    if (al > 1)
                        lv->setAlignment(Align(al));
                    emit_memcpy(ctx, lv, tbaa_arraybuf, ctx.builder.CreateInBoundsGEP(AT, data, idx), tbaa_arraybuf, elsz, al, false);
                    *ret = mark_julia_slot(lv, ety, ctx.builder.CreateNUWAdd(ConstantInt::get(T_int8, 1), tindex), tbaa_arraybuf);
                }
                else {
                    MDNode *aliasscope = (f == jl_builtin_const_arrayref) ? ctx.aliasscope : nullptr;
                    *ret = typed_load(ctx,
                            emit_arrayptr(ctx, ary, ary_ex),
                            idx, ety,
                            !isboxed ? tbaa_arraybuf : tbaa_ptrarraybuf, aliasscope);
                }
                return true;
            }
        }
    }

    else if (f == jl_builtin_arrayset && nargs >= 4) {
        const jl_cgval_t &ary = argv[2];
        const jl_cgval_t &val = argv[3];
        bool indices_ok = true;
        for (size_t i = 4; i <= nargs; i++) {
            if (argv[i].typ != (jl_value_t*)jl_long_type) {
                indices_ok = false;
                break;
            }
        }
        jl_value_t *aty_dt = jl_unwrap_unionall(ary.typ);
        if (jl_is_array_type(aty_dt) && indices_ok) {
            jl_value_t *ety = jl_tparam0(aty_dt);
            jl_value_t *ndp = jl_tparam1(aty_dt);
            if (!jl_has_free_typevars(ety) && (jl_is_long(ndp) || nargs == 4)) {
                if (jl_subtype(val.typ, ety)) { // TODO: probably should just convert this to a type-assert
                    size_t elsz = 0, al = 0;
                    int union_max = jl_islayout_inline(ety, &elsz, &al);
                    bool isboxed = (union_max == 0);
                    if (isboxed)
                        ety = (jl_value_t*)jl_any_type;
                    jl_value_t *ary_ex = jl_exprarg(ex, 2);
                    ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                    jl_value_t *boundscheck = argv[1].constant;
                    Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[4], nargs - 3, boundscheck);
                    if (!isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
                        // no-op
                    }
                    else {
                        PHINode *data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed || (jl_is_datatype(ety) && ((jl_datatype_t*)ety)->layout->npointers > 0)) { // if elements are just bits, don't need a write barrier
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
                            Instruction *own_ptr;
                            if (jl_is_long(ndp)) {
                                own_ptr = ctx.builder.CreateLoad(T_prjlvalue,
                                        ctx.builder.CreateConstGEP1_32(T_prjlvalue,
                                            emit_bitcast(ctx, decay_derived(aryv), T_pprjlvalue),
                                            jl_array_data_owner_offset(nd) / sizeof(jl_value_t*)));
                                tbaa_decorate(tbaa_const, maybe_mark_load_dereferenceable(own_ptr, false, (jl_value_t*)jl_array_any_type));
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
                        if (jl_is_uniontype(ety)) {
                            Type *AT = ArrayType::get(IntegerType::get(jl_LLVMContext, 8 * al), (elsz + al - 1) / al);
                            Value *data = emit_bitcast(ctx, emit_arrayptr(ctx, ary, ary_ex), AT->getPointerTo());
                            // compute tindex from val
                            jl_cgval_t rhs_union = convert_julia_type(ctx, val, ety);
                            Value *tindex = compute_tindex_unboxed(ctx, rhs_union, ety);
                            tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(T_int8, 1));
                            Value *ndims = (nd == -1 ? emit_arrayndims(ctx, ary) : ConstantInt::get(T_int16, nd));
                            Value *is_vector = ctx.builder.CreateICmpEQ(ndims, ConstantInt::get(T_int16, 1));
                            Value *offset = emit_arrayoffset(ctx, ary, nd);
                            Value *selidx_v = ctx.builder.CreateSub(emit_vectormaxsize(ctx, ary), ctx.builder.CreateZExt(offset, T_size));
                            Value *selidx_m = emit_arraylen(ctx, ary);
                            Value *selidx = ctx.builder.CreateSelect(is_vector, selidx_v, selidx_m);
                            Value *ptindex = ctx.builder.CreateInBoundsGEP(AT, data, selidx);
                            ptindex = emit_bitcast(ctx, ptindex, T_pint8);
                            ptindex = ctx.builder.CreateInBoundsGEP(T_int8, ptindex, offset);
                            ptindex = ctx.builder.CreateInBoundsGEP(T_int8, ptindex, idx);
                            tbaa_decorate(tbaa_arrayselbyte, ctx.builder.CreateStore(tindex, ptindex));
                            if (jl_is_datatype(val.typ) && jl_datatype_size(val.typ) == 0) {
                                // no-op
                            }
                            else {
                                // copy data
                                Value *addr = ctx.builder.CreateInBoundsGEP(AT, data, idx);
                                emit_unionmove(ctx, addr, tbaa_arraybuf, val, nullptr);
                            }
                        }
                        else {
                            typed_store(ctx,
                                        emit_arrayptr(ctx, ary, ary_ex, isboxed),
                                        idx, val, ety,
                                        !isboxed ? tbaa_arraybuf : tbaa_ptrarraybuf,
                                        ctx.aliasscope, data_owner, 0);
                        }
                    }
                    *ret = ary;
                    return true;
                }
            }
        }
    }

    else if (f == jl_builtin_getfield && (nargs == 2 || nargs == 3)) {
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
                    if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot && ctx.argArray) {
                        Value *valen = emit_n_varargs(ctx);
                        jl_cgval_t va_ary( // fake instantiation of a cgval, in order to call emit_bounds_check
                                ctx.builder.CreateInBoundsGEP(T_prjlvalue, ctx.argArray, ConstantInt::get(T_size, ctx.nReqArgs)),
                                NULL, false, NULL, NULL);
                        Value *idx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                        jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                        idx = emit_bounds_check(ctx, va_ary, NULL, idx, valen, boundscheck);
                        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(T_size, ctx.nReqArgs));
                        Instruction *v = ctx.builder.CreateLoad(T_prjlvalue, ctx.builder.CreateInBoundsGEP(ctx.argArray, idx));
                        // if we know the result type of this load, we will mark that information here too
                        tbaa_decorate(tbaa_value, maybe_mark_load_dereferenceable(v, false, rt));
                        *ret = mark_julia_type(ctx, v, /*boxed*/ true, rt);
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
                        jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                        if (emit_getfield_unknownidx(ctx, ret, obj, vidx, utt, boundscheck)) {
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
                        jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                        if (!bounds_check_enabled(ctx, boundscheck)) {
                            vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(T_size, 1));
                        } else {
                            vidx = emit_bounds_check(ctx, obj, (jl_value_t*)obj.typ, vidx,
                                emit_datatype_nfields(ctx, emit_typeof_boxed(ctx, obj)),
                                jl_true);
                        }
                        bool isboxed = !jl_datatype_isinlinealloc(jt);
                        Value *ptr = maybe_decay_tracked(data_pointer(ctx, obj));
                        *ret = typed_load(ctx, ptr, vidx,
                                isboxed ? (jl_value_t*)jl_any_type : jt,
                                obj.tbaa, nullptr, false);
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
            if (jl_is_concrete_type(tp0)) {
                *ret = mark_julia_type(ctx, ConstantInt::get(T_size, jl_datatype_nfields(tp0)), false, jl_long_type);
                return true;
            }
        }
        else if (jl_is_concrete_type(obj.typ) || obj.constant) {
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

    else if (f == jl_builtin_fieldtype && (nargs == 2 || nargs == 3)) {
        const jl_cgval_t &typ = argv[1];
        const jl_cgval_t &fld = argv[2];
        if ((jl_is_type_type(typ.typ) && jl_is_concrete_type(jl_tparam0(typ.typ))) ||
                (typ.constant && jl_is_concrete_type(typ.constant))) {
            if (fld.typ == (jl_value_t*)jl_long_type) {
                assert(typ.isboxed);
                Value *tyv = boxed(ctx, typ);
                Value *types_svec = emit_datatype_types(ctx, tyv);
                Value *types_len = emit_datatype_nfields(ctx, tyv);
                Value *idx = emit_unbox(ctx, T_size, fld, (jl_value_t*)jl_long_type);
                jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                emit_bounds_check(ctx, typ, (jl_value_t*)jl_datatype_type, idx, types_len, boundscheck);
                Value *fieldtyp_p = ctx.builder.CreateInBoundsGEP(T_prjlvalue, decay_derived(emit_bitcast(ctx, types_svec, T_pprjlvalue)), idx);
                Value *fieldtyp = tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_prjlvalue, fieldtyp_p));
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
            Value *len = tbaa_decorate(tbaa_mutab, ctx.builder.CreateLoad(T_size, ptr));
            if (sty == jl_simplevector_type) {
                len = ctx.builder.CreateMul(len, ConstantInt::get(T_size, sizeof(void*)));
                len = ctx.builder.CreateAdd(len, ConstantInt::get(T_size, sizeof(void*)));
            }
            *ret = mark_julia_type(ctx, len, false, jl_long_type);
            return true;
        }
        else if (jl_is_datatype(sty) && sty->name == jl_array_typename) {
            auto len = emit_arraylen(ctx, obj);
            jl_value_t *ety = jl_tparam0(sty);
            Value *elsize;
            size_t elsz = 0, al = 0;
            int union_max = jl_islayout_inline(ety, &elsz, &al);
            bool isboxed = (union_max == 0);
            if (!jl_has_free_typevars(ety)) {
                if (isboxed) {
                    elsize = ConstantInt::get(T_size, sizeof(void*));
                }
                else {
                    elsize = ConstantInt::get(T_size, elsz);
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
            if (jl_is_concrete_type((jl_value_t*)sty) ||
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
        if (jl_is_type_type((jl_value_t*)stt)) {
            // the representation type of Type{T} is either typeof(T), or unknown
            // TODO: could use `issingletontype` predicate here, providing better type knowledge
            // than only handling DataType
            if (jl_is_concrete_type(jl_tparam0(stt)))
                stt = (jl_datatype_t*)jl_typeof(jl_tparam0(stt));
            else
                return false;
        }
        if (!jl_is_concrete_type((jl_value_t*)stt) || jl_is_array_type(stt) ||
            stt == jl_module_type) { // TODO: use ->layout here instead of concrete_type
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
        else if (fieldidx < stt->ninitialized) {
            *ret = mark_julia_const(jl_true);
        }
        else if (jl_field_isptr(stt, fieldidx) || jl_type_hasptr(jl_field_type(stt, fieldidx))) {
            Value *fldv;
            size_t offs = jl_field_offset(stt, fieldidx) / sizeof(jl_value_t*);
            if (obj.ispointer()) {
                if (!jl_field_isptr(stt, fieldidx))
                    offs += ((jl_datatype_t*)jl_field_type(stt, fieldidx))->layout->first_ptr;
                Value *ptr = emit_bitcast(ctx, maybe_decay_tracked(data_pointer(ctx, obj)), T_pprjlvalue);
                Value *addr = ctx.builder.CreateConstInBoundsGEP1_32(T_prjlvalue, ptr, offs);
                // emit this using the same type as emit_getfield_knownidx
                // so that LLVM may be able to load-load forward them and fold the result
                fldv = tbaa_decorate(obj.tbaa, ctx.builder.CreateLoad(T_prjlvalue, addr));
            }
            else {
                fldv = ctx.builder.CreateExtractValue(obj.V, offs);
                if (!jl_field_isptr(stt, fieldidx)) {
                    fldv = extract_first_ptr(ctx, fldv);
                    assert(fldv);
                }
            }
            Value *isdef = ctx.builder.CreateIsNotNull(fldv);
            *ret = mark_julia_type(ctx, isdef, false, jl_bool_type);
        }
        else {
            *ret = mark_julia_const(jl_true);
        }
        return true;
    }
    return false;
}

static CallInst *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                             jl_cgval_t *argv, size_t nargs, CallingConv::ID cc)
{
    // emit arguments
    SmallVector<Value*, 3> theArgs;
    SmallVector<Type*, 3> argsT;
    if (theF) {
        theArgs.push_back(theF);
        argsT.push_back(T_prjlvalue);
    }
    for (size_t i = 0; i < nargs; i++) {
        Value *arg = maybe_decay_untracked(boxed(ctx, argv[i]));
        theArgs.push_back(arg);
        argsT.push_back(T_prjlvalue);
    }
    FunctionType *FTy = FunctionType::get(T_prjlvalue, argsT, false);
    CallInst *result = ctx.builder.CreateCall(FTy,
        ctx.builder.CreateBitCast(prepare_call(theFptr), FTy->getPointerTo()),
        theArgs);
    add_return_attr(result, Attribute::NonNull);
    result->setCallingConv(cc);
    return result;
}


static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, jl_method_instance_t *mi, jl_value_t *jlretty, StringRef specFunctionObject,
                                          jl_cgval_t *argv, size_t nargs, jl_returninfo_t::CallingConv *cc, unsigned *return_roots, jl_value_t *inferred_retty)
{
    // emit specialized call site
    jl_returninfo_t returninfo = get_specsig_function(ctx, jl_Module, specFunctionObject, mi->specTypes, jlretty);
    FunctionType *cft = returninfo.decl->getFunctionType();
    *cc = returninfo.cc;
    *return_roots = returninfo.return_roots;

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
        result = emit_static_alloca(ctx, cft->getParamType(0)->getPointerElementType());
        argvals[idx] = result;
        idx++;
        break;
    case jl_returninfo_t::Union:
        result = emit_static_alloca(ctx, ArrayType::get(T_int8, returninfo.union_bytes));
        if (returninfo.union_align > 1)
            result->setAlignment(Align(returninfo.union_align));
        argvals[idx] = result;
        idx++;
        break;
    }

    if (returninfo.return_roots) {
        AllocaInst *return_roots = emit_static_alloca(ctx, T_prjlvalue);
        return_roots->setOperand(0, ConstantInt::get(T_int32, returninfo.return_roots));
        argvals[idx] = return_roots;
        idx++;
    }

    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_nth_slot_type(mi->specTypes, i);
        bool isboxed = deserves_argbox(jt);
        Type *et = isboxed ?  T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (type_is_ghost(et))
            continue;
        assert(idx < nfargs);
        Type *at = cft->getParamType(idx);
        jl_cgval_t arg = argv[i];
        if (isboxed) {
            assert(at == T_prjlvalue && et == T_prjlvalue);
            argvals[idx] = maybe_decay_untracked(boxed(ctx, arg));
        }
        else if (et->isAggregateType()) {
            if (!arg.ispointer())
                arg = value_to_pointer(ctx, arg);
            // can lazy load on demand, no copy needed
            assert(at == PointerType::get(et, AddressSpace::Derived));
            argvals[idx] = decay_derived(maybe_bitcast(ctx,
                data_pointer(ctx, arg), at));
        }
        else {
            assert(at == et);
            Value *val = emit_unbox(ctx, et, arg, jt);
            if (!val) {
                // There was a type mismatch of some sort - exit early
                CreateTrap(ctx.builder);
                return jl_cgval_t();
            }
            argvals[idx] = val;
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
            retval.Vboxed = box;
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

static jl_cgval_t emit_call_specfun_boxed(jl_codectx_t &ctx, StringRef specFunctionObject,
                                          jl_cgval_t *argv, size_t nargs, jl_value_t *inferred_retty)
{
    auto theFptr = jl_Module->getOrInsertFunction(specFunctionObject, jl_func_sig)
#if JL_LLVM_VERSION >= 90000
                .getCallee();
#else
                ;
#endif
    if (auto F = dyn_cast<Function>(theFptr->stripPointerCasts())) {
        add_return_attr(F, Attribute::NonNull);
        F->addFnAttr(Thunk);
    }
    Value *ret = emit_jlcall(ctx, theFptr, nullptr, argv, nargs, JLCALL_F_CC);
    return mark_julia_type(ctx, ret, true, inferred_retty);
}

static jl_cgval_t emit_invoke(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
{
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 2);

    jl_cgval_t lival = emit_expr(ctx, args[0]);
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t();
    }

    bool handled = false;
    jl_cgval_t result;
    if (lival.constant) {
        jl_method_instance_t *mi = (jl_method_instance_t*)lival.constant;
        assert(jl_is_method_instance(mi));
        if (mi == ctx.linfo) {
            // handle self-recursion specially
            jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
            FunctionType *ft = ctx.f->getFunctionType();
            StringRef protoname = ctx.f->getName();
            if (ft == jl_func_sig) {
                result = emit_call_specfun_boxed(ctx, protoname, argv, nargs, rt);
                handled = true;
            }
            else if (ft != jl_func_sig_sparams) {
                unsigned return_roots = 0;
                result = emit_call_specfun_other(ctx, mi, ctx.rettype, protoname, argv, nargs, &cc, &return_roots, rt);
                handled = true;
            }
        }
        else {
            jl_value_t *ci = jl_rettype_inferred(mi, ctx.world, ctx.world); // TODO: need to use the right pair world here
            jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
            if (ci != jl_nothing && codeinst->invoke != jl_fptr_sparam) { // check if we know we definitely can't handle this specptr
                if (codeinst->invoke == jl_fptr_const_return) {
                    result = mark_julia_const(codeinst->rettype_const);
                    handled = true;
                }
                else {
                    bool specsig, needsparams;
                    std::tie(specsig, needsparams) = uses_specsig(mi, codeinst->rettype, ctx.params->prefer_specsig);
                    std::string name;
                    StringRef protoname;
                    bool need_to_emit = true;
                    if (ctx.use_cache) {
                        // optimization: emit the correct name immediately, if we know it
                        // TODO: use `emitted` map here too to try to consolidate names?
                        if (codeinst->specptr.fptr) {
                            if (specsig ? codeinst->isspecsig : codeinst->invoke == jl_fptr_args) {
                                protoname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)codeinst->specptr.fptr, codeinst);
                                need_to_emit = false;
                            }
                        }
                    }
                    if (need_to_emit) {
                        std::stringstream namestream;
                        namestream << (specsig ? "j_" : "j1_") << name_from_method_instance(mi) << "_" << globalUnique++;
                        name = namestream.str();
                        protoname = StringRef(name);
                    }
                    jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
                    unsigned return_roots = 0;
                    if (specsig)
                        result = emit_call_specfun_other(ctx, mi, codeinst->rettype, protoname, argv, nargs, &cc, &return_roots, rt);
                    else
                        result = emit_call_specfun_boxed(ctx, protoname, argv, nargs, rt);
                    handled = true;
                    if (need_to_emit) {
                        Function *trampoline_decl = cast<Function>(jl_Module->getNamedValue(protoname));
                        ctx.call_targets.push_back(std::make_tuple(codeinst, cc, return_roots, trampoline_decl, specsig));
                    }
                }
            }
        }
    }
    if (!handled) {
        Value *r = emit_jlcall(ctx, prepare_call(jlinvoke_func), boxed(ctx, lival), argv, nargs, JLCALL_F2_CC);
        result = mark_julia_type(ctx, r, true, rt);
    }
    if (result.typ == jl_bottom_type)
        CreateTrap(ctx.builder);
    return result;
}

static jl_cgval_t emit_call(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
{
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_dim0(ex->args);
    assert(nargs >= 1);
    jl_cgval_t f = emit_expr(ctx, args[0]);

    if (f.constant && jl_typeis(f.constant, jl_intrinsic_type)) {
        JL_I::intrinsic fi = (intrinsic)*(uint32_t*)jl_data_ptr(f.constant);
        return emit_intrinsic(ctx, fi, args, nargs - 1);
    }

    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    argv[0] = f;
    for (size_t i = 1; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t(); // anything past here is unreachable
    }

    if (f.constant && jl_isa(f.constant, (jl_value_t*)jl_builtin_type)) {
        if (f.constant == jl_builtin_ifelse && nargs == 4)
            return emit_ifelse(ctx, argv[1], argv[2], argv[3], rt);
        jl_cgval_t result;
        bool handled = emit_builtin_call(ctx, &result, f.constant, argv, nargs - 1, rt, ex);
        if (handled) {
            return result;
        }

        // special case for known builtin not handled by emit_builtin_call
        std::map<jl_fptr_args_t, Function*>::iterator it = builtin_func_map.find(jl_get_builtin_fptr(f.constant));
        if (it != builtin_func_map.end()) {
            Value *theFptr = it->second;
            Value *ret = emit_jlcall(ctx, theFptr, maybe_decay_untracked(V_null), &argv[1], nargs - 1, JLCALL_F_CC);
            return mark_julia_type(ctx, ret, true, rt);
        }
    }

    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, nargs, JLCALL_F_CC);
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
        b = jl_get_binding_wr(m, s, 0);
        assert(b != NULL);
        if (b->owner != m) {
            char *msg;
            (void)asprintf(&msg, "cannot assign a value to variable %s.%s from module %s",
                    jl_symbol_name(b->owner->name), jl_symbol_name(s), jl_symbol_name(m->name));
            emit_error(ctx, msg);
            free(msg);
        }
    }
    else {
        b = jl_get_binding(m, s);
        if (b == NULL) {
            // var not found. switch to delayed lookup.
            std::stringstream name;
            name << "delayedvar" << globalUnique++;
            Constant *initnul = V_null;
            GlobalVariable *bindinggv = new GlobalVariable(*ctx.f->getParent(), T_pjlvalue,
                    false, GlobalVariable::InternalLinkage,
                    initnul, name.str());
            Value *cachedval = ctx.builder.CreateLoad(T_pjlvalue, bindinggv);
            BasicBlock *have_val = BasicBlock::Create(jl_LLVMContext, "found"),
                *not_found = BasicBlock::Create(jl_LLVMContext, "notfound");
            BasicBlock *currentbb = ctx.builder.GetInsertBlock();
            ctx.builder.CreateCondBr(ctx.builder.CreateICmpNE(cachedval, initnul), have_val, not_found);
            ctx.f->getBasicBlockList().push_back(not_found);
            ctx.builder.SetInsertPoint(not_found);
            Value *bval = ctx.builder.CreateCall(prepare_call(jlgetbindingorerror_func),
                    { literal_pointer_val(ctx, (jl_value_t*)m),
                      literal_pointer_val(ctx, (jl_value_t*)s) });
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
    LoadInst *v = ctx.builder.CreateLoad(T_prjlvalue, bp);
    if (isvol)
        v->setVolatile(true);
    if (tbaa)
        tbaa_decorate(tbaa, v);
    undef_var_error_ifnot(ctx, ctx.builder.CreateIsNotNull(v), name);
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
    Value *sp = tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_prjlvalue, bp));
    Value *isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp),
            maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jl_tvar_type)));
    jl_unionall_t *sparam = (jl_unionall_t*)ctx.linfo->def.method->sig;
    for (size_t j = 0; j < i; j++) {
        sparam = (jl_unionall_t*)sparam->body;
        assert(jl_is_unionall(sparam));
    }
    undef_var_error_ifnot(ctx, isnull, sparam->var->name);
    return mark_julia_type(ctx, sp, true, jl_any_type);
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
        return mark_julia_type(ctx, tbaa_decorate(tbaa_binding, ctx.builder.CreateLoad(T_prjlvalue, bp)), true, jl_any_type);
    }
    return emit_checked_var(ctx, bp, sym, false, tbaa_binding);
}

static jl_cgval_t emit_isdefined(jl_codectx_t &ctx, jl_value_t *sym)
{
    Value *isnull = NULL;
    if (jl_is_slot(sym)) {
        size_t sl = jl_slot_number(sym) - 1;
        jl_varinfo_t &vi = ctx.slots[sl];
        if (!vi.usedUndef)
            return mark_julia_const(jl_true);
        if (vi.boxroot == NULL || vi.pTIndex != NULL) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateLoad(T_int1, vi.defFlag, vi.isVolatile);
        }
        if (vi.boxroot != NULL) {
            Value *boxed = ctx.builder.CreateLoad(T_prjlvalue, vi.boxroot, vi.isVolatile);
            Value *box_isnull = ctx.builder.CreateICmpNE(boxed, maybe_decay_untracked(V_null));
            if (vi.pTIndex) {
                // value is either boxed in the stack slot, or unboxed in value
                // as indicated by testing (pTIndex & 0x80)
                Value *tindex = ctx.builder.CreateLoad(T_int8, vi.pTIndex, vi.isVolatile);
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
    else if (jl_is_expr(sym)) {
        assert(((jl_expr_t*)sym)->head == static_parameter_sym && "malformed isdefined expression");
        size_t i = jl_unbox_long(jl_exprarg(sym, 0)) - 1;
        if (jl_svec_len(ctx.linfo->sparam_vals) > 0) {
            jl_value_t *e = jl_svecref(ctx.linfo->sparam_vals, i);
            if (!jl_is_typevar(e)) {
                return mark_julia_const(jl_true);
            }
        }
        assert(ctx.spvals_ptr != NULL);
        Value *bp = ctx.builder.CreateConstInBoundsGEP1_32(
                T_prjlvalue,
                ctx.spvals_ptr,
                i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
        Value *sp = tbaa_decorate(tbaa_const, ctx.builder.CreateLoad(T_prjlvalue, bp));
        isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp),
            maybe_decay_untracked(literal_pointer_val(ctx, (jl_value_t*)jl_tvar_type)));
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
            Instruction *v = ctx.builder.CreateLoad(T_prjlvalue, bp);
            tbaa_decorate(tbaa_binding, v);
            isnull = ctx.builder.CreateICmpNE(v, maybe_decay_untracked(V_null));
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

static jl_cgval_t emit_varinfo(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_sym_t *varname, jl_value_t *better_typ=NULL) {
    jl_value_t *typ = better_typ ? better_typ : vi.value.typ;
    jl_cgval_t v;
    Value *isnull = NULL;
    if (vi.boxroot == NULL || vi.pTIndex != NULL) {
        if ((!vi.isVolatile && vi.isSA) || vi.isArgument || vi.value.constant || !vi.value.V) {
            v = vi.value;
            if (vi.pTIndex)
                v.TIndex = ctx.builder.CreateLoad(T_int8, vi.pTIndex);
        }
        else {
            // copy value to a non-mutable (non-volatile SSA) location
            AllocaInst *varslot = cast<AllocaInst>(vi.value.V);
            Type *T = varslot->getAllocatedType();
            assert(!varslot->isArrayAllocation() && "variables not expected to be VLA");
            AllocaInst *ssaslot = cast<AllocaInst>(varslot->clone());
            ssaslot->insertAfter(varslot);
            if (vi.isVolatile) {
                Value *unbox = ctx.builder.CreateAlignedLoad(ssaslot->getAllocatedType(), varslot, varslot->getAlignment(), true);
                ctx.builder.CreateAlignedStore(unbox, ssaslot, ssaslot->getAlignment());
            }
            else {
                const DataLayout &DL = jl_data_layout;
                uint64_t sz = DL.getTypeStoreSize(T);
                emit_memcpy(ctx, ssaslot, tbaa_stack, vi.value, sz, ssaslot->getAlignment());
            }
            Value *tindex = NULL;
            if (vi.pTIndex)
                tindex = ctx.builder.CreateLoad(T_int8, vi.pTIndex, vi.isVolatile);
            v = mark_julia_slot(ssaslot, vi.value.typ, tindex, tbaa_stack);
        }
        if (vi.boxroot == NULL)
            v = update_julia_type(ctx, v, typ);
        if (vi.usedUndef) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateLoad(T_int1, vi.defFlag, vi.isVolatile);
        }
    }
    if (vi.boxroot != NULL) {
        Instruction *boxed = ctx.builder.CreateLoad(T_prjlvalue, vi.boxroot, vi.isVolatile);
        Value *box_isnull = NULL;
        if (vi.usedUndef)
            box_isnull = ctx.builder.CreateICmpNE(boxed, maybe_decay_untracked(V_null));
        maybe_mark_load_dereferenceable(boxed, vi.usedUndef || vi.pTIndex, typ);
        if (vi.pTIndex) {
            // value is either boxed in the stack slot, or unboxed in value
            // as indicated by testing (pTIndex & 0x80)
            Value *load_unbox = ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(T_int8, 0x80)),
                        ConstantInt::get(T_int8, 0));
            if (vi.usedUndef)
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            if (v.V) { // v.V will be null if it is a union of all ghost values
                v.V = ctx.builder.CreateSelect(load_unbox, emit_bitcast(ctx,
                    decay_derived(v.V), boxed->getType()), decay_derived(boxed));
            } else
                v.V = boxed;
            v.Vboxed = boxed;
            v = update_julia_type(ctx, v, typ);
        }
        else {
            v = mark_julia_type(ctx, boxed, true, typ);
            if (vi.usedUndef)
                isnull = box_isnull;
        }
    }
    if (isnull)
        undef_var_error_ifnot(ctx, isnull, varname);
    return v;
}

static jl_cgval_t emit_local(jl_codectx_t &ctx, jl_value_t *slotload)
{
    size_t sl = jl_slot_number(slotload) - 1;
    jl_varinfo_t &vi = ctx.slots[sl];
    jl_sym_t *sym = slot_symbol(ctx, sl);
    jl_value_t *typ = NULL;
    if (jl_typeis(slotload, jl_typedslot_type)) {
        // use the better type from inference for this load
        typ = jl_typedslot_get_type(slotload);
        if (jl_is_typevar(typ))
            typ = ((jl_tvar_t*)typ)->ub;
    }
    return emit_varinfo(ctx, vi, sym, typ);
}

static void emit_vi_assignment_unboxed(jl_codectx_t &ctx, jl_varinfo_t &vi, Value *isboxed, jl_cgval_t rval_info)
{
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
                    Type *store_ty = julia_type_to_llvm(ctx, rval_info.constant ? jl_typeof(rval_info.constant) : rval_info.typ);
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
            if (vi.pTIndex == NULL) {
                assert(jl_is_concrete_type(vi.value.typ));
                // Sometimes we can get into situations where the LHS and RHS
                // are the same slot. We're not allowed to memcpy in that case
                // due to LLVM bugs.
                // This check should probably mostly catch the relevant situations.
                if (vi.value.V != rval_info.V) {
                    Value *copy_bytes = ConstantInt::get(T_int32, jl_datatype_size(vi.value.typ));
                    emit_memcpy(ctx, vi.value.V, tbaa_stack, rval_info, copy_bytes,
                                julia_alignment(rval_info.typ), vi.isVolatile);
                }
            }
            else {
                emit_unionmove(ctx, vi.value.V, tbaa_stack, rval_info, isboxed, vi.isVolatile);
            }
        }
    }
    else {
        assert(vi.pTIndex == NULL);
    }
}

static void emit_phinode_assign(jl_codectx_t &ctx, ssize_t idx, jl_value_t *r)
{
    jl_value_t *ssavalue_types = (jl_value_t*)ctx.source->ssavaluetypes;
    assert(jl_is_array(ssavalue_types));
    jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(r, 0);
    jl_value_t *phiType = jl_array_ptr_ref(ssavalue_types, idx);
    BasicBlock *BB = ctx.builder.GetInsertBlock();
    auto InsertPt = BB->getFirstInsertionPt();
    if (phiType == jl_bottom_type) {
        return;
    }
    AllocaInst *dest = nullptr;
    // N.B.: For any memory space, used as a phi,
    // we need to emit space twice here. The reason for this is that
    // phi nodes may be arguments of other phi nodes, so if we don't
    // have two buffers, one may be overwritten before its value is
    // used. Hopefully LLVM will be able to fold this back where legal.
    if (jl_is_uniontype(phiType)) {
        bool allunbox;
        size_t min_align, nbytes;
        dest = try_emit_union_alloca(ctx, ((jl_uniontype_t*)phiType), allunbox, min_align, nbytes);
        if (dest) {
            Instruction *phi = dest->clone();
            phi->insertAfter(dest);
            PHINode *Tindex_phi = PHINode::Create(T_int8, jl_array_len(edges), "tindex_phi");
            BB->getInstList().insert(InsertPt, Tindex_phi);
            PHINode *ptr_phi = PHINode::Create(T_prjlvalue, jl_array_len(edges), "ptr_phi");
            BB->getInstList().insert(InsertPt, ptr_phi);
            Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(Tindex_phi, ConstantInt::get(T_int8, 0x80)),
                    ConstantInt::get(T_int8, 0));
            ctx.builder.CreateMemCpy(phi, min_align, dest, 0, nbytes, false);
            ctx.builder.CreateLifetimeEnd(dest);
            Value *ptr = ctx.builder.CreateSelect(isboxed,
                maybe_bitcast(ctx, decay_derived(ptr_phi), T_pint8),
                maybe_bitcast(ctx, decay_derived(phi), T_pint8));
            jl_cgval_t val = mark_julia_slot(ptr, phiType, Tindex_phi, tbaa_stack); // XXX: this TBAA is wrong for ptr_phi
            val.Vboxed = ptr_phi;
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, ptr_phi, r));
            ctx.SAvalues.at(idx) = val;
            ctx.ssavalue_assigned.at(idx) = true;
            return;
        }
        else if (allunbox) {
            PHINode *Tindex_phi = PHINode::Create(T_int8, jl_array_len(edges), "tindex_phi");
            BB->getInstList().insert(InsertPt, Tindex_phi);
            jl_cgval_t val = mark_julia_slot(NULL, phiType, Tindex_phi, tbaa_stack);
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, (PHINode*)NULL, r));
            ctx.SAvalues.at(idx) = val;
            ctx.ssavalue_assigned.at(idx) = true;
            return;
        }
    }
    bool isboxed = !deserves_stack(phiType);
    Type *vtype = isboxed ? T_prjlvalue : julia_type_to_llvm(ctx, phiType);
    // The frontend should really not emit this, but we allow it
    // for convenience.
    if (type_is_ghost(vtype)) {
        assert(jl_is_datatype(phiType) && ((jl_datatype_t*)phiType)->instance);
        // Skip adding it to the PhiNodes list, since we didn't create one.
        ctx.SAvalues.at(idx) = mark_julia_const(((jl_datatype_t*)phiType)->instance);
        ctx.ssavalue_assigned.at(idx) = true;
        return;
    }
    jl_cgval_t slot;
    PHINode *value_phi = NULL;
    if (vtype->isAggregateType() && CountTrackedPointers(vtype).count == 0) {
        // the value will be moved into dest in the predecessor critical block.
        // here it's moved into phi in the successor (from dest)
        dest = emit_static_alloca(ctx, vtype);
        Value *phi = emit_static_alloca(ctx, vtype);
        ctx.builder.CreateMemCpy(phi, julia_alignment(phiType),
             dest, 0,
             jl_datatype_size(phiType), false);
        ctx.builder.CreateLifetimeEnd(dest);
        slot = mark_julia_slot(phi, phiType, NULL, tbaa_stack);
    }
    else {
        value_phi = PHINode::Create(vtype, jl_array_len(edges), "value_phi");
        BB->getInstList().insert(InsertPt, value_phi);
        slot = mark_julia_type(ctx, value_phi, isboxed, phiType);
    }
    ctx.PhiNodes.push_back(std::make_tuple(slot, BB, dest, value_phi, r));
    ctx.SAvalues.at(idx) = slot;
    ctx.ssavalue_assigned.at(idx) = true;
    return;
}

static void emit_ssaval_assign(jl_codectx_t &ctx, ssize_t idx, jl_value_t *r)
{
    assert(!ctx.ssavalue_assigned.at(idx));
    if (jl_is_phinode(r)) {
        return emit_phinode_assign(ctx, idx, r);
    }

    jl_cgval_t slot;
    if (jl_is_phicnode(r)) {
        jl_varinfo_t &vi = ctx.phic_slots[idx];
        slot = emit_varinfo(ctx, vi, jl_symbol("phic"));
    } else {
        slot = emit_expr(ctx, r, idx); // slot could be a jl_value_t (unboxed) or jl_value_t* (ispointer)
    }
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
    ctx.SAvalues.at(idx) = slot; // now SAvalues[idx] contains the SAvalue
    ctx.ssavalue_assigned.at(idx) = true;
}

static void emit_varinfo_assign(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_cgval_t rval_info, jl_value_t *l=NULL)
{
    if (!vi.used)
        return;

    // convert rval-type to lval-type
    jl_value_t *slot_type = vi.value.typ;
    rval_info = convert_julia_type(ctx, rval_info, slot_type);
    if (rval_info.typ == jl_bottom_type)
        return;

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
            else
                rval_info.TIndex = tindex;
        }
        ctx.builder.CreateStore(tindex, vi.pTIndex, vi.isVolatile);
    }

    // store boxed variables
    Value *isboxed = NULL;
    if (vi.boxroot) {
        Value *rval;
        if (vi.pTIndex && rval_info.TIndex) {
            ctx.builder.CreateStore(rval_info.TIndex, vi.pTIndex, vi.isVolatile);
            isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(rval_info.TIndex, ConstantInt::get(T_int8, 0x80)),
                    ConstantInt::get(T_int8, 0));
            rval = maybe_decay_untracked(rval_info.Vboxed ? rval_info.Vboxed : V_null);
            assert(!vi.value.constant);
        }
        else {
            assert(!vi.pTIndex || rval_info.isboxed || rval_info.constant);
            rval = maybe_decay_untracked(boxed(ctx, rval_info));
        }
        ctx.builder.CreateStore(maybe_decay_untracked(rval), vi.boxroot, vi.isVolatile);
    }

    // store unboxed variables
    if (!vi.boxroot || (vi.pTIndex && rval_info.TIndex)) {
        emit_vi_assignment_unboxed(ctx, vi, isboxed, rval_info);
    }
}

static void emit_assignment(jl_codectx_t &ctx, jl_value_t *l, jl_value_t *r, ssize_t ssaval)
{
    assert(!jl_is_ssavalue(l));

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
        assert(bnd);
        Value *rval = mark_callee_rooted(boxed(ctx, emit_expr(ctx, r, ssaval)));
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
    jl_cgval_t rval_info = emit_expr(ctx, r, ssaval);
    emit_varinfo_assign(ctx, vi, rval_info, l);
}

// --- convert expression to code ---

static jl_cgval_t emit_cfunction(jl_codectx_t &ctx, jl_value_t *output_type, const jl_cgval_t &fexpr, jl_value_t *rt, jl_svec_t *argt);

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

static void emit_stmtpos(jl_codectx_t &ctx, jl_value_t *expr, int ssaval_result)
{
    if (jl_is_ssavalue(expr) && ssaval_result == -1)
        return; // value not used, no point in attempting codegen for it
    if (jl_is_slot(expr) && ssaval_result == -1) {
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
        assert(ssaval_result != -1);
        emit_ssaval_assign(ctx, ssaval_result, expr);
        return;
    }
    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    jl_sym_t *head = ex->head;
    if (head == meta_sym || head == inbounds_sym || head == coverageeffect_sym
            || head == aliasscope_sym || head == popaliasscope_sym) {
        // some expression types are metadata and can be ignored
        // in statement position
        return;
    }
    else if (head == leave_sym) {
        assert(jl_is_long(args[0]));
        ctx.builder.CreateCall(prepare_call(jlleave_func),
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else if (head == pop_exception_sym) {
        jl_cgval_t excstack_state = emit_expr(ctx, jl_exprarg(expr, 0));
        assert(excstack_state.V && excstack_state.V->getType() == T_size);
        ctx.builder.CreateCall(prepare_call(jl_restore_excstack_func), excstack_state.V);
        return;
    }
    else {
        if (!jl_is_method(ctx.linfo->def.method)) {
            // TODO: inference is invalid if this has an effect
            Value *world = ctx.builder.CreateLoad(prepare_global(jlgetworld_global));
            ctx.builder.CreateStore(world, ctx.world_age_field);
        }
        assert(ssaval_result != -1);
        emit_ssaval_assign(ctx, ssaval_result, expr);
    }
}

// `expr` is not clobbered in JL_TRY
JL_GCC_IGNORE_START("-Wclobbered")
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaval)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_global(ctx, sym);
    }
    if (jl_is_slot(expr)) {
        return emit_local(ctx, expr);
    }
    if (jl_is_ssavalue(expr)) {
        ssize_t idx = ((jl_ssavalue_t*)expr)->id - 1;
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
    if (jl_is_linenode(expr)) {
        jl_error("LineNumberNode in value position");
    }
    if (jl_is_gotonode(expr)) {
        jl_error("GotoNode in value position");
    }
    if (jl_is_pinode(expr)) {
        return convert_julia_type(ctx, emit_expr(ctx, jl_fieldref_noalloc(expr, 0)), jl_fieldref_noalloc(expr, 1));
    }
    if (!jl_is_expr(expr)) {
        int needroot = true;
        if (jl_is_quotenode(expr)) {
            expr = jl_fieldref_noalloc(expr,0);
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
        else if (jl_is_uint8(expr)) {
            expr = jl_box_uint8(jl_unbox_uint8(expr));
            needroot = false;
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
    else if (head == throw_undef_if_not_sym) {
        jl_sym_t *var = (jl_sym_t*)args[0];
        Value *cond = ctx.builder.CreateTrunc(emit_unbox(ctx, T_int8, emit_expr(ctx, args[1]), (jl_value_t*)jl_bool_type), T_int1);
        if (var == getfield_undefref_sym) {
            raise_exception_unless(ctx, cond,
                literal_pointer_val(ctx, jl_undefref_exception));
        }
        else {
            undef_var_error_ifnot(ctx, cond, var);
        }
        return ghostValue(jl_nothing_type);
    }
    else if (head == invoke_sym) {
        assert(ssaval >= 0);
        jl_value_t *expr_t = jl_is_long(ctx.source->ssavaluetypes) ? (jl_value_t*)jl_any_type :
            jl_array_ptr_ref(ctx.source->ssavaluetypes, ssaval);
        return emit_invoke(ctx, ex, expr_t);
    }
    else if (head == call_sym) {
        jl_value_t *expr_t;
        if (ssaval < 0)
            // TODO: this case is needed for the call to emit_expr in emit_llvmcall
            expr_t = (jl_value_t*)jl_any_type;
        else
            expr_t = jl_is_long(ctx.source->ssavaluetypes) ? (jl_value_t*)jl_any_type : jl_array_ptr_ref(ctx.source->ssavaluetypes, ssaval);
        jl_cgval_t res = emit_call(ctx, ex, expr_t);
        // some intrinsics (e.g. typeassert) can return a wider type
        // than what's actually possible
        res = update_julia_type(ctx, res, expr_t);
        if (res.typ == jl_bottom_type || expr_t == jl_bottom_type) {
            CreateTrap(ctx.builder);
        }
        return res;
    }
    else if (head == foreigncall_sym) {
        return emit_ccall(ctx, args, jl_array_dim0(ex->args));
    }
    else if (head == cfunction_sym) {
        jl_cgval_t fexpr_rt = emit_expr(ctx, args[1]);
        return emit_cfunction(ctx, args[0], fexpr_rt, args[2], (jl_svec_t*)args[3]);
    }
    else if (head == assign_sym) {
        emit_assignment(ctx, args[0], args[1], ssaval);
        return ghostValue(jl_nothing_type);
    }
    else if (head == static_parameter_sym) {
        return emit_sparam(ctx, jl_unbox_long(args[0]) - 1);
    }
    else if (head == method_sym) {
        jl_value_t *mn = args[0];
        assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(mn) || jl_is_slot(mn));

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
            JL_TRY {
                if (jl_symbol_name((jl_sym_t*)mn)[0] == '@')
                    jl_errorf("macro definition not allowed inside a local scope");
                name = literal_pointer_val(ctx, mn);
                bnd = jl_get_binding_for_method_def(mod, (jl_sym_t*)mn);
            }
            JL_CATCH {
                jl_value_t *e = jl_current_exception();
                // errors. boo. root it somehow :(
                bnd = jl_get_binding_wr(ctx.module, (jl_sym_t*)jl_gensym(), 1);
                bnd->value = e;
                bnd->constp = 1;
                raise_exception(ctx, literal_pointer_val(ctx, e));
                return ghostValue(jl_nothing_type);
            }
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
                                 bp_owner, literal_pointer_val(ctx, bnd) };
            jl_cgval_t gf = mark_julia_type(
                    ctx,
                    ctx.builder.CreateCall(prepare_call(jlgenericfunction_func), makeArrayRef(mdargs)),
                    true,
                    jl_function_type);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }
        Value *a1 = boxed(ctx, emit_expr(ctx, args[1]));
        Value *a2 = boxed(ctx, emit_expr(ctx, args[2]));
        Value *mdargs[3] = {
            /*argdata*/a1,
            /*code*/a2,
            /*module*/literal_pointer_val(ctx, (jl_value_t*)ctx.module)
        };
        ctx.builder.CreateCall(prepare_call(jlmethod_func), makeArrayRef(mdargs));
        return ghostValue(jl_nothing_type);
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx.module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (jl_is_symbol(sym)) {
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
                jl_is_concrete_type(jl_tparam0(ty))) {
            assert(nargs <= jl_datatype_nfields(jl_tparam0(ty)) + 1);
            return emit_new_struct(ctx, jl_tparam0(ty), nargs - 1, &argv[1]);
        }
        Value *val = emit_jlcall(ctx, jlnew_func, nullptr, argv, nargs, JLCALL_F_CC);
        // temporarily mark as `Any`, expecting `emit_ssaval_assign` to update
        // it to the inferred type.
        return mark_julia_type(ctx, val, true, (jl_value_t*)jl_any_type);
    }
    else if (head == splatnew_sym) {
        jl_cgval_t argv[2];
        argv[0] = emit_expr(ctx, args[0]);
        argv[1] = emit_expr(ctx, args[1]);
        Value *typ = boxed(ctx, argv[0]);
        Value *tup = boxed(ctx, argv[1]);
        Value *val = ctx.builder.CreateCall(prepare_call(jlsplatnew_func), { typ, tup });
        // temporarily mark as `Any`, expecting `emit_ssaval_assign` to update
        // it to the inferred type.
        return mark_julia_type(ctx, val, true, (jl_value_t*)jl_any_type);
    }
    else if (head == exc_sym) {
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jl_current_exception_func)),
                true, jl_any_type);
    }
    else if (head == copyast_sym) {
        jl_cgval_t ast = emit_expr(ctx, args[0]);
        if (ast.typ != (jl_value_t*)jl_expr_type && ast.typ != (jl_value_t*)jl_any_type) {
            // elide call to jl_copy_ast when possible
            return ast;
        }
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jlcopyast_func),
                    maybe_decay_untracked(boxed(ctx, ast))), true, jl_expr_type);
    }
    else if (head == loopinfo_sym) {
        // parse Expr(:loopinfo, "julia.simdloop", ("llvm.loop.vectorize.width", 4))
        SmallVector<Metadata *, 8> MDs;
        for (int i = 0, ie = jl_expr_nargs(ex); i < ie; ++i) {
            Metadata *MD = to_md_tree(args[i]);
            if (MD)
                MDs.push_back(MD);
        }

        MDNode* MD = MDNode::get(jl_LLVMContext, MDs);
        CallInst *I = ctx.builder.CreateCall(prepare_call(jl_loopinfo_marker_func));
        I->setMetadata("julia.loopinfo", MD);
        return jl_cgval_t();
    }
    else if (head == goto_ifnot_sym || head == leave_sym || head == coverageeffect_sym
            || head == pop_exception_sym || head == enter_sym || head == inbounds_sym
            || head == aliasscope_sym || head == popaliasscope_sym) {
        jl_errorf("Expr(:%s) in value position", jl_symbol_name(head));
    }
    else if (head == boundscheck_sym) {
        return mark_julia_const(bounds_check_enabled(ctx, jl_true) ? jl_true : jl_false);
    }
    else if (head == gc_preserve_begin_sym) {
        size_t nargs = jl_array_len(ex->args);
        jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        std::vector<Value*> vals;
        for (size_t i = 0; i < nargs; ++i) {
            const jl_cgval_t &ai = argv[i];
            if (ai.constant)
                continue;
            if (ai.isboxed) {
                vals.push_back(ai.Vboxed);
            }
            else if (!jl_is_pointerfree(ai.typ)) {
                Type *at = julia_type_to_llvm(ctx, ai.typ);
                vals.push_back(emit_unbox(ctx, at, ai, ai.typ));
            }
        }
        Value *token = vals.empty()
            ? (Value*)ConstantTokenNone::get(jl_LLVMContext)
            : ctx.builder.CreateCall(prepare_call(gc_preserve_begin_func), vals);
        jl_cgval_t tok(token, NULL, false, (jl_value_t*)jl_nothing_type, NULL);
        return tok;
    }
    else if (head == gc_preserve_end_sym) {
        // We only support ssa values as the argument. Everything else will
        // fall back to the default behavior of preserving the argument value
        // until the end of the scope, which is correct, but not optimal.
        if (!jl_is_ssavalue(args[0])) {
            return jl_cgval_t((jl_value_t*)jl_nothing_type);
        }
        jl_cgval_t token = emit_expr(ctx, args[0]);
        assert(token.V->getType()->isTokenTy());
        if (!isa<ConstantTokenNone>(token.V))
            ctx.builder.CreateCall(prepare_call(gc_preserve_end_func), {token.V});
        return jl_cgval_t((jl_value_t*)jl_nothing_type);
    }
    else {
        if (jl_is_toplevel_only_expr(expr) &&
            !jl_is_method(ctx.linfo->def.method)) {
            // call interpreter to run a toplevel expr from inside a
            // compiled toplevel thunk.
            Value *args[2] = {
                literal_pointer_val(ctx, (jl_value_t*)ctx.module),
                literal_pointer_val(ctx, expr)
            };
            ctx.builder.CreateCall(prepare_call(jltopeval_func), args);
            return ghostValue(jl_nothing_type);
        }
        if (head == abstracttype_sym || head == structtype_sym ||
            head == primtype_sym) {
            jl_errorf("type definition not allowed inside a local scope");
        }
        else {
            jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                      jl_symbol_name(head), ctx.name);
        }
    }
    return jl_cgval_t();
}
JL_GCC_IGNORE_STOP

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0)
{
    // TODO: requires the runtime, but is generated unconditionally

    // allocate a placeholder gc instruction
    ctx.ptlsStates = ctx.builder.CreateCall(prepare_call(jltls_states_func));
    int nthfield = offsetof(jl_tls_states_t, safepoint) / sizeof(void*);
    ctx.signalPage = emit_nthptr_recast(ctx, ctx.ptlsStates, nthfield, tbaa_const,
                                        PointerType::get(T_psize, 0));
}

static void emit_last_age_field(jl_codectx_t &ctx)
{
    ctx.world_age_field = ctx.builder.CreateInBoundsGEP(
            ctx.builder.CreateBitCast(ctx.ptlsStates, T_psize),
            ConstantInt::get(T_size, offsetof(jl_tls_states_t, world_age) / sizeof(size_t)));
}

static Function *emit_tojlinvoke(jl_code_instance_t *codeinst, Module *M, jl_codegen_params_t &params)
{
    jl_codectx_t ctx(jl_LLVMContext, params);
    std::stringstream name;
    name << "tojlinvoke" << globalUnique++;
    Function *f = Function::Create(jl_func_sig,
            GlobalVariable::PrivateLinkage,
            name.str(), M);
    jl_init_function(f);
    f->addFnAttr(Thunk);
    //f->setAlwaysInline();
    ctx.f = f; // for jl_Module
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    ctx.builder.SetInsertPoint(b0);
    Value *theFptr;
    Value *theFarg;
    if (codeinst->invoke != NULL) {
        StringRef theFptrName = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)codeinst->invoke, codeinst);
        theFptr = M->getOrInsertFunction(theFptrName, jlinvoke_func->getFunctionType())
#if JL_LLVM_VERSION >= 90000
                .getCallee();
#else
                ;
#endif
        theFarg = literal_pointer_val(ctx, (jl_value_t*)codeinst);
    }
    else {
        theFptr = prepare_call(jlinvoke_func);
        theFarg = literal_pointer_val(ctx, (jl_value_t*)codeinst->def);
    }
    theFarg = maybe_decay_untracked(theFarg);
    auto args = f->arg_begin();
    CallInst *r = ctx.builder.CreateCall(theFptr, { &*args, &*++args, &*++args, theFarg });
    r->setAttributes(cast<Function>(theFptr)->getAttributes());
    ctx.builder.CreateRet(r);
    return f;
}

static void emit_cfunc_invalidate(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype,
        size_t nargs,
        jl_codegen_params_t &params,
        Function *target=jlapplygeneric_func)
{
    jl_codectx_t ctx(jl_LLVMContext, params);
    ctx.f = gf_thunk;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", gf_thunk);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    Function::arg_iterator AI = gf_thunk->arg_begin();
    jl_cgval_t *myargs = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    if (cc == jl_returninfo_t::SRet || cc == jl_returninfo_t::Union)
        ++AI;
    if (return_roots)
        ++AI;
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_nth_slot_type(calltype, i);
        bool isboxed = deserves_argbox(jt);
        Type *et = isboxed ?  T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (type_is_ghost(et)) {
            assert(jl_is_datatype(jt) && ((jl_datatype_t*)jt)->instance);
            myargs[i] = mark_julia_const(((jl_datatype_t*)jt)->instance);
        }
        else {
            Value *arg_v = &*AI;
            ++AI;
            Type *at = arg_v->getType();
            if (!isboxed && et->isAggregateType()) {
                myargs[i] = mark_julia_slot(arg_v, jt, NULL, tbaa_const);
            }
            else {
                assert(at == et);
                myargs[i] = mark_julia_type(ctx, arg_v, isboxed, jt);
            }
            (void)at;
        }
    }
    assert(AI == gf_thunk->arg_end());
    Value *gf_ret = emit_jlcall(ctx, target, nullptr, myargs, nargs, JLCALL_F_CC);
    jl_cgval_t gf_retbox = mark_julia_type(ctx, gf_ret, true, jl_any_type);
    if (cc != jl_returninfo_t::Boxed) {
        emit_typecheck(ctx, gf_retbox, rettype, "cfunction");
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
            ctx.builder.CreateRet(ctx.builder.CreateAlignedLoad(gf_ret, julia_alignment(rettype)));
        }
        break;
    }
    case jl_returninfo_t::SRet: {
        if (return_roots)
            ctx.builder.CreateStore(gf_ret, gf_thunk->arg_begin() + 1);
        emit_memcpy(ctx, &*gf_thunk->arg_begin(), nullptr, gf_ret, nullptr, jl_datatype_size(rettype), julia_alignment(rettype));
        ctx.builder.CreateRetVoid();
        break;
    }
    case jl_returninfo_t::Union: {
        Type *retty = gf_thunk->getReturnType();
        Value *gf_retval = UndefValue::get(retty);
        Value *tindex = compute_box_tindex(ctx, emit_typeof_boxed(ctx, gf_retbox), (jl_value_t*)jl_any_type, rettype);
        tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
        gf_retval = ctx.builder.CreateInsertValue(gf_retval, gf_ret, 0);
        gf_retval = ctx.builder.CreateInsertValue(gf_retval, tindex, 1);
        ctx.builder.CreateRet(gf_retval);
        break;
    }
    case jl_returninfo_t::Ghosts: {
        Value *gf_retval = compute_tindex_unboxed(ctx, gf_retbox, rettype);
        ctx.builder.CreateRet(gf_retval);
        break;
    }
    }
}

static Function* gen_cfun_wrapper(
    Module *into, jl_codegen_params_t &params,
    const function_sig_t &sig, jl_value_t *ff,
    jl_typemap_entry_t *sf, jl_value_t *declrt, jl_method_instance_t *lam,
    jl_unionall_t *unionall_env, jl_svec_t *sparam_vals, jl_array_t **closure_types)
{
    // Generate a c-callable wrapper
    size_t nargs = sig.nccallargs;
    const char *name = "cfunction";
    size_t world = jl_world_counter;
    jl_code_instance_t *codeinst = NULL;
    bool nest = (!ff || unionall_env);
    jl_value_t *astrt = (jl_value_t*)jl_any_type;
    void *callptr = NULL;
    int calltype = 0;
    // infer it first, if necessary
    // FIXME! pretend this is OK
    if (lam)
        name = jl_symbol_name(lam->def.method->name);
    if (lam && params.cache) {
        // if (!into)
        // TODO: this isn't ideal to be unconditionally calling type inference (and compile) from here
        codeinst = jl_compile_method_internal(lam, world);
        assert(codeinst->invoke);
        if (codeinst->invoke == jl_fptr_args) {
            callptr = codeinst->specptr.fptr;
            calltype = 1;
        }
        else if (codeinst->invoke == jl_fptr_const_return) {
            // don't need the fptr
            callptr = (void*)codeinst->rettype_const;
            calltype = 2;
        }
        else if (codeinst->isspecsig) {
            callptr = codeinst->specptr.fptr;
            calltype = 3;
        }
        astrt = codeinst->rettype;
        if (astrt != (jl_value_t*)jl_bottom_type &&
            jl_type_intersection(astrt, declrt) == jl_bottom_type) {
            // Do not warn if the function never returns since it is
            // occasionally required by the C API (typically error callbacks)
            // even though we're likely to encounter memory errors in that case
            jl_printf(JL_STDERR, "WARNING: cfunction: return type of %s does not match\n", name);
        }
    }

    std::stringstream funcName;
    funcName << "jlcapi_" << name << "_" << globalUnique++;

    Module *M = into;
    if (!M) {
        M = new Module(name, jl_LLVMContext);
        jl_setup_module(M);
    }
    AttributeList attributes = sig.attributes;
    FunctionType *functype;
    if (nest) {
        // add nest parameter (pointer to jl_value_t* data array) after sret arg
        assert(closure_types);
        std::vector<Type*> fargt_sig(sig.fargt_sig);
        fargt_sig.insert(fargt_sig.begin() + sig.sret, T_pprjlvalue);
        functype = FunctionType::get(sig.sret ? T_void : sig.prt, fargt_sig, /*isVa*/false);
        attributes = attributes.addAttribute(jl_LLVMContext, 1 + sig.sret, Attribute::Nest);
    }
    else {
        functype = sig.functype();
    }
    Function *cw = Function::Create(functype,
            GlobalVariable::ExternalLinkage,
            funcName.str(), M);
    cw->setAttributes(attributes);
    jl_init_function(cw);
    Function *cw_proto = into ? cw : function_proto(cw);
    // Save the Function object reference
    if (sf) {
        jl_value_t *oldsf = sf->func.value;
        size_t i, oldlen = jl_svec_len(oldsf);
        jl_value_t *newsf = (jl_value_t*)jl_alloc_svec(oldlen + 2);
        JL_GC_PUSH1(&newsf);
        jl_svecset(newsf, 0, sig.rt);
        jl_svecset(newsf, 1, jl_box_voidpointer((void*)cw_proto));
        for (i = 0; i < oldlen; i++)
            jl_svecset(newsf, i + 2, jl_svecref(oldsf, i));
        sf->func.value = newsf;
        jl_gc_wb(sf, sf->func.value);
        JL_GC_POP();
    }

    jl_codectx_t ctx(jl_LLVMContext, params);
    ctx.f = cw;
    ctx.world = world;
    ctx.name = name;
    ctx.funcName = name;

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
    Value *valid_tls = ctx.builder.CreateIsNotNull(last_age);
    have_tls = ctx.builder.CreateAnd(have_tls, valid_tls);
    ctx.world_age_field = ctx.builder.CreateSelect(valid_tls, ctx.world_age_field, dummy_world);
    Value *world_v = ctx.builder.CreateLoad(prepare_global(jlgetworld_global));

    Value *age_ok = NULL;
    if (calltype) {
        Value *lam_max = ctx.builder.CreateLoad(
                T_size,
                ctx.builder.CreateConstInBoundsGEP1_32(
                    T_size,
                    emit_bitcast(ctx, literal_pointer_val(ctx, (jl_value_t*)codeinst), T_psize),
                    offsetof(jl_code_instance_t, max_world) / sizeof(size_t)));
        // XXX: age is always OK if we don't have a TLS. This is a hack required due to `@threadcall` abuse.
        // and adds quite a bit of complexity here, even though it's still wrong
        // (anything that tries to interact with the runtime will fault)
        age_ok = ctx.builder.CreateICmpUGE(lam_max, world_v);
        world_v = ctx.builder.CreateSelect(ctx.builder.CreateOr(have_tls, age_ok), world_v, lam_max);
        age_ok = ctx.builder.CreateOr(ctx.builder.CreateNot(have_tls), age_ok);
    }
    ctx.builder.CreateStore(world_v, ctx.world_age_field);

    // first emit code to record the arguments
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = sig.sret ? &*AI++ : NULL;
    Value *nestPtr = nest ? &*AI++ : NULL;
    jl_cgval_t *inputargs = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * (nargs + 1));
    if (ff) {
        // we need to pass the function object even if (even though) it is a singleton
        inputargs[0] = mark_julia_const(ff);
    }
    else {
        assert(nest && nestPtr);
        Value *ff = ctx.builder.CreateLoad(T_prjlvalue, nestPtr);
        inputargs[0] = mark_julia_type(ctx, ff, true, jl_any_type);
    }
    // XXX: these values may need to be rooted until the end of the function
    jl_value_t *rt1 = NULL;
    jl_value_t *rt2 = NULL;
    JL_GC_PUSH2(&rt1, &rt2);
    for (size_t i = 0; i < nargs; ++i, ++AI) {
        // figure out how to unpack this argument type
        Value *val = &*AI;
        assert(sig.fargt_sig.at(i + sig.sret) == val->getType());
        jl_cgval_t &inputarg = inputargs[i + 1];
        jl_value_t *jargty = jl_svecref(sig.at, i);
        bool aref = jl_is_abstract_ref_type(jargty);
        if (aref) // a pointer to a value
            jargty = jl_tparam0(jargty);

        // if we know the outer function sparams, try to fill those in now
        // so that the julia_to_native type checks are more likely to be doable (e.g. concrete types) at compile-time
        jl_value_t *jargty_proper = jargty;
        bool static_at = !(unionall_env && jl_has_typevar_from_unionall(jargty, unionall_env));
        if (!static_at) {
            if (sparam_vals) {
                jargty_proper = rt1 = jl_instantiate_type_in_env(jargty, unionall_env, jl_svec_data(sparam_vals));
                assert(jargty_proper != jargty);
                jargty = jargty_proper;
                static_at = true;
            }
            else {
                jargty_proper = rt1 = jl_rewrap_unionall(jargty, (jl_value_t*)unionall_env);
            }
        }

        if (aref) {
            if (jargty == (jl_value_t*)jl_any_type) {
                inputarg = mark_julia_type(ctx,
                        ctx.builder.CreateLoad(T_prjlvalue, emit_bitcast(ctx, val, T_pprjlvalue)),
                        true, jl_any_type);
            }
            else if (static_at && jl_is_concrete_immutable(jargty)) { // anything that could be stored unboxed
                bool isboxed;
                Type *T = julia_type_to_llvm(ctx, jargty, &isboxed);
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
            else if (static_at || (!jl_is_typevar(jargty) && !jl_is_immutable_datatype(jargty))) {
                // must be a jl_value_t* (because it's mutable or contains gc roots)
                inputarg = mark_julia_type(ctx, maybe_decay_untracked(emit_bitcast(ctx, val, T_prjlvalue)), true, jargty_proper);
            }
            else {
                // allocate val into a new box, if it might not be boxed
                // otherwise preserve / reuse the existing box identity
                // TODO: could inspect `jargty` and eliminate some of these cases
                if (!*closure_types)
                    *closure_types = jl_alloc_vec_any(0);
                jl_array_ptr_1d_push(*closure_types, jargty);
                Value *runtime_dt = ctx.builder.CreateLoad(T_prjlvalue,
                        ctx.builder.CreateConstGEP1_32(T_prjlvalue, nestPtr, jl_array_len(*closure_types)));
                BasicBlock *boxedBB = BasicBlock::Create(jl_LLVMContext, "isboxed", cw);
                BasicBlock *loadBB = BasicBlock::Create(jl_LLVMContext, "need-load", cw);
                BasicBlock *unboxedBB = BasicBlock::Create(jl_LLVMContext, "maybe-unboxed", cw);
                BasicBlock *isanyBB = BasicBlock::Create(jl_LLVMContext, "any", cw);
                BasicBlock *afterBB = BasicBlock::Create(jl_LLVMContext, "after", cw);
                Value *isrtboxed = ctx.builder.CreateIsNull(val);
                ctx.builder.CreateCondBr(isrtboxed, boxedBB, loadBB);
                ctx.builder.SetInsertPoint(boxedBB);
                Value *p1 = ctx.builder.CreateBitCast(val, T_pjlvalue);
                p1 = maybe_decay_untracked(p1);
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(loadBB);
                Value *isrtany = ctx.builder.CreateICmpEQ(
                        literal_pointer_val(ctx, (jl_value_t*)jl_any_type),
                        ctx.builder.CreateBitCast(val, T_pjlvalue));
                ctx.builder.CreateCondBr(isrtany, isanyBB, unboxedBB);
                ctx.builder.SetInsertPoint(isanyBB);
                Value *p2 = ctx.builder.CreateLoad(T_prjlvalue, ctx.builder.CreateBitCast(val, T_pprjlvalue));
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(unboxedBB);
                Value *p3 = emit_new_bits(ctx, runtime_dt, val);
                unboxedBB = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(afterBB);
                PHINode *p = ctx.builder.CreatePHI(T_prjlvalue, 3);
                p->addIncoming(p1, boxedBB);
                p->addIncoming(p2, isanyBB);
                p->addIncoming(p3, unboxedBB);
                inputarg = mark_julia_type(ctx, p, true, jargty_proper);
            }
        }
        else {
            bool argboxed = sig.fargt_isboxed.at(i);
            if (argboxed) {
                // a jl_value_t*, even when represented as a struct
                inputarg = mark_julia_type(ctx, val, true, jargty_proper);
            }
            else {
                // something of type T
                // undo whatever we might have done to this poor argument
                assert(jl_is_datatype(jargty));
                if (sig.byRefList.at(i)) {
                    assert(cast<PointerType>(val->getType())->getElementType() == sig.fargt[i]);
                    val = ctx.builder.CreateAlignedLoad(val, 1); // unknown alignment from C
                }
                else {
                    bool issigned = jl_signed_type && jl_subtype(jargty_proper, (jl_value_t*)jl_signed_type);
                    val = llvm_type_rewrite(ctx, val, sig.fargt[i], issigned);
                }
                // passed an unboxed T, but may need something boxed (not valid to be unboxed)
                if (static_at) {
                    bool isboxed;
                    assert(jargty == jargty_proper);
                    (void)julia_type_to_llvm(ctx, jargty, &isboxed);
                    if (isboxed)
                        inputarg = mark_julia_type(ctx,
                                box_ccall_result(ctx, val, literal_pointer_val(ctx, jargty), jargty),
                                true, jargty_proper);
                    else
                        inputarg = mark_julia_type(ctx, val, false, jargty);
                }
                else {
                    if (!*closure_types)
                        *closure_types = jl_alloc_vec_any(0);
                    jl_array_ptr_1d_push(*closure_types, jargty);
                    Value *runtime_dt = ctx.builder.CreateLoad(T_prjlvalue,
                            ctx.builder.CreateConstGEP1_32(T_prjlvalue, nestPtr, jl_array_len(*closure_types)));
                    Value *strct = box_ccall_result(ctx, val, runtime_dt, jargty);
                    inputarg = mark_julia_type(ctx, strct, true, jargty_proper);
                }
            }
        }
    }
    JL_GC_POP();
    assert(AI == cw->arg_end());

    // Create the call
    bool jlfunc_sret;
    jl_cgval_t retval;
    if (calltype == 2) {
        nargs = 0; // arguments not needed -- TODO: not really true, should emit an age_ok test and jlcall
        jlfunc_sret = false;
        retval = mark_julia_const((jl_value_t*)callptr);
    }
    else if (calltype == 0 || calltype == 1) {
        // emit a jlcall
        jlfunc_sret = false;
        Function *theFptr = NULL;
        if (calltype == 1) {
            StringRef fname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)callptr, codeinst);
            theFptr = cast_or_null<Function>(jl_Module->getNamedValue(fname));
            if (!theFptr) {
                theFptr = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage,
                                           fname, jl_Module);
                jl_init_function(theFptr);
            }
            else {
                assert(theFptr->getFunctionType() == jl_func_sig);
            }
            add_return_attr(theFptr, Attribute::NonNull);
            theFptr->addFnAttr(Thunk);
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
            ret_jlcall = emit_jlcall(ctx, theFptr, theF, &inputargs[1], nargs, JLCALL_F_CC);
            ctx.builder.CreateBr(b_after);
            ctx.builder.SetInsertPoint(b_generic);
        }
        Value *ret = emit_jlcall(ctx, prepare_call(jlapplygeneric_func), NULL, inputargs, nargs + 1, JLCALL_F_CC);
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
    else {
        assert(calltype == 3);
        // emit a specsig call
        StringRef protoname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)callptr, codeinst);
        jl_returninfo_t returninfo = get_specsig_function(ctx, M, protoname, lam->specTypes, astrt);
        FunctionType *cft = returninfo.decl->getFunctionType();
        jlfunc_sret = (returninfo.cc == jl_returninfo_t::SRet);

        // TODO: Can use use emit_call_specfun_other here?
        std::vector<Value*> args;
        Value *result;
        if (jlfunc_sret || returninfo.cc == jl_returninfo_t::Union) {
            // fuse the two sret together, or emit an alloca to hold it
            if (sig.sret && jlfunc_sret) {
                result = emit_bitcast(ctx, sretPtr, cft->getParamType(0));
            }
            else {
                result = emit_static_alloca(ctx, cft->getParamType(0)->getPointerElementType());
            }
            args.push_back(result);
        }
        if (returninfo.return_roots) {
            AllocaInst *return_roots = emit_static_alloca(ctx, T_prjlvalue);
            return_roots->setOperand(0, ConstantInt::get(T_int32, returninfo.return_roots));
            args.push_back(return_roots);
        }
        for (size_t i = 0; i < nargs + 1; i++) {
            // figure out how to repack the arguments
            jl_cgval_t &inputarg = inputargs[i];
            Value *arg;
            jl_value_t *spect = jl_nth_slot_type(lam->specTypes, i);
            bool isboxed = deserves_argbox(spect);
            Type *T = isboxed ? T_prjlvalue : julia_type_to_llvm(ctx, spect);
            if (isboxed) {
                arg = boxed(ctx, inputarg);
            }
            else if (type_is_ghost(T)) {
                continue; // ghost types are skipped by the specsig method signature
            }
            else if (T->isAggregateType()) {
                // aggregate types are passed by pointer
                if (!inputarg.ispointer())
                    inputarg = value_to_pointer(ctx, inputarg);
                arg = maybe_bitcast(ctx, decay_derived(data_pointer(ctx, inputarg)),
                    T->getPointerTo());
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
            gf_thunk->setAttributes(returninfo.decl->getAttributes());
            jl_init_function(gf_thunk);
            // build a  specsig -> jl_apply_generic converter thunk
            // this builds a method that calls jl_apply_generic (as a closure over a singleton function pointer),
            // but which has the signature of a specsig
            emit_cfunc_invalidate(gf_thunk, returninfo.cc, returninfo.return_roots, lam->specTypes, codeinst->rettype, nargs + 1, ctx.emission_context);
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
            case jl_returninfo_t::Union: {
                Value *box = ctx.builder.CreateExtractValue(call, 0);
                Value *tindex = ctx.builder.CreateExtractValue(call, 1);
                Value *derived = ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x80)),
                            ConstantInt::get(T_int8, 0)),
                    decay_derived(ctx.builder.CreateBitCast(result, T_pjlvalue)),
                    decay_derived(box));
                retval = mark_julia_slot(derived,
                                         astrt,
                                         tindex,
                                         tbaa_stack);
                retval.Vboxed = box;
                break;
            }
            case jl_returninfo_t::Ghosts:
                retval = mark_julia_slot(NULL, astrt, call, tbaa_stack);
                break;
        }
    }

    // inline a call to typeassert here, if required
    emit_typecheck(ctx, retval, declrt, "cfunction");
    retval = update_julia_type(ctx, retval, declrt);

    // Prepare the return value
    Value *r;
    if (sig.retboxed) {
        assert(!sig.sret);
        // return a jl_value_t*
        r = boxed(ctx, retval);
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
        Value *v = emit_unbox(ctx, sig.lrt, retval, retval.typ);
        r = llvm_type_rewrite(ctx, v, prt, issigned);
        if (sig.sret) {
            ctx.builder.CreateStore(r, sretPtr);
            r = NULL;
        }
    }
    else {
        r = NULL;
    }

    ctx.builder.CreateStore(last_age, ctx.world_age_field);
    ctx.builder.CreateRet(r);

    ctx.builder.SetCurrentDebugLocation(noDbg);
    ctx.builder.ClearInsertionPoint();

    if (nest) {
        funcName << "make";
        Function *cw_make = Function::Create(
                FunctionType::get(T_pint8, { T_pint8, T_ppjlvalue }, false),
                GlobalVariable::ExternalLinkage,
                funcName.str(), M);
        jl_init_function(cw_make);
        BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", cw_make);
        IRBuilder<> cwbuilder(b0);
        Function::arg_iterator AI = cw_make->arg_begin();
        Argument *Tramp = &*AI; ++AI;
        Argument *NVal = &*AI; ++AI;
        Function *init_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::init_trampoline);
        Function *adjust_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::adjust_trampoline);
        cwbuilder.CreateCall(init_trampoline, {
                Tramp,
                cwbuilder.CreateBitCast(cw, T_pint8),
                cwbuilder.CreateBitCast(NVal, T_pint8)
            });
        cwbuilder.CreateRet(cwbuilder.CreateCall(adjust_trampoline, { Tramp }));
        cw_proto = into ? cw_make : function_proto(cw_make);
    }

    if (!into)
        jl_finalize_module(std::unique_ptr<Module>(M));

    return cw_proto;
}

// Get the LLVM Function* for the C-callable entry point for a certain function
// and argument types.
// here argt does not include the leading function type argument
static jl_cgval_t emit_cfunction(jl_codectx_t &ctx, jl_value_t *output_type, const jl_cgval_t &fexpr_rt, jl_value_t *declrt, jl_svec_t *argt)
{
    jl_unionall_t *unionall_env = (jl_is_method(ctx.linfo->def.method) && jl_is_unionall(ctx.linfo->def.method->sig))
        ? (jl_unionall_t*)ctx.linfo->def.method->sig
        : NULL;
    jl_svec_t *sparam_vals = NULL;
    if (ctx.spvals_ptr == NULL && jl_svec_len(ctx.linfo->sparam_vals) > 0)
        sparam_vals = ctx.linfo->sparam_vals;

    jl_value_t *rt = declrt;
    if (jl_is_abstract_ref_type(declrt)) {
        declrt = jl_tparam0(declrt);
        if (!verify_ref_type(ctx, declrt, unionall_env, 0, "cfunction")) {
            return jl_cgval_t();
        }
        if (unionall_env)
            declrt = jl_rewrap_unionall(declrt, (jl_value_t*)unionall_env);
        rt = (jl_value_t*)jl_any_type; // convert return type to jl_value_t*
    }

    // some sanity checking and check whether there's a vararg
    size_t nargt = jl_svec_len(argt);
    bool isVa = (nargt > 0 && jl_is_vararg_type(jl_svecref(argt, nargt - 1)));
    if (isVa) {
        emit_error(ctx, "cfunction: Vararg syntax not allowed for argument list");
        return jl_cgval_t();
    }

    jl_array_t *closure_types = NULL;
    jl_value_t *sigt = NULL; // dispatch-sig = type signature with Ref{} annotations removed and applied to the env
    JL_GC_PUSH4(&declrt, &sigt, &rt, &closure_types);
    Type *lrt;
    bool retboxed;
    bool static_rt;
    const std::string err = verify_ccall_sig(
            /* inputs:  */
            rt, (jl_value_t*)argt, unionall_env,
            sparam_vals,
            &ctx.emission_context,
            /* outputs: */
            lrt, retboxed, static_rt);
    if (!err.empty()) {
        emit_error(ctx, "cfunction " + err);
        JL_GC_POP();
        return jl_cgval_t();
    }
    if (rt != declrt && rt != (jl_value_t*)jl_any_type)
        jl_add_method_root(ctx, rt);

    function_sig_t sig("cfunction", lrt, rt, retboxed, argt, unionall_env, false, CallingConv::C, false, &ctx.emission_context);
    assert(sig.fargt.size() + sig.sret == sig.fargt_sig.size());
    if (!sig.err_msg.empty()) {
        emit_error(ctx, sig.err_msg);
        JL_GC_POP();
        return jl_cgval_t();
    }

    // compute+verify the dispatch signature, and see if it depends on the environment sparams
    bool approx = false;
    sigt = (jl_value_t*)jl_alloc_svec(nargt + 1);
    jl_svecset(sigt, 0, fexpr_rt.typ);
    if (!fexpr_rt.constant && (!jl_is_concrete_type(fexpr_rt.typ) || jl_is_kind(fexpr_rt.typ)))
        approx = true;
    for (size_t i = 0; i < nargt; i++) {
        jl_value_t *jargty = jl_svecref(argt, i);
        if (jl_is_abstract_ref_type(jargty)) {
            jargty = jl_tparam0(jargty);
            if (!verify_ref_type(ctx, jargty, unionall_env, i + 1, "cfunction")) {
                JL_GC_POP();
                return jl_cgval_t();
            }
        }
        if (unionall_env && jl_has_typevar_from_unionall(jargty, unionall_env)) {
            if (sparam_vals)
                jargty = jl_instantiate_type_in_env(jargty, unionall_env, jl_svec_data(sparam_vals));
            else
                approx = true;
        }
        jl_svecset(sigt, i + 1, jargty);
    }
    if (approx) {
        sigt = NULL;
    }
    else {
        sigt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)sigt);
    }
    if (sigt && !(unionall_env && jl_has_typevar_from_unionall(rt, unionall_env))) {
        unionall_env = NULL;
    }

    bool nest = (!fexpr_rt.constant || unionall_env);
#if defined(_CPU_AARCH64_) || defined(_CPU_ARM_) || defined(_CPU_PPC64_)
    if (nest) {
        emit_error(ctx, "cfunction: closures are not supported on this platform");
        return jl_cgval_t();
    }
#endif
    size_t world = jl_world_counter;
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    // try to look up this function for direct invoking
    jl_method_instance_t *lam = sigt ? jl_get_specialization1((jl_tupletype_t*)sigt, world, &min_valid, &max_valid, 0) : NULL;
    Value *F = gen_cfun_wrapper(
            jl_Module, ctx.emission_context,
            sig, fexpr_rt.constant,
            NULL, declrt, lam,
            unionall_env, sparam_vals, &closure_types);
    bool outboxed;
    if (nest) {
        // F is actually an init_trampoline function that returns the real address
        // Now fill in the nest parameters
        Value *fobj = boxed(ctx, fexpr_rt);
        jl_svec_t *fill = jl_emptysvec;
        if (closure_types) {
            assert(ctx.spvals_ptr);
            size_t n = jl_array_len(closure_types);
            jl_svec_t *fill = jl_alloc_svec_uninit(n);
            for (size_t i = 0; i < n; i++) {
                jl_svecset(fill, i, jl_array_ptr_ref(closure_types, i));
            }
            jl_add_method_root(ctx, (jl_value_t*)fill);
        }
        std::stringstream cname;
        cname << "trampolines" << globalUnique++;
        Type *T_htable = ArrayType::get(T_size, sizeof(htable_t) / sizeof(void*));
        Value *cache = new GlobalVariable(*jl_Module, T_htable, false,
                               GlobalVariable::InternalLinkage,
                               ConstantAggregateZero::get(T_htable),
                               cname.str());
        F = ctx.builder.CreateCall(prepare_call(jlgetcfunctiontrampoline_func), {
                 fobj,
                 literal_pointer_val(ctx, output_type),
                 ctx.builder.CreateBitCast(cache, T_pint8),
                 literal_pointer_val(ctx, (jl_value_t*)fill),
                 F,
                 closure_types ? literal_pointer_val(ctx, (jl_value_t*)unionall_env) : V_null,
                 closure_types ? ctx.spvals_ptr : ConstantPointerNull::get(cast<PointerType>(T_pprjlvalue))
             });
        outboxed = true;
    }
    else {
        F = ctx.builder.CreatePtrToInt(F, T_size);
        outboxed = (output_type != (jl_value_t*)jl_voidpointer_type);
        if (outboxed) {
            assert(jl_datatype_size(output_type) == sizeof(void*) * 4);
            Value *strct = emit_allocobj(ctx, jl_datatype_size(output_type),
                                         literal_pointer_val(ctx, (jl_value_t*)output_type));
            Value *derived_strct = emit_bitcast(ctx, decay_derived(strct), T_psize);
            MDNode *tbaa = best_tbaa(output_type);
            tbaa_decorate(tbaa, ctx.builder.CreateStore(F, derived_strct));
            tbaa_decorate(tbaa, ctx.builder.CreateStore(
                ctx.builder.CreatePtrToInt(literal_pointer_val(ctx, fexpr_rt.constant), T_size),
                ctx.builder.CreateConstGEP1_32(T_size, derived_strct, 1)));
            Value *zero = ConstantInt::get(T_size, 0);
            tbaa_decorate(tbaa, ctx.builder.CreateStore(zero,
                    ctx.builder.CreateConstGEP1_32(T_size, derived_strct, 2)));
            tbaa_decorate(tbaa, ctx.builder.CreateStore(zero,
                    ctx.builder.CreateConstGEP1_32(T_size, derived_strct, 3)));
            F = strct;
        }
    }
    JL_GC_POP();
    return mark_julia_type(ctx, F, outboxed, output_type);
}

const struct jl_typemap_info cfunction_cache = {
    1, (jl_datatype_t**)&jl_array_any_type
};

jl_array_t *jl_cfunction_list;

Function *jl_cfunction_object(jl_function_t *ff, jl_value_t *declrt, jl_tupletype_t *argt,
                              jl_codegen_params_t &params)
{
    // Assumes the codegen lock is acquired. The caller is responsible for that.
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->in_pure_callback)
        jl_error("cfunction cannot be used in a generated function");

    // validate and unpack the arguments
    JL_TYPECHK(cfunction, type, declrt);
    if (!jl_is_tuple_type(argt)) // the C API requires that argt Tuple type actually be an svec
        jl_type_error("cfunction", (jl_value_t*)jl_anytuple_type_type, (jl_value_t*)argt);
    // trampolines are not supported here:
    // check that f is a guaranteed singleton type
    jl_value_t *ft = jl_typeof(ff);
    if (((jl_datatype_t*)ft)->instance != ff)
        jl_error("cfunction: use `@cfunction` to make closures");

    // check the cache structure
    // this has three levels (for the 3 parameters above)
    // first split on `ft` using a simple eqtable
    // then use the typemap to split on argt
    // and finally, pick declrt from the pair-list
    jl_typemap_t *cache_l2 = NULL;
    jl_typemap_entry_t *cache_l3 = NULL;
    if (!jl_cfunction_list) {
        jl_cfunction_list = jl_alloc_vec_any(16);
    }
    else {
        cache_l2 = jl_eqtable_get(jl_cfunction_list, ft, NULL);
        if (cache_l2) {
            struct jl_typemap_assoc search = {(jl_value_t*)argt, 1, 0, NULL, 0, ~(size_t)0};
            cache_l3 = jl_typemap_assoc_by_type(cache_l2, &search, /*offs*/0, /*subtype*/0);
            if (cache_l3) {
                jl_svec_t *sf = (jl_svec_t*)cache_l3->func.value;
                size_t i, l = jl_svec_len(sf);
                for (i = 0; i < l; i += 2) {
                    jl_value_t *ti = jl_svecref(sf, i);
                    if (jl_egal(ti, declrt)) {
                        return (Function*)jl_unbox_voidpointer(jl_svecref(sf, i + 1));
                    }
                }
            }
        }
    }

    if (cache_l3 == NULL) {
        jl_typemap_t *insert = cache_l2;
        if (!insert)
            insert = jl_nothing;
        cache_l3 = jl_typemap_insert(&insert, (jl_value_t*)insert, (jl_tupletype_t*)argt,
            NULL, jl_emptysvec, (jl_value_t*)jl_emptysvec, /*offs*/0, &cfunction_cache, 1, ~(size_t)0);
        if (insert != cache_l2)
            jl_cfunction_list = jl_eqtable_put(jl_cfunction_list, ft, insert, NULL);
    }

    // compute / validate return type
    jl_value_t *crt = declrt;
    if (jl_is_abstract_ref_type(declrt)) {
        declrt = jl_tparam0(declrt);
        if (jl_is_typevar(declrt))
            jl_error("cfunction: return type Ref should have an element type, not Ref{<:T}");
        if (declrt == (jl_value_t*)jl_any_type)
            jl_error("cfunction: return type Ref{Any} is invalid. Use Any or Ptr{Any} instead.");
        crt = (jl_value_t*)jl_any_type;
    }
    bool toboxed;
    Type *lcrt = _julia_struct_to_llvm(&params, crt, NULL, &toboxed);
    if (lcrt == NULL)
        jl_error("cfunction: return type doesn't correspond to a C type");
    else if (toboxed)
        lcrt = T_prjlvalue;

    // compute / validate method signature
    jl_value_t *sigt = NULL; // dispatch sig: type signature (argt) with Ref{} annotations removed and ft added
    JL_GC_PUSH1(&sigt);
    size_t i, nargs = jl_nparams(argt);
    sigt = (jl_value_t*)jl_alloc_svec(nargs + 1);
    jl_svecset(sigt, 0, ft);
    for (i = 0; i < nargs; i++) {
        jl_value_t *ati = jl_tparam(argt, i);
        if (jl_is_abstract_ref_type(ati)) {
            ati = jl_tparam0(ati);
            if (jl_is_typevar(ati))
                jl_error("cfunction: argument type Ref should have an element type, not Ref{<:T}");
        }
        if (jl_is_pointer(ati) && jl_is_typevar(jl_tparam0(ati)))
            jl_error("cfunction: argument type Ptr should have an element type, Ptr{<:T}");
        jl_svecset(sigt, i + 1, ati);
    }
    sigt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)sigt);

    // emit cfunction (trampoline)
    jl_value_t *err;
    { // scope block for sig
        function_sig_t sig("cfunction", lcrt, crt, toboxed,
                           argt->parameters, NULL, false, CallingConv::C, false, &params);
        if (sig.err_msg.empty()) {
            size_t world = jl_world_counter;
            size_t min_valid = 0;
            size_t max_valid = ~(size_t)0;
            // try to look up this function for direct invoking
            jl_method_instance_t *lam = jl_get_specialization1((jl_tupletype_t*)sigt, world, &min_valid, &max_valid, 0);
            Function *F = gen_cfun_wrapper(NULL, params, sig, ff, cache_l3, declrt, lam, NULL, NULL, NULL);
            JL_GC_POP();
            return F;
        }
        err = jl_get_exceptionf(jl_errorexception_type, "%s", sig.err_msg.c_str());
    }
    jl_throw(err);
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_invoke_wrapper(jl_method_instance_t *lam, jl_value_t *jlretty, const jl_returninfo_t &f, int retarg, StringRef funcName,
        Module *M, jl_codegen_params_t &params)
{
    Function *w = Function::Create(jl_func_sig, GlobalVariable::ExternalLinkage, funcName, M);
    add_return_attr(w, Attribute::NonNull);
    w->addFnAttr(Thunk);
    jl_init_function(w);
    Function::arg_iterator AI = w->arg_begin();
    Value *funcArg = &*AI++;
    Value *argArray = &*AI++;
    Value *argCount = &*AI++; (void)argCount; // unused
    //Value *mfunc = &*AI++; (void)mfunc; // unused
    assert(AI == w->arg_end());

    jl_codectx_t ctx(jl_LLVMContext, params);
    ctx.f = w;
    ctx.linfo = lam;
    ctx.rettype = jlretty;
    ctx.world = 0;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    // TODO: replace this with emit_call_specfun_other?
    FunctionType *ftype = f.decl->getFunctionType();
    size_t nfargs = ftype->getNumParams();
    Value **args = (Value**) alloca(nfargs * sizeof(Value*));
    unsigned idx = 0;
    AllocaInst *result = NULL;
    switch (f.cc) {
    case jl_returninfo_t::Boxed:
    case jl_returninfo_t::Register:
    case jl_returninfo_t::Ghosts:
        break;
    case jl_returninfo_t::SRet:
        result = ctx.builder.CreateAlloca(ftype->getParamType(0)->getPointerElementType());
        args[idx] = result;
        idx++;
        break;
    case jl_returninfo_t::Union:
        result = ctx.builder.CreateAlloca(ArrayType::get(T_int8, f.union_bytes));
        if (f.union_align > 1)
            result->setAlignment(Align(f.union_align));
        args[idx] = result;
        idx++;
        break;
    }
    if (f.return_roots) {
        AllocaInst *return_roots = emit_static_alloca(ctx, T_prjlvalue);
        return_roots->setOperand(0, ConstantInt::get(T_int32, f.return_roots));
        args[idx] = return_roots;
        idx++;
    }

    for (size_t i = 0; i < jl_nparams(lam->specTypes) && idx < nfargs; ++i) {
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed = deserves_argbox(ty);
        Type *lty = isboxed ?  T_prjlvalue : julia_type_to_llvm(ctx, ty);
        if (type_is_ghost(lty))
            continue;
        Value *theArg;
        if (i == 0) {
            theArg = funcArg;
        }
        else {
            Value *argPtr = ctx.builder.CreateConstInBoundsGEP1_32(T_prjlvalue, argArray, i - 1);
            theArg = maybe_mark_load_dereferenceable(ctx.builder.CreateLoad(T_prjlvalue, argPtr), false, ty);
        }
        if (!isboxed) {
            theArg = decay_derived(emit_bitcast(ctx, theArg, PointerType::get(lty, 0)));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = ctx.builder.CreateAlignedLoad(theArg, julia_alignment(ty));
        }
        assert(dyn_cast<UndefValue>(theArg) == NULL);
        args[idx] = theArg;
        idx++;
    }
    CallInst *call = ctx.builder.CreateCall(f.decl, ArrayRef<Value*>(&args[0], nfargs));
    call->setAttributes(f.decl->getAttributes());

    jl_cgval_t retval;
    if (retarg != -1) {
        Value *theArg;
        if (retarg == 0)
            theArg = funcArg;
        else
            theArg = ctx.builder.CreateLoad(T_prjlvalue, ctx.builder.CreateConstInBoundsGEP1_32(T_prjlvalue, argArray, retarg - 1));
        retval = mark_julia_type(ctx, theArg, true, jl_any_type);
    }
    else {
        switch (f.cc) {
        case jl_returninfo_t::Boxed:
            retval = mark_julia_type(ctx, call, true, jlretty);
            break;
        case jl_returninfo_t::Register:
            retval = mark_julia_type(ctx, call, false, jlretty);
            break;
        case jl_returninfo_t::SRet:
            retval = mark_julia_slot(result, jlretty, NULL, tbaa_stack);
            break;
        case jl_returninfo_t::Union:
            // result is technically not right here, but `boxed` will only look at it
            // for the unboxed values, so it's ok.
            retval = mark_julia_slot(result,
                                     jlretty,
                                     ctx.builder.CreateExtractValue(call, 1),
                                     tbaa_stack);
            retval.Vboxed = ctx.builder.CreateExtractValue(call, 0);
            break;
        case jl_returninfo_t::Ghosts:
            retval = mark_julia_slot(NULL, jlretty, call, tbaa_stack);
            break;
        }
    }
    ctx.builder.CreateRet(boxed(ctx, retval));
    assert(!ctx.roots);
    return w;
}

static jl_returninfo_t get_specsig_function(jl_codectx_t &ctx, Module *M, StringRef name, jl_value_t *sig, jl_value_t *jlrettype)
{
    jl_returninfo_t props = {};
    SmallVector<Type*, 8> fsig;
    Type *rt;
    if (jl_is_structtype(jlrettype) && jl_is_datatype_singleton((jl_datatype_t*)jlrettype)) {
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
    else if (!deserves_retbox(jlrettype)) {
        bool retboxed;
        rt = julia_type_to_llvm(ctx, jlrettype, &retboxed);
        assert(!retboxed);
        if (rt != T_void && deserves_sret(jlrettype, rt)) {
            auto tracked = CountTrackedPointers(rt);
            assert(!tracked.derived);
            if (tracked.count && !tracked.all)
                props.return_roots = tracked.count;
            props.cc = jl_returninfo_t::SRet;
            fsig.push_back(rt->getPointerTo());
            rt = T_void;
        }
        else {
            props.cc = jl_returninfo_t::Register;
        }
    }
    else {
        rt = T_prjlvalue;
    }

    AttributeList attributes; // function declaration attributes
    if (props.cc == jl_returninfo_t::SRet) {
        unsigned argno = 1;
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::StructRet);
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoAlias);
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoCapture);
    }
    if (props.cc == jl_returninfo_t::Union) {
        unsigned argno = 1;
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoAlias);
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoCapture);
    }

    if (props.return_roots) {
        fsig.push_back(T_pprjlvalue);
        unsigned argno = fsig.size();
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoAlias);
        attributes = attributes.addAttribute(jl_LLVMContext, argno, Attribute::NoCapture);
    }

    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        Type *ty = deserves_argbox(jt) ? T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (type_is_ghost(ty))
            continue;
        unsigned argno = fsig.size();
        if (ty->isAggregateType()) { // aggregate types are passed by pointer
            attributes = attributes.addParamAttribute(jl_LLVMContext, argno, Attribute::NoCapture);
            attributes = attributes.addParamAttribute(jl_LLVMContext, argno, Attribute::ReadOnly);
            ty = PointerType::get(ty, AddressSpace::Derived);
        }
        fsig.push_back(ty);
    }

    FunctionType *ftype = FunctionType::get(rt, fsig, false);
    Function *f = M ? cast_or_null<Function>(M->getNamedValue(name)) : NULL;
    if (f == NULL) {
        f = Function::Create(ftype, GlobalVariable::ExternalLinkage, name, M);
        f->setAttributes(attributes);
        jl_init_function(f);
    }
    else {
        assert(f->getFunctionType() == ftype);
    }
    if (rt == T_prjlvalue)
        add_return_attr(f, Attribute::NonNull);
    props.decl = f;
    return props;
}

static void emit_sret_roots(jl_codectx_t &ctx, bool isptr, Value *Src, Type *T, Value *Shadow, unsigned count)
{
    if (isptr)
        Src = maybe_decay_tracked(Src);
    if (isptr && Src->getType()->getPointerElementType() != T)
        Src = ctx.builder.CreateBitCast(Src, T->getPointerTo(Src->getType()->getPointerAddressSpace()));
    unsigned emitted = TrackWithShadow(Src, T, isptr, Shadow, ctx.builder);
    assert(emitted == count); (void)emitted; (void)count;
}

static DISubroutineType *
get_specsig_di(jl_codectx_t &ctx, jl_value_t *rt, jl_value_t *sig, DIBuilder &dbuilder)
{
    size_t nargs = jl_nparams(sig); // TODO: if this is a Varargs function, our debug info for the `...` var may be misleading
    std::vector<Metadata*> ditypes(nargs + 1);
    ditypes[0] = julia_type_to_di(ctx, rt, &dbuilder, false);
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        ditypes[i + 1] = julia_type_to_di(ctx, jt, &dbuilder, false);
    }
    return dbuilder.createSubroutineType(dbuilder.getOrCreateTypeArray(ditypes));
}

static jl_datatype_t *compute_va_type(jl_method_instance_t *lam, size_t nreq)
{
    size_t nvargs = jl_nparams(lam->specTypes)-nreq;
    jl_svec_t *tupargs = jl_alloc_svec(nvargs);
    JL_GC_PUSH1(&tupargs);
    for (size_t i = nreq; i < jl_nparams(lam->specTypes); ++i) {
        jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
        jl_svecset(tupargs, i-nreq, argType);
    }
    jl_datatype_t *typ = jl_apply_tuple_type(tupargs);
    JL_GC_POP();
    return typ;
}

// Compile to LLVM IR, using a specialized signature if applicable.
static std::pair<std::unique_ptr<Module>, jl_llvm_functions_t>
    emit_function(
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
{
    // step 1. unpack AST and allocate codegen context for this function
    jl_llvm_functions_t declarations;
    jl_codectx_t ctx(jl_LLVMContext, params);
    JL_GC_PUSH2(&ctx.code, &ctx.roots);
    ctx.code = src->code;

    //jl_static_show(JL_STDOUT, (jl_value_t*)ast);
    //jl_printf(JL_STDOUT, "\n");
    std::map<int, BasicBlock*> labels;
    ctx.module = jl_is_method(lam->def.method) ? lam->def.method->module : lam->def.module;
    ctx.linfo = lam;
    ctx.rettype = jlrettype;
    ctx.source = src;
    ctx.name = name_from_method_instance(lam);
    ctx.funcName = ctx.name;
    ctx.spvals_ptr = NULL;
    ctx.nargs = jl_is_method(lam->def.method) ? lam->def.method->nargs : 0;
    bool toplevel = !jl_is_method(lam->def.method);
    jl_array_t *stmts = ctx.code;
    size_t stmtslen = jl_array_dim0(stmts);

    if (JL_HOOK_TEST(ctx.params, emit_function)) {
        JL_HOOK_CALL(ctx.params, emit_function, 2, (jl_value_t*)ctx.linfo,
                     (jl_value_t*)ctx.source);
    }

    // step 1b. unpack debug information
    int coverage_mode = jl_options.code_coverage;
    int malloc_log_mode = jl_options.malloc_log;
    if (!JL_FEAT_TEST(ctx, code_coverage))
        coverage_mode = JL_LOG_NONE;
    if (!JL_FEAT_TEST(ctx, track_allocations))
        malloc_log_mode = JL_LOG_NONE;

    StringRef dbgFuncName = ctx.name;
    int toplineno = -1;
    if (jl_is_method(lam->def.method)) {
        toplineno = lam->def.method->line;
        ctx.file = jl_symbol_name(lam->def.method->file);
    }
    else if (jl_array_len(src->linetable) > 0) {
        jl_value_t *locinfo = jl_array_ptr_ref(src->linetable, 0);
        ctx.file = jl_symbol_name((jl_sym_t*)jl_fieldref_noalloc(locinfo, 1));
        toplineno = jl_unbox_long(jl_fieldref(locinfo, 2));
    }
    if (ctx.file.empty())
        ctx.file = "<missing>";
    // jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           jl_symbol_name(ctx.name), ctx.file.str().c_str(), toplineno);

    ctx.debug_enabled = true;
    if (dbgFuncName.empty()) // Should never happen anymore?
        ctx.debug_enabled = 0;
    if (jl_options.debug_level == 0)
        ctx.debug_enabled = 0;

    // step 2. process var-info lists to see what vars need boxing
    int n_ssavalues = jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_len(src->ssavaluetypes);
    size_t vinfoslen = jl_array_dim0(src->slotflags);
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

    bool specsig, needsparams;
    std::tie(specsig, needsparams) = uses_specsig(lam, jlrettype, params.params->prefer_specsig);
    if (!src->inferred)
        specsig = false;

    // step 3. some variable analysis
    size_t i;
    for (i = 0; i < nreq; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        varinfo.isArgument = true;
        jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        if (argname == unused_sym)
            continue;
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, ty);
    }
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &varinfo = ctx.slots[ctx.vaSlot];
        varinfo.isArgument = true;
        jl_datatype_t *vatyp = specsig ? compute_va_type(lam, nreq) : (jl_tuple_type);
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, vatyp);
    }

    for (i = 0; i < vinfoslen; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        uint8_t flags = jl_array_uint8_ref(src->slotflags, i);
        varinfo.isSA = (jl_vinfo_sa(flags) != 0) || varinfo.isArgument;
        varinfo.usedUndef = (jl_vinfo_usedundef(flags) != 0) || (!varinfo.isArgument && !src->inferred);
        if (!varinfo.isArgument) {
            varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, (jl_value_t*)jl_any_type);
        }
    }

    // finish recording variable use info
    for (i = 0; i < stmtslen; i++)
        simple_use_analysis(ctx, jl_array_ptr_ref(stmts, i));

    // determine which vars need to be volatile
    mark_volatile_vars(stmts, ctx.slots);

    // step 4. determine function signature
    if (!specsig)
        ctx.nReqArgs--;  // function not part of argArray in jlcall

    std::stringstream funcName;
    // try to avoid conflicts in the global symbol table
    if (specsig)
        funcName << "julia_"; // api 5
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
    declarations.specFunctionObject = funcName.str();

    // allocate Function declarations and wrapper objects
    Module *M = new Module(ctx.name, jl_LLVMContext);
    jl_setup_module(M, ctx.params);
    jl_returninfo_t returninfo = {};
    Function *f = NULL;
    bool has_sret = false;
    if (specsig) { // assumes !va and !needsparams
        returninfo = get_specsig_function(ctx, M, declarations.specFunctionObject, lam->specTypes, jlrettype);
        f = returninfo.decl;
        has_sret = (returninfo.cc == jl_returninfo_t::SRet || returninfo.cc == jl_returninfo_t::Union);
        jl_init_function(f);

        // common pattern: see if all return statements are an argument in that
        // case the apply-generic call can re-use the original box for the return
        int retarg = [stmts, nreq]() {
            int retarg = -1;
            for (size_t i = 0; i < jl_array_len(stmts); ++i) {
                jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
                if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == return_sym) {
                    stmt = jl_exprarg(stmt, 0);
                    if (!jl_is_slot(stmt))
                        return -1;
                    unsigned sl = jl_slot_number(stmt) - 1;
                    if (sl >= nreq)
                        return -1;
                    if (retarg == -1)
                        retarg = sl;
                    else if ((unsigned)retarg != sl)
                        return -1;
                }
            }
            return retarg;
        }();

        std::stringstream wrapName;
        wrapName << "jfptr_" << unadorned_name << "_" << globalUnique;
        declarations.functionObject = wrapName.str();
        (void)gen_invoke_wrapper(lam, jlrettype, returninfo, retarg, declarations.functionObject, M, ctx.emission_context);
    }
    else {
        f = Function::Create(needsparams ? jl_func_sig_sparams : jl_func_sig,
                             GlobalVariable::ExternalLinkage,
                             declarations.specFunctionObject, M);
        jl_init_function(f);
        add_return_attr(f, Attribute::NonNull);
        f->addFnAttr(Thunk);
        // TODO: (if needsparams) add attributes: dereferenceable<sizeof(void*) * length(sp)>, readonly, nocapture
        // TODO: add attributes: dereferenceable<sizeof(ft)>, readonly, nocapture - e.g. maybe_mark_argument_dereferenceable(Arg, argType);
        // TODO: add attributes: dereferenceable<sizeof(void*) * nreq>, readonly, nocapture
        returninfo.decl = f;
        declarations.functionObject = needsparams ? "jl_fptr_sparam" : "jl_fptr_args";
    }

    if (jlrettype == (jl_value_t*)jl_bottom_type)
        f->setDoesNotReturn();

#ifdef USE_POLLY
    if (!jl_has_meta(stmts, polly_sym) || jl_options.polly == JL_OPTIONS_POLLY_OFF) {
        f->addFnAttr(polly::PollySkipFnAttr);
    }
#endif

    if (jl_has_meta(stmts, noinline_sym)) {
        f->addFnAttr(Attribute::NoInline);
    }

    if (returninfo.cc == jl_returninfo_t::Union) {
        f->addAttribute(1, Attribute::getWithDereferenceableBytes(jl_LLVMContext, returninfo.union_bytes));
        f->addAttribute(1, Attribute::getWithAlignment(jl_LLVMContext, Align(returninfo.union_align)));
    }

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
        DICompileUnit::DebugEmissionKind emissionKind = (DICompileUnit::DebugEmissionKind) ctx.params->debug_info_kind;
        DICompileUnit::DebugNameTableKind tableKind;

        if (JL_FEAT_TEST(ctx, gnu_pubnames)) {
            tableKind = DICompileUnit::DebugNameTableKind::GNU;
        }
        else {
            tableKind = DICompileUnit::DebugNameTableKind::None;
        }
        topfile = dbuilder.createFile(ctx.file, ".");
        DICompileUnit *CU =
            dbuilder.createCompileUnit(llvm::dwarf::DW_LANG_Julia
                                       ,topfile      // File
                                       ,"julia"      // Producer
                                       ,true         // isOptimized
                                       ,""           // Flags
                                       ,0            // RuntimeVersion
                                       ,""           // SplitName
                                       ,emissionKind // Kind
                                       ,0            // DWOId
                                       ,true         // SplitDebugInlining
                                       ,false        // DebugInfoForProfiling
                                       ,tableKind    // NameTableKind
                                       );

        DISubroutineType *subrty;
        if (jl_options.debug_level <= 1) {
            subrty = jl_di_func_null_sig;
        }
        else if (!specsig) {
            subrty = jl_di_func_sig;
        }
        else {
            subrty = get_specsig_di(ctx, jlrettype, lam->specTypes, dbuilder);
        }
        SP = dbuilder.createFunction(CU
                                     ,dbgFuncName      // Name
                                     ,f->getName()     // LinkageName
                                     ,topfile          // File
                                     ,toplineno        // LineNo
                                     ,subrty           // Ty
                                     ,toplineno        // ScopeLine
                                     ,DINode::FlagZero // Flags
                                     ,DISubprogram::SPFlagDefinition | DISubprogram::SPFlagOptimized // SPFlags
                                     ,nullptr          // Template Parameters
                                     ,nullptr          // Template Declaration
                                     ,nullptr          // ThrownTypes
                                     );
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
                    has_sret + i + 1,                   // Argument number (1-based)
                    topfile,                            // File
                    toplineno == -1 ? 0 : toplineno,    // Line
                    // Variable type
                    julia_type_to_di(ctx, varinfo.value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DINode::FlagZero);                  // Flags (TODO: Do we need any)
            }
            if (va && ctx.vaSlot != -1) {
                ctx.slots[ctx.vaSlot].dinfo = dbuilder.createParameterVariable(
                    SP,                                 // Scope (current function will be fill in later)
                    std::string(jl_symbol_name(slot_symbol(ctx, ctx.vaSlot))) + "...",  // Variable name
                    has_sret + nreq + 1,                // Argument number (1-based)
                    topfile,                            // File
                    toplineno == -1 ? 0 : toplineno,    // Line (for now, use lineno of the function)
                    julia_type_to_di(ctx, ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DINode::FlagZero);                  // Flags (TODO: Do we need any)
            }
            for (i = 0; i < vinfoslen; i++) {
                jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
                jl_varinfo_t &varinfo = ctx.slots[i];
                if (varinfo.isArgument || s == empty_sym || s == unused_sym)
                    continue;
                // LLVM 4.0: Assume the variable has default alignment
                varinfo.dinfo = dbuilder.createAutoVariable(
                    SP,                      // Scope (current function will be fill in later)
                    jl_symbol_name(s),       // Variable name
                    topfile,                 // File
                    toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                    julia_type_to_di(ctx, varinfo.value.typ, &dbuilder, false), // Variable type
                    AlwaysPreserve,          // May be deleted if optimized out
                    DINode::FlagZero         // Flags (TODO: Do we need any)
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
        fArg = &*AI++;
        argArray = &*AI++;
        pargArray = ctx.builder.CreateAlloca(argArray->getType());
        ctx.builder.CreateStore(argArray, pargArray, true/*volatile store to prevent removal of this alloca*/);
        argCount = &*AI++;
        ctx.argArray = argArray;
        ctx.argCount = argCount;
        if (needsparams) {
            ctx.spvals_ptr = &*AI++;
        }
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
    auto allocate_local = [&](jl_varinfo_t &varinfo, jl_sym_t *s) {
        jl_value_t *jt = varinfo.value.typ;
        assert(!varinfo.boxroot); // variables shouldn't have memory locs already
        if (varinfo.value.constant) {
            // no need to explicitly load/store a constant/ghost value
            alloc_def_flag(ctx, varinfo);
            return;
        }
        else if (varinfo.isArgument && !(specsig && i == (size_t)ctx.vaSlot)) {
            // if we can unbox it, just use the input pointer
            if (i != (size_t)ctx.vaSlot && jl_is_concrete_immutable(jt))
                return;
        }
        else if (jl_is_uniontype(jt)) {
            bool allunbox;
            size_t align, nbytes;
            Value *lv = try_emit_union_alloca(ctx, (jl_uniontype_t*)jt, allunbox, align, nbytes);
            if (lv) {
                lv->setName(jl_symbol_name(s));
                varinfo.value = mark_julia_slot(lv, jt, NULL, tbaa_stack);
                varinfo.pTIndex = emit_static_alloca(ctx, T_int8);
            }
            else if (allunbox) {
                // all ghost values just need a selector allocated
                AllocaInst *lv = emit_static_alloca(ctx, T_int8);
                lv->setName(jl_symbol_name(s));
                varinfo.pTIndex = lv;
                varinfo.value.tbaa = NULL;
                varinfo.value.isboxed = false;
            }
            if (lv || allunbox)
                alloc_def_flag(ctx, varinfo);
            if (allunbox)
                return;
        }
        else if (deserves_stack(jt, true)) {
            bool isboxed;
            Type *vtype = julia_type_to_llvm(ctx, jt, &isboxed);
            assert(!isboxed);
            assert(!type_is_ghost(vtype) && "constants should already be handled");
            // CreateAlloca is OK during prologue setup
            Value *lv = ctx.builder.CreateAlloca(vtype, NULL, jl_symbol_name(s));
            varinfo.value = mark_julia_slot(lv, jt, NULL, tbaa_stack);
            alloc_def_flag(ctx, varinfo);
            if (ctx.debug_enabled && varinfo.dinfo) {
                assert((Metadata*)varinfo.dinfo->getType() != jl_pvalue_dillvmt);
                dbuilder.insertDeclare(lv, varinfo.dinfo, dbuilder.createExpression(),
                                       topdebugloc,
                                       ctx.builder.GetInsertBlock());
            }
            return;
        }
        if (!varinfo.isArgument || // always need a slot if the variable is assigned
            specsig || // for arguments, give them stack slots if they aren't in `argArray` (otherwise, will use that pointer)
            (va && (int)i == ctx.vaSlot) || // or it's the va arg tuple
            i == 0) { // or it is the first argument (which isn't in `argArray`)
            AllocaInst *av = new AllocaInst(T_prjlvalue, 0,
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
    };

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
        allocate_local(varinfo, s);
    }

    std::map<int, int> upsilon_to_phic;

    // Scan for PhiC nodes, emit their slots and record which upsilon nodes
    // yield to them.
    {
        for (size_t i = 0; i < jl_array_len(stmts); ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            if (jl_is_phicnode(stmt)) {
                jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(stmt, 0);
                for (size_t j = 0; j < jl_array_len(values); ++j) {
                    jl_value_t *val = jl_array_ptr_ref(values, j);
                    assert(jl_is_ssavalue(val));
                    upsilon_to_phic[((jl_ssavalue_t*)val)->id] = i;
                }
                ctx.phic_slots[i] = jl_varinfo_t{};
                jl_varinfo_t &vi = ctx.phic_slots[i];
                jl_value_t *typ = jl_array_ptr_ref(src->ssavaluetypes, i);
                vi.used = true;
                vi.isVolatile = true;
                vi.value = mark_julia_type(ctx, (Value*)NULL, false, typ);
                allocate_local(vi, jl_symbol("phic"));
            }
        }
    }

    // step 9. move args into local variables
    Function::arg_iterator AI = f->arg_begin();

    auto get_specsig_arg = [&](jl_value_t *argType, Type *llvmArgType, bool isboxed) {
        jl_cgval_t theArg;
        if (type_is_ghost(llvmArgType)) { // this argument is not actually passed
            theArg = ghostValue(argType);
        }
        else if (llvmArgType->isAggregateType()) {
            Argument *Arg = &*AI; ++AI;
            maybe_mark_argument_dereferenceable(Arg, argType);
            theArg = mark_julia_slot(Arg, argType, NULL, tbaa_const); // this argument is by-pointer
        }
        else {
            Argument *Arg = &*AI; ++AI;
            if (isboxed) // e.g. is-pointer
                maybe_mark_argument_dereferenceable(Arg, argType);
            theArg = mark_julia_type(ctx, Arg, isboxed, argType);
        }
        return theArg;
    };

    if (has_sret)
        AI++; // skip sret slot
    if (returninfo.return_roots)
        AI++; // skip return_roots slot
    for (i = 0; i < nreq; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
        bool isboxed = deserves_argbox(argType);
        Type *llvmArgType = isboxed ? T_prjlvalue : julia_type_to_llvm(ctx, argType);
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
                theArg = get_specsig_arg(argType, llvmArgType, isboxed);
            }
            else {
                if (i == 0) {
                    // first (function) arg is separate in jlcall
                    theArg = mark_julia_type(ctx, fArg, true, vi.value.typ);
                }
                else {
                    Value *argPtr = ctx.builder.CreateInBoundsGEP(T_prjlvalue, argArray, ConstantInt::get(T_size, i-1));
                    auto load = maybe_mark_load_dereferenceable(ctx.builder.CreateLoad(T_prjlvalue, argPtr),
                                                                false, vi.value.typ);
                    theArg = mark_julia_type(ctx, load, true, vi.value.typ);
                    if (ctx.debug_enabled && vi.dinfo && !vi.boxroot && !vi.value.V) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus_uconst);
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
                    Value *parg;
                    if (theArg.ispointer()) {
                        parg = theArg.V;
                        if ((Metadata*)vi.dinfo->getType() != jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                    }
                    else {
                        parg = ctx.builder.CreateAlloca(theArg.V->getType(), NULL, jl_symbol_name(s));
                        ctx.builder.CreateStore(theArg.V, parg);
                    }
                    dbuilder.insertDeclare(parg, vi.dinfo, dbuilder.createExpression(addr),
                                                topdebugloc,
                                                ctx.builder.GetInsertBlock());
                }
            }
            else {
                Value *argp = boxed(ctx, theArg);
                ctx.builder.CreateStore(argp, vi.boxroot);
            }
        }
    }

    // step 10. allocate rest argument
    CallInst *restTuple = NULL;
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &vi = ctx.slots[ctx.vaSlot];
        if (vi.value.constant || !vi.used) {
            assert(vi.boxroot == NULL);
        }
        else if (specsig) {
            ctx.nvargs = jl_nparams(lam->specTypes) - nreq;
            jl_cgval_t *vargs = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * ctx.nvargs);
            for (size_t i = nreq; i < jl_nparams(lam->specTypes); ++i) {
                jl_value_t *argType = jl_nth_slot_type(lam->specTypes, i);
                bool isboxed = deserves_argbox(argType);
                Type *llvmArgType = isboxed ?  T_prjlvalue : julia_type_to_llvm(ctx, argType);
                vargs[i - nreq] = get_specsig_arg(argType, llvmArgType, isboxed);
            }
            if (jl_is_concrete_type(vi.value.typ)) {
                jl_cgval_t tuple = emit_new_struct(ctx, vi.value.typ, ctx.nvargs, vargs);
                emit_varinfo_assign(ctx, vi, tuple);
            }
            else {
                restTuple = emit_jlcall(ctx, prepare_call(jltuple_func), maybe_decay_untracked(V_null),
                    vargs, ctx.nvargs, JLCALL_F_CC);
                jl_cgval_t tuple = mark_julia_type(ctx, restTuple, true, vi.value.typ);
                emit_varinfo_assign(ctx, vi, tuple);
            }
        }
        else {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs - nreq)
            restTuple =
                ctx.builder.CreateCall(prepare_call(jltuple_func),
                        { maybe_decay_untracked(V_null),
                          ctx.builder.CreateInBoundsGEP(T_prjlvalue, argArray,
                                  ConstantInt::get(T_size, nreq - 1)),
                          ctx.builder.CreateSub(argCount,
                                  ConstantInt::get(T_int32, nreq - 1)) });
            restTuple->setAttributes(jltuple_func->getAttributes());
            ctx.builder.CreateStore(restTuple, vi.boxroot);
        }
    }

    // step 11. Compute properties for each statements
    //     This needs to be computed by iterating in the IR order
    //     instead of control flow order.
    auto in_user_mod = [] (jl_module_t *mod) {
        return (!jl_is_submodule(mod, jl_base_module) &&
                !jl_is_submodule(mod, jl_core_module));
    };
    bool mod_is_user_mod = in_user_mod(ctx.module);
    struct DebugLineTable {
        DebugLoc loc;
        StringRef file;
        ssize_t line;
        bool is_user_code;
        unsigned inlined_at;
    };
    std::vector<DebugLineTable> linetable;
    {
        size_t nlocs = jl_array_len(src->linetable);
        std::map<std::tuple<StringRef, StringRef>, DISubprogram*> subprograms;
        linetable.resize(nlocs + 1);
        for (size_t i = 0; i < nlocs; i++) {
            // LineInfoNode(mod::Module, method::Any, file::Symbol, line::Int, inlined_at::Int)
            jl_value_t *locinfo = jl_array_ptr_ref(src->linetable, i);
            DebugLineTable &info = linetable[i + 1];
            assert(jl_typeis(locinfo, jl_lineinfonode_type));
            jl_value_t *method = jl_fieldref_noalloc(locinfo, 0);
            if (jl_is_method_instance(method))
                method = ((jl_method_instance_t*)method)->def.value;
            jl_sym_t *filesym = (jl_sym_t*)jl_fieldref_noalloc(locinfo, 1);
            info.line = jl_unbox_long(jl_fieldref(locinfo, 2));
            info.inlined_at = jl_unbox_long(jl_fieldref(locinfo, 3));
            assert(info.inlined_at <= i);
            if (jl_is_method(method)) {
                jl_module_t *module = ((jl_method_t*)method)->module;
                if (module == ctx.module)
                    info.is_user_code = mod_is_user_mod;
                else
                    info.is_user_code = in_user_mod(module);
            }
            else {
                info.is_user_code = (info.inlined_at == 0) ? mod_is_user_mod : linetable.at(info.inlined_at).is_user_code;
            }
            info.file = jl_symbol_name(filesym);
            if (info.file.empty())
                info.file = "<missing>";
            if (ctx.debug_enabled) {
                StringRef fname;
                if (jl_is_method(method))
                    method = (jl_value_t*)((jl_method_t*)method)->name;
                if (jl_is_symbol(method))
                    fname = jl_symbol_name((jl_sym_t*)method);
                if (fname.empty())
                    fname = "macro expansion";
                if (info.inlined_at == 0 && info.file == ctx.file) { // if everything matches, emit a toplevel line number
                    info.loc = DebugLoc::get(info.line, 0, SP, NULL);
                }
                else { // otherwise, describe this as an inlining frame
                    DISubprogram *&inl_SP = subprograms[std::make_tuple(fname, info.file)];
                    if (inl_SP == NULL) {
                        DIFile *difile = dbuilder.createFile(info.file, ".");
                        inl_SP = dbuilder.createFunction(difile
                                                     ,std::string(fname) + ";" // Name
                                                     ,fname            // LinkageName
                                                     ,difile           // File
                                                     ,0                // LineNo
                                                     ,jl_di_func_null_sig // Ty
                                                     ,0                // ScopeLine
                                                     ,DINode::FlagZero // Flags
                                                     ,DISubprogram::SPFlagDefinition | DISubprogram::SPFlagOptimized // SPFlags
                                                     ,nullptr          // Template Parameters
                                                     ,nullptr          // Template Declaration
                                                     ,nullptr          // ThrownTypes
                                                     );
                    }
                    DebugLoc inl_loc = (info.inlined_at == 0) ? DebugLoc::get(0, 0, SP, NULL) : linetable.at(info.inlined_at).loc;
                    info.loc = DebugLoc::get(info.line, 0, inl_SP, inl_loc);
                }
            }
        }
    }

    std::vector<MDNode*> aliasscopes;
    MDNode* current_aliasscope = nullptr;
    std::vector<Metadata*> scope_stack;
    std::vector<MDNode*> scope_list_stack;
    {
        size_t nstmts = jl_array_len(stmts);
        aliasscopes.resize(nstmts + 1, nullptr);
        MDBuilder mbuilder(jl_LLVMContext);
        MDNode *alias_domain = mbuilder.createAliasScopeDomain(ctx.name);
        for (i = 0; i < nstmts; i++) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            jl_expr_t *expr = jl_is_expr(stmt) ? (jl_expr_t*)stmt : nullptr;
            if (expr) {
                if (expr->head == aliasscope_sym) {
                    MDNode *scope = mbuilder.createAliasScope("aliasscope", alias_domain);
                    scope_stack.push_back(scope);
                    MDNode *scope_list = MDNode::get(jl_LLVMContext, ArrayRef<Metadata*>(scope_stack));
                    scope_list_stack.push_back(scope_list);
                    current_aliasscope = scope_list;
                } else if (expr->head == popaliasscope_sym) {
                    scope_stack.pop_back();
                    scope_list_stack.pop_back();
                    if (scope_list_stack.empty()) {
                        current_aliasscope = NULL;
                    } else {
                        current_aliasscope = scope_list_stack.back();
                    }
                }
            }
            aliasscopes[i+1] = current_aliasscope;
        }
    }

    Instruction &prologue_end = ctx.builder.GetInsertBlock()->back();


    // step 12. Do codegen in control flow order
    std::vector<int> workstack;
    std::map<int, BasicBlock*> BB;
    std::map<size_t, BasicBlock*> come_from_bb;
    int cursor = 0;
    auto find_next_stmt = [&] (int seq_next) {
        // new style ir is always in dominance order, but frontend IR might not be
        // `seq_next` is the next statement we want to emit
        // i.e. if it exists, it's the next one following control flow and
        // should be emitted into the current insert point.
        if (seq_next >= 0 && (unsigned)seq_next < stmtslen) {
            workstack.push_back(seq_next);
        }
        else if (!ctx.builder.GetInsertBlock()->getTerminator()) {
            ctx.builder.CreateUnreachable();
        }
        while (!workstack.empty()) {
            int item = workstack.back();
            workstack.pop_back();
            auto nextbb = BB.find(item + 1);
            if (nextbb == BB.end()) {
                cursor = item;
                return;
            }
            if (seq_next != -1 && !ctx.builder.GetInsertBlock()->getTerminator()) {
                come_from_bb[cursor + 1] = ctx.builder.GetInsertBlock();
                ctx.builder.CreateBr(nextbb->second);
            }
            seq_next = -1;
            // if this BB is non-empty, we've visited it before so skip it
            if (!nextbb->second->getTerminator()) {
                ctx.builder.SetInsertPoint(nextbb->second);
                cursor = item;
                return;
            }
        }
        cursor = -1;
    };

    auto do_coverage = [&] (bool in_user_code) {
        return (coverage_mode == JL_LOG_ALL ||
                (coverage_mode == JL_LOG_USER && in_user_code));
    };
    auto do_malloc_log = [&] (bool in_user_code) {
        return (malloc_log_mode == JL_LOG_ALL ||
                (malloc_log_mode == JL_LOG_USER && in_user_code));
    };
    std::vector<unsigned> current_lineinfo, new_lineinfo;
    auto coverageVisitStmt = [&] (size_t dbg) {
        if (dbg == 0)
            return;
        while (dbg) {
            new_lineinfo.push_back(dbg);
            dbg = linetable.at(dbg).inlined_at;
        }
        current_lineinfo.resize(new_lineinfo.size(), 0);
        for (dbg = 0; dbg < new_lineinfo.size(); dbg++) {
            unsigned newdbg = new_lineinfo[new_lineinfo.size() - dbg - 1];
            if (newdbg != current_lineinfo[dbg]) {
                current_lineinfo[dbg] = newdbg;
                const auto &info = linetable.at(newdbg);
                if (do_coverage(info.is_user_code))
                    coverageVisitLine(ctx, info.file, info.line);
            }
        }
        new_lineinfo.clear();
    };
    auto mallocVisitStmt = [&] (unsigned dbg, Value *sync) {
        if (!do_malloc_log(mod_is_user_mod) || dbg == 0) {
            if (do_malloc_log(true) && sync)
                ctx.builder.CreateCall(prepare_call(sync_gc_total_bytes_func), {sync});
            return;
        }
        while (linetable.at(dbg).inlined_at)
            dbg = linetable.at(dbg).inlined_at;
        mallocVisitLine(ctx, ctx.file, linetable.at(dbg).line, sync);
    };
    if (coverage_mode != JL_LOG_NONE) {
        // record all lines that could be covered
        for (const auto &info : linetable)
            if (do_coverage(info.is_user_code))
                coverageAllocLine(info.file, info.line);
    }

    come_from_bb[0] = ctx.builder.GetInsertBlock();

    // First go through and collect all branch targets, so we know where to
    // split basic blocks.
    std::set<int> branch_targets; // 1-indexed
    {
        for (size_t i = 0; i < stmtslen; ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            if (jl_is_expr(stmt)) {
                if (((jl_expr_t*)stmt)->head == goto_ifnot_sym) {
                    int dest = jl_unbox_long(jl_array_ptr_ref(((jl_expr_t*)stmt)->args, 1));
                    branch_targets.insert(dest);
                    // The next 1-indexed statement
                    branch_targets.insert(i + 2);
                } else if (((jl_expr_t*)stmt)->head == return_sym) {
                    // We don't do dead branch elimination before codegen
                    // so we need to make sure to start a BB after any
                    // return node, even if they aren't otherwise branch
                    // targets.
                    if (i + 2 <= stmtslen)
                        branch_targets.insert(i + 2);
                } else if (((jl_expr_t*)stmt)->head == unreachable_sym) {
                    if (i + 2 <= stmtslen)
                        branch_targets.insert(i + 2);
                } else if (((jl_expr_t*)stmt)->head == enter_sym) {
                    branch_targets.insert(i + 1);
                    if (i + 2 <= stmtslen)
                        branch_targets.insert(i + 2);
                    int dest = jl_unbox_long(jl_array_ptr_ref(((jl_expr_t*)stmt)->args, 0));
                    branch_targets.insert(dest);
                }
            } else if (jl_is_gotonode(stmt)) {
                int dest = jl_gotonode_label(stmt);
                branch_targets.insert(dest);
                if (i + 2 <= stmtslen)
                    branch_targets.insert(i + 2);
            } else if (jl_is_phinode(stmt)) {
                jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(stmt, 0);
                for (size_t j = 0; j < jl_array_len(edges); ++j) {
                    size_t edge = jl_unbox_long(jl_array_ptr_ref(edges, j));
                    if (edge == i)
                        branch_targets.insert(i + 1);
                }
            }
        }
    }

    for (int label : branch_targets) {
        BasicBlock *bb = BasicBlock::Create(jl_LLVMContext,
            "L" + std::to_string(label), f);
        BB[label] = bb;
    }

    if (do_coverage(mod_is_user_mod)) {
        coverageVisitLine(ctx, ctx.file, toplineno);
        if (linetable.size() >= 2) {
            // avoid double-counting the entry line
            const auto &info = linetable.at(1);
            if (info.file == ctx.file && info.line == toplineno && info.is_user_code == mod_is_user_mod)
                current_lineinfo.push_back(1);
        }
    }
    Value *sync_bytes = nullptr;
    if (do_malloc_log(true))
        sync_bytes = ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});

    find_next_stmt(0);
    while (cursor != -1) {
        int32_t debuginfoloc = ((int32_t*)jl_array_data(src->codelocs))[cursor];
        if (debuginfoloc > 0) {
            if (ctx.debug_enabled)
                ctx.builder.SetCurrentDebugLocation(linetable.at(debuginfoloc).loc);
            coverageVisitStmt(debuginfoloc);
        }
        ctx.aliasscope = aliasscopes[cursor];
        jl_value_t *stmt = jl_array_ptr_ref(stmts, cursor);
        jl_expr_t *expr = jl_is_expr(stmt) ? (jl_expr_t*)stmt : nullptr;
        if (expr && expr->head == unreachable_sym) {
            ctx.builder.CreateUnreachable();
            find_next_stmt(-1);
            continue;
        }
        if (expr && expr->head == return_sym) {
            // this is basically a copy of emit_assignment,
            // but where the assignment slot is the retval
            jl_cgval_t retvalinfo = emit_expr(ctx, jl_exprarg(expr, 0));
            retvalinfo = convert_julia_type(ctx, retvalinfo, jlrettype);
            if (retvalinfo.typ == jl_bottom_type) {
                ctx.builder.CreateUnreachable();
                find_next_stmt(-1);
                continue;
            }

            Value *isboxed_union = NULL;
            Value *retval = NULL;
            Value *sret = has_sret ? f->arg_begin() : NULL;
            Type *retty = f->getReturnType();
            switch (returninfo.cc) {
            case jl_returninfo_t::Boxed:
                retval = boxed(ctx, retvalinfo); // skip the gcroot on the return path
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
                        if (retvalinfo.Vboxed) {
                            // also need to account for the possibility the return object is boxed
                            // and avoid / skip copying it to the stack
                            isboxed_union = ctx.builder.CreateICmpNE(
                                    ctx.builder.CreateAnd(tindex, ConstantInt::get(T_int8, 0x80)),
                                    ConstantInt::get(T_int8, 0));
                            data = ctx.builder.CreateSelect(isboxed_union, retvalinfo.Vboxed, data);
                        }
                    }
                }
                else {
                    // treat this as a simple boxed returninfo
                    //assert(retvalinfo.isboxed);
                    tindex = compute_tindex_unboxed(ctx, retvalinfo, jlrettype);
                    tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(T_int8, 0x80));
                    data = maybe_decay_untracked(boxed(ctx, retvalinfo));
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
                    if (returninfo.return_roots) {
                        Type *store_ty = julia_type_to_llvm(ctx, retvalinfo.typ);
                        emit_sret_roots(ctx, true, data_pointer(ctx, retvalinfo), store_ty, f->arg_begin() + 1, returninfo.return_roots);
                    }
                    if (returninfo.cc == jl_returninfo_t::SRet) {
                        assert(jl_is_concrete_type(jlrettype));
                        emit_memcpy(ctx, sret, nullptr, retvalinfo, jl_datatype_size(jlrettype),
                                    julia_alignment(jlrettype));
                    }
                    else { // must be jl_returninfo_t::Union
                        emit_unionmove(ctx, sret, nullptr, retvalinfo, /*skip*/isboxed_union);
                    }
                }
                else {
                    Type *store_ty = retvalinfo.V->getType();
                    Type *dest_ty = store_ty->getPointerTo();
                    Value *Val = retvalinfo.V;
                    if (returninfo.return_roots) {
                        assert(julia_type_to_llvm(ctx, retvalinfo.typ) == store_ty);
                        emit_sret_roots(ctx, false, Val, store_ty, f->arg_begin() + 1, returninfo.return_roots);
                    }
                    if (dest_ty != sret->getType())
                        sret = emit_bitcast(ctx, sret, dest_ty);
                    ctx.builder.CreateAlignedStore(Val, sret, julia_alignment(retvalinfo.typ));
                    assert(retvalinfo.TIndex == NULL && "unreachable"); // unimplemented representation
                }
            }

            mallocVisitStmt(debuginfoloc, sync_bytes);
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
            come_from_bb[cursor+1] = ctx.builder.GetInsertBlock();
            ctx.builder.CreateBr(BB[lname]);
            find_next_stmt(lname - 1);
            continue;
        }
        if (jl_is_upsilonnode(stmt)) {
            jl_value_t *val = jl_fieldref_noalloc(stmt, 0);
            // If the val is null, we can ignore the store.
            // The middle end guarantees that the value from this
            // upsilon node is not dynamically observed.
            jl_varinfo_t &vi = ctx.phic_slots[upsilon_to_phic[cursor+1]];
            if (val) {
                jl_cgval_t rval_info = emit_expr(ctx, val);
                emit_varinfo_assign(ctx, vi, rval_info);
            } else if (vi.pTIndex) {
                // We don't care what the contents of the variable are, but it
                // does need to satisfy the union invariants (i.e. inbounds
                // tindex).
                ctx.builder.CreateStore(
                    vi.boxroot ? ConstantInt::get(T_int8, 0x80) :
                                 ConstantInt::get(T_int8, 0x01),
                    vi.pTIndex, true);
            }
            find_next_stmt(cursor + 1);
            continue;
        }
        if (expr && expr->head == goto_ifnot_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);
            jl_value_t *cond = args[0];
            int lname = jl_unbox_long(args[1]);
            Value *isfalse = emit_condition(ctx, cond, "if");
            mallocVisitStmt(debuginfoloc, nullptr);
            come_from_bb[cursor+1] = ctx.builder.GetInsertBlock();
            workstack.push_back(lname - 1);
            BasicBlock *ifnot = BB[lname];
            BasicBlock *ifso = BB[cursor+2];
            if (ifnot == ifso)
                ctx.builder.CreateBr(ifnot);
            else
                ctx.builder.CreateCondBr(isfalse, ifnot, ifso);
            find_next_stmt(cursor + 1);
            continue;
        }
        else if (expr && expr->head == enter_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);

            assert(jl_is_long(args[0]));
            int lname = jl_unbox_long(args[0]);
            // Save exception stack depth at enter for use in pop_exception
            Value *excstack_state =
                ctx.builder.CreateCall(prepare_call(jl_excstack_state_func));
            assert(!ctx.ssavalue_assigned.at(cursor));
            ctx.SAvalues.at(cursor) = jl_cgval_t(excstack_state, NULL, false,
                                                 (jl_value_t*)jl_ulong_type, NULL);
            ctx.ssavalue_assigned.at(cursor) = true;
            CallInst *sj = ctx.builder.CreateCall(prepare_call(except_enter_func));
            // We need to mark this on the call site as well. See issue #6757
            sj->setCanReturnTwice();
            Value *isz = ctx.builder.CreateICmpEQ(sj, ConstantInt::get(T_int32, 0));
            BasicBlock *tryblk = BasicBlock::Create(jl_LLVMContext, "try", f);
            BasicBlock *handlr = NULL;
            handlr = BB[lname];
            workstack.push_back(lname - 1);
            come_from_bb[cursor + 1] = ctx.builder.GetInsertBlock();
            ctx.builder.CreateCondBr(isz, tryblk, handlr);
            ctx.builder.SetInsertPoint(tryblk);
        }
        else {
            emit_stmtpos(ctx, stmt, cursor);
            mallocVisitStmt(debuginfoloc, nullptr);
        }
        find_next_stmt(cursor + 1);
    }

    // Delete any unreachable blocks
    for (auto &item : BB) {
        if (!item.second->getTerminator())
            item.second->eraseFromParent();
    }

    ctx.builder.SetCurrentDebugLocation(noDbg);
    ctx.builder.ClearInsertionPoint();

    auto undef_value_for_type = [&](Type *T) {
        auto tracked = CountTrackedPointers(T);
        Value *undef;
        if (tracked.count)
            // make sure gc pointers (including ptr_phi of union-split) are initialized to NULL
            undef = Constant::getNullValue(T);
        else
            undef = UndefValue::get(T);
        return undef;
    };

    // Codegen Phi nodes
    std::map<std::pair<BasicBlock*, BasicBlock*>, BasicBlock*> BB_rewrite_map;
    std::vector<llvm::PHINode*> ToDelete;
    for (auto &tup : ctx.PhiNodes) {
        jl_cgval_t phi_result;
        PHINode *VN;
        jl_value_t *r;
        AllocaInst *dest;
        BasicBlock *PhiBB;
        std::tie(phi_result, PhiBB, dest, VN, r) = tup;
        jl_value_t *phiType = phi_result.typ;
        jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(r, 0);
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(r, 1);
        PHINode *TindexN = cast_or_null<PHINode>(phi_result.TIndex);
        DenseSet<BasicBlock*> preds;
        for (size_t i = 0; i < jl_array_len(edges); ++i) {
            size_t edge = jl_unbox_long(jl_array_ptr_ref(edges, i));
            jl_value_t *value = jl_array_ptr_ref(values, i);
            // This edge value is undef, handle it the same as if the edge wasn't listed at all
            if (!value)
                continue;
            BasicBlock *FromBB = come_from_bb[edge];
            // This edge was statically unreachable. Don't codegen it.
            if (!FromBB)
                continue;
            // see if this edge has already been rewritten
            // (we'll continue appending blocks to the current end)
            std::pair<BasicBlock*, BasicBlock*> LookupKey(FromBB, PhiBB);
            if (BB_rewrite_map.count(LookupKey)) {
                FromBB = BB_rewrite_map[LookupKey];
            }
            if (!preds.insert(FromBB).second) {
                // Only codegen this branch once for each PHI (the expression must be the same on all branches)
#ifndef NDEBUG
                for (size_t j = 0; j < i; ++j) {
                    size_t j_edge = jl_unbox_long(jl_array_ptr_ref(edges, j));
                    if (j_edge == edge) {
                        assert(jl_egal(value, jl_array_ptr_ref(values, j)));
                    }
                }
#endif
                continue;
            }
            assert(find(pred_begin(PhiBB), pred_end(PhiBB), FromBB) != pred_end(PhiBB)); // consistency check
            TerminatorInst *terminator = FromBB->getTerminator();
            if (!terminator->getParent()->getUniqueSuccessor()) {
                // Can't use `llvm::SplitCriticalEdge` here because
                // we may have invalid phi nodes in the destination.
                BasicBlock *NewBB = BasicBlock::Create(terminator->getContext(),
                   FromBB->getName() + "." + PhiBB->getName() + "_crit_edge");
                Function::iterator FBBI = FromBB->getIterator();
                ctx.f->getBasicBlockList().insert(++FBBI, NewBB); // insert after existing block
#if JL_LLVM_VERSION >= 90000
                terminator->replaceSuccessorWith(PhiBB, NewBB);
#else
                for (unsigned Idx = 0, NumSuccessors = terminator->getNumSuccessors(); Idx != NumSuccessors; ++Idx) {
                    if (terminator->getSuccessor(Idx) == PhiBB)
                      terminator->setSuccessor(Idx, NewBB);
                }
#endif
                DebugLoc Loc = terminator->getDebugLoc();
                terminator = BranchInst::Create(PhiBB);
                terminator->setDebugLoc(Loc);
                ctx.builder.SetInsertPoint(NewBB);
            }
            else {
                terminator->removeFromParent();
                ctx.builder.SetInsertPoint(FromBB);
            }
            if (dest)
                ctx.builder.CreateLifetimeStart(dest);
            jl_cgval_t val = emit_expr(ctx, value);
            if (val.constant)
                val = mark_julia_const(val.constant); // be over-conservative at making sure `.typ` is set concretely, not tindex
            if (!jl_is_uniontype(phiType) || !TindexN) {
                Type *lty = julia_type_to_llvm(ctx, phiType);
                if (VN) {
                    Value *V;
                    if (val.typ == (jl_value_t*)jl_bottom_type) {
                        V = undef_value_for_type(VN->getType());
                    }
                    else if (VN && VN->getType() == T_prjlvalue) {
                        // Includes the jl_is_uniontype(phiType) && !TindexN case
                        V = boxed(ctx, val);
                    }
                    else {
                        V = emit_unbox(ctx, VN->getType(), val, phiType);
                    }
                    VN->addIncoming(V, ctx.builder.GetInsertBlock());
                    assert(!TindexN);
                }
                else if (dest && val.typ != (jl_value_t*)jl_bottom_type) {
                    assert(lty != T_prjlvalue);
                    (void)emit_unbox(ctx, lty, val, phiType, maybe_decay_tracked(dest));
                }
            }
            else {
                Value *RTindex;
                Value *V;
                if (val.typ == (jl_value_t*)jl_bottom_type) {
                    V = undef_value_for_type(VN->getType());
                    RTindex = UndefValue::get(T_int8);
                }
                else if (jl_is_concrete_type(val.typ) || val.constant) {
                    size_t tindex = get_box_tindex((jl_datatype_t*)val.typ, phiType);
                    if (tindex == 0) {
                        V = boxed(ctx, val);
                        RTindex = ConstantInt::get(T_int8, 0x80);
                    }
                    else {
                        V = ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
                        Type *lty = julia_type_to_llvm(ctx, val.typ);
                        if (dest && !type_is_ghost(lty)) // basically, if !ghost union
                            emit_unbox(ctx, lty, val, val.typ, dest);
                        RTindex = ConstantInt::get(T_int8, tindex);
                    }
                }
                else {
                    jl_cgval_t new_union = convert_julia_type(ctx, val, phiType);
                    RTindex = new_union.TIndex;
                    if (!RTindex) {
                        assert(new_union.isboxed && new_union.Vboxed && "convert_julia_type failed");
                        RTindex = compute_tindex_unboxed(ctx, new_union, phiType);
                        if (dest) {
                            // If dest is not set, this is a ghost union, the recipient of which
                            // is often not prepared to handle a boxed representation of the ghost.
                            RTindex = ctx.builder.CreateOr(RTindex, ConstantInt::get(T_int8, 0x80));
                        }
                        new_union.TIndex = RTindex;
                    }
                    V = new_union.Vboxed ? new_union.Vboxed : ConstantPointerNull::get(cast<PointerType>(T_prjlvalue));
                    if (dest) { // basically, if !ghost union
                        Value *skip = NULL;
                        if (new_union.Vboxed != nullptr)
                            skip = ctx.builder.CreateICmpNE( // if 0x80 is set, we won't select this slot anyways
                                    ctx.builder.CreateAnd(RTindex, ConstantInt::get(T_int8, 0x80)),
                                    ConstantInt::get(T_int8, 0));
                        emit_unionmove(ctx, dest, tbaa_arraybuf, new_union, skip);
                    }
                }
                if (VN)
                    VN->addIncoming(V, ctx.builder.GetInsertBlock());
                if (TindexN)
                    TindexN->addIncoming(RTindex, ctx.builder.GetInsertBlock());
            }
            // put the branch back at the end of our current basic block
            ctx.builder.Insert(terminator);
            // Record the current tail of this Phi edge in the rewrite map and
            // check any phi nodes in the Phi block to see if by emitting on the edges
            // we made things inconsistent.
            BasicBlock *NewBB = ctx.builder.GetInsertBlock();
            if (FromBB != NewBB) {
                BB_rewrite_map[LookupKey] = NewBB;
                preds.insert(NewBB);
#if JL_LLVM_VERSION >= 90000
                PhiBB->replacePhiUsesWith(FromBB, NewBB);
#else
                for (BasicBlock::iterator I = PhiBB->begin(); isa<PHINode>(I); ++I) {
                    PHINode *PN = cast<PHINode>(I);
                    ssize_t BBIdx = PN->getBasicBlockIndex(FromBB);
                    if (BBIdx == -1)
                        continue;
                    PN->setIncomingBlock(BBIdx, NewBB);
                }
#endif
            }
            ctx.builder.ClearInsertionPoint();
        }
        // In LLVM IR it is illegal to have phi nodes without incoming values, even if
        // there are no operands (no incoming branches), so delete any such phi nodes
        if (pred_empty(PhiBB)) {
            if (VN)
                ToDelete.push_back(VN);
            if (TindexN)
                ToDelete.push_back(TindexN);
            continue;
        }
        // Julia PHINodes may be incomplete with respect to predecessors, LLVM's may not
        for (auto *FromBB : predecessors(PhiBB)) {
            if (preds.count(FromBB))
                continue;
            ctx.builder.SetInsertPoint(FromBB->getTerminator());
            // PHI is undef on this branch. But still may need to put a valid pointer in place.
            Value *RTindex = TindexN ? UndefValue::get(T_int8) : NULL;
            if (VN) {
                Value *undef = undef_value_for_type(VN->getType());
                VN->addIncoming(undef, FromBB);
                if (TindexN) // let the runtime / optimizer know this is unknown / boxed / null, so that it won't try to union_move / copy it later
                    RTindex = ConstantInt::get(T_int8, 0x80);
            }
            if (TindexN)
                TindexN->addIncoming(RTindex, FromBB);
            if (dest) {
                ctx.builder.CreateLifetimeStart(dest);
                if (CountTrackedPointers(dest->getAllocatedType()).count)
                    ctx.builder.CreateStore(Constant::getNullValue(dest->getAllocatedType()), dest);
            }
            ctx.builder.ClearInsertionPoint();
        }
    }

    for (PHINode *PN : ToDelete) {
        PN->replaceAllUsesWith(UndefValue::get(PN->getType()));
        PN->eraseFromParent();
    }

    // step 13. Perform any delayed instantiations
    if (ctx.debug_enabled) {
        bool in_prologue = true;
        for (auto &BB : *ctx.f) {
            for (auto &I : BB) {
                CallSite call(&I);
                if (call && !I.getDebugLoc()) {
                    // LLVM Verifier: inlinable function call in a function with debug info must have a !dbg location
                    // make sure that anything we attempt to call has some inlining info, just in case optimization messed up
                    // (except if we know that it is an intrinsic used in our prologue, which should never have its own debug subprogram)
                    Function *F = call.getCalledFunction();
                    if (!in_prologue || !F || !(F->isIntrinsic() || F->getName().startswith("julia.") || &I == restTuple)) {
                        I.setDebugLoc(topdebugloc);
                    }
                }
                if (&I == &prologue_end)
                    in_prologue = false;
            }
        }
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
                Instruction *use = NULL;
                for (Use &U : root->uses()) {
                    if (use) // erase after the iterator moves on
                        use->eraseFromParent();
                    User *RU = U.getUser();
                    use = cast<Instruction>(RU);
                }
                if (use)
                    use->eraseFromParent();
                root->eraseFromParent();
                assert(!store_value || store_value == restTuple);
                restTuple->eraseFromParent();
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

    if (JL_HOOK_TEST(ctx.params, emitted_function)) {
        JL_HOOK_CALL(ctx.params, emitted_function, 2, (jl_value_t*)ctx.linfo,
                     (jl_value_t*)ctx.source);
    }

    JL_GC_POP();
    return std::make_pair(std::unique_ptr<Module>(M), declarations);
}

// --- entry point ---

void jl_add_code_in_flight(StringRef name, jl_code_instance_t *codeinst, const DataLayout &DL);

JL_GCC_IGNORE_START("-Wclobbered")
jl_compile_result_t jl_emit_code(
        jl_method_instance_t *li,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
{
    // caller must hold codegen_lock
    jl_llvm_functions_t decls = {};
    std::unique_ptr<Module> m;
    assert((params.params == &jl_default_cgparams /* fast path */ || !params.cache ||
        compare_cgparams(params.params, &jl_default_cgparams)) &&
        "functions compiled with custom codegen params must not be cached");
    JL_TRY {
        std::tie(m, decls) = emit_function(li, src, jlrettype, params);
    }
    JL_CATCH {
        // Something failed! This is very, very bad.
        // Try to pretend that it isn't and attempt to recover.
        m.reset();
        decls.functionObject = "";
        decls.specFunctionObject = "";
        const char *mname = name_from_method_instance(li);
        jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: encountered unexpected error during compilation of %s:\n", mname);
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception());
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        jlbacktrace(); // written to STDERR_FILENO
    }

    return std::make_tuple(std::move(m), decls);
}

jl_compile_result_t jl_emit_codeinst(
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params)
{
    JL_GC_PUSH1(&src);
    if (!src) {
        src = (jl_code_info_t*)codeinst->inferred;
        jl_method_t *def = codeinst->def->def.method;
        if (src && (jl_value_t*)src != jl_nothing && jl_is_method(def))
            src = jl_uncompress_ir(def, codeinst, (jl_array_t*)src);
        if (!src || !jl_is_code_info(src)) {
            JL_GC_POP();
            return jl_compile_result_t(); // failed
        }
    }
    jl_compile_result_t result = jl_emit_code(codeinst->def, src, codeinst->rettype, params);

    const jl_llvm_functions_t &decls = std::get<1>(result);
    const std::string &specf = decls.specFunctionObject;
    const std::string &f = decls.functionObject;
    if (params.cache && !f.empty()) {
        const Module *m = std::get<0>(result).get();
        // Prepare debug info to receive this function
        // record that this function name came from this linfo,
        // so we can build a reverse mapping for debug-info.
        if (!JL_HOOK_TEST(params.params, module_activation)) {
            bool toplevel = !jl_is_method(codeinst->def->def.method);
            if (!toplevel) {
                const DataLayout &DL = m->getDataLayout();
                // but don't remember toplevel thunks because
                // they may not be rooted in the gc for the life of the program,
                // and the runtime doesn't notify us when the code becomes unreachable :(
                if (!specf.empty())
                    jl_add_code_in_flight(specf, codeinst, DL);
                if (!f.empty() && f != "jl_fptr_args" && f != "jl_fptr_sparam")
                    jl_add_code_in_flight(f, codeinst, DL);
            }
        }

        if (// don't alter `inferred` when the code is not directly being used
            params.world &&
            // don't change inferred state
            codeinst->inferred) {
            jl_method_t *def = codeinst->def->def.method;
            if (// keep code when keeping everything
                !(JL_DELETE_NON_INLINEABLE) ||
                // aggressively keep code when debugging level >= 2
                jl_options.debug_level > 1) {
                // update the stored code
                if (codeinst->inferred != (jl_value_t*)src) {
                    if (jl_is_method(def))
                        src = (jl_code_info_t*)jl_compress_ir(def, src);
                    codeinst->inferred = (jl_value_t*)src;
                    jl_gc_wb(codeinst, src);
                }
            }
            else if (// don't delete toplevel code
                     jl_is_method(def) &&
                     // and there is something to delete (test this before calling jl_ir_flag_inlineable)
                     codeinst->inferred != jl_nothing &&
                     // don't delete inlineable code, unless it is constant
                     (codeinst->invoke == jl_fptr_const_return || !jl_ir_flag_inlineable((jl_array_t*)codeinst->inferred)) &&
                     // don't delete code when generating a precompile file
                     !imaging_mode) {
                // if not inlineable, code won't be needed again
                codeinst->inferred = jl_nothing;
            }
        }
    }
    JL_GC_POP();
    return result;
}


void jl_compile_workqueue(
    std::map<jl_code_instance_t*, jl_compile_result_t> &emitted,
    jl_codegen_params_t &params)
{
    while (!params.workqueue.empty()) {
        jl_code_instance_t *codeinst;
        Function *protodecl;
        jl_returninfo_t::CallingConv proto_cc;
        bool proto_specsig;
        unsigned proto_return_roots;
        std::tie(codeinst, proto_cc, proto_return_roots, protodecl, proto_specsig) = params.workqueue.back();
        params.workqueue.pop_back();
        // try to emit code for this item from the workqueue
        assert(codeinst->min_world <= params.world && codeinst->max_world >= params.world &&
            "invalid world for code-instance");
        StringRef preal_decl = "";
        bool preal_specsig = false;
        if (params.cache && codeinst->invoke != NULL) {
            if (codeinst->invoke == jl_fptr_args) {
                preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)codeinst->specptr.fptr, codeinst);
            }
            else if (codeinst->isspecsig) {
                preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)codeinst->specptr.fptr, codeinst);
                preal_specsig = true;
            }
        }
        else {
            jl_compile_result_t &result = emitted[codeinst];
            jl_llvm_functions_t *decls = NULL;
            if (std::get<0>(result)) {
                decls = &std::get<1>(result);
            }
            else {
                result = jl_emit_codeinst(codeinst, NULL, params);
                if (std::get<0>(result))
                    decls = &std::get<1>(result);
                else
                    emitted.erase(codeinst); // undo the insert above
            }
            if (decls) {
                if (decls->functionObject == "jl_fptr_args") {
                    preal_decl = decls->specFunctionObject;
                }
                else if (decls->functionObject != "jl_fptr_sparam") {
                    preal_decl = decls->specFunctionObject;
                    preal_specsig = true;
                }
            }
        }
        // patch up the prototype we emitted earlier
        Module *mod = protodecl->getParent();
        assert(protodecl->isDeclaration());
        if (proto_specsig) {
            // expected specsig
            if (!preal_specsig) {
                // emit specsig-to-(jl)invoke conversion
                Function *preal = emit_tojlinvoke(codeinst, mod, params);
                protodecl->setLinkage(GlobalVariable::PrivateLinkage);
                //protodecl->setAlwaysInline();
                protodecl->addFnAttr("no-frame-pointer-elim", "true");
                size_t nrealargs = jl_nparams(codeinst->def->specTypes); // number of actual arguments being passed
                // TODO: maybe this can be cached in codeinst->specfptr?
                emit_cfunc_invalidate(protodecl, proto_cc, proto_return_roots, codeinst->def->specTypes, codeinst->rettype, nrealargs, params, preal);
                preal_decl = ""; // no need to fixup the name
            }
            else {
                assert(!preal_decl.empty());
            }
        }
        else {
            // expected non-specsig
            if (preal_decl.empty() || preal_specsig) {
                // emit jlcall1-to-(jl)invoke conversion
                preal_decl = emit_tojlinvoke(codeinst, mod, params)->getName();
            }
        }
        if (!preal_decl.empty()) {
            // merge and/or rename this prototype to the real function
            if (Value *specfun = mod->getNamedValue(preal_decl)) {
                if (protodecl != specfun)
                    protodecl->replaceAllUsesWith(specfun);
            }
            else {
                protodecl->setName(preal_decl);
            }
        }
    }
}


// --- initialization ---

std::pair<MDNode*,MDNode*> tbaa_make_child(const char *name, MDNode *parent=nullptr, bool isConstant=false)
{
    MDBuilder mbuilder(jl_LLVMContext);
    if (tbaa_root == nullptr) {
        MDNode *jtbaa = mbuilder.createTBAARoot("jtbaa");
        tbaa_root = mbuilder.createTBAAScalarTypeNode("jtbaa", jtbaa);
    }
    MDNode *scalar = mbuilder.createTBAAScalarTypeNode(name, parent ? parent : tbaa_root);
    MDNode *n = mbuilder.createTBAAStructTagNode(scalar, scalar, 0, isConstant);
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

// TODO: delete this
extern "C" void jl_fptr_to_llvm(void *fptr, jl_code_instance_t *lam, int specsig) { }

static void init_julia_llvm_meta(void)
{
    tbaa_gcframe = tbaa_make_child("jtbaa_gcframe").first;
    MDNode *tbaa_stack_scalar;
    std::tie(tbaa_stack, tbaa_stack_scalar) = tbaa_make_child("jtbaa_stack");
    tbaa_unionselbyte = tbaa_make_child("jtbaa_unionselbyte", tbaa_stack_scalar).first;
    MDNode *tbaa_data_scalar;
    std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child("jtbaa_data");
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
    tbaa_arrayoffset = tbaa_make_child("jtbaa_arrayoffset", tbaa_array_scalar).first;
    tbaa_const = tbaa_make_child("jtbaa_const", nullptr, true).first;
    tbaa_arrayselbyte = tbaa_make_child("jtbaa_arrayselbyte", tbaa_array_scalar).first;

    Thunk = Attribute::get(jl_LLVMContext, "thunk");
}

static Function *jlcall_func_to_llvm(const std::string &cname, jl_fptr_args_t addr, Module *m)
{
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage, cname, m);
    add_return_attr(f, Attribute::NonNull);
    f->addFnAttr(Thunk);
    add_named_global(f, addr);
    return f;
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

    // add needed base debugging definitions to our LLVM environment
    DIBuilder dbuilder(*m);
    DIFile *julia_h = dbuilder.createFile("julia.h", "");
    jl_value_dillvmt = dbuilder.createStructType(nullptr,
        "jl_value_t",
        julia_h,
        71, // At the time of this writing. Not sure if it's worth it to keep this in sync
        0 * 8, // sizeof(jl_value_t) * 8,
        __alignof__(void*) * 8, // __alignof__(jl_value_t) * 8,
        DINode::FlagZero, // Flags
        nullptr,    // Derived from
        nullptr);  // Elements - will be corrected later

    jl_pvalue_dillvmt = dbuilder.createPointerType(jl_value_dillvmt, sizeof(jl_value_t*) * 8,
                                                   __alignof__(jl_value_t*) * 8);

    SmallVector<llvm::Metadata *, 1> Elts;
    std::vector<Metadata*> diargs(0);
    Elts.push_back(jl_pvalue_dillvmt);
    dbuilder.replaceArrays(jl_value_dillvmt,
       dbuilder.getOrCreateArray(Elts));

    jl_ppvalue_dillvmt = dbuilder.createPointerType(jl_pvalue_dillvmt, sizeof(jl_value_t**) * 8,
                                                    __alignof__(jl_value_t**) * 8);

    diargs.push_back(jl_pvalue_dillvmt);    // Return Type (ret value)
    diargs.push_back(jl_pvalue_dillvmt);    // First Argument (function)
    diargs.push_back(jl_ppvalue_dillvmt);   // Second Argument (argv)
    // Third argument (length(argv))
    diargs.push_back(_julia_type_to_di(NULL, (jl_value_t*)jl_int32_type, &dbuilder, false));

    jl_di_func_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(diargs));
    jl_di_func_null_sig = dbuilder.createSubroutineType(
        dbuilder.getOrCreateTypeArray(None));

    T_jlvalue = StructType::create(jl_LLVMContext, "jl_value_t");
    T_pjlvalue = PointerType::get(T_jlvalue, 0);
    T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
    T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
    T_pprjlvalue = PointerType::get(T_prjlvalue, 0);
    V_null = Constant::getNullValue(T_pjlvalue);

    std::vector<Type*> ftargs(0);
    ftargs.push_back(T_prjlvalue);  // function
    ftargs.push_back(T_pprjlvalue); // args[]
    ftargs.push_back(T_int32);      // nargs
    jl_func_sig = FunctionType::get(T_prjlvalue, ftargs, false);
    assert(jl_func_sig != NULL);
    ftargs.push_back(T_pprjlvalue); // linfo->sparam_vals
    jl_func_sig_sparams = FunctionType::get(T_prjlvalue, ftargs, false);
    assert(jl_func_sig_sparams != NULL);

    Type *vaelts[] = {PointerType::get(T_int8, AddressSpace::Loaded)
#ifdef STORE_ARRAY_LEN
                      , T_size
#endif
                      , T_int16
                      , T_int16
                      , T_int32
    };
    static_assert(sizeof(jl_array_flags_t) == sizeof(int16_t),
                  "Size of jl_array_flags_t is not the same as int16_t");
    jl_array_llvmt =
        StructType::create(jl_LLVMContext, makeArrayRef(vaelts), "jl_array_t");
    jl_parray_llvmt = PointerType::get(jl_array_llvmt, 0);

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

    jltls_states_func = Function::Create(FunctionType::get(PointerType::get(T_ppjlvalue, 0), false),
                                         Function::ExternalLinkage, "julia.ptls_states");
    add_named_global(jltls_states_func, (void*)NULL, /*dllimport*/false);

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
    add_return_attr(jlnew_func, Attribute::NonNull);
    jlnew_func->addFnAttr(Thunk);
    add_named_global(jlnew_func, &jl_new_structv);

    std::vector<Type *> args_2rptrs_(0);
    args_2rptrs_.push_back(T_prjlvalue);
    args_2rptrs_.push_back(T_prjlvalue);
    jlsplatnew_func =
        Function::Create(FunctionType::get(T_prjlvalue, args_2rptrs_, false),
                         Function::ExternalLinkage,
                         "jl_new_structt", m);
    add_return_attr(jlsplatnew_func, Attribute::NonNull);
    jlsplatnew_func->addFnAttr(Thunk);
    add_named_global(jlsplatnew_func, &jl_new_structt);

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
    memcmp_func->addFnAttr(Attribute::ReadOnly);
    memcmp_func->addFnAttr(Attribute::NoUnwind);
    memcmp_func->addFnAttr(Attribute::ArgMemOnly);
    add_named_global(memcmp_func, &memcmp);
    // TODO: inferLibFuncAttributes(*memcmp_func, TLI);

    std::vector<Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_prjlvalue);
    te_args.push_back(PointerType::get(T_jlvalue, AddressSpace::CalleeRooted));
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error", m);
    jltypeerror_func->setDoesNotReturn();
    add_named_global(jltypeerror_func, &jl_type_error);

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
    args_2ptrs_.push_back(T_pjlvalue);
    args_2ptrs_.push_back(T_pjlvalue);
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
    builtin_func_map[jl_f_ifelse] = jlcall_func_to_llvm("jl_f_ifelse", &jl_f_ifelse, m);
    builtin_func_map[jl_f__apply] = jlcall_func_to_llvm("jl_f__apply", &jl_f__apply, m);
    builtin_func_map[jl_f__apply_iterate] = jlcall_func_to_llvm("jl_f__apply_iterate", &jl_f__apply_iterate, m);
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
    builtin_func_map[jl_f__typevar] = jlcall_func_to_llvm("jl_f__typevar", &jl_f__typevar, m);
    builtin_func_map[jl_f_arrayref] = jlcall_func_to_llvm("jl_f_arrayref", &jl_f_arrayref, m);
    builtin_func_map[jl_f_const_arrayref] = jlcall_func_to_llvm("jl_f_const_arrayref", &jl_f_arrayref, m);
    builtin_func_map[jl_f_arrayset] = jlcall_func_to_llvm("jl_f_arrayset", &jl_f_arrayset, m);
    builtin_func_map[jl_f_arraysize] = jlcall_func_to_llvm("jl_f_arraysize", &jl_f_arraysize, m);
    builtin_func_map[jl_f_apply_type] = jlcall_func_to_llvm("jl_f_apply_type", &jl_f_apply_type, m);
    jltuple_func = builtin_func_map[jl_f_tuple];
    jlgetfield_func = builtin_func_map[jl_f_getfield];

    jlapplygeneric_func = Function::Create(jl_func_sig,
                                           Function::ExternalLinkage,
                                           "jl_apply_generic", m);
    add_return_attr(jlapplygeneric_func, Attribute::NonNull);
    jlapplygeneric_func->addFnAttr(Thunk);
    add_named_global(jlapplygeneric_func, &jl_apply_generic);

    std::vector<Type*> invokeargs(0);
    invokeargs.push_back(T_prjlvalue);
    invokeargs.push_back(T_pprjlvalue);
    invokeargs.push_back(T_uint32);
    invokeargs.push_back(T_prjlvalue);
    jlinvoke_func = Function::Create(FunctionType::get(T_prjlvalue, invokeargs, false),
                                     Function::ExternalLinkage,
                                     "jl_invoke", m);
    add_return_attr(jlinvoke_func, Attribute::NonNull);
    jlinvoke_func->addAttribute(2, Attribute::ReadOnly);
    jlinvoke_func->addAttribute(2, Attribute::NoCapture);
    add_named_global(jlinvoke_func, &jl_invoke);

    std::vector<Type *> exp_args(0);
    exp_args.push_back(T_int1);
    expect_func = Intrinsic::getDeclaration(m, Intrinsic::expect, exp_args);

    std::vector<Type*> args_topeval(0);
    args_topeval.push_back(T_pjlvalue);
    args_topeval.push_back(T_pjlvalue);
    jltopeval_func =
        Function::Create(FunctionType::get(T_pjlvalue, args_topeval, false),
                         Function::ExternalLinkage,
                         "jl_toplevel_eval", m);
    add_return_attr(jltopeval_func, Attribute::NonNull);
    add_named_global(jltopeval_func, &jl_toplevel_eval);

    std::vector<Type*> args_copyast(0);
    args_copyast.push_back(T_prjlvalue);
    jlcopyast_func =
        Function::Create(FunctionType::get(T_prjlvalue, args_copyast, false),
                         Function::ExternalLinkage,
                         "jl_copy_ast", m);
    add_return_attr(jlcopyast_func, Attribute::NonNull);
    add_named_global(jlcopyast_func, &jl_copy_ast);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlnsvec_func =
        Function::Create(FunctionType::get(T_pjlvalue, args5, true),
                         Function::ExternalLinkage,
                         "jl_svec", m);
    add_return_attr(jlnsvec_func, Attribute::NonNull);
    add_named_global(jlnsvec_func, &jl_svec);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(T_prjlvalue);
    mdargs.push_back(T_prjlvalue);
    mdargs.push_back(T_pjlvalue);
    jlmethod_func =
        Function::Create(FunctionType::get(T_void, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", m);
    add_named_global(jlmethod_func, &jl_method_def);

    std::vector<Type*> funcdefargs(0);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_pjlvalue);
    funcdefargs.push_back(T_pprjlvalue);
    funcdefargs.push_back(T_pjlvalue);
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

    jl_current_exception_func =
        Function::Create(FunctionType::get(T_prjlvalue, false),
                         Function::ExternalLinkage,
                         "jl_current_exception", m);
    add_named_global(jl_current_exception_func, &jl_current_exception);

#ifdef _OS_WINDOWS_
#ifndef FORCE_ELF
#if defined(_CPU_X86_64_)
#if defined(_COMPILER_GCC_)
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "___chkstk_ms", m);
    add_named_global(chkstk_func, &___chkstk_ms, /*dllimport*/false);
#else
    Function *chkstk_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "__chkstk", m);
    add_named_global(chkstk_func, &__chkstk, /*dllimport*/false);
#endif
#else
#if defined(_COMPILER_GCC_)
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

    jl_restore_excstack_func =
        Function::Create(FunctionType::get(T_void, T_size, false),
                         Function::ExternalLinkage,
                         "jl_restore_excstack", m);
    add_named_global(jl_restore_excstack_func, &jl_restore_excstack);

    jl_excstack_state_func =
        Function::Create(FunctionType::get(T_size, false),
                         Function::ExternalLinkage,
                         "jl_excstack_state", m);
    add_named_global(jl_excstack_state_func, &jl_excstack_state);

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
    applytype_args.push_back(T_pjlvalue);
    applytype_args.push_back(T_pjlvalue);
    applytype_args.push_back(T_pprjlvalue);
    jlapplytype_func =
        Function::Create(FunctionType::get(T_prjlvalue, applytype_args, false),
                         Function::ExternalLinkage,
                         "jl_instantiate_type_in_env", m);
    add_return_attr(jlapplytype_func, Attribute::NonNull);
    add_named_global(jlapplytype_func, &jl_instantiate_type_in_env);

    std::vector<Type *> objectid__args(0);
    objectid__args.push_back(T_prjlvalue);
    objectid__args.push_back(T_pint8_derived);
    jl_object_id__func =
        Function::Create(FunctionType::get(T_size, objectid__args, false),
                         Function::ExternalLinkage,
                         "jl_object_id_", m);
    add_named_global(jl_object_id__func, &jl_object_id_);

    std::vector<Type*> gc_alloc_args(0);
    gc_alloc_args.push_back(T_pint8);
    gc_alloc_args.push_back(T_size);
    gc_alloc_args.push_back(T_prjlvalue);
    jl_alloc_obj_func = Function::Create(FunctionType::get(T_prjlvalue, gc_alloc_args, false),
                                         Function::ExternalLinkage,
                                         "julia.gc_alloc_obj");
    add_return_attr(jl_alloc_obj_func, Attribute::NoAlias);
    add_return_attr(jl_alloc_obj_func, Attribute::NonNull);
    jl_alloc_obj_func->addFnAttr(Attribute::getWithAllocSizeArgs(jl_LLVMContext, 1, None)); // returns %1 bytes
    add_named_global(jl_alloc_obj_func, (void*)NULL, /*dllimport*/false);

    std::vector<Type*> newbits_args(0);
    newbits_args.push_back(T_prjlvalue);
    newbits_args.push_back(T_pint8);
    jl_newbits_func = Function::Create(FunctionType::get(T_prjlvalue, newbits_args, false),
                                         Function::ExternalLinkage,
                                         "jl_new_bits");
    add_return_attr(jl_newbits_func, Attribute::NoAlias);
    add_return_attr(jl_newbits_func, Attribute::NonNull);
    add_named_global(jl_newbits_func, (void*)jl_new_bits);

    jl_loopinfo_marker_func = Function::Create(FunctionType::get(T_void, {}, false),
                                               Function::ExternalLinkage,
                                               "julia.loopinfo_marker");
    jl_loopinfo_marker_func->addFnAttr(Attribute::NoUnwind);
    jl_loopinfo_marker_func->addFnAttr(Attribute::NoRecurse);
    jl_loopinfo_marker_func->addFnAttr(Attribute::InaccessibleMemOnly);

    jl_typeof_func = Function::Create(FunctionType::get(T_prjlvalue, {T_prjlvalue}, false),
                                      Function::ExternalLinkage,
                                      "julia.typeof");
    jl_typeof_func->addFnAttr(Attribute::ReadOnly);
    jl_typeof_func->addFnAttr(Attribute::NoUnwind);
    jl_typeof_func->addFnAttr(Attribute::ArgMemOnly);
    jl_typeof_func->addFnAttr(Attribute::NoRecurse);
    add_return_attr(jl_typeof_func, Attribute::NonNull);
    add_named_global(jl_typeof_func, (void*)NULL, /*dllimport*/false);

    jl_write_barrier_func = Function::Create(FunctionType::get(T_void, {T_prjlvalue,}, true),
                                             Function::ExternalLinkage,
                                             "julia.write_barrier");
    jl_write_barrier_func->addFnAttr(Attribute::InaccessibleMemOnly);
    jl_write_barrier_func->addFnAttr(Attribute::NoUnwind);
    jl_write_barrier_func->addFnAttr(Attribute::NoRecurse);
    add_named_global(jl_write_barrier_func, (void*)NULL, /*dllimport*/false);

    std::vector<Type *> dlsym_args(0);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(T_pint8);
    dlsym_args.push_back(PointerType::get(T_pint8,0));
    jldlsym_func =
        Function::Create(FunctionType::get(T_pvoidfunc, dlsym_args, false),
                         Function::ExternalLinkage,
                         "jl_load_and_lookup", m);
    add_named_global(jldlsym_func, &jl_load_and_lookup);

    std::vector<Type *> getcfunctiontrampoline_args(0);
    getcfunctiontrampoline_args.push_back(T_prjlvalue); // f (object)
    getcfunctiontrampoline_args.push_back(T_pjlvalue); // result
    getcfunctiontrampoline_args.push_back(T_pint8); // cache
    getcfunctiontrampoline_args.push_back(T_pjlvalue); // fill
    getcfunctiontrampoline_args.push_back(FunctionType::get(T_pint8, { T_pint8, T_ppjlvalue }, false)->getPointerTo()); // trampoline
    getcfunctiontrampoline_args.push_back(T_pjlvalue); // env
    getcfunctiontrampoline_args.push_back(T_pprjlvalue); // vals
    jlgetcfunctiontrampoline_func =
        Function::Create(FunctionType::get(T_prjlvalue, getcfunctiontrampoline_args, false),
                         Function::ExternalLinkage,
                         "jl_get_cfunction_trampoline", m);
    add_return_attr(jlgetcfunctiontrampoline_func, Attribute::NonNull);
    add_named_global(jlgetcfunctiontrampoline_func, &jl_get_cfunction_trampoline);

    std::vector<Type *> getnthfld_args(0);
    getnthfld_args.push_back(T_prjlvalue);
    getnthfld_args.push_back(T_size);
    jlgetnthfieldchecked_func =
        Function::Create(FunctionType::get(T_prjlvalue, getnthfld_args, false),
                         Function::ExternalLinkage,
                         "jl_get_nth_field_checked", m);
    add_return_attr(jlgetnthfieldchecked_func, Attribute::NonNull);
    add_named_global(jlgetnthfieldchecked_func, &jl_get_nth_field_checked);

    diff_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, false),
                         Function::ExternalLinkage,
                         "jl_gc_diff_total_bytes", m);
    add_named_global(diff_gc_total_bytes_func, &jl_gc_diff_total_bytes);

    sync_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, {T_int64}, false),
                         Function::ExternalLinkage,
                         "jl_gc_sync_total_bytes", m);
    add_named_global(sync_gc_total_bytes_func, &jl_gc_sync_total_bytes);


    std::vector<Type*> array_owner_args(0);
    array_owner_args.push_back(T_prjlvalue);
    jlarray_data_owner_func =
        Function::Create(FunctionType::get(T_prjlvalue, array_owner_args, false),
                         Function::ExternalLinkage,
                         "jl_array_data_owner", m);
    jlarray_data_owner_func->addFnAttr(Attribute::ReadOnly);
    jlarray_data_owner_func->addFnAttr(Attribute::NoUnwind);
    add_return_attr(jlarray_data_owner_func, Attribute::NonNull);
    add_named_global(jlarray_data_owner_func, &jl_array_data_owner);

    gcroot_flush_func = Function::Create(FunctionType::get(T_void, false),
                                         Function::ExternalLinkage,
                                         "julia.gcroot_flush");
    add_named_global(gcroot_flush_func, (void*)NULL, /*dllimport*/false);

    gc_preserve_begin_func = Function::Create(FunctionType::get(Type::getTokenTy(jl_LLVMContext),
                                         ArrayRef<Type*>(), true),
                                         Function::ExternalLinkage,
                                         "llvm.julia.gc_preserve_begin");
    add_named_global(gc_preserve_begin_func, (void*)NULL, /*dllimport*/false);

    gc_preserve_end_func = Function::Create(FunctionType::get(T_void,
                                        ArrayRef<Type*>(Type::getTokenTy(jl_LLVMContext)), false),
                                        Function::ExternalLinkage,
                                        "llvm.julia.gc_preserve_end");
    add_named_global(gc_preserve_end_func, (void*)NULL, /*dllimport*/false);

    pointer_from_objref_func = Function::Create(FunctionType::get(T_pjlvalue,
                                         ArrayRef<Type*>(PointerType::get(T_jlvalue, AddressSpace::Derived)), false),
                                         Function::ExternalLinkage,
                                         "julia.pointer_from_objref");
    pointer_from_objref_func->addFnAttr(Attribute::ReadNone);
    pointer_from_objref_func->addFnAttr(Attribute::NoUnwind);
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
}

extern "C" void jl_init_llvm(void)
{
    const char *const argv_tailmerge[] = {"", "-enable-tail-merge=0"}; // NOO TOUCHIE; NO TOUCH! See #922
    cl::ParseCommandLineOptions(sizeof(argv_tailmerge)/sizeof(argv_tailmerge[0]), argv_tailmerge, "disable-tail-merge\n");
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    const char *const argv_copyprop[] = {"", "-disable-copyprop"}; // llvm bug 21743
    cl::ParseCommandLineOptions(sizeof(argv_copyprop)/sizeof(argv_copyprop[0]), argv_copyprop, "disable-copyprop\n");
#endif
#if defined(_CPU_X86_) || defined(_CPU_X86_64_)
    const char *const argv_avoidsfb[] = {"", "-x86-disable-avoid-SFB"}; // llvm bug 41629, see https://gist.github.com/vtjnash/192cab72a6cfc00256ff118238163b55
    cl::ParseCommandLineOptions(sizeof(argv_avoidsfb)/sizeof(argv_avoidsfb[0]), argv_avoidsfb, "disable-avoidsfb\n");
#endif
    cl::ParseEnvironmentOptions("Julia", "JULIA_LLVM_ARGS");

    // if the patch adding this option has been applied, lower its limit to provide
    // better DAGCombiner performance.
    auto &clOptions = cl::getRegisteredOptions();
    if (clOptions.find("combiner-store-merge-dependence-limit") != clOptions.end()) {
        const char *const argv_smdl[] = {"", "-combiner-store-merge-dependence-limit=4"};
        cl::ParseCommandLineOptions(sizeof(argv_smdl)/sizeof(argv_smdl[0]), argv_smdl);
    }

    jl_page_size = jl_getpagesize();
    imaging_mode = jl_generating_output() && !jl_options.incremental;
    jl_default_cgparams.module_setup = jl_nothing;
    jl_default_cgparams.module_activation = jl_nothing;
    jl_default_cgparams.raise_exception = jl_nothing;
    jl_default_cgparams.emit_function = jl_nothing;
    jl_default_cgparams.emitted_function = jl_nothing;
    jl_init_debuginfo();

#ifdef USE_POLLY
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    polly::initializePollyPasses(Registry);
    initializeAnalysis(Registry);
#endif

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetDisassembler();

    TargetOptions options = TargetOptions();
    //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to assume the stack is always 16-byte aligned,
    // and to ensure that it is 16-byte aligned for out-going calls,
    // to ensure compatibility with GCC codes
    options.StackAlignmentOverride = 16;
#endif
    Triple TheTriple(sys::getProcessTriple());
#if defined(FORCE_ELF)
    TheTriple.setObjectFormat(Triple::ELF);
#endif
    uint32_t target_flags = 0;
    auto target = jl_get_llvm_target(imaging_mode, target_flags);
    auto &TheCPU = target.first;
    SmallVector<std::string, 10> targetFeatures(target.second.begin(), target.second.end());
    std::string errorstr;
    const Target *TheTarget = TargetRegistry::lookupTarget("", TheTriple, errorstr);
    if (!TheTarget)
        jl_errorf("%s", errorstr.c_str());
    if (jl_processor_print_help || (target_flags & JL_TARGET_UNKNOWN_NAME)) {
        std::unique_ptr<MCSubtargetInfo> MSTI(
            TheTarget->createMCSubtargetInfo(TheTriple.str(), "", ""));
        if (!MSTI->isCPUStringValid(TheCPU))
            jl_errorf("Invalid CPU name %s.", TheCPU.c_str());
        if (jl_processor_print_help) {
            // This is the only way I can find to print the help message once.
            // It'll be nice if we can iterate through the features and print our own help
            // message...
            MSTI->setDefaultFeatures("help", "");
        }
    }
    // Package up features to be passed to target/subtarget
    std::string FeaturesStr;
    if (!targetFeatures.empty()) {
        SubtargetFeatures Features;
        for (unsigned i = 0; i != targetFeatures.size(); ++i)
            Features.AddFeature(targetFeatures[i]);
        FeaturesStr = Features.getString();
    }
    // Allocate a target...
    Optional<CodeModel::Model> codemodel =
#ifdef _P64
        // Make sure we are using the large code model on 64bit
        // Let LLVM pick a default suitable for jitting on 32bit
        CodeModel::Large;
#elif JL_LLVM_VERSION < 60000
        CodeModel::JITDefault;
#else
        None;
#endif
    auto optlevel =
#ifdef DISABLE_OPT
        CodeGenOpt::None;
#else
        (jl_options.opt_level < 2 ? CodeGenOpt::None :
         jl_options.opt_level == 2 ? CodeGenOpt::Default :
         CodeGenOpt::Aggressive);
#endif
    jl_TargetMachine = TheTarget->createTargetMachine(
            TheTriple.getTriple(), TheCPU, FeaturesStr,
            options,
            Reloc::Static, // Generate simpler code for JIT
            codemodel,
            optlevel
#if JL_LLVM_VERSION >= 60000
            , /*JIT*/ true
#endif
            );
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
    jl_data_layout = jl_ExecutionEngine->getDataLayout();
    std::string DL = jl_data_layout.getStringRepresentation() + "-ni:10:11:12:13";
    jl_data_layout.reset(DL);

// Register GDB event listener
#ifdef JL_DEBUG_BUILD
    jl_ExecutionEngine->RegisterJITEventListener(JITEventListener::createGDBRegistrationListener());
#endif

#ifdef JL_USE_INTEL_JITEVENTS
    if (jl_using_intel_jitevents)
        jl_ExecutionEngine->RegisterJITEventListener(JITEventListener::createIntelJITEventListener());
#endif

#ifdef JL_USE_OPROFILE_JITEVENTS
    if (jl_using_oprofile_jitevents)
        jl_ExecutionEngine->RegisterJITEventListener(JITEventListener::createOProfileJITEventListener());
#endif

#ifdef JL_USE_PERF_JITEVENTS
    if (jl_using_perf_jitevents)
        jl_ExecutionEngine->RegisterJITEventListener(JITEventListener::createPerfJITEventListener());
#endif
}

extern "C" void jl_init_codegen(void)
{
    jl_init_llvm();
    // Now that the execution engine exists, initialize all modules
    jl_init_jit();
    Module *m = new Module("julia", jl_LLVMContext);
    shadow_output = m;
    jl_setup_module(m);
    init_julia_llvm_env(m);

    SBOX_F_PERM(int8,int8); UBOX_F_PERM(uint8,uint8);
    SBOX_F(int16,int16); UBOX_F(uint16,uint16);
    SBOX_F(int32,int32); UBOX_F(uint32,uint32);
    SBOX_F(int64,int64); UBOX_F(uint64,uint64);
    BOX_F(float32,float32,T_prjlvalue); BOX_F(float64,float64,T_prjlvalue);
    UBOX_F(char,char);
    UBOX_F(ssavalue,size);

    jl_init_intrinsic_functions_codegen(m);
}

// the rest of this file are convenience functions
// that are exported for assisting with debugging from gdb
extern "C" void jl_dump_llvm_value(void *v)
{
    llvm_dump((Value*)v);
}

extern "C" void jl_dump_llvm_inst_function(void *v)
{
    llvm_dump(cast<Instruction>(((Value*)v))->getParent()->getParent());
}

extern "C" void jl_dump_llvm_type(void *v)
{
    llvm_dump((Type*)v);
}

extern "C" void jl_dump_llvm_module(void *v)
{
    llvm_dump((Module*)v);
}

extern "C" void jl_dump_llvm_metadata(void *v)
{
    llvm_dump((Metadata*)v);
}

extern "C" void jl_dump_llvm_debugloc(void *v)
{
    llvm_dump((DebugLoc*)v);
}

extern void jl_write_bitcode_func(void *F, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::F_None);
    llvm::WriteBitcodeToFile(*((llvm::Function*)F)->getParent(), OS);
}

extern void jl_write_bitcode_module(void *M, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::F_None);
    llvm::WriteBitcodeToFile(*(llvm::Module*)M, OS);
}
