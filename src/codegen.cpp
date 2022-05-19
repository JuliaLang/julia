// This file is a part of Julia. License is MIT: https://julialang.org/license

#undef DEBUG
#include "llvm-version.h"
#include "platform.h"
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
#include <fstream>
#include <map>
#include <array>
#include <vector>
#include <set>
#include <functional>

// target machine computation
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#if JL_LLVM_VERSION >= 140000
#include <llvm/MC/TargetRegistry.h>
#else
#include <llvm/Support/TargetRegistry.h>
#endif
#include <llvm/Target/TargetOptions.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Object/SymbolSize.h>

#include <llvm/InitializePasses.h>

// IR building
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/Object/ObjectFile.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/AsmParser/Parser.h>
#include <llvm/DebugInfo/DIContext.h>
#include "llvm/IR/DebugInfoMetadata.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>

// support
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/Statistic.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/SourceMgr.h> // for llvmcall
#include <llvm/Transforms/Utils/Cloning.h> // for llvmcall inlining
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/IR/Verifier.h> // for llvmcall validation
#include <llvm/IR/PassTimingInfo.h>
#include <llvm/Bitcode/BitcodeWriter.h>

// C API
#include <llvm-c/Types.h>

// for configuration options
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Process.h>

#include <llvm/IR/InlineAsm.h>
#if defined(_CPU_ARM_) || defined(_CPU_AARCH64_)
#  include <sys/utsname.h>
#endif
#if defined(USE_POLLY)
#include <polly/RegisterPasses.h>
#include <polly/ScopDetection.h>
#endif
#include <llvm/Target/TargetMachine.h>

#include "llvm/Support/Path.h" // for llvm::sys::path
#include <llvm/Bitcode/BitcodeReader.h>
#include <llvm/Linker/Linker.h>

using namespace llvm;

//Drag some useful type functions into our namespace
//to reduce verbosity of our code
auto getInt1Ty(LLVMContext &ctxt) {
    return Type::getInt1Ty(ctxt);
}
auto getInt8Ty(LLVMContext &ctxt) {
    return Type::getInt8Ty(ctxt);
}
auto getInt16Ty(LLVMContext &ctxt) {
    return Type::getInt16Ty(ctxt);
}
auto getInt32Ty(LLVMContext &ctxt) {
    return Type::getInt32Ty(ctxt);
}
auto getInt64Ty(LLVMContext &ctxt) {
    return Type::getInt64Ty(ctxt);
}
auto getHalfTy(LLVMContext &ctxt) {
    return Type::getHalfTy(ctxt);
}
auto getFloatTy(LLVMContext &ctxt) {
    return Type::getFloatTy(ctxt);
}
auto getDoubleTy(LLVMContext &ctxt) {
    return Type::getDoubleTy(ctxt);
}
auto getFP128Ty(LLVMContext &ctxt) {
    return Type::getFP128Ty(ctxt);
}
auto getVoidTy(LLVMContext &ctxt) {
    return Type::getVoidTy(ctxt);
}
auto getCharTy(LLVMContext &ctxt) {
    return getInt32Ty(ctxt);
}
auto getInt8PtrTy(LLVMContext &ctxt) {
    return Type::getInt8PtrTy(ctxt);
}
auto getInt16PtrTy(LLVMContext &ctxt) {
    return Type::getInt16PtrTy(ctxt);
}
auto getInt32PtrTy(LLVMContext &ctxt) {
    return Type::getInt32PtrTy(ctxt);
}
auto getInt64PtrTy(LLVMContext &ctxt) {
    return Type::getInt64PtrTy(ctxt);
}
auto getFloatPtrTy(LLVMContext &ctxt) {
    return Type::getFloatPtrTy(ctxt);
}
auto getDoublePtrTy(LLVMContext &ctxt) {
    return Type::getDoublePtrTy(ctxt);
}
auto getSizePtrTy(LLVMContext &ctxt) {
    if (sizeof(size_t) > sizeof(uint32_t)) {
        return getInt64PtrTy(ctxt);
    } else {
        return getInt32PtrTy(ctxt);
    }
}

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

#undef DEBUG_TYPE //LLVM occasionally likes to set DEBUG_TYPE in a header...
#define DEBUG_TYPE "julia_irgen_codegen"

STATISTIC(EmittedAllocas, "Number of allocas emitted");
STATISTIC(EmittedIntToPtrs, "Number of inttoptrs emitted");
STATISTIC(ModulesCreated, "Number of LLVM Modules created");
STATISTIC(EmittedBoxCompares, "Number of box compares emitted");
STATISTIC(EmittedBitsUnionCompares, "Number of bitsunion compares emitted");
STATISTIC(EmittedBitsCompares, "Number of bits compares emitted");
STATISTIC(EmittedEgals, "Number of egals emitted");
STATISTIC(EmittedOpfields, "Number of opfields emitted");
STATISTIC(EmittedBuiltinCalls, "Number of builtin calls emitted");
STATISTIC(EmittedJLCalls, "Number of jlcalls emitted");
STATISTIC(EmittedSpecfunCalls, "Number of specialized calls emitted");
STATISTIC(EmittedInvokes, "Number of invokes emitted");
STATISTIC(EmittedCalls, "Number of calls emitted");
STATISTIC(EmittedUndefVarErrors, "Number of undef var errors emitted");
STATISTIC(EmittedOpaqueClosureFunctions, "Number of opaque closures emitted");
STATISTIC(EmittedToJLInvokes, "Number of tojlinvoke calls emitted");
STATISTIC(EmittedCFuncInvalidates, "Number of C function invalidates emitted");
STATISTIC(GeneratedCFuncWrappers, "Number of C function wrappers generated");
STATISTIC(GeneratedCCallables, "Number of C-callable functions generated");
STATISTIC(GeneratedInvokeWrappers, "Number of invoke wrappers generated");
STATISTIC(EmittedFunctions, "Number of functions emitted");

extern "C" JL_DLLEXPORT
void jl_dump_emitted_mi_name_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_emitted_mi_name_stream() = (JL_STREAM*)s;
}

extern "C" {

#include "builtin_proto.h"

extern void __stack_chk_fail();

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

// shared llvm state
#define jl_Module ctx.f->getParent()
#define jl_builderModule(builder) (builder).GetInsertBlock()->getParent()->getParent()
#define prepare_call(Callee) prepare_call_in(jl_Module, (Callee))

// types
struct jl_typecache_t {
    Type *T_jlvalue;
    Type *T_pjlvalue;
    Type *T_prjlvalue;
    Type *T_ppjlvalue;
    Type *T_pprjlvalue;
    StructType *T_jlarray;
    Type *T_pjlarray;
    FunctionType *T_jlfunc;
    FunctionType *T_jlfuncparams;

    IntegerType *T_sigatomic;

    Type *T_ppint8;

    bool initialized;

    jl_typecache_t() :
        T_jlvalue(nullptr), T_pjlvalue(nullptr), T_prjlvalue(nullptr),
        T_ppjlvalue(nullptr), T_pprjlvalue(nullptr), T_jlarray(nullptr),
        T_pjlarray(nullptr), T_jlfunc(nullptr), T_jlfuncparams(nullptr),
        T_sigatomic(nullptr), T_ppint8(nullptr), initialized(false) {}

    void initialize(LLVMContext &context) {
        if (initialized) {
            return;
        }
        initialized = true;
        T_ppint8 = PointerType::get(getInt8PtrTy(context), 0);
        T_sigatomic = Type::getIntNTy(context, sizeof(sig_atomic_t) * 8);
        T_jlvalue = JuliaType::get_jlvalue_ty(context);
        T_pjlvalue = PointerType::get(T_jlvalue, 0);
        T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
        T_pprjlvalue = PointerType::get(T_prjlvalue, 0);

        T_jlfunc = JuliaType::get_jlfunc_ty(context);
        assert(T_jlfunc != NULL);
        T_jlfuncparams = JuliaType::get_jlfuncparams_ty(context);
        assert(T_jlfuncparams != NULL);

        Type *vaelts[] = {PointerType::get(getInt8Ty(context), AddressSpace::Loaded)
                        , getSizeTy(context)
                        , getInt16Ty(context)
                        , getInt16Ty(context)
                        , getInt32Ty(context)
        };
        static_assert(sizeof(jl_array_flags_t) == sizeof(int16_t),
                    "Size of jl_array_flags_t is not the same as int16_t");
        T_jlarray = StructType::get(context, makeArrayRef(vaelts));
        T_pjlarray = PointerType::get(T_jlarray, 0);
    }
};

struct jl_tbaacache_t {
    // type-based alias analysis nodes.  Indentation of comments indicates hierarchy.
    MDNode *tbaa_root;     // Everything
    MDNode *tbaa_gcframe;    // GC frame
    // LLVM should have enough info for alias analysis of non-gcframe stack slot
    // this is mainly a place holder for `jl_cgval_t::tbaa`
    MDNode *tbaa_stack;      // stack slot
    MDNode *tbaa_unionselbyte;   // a selector byte in isbits Union struct fields
    MDNode *tbaa_data;       // Any user data that `pointerset/ref` are allowed to alias
    MDNode *tbaa_binding;        // jl_binding_t::value
    MDNode *tbaa_value;          // jl_value_t, that is not jl_array_t
    MDNode *tbaa_mutab;              // mutable type
    MDNode *tbaa_datatype;               // datatype
    MDNode *tbaa_immut;              // immutable type
    MDNode *tbaa_ptrarraybuf;    // Data in an array of boxed values
    MDNode *tbaa_arraybuf;       // Data in an array of POD
    MDNode *tbaa_array;      // jl_array_t
    MDNode *tbaa_arrayptr;       // The pointer inside a jl_array_t
    MDNode *tbaa_arraysize;      // A size in a jl_array_t
    MDNode *tbaa_arraylen;       // The len in a jl_array_t
    MDNode *tbaa_arrayflags;     // The flags in a jl_array_t
    MDNode *tbaa_arrayoffset;     // The offset in a jl_array_t
    MDNode *tbaa_arrayselbyte;   // a selector byte in a isbits Union jl_array_t
    MDNode *tbaa_const;      // Memory that is immutable by the time LLVM can see it
    bool initialized;

    jl_tbaacache_t(): tbaa_root(nullptr), tbaa_gcframe(nullptr), tbaa_stack(nullptr),
                    tbaa_unionselbyte(nullptr), tbaa_data(nullptr), tbaa_binding(nullptr),
                    tbaa_value(nullptr), tbaa_mutab(nullptr), tbaa_datatype(nullptr),
                    tbaa_immut(nullptr), tbaa_ptrarraybuf(nullptr), tbaa_arraybuf(nullptr),
                    tbaa_array(nullptr), tbaa_arrayptr(nullptr), tbaa_arraysize(nullptr),
                    tbaa_arraylen(nullptr), tbaa_arrayflags(nullptr), tbaa_arrayoffset(nullptr),
                    tbaa_arrayselbyte(nullptr), tbaa_const(nullptr), initialized(false) {}

    auto tbaa_make_child(MDBuilder &mbuilder, const char *name, MDNode *parent = nullptr, bool isConstant = false) {
        MDNode *scalar = mbuilder.createTBAAScalarTypeNode(name, parent ? parent : tbaa_root);
        MDNode *n = mbuilder.createTBAAStructTagNode(scalar, scalar, 0, isConstant);
        return std::make_pair(n, scalar);
    }

    void initialize(llvm::LLVMContext &context) {
        if (initialized) {
            assert(&tbaa_root->getContext() == &context);
            return;
        }
        initialized = true;
        MDBuilder mbuilder(context);
        MDNode *jtbaa = mbuilder.createTBAARoot("jtbaa");
        tbaa_root = mbuilder.createTBAAScalarTypeNode("jtbaa", jtbaa);
        tbaa_gcframe = tbaa_make_child(mbuilder, "jtbaa_gcframe").first;
        MDNode *tbaa_stack_scalar;
        std::tie(tbaa_stack, tbaa_stack_scalar) = tbaa_make_child(mbuilder, "jtbaa_stack");
        tbaa_unionselbyte = tbaa_make_child(mbuilder, "jtbaa_unionselbyte", tbaa_stack_scalar).first;
        MDNode *tbaa_data_scalar;
        std::tie(tbaa_data, tbaa_data_scalar) = tbaa_make_child(mbuilder, "jtbaa_data");
        tbaa_binding = tbaa_make_child(mbuilder, "jtbaa_binding", tbaa_data_scalar).first;
        MDNode *tbaa_value_scalar;
        std::tie(tbaa_value, tbaa_value_scalar) =
            tbaa_make_child(mbuilder, "jtbaa_value", tbaa_data_scalar);
        MDNode *tbaa_mutab_scalar;
        std::tie(tbaa_mutab, tbaa_mutab_scalar) =
            tbaa_make_child(mbuilder, "jtbaa_mutab", tbaa_value_scalar);
        tbaa_datatype = tbaa_make_child(mbuilder, "jtbaa_datatype", tbaa_mutab_scalar).first;
        tbaa_immut = tbaa_make_child(mbuilder, "jtbaa_immut", tbaa_value_scalar).first;
        tbaa_arraybuf = tbaa_make_child(mbuilder, "jtbaa_arraybuf", tbaa_data_scalar).first;
        tbaa_ptrarraybuf = tbaa_make_child(mbuilder, "jtbaa_ptrarraybuf", tbaa_data_scalar).first;
        MDNode *tbaa_array_scalar;
        std::tie(tbaa_array, tbaa_array_scalar) = tbaa_make_child(mbuilder, "jtbaa_array");
        tbaa_arrayptr = tbaa_make_child(mbuilder, "jtbaa_arrayptr", tbaa_array_scalar).first;
        tbaa_arraysize = tbaa_make_child(mbuilder, "jtbaa_arraysize", tbaa_array_scalar).first;
        tbaa_arraylen = tbaa_make_child(mbuilder, "jtbaa_arraylen", tbaa_array_scalar).first;
        tbaa_arrayflags = tbaa_make_child(mbuilder, "jtbaa_arrayflags", tbaa_array_scalar).first;
        tbaa_arrayoffset = tbaa_make_child(mbuilder, "jtbaa_arrayoffset", tbaa_array_scalar).first;
        tbaa_const = tbaa_make_child(mbuilder, "jtbaa_const", nullptr, true).first;
        tbaa_arrayselbyte = tbaa_make_child(mbuilder, "jtbaa_arrayselbyte", tbaa_array_scalar).first;
    }
};

struct jl_debugcache_t {
    // Basic DITypes
    DIDerivedType *jl_pvalue_dillvmt;
    DIDerivedType *jl_ppvalue_dillvmt;
    DISubroutineType *jl_di_func_sig;
    DISubroutineType *jl_di_func_null_sig;
    bool initialized;

    jl_debugcache_t()
    : jl_pvalue_dillvmt(nullptr), jl_ppvalue_dillvmt(nullptr),
    jl_di_func_sig(nullptr), jl_di_func_null_sig(nullptr), initialized(false) {}

    void initialize(Module *m);
};


// constants
static bool type_is_ghost(Type *ty)
{
    return (ty == getVoidTy(ty->getContext()) || ty->isEmptyTy());
}

// should agree with `Core.Compiler.hasuniquerep`
static bool type_has_unique_rep(jl_value_t *t)
{
    if (t == (jl_value_t*)jl_typeofbottom_type)
        return false;
    if (t == jl_bottom_type)
        return true;
    if (jl_is_typevar(t))
        return false;
    if (!jl_is_kind(jl_typeof(t)))
        return true;
    if (jl_is_concrete_type(t))
        return true;
    if (jl_is_datatype(t)) {
        jl_datatype_t *dt = (jl_datatype_t*)t;
        if (dt->name != jl_tuple_typename) {
            for (size_t i = 0; i < jl_nparams(dt); i++)
                if (!type_has_unique_rep(jl_tparam(dt, i)))
                    return false;
            return true;
        }
    }
    return false;
}

static bool is_uniquerep_Type(jl_value_t *t)
{
    return jl_is_type_type(t) && type_has_unique_rep(jl_tparam0(t));
}

class jl_codectx_t;
struct JuliaVariable {
public:
    StringLiteral name;
    bool isconst;
    Type *(*_type)(LLVMContext &C);

    JuliaVariable(const JuliaVariable&) = delete;
    JuliaVariable(const JuliaVariable&&) = delete;
    GlobalVariable *realize(Module *m) {
        if (GlobalValue *V = m->getNamedValue(name))
            return cast<GlobalVariable>(V);
        return new GlobalVariable(*m, _type(m->getContext()),
                isconst, GlobalVariable::ExternalLinkage,
                NULL, name);
    }
    GlobalVariable *realize(jl_codectx_t &ctx);
};
static inline void add_named_global(JuliaVariable *name, void *addr)
{
    add_named_global(name->name, addr);
}

struct JuliaFunction {
public:
    llvm::StringLiteral name;
    llvm::FunctionType *(*_type)(llvm::LLVMContext &C);
    llvm::AttributeList (*_attrs)(llvm::LLVMContext &C);

    JuliaFunction(const JuliaFunction&) = delete;
    JuliaFunction(const JuliaFunction&&) = delete;
    llvm::Function *realize(llvm::Module *m) {
        if (llvm::GlobalValue *V = m->getNamedValue(name))
            return llvm::cast<llvm::Function>(V);
        llvm::Function *F = llvm::Function::Create(_type(m->getContext()),
                         llvm::Function::ExternalLinkage,
                         name, m);
        if (_attrs)
            F->setAttributes(_attrs(m->getContext()));
        return F;
    }
};

template<typename T>
static inline void add_named_global(JuliaFunction *name, T *addr)
{
    // cast through integer to avoid c++ pedantic warning about casting between
    // data and code pointers
    add_named_global(name->name, (void*)(uintptr_t)addr);
}
template<typename T>
static inline void add_named_global(StringRef name, T *addr)
{
    // cast through integer to avoid c++ pedantic warning about casting between
    // data and code pointers
    add_named_global(name, (void*)(uintptr_t)addr);
}

AttributeSet Attributes(LLVMContext &C, std::initializer_list<Attribute::AttrKind> attrkinds)
{
    SmallVector<Attribute, 8> attrs(attrkinds.size());
    for (size_t i = 0; i < attrkinds.size(); i++)
        attrs[i] = Attribute::get(C, attrkinds.begin()[i]);
    return AttributeSet::get(C, makeArrayRef(attrs));
}

static Type *get_pjlvalue(LLVMContext &C) { return JuliaType::get_pjlvalue_ty(C); }

static FunctionType *get_func_sig(LLVMContext &C) { return JuliaType::get_jlfunc_ty(C); }

static FunctionType *get_donotdelete_sig(LLVMContext &C) {
    return FunctionType::get(getVoidTy(C), true);
}

static AttributeList get_func_attrs(LLVMContext &C)
{
    return AttributeList::get(C,
            AttributeSet::get(C, makeArrayRef({Attribute::get(C, "thunk")})),
            Attributes(C, {Attribute::NonNull}),
            None);
}

static AttributeList get_donotdelete_func_attrs(LLVMContext &C)
{
    AttributeSet FnAttrs = AttributeSet::get(C, makeArrayRef({Attribute::get(C, "thunk")}));
    FnAttrs = FnAttrs.addAttribute(C, Attribute::InaccessibleMemOnly);
    FnAttrs = FnAttrs.addAttribute(C, Attribute::WillReturn);
    FnAttrs = FnAttrs.addAttribute(C, Attribute::NoUnwind);
    return AttributeList::get(C,
            FnAttrs,
            Attributes(C, {}),
            None);
}

static AttributeList get_attrs_noreturn(LLVMContext &C)
{
    return AttributeList::get(C,
                Attributes(C, {Attribute::NoReturn}),
                AttributeSet(),
                None);
}

static AttributeList get_attrs_sext(LLVMContext &C)
{
    return AttributeList::get(C,
                AttributeSet(),
                Attributes(C, {Attribute::NonNull}),
                {Attributes(C, {Attribute::SExt})});
}

static AttributeList get_attrs_zext(LLVMContext &C)
{
    return AttributeList::get(C,
                AttributeSet(),
                Attributes(C, {Attribute::NonNull}),
                {Attributes(C, {Attribute::ZExt})});
}


// global vars
static const auto jlRTLD_DEFAULT_var = new JuliaVariable{
    XSTR(jl_RTLD_DEFAULT_handle),
    true,
    [](LLVMContext &C) { return static_cast<llvm::Type*>(getInt8PtrTy(C)); },
};
#ifdef _OS_WINDOWS_
static const auto jlexe_var = new JuliaVariable{
    XSTR(jl_exe_handle),
    true,
    [](LLVMContext &C) { return static_cast<llvm::Type*>(getInt8PtrTy(C)); },
};
static const auto jldll_var = new JuliaVariable{
    XSTR(jl_libjulia_handle),
    true,
    [](LLVMContext &C) { return static_cast<llvm::Type*>(getInt8PtrTy(C)); },
};
static const auto jldlli_var = new JuliaVariable{
    XSTR(jl_libjulia_internal_handle),
    true,
    [](LLVMContext &C) { return static_cast<llvm::Type*>(getInt8PtrTy(C)); },
};
#endif //_OS_WINDOWS_

static const auto jlstack_chk_guard_var = new JuliaVariable{
    XSTR(__stack_chk_guard),
    true,
    get_pjlvalue,
};

static const auto jlgetworld_global = new JuliaVariable{
    XSTR(jl_world_counter),
    false,
    [](LLVMContext &C) { return (Type*)getSizeTy(C); },
};

static const auto jlboxed_int8_cache = new JuliaVariable{
    XSTR(jl_boxed_int8_cache),
    true,
    [](LLVMContext &C) { return (Type*)ArrayType::get(get_pjlvalue(C), 256); },
};

static const auto jlboxed_uint8_cache = new JuliaVariable{
    XSTR(jl_boxed_uint8_cache),
    true,
    [](LLVMContext &C) { return (Type*)ArrayType::get(get_pjlvalue(C), 256); },
};

static const auto jlpgcstack_func = new JuliaFunction{
    "julia.get_pgcstack",
    [](LLVMContext &C) { return FunctionType::get(PointerType::get(JuliaType::get_ppjlvalue_ty(C), 0), false); },
    nullptr,
};



// important functions
// Symbols are not gc-tracked, but we'll treat them as callee rooted anyway,
// because they may come from a gc-rooted location
static const auto jlnew_func = new JuliaFunction{
    XSTR(jl_new_structv),
    get_func_sig,
    get_func_attrs,
};
static const auto jlsplatnew_func = new JuliaFunction{
    XSTR(jl_new_structt),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_prjlvalue, T_prjlvalue}, false);
    },
    get_func_attrs,
};
static const auto jlthrow_func = new JuliaFunction{
    XSTR(jl_throw),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlerror_func = new JuliaFunction{
    XSTR(jl_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getInt8PtrTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlatomicerror_func = new JuliaFunction{
    XSTR(jl_atomic_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getInt8PtrTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jltypeerror_func = new JuliaFunction{
    XSTR(jl_type_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getInt8PtrTy(C), JuliaType::get_prjlvalue_ty(C), PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlundefvarerror_func = new JuliaFunction{
    XSTR(jl_undefined_var_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlboundserrorv_func = new JuliaFunction{
    XSTR(jl_bounds_error_ints),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted), getSizePtrTy(C), getSizeTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlboundserror_func = new JuliaFunction{
    XSTR(jl_bounds_error_int),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted), getSizeTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlvboundserror_func = new JuliaFunction{
    XSTR(jl_bounds_error_tuple_int),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_pprjlvalue_ty(C), getSizeTy(C), getSizeTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jluboundserror_func = new JuliaFunction{
    XSTR(jl_bounds_error_unboxed_int),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(getInt8Ty(C), AddressSpace::Derived), JuliaType::get_pjlvalue_ty(C), getSizeTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlcheckassign_func = new JuliaFunction{
    XSTR(jl_checked_assignment),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_pjlvalue_ty(C), PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    nullptr,
};
static const auto jldeclareconst_func = new JuliaFunction{
    XSTR(jl_declare_constant),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_pjlvalue_ty(C)}, false); },
    nullptr,
};
static const auto jlgetbindingorerror_func = new JuliaFunction{
    XSTR(jl_get_binding_or_error),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(T_pjlvalue,
                {T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jlgetbindingwrorerror_func = new JuliaFunction{
    XSTR(jl_get_binding_wr_or_error),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(T_pjlvalue,
                {T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jlboundp_func = new JuliaFunction{
    XSTR(jl_boundp),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
                {T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jltopeval_func = new JuliaFunction{
    XSTR(jl_toplevel_eval),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(T_pjlvalue,
                {T_pjlvalue, T_pjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto jlcopyast_func = new JuliaFunction{
    XSTR(jl_copy_ast),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
//static const auto jlnsvec_func = new JuliaFunction{
//    XSTR(jl_svec),
//    [](LLVMContext &C) { return FunctionType::get(T_prjlvalue,
//                {getSizeTy(C)}, true); },
//    [](LLVMContext &C) { return AttributeList::get(C,
//            AttributeSet(),
//            Attributes(C, {Attribute::NonNull}),
//            None); },
//};
static const auto jlapplygeneric_func = new JuliaFunction{
    XSTR(jl_apply_generic),
    get_func_sig,
    get_func_attrs,
};
static const auto jlinvoke_func = new JuliaFunction{
    XSTR(jl_invoke),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue, PointerType::get(T_prjlvalue, 0), getInt32Ty(C), T_prjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            {AttributeSet(),
             Attributes(C, {Attribute::ReadOnly, Attribute::NoCapture})}); },
};
static const auto jlmethod_func = new JuliaFunction{
    XSTR(jl_method_def),
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue, T_prjlvalue, T_prjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jlgenericfunction_func = new JuliaFunction{
    XSTR(jl_generic_function_def),
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_pprjlvalue = PointerType::get(T_prjlvalue, 0);
        return FunctionType::get(T_prjlvalue,
                {T_pjlvalue, T_pjlvalue, T_pprjlvalue, T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jllockvalue_func = new JuliaFunction{
    XSTR(jl_lock_value),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jlunlockvalue_func = new JuliaFunction{
    XSTR(jl_unlock_value),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jlenter_func = new JuliaFunction{
    XSTR(jl_enter_handler),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getInt8PtrTy(C)}, false); },
    nullptr,
};
static const auto jl_current_exception_func = new JuliaFunction{
    XSTR(jl_current_exception),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), false); },
    nullptr,
};
static const auto jlleave_func = new JuliaFunction{
    XSTR(jl_pop_handler),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getInt32Ty(C)}, false); },
    nullptr,
};
static const auto jl_restore_excstack_func = new JuliaFunction{
    XSTR(jl_restore_excstack),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getSizeTy(C)}, false); },
    nullptr,
};
static const auto jl_excstack_state_func = new JuliaFunction{
    XSTR(jl_excstack_state),
    [](LLVMContext &C) { return FunctionType::get(getSizeTy(C), false); },
    nullptr,
};
static const auto jlegalx_func = new JuliaFunction{
    XSTR(jl_egal__unboxed),
    [](LLVMContext &C) {
        Type *T = PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Derived);
        return FunctionType::get(getInt32Ty(C), {T, T, JuliaType::get_prjlvalue_ty(C)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReadOnly, Attribute::NoUnwind, Attribute::ArgMemOnly}),
            AttributeSet(),
            None); },
};
static const auto jl_alloc_obj_func = new JuliaFunction{
    "julia.gc_alloc_obj",
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_ppjlvalue = PointerType::get(PointerType::get(T_jlvalue, 0), 0);
        return FunctionType::get(T_prjlvalue,
                {T_ppjlvalue, getSizeTy(C), T_prjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet::get(C, makeArrayRef({Attribute::getWithAllocSizeArgs(C, 1, None)})), // returns %1 bytes
            Attributes(C, {Attribute::NoAlias, Attribute::NonNull}),
            None); },
};
static const auto jl_newbits_func = new JuliaFunction{
    XSTR(jl_new_bits),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue, getInt8PtrTy(C)}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
// `julia.typeof` does read memory, but it is effectively readnone before we lower
// the allocation function. This is OK as long as we lower `julia.typeof` no later than
// `julia.gc_alloc_obj`.
static const auto jl_typeof_func = new JuliaFunction{
    "julia.typeof",
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReadNone, Attribute::NoUnwind, Attribute::NoRecurse}),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto jl_loopinfo_marker_func = new JuliaFunction{
    "julia.loopinfo_marker",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C), false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReadOnly, Attribute::NoRecurse, Attribute::InaccessibleMemOnly}),
            AttributeSet(),
            None); },
};
static const auto jl_write_barrier_func = new JuliaFunction{
    "julia.write_barrier",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_prjlvalue_ty(C)}, true); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::NoUnwind, Attribute::NoRecurse, Attribute::InaccessibleMemOnly}),
            AttributeSet(),
            {Attributes(C, {Attribute::ReadOnly})}); },
};
static const auto jl_write_barrier_binding_func = new JuliaFunction{
    "julia.write_barrier_binding",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_prjlvalue_ty(C)}, true); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::NoUnwind, Attribute::NoRecurse, Attribute::InaccessibleMemOnly}),
            AttributeSet(),
            {Attributes(C, {Attribute::ReadOnly})}); },
};
static const auto jlisa_func = new JuliaFunction{
    XSTR(jl_isa),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};

static const auto jlsubtype_func = new JuliaFunction{
    XSTR(jl_subtype),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};
static const auto jlapplytype_func = new JuliaFunction{
    XSTR(jl_instantiate_type_in_env),
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_pprjlvalue = PointerType::get(T_prjlvalue, 0);
        return FunctionType::get(T_prjlvalue,
            {T_pjlvalue, T_pjlvalue, T_pprjlvalue}, false);
    },
    [](LLVMContext &C) {
        return AttributeList::get(C,
            AttributeSet(),
            AttributeSet::get(C, makeArrayRef({Attribute::get(C, Attribute::NonNull),
                                               Attribute::getWithAlignment(C, Align(16))})),
            None);
    },
};
static const auto jl_object_id__func = new JuliaFunction{
    XSTR(jl_object_id_),
    [](LLVMContext &C) { return FunctionType::get(getSizeTy(C),
            {JuliaType::get_prjlvalue_ty(C), PointerType::get(getInt8Ty(C), AddressSpace::Derived)}, false); },
    nullptr,
};
static const auto setjmp_func = new JuliaFunction{
    jl_setjmp_name,
    [](LLVMContext &C) { return FunctionType::get(getInt32Ty(C),
            {getInt8PtrTy(C),
#ifndef _OS_WINDOWS_
            getInt32Ty(C),
#endif
            }, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReturnsTwice}),
            AttributeSet(),
            None); },
};
static const auto memcmp_func = new JuliaFunction{
    XSTR(memcmp),
    [](LLVMContext &C) { return FunctionType::get(getInt32Ty(C),
            {getInt8PtrTy(C), getInt8PtrTy(C), getSizeTy(C)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReadOnly, Attribute::NoUnwind, Attribute::ArgMemOnly}),
            AttributeSet(),
            None); },
    // TODO: inferLibFuncAttributes(*memcmp_func, TLI);
};
static const auto jldlsym_func = new JuliaFunction{
    XSTR(jl_load_and_lookup),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pvoidfunc_ty(C),
            {getInt8PtrTy(C), getInt8PtrTy(C), PointerType::get(getInt8PtrTy(C), 0)}, false); },
    nullptr,
};
static const auto jllazydlsym_func = new JuliaFunction{
    XSTR(jl_lazy_load_and_lookup),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pvoidfunc_ty(C),
            {JuliaType::get_prjlvalue_ty(C), getInt8PtrTy(C)}, false); },
    nullptr,
};
static const auto jltypeassert_func = new JuliaFunction{
    XSTR(jl_typeassert),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};
static const auto jlgetnthfieldchecked_func = new JuliaFunction{
    XSTR(jl_get_nth_field_checked),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_prjlvalue, getSizeTy(C)}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto jlgetcfunctiontrampoline_func = new JuliaFunction{
    XSTR(jl_get_cfunction_trampoline),
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
        auto T_pprjlvalue = PointerType::get(T_prjlvalue, 0);
        return FunctionType::get(T_prjlvalue,
            {
                T_prjlvalue, // f (object)
                T_pjlvalue, // result
                getInt8PtrTy(C), // cache
                T_pjlvalue, // fill
                FunctionType::get(getInt8PtrTy(C), { getInt8PtrTy(C), T_ppjlvalue }, false)->getPointerTo(), // trampoline
                T_pjlvalue, // env
                T_pprjlvalue, // vals
            }, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto diff_gc_total_bytes_func = new JuliaFunction{
    XSTR(jl_gc_diff_total_bytes),
    [](LLVMContext &C) { return FunctionType::get(getInt64Ty(C), false); },
    nullptr,
};
static const auto sync_gc_total_bytes_func = new JuliaFunction{
    XSTR(jl_gc_sync_total_bytes),
    [](LLVMContext &C) { return FunctionType::get(getInt64Ty(C),
            {getInt64Ty(C)}, false); },
    nullptr,
};
static const auto jlarray_data_owner_func = new JuliaFunction{
    XSTR(jl_array_data_owner),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_prjlvalue}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReadOnly, Attribute::NoUnwind}),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
#define BOX_FUNC(ct,at,attrs)                                                    \
static const auto box_##ct##_func = new JuliaFunction{                           \
    XSTR(jl_box_##ct),                                                           \
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C),\
            {at}, false); },                                                     \
    attrs,                                                                       \
}
BOX_FUNC(int16, getInt16Ty(C), get_attrs_sext);
BOX_FUNC(uint16, getInt16Ty(C), get_attrs_zext);
BOX_FUNC(int32, getInt32Ty(C), get_attrs_sext);
BOX_FUNC(uint32, getInt32Ty(C), get_attrs_zext);
BOX_FUNC(int64, getInt64Ty(C), get_attrs_sext);
BOX_FUNC(uint64, getInt64Ty(C), get_attrs_zext);
BOX_FUNC(char, getCharTy(C), get_attrs_zext);
BOX_FUNC(float32, getFloatTy(C), get_func_attrs);
BOX_FUNC(float64, getDoubleTy(C), get_func_attrs);
BOX_FUNC(ssavalue, getSizeTy(C), get_func_attrs);
#undef BOX_FUNC


// placeholder functions
static const auto gcroot_flush_func = new JuliaFunction{
    "julia.gcroot_flush",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C), false); },
    nullptr,
};
static const auto gc_preserve_begin_func = new JuliaFunction{
    "llvm.julia.gc_preserve_begin",
    [](LLVMContext &C) { return FunctionType::get(Type::getTokenTy(C), true); },
    nullptr,
};
static const auto gc_preserve_end_func = new JuliaFunction {
    "llvm.julia.gc_preserve_end",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C), {Type::getTokenTy(C)}, false); },
    nullptr,
};
static const auto except_enter_func = new JuliaFunction{
    "julia.except_enter",
    [](LLVMContext &C) { return FunctionType::get(getInt32Ty(C), false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet::get(C, makeArrayRef({Attribute::get(C, Attribute::ReturnsTwice)})),
            AttributeSet(),
            None); },
};
static const auto pointer_from_objref_func = new JuliaFunction{
    "julia.pointer_from_objref",
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pjlvalue_ty(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Derived)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet::get(C, makeArrayRef({Attribute::get(C, Attribute::ReadNone), Attribute::get(C, Attribute::NoUnwind)})),
            Attributes(C, {Attribute::NonNull}),
            None); },
};

static const auto jltuple_func = new JuliaFunction{XSTR(jl_f_tuple), get_func_sig, get_func_attrs};
static const auto &builtin_func_map() {
    static std::map<jl_fptr_args_t, JuliaFunction*> builtins = { { jl_f_is_addr,                 new JuliaFunction{XSTR(jl_f_is), get_func_sig, get_func_attrs} },
          { jl_f_typeof_addr,             new JuliaFunction{XSTR(jl_f_typeof), get_func_sig, get_func_attrs} },
          { jl_f_sizeof_addr,             new JuliaFunction{XSTR(jl_f_sizeof), get_func_sig, get_func_attrs} },
          { jl_f_issubtype_addr,          new JuliaFunction{XSTR(jl_f_issubtype), get_func_sig, get_func_attrs} },
          { jl_f_isa_addr,                new JuliaFunction{XSTR(jl_f_isa), get_func_sig, get_func_attrs} },
          { jl_f_typeassert_addr,         new JuliaFunction{XSTR(jl_f_typeassert), get_func_sig, get_func_attrs} },
          { jl_f_ifelse_addr,             new JuliaFunction{XSTR(jl_f_ifelse), get_func_sig, get_func_attrs} },
          { jl_f__apply_iterate_addr,     new JuliaFunction{XSTR(jl_f__apply_iterate), get_func_sig, get_func_attrs} },
          { jl_f__apply_pure_addr,        new JuliaFunction{XSTR(jl_f__apply_pure), get_func_sig, get_func_attrs} },
          { jl_f__call_latest_addr,       new JuliaFunction{XSTR(jl_f__call_latest), get_func_sig, get_func_attrs} },
          { jl_f__call_in_world_addr,     new JuliaFunction{XSTR(jl_f__call_in_world), get_func_sig, get_func_attrs} },
          { jl_f__call_in_world_total_addr, new JuliaFunction{XSTR(jl_f__call_in_world_total), get_func_sig, get_func_attrs} },
          { jl_f_throw_addr,              new JuliaFunction{XSTR(jl_f_throw), get_func_sig, get_func_attrs} },
          { jl_f_tuple_addr,              jltuple_func },
          { jl_f_svec_addr,               new JuliaFunction{XSTR(jl_f_svec), get_func_sig, get_func_attrs} },
          { jl_f_applicable_addr,         new JuliaFunction{XSTR(jl_f_applicable), get_func_sig, get_func_attrs} },
          { jl_f_invoke_addr,             new JuliaFunction{XSTR(jl_f_invoke), get_func_sig, get_func_attrs} },
          { jl_f_invoke_kwsorter_addr,    new JuliaFunction{XSTR(jl_f_invoke_kwsorter), get_func_sig, get_func_attrs} },
          { jl_f_isdefined_addr,          new JuliaFunction{XSTR(jl_f_isdefined), get_func_sig, get_func_attrs} },
          { jl_f_getfield_addr,           new JuliaFunction{XSTR(jl_f_getfield), get_func_sig, get_func_attrs} },
          { jl_f_setfield_addr,           new JuliaFunction{XSTR(jl_f_setfield), get_func_sig, get_func_attrs} },
          { jl_f_swapfield_addr,          new JuliaFunction{XSTR(jl_f_swapfield), get_func_sig, get_func_attrs} },
          { jl_f_modifyfield_addr,        new JuliaFunction{XSTR(jl_f_modifyfield), get_func_sig, get_func_attrs} },
          { jl_f_fieldtype_addr,          new JuliaFunction{XSTR(jl_f_fieldtype), get_func_sig, get_func_attrs} },
          { jl_f_nfields_addr,            new JuliaFunction{XSTR(jl_f_nfields), get_func_sig, get_func_attrs} },
          { jl_f__expr_addr,              new JuliaFunction{XSTR(jl_f__expr), get_func_sig, get_func_attrs} },
          { jl_f__typevar_addr,           new JuliaFunction{XSTR(jl_f__typevar), get_func_sig, get_func_attrs} },
          { jl_f_arrayref_addr,           new JuliaFunction{XSTR(jl_f_arrayref), get_func_sig, get_func_attrs} },
          { jl_f_const_arrayref_addr,     new JuliaFunction{XSTR(jl_f_const_arrayref), get_func_sig, get_func_attrs} },
          { jl_f_arrayset_addr,           new JuliaFunction{XSTR(jl_f_arrayset), get_func_sig, get_func_attrs} },
          { jl_f_arraysize_addr,          new JuliaFunction{XSTR(jl_f_arraysize), get_func_sig, get_func_attrs} },
          { jl_f_apply_type_addr,         new JuliaFunction{XSTR(jl_f_apply_type), get_func_sig, get_func_attrs} },
          { jl_f_donotdelete_addr,        new JuliaFunction{XSTR(jl_f_donotdelete), get_donotdelete_sig, get_donotdelete_func_attrs} }
        };
    return builtins;
}

static const auto jl_new_opaque_closure_jlcall_func = new JuliaFunction{XSTR(jl_new_opaque_closure_jlcall), get_func_sig, get_func_attrs};

static std::atomic<int> globalUniqueGeneratedNames{0};

// --- code generation ---
extern "C" {
    jl_cgparams_t jl_default_cgparams = {1, 1, 0,
#ifdef _OS_WINDOWS_
        0,
#else
        1,
#endif
        (int) DICompileUnit::DebugEmissionKind::FullDebug,
        jl_rettype_inferred, NULL };
}


static MDNode *best_tbaa(jl_tbaacache_t &tbaa_cache, jl_value_t *jt) {
    jt = jl_unwrap_unionall(jt);
    if (jt == (jl_value_t*)jl_datatype_type ||
        (jl_is_type_type(jt) && jl_is_datatype(jl_tparam0(jt))))
        return tbaa_cache.tbaa_datatype;
    if (!jl_is_datatype(jt))
        return tbaa_cache.tbaa_value;
    if (jl_is_abstracttype(jt))
        return tbaa_cache.tbaa_value;
    // If we're here, we know all subtypes are (im)mutable, even if we
    // don't know what the exact type is
    return jl_is_mutable(jt) ? tbaa_cache.tbaa_mutab : tbaa_cache.tbaa_immut;
}

// tracks whether codegen is currently able to simply stack-allocate this type
// note that this includes jl_isbits, although codegen should work regardless
static bool jl_is_concrete_immutable(jl_value_t* t)
{
    return jl_is_immutable_datatype(t) && ((jl_datatype_t*)t)->isconcretetype;
}

static bool jl_is_pointerfree(jl_value_t* t)
{
    if (!jl_is_concrete_immutable(t))
        return 0;
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)t)->layout;
    return layout && layout->npointers == 0;
}

// these queries are usually related, but we split them out here
// for convenience and clarity (and because it changes the calling convention)
// n.b. this must include jl_is_datatype_singleton (ghostType) and primitive types
static bool deserves_stack(jl_value_t* t)
{
    if (!jl_is_concrete_immutable(t))
        return false;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    return jl_is_datatype_singleton(dt) || jl_datatype_isinlinealloc(dt, 0);
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
    // If this is non-NULL, it is always of type `T_prjlvalue`
    Value *Vboxed;
    Value *TIndex; // if `V` is an unboxed (tagged) Union described by `typ`, this gives the DataType index (1-based, small int) as an i8
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    jl_value_t *typ; // the original type of V, never NULL
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    MDNode *tbaa; // The related tbaa node. Non-NULL iff this holds an address.
    // If non-null, this memory location may be promoted on use, by hoisting the
    // destination memory above the promotion point.
    Instruction *promotion_point;
    // If promotion_ssa is non-null, the julia src ssa value that corresponds
    // to the promotion point. This is used for dominator analysis, since LLVM's
    // dominator analysis has algorithmic problems for large basic blocks.
    ssize_t promotion_ssa;
    bool ispointer() const
    {
        // whether this value is compatible with `data_pointer`
        return tbaa != nullptr;
    }
    jl_cgval_t(Value *V, Value *gcroot, bool isboxed, jl_value_t *typ, Value *tindex, jl_tbaacache_t &tbaa_cache) : // general constructor (with pointer type auto-detect)
        V(V), // V is allowed to be NULL in a jl_varinfo_t context, but not during codegen contexts
        Vboxed(isboxed ? V : nullptr),
        TIndex(tindex),
        constant(NULL),
        typ(typ),
        isboxed(isboxed),
        isghost(false),
        tbaa(isboxed ? best_tbaa(tbaa_cache, typ) : nullptr),
        promotion_point(nullptr),
        promotion_ssa(-1)
    {
        if (Vboxed)
            assert(Vboxed->getType() == JuliaType::get_prjlvalue_ty(Vboxed->getContext()));
        assert(gcroot == nullptr);
        assert(!(isboxed && TIndex != NULL));
        assert(TIndex == NULL || TIndex->getType() == getInt8Ty(TIndex->getContext()));
    }
    explicit jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        // mark explicit to avoid being used implicitly for conversion from NULL (use jl_cgval_t(ctx.builder.getContext()) instead)
        V(NULL),
        Vboxed(NULL),
        TIndex(NULL),
        constant(((jl_datatype_t*)typ)->instance),
        typ(typ),
        isboxed(false),
        isghost(true),
        tbaa(nullptr),
        promotion_point(nullptr),
        promotion_ssa(-1)
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
        tbaa(v.tbaa),
        promotion_point(v.promotion_point),
        promotion_ssa(v.promotion_ssa)
    {
        if (Vboxed)
            assert(Vboxed->getType() == JuliaType::get_prjlvalue_ty(Vboxed->getContext()));
        // this constructor expects we had a badly or equivalently typed version
        // make sure we aren't discarding the actual type information
        if (v.TIndex) {
            assert((TIndex == NULL) == jl_is_concrete_type(typ));
        }
        else {
            assert(isboxed || v.typ == typ || tindex);
        }
    }
    explicit jl_cgval_t(LLVMContext &ctxt) : // undef / unreachable constructor
        V(UndefValue::get(getVoidTy(ctxt))),
        Vboxed(NULL),
        TIndex(NULL),
        constant(NULL),
        typ(jl_bottom_type),
        isboxed(false),
        isghost(true),
        tbaa(nullptr),
        promotion_point(nullptr),
        promotion_ssa(-1)
    {
    }
};

// per-local-variable information
struct jl_varinfo_t {
    Instruction *boxroot; // an address, if the var might be in a jl_value_t** stack slot (marked ctx.tbaa().tbaa_const, if appropriate)
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

    jl_varinfo_t(LLVMContext &ctxt) : boxroot(NULL),
                     value(jl_cgval_t(ctxt)),
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
    llvm::MapVector<jl_code_instance_t*, jl_codegen_call_target_t> call_targets;
    std::map<void*, GlobalVariable*> &global_targets;
    Function *f = NULL;
    // local var info. globals are not in here.
    std::vector<jl_varinfo_t> slots;
    std::map<int, jl_varinfo_t> phic_slots;
    std::vector<jl_cgval_t> SAvalues;
    std::vector<std::tuple<jl_cgval_t, BasicBlock *, AllocaInst *, PHINode *, jl_value_t *>> PhiNodes;
    std::vector<bool> ssavalue_assigned;
    std::vector<int> ssavalue_usecount;
    std::vector<orc::ThreadSafeModule> oc_modules;
    jl_module_t *module = NULL;
    jl_typecache_t type_cache;
    jl_tbaacache_t tbaa_cache;
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
    bool is_opaque_closure = false;

    CallInst *pgcstack = NULL;
    Value *world_age_field = NULL;

    bool debug_enabled = false;
    bool use_cache = false;
    const jl_cgparams_t *params = NULL;

    std::vector<orc::ThreadSafeModule> llvmcall_modules;

    jl_codectx_t(LLVMContext &llvmctx, jl_codegen_params_t &params)
      : builder(llvmctx),
        emission_context(params),
        call_targets(),
        global_targets(params.globals),
        world(params.world),
        use_cache(params.cache),
        params(params.params) { }

    jl_typecache_t &types() {
        type_cache.initialize(builder.getContext());
        return type_cache;
    }

    jl_tbaacache_t &tbaa() {
        tbaa_cache.initialize(builder.getContext());
        return tbaa_cache;
    }

    ~jl_codectx_t() {
        assert(this->roots == NULL);
        // Transfer local delayed calls to the global queue
        for (auto call_target : call_targets)
            emission_context.workqueue.push_back(call_target);
    }
};

GlobalVariable *JuliaVariable::realize(jl_codectx_t &ctx) {
    return realize(jl_Module);
}

static Type *julia_type_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed = NULL);
static jl_returninfo_t get_specsig_function(jl_codectx_t &ctx, Module *M, StringRef name, jl_value_t *sig, jl_value_t *jlrettype, bool is_opaque_closure);
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaval = -1);
static Value *global_binding_pointer(jl_codectx_t &ctx, jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign);
static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, bool isvol, MDNode *tbaa);
static jl_cgval_t emit_sparam(jl_codectx_t &ctx, size_t i);
static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const std::string &msg);
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0);
static Value *get_current_task(jl_codectx_t &ctx);
static Value *get_current_ptls(jl_codectx_t &ctx);
static Value *get_current_signal_page(jl_codectx_t &ctx);
static void CreateTrap(IRBuilder<> &irbuilder, bool create_new_block = true);
static CallInst *emit_jlcall(jl_codectx_t &ctx, Function *theFptr, Value *theF,
                             const jl_cgval_t *args, size_t nargs, CallingConv::ID cc);
static CallInst *emit_jlcall(jl_codectx_t &ctx, JuliaFunction *theFptr, Value *theF,
                             const jl_cgval_t *args, size_t nargs, CallingConv::ID cc);
static Value *emit_f_is(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2,
                        Value *nullcheck1 = nullptr, Value *nullcheck2 = nullptr);
static jl_cgval_t emit_new_struct(jl_codectx_t &ctx, jl_value_t *ty, size_t nargs, const jl_cgval_t *argv, bool is_promotable=false);
static jl_cgval_t emit_invoke(jl_codectx_t &ctx, const jl_cgval_t &lival, const jl_cgval_t *argv, size_t nargs, jl_value_t *rt);

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p);
static GlobalVariable *prepare_global_in(Module *M, GlobalVariable *G);
Instruction *tbaa_decorate(MDNode *md, Instruction *inst);

static GlobalVariable *prepare_global_in(Module *M, JuliaVariable *G)
{
    return G->realize(M);
}

static Function *prepare_call_in(Module *M, JuliaFunction *G)
{
    return G->realize(M);
}

static inline GlobalVariable *prepare_global_in(Module *M, GlobalVariable *G)
{
    if (G->getParent() == M)
        return G;
    GlobalValue *local = M->getNamedValue(G->getName());
    if (!local) {
        // Copy the GlobalVariable, but without the initializer, so it becomes a declaration
        GlobalVariable *proto = new GlobalVariable(*M, G->getValueType(),
                G->isConstant(), GlobalVariable::ExternalLinkage,
                nullptr, G->getName(), nullptr, G->getThreadLocalMode());
        proto->copyAttributesFrom(G);
        // DLLImport only needs to be set for the shadow module
        // it just gets annoying in the JIT
        proto->setDLLStorageClass(GlobalValue::DefaultStorageClass);
        return proto;
    }
    return cast<GlobalVariable>(local);
}


// --- convenience functions for tagging llvm values with julia types ---

static GlobalVariable *get_pointer_to_constant(jl_codegen_params_t &emission_context, Constant *val, StringRef name, Module &M)
{
    GlobalVariable *&gv = emission_context.mergedConstants[val];
    StringRef localname;
    std::string ssno;
    if (gv == nullptr) {
        raw_string_ostream(ssno) << name << emission_context.mergedConstants.size();
        localname = StringRef(ssno);
    }
    else {
        localname = gv->getName();
        if (gv->getParent() != &M)
            gv = cast_or_null<GlobalVariable>(M.getNamedValue(localname));
    }
    if (gv == nullptr) {
        gv = new GlobalVariable(
                M,
                val->getType(),
                true,
                GlobalVariable::PrivateLinkage,
                val,
                localname);
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    }
    assert(localname == gv->getName());
    assert(val == gv->getInitializer());
    return gv;
}

static AllocaInst *emit_static_alloca(jl_codectx_t &ctx, Type *lty)
{
    ++EmittedAllocas;
    return new AllocaInst(lty, 0, "", /*InsertBefore=*/ctx.pgcstack);
}

static void undef_derived_strct(IRBuilder<> &irbuilder, Value *ptr, jl_datatype_t *sty, MDNode *tbaa)
{
    assert(ptr->getType()->getPointerAddressSpace() != AddressSpace::Tracked);
    size_t first_offset = sty->layout->nfields ? jl_field_offset(sty, 0) : 0;
    if (first_offset != 0)
        irbuilder.CreateMemSet(ptr, ConstantInt::get(getInt8Ty(irbuilder.getContext()), 0), first_offset, MaybeAlign(0));
    size_t i, np = sty->layout->npointers;
    if (np == 0)
        return;
    auto T_prjlvalue = JuliaType::get_prjlvalue_ty(irbuilder.getContext());
    ptr = irbuilder.CreateBitCast(ptr, T_prjlvalue->getPointerTo(ptr->getType()->getPointerAddressSpace()));
    for (i = 0; i < np; i++) {
        Value *fld = irbuilder.CreateConstInBoundsGEP1_32(T_prjlvalue, ptr, jl_ptr_offset(sty, i));
        tbaa_decorate(tbaa, irbuilder.CreateStore(Constant::getNullValue(T_prjlvalue), fld));
    }
}

static Value *emit_inttoptr(jl_codectx_t &ctx, Value *v, Type *ty)
{
    // Almost all of our inttoptr are generated due to representing `Ptr` with `getSizeTy(ctx.builder.getContext())`
    // in LLVM and most of these integers are generated from `ptrtoint` in the first place.
    if (auto I = dyn_cast<PtrToIntInst>(v)) {
        auto ptr = I->getOperand(0);
        if (ty->getPointerAddressSpace() == ptr->getType()->getPointerAddressSpace())
            return ctx.builder.CreateBitCast(ptr, ty);
        else if (cast<PointerType>(ty)->hasSameElementTypeAs(cast<PointerType>(ptr->getType())))
            return ctx.builder.CreateAddrSpaceCast(ptr, ty);
    }
    ++EmittedIntToPtrs;
    return ctx.builder.CreateIntToPtr(v, ty);
}

static inline jl_cgval_t ghostValue(jl_codectx_t &ctx, jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return jl_cgval_t(ctx.builder.getContext()); // Undef{}
    if (typ == (jl_value_t*)jl_typeofbottom_type) {
        // normalize TypeofBottom to Type{Union{}}
        typ = (jl_value_t*)jl_typeofbottom_type->super;
    }
    if (jl_is_type_type(typ)) {
        // replace T::Type{T} with T, by assuming that T must be a leaftype of some sort
        jl_cgval_t constant(NULL, NULL, true, typ, NULL, ctx.tbaa());
        constant.constant = jl_tparam0(typ);
        return constant;
    }
    return jl_cgval_t(typ);
}
static inline jl_cgval_t ghostValue(jl_codectx_t &ctx, jl_datatype_t *typ)
{
    return ghostValue(ctx, (jl_value_t*)typ);
}

static inline jl_cgval_t mark_julia_const(jl_codectx_t &ctx, jl_value_t *jv)
{
    jl_value_t *typ;
    if (jl_is_type(jv)) {
        typ = (jl_value_t*)jl_wrap_Type(jv); // TODO: gc-root this?
    }
    else {
        typ = jl_typeof(jv);
        if (jl_is_datatype_singleton((jl_datatype_t*)typ))
            return ghostValue(ctx, typ);
    }
    jl_cgval_t constant(NULL, NULL, true, typ, NULL, ctx.tbaa());
    constant.constant = jv;
    return constant;
}


static inline jl_cgval_t mark_julia_slot(Value *v, jl_value_t *typ, Value *tindex, jl_tbaacache_t &tbaa_cache, MDNode *tbaa)
{
    // this enables lazy-copying of immutable values and stack or argument slots
    assert(tbaa);
    jl_cgval_t tagval(v, NULL, false, typ, tindex, tbaa_cache);
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
        loc = get_pointer_to_constant(ctx.emission_context, cast<Constant>(v), "_j_const", *jl_Module);
    }
    else {
        loc = emit_static_alloca(ctx, v->getType());
        ctx.builder.CreateStore(v, loc);
    }
    return mark_julia_slot(loc, typ, tindex, ctx.tbaa(), ctx.tbaa().tbaa_stack);
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
        return ghostValue(ctx, typ);
    }
    if (jl_is_type_type(typ)) {
        jl_value_t *tp0 = jl_tparam0(typ);
        if (jl_is_concrete_type(tp0) || tp0 == jl_bottom_type) {
            // replace T::Type{T} with T
            return ghostValue(ctx, typ);
        }
    }
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T)) {
        return ghostValue(ctx, typ);
    }
    if (v && !isboxed && v->getType()->isAggregateType() && !jl_is_vecelement_type(typ) && CountTrackedPointers(v->getType()).count == 0) {
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        return value_to_pointer(ctx, v, typ, NULL);
    }
    return jl_cgval_t(v, NULL, isboxed, typ, NULL, ctx.tbaa());
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_datatype_t *typ)
{
    return mark_julia_type(ctx, v, isboxed, (jl_value_t*)typ);
}

// see if it might be profitable (and cheap) to change the type of v to typ
static inline jl_cgval_t update_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    if (v.typ == jl_bottom_type || v.constant || typ == (jl_value_t*)jl_any_type || jl_egal(v.typ, typ))
        return v; // fast-path
    if (jl_is_concrete_type(v.typ) && !jl_is_kind(v.typ)) {
        if (jl_is_concrete_type(typ) && !jl_is_kind(typ)) {
            // type mismatch: changing from one leaftype to another
            CreateTrap(ctx.builder);
            return jl_cgval_t(ctx.builder.getContext());
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
                alwaysboxed = !((jl_datatype_t*)utyp)->name->abstract && ((jl_datatype_t*)utyp)->name->mutabl;
            if (alwaysboxed) {
                // discovered that this union-split type must actually be isboxed
                if (v.Vboxed) {
                    return jl_cgval_t(v.Vboxed, nullptr, true, typ, NULL, ctx.tbaa());
                }
                else {
                    // type mismatch (there weren't any boxed values in the union)
                    CreateTrap(ctx.builder);
                    return jl_cgval_t(ctx.builder.getContext());
                }
            }
        }
        if (!jl_is_concrete_type(typ))
            return v; // not generally worth trying to change type info (which would require recomputing tindex)
    }
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T))
        return ghostValue(ctx, typ);
    return jl_cgval_t(v, typ, NULL);
}

static jl_cgval_t convert_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ, Value **skip=nullptr);

// --- allocating local variables ---

static jl_sym_t *slot_symbol(jl_codectx_t &ctx, int s)
{
    return (jl_sym_t*)jl_array_ptr_ref(ctx.source->slotnames, s);
}

static void store_def_flag(jl_codectx_t &ctx, const jl_varinfo_t &vi, bool val)
{
    assert((!vi.boxroot || vi.pTIndex) && "undef check is null pointer for boxed things");
    assert(vi.usedUndef && vi.defFlag && "undef flag codegen corrupted");
    ctx.builder.CreateStore(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), val), vi.defFlag, vi.isVolatile);
}

static void alloc_def_flag(jl_codectx_t &ctx, jl_varinfo_t& vi)
{
    assert((!vi.boxroot || vi.pTIndex) && "undef check is null pointer for boxed things");
    if (vi.usedUndef) {
        vi.defFlag = emit_static_alloca(ctx, getInt1Ty(ctx.builder.getContext()));
        store_def_flag(ctx, vi, false);
    }
}


// --- utilities ---

static Constant *undef_value_for_type(Type *T) {
    auto tracked = CountTrackedPointers(T);
    Constant *undef;
    if (tracked.count)
        // make sure gc pointers (including ptr_phi of union-split) are initialized to NULL
        undef = Constant::getNullValue(T);
    else
        undef = UndefValue::get(T);
    return undef;
}

static void CreateTrap(IRBuilder<> &irbuilder, bool create_new_block)
{
    Function *f = irbuilder.GetInsertBlock()->getParent();
    Function *trap_func = Intrinsic::getDeclaration(
            f->getParent(),
            Intrinsic::trap);
    irbuilder.CreateCall(trap_func);
    irbuilder.CreateUnreachable();
    if (create_new_block) {
        BasicBlock *newBB = BasicBlock::Create(irbuilder.getContext(), "after_noret", f);
        irbuilder.SetInsertPoint(newBB);
    }
    else {
        irbuilder.ClearInsertionPoint();
    }
}

#if 0 // this code is likely useful, but currently unused
#ifndef JL_NDEBUG
static void CreateConditionalAbort(IRBuilder<> &irbuilder, Value *test)
{
    Function *f = irbuilder.GetInsertBlock()->getParent();
    BasicBlock *abortBB = BasicBlock::Create(irbuilder.getContext(), "debug_abort", f);
    BasicBlock *postBB = BasicBlock::Create(irbuilder.getContext(), "post_abort", f);
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

static jl_cgval_t convert_julia_type_union(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ, Value **skip)
{
    // previous value was a split union, compute new index, or box
    Value *new_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80);
    SmallBitVector skip_box(1, true);
    Value *tindex = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
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
                    Value *cmp = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx));
                    new_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), new_idx), new_tindex);
                    t = true;
                }
                else if (!jl_subtype((jl_value_t*)jt, typ)) {
                    // new value doesn't need to be boxed
                    // since it isn't part of the new union
                    t = true;
                    if (skip) {
                        Value *skip1 = ctx.builder.CreateICmpEQ(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx));
                        *skip = *skip ? ctx.builder.CreateOr(*skip, skip1) : skip1;
                    }
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
            wasboxed = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
            new_tindex = ctx.builder.CreateOr(wasboxed, new_tindex);
            wasboxed = ctx.builder.CreateICmpNE(wasboxed, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));

            BasicBlock *currBB = ctx.builder.GetInsertBlock();

            // We lazily create a BB for this, once we decide that we
            // actually need it.
            Value *union_box_dt = NULL;
            BasicBlock *union_isaBB = NULL;
            BasicBlock *post_union_isaBB = NULL;
            auto maybe_setup_union_isa = [&]() {
                if (!union_isaBB) {
                    union_isaBB = BasicBlock::Create(ctx.builder.getContext(), "union_isa", ctx.f);
                    ctx.builder.SetInsertPoint(union_isaBB);
                    union_box_dt = emit_typeof(ctx, v.Vboxed, skip != NULL);
                    post_union_isaBB = ctx.builder.GetInsertBlock();
                }
            };

            // If we don't find a match. The type remains unknown
            // (0x80). We could use `v.Tindex`, here, since we know
            // it has to be 0x80, but it seems likely the backend
            // will like the explicit constant better.
            Value *union_box_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80);
            unsigned counter = 0;
            for_each_uniontype_small(
                // for each new union-split value
                [&](unsigned idx, jl_datatype_t *jt) {
                    unsigned old_idx = get_box_tindex(jt, v.typ);
                    if (old_idx == 0) {
                        // didn't handle this item before, select its new union index
                        maybe_setup_union_isa();
                        Value *cmp = ctx.builder.CreateICmpEQ(track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jt)), union_box_dt);
                        union_box_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80 | idx), union_box_tindex);
                    }
                },
                typ,
                counter);
            if (union_box_dt) {
                BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_union_isa", ctx.f);
                ctx.builder.CreateBr(postBB);
                ctx.builder.SetInsertPoint(currBB);
                Value *wasunknown = ctx.builder.CreateICmpEQ(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
                ctx.builder.CreateCondBr(wasunknown, union_isaBB, postBB);
                ctx.builder.SetInsertPoint(postBB);
                PHINode *tindex_phi = ctx.builder.CreatePHI(getInt8Ty(ctx.builder.getContext()), 2);
                tindex_phi->addIncoming(new_tindex, currBB);
                tindex_phi->addIncoming(union_box_tindex, post_union_isaBB);
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
                    ctx.builder.CreateAnd(new_tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
                boxv = ctx.builder.CreateSelect(
                    ctx.builder.CreateAnd(wasboxed, isboxed), v.Vboxed, boxv);
            }
            Value *slotv;
            MDNode *tbaa;
            if (v.V == NULL) {
                // v.V might be NULL if it was all ghost objects before
                slotv = NULL;
                tbaa = ctx.tbaa().tbaa_const;
            }
            else {
                Value *isboxv = ctx.builder.CreateIsNotNull(boxv);
                if (v.ispointer()) {
                    slotv = v.V;
                    tbaa = v.tbaa;
                }
                else {
                    slotv = emit_static_alloca(ctx, v.V->getType());
                    ctx.builder.CreateStore(v.V, slotv);
                    tbaa = ctx.tbaa().tbaa_stack;
                }
                slotv = ctx.builder.CreateSelect(isboxv,
                            decay_derived(ctx, boxv),
                            decay_derived(ctx, emit_bitcast(ctx, slotv, boxv->getType())));
            }
            jl_cgval_t newv = jl_cgval_t(slotv, NULL, false, typ, new_tindex, ctx.tbaa());
            assert(boxv->getType() == ctx.types().T_prjlvalue);
            newv.Vboxed = boxv;
            newv.tbaa = tbaa;
            return newv;
        }
    }
    else {
        return jl_cgval_t(boxed(ctx, v), NULL, true, typ, NULL, ctx.tbaa());
    }
    return jl_cgval_t(v, typ, new_tindex);
}

// given a value marked with type `v.typ`, compute the mapping and/or boxing to return a value of type `typ`
// TODO: should this set TIndex when trivial (such as 0x80 or concrete types) ?
static jl_cgval_t convert_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ, Value **skip)
{
    if (typ == (jl_value_t*)jl_typeofbottom_type)
        return ghostValue(ctx, typ); // normalize TypeofBottom to Type{Union{}}
    if (v.typ == jl_bottom_type || jl_egal(v.typ, typ))
        return v; // fast-path
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T))
        return ghostValue(ctx, typ);
    Value *new_tindex = NULL;
    if (jl_is_concrete_type(typ)) {
        if (v.TIndex && !jl_is_pointerfree(typ)) {
            // discovered that this union-split type must actually be isboxed
            if (v.Vboxed) {
                return jl_cgval_t(v.Vboxed, nullptr, true, typ, NULL, ctx.tbaa());
            }
            else {
                // type mismatch: there weren't any boxed values in the union
                if (skip)
                    *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
                else
                    CreateTrap(ctx.builder);
                return jl_cgval_t(ctx.builder.getContext());
            }
        }
        if (jl_is_concrete_type(v.typ) && !jl_is_kind(v.typ)) {
            if (jl_is_concrete_type(typ) && !jl_is_kind(typ)) {
                // type mismatch: changing from one leaftype to another
                if (skip)
                    *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
                else
                    CreateTrap(ctx.builder);
                return jl_cgval_t(ctx.builder.getContext());
            }
        }
    }
    else {
        bool makeboxed = false;
        if (v.TIndex) {
            return convert_julia_type_union(ctx, v, typ, skip);
        }
        else if (!v.isboxed && jl_is_uniontype(typ)) {
            // previous value was unboxed (leaftype), statically compute union tindex
            assert(jl_is_concrete_type(v.typ));
            unsigned new_idx = get_box_tindex((jl_datatype_t*)v.typ, typ);
            if (new_idx) {
                new_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), new_idx);
                if (v.V && !v.ispointer()) {
                    // TODO: remove this branch once all consumers of v.TIndex understand how to handle a non-ispointer value
                    Value *slotv = emit_static_alloca(ctx, v.V->getType());
                    ctx.builder.CreateStore(v.V, slotv);
                    jl_cgval_t newv = jl_cgval_t(slotv, NULL, false, typ, new_tindex, ctx.tbaa());
                    newv.tbaa = ctx.tbaa().tbaa_stack;
                    return newv;
                }
            }
            else if (jl_subtype(v.typ, typ)) {
                makeboxed = true;
            }
            else if (skip) {
                // undef
                *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
                return jl_cgval_t(ctx.builder.getContext());
            }
            else {
                // unreachable
                CreateTrap(ctx.builder);
                return jl_cgval_t(ctx.builder.getContext());
            }
        }
        else if (!v.isboxed) {
            makeboxed = true;
        }
        if (makeboxed) {
            // convert to a simple isboxed value
            return jl_cgval_t(boxed(ctx, v), NULL, true, typ, NULL, ctx.tbaa());
        }
    }
    return jl_cgval_t(v, typ, new_tindex);
}

orc::ThreadSafeModule jl_create_llvm_module(StringRef name, orc::ThreadSafeContext context, bool imaging_mode, const DataLayout &DL, const Triple &triple)
{
    ++ModulesCreated;
    auto lock = context.getLock();
    Module *m = new Module(name, *context.getContext());
    orc::ThreadSafeModule TSM(std::unique_ptr<Module>(m), std::move(context));
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
        m->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
    m->setDataLayout(DL);
    m->setTargetTriple(triple.str());

#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_) && JL_LLVM_VERSION >= 130000
    // tell Win32 to assume the stack is always 16-byte aligned,
    // and to ensure that it is 16-byte aligned for out-going calls,
    // to ensure compatibility with GCC codes
    m->setOverrideStackAlignment(16);
#endif
#if defined(JL_DEBUG_BUILD) && JL_LLVM_VERSION >= 130000
    m->setStackProtectorGuard("global");
#endif
    return TSM;
}

static void jl_init_function(Function *F)
{
    // set any attributes that *must* be set on all functions
#if defined(_OS_WINDOWS_) && !defined(_CPU_X86_64_)
    // tell Win32 to realign the stack to the next 16-byte boundary
    // upon entry to any function. This achieves compatibility
    // with both MinGW-GCC (which assumes an 16-byte-aligned stack) and
    // i686 Windows (which uses a 4-byte-aligned stack)
#if JL_LLVM_VERSION >= 140000
    AttrBuilder attr(F->getContext());
#else
    AttrBuilder attr;
#endif
    attr.addStackAlignmentAttr(16);
    F->addAttributes(AttributeList::FunctionIndex, attr);
#endif
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    F->setHasUWTable(); // force NeedsWinEH
#endif
#ifdef JL_DISABLE_FPO
    F->addFnAttr("frame-pointer", "all");
#endif
#if !defined(_COMPILER_ASAN_ENABLED_) && !defined(_OS_WINDOWS_)
    // ASAN won't like us accessing undefined memory causing spurious issues,
    // and Windows has platform-specific handling which causes it to mishandle
    // this annotation. Other platforms should just ignore this if they don't
    // implement it.
    F->addFnAttr("probe-stack", "inline-asm");
    //F->addFnAttr("stack-probe-size", 4096); // can use this to change the default
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
        if (jl_is_vararg(jl_tparam(sig, jl_nparams(sig) - 1)))
            return std::make_pair(false, false);
    }
    // not invalid, consider if specialized signature is worthwhile
    if (prefer_specsig)
        return std::make_pair(true, false);
    if (!deserves_retbox(rettype) && !jl_is_datatype_singleton((jl_datatype_t*)rettype) && rettype != (jl_value_t*)jl_bool_type)
        return std::make_pair(true, false);
    if (jl_is_uniontype(rettype)) {
        bool allunbox;
        size_t nbytes, align, min_align;
        union_alloca_type((jl_uniontype_t*)rettype, allunbox, nbytes, align, min_align);
        if (nbytes > 0)
            return std::make_pair(true, false); // some elements of the union could be returned unboxed avoiding allocation
    }
    if (jl_nparams(sig) <= 3) // few parameters == more efficient to pass directly
        return std::make_pair(true, false);
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

JL_DLLEXPORT void jl_coverage_alloc_line(StringRef filename, int line);
JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(StringRef filename, int line);
JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(StringRef filename, int line);

static void visitLine(jl_codectx_t &ctx, uint64_t *ptr, Value *addend, const char *name)
{
    Value *pv = ConstantExpr::getIntToPtr(
        ConstantInt::get(getSizeTy(ctx.builder.getContext()), (uintptr_t)ptr),
        getInt64PtrTy(ctx.builder.getContext()));
    Value *v = ctx.builder.CreateLoad(getInt64Ty(ctx.builder.getContext()), pv, true, name);
    v = ctx.builder.CreateAdd(v, addend);
    ctx.builder.CreateStore(v, pv, true); // volatile, not atomic, so this might be an underestimate,
                                          // but it's faster this way
}

// Code coverage

static void coverageVisitLine(jl_codectx_t &ctx, StringRef filename, int line)
{
    assert(!ctx.emission_context.imaging);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    visitLine(ctx, jl_coverage_data_pointer(filename, line), ConstantInt::get(getInt64Ty(ctx.builder.getContext()), 1), "lcnt");
}

// Memory allocation log (malloc_log)

static void mallocVisitLine(jl_codectx_t &ctx, StringRef filename, int line, Value *sync)
{
    assert(!ctx.emission_context.imaging);
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    Value *addend = sync
        ? ctx.builder.CreateCall(prepare_call(sync_gc_total_bytes_func), {sync})
        : ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    visitLine(ctx, jl_malloc_data_pointer(filename, line), addend, "bytecnt");
}

// --- constant determination ---

static void show_source_loc(jl_codectx_t &ctx, JL_STREAM *out)
{
    jl_printf(out, "in %s at %s", ctx.name, ctx.file.str().c_str());
}

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
    assert(nargs > 1);
    jl_value_t **v = (jl_value_t**)alloca(sizeof(jl_value_t*) * nargs);
    for (size_t i = 0; i < nargs; i++) {
        if (!args[i].constant)
            return NULL;
        v[i] = args[i].constant;
    }
    assert(v[0] == jl_builtin_apply_type);
    size_t last_age = jl_current_task->world_age;
    // call apply_type, but ignore errors. we know that will work in world 1.
    jl_current_task->world_age = 1;
    jl_value_t *result;
    JL_TRY {
        result = jl_apply(v, nargs);
    }
    JL_CATCH {
        result = NULL;
    }
    jl_current_task->world_age = last_age;
    return result;
}

// try to statically evaluate, NULL if not possible. note that this may allocate, and as
// such the resulting value should not be embedded directly in the generated code.
static jl_value_t *static_eval(jl_codectx_t &ctx, jl_value_t *ex)
{
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (jl_is_const(ctx.module, sym))
            return jl_get_global(ctx.module, sym);
        return NULL;
    }
    if (jl_is_slot(ex) || jl_is_argument(ex))
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
        if (e->head == jl_call_sym) {
            jl_value_t *f = static_eval(ctx, jl_exprarg(e, 0));
            if (f) {
                if (jl_array_dim0(e->args) == 3 && (f == jl_builtin_getfield || f == jl_builtin_getglobal)) {
                    m = (jl_module_t*)static_eval(ctx, jl_exprarg(e, 1));
                    // Check the tag before evaluating `s` so that a value of random
                    // type won't be corrupted.
                    if (!m || !jl_is_module(m))
                        return NULL;
                    // Assumes that the module is rooted somewhere.
                    s = (jl_sym_t*)static_eval(ctx, jl_exprarg(e, 2));
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
                    jl_value_t **v;
                    JL_GC_PUSHARGS(v, n+1);
                    v[0] = f;
                    for (i = 0; i < n; i++) {
                        v[i+1] = static_eval(ctx, jl_exprarg(e, i+1));
                        if (v[i+1] == NULL) {
                            JL_GC_POP();
                            return NULL;
                        }
                    }
                    size_t last_age = jl_current_task->world_age;
                    // here we know we're calling specific builtin functions that work in world 1.
                    jl_current_task->world_age = 1;
                    jl_value_t *result;
                    JL_TRY {
                        result = jl_apply(v, n+1);
                    }
                    JL_CATCH {
                        result = NULL;
                    }
                    jl_current_task->world_age = last_age;
                    JL_GC_POP();
                    return result;
                }
            }
        }
        else if (e->head == jl_static_parameter_sym) {
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
    return (jl_is_slot(e) || jl_is_argument(e)) && jl_slot_number(e)-1 == sl;
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
    else if (jl_is_returnnode(e)) {
        jl_value_t *retexpr = jl_returnnode_value(e);
        if (retexpr != NULL)
            return local_var_occurs(retexpr, sl);
    }
    else if (jl_is_gotoifnot(e)) {
        return local_var_occurs(jl_gotoifnot_cond(e), sl);
    }
    return false;
}

static std::set<int> assigned_in_try(jl_array_t *stmts, int s, long l)
{
    std::set<int> av;
    for(int i=s; i <= l; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == jl_assign_sym) {
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
            if (((jl_expr_t*)st)->head == jl_enter_sym) {
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

template <typename callback>
static void general_use_analysis(jl_codectx_t &ctx, jl_value_t *expr, callback &f)
{
    if (f(expr)) {
        return;
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == jl_method_sym) {
            general_use_analysis(ctx, jl_exprarg(e, 0), f);
            if (jl_expr_nargs(e) > 1) {
                general_use_analysis(ctx, jl_exprarg(e, 1), f);
                general_use_analysis(ctx, jl_exprarg(e, 2), f);
            }
        }
        else if (e->head == jl_assign_sym) {
            // don't consider assignment LHS as a variable "use"
            general_use_analysis(ctx, jl_exprarg(e, 1), f);
        }
        else {
            size_t i, elen = jl_array_dim0(e->args);
            for (i = 0; i < elen; i++) {
                general_use_analysis(ctx, jl_exprarg(e, i), f);
            }
        }
    }
    else if (jl_is_returnnode(expr)) {
        jl_value_t *retexpr = jl_returnnode_value(expr);
        if (retexpr != NULL)
            general_use_analysis(ctx, retexpr, f);
    }
    else if (jl_is_gotoifnot(expr)) {
        general_use_analysis(ctx, jl_gotoifnot_cond(expr), f);
    }
    else if (jl_is_pinode(expr)) {
        general_use_analysis(ctx, jl_fieldref_noalloc(expr, 0), f);
    }
    else if (jl_is_upsilonnode(expr)) {
        jl_value_t *val = jl_fieldref_noalloc(expr, 0);
        if (val)
            general_use_analysis(ctx, val, f);
    }
    else if (jl_is_phicnode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 0);
        size_t i, elen = jl_array_len(values);
        for (i = 0; i < elen; i++) {
            jl_value_t *v = jl_array_ptr_ref(values, i);
            general_use_analysis(ctx, v, f);
        }
    }
    else if (jl_is_phinode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 1);
        size_t i, elen = jl_array_len(values);
        for (i = 0; i < elen; i++) {
            jl_value_t *v = jl_array_ptr_ref(values, i);
            if (v)
                general_use_analysis(ctx, v, f);
        }
    }
}

static void simple_use_analysis(jl_codectx_t &ctx, jl_value_t *expr)
{
    auto scan_slot_arg = [&](jl_value_t *expr) {
        if (jl_is_slot(expr) || jl_is_argument(expr)) {
            int i = jl_slot_number(expr) - 1;
            ctx.slots[i].used = true;
            return true;
        }
        return false;
    };
    return general_use_analysis(ctx, expr, scan_slot_arg);
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

static jl_cgval_t emit_globalref(jl_codectx_t &ctx, jl_module_t *mod, jl_sym_t *name, AtomicOrdering order)
{
    jl_binding_t *bnd = NULL;
    Value *bp = global_binding_pointer(ctx, mod, name, &bnd, false);
    if (bp == NULL)
        return jl_cgval_t(ctx.builder.getContext());
    bp = julia_binding_pvalue(ctx, bp);
    if (bnd && bnd->value != NULL) {
        if (bnd->constp) {
            return mark_julia_const(ctx, bnd->value);
        }
        LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
        v->setOrdering(order);
        tbaa_decorate(ctx.tbaa().tbaa_binding, v);
        return mark_julia_type(ctx, v, true, bnd->ty);
    }
    // todo: use type info to avoid undef check
    return emit_checked_var(ctx, bp, name, false, ctx.tbaa().tbaa_binding);
}

static void emit_globalset(jl_codectx_t &ctx, jl_binding_t *bnd, Value *bp, const jl_cgval_t &rval_info, AtomicOrdering Order)
{
    Value *rval = boxed(ctx, rval_info);
    if (bnd && !bnd->constp && bnd->ty && jl_subtype(rval_info.typ, bnd->ty)) {
        StoreInst *v = ctx.builder.CreateAlignedStore(rval, julia_binding_pvalue(ctx, bp), Align(sizeof(void*)));
        v->setOrdering(Order);
        tbaa_decorate(ctx.tbaa().tbaa_binding, v);
        emit_write_barrier_binding(ctx, bp, rval);
    }
    else {
        ctx.builder.CreateCall(prepare_call(jlcheckassign_func), { bp, mark_callee_rooted(ctx, rval) });
    }
}

static Value *emit_box_compare(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2,
                               Value *nullcheck1, Value *nullcheck2)
{
    ++EmittedBoxCompares;
    if (jl_pointer_egal(arg1.typ) || jl_pointer_egal(arg2.typ)) {
        // if we can be certain we won't try to load from the pointer (because
        // we know boxed is trivial), we can skip the separate null checks
        // and just do the ICmpEQ test
        if (!arg1.TIndex && !arg2.TIndex)
            nullcheck1 = nullcheck2 = nullptr;
    }
    return emit_nullcheck_guard2(ctx, nullcheck1, nullcheck2, [&] {
        Value *varg1 = decay_derived(ctx, boxed(ctx, arg1));
        Value *varg2 = decay_derived(ctx, boxed(ctx, arg2));
        if (jl_pointer_egal(arg1.typ) || jl_pointer_egal(arg2.typ)) {
            return ctx.builder.CreateICmpEQ(varg1, varg2);
        }
        Value *neq = ctx.builder.CreateICmpNE(varg1, varg2);
        return emit_guarded_test(ctx, neq, true, [&] {
            Value *dtarg = emit_typeof_boxed(ctx, arg1);
            Value *dt_eq = ctx.builder.CreateICmpEQ(dtarg, emit_typeof_boxed(ctx, arg2));
            return emit_guarded_test(ctx, dt_eq, false, [&] {
                return ctx.builder.CreateTrunc(ctx.builder.CreateCall(prepare_call(jlegalx_func),
                                                                      {varg1, varg2, dtarg}), getInt1Ty(ctx.builder.getContext()));
            });
        });
    });
}

static Value *emit_bits_compare(jl_codectx_t &ctx, jl_cgval_t arg1, jl_cgval_t arg2);

static Value *emit_bitsunion_compare(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2)
{
    ++EmittedBitsUnionCompares;
    assert(jl_egal(arg1.typ, arg2.typ) && arg1.TIndex && arg2.TIndex && jl_is_uniontype(arg1.typ) && "unimplemented");
    Value *tindex = arg1.TIndex;
    tindex = ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
    Value *tindex2 = arg2.TIndex;
    tindex2 = ctx.builder.CreateAnd(tindex2, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
    Value *typeeq = ctx.builder.CreateICmpEQ(tindex, tindex2);
    tindex = ctx.builder.CreateSelect(typeeq, tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x00));
    BasicBlock *defaultBB = BasicBlock::Create(ctx.builder.getContext(), "unionbits_is_boxed", ctx.f);
    SwitchInst *switchInst = ctx.builder.CreateSwitch(tindex, defaultBB);
    BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_unionbits_is", ctx.f);
    ctx.builder.SetInsertPoint(postBB);
    PHINode *phi = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), 2);
    switchInst->addCase(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0), postBB);
    phi->addIncoming(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0), switchInst->getParent());
    unsigned counter = 0;
    bool allunboxed = for_each_uniontype_small(
        [&](unsigned idx, jl_datatype_t *jt) {
            BasicBlock *tempBB = BasicBlock::Create(ctx.builder.getContext(), "unionbits_is", ctx.f);
            ctx.builder.SetInsertPoint(tempBB);
            switchInst->addCase(ConstantInt::get(getInt8Ty(ctx.builder.getContext()), idx), tempBB);
            jl_cgval_t sel_arg1(arg1, (jl_value_t*)jt, NULL);
            jl_cgval_t sel_arg2(arg2, (jl_value_t*)jt, NULL);
            Value *cmp = emit_bits_compare(ctx, sel_arg1, sel_arg2);
            tempBB = ctx.builder.GetInsertBlock(); // could have changed
            phi->addIncoming(cmp, tempBB);
            ctx.builder.CreateBr(postBB);
        },
        arg1.typ,
        counter);
    assert(allunboxed); (void)allunboxed;
    ctx.builder.SetInsertPoint(defaultBB);
    Function *trap_func = Intrinsic::getDeclaration(
        ctx.f->getParent(),
        Intrinsic::trap);
    ctx.builder.CreateCall(trap_func);
    ctx.builder.CreateUnreachable();
    ctx.builder.SetInsertPoint(postBB);
    return phi;
}

static Value *emit_bits_compare(jl_codectx_t &ctx, jl_cgval_t arg1, jl_cgval_t arg2)
{
    ++EmittedBitsCompares;
    bool isboxed;
    Type *at = julia_type_to_llvm(ctx, arg1.typ, &isboxed);
    assert(jl_is_datatype(arg1.typ) && arg1.typ == arg2.typ && !isboxed);

    if (type_is_ghost(at))
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);

    if (at->isIntegerTy() || at->isPointerTy() || at->isFloatingPointTy()) {
        Type *at_int = INTT(at);
        Value *varg1 = emit_unbox(ctx, at_int, arg1, arg1.typ);
        Value *varg2 = emit_unbox(ctx, at_int, arg2, arg2.typ);
        return ctx.builder.CreateICmpEQ(varg1, varg2);
    }

    if (at->isVectorTy()) {
        jl_svec_t *types = ((jl_datatype_t*)arg1.typ)->types;
        Value *answer = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
        Value *varg1 = emit_unbox(ctx, at, arg1, arg1.typ);
        Value *varg2 = emit_unbox(ctx, at, arg2, arg2.typ);
        for (size_t i = 0, l = jl_svec_len(types); i < l; i++) {
            jl_value_t *fldty = jl_svecref(types, i);
            Value *subAns, *fld1, *fld2;
            fld1 = ctx.builder.CreateExtractElement(varg1, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), i)),
            fld2 = ctx.builder.CreateExtractElement(varg2, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), i)),
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
        if (sz > 512 && !sty->layout->haspadding) {
            Value *varg1 = arg1.ispointer() ? maybe_decay_tracked(ctx, data_pointer(ctx, arg1)) :
                value_to_pointer(ctx, arg1).V;
            Value *varg2 = arg2.ispointer() ? maybe_decay_tracked(ctx, data_pointer(ctx, arg2)) :
                value_to_pointer(ctx, arg2).V;
            varg1 = emit_pointer_from_objref(ctx, varg1);
            varg2 = emit_pointer_from_objref(ctx, varg2);
            Value *gc_uses[2];
            int nroots = 0;
            if ((gc_uses[nroots] = get_gc_root_for(arg1)))
                nroots++;
            if ((gc_uses[nroots] = get_gc_root_for(arg2)))
                nroots++;
            OperandBundleDef OpBundle("jl_roots", makeArrayRef(gc_uses, nroots));
            auto answer = ctx.builder.CreateCall(prepare_call(memcmp_func), {
                        ctx.builder.CreateBitCast(varg1, getInt8PtrTy(ctx.builder.getContext())),
                        ctx.builder.CreateBitCast(varg2, getInt8PtrTy(ctx.builder.getContext())),
                        ConstantInt::get(getSizeTy(ctx.builder.getContext()), sz) },
                    ArrayRef<OperandBundleDef>(&OpBundle, nroots ? 1 : 0));
            MDNode *tbaa = nullptr;
            if (!arg1.tbaa) {
                tbaa = arg2.tbaa;
            }
            else if (!arg2.tbaa) {
                tbaa = arg1.tbaa;
            }
            else {
                tbaa = MDNode::getMostGenericTBAA(arg1.tbaa, arg2.tbaa);
            }
            if (tbaa)
                tbaa_decorate(tbaa, answer);
            return ctx.builder.CreateICmpEQ(answer, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0));
        }
        else {
            jl_svec_t *types = sty->types;
            Value *answer = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
            for (size_t i = 0, l = jl_svec_len(types); i < l; i++) {
                jl_value_t *fldty = jl_svecref(types, i);
                if (type_is_ghost(julia_type_to_llvm(ctx, fldty)))
                    continue;
                Value *nullcheck1 = nullptr;
                Value *nullcheck2 = nullptr;
                auto fld1 = emit_getfield_knownidx(ctx, arg1, i, sty, jl_memory_order_notatomic, &nullcheck1);
                auto fld2 = emit_getfield_knownidx(ctx, arg2, i, sty, jl_memory_order_notatomic, &nullcheck2);
                Value *fld_answer;
                if (jl_field_isptr(sty, i) && jl_is_concrete_immutable(fldty)) {
                    // concrete immutables that are !isinlinealloc might be reference cycles
                    // issue #37872
                    fld_answer = emit_box_compare(ctx, fld1, fld2, nullcheck1, nullcheck2);
                }
                else {
                    fld_answer = emit_f_is(ctx, fld1, fld2, nullcheck1, nullcheck2);
                }
                answer = ctx.builder.CreateAnd(answer, fld_answer);
            }
            return answer;
        }
    }
    assert(0 && "what is this llvm type?");
    abort();
}

// emit code for is (===).
// If either `nullcheck1` or `nullcheck2` are non-NULL, they are pointer values
// representing the undef-ness of `arg1` and `arg2`.
// This can only happen when comparing two fields of the same time and the result should be
// true if both are NULL
// Like the runtime counterpart, this is codegen guaranteed to be non-allocating and to exclude safepoints
static Value *emit_f_is(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2,
                        Value *nullcheck1, Value *nullcheck2)
{
    ++EmittedEgals;
    // handle simple static expressions with no side-effects
    if (arg1.constant && arg2.constant)
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), jl_egal(arg1.constant, arg2.constant));

    jl_value_t *rt1 = arg1.typ;
    jl_value_t *rt2 = arg2.typ;
    if (jl_is_concrete_type(rt1) && jl_is_concrete_type(rt2) && !jl_is_kind(rt1) && !jl_is_kind(rt2) && rt1 != rt2) {
        // disjoint concrete leaf types are never equal (quick test)
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
    }

    if (arg1.isghost || arg2.isghost || arg1.constant == jl_bottom_type ||
        arg2.constant == jl_bottom_type) {
        // comparing to a singleton object, special case for value `jl_bottom_type`
        // since it is normalized to `::Type{Union{}}` instead...
        if (arg1.TIndex)
            return emit_nullcheck_guard(ctx, nullcheck1, [&] {
                return emit_exactly_isa(ctx, arg1, rt2); // rt2 is a singleton type
            });
        if (arg2.TIndex)
            return emit_nullcheck_guard(ctx, nullcheck2, [&] {
                return emit_exactly_isa(ctx, arg2, rt1); // rt1 is a singleton type
            });
        if (!(arg1.isboxed || arg1.constant) || !(arg2.isboxed || arg2.constant))
            // not TIndex && not boxed implies it is an unboxed value of a different type from this singleton
            // (which was probably caught above, but just to be safe, we repeat it here explicitly)
            return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
        Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : maybe_bitcast(ctx, arg1.Vboxed, ctx.types().T_pjlvalue);
        Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : maybe_bitcast(ctx, arg2.Vboxed, ctx.types().T_pjlvalue);
        // rooting these values isn't needed since we won't load this pointer
        // and we know at least one of them is a unique Singleton
        // which is already enough to ensure pointer uniqueness for this test
        // even if the other pointer managed to get garbage collected
        // TODO: use emit_pointer_from_objref instead, per comment above
        return ctx.builder.CreateICmpEQ(decay_derived(ctx, varg1), decay_derived(ctx, varg2));
    }

    if (jl_type_intersection(rt1, rt2) == (jl_value_t*)jl_bottom_type) // types are disjoint (exhaustive test)
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);

    bool justbits1 = jl_is_concrete_immutable(rt1);
    bool justbits2 = jl_is_concrete_immutable(rt2);
    if (justbits1 || justbits2) { // whether this type is unique'd by value
        return emit_nullcheck_guard2(ctx, nullcheck1, nullcheck2, [&] () -> Value* {
            jl_value_t *typ = justbits1 ? rt1 : rt2;
            if (typ == (jl_value_t*)jl_bool_type) { // aka jl_pointer_egal
                // some optimizations for bool, since pointer comparison may be better
                if ((arg1.isboxed || arg1.constant) && (arg2.isboxed || arg2.constant)) { // aka have-fast-pointer
                    Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : maybe_bitcast(ctx, arg1.Vboxed, ctx.types().T_pjlvalue);
                    Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : maybe_bitcast(ctx, arg2.Vboxed, ctx.types().T_pjlvalue);
                    return ctx.builder.CreateICmpEQ(decay_derived(ctx, varg1), decay_derived(ctx, varg2));
                }
            }
            if (rt1 == rt2)
                return emit_bits_compare(ctx, arg1, arg2);
            Value *same_type = emit_exactly_isa(ctx, (typ == rt2 ? arg1 : arg2), typ);
            BasicBlock *currBB = ctx.builder.GetInsertBlock();
            BasicBlock *isaBB = BasicBlock::Create(ctx.builder.getContext(), "is", ctx.f);
            BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_is", ctx.f);
            ctx.builder.CreateCondBr(same_type, isaBB, postBB);
            ctx.builder.SetInsertPoint(isaBB);
            Value *bitcmp = emit_bits_compare(ctx, jl_cgval_t(arg1, typ, NULL),
                                              jl_cgval_t(arg2, typ, NULL));
            isaBB = ctx.builder.GetInsertBlock(); // might have changed
            ctx.builder.CreateBr(postBB);
            ctx.builder.SetInsertPoint(postBB);
            PHINode *cmp = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), 2);
            cmp->addIncoming(ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0), currBB);
            cmp->addIncoming(bitcmp, isaBB);
            return cmp;
        });
    }

    // TODO: handle the case where arg1.typ is not exactly arg2.typ, or when
    // one of these isn't union, or when the union can be pointer
    if (arg1.TIndex && arg2.TIndex && jl_egal(arg1.typ, arg2.typ) &&
        jl_is_uniontype(arg1.typ) && is_uniontype_allunboxed(arg1.typ))
        return emit_nullcheck_guard2(ctx, nullcheck1, nullcheck2, [&] {
            return emit_bitsunion_compare(ctx, arg1, arg2);
        });

    return emit_box_compare(ctx, arg1, arg2, nullcheck1, nullcheck2);
}

static bool emit_f_opglobal(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                            const jl_cgval_t *argv, size_t nargs, const jl_cgval_t *modifyop)
{
    const jl_cgval_t &mod = argv[1];
    const jl_cgval_t &sym = argv[2];
    const jl_cgval_t &val = argv[3];
    enum jl_memory_order order = jl_memory_order_unspecified;

    if (nargs == 4) {
        const jl_cgval_t &arg4 = argv[4];
        if (arg4.constant && jl_is_symbol(arg4.constant))
            order = jl_get_atomic_order((jl_sym_t*)arg4.constant, false, true);
        else
            return false;
    }
    else
        order = jl_memory_order_monotonic;

    if (order == jl_memory_order_invalid || order == jl_memory_order_notatomic) {
        emit_atomic_error(ctx, order == jl_memory_order_invalid ? "invalid atomic ordering" : "setglobal!: module binding cannot be written non-atomically");
        *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
        return true;
    }

    if (sym.constant && jl_is_symbol(sym.constant)) {
        jl_sym_t *name = (jl_sym_t*)sym.constant;
        if (mod.constant && jl_is_module(mod.constant)) {
            jl_binding_t *bnd = NULL;
            Value *bp = global_binding_pointer(ctx, (jl_module_t*)mod.constant, name, &bnd, true);
            if (bp) {
                emit_globalset(ctx, bnd, bp, val, get_llvm_atomic_order(order));
                *ret = val;
            }
            else {
                *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            }
            return true;
        }
    }

    return false;
}

static bool emit_f_opfield(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                           const jl_cgval_t *argv, size_t nargs, const jl_cgval_t *modifyop)
{
    ++EmittedOpfields;
    bool issetfield = f == jl_builtin_setfield;
    bool isreplacefield = f == jl_builtin_replacefield;
    bool isswapfield = f == jl_builtin_swapfield;
    bool ismodifyfield = f == jl_builtin_modifyfield;
    const jl_cgval_t undefval(ctx.builder.getContext());
    const jl_cgval_t &obj = argv[1];
    const jl_cgval_t &fld = argv[2];
    jl_cgval_t val = argv[isreplacefield || ismodifyfield ? 4 : 3];
    const jl_cgval_t &cmp = isreplacefield || ismodifyfield ? argv[3] : undefval;
    enum jl_memory_order order = jl_memory_order_notatomic;
    const std::string fname = issetfield ? "setfield!" : isreplacefield ? "replacefield!" : isswapfield ? "swapfield!" : "modifyfield!";
    if (nargs >= (isreplacefield || ismodifyfield ? 5 : 4)) {
        const jl_cgval_t &ord = argv[isreplacefield || ismodifyfield ? 5 : 4];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        order = jl_get_atomic_order((jl_sym_t*)ord.constant, !issetfield, true);
    }
    enum jl_memory_order fail_order = order;
    if (isreplacefield && nargs == 6) {
        const jl_cgval_t &ord = argv[6];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        fail_order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
    }
    if (order == jl_memory_order_invalid || fail_order == jl_memory_order_invalid || fail_order > order) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
        return true;
    }

    jl_datatype_t *uty = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
    if (jl_is_datatype(uty) && jl_struct_try_layout(uty)) {
        ssize_t idx = -1;
        if (fld.constant && jl_is_symbol(fld.constant)) {
            idx = jl_field_index(uty, (jl_sym_t*)fld.constant, 0);
        }
        else if (fld.constant && fld.typ == (jl_value_t*)jl_long_type) {
            ssize_t i = jl_unbox_long(fld.constant);
            if (i > 0 && i <= jl_datatype_nfields(uty))
                idx = i - 1;
        }
        if (idx != -1) {
            jl_value_t *ft = jl_field_type(uty, idx);
            if (!jl_has_free_typevars(ft)) {
                if (!ismodifyfield && !jl_subtype(val.typ, ft)) {
                    emit_typecheck(ctx, val, ft, fname);
                    val = update_julia_type(ctx, val, ft);
                }
                // TODO: attempt better codegen for approximate types
                bool isboxed = jl_field_isptr(uty, idx);
                bool isatomic = jl_field_isatomic(uty, idx);
                bool needlock = isatomic && !isboxed && jl_datatype_size(jl_field_type(uty, idx)) > MAX_ATOMIC_SIZE;
                *ret = jl_cgval_t(ctx.builder.getContext());
                if (isatomic == (order == jl_memory_order_notatomic)) {
                    emit_atomic_error(ctx,
                            issetfield ?
                            (isatomic ? "setfield!: atomic field cannot be written non-atomically"
                                      : "setfield!: non-atomic field cannot be written atomically") :
                            isreplacefield ?
                            (isatomic ? "replacefield!: atomic field cannot be written non-atomically"
                                      : "replacefield!: non-atomic field cannot be written atomically") :
                            isswapfield ?
                            (isatomic ? "swapfield!: atomic field cannot be written non-atomically"
                                      : "swapfield!: non-atomic field cannot be written atomically") :
                            (isatomic ? "modifyfield!: atomic field cannot be written non-atomically"
                                      : "modifyfield!: non-atomic field cannot be written atomically"));
                }
                else if (isatomic == (fail_order == jl_memory_order_notatomic)) {
                    emit_atomic_error(ctx,
                            (isatomic ? "replacefield!: atomic field cannot be accessed non-atomically"
                                      : "replacefield!: non-atomic field cannot be accessed atomically"));
                }
                else if (!uty->name->mutabl) {
                    std::string msg = fname + ": immutable struct of type "
                        + std::string(jl_symbol_name(uty->name->name))
                        + " cannot be changed";
                    emit_error(ctx, msg);
                }
                else if (jl_field_isconst(uty, idx)) {
                    std::string msg = fname + ": const field ."
                        + std::string(jl_symbol_name((jl_sym_t*)jl_svec_ref(jl_field_names(uty), idx)))
                        + " of type "
                        + std::string(jl_symbol_name(uty->name->name))
                        + " cannot be changed";
                    emit_error(ctx, msg);
                }
                else {
                    *ret = emit_setfield(ctx, uty, obj, idx, val, cmp, true,
                            (needlock || order <= jl_memory_order_notatomic)
                            ? (isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic) // TODO: we should do this for anything with CountTrackedPointers(elty).count > 0
                            : get_llvm_atomic_order(order),
                            (needlock || fail_order <= jl_memory_order_notatomic)
                            ? (isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic) // TODO: we should do this for anything with CountTrackedPointers(elty).count > 0
                            : get_llvm_atomic_order(fail_order),
                            needlock, issetfield, isreplacefield, isswapfield, ismodifyfield,
                            modifyop, fname);
                }
                return true;
            }
        }
    }
    return false;
}

static jl_llvm_functions_t
    emit_function(
        orc::ThreadSafeModule &TSM,
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params);

static bool emit_builtin_call(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                              const jl_cgval_t *argv, size_t nargs, jl_value_t *rt,
                              jl_expr_t *ex, bool is_promotable)
// returns true if the call has been handled
{
    ++EmittedBuiltinCalls;
    if (f == jl_builtin_is && nargs == 2) {
        // emit comparison test
        Value *ans = emit_f_is(ctx, argv[1], argv[2]);
        *ret = mark_julia_type(ctx, ctx.builder.CreateZExt(ans, getInt8Ty(ctx.builder.getContext())), false, jl_bool_type);
        return true;
    }

    else if (f == jl_builtin_typeof && nargs == 1) {
        *ret = emit_typeof(ctx, argv[1], false);
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
            if (isa_result->getType() == getInt1Ty(ctx.builder.getContext()))
                isa_result = ctx.builder.CreateZExt(isa_result, getInt8Ty(ctx.builder.getContext()));
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
            *ret = mark_julia_type(ctx, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), issub), false, jl_bool_type);
            return true;
        }
    }

    else if ((f == jl_builtin__apply_iterate && nargs == 3) && ctx.vaSlot > 0) {
        // turn Core._apply_iterate(iter, f, Tuple) ==> f(Tuple...) using the jlcall calling convention if Tuple is the va allocation
        if (LoadInst *load = dyn_cast_or_null<LoadInst>(argv[3].V)) {
            if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot && ctx.argArray) {
                Value *theF = boxed(ctx, argv[2]);
                Value *nva = emit_n_varargs(ctx);
#ifdef _P64
                nva = ctx.builder.CreateTrunc(nva, getInt32Ty(ctx.builder.getContext()));
#endif
                Value *theArgs = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, ctx.argArray, ConstantInt::get(getSizeTy(ctx.builder.getContext()), ctx.nReqArgs));
                Value *r = ctx.builder.CreateCall(prepare_call(jlapplygeneric_func), { theF, theArgs, nva });
                *ret = mark_julia_type(ctx, r, true, jl_any_type);
                return true;
            }
        }
    }

    else if (f == jl_builtin_tuple) {
        if (nargs == 0) {
            *ret = ghostValue(ctx, jl_emptytuple_type);
            return true;
        }
        if (jl_is_tuple_type(rt) && jl_is_concrete_type(rt) && nargs == jl_datatype_nfields(rt)) {
            *ret = emit_new_struct(ctx, rt, nargs, &argv[1], is_promotable);
            return true;
        }
    }

    else if (f == jl_builtin_throw && nargs == 1) {
        Value *arg1 = boxed(ctx, argv[1]);
        raise_exception(ctx, arg1);
        *ret = jl_cgval_t(ctx.builder.getContext());
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
                        *ret = mark_julia_type(ctx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), 1), false, jl_long_type);
                        return true;
                    }
                }
                else {
                    Value *idx_dyn = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), idx, (jl_value_t*)jl_long_type);
                    error_unless(ctx, ctx.builder.CreateICmpSGT(idx_dyn, Constant::getNullValue(getSizeTy(ctx.builder.getContext()))),
                                 "arraysize: dimension out of range");
                    BasicBlock *outBB = BasicBlock::Create(ctx.builder.getContext(), "outofrange", ctx.f);
                    BasicBlock *inBB = BasicBlock::Create(ctx.builder.getContext(), "inrange");
                    BasicBlock *ansBB = BasicBlock::Create(ctx.builder.getContext(), "arraysize");
                    ctx.builder.CreateCondBr(ctx.builder.CreateICmpSLE(idx_dyn,
                                ConstantInt::get(getSizeTy(ctx.builder.getContext()), ndims)),
                            inBB, outBB);
                    ctx.builder.SetInsertPoint(outBB);
                    Value *v_one = ConstantInt::get(getSizeTy(ctx.builder.getContext()), 1);
                    ctx.builder.CreateBr(ansBB);
                    ctx.f->getBasicBlockList().push_back(inBB);
                    ctx.builder.SetInsertPoint(inBB);
                    Value *v_sz = emit_arraysize(ctx, ary, idx_dyn);
                    ctx.builder.CreateBr(ansBB);
                    inBB = ctx.builder.GetInsertBlock(); // could have changed
                    ctx.f->getBasicBlockList().push_back(ansBB);
                    ctx.builder.SetInsertPoint(ansBB);
                    PHINode *result = ctx.builder.CreatePHI(getSizeTy(ctx.builder.getContext()), 2);
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
                emit_typecheck(ctx, argv[1], (jl_value_t*)jl_bool_type, "arrayref");
                Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[3], nargs - 2, boundscheck);
                if (!isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
                    assert(((jl_datatype_t*)ety)->instance != NULL);
                    *ret = ghostValue(ctx, ety);
                }
                else if (!isboxed && jl_is_uniontype(ety)) {
                    Value *data = emit_arrayptr(ctx, ary, ary_ex);
                    Value *offset = emit_arrayoffset(ctx, ary, nd);
                    Value *ptindex;
                    if (elsz == 0) {
                        ptindex = data;
                    }
                    else {
                        Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * al), (elsz + al - 1) / al);
                        data = emit_bitcast(ctx, data, AT->getPointerTo());
                        // isbits union selector bytes are stored after a->maxsize
                        Value *ndims = (nd == -1 ? emit_arrayndims(ctx, ary) : ConstantInt::get(getInt16Ty(ctx.builder.getContext()), nd));
                        Value *is_vector = ctx.builder.CreateICmpEQ(ndims, ConstantInt::get(getInt16Ty(ctx.builder.getContext()), 1));
                        Value *selidx_v = ctx.builder.CreateSub(emit_vectormaxsize(ctx, ary), ctx.builder.CreateZExt(offset, getSizeTy(ctx.builder.getContext())));
                        Value *selidx_m = emit_arraylen(ctx, ary);
                        Value *selidx = ctx.builder.CreateSelect(is_vector, selidx_v, selidx_m);
                        ptindex = ctx.builder.CreateInBoundsGEP(AT, data, selidx);
                        data = ctx.builder.CreateInBoundsGEP(AT, data, idx);
                    }
                    ptindex = emit_bitcast(ctx, ptindex, getInt8PtrTy(ctx.builder.getContext()));
                    ptindex = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptindex, offset);
                    ptindex = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptindex, idx);
                    *ret = emit_unionload(ctx, data, ptindex, ety, elsz, al, ctx.tbaa().tbaa_arraybuf, true, union_max, ctx.tbaa().tbaa_arrayselbyte);
                }
                else {
                    MDNode *aliasscope = (f == jl_builtin_const_arrayref) ? ctx.aliasscope : nullptr;
                    *ret = typed_load(ctx,
                            emit_arrayptr(ctx, ary, ary_ex),
                            idx, ety,
                            isboxed ? ctx.tbaa().tbaa_ptrarraybuf : ctx.tbaa().tbaa_arraybuf,
                            aliasscope,
                            isboxed,
                            AtomicOrdering::NotAtomic);
                }
                return true;
            }
        }
    }

    else if (f == jl_builtin_arrayset && nargs >= 4) {
        const jl_cgval_t &ary = argv[2];
        jl_cgval_t val = argv[3];
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
                if (!jl_subtype(val.typ, ety)) {
                    emit_typecheck(ctx, val, ety, "arrayset");
                    val = update_julia_type(ctx, val, ety);
                }
                size_t elsz = 0, al = 0;
                int union_max = jl_islayout_inline(ety, &elsz, &al);
                bool isboxed = (union_max == 0);
                if (isboxed)
                    ety = (jl_value_t*)jl_any_type;
                jl_value_t *ary_ex = jl_exprarg(ex, 2);
                ssize_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : -1;
                jl_value_t *boundscheck = argv[1].constant;
                emit_typecheck(ctx, argv[1], (jl_value_t*)jl_bool_type, "arrayset");
                Value *idx = emit_array_nd_index(ctx, ary, ary_ex, nd, &argv[4], nargs - 3, boundscheck);
                if (!isboxed && jl_is_datatype(ety) && jl_datatype_size(ety) == 0) {
                    // no-op
                }
                else {
                    PHINode *data_owner = NULL; // owner object against which the write barrier must check
                    if (isboxed || (jl_is_datatype(ety) && ((jl_datatype_t*)ety)->layout->npointers > 0)) { // if elements are just bits, don't need a write barrier
                        Value *aryv = boxed(ctx, ary);
                        Value *flags = emit_arrayflags(ctx, ary);
                        // the owner of the data is ary itself except if ary->how == 3
                        flags = ctx.builder.CreateAnd(flags, 3);
                        Value *is_owned = ctx.builder.CreateICmpEQ(flags, ConstantInt::get(getInt16Ty(ctx.builder.getContext()), 3));
                        BasicBlock *curBB = ctx.builder.GetInsertBlock();
                        BasicBlock *ownedBB = BasicBlock::Create(ctx.builder.getContext(), "array_owned", ctx.f);
                        BasicBlock *mergeBB = BasicBlock::Create(ctx.builder.getContext(), "merge_own", ctx.f);
                        ctx.builder.CreateCondBr(is_owned, ownedBB, mergeBB);
                        ctx.builder.SetInsertPoint(ownedBB);
                        // load owner pointer
                        Instruction *own_ptr;
                        if (jl_is_long(ndp)) {
                            own_ptr = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                                    ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue,
                                        emit_bitcast(ctx, decay_derived(ctx, aryv), ctx.types().T_pprjlvalue),
                                        jl_array_data_owner_offset(nd) / sizeof(jl_value_t*)),
                                    Align(sizeof(void*)));
                            tbaa_decorate(ctx.tbaa().tbaa_const, maybe_mark_load_dereferenceable(own_ptr, false, (jl_value_t*)jl_array_any_type));
                        }
                        else {
                            own_ptr = ctx.builder.CreateCall(
                                prepare_call(jlarray_data_owner_func),
                                {aryv});
                        }
                        ctx.builder.CreateBr(mergeBB);
                        ctx.builder.SetInsertPoint(mergeBB);
                        data_owner = ctx.builder.CreatePHI(ctx.types().T_prjlvalue, 2);
                        data_owner->addIncoming(aryv, curBB);
                        data_owner->addIncoming(own_ptr, ownedBB);
                    }
                    if (!isboxed && jl_is_uniontype(ety)) {
                        Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * al), (elsz + al - 1) / al);
                        Value *data = emit_bitcast(ctx, emit_arrayptr(ctx, ary, ary_ex), AT->getPointerTo());
                        Value *offset = emit_arrayoffset(ctx, ary, nd);
                        // compute tindex from val
                        jl_cgval_t rhs_union = convert_julia_type(ctx, val, ety);
                        Value *tindex = compute_tindex_unboxed(ctx, rhs_union, ety);
                        tindex = ctx.builder.CreateNUWSub(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 1));
                        Value *ptindex;
                        if (elsz == 0) {
                            ptindex = data;
                        }
                        else {
                            Value *ndims = (nd == -1 ? emit_arrayndims(ctx, ary) : ConstantInt::get(getInt16Ty(ctx.builder.getContext()), nd));
                            Value *is_vector = ctx.builder.CreateICmpEQ(ndims, ConstantInt::get(getInt16Ty(ctx.builder.getContext()), 1));
                            Value *selidx_v = ctx.builder.CreateSub(emit_vectormaxsize(ctx, ary), ctx.builder.CreateZExt(offset, getSizeTy(ctx.builder.getContext())));
                            Value *selidx_m = emit_arraylen(ctx, ary);
                            Value *selidx = ctx.builder.CreateSelect(is_vector, selidx_v, selidx_m);
                            ptindex = ctx.builder.CreateInBoundsGEP(AT, data, selidx);
                            data = ctx.builder.CreateInBoundsGEP(AT, data, idx);
                        }
                        ptindex = emit_bitcast(ctx, ptindex, getInt8PtrTy(ctx.builder.getContext()));
                        ptindex = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptindex, offset);
                        ptindex = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), ptindex, idx);
                        tbaa_decorate(ctx.tbaa().tbaa_arrayselbyte, ctx.builder.CreateStore(tindex, ptindex));
                        if (elsz > 0 && (!jl_is_datatype(val.typ) || jl_datatype_size(val.typ) > 0)) {
                            // copy data (if any)
                            emit_unionmove(ctx, data, ctx.tbaa().tbaa_arraybuf, val, nullptr);
                        }
                    }
                    else {
                        typed_store(ctx,
                                    emit_arrayptr(ctx, ary, ary_ex, isboxed),
                                    idx, val, jl_cgval_t(ctx.builder.getContext()), ety,
                                    isboxed ? ctx.tbaa().tbaa_ptrarraybuf : ctx.tbaa().tbaa_arraybuf,
                                    ctx.aliasscope,
                                    data_owner,
                                    isboxed,
                                    isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic, // TODO: we should do this for anything with CountTrackedPointers(elty).count > 0
                                    isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic, // TODO: we should do this for anything with CountTrackedPointers(elty).count > 0
                                    0,
                                    false,
                                    true,
                                    false,
                                    false,
                                    false,
                                    false,
                                    nullptr,
                                    "");
                    }
                }
                *ret = ary;
                return true;
            }
        }
    }

    else if (f == jl_builtin_getfield && (nargs == 2 || nargs == 3 || nargs == 4)) {
        const jl_cgval_t &obj = argv[1];
        const jl_cgval_t &fld = argv[2];
        enum jl_memory_order order = jl_memory_order_unspecified;
        jl_value_t *boundscheck = jl_true;

        if (nargs == 4) {
            const jl_cgval_t &ord = argv[3];
            const jl_cgval_t &inb = argv[4];
            emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, "getfield");
            emit_typecheck(ctx, inb, (jl_value_t*)jl_bool_type, "getfield");
            if (!ord.constant)
                return false;
            order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
            if (inb.constant == jl_false)
                boundscheck = jl_false;
        }
        else if (nargs == 3) {
            const jl_cgval_t &arg3 = argv[3];
            if (arg3.constant && jl_is_symbol(arg3.constant))
                order = jl_get_atomic_order((jl_sym_t*)arg3.constant, true, false);
            else if (arg3.constant == jl_false)
                boundscheck = jl_false;
            else if (arg3.typ != (jl_value_t*)jl_bool_type)
                return false;
        }
        if (order == jl_memory_order_invalid) {
            emit_atomic_error(ctx, "invalid atomic ordering");
            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            return true;
        }

        jl_datatype_t *utt = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
        if (jl_is_type_type((jl_value_t*)utt) && jl_is_concrete_type(jl_tparam0(utt)))
            utt = (jl_datatype_t*)jl_typeof(jl_tparam0(utt));

        if (fld.constant && jl_is_symbol(fld.constant)) {
            jl_sym_t *name = (jl_sym_t*)fld.constant;
            if (obj.constant && jl_is_module(obj.constant)) {
                *ret = emit_globalref(ctx, (jl_module_t*)obj.constant, name, order == jl_memory_order_unspecified ? AtomicOrdering::Unordered : get_llvm_atomic_order(order));
                return true;
            }

            if (jl_is_datatype(utt) && jl_struct_try_layout(utt)) {
                ssize_t idx = jl_field_index(utt, name, 0);
                if (idx != -1 && !jl_has_free_typevars(jl_field_type(utt, idx))) {
                    *ret = emit_getfield_knownidx(ctx, obj, idx, utt, order);
                    return true;
                }
            }
        }
        else if (fld.typ == (jl_value_t*)jl_long_type) {
            if (ctx.vaSlot > 0) {
                // optimize VA tuple
                if (LoadInst *load = dyn_cast_or_null<LoadInst>(obj.V)) {
                    if (load->getPointerOperand() == ctx.slots[ctx.vaSlot].boxroot && ctx.argArray) {
                        Value *valen = emit_n_varargs(ctx);
                        jl_cgval_t va_ary( // fake instantiation of a cgval, in order to call emit_bounds_check
                                ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, ctx.argArray, ConstantInt::get(getSizeTy(ctx.builder.getContext()), ctx.nReqArgs)),
                                NULL, false, NULL, NULL, ctx.tbaa());
                        Value *idx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), fld, (jl_value_t*)jl_long_type);
                        idx = emit_bounds_check(ctx, va_ary, NULL, idx, valen, boundscheck);
                        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), ctx.nReqArgs));
                        Instruction *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, ctx.argArray, idx), Align(sizeof(void*)));
                        // if we know the result type of this load, we will mark that information here too
                        tbaa_decorate(ctx.tbaa().tbaa_value, maybe_mark_load_dereferenceable(v, false, rt));
                        *ret = mark_julia_type(ctx, v, /*boxed*/ true, rt);
                        return true;
                    }
                }
            }

            if (jl_is_datatype(utt)) {
                if (jl_struct_try_layout(utt)) {
                    size_t nfields = jl_datatype_nfields(utt);
                    // integer index
                    size_t idx;
                    if (fld.constant && (idx = jl_unbox_long(fld.constant) - 1) < nfields) {
                        if (!jl_has_free_typevars(jl_field_type(utt, idx))) {
                            // known index
                            *ret = emit_getfield_knownidx(ctx, obj, idx, utt, order);
                            return true;
                        }
                    }
                    else {
                        // unknown index
                        Value *vidx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), fld, (jl_value_t*)jl_long_type);
                        if (emit_getfield_unknownidx(ctx, ret, obj, vidx, utt, boundscheck, order)) {
                            return true;
                        }
                    }
                }
                if (jl_is_tuple_type(utt) && is_tupletype_homogeneous(utt->parameters, true)) {
                    // For tuples, we can emit code even if we don't know the exact
                    // type (e.g. because we don't know the length). This is possible
                    // as long as we know that all elements are of the same (leaf) type.
                    if (obj.ispointer()) {
                        if (order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
                            emit_atomic_error(ctx, "getfield: non-atomic field cannot be accessed atomically");
                            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
                            return true;
                        }
                        // Determine which was the type that was homogenous
                        jl_value_t *jt = jl_tparam0(utt);
                        if (jl_is_vararg(jt))
                            jt = jl_unwrap_vararg(jt);
                        assert(jl_is_datatype(jt));
                        Value *vidx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), fld, (jl_value_t*)jl_long_type);
                        // This is not necessary for correctness, but allows to omit
                        // the extra code for getting the length of the tuple
                        if (!bounds_check_enabled(ctx, boundscheck)) {
                            vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), 1));
                        } else {
                            vidx = emit_bounds_check(ctx, obj, (jl_value_t*)obj.typ, vidx,
                                emit_datatype_nfields(ctx, emit_typeof_boxed(ctx, obj)),
                                jl_true);
                        }
                        bool isboxed = !jl_datatype_isinlinealloc((jl_datatype_t*)jt, 0);
                        Value *ptr = maybe_decay_tracked(ctx, data_pointer(ctx, obj));
                        *ret = typed_load(ctx, ptr, vidx,
                                isboxed ? (jl_value_t*)jl_any_type : jt,
                                obj.tbaa, nullptr, isboxed, AtomicOrdering::NotAtomic, false);
                        return true;
                    }
                }
            }
        }
        // TODO: generic getfield func with more efficient calling convention
        return false;
    }

    else if (f == jl_builtin_getglobal && (nargs == 2 || nargs == 3)) {
        const jl_cgval_t &mod = argv[1];
        const jl_cgval_t &sym = argv[2];
        enum jl_memory_order order = jl_memory_order_unspecified;

        if (nargs == 3) {
            const jl_cgval_t &arg3 = argv[3];
            if (arg3.constant && jl_is_symbol(arg3.constant))
                order = jl_get_atomic_order((jl_sym_t*)arg3.constant, true, false);
            else
                return false;
        }
        else
            order = jl_memory_order_monotonic;

        if (order == jl_memory_order_invalid || order == jl_memory_order_notatomic) {
            emit_atomic_error(ctx, order == jl_memory_order_invalid ? "invalid atomic ordering" : "getglobal: module binding cannot be read non-atomically");
            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            return true;
        }

        if (sym.constant && jl_is_symbol(sym.constant)) {
            jl_sym_t *name = (jl_sym_t*)sym.constant;
            if (mod.constant && jl_is_module(mod.constant)) {
                *ret = emit_globalref(ctx, (jl_module_t*)mod.constant, name, get_llvm_atomic_order(order));
                return true;
            }
        }

        return false;
    }

    else if (f == jl_builtin_setglobal && (nargs == 3 || nargs == 4)) {
        return emit_f_opglobal(ctx, ret, f, argv, nargs, nullptr);
    }

    else if ((f == jl_builtin_setfield && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_swapfield && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_replacefield && (nargs == 4 || nargs == 5 || nargs == 6)) ||
             (f == jl_builtin_modifyfield && (nargs == 4 || nargs == 5))) {
        return emit_f_opfield(ctx, ret, f, argv, nargs, nullptr);
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
        ssize_t nf = -1;
        if (obj.constant) {
            nf = jl_datatype_nfields(jl_typeof(obj.constant));
        }
        else if (jl_is_type_type(obj.typ)) {
            jl_value_t *tp0 = jl_tparam0(obj.typ);
            if (jl_is_datatype(tp0) && jl_is_datatype_singleton((jl_datatype_t*)tp0))
                nf = jl_datatype_nfields((jl_value_t*)jl_datatype_type);
        }
        else if (jl_is_concrete_type(obj.typ)) {
            nf = jl_datatype_nfields(obj.typ);
        }
        Value *sz;
        if (nf != -1)
            sz = ConstantInt::get(getSizeTy(ctx.builder.getContext()), nf);
        else
            sz = emit_datatype_nfields(ctx, emit_typeof_boxed(ctx, obj));
        *ret = mark_julia_type(ctx, sz, false, jl_long_type);
        return true;
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
                Value *idx = emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), fld, (jl_value_t*)jl_long_type);
                jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                if (nargs == 3)
                    emit_typecheck(ctx, argv[3], (jl_value_t*)jl_bool_type, "fieldtype");
                emit_bounds_check(ctx, typ, (jl_value_t*)jl_datatype_type, idx, types_len, boundscheck);
                Value *fieldtyp_p = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, decay_derived(ctx, emit_bitcast(ctx, types_svec, ctx.types().T_pprjlvalue)), idx);
                Value *fieldtyp = tbaa_decorate(ctx.tbaa().tbaa_const, ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, fieldtyp_p, Align(sizeof(void*))));
                *ret = mark_julia_type(ctx, fieldtyp, true, (jl_value_t*)jl_type_type);
                return true;
            }
        }
    }

    else if (f == jl_builtin_sizeof && nargs == 1) {
        const jl_cgval_t &obj = argv[1];
        jl_datatype_t *sty = (jl_datatype_t*)jl_unwrap_unionall(obj.typ);
        assert(jl_string_type->name->mutabl);
        if (sty == jl_string_type || sty == jl_simplevector_type) {
            if (obj.constant) {
                size_t sz;
                if (sty == jl_string_type) {
                    sz = jl_string_len(obj.constant);
                }
                else {
                    sz = (1 + jl_svec_len(obj.constant)) * sizeof(void*);
                }
                *ret = mark_julia_type(ctx, ConstantInt::get(getSizeTy(ctx.builder.getContext()), sz), false, jl_long_type);
                return true;
            }
            // String and SimpleVector's length fields have the same layout
            auto ptr = emit_bitcast(ctx, boxed(ctx, obj), getSizePtrTy(ctx.builder.getContext()));
            Value *len = tbaa_decorate(ctx.tbaa().tbaa_const, ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()), ptr, Align(sizeof(size_t))));
            MDBuilder MDB(ctx.builder.getContext());
            if (sty == jl_simplevector_type) {
                auto rng = MDB.createRange(
                    Constant::getNullValue(getSizeTy(ctx.builder.getContext())), ConstantInt::get(getSizeTy(ctx.builder.getContext()), INTPTR_MAX / sizeof(void*) - 1));
                cast<LoadInst>(len)->setMetadata(LLVMContext::MD_range, rng);
                len = ctx.builder.CreateMul(len, ConstantInt::get(getSizeTy(ctx.builder.getContext()), sizeof(void*)));
                len = ctx.builder.CreateAdd(len, ConstantInt::get(getSizeTy(ctx.builder.getContext()), sizeof(void*)));
            }
            else {
                auto rng = MDB.createRange(Constant::getNullValue(getSizeTy(ctx.builder.getContext())), ConstantInt::get(getSizeTy(ctx.builder.getContext()), INTPTR_MAX));
                cast<LoadInst>(len)->setMetadata(LLVMContext::MD_range, rng);
            }
            *ret = mark_julia_type(ctx, len, false, jl_long_type);
            return true;
        }
        else if (jl_is_array_type(sty)) {
            auto len = emit_arraylen(ctx, obj);
            Value *elsize;
            size_t elsz;
            if (arraytype_constelsize(sty, &elsz)) {
                elsize = ConstantInt::get(getSizeTy(ctx.builder.getContext()), elsz);
            }
            else {
                elsize = ctx.builder.CreateZExt(emit_arrayelsize(ctx, obj), getSizeTy(ctx.builder.getContext()));
            }
            *ret = mark_julia_type(ctx, ctx.builder.CreateMul(len, elsize), false, jl_long_type);
            return true;
        }
    }

    else if (f == jl_builtin_apply_type && nargs > 0) {
        if (jl_is_method(ctx.linfo->def.method)) {
            // don't bother codegen constant-folding for toplevel.
            jl_value_t *ty = static_apply_type(ctx, argv, nargs + 1);
            if (ty != NULL) {
                jl_add_method_root(ctx, ty);
                *ret = mark_julia_const(ctx, ty);
                return true;
            }
        }
    }

    else if (f == jl_builtin_isdefined && (nargs == 2 || nargs == 3)) {
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
        if (fld.constant && jl_is_symbol(fld.constant)) {
            jl_sym_t *sym = (jl_sym_t*)fld.constant;
            fieldidx = jl_field_index(stt, sym, 0);
        }
        else if (fld.constant && fld.typ == (jl_value_t*)jl_long_type) {
            fieldidx = jl_unbox_long(fld.constant) - 1;
        }
        else {
            return false;
        }
        enum jl_memory_order order = jl_memory_order_unspecified;
        if (nargs == 3) {
            const jl_cgval_t &ord = argv[3];
            emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, "isdefined");
            if (!ord.constant)
                return false;
            order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
        }
        if (order == jl_memory_order_invalid) {
            emit_atomic_error(ctx, "invalid atomic ordering");
            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            return true;
        }
        ssize_t nf = jl_datatype_nfields(stt);
        if (fieldidx < 0 || fieldidx >= nf) {
            if (order != jl_memory_order_unspecified) {
                emit_atomic_error(ctx, "isdefined: atomic ordering cannot be specified for nonexistent field");
                *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
                return true;
            }
            *ret = mark_julia_const(ctx, jl_false);
            return true;
        }
        bool isatomic = jl_field_isatomic(stt, fieldidx);
        if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
            emit_atomic_error(ctx, "isdefined: non-atomic field cannot be accessed atomically");
            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            return true;
        }
        if (isatomic && order == jl_memory_order_notatomic) {
            emit_atomic_error(ctx, "isdefined: atomic field cannot be accessed non-atomically");
            *ret = jl_cgval_t(ctx.builder.getContext()); // unreachable
            return true;
        }
        else if (fieldidx < nf - stt->name->n_uninitialized) {
            *ret = mark_julia_const(ctx, jl_true);
        }
        else if (jl_field_isptr(stt, fieldidx) || jl_type_hasptr(jl_field_type(stt, fieldidx))) {
            Value *fldv;
            size_t offs = jl_field_offset(stt, fieldidx) / sizeof(jl_value_t*);
            auto tbaa = obj.tbaa;
            if (tbaa == ctx.tbaa().tbaa_datatype && offs != offsetof(jl_datatype_t, types))
                tbaa = ctx.tbaa().tbaa_const;
            if (obj.ispointer()) {
                if (!jl_field_isptr(stt, fieldidx))
                    offs += ((jl_datatype_t*)jl_field_type(stt, fieldidx))->layout->first_ptr;
                Value *ptr = emit_bitcast(ctx, maybe_decay_tracked(ctx, data_pointer(ctx, obj)), ctx.types().T_pprjlvalue);
                Value *addr = ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, ptr, offs);
                // emit this using the same type as emit_getfield_knownidx
                // so that LLVM may be able to load-load forward them and fold the result
                fldv = tbaa_decorate(tbaa, ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, addr, Align(sizeof(size_t))));
                cast<LoadInst>(fldv)->setOrdering(order <= jl_memory_order_notatomic ? AtomicOrdering::Unordered : get_llvm_atomic_order(order));
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
            *ret = mark_julia_const(ctx, jl_true);
        }
        if (order > jl_memory_order_monotonic && ret->constant) {
            // fence instructions may only have acquire, release, acq_rel, or seq_cst ordering.
            ctx.builder.CreateFence(get_llvm_atomic_order(order));
        }
        return true;
    }

    else if (f == jl_builtin_donotdelete) {
        // For now we emit this as a vararg call to the builtin
        // (which doesn't look at the arguments). In the future,
        // this should be an LLVM builtin.
        auto it = builtin_func_map().find(jl_f_donotdelete_addr);
        if (it == builtin_func_map().end()) {
            return false;
        }

        *ret = mark_julia_const(ctx, jl_nothing);
        FunctionType *Fty = FunctionType::get(getVoidTy(ctx.builder.getContext()), true);
        Function *dnd = prepare_call(it->second);
        SmallVector<Value*, 1> call_args;

        for (size_t i = 1; i <= nargs; ++i) {
            const jl_cgval_t &obj = argv[i];
            if (obj.V) {
                // TODO is this strong enough to constitute a read of any contained
                // pointers?
                Value *V = obj.V;
                if (obj.isboxed) {
                    V = emit_pointer_from_objref(ctx, V);
                }
                call_args.push_back(V);
            }
        }
        ctx.builder.CreateCall(Fty, dnd, call_args);
        return true;
    }

    return false;
}

// Returns ctx.types().T_prjlvalue
static CallInst *emit_jlcall(jl_codectx_t &ctx, Function *theFptr, Value *theF,
                             const jl_cgval_t *argv, size_t nargs, CallingConv::ID cc)
{
    ++EmittedJLCalls;
    // emit arguments
    SmallVector<Value*, 3> theArgs;
    SmallVector<Type*, 3> argsT;
    if (theF) {
        theArgs.push_back(theF);
        argsT.push_back(ctx.types().T_prjlvalue);
    }
    for (size_t i = 0; i < nargs; i++) {
        Value *arg = boxed(ctx, argv[i]);
        theArgs.push_back(arg);
        argsT.push_back(ctx.types().T_prjlvalue);
    }
    FunctionType *FTy = FunctionType::get(ctx.types().T_prjlvalue, argsT, false);
    CallInst *result = ctx.builder.CreateCall(FTy,
        ctx.builder.CreateBitCast(theFptr, FTy->getPointerTo()),
        theArgs);
    addRetAttr(result, Attribute::NonNull);
    result->setCallingConv(cc);
    return result;
}
// Returns ctx.types().T_prjlvalue
static CallInst *emit_jlcall(jl_codectx_t &ctx, JuliaFunction *theFptr, Value *theF,
                             const jl_cgval_t *argv, size_t nargs, CallingConv::ID cc)
{
    return emit_jlcall(ctx, prepare_call(theFptr), theF, argv, nargs, cc);
}


static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, jl_method_instance_t *mi, jl_value_t *jlretty, StringRef specFunctionObject,
                                          const jl_cgval_t *argv, size_t nargs, jl_returninfo_t::CallingConv *cc, unsigned *return_roots, jl_value_t *inferred_retty)
{
    ++EmittedSpecfunCalls;
    // emit specialized call site
    bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
    jl_returninfo_t returninfo = get_specsig_function(ctx, jl_Module, specFunctionObject, mi->specTypes, jlretty, is_opaque_closure);
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
        result = emit_static_alloca(ctx, getAttributeAtIndex(returninfo.decl->getAttributes(), 1, Attribute::StructRet).getValueAsType());
        assert(cast<PointerType>(result->getType())->hasSameElementTypeAs(cast<PointerType>(cft->getParamType(0))));
        argvals[idx] = result;
        idx++;
        break;
    case jl_returninfo_t::Union:
        result = emit_static_alloca(ctx, ArrayType::get(getInt8Ty(ctx.builder.getContext()), returninfo.union_bytes));
        if (returninfo.union_align > 1)
            result->setAlignment(Align(returninfo.union_align));
        argvals[idx] = result;
        idx++;
        break;
    }

    if (returninfo.return_roots) {
        AllocaInst *return_roots = emit_static_alloca(ctx, ArrayType::get(ctx.types().T_prjlvalue, returninfo.return_roots));
        argvals[idx] = return_roots;
        idx++;
    }

    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = (is_opaque_closure && i == 0) ? (jl_value_t*)jl_any_type :
            jl_nth_slot_type(mi->specTypes, i);
        if (is_uniquerep_Type(jt))
            continue;
        bool isboxed = deserves_argbox(jt);
        Type *et = isboxed ?  ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (type_is_ghost(et))
            continue;
        assert(idx < nfargs);
        Type *at = cft->getParamType(idx);
        jl_cgval_t arg = argv[i];
        if (isboxed) {
            assert(at == ctx.types().T_prjlvalue && et == ctx.types().T_prjlvalue);
            argvals[idx] = boxed(ctx, arg);
        }
        else if (et->isAggregateType()) {
            arg = value_to_pointer(ctx, arg);
            // can lazy load on demand, no copy needed
            assert(at == PointerType::get(et, AddressSpace::Derived));
            argvals[idx] = decay_derived(ctx, maybe_bitcast(ctx,
                data_pointer(ctx, arg), at));
        }
        else {
            assert(at == et);
            Value *val = emit_unbox(ctx, et, arg, jt);
            if (!val) {
                // There was a type mismatch of some sort - exit early
                CreateTrap(ctx.builder);
                return jl_cgval_t(ctx.builder.getContext());
            }
            argvals[idx] = val;
        }
        idx++;
    }
    assert(idx == nfargs);
    CallInst *call = ctx.builder.CreateCall(returninfo.decl, ArrayRef<Value*>(&argvals[0], nfargs));
    call->setAttributes(returninfo.decl->getAttributes());

    jl_cgval_t retval(ctx.builder.getContext());
    switch (returninfo.cc) {
        case jl_returninfo_t::Boxed:
            retval = mark_julia_type(ctx, call, true, jlretty);
            break;
        case jl_returninfo_t::Register:
            retval = mark_julia_type(ctx, call, false, jlretty);
            break;
        case jl_returninfo_t::SRet:
            retval = mark_julia_slot(result, jlretty, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            break;
        case jl_returninfo_t::Union: {
            Value *box = ctx.builder.CreateExtractValue(call, 0);
            Value *tindex = ctx.builder.CreateExtractValue(call, 1);
            Value *derived = ctx.builder.CreateSelect(
                ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                        ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0)),
                decay_derived(ctx, ctx.builder.CreateBitCast(argvals[0], ctx.types().T_pjlvalue)),
                decay_derived(ctx, box)
            );
            retval = mark_julia_slot(derived,
                                     jlretty,
                                     tindex,
                                     ctx.tbaa(),
                                     ctx.tbaa().tbaa_stack);
            retval.Vboxed = box;
            break;
        }
        case jl_returninfo_t::Ghosts:
            retval = mark_julia_slot(NULL, jlretty, call, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            break;
    }
    // see if inference has a different / better type for the call than the lambda
    return update_julia_type(ctx, retval, inferred_retty);
}

static jl_cgval_t emit_call_specfun_boxed(jl_codectx_t &ctx, jl_value_t *jlretty, StringRef specFunctionObject,
                                          const jl_cgval_t *argv, size_t nargs, jl_value_t *inferred_retty)
{
    auto theFptr = cast<Function>(
        jl_Module->getOrInsertFunction(specFunctionObject, ctx.types().T_jlfunc).getCallee());
    addRetAttr(theFptr, Attribute::NonNull);
    theFptr->addFnAttr(Attribute::get(ctx.builder.getContext(), "thunk"));
    Value *ret = emit_jlcall(ctx, theFptr, nullptr, argv, nargs, JLCALL_F_CC);
    return update_julia_type(ctx, mark_julia_type(ctx, ret, true, jlretty), inferred_retty);
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
            return jl_cgval_t(ctx.builder.getContext());
    }
    return emit_invoke(ctx, lival, argv, nargs, rt);
}

static jl_cgval_t emit_invoke(jl_codectx_t &ctx, const jl_cgval_t &lival, const jl_cgval_t *argv, size_t nargs, jl_value_t *rt)
{
    ++EmittedInvokes;
    bool handled = false;
    jl_cgval_t result(ctx.builder.getContext());
    if (lival.constant) {
        jl_method_instance_t *mi = (jl_method_instance_t*)lival.constant;
        assert(jl_is_method_instance(mi));
        if (mi == ctx.linfo) {
            // handle self-recursion specially
            jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
            FunctionType *ft = ctx.f->getFunctionType();
            StringRef protoname = ctx.f->getName();
            if (ft == ctx.types().T_jlfunc) {
                result = emit_call_specfun_boxed(ctx, ctx.rettype, protoname, argv, nargs, rt);
                handled = true;
            }
            else if (ft != ctx.types().T_jlfuncparams) {
                unsigned return_roots = 0;
                result = emit_call_specfun_other(ctx, mi, ctx.rettype, protoname, argv, nargs, &cc, &return_roots, rt);
                handled = true;
            }
        }
        else {
            jl_value_t *ci = ctx.params->lookup(mi, ctx.world, ctx.world); // TODO: need to use the right pair world here
            jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
            if (ci != jl_nothing) {
                auto invoke = jl_atomic_load_relaxed(&codeinst->invoke);
                 // check if we know how to handle this specptr
                if (invoke == jl_fptr_const_return_addr) {
                    result = mark_julia_const(ctx, codeinst->rettype_const);
                    handled = true;
                }
                else if (invoke != jl_fptr_sparam_addr) {
                    bool specsig, needsparams;
                    std::tie(specsig, needsparams) = uses_specsig(mi, codeinst->rettype, ctx.params->prefer_specsig);
                    std::string name;
                    StringRef protoname;
                    bool need_to_emit = true;
                    if (ctx.use_cache) {
                        // optimization: emit the correct name immediately, if we know it
                        // TODO: use `emitted` map here too to try to consolidate names?
                        auto invoke = jl_atomic_load_relaxed(&codeinst->invoke);
                        auto fptr = jl_atomic_load_relaxed(&codeinst->specptr.fptr);
                        if (fptr) {
                            if (specsig ? codeinst->isspecsig : invoke == jl_fptr_args_addr) {
                                protoname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, codeinst);
                                need_to_emit = false;
                            }
                        }
                    }
                    auto it = ctx.call_targets.find(codeinst);
                    if (need_to_emit && it != ctx.call_targets.end()) {
                        protoname = std::get<2>(it->second)->getName();
                        need_to_emit = false;
                    }
                    if (need_to_emit) {
                        raw_string_ostream(name) << (specsig ? "j_" : "j1_") << name_from_method_instance(mi) << "_" << globalUniqueGeneratedNames++;
                        protoname = StringRef(name);
                    }
                    jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
                    unsigned return_roots = 0;
                    if (specsig)
                        result = emit_call_specfun_other(ctx, mi, codeinst->rettype, protoname, argv, nargs, &cc, &return_roots, rt);
                    else
                        result = emit_call_specfun_boxed(ctx, codeinst->rettype, protoname, argv, nargs, rt);
                    handled = true;
                    if (need_to_emit) {
                        Function *trampoline_decl = cast<Function>(jl_Module->getNamedValue(protoname));
                        ctx.call_targets[codeinst] = std::make_tuple(cc, return_roots, trampoline_decl, specsig);
                    }
                }
            }
        }
    }
    if (!handled) {
        Value *r = emit_jlcall(ctx, jlinvoke_func, boxed(ctx, lival), argv, nargs, JLCALL_F2_CC);
        result = mark_julia_type(ctx, r, true, rt);
    }
    if (result.typ == jl_bottom_type)
        CreateTrap(ctx.builder);
    return result;
}

static jl_cgval_t emit_invoke_modify(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
{
    ++EmittedInvokes;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 2);
    jl_cgval_t lival = emit_expr(ctx, args[0]);
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t(ctx.builder.getContext());
    }
    const jl_cgval_t &f = argv[0];
    jl_cgval_t ret(ctx.builder.getContext());
    if (f.constant && f.constant == jl_builtin_modifyfield) {
        if (emit_f_opfield(ctx, &ret, jl_builtin_modifyfield, argv, nargs - 1, &lival))
            return ret;
        auto it = builtin_func_map().find(jl_f_modifyfield_addr);
        assert(it != builtin_func_map().end());
        Value *oldnew = emit_jlcall(ctx, it->second, Constant::getNullValue(ctx.types().T_prjlvalue), &argv[1], nargs - 1, JLCALL_F_CC);
        return mark_julia_type(ctx, oldnew, true, rt);
    }
    if (f.constant && jl_typeis(f.constant, jl_intrinsic_type)) {
        JL_I::intrinsic fi = (intrinsic)*(uint32_t*)jl_data_ptr(f.constant);
        if (fi == JL_I::atomic_pointermodify && jl_intrinsic_nargs((int)fi) == nargs - 1)
            return emit_atomic_pointerop(ctx, fi, argv, nargs - 1, &lival);
    }

    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, nargs, JLCALL_F_CC);
    return mark_julia_type(ctx, callval, true, rt);
}

static jl_cgval_t emit_call(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt, bool is_promotable)
{
    ++EmittedCalls;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_dim0(ex->args);
    assert(nargs >= 1);
    jl_cgval_t f = emit_expr(ctx, args[0]);

    if (f.constant && jl_typeis(f.constant, jl_intrinsic_type)) {
        JL_I::intrinsic fi = (intrinsic)*(uint32_t*)jl_data_ptr(f.constant);
        return emit_intrinsic(ctx, fi, args, nargs - 1);
    }

    jl_value_t *context = ctx.params->generic_context == jl_nothing ? nullptr : ctx.params->generic_context;
    size_t n_generic_args = nargs + (context ? 1 : 0);

    jl_cgval_t *generic_argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * n_generic_args);
    jl_cgval_t *argv = generic_argv;
    if (context) {
        generic_argv[0] = mark_julia_const(ctx, context);
        argv = &generic_argv[1];
    }
    argv[0] = f;
    for (size_t i = 1; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t(ctx.builder.getContext()); // anything past here is unreachable
    }

    if (f.constant && jl_isa(f.constant, (jl_value_t*)jl_builtin_type)) {
        if (f.constant == jl_builtin_ifelse && nargs == 4)
            return emit_ifelse(ctx, argv[1], argv[2], argv[3], rt);
        jl_cgval_t result(ctx.builder.getContext());
        bool handled = emit_builtin_call(ctx, &result, f.constant, argv, nargs - 1, rt, ex, is_promotable);
        if (handled) {
            return result;
        }

        // special case for known builtin not handled by emit_builtin_call
        auto it = builtin_func_map().find(jl_get_builtin_fptr(f.constant));
        if (it != builtin_func_map().end()) {
            Value *ret = emit_jlcall(ctx, it->second, Constant::getNullValue(ctx.types().T_prjlvalue), &argv[1], nargs - 1, JLCALL_F_CC);
            return mark_julia_type(ctx, ret, true, rt);
        }
    }

    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, generic_argv, n_generic_args, JLCALL_F_CC);
    return mark_julia_type(ctx, callval, true, rt);
}

// --- accessing and assigning variables ---

static void undef_var_error_ifnot(jl_codectx_t &ctx, Value *ok, jl_sym_t *name)
{
    ++EmittedUndefVarErrors;
    BasicBlock *err = BasicBlock::Create(ctx.builder.getContext(), "err", ctx.f);
    BasicBlock *ifok = BasicBlock::Create(ctx.builder.getContext(), "ok");
    ctx.builder.CreateCondBr(ok, ifok, err);
    ctx.builder.SetInsertPoint(err);
    ctx.builder.CreateCall(prepare_call(jlundefvarerror_func),
        mark_callee_rooted(ctx, literal_pointer_val(ctx, (jl_value_t*)name)));
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
    if (assign)
        b = jl_get_binding_wr(m, s, 0);
    else
        b = jl_get_binding(m, s);
    if (b == NULL) {
        // var not found. switch to delayed lookup.
        Constant *initnul = Constant::getNullValue(ctx.types().T_pjlvalue);
        GlobalVariable *bindinggv = new GlobalVariable(*ctx.f->getParent(), ctx.types().T_pjlvalue,
                false, GlobalVariable::PrivateLinkage, initnul);
        LoadInst *cachedval = ctx.builder.CreateAlignedLoad(ctx.types().T_pjlvalue, bindinggv, Align(sizeof(void*)));
        cachedval->setOrdering(AtomicOrdering::Unordered);
        BasicBlock *have_val = BasicBlock::Create(ctx.builder.getContext(), "found");
        BasicBlock *not_found = BasicBlock::Create(ctx.builder.getContext(), "notfound");
        BasicBlock *currentbb = ctx.builder.GetInsertBlock();
        ctx.builder.CreateCondBr(ctx.builder.CreateICmpNE(cachedval, initnul), have_val, not_found);
        ctx.f->getBasicBlockList().push_back(not_found);
        ctx.builder.SetInsertPoint(not_found);
        Value *bval = ctx.builder.CreateCall(prepare_call(assign ? jlgetbindingwrorerror_func : jlgetbindingorerror_func),
                { literal_pointer_val(ctx, (jl_value_t*)m),
                  literal_pointer_val(ctx, (jl_value_t*)s) });
        ctx.builder.CreateAlignedStore(bval, bindinggv, Align(sizeof(void*)))->setOrdering(AtomicOrdering::Release);
        ctx.builder.CreateBr(have_val);
        ctx.f->getBasicBlockList().push_back(have_val);
        ctx.builder.SetInsertPoint(have_val);
        PHINode *p = ctx.builder.CreatePHI(ctx.types().T_pjlvalue, 2);
        p->addIncoming(cachedval, currentbb);
        p->addIncoming(bval, not_found);
        return p;
    }
    if (assign) {
        if (b->owner != m) {
            char *msg;
            (void)asprintf(&msg, "cannot assign a value to imported variable %s.%s from module %s",
                    jl_symbol_name(b->owner->name), jl_symbol_name(s), jl_symbol_name(m->name));
            emit_error(ctx, msg);
            free(msg);
            return NULL;
        }
    }
    else {
        if (b->deprecated)
            cg_bdw(ctx, b);
    }
    *pbnd = b;
    return julia_binding_gv(ctx, b);
}

static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, bool isvol, MDNode *tbaa)
{
    LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
    if (isvol)
        v->setVolatile(true);
    v->setOrdering(AtomicOrdering::Unordered);
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
            return mark_julia_const(ctx, e);
        }
    }
    assert(ctx.spvals_ptr != NULL);
    Value *bp = ctx.builder.CreateConstInBoundsGEP1_32(
            ctx.types().T_prjlvalue,
            ctx.spvals_ptr,
            i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
    Value *sp = tbaa_decorate(ctx.tbaa().tbaa_const, ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*))));
    Value *isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp, false),
            track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jl_tvar_type)));
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
    if (bp == NULL)
        return jl_cgval_t(ctx.builder.getContext());
    if (jbp && jbp->value != NULL) {
        if (jbp->constp)
            return mark_julia_const(ctx, jbp->value);
        // double-check that a global variable is actually defined. this
        // can be a problem in parallel when a definition is missing on
        // one machine.
        LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
        v->setOrdering(AtomicOrdering::Unordered);
        tbaa_decorate(ctx.tbaa().tbaa_binding, v);
        return mark_julia_type(ctx, v, true, jl_any_type);
    }
    return emit_checked_var(ctx, bp, sym, false, ctx.tbaa().tbaa_binding);
}

static jl_cgval_t emit_isdefined(jl_codectx_t &ctx, jl_value_t *sym)
{
    Value *isnull = NULL;
    if (jl_is_slot(sym) || jl_is_argument(sym)) {
        size_t sl = jl_slot_number(sym) - 1;
        jl_varinfo_t &vi = ctx.slots[sl];
        if (!vi.usedUndef)
            return mark_julia_const(ctx, jl_true);
        if (vi.boxroot == NULL || vi.pTIndex != NULL) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateAlignedLoad(getInt1Ty(ctx.builder.getContext()), vi.defFlag, Align(1), vi.isVolatile);
        }
        if (vi.boxroot != NULL) {
            Value *boxed = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, vi.boxroot, Align(sizeof(void*)), vi.isVolatile);
            Value *box_isnull = ctx.builder.CreateICmpNE(boxed, Constant::getNullValue(ctx.types().T_prjlvalue));
            if (vi.pTIndex) {
                // value is either boxed in the stack slot, or unboxed in value
                // as indicated by testing (pTIndex & 0x80)
                Value *tindex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(sizeof(void*)), vi.isVolatile);
                Value *load_unbox = ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                            ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            }
            else {
                isnull = box_isnull;
            }
        }
    }
    else if (jl_is_expr(sym)) {
        assert(((jl_expr_t*)sym)->head == jl_static_parameter_sym && "malformed isdefined expression");
        size_t i = jl_unbox_long(jl_exprarg(sym, 0)) - 1;
        if (jl_svec_len(ctx.linfo->sparam_vals) > 0) {
            jl_value_t *e = jl_svecref(ctx.linfo->sparam_vals, i);
            if (!jl_is_typevar(e)) {
                return mark_julia_const(ctx, jl_true);
            }
        }
        assert(ctx.spvals_ptr != NULL);
        Value *bp = ctx.builder.CreateConstInBoundsGEP1_32(
                ctx.types().T_prjlvalue,
                ctx.spvals_ptr,
                i + sizeof(jl_svec_t) / sizeof(jl_value_t*));
        Value *sp = tbaa_decorate(ctx.tbaa().tbaa_const, ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*))));
        isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp, false),
            track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)jl_tvar_type)));
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
                return mark_julia_const(ctx, jl_true);
            Value *bp = julia_binding_gv(ctx, bnd);
            bp = julia_binding_pvalue(ctx, bp);
            LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
            tbaa_decorate(ctx.tbaa().tbaa_binding, v);
            v->setOrdering(AtomicOrdering::Unordered);
            isnull = ctx.builder.CreateICmpNE(v, Constant::getNullValue(ctx.types().T_prjlvalue));
        }
        else {
            Value *v = ctx.builder.CreateCall(prepare_call(jlboundp_func), {
                    literal_pointer_val(ctx, (jl_value_t*)modu),
                    literal_pointer_val(ctx, (jl_value_t*)name)
                });
            isnull = ctx.builder.CreateICmpNE(v, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0));
        }
    }
    return mark_julia_type(ctx, isnull, false, jl_bool_type);
}

static jl_cgval_t emit_varinfo(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_sym_t *varname, jl_value_t *better_typ=NULL) {
    jl_value_t *typ = better_typ ? better_typ : vi.value.typ;
    jl_cgval_t v(ctx.builder.getContext());
    Value *isnull = NULL;
    if (vi.boxroot == NULL || vi.pTIndex != NULL) {
        if ((!vi.isVolatile && vi.isSA) || vi.isArgument || vi.value.constant || !vi.value.V) {
            v = vi.value;
            if (vi.pTIndex)
                v.TIndex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(1));
        }
        else {
            // copy value to a non-mutable (non-volatile SSA) location
            AllocaInst *varslot = cast<AllocaInst>(vi.value.V);
            Type *T = varslot->getAllocatedType();
            assert(!varslot->isArrayAllocation() && "variables not expected to be VLA");
            AllocaInst *ssaslot = cast<AllocaInst>(varslot->clone());
            ssaslot->insertAfter(varslot);
            if (vi.isVolatile) {
                Value *unbox = ctx.builder.CreateAlignedLoad(ssaslot->getAllocatedType(), varslot,
                        varslot->getAlign(),
                        true);
                ctx.builder.CreateAlignedStore(unbox, ssaslot, ssaslot->getAlign());
            }
            else {
                const DataLayout &DL = jl_Module->getDataLayout();
                uint64_t sz = DL.getTypeStoreSize(T);
                emit_memcpy(ctx, ssaslot, ctx.tbaa().tbaa_stack, vi.value, sz, ssaslot->getAlign().value());
            }
            Value *tindex = NULL;
            if (vi.pTIndex)
                tindex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(1), vi.isVolatile);
            v = mark_julia_slot(ssaslot, vi.value.typ, tindex, ctx.tbaa(), ctx.tbaa().tbaa_stack);
        }
        if (vi.boxroot == NULL)
            v = update_julia_type(ctx, v, typ);
        if (vi.usedUndef) {
            assert(vi.defFlag);
            isnull = ctx.builder.CreateAlignedLoad(getInt1Ty(ctx.builder.getContext()), vi.defFlag, Align(1), vi.isVolatile);
        }
    }
    if (vi.boxroot != NULL) {
        Instruction *boxed = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, vi.boxroot, Align(sizeof(void*)), vi.isVolatile);
        Value *box_isnull = NULL;
        if (vi.usedUndef)
            box_isnull = ctx.builder.CreateICmpNE(boxed, Constant::getNullValue(ctx.types().T_prjlvalue));
        maybe_mark_load_dereferenceable(boxed, vi.usedUndef || vi.pTIndex, typ);
        if (vi.pTIndex) {
            // value is either boxed in the stack slot, or unboxed in value
            // as indicated by testing (pTIndex & 0x80)
            Value *load_unbox = ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                        ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            if (vi.usedUndef)
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            if (v.V) { // v.V will be null if it is a union of all ghost values
                v.V = ctx.builder.CreateSelect(load_unbox, emit_bitcast(ctx,
                    decay_derived(ctx, v.V), boxed->getType()), decay_derived(ctx, boxed));
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
                    tbaa_decorate(ctx.tbaa().tbaa_stack, ctx.builder.CreateStore(
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
                    Value *copy_bytes = ConstantInt::get(getInt32Ty(ctx.builder.getContext()), jl_datatype_size(vi.value.typ));
                    emit_memcpy(ctx, vi.value.V, ctx.tbaa().tbaa_stack, rval_info, copy_bytes,
                                julia_alignment(rval_info.typ), vi.isVolatile);
                }
            }
            else {
                emit_unionmove(ctx, vi.value.V, ctx.tbaa().tbaa_stack, rval_info, /*skip*/isboxed, vi.isVolatile);
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
    jl_value_t *phiType = NULL;
    if (jl_is_array(ssavalue_types)) {
        phiType = jl_array_ptr_ref(ssavalue_types, idx);
    } else {
        phiType = (jl_value_t*)jl_any_type;
    }
    jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(r, 0);
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
            PHINode *Tindex_phi = PHINode::Create(getInt8Ty(ctx.builder.getContext()), jl_array_len(edges), "tindex_phi");
            BB->getInstList().insert(InsertPt, Tindex_phi);
            PHINode *ptr_phi = PHINode::Create(ctx.types().T_prjlvalue, jl_array_len(edges), "ptr_phi");
            BB->getInstList().insert(InsertPt, ptr_phi);
            Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(Tindex_phi, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            ctx.builder.CreateMemCpy(phi, MaybeAlign(min_align), dest, MaybeAlign(0), nbytes, false);
            ctx.builder.CreateLifetimeEnd(dest);
            Value *ptr = ctx.builder.CreateSelect(isboxed,
                maybe_bitcast(ctx, decay_derived(ctx, ptr_phi), getInt8PtrTy(ctx.builder.getContext())),
                maybe_bitcast(ctx, decay_derived(ctx, phi), getInt8PtrTy(ctx.builder.getContext())));
            jl_cgval_t val = mark_julia_slot(ptr, phiType, Tindex_phi, ctx.tbaa(), ctx.tbaa().tbaa_stack); // XXX: this TBAA is wrong for ptr_phi
            val.Vboxed = ptr_phi;
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, ptr_phi, r));
            ctx.SAvalues.at(idx) = val;
            ctx.ssavalue_assigned.at(idx) = true;
            return;
        }
        else if (allunbox) {
            PHINode *Tindex_phi = PHINode::Create(getInt8Ty(ctx.builder.getContext()), jl_array_len(edges), "tindex_phi");
            BB->getInstList().insert(InsertPt, Tindex_phi);
            jl_cgval_t val = mark_julia_slot(NULL, phiType, Tindex_phi, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, (PHINode*)NULL, r));
            ctx.SAvalues.at(idx) = val;
            ctx.ssavalue_assigned.at(idx) = true;
            return;
        }
    }
    bool isboxed = !deserves_stack(phiType);
    Type *vtype = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, phiType);
    // The frontend should really not emit this, but we allow it
    // for convenience.
    if (type_is_ghost(vtype)) {
        assert(jl_is_datatype(phiType) && ((jl_datatype_t*)phiType)->instance);
        // Skip adding it to the PhiNodes list, since we didn't create one.
        ctx.SAvalues.at(idx) = mark_julia_const(ctx, ((jl_datatype_t*)phiType)->instance);
        ctx.ssavalue_assigned.at(idx) = true;
        return;
    }
    jl_cgval_t slot(ctx.builder.getContext());
    PHINode *value_phi = NULL;
    if (vtype->isAggregateType() && CountTrackedPointers(vtype).count == 0) {
        // the value will be moved into dest in the predecessor critical block.
        // here it's moved into phi in the successor (from dest)
        dest = emit_static_alloca(ctx, vtype);
        Value *phi = emit_static_alloca(ctx, vtype);
        ctx.builder.CreateMemCpy(phi, MaybeAlign(julia_alignment(phiType)),
             dest, MaybeAlign(0),
             jl_datatype_size(phiType), false);
        ctx.builder.CreateLifetimeEnd(dest);
        slot = mark_julia_slot(phi, phiType, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
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

static void emit_ssaval_assign(jl_codectx_t &ctx, ssize_t ssaidx_0based, jl_value_t *r)
{
    assert(!ctx.ssavalue_assigned.at(ssaidx_0based));
    if (jl_is_phinode(r)) {
        return emit_phinode_assign(ctx, ssaidx_0based, r);
    }

    jl_cgval_t slot(ctx.builder.getContext());
    if (jl_is_phicnode(r)) {
        auto it = ctx.phic_slots.find(ssaidx_0based);
        if (it == ctx.phic_slots.end()) {
            it = ctx.phic_slots.emplace(ssaidx_0based, jl_varinfo_t(ctx.builder.getContext())).first;
        }
        slot = emit_varinfo(ctx, it->second, jl_symbol("phic"));
    } else {
        slot = emit_expr(ctx, r, ssaidx_0based); // slot could be a jl_value_t (unboxed) or jl_value_t* (ispointer)
    }
    if (slot.isboxed || slot.TIndex) {
        // see if inference suggested a different type for the ssavalue than the expression
        // e.g. sometimes the information is inconsistent after inlining getfield on a Tuple
        jl_value_t *ssavalue_types = (jl_value_t*)ctx.source->ssavaluetypes;
        if (jl_is_array(ssavalue_types)) {
            jl_value_t *declType = jl_array_ptr_ref(ssavalue_types, ssaidx_0based);
            if (declType != slot.typ) {
                slot = update_julia_type(ctx, slot, declType);
            }
        }
    }
    ctx.SAvalues.at(ssaidx_0based) = slot; // now SAvalues[ssaidx_0based] contains the SAvalue
    ctx.ssavalue_assigned.at(ssaidx_0based) = true;
}

static void emit_varinfo_assign(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_cgval_t rval_info, jl_value_t *l=NULL)
{
    if (!vi.used || vi.value.typ == jl_bottom_type)
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
                tindex = ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x7f));
        }
        else {
            assert(rval_info.isboxed || rval_info.constant);
            tindex = compute_tindex_unboxed(ctx, rval_info, vi.value.typ);
            if (vi.boxroot)
                tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
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
                    ctx.builder.CreateAnd(rval_info.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            rval = rval_info.Vboxed ? rval_info.Vboxed : Constant::getNullValue(ctx.types().T_prjlvalue);
            assert(rval->getType() == ctx.types().T_prjlvalue);
            assert(!vi.value.constant);
        }
        else {
            assert(!vi.pTIndex || rval_info.isboxed || rval_info.constant);
            rval = boxed(ctx, rval_info);
        }
        ctx.builder.CreateStore(rval, vi.boxroot, vi.isVolatile);
    }

    // store unboxed variables
    if (!vi.boxroot || (vi.pTIndex && rval_info.TIndex)) {
        emit_vi_assignment_unboxed(ctx, vi, isboxed, rval_info);
    }
}

static void emit_assignment(jl_codectx_t &ctx, jl_value_t *l, jl_value_t *r, ssize_t ssaval)
{
    assert(!jl_is_ssavalue(l));
    jl_cgval_t rval_info = emit_expr(ctx, r, ssaval);

    jl_binding_t *bnd = NULL;
    Value *bp = NULL;
    if (jl_is_symbol(l))
        bp = global_binding_pointer(ctx, ctx.module, (jl_sym_t*)l, &bnd, true); // now bp != NULL or bnd != NULL
    else if (jl_is_globalref(l))
        bp = global_binding_pointer(ctx, jl_globalref_mod(l), jl_globalref_name(l), &bnd, true); // now bp != NULL or bnd != NULL
    else
        assert(jl_is_slot(l));
    if (bp != NULL || bnd != NULL) { // it is a global
        if (bp != NULL) {
            emit_globalset(ctx, bnd, bp, rval_info, AtomicOrdering::Unordered);
            // Global variable. Does not need debug info because the debugger knows about
            // its memory location.
        }
        return;
    }

    int sl = jl_slot_number(l) - 1;
    // it's a local variable
    jl_varinfo_t &vi = ctx.slots[sl];
    emit_varinfo_assign(ctx, vi, rval_info, l);
}

static void emit_upsilonnode(jl_codectx_t &ctx, ssize_t phic, jl_value_t *val)
{
    auto it = ctx.phic_slots.find(phic);
    if (it == ctx.phic_slots.end()) {
        it = ctx.phic_slots.emplace(phic, jl_varinfo_t(ctx.builder.getContext())).first;
    }
    jl_varinfo_t &vi = it->second;
    // If the val is null, we can ignore the store.
    // The middle end guarantees that the value from this
    // upsilon node is not dynamically observed.
    if (val) {
        jl_cgval_t rval_info = emit_expr(ctx, val);
        if (rval_info.typ == jl_bottom_type)
            // as a special case, PhiC nodes are allowed to use undefined
            // values, since they are just copy operations, so we need to
            // ignore the store (it will not by dynamically observed), while
            // normally, for any other operation result, we'd assume this store
            // was unreachable and dead
            val = NULL;
        else
            emit_varinfo_assign(ctx, vi, rval_info);
    }
    if (!val) {
        if (vi.boxroot) {
            // memory optimization: eagerly clear this gc-root now
            ctx.builder.CreateAlignedStore(Constant::getNullValue(ctx.types().T_prjlvalue), vi.boxroot, Align(sizeof(void*)), true);
        }
        if (vi.pTIndex) {
            // We don't care what the contents of the variable are, but it
            // does need to satisfy the union invariants (i.e. inbounds
            // tindex).
            ctx.builder.CreateAlignedStore(
                vi.boxroot ? ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80) :
                             ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x01),
                vi.pTIndex, Align(1), true);
        }
        else if (vi.value.V && !vi.value.constant && vi.value.typ != jl_bottom_type) {
            assert(vi.value.ispointer());
            Type *T = cast<AllocaInst>(vi.value.V)->getAllocatedType();
            if (CountTrackedPointers(T).count) {
                // make sure gc pointers (including ptr_phi of union-split) are initialized to NULL
                ctx.builder.CreateStore(Constant::getNullValue(T), vi.value.V, true);
            }
        }
    }
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
        Value *cond = emit_unbox(ctx, getInt8Ty(ctx.builder.getContext()), condV, (jl_value_t*)jl_bool_type);
        assert(cond->getType() == getInt8Ty(ctx.builder.getContext()));
        return ctx.builder.CreateXor(ctx.builder.CreateTrunc(cond, getInt1Ty(ctx.builder.getContext())), ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1));
    }
    if (condV.isboxed) {
        return ctx.builder.CreateICmpEQ(boxed(ctx, condV),
            track_pjlvalue(ctx, literal_pointer_val(ctx, jl_false)));
    }
    // not a boolean
    return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0); // TODO: replace with Undef
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
    if (jl_is_argument(expr) && ssaval_result == -1) {
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
                ctx.builder.CreateStore(Constant::getNullValue(ctx.types().T_prjlvalue), lv);
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
    if (head == jl_meta_sym || head == jl_inbounds_sym || head == jl_coverageeffect_sym
            || head == jl_aliasscope_sym || head == jl_popaliasscope_sym || head == jl_inline_sym || head == jl_noinline_sym) {
        // some expression types are metadata and can be ignored
        // in statement position
        return;
    }
    else if (head == jl_leave_sym) {
        assert(jl_is_long(args[0]));
        ctx.builder.CreateCall(prepare_call(jlleave_func),
                           ConstantInt::get(getInt32Ty(ctx.builder.getContext()), jl_unbox_long(args[0])));
    }
    else if (head == jl_pop_exception_sym) {
        jl_cgval_t excstack_state = emit_expr(ctx, jl_exprarg(expr, 0));
        assert(excstack_state.V && excstack_state.V->getType() == getSizeTy(ctx.builder.getContext()));
        ctx.builder.CreateCall(prepare_call(jl_restore_excstack_func), excstack_state.V);
        return;
    }
    else {
        if (!jl_is_method(ctx.linfo->def.method) && !ctx.is_opaque_closure) {
            // TODO: inference is invalid if this has any effect (which it often does)
            LoadInst *world = ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()),
                prepare_global_in(jl_Module, jlgetworld_global), Align(sizeof(size_t)));
            world->setOrdering(AtomicOrdering::Acquire);
            ctx.builder.CreateAlignedStore(world, ctx.world_age_field, Align(sizeof(size_t)));
        }
        assert(ssaval_result != -1);
        emit_ssaval_assign(ctx, ssaval_result, expr);
    }
}

static std::pair<Function*, Function*> get_oc_function(jl_codectx_t &ctx, jl_method_t *closure_method, jl_tupletype_t *env_t, jl_tupletype_t *argt_typ, jl_value_t *rettype)
{
    jl_svec_t *sig_args = NULL;
    jl_value_t *sigtype = NULL;
    jl_code_info_t *ir = NULL;
    JL_GC_PUSH3(&sig_args, &sigtype, &ir);

    size_t nsig = 1 + jl_svec_len(argt_typ->parameters);
    sig_args = jl_alloc_svec_uninit(nsig);
    jl_svecset(sig_args, 0, env_t);
    for (size_t i = 0; i < jl_svec_len(argt_typ->parameters); ++i) {
        jl_svecset(sig_args, 1+i, jl_svecref(argt_typ->parameters, i));
    }
    sigtype = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(sig_args), nsig);

    jl_method_instance_t *mi = jl_specializations_get_linfo(closure_method, sigtype, jl_emptysvec);
    jl_code_instance_t *ci = (jl_code_instance_t*)jl_rettype_inferred(mi, ctx.world, ctx.world);

    if (ci == NULL || (jl_value_t*)ci == jl_nothing || ci->inferred == NULL || ci->inferred == jl_nothing) {
        JL_GC_POP();
        return std::make_pair((Function*)NULL, (Function*)NULL);
    }
    ++EmittedOpaqueClosureFunctions;

    ir = jl_uncompress_ir(closure_method, ci, (jl_array_t*)ci->inferred);

    // TODO: Emit this inline and outline it late using LLVM's coroutine support.
    orc::ThreadSafeModule closure_m = jl_create_llvm_module(
            name_from_method_instance(mi), ctx.emission_context.tsctx,
            ctx.emission_context.imaging,
            jl_Module->getDataLayout(), Triple(jl_Module->getTargetTriple()));
    jl_llvm_functions_t closure_decls = emit_function(closure_m, mi, ir, rettype, ctx.emission_context);

    assert(closure_decls.functionObject != "jl_fptr_sparam");
    bool isspecsig = closure_decls.functionObject != "jl_fptr_args";

    Function *F = NULL;
    std::string fname = isspecsig ?
        closure_decls.functionObject :
        closure_decls.specFunctionObject;
    if (GlobalValue *V = jl_Module->getNamedValue(fname)) {
        F = cast<Function>(V);
    } else {
        F = Function::Create(get_func_sig(ctx.builder.getContext()),
                             Function::ExternalLinkage,
                             fname, jl_Module);
        F->setAttributes(get_func_attrs(ctx.builder.getContext()));
    }
    Function *specF = NULL;
    if (!isspecsig) {
        specF = F;
    } else {
        //emission context holds context lock so can get module
        specF = closure_m.getModuleUnlocked()->getFunction(closure_decls.specFunctionObject);
        if (specF) {
            jl_returninfo_t returninfo = get_specsig_function(ctx, jl_Module,
                closure_decls.specFunctionObject, sigtype, rettype, true);
            specF = returninfo.decl;
        }
    }
    ctx.oc_modules.push_back(std::move(closure_m));
    JL_GC_POP();
    return std::make_pair(F, specF);
}

// `expr` is not clobbered in JL_TRY
JL_GCC_IGNORE_START("-Wclobbered")
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaidx_0based)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_global(ctx, sym);
    }
    if (jl_is_slot(expr) || jl_is_argument(expr)) {
        return emit_local(ctx, expr);
    }
    if (jl_is_ssavalue(expr)) {
        ssize_t idx = ((jl_ssavalue_t*)expr)->id - 1;
        assert(idx >= 0);
        if (!ctx.ssavalue_assigned.at(idx)) {
            ctx.ssavalue_assigned.at(idx) = true; // (assignment, not comparison test)
            return jl_cgval_t(ctx.builder.getContext()); // dead code branch
        }
        else {
            return ctx.SAvalues.at(idx); // at this point, SAvalues[idx] actually contains the SAvalue
        }
    }
    if (jl_is_globalref(expr)) {
        return emit_globalref(ctx, jl_globalref_mod(expr), jl_globalref_name(expr), AtomicOrdering::Unordered);
    }
    if (jl_is_linenode(expr)) {
        jl_error("LineNumberNode in value position");
    }
    if (jl_is_gotonode(expr)) {
        jl_error("GotoNode in value position");
    }
    if (jl_is_gotoifnot(expr)) {
        jl_error("GotoIfNot in value position");
    }
    if (jl_is_pinode(expr)) {
        Value *skip = NULL;
        return convert_julia_type(ctx, emit_expr(ctx, jl_fieldref_noalloc(expr, 0)), jl_fieldref_noalloc(expr, 1), &skip);
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
        return mark_julia_const(ctx, expr);
    }

    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    jl_sym_t *head = ex->head;
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (head == jl_isdefined_sym) {
        assert(nargs == 1);
        return emit_isdefined(ctx, args[0]);
    }
    else if (head == jl_throw_undef_if_not_sym) {
        assert(nargs == 2);
        jl_sym_t *var = (jl_sym_t*)args[0];
        Value *cond = ctx.builder.CreateTrunc(emit_unbox(ctx, getInt8Ty(ctx.builder.getContext()), emit_expr(ctx, args[1]), (jl_value_t*)jl_bool_type), getInt1Ty(ctx.builder.getContext()));
        if (var == jl_getfield_undefref_sym) {
            raise_exception_unless(ctx, cond,
                literal_pointer_val(ctx, jl_undefref_exception));
        }
        else {
            undef_var_error_ifnot(ctx, cond, var);
        }
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (head == jl_invoke_sym) {
        assert(ssaidx_0based >= 0);
        jl_value_t *expr_t = jl_is_long(ctx.source->ssavaluetypes) ? (jl_value_t*)jl_any_type :
            jl_array_ptr_ref(ctx.source->ssavaluetypes, ssaidx_0based);
        return emit_invoke(ctx, ex, expr_t);
    }
    else if (head == jl_invoke_modify_sym) {
        assert(ssaidx_0based >= 0);
        jl_value_t *expr_t = jl_is_long(ctx.source->ssavaluetypes) ? (jl_value_t*)jl_any_type :
            jl_array_ptr_ref(ctx.source->ssavaluetypes, ssaidx_0based);
        return emit_invoke_modify(ctx, ex, expr_t);
    }
    else if (head == jl_call_sym) {
        jl_value_t *expr_t;
        bool is_promotable = false;
        if (ssaidx_0based < 0)
            // TODO: this case is needed for the call to emit_expr in emit_llvmcall
            expr_t = (jl_value_t*)jl_any_type;
        else {
            expr_t = jl_is_long(ctx.source->ssavaluetypes) ? (jl_value_t*)jl_any_type : jl_array_ptr_ref(ctx.source->ssavaluetypes, ssaidx_0based);
            is_promotable = ctx.ssavalue_usecount[ssaidx_0based] == 1;
        }
        jl_cgval_t res = emit_call(ctx, ex, expr_t, is_promotable);
        // some intrinsics (e.g. typeassert) can return a wider type
        // than what's actually possible
        if (is_promotable && res.promotion_point) {
            res.promotion_ssa = ssaidx_0based;
        }
        res = update_julia_type(ctx, res, expr_t);
        if (res.typ == jl_bottom_type || expr_t == jl_bottom_type) {
            CreateTrap(ctx.builder);
        }
        return res;
    }
    else if (head == jl_foreigncall_sym) {
        return emit_ccall(ctx, args, jl_array_dim0(ex->args));
    }
    else if (head == jl_cfunction_sym) {
        assert(nargs == 5);
        jl_cgval_t fexpr_rt = emit_expr(ctx, args[1]);
        return emit_cfunction(ctx, args[0], fexpr_rt, args[2], (jl_svec_t*)args[3]);
    }
    else if (head == jl_assign_sym) {
        assert(nargs == 2);
        emit_assignment(ctx, args[0], args[1], ssaidx_0based);
        return ghostValue(ctx, jl_nothing_type);
    }
    else if (head == jl_static_parameter_sym) {
        assert(nargs == 1);
        return emit_sparam(ctx, jl_unbox_long(args[0]) - 1);
    }
    else if (head == jl_method_sym) {
        if (nargs == 1) {
            jl_value_t *mn = args[0];
            assert(jl_is_symbol(mn) || jl_is_slot(mn));

            Value *bp = NULL, *name, *bp_owner = Constant::getNullValue(ctx.types().T_pjlvalue);
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
                    return ghostValue(ctx, jl_nothing_type);
                }
                bp = julia_binding_gv(ctx, bnd);
                bp = julia_binding_pvalue(ctx, bp);
                bp_owner = literal_pointer_val(ctx, (jl_value_t*)mod);
            }
            else if (jl_is_slot(mn) || jl_is_argument(mn)) {
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
                return gf;
            }
            emit_error(ctx, "method: invalid declaration");
            return jl_cgval_t(ctx.builder.getContext());
        }
        assert(nargs == 3);
        Value *a1 = boxed(ctx, emit_expr(ctx, args[1]));
        Value *a2 = boxed(ctx, emit_expr(ctx, args[2]));
        Value *mdargs[4] = {
            /*argdata*/a1,
            ConstantPointerNull::get(cast<PointerType>(ctx.types().T_prjlvalue)),
            /*code*/a2,
            /*module*/literal_pointer_val(ctx, (jl_value_t*)ctx.module)
        };
        jl_cgval_t meth = mark_julia_type(
            ctx,
            ctx.builder.CreateCall(prepare_call(jlmethod_func), makeArrayRef(mdargs)),
            true,
            jl_method_type);
        return meth;
    }
    else if (head == jl_const_sym) {
        assert(nargs == 1);
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx.module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (jl_is_symbol(sym)) {
            jl_binding_t *bnd = NULL;
            Value *bp = global_binding_pointer(ctx, mod, sym, &bnd, true);
            if (bp)
                ctx.builder.CreateCall(prepare_call(jldeclareconst_func), bp);
        }
    }
    else if (head == jl_new_sym) {
        bool is_promotable = false;
        if (ssaidx_0based >= 0) {
            is_promotable = ctx.ssavalue_usecount[ssaidx_0based] == 1;
        }
        assert(nargs > 0);
        jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        jl_value_t *ty = argv[0].typ;
        if (jl_is_type_type(ty) &&
                jl_is_datatype(jl_tparam0(ty)) &&
                jl_is_concrete_type(jl_tparam0(ty))) {
            assert(nargs <= jl_datatype_nfields(jl_tparam0(ty)) + 1);
            jl_cgval_t res = emit_new_struct(ctx, jl_tparam0(ty), nargs - 1, &argv[1], is_promotable);
            if (is_promotable && res.promotion_point && res.promotion_ssa==-1)
                res.promotion_ssa = ssaidx_0based;
            return res;
        }
        Value *val = emit_jlcall(ctx, jlnew_func, nullptr, argv, nargs, JLCALL_F_CC);
        // temporarily mark as `Any`, expecting `emit_ssaval_assign` to update
        // it to the inferred type.
        return mark_julia_type(ctx, val, true, (jl_value_t*)jl_any_type);
    }
    else if (head == jl_splatnew_sym) {
        jl_cgval_t argv[2] = {jl_cgval_t(ctx.builder.getContext()), jl_cgval_t(ctx.builder.getContext())};
        assert(nargs == 2);
        argv[0] = emit_expr(ctx, args[0]);
        argv[1] = emit_expr(ctx, args[1]);
        Value *typ = boxed(ctx, argv[0]);
        Value *tup = boxed(ctx, argv[1]);
        Value *val = ctx.builder.CreateCall(prepare_call(jlsplatnew_func), { typ, tup });
        // temporarily mark as `Any`, expecting `emit_ssaval_assign` to update
        // it to the inferred type.
        return mark_julia_type(ctx, val, true, (jl_value_t*)jl_any_type);
    }
    else if (head == jl_new_opaque_closure_sym) {
        assert(nargs >= 4 && "Not enough arguments in new_opaque_closure");
        SmallVector<jl_cgval_t, 4> argv(nargs, jl_cgval_t(ctx.builder.getContext()));
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        const jl_cgval_t &argt = argv[0];
        const jl_cgval_t &lb = argv[1];
        const jl_cgval_t &ub = argv[2];
        const jl_cgval_t &source = argv[3];
        if (source.constant == NULL) {
            // For now, we require non-constant source to be handled by using
            // eval. This should probably be a verifier error and an abort here.
            emit_error(ctx, "(internal error) invalid IR: opaque closure source be constant");
            return jl_cgval_t(ctx.builder.getContext());
        }
        bool can_optimize = argt.constant != NULL && lb.constant != NULL && ub.constant != NULL &&
            jl_is_tuple_type(argt.constant) &&
            jl_is_type(lb.constant) && jl_is_type(ub.constant) && jl_is_method(source.constant) &&
            ((jl_method_t*)source.constant)->nargs > 0 &&
            jl_is_valid_oc_argtype((jl_tupletype_t*)argt.constant, (jl_method_t*)source.constant);

        if (can_optimize) {
            jl_value_t *closure_t = NULL;
            jl_tupletype_t *env_t = NULL;
            JL_GC_PUSH2(&closure_t, &env_t);

            jl_value_t **env_component_ts = (jl_value_t**)alloca(sizeof(jl_value_t*) * (nargs-4));
            for (size_t i = 0; i < nargs - 4; ++i) {
                env_component_ts[i] = argv[4+i].typ;
            }

            env_t = jl_apply_tuple_type_v(env_component_ts, nargs-4);
            // we need to know the full env type to look up the right specialization
            if (jl_is_concrete_type((jl_value_t*)env_t)) {
                jl_tupletype_t *argt_typ = (jl_tupletype_t*)argt.constant;
                Function *F, *specF;
                std::tie(F, specF) = get_oc_function(ctx, (jl_method_t*)source.constant, env_t, argt_typ, ub.constant);
                if (F) {
                    jl_cgval_t jlcall_ptr = mark_julia_type(ctx, F, false, jl_voidpointer_type);
                    jl_cgval_t world_age = mark_julia_type(ctx,
                                      tbaa_decorate(ctx.tbaa().tbaa_gcframe,
                                      ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()), ctx.world_age_field, Align(sizeof(size_t)))),
                        false,
                        jl_long_type);
                    jl_cgval_t fptr(ctx.builder.getContext());
                    if (specF)
                        fptr = mark_julia_type(ctx, specF, false, jl_voidpointer_type);
                    else
                        fptr = mark_julia_type(ctx, (llvm::Value*)Constant::getNullValue(getSizeTy(ctx.builder.getContext())), false, jl_voidpointer_type);

                    // TODO: Inline the env at the end of the opaque closure and generate a descriptor for GC
                    jl_cgval_t env = emit_new_struct(ctx, (jl_value_t*)env_t, nargs-4, &argv.data()[4]);

                    jl_cgval_t closure_fields[5] = {
                        env,
                        world_age,
                        source,
                        jlcall_ptr,
                        fptr
                    };

                    closure_t = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt_typ, ub.constant);
                    jl_cgval_t ret = emit_new_struct(ctx, closure_t, 5, closure_fields);

                    JL_GC_POP();
                    return ret;
                }
            }
            JL_GC_POP();
        }

        return mark_julia_type(ctx,
                emit_jlcall(ctx, jl_new_opaque_closure_jlcall_func, Constant::getNullValue(ctx.types().T_prjlvalue), argv.data(), nargs, JLCALL_F_CC),
                true, jl_any_type);
    }
    else if (head == jl_exc_sym) {
        assert(nargs == 0);
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jl_current_exception_func)),
                true, jl_any_type);
    }
    else if (head == jl_copyast_sym) {
        assert(nargs == 1);
        jl_cgval_t ast = emit_expr(ctx, args[0]);
        if (ast.typ != (jl_value_t*)jl_expr_type && ast.typ != (jl_value_t*)jl_any_type) {
            // elide call to jl_copy_ast when possible
            return ast;
        }
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jlcopyast_func),
                                       boxed(ctx, ast)), true, jl_expr_type);
    }
    else if (head == jl_loopinfo_sym) {
        // parse Expr(:loopinfo, "julia.simdloop", ("llvm.loop.vectorize.width", 4))
        SmallVector<Metadata *, 8> MDs;
        for (int i = 0, ie = nargs; i < ie; ++i) {
            Metadata *MD = to_md_tree(args[i], ctx.builder.getContext());
            if (MD)
                MDs.push_back(MD);
        }

        MDNode* MD = MDNode::get(ctx.builder.getContext(), MDs);
        CallInst *I = ctx.builder.CreateCall(prepare_call(jl_loopinfo_marker_func));
        I->setMetadata("julia.loopinfo", MD);
        return jl_cgval_t(ctx.builder.getContext());
    }
    else if (head == jl_leave_sym || head == jl_coverageeffect_sym
            || head == jl_pop_exception_sym || head == jl_enter_sym || head == jl_inbounds_sym
            || head == jl_aliasscope_sym || head == jl_popaliasscope_sym || head == jl_inline_sym || head == jl_noinline_sym) {
        jl_errorf("Expr(:%s) in value position", jl_symbol_name(head));
    }
    else if (head == jl_boundscheck_sym) {
        return mark_julia_const(ctx, bounds_check_enabled(ctx, jl_true) ? jl_true : jl_false);
    }
    else if (head == jl_gc_preserve_begin_sym) {
        jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        std::vector<Value*> vals;
        for (size_t i = 0; i < nargs; ++i) {
            const jl_cgval_t &ai = argv[i];
            if (ai.constant || ai.typ == jl_bottom_type)
                continue;
            if (ai.isboxed) {
                vals.push_back(ai.Vboxed);
            }
            else if (jl_is_concrete_immutable(ai.typ) && !jl_is_pointerfree(ai.typ)) {
                Type *at = julia_type_to_llvm(ctx, ai.typ);
                vals.push_back(emit_unbox(ctx, at, ai, ai.typ));
            }
        }
        Value *token = vals.empty()
            ? (Value*)ConstantTokenNone::get(ctx.builder.getContext())
            : ctx.builder.CreateCall(prepare_call(gc_preserve_begin_func), vals);
        jl_cgval_t tok(token, NULL, false, (jl_value_t*)jl_nothing_type, NULL, ctx.tbaa());
        return tok;
    }
    else if (head == jl_gc_preserve_end_sym) {
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
            return ghostValue(ctx, jl_nothing_type);
        }
        jl_errorf("unsupported or misplaced expression \"%s\" in function %s",
                  jl_symbol_name(head), ctx.name);
    }
    return jl_cgval_t(ctx.builder.getContext());
}
JL_GCC_IGNORE_STOP

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0)
{
    // TODO: requires the runtime, but is generated unconditionally
    // allocate a placeholder gc instruction
    ctx.pgcstack = ctx.builder.CreateCall(prepare_call(jlpgcstack_func));
}

static Value *get_current_task(jl_codectx_t &ctx)
{
    const int ptls_offset = offsetof(jl_task_t, gcstack);
    return ctx.builder.CreateInBoundsGEP(
        ctx.types().T_pjlvalue, emit_bitcast(ctx, ctx.pgcstack, ctx.types().T_ppjlvalue),
        ConstantInt::get(getSizeTy(ctx.builder.getContext()), -(ptls_offset / sizeof(void *))),
        "current_task");
}

// Get PTLS through current task.
static Value *get_current_ptls(jl_codectx_t &ctx)
{
    return get_current_ptls_from_task(ctx.builder, get_current_task(ctx), ctx.tbaa().tbaa_gcframe);
}

// Store world age at the entry block of the function. This function should be
// called right after `allocate_gc_frame` and there should be no context switch.
static void emit_last_age_field(jl_codectx_t &ctx)
{
    auto ptls = get_current_task(ctx);
    assert(ctx.builder.GetInsertBlock() == ctx.pgcstack->getParent());
    ctx.world_age_field = ctx.builder.CreateInBoundsGEP(
            getSizeTy(ctx.builder.getContext()),
            ctx.builder.CreateBitCast(ptls, getSizePtrTy(ctx.builder.getContext())),
            ConstantInt::get(getSizeTy(ctx.builder.getContext()), offsetof(jl_task_t, world_age) / sizeof(size_t)),
            "world_age");
}

// Get signal page through current task.
static Value *get_current_signal_page(jl_codectx_t &ctx)
{
    // return ctx.builder.CreateCall(prepare_call(reuse_signal_page_func));
    auto ptls = get_current_ptls(ctx);
    int nthfield = offsetof(jl_tls_states_t, safepoint) / sizeof(void *);
    return emit_nthptr_recast(ctx, ptls, nthfield, ctx.tbaa().tbaa_const, getSizePtrTy(ctx.builder.getContext()));
}

static Function *emit_tojlinvoke(jl_code_instance_t *codeinst, Module *M, jl_codegen_params_t &params)
{
    ++EmittedToJLInvokes;
    jl_codectx_t ctx(M->getContext(), params);
    std::string name;
    raw_string_ostream(name) << "tojlinvoke" << globalUniqueGeneratedNames++;
    Function *f = Function::Create(ctx.types().T_jlfunc,
            GlobalVariable::InternalLinkage,
            name, M);
    jl_init_function(f);
    f->addFnAttr(Attribute::get(M->getContext(), "thunk"));
    //f->setAlwaysInline();
    ctx.f = f; // for jl_Module
    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", f);
    ctx.builder.SetInsertPoint(b0);
    Function *theFunc;
    Value *theFarg;
    auto invoke = jl_atomic_load_relaxed(&codeinst->invoke);
    if (params.cache && invoke != NULL) {
        StringRef theFptrName = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)invoke, codeinst);
        theFunc = cast<Function>(
            M->getOrInsertFunction(theFptrName, jlinvoke_func->_type(ctx.builder.getContext())).getCallee());
        theFarg = literal_pointer_val(ctx, (jl_value_t*)codeinst);
    }
    else {
        theFunc = prepare_call(jlinvoke_func);
        theFarg = literal_pointer_val(ctx, (jl_value_t*)codeinst->def);
    }
    theFarg = track_pjlvalue(ctx, theFarg);
    auto args = f->arg_begin();
    CallInst *r = ctx.builder.CreateCall(theFunc, { &*args, &*++args, &*++args, theFarg });
    r->setAttributes(theFunc->getAttributes());
    ctx.builder.CreateRet(r);
    return f;
}

static Type *get_returnroots_type(jl_codectx_t &ctx, unsigned rootcount) {
    return ArrayType::get(ctx.types().T_prjlvalue, rootcount);
}

static Type *get_unionbytes_type(LLVMContext &C, unsigned unionbytes) {
    return ArrayType::get(getInt8Ty(C), unionbytes);
}

static void emit_cfunc_invalidate(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype,
        size_t nargs,
        jl_codegen_params_t &params,
        Function *target)
{
    ++EmittedCFuncInvalidates;
    jl_codectx_t ctx(gf_thunk->getParent()->getContext(), params);
    ctx.f = gf_thunk;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", gf_thunk);
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
        Type *et = isboxed ?  ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (is_uniquerep_Type(jt)) {
            myargs[i] = mark_julia_const(ctx, jl_tparam0(jt));
        }
        else if (type_is_ghost(et)) {
            assert(jl_is_datatype(jt) && ((jl_datatype_t*)jt)->instance);
            myargs[i] = mark_julia_const(ctx, ((jl_datatype_t*)jt)->instance);
        }
        else {
            Value *arg_v = &*AI;
            ++AI;
            Type *at = arg_v->getType();
            if (!isboxed && et->isAggregateType()) {
                myargs[i] = mark_julia_slot(arg_v, jt, NULL, ctx.tbaa(), ctx.tbaa().tbaa_const);
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
            ctx.builder.CreateRet(ctx.builder.CreateAlignedLoad(gfrt, gf_ret, Align(julia_alignment(rettype))));
        }
        break;
    }
    case jl_returninfo_t::SRet: {
        if (return_roots) {
            Value *root1 = gf_thunk->arg_begin() + 1; // root1 has type [n x {}*]*
            assert(cast<PointerType>(root1->getType())->isOpaqueOrPointeeTypeMatches(get_returnroots_type(ctx, return_roots)));
            root1 = ctx.builder.CreateConstInBoundsGEP2_32(get_returnroots_type(ctx, return_roots), root1, 0, 0);
            ctx.builder.CreateStore(gf_ret, root1);
        }
        emit_memcpy(ctx, &*gf_thunk->arg_begin(), nullptr, gf_ret, nullptr, jl_datatype_size(rettype), julia_alignment(rettype));
        ctx.builder.CreateRetVoid();
        break;
    }
    case jl_returninfo_t::Union: {
        Type *retty = gf_thunk->getReturnType();
        Value *gf_retval = UndefValue::get(retty);
        Value *tindex = compute_box_tindex(ctx, emit_typeof_boxed(ctx, gf_retbox), (jl_value_t*)jl_any_type, rettype);
        tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
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

static void emit_cfunc_invalidate(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype,
        size_t nargs,
        jl_codegen_params_t &params)
{
    emit_cfunc_invalidate(gf_thunk, cc, return_roots, calltype, rettype, nargs, params,
        prepare_call_in(gf_thunk->getParent(), jlapplygeneric_func));
}

static Function* gen_cfun_wrapper(
    Module *into, jl_codegen_params_t &params,
    const function_sig_t &sig, jl_value_t *ff, const char *aliasname,
    jl_value_t *declrt, jl_method_instance_t *lam,
    jl_unionall_t *unionall_env, jl_svec_t *sparam_vals, jl_array_t **closure_types)
{
    ++GeneratedCFuncWrappers;
    // Generate a c-callable wrapper
    assert(into);
    size_t nargs = sig.nccallargs;
    const char *name = "cfunction";
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_code_instance_t *codeinst = NULL;
    bool nest = (!ff || unionall_env);
    jl_value_t *astrt = (jl_value_t*)jl_any_type;
    void *callptr = NULL;
    int calltype = 0;
    if (aliasname)
        name = aliasname;
    else if (lam)
        name = jl_symbol_name(lam->def.method->name);
    if (lam && params.cache) {
        // TODO: this isn't ideal to be unconditionally calling type inference (and compile) from here
        codeinst = jl_compile_method_internal(lam, world);
        assert(codeinst->invoke);
        if (codeinst->invoke == jl_fptr_args_addr) {
            callptr = codeinst->specptr.fptr;
            calltype = 1;
        }
        else if (codeinst->invoke == jl_fptr_const_return_addr) {
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

    std::string funcName;
    raw_string_ostream(funcName) << "jlcapi_" << name << "_" << globalUniqueGeneratedNames++;

    Module *M = into; // Safe because ctx lock is held by params
    AttributeList attributes = sig.attributes;
    FunctionType *functype;
    if (nest) {
        // add nest parameter (pointer to jl_value_t* data array) after sret arg
        assert(closure_types);
        std::vector<Type*> fargt_sig(sig.fargt_sig);

        fargt_sig.insert(fargt_sig.begin() + sig.sret, JuliaType::get_pprjlvalue_ty(M->getContext()));

        // Shift LLVM attributes for parameters one to the right, as
        // we are adding the extra nest parameter after sret arg.
        std::vector<std::pair<unsigned, AttributeSet>> newAttributes;
        newAttributes.reserve(attributes.getNumAttrSets() + 1);
#if JL_LLVM_VERSION >= 140000
        auto it = *attributes.indexes().begin();
        const auto it_end = *attributes.indexes().end();
#else
        auto it = attributes.index_begin();
        const auto it_end = attributes.index_end();
#endif

        // Skip past FunctionIndex
        if (it == AttributeList::AttrIndex::FunctionIndex) {
            ++it;
        }

        // Move past ReturnValue and parameter return value
        for (;it < AttributeList::AttrIndex::FirstArgIndex + sig.sret; ++it) {
            if (hasAttributesAtIndex(attributes, it)) {
                newAttributes.emplace_back(it, attributes.getAttributes(it));
            }
        }

        // Add the new nest attribute
#if JL_LLVM_VERSION >= 140000
        AttrBuilder attrBuilder(M->getContext());
#else
        AttrBuilder attrBuilder;
#endif
        attrBuilder.addAttribute(Attribute::Nest);
        newAttributes.emplace_back(it, AttributeSet::get(M->getContext(), attrBuilder));

        // Shift forward the rest of the attributes
        if (attributes.getNumAttrSets() > 0) { // without this check the loop range below is invalid
            for(; it != it_end; ++it) {
                if (hasAttributesAtIndex(attributes, it)) {
                    newAttributes.emplace_back(it + 1, attributes.getAttributes(it));
                }
            }
        }

        // Remember to add back FunctionIndex
        if (hasAttributesAtIndex(attributes, AttributeList::AttrIndex::FunctionIndex)) {
            newAttributes.emplace_back(AttributeList::AttrIndex::FunctionIndex,
                                       getFnAttrs(attributes));
        }

        // Create the new AttributeList
        attributes = AttributeList::get(M->getContext(), newAttributes);
        functype = FunctionType::get(sig.sret ? getVoidTy(M->getContext()) : sig.prt, fargt_sig, /*isVa*/false);
    }
    else {
        functype = sig.functype(M->getContext());
    }
    Function *cw = Function::Create(functype,
            GlobalVariable::ExternalLinkage,
            funcName, M);
    cw->setAttributes(attributes);
    jl_init_function(cw);

    jl_codectx_t ctx(M->getContext(), params);
    ctx.f = cw;
    ctx.world = world;
    ctx.name = name;
    ctx.funcName = name;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", cw);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);
    emit_last_age_field(ctx);

    Value *dummy_world = ctx.builder.CreateAlloca(getSizeTy(ctx.builder.getContext()));
    Value *have_tls = ctx.builder.CreateIsNotNull(ctx.pgcstack);
    // TODO: in the future, try to initialize a full TLS context here
    // for now, just use a dummy field to avoid a branch in this function
    ctx.world_age_field = ctx.builder.CreateSelect(have_tls, ctx.world_age_field, dummy_world);
    Value *last_age = tbaa_decorate(ctx.tbaa().tbaa_gcframe, ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()), ctx.world_age_field, Align(sizeof(size_t))));
    Value *world_v = ctx.builder.CreateAlignedLoad(getSizeTy(ctx.builder.getContext()),
        prepare_global_in(jl_Module, jlgetworld_global), Align(sizeof(size_t)));
    cast<LoadInst>(world_v)->setOrdering(AtomicOrdering::Acquire);

    Value *age_ok = NULL;
    if (calltype) {
        LoadInst *lam_max = ctx.builder.CreateAlignedLoad(
                getSizeTy(ctx.builder.getContext()),
                ctx.builder.CreateConstInBoundsGEP1_32(
                    getSizeTy(ctx.builder.getContext()),
                    emit_bitcast(ctx, literal_pointer_val(ctx, (jl_value_t*)codeinst), getSizePtrTy(ctx.builder.getContext())),
                    offsetof(jl_code_instance_t, max_world) / sizeof(size_t)),
                Align(sizeof(size_t)));
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
        inputargs[0] = mark_julia_const(ctx, ff);
    }
    else {
        assert(nest && nestPtr);
        Value *ff = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, nestPtr, Align(sizeof(void*)));
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
                        ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, emit_bitcast(ctx, val, ctx.types().T_pprjlvalue), Align(sizeof(void*))),
                        true, jl_any_type);
            }
            else if (static_at && jl_is_concrete_immutable(jargty)) { // anything that could be stored unboxed
                bool isboxed;
                Type *T = julia_type_to_llvm(ctx, jargty, &isboxed);
                assert(!isboxed);
                // a T* (of unknown origin)
                if (type_is_ghost(T)) {
                    inputarg = ghostValue(ctx, jargty);
                }
                else {
                    val = emit_bitcast(ctx, val, T->getPointerTo());
                    val = ctx.builder.CreateAlignedLoad(T, val, Align(1)); // make no alignment assumption about pointer from C
                    inputarg = mark_julia_type(ctx, val, false, jargty);
                }
            }
            else if (static_at || (!jl_is_typevar(jargty) && !jl_is_immutable_datatype(jargty))) {
                // must be a jl_value_t* (because it's mutable or contains gc roots)
                inputarg = mark_julia_type(ctx, maybe_decay_untracked(ctx, emit_bitcast(ctx, val, ctx.types().T_prjlvalue)), true, jargty_proper);
            }
            else {
                // allocate val into a new box, if it might not be boxed
                // otherwise preserve / reuse the existing box identity
                // TODO: could inspect `jargty` and eliminate some of these cases
                if (!*closure_types)
                    *closure_types = jl_alloc_vec_any(0);
                jl_array_ptr_1d_push(*closure_types, jargty);
                Value *runtime_dt = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                        ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, nestPtr, jl_array_len(*closure_types)),
                        Align(sizeof(void*)));
                BasicBlock *boxedBB = BasicBlock::Create(ctx.builder.getContext(), "isboxed", cw);
                BasicBlock *loadBB = BasicBlock::Create(ctx.builder.getContext(), "need-load", cw);
                BasicBlock *unboxedBB = BasicBlock::Create(ctx.builder.getContext(), "maybe-unboxed", cw);
                BasicBlock *isanyBB = BasicBlock::Create(ctx.builder.getContext(), "any", cw);
                BasicBlock *afterBB = BasicBlock::Create(ctx.builder.getContext(), "after", cw);
                Value *isrtboxed = ctx.builder.CreateIsNull(val);
                ctx.builder.CreateCondBr(isrtboxed, boxedBB, loadBB);
                ctx.builder.SetInsertPoint(boxedBB);
                Value *p1 = ctx.builder.CreateBitCast(val, ctx.types().T_pjlvalue);
                p1 = track_pjlvalue(ctx, p1);
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(loadBB);
                Value *isrtany = ctx.builder.CreateICmpEQ(
                        literal_pointer_val(ctx, (jl_value_t*)jl_any_type),
                        ctx.builder.CreateBitCast(val, ctx.types().T_pjlvalue));
                ctx.builder.CreateCondBr(isrtany, isanyBB, unboxedBB);
                ctx.builder.SetInsertPoint(isanyBB);
                Value *p2 = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, ctx.builder.CreateBitCast(val, ctx.types().T_pprjlvalue), Align(sizeof(void*)));
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(unboxedBB);
                Value *p3 = emit_new_bits(ctx, runtime_dt, val);
                unboxedBB = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(afterBB);
                PHINode *p = ctx.builder.CreatePHI(ctx.types().T_prjlvalue, 3);
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
                    val = ctx.builder.CreateAlignedLoad(sig.fargt[i], val, Align(1)); // unknown alignment from C
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
                    Value *runtime_dt = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                            ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, nestPtr, jl_array_len(*closure_types)),
                            Align(sizeof(void*)));
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
    jl_cgval_t retval(ctx.builder.getContext());
    if (calltype == 2) {
        nargs = 0; // arguments not needed -- TODO: not really true, should emit an age_ok test and jlcall
        jlfunc_sret = false;
        retval = mark_julia_const(ctx, (jl_value_t*)callptr);
    }
    else if (calltype == 0 || calltype == 1) {
        // emit a jlcall
        jlfunc_sret = false;
        Function *theFptr = NULL;
        if (calltype == 1) {
            StringRef fname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)callptr, codeinst);
            theFptr = cast_or_null<Function>(jl_Module->getNamedValue(fname));
            if (!theFptr) {
                theFptr = Function::Create(ctx.types().T_jlfunc, GlobalVariable::ExternalLinkage,
                                           fname, jl_Module);
                jl_init_function(theFptr);
            }
            else {
                assert(theFptr->getFunctionType() == ctx.types().T_jlfunc);
            }
            addRetAttr(theFptr, Attribute::NonNull);
            theFptr->addFnAttr(Attribute::get(ctx.builder.getContext(), "thunk"));
        }
        BasicBlock *b_generic, *b_jlcall, *b_after;
        Value *ret_jlcall;
        if (age_ok) {
            assert(theFptr);
            b_generic = BasicBlock::Create(ctx.builder.getContext(), "generic", cw);
            b_jlcall = BasicBlock::Create(ctx.builder.getContext(), "apply", cw);
            b_after = BasicBlock::Create(ctx.builder.getContext(), "after", cw);
            ctx.builder.CreateCondBr(age_ok, b_jlcall, b_generic);
            ctx.builder.SetInsertPoint(b_jlcall);
            // for jlcall, we need to pass the function object even if it is a ghost.
            Value *theF = boxed(ctx, inputargs[0]);
            assert(theF);
            ret_jlcall = emit_jlcall(ctx, theFptr, theF, &inputargs[1], nargs, JLCALL_F_CC);
            ctx.builder.CreateBr(b_after);
            ctx.builder.SetInsertPoint(b_generic);
        }
        Value *ret = emit_jlcall(ctx, jlapplygeneric_func, NULL, inputargs, nargs + 1, JLCALL_F_CC);
        if (age_ok) {
            ctx.builder.CreateBr(b_after);
            ctx.builder.SetInsertPoint(b_after);
            PHINode *retphi = ctx.builder.CreatePHI(ctx.types().T_prjlvalue, 2);
            retphi->addIncoming(ret_jlcall, b_jlcall);
            retphi->addIncoming(ret, b_generic);
            ret = retphi;
        }
        retval = mark_julia_type(ctx, ret, true, astrt);
    }
    else {
        bool is_opaque_closure = jl_is_method(lam->def.value) && lam->def.method->is_for_opaque_closure;
        assert(calltype == 3);
        // emit a specsig call
        StringRef protoname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)callptr, codeinst);
        jl_returninfo_t returninfo = get_specsig_function(ctx, M, protoname, lam->specTypes, astrt, is_opaque_closure);
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
                if (jlfunc_sret) {
                    result = emit_static_alloca(ctx, getAttributeAtIndex(returninfo.decl->getAttributes(), 1, Attribute::StructRet).getValueAsType());
                    assert(cast<PointerType>(result->getType())->hasSameElementTypeAs(cast<PointerType>(cft->getParamType(0))));
                } else {
                    result = emit_static_alloca(ctx, get_unionbytes_type(ctx.builder.getContext(), returninfo.union_bytes));
                    assert(cast<PointerType>(result->getType())->hasSameElementTypeAs(cast<PointerType>(cft->getParamType(0))));
                }
            }
            args.push_back(result);
        }
        if (returninfo.return_roots) {
            AllocaInst *return_roots = emit_static_alloca(ctx, get_returnroots_type(ctx, returninfo.return_roots));
            args.push_back(return_roots);
        }
        for (size_t i = 0; i < nargs + 1; i++) {
            // figure out how to repack the arguments
            jl_cgval_t &inputarg = inputargs[i];
            Value *arg;
            jl_value_t *spect = (i == 0 && is_opaque_closure) ? (jl_value_t*)jl_any_type :
                jl_nth_slot_type(lam->specTypes, i);
            bool isboxed = deserves_argbox(spect);
            Type *T = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, spect);
            if (is_uniquerep_Type(spect)) {
                continue;
            }
            else if (isboxed) {
                arg = boxed(ctx, inputarg);
            }
            else if (type_is_ghost(T)) {
                continue; // ghost types are skipped by the specsig method signature
            }
            else if (T->isAggregateType()) {
                // aggregate types are passed by pointer
                inputarg = value_to_pointer(ctx, inputarg);
                arg = maybe_bitcast(ctx, decay_derived(ctx, data_pointer(ctx, inputarg)),
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
            funcName += "_gfthunk";
            Function *gf_thunk = Function::Create(returninfo.decl->getFunctionType(),
                    GlobalVariable::InternalLinkage, funcName, M);
            gf_thunk->setAttributes(returninfo.decl->getAttributes());
            jl_init_function(gf_thunk);
            // build a  specsig -> jl_apply_generic converter thunk
            // this builds a method that calls jl_apply_generic (as a closure over a singleton function pointer),
            // but which has the signature of a specsig
            emit_cfunc_invalidate(gf_thunk, returninfo.cc, returninfo.return_roots, lam->specTypes, codeinst->rettype, nargs + 1, ctx.emission_context);
            theFptr = ctx.builder.CreateSelect(age_ok, theFptr, gf_thunk);
        }
        assert(cast<PointerType>(theFptr->getType())->isOpaqueOrPointeeTypeMatches(returninfo.decl->getFunctionType()));
        CallInst *call = ctx.builder.CreateCall(
            cast<FunctionType>(returninfo.decl->getFunctionType()),
            theFptr, ArrayRef<Value*>(args));
        call->setAttributes(returninfo.decl->getAttributes());
        switch (returninfo.cc) {
            case jl_returninfo_t::Boxed:
                retval = mark_julia_type(ctx, call, true, astrt);
                break;
            case jl_returninfo_t::Register:
                retval = mark_julia_type(ctx, call, false, astrt);
                break;
            case jl_returninfo_t::SRet:
                retval = mark_julia_slot(result, astrt, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
                break;
            case jl_returninfo_t::Union: {
                Value *box = ctx.builder.CreateExtractValue(call, 0);
                Value *tindex = ctx.builder.CreateExtractValue(call, 1);
                Value *derived = ctx.builder.CreateSelect(
                    ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                            ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0)),
                    decay_derived(ctx, ctx.builder.CreateBitCast(result, ctx.types().T_pjlvalue)),
                    decay_derived(ctx, box));
                retval = mark_julia_slot(derived,
                                         astrt,
                                         tindex,
                                         ctx.tbaa(),
                                         ctx.tbaa().tbaa_stack);
                assert(box->getType() == ctx.types().T_prjlvalue);
                retval.Vboxed = box;
                break;
            }
            case jl_returninfo_t::Ghosts:
                retval = mark_julia_slot(NULL, astrt, call, ctx.tbaa(), ctx.tbaa().tbaa_stack);
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

    if (aliasname) {
        GlobalAlias::create(cw->getValueType(), cw->getType()->getAddressSpace(),
                            GlobalValue::ExternalLinkage, aliasname, cw, M);
    }

    if (nest) {
        funcName += "make";
        Function *cw_make = Function::Create(
                FunctionType::get(getInt8PtrTy(ctx.builder.getContext()), { getInt8PtrTy(ctx.builder.getContext()), ctx.types().T_ppjlvalue }, false),
                GlobalVariable::ExternalLinkage,
                funcName, M);
        jl_init_function(cw_make);
        BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", cw_make);
        IRBuilder<> cwbuilder(b0);
        Function::arg_iterator AI = cw_make->arg_begin();
        Argument *Tramp = &*AI; ++AI;
        Argument *NVal = &*AI; ++AI;
        Function *init_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::init_trampoline);
        Function *adjust_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::adjust_trampoline);
        cwbuilder.CreateCall(init_trampoline, {
                Tramp,
                cwbuilder.CreateBitCast(cw, getInt8PtrTy(ctx.builder.getContext())),
                cwbuilder.CreateBitCast(NVal, getInt8PtrTy(ctx.builder.getContext()))
            });
        cwbuilder.CreateRet(cwbuilder.CreateCall(adjust_trampoline, { Tramp }));
        cw = cw_make;
    }

    return cw;
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
            return jl_cgval_t(ctx.builder.getContext());
        }
        if (unionall_env)
            declrt = jl_rewrap_unionall(declrt, (jl_value_t*)unionall_env);
        rt = (jl_value_t*)jl_any_type; // convert return type to jl_value_t*
    }

    // some sanity checking and check whether there's a vararg
    size_t nargt = jl_svec_len(argt);
    bool isVa = (nargt > 0 && jl_is_vararg(jl_svecref(argt, nargt - 1)));
    assert(!isVa); (void)isVa;

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
            lrt, ctx.builder.getContext(),
            retboxed, static_rt);
    if (!err.empty()) {
        emit_error(ctx, "cfunction " + err);
        JL_GC_POP();
        return jl_cgval_t(ctx.builder.getContext());
    }
    if (rt != declrt && rt != (jl_value_t*)jl_any_type)
        jl_add_method_root(ctx, rt);

    function_sig_t sig("cfunction", lrt, rt, retboxed, argt, unionall_env, false, CallingConv::C, false, &ctx.emission_context);
    assert(sig.fargt.size() + sig.sret == sig.fargt_sig.size());
    if (!sig.err_msg.empty()) {
        emit_error(ctx, sig.err_msg);
        JL_GC_POP();
        return jl_cgval_t(ctx.builder.getContext());
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
                return jl_cgval_t(ctx.builder.getContext());
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
        return jl_cgval_t(ctx.builder.getContext());
    }
#endif
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    // try to look up this function for direct invoking
    jl_method_instance_t *lam = sigt ? jl_get_specialization1((jl_tupletype_t*)sigt, world, &min_valid, &max_valid, 0) : NULL;
    Value *F = gen_cfun_wrapper(
            jl_Module, ctx.emission_context,
            sig, fexpr_rt.constant, NULL,
            declrt, lam,
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
        Type *T_htable = ArrayType::get(getSizeTy(ctx.builder.getContext()), sizeof(htable_t) / sizeof(void*));
        Value *cache = new GlobalVariable(*jl_Module, T_htable, false,
                               GlobalVariable::PrivateLinkage,
                               ConstantAggregateZero::get(T_htable));
        F = ctx.builder.CreateCall(prepare_call(jlgetcfunctiontrampoline_func), {
                 fobj,
                 literal_pointer_val(ctx, output_type),
                 ctx.builder.CreateBitCast(cache, getInt8PtrTy(ctx.builder.getContext())),
                 literal_pointer_val(ctx, (jl_value_t*)fill),
                 F,
                 closure_types ? literal_pointer_val(ctx, (jl_value_t*)unionall_env) : Constant::getNullValue(ctx.types().T_pjlvalue),
                 closure_types ? ctx.spvals_ptr : ConstantPointerNull::get(cast<PointerType>(ctx.types().T_pprjlvalue))
             });
        outboxed = true;
    }
    else {
        F = ctx.builder.CreatePtrToInt(F, getSizeTy(ctx.builder.getContext()));
        outboxed = (output_type != (jl_value_t*)jl_voidpointer_type);
        if (outboxed) {
            assert(jl_datatype_size(output_type) == sizeof(void*) * 4);
            Value *strct = emit_allocobj(ctx, jl_datatype_size(output_type),
                                         literal_pointer_val(ctx, (jl_value_t*)output_type));
            Value *derived_strct = emit_bitcast(ctx, decay_derived(ctx, strct), getSizePtrTy(ctx.builder.getContext()));
            MDNode *tbaa = best_tbaa(ctx.tbaa(), output_type);
            tbaa_decorate(tbaa, ctx.builder.CreateStore(F, derived_strct));
            tbaa_decorate(tbaa, ctx.builder.CreateStore(
                ctx.builder.CreatePtrToInt(literal_pointer_val(ctx, fexpr_rt.constant), getSizeTy(ctx.builder.getContext())),
                ctx.builder.CreateConstInBoundsGEP1_32(getSizeTy(ctx.builder.getContext()), derived_strct, 1)));
            tbaa_decorate(tbaa, ctx.builder.CreateStore(Constant::getNullValue(getSizeTy(ctx.builder.getContext())),
                    ctx.builder.CreateConstInBoundsGEP1_32(getSizeTy(ctx.builder.getContext()), derived_strct, 2)));
            tbaa_decorate(tbaa, ctx.builder.CreateStore(Constant::getNullValue(getSizeTy(ctx.builder.getContext())),
                    ctx.builder.CreateConstInBoundsGEP1_32(getSizeTy(ctx.builder.getContext()), derived_strct, 3)));
            F = strct;
        }
    }
    JL_GC_POP();
    return mark_julia_type(ctx, F, outboxed, output_type);
}

// do codegen to create a C-callable alias/wrapper, or if sysimg_handle is set,
// restore one from a loaded system image.
const char *jl_generate_ccallable(LLVMOrcThreadSafeModuleRef llvmmod, void *sysimg_handle, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params)
{
    ++GeneratedCCallables;
    jl_datatype_t *ft = (jl_datatype_t*)jl_tparam0(sigt);
    jl_value_t *ff = ft->instance;
    assert(ff);
    const char *name = jl_symbol_name(ft->name->mt->name);
    jl_value_t *crt = declrt;
    if (jl_is_abstract_ref_type(declrt)) {
        declrt = jl_tparam0(declrt);
        crt = (jl_value_t*)jl_any_type;
    }
    bool toboxed;
    Type *lcrt = _julia_struct_to_llvm(&params, *params.tsctx.getContext(), crt, &toboxed);
    if (toboxed)
        lcrt = JuliaType::get_prjlvalue_ty(lcrt->getContext());
    size_t nargs = jl_nparams(sigt)-1;
    jl_svec_t *argtypes = NULL;
    JL_GC_PUSH1(&argtypes);
    argtypes = jl_alloc_svec(nargs);
    for (size_t i = 0; i < nargs; i++) {
        jl_svecset(argtypes, i, jl_tparam(sigt, i+1));
    }
    jl_value_t *err;
    { // scope block for sig
        function_sig_t sig("cfunction", lcrt, crt, toboxed,
                           argtypes, NULL, false, CallingConv::C, false, &params);
        if (sig.err_msg.empty()) {
            size_t world = jl_atomic_load_acquire(&jl_world_counter);
            size_t min_valid = 0;
            size_t max_valid = ~(size_t)0;
            if (sysimg_handle) {
                // restore a ccallable from the system image
                void *addr;
                int found = jl_dlsym(sysimg_handle, name, &addr, 0);
                if (found)
                    add_named_global(name, addr);
            }
            else {
                jl_method_instance_t *lam = jl_get_specialization1((jl_tupletype_t*)sigt, world, &min_valid, &max_valid, 0);
                //Safe b/c params holds context lock
                gen_cfun_wrapper(unwrap(llvmmod)->getModuleUnlocked(), params, sig, ff, name, declrt, lam, NULL, NULL, NULL);
            }
            JL_GC_POP();
            return name;
        }
        err = jl_get_exceptionf(jl_errorexception_type, "%s", sig.err_msg.c_str());
    }
    jl_throw(err);
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_invoke_wrapper(jl_method_instance_t *lam, jl_value_t *jlretty, const jl_returninfo_t &f, int retarg, StringRef funcName,
        Module *M, jl_codegen_params_t &params)
{
    ++GeneratedInvokeWrappers;
    Function *w = Function::Create(JuliaType::get_jlfunc_ty(M->getContext()), GlobalVariable::ExternalLinkage, funcName, M);
    addRetAttr(w, Attribute::NonNull);
    w->addFnAttr(Attribute::get(M->getContext(), "thunk"));
    jl_init_function(w);
    Function::arg_iterator AI = w->arg_begin();
    Value *funcArg = &*AI++;
    Value *argArray = &*AI++;
    Value *argCount = &*AI++; (void)argCount; // unused
    //Value *mfunc = &*AI++; (void)mfunc; // unused
    assert(AI == w->arg_end());

    jl_codectx_t ctx(M->getContext(), params);
    ctx.f = w;
    ctx.linfo = lam;
    ctx.rettype = jlretty;
    ctx.world = 0;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", w);
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
        assert(cast<PointerType>(ftype->getParamType(0))->isOpaqueOrPointeeTypeMatches(getAttributeAtIndex(f.decl->getAttributes(), 1, Attribute::StructRet).getValueAsType()));
        result = ctx.builder.CreateAlloca(getAttributeAtIndex(f.decl->getAttributes(), 1, Attribute::StructRet).getValueAsType());
        args[idx] = result;
        idx++;
        break;
    case jl_returninfo_t::Union:
        result = ctx.builder.CreateAlloca(ArrayType::get(getInt8Ty(ctx.builder.getContext()), f.union_bytes));
        if (f.union_align > 1)
            result->setAlignment(Align(f.union_align));
        args[idx] = result;
        idx++;
        break;
    }
    if (f.return_roots) {
        AllocaInst *return_roots = emit_static_alloca(ctx, ArrayType::get(ctx.types().T_prjlvalue, f.return_roots));
        args[idx] = return_roots;
        idx++;
    }

    bool is_opaque_closure = jl_is_method(lam->def.value) && lam->def.method->is_for_opaque_closure;
    for (size_t i = 0; i < jl_nparams(lam->specTypes) && idx < nfargs; ++i) {
        jl_value_t *ty = ((i == 0) && is_opaque_closure) ? (jl_value_t*)jl_any_type :
            jl_nth_slot_type(lam->specTypes, i);
        bool isboxed = deserves_argbox(ty);
        Type *lty = isboxed ?  ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, ty);
        if (type_is_ghost(lty) || is_uniquerep_Type(ty))
            continue;
        Value *theArg;
        if (i == 0) {
            theArg = funcArg;
        }
        else {
            Value *argPtr = ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, argArray, i - 1);
            theArg = maybe_mark_load_dereferenceable(
                    ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, argPtr, Align(sizeof(void*))),
                    false,
                    ty);
        }
        if (!isboxed) {
            theArg = decay_derived(ctx, emit_bitcast(ctx, theArg, PointerType::get(lty, 0)));
            if (!lty->isAggregateType()) // keep "aggregate" type values in place as pointers
                theArg = ctx.builder.CreateAlignedLoad(lty, theArg, Align(julia_alignment(ty)));
        }
        assert(dyn_cast<UndefValue>(theArg) == NULL);
        args[idx] = theArg;
        idx++;
    }
    CallInst *call = ctx.builder.CreateCall(f.decl, ArrayRef<Value*>(&args[0], nfargs));
    call->setAttributes(f.decl->getAttributes());

    jl_cgval_t retval(ctx.builder.getContext());
    if (retarg != -1) {
        Value *theArg;
        if (retarg == 0)
            theArg = funcArg;
        else
            theArg = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                    ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_prjlvalue, argArray, retarg - 1),
                    Align(sizeof(void*)));
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
            retval = mark_julia_slot(result, jlretty, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            break;
        case jl_returninfo_t::Union:
            // result is technically not right here, but `boxed` will only look at it
            // for the unboxed values, so it's ok.
            retval = mark_julia_slot(result,
                                     jlretty,
                                     ctx.builder.CreateExtractValue(call, 1),
                                     ctx.tbaa(),
                                     ctx.tbaa().tbaa_stack);
            retval.Vboxed = ctx.builder.CreateExtractValue(call, 0);
            assert(retval.Vboxed->getType() == ctx.types().T_prjlvalue);
            break;
        case jl_returninfo_t::Ghosts:
            retval = mark_julia_slot(NULL, jlretty, call, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            break;
        }
    }
    ctx.builder.CreateRet(boxed(ctx, retval));
    assert(!ctx.roots);
    return w;
}

static jl_returninfo_t get_specsig_function(jl_codectx_t &ctx, Module *M, StringRef name, jl_value_t *sig, jl_value_t *jlrettype, bool is_opaque_closure)
{
    jl_returninfo_t props = {};
    SmallVector<Type*, 8> fsig;
    Type *rt = NULL;
    Type *srt = NULL;
    if (jl_is_structtype(jlrettype) && jl_is_datatype_singleton((jl_datatype_t*)jlrettype)) {
        rt = getVoidTy(ctx.builder.getContext());
        props.cc = jl_returninfo_t::Register;
    }
    else if (jl_is_uniontype(jlrettype)) {
        bool allunbox;
        union_alloca_type((jl_uniontype_t*)jlrettype, allunbox, props.union_bytes, props.union_align, props.union_minalign);
        if (props.union_bytes) {
            props.cc = jl_returninfo_t::Union;
            Type *AT = ArrayType::get(getInt8Ty(ctx.builder.getContext()), props.union_bytes);
            fsig.push_back(AT->getPointerTo());
            Type *pair[] = { ctx.types().T_prjlvalue, getInt8Ty(ctx.builder.getContext()) };
            rt = StructType::get(ctx.builder.getContext(), makeArrayRef(pair));
        }
        else if (allunbox) {
            props.cc = jl_returninfo_t::Ghosts;
            rt = getInt8Ty(ctx.builder.getContext());
        }
        else {
            rt = ctx.types().T_prjlvalue;
        }
    }
    else if (!deserves_retbox(jlrettype)) {
        bool retboxed;
        rt = julia_type_to_llvm(ctx, jlrettype, &retboxed);
        assert(!retboxed);
        if (rt != getVoidTy(ctx.builder.getContext()) && deserves_sret(jlrettype, rt)) {
            auto tracked = CountTrackedPointers(rt);
            assert(!tracked.derived);
            if (tracked.count && !tracked.all)
                props.return_roots = tracked.count;
            props.cc = jl_returninfo_t::SRet;
            fsig.push_back(rt->getPointerTo());
            srt = rt;
            rt = getVoidTy(ctx.builder.getContext());
        }
        else {
            props.cc = jl_returninfo_t::Register;
        }
    }
    else {
        rt = ctx.types().T_prjlvalue;
    }

    AttributeList attributes; // function declaration attributes
    if (props.cc == jl_returninfo_t::SRet) {
        assert(srt);
        unsigned argno = 1;
        Attribute sret = Attribute::getWithStructRetType(ctx.builder.getContext(), srt);
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, sret);
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoAlias);
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoCapture);
    }
    if (props.cc == jl_returninfo_t::Union) {
        unsigned argno = 1;
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoAlias);
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoCapture);
    }

    if (props.return_roots) {
        fsig.push_back(get_returnroots_type(ctx, props.return_roots)->getPointerTo(0));
        unsigned argno = fsig.size();
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoAlias);
        attributes = addAttributeAtIndex(attributes, ctx.builder.getContext(), argno, Attribute::NoCapture);
    }

    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        if (i == 0 && is_opaque_closure) {
            jt = (jl_value_t*)jl_any_type;
        }
        if (is_uniquerep_Type(jt))
            continue;
        bool isboxed = deserves_argbox(jt);
        Type *ty = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jt);
        if (type_is_ghost(ty))
            continue;
        unsigned argno = fsig.size();
        if (ty->isAggregateType()) { // aggregate types are passed by pointer
            attributes = attributes.addParamAttribute(ctx.builder.getContext(), argno, Attribute::NoCapture);
            attributes = attributes.addParamAttribute(ctx.builder.getContext(), argno, Attribute::ReadOnly);
            ty = PointerType::get(ty, AddressSpace::Derived);
        }
        else if (isboxed && jl_is_immutable_datatype(jt)) {
            attributes = attributes.addParamAttribute(ctx.builder.getContext(), argno, Attribute::ReadOnly);
        }
        else if (jl_is_primitivetype(jt) && ty->isIntegerTy()) {
            bool issigned = jl_signed_type && jl_subtype(jt, (jl_value_t*)jl_signed_type);
            Attribute::AttrKind attr = issigned ? Attribute::SExt : Attribute::ZExt;
            attributes = attributes.addParamAttribute(ctx.builder.getContext(), argno, attr);
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
    if (rt == ctx.types().T_prjlvalue)
        addRetAttr(f, Attribute::NonNull);
    props.decl = f;
    return props;
}

static void emit_sret_roots(jl_codectx_t &ctx, bool isptr, Value *Src, Type *T, Value *Shadow, Type *ShadowT, unsigned count)
{
    if (isptr)
        Src = maybe_decay_tracked(ctx, Src);
    if (isptr && !cast<PointerType>(Src->getType())->isOpaqueOrPointeeTypeMatches(T))
        Src = ctx.builder.CreateBitCast(Src, T->getPointerTo(Src->getType()->getPointerAddressSpace()));
    unsigned emitted = TrackWithShadow(Src, T, isptr, Shadow, ShadowT, ctx.builder); //This comes from Late-GC-Lowering??
    assert(emitted == count); (void)emitted; (void)count;
}

static DISubroutineType *
get_specsig_di(jl_codectx_t &ctx, jl_debugcache_t &debuginfo, jl_value_t *rt, jl_value_t *sig, DIBuilder &dbuilder)
{
    size_t nargs = jl_nparams(sig); // TODO: if this is a Varargs function, our debug info for the `...` var may be misleading
    std::vector<Metadata*> ditypes(nargs + 1);
    ditypes[0] = julia_type_to_di(ctx, debuginfo, rt, &dbuilder, false);
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        ditypes[i + 1] = julia_type_to_di(ctx, debuginfo, jt, &dbuilder, false);
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
static jl_llvm_functions_t
    emit_function(
        orc::ThreadSafeModule &TSM,
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
{
    ++EmittedFunctions;
    // step 1. unpack AST and allocate codegen context for this function
    jl_llvm_functions_t declarations;
    jl_codectx_t ctx(*params.tsctx.getContext(), params);
    jl_datatype_t *vatyp = NULL;
    JL_GC_PUSH3(&ctx.code, &ctx.roots, &vatyp);
    ctx.code = src->code;

    std::map<int, BasicBlock*> labels;
    bool toplevel = false;
    ctx.module = jl_is_method(lam->def.method) ? lam->def.method->module : lam->def.module;
    ctx.linfo = lam;
    ctx.name = TSM.getModuleUnlocked()->getModuleIdentifier().data();
    size_t nreq = 0;
    int va = 0;
    if (jl_is_method(lam->def.method)) {
        ctx.nargs = nreq = lam->def.method->nargs;
        ctx.is_opaque_closure = lam->def.method->is_for_opaque_closure;
        if ((nreq > 0 && jl_is_method(lam->def.value) && lam->def.method->isva)) {
            assert(nreq > 0);
            nreq--;
            va = 1;
        }
    }
    else {
        ctx.nargs = 0;
    }
    ctx.nReqArgs = nreq;
    if (va) {
        jl_sym_t *vn = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, ctx.nargs - 1);
        if (vn != jl_unused_sym)
            ctx.vaSlot = ctx.nargs - 1;
    }
    toplevel = !jl_is_method(lam->def.method);
    ctx.rettype = jlrettype;
    ctx.source = src;
    ctx.funcName = ctx.name;
    ctx.spvals_ptr = NULL;
    jl_array_t *stmts = ctx.code;
    size_t stmtslen = jl_array_dim0(stmts);

    // step 1b. unpack debug information
    int coverage_mode = jl_options.code_coverage;
    int malloc_log_mode = jl_options.malloc_log;
    if (!JL_FEAT_TEST(ctx, code_coverage))
        coverage_mode = JL_LOG_NONE;
    if (!JL_FEAT_TEST(ctx, track_allocations))
        malloc_log_mode = JL_LOG_NONE;

    StringRef dbgFuncName = ctx.name;
    int toplineno = -1;
    if (lam && jl_is_method(lam->def.method)) {
        toplineno = lam->def.method->line;
        ctx.file = jl_symbol_name(lam->def.method->file);
    }
    else if (jl_array_len(src->linetable) > 0) {
        jl_value_t *locinfo = jl_array_ptr_ref(src->linetable, 0);
        ctx.file = jl_symbol_name((jl_sym_t*)jl_fieldref_noalloc(locinfo, 2));
        toplineno = jl_unbox_int32(jl_fieldref(locinfo, 3));
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
    ctx.slots.resize(vinfoslen, jl_varinfo_t(ctx.builder.getContext()));
    assert(lam->specTypes); // the specTypes field should always be assigned


    // create SAvalue locations for SSAValue objects
    ctx.ssavalue_assigned.assign(n_ssavalues, false);
    ctx.SAvalues.assign(n_ssavalues, jl_cgval_t(ctx.builder.getContext()));
    ctx.ssavalue_usecount.assign(n_ssavalues, 0);

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
        if (argname == jl_unused_sym)
            continue;
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        // OpaqueClosure implicitly loads the env
        if (i == 0 && ctx.is_opaque_closure) {
            if (jl_is_array(src->slottypes)) {
                ty = jl_arrayref((jl_array_t*)src->slottypes, i);
            }
            else {
                ty = (jl_value_t*)jl_any_type;
            }
        }
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, ty);
    }
    if (va && ctx.vaSlot != -1) {
        jl_varinfo_t &varinfo = ctx.slots[ctx.vaSlot];
        varinfo.isArgument = true;
        vatyp = specsig ? compute_va_type(lam, nreq) : (jl_tuple_type);
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

    std::string _funcName;
    raw_string_ostream funcName(_funcName);
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
    funcName << unadorned_name << "_" << globalUniqueGeneratedNames++;
    declarations.specFunctionObject = funcName.str();

    // allocate Function declarations and wrapper objects
    //Safe because params holds ctx lock
    Module *M = TSM.getModuleUnlocked();
    jl_debugcache_t debuginfo;
    debuginfo.initialize(M);
    jl_returninfo_t returninfo = {};
    Function *f = NULL;
    bool has_sret = false;
    if (specsig) { // assumes !va and !needsparams
        returninfo = get_specsig_function(ctx, M, declarations.specFunctionObject, lam->specTypes, jlrettype, ctx.is_opaque_closure);
        f = returninfo.decl;
        has_sret = (returninfo.cc == jl_returninfo_t::SRet || returninfo.cc == jl_returninfo_t::Union);
        jl_init_function(f);

        // common pattern: see if all return statements are an argument in that
        // case the apply-generic call can re-use the original box for the return
        int retarg = [stmts, nreq]() {
            int retarg = -1;
            for (size_t i = 0; i < jl_array_len(stmts); ++i) {
                jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
                if (jl_is_returnnode(stmt)) {
                    stmt = jl_returnnode_value(stmt);
                    if (stmt == NULL)
                        continue;
                    if (!jl_is_argument(stmt))
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

        std::string wrapName;
        raw_string_ostream(wrapName) << "jfptr_" << unadorned_name << "_" << globalUniqueGeneratedNames++;
        declarations.functionObject = wrapName;
        (void)gen_invoke_wrapper(lam, jlrettype, returninfo, retarg, declarations.functionObject, M, ctx.emission_context);
    }
    else {
        f = Function::Create(needsparams ? ctx.types().T_jlfuncparams : ctx.types().T_jlfunc,
                             GlobalVariable::ExternalLinkage,
                             declarations.specFunctionObject, M);
        jl_init_function(f);
        addRetAttr(f, Attribute::NonNull);
        f->addFnAttr(Attribute::get(ctx.builder.getContext(), "thunk"));
        // TODO: (if needsparams) add attributes: dereferenceable<sizeof(void*) * length(sp)>, readonly, nocapture
        // TODO: add attributes: dereferenceable<sizeof(ft)>, readonly, nocapture - e.g. maybe_mark_argument_dereferenceable(Arg, argType);
        // TODO: add attributes: dereferenceable<sizeof(void*) * nreq>, readonly, nocapture
        returninfo.decl = f;
        declarations.functionObject = needsparams ? "jl_fptr_sparam" : "jl_fptr_args";
    }

    if (jlrettype == (jl_value_t*)jl_bottom_type)
        f->setDoesNotReturn();

#ifdef USE_POLLY
    if (!jl_has_meta(stmts, jl_polly_sym) || jl_options.polly == JL_OPTIONS_POLLY_OFF) {
        f->addFnAttr(polly::PollySkipFnAttr);
    }
#endif

    if (jl_has_meta(stmts, jl_noinline_sym)) {
        f->addFnAttr(Attribute::NoInline);
    }

    if (returninfo.cc == jl_returninfo_t::Union) {
        addAttributeAtIndex(f, 1, Attribute::getWithDereferenceableBytes(ctx.builder.getContext(), returninfo.union_bytes));
        addAttributeAtIndex(f, 1, Attribute::getWithAlignment(ctx.builder.getContext(), Align(returninfo.union_align)));
    }

#ifdef JL_DEBUG_BUILD
    f->addFnAttr(Attribute::StackProtectStrong);
#endif

#ifdef _COMPILER_TSAN_ENABLED_
    // TODO: enable this only when a argument like `-race` is passed to Julia
    //       add a macro for no_sanitize_thread
    f->addFnAttr(llvm::Attribute::SanitizeThread);
#endif

    // add the optimization level specified for this module, if any
    int optlevel = jl_get_module_optlevel(ctx.module);
    if (optlevel >= 0 && optlevel <= 3) {
        static const char* const optLevelStrings[] = { "0", "1", "2", "3" };
        f->addFnAttr("julia-optimization-level", optLevelStrings[optlevel]);
    }

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
            subrty = debuginfo.jl_di_func_null_sig;
        }
        else if (!specsig) {
            subrty = debuginfo.jl_di_func_sig;
        }
        else {
            subrty = get_specsig_di(ctx, debuginfo, jlrettype, lam->specTypes, dbuilder);
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
        topdebugloc = DILocation::get(ctx.builder.getContext(), toplineno, 0, SP, NULL);
        f->setSubprogram(SP);
        if (jl_options.debug_level >= 2) {
            const bool AlwaysPreserve = true;
            // Go over all arguments and local variables and initialize their debug information
            for (i = 0; i < nreq; i++) {
                jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
                if (argname == jl_unused_sym)
                    continue;
                jl_varinfo_t &varinfo = ctx.slots[i];
                varinfo.dinfo = dbuilder.createParameterVariable(
                    SP,                                 // Scope (current function will be fill in later)
                    jl_symbol_name(argname),            // Variable name
                    has_sret + i + 1,                   // Argument number (1-based)
                    topfile,                            // File
                    toplineno == -1 ? 0 : toplineno,    // Line
                    // Variable type
                    julia_type_to_di(ctx, debuginfo, varinfo.value.typ, &dbuilder, false),
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
                    julia_type_to_di(ctx, debuginfo, ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DINode::FlagZero);                  // Flags (TODO: Do we need any)
            }
            for (i = 0; i < vinfoslen; i++) {
                jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
                jl_varinfo_t &varinfo = ctx.slots[i];
                if (varinfo.isArgument || s == jl_empty_sym || s == jl_unused_sym)
                    continue;
                // LLVM 4.0: Assume the variable has default alignment
                varinfo.dinfo = dbuilder.createAutoVariable(
                    SP,                      // Scope (current function will be fill in later)
                    jl_symbol_name(s),       // Variable name
                    topfile,                 // File
                    toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                    julia_type_to_di(ctx, debuginfo, varinfo.value.typ, &dbuilder, false), // Variable type
                    AlwaysPreserve,          // May be deleted if optimized out
                    DINode::FlagZero         // Flags (TODO: Do we need any)
                    );
            }
        }
    }

    // step 5. create first basic block
    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", f);
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

    // step 6. set up GC frame
    allocate_gc_frame(ctx, b0);
    Value *last_age = NULL;
    emit_last_age_field(ctx);
    if (toplevel || ctx.is_opaque_closure) {
        last_age = tbaa_decorate(ctx.tbaa().tbaa_gcframe, ctx.builder.CreateAlignedLoad(
            getSizeTy(ctx.builder.getContext()), ctx.world_age_field, Align(sizeof(size_t))));
    }

    // step 7. allocate local variables slots
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
                varinfo.value = mark_julia_slot(lv, jt, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
                varinfo.pTIndex = emit_static_alloca(ctx, getInt8Ty(ctx.builder.getContext()));
            }
            else if (allunbox) {
                // all ghost values just need a selector allocated
                AllocaInst *lv = emit_static_alloca(ctx, getInt8Ty(ctx.builder.getContext()));
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
        else if (deserves_stack(jt)) {
            bool isboxed;
            Type *vtype = julia_type_to_llvm(ctx, jt, &isboxed);
            assert(!isboxed);
            assert(!type_is_ghost(vtype) && "constants should already be handled");
            Value *lv = new AllocaInst(vtype, 0, jl_symbol_name(s), /*InsertBefore*/ctx.pgcstack);
            if (CountTrackedPointers(vtype).count) {
                StoreInst *SI = new StoreInst(Constant::getNullValue(vtype), lv, false, Align(sizeof(void*)));
                SI->insertAfter(ctx.pgcstack);
            }
            varinfo.value = mark_julia_slot(lv, jt, NULL, ctx.tbaa(), ctx.tbaa().tbaa_stack);
            alloc_def_flag(ctx, varinfo);
            if (ctx.debug_enabled && varinfo.dinfo) {
                assert((Metadata*)varinfo.dinfo->getType() != debuginfo.jl_pvalue_dillvmt);
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
            AllocaInst *av = new AllocaInst(ctx.types().T_prjlvalue, 0,
                jl_symbol_name(s), /*InsertBefore*/ctx.pgcstack);
            StoreInst *SI = new StoreInst(Constant::getNullValue(ctx.types().T_prjlvalue), av, false, Align(sizeof(void*)));
            SI->insertAfter(ctx.pgcstack);
            varinfo.boxroot = av;
            if (ctx.debug_enabled && varinfo.dinfo) {
                DIExpression *expr;
                if ((Metadata*)varinfo.dinfo->getType() == debuginfo.jl_pvalue_dillvmt) {
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
        if (s == jl_unused_sym)
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
    // Also count ssavalue uses.
    {
        for (size_t i = 0; i < jl_array_len(stmts); ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);

            auto scan_ssavalue = [&](jl_value_t *val) {
                if (jl_is_ssavalue(val)) {
                    ctx.ssavalue_usecount[((jl_ssavalue_t*)val)->id-1] += 1;
                    return true;
                }
                return false;
            };
            general_use_analysis(ctx, stmt, scan_ssavalue);

            if (jl_is_phicnode(stmt)) {
                jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(stmt, 0);
                for (size_t j = 0; j < jl_array_len(values); ++j) {
                    jl_value_t *val = jl_array_ptr_ref(values, j);
                    assert(jl_is_ssavalue(val));
                    upsilon_to_phic[((jl_ssavalue_t*)val)->id] = i;
                }
                jl_varinfo_t &vi = (ctx.phic_slots.emplace(i, jl_varinfo_t(ctx.builder.getContext())).first->second =
                                    jl_varinfo_t(ctx.builder.getContext()));
                jl_value_t *typ = jl_array_ptr_ref(src->ssavaluetypes, i);
                vi.used = true;
                vi.isVolatile = true;
                vi.value = mark_julia_type(ctx, (Value*)NULL, false, typ);
                allocate_local(vi, jl_symbol("phic"));
            }
        }
    }

    // step 8. move args into local variables
    Function::arg_iterator AI = f->arg_begin();

    auto get_specsig_arg = [&](jl_value_t *argType, Type *llvmArgType, bool isboxed) {
        jl_cgval_t theArg(ctx.builder.getContext());
        if (type_is_ghost(llvmArgType)) { // this argument is not actually passed
            theArg = ghostValue(ctx, argType);
        }
        else if (is_uniquerep_Type(argType)) {
            theArg = mark_julia_const(ctx, jl_tparam0(argType));
        }
        else if (llvmArgType->isAggregateType()) {
            Argument *Arg = &*AI; ++AI;
            maybe_mark_argument_dereferenceable(Arg, argType);
            theArg = mark_julia_slot(Arg, argType, NULL, ctx.tbaa(), ctx.tbaa().tbaa_const); // this argument is by-pointer
        }
        else {
            Argument *Arg = &*AI; ++AI;
            if (isboxed) // e.g. is-pointer
                maybe_mark_argument_dereferenceable(Arg, argType);
            theArg = mark_julia_type(ctx, Arg, isboxed, argType);
            if (theArg.tbaa == ctx.tbaa().tbaa_immut)
                theArg.tbaa = ctx.tbaa().tbaa_const;
        }
        return theArg;
    };

    if (has_sret)
        AI++; // skip sret slot
    if (returninfo.return_roots)
        AI++; // skip return_roots slot
    for (i = 0; i < nreq; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_array_ptr_ref(src->slotnames, i);
        jl_value_t *argType = (i == 0 && ctx.is_opaque_closure) ? (jl_value_t*)jl_any_type :
            jl_nth_slot_type(lam->specTypes, i);
        bool isboxed = deserves_argbox(argType);
        Type *llvmArgType = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, argType);
        if (s == jl_unused_sym) {
            if (specsig && !type_is_ghost(llvmArgType) && !is_uniquerep_Type(argType))
                ++AI;
            continue;
        }
        jl_varinfo_t &vi = ctx.slots[i];
        jl_cgval_t theArg(ctx.builder.getContext());
        if (s == jl_unused_sym || vi.value.constant) {
            assert(vi.boxroot == NULL);
            if (specsig && !type_is_ghost(llvmArgType) && !is_uniquerep_Type(argType))
                ++AI;
        }
        else {
            if (specsig) {
                theArg = get_specsig_arg(argType, llvmArgType, isboxed);
            }
            else {
                if (i == 0) {
                    // first (function) arg is separate in jlcall
                    theArg = mark_julia_type(ctx, fArg, true, ctx.is_opaque_closure ?
                        argType : vi.value.typ);
                }
                else {
                    Value *argPtr = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, argArray, ConstantInt::get(getSizeTy(ctx.builder.getContext()), i-1));
                    Value *load = maybe_mark_load_dereferenceable(
                            ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, argPtr, Align(sizeof(void*))),
                            false, vi.value.typ);
                    theArg = mark_julia_type(ctx, load, true, vi.value.typ);
                    if (ctx.debug_enabled && vi.dinfo && !vi.boxroot && !vi.value.V) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus_uconst);
                        addr.push_back((i - 1) * sizeof(void*));
                        if ((Metadata*)vi.dinfo->getType() != debuginfo.jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        dbuilder.insertDeclare(pargArray, vi.dinfo, dbuilder.createExpression(addr),
                                        topdebugloc,
                                        ctx.builder.GetInsertBlock());
                    }
                }
            }

            // If this is an opaque closure, implicitly load the env and switch
            // the world age.
            if (i == 0 && ctx.is_opaque_closure) {
                // Load closure world
                Value *argaddr = emit_bitcast(ctx, maybe_decay_tracked(ctx, data_pointer(ctx, theArg)), getInt8PtrTy(ctx.builder.getContext()));
                Value *worldaddr = ctx.builder.CreateInBoundsGEP(
                        getInt8Ty(ctx.builder.getContext()), argaddr,
                        ConstantInt::get(getSizeTy(ctx.builder.getContext()), offsetof(jl_opaque_closure_t, world)));

                jl_cgval_t closure_world = typed_load(ctx, worldaddr, NULL, (jl_value_t*)jl_long_type,
                    theArg.tbaa, nullptr, false, AtomicOrdering::NotAtomic, false, sizeof(size_t));
                emit_unbox(ctx, getSizeTy(ctx.builder.getContext()), closure_world, (jl_value_t*)jl_long_type, ctx.world_age_field, ctx.tbaa().tbaa_gcframe);

                // Load closure env
                Value *envaddr = ctx.builder.CreateInBoundsGEP(
                        getInt8Ty(ctx.builder.getContext()), argaddr,
                        ConstantInt::get(getSizeTy(ctx.builder.getContext()), offsetof(jl_opaque_closure_t, captures)));

                jl_cgval_t closure_env = typed_load(ctx, envaddr, NULL, (jl_value_t*)jl_any_type,
                    theArg.tbaa, nullptr, true, AtomicOrdering::NotAtomic, false, sizeof(void*));
                theArg = convert_julia_type(ctx, closure_env, vi.value.typ);
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
                        if ((Metadata*)vi.dinfo->getType() != debuginfo.jl_pvalue_dillvmt)
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

    // step 9. allocate rest argument
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
                Type *llvmArgType = isboxed ?  ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, argType);
                vargs[i - nreq] = get_specsig_arg(argType, llvmArgType, isboxed);
            }
            if (jl_is_concrete_type(vi.value.typ)) {
                jl_cgval_t tuple = emit_new_struct(ctx, vi.value.typ, ctx.nvargs, vargs);
                emit_varinfo_assign(ctx, vi, tuple);
            }
            else {
                restTuple = emit_jlcall(ctx, jltuple_func, Constant::getNullValue(ctx.types().T_prjlvalue),
                    vargs, ctx.nvargs, JLCALL_F_CC);
                jl_cgval_t tuple = mark_julia_type(ctx, restTuple, true, vi.value.typ);
                emit_varinfo_assign(ctx, vi, tuple);
            }
        }
        else {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs - nreq)
            Function *F = prepare_call(jltuple_func);
            restTuple =
                ctx.builder.CreateCall(F,
                        { Constant::getNullValue(ctx.types().T_prjlvalue),
                          ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, argArray,
                                  ConstantInt::get(getSizeTy(ctx.builder.getContext()), nreq - 1)),
                          ctx.builder.CreateSub(argCount,
                                  ConstantInt::get(getInt32Ty(ctx.builder.getContext()), nreq - 1)) });
            restTuple->setAttributes(F->getAttributes());
            ctx.builder.CreateStore(restTuple, vi.boxroot);
        }
    }

    // step 10. Compute properties for each statements
    //     This needs to be computed by iterating in the IR order
    //     instead of control flow order.
    auto in_user_mod = [] (jl_module_t *mod) {
        return (!jl_is_submodule(mod, jl_base_module) &&
                !jl_is_submodule(mod, jl_core_module));
    };
    auto in_tracked_path = [] (StringRef file) {
        return jl_options.tracked_path != NULL && file.startswith(jl_options.tracked_path);
    };
    bool mod_is_user_mod = in_user_mod(ctx.module);
    bool mod_is_tracked = in_tracked_path(ctx.file);
    struct DebugLineTable {
        DebugLoc loc;
        StringRef file;
        ssize_t line;
        bool is_user_code;
        bool is_tracked; // falls within an explicitly set file or directory
        unsigned inlined_at;
        bool operator ==(const DebugLineTable &other) const {
            return other.loc == loc && other.file == file && other.line == line && other.is_user_code == is_user_code && other.is_tracked == is_tracked && other.inlined_at == inlined_at;
        }
    };
    std::vector<DebugLineTable> linetable;
    { // populate the linetable data format
        assert(jl_is_array(src->linetable));
        size_t nlocs = jl_array_len(src->linetable);
        std::map<std::tuple<StringRef, StringRef>, DISubprogram*> subprograms;
        linetable.resize(nlocs + 1);
        DebugLineTable &topinfo = linetable[0];
        topinfo.file = ctx.file;
        topinfo.line = toplineno;
        topinfo.is_user_code = mod_is_user_mod;
        topinfo.is_tracked = mod_is_tracked;
        topinfo.inlined_at = 0;
        topinfo.loc = topdebugloc;
        for (size_t i = 0; i < nlocs; i++) {
            // LineInfoNode(mod::Module, method::Any, file::Symbol, line::Int32, inlined_at::Int32)
            jl_value_t *locinfo = jl_array_ptr_ref(src->linetable, i);
            DebugLineTable &info = linetable[i + 1];
            assert(jl_typeis(locinfo, jl_lineinfonode_type));
            jl_module_t *module = (jl_module_t*)jl_fieldref_noalloc(locinfo, 0);
            jl_value_t *method = jl_fieldref_noalloc(locinfo, 1);
            jl_sym_t *filesym = (jl_sym_t*)jl_fieldref_noalloc(locinfo, 2);
            info.line = jl_unbox_int32(jl_fieldref(locinfo, 3));
            info.inlined_at = jl_unbox_int32(jl_fieldref(locinfo, 4));
            assert(info.inlined_at <= i);
            info.file = jl_symbol_name(filesym);
            if (info.file.empty())
                info.file = "<missing>";
            if (module == ctx.module)
                info.is_user_code = mod_is_user_mod;
            else
                info.is_user_code = in_user_mod(module);
            info.is_tracked = in_tracked_path(info.file);
            if (ctx.debug_enabled) {
                StringRef fname;
                if (jl_is_method_instance(method))
                    method = ((jl_method_instance_t*)method)->def.value;
                if (jl_is_method(method))
                    method = (jl_value_t*)((jl_method_t*)method)->name;
                if (jl_is_symbol(method))
                    fname = jl_symbol_name((jl_sym_t*)method);
                if (fname.empty())
                    fname = "macro expansion";
                if (info.inlined_at == 0 && info.file == ctx.file) { // if everything matches, emit a toplevel line number
                    info.loc = DILocation::get(ctx.builder.getContext(), info.line, 0, SP, NULL);
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
                                                     ,debuginfo.jl_di_func_null_sig // Ty
                                                     ,0                // ScopeLine
                                                     ,DINode::FlagZero // Flags
                                                     ,DISubprogram::SPFlagDefinition | DISubprogram::SPFlagOptimized // SPFlags
                                                     ,nullptr          // Template Parameters
                                                     ,nullptr          // Template Declaration
                                                     ,nullptr          // ThrownTypes
                                                     );
                    }
                    DebugLoc inl_loc = (info.inlined_at == 0) ? DebugLoc(DILocation::get(ctx.builder.getContext(), 0, 0, SP, NULL)) : linetable.at(info.inlined_at).loc;
                    info.loc = DILocation::get(ctx.builder.getContext(), info.line, 0, inl_SP, inl_loc);
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
        MDBuilder mbuilder(ctx.builder.getContext());
        MDNode *alias_domain = mbuilder.createAliasScopeDomain(ctx.name);
        for (i = 0; i < nstmts; i++) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            jl_expr_t *expr = jl_is_expr(stmt) ? (jl_expr_t*)stmt : nullptr;
            if (expr) {
                if (expr->head == jl_aliasscope_sym) {
                    MDNode *scope = mbuilder.createAliasScope("aliasscope", alias_domain);
                    scope_stack.push_back(scope);
                    MDNode *scope_list = MDNode::get(ctx.builder.getContext(), ArrayRef<Metadata*>(scope_stack));
                    scope_list_stack.push_back(scope_list);
                    current_aliasscope = scope_list;
                } else if (expr->head == jl_popaliasscope_sym) {
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


    // step 11. Do codegen in control flow order
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
        else if (ctx.builder.GetInsertBlock() && !ctx.builder.GetInsertBlock()->getTerminator()) {
            CreateTrap(ctx.builder, false);
        }
        while (!workstack.empty()) {
            int item = workstack.back();
            workstack.pop_back();
            auto nextbb = BB.find(item + 1);
            if (nextbb == BB.end()) {
                cursor = item;
                return;
            }
            if (seq_next != -1 && ctx.builder.GetInsertBlock() && !ctx.builder.GetInsertBlock()->getTerminator()) {
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

    auto do_coverage = [&] (bool in_user_code, bool is_tracked) {
        return (coverage_mode == JL_LOG_ALL ||
                (in_user_code && coverage_mode == JL_LOG_USER) ||
                (is_tracked && coverage_mode == JL_LOG_PATH));
    };
    auto do_malloc_log = [&] (bool in_user_code, bool is_tracked) {
        return (malloc_log_mode == JL_LOG_ALL ||
                (in_user_code && malloc_log_mode == JL_LOG_USER) ||
                (is_tracked && malloc_log_mode == JL_LOG_PATH));
    };
    std::vector<unsigned> current_lineinfo, new_lineinfo;
    auto coverageVisitStmt = [&] (size_t dbg) {
        if (dbg == 0 || dbg >= linetable.size())
            return;
        // Compute inlining stack for current line, inner frame first
        while (dbg) {
            new_lineinfo.push_back(dbg);
            dbg = linetable.at(dbg).inlined_at;
        }
        // Visit frames which differ from previous statement as tracked in
        // current_lineinfo (tracked outer frame first).
        current_lineinfo.resize(new_lineinfo.size(), 0);
        for (dbg = 0; dbg < new_lineinfo.size(); dbg++) {
            unsigned newdbg = new_lineinfo[new_lineinfo.size() - dbg - 1];
            if (newdbg != current_lineinfo[dbg]) {
                current_lineinfo[dbg] = newdbg;
                const auto &info = linetable.at(newdbg);
                if (do_coverage(info.is_user_code, info.is_tracked))
                    coverageVisitLine(ctx, info.file, info.line);
            }
        }
        new_lineinfo.clear();
    };
    auto mallocVisitStmt = [&] (unsigned dbg, Value *sync) {
        if (!do_malloc_log(mod_is_user_mod, mod_is_tracked) || dbg == 0) {
            if (do_malloc_log(true, mod_is_tracked) && sync)
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
            if (do_coverage(info.is_user_code, info.is_tracked))
                jl_coverage_alloc_line(info.file, info.line);
    }

    come_from_bb[0] = ctx.builder.GetInsertBlock();

    // First go through and collect all branch targets, so we know where to
    // split basic blocks.
    std::set<int> branch_targets; // 1-indexed
    {
        for (size_t i = 0; i < stmtslen; ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            if (jl_is_gotoifnot(stmt)) {
                int dest = jl_gotoifnot_label(stmt);
                branch_targets.insert(dest);
                // The next 1-indexed statement
                branch_targets.insert(i + 2);
            } else if (jl_is_returnnode(stmt)) {
                // We don't do dead branch elimination before codegen
                // so we need to make sure to start a BB after any
                // return node, even if they aren't otherwise branch
                // targets.
                if (i + 2 <= stmtslen)
                    branch_targets.insert(i + 2);
            } else if (jl_is_expr(stmt)) {
                if (((jl_expr_t*)stmt)->head == jl_enter_sym) {
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
                    size_t edge = ((int32_t*)jl_array_data(edges))[j];
                    if (edge == i)
                        branch_targets.insert(i + 1);
                }
            }
        }
    }

    for (int label : branch_targets) {
        BasicBlock *bb = BasicBlock::Create(ctx.builder.getContext(),
            "L" + std::to_string(label), f);
        BB[label] = bb;
    }

    Value *sync_bytes = nullptr;
    if (do_malloc_log(true, mod_is_tracked))
        sync_bytes = ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    { // coverage for the function definition line number
        const auto &topinfo = linetable.at(0);
        if (linetable.size() > 1) {
            if (topinfo == linetable.at(1))
                current_lineinfo.push_back(1);
        }
        if (do_coverage(topinfo.is_user_code, topinfo.is_tracked))
            coverageVisitLine(ctx, topinfo.file, topinfo.line);
    }

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
        if (jl_is_returnnode(stmt)) {
            jl_value_t *retexpr = jl_returnnode_value(stmt);
            if (retexpr == NULL) {
                CreateTrap(ctx.builder, false);
                find_next_stmt(-1);
                continue;
            }
            // this is basically a copy of emit_assignment,
            // but where the assignment slot is the retval
            jl_cgval_t retvalinfo = emit_expr(ctx, retexpr);
            retvalinfo = convert_julia_type(ctx, retvalinfo, jlrettype);
            if (retvalinfo.typ == jl_bottom_type) {
                CreateTrap(ctx.builder, false);
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
                    data = Constant::getNullValue(ctx.types().T_prjlvalue);
                    if (retvalinfo.V == NULL) {
                        // treat this as a simple Ghosts
                        sret = NULL;
                    }
                    else if (retvalinfo.Vboxed) {
                        // also need to account for the possibility the return object is boxed
                        // and avoid / skip copying it to the stack
                        isboxed_union = ctx.builder.CreateICmpNE(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                            ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
                        data = ctx.builder.CreateSelect(isboxed_union, retvalinfo.Vboxed, data);
                    }
                }
                else {
                    // treat this as a simple boxed returninfo
                    //assert(retvalinfo.isboxed);
                    tindex = compute_tindex_unboxed(ctx, retvalinfo, jlrettype);
                    tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
                    data = boxed(ctx, retvalinfo);
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
                        emit_sret_roots(ctx, true, data_pointer(ctx, retvalinfo), store_ty, f->arg_begin() + 1, get_returnroots_type(ctx, returninfo.return_roots), returninfo.return_roots);
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
                        emit_sret_roots(ctx, false, Val, store_ty, f->arg_begin() + 1, get_returnroots_type(ctx, returninfo.return_roots), returninfo.return_roots);
                    }
                    if (dest_ty != sret->getType())
                        sret = emit_bitcast(ctx, sret, dest_ty);
                    ctx.builder.CreateAlignedStore(Val, sret, Align(julia_alignment(retvalinfo.typ)));
                    assert(retvalinfo.TIndex == NULL && "unreachable"); // unimplemented representation
                }
            }

            mallocVisitStmt(debuginfoloc, sync_bytes);
            if (toplevel || ctx.is_opaque_closure)
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
            emit_upsilonnode(ctx, upsilon_to_phic[cursor + 1], jl_fieldref_noalloc(stmt, 0));
            find_next_stmt(cursor + 1);
            continue;
        }
        if (jl_is_gotoifnot(stmt)) {
            jl_value_t *cond = jl_gotoifnot_cond(stmt);
            int lname = jl_gotoifnot_label(stmt);
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
        else if (expr && expr->head == jl_enter_sym) {
            jl_value_t **args = (jl_value_t**)jl_array_data(expr->args);

            assert(jl_is_long(args[0]));
            int lname = jl_unbox_long(args[0]);
            // Save exception stack depth at enter for use in pop_exception
            Value *excstack_state =
                ctx.builder.CreateCall(prepare_call(jl_excstack_state_func));
            assert(!ctx.ssavalue_assigned.at(cursor));
            ctx.SAvalues.at(cursor) = jl_cgval_t(excstack_state, NULL, false,
                                                 (jl_value_t*)jl_ulong_type, NULL, ctx.tbaa());
            ctx.ssavalue_assigned.at(cursor) = true;
            CallInst *sj = ctx.builder.CreateCall(prepare_call(except_enter_func));
            // We need to mark this on the call site as well. See issue #6757
            sj->setCanReturnTwice();
            Value *isz = ctx.builder.CreateICmpEQ(sj, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0));
            BasicBlock *tryblk = BasicBlock::Create(ctx.builder.getContext(), "try", f);
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

    // Codegen Phi nodes
    std::map<std::pair<BasicBlock*, BasicBlock*>, BasicBlock*> BB_rewrite_map;
    std::vector<llvm::PHINode*> ToDelete;
    for (auto &tup : ctx.PhiNodes) {
        jl_cgval_t phi_result(ctx.builder.getContext());
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
            size_t edge = ((int32_t*)jl_array_data(edges))[i];
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
                    size_t j_edge = ((int32_t*)jl_array_data(edges))[j];
                    if (j_edge == edge) {
                        assert(jl_egal(value, jl_array_ptr_ref(values, j)));
                    }
                }
#endif
                continue;
            }
            assert(std::find(pred_begin(PhiBB), pred_end(PhiBB), FromBB) != pred_end(PhiBB)); // consistency check
            TerminatorInst *terminator = FromBB->getTerminator();
            if (!terminator->getParent()->getUniqueSuccessor()) {
                // Can't use `llvm::SplitCriticalEdge` here because
                // we may have invalid phi nodes in the destination.
                BasicBlock *NewBB = BasicBlock::Create(terminator->getContext(),
                   FromBB->getName() + "." + PhiBB->getName() + "_crit_edge");
                Function::iterator FBBI = FromBB->getIterator();
                ctx.f->getBasicBlockList().insert(++FBBI, NewBB); // insert after existing block
                terminator->replaceSuccessorWith(PhiBB, NewBB);
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
                val = mark_julia_const(ctx, val.constant); // be over-conservative at making sure `.typ` is set concretely, not tindex
            if (!jl_is_uniontype(phiType) || !TindexN) {
                Type *lty = julia_type_to_llvm(ctx, phiType);
                if (VN) {
                    Value *V;
                    if (val.typ == (jl_value_t*)jl_bottom_type) {
                        V = undef_value_for_type(VN->getType());
                    }
                    else if (VN->getType() == ctx.types().T_prjlvalue) {
                        // Includes the jl_is_uniontype(phiType) && !TindexN case
                        // TODO: if convert_julia_type says it is wasted effort and to skip it, is it worth using Constant::getNullValue(ctx.types().T_prjlvalue) (dynamically)?
                        V = boxed(ctx, val);
                    }
                    else {
                        // must be careful to emit undef here (rather than a bitcast or
                        // load of val) if the runtime type of val isn't phiType
                        Value *isvalid = emit_isa_and_defined(ctx, val, phiType);
                        V = emit_guarded_test(ctx, isvalid, undef_value_for_type(VN->getType()), [&] {
                            return emit_unbox(ctx, VN->getType(), val, phiType);
                        });
                    }
                    VN->addIncoming(V, ctx.builder.GetInsertBlock());
                    assert(!TindexN);
                }
                else if (dest && val.typ != (jl_value_t*)jl_bottom_type) {
                    // must be careful to emit undef here (rather than a bitcast or
                    // load of val) if the runtime type of val isn't phiType
                    assert(lty != ctx.types().T_prjlvalue);
                    Value *isvalid = emit_isa_and_defined(ctx, val, phiType);
                    emit_guarded_test(ctx, isvalid, nullptr, [&] {
                        (void)emit_unbox(ctx, lty, val, phiType, maybe_decay_tracked(ctx, dest), ctx.tbaa().tbaa_stack);
                        return nullptr;
                    });
                }
            }
            else {
                Value *RTindex;
                // The branch below is a bit too complex for GCC to realize that
                // `V` is always initialized when it is used.
                // Ref https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96629
                Value *V = nullptr;
                if (val.typ == (jl_value_t*)jl_bottom_type) {
                    if (VN)
                        V = undef_value_for_type(VN->getType());
                    RTindex = UndefValue::get(getInt8Ty(ctx.builder.getContext()));
                }
                else if (jl_is_concrete_type(val.typ) || val.constant) {
                    size_t tindex = get_box_tindex((jl_datatype_t*)val.typ, phiType);
                    if (tindex == 0) {
                        if (VN)
                            V = boxed(ctx, val);
                        RTindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80);
                    }
                    else {
                        if (VN)
                            V = Constant::getNullValue(ctx.types().T_prjlvalue);
                        Type *lty = julia_type_to_llvm(ctx, val.typ);
                        if (dest && !type_is_ghost(lty)) // basically, if !ghost union
                            emit_unbox(ctx, lty, val, val.typ, dest, ctx.tbaa().tbaa_stack);
                        RTindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), tindex);
                    }
                }
                else {
                    Value *skip = NULL;
                    // must compute skip here, since the runtime type of val might not be in phiType
                    // caution: only Phi and PhiC are allowed to do this (and maybe sometimes Pi)
                    jl_cgval_t new_union = convert_julia_type(ctx, val, phiType, &skip);
                    RTindex = new_union.TIndex;
                    if (!RTindex) {
                        assert(new_union.isboxed && new_union.Vboxed && "convert_julia_type failed");
                        RTindex = compute_tindex_unboxed(ctx, new_union, phiType, true);
                        if (dest) {
                            // If dest is not set, this is a ghost union, the recipient of which
                            // is often not prepared to handle a boxed representation of the ghost.
                            RTindex = ctx.builder.CreateOr(RTindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80));
                        }
                        new_union.TIndex = RTindex;
                    }
                    if (VN)
                        V = new_union.Vboxed ? new_union.Vboxed : Constant::getNullValue(ctx.types().T_prjlvalue);
                    if (dest) { // basically, if !ghost union
                        if (new_union.Vboxed != nullptr) {
                            Value *isboxed = ctx.builder.CreateICmpNE( // if 0x80 is set, we won't select this slot anyways
                                    ctx.builder.CreateAnd(RTindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80)),
                                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
                            skip = skip ? ctx.builder.CreateOr(isboxed, skip) : isboxed;
                        }
                        emit_unionmove(ctx, dest, ctx.tbaa().tbaa_arraybuf, new_union, skip);
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
                PhiBB->replacePhiUsesWith(FromBB, NewBB);
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
            Value *RTindex = TindexN ? UndefValue::get(getInt8Ty(ctx.builder.getContext())) : NULL;
            if (VN) {
                Value *undef = undef_value_for_type(VN->getType());
                VN->addIncoming(undef, FromBB);
                if (TindexN) // let the runtime / optimizer know this is unknown / boxed / null, so that it won't try to union_move / copy it later
                    RTindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x80);
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

    // step 12. Perform any delayed instantiations
    if (ctx.debug_enabled) {
        bool in_prologue = true;
        for (auto &BB : *ctx.f) {
            for (auto &I : BB) {
                CallBase *call = dyn_cast<CallBase>(&I);
                if (call && !I.getDebugLoc()) {
                    // LLVM Verifier: inlinable function call in a function with debug info must have a !dbg location
                    // make sure that anything we attempt to call has some inlining info, just in case optimization messed up
                    // (except if we know that it is an intrinsic used in our prologue, which should never have its own debug subprogram)
                    Function *F = call->getCalledFunction();
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
                    jl_add_method_root(m, jl_precompile_toplevel_module, ival);
            }
        }
        ctx.roots = NULL;
        JL_UNLOCK(&m->writelock);
    }

    // link the dependent llvmcall modules, but switch their function's linkage to internal
    // so that they don't conflict when they show up in the execution engine.
    for (auto &TSMod : ctx.llvmcall_modules) {
        SmallVector<std::string, 1> Exports;
        TSMod.withModuleDo([&](Module &Mod) {
            for (const auto &F: Mod.functions())
                if (!F.isDeclaration())
                    Exports.push_back(F.getName().str());
        });
        jl_merge_module(TSM, std::move(TSMod));
        for (auto FN: Exports)
            jl_Module->getFunction(FN)->setLinkage(GlobalVariable::InternalLinkage);
    }

    // link in opaque closure modules
    for (auto &TSMod : ctx.oc_modules) {
        SmallVector<std::string, 1> Exports;
        TSMod.withModuleDo([&](Module &Mod) {
            for (const auto &F: Mod.functions())
                if (!F.isDeclaration())
                    Exports.push_back(F.getName().str());
        });
        jl_merge_module(TSM, std::move(TSMod));
        for (auto FN: Exports)
            jl_Module->getFunction(FN)->setLinkage(GlobalVariable::InternalLinkage);
    }

    JL_GC_POP();
    return declarations;
}

// --- entry point ---

void jl_add_code_in_flight(StringRef name, jl_code_instance_t *codeinst, const DataLayout &DL);

JL_GCC_IGNORE_START("-Wclobbered")
jl_llvm_functions_t jl_emit_code(
        orc::ThreadSafeModule &m,
        jl_method_instance_t *li,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
{
    JL_TIMING(CODEGEN);
    // caller must hold codegen_lock
    jl_llvm_functions_t decls = {};
    assert((params.params == &jl_default_cgparams /* fast path */ || !params.cache ||
        compare_cgparams(params.params, &jl_default_cgparams)) &&
        "functions compiled with custom codegen params must not be cached");
    JL_TRY {
        decls = emit_function(m, li, src, jlrettype, params);
        auto stream = *jl_ExecutionEngine->get_dump_emitted_mi_name_stream();
        if (stream) {
            jl_printf(stream, "%s\t", decls.specFunctionObject.c_str());
            // NOTE: We print the Type Tuple without surrounding quotes, because the quotes
            // break CSV parsing if there are any internal quotes in the Type name (e.g. in
            // Symbol("...")). The \t delineator should be enough to ensure whitespace is
            // handled correctly. (And we don't need to worry about any tabs in the printed
            // string, because tabs are printed as "\t" by `show`.)
            jl_static_show(stream, li->specTypes);
            jl_printf(stream, "\n");
        }
    }
    JL_CATCH {
        // Something failed! This is very, very bad.
        // Try to pretend that it isn't and attempt to recover.
        const char *mname = m.getModuleUnlocked()->getModuleIdentifier().data();
        m = orc::ThreadSafeModule();
        decls.functionObject = "";
        decls.specFunctionObject = "";
        jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: encountered unexpected error during compilation of %s:\n", mname);
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception());
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        jlbacktrace(); // written to STDERR_FILENO
    }

    return decls;
}

jl_llvm_functions_t jl_emit_codeinst(
        orc::ThreadSafeModule &m,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params)
{
    JL_TIMING(CODEGEN);
    JL_GC_PUSH1(&src);
    if (!src) {
        src = (jl_code_info_t*)codeinst->inferred;
        jl_method_t *def = codeinst->def->def.method;
        if (src && (jl_value_t*)src != jl_nothing && jl_is_method(def))
            src = jl_uncompress_ir(def, codeinst, (jl_array_t*)src);
        if (!src || !jl_is_code_info(src)) {
            JL_GC_POP();
            m = orc::ThreadSafeModule();
            return jl_llvm_functions_t(); // failed
        }
    }
    jl_llvm_functions_t decls = jl_emit_code(m, codeinst->def, src, codeinst->rettype, params);

    const std::string &specf = decls.specFunctionObject;
    const std::string &f = decls.functionObject;
    if (params.cache && !f.empty()) {
        // Prepare debug info to receive this function
        // record that this function name came from this linfo,
        // so we can build a reverse mapping for debug-info.
        bool toplevel = !jl_is_method(codeinst->def->def.method);
        if (!toplevel) {
            //Safe b/c params holds context lock
            const DataLayout &DL = m.getModuleUnlocked()->getDataLayout();
            // but don't remember toplevel thunks because
            // they may not be rooted in the gc for the life of the program,
            // and the runtime doesn't notify us when the code becomes unreachable :(
            if (!specf.empty())
                jl_add_code_in_flight(specf, codeinst, DL);
            if (!f.empty() && f != "jl_fptr_args" && f != "jl_fptr_sparam")
                jl_add_code_in_flight(f, codeinst, DL);
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
                    if (jl_is_method(def)) {
                        src = (jl_code_info_t*)jl_compress_ir(def, src);
                        assert(jl_typeis(src, jl_array_uint8_type));
                        codeinst->relocatability = ((uint8_t*)jl_array_data(src))[jl_array_len(src)-1];
                    }
                    codeinst->inferred = (jl_value_t*)src;
                    jl_gc_wb(codeinst, src);
                }
            }
            else if (// don't delete toplevel code
                     jl_is_method(def) &&
                     // and there is something to delete (test this before calling jl_ir_flag_inlineable)
                     codeinst->inferred != jl_nothing &&
                     // don't delete inlineable code, unless it is constant
                     (codeinst->invoke == jl_fptr_const_return_addr || !jl_ir_flag_inlineable((jl_array_t*)codeinst->inferred)) &&
                     // don't delete code when generating a precompile file
                     !(params.imaging || jl_options.incremental)) {
                // if not inlineable, code won't be needed again
                codeinst->inferred = jl_nothing;
            }
        }
    }
    JL_GC_POP();
    return decls;
}


void jl_compile_workqueue(
    jl_workqueue_t &emitted,
    Module &original,
    jl_codegen_params_t &params, CompilationPolicy policy)
{
    JL_TIMING(CODEGEN);
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    while (!params.workqueue.empty()) {
        jl_code_instance_t *codeinst;
        Function *protodecl;
        jl_returninfo_t::CallingConv proto_cc;
        bool proto_specsig;
        unsigned proto_return_roots;
        auto it = params.workqueue.back();
        codeinst = it.first;
        std::tie(proto_cc, proto_return_roots, protodecl, proto_specsig) = it.second;
        params.workqueue.pop_back();
        // try to emit code for this item from the workqueue
        assert(codeinst->min_world <= params.world && codeinst->max_world >= params.world &&
            "invalid world for code-instance");
        StringRef preal_decl = "";
        bool preal_specsig = false;
        auto invoke = jl_atomic_load_relaxed(&codeinst->invoke);
        if (params.cache && invoke != NULL) {
            auto fptr = jl_atomic_load_relaxed(&codeinst->specptr.fptr);
            if (invoke == jl_fptr_args_addr) {
                preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, codeinst);
            }
            else if (codeinst->isspecsig) {
                preal_decl = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, codeinst);
                preal_specsig = true;
            }
        }
        else {
            auto &result = emitted[codeinst];
            jl_llvm_functions_t *decls = NULL;
            if (std::get<0>(result)) {
                decls = &std::get<1>(result);
            }
            else {
                // Reinfer the function. The JIT came along and removed the inferred
                // method body. See #34993
                if (policy != CompilationPolicy::Default &&
                    codeinst->inferred && codeinst->inferred == jl_nothing) {
                    src = jl_type_infer(codeinst->def, jl_atomic_load_acquire(&jl_world_counter), 0);
                    if (src) {
                        orc::ThreadSafeModule result_m =
                        jl_create_llvm_module(name_from_method_instance(codeinst->def),
                            params.tsctx, params.imaging,
                            original.getDataLayout(), Triple(original.getTargetTriple()));
                        result.second = jl_emit_code(result_m, codeinst->def, src, src->rettype, params);
                        result.first = std::move(result_m);
                    }
                }
                else {
                    orc::ThreadSafeModule result_m =
                        jl_create_llvm_module(name_from_method_instance(codeinst->def),
                            params.tsctx, params.imaging,
                            original.getDataLayout(), Triple(original.getTargetTriple()));
                    result.second = jl_emit_codeinst(result_m, codeinst, NULL, params);
                    result.first = std::move(result_m);
                }
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
                protodecl->setLinkage(GlobalVariable::InternalLinkage);
                //protodecl->setAlwaysInline();
                jl_init_function(protodecl);
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
    JL_GC_POP();
}


// --- initialization ---
std::vector<std::pair<jl_value_t**, JuliaVariable*>> gv_for_global;
static void global_jlvalue_to_llvm(JuliaVariable *var, jl_value_t **addr)
{
    gv_for_global.push_back(std::make_pair(addr, var));
}
static JuliaVariable *julia_const_gv(jl_value_t *val)
{
    for (auto &kv : gv_for_global) {
        if (*kv.first == val)
            return kv.second;
    }
    return nullptr;
}

static void init_jit_functions(void)
{
    add_named_global(jlstack_chk_guard_var, &__stack_chk_guard);
    add_named_global(jlRTLD_DEFAULT_var, &jl_RTLD_DEFAULT_handle);
#ifdef _OS_WINDOWS_
    add_named_global(jlexe_var, &jl_exe_handle);
    add_named_global(jldll_var, &jl_libjulia_handle);
    add_named_global(jldlli_var, &jl_libjulia_internal_handle);
#endif
    global_jlvalue_to_llvm(new JuliaVariable{"jl_true", true, get_pjlvalue}, &jl_true);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_false", true, get_pjlvalue}, &jl_false);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_emptysvec", true, get_pjlvalue}, (jl_value_t**)&jl_emptysvec);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_emptytuple", true, get_pjlvalue}, &jl_emptytuple);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_diverror_exception", true, get_pjlvalue}, &jl_diverror_exception);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_undefref_exception", true, get_pjlvalue}, &jl_undefref_exception);
    add_named_global(jlgetworld_global, &jl_world_counter);
    add_named_global("__stack_chk_fail", &__stack_chk_fail);
    add_named_global(jlpgcstack_func, (void*)NULL);
    add_named_global(jlerror_func, &jl_error);
    add_named_global(jlatomicerror_func, &jl_atomic_error);
    add_named_global(jlthrow_func, &jl_throw);
    add_named_global(jlundefvarerror_func, &jl_undefined_var_error);
    add_named_global(jlboundserrorv_func, &jl_bounds_error_ints);
    add_named_global(jlboundserror_func, &jl_bounds_error_int);
    add_named_global(jlvboundserror_func, &jl_bounds_error_tuple_int);
    add_named_global(jluboundserror_func, &jl_bounds_error_unboxed_int);
    add_named_global(jlnew_func, &jl_new_structv);
    add_named_global(jlsplatnew_func, &jl_new_structt);
    add_named_global(setjmp_func, &jl_setjmp_f);
    add_named_global(memcmp_func, &memcmp);
    add_named_global(jltypeerror_func, &jl_type_error);
    add_named_global(jlcheckassign_func, &jl_checked_assignment);
    add_named_global(jldeclareconst_func, &jl_declare_constant);
    add_named_global(jlgetbindingorerror_func, &jl_get_binding_or_error);
    add_named_global(jlgetbindingwrorerror_func, &jl_get_binding_wr_or_error);
    add_named_global(jlboundp_func, &jl_boundp);
    for (auto it : builtin_func_map())
        add_named_global(it.second, it.first);
    add_named_global(jlapplygeneric_func, &jl_apply_generic);
    add_named_global(jlinvoke_func, &jl_invoke);
    add_named_global(jltopeval_func, &jl_toplevel_eval);
    add_named_global(jlcopyast_func, &jl_copy_ast);
    //add_named_global(jlnsvec_func, &jl_svec);
    add_named_global(jlmethod_func, &jl_method_def);
    add_named_global(jlgenericfunction_func, &jl_generic_function_def);
    add_named_global(jlenter_func, &jl_enter_handler);
    add_named_global(jl_current_exception_func, &jl_current_exception);
    add_named_global(jlleave_func, &jl_pop_handler);
    add_named_global(jl_restore_excstack_func, &jl_restore_excstack);
    add_named_global(jl_excstack_state_func, &jl_excstack_state);
    add_named_global(jlegalx_func, &jl_egal__unboxed);
    add_named_global(jlisa_func, &jl_isa);
    add_named_global(jlsubtype_func, &jl_subtype);
    add_named_global(jltypeassert_func, &jl_typeassert);
    add_named_global(jlapplytype_func, &jl_instantiate_type_in_env);
    add_named_global(jl_object_id__func, &jl_object_id_);
    add_named_global(jl_alloc_obj_func, (void*)NULL);
    add_named_global(jl_newbits_func, (void*)jl_new_bits);
    add_named_global(jl_loopinfo_marker_func, (void*)NULL);
    add_named_global(jl_typeof_func, (void*)NULL);
    add_named_global(jl_write_barrier_func, (void*)NULL);
    add_named_global(jl_write_barrier_binding_func, (void*)NULL);
    add_named_global(jldlsym_func, &jl_load_and_lookup);
    add_named_global(jlgetcfunctiontrampoline_func, &jl_get_cfunction_trampoline);
    add_named_global(jlgetnthfieldchecked_func, &jl_get_nth_field_checked);
    add_named_global(diff_gc_total_bytes_func, &jl_gc_diff_total_bytes);
    add_named_global(sync_gc_total_bytes_func, &jl_gc_sync_total_bytes);
    add_named_global(jlarray_data_owner_func, &jl_array_data_owner);
    add_named_global(gcroot_flush_func, (void*)NULL);
    add_named_global(gc_preserve_begin_func, (void*)NULL);
    add_named_global(gc_preserve_end_func, (void*)NULL);
    add_named_global(pointer_from_objref_func, (void*)NULL);
    add_named_global(except_enter_func, (void*)NULL);

#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
#if defined(_COMPILER_GCC_)
    add_named_global("___chkstk_ms", &___chkstk_ms);
#else
    add_named_global("__chkstk", &__chkstk);
#endif
#else
#if defined(_COMPILER_GCC_)
    add_named_global("_alloca", &_alloca);
#else
    add_named_global("_chkstk", &_chkstk);
#endif
#endif
#endif

#define BOX_F(ct) add_named_global(XSTR(jl_box_##ct), &jl_box_##ct);
    BOX_F(int8); BOX_F(uint8);
    BOX_F(int16); BOX_F(uint16);
    BOX_F(int32); BOX_F(uint32);
    BOX_F(int64); BOX_F(uint64);
    BOX_F(float32); BOX_F(float64);
    BOX_F(char); BOX_F(ssavalue);
#undef BOX_F
}

#ifdef JL_USE_INTEL_JITEVENTS
char jl_using_intel_jitevents; // Non-zero if running under Intel VTune Amplifier
#endif

#ifdef JL_USE_OPROFILE_JITEVENTS
char jl_using_oprofile_jitevents = 0; // Non-zero if running under OProfile
#endif

#ifdef JL_USE_PERF_JITEVENTS
char jl_using_perf_jitevents = 0;
#endif

extern "C" void jl_init_llvm(void)
{
    jl_page_size = jl_getpagesize();
    jl_default_debug_info_kind = (int) DICompileUnit::DebugEmissionKind::FullDebug;
    jl_default_cgparams.generic_context = jl_nothing;

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetDisassembler();

    // Initialize passes
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    initializeCore(Registry);
    initializeCoroutines(Registry);
    initializeScalarOpts(Registry);
    initializeVectorization(Registry);
    initializeAnalysis(Registry);
    initializeTransformUtils(Registry);
    initializeInstCombine(Registry);
    initializeAggressiveInstCombine(Registry);
    initializeInstrumentation(Registry);
    initializeTarget(Registry);
#ifdef USE_POLLY
    polly::initializePollyPasses(Registry);
#endif

    // Parse command line flags after initialization
    StringMap<cl::Option*> &llvmopts = cl::getRegisteredOptions();
    const char *const argv[1] = {"julia"};
    cl::ParseCommandLineOptions(1, argv, "", nullptr, "JULIA_LLVM_ARGS");

    // Set preferred non-default options
    cl::Option *clopt;
    clopt = llvmopts.lookup("enable-tail-merge"); // NOO TOUCHIE; NO TOUCH! See #922
    if (clopt->getNumOccurrences() == 0)
        cl::ProvidePositionalOption(clopt, "0", 1);
    // if the patch adding this option has been applied, lower its limit to provide
    // better DAGCombiner performance.
    clopt = llvmopts.lookup("combiner-store-merge-dependence-limit");
    if (clopt && clopt->getNumOccurrences() == 0)
        cl::ProvidePositionalOption(clopt, "4", 1);

    jl_ExecutionEngine = new JuliaOJIT();

    bool jl_using_gdb_jitevents = false;
    // Register GDB event listener
#if defined(JL_DEBUG_BUILD)
    jl_using_gdb_jitevents = true;
# else
    const char *jit_gdb = getenv("ENABLE_GDBLISTENER");
    if (jit_gdb && atoi(jit_gdb)) {
        jl_using_gdb_jitevents = true;
    }
#endif
    if (jl_using_gdb_jitevents)
        jl_ExecutionEngine->enableJITDebuggingSupport();

#if defined(JL_USE_INTEL_JITEVENTS) || \
    defined(JL_USE_OPROFILE_JITEVENTS) || \
    defined(JL_USE_PERF_JITEVENTS)
#ifdef JL_USE_JITLINK
#error "JIT profiling support (JL_USE_*_JITEVENTS) not yet available on platforms that use JITLink"
#else
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");

#if defined(JL_USE_INTEL_JITEVENTS)
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_intel_jitevents = 1;
    }
#endif

#if defined(JL_USE_OPROFILE_JITEVENTS)
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_oprofile_jitevents = 1;
    }
#endif

#if defined(JL_USE_PERF_JITEVENTS)
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_perf_jitevents= 1;
    }
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
#endif
#endif

    cl::PrintOptionValues();
}

extern "C" JL_DLLEXPORT void jl_init_codegen_impl(void)
{
    jl_init_llvm();
    // Now that the execution engine exists, initialize all modules
    init_jit_functions();
}

extern "C" JL_DLLEXPORT void jl_teardown_codegen_impl()
{
    // output LLVM timings and statistics
    reportAndResetTimings();
    PrintStatistics();
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

namespace llvm {
    class MachineBasicBlock;
    class MachineFunction;
    raw_ostream& operator<<(raw_ostream &OS, const MachineBasicBlock &MBB);
    void printMIR(raw_ostream &OS, const MachineFunction &MF);
}
extern "C" void jl_dump_llvm_mbb(void *v)
{
    errs() << *(llvm::MachineBasicBlock*)v;
}
extern "C" void jl_dump_llvm_mfunction(void *v)
{
    llvm::printMIR(errs(), *(llvm::MachineFunction*)v);
}


extern void jl_write_bitcode_func(void *F, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::OF_None);
    llvm::WriteBitcodeToFile(*((llvm::Function*)F)->getParent(), OS);
}

extern void jl_write_bitcode_module(void *M, char *fname) {
    std::error_code EC;
    raw_fd_ostream OS(fname, EC, sys::fs::OF_None);
    llvm::WriteBitcodeToFile(*(llvm::Module*)M, OS);
}

#ifdef _OS_WINDOWS_
#include <psapi.h>
#else
#include <dlfcn.h>
#endif

#include <llvm-c/Core.h>

extern "C" JL_DLLEXPORT jl_value_t *jl_get_libllvm_impl(void) JL_NOTSAFEPOINT
{
#if defined(_OS_WINDOWS_)
    HMODULE mod;
    if (!GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, (LPCSTR)&llvm::DebugFlag, &mod))
        return jl_nothing;

    char path[MAX_PATH];
    if (!GetModuleFileNameA(mod, path, sizeof(path)))
        return jl_nothing;
    return (jl_value_t*) jl_symbol(path);
#else
    Dl_info dli;
    if (!dladdr((void*)LLVMContextCreate, &dli))
        return jl_nothing;
    return (jl_value_t*) jl_symbol(dli.dli_fname);
#endif
}
