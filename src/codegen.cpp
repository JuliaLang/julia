// This file is a part of Julia. License is MIT: https://julialang.org/license

#undef DEBUG
#include "llvm-version.h"
#include "platform.h"

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
#include <unordered_set>
#include <functional>

// target machine computation
#include <llvm/CodeGen/TargetSubtargetInfo.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
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
#include <llvm/Analysis/InstructionSimplify.h>

// support
#include <llvm/ADT/SmallBitVector.h>
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

#ifdef USE_ITTAPI
#include "ittapi/ittnotify.h"
#endif

using namespace llvm;

static bool jl_fpo_disabled(const Triple &TT) {
#ifdef JL_DISABLE_FPO
    return true;
#endif
#ifdef _COMPILER_MSAN_ENABLED_
    // MSAN doesn't support FPO
    return true;
#endif
    if (TT.isOSLinux() || TT.isOSWindows() || TT.isOSFreeBSD() || TT.isOSOpenBSD()) {
        return true;
    }
    return false;
}

static bool jl_floattemp_var_needed(const Triple &TT) {
#ifdef JL_NEED_FLOATTEMP_VAR
    return true;
#endif
    return TT.getArch() == Triple::x86;
}

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
auto getBFloatTy(LLVMContext &ctxt) {
    return Type::getBFloatTy(ctxt);
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
auto getPointerTy(LLVMContext &ctxt) {
    return PointerType::get(ctxt, 0);
}

typedef Instruction TerminatorInst;

#if defined(_OS_WINDOWS_) && !defined(NOMINMAX)
#define NOMINMAX
#endif

#include "jitlayers.h"
#include "processor.h"
#include "julia_assert.h"

#undef DEBUG_TYPE //LLVM occasionally likes to set DEBUG_TYPE in a header...
#define DEBUG_TYPE "julia_irgen_codegen"

void setName(jl_codegen_params_t &params, Value *V, const Twine &Name)
{
    // we do the constant check again later, duplicating it here just makes sure the assertion
    // fires on debug builds even if debug info is not enabled
    // note that if this assertion fires then the implication is that the caller of setName
    // is not checking that setName is only called for non-folded instructions (e.g. folded bitcasts
    // and 0-byte geps), which can result in information loss on the renamed instruction.
    assert((isa<Constant>(V) || isa<Instruction>(V)) && "Should only set names on instructions!");
    if (!isa<Constant>(V)) {
        V->setName(Name);
    }
}

void maybeSetName(jl_codegen_params_t &params, Value *V, const Twine &Name)
{
    // To be used when we may get an Instruction or something that is not an instruction i.e Constants/Arguments
    if (isa<Instruction>(V))
        V->setName(Name);
}

void setName(jl_codegen_params_t &params, Value *V, std::function<std::string()> GetName)
{
    assert((isa<Constant>(V) || isa<Instruction>(V)) && "Should only set names on instructions!");
    if (!params.getContext().shouldDiscardValueNames() && !isa<Constant>(V))
        V->setName(Twine(GetName()));
}

void setNameWithField(jl_codegen_params_t &params, Value *V, std::function<StringRef()> GetObjName, jl_datatype_t *jt, unsigned idx, const Twine &suffix)
{
    assert((isa<Constant>(V) || isa<Instruction>(V)) && "Should only set names on instructions!");
    if (!params.getContext().shouldDiscardValueNames() && !isa<Constant>(V)) {
        if (jl_is_tuple_type(jt)){
            V->setName(Twine(GetObjName()) + "[" + Twine(idx + 1) + "]"+ suffix);
            return;
        }

        if (jl_is_namedtuple_type(jt)) {
            auto names = jl_tparam0(jt);
            assert(jl_is_tuple(names));
            if (idx < jl_nfields(names)) {
                auto name = jl_fieldref(names, idx);
                assert(jl_is_symbol(name));
                V->setName(Twine(GetObjName()) + "." + Twine(jl_symbol_name((jl_sym_t*)name)) + suffix);
                return;
            }
        } else {
            auto flds = jl_field_names(jt);
            if (idx < jl_svec_len(flds)) {
                auto name = jl_svecref(flds, idx);
                assert(jl_is_symbol(name));
                V->setName(Twine(GetObjName()) + "." + Twine(jl_symbol_name((jl_sym_t*)name)) + suffix);
                return;
            }
        }
        V->setName(Twine(GetObjName()) + "." + Twine("unknown field") + suffix);
    }
}

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
STATISTIC(EmittedToJLInvokes, "Number of tojlinvoke calls emitted");
STATISTIC(EmittedCFuncInvalidates, "Number of C function invalidates emitted");
STATISTIC(GeneratedCFuncWrappers, "Number of C function wrappers generated");
STATISTIC(GeneratedCCallables, "Number of C-callable functions generated");
STATISTIC(GeneratedInvokeWrappers, "Number of invoke wrappers generated");
STATISTIC(EmittedFunctions, "Number of functions emitted");

extern "C" JL_DLLEXPORT_CODEGEN
void jl_dump_emitted_mi_name_impl(void *s)
{
    **jl_ExecutionEngine->get_dump_emitted_mi_name_stream() = (ios_t*)s;
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
    PointerType *T_ptr;
    Type *T_size;
    Type *T_jlvalue;
    PointerType *T_pjlvalue;
    PointerType *T_prjlvalue;
    PointerType *T_ppjlvalue;
    PointerType *T_pprjlvalue;
    StructType *T_jlgenericmemory;
    StructType *T_jlarray;
    PointerType *T_pjlarray;
    FunctionType *T_jlfunc;
    FunctionType *T_jlfuncparams;

    IntegerType *T_sigatomic;

    unsigned sizeof_ptr;
    Align alignof_ptr;

    bool initialized;

    jl_typecache_t() :
        T_ptr(nullptr), T_jlvalue(nullptr), T_pjlvalue(nullptr), T_prjlvalue(nullptr),
        T_ppjlvalue(nullptr), T_pprjlvalue(nullptr),
        T_jlgenericmemory(nullptr), T_jlarray(nullptr), T_pjlarray(nullptr),
        T_jlfunc(nullptr), T_jlfuncparams(nullptr), T_sigatomic(nullptr),
        initialized(false) {}

    void initialize(LLVMContext &context, const DataLayout &DL) {
        if (initialized) {
            return;
        }
        initialized = true;
        T_ptr = getPointerTy(context);
        T_sigatomic = Type::getIntNTy(context, sizeof(sig_atomic_t) * 8);
        T_size = DL.getIntPtrType(context);
        sizeof_ptr = DL.getPointerSize();
        // use pointer abi alignment for intptr_t
        alignof_ptr = DL.getPointerABIAlignment(0);
        T_jlvalue = JuliaType::get_jlvalue_ty(context);
        T_pjlvalue = PointerType::get(T_jlvalue, 0);
        T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        T_ppjlvalue = PointerType::get(T_pjlvalue, 0);
        T_pprjlvalue = PointerType::get(T_prjlvalue, 0);

        T_jlfunc = JuliaType::get_jlfunc_ty(context);
        assert(T_jlfunc != NULL);
        T_jlfuncparams = JuliaType::get_jlfuncparams_ty(context);
        assert(T_jlfuncparams != NULL);

        T_jlgenericmemory = StructType::get(context, { T_size, T_pprjlvalue /* [, real-owner] */ });
        Type *vaelts[] = { PointerType::get(getInt8Ty(context), AddressSpace::Loaded),
                           PointerType::get(T_jlgenericmemory, AddressSpace::Tracked),
                           // dimsize[ndims]
        };
        T_jlarray = StructType::get(context, ArrayRef<Type*>(vaelts));
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
    MDNode *tbaa_value;          // jl_value_t, that is not jl_array_t or jl_genericmemory_t
    MDNode *tbaa_mutab;              // mutable type
    MDNode *tbaa_datatype;               // datatype
    MDNode *tbaa_immut;              // immutable type
    MDNode *tbaa_ptrarraybuf;    // Data in an array of boxed values
    MDNode *tbaa_arraybuf;       // Data in an array of POD
    MDNode *tbaa_array;      // jl_array_t or jl_genericmemory_t
    MDNode *tbaa_arrayptr;       // The pointer inside a jl_array_t (to memoryref)
    MDNode *tbaa_arraysize;      // A size in a jl_array_t
    MDNode *tbaa_arrayselbyte;   // a selector byte in a isbits Union jl_genericmemory_t
    MDNode *tbaa_memoryptr;      // The pointer inside a jl_genericmemory_t
    MDNode *tbaa_memorylen;      // The length in a jl_genericmemory_t
    MDNode *tbaa_memoryown;      // The owner in a foreign jl_genericmemory_t
    MDNode *tbaa_const;      // Memory that is immutable by the time LLVM can see it
    bool initialized;

    jl_tbaacache_t(): tbaa_root(nullptr), tbaa_gcframe(nullptr), tbaa_stack(nullptr),
                    tbaa_unionselbyte(nullptr), tbaa_data(nullptr), tbaa_binding(nullptr),
                    tbaa_value(nullptr), tbaa_mutab(nullptr), tbaa_datatype(nullptr),
                    tbaa_immut(nullptr), tbaa_ptrarraybuf(nullptr), tbaa_arraybuf(nullptr),
                    tbaa_array(nullptr), tbaa_arrayptr(nullptr), tbaa_arraysize(nullptr),
                    tbaa_arrayselbyte(nullptr), tbaa_memoryptr(nullptr), tbaa_memorylen(nullptr), tbaa_memoryown(nullptr),
                    tbaa_const(nullptr), initialized(false) {}

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
        tbaa_arrayselbyte = tbaa_make_child(mbuilder, "jtbaa_arrayselbyte", tbaa_array_scalar).first;
        tbaa_memoryptr = tbaa_make_child(mbuilder, "jtbaa_memoryptr", tbaa_array_scalar).first;
        tbaa_memorylen = tbaa_make_child(mbuilder, "jtbaa_memorylen", tbaa_array_scalar).first;
        tbaa_memoryown = tbaa_make_child(mbuilder, "jtbaa_memoryown", tbaa_array_scalar).first;
        tbaa_const = tbaa_make_child(mbuilder, "jtbaa_const", nullptr, true).first;
    }
};

struct jl_noaliascache_t {
    // Each domain operates completely independently.
    // "No aliasing" is inferred if it is implied by any domain.

    // memory regions domain
    struct jl_regions_t {
        MDNode *gcframe;        // GC frame
        MDNode *stack;          // Stack slot
        MDNode *data;           // Any user data that `pointerset/ref` are allowed to alias
        MDNode *type_metadata;  // Non-user-accessible type metadata incl. union selectors, etc.
        MDNode *constant;       // Memory that is immutable by the time LLVM can see it

        jl_regions_t(): gcframe(nullptr), stack(nullptr), data(nullptr), type_metadata(nullptr), constant(nullptr) {}

        void initialize(llvm::LLVMContext &context) {
            MDBuilder mbuilder(context);
            MDNode *domain = mbuilder.createAliasScopeDomain("jnoalias");

            this->gcframe = mbuilder.createAliasScope("jnoalias_gcframe", domain);
            this->stack = mbuilder.createAliasScope("jnoalias_stack", domain);
            this->data = mbuilder.createAliasScope("jnoalias_data", domain);
            this->type_metadata = mbuilder.createAliasScope("jnoalias_typemd", domain);
            this->constant = mbuilder.createAliasScope("jnoalias_const", domain);
        }
    } regions;

    // `@aliasscope` domain
    struct jl_aliasscope_t {
        MDNode *current;

        jl_aliasscope_t(): current(nullptr) {}

        // No init required, this->current is only used to store the currently active aliasscope
        void initialize(llvm::LLVMContext &context) {}
    } aliasscope;

    bool initialized;

    jl_noaliascache_t(): regions(), aliasscope(), initialized(false) {}

    void initialize(llvm::LLVMContext &context) {
        if (initialized) {
            assert(&regions.constant->getContext() == &context);
            return;
        }
        initialized = true;
        regions.initialize(context);
        aliasscope.initialize(context);
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
    Type *(*_type)(Type *T_size);

    JuliaVariable(const JuliaVariable&) = delete;
    JuliaVariable(const JuliaVariable&&) = delete;
    GlobalVariable *realize(Module *m) {
        if (GlobalValue *V = m->getNamedValue(name))
            return cast<GlobalVariable>(V);
        auto T_size = m->getDataLayout().getIntPtrType(m->getContext());
        auto var = new GlobalVariable(*m, _type(T_size),
                isconst, GlobalVariable::ExternalLinkage,
                NULL, name);
        if (Triple(m->getTargetTriple()).isOSWindows())
            var->setDLLStorageClass(GlobalValue::DLLStorageClassTypes::DLLImportStorageClass); // Cross-library imports must be explicit for COFF (Windows)
        return var;
    }
    GlobalVariable *realize(jl_codectx_t &ctx);
};
static inline void add_named_global(JuliaVariable *name, void *addr)
{
    add_named_global(name->name, addr);
}


typedef FunctionType *(*TypeFnContextOnly)(LLVMContext &C);
typedef FunctionType *(*TypeFnContextAndSizeT)(LLVMContext &C, Type *T_size);
typedef FunctionType *(*TypeFnContextAndTriple)(LLVMContext &C, const Triple &triple);

FunctionType *invoke_type(TypeFnContextOnly f, Module &M)
{
    return f(M.getContext());
}

FunctionType *invoke_type(TypeFnContextAndSizeT f, Module &M)
{
    return f(M.getContext(), M.getDataLayout().getIntPtrType(M.getContext()));
}

FunctionType *invoke_type(TypeFnContextAndTriple f, Module &M)
{
    return f(M.getContext(), Triple(M.getTargetTriple()));
}

template<typename TypeFn_t = TypeFnContextOnly>
struct JuliaFunction {
public:
    llvm::StringLiteral name;
    TypeFn_t _type;
    llvm::AttributeList (*_attrs)(llvm::LLVMContext &C);

    JuliaFunction(const JuliaFunction&) = delete;
    JuliaFunction(const JuliaFunction&&) = delete;
    llvm::Function *realize(llvm::Module *m) {
        if (llvm::GlobalValue *V = m->getNamedValue(name))
            return llvm::cast<llvm::Function>(V);
        llvm::Function *F = llvm::Function::Create(invoke_type(_type, *m),
                         llvm::Function::ExternalLinkage,
                         name, m);
        if (_attrs)
            F->setAttributes(_attrs(m->getContext()));
        return F;
    }
};

template<typename T, typename TypeFn_t>
static inline void add_named_global(JuliaFunction<TypeFn_t> *name, T *addr)
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

AttributeSet Attributes(LLVMContext &C, std::initializer_list<Attribute::AttrKind> attrkinds, std::initializer_list<Attribute> extra={})
{
    SmallVector<Attribute, 8> attrs(attrkinds.size() + extra.size());
    for (size_t i = 0; i < attrkinds.size(); i++)
        attrs[i] = Attribute::get(C, attrkinds.begin()[i]);
    for (size_t i = 0; i < extra.size(); i++)
        attrs[attrkinds.size() + i] = extra.begin()[i];
    return AttributeSet::get(C, ArrayRef<Attribute>(attrs));
}

static Type *get_pjlvalue(LLVMContext &C) { return JuliaType::get_pjlvalue_ty(C); }

static FunctionType *get_func_sig(LLVMContext &C) { return JuliaType::get_jlfunc_ty(C); }
static FunctionType *get_func2_sig(LLVMContext &C) { return JuliaType::get_jlfunc2_ty(C); }
static FunctionType *get_func3_sig(LLVMContext &C) { return JuliaType::get_jlfunc3_ty(C); }

static FunctionType *get_donotdelete_sig(LLVMContext &C) {
    return FunctionType::get(getVoidTy(C), true);
}

static AttributeList get_func_attrs(LLVMContext &C)
{
    return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            {AttributeSet(),
             Attributes(C, {Attribute::NoAlias, Attribute::ReadOnly, Attribute::NoCapture, Attribute::NoUndef})});
}

static AttributeList get_donotdelete_func_attrs(LLVMContext &C)
{
    AttrBuilder FnAttrs(C);
    FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly());
    FnAttrs.addAttribute(Attribute::WillReturn);
    FnAttrs.addAttribute(Attribute::NoUnwind);
    return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
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

static AttributeList get_attrs_basic(LLVMContext &C)
{
    return AttributeList::get(C,
                AttributeSet(),
                Attributes(C, {Attribute::NonNull}),
                None);
}

static AttributeList get_attrs_box_float(LLVMContext &C, unsigned nbytes)
{
    auto FnAttrs = AttrBuilder(C);
    FnAttrs.addAttribute(Attribute::WillReturn);
    FnAttrs.addAttribute(Attribute::NoUnwind);
    FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly());
    auto RetAttrs = AttrBuilder(C);
    RetAttrs.addAttribute(Attribute::NonNull);
    RetAttrs.addDereferenceableAttr(nbytes);
    RetAttrs.addAlignmentAttr(Align(alignof(void*)));
    return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet::get(C, RetAttrs),
                None);
}

static AttributeList get_attrs_box_sext(LLVMContext &C, unsigned nbytes)
{
    auto FnAttrs = AttrBuilder(C);
    FnAttrs.addAttribute(Attribute::WillReturn);
    FnAttrs.addAttribute(Attribute::NoUnwind);
    FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly());
    auto RetAttrs = AttrBuilder(C);
    RetAttrs.addAttribute(Attribute::NonNull);
    RetAttrs.addAttribute(Attribute::getWithDereferenceableBytes(C, nbytes));
    RetAttrs.addDereferenceableAttr(nbytes);
    RetAttrs.addAlignmentAttr(Align(alignof(void*)));
    return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet::get(C, RetAttrs),
                AttributeSet::get(C, {Attribute::get(C, Attribute::SExt)}));
}

static AttributeList get_attrs_box_zext(LLVMContext &C, unsigned nbytes)
{
    auto FnAttrs = AttrBuilder(C);
    FnAttrs.addAttribute(Attribute::WillReturn);
    FnAttrs.addAttribute(Attribute::NoUnwind);
    FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly());
    auto RetAttrs = AttrBuilder(C);
    RetAttrs.addAttribute(Attribute::NonNull);
    RetAttrs.addDereferenceableAttr(nbytes);
    RetAttrs.addAlignmentAttr(Align(alignof(void*)));
    return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet::get(C, RetAttrs),
                AttributeSet::get(C, {Attribute::get(C, Attribute::ZExt)}));
}


// global vars
static const auto jlRTLD_DEFAULT_var = new JuliaVariable{
    XSTR(jl_RTLD_DEFAULT_handle),
    true,
    [](Type *T_size) -> Type * { return getPointerTy(T_size->getContext()); },
};
static const auto jlexe_var = new JuliaVariable{
    XSTR(jl_exe_handle),
    true,
    [](Type *T_size) -> Type * { return getPointerTy(T_size->getContext()); },
};
static const auto jldll_var = new JuliaVariable{
    XSTR(jl_libjulia_handle),
    true,
    [](Type *T_size) -> Type * { return getPointerTy(T_size->getContext()); },
};
static const auto jldlli_var = new JuliaVariable{
    XSTR(jl_libjulia_internal_handle),
    true,
    [](Type *T_size) -> Type * { return getPointerTy(T_size->getContext()); },
};
static const auto jl_small_typeof_var = new JuliaVariable{
    XSTR(jl_small_typeof),
    true,
    [](Type *T_size) -> Type * { return getInt8Ty(T_size->getContext()); },
};

static const auto jlstack_chk_guard_var = new JuliaVariable{
    XSTR(__stack_chk_guard),
    true,
    [](Type *T_size) -> Type * { return get_pjlvalue(T_size->getContext()); },
};

static const auto jlgetworld_global = new JuliaVariable{
    XSTR(jl_world_counter),
    false,
    [](Type *T_size) -> Type * { return T_size; },
};

static const auto jlboxed_int8_cache = new JuliaVariable{
    XSTR(jl_boxed_int8_cache),
    true,
    [](Type *T_size) -> Type * { return ArrayType::get(get_pjlvalue(T_size->getContext()), 256); },
};

static const auto jlboxed_uint8_cache = new JuliaVariable{
    XSTR(jl_boxed_uint8_cache),
    true,
    [](Type *T_size) -> Type * { return ArrayType::get(get_pjlvalue(T_size->getContext()), 256); },
};

static const auto jlpgcstack_func = new JuliaFunction<>{
    "julia.get_pgcstack",
    [](LLVMContext &C) { return FunctionType::get(PointerType::get(JuliaType::get_ppjlvalue_ty(C), 0), false); },
    nullptr,
};

static const auto jladoptthread_func = new JuliaFunction<>{
    "julia.get_pgcstack_or_new",
    jlpgcstack_func->_type,
    jlpgcstack_func->_attrs,
};


// important functions
// Symbols are not gc-tracked, but we'll treat them as callee rooted anyway,
// because they may come from a gc-rooted location
static const auto jlnew_func = new JuliaFunction<>{
    XSTR(jl_new_structv),
    get_func_sig,
    get_func_attrs,
};
static const auto jlsplatnew_func = new JuliaFunction<>{
    XSTR(jl_new_structt),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_prjlvalue, T_prjlvalue}, false);
    },
    get_attrs_basic,
};
static const auto jlthrow_func = new JuliaFunction<>{
    XSTR(jl_throw),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlerror_func = new JuliaFunction<>{
    XSTR(jl_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getPointerTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlargumenterror_func = new JuliaFunction<>{
    XSTR(jl_argument_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getPointerTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jlatomicerror_func = new JuliaFunction<>{
    XSTR(jl_atomic_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getPointerTy(C)}, false); },
    get_attrs_noreturn,
};
static const auto jltypeerror_func = new JuliaFunction<>{
    XSTR(jl_type_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {getPointerTy(C), JuliaType::get_prjlvalue_ty(C), PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlundefvarerror_func = new JuliaFunction<>{
    XSTR(jl_undefined_var_error),
    [](LLVMContext &C) {
        Type *T = PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted);
        return FunctionType::get(getVoidTy(C), {T, T}, false);
    },
    get_attrs_noreturn,
};
static const auto jlhasnofield_func = new JuliaFunction<>{
    XSTR(jl_has_no_field_error),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted),
             PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    get_attrs_noreturn,
};
static const auto jlboundserrorv_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_bounds_error_ints),
    [](LLVMContext &C, Type *T_size) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted), T_size->getPointerTo(), T_size}, false); },
    get_attrs_noreturn,
};
static const auto jlboundserror_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_bounds_error_int),
    [](LLVMContext &C, Type *T_size) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted), T_size}, false); },
    get_attrs_noreturn,
};
static const auto jlvboundserror_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_bounds_error_tuple_int),
    [](LLVMContext &C, Type *T_size) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_pprjlvalue_ty(C), T_size, T_size}, false); },
    get_attrs_noreturn,
};
static const auto jluboundserror_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_bounds_error_unboxed_int),
    [](LLVMContext &C, Type *T_size) {
        return FunctionType::get(getVoidTy(C),
            {PointerType::get(getInt8Ty(C), AddressSpace::Derived), JuliaType::get_pjlvalue_ty(C), T_size}, false); },
    get_attrs_noreturn,
};
static const auto jlcheckassign_func = new JuliaFunction<>{
    XSTR(jl_checked_assignment),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    nullptr,
};
static const auto jlcheckreplace_func = new JuliaFunction<>{
    XSTR(jl_checked_replace),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, T_prjlvalue, T_prjlvalue}, false); },
    nullptr,
};
static const auto jlcheckmodify_func = new JuliaFunction<>{
    XSTR(jl_checked_modify),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, T_prjlvalue, T_prjlvalue}, false); },
    nullptr,
};
static const auto jlcheckswap_func = new JuliaFunction<>{
    XSTR(jl_checked_swap),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    nullptr,
};
static const auto jlcheckassignonce_func = new JuliaFunction<>{
    XSTR(jl_checked_assignonce),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    nullptr,
};
static const auto jldeclareconstval_func = new JuliaFunction<>{
    XSTR(jl_declare_constant_val),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, T_pjlvalue, T_pjlvalue, T_prjlvalue}, false); },
    nullptr,
};
static const auto jldeclareglobal_func = new JuliaFunction<>{
    XSTR(jl_declare_global),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, T_pjlvalue, T_prjlvalue, getInt32Ty(C)}, false); },
    nullptr,
};
static const auto jldepcheck_func = new JuliaFunction<>{
    XSTR(jl_binding_deprecation_check),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue}, false); },
    nullptr,
};
static const auto jlcheckbpwritable_func = new JuliaFunction<>{
    XSTR(jl_check_binding_currently_writable),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
                {T_pjlvalue, T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jlgetbindingvalue_func = new JuliaFunction<>{
    XSTR(jl_get_binding_value_seqcst),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jlboundp_func = new JuliaFunction<>{
    XSTR(jl_boundp),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
                {T_pjlvalue, T_pjlvalue, getInt32Ty(C)}, false);
    },
    nullptr,
};
static const auto jltopeval_func = new JuliaFunction<>{
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
static const auto jlcopyast_func = new JuliaFunction<>{
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
static const auto jlapplygeneric_func = new JuliaFunction<>{
    XSTR(jl_apply_generic),
    get_func_sig,
    get_func_attrs,
};
static const auto jlinvoke_func = new JuliaFunction<>{
    XSTR(jl_invoke),
    get_func2_sig,
    get_func_attrs,
};
static const auto jlinvokeoc_func = new JuliaFunction<>{
    XSTR(jl_invoke_oc),
    get_func2_sig,
    get_func_attrs,
};
static const auto jlopaque_closure_call_func = new JuliaFunction<>{
    XSTR(jl_f_opaque_closure_call),
    get_func_sig,
    get_func_attrs,
};
static const auto jlmethod_func = new JuliaFunction<>{
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
static const auto jlgenericfunction_func = new JuliaFunction<>{
    XSTR(jl_declare_const_gf),
    [](LLVMContext &C) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        return FunctionType::get(T_prjlvalue, {T_pjlvalue, T_pjlvalue}, false);
    },
    nullptr,
};
static const auto jllockvalue_func = new JuliaFunction<>{
    XSTR(jl_lock_value),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jlunlockvalue_func = new JuliaFunction<>{
    XSTR(jl_unlock_value),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::CalleeRooted)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jllockfield_func = new JuliaFunction<>{
    XSTR(jl_lock_field),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Loaded)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jlunlockfield_func = new JuliaFunction<>{
    XSTR(jl_unlock_field),
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Loaded)}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            AttributeSet(),
            {Attributes(C, {Attribute::NoCapture})}); },
};
static const auto jlenter_func = new JuliaFunction<>{
    XSTR(jl_enter_handler),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, getPointerTy(C)}, false); },
    nullptr,
};
static const auto jl_current_exception_func = new JuliaFunction<>{
    XSTR(jl_current_exception),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C), {JuliaType::get_pjlvalue_ty(C)}, false); },
    nullptr,
};
static const auto jlleave_func = new JuliaFunction<>{
    XSTR(jl_pop_handler),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, getInt32Ty(C)}, false); },
    [](LLVMContext &C) {
            auto FnAttrs = AttrBuilder(C);
            FnAttrs.addAttribute(Attribute::WillReturn);
            FnAttrs.addAttribute(Attribute::NoUnwind);
            auto RetAttrs = AttrBuilder(C);
            return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet(),
                None);
        },
};
static const auto jlleave_noexcept_func = new JuliaFunction<>{
    XSTR(jl_pop_handler_noexcept),
    [](LLVMContext &C) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, getInt32Ty(C)}, false); },
    [](LLVMContext &C) {
            auto FnAttrs = AttrBuilder(C);
            FnAttrs.addAttribute(Attribute::WillReturn);
            FnAttrs.addAttribute(Attribute::NoUnwind);
            auto RetAttrs = AttrBuilder(C);
            return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet(),
                None);
        },
};
static const auto jl_restore_excstack_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_restore_excstack),
    [](LLVMContext &C, Type *T_size) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_pjlvalue, T_size}, false); },
    nullptr,
};
static const auto jl_excstack_state_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_excstack_state),
    [](LLVMContext &C, Type *T_size) {
        auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
        return FunctionType::get(T_size, {T_pjlvalue}, false); },
    nullptr,
};
static const auto jlegalx_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_egal__unboxed),
    [](LLVMContext &C, Type *T_size) {
        Type *T = PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Derived);
        return FunctionType::get(getInt32Ty(C), {T, T, T_size}, false); },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleOrArgMemOnly());
        FnAttrs.addAttribute(Attribute::NoUnwind);
        return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet(),
                None); },
};
static const auto jl_alloc_obj_func = new JuliaFunction<TypeFnContextAndSizeT>{
    "julia.gc_alloc_obj",
    [](LLVMContext &C, Type *T_size) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        return FunctionType::get(T_prjlvalue,
                {T_pjlvalue, T_size, T_prjlvalue}, false);
    },
    [](LLVMContext &C) {
        auto FnAttrs = AttrBuilder(C);
        FnAttrs.addAllocSizeAttr(1, None); // returns %1 bytes
        FnAttrs.addAllocKindAttr(AllocFnKind::Alloc);
        FnAttrs.addMemoryAttr(MemoryEffects::argMemOnly(ModRefInfo::Ref) | MemoryEffects::inaccessibleMemOnly(ModRefInfo::ModRef));
        FnAttrs.addAttribute(Attribute::WillReturn);
        FnAttrs.addAttribute(Attribute::NoUnwind);
        auto RetAttrs = AttrBuilder(C);
        RetAttrs.addAttribute(Attribute::NoAlias);
        RetAttrs.addAttribute(Attribute::NonNull);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            AttributeSet::get(C, RetAttrs),
            None);
    },
};
static const auto jl_alloc_genericmemory_unchecked_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_alloc_genericmemory_unchecked),
    [](LLVMContext &C, Type *T_size) {
        auto T_jlvalue = JuliaType::get_jlvalue_ty(C);
        auto T_prjlvalue = PointerType::get(T_jlvalue, AddressSpace::Tracked);
        auto T_pjlvalue = PointerType::get(T_jlvalue, 0);
        return FunctionType::get(T_prjlvalue,
                {T_pjlvalue, T_size, T_pjlvalue}, false);
    },
    [](LLVMContext &C) {
        auto FnAttrs = AttrBuilder(C);
        FnAttrs.addAllocKindAttr(AllocFnKind::Alloc);
        FnAttrs.addMemoryAttr(MemoryEffects::argMemOnly(ModRefInfo::Ref) | MemoryEffects::inaccessibleMemOnly(ModRefInfo::ModRef));
        FnAttrs.addAttribute(Attribute::WillReturn);
        FnAttrs.addAttribute(Attribute::NoUnwind);
        auto RetAttrs = AttrBuilder(C);
        RetAttrs.addAttribute(Attribute::NoAlias);
        RetAttrs.addAttribute(Attribute::NonNull);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            AttributeSet::get(C, RetAttrs),
            None);
    },
};
static const auto jl_newbits_func = new JuliaFunction<>{
    XSTR(jl_new_bits),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue, getPointerTy(C)}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
// `julia.typeof` does read memory, but it is effectively readnone before we lower
// the allocation function. This is OK as long as we lower `julia.typeof` no later than
// `julia.gc_alloc_obj`.
static const auto jl_typeof_func = new JuliaFunction<>{
    "julia.typeof",
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
                {T_prjlvalue}, false);
    },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::none());
        FnAttrs.addAttribute(Attribute::NoUnwind);
        FnAttrs.addAttribute(Attribute::NoRecurse);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            Attributes(C, {Attribute::NonNull}),
            None); },
};

static const auto jl_write_barrier_func = new JuliaFunction<>{
    "julia.write_barrier",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C),
            {JuliaType::get_prjlvalue_ty(C)}, true); },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly());
        FnAttrs.addAttribute(Attribute::NoUnwind);
        FnAttrs.addAttribute(Attribute::NoRecurse);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            AttributeSet(),
            {Attributes(C, {Attribute::ReadOnly})});
    },
};

static const auto jlisa_func = new JuliaFunction<>{
    XSTR(jl_isa),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};

static const auto jlsubtype_func = new JuliaFunction<>{
    XSTR(jl_subtype),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};
static const auto jlapplytype_func = new JuliaFunction<>{
    XSTR(jl_instantiate_type_in_env),
    [](LLVMContext &C) {
        auto T_ptr = PointerType::get(C, 0);
        auto T_tracked = PointerType::get(C, AddressSpace::Tracked);
        auto T_derived = PointerType::get(C, AddressSpace::Derived);
        return FunctionType::get(T_tracked,
            {T_ptr, T_ptr, T_derived}, false);
    },
    [](LLVMContext &C) {
        return AttributeList::get(C,
            AttributeSet(),
            AttributeSet::get(C, ArrayRef<Attribute>({Attribute::get(C, Attribute::NonNull),
                                               Attribute::getWithAlignment(C, Align(16))})),
            None);
    },
};
static const auto jl_object_id__func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_object_id_),
    [](LLVMContext &C, Type *T_size) { return FunctionType::get(T_size,
            {T_size, PointerType::get(getInt8Ty(C), AddressSpace::Derived)}, false); },
    nullptr,
};
static const auto setjmp_func = new JuliaFunction<TypeFnContextAndTriple>{
    jl_setjmp_name,
    [](LLVMContext &C, const Triple &T) {
        if (T.isOSWindows())
            return FunctionType::get(getInt32Ty(C),
                {getPointerTy(C)}, false);
        return FunctionType::get(getInt32Ty(C),
            {getPointerTy(C), getInt32Ty(C)}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReturnsTwice}),
            AttributeSet(),
            None); },
};
static const auto memcmp_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(memcmp),
    [](LLVMContext &C, Type *T_size) { return FunctionType::get(getInt32Ty(C),
            {getPointerTy(C), getPointerTy(C), T_size}, false); },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::argMemOnly(ModRefInfo::Ref));
        FnAttrs.addAttribute(Attribute::NoUnwind);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            AttributeSet(),
            None); },
    // TODO: inferLibFuncAttributes(*memcmp_func, TLI);
};
static const auto jldlsym_func = new JuliaFunction<>{
    XSTR(jl_load_and_lookup),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pvoidfunc_ty(C),
            {getPointerTy(C), getPointerTy(C), PointerType::get(getPointerTy(C), 0)}, false); },
    nullptr,
};
static const auto jllazydlsym_func = new JuliaFunction<>{
    XSTR(jl_lazy_load_and_lookup),
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pvoidfunc_ty(C),
            {JuliaType::get_prjlvalue_ty(C), getPointerTy(C)}, false); },
    nullptr,
};
static const auto jltypeassert_func = new JuliaFunction<>{
    XSTR(jl_typeassert),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getVoidTy(C),
            {T_prjlvalue, T_prjlvalue}, false);
    },
    nullptr,
};
static const auto jlgetnthfieldchecked_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_get_nth_field_checked),
    [](LLVMContext &C, Type *T_size) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_prjlvalue, T_size}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto jlfieldindex_func = new JuliaFunction<>{
    XSTR(jl_field_index),
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_prjlvalue, getInt32Ty(C)}, false);
    },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::readOnly());
        FnAttrs.addAttribute(Attribute::NoUnwind);
        FnAttrs.addAttribute(Attribute::WillReturn);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            AttributeSet(),
            None); }, // This function can error if the third argument is 1 so don't do that.
};
static const auto jlfieldisdefinedchecked_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_field_isdefined_checked),
    [](LLVMContext &C, Type *T_size) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(getInt32Ty(C),
            {T_prjlvalue, T_size}, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {}),
            None); },
};
static const auto jlgetcfunctiontrampoline_func = new JuliaFunction<>{
    XSTR(jl_get_cfunction_trampoline),
    [](LLVMContext &C) {
        auto T_pjlvalue = PointerType::get(C, 0);
        auto T_prjlvalue = PointerType::get(C, AddressSpace::Tracked);
        auto T_ppjlvalue = PointerType::get(C, 0);
        auto T_derived = PointerType::get(C, AddressSpace::Derived);
        return FunctionType::get(T_prjlvalue,
            {
                T_prjlvalue, // f (object)
                T_pjlvalue, // result
                getPointerTy(C), // cache
                T_pjlvalue, // fill
                FunctionType::get(getPointerTy(C), { getPointerTy(C), T_ppjlvalue }, false)->getPointerTo(), // trampoline
                T_pjlvalue, // env
                T_derived, // vals
            }, false);
    },
    [](LLVMContext &C) { return AttributeList::get(C,
            AttributeSet(),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto jlgetabiconverter_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_get_abi_converter),
    [](LLVMContext &C, Type *T_size) {
        Type *T_ptr = getPointerTy(C);
        return FunctionType::get(T_ptr,
            {T_ptr, T_ptr, T_ptr, T_ptr}, false); },
    nullptr,
};
static const auto diff_gc_total_bytes_func = new JuliaFunction<>{
    XSTR(jl_gc_diff_total_bytes),
    [](LLVMContext &C) { return FunctionType::get(getInt64Ty(C), false); },
    nullptr,
};
static const auto sync_gc_total_bytes_func = new JuliaFunction<>{
    XSTR(jl_gc_sync_total_bytes),
    [](LLVMContext &C) { return FunctionType::get(getInt64Ty(C),
            {getInt64Ty(C)}, false); },
    nullptr,
};
static const auto jl_allocgenericmemory = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_alloc_genericmemory),
    [](LLVMContext &C, Type *T_Size) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue, // new Memory
                                {T_prjlvalue, // type
                                T_Size        // nelements
                                }, false); },
        [](LLVMContext &C) {
            AttrBuilder FnAttrs(C);
            AttrBuilder RetAttrs(C);
            FnAttrs.addMemoryAttr(MemoryEffects::inaccessibleMemOnly(ModRefInfo::ModRef) | MemoryEffects::argMemOnly(ModRefInfo::Ref));
            FnAttrs.addAttribute(Attribute::WillReturn);
            RetAttrs.addAlignmentAttr(Align(16));
            RetAttrs.addAttribute(Attribute::NonNull);
            RetAttrs.addDereferenceableAttr(16);
            return AttributeList::get(C,
                AttributeSet::get(C, FnAttrs),
                AttributeSet::get(C, RetAttrs),
                None); },
};
#define BOX_FUNC(ct,at,attrs,nbytes)                                                    \
static const auto box_##ct##_func = new JuliaFunction<>{                           \
    XSTR(jl_box_##ct),                                                           \
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_prjlvalue_ty(C),\
            {at}, false); },                                                     \
    [](LLVMContext &C) { return attrs(C,nbytes); },                                                                \
}
BOX_FUNC(int16, getInt16Ty(C), get_attrs_box_sext, 2);
BOX_FUNC(uint16, getInt16Ty(C), get_attrs_box_zext, 2);
BOX_FUNC(int32, getInt32Ty(C), get_attrs_box_sext, 4);
BOX_FUNC(uint32, getInt32Ty(C), get_attrs_box_zext, 4);
BOX_FUNC(int64, getInt64Ty(C), get_attrs_box_sext, 8);
BOX_FUNC(uint64, getInt64Ty(C), get_attrs_box_zext, 8);
BOX_FUNC(char, getCharTy(C), get_attrs_box_zext, 1);
BOX_FUNC(float32, getFloatTy(C), get_attrs_box_float, 4);
BOX_FUNC(float64, getDoubleTy(C), get_attrs_box_float, 8);
#undef BOX_FUNC

static const auto box_ssavalue_func = new JuliaFunction<TypeFnContextAndSizeT>{
    XSTR(jl_box_ssavalue),
    [](LLVMContext &C, Type *T_size) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {T_size}, false);
    },
    get_attrs_basic,
};
static const auto jlgetbuiltinfptr_func = new JuliaFunction<>{
    XSTR(jl_get_builtin_fptr),
    [](LLVMContext &C) { return FunctionType::get(get_func_sig(C)->getPointerTo(),
            {JuliaType::get_prjlvalue_ty(C)}, false); },
    nullptr,
};

// placeholder functions
static const auto gcroot_flush_func = new JuliaFunction<>{
    "julia.gcroot_flush",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C), false); },
    nullptr,
};
static const auto gc_preserve_begin_func = new JuliaFunction<>{
    "llvm.julia.gc_preserve_begin",
    [](LLVMContext &C) { return FunctionType::get(Type::getTokenTy(C), true); },
    nullptr,
};
static const auto gc_preserve_end_func = new JuliaFunction<> {
    "llvm.julia.gc_preserve_end",
    [](LLVMContext &C) { return FunctionType::get(getVoidTy(C), {Type::getTokenTy(C)}, false); },
    nullptr,
};
static const auto except_enter_func = new JuliaFunction<>{
    "julia.except_enter",
    [](LLVMContext &C) {
         auto T_pjlvalue = JuliaType::get_pjlvalue_ty(C);
         auto RT = StructType::get(getInt32Ty(C), getPointerTy(C));
         return FunctionType::get(RT, {T_pjlvalue}, false); },
    [](LLVMContext &C) { return AttributeList::get(C,
            Attributes(C, {Attribute::ReturnsTwice}),
            AttributeSet(),
            None); },
};
static const auto pointer_from_objref_func = new JuliaFunction<>{
    "julia.pointer_from_objref",
    [](LLVMContext &C) { return FunctionType::get(JuliaType::get_pjlvalue_ty(C),
            {PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Derived)}, false); },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addMemoryAttr(MemoryEffects::none());
        FnAttrs.addAttribute(Attribute::NoUnwind);
        FnAttrs.addAttribute(Attribute::Speculatable);
        FnAttrs.addAttribute(Attribute::WillReturn);
        FnAttrs.addAttribute(Attribute::NoRecurse);
        FnAttrs.addAttribute(Attribute::NoSync);
        return AttributeList::get(C,
            AttributeSet::get(C, FnAttrs),
            Attributes(C, {Attribute::NonNull}),
            None); },
};
static const auto gc_loaded_func = new JuliaFunction<>{
    "julia.gc_loaded",
    // # memory(none) nosync nounwind speculatable willreturn norecurse
    // declare nonnull noundef ptr(Loaded) @"julia.gc_loaded"(ptr(Tracked) nocapture nonnull noundef readnone, ptr nonnull noundef readnone)
    //  top:
    //   %metadata GC base pointer is ptr(Tracked)
    //   ret addrspacecast ptr to ptr(Loaded)
    [](LLVMContext &C) { return FunctionType::get(PointerType::get(JuliaType::get_prjlvalue_ty(C), AddressSpace::Loaded),
            {JuliaType::get_prjlvalue_ty(C), PointerType::get(JuliaType::get_prjlvalue_ty(C), 0)}, false); },
    [](LLVMContext &C) {
        AttrBuilder FnAttrs(C);
        FnAttrs.addAttribute(Attribute::NoSync);
        FnAttrs.addAttribute(Attribute::NoUnwind);
        FnAttrs.addAttribute(Attribute::Speculatable);
        FnAttrs.addAttribute(Attribute::WillReturn);
        FnAttrs.addAttribute(Attribute::NoRecurse);
        FnAttrs.addMemoryAttr(MemoryEffects::none());
        AttrBuilder RetAttrs(C);
        RetAttrs.addAttribute(Attribute::NonNull);
        RetAttrs.addAttribute(Attribute::NoUndef);
        return AttributeList::get(C, AttributeSet::get(C,FnAttrs), AttributeSet::get(C,RetAttrs),
                { Attributes(C, {Attribute::NonNull, Attribute::NoUndef, Attribute::ReadNone, Attribute::NoCapture}),
                  Attributes(C, {Attribute::NonNull, Attribute::NoUndef, Attribute::ReadNone}) });
                  },
};

// julia.call represents a call with julia calling convention, it is used as
//
//   ptr julia.call(ptr %fptr, ptr %f, ptr %arg1, ptr %arg2, ...)
//
// In late lowering the call will then be rewritten as
//
//   ptr %fptr(ptr %f, ptr args, i64 nargs)
//
// with all the spelled out args appropriately moved into the argument stack buffer.
// By representing it this way rather than allocating the stack buffer earlier, we
// allow LLVM to make more aggressive optimizations on the call arguments.
static const auto julia_call = new JuliaFunction<>{
    "julia.call",
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {get_func_sig(C)->getPointerTo(),
             T_prjlvalue}, // %f
            true); }, // %args
    get_attrs_basic,
};

// julia.call2 is like julia.call, except that %arg1 gets passed as a register
// argument at the end of the argument list.
static const auto julia_call2 = new JuliaFunction<>{
    "julia.call2",
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        return FunctionType::get(T_prjlvalue,
            {get_func2_sig(C)->getPointerTo(),
             T_prjlvalue, // %arg1
             T_prjlvalue}, // %f
            true); }, // %args
    get_attrs_basic,
};

// julia.call3 is like julia.call, except that %fptr is derived rather than tracked
static const auto julia_call3 = new JuliaFunction<>{
    "julia.call3",
    [](LLVMContext &C) {
        auto T_prjlvalue = JuliaType::get_prjlvalue_ty(C);
        Type *T = PointerType::get(JuliaType::get_jlvalue_ty(C), AddressSpace::Derived);
        return FunctionType::get(T_prjlvalue,
            {get_func3_sig(C)->getPointerTo(),
             T}, // %f
            true); }, // %args
    get_attrs_basic,
};


static const auto jltuple_func = new JuliaFunction<>{XSTR(jl_f_tuple), get_func_sig, get_func_attrs};
static const auto jlintrinsic_func = new JuliaFunction<>{XSTR(jl_f_intrinsic_call), get_func3_sig, get_func_attrs};

static const auto &builtin_func_map() {
    static auto builtins = new DenseMap<jl_fptr_args_t, JuliaFunction<>*> {
          { jl_f_is_addr,                 new JuliaFunction<>{XSTR(jl_f_is), get_func_sig, get_func_attrs} },
          { jl_f_typeof_addr,             new JuliaFunction<>{XSTR(jl_f_typeof), get_func_sig, get_func_attrs} },
          { jl_f_sizeof_addr,             new JuliaFunction<>{XSTR(jl_f_sizeof), get_func_sig, get_func_attrs} },
          { jl_f_issubtype_addr,          new JuliaFunction<>{XSTR(jl_f_issubtype), get_func_sig, get_func_attrs} },
          { jl_f_isa_addr,                new JuliaFunction<>{XSTR(jl_f_isa), get_func_sig, get_func_attrs} },
          { jl_f_typeassert_addr,         new JuliaFunction<>{XSTR(jl_f_typeassert), get_func_sig, get_func_attrs} },
          { jl_f_ifelse_addr,             new JuliaFunction<>{XSTR(jl_f_ifelse), get_func_sig, get_func_attrs} },
          { jl_f__apply_iterate_addr,     new JuliaFunction<>{XSTR(jl_f__apply_iterate), get_func_sig, get_func_attrs} },
          { jl_f_invokelatest_addr,       new JuliaFunction<>{XSTR(jl_f_invokelatest), get_func_sig, get_func_attrs} },
          { jl_f_invoke_in_world_addr,    new JuliaFunction<>{XSTR(jl_f_invoke_in_world), get_func_sig, get_func_attrs} },
          { jl_f__call_in_world_total_addr, new JuliaFunction<>{XSTR(jl_f__call_in_world_total), get_func_sig, get_func_attrs} },
          { jl_f_throw_addr,              new JuliaFunction<>{XSTR(jl_f_throw), get_func_sig, get_func_attrs} },
          { jl_f_throw_methoderror_addr,  new JuliaFunction<>{XSTR(jl_f_throw_methoderror), get_func_sig, get_func_attrs} },
          { jl_f_tuple_addr,              jltuple_func },
          { jl_f_svec_addr,               new JuliaFunction<>{XSTR(jl_f_svec), get_func_sig, get_func_attrs} },
          { jl_f_applicable_addr,         new JuliaFunction<>{XSTR(jl_f_applicable), get_func_sig, get_func_attrs} },
          { jl_f_invoke_addr,             new JuliaFunction<>{XSTR(jl_f_invoke), get_func_sig, get_func_attrs} },
          { jl_f_isdefined_addr,          new JuliaFunction<>{XSTR(jl_f_isdefined), get_func_sig, get_func_attrs} },
          { jl_f_getfield_addr,           new JuliaFunction<>{XSTR(jl_f_getfield), get_func_sig, get_func_attrs} },
          { jl_f_setfield_addr,           new JuliaFunction<>{XSTR(jl_f_setfield), get_func_sig, get_func_attrs} },
          { jl_f_swapfield_addr,          new JuliaFunction<>{XSTR(jl_f_swapfield), get_func_sig, get_func_attrs} },
          { jl_f_modifyfield_addr,        new JuliaFunction<>{XSTR(jl_f_modifyfield), get_func_sig, get_func_attrs} },
          { jl_f_fieldtype_addr,          new JuliaFunction<>{XSTR(jl_f_fieldtype), get_func_sig, get_func_attrs} },
          { jl_f_nfields_addr,            new JuliaFunction<>{XSTR(jl_f_nfields), get_func_sig, get_func_attrs} },
          { jl_f__expr_addr,              new JuliaFunction<>{XSTR(jl_f__expr), get_func_sig, get_func_attrs} },
          { jl_f__typevar_addr,           new JuliaFunction<>{XSTR(jl_f__typevar), get_func_sig, get_func_attrs} },
          { jl_f_memorynew_addr,          new JuliaFunction<>{XSTR(jl_f_memorynew), get_func_sig, get_func_attrs} },
          { jl_f_memoryref_addr,          new JuliaFunction<>{XSTR(jl_f_memoryref), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefoffset_addr,    new JuliaFunction<>{XSTR(jl_f_memoryrefoffset), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefset_addr,       new JuliaFunction<>{XSTR(jl_f_memoryrefset), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefswap_addr,      new JuliaFunction<>{XSTR(jl_f_memoryrefswap), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefreplace_addr,   new JuliaFunction<>{XSTR(jl_f_memoryrefreplace), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefmodify_addr,    new JuliaFunction<>{XSTR(jl_f_memoryrefmodify), get_func_sig, get_func_attrs} },
          { jl_f_memoryrefsetonce_addr,   new JuliaFunction<>{XSTR(jl_f_memoryrefsetonce), get_func_sig, get_func_attrs} },
          { jl_f_memoryref_isassigned_addr,new JuliaFunction<>{XSTR(jl_f_memoryref_isassigned), get_func_sig, get_func_attrs} },
          { jl_f_apply_type_addr,         new JuliaFunction<>{XSTR(jl_f_apply_type), get_func_sig, get_func_attrs} },
          { jl_f_donotdelete_addr,        new JuliaFunction<>{XSTR(jl_f_donotdelete), get_donotdelete_sig, get_donotdelete_func_attrs} },
          { jl_f_compilerbarrier_addr,    new JuliaFunction<>{XSTR(jl_f_compilerbarrier), get_func_sig, get_func_attrs} },
          { jl_f_finalizer_addr,          new JuliaFunction<>{XSTR(jl_f_finalizer), get_func_sig, get_func_attrs} },
          { jl_f__svec_ref_addr,          new JuliaFunction<>{XSTR(jl_f__svec_ref), get_func_sig, get_func_attrs} },
          { jl_f_current_scope_addr,      new JuliaFunction<>{XSTR(jl_f_current_scope), get_func_sig, get_func_attrs} },
        };
    return *builtins;
}

static const auto jl_new_opaque_closure_jlcall_func = new JuliaFunction<>{XSTR(jl_new_opaque_closure_jlcall), get_func_sig, get_func_attrs};

static _Atomic(uint64_t) globalUniqueGeneratedNames{1};

// --- code generation ---

static MDNode *best_tbaa(jl_tbaacache_t &tbaa_cache, jl_value_t *jt) {
    jt = jl_unwrap_unionall(jt);
    if (jt == (jl_value_t*)jl_datatype_type ||
        (jl_is_type_type(jt) && jl_is_datatype(jl_tparam0(jt))))
        return tbaa_cache.tbaa_datatype;
    if (!jl_is_datatype(jt))
        return tbaa_cache.tbaa_value;
    if (jl_is_abstracttype(jt))
        return tbaa_cache.tbaa_value;
    if (jl_is_genericmemory_type(jt) || jl_is_array_type(jt))
        return tbaa_cache.tbaa_array;
    // If we're here, we know all subtypes are (im)mutable, even if we
    // don't know what the exact type is
    return jl_is_mutable(jt) ? tbaa_cache.tbaa_mutab : tbaa_cache.tbaa_immut;
}

// tracks whether codegen is currently able to simply stack-allocate this type
// note that this includes jl_isbits, although codegen should work regardless
static bool jl_is_concrete_immutable(jl_value_t* t)
{
    return jl_may_be_immutable_datatype(t) && ((jl_datatype_t*)t)->isconcretetype;
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

// Alias Analysis Info (analogous to llvm::AAMDNodes)
struct jl_aliasinfo_t {
    MDNode *tbaa = nullptr;          // '!tbaa': Struct-path TBAA. TBAA graph forms a tree (indexed by offset).
                                     //          Two pointers do not alias if they are not transitive parents
                                     //          (effectively, subfields) of each other or equal.
    MDNode *tbaa_struct = nullptr;   // '!tbaa.struct': Describes memory layout of struct.
    MDNode *scope = nullptr;         // '!alias.scope': Generic "noalias" memory access sets.
                                     //                 If alias.scope(inst_a) ⊆ noalias(inst_b) (in any "domain")
                                     //                    => inst_a, inst_b do not alias.
    MDNode *noalias = nullptr;       // '!noalias': See '!alias.scope' above.

    enum class Region { unknown, gcframe, stack, data, constant, type_metadata }; // See jl_regions_t

    explicit jl_aliasinfo_t() = default;
    explicit jl_aliasinfo_t(jl_codectx_t &ctx, Region r, MDNode *tbaa);
    explicit jl_aliasinfo_t(MDNode *tbaa, MDNode *tbaa_struct, MDNode *scope, MDNode *noalias)
        : tbaa(tbaa), tbaa_struct(tbaa_struct), scope(scope), noalias(noalias) {}
    jl_aliasinfo_t(const jl_aliasinfo_t &) = default;

    // Add !tbaa, !tbaa.struct, !alias.scope, !noalias annotations to an instruction.
    //
    // Also adds `invariant.load` to load instructions in the constant !noalias scope.
    Instruction *decorateInst(Instruction *inst) const {

        if (this->tbaa)
            inst->setMetadata(LLVMContext::MD_tbaa, this->tbaa);
        if (this->tbaa_struct)
            inst->setMetadata(LLVMContext::MD_tbaa_struct, this->tbaa_struct);
        if (this->scope)
            inst->setMetadata(LLVMContext::MD_alias_scope, this->scope);
        if (this->noalias)
            inst->setMetadata(LLVMContext::MD_noalias, this->noalias);

        if (this->scope && isa<LoadInst>(inst)) {
            // If this is in the read-only region, mark the load with "!invariant.load"
            if (this->scope->getNumOperands() == 1) {
                MDNode *operand = cast<MDNode>(this->scope->getOperand(0));
                auto scope_name = cast<MDString>(operand->getOperand(0))->getString();
                if (scope_name == "jnoalias_const")
                    inst->setMetadata(LLVMContext::MD_invariant_load, MDNode::get(inst->getContext(), None));
            }
        }

        return inst;
    }

    // Merge two sets of alias information.
    jl_aliasinfo_t merge(const jl_aliasinfo_t &other) const {
        jl_aliasinfo_t result;
        result.tbaa = MDNode::getMostGenericTBAA(this->tbaa, other.tbaa);
        result.tbaa_struct = nullptr;
        result.scope = MDNode::getMostGenericAliasScope(this->scope, other.scope);
        result.noalias = MDNode::intersect(this->noalias, other.noalias);
        return result;
    }

    // Create alias information based on the provided TBAA metadata.
    //
    // This function only exists to help transition to using !noalias to encode
    // memory region non-aliasing. It should be deleted once the TBAA metadata
    // is improved to encode only memory layout and *not* memory regions.
    static jl_aliasinfo_t fromTBAA(jl_codectx_t &ctx, MDNode *tbaa);
};

// metadata tracking for a llvm Value* during codegen
const uint8_t UNION_BOX_MARKER = 0x80;
struct jl_cgval_t {
    Value *V; // may be of type T* or T, or set to NULL if ghost (or if the value has not been initialized yet, for a variable definition)
    // For unions, we may need to keep a reference to the boxed part individually.
    // If this is non-NULL, then, at runtime, we satisfy the invariant that (for the corresponding
    // runtime values) if `(TIndex | UNION_BOX_MARKER) != 0`, then `Vboxed == V` (by value).
    // For convenience, we also set this value of isboxed values, in which case
    // it is equal (at compile time) to V.

    // If this is non-NULL (at compile time), it is always of type `T_prjlvalue`.
    // N.B.: In general we expect this to always be a dereferenceable pointer at runtime.
    //       However, there are situations where this value may be a runtime NULL
    //       (PhiNodes with undef predecessors or PhiC with undef UpsilonNode).
    //       The middle-end arranges appropriate error checks before any use
    //       of this value that may read a non-dereferenceable Vboxed, with two
    //       exceptions: PhiNode and UpsilonNode arguments which need special
    //       handling to account for the possibility that this may be NULL.
    Value *Vboxed;

    Value *TIndex; // if `V` is an unboxed (tagged) Union described by `typ`, this gives the DataType index (1-based, small int) as an i8
    SmallVector<Value*,0> inline_roots; // if present, `V` is a pointer, but not in canonical layout
    jl_value_t *constant; // constant value (rooted in linfo.def.roots)
    jl_value_t *typ; // the original type of V, never nullptr
    bool isboxed; // whether this value is a jl_value_t* allocated on the heap with the right type tag
    bool isghost; // whether this value is "ghost"
    MDNode *tbaa; // The related tbaa node. Non-nullptr iff this holds an address.
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
        assert(inline_roots.empty());
        return tbaa != nullptr;
    }
    jl_cgval_t(Value *Vval, jl_value_t *typ, Value *tindex) : // general value constructor
        V(Vval), // V is allowed to be nullptr in a jl_varinfo_t context, but not during codegen contexts
        Vboxed(nullptr),
        TIndex(tindex),
        inline_roots(),
        constant(nullptr),
        typ(typ),
        isboxed(false),
        isghost(false),
        tbaa(nullptr),
        promotion_point(nullptr),
        promotion_ssa(-1)
    {
        assert(TIndex == nullptr || TIndex->getType() == getInt8Ty(TIndex->getContext()));
    }
    jl_cgval_t(Value *Vptr, bool isboxed, jl_value_t *typ, Value *tindex, MDNode *tbaa, Value* inline_roots) = delete;
    jl_cgval_t(Value *Vptr, bool isboxed, jl_value_t *typ, Value *tindex, MDNode *tbaa, ArrayRef<Value*> inline_roots) : // general pointer constructor
        V(Vptr),
        Vboxed(isboxed ? Vptr : nullptr),
        TIndex(tindex),
        inline_roots(inline_roots),
        constant(nullptr),
        typ(typ),
        isboxed(isboxed),
        isghost(false),
        tbaa(tbaa),
        promotion_point(nullptr),
        promotion_ssa(-1)
    {
        if (Vboxed)
            assert(Vboxed->getType() == JuliaType::get_prjlvalue_ty(Vboxed->getContext()));
        assert(tbaa != nullptr);
        assert(!(isboxed && TIndex != nullptr));
        assert(TIndex == nullptr || TIndex->getType() == getInt8Ty(TIndex->getContext()));
    }
    explicit jl_cgval_t(jl_value_t *typ) : // ghost value constructor
        // mark explicit to avoid being used implicitly for conversion from nullptr (use jl_cgval_t() instead)
        V(nullptr),
        Vboxed(nullptr),
        TIndex(nullptr),
        inline_roots(),
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
        inline_roots(v.inline_roots),
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
            assert((TIndex == nullptr) == jl_is_concrete_type(typ));
        }
        else {
            assert(isboxed || v.typ == typ || tindex);
        }
    }
    explicit jl_cgval_t() : // undef / unreachable constructor
        V(nullptr),
        Vboxed(nullptr),
        TIndex(nullptr),
        inline_roots(),
        constant(nullptr),
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
    AllocaInst *inline_roots; // stack roots for the inline_roots array, if needed
    DILocalVariable *dinfo;
    // if the variable might be used undefined and is not boxed
    // this i1 flag is true when it is defined
    Value *defFlag;
    bool isSA; // whether all stores dominate all uses
    bool isVolatile;
    bool isArgument;
    bool usedUndef;
    bool used;

    jl_varinfo_t(LLVMContext &ctxt) : boxroot(nullptr),
                     value(jl_cgval_t()),
                     pTIndex(nullptr),
                     inline_roots(nullptr),
                     dinfo(nullptr),
                     defFlag(nullptr),
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
    Function *f = NULL;
    MDNode* LoopID = NULL;
    // local var info. globals are not in here.
    SmallVector<jl_varinfo_t, 0> slots;
    std::map<int, jl_varinfo_t> phic_slots;
    std::map<int, std::pair<Value*, Value*> > scope_restore;
    SmallVector<jl_cgval_t, 0> SAvalues;
    SmallVector<std::tuple<jl_cgval_t, BasicBlock *, AllocaInst *, PHINode *, SmallVector<PHINode*,0>, jl_value_t *>, 0> PhiNodes;
    SmallVector<bool, 0> ssavalue_assigned;
    SmallVector<int, 0> ssavalue_usecount;
    jl_module_t *module = NULL;
    jl_typecache_t type_cache;
    jl_tbaacache_t tbaa_cache;
    jl_noaliascache_t aliasscope_cache;
    jl_method_instance_t *linfo = NULL;
    jl_value_t *rettype = NULL;
    jl_code_info_t *source = NULL;
    jl_array_t *code = NULL;
    size_t min_world = 0;
    size_t max_world = -1;
    const char *name = NULL;
    StringRef file{};
    int32_t line = -1;
    Value *spvals_ptr = NULL;
    Value *argArray = NULL;
    Value *argCount = NULL;
    std::string funcName;
    int vaSlot = -1;        // name of vararg argument
    int nReqArgs = 0;
    int nargs = 0;
    int nvargs = -1;
    bool is_opaque_closure = false;

    Value *pgcstack = NULL;
    Instruction *topalloca = NULL;
    Value *world_age_at_entry = NULL;

    bool use_cache = false;
    bool external_linkage = false;
    const jl_cgparams_t *params = NULL;

    SmallVector<std::unique_ptr<Module>, 0> llvmcall_modules;

    jl_codectx_t(LLVMContext &llvmctx, jl_codegen_params_t &params, size_t min_world, size_t max_world)
      : builder(llvmctx),
        emission_context(params),
        call_targets(),
        min_world(min_world),
        max_world(max_world),
        use_cache(params.cache),
        external_linkage(params.external_linkage),
        params(params.params) {
    }

    jl_codectx_t(LLVMContext &llvmctx, jl_codegen_params_t &params, jl_code_instance_t *ci) :
        jl_codectx_t(llvmctx, params, jl_atomic_load_relaxed(&ci->min_world), jl_atomic_load_relaxed(&ci->max_world)) {}

    jl_typecache_t &types() {
        type_cache.initialize(builder.getContext(), emission_context.DL);
        return type_cache;
    }

    jl_tbaacache_t &tbaa() {
        tbaa_cache.initialize(builder.getContext());
        return tbaa_cache;
    }

    jl_noaliascache_t &noalias() {
        aliasscope_cache.initialize(builder.getContext());
        return aliasscope_cache;
    }

    ~jl_codectx_t() {
        // Transfer local delayed calls to the global queue
        for (auto call_target : call_targets)
            emission_context.workqueue.push_back(call_target);
    }
};

GlobalVariable *JuliaVariable::realize(jl_codectx_t &ctx) {
    return realize(jl_Module);
}

jl_aliasinfo_t::jl_aliasinfo_t(jl_codectx_t &ctx, Region r, MDNode *tbaa): tbaa(tbaa), tbaa_struct(nullptr) {
    MDNode *alias_scope = nullptr;
    jl_noaliascache_t::jl_regions_t regions = ctx.noalias().regions;
    switch (r) {
        case Region::unknown:
            alias_scope = nullptr;
            break;
        case Region::gcframe:
            alias_scope = regions.gcframe;
            break;
        case Region::stack:
            alias_scope = regions.stack;
            break;
        case Region::data:
            alias_scope = regions.data;
            break;
        case Region::constant:
            alias_scope = regions.constant;
            break;
        case Region::type_metadata:
            alias_scope = regions.type_metadata;
            break;
    }

    MDNode *all_scopes[5] = { regions.gcframe, regions.stack, regions.data, regions.type_metadata, regions.constant };
    if (alias_scope) {
        // The matching region is added to !alias.scope
        // All other regions are added to !noalias

        int i = 0;
        Metadata *scopes[1] = { alias_scope };
        Metadata *noaliases[4];
        for (auto const &scope: all_scopes) {
            if (scope == alias_scope) continue;
            noaliases[i++] = scope;
        }

        this->scope = MDNode::get(ctx.builder.getContext(), ArrayRef<Metadata*>(scopes));
        this->noalias = MDNode::get(ctx.builder.getContext(), ArrayRef<Metadata*>(noaliases));
    }
}

jl_aliasinfo_t jl_aliasinfo_t::fromTBAA(jl_codectx_t &ctx, MDNode *tbaa) {
    auto cache = ctx.tbaa();

    // Each top-level TBAA node has a corresponding !alias.scope scope
    MDNode *tbaa_srcs[5] = { cache.tbaa_gcframe, cache.tbaa_stack, cache.tbaa_data, cache.tbaa_array, cache.tbaa_const };
    Region regions[5] = { Region::gcframe, Region::stack, Region::data, Region::type_metadata, Region::constant };

    if (tbaa != nullptr) {
        MDNode *node = cast<MDNode>(tbaa->getOperand(1));
        if (cast<MDString>(node->getOperand(0))->getString() != "jtbaa") {

            // Climb up to node just before root
            MDNode *parent_node = cast<MDNode>(node->getOperand(1));
            while (cast<MDString>(parent_node->getOperand(0))->getString() != "jtbaa") {
                node = parent_node;
                parent_node = cast<MDNode>(node->getOperand(1));
            }

            // Find the matching node's index
            for (int i = 0; i < 5; i++) {
                if (cast<MDNode>(tbaa_srcs[i]->getOperand(1)) == node)
                    return jl_aliasinfo_t(ctx, regions[i], tbaa);
            }
        }
    }

    return jl_aliasinfo_t(ctx, Region::unknown, tbaa);
}

static Type *julia_type_to_llvm(jl_codectx_t &ctx, jl_value_t *jt, bool *isboxed = NULL);
static jl_returninfo_t get_specsig_function(jl_codegen_params_t &ctx, Module *M, Value *fval, StringRef name, jl_value_t *sig, jl_value_t *jlrettype, bool is_opaque_closure,
        ArrayRef<const char*> ArgNames=None, unsigned nreq=0);
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaval = -1);
static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, jl_value_t *scope, bool isvol, MDNode *tbaa);
static jl_cgval_t emit_sparam(jl_codectx_t &ctx, size_t i);
static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const Twine &msg);
static Value *get_current_task(jl_codectx_t &ctx);
static Value *get_current_ptls(jl_codectx_t &ctx);
static Value *get_tls_world_age(jl_codectx_t &ctx);
static Value *get_scope_field(jl_codectx_t &ctx);
static Value *get_tls_world_age_field(jl_codectx_t &ctx);
static void CreateTrap(IRBuilder<> &irbuilder, bool create_new_block = true);
static CallInst *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                             ArrayRef<jl_cgval_t> args, size_t nargs, JuliaFunction<> *trampoline);
static CallInst *emit_jlcall(jl_codectx_t &ctx, JuliaFunction<> *theFptr, Value *theF,
                             ArrayRef<jl_cgval_t> args, size_t nargs, JuliaFunction<> *trampoline);
static Value *emit_f_is(jl_codectx_t &ctx, const jl_cgval_t &arg1, const jl_cgval_t &arg2,
                        Value *nullcheck1 = nullptr, Value *nullcheck2 = nullptr);
static jl_cgval_t emit_new_struct(jl_codectx_t &ctx, jl_value_t *ty, size_t nargs, ArrayRef<jl_cgval_t> argv, bool is_promotable=false);
static jl_cgval_t emit_invoke(jl_codectx_t &ctx, const jl_cgval_t &lival, ArrayRef<jl_cgval_t> argv, size_t nargs, jl_value_t *rt);

static Value *literal_pointer_val(jl_codectx_t &ctx, jl_value_t *p);
static unsigned julia_alignment(jl_value_t *jt);
static void recombine_value(jl_codectx_t &ctx, const jl_cgval_t &x, Value *dst, jl_aliasinfo_t const &dst_ai, Align alignment, bool isVolatile);

static GlobalVariable *prepare_global_in(Module *M, JuliaVariable *G)
{
    return G->realize(M);
}

template<typename TypeFn_t>
static Function *prepare_call_in(Module *M, JuliaFunction<TypeFn_t> *G)
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
                G->isConstant(), G->getLinkage(),
                nullptr, G->getName(), nullptr, G->getThreadLocalMode());
        if (proto->hasLocalLinkage()) {
            proto->setInitializer(G->getInitializer());
        }
        proto->copyAttributesFrom(G);
        return proto;
    }
    return cast<GlobalVariable>(local);
}

static Value *emit_ptrgep(jl_codectx_t &ctx, Value *base, size_t byte_offset, const Twine &Name="")
{
    auto *gep = ctx.builder.CreateConstInBoundsGEP1_32(getInt8Ty(ctx.builder.getContext()), base, byte_offset);
    setName(ctx.emission_context, gep, Name);
    return gep;
}

static Value *emit_ptrgep(jl_codectx_t &ctx, Value *base, Value *byte_offset, const Twine &Name="")
{
    auto *gep = ctx.builder.CreateInBoundsGEP(getInt8Ty(ctx.builder.getContext()), base, byte_offset, Name);
    setName(ctx.emission_context, gep, Name);
    return gep;
}


// --- convenience functions for tagging llvm values with julia types ---

static GlobalVariable *get_pointer_to_constant(jl_codegen_params_t &emission_context, Constant *val, Align align, const Twine &name, Module &M)
{
    GlobalVariable *&gv = emission_context.mergedConstants[val];
    auto get_gv = [&](const Twine &name) {
        auto gv = new GlobalVariable(
                M,
                val->getType(),
                true,
                GlobalVariable::PrivateLinkage,
                val,
                name);
        gv->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
        gv->setAlignment(align);
        return gv;
    };
    if (gv == nullptr) {
        gv = get_gv(name + "#" + Twine(emission_context.mergedConstants.size()));
    }
    else if (gv->getParent() != &M) {
        StringRef gvname = gv->getName();
        gv = M.getNamedGlobal(gvname);
        if (!gv) {
            gv = get_gv(gvname);
        }
    }
    assert(gv->getName().starts_with(name.str()));
    assert(val == gv->getInitializer());
    return gv;
}

static AllocaInst *emit_static_alloca(jl_codectx_t &ctx, Type *lty, Align align)
{
    ++EmittedAllocas;
    return new AllocaInst(lty, ctx.topalloca->getModule()->getDataLayout().getAllocaAddrSpace(), nullptr, align, "", /*InsertBefore=*/ctx.topalloca);
}

static AllocaInst *emit_static_alloca(jl_codectx_t &ctx, unsigned nb, Align align)
{
    // Stupid hack: SROA takes hints from the element type, and will happily split this allocation into lots of unaligned bits
    // if it cannot find something better to do, which is terrible for performance.
    // However, if we emit this with an element size equal to the alignment, it will instead split it into aligned chunks
    // which is great for performance and vectorization.
    if (alignTo(nb, align) == align.value()) // don't bother with making an array of length 1
        return emit_static_alloca(ctx, ctx.builder.getIntNTy(align.value() * 8), align);
    return emit_static_alloca(ctx, ArrayType::get(ctx.builder.getIntNTy(align.value() * 8), alignTo(nb, align) / align.value()), align);
}

static AllocaInst *emit_static_roots(jl_codectx_t &ctx, unsigned nroots)
{
    AllocaInst *staticroots = emit_static_alloca(ctx, ctx.types().T_prjlvalue, Align(sizeof(void*)));
    staticroots->setOperand(0, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), nroots));
    IRBuilder<> builder(ctx.topalloca);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
    // make sure these are nullptr early from LLVM's perspective, in case it decides to SROA it
    ai.decorateInst(builder.CreateMemSet(staticroots, builder.getInt8(0), nroots * sizeof(void*), staticroots->getAlign()))->moveAfter(ctx.topalloca);
    return staticroots;
}

static void undef_derived_strct(jl_codectx_t &ctx, Value *ptr, jl_datatype_t *sty, MDNode *tbaa)
{
    assert(ptr->getType()->getPointerAddressSpace() != AddressSpace::Tracked);
    size_t first_offset = sty->layout->nfields ? jl_field_offset(sty, 0) : 0;
    if (first_offset != 0)
        ctx.builder.CreateMemSet(ptr, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0), first_offset, MaybeAlign(0));
    if (sty->layout->first_ptr < 0)
        return;
    size_t i, np = sty->layout->npointers;
    auto T_prjlvalue = JuliaType::get_prjlvalue_ty(ctx.builder.getContext());
    for (i = 0; i < np; i++) {
        Value *fld = emit_ptrgep(ctx, ptr, jl_ptr_offset(sty, i) * sizeof(jl_value_t*));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        ai.decorateInst(ctx.builder.CreateStore(Constant::getNullValue(T_prjlvalue), fld));
    }
}

static Value *emit_inttoptr(jl_codectx_t &ctx, Value *v, Type *ty)
{
    // Almost all of our inttoptr are generated due to representing `Ptr` with `ctx.types().T_size`
    // in LLVM and most of these integers are generated from `ptrtoint` in the first place.
    if (auto I = dyn_cast<PtrToIntInst>(v)) {
        auto ptr = I->getOperand(0);
        if (ty->getPointerAddressSpace() == ptr->getType()->getPointerAddressSpace())
            return ptr;
        else
            return ctx.builder.CreateAddrSpaceCast(ptr, ty);
    }
    ++EmittedIntToPtrs;
    return ctx.builder.CreateIntToPtr(v, ty);
}

static inline jl_cgval_t ghostValue(jl_codectx_t &ctx, jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return jl_cgval_t(); // Undef{}
    if (typ == (jl_value_t*)jl_typeofbottom_type) {
        // normalize TypeofBottom to Type{Union{}}
        typ = (jl_value_t*)jl_typeofbottom_type->super;
    }
    if (jl_is_type_type(typ)) {
        assert(is_uniquerep_Type(typ));
        // replace T::Type{T} with T, by assuming that T must be a leaftype of some sort
        jl_cgval_t constant(NULL, true, typ, NULL, best_tbaa(ctx.tbaa(), typ), None);
        constant.constant = jl_tparam0(typ);
        if (typ == (jl_value_t*)jl_typeofbottom_type->super)
            constant.isghost = true;
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
    if (jl_is_type(jv) && jv != jl_bottom_type) {
        typ = (jl_value_t*)jl_wrap_Type(jv); // TODO: gc-root this?
    }
    else {
        typ = jl_typeof(jv);
        if (jl_is_datatype_singleton((jl_datatype_t*)typ))
            return ghostValue(ctx, typ);
    }
    jl_cgval_t constant(NULL, true, typ, NULL, best_tbaa(ctx.tbaa(), typ), None);
    constant.constant = jv;
    return constant;
}


static inline jl_cgval_t mark_julia_slot(Value *v, jl_value_t *typ, Value *tindex, MDNode *tbaa, ArrayRef<Value*> inline_roots=None)
{
    // this enables lazy-copying of immutable values and stack or argument slots
    jl_cgval_t tagval(v, false, typ, tindex, tbaa, inline_roots);
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

static Value *zext_struct(jl_codectx_t &ctx, Value *V);

// TODO: in the future, assume all callers will handle the interior pointers separately, and have
// have zext_struct strip them out, so we aren't saving those to the stack here causing shadow stores
// to be necessary too
static inline jl_cgval_t value_to_pointer(jl_codectx_t &ctx, Value *v, jl_value_t *typ, Value *tindex)
{
    Value *loc;
    v = zext_struct(ctx, v);
    Align align(julia_alignment(typ));
    if (valid_as_globalinit(v)) { // llvm can't handle all the things that could be inside a ConstantExpr
        assert(jl_is_concrete_type(typ)); // not legal to have an unboxed abstract type
        loc = get_pointer_to_constant(ctx.emission_context, cast<Constant>(v), align, "_j_const", *jl_Module);
    }
    else {
        loc = emit_static_alloca(ctx, v->getType(), align);
        ctx.builder.CreateAlignedStore(v, loc, align);
    }
    return mark_julia_slot(loc, typ, tindex, ctx.tbaa().tbaa_stack);
}
static inline jl_cgval_t value_to_pointer(jl_codectx_t &ctx, const jl_cgval_t &v)
{
    if (!v.inline_roots.empty()) {
        //if (v.V == nullptr) {
        //    AllocaInst *loc = emit_static_roots(ctx, v.inline_roots.size());
        //    for (size_t i = 0; i < v.inline_roots.counts(); i++)
        //        ctx.builder.CreateAlignedStore(v.inline_roots[i], emit_ptrgep(ctx, loc, i * sizeof(void*)), Align(sizeof(void*)));
        //    return mark_julia_slot(loc, v.typ, v.TIndex, ctx.tbaa().tbaa_gcframe);
        //}
        Align align(julia_alignment(v.typ));
        Type *ty = julia_type_to_llvm(ctx, v.typ);
        AllocaInst *loc = emit_static_alloca(ctx, ty, align);
        auto tbaa = v.V == nullptr ? ctx.tbaa().tbaa_gcframe : ctx.tbaa().tbaa_stack;
        auto stack_ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        recombine_value(ctx, v, loc, stack_ai, align, false);
        return mark_julia_slot(loc, v.typ, v.TIndex, tbaa);
    }
    if (v.ispointer())
        return v;
    return value_to_pointer(ctx, v.V, v.typ, v.TIndex);
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_value_t *typ)
{
    if (jl_is_type_type(typ)) {
        if (is_uniquerep_Type(typ)) {
            // replace T::Type{T} with T
            return ghostValue(ctx, typ);
        }
    }
    else if (jl_is_datatype(typ) && jl_is_datatype_singleton((jl_datatype_t*)typ)) {
        // no need to explicitly load/store a constant/ghost value
        return ghostValue(ctx, typ);
    }
    Type *T = julia_type_to_llvm(ctx, typ);
    if (type_is_ghost(T)) {
        return ghostValue(ctx, typ);
    }
    if (v && !isboxed && v->getType()->isAggregateType()) {
        // eagerly put this back onto the stack
        // llvm mem2reg pass will remove this if unneeded
        if (CountTrackedPointers(v->getType()).count == 0)
            return value_to_pointer(ctx, v, typ, NULL);
    }
    if (isboxed)
        return jl_cgval_t(v, isboxed, typ, NULL, best_tbaa(ctx.tbaa(), typ), None);
    return jl_cgval_t(v, typ, NULL);
}

static inline jl_cgval_t mark_julia_type(jl_codectx_t &ctx, Value *v, bool isboxed, jl_datatype_t *typ)
{
    return mark_julia_type(ctx, v, isboxed, (jl_value_t*)typ);
}

// see if it might be profitable (and cheap) to change the type of v to typ
static inline jl_cgval_t update_julia_type(jl_codectx_t &ctx, const jl_cgval_t &v, jl_value_t *typ)
{
    if (v.typ == jl_bottom_type || typ == (jl_value_t*)jl_any_type || jl_egal(v.typ, typ))
        return v; // fast-path
    if (v.constant)
        return jl_isa(v.constant, typ) ? v : jl_cgval_t();
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
                alwaysboxed = !((jl_datatype_t*)utyp)->name->abstract && ((jl_datatype_t*)utyp)->name->mutabl;
            if (alwaysboxed) {
                // discovered that this union-split type must actually be isboxed
                if (v.Vboxed) {
                    return jl_cgval_t(v.Vboxed, true, typ, NULL, best_tbaa(ctx.tbaa(), typ), v.inline_roots);
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
        return ghostValue(ctx, typ);
    else if (v.TIndex && v.V == NULL) {
        // type mismatch (there weren't any non-ghost values in the union)
        CreateTrap(ctx.builder);
        return jl_cgval_t();
    }
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
        vi.defFlag = emit_static_alloca(ctx, getInt1Ty(ctx.builder.getContext()), Align(1));
        setName(ctx.emission_context, vi.defFlag, "isdefined");
        store_def_flag(ctx, vi, false);
    }
}


// --- utilities ---

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
    Value *new_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER);
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
    setName(ctx.emission_context, new_tindex, "tindex");

    // some of the values are still unboxed
    if (!isa<Constant>(new_tindex)) {
        Value *wasboxed = NULL;
        // If the old value was boxed and unknown (type tag UNION_BOX_MARKER),
        // it is possible that the tag was actually one of the types
        // that are now explicitly represented. To find out, we need
        // to compare typeof(v.Vboxed) (i.e. the type of the unknown
        // value) against all the types that are now explicitly
        // selected and select the appropriate one as our new tindex.
        if (v.Vboxed) {
            wasboxed = ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
            new_tindex = ctx.builder.CreateOr(wasboxed, new_tindex);
            wasboxed = ctx.builder.CreateICmpNE(wasboxed, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            setName(ctx.emission_context, wasboxed, "wasboxed");

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
                    union_box_dt = emit_typeof(ctx, v.Vboxed, skip != NULL, true);
                    post_union_isaBB = ctx.builder.GetInsertBlock();
                }
            };

            // If we don't find a match. The type remains unknown
            // (UNION_BOX_MARKER). We could use `v.Tindex`, here, since we know
            // it has to be UNION_BOX_MARKER, but it seems likely the backend
            // will like the explicit constant better.
            Value *union_box_tindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER);
            unsigned counter = 0;
            for_each_uniontype_small(
                // for each new union-split value
                [&](unsigned idx, jl_datatype_t *jt) {
                    unsigned old_idx = get_box_tindex(jt, v.typ);
                    if (old_idx == 0) {
                        // didn't handle this item before, select its new union index
                        maybe_setup_union_isa();
                        Value *cmp = ctx.builder.CreateICmpEQ(emit_tagfrom(ctx, jt), union_box_dt);
                        union_box_tindex = ctx.builder.CreateSelect(cmp, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER | idx), union_box_tindex);
                    }
                },
                typ,
                counter);
            setName(ctx.emission_context, union_box_tindex, "union_box_tindex");
            if (union_box_dt) {
                BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_union_isa", ctx.f);
                ctx.builder.CreateBr(postBB);
                ctx.builder.SetInsertPoint(currBB);
                Value *wasunknown = ctx.builder.CreateICmpEQ(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
                ctx.builder.CreateCondBr(wasunknown, union_isaBB, postBB);
                ctx.builder.SetInsertPoint(postBB);
                PHINode *tindex_phi = ctx.builder.CreatePHI(getInt8Ty(ctx.builder.getContext()), 2);
                tindex_phi->addIncoming(new_tindex, currBB);
                tindex_phi->addIncoming(union_box_tindex, post_union_isaBB);
                new_tindex = tindex_phi;
                setName(ctx.emission_context, new_tindex, "tindex");
            }
        }
        if (!skip_box.all()) {
            // some values weren't unboxed in the new union
            // box them now (tindex above already selected UNION_BOX_MARKER = box for them)
            Value *boxv = box_union(ctx, v, skip_box);
            if (v.Vboxed) {
                // If the value is boxed both before and after, we don't need
                // to touch it at all. Otherwise we're either transitioning
                // unboxed->boxed, or leaving an unboxed value in place.
                Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(new_tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
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
                jl_cgval_t oldv = value_to_pointer(ctx, v);
                slotv = oldv.V;
                tbaa = oldv.tbaa;
                slotv = ctx.builder.CreateSelect(isboxv,
                            decay_derived(ctx, boxv),
                            decay_derived(ctx, slotv));
            }
            jl_cgval_t newv = jl_cgval_t(slotv, false, typ, new_tindex, tbaa, v.inline_roots);
            assert(boxv->getType() == ctx.types().T_prjlvalue);
            newv.Vboxed = boxv;
            return newv;
        }
    }
    else {
        return jl_cgval_t(boxed(ctx, v), true, typ, NULL, best_tbaa(ctx.tbaa(), typ), None);
    }
    return jl_cgval_t(v, typ, new_tindex);
}

// given a value marked with type `v.typ`, compute the mapping and/or boxing to return a value of type `typ`
// TODO: should this set TIndex when trivial (such as UNION_BOX_MARKER or concrete types) ?
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
        if (jl_is_concrete_type(v.typ)) {
            // type mismatch: changing from one leaftype to another
            if (skip)
                *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
            else
                CreateTrap(ctx.builder);
            return jl_cgval_t();
        }
        bool mustbox_union = v.TIndex && !jl_is_pointerfree(typ);
        if (v.Vboxed && (v.isboxed || mustbox_union)) {
            if (skip) {
                *skip = ctx.builder.CreateNot(emit_exactly_isa(ctx, v, (jl_datatype_t*)typ, true));
            }
            return jl_cgval_t(v.Vboxed, true, typ, NULL, best_tbaa(ctx.tbaa(), typ), v.inline_roots);
        }
        if (mustbox_union) {
            // type mismatch: there weren't any boxed values in the union
            if (skip)
                *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
            else
                CreateTrap(ctx.builder);
            return jl_cgval_t();
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
                if (v.V && v.inline_roots.empty() && !v.ispointer()) {
                    // TODO: remove this branch once all consumers of v.TIndex understand how to handle a non-ispointer value
                    return jl_cgval_t(value_to_pointer(ctx, v), typ, new_tindex);
                }
            }
            else if (jl_subtype(v.typ, typ)) {
                makeboxed = true;
            }
            else if (skip) {
                // undef
                *skip = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
                return jl_cgval_t();
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
            return mark_julia_type(ctx, boxed(ctx, v), true, typ);
        }
    }
    return jl_cgval_t(v, typ, new_tindex);
}

std::unique_ptr<Module> jl_create_llvm_module(StringRef name, LLVMContext &context, const DataLayout &DL, const Triple &triple) JL_NOTSAFEPOINT
{
    ++ModulesCreated;
    auto m = std::make_unique<Module>(name, context);
    // According to clang darwin above 10.10 supports dwarfv4
    if (!m->getModuleFlag("Dwarf Version")) {
        m->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 4);
    }
    if (!m->getModuleFlag("Debug Info Version"))
        m->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
            llvm::DEBUG_METADATA_VERSION);
    m->setDataLayout(DL);
    m->setTargetTriple(triple.str());

    if (triple.isOSWindows() && triple.getArch() == Triple::x86) {
        // tell Win32 to assume the stack is always 16-byte aligned,
        // and to ensure that it is 16-byte aligned for out-going calls,
        // to ensure compatibility with GCC codes
        m->setOverrideStackAlignment(16);
    }

#if defined(JL_DEBUG_BUILD)
    m->setStackProtectorGuard("global");
#endif
    return m;
}

static void jl_name_jlfunc_args(jl_codegen_params_t &params, Function *F) JL_NOTSAFEPOINT
{
    assert(F->arg_size() == 3);
    F->getArg(0)->setName("function::Core.Function");
    F->getArg(1)->setName("args::Any[]");
    F->getArg(2)->setName("nargs::UInt32");
}

static void jl_name_jlfuncparams_args(jl_codegen_params_t &params, Function *F) JL_NOTSAFEPOINT
{
    assert(F->arg_size() == 4);
    F->getArg(0)->setName("function::Core.Function");
    F->getArg(1)->setName("args::Any[]");
    F->getArg(2)->setName("nargs::UInt32");
    F->getArg(3)->setName("sparams::Any");
}

void jl_init_function(Function *F, const Triple &TT) JL_NOTSAFEPOINT
{
    // set any attributes that *must* be set on all functions
    AttrBuilder attr(F->getContext());
    if (TT.isOSWindows() && TT.getArch() == Triple::x86) {
        // tell Win32 to assume the stack is always 16-byte aligned,
        // and to ensure that it is 16-byte aligned for out-going calls,
        // to ensure compatibility with GCC codes
        attr.addStackAlignmentAttr(16);
    }
    if (TT.isOSWindows() && TT.getArch() == Triple::x86_64) {
        attr.addUWTableAttr(llvm::UWTableKind::Default); // force NeedsWinEH
    }
    if (jl_fpo_disabled(TT))
        attr.addAttribute("frame-pointer", "all");
    if (!TT.isOSWindows()) {
#if !defined(_COMPILER_ASAN_ENABLED_)
        // ASAN won't like us accessing undefined memory causing spurious issues,
        // and Windows has platform-specific handling which causes it to mishandle
        // this annotation. Other platforms should just ignore this if they don't
        // implement it.
        attr.addAttribute("probe-stack", "inline-asm");
        //attr.addAttribute("stack-probe-size", "4096"); // can use this to change the default
#endif
    }
#if defined(_COMPILER_ASAN_ENABLED_)
    attr.addAttribute(Attribute::SanitizeAddress);
#endif
#if defined(_COMPILER_MSAN_ENABLED_)
    attr.addAttribute(Attribute::SanitizeMemory);
#endif
    F->addFnAttrs(attr);
}

static bool uses_specsig(jl_value_t *sig, bool needsparams, jl_value_t *rettype, bool prefer_specsig)
{
    if (needsparams)
        return false;
    if (sig == (jl_value_t*)jl_anytuple_type)
        return false;
    if (!jl_is_datatype(sig))
        return false;
    if (jl_nparams(sig) == 0)
        return false;
    if (jl_vararg_kind(jl_tparam(sig, jl_nparams(sig) - 1)) == JL_VARARG_UNBOUND)
        return false;
    // not invalid, consider if specialized signature is worthwhile
    // n.b. sig is sometimes wrong for OC (tparam0 might be the captures type of the specialization, even though what gets passed in that slot is an OC object), so prefer_specsig is always set (instead of recomputing tparam0 using get_oc_type)
    if (prefer_specsig)
        return true;
    if (!deserves_retbox(rettype) && !jl_is_datatype_singleton((jl_datatype_t*)rettype) && rettype != (jl_value_t*)jl_bool_type)
        return true;
    if (jl_is_uniontype(rettype)) {
        bool allunbox;
        size_t nbytes, align, min_align;
        union_alloca_type((jl_uniontype_t*)rettype, allunbox, nbytes, align, min_align);
        if (nbytes > 0)
            return true; // some elements of the union could be returned unboxed avoiding allocation
    }
    if (jl_nparams(sig) <= 3) // few parameters == more efficient to pass directly
        return true;
    bool allSingleton = true;
    for (size_t i = 0; i < jl_nparams(sig); i++) {
        jl_value_t *sigt = jl_tparam(sig, i);
        // TODO: sigt = unwrap_va(sigt)
        bool issing = jl_is_datatype(sigt) && jl_is_datatype_singleton((jl_datatype_t*)sigt);
        allSingleton &= issing;
        if (!deserves_argbox(sigt) && !issing) {
            return true;
        }
    }
    if (allSingleton)
        return true;
    return false; // jlcall sig won't require any box allocations
}

static std::pair<bool, bool> uses_specsig(jl_value_t *abi, jl_method_instance_t *lam, jl_value_t *rettype, bool prefer_specsig)
{
    bool needsparams = false;
    if (jl_is_method(lam->def.method)) {
        if ((size_t)jl_subtype_env_size(lam->def.method->sig) != jl_svec_len(lam->sparam_vals))
            needsparams = true;
        for (size_t i = 0; i < jl_svec_len(lam->sparam_vals); ++i) {
            if (jl_is_typevar(jl_svecref(lam->sparam_vals, i)))
                needsparams = true;
        }
    }
    return std::make_pair(uses_specsig(abi, needsparams, rettype, prefer_specsig), needsparams);
}


// Logging for code coverage and memory allocation

JL_DLLEXPORT void jl_coverage_alloc_line(StringRef filename, int line);
JL_DLLEXPORT uint64_t *jl_coverage_data_pointer(StringRef filename, int line);
JL_DLLEXPORT uint64_t *jl_malloc_data_pointer(StringRef filename, int line);

static void visitLine(jl_codectx_t &ctx, uint64_t *ptr, Value *addend, const char *name)
{
    Value *pv = ConstantExpr::getIntToPtr(
        ConstantInt::get(ctx.types().T_size, (uintptr_t)ptr),
        getPointerTy(ctx.builder.getContext()));
    Value *v = ctx.builder.CreateLoad(getInt64Ty(ctx.builder.getContext()), pv, true, name);
    v = ctx.builder.CreateAdd(v, addend);
    ctx.builder.CreateStore(v, pv, true); // volatile, not atomic, so this might be an underestimate,
                                          // but it's faster this way
}

// Code coverage

static void coverageVisitLine(jl_codectx_t &ctx, StringRef filename, int line)
{
    if (ctx.emission_context.imaging_mode)
        return; // TODO
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    visitLine(ctx, jl_coverage_data_pointer(filename, line), ConstantInt::get(getInt64Ty(ctx.builder.getContext()), 1), "lcnt");
}

// Memory allocation log (malloc_log)

static void mallocVisitLine(jl_codectx_t &ctx, StringRef filename, int line, Value *sync)
{
    if (ctx.emission_context.imaging_mode)
        return; // TODO
    if (filename == "" || filename == "none" || filename == "no file" || filename == "<missing>" || line < 0)
        return;
    Value *addend = sync
        ? ctx.builder.CreateCall(prepare_call(sync_gc_total_bytes_func), {sync})
        : ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    visitLine(ctx, jl_malloc_data_pointer(filename, line), addend, "bytecnt");
}

// --- constant determination ---

static jl_value_t *static_apply_type(jl_codectx_t &ctx, ArrayRef<jl_cgval_t> args, size_t nargs)
{
    assert(nargs > 1);
    SmallVector<jl_value_t *, 0> v(nargs);
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
        result = jl_apply(v.data(), nargs);
    }
    JL_CATCH {
        result = NULL;
    }
    jl_current_task->world_age = last_age;
    return result;
}

static void emit_depwarn_check(jl_codectx_t &ctx, jl_binding_t *b)
{
    Value *bp = julia_binding_gv(ctx, b);
    ctx.builder.CreateCall(prepare_call(jldepcheck_func), { bp });
}

// try to statically evaluate, NULL if not possible. note that this may allocate, and as
// such the resulting value should not be embedded directly in the generated code.
static jl_value_t *static_eval(jl_codectx_t &ctx, jl_value_t *ex)
{
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        jl_binding_t *bnd = jl_get_module_binding(ctx.module, sym, 1);
        int possibly_deprecated = 0;
        jl_value_t *cval = jl_get_binding_leaf_partitions_value_if_const(bnd, &possibly_deprecated, ctx.min_world, ctx.max_world);
        if (cval) {
            if (possibly_deprecated)
                emit_depwarn_check(ctx, bnd);
            return cval;
        }
        return NULL;
    }
    if (jl_is_slotnumber(ex) || jl_is_argument(ex))
        return NULL;
    if (jl_is_ssavalue(ex)) {
        ssize_t idx = ((jl_ssavalue_t*)ex)->id - 1;
        assert(idx >= 0);
        if (ctx.ssavalue_assigned[idx]) {
            return ctx.SAvalues[idx].constant;
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
        jl_binding_t *bnd = jl_get_module_binding(jl_globalref_mod(ex), s, 1);
        int possibly_deprecated = 0;
        jl_value_t *v = jl_get_binding_leaf_partitions_value_if_const(bnd, &possibly_deprecated, ctx.min_world, ctx.max_world);
        if (v) {
            if (possibly_deprecated)
                emit_depwarn_check(ctx, bnd);
            return v;
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
                        jl_binding_t *bnd = jl_get_module_binding(m, s, 1);
                        int possibly_deprecated = 0;
                        jl_value_t *v = jl_get_binding_leaf_partitions_value_if_const(bnd, &possibly_deprecated, ctx.min_world, ctx.max_world);
                        if (v) {
                            if (possibly_deprecated)
                                emit_depwarn_check(ctx, bnd);
                            return v;
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
    return (jl_is_slotnumber(e) || jl_is_argument(e)) && jl_slot_number(e)-1 == sl;
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

static bool have_try_block(jl_array_t *stmts)
{
    size_t slength = jl_array_dim0(stmts);
    for (int i = 0; i < (int)slength; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_enternode(st)) {
            int last = jl_enternode_catch_dest(st);
            if (last == 0)
                continue;
            return 1;
        }
    }
    return 0;
}

// conservative marking of all variables potentially used after a catch block that were assigned after the try
static void mark_volatile_vars(jl_array_t *stmts, SmallVectorImpl<jl_varinfo_t> &slots, const std::set<int> &bbstarts)
{
    if (!have_try_block(stmts))
        return;
    size_t slength = jl_array_dim0(stmts);
    BitVector assigned_in_block(slots.size()); // since we don't have domtree access, conservatively only ignore slots assigned in the same basic block
    for (int j = 0; j < (int)slength; j++) {
        if (bbstarts.count(j + 1))
            assigned_in_block.reset();
        jl_value_t *stmt = jl_array_ptr_ref(stmts, j);
        if (jl_is_expr(stmt)) {
            jl_expr_t *e = (jl_expr_t*)stmt;
            if (e->head == jl_assign_sym) {
                jl_value_t *l = jl_exprarg(e, 0);
                if (jl_is_slotnumber(l)) {
                    assigned_in_block.set(jl_slot_number(l)-1);
                }
            }
        }
        for (int slot = 0; slot < (int)slots.size(); slot++) {
            if (!assigned_in_block.test(slot) && local_var_occurs(stmt, slot)) {
                jl_varinfo_t &vi = slots[slot];
                vi.isVolatile = true;
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
        size_t i, elen = jl_array_nrows(values);
        for (i = 0; i < elen; i++) {
            jl_value_t *v = jl_array_ptr_ref(values, i);
            general_use_analysis(ctx, v, f);
        }
    }
    else if (jl_is_phinode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 1);
        size_t i, elen = jl_array_nrows(values);
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
        if (jl_is_slotnumber(expr) || jl_is_argument(expr)) {
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

static void jl_temporary_root(jl_codegen_params_t &ctx, jl_value_t *val)
{
    if (!jl_is_globally_rooted(val)) {
        jl_array_t *roots = ctx.temporary_roots;
        for (size_t i = 0; i < jl_array_dim0(roots); i++) {
            if (jl_array_ptr_ref(roots, i) == val)
                return;
        }
        jl_array_ptr_1d_push(roots, val);
    }
}
static void jl_temporary_root(jl_codectx_t &ctx, jl_value_t *val)
{
    jl_temporary_root(ctx.emission_context, val);
}

// --- generating function calls ---

static jl_cgval_t emit_globalref_runtime(jl_codectx_t &ctx, jl_binding_t *bnd, jl_module_t *mod, jl_sym_t *name)
{
    Value *bp = julia_binding_gv(ctx, bnd);
    Value *v = ctx.builder.CreateCall(prepare_call(jlgetbindingvalue_func), { bp });
    undef_var_error_ifnot(ctx, ctx.builder.CreateIsNotNull(v), name, (jl_value_t*)mod);
    return mark_julia_type(ctx, v, true, jl_any_type);
}

static jl_cgval_t emit_globalref(jl_codectx_t &ctx, jl_module_t *mod, jl_sym_t *name, AtomicOrdering order)
{
    jl_binding_t *bnd = jl_get_module_binding(mod, name, 1);
    struct restriction_kind_pair rkp = { NULL, NULL, PARTITION_KIND_GUARD, 0 };
    if (!jl_get_binding_leaf_partitions_restriction_kind(bnd, &rkp, ctx.min_world, ctx.max_world)) {
        return emit_globalref_runtime(ctx, bnd, mod, name);
    }
    if (jl_bkind_is_some_constant(rkp.kind) && rkp.kind != PARTITION_KIND_BACKDATED_CONST) {
        if (rkp.maybe_depwarn) {
            Value *bp = julia_binding_gv(ctx, bnd);
            ctx.builder.CreateCall(prepare_call(jldepcheck_func), { bp });
        }
        jl_value_t *constval = rkp.restriction;
        if (!constval) {
            undef_var_error_ifnot(ctx, ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0), name, (jl_value_t*)mod);
            return jl_cgval_t();
        }
        return mark_julia_const(ctx, constval);
    }
    if (rkp.kind != PARTITION_KIND_GLOBAL) {
        return emit_globalref_runtime(ctx, bnd, mod, name);
    }
    Value *bp = julia_binding_gv(ctx, bnd);
    if (rkp.maybe_depwarn) {
        ctx.builder.CreateCall(prepare_call(jldepcheck_func), { bp });
    }
    if (bnd != rkp.binding_if_global)
        bp = julia_binding_gv(ctx, rkp.binding_if_global);
    jl_value_t *ty = rkp.restriction;
    Value *bpval = julia_binding_pvalue(ctx, bp);
    if (ty == nullptr)
        ty = (jl_value_t*)jl_any_type;
    return update_julia_type(ctx, emit_checked_var(ctx, bpval, name, (jl_value_t*)mod, false, ctx.tbaa().tbaa_binding), ty);
}

static jl_cgval_t emit_globalop(jl_codectx_t &ctx, jl_module_t *mod, jl_sym_t *sym, jl_cgval_t rval, const jl_cgval_t &cmp,
                                AtomicOrdering Order, AtomicOrdering FailOrder,
                                bool issetglobal, bool isreplaceglobal, bool isswapglobal, bool ismodifyglobal, bool issetglobalonce,
                                const jl_cgval_t *modifyop, bool alloc)
{
    jl_binding_t *bnd = jl_get_module_binding(mod, sym, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition_all(bnd, ctx.min_world, ctx.max_world);
    Value *bp = julia_binding_gv(ctx, bnd);
    if (bpart) {
        if (jl_binding_kind(bpart) == PARTITION_KIND_GLOBAL) {
            int possibly_deprecated = bpart->kind & PARTITION_FLAG_DEPWARN;
            jl_value_t *ty = bpart->restriction;
            if (ty != nullptr) {
                const std::string fname = issetglobal ? "setglobal!" : isreplaceglobal ? "replaceglobal!" : isswapglobal ? "swapglobal!" : ismodifyglobal ? "modifyglobal!" : "setglobalonce!";
                if (!ismodifyglobal) {
                    // TODO: use typeassert in jl_check_binding_assign_value too
                    emit_typecheck(ctx, rval, ty, "typeassert");
                    rval = update_julia_type(ctx, rval, ty);
                    if (rval.typ == jl_bottom_type)
                        return jl_cgval_t();
                }
                bool isboxed = true;
                bool maybe_null = jl_atomic_load_relaxed(&bnd->value) == NULL;
                if (possibly_deprecated) {
                    ctx.builder.CreateCall(prepare_call(jldepcheck_func), { bp });
                }
                return typed_store(ctx,
                                julia_binding_pvalue(ctx, bp),
                                rval, cmp, ty,
                                ctx.tbaa().tbaa_binding,
                                nullptr,
                                bp,
                                isboxed,
                                Order,
                                FailOrder,
                                0,
                                nullptr,
                                issetglobal,
                                isreplaceglobal,
                                isswapglobal,
                                ismodifyglobal,
                                issetglobalonce,
                                maybe_null,
                                modifyop,
                                fname,
                                mod,
                                sym);

            }
        }
    }
    Value *m = literal_pointer_val(ctx, (jl_value_t*)mod);
    Value *s = literal_pointer_val(ctx, (jl_value_t*)sym);
    ctx.builder.CreateCall(prepare_call(jlcheckbpwritable_func),
        { bp, m, s });
    if (issetglobal) {
        ctx.builder.CreateCall(prepare_call(jlcheckassign_func),
                { bp, m, s, mark_callee_rooted(ctx, boxed(ctx, rval)) });
        return rval;
    }
    else if (isreplaceglobal) {
        Value *r = ctx.builder.CreateCall(prepare_call(jlcheckreplace_func),
                { bp, m, s, boxed(ctx, cmp), boxed(ctx, rval) });
        return mark_julia_type(ctx, r, true, jl_any_type);
    }
    else if (isswapglobal) {
        Value *r = ctx.builder.CreateCall(prepare_call(jlcheckswap_func),
                { bp, m, s, mark_callee_rooted(ctx, boxed(ctx, rval)) });
        return mark_julia_type(ctx, r, true, jl_any_type);
    }
    else if (ismodifyglobal) {
        Value *r = ctx.builder.CreateCall(prepare_call(jlcheckmodify_func),
                { bp, m, s, boxed(ctx, cmp), boxed(ctx, rval) });
        return mark_julia_type(ctx, r, true, jl_any_type);
    }
    else if (issetglobalonce) {
        Value *r = ctx.builder.CreateCall(prepare_call(jlcheckassignonce_func),
                { bp, m, s, mark_callee_rooted(ctx, boxed(ctx, rval)) });
        return mark_julia_type(ctx, r, true, jl_bool_type);
    }
    abort(); // unreachable
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
            Value *dtarg = emit_typeof(ctx, arg1, false, true);
            Value *dt_eq = ctx.builder.CreateICmpEQ(dtarg, emit_typeof(ctx, arg2, false, true));
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
    setName(ctx.emission_context, typeeq, "typematch");
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
    setName(ctx.emission_context, phi, "unionbits_is");
    return phi;
}

static Value *emit_bits_compare(jl_codectx_t &ctx, jl_cgval_t arg1, jl_cgval_t arg2)
{
    ++EmittedBitsCompares;
    jl_value_t *argty = (arg1.constant ? jl_typeof(arg1.constant) : arg1.typ);
    bool isboxed;
    Type *at = julia_type_to_llvm(ctx, arg1.typ, &isboxed);
    assert(jl_is_datatype(arg1.typ) && arg1.typ == (arg2.constant ? jl_typeof(arg2.constant) : arg2.typ) && !isboxed);

    if (type_is_ghost(at))
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);

    if (at->isIntegerTy() || at->isPointerTy() || at->isFloatingPointTy()) {
        Type *at_int = INTT(at, ctx.emission_context.DL);
        Value *varg1 = emit_unbox(ctx, at_int, arg1, argty);
        Value *varg2 = emit_unbox(ctx, at_int, arg2, argty);
        return ctx.builder.CreateICmpEQ(varg1, varg2);
    }

    if (at->isVectorTy()) {
        jl_svec_t *types = ((jl_datatype_t*)argty)->types;
        Value *answer = ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 1);
        Value *varg1 = emit_unbox(ctx, at, arg1, argty);
        Value *varg2 = emit_unbox(ctx, at, arg2, argty);
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
        jl_datatype_t *sty = (jl_datatype_t*)argty;
        size_t sz = jl_datatype_size(sty);
        if (sz > 512 && !sty->layout->flags.haspadding && sty->layout->flags.isbitsegal) {
            Value *varg1 = arg1.inline_roots.empty() && arg1.ispointer() ? data_pointer(ctx, arg1) :
                value_to_pointer(ctx, arg1).V;
            Value *varg2 = arg2.inline_roots.empty() && arg2.ispointer() ? data_pointer(ctx, arg2) :
                value_to_pointer(ctx, arg2).V;
            varg1 = emit_pointer_from_objref(ctx, varg1);
            varg2 = emit_pointer_from_objref(ctx, varg2);
            SmallVector<Value*, 0> gc_uses;
            // these roots may seem a bit overkill, but we want to make sure
            // that a!=b implies (a,)!=(b,) even if a and b are unused and
            // therefore could be freed and then the memory for a reused for b
            gc_uses.append(get_gc_roots_for(ctx, arg1));
            gc_uses.append(get_gc_roots_for(ctx, arg2));
            OperandBundleDef OpBundle("jl_roots", gc_uses);
            auto answer = ctx.builder.CreateCall(prepare_call(memcmp_func), {
                        varg1,
                        varg2,
                        ConstantInt::get(ctx.types().T_size, sz) },
                    ArrayRef<OperandBundleDef>(&OpBundle, gc_uses.empty() ? 0 : 1));

            if (arg1.tbaa || arg2.tbaa) {
                jl_aliasinfo_t ai;
                if (!arg1.tbaa) {
                    ai = jl_aliasinfo_t::fromTBAA(ctx, arg2.tbaa);
                }
                else if (!arg2.tbaa) {
                    ai = jl_aliasinfo_t::fromTBAA(ctx, arg1.tbaa);
                }
                else {
                    jl_aliasinfo_t arg1_ai = jl_aliasinfo_t::fromTBAA(ctx, arg1.tbaa);
                    jl_aliasinfo_t arg2_ai = jl_aliasinfo_t::fromTBAA(ctx, arg2.tbaa);
                    ai = arg1_ai.merge(arg2_ai);
                }
                ai.decorateInst(answer);
            }
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

    jl_value_t *rt1 = (arg1.constant ? jl_typeof(arg1.constant) : arg1.typ);
    jl_value_t *rt2 = (arg2.constant ? jl_typeof(arg2.constant) : arg2.typ);
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
                return emit_exactly_isa(ctx, arg1, (jl_datatype_t*)rt2); // rt2 is a singleton type
            });
        if (arg2.TIndex)
            return emit_nullcheck_guard(ctx, nullcheck2, [&] {
                return emit_exactly_isa(ctx, arg2, (jl_datatype_t*)rt1); // rt1 is a singleton type
            });
        if (!(arg1.isboxed || arg1.constant) || !(arg2.isboxed || arg2.constant))
            // not TIndex && not boxed implies it is an unboxed value of a different type from this singleton
            // (which was probably caught above, but just to be safe, we repeat it here explicitly)
            return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);
        Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : arg1.Vboxed;
        Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : arg2.Vboxed;
        // rooting these values isn't needed since we won't load this pointer
        // and we know at least one of them is a unique Singleton
        // which is already enough to ensure pointer uniqueness for this test
        // even if the other pointer managed to get garbage collected
        // TODO: use emit_pointer_from_objref instead, per comment above
        return ctx.builder.CreateICmpEQ(decay_derived(ctx, varg1), decay_derived(ctx, varg2));
    }

    if (jl_type_intersection(rt1, rt2) == (jl_value_t*)jl_bottom_type) // types are disjoint (exhaustive test)
        return ConstantInt::get(getInt1Ty(ctx.builder.getContext()), 0);

    // can compare any concrete immutable by bits, except for UnionAll
    // which has a special non-bits based egal
    bool justbits1 = jl_is_concrete_immutable(rt1) && !jl_is_kind(rt1);
    bool justbits2 = jl_is_concrete_immutable(rt2) && !jl_is_kind(rt2);
    if (justbits1 || justbits2) { // whether this type is unique'd by value
        return emit_nullcheck_guard2(ctx, nullcheck1, nullcheck2, [&] () -> Value* {
            jl_datatype_t *typ = (jl_datatype_t*)(justbits1 ? rt1 : rt2);
            if (typ == jl_bool_type) { // aka jl_pointer_egal
                // some optimizations for bool, since pointer comparison may be better
                if ((arg1.isboxed || arg1.constant) && (arg2.isboxed || arg2.constant)) { // aka have-fast-pointer
                    Value *varg1 = arg1.constant ? literal_pointer_val(ctx, arg1.constant) : arg1.Vboxed;
                    Value *varg2 = arg2.constant ? literal_pointer_val(ctx, arg2.constant) : arg2.Vboxed;
                    return ctx.builder.CreateICmpEQ(decay_derived(ctx, varg1), decay_derived(ctx, varg2));
                }
            }
            if (rt1 == rt2)
                return emit_bits_compare(ctx, arg1, arg2);
            Value *same_type = emit_exactly_isa(ctx, (justbits1 ? arg2 : arg1), typ);
            BasicBlock *currBB = ctx.builder.GetInsertBlock();
            BasicBlock *isaBB = BasicBlock::Create(ctx.builder.getContext(), "is", ctx.f);
            BasicBlock *postBB = BasicBlock::Create(ctx.builder.getContext(), "post_is", ctx.f);
            ctx.builder.CreateCondBr(same_type, isaBB, postBB);
            ctx.builder.SetInsertPoint(isaBB);
            Value *bitcmp = emit_bits_compare(ctx, jl_cgval_t(arg1, (jl_value_t*)typ, NULL),
                                              jl_cgval_t(arg2, (jl_value_t*)typ, NULL));
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
                            ArrayRef<jl_cgval_t> argv, size_t nargs, const jl_cgval_t *modifyop)
{
    bool issetglobal = f == jl_builtin_setglobal;
    bool isreplaceglobal = f == jl_builtin_replaceglobal;
    bool isswapglobal = f == jl_builtin_swapglobal;
    bool ismodifyglobal = f == jl_builtin_modifyglobal;
    bool issetglobalonce = f == jl_builtin_setglobalonce;
    const jl_cgval_t undefval;
    const jl_cgval_t &mod = argv[1];
    const jl_cgval_t &sym = argv[2];
    jl_cgval_t val = argv[isreplaceglobal || ismodifyglobal ? 4 : 3];
    const jl_cgval_t &cmp = isreplaceglobal || ismodifyglobal ? argv[3] : undefval;
    enum jl_memory_order order = jl_memory_order_release;
    const std::string fname = issetglobal ? "setglobal!" : isreplaceglobal ? "replaceglobal!" : isswapglobal ? "swapglobal!" : ismodifyglobal ? "modifyglobal!" : "setglobalonce!";
    if (nargs >= (isreplaceglobal || ismodifyglobal ? 5 : 4)) {
        const jl_cgval_t &ord = argv[isreplaceglobal || ismodifyglobal ? 5 : 4];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        order = jl_get_atomic_order((jl_sym_t*)ord.constant, !issetglobal, true);
    }
    enum jl_memory_order fail_order = order;
    if ((isreplaceglobal || issetglobalonce) && nargs == (isreplaceglobal ? 6 : 5)) {
        const jl_cgval_t &ord = argv[isreplaceglobal ? 6 : 5];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        fail_order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
    }
    if (order == jl_memory_order_invalid || fail_order == jl_memory_order_invalid || fail_order > order) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        *ret = jl_cgval_t(); // unreachable
        return true;
    }

    if (order == jl_memory_order_notatomic) {
        emit_atomic_error(ctx,
                issetglobal ? "setglobal!: module binding cannot be written non-atomically" :
                isreplaceglobal ? "replaceglobal!: module binding cannot be written non-atomically" :
                isswapglobal ? "swapglobal!: module binding cannot be written non-atomically" :
                ismodifyglobal ? "modifyglobal!: module binding cannot be written non-atomically" :
                "setglobalonce!: module binding cannot be written non-atomically");
        *ret = jl_cgval_t(); // unreachable
        return true;
    }
    else if (fail_order == jl_memory_order_notatomic) {
        emit_atomic_error(ctx,
                isreplaceglobal ? "replaceglobal!: module binding cannot be accessed non-atomically" :
                "setglobalonce!: module binding cannot be accessed non-atomically");
        *ret = jl_cgval_t(); // unreachable
        return true;
    }

    if (sym.constant && jl_is_symbol(sym.constant)) {
        if (mod.constant && jl_is_module(mod.constant)) {
            *ret = emit_globalop(ctx, (jl_module_t*)mod.constant, (jl_sym_t*)sym.constant, val, cmp,
                                 get_llvm_atomic_order(order), get_llvm_atomic_order(fail_order),
                                 issetglobal,
                                 isreplaceglobal,
                                 isswapglobal,
                                 ismodifyglobal,
                                 issetglobalonce,
                                 modifyop,
                                 false);
            return true;
        }
    }

    return false;
}

static bool emit_f_opfield(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                           ArrayRef<jl_cgval_t> argv, size_t nargs, const jl_cgval_t *modifyop)
{
    ++EmittedOpfields;
    bool issetfield = f == jl_builtin_setfield;
    bool isreplacefield = f == jl_builtin_replacefield;
    bool isswapfield = f == jl_builtin_swapfield;
    bool ismodifyfield = f == jl_builtin_modifyfield;
    bool issetfieldonce = f == jl_builtin_setfieldonce;
    const jl_cgval_t undefval;
    const jl_cgval_t &obj = argv[1];
    const jl_cgval_t &fld = argv[2];
    jl_cgval_t val = argv[isreplacefield || ismodifyfield ? 4 : 3];
    const jl_cgval_t &cmp = isreplacefield || ismodifyfield ? argv[3] : undefval;
    enum jl_memory_order order = jl_memory_order_notatomic;
    const std::string fname = issetfield ? "setfield!" : isreplacefield ? "replacefield!" : isswapfield ? "swapfield!" : ismodifyfield ? "modifyfield!" : "setfieldonce!";
    if (nargs >= (isreplacefield || ismodifyfield ? 5 : 4)) {
        const jl_cgval_t &ord = argv[isreplacefield || ismodifyfield ? 5 : 4];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        order = jl_get_atomic_order((jl_sym_t*)ord.constant, !issetfield, true);
    }
    enum jl_memory_order fail_order = order;
    if ((isreplacefield || issetfieldonce) && nargs == (isreplacefield ? 6 : 5)) {
        const jl_cgval_t &ord = argv[isreplacefield ? 6 : 5];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        fail_order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
    }
    if (order == jl_memory_order_invalid || fail_order == jl_memory_order_invalid || fail_order > order) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        *ret = jl_cgval_t(); // unreachable
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
            if (i > 0 && i <= (ssize_t)jl_datatype_nfields(uty))
                idx = i - 1;
        }
        if (idx != -1) {
            jl_value_t *ft = jl_field_type(uty, idx);
            if (!jl_has_free_typevars(ft)) {
                if (!ismodifyfield) {
                    emit_typecheck(ctx, val, ft, fname);
                    val = update_julia_type(ctx, val, ft);
                    if (val.typ == jl_bottom_type)
                        return true;
                }
                // TODO: attempt better codegen for approximate types
                bool isboxed = jl_field_isptr(uty, idx);
                bool isatomic = jl_field_isatomic(uty, idx);
                bool needlock = isatomic && !isboxed && jl_datatype_size(jl_field_type(uty, idx)) > MAX_ATOMIC_SIZE;
                *ret = jl_cgval_t();
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
                            ismodifyfield ?
                            (isatomic ? "modifyfield!: atomic field cannot be written non-atomically"
                                      : "modifyfield!: non-atomic field cannot be written atomically") :
                            (isatomic ? "setfieldonce!: atomic field cannot be written non-atomically"
                                      : "setfieldonce!: non-atomic field cannot be written atomically"));
                }
                else if (isatomic == (fail_order == jl_memory_order_notatomic)) {
                    emit_atomic_error(ctx,
                            isreplacefield ?
                            (isatomic ? "replacefield!: atomic field cannot be accessed non-atomically"
                                      : "replacefield!: non-atomic field cannot be accessed atomically") :
                            (isatomic ? "setfieldonce!: atomic field cannot be accessed non-atomically"
                                      : "setfieldonce!: non-atomic field cannot be accessed atomically"));
                }
                else if (!uty->name->mutabl) {
                    std::string msg = fname + ": immutable struct of type "
                        + std::string(jl_symbol_name(uty->name->name))
                        + " cannot be changed";
                    emit_error(ctx, msg);
                }
                else if (jl_field_isconst(uty, idx)) {
                    std::string msg = fname + ": const field ."
                        + std::string(jl_symbol_name((jl_sym_t*)jl_svecref(jl_field_names(uty), idx)))
                        + " of type "
                        + std::string(jl_symbol_name(uty->name->name))
                        + " cannot be changed";
                    emit_error(ctx, msg);
                }
                else {
                    assert(obj.isboxed);
                    *ret = emit_setfield(ctx, uty, obj, idx, val, cmp, true,
                            (needlock || order <= jl_memory_order_notatomic)
                                ? AtomicOrdering::NotAtomic
                                : get_llvm_atomic_order(order),
                            (needlock || fail_order <= jl_memory_order_notatomic)
                                ? AtomicOrdering::NotAtomic
                                : get_llvm_atomic_order(fail_order),
                            needlock ? boxed(ctx, obj) : nullptr,
                            issetfield, isreplacefield, isswapfield, ismodifyfield, issetfieldonce,
                            modifyop, fname);
                }
                return true;
            }
        }
    }
    return false;
}

static jl_cgval_t emit_isdefinedglobal(jl_codectx_t &ctx, jl_module_t *modu, jl_sym_t *name, int allow_import, enum jl_memory_order order)
{
    jl_binding_t *bnd = allow_import ? jl_get_binding(modu, name) : jl_get_module_binding(modu, name, 0);
    struct restriction_kind_pair rkp = { NULL, NULL, PARTITION_KIND_GUARD, 0 };
    if (allow_import && jl_get_binding_leaf_partitions_restriction_kind(bnd, &rkp, ctx.min_world, ctx.max_world)) {
        if (jl_bkind_is_some_constant(rkp.kind) && rkp.restriction)
            return mark_julia_const(ctx, jl_true);
        if (rkp.kind == PARTITION_KIND_GLOBAL) {
            Value *bp = julia_binding_gv(ctx, rkp.binding_if_global);
            bp = julia_binding_pvalue(ctx, bp);
            LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_binding);
            ai.decorateInst(v);
            v->setOrdering(get_llvm_atomic_order(order));
            Value *isnull = ctx.builder.CreateICmpNE(v, Constant::getNullValue(ctx.types().T_prjlvalue));
            return mark_julia_type(ctx, isnull, false, jl_bool_type);
        }
    }
    Value *isdef = ctx.builder.CreateCall(prepare_call(jlboundp_func), {
            literal_pointer_val(ctx, (jl_value_t*)modu),
            literal_pointer_val(ctx, (jl_value_t*)name),
            ConstantInt::get(getInt32Ty(ctx.builder.getContext()), allow_import)
        });
    isdef = ctx.builder.CreateTrunc(isdef, getInt1Ty(ctx.builder.getContext()));
    return mark_julia_type(ctx, isdef, false, jl_bool_type);
}

static bool emit_f_opmemory(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                            ArrayRef<jl_cgval_t> argv, size_t nargs, const jl_cgval_t *modifyop)
{
    bool issetmemory = f == jl_builtin_memoryrefset;
    bool isreplacememory = f == jl_builtin_memoryrefreplace;
    bool isswapmemory = f == jl_builtin_memoryrefswap;
    bool ismodifymemory = f == jl_builtin_memoryrefmodify;
    bool issetmemoryonce = f == jl_builtin_memoryrefsetonce;

    const jl_cgval_t undefval;
    const jl_cgval_t &ref = argv[1];
    jl_cgval_t val = argv[isreplacememory || ismodifymemory ? 3 : 2];
    jl_value_t *mty_dt = jl_unwrap_unionall(ref.typ);
    if (!jl_is_genericmemoryref_type(mty_dt) || !jl_is_concrete_type(mty_dt))
        return false;

    jl_value_t *kind = jl_tparam0(mty_dt);
    jl_value_t *ety = jl_tparam1(mty_dt);
    jl_value_t *addrspace = jl_tparam2(mty_dt); (void)addrspace; // TODO
    mty_dt = jl_field_type_concrete((jl_datatype_t*)mty_dt, 1);
    if (kind != (jl_value_t*)jl_not_atomic_sym && kind != (jl_value_t*)jl_atomic_sym)
        return false;

    const jl_cgval_t &cmp = isreplacememory || ismodifymemory ? argv[2] : undefval;
    enum jl_memory_order order = jl_memory_order_notatomic;
    const std::string fname = issetmemory ? "memoryrefset!" : isreplacememory ? "memoryrefreplace!" : isswapmemory ? "memoryrefswap!" : ismodifymemory ? "memoryrefmodify!" : "memoryrefsetonce!";
    {
        const jl_cgval_t &ord = argv[isreplacememory || ismodifymemory ? 4 : 3];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        order = jl_get_atomic_order((jl_sym_t*)ord.constant, !issetmemory, true);
    }
    enum jl_memory_order fail_order = order;
    if (isreplacememory || issetmemoryonce) {
        const jl_cgval_t &ord = argv[isreplacememory ? 5 : 4];
        emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
        if (!ord.constant)
            return false;
        fail_order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
    }
    if (order == jl_memory_order_invalid || fail_order == jl_memory_order_invalid || fail_order > order) {
        emit_atomic_error(ctx, "invalid atomic ordering");
        *ret = jl_cgval_t(); // unreachable
        return true;
    }

    jl_value_t *boundscheck = argv[nargs].constant;
    emit_typecheck(ctx, argv[nargs], (jl_value_t*)jl_bool_type, fname);
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
    bool isboxed = layout->flags.arrayelem_isboxed;
    bool isunion = layout->flags.arrayelem_isunion;
    bool isatomic = kind == (jl_value_t*)jl_atomic_sym;
    bool needlock = isatomic && layout->size > MAX_ATOMIC_SIZE;
    size_t elsz = layout->size;
    size_t al = layout->alignment;
    if (al > JL_HEAP_ALIGNMENT)
        al = JL_HEAP_ALIGNMENT;
    if (isatomic == (order == jl_memory_order_notatomic)) {
        emit_atomic_error(ctx,
                issetmemory ?
                (isatomic ? "memoryrefset!: atomic memory cannot be written non-atomically"
                          : "memoryrefset!: non-atomic memory cannot be written atomically") :
                isreplacememory ?
                (isatomic ? "memoryrefreplace!: atomic memory cannot be written non-atomically"
                          : "memoryrefreplace!: non-atomic memory cannot be written atomically") :
                isswapmemory ?
                (isatomic ? "memoryrefswap!: atomic memory cannot be written non-atomically"
                          : "memoryrefswap!: non-atomic memory cannot be written atomically") :
                ismodifymemory ?
                (isatomic ? "memoryrefmodify!: atomic memory cannot be written non-atomically"
                          : "memoryrefmodify!: non-atomic memory cannot be written atomically") :
                (isatomic ? "memoryrefsetonce!: atomic memory cannot be written non-atomically"
                          : "memoryrefsetonce!: non-atomic memory cannot be written atomically"));
        *ret = jl_cgval_t();
        return true;
    }
    else if (isatomic == (fail_order == jl_memory_order_notatomic)) {
        emit_atomic_error(ctx,
                isreplacememory ?
                (isatomic ? "memoryrefreplace!: atomic memory cannot be accessed non-atomically"
                          : "memoryrefreplace!: non-atomic memory cannot be accessed atomically") :
                (isatomic ? "memoryrefsetonce!: atomic memory cannot be accessed non-atomically"
                          : "memoryrefsetonce!: non-atomic memory cannot be accessed atomically"));
        *ret = jl_cgval_t();
        return true;
    }
    Value *mem = emit_memoryref_mem(ctx, ref, layout);
    Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
    if (bounds_check_enabled(ctx, boundscheck)) {
        BasicBlock *failBB, *endBB;
        failBB = BasicBlock::Create(ctx.builder.getContext(), "oob");
        endBB = BasicBlock::Create(ctx.builder.getContext(), "load");
        ctx.builder.CreateCondBr(ctx.builder.CreateIsNull(mlen), failBB, endBB);
        failBB->insertInto(ctx.f);
        ctx.builder.SetInsertPoint(failBB);
        ctx.builder.CreateCall(prepare_call(jlboundserror_func), { mark_callee_rooted(ctx, mem), ConstantInt::get(ctx.types().T_size, 1) });
        ctx.builder.CreateUnreachable();
        endBB->insertInto(ctx.f);
        ctx.builder.SetInsertPoint(endBB);
    }
    if (!ismodifymemory) {
        emit_typecheck(ctx, val, ety, fname);
        val = update_julia_type(ctx, val, ety);
        if (val.typ == jl_bottom_type)
            return true;
    }
    AtomicOrdering Order = (needlock || order <= jl_memory_order_notatomic)
                            ? AtomicOrdering::NotAtomic
                            : get_llvm_atomic_order(order);
    AtomicOrdering FailOrder = (needlock || fail_order <= jl_memory_order_notatomic)
                        ? AtomicOrdering::NotAtomic
                        : get_llvm_atomic_order(fail_order);
    if (isunion) {
        assert(!isatomic && !needlock);
        Value *V = emit_memoryref_FCA(ctx, ref, layout);
        Value *idx0 = CreateSimplifiedExtractValue(ctx, V, 0);
        Value *mem = CreateSimplifiedExtractValue(ctx, V, 1);
        Value *data = emit_genericmemoryptr(ctx, mem, layout, AddressSpace::Loaded);
        Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * al), (elsz + al - 1) / al);
        // compute tindex from val
        Value *ptindex;
        if (elsz == 0) {
            ptindex = data;
        }
        else {
            // isbits union selector bytes are stored after mem->length
            ptindex = ctx.builder.CreateInBoundsGEP(AT, data, mlen);
            data = ctx.builder.CreateInBoundsGEP(AT, data, idx0);
        }
        ptindex = emit_ptrgep(ctx, ptindex, idx0);
        *ret = union_store(ctx, data, ptindex, val, cmp, ety,
            ctx.tbaa().tbaa_arraybuf, ctx.tbaa().tbaa_arrayselbyte,
            Order, FailOrder,
            nullptr, issetmemory, isreplacememory, isswapmemory, ismodifymemory, issetmemoryonce,
            modifyop, fname);
    }
    else {
        Value *ptr = (layout->size == 0 ? nullptr : emit_memoryref_ptr(ctx, ref, layout));
        Value *lock = nullptr;
        bool maybenull = true;
        if (needlock) {
            assert(ptr);
            lock = ptr;
            // ptr += sizeof(lock);
            ptr = emit_ptrgep(ctx, ptr, LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT));
        }
        Value *data_owner = NULL; // owner object against which the write barrier must check
        if (isboxed || layout->first_ptr >= 0) { // if elements are just bits, don't need a write barrier
            data_owner = emit_memoryref_mem(ctx, ref, layout);
        }
        *ret = typed_store(ctx,
                    ptr,
                    val, cmp, ety,
                    isboxed ? ctx.tbaa().tbaa_ptrarraybuf : ctx.tbaa().tbaa_arraybuf,
                    ctx.noalias().aliasscope.current,
                    data_owner,
                    isboxed,
                    Order,
                    FailOrder,
                    al,
                    lock,
                    issetmemory,
                    isreplacememory,
                    isswapmemory,
                    ismodifymemory,
                    issetmemoryonce,
                    maybenull,
                    modifyop,
                    fname,
                    nullptr,
                    nullptr);
    }
    return true;
}

static jl_llvm_functions_t
    emit_function(
        orc::ThreadSafeModule &TSM,
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *abi,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params);

static void emit_hasnofield_error_ifnot(jl_codectx_t &ctx, Value *ok, jl_datatype_t *type, jl_cgval_t name);

static bool emit_builtin_call(jl_codectx_t &ctx, jl_cgval_t *ret, jl_value_t *f,
                              ArrayRef<jl_cgval_t> argv, size_t nargs, jl_value_t *rt,
                              jl_expr_t *ex, bool is_promotable)
// returns true if the call has been handled
{
    ++EmittedBuiltinCalls;
    if (f == jl_builtin_is && nargs == 2) {
        // emit comparison test
        Value *ans = emit_f_is(ctx, argv[1], argv[2]);
        *ret = mark_julia_type(ctx, ans, false, jl_bool_type);
        return true;
    }

    else if (f == jl_builtin_typeof && nargs == 1) {
        const jl_cgval_t &p = argv[1];
        if (p.constant)
            *ret = mark_julia_const(ctx, jl_typeof(p.constant));
        else if (jl_is_concrete_type(p.typ))
            *ret = mark_julia_const(ctx, p.typ);
        else
            *ret = mark_julia_type(ctx, emit_typeof(ctx, p, false, false), true, jl_datatype_type);
        return true;
    }

    else if (f == jl_builtin_typeassert && nargs == 2) {
        const jl_cgval_t &arg = argv[1];
        const jl_cgval_t &ty = argv[2];
        if (jl_is_type_type(ty.typ) && !jl_has_free_typevars(ty.typ)) {
            jl_value_t *tp0 = jl_tparam0(ty.typ);
            emit_typecheck(ctx, arg, tp0, "typeassert");
            *ret = update_julia_type(ctx, arg, tp0);
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
            Value *isa_result = emit_isa(ctx, arg, tp0, Twine()).first;
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
                Value *theArgs = emit_ptrgep(ctx, ctx.argArray, ctx.nReqArgs * sizeof(jl_value_t*));
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
            *ret = emit_new_struct(ctx, rt, nargs, argv.drop_front(), is_promotable);
            return true;
        }
    }

    else if (f == jl_builtin_throw && nargs == 1) {
        Value *arg1 = boxed(ctx, argv[1]);
        raise_exception(ctx, arg1);
        *ret = jl_cgval_t();
        return true;
    }

    else if (f == jl_builtin_memorynew && (nargs == 2)) {
        const jl_cgval_t &memty = argv[1];
        if (!memty.constant)
            return false;
        jl_datatype_t *typ = (jl_datatype_t*) memty.constant;
        if (!jl_is_concrete_type((jl_value_t*)typ) || !jl_is_genericmemory_type(typ))
            return false;
        jl_genericmemory_t *inst = (jl_genericmemory_t*)((jl_datatype_t*)typ)->instance;
        if (inst == NULL)
            return false;
        if (argv[2].constant) {
            if (!jl_is_long(argv[2].constant))
                return false;
            size_t nel = jl_unbox_long(argv[2].constant);
            if (nel < 0)
                return false;
            *ret = emit_const_len_memorynew(ctx, typ, nel, inst);
        }
        else {
            *ret = emit_memorynew(ctx, typ, argv[2], inst);
        }
        return true;
    }

    else if (f == jl_builtin_memoryref && nargs == 1) {
        const jl_cgval_t &mem = argv[1];
        jl_datatype_t *mty_dt = (jl_datatype_t*)jl_unwrap_unionall(mem.typ);
        if (jl_is_genericmemory_type(mty_dt) && jl_is_concrete_type((jl_value_t*)mty_dt)) {
            jl_value_t *typ = jl_apply_type((jl_value_t*)jl_genericmemoryref_type, jl_svec_data(mty_dt->parameters), jl_svec_len(mty_dt->parameters));
            const jl_datatype_layout_t *layout = mty_dt->layout;
            *ret = _emit_memoryref(ctx, mem, layout, typ);
            return true;
        }
    }

    else if (f == jl_builtin_memoryref && (nargs == 2 || nargs == 3)) {
        const jl_cgval_t &ref = argv[1];
        jl_value_t *mty_dt = jl_unwrap_unionall(ref.typ);
        if (jl_is_genericmemoryref_type(mty_dt) && jl_is_concrete_type(mty_dt)) {
            mty_dt = jl_field_type_concrete((jl_datatype_t*)mty_dt, 1);
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
            jl_value_t *boundscheck = nargs == 3 ? argv[3].constant : nullptr;
            if (nargs == 3)
                emit_typecheck(ctx, argv[3], (jl_value_t*)jl_bool_type, "memoryref");
            *ret = emit_memoryref(ctx, ref, argv[2], boundscheck, layout);
            return true;
        }
    }

    else if (f == jl_builtin_memoryrefoffset && nargs == 1) {
        const jl_cgval_t &ref = argv[1];
        jl_value_t *mty_dt = jl_unwrap_unionall(ref.typ);
        if (jl_is_genericmemoryref_type(mty_dt) && jl_is_concrete_type(mty_dt)) {
            mty_dt = jl_field_type_concrete((jl_datatype_t*)mty_dt, 1);
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
            *ret = emit_memoryref_offset(ctx, ref, layout);
            return true;
        }
    }

    else if (f == jl_builtin_memoryrefget && nargs == 3) {
        const jl_cgval_t &ref = argv[1];
        jl_value_t *mty_dt = jl_unwrap_unionall(ref.typ);
        if (jl_is_genericmemoryref_type(mty_dt) && jl_is_concrete_type(mty_dt)) {
            jl_value_t *kind = jl_tparam0(mty_dt);
            jl_value_t *ety = jl_tparam1(mty_dt);
            jl_value_t *addrspace = jl_tparam2(mty_dt); (void)addrspace; // TODO
            mty_dt = jl_field_type_concrete((jl_datatype_t*)mty_dt, 1);
            if (kind != (jl_value_t*)jl_not_atomic_sym && kind != (jl_value_t*)jl_atomic_sym)
                return false;
            enum jl_memory_order order = jl_memory_order_unspecified;
            const std::string fname = "memoryrefget";
            {
                const jl_cgval_t &ord = argv[2];
                emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
                if (!ord.constant)
                    return false;
                order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
            }
            if (order == jl_memory_order_invalid) {
                emit_atomic_error(ctx, "invalid atomic ordering");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            bool isatomic = kind == (jl_value_t*)jl_atomic_sym;
            if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
                emit_atomic_error(ctx, "memoryrefget: non-atomic memory cannot be accessed atomically");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            if (isatomic && order == jl_memory_order_notatomic) {
                emit_atomic_error(ctx, "memoryrefget: atomic memory cannot be accessed non-atomically");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            if (order == jl_memory_order_unspecified) {
                order = isatomic ? jl_memory_order_unordered : jl_memory_order_notatomic;
            }
            jl_value_t *boundscheck = argv[3].constant;
            emit_typecheck(ctx, argv[3], (jl_value_t*)jl_bool_type, "memoryref");
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
            Value *mem = emit_memoryref_mem(ctx, ref, layout);
            Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
            if (bounds_check_enabled(ctx, boundscheck)) {
                BasicBlock *failBB, *endBB;
                failBB = BasicBlock::Create(ctx.builder.getContext(), "oob");
                endBB = BasicBlock::Create(ctx.builder.getContext(), "load");
                ctx.builder.CreateCondBr(ctx.builder.CreateIsNull(mlen), failBB, endBB);
                failBB->insertInto(ctx.f);
                ctx.builder.SetInsertPoint(failBB);
                ctx.builder.CreateCall(prepare_call(jlboundserror_func), { mark_callee_rooted(ctx, mem), ConstantInt::get(ctx.types().T_size, 1) });
                ctx.builder.CreateUnreachable();
                endBB->insertInto(ctx.f);
                ctx.builder.SetInsertPoint(endBB);
            }
            bool isboxed = layout->flags.arrayelem_isboxed;
            bool isunion = layout->flags.arrayelem_isunion;
            size_t elsz = layout->size;
            size_t al = layout->alignment;
            if (al > JL_HEAP_ALIGNMENT)
                al = JL_HEAP_ALIGNMENT;
            bool needlock = isatomic && !isboxed && elsz > MAX_ATOMIC_SIZE;
            AtomicOrdering Order = (needlock || order <= jl_memory_order_notatomic)
                                    ? (isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic)
                                    : get_llvm_atomic_order(order);
            bool maybenull = true;
            if (!isboxed && !isunion && elsz == 0) {
                assert(jl_is_datatype(ety) && jl_is_datatype_singleton((jl_datatype_t*)ety));
                *ret = ghostValue(ctx, ety);
                if (isStrongerThanMonotonic(Order))
                    ctx.builder.CreateFence(Order);
            }
            else if (isunion) {
                assert(!isatomic && !needlock);
                Value *V = emit_memoryref_FCA(ctx, ref, layout);
                Value *idx0 = CreateSimplifiedExtractValue(ctx, V, 0);
                Value *mem = CreateSimplifiedExtractValue(ctx, V, 1);
                Value *data = emit_genericmemoryptr(ctx, mem, layout, AddressSpace::Loaded);
                Value *ptindex;
                if (elsz == 0) {
                    ptindex = data;
                }
                else {
                    Type *AT = ArrayType::get(IntegerType::get(ctx.builder.getContext(), 8 * al), (elsz + al - 1) / al);
                    // isbits union selector bytes are stored after mem->length bytes
                    ptindex = ctx.builder.CreateInBoundsGEP(AT, data, mlen);
                    data = ctx.builder.CreateInBoundsGEP(AT, data, idx0);
                }
                ptindex = emit_ptrgep(ctx, ptindex, idx0);
                size_t elsz_c = 0, al_c = 0;
                int union_max = jl_islayout_inline(ety, &elsz_c, &al_c);
                assert(union_max && LLT_ALIGN(elsz_c, al_c) == elsz && al_c == al);
                *ret = emit_unionload(ctx, data, ptindex, ety, elsz_c, al, ctx.tbaa().tbaa_arraybuf, true, union_max, ctx.tbaa().tbaa_arrayselbyte);
            }
            else {
                Value *ptr = (layout->size == 0 ? nullptr : emit_memoryref_ptr(ctx, ref, layout));
                Value *lock = nullptr;
                if (needlock) {
                    assert(ptr);
                    lock = ptr;
                    // ptr += sizeof(lock);
                    ptr = emit_ptrgep(ctx, ptr, LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT));
                    emit_lockstate_value(ctx, lock, true);
                }
                *ret = typed_load(ctx, ptr, nullptr, ety,
                        isboxed ? ctx.tbaa().tbaa_ptrarraybuf : ctx.tbaa().tbaa_arraybuf,
                        ctx.noalias().aliasscope.current,
                        isboxed, Order, maybenull, al);
                if (needlock) {
                    emit_lockstate_value(ctx, lock, false);
                }
            }
            return true;
        }
    }

    else if ((f == jl_builtin_memoryrefset && nargs == 4) ||
             (f == jl_builtin_memoryrefswap && nargs == 4) ||
             (f == jl_builtin_memoryrefreplace && nargs == 6) ||
             (f == jl_builtin_memoryrefmodify && nargs == 5) ||
             (f == jl_builtin_memoryrefsetonce && nargs == 5)) {
        return emit_f_opmemory(ctx, ret, f, argv, nargs, nullptr);
    }


    else if (f == jl_builtin_memoryref_isassigned && nargs == 3) {
        const jl_cgval_t &ref = argv[1];
        jl_value_t *mty_dt = jl_unwrap_unionall(ref.typ);
        if (jl_is_genericmemoryref_type(mty_dt) && jl_is_concrete_type(mty_dt)) {
            jl_value_t *kind = jl_tparam0(mty_dt);
            mty_dt = jl_field_type_concrete((jl_datatype_t*)mty_dt, 1);
            if (kind != (jl_value_t*)jl_not_atomic_sym && kind != (jl_value_t*)jl_atomic_sym)
                return false;
            enum jl_memory_order order = jl_memory_order_unspecified;
            const std::string fname = "memoryref_isassigned";
            {
                const jl_cgval_t &ord = argv[2];
                emit_typecheck(ctx, ord, (jl_value_t*)jl_symbol_type, fname);
                if (!ord.constant)
                    return false;
                order = jl_get_atomic_order((jl_sym_t*)ord.constant, true, false);
            }
            if (order == jl_memory_order_invalid) {
                emit_atomic_error(ctx, "invalid atomic ordering");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            bool isatomic = kind == (jl_value_t*)jl_atomic_sym;
            if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
                emit_atomic_error(ctx, "memoryref_isassigned: non-atomic memory cannot be accessed atomically");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            if (isatomic && order == jl_memory_order_notatomic) {
                emit_atomic_error(ctx, "memoryref_isassigned: atomic memory cannot be accessed non-atomically");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            if (order == jl_memory_order_unspecified) {
                order = isatomic ? jl_memory_order_unordered : jl_memory_order_notatomic;
            }
            jl_value_t *boundscheck = argv[3].constant;
            emit_typecheck(ctx, argv[3], (jl_value_t*)jl_bool_type, fname);
            const jl_datatype_layout_t *layout = ((jl_datatype_t*)mty_dt)->layout;
            Value *mem = emit_memoryref_mem(ctx, ref, layout);
            Value *mlen = emit_genericmemorylen(ctx, mem, ref.typ);
            Value *oob = bounds_check_enabled(ctx, boundscheck) ? ctx.builder.CreateIsNull(mlen) : nullptr;
            bool isboxed = layout->flags.arrayelem_isboxed;
            if (isboxed || layout->first_ptr >= 0) {
                bool needlock = isatomic && !isboxed && layout->size > MAX_ATOMIC_SIZE;
                AtomicOrdering Order = (needlock || order <= jl_memory_order_notatomic)
                                        ? (isboxed ? AtomicOrdering::Unordered : AtomicOrdering::NotAtomic)
                                        : get_llvm_atomic_order(order);
                PHINode *result = nullptr;
                if (oob) {
                    BasicBlock *passBB, *endBB, *fromBB;
                    passBB = BasicBlock::Create(ctx.builder.getContext(), "load");
                    endBB = BasicBlock::Create(ctx.builder.getContext(), "oob");

                    passBB->insertInto(ctx.f);
                    endBB->insertInto(ctx.f);
                    fromBB = ctx.builder.CreateCondBr(oob, endBB, passBB)->getParent();
                    ctx.builder.SetInsertPoint(endBB);
                    result = ctx.builder.CreatePHI(getInt1Ty(ctx.builder.getContext()), 2);
                    result->addIncoming(ConstantInt::get(result->getType(), 0), fromBB);
                    setName(ctx.emission_context, result, "arraysize");
                    ctx.builder.SetInsertPoint(passBB);
                }
                Value *elem = emit_memoryref_ptr(ctx, ref, layout);
                if (needlock) {
                    // n.b. no actual lock acquire needed, as the check itself only needs to load a single pointer and check for null
                    // elem += sizeof(lock);
                    elem = emit_ptrgep(ctx, elem, LLT_ALIGN(sizeof(jl_mutex_t), JL_SMALL_BYTE_ALIGNMENT));
                }
                if (!isboxed)
                    elem = emit_ptrgep(ctx, elem, layout->first_ptr * sizeof(void*));
                // emit this using the same type as jl_builtin_memoryrefget
                // so that LLVM may be able to load-load forward them and fold the result
                auto tbaa = isboxed ? ctx.tbaa().tbaa_ptrarraybuf : ctx.tbaa().tbaa_arraybuf;
                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
                LoadInst *fldv = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, elem, ctx.types().alignof_ptr);
                fldv->setOrdering(Order);
                ai.decorateInst(fldv);
                Value *isdef = ctx.builder.CreateIsNotNull(fldv);
                setName(ctx.emission_context, isdef, fname);
                if (oob) {
                    assert(result);
                    result->addIncoming(isdef, ctx.builder.CreateBr(result->getParent())->getParent());
                    ctx.builder.SetInsertPoint(result->getParent());
                    isdef = result;
                }
                *ret = mark_julia_type(ctx, isdef, false, jl_bool_type);
            }
            else if (oob) {
                Value *isdef = ctx.builder.CreateNot(oob);
                *ret = mark_julia_type(ctx, isdef, false, jl_bool_type);
            }
            else {
                *ret = mark_julia_const(ctx, jl_true);
            }
            return true;
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
            *ret = jl_cgval_t(); // unreachable
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
                        jl_cgval_t va_ary( // fake instantiation of a cgval, in order to call emit_bounds_check (it only checks the `.V` field)
                                emit_ptrgep(ctx, ctx.argArray, ctx.nReqArgs * sizeof(jl_value_t*)),
                                NULL, NULL);
                        Value *idx = emit_unbox(ctx, ctx.types().T_size, fld, (jl_value_t*)jl_long_type);
                        idx = emit_bounds_check(ctx, va_ary, NULL, idx, valen, boundscheck);
                        idx = ctx.builder.CreateAdd(idx, ConstantInt::get(ctx.types().T_size, ctx.nReqArgs));
                        Instruction *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, ctx.argArray, idx), Align(sizeof(void*)));
                        setName(ctx.emission_context, v, "getfield");
                        // if we know the result type of this load, we will mark that information here too
                        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_value);
                        ai.decorateInst(maybe_mark_load_dereferenceable(v, false, rt));
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
                        Value *vidx = emit_unbox(ctx, ctx.types().T_size, fld, (jl_value_t*)jl_long_type);
                        if (emit_getfield_unknownidx(ctx, ret, obj, vidx, utt, boundscheck, order)) {
                            return true;
                        }
                    }
                }
                Value *vidx = emit_unbox(ctx, ctx.types().T_size, fld, (jl_value_t*)jl_long_type);
                if (jl_is_tuple_type(utt) && is_tupletype_homogeneous(utt->parameters, true)) {
                    // For tuples, we can emit code even if we don't know the exact
                    // type (e.g. because we don't know the length). This is possible
                    // as long as we know that all elements are of the same (leaf) type.
                    jl_cgval_t ptrobj = obj.isboxed ? obj : value_to_pointer(ctx, obj);
                    if (order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
                        emit_atomic_error(ctx, "getfield: non-atomic field cannot be accessed atomically");
                        *ret = jl_cgval_t(); // unreachable
                        return true;
                    }
                    // Determine which was the type that was homogeneous
                    jl_value_t *jt = jl_tparam0(utt);
                    if (jl_is_vararg(jt))
                        jt = jl_unwrap_vararg(jt);
                    assert(jl_is_datatype(jt));
                    // This is not necessary for correctness, but allows to omit
                    // the extra code for getting the length of the tuple
                    if (!bounds_check_enabled(ctx, boundscheck)) {
                        vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(ctx.types().T_size, 1));
                    }
                    else {
                        vidx = emit_bounds_check(ctx, ptrobj, (jl_value_t*)ptrobj.typ, vidx,
                            emit_datatype_nfields(ctx, emit_typeof(ctx, ptrobj, false, false)),
                            jl_true);
                    }
                    bool isboxed = !jl_datatype_isinlinealloc((jl_datatype_t*)jt, 0);
                    Value *ptr = data_pointer(ctx, ptrobj);
                    *ret = typed_load(ctx, ptr, vidx,
                            isboxed ? (jl_value_t*)jl_any_type : jt,
                            ptrobj.tbaa, nullptr, isboxed, AtomicOrdering::NotAtomic, false);
                    return true;
                }

                // Unknown object, but field known to be integer
                vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(ctx.types().T_size, 1));
                Value *fld_val = ctx.builder.CreateCall(prepare_call(jlgetnthfieldchecked_func), { boxed(ctx, obj), vidx }, "getfield");
                *ret = mark_julia_type(ctx, fld_val, true, jl_any_type);
                return true;
            }
        }
        else if (fld.typ == (jl_value_t*)jl_symbol_type) { // Known type but unknown symbol
            if (jl_is_datatype(utt) && (utt != jl_module_type) && jl_struct_try_layout(utt)) {
                if ((jl_datatype_nfields(utt) == 1 && !jl_is_namedtuple_type(utt) && !jl_is_tuple_type(utt))) {
                    jl_svec_t *fn = jl_field_names(utt);
                    assert(jl_svec_len(fn) == 1);
                    Value *typ_sym = literal_pointer_val(ctx, jl_svecref(fn, 0));
                    Value *cond = ctx.builder.CreateICmpEQ(mark_callee_rooted(ctx, typ_sym), mark_callee_rooted(ctx, boxed(ctx, fld)));
                    emit_hasnofield_error_ifnot(ctx, cond, utt, fld);
                    *ret = emit_getfield_knownidx(ctx, obj, 0, utt, order);
                    return true;
                }
                else {
                    Value *index = ctx.builder.CreateCall(prepare_call(jlfieldindex_func),
                            {emit_typeof(ctx, obj, false, false), boxed(ctx, fld), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0)});
                    Value *cond = ctx.builder.CreateICmpNE(index, ConstantInt::get(getInt32Ty(ctx.builder.getContext()), -1));
                    emit_hasnofield_error_ifnot(ctx, cond, utt, fld);
                    Value *idx2 = ctx.builder.CreateAdd(ctx.builder.CreateIntCast(index, ctx.types().T_size, false), ConstantInt::get(ctx.types().T_size, 1)); // getfield_unknown is 1 based
                    if (emit_getfield_unknownidx(ctx, ret, obj, idx2, utt, jl_false, order))
                        return true;
                }
            }
        }
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
            *ret = jl_cgval_t(); // unreachable
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

    else if ((f == jl_builtin_setglobal && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_swapglobal && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_replaceglobal && (nargs == 4 || nargs == 5 || nargs == 6)) ||
             (f == jl_builtin_modifyglobal && (nargs == 4 || nargs == 5)) ||
             (f == jl_builtin_setglobalonce && (nargs == 3 || nargs == 4 || nargs == 5))) {
        return emit_f_opglobal(ctx, ret, f, argv, nargs, nullptr);
    }

    else if ((f == jl_builtin_setfield && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_swapfield && (nargs == 3 || nargs == 4)) ||
             (f == jl_builtin_replacefield && (nargs == 4 || nargs == 5 || nargs == 6)) ||
             (f == jl_builtin_modifyfield && (nargs == 4 || nargs == 5)) ||
             (f == jl_builtin_setfieldonce && (nargs == 3 || nargs == 4 || nargs == 5))) {
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
            sz = ConstantInt::get(ctx.types().T_size, nf);
        else
            sz = emit_datatype_nfields(ctx, emit_typeof(ctx, obj, false, false));
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
                Value *idx = emit_unbox(ctx, ctx.types().T_size, fld, (jl_value_t*)jl_long_type);
                jl_value_t *boundscheck = (nargs == 3 ? argv[3].constant : jl_true);
                if (nargs == 3)
                    emit_typecheck(ctx, argv[3], (jl_value_t*)jl_bool_type, "fieldtype");
                emit_bounds_check(ctx, typ, (jl_value_t*)jl_datatype_type, idx, types_len, boundscheck);
                Value *fieldtyp_p = ctx.builder.CreateInBoundsGEP(ctx.types().T_prjlvalue, decay_derived(ctx, types_svec), idx);
                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
                Value *fieldtyp = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, fieldtyp_p, Align(sizeof(void*))));
                setName(ctx.emission_context, fieldtyp, "fieldtype");
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
                *ret = mark_julia_type(ctx, ConstantInt::get(ctx.types().T_size, sz), false, jl_long_type);
                return true;
            }
            // String and SimpleVector's length fields have the same layout
            auto ptr = boxed(ctx, obj);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
            Value *len = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_size, ptr, ctx.types().alignof_ptr));
            MDBuilder MDB(ctx.builder.getContext());
            if (sty == jl_simplevector_type) {
                auto rng = MDB.createRange(
                    Constant::getNullValue(ctx.types().T_size), ConstantInt::get(ctx.types().T_size, INTPTR_MAX / sizeof(void*) - 1));
                cast<LoadInst>(len)->setMetadata(LLVMContext::MD_range, rng);
                len = ctx.builder.CreateMul(len, ConstantInt::get(ctx.types().T_size, sizeof(void*)));
                len = ctx.builder.CreateAdd(len, ConstantInt::get(ctx.types().T_size, sizeof(void*)));
            }
            else {
                auto rng = MDB.createRange(Constant::getNullValue(ctx.types().T_size), ConstantInt::get(ctx.types().T_size, INTPTR_MAX));
                cast<LoadInst>(len)->setMetadata(LLVMContext::MD_range, rng);
            }
            setName(ctx.emission_context, len, "sizeof");
            *ret = mark_julia_type(ctx, len, false, jl_long_type);
            return true;
        }
        else if (jl_is_genericmemory_type(sty)) {
            Value *v = boxed(ctx, obj);
            auto len = emit_genericmemorylen(ctx, v, (jl_value_t*)sty);
            auto elsize = emit_genericmemoryelsize(ctx, v, obj.typ, true);
            *ret = mark_julia_type(ctx, ctx.builder.CreateMul(len, elsize), false, jl_long_type);
            return true;
        }
    }

    else if (f == jl_builtin_apply_type && nargs > 0) {
        if (jl_is_method(ctx.linfo->def.method)) {
            // don't bother codegen constant-folding for toplevel.
            jl_value_t *ty = static_apply_type(ctx, argv, nargs + 1);
            if (ty != NULL) {
                JL_GC_PUSH1(&ty);
                jl_temporary_root(ctx, ty);
                JL_GC_POP();
                *ret = mark_julia_const(ctx, ty);
                return true;
            }
        }
    }

    else if (f == jl_builtin_isdefinedglobal && (nargs == 2 || nargs == 3 || nargs == 4)) {
        const jl_cgval_t &mod = argv[1];
        const jl_cgval_t &sym = argv[2];
        bool allow_import = true;
        enum jl_memory_order order = jl_memory_order_unspecified;

        if (nargs >= 3) {
            const jl_cgval_t &arg3 = argv[3];
            if (arg3.constant && jl_is_bool(arg3.constant))
                allow_import = jl_unbox_bool(arg3.constant);
            else
                return false;
        }

        if (nargs == 4) {
            const jl_cgval_t &arg4 = argv[4];
            if (arg4.constant && jl_is_symbol(arg4.constant))
                order = jl_get_atomic_order((jl_sym_t*)arg4.constant, true, false);
            else
                return false;
        }
        else
            order = jl_memory_order_unordered;

        if (order < jl_memory_order_unordered) {
            return false;
        }

        if (!mod.constant || !sym.constant || !jl_is_symbol(sym.constant) || !jl_is_module(mod.constant)) {
            return false;
        }

        *ret = emit_isdefinedglobal(ctx, (jl_module_t*)mod.constant, (jl_sym_t*)sym.constant, allow_import, order);
        return true;
    }

    else if (f == jl_builtin_isdefined && (nargs == 2 || nargs == 3)) {
        const jl_cgval_t &obj = argv[1];
        const jl_cgval_t &fld = argv[2];
        jl_datatype_t *stt = (jl_datatype_t*)obj.typ;
        ssize_t fieldidx = -1;
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
            goto isdefined_unknown_idx;
        }
        assert(jl_is_datatype(stt));

        if (fld.constant && jl_is_symbol(fld.constant)) {
            jl_sym_t *sym = (jl_sym_t*)fld.constant;
            fieldidx = jl_field_index(stt, sym, 0);
        }
        else if (fld.constant && fld.typ == (jl_value_t*)jl_long_type) {
            fieldidx = jl_unbox_long(fld.constant) - 1;
        }
        else {
isdefined_unknown_idx:
            if (nargs == 3 || fld.typ != (jl_value_t*)jl_long_type)
                return false;
            Value *vidx = emit_unbox(ctx, ctx.types().T_size, fld, (jl_value_t*)jl_long_type);
            vidx = ctx.builder.CreateSub(vidx, ConstantInt::get(ctx.types().T_size, 1));
            Value *isd = ctx.builder.CreateCall(prepare_call(jlfieldisdefinedchecked_func), { boxed(ctx, obj), vidx });
            isd = ctx.builder.CreateTrunc(isd, getInt8Ty(ctx.builder.getContext()));
            *ret = mark_julia_type(ctx, isd, false, jl_bool_type);
            return true;
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
            *ret = jl_cgval_t(); // unreachable
            return true;
        }
        ssize_t nf = jl_datatype_nfields(stt);
        if (fieldidx < 0 || fieldidx >= nf) {
            if (order != jl_memory_order_unspecified) {
                emit_atomic_error(ctx, "isdefined: atomic ordering cannot be specified for nonexistent field");
                *ret = jl_cgval_t(); // unreachable
                return true;
            }
            *ret = mark_julia_const(ctx, jl_false);
            return true;
        }
        bool isatomic = jl_field_isatomic(stt, fieldidx);
        if (!isatomic && order != jl_memory_order_notatomic && order != jl_memory_order_unspecified) {
            emit_atomic_error(ctx, "isdefined: non-atomic field cannot be accessed atomically");
            *ret = jl_cgval_t(); // unreachable
            return true;
        }
        if (isatomic && order == jl_memory_order_notatomic) {
            emit_atomic_error(ctx, "isdefined: atomic field cannot be accessed non-atomically");
            *ret = jl_cgval_t(); // unreachable
            return true;
        }
        else if (!field_may_be_null(obj, stt, fieldidx)) {
            *ret = mark_julia_const(ctx, jl_true);
        }
        else if (jl_field_isptr(stt, fieldidx) || jl_type_hasptr(jl_field_type(stt, fieldidx))) {
            Value *fldv;
            size_t offs = jl_field_offset(stt, fieldidx) / sizeof(jl_value_t*);
            if (!obj.inline_roots.empty()) {
                auto offsets = split_value_field(stt, fieldidx);
                assert(offsets.second >= 0);
                fldv = obj.inline_roots[offsets.second];
            }
            else if (obj.ispointer()) {
                auto tbaa = best_field_tbaa(ctx, obj, stt, fieldidx, offs);
                if (!jl_field_isptr(stt, fieldidx))
                    offs += ((jl_datatype_t*)jl_field_type(stt, fieldidx))->layout->first_ptr;
                Value *ptr = data_pointer(ctx, obj);
                Value *addr = emit_ptrgep(ctx, ptr, offs * sizeof(jl_value_t*));
                // emit this using the same type as emit_getfield_knownidx
                // so that LLVM may be able to load-load forward them and fold the result
                jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
                fldv = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, addr, ctx.types().alignof_ptr));
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
            setName(ctx.emission_context, isdef, "isdefined");
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

    else if (f == jl_builtin_current_scope && (nargs == 0)) {
        jl_aliasinfo_t scope_ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
        Instruction *v = scope_ai.decorateInst(
            ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, get_scope_field(ctx), ctx.types().alignof_ptr));
        *ret = mark_julia_type(ctx, v, /*boxed*/ true, rt);
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

    else if (f == jl_builtin_compilerbarrier && (nargs == 2)) {
        emit_typecheck(ctx, argv[1], (jl_value_t*)jl_symbol_type, "compilerbarrier");
        *ret = argv[2];
        return true;
    }

    return false;
}

// Returns ctx.types().T_prjlvalue
static CallInst *emit_jlcall(jl_codectx_t &ctx, Value *theFptr, Value *theF,
                             ArrayRef<jl_cgval_t> argv, size_t nargs, JuliaFunction<> *trampoline)
{
    ++EmittedJLCalls;
    Function *TheTrampoline = prepare_call(trampoline);
    // emit arguments
    SmallVector<Value*, 4> theArgs;
    theArgs.push_back(theFptr);
    if (theF)
        theArgs.push_back(theF);
    for (size_t i = 0; i < nargs; i++) {
        Value *arg;
        if (i == 0 && trampoline == julia_call3) {
            const jl_cgval_t &f = argv[i];
            arg = f.inline_roots.empty() && f.ispointer() ? data_pointer(ctx, f) : value_to_pointer(ctx, f).V;
            arg = decay_derived(ctx, arg);
        }
        else {
            arg = boxed(ctx, argv[i]);
        }
        theArgs.push_back(arg);
    }
    CallInst *result = ctx.builder.CreateCall(TheTrampoline, theArgs);
    result->setAttributes(TheTrampoline->getAttributes());
    // TODO: we could add readonly attributes in many cases to the args
    return result;
}

// Returns ctx.types().T_prjlvalue
static CallInst *emit_jlcall(jl_codectx_t &ctx, JuliaFunction<> *theFptr, Value *theF,
                             ArrayRef<jl_cgval_t> argv, size_t nargs, JuliaFunction<> *trampoline)
{
    return emit_jlcall(ctx, prepare_call(theFptr), theF, argv, nargs, trampoline);
}

static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, bool is_opaque_closure, jl_value_t *specTypes, jl_value_t *jlretty, jl_returninfo_t &returninfo, ArrayRef<jl_cgval_t> argv, size_t nargs)
{
    ++EmittedSpecfunCalls;
    // emit specialized call site
    bool gcstack_arg = JL_FEAT_TEST(ctx, gcstack_arg);
    size_t nfargs = returninfo.decl.getFunctionType()->getNumParams();
    SmallVector<Value *, 0> argvals(nfargs);
    unsigned idx = 0;
    AllocaInst *result = nullptr;

    if (returninfo.cc == jl_returninfo_t::SRet || returninfo.cc == jl_returninfo_t::Union) {
        result = emit_static_alloca(ctx, returninfo.union_bytes, Align(returninfo.union_align));
        setName(ctx.emission_context, result, "sret_box");
        argvals[idx] = result;
        idx++;
    }

    AllocaInst *return_roots = nullptr;
    if (returninfo.return_roots) {
        assert(returninfo.cc == jl_returninfo_t::SRet);
        return_roots = emit_static_roots(ctx, returninfo.return_roots);
        argvals[idx] = return_roots;
        idx++;
    }
    if (gcstack_arg) {
        argvals[idx] = ctx.pgcstack;
        idx++;
    }
    for (size_t i = 0; i < nargs; i++) {
        // n.b.: specTypes is required to be a datatype by construction for specsig
        if (is_opaque_closure && i == 0) {
            // Special implementation for opaque closures: their jt and thus
            // julia_type_to_llvm values are likely wrong (based on captures instead of the OC), so override the
            // behavior here to directly pass the expected pointer directly instead of
            // computing it from the available information
            // jl_value_t *oc_type = (jl_value_t*)jl_any_type; // more accurately: get_oc_type(specTypes, jlretty)
            jl_cgval_t arg = argv[i];
            if (arg.isghost) {
                argvals[idx] = Constant::getNullValue(ctx.builder.getPtrTy(AddressSpace::Derived));
            }
            else {
                if (!arg.isboxed)
                    arg = value_to_pointer(ctx, arg);
                argvals[idx] = decay_derived(ctx, data_pointer(ctx, arg));
            }
            idx++;
            continue;
        }
        jl_value_t *jt = jl_nth_slot_type(specTypes, i);
        jl_cgval_t arg = update_julia_type(ctx, argv[i], jt);
        if (arg.typ == jl_bottom_type)
            return jl_cgval_t();
        if (is_uniquerep_Type(jt)) {
            continue;
        }
        else {
            bool isboxed = deserves_argbox(jt);
            Type *et = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, jt);
            if (type_is_ghost(et))
                continue;
            assert(idx < nfargs);
            if (isboxed) {
                argvals[idx] = boxed(ctx, arg);
            }
            else if (et->isAggregateType()) {
                auto tracked = CountTrackedPointers(et);
                if (tracked.count && !tracked.all) {
                    Value *val = arg.V;
                    SmallVector<Value*,0> roots(arg.inline_roots);
                    if (roots.empty())
                        std::tie(val, roots) = split_value(ctx, arg, Align(julia_alignment(jt)));
                    AllocaInst *proots = emit_static_roots(ctx, roots.size());
                    for (size_t i = 0; i < roots.size(); i++)
                        ctx.builder.CreateAlignedStore(roots[i], emit_ptrgep(ctx, proots, i * sizeof(void*)), Align(sizeof(void*)));
                    assert(val);
                    argvals[idx++] = decay_derived(ctx, val);
                    argvals[idx] = proots;
                }
                else {
                    if (!arg.isboxed)
                        arg = value_to_pointer(ctx, arg);
                    argvals[idx] = decay_derived(ctx, data_pointer(ctx, arg));
                }
            }
            else {
                Value *val = emit_unbox(ctx, et, arg, jt);
                if (!val) {
                    // There was a type mismatch of some sort - exit early
                    CreateTrap(ctx.builder);
                    return jl_cgval_t();
                }
                argvals[idx] = val;
            }
        }
        idx++;
    }
    assert(idx == nfargs);
    CallInst *call = ctx.builder.CreateCall(returninfo.decl, argvals);
    call->setAttributes(returninfo.attrs);
    if (gcstack_arg && ctx.emission_context.use_swiftcc)
        call->setCallingConv(CallingConv::Swift);

    jl_cgval_t retval;
    switch (returninfo.cc) {
        case jl_returninfo_t::Boxed:
            retval = mark_julia_type(ctx, call, true, jlretty);
            break;
        case jl_returninfo_t::Register:
            retval = mark_julia_type(ctx, call, false, jlretty);
            break;
        case jl_returninfo_t::SRet:
            assert(result);
            retval = mark_julia_slot(result, jlretty, NULL, ctx.tbaa().tbaa_gcframe, load_gc_roots(ctx, return_roots, returninfo.return_roots));
            break;
        case jl_returninfo_t::Union: {
            Value *box = ctx.builder.CreateExtractValue(call, 0);
            Value *tindex = ctx.builder.CreateExtractValue(call, 1);
            Value *derived = ctx.builder.CreateSelect(
                ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
                        ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0)),
                decay_derived(ctx, result),
                decay_derived(ctx, box)
            );
            retval = mark_julia_slot(derived,
                                     jlretty,
                                     tindex,
                                     ctx.tbaa().tbaa_stack);
            retval.Vboxed = box;
            break;
        }
        case jl_returninfo_t::Ghosts:
            retval = mark_julia_slot(NULL, jlretty, call, ctx.tbaa().tbaa_stack);
            break;
    }
    return retval;
}

static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, bool is_opaque_closure, jl_value_t *specTypes, jl_value_t *jlretty, llvm::Value *callee, StringRef specFunctionObject, jl_code_instance_t *fromexternal,
                                          ArrayRef<jl_cgval_t> argv, size_t nargs, jl_returninfo_t::CallingConv *cc, unsigned *nreturn_roots, jl_value_t *inferred_retty)
{
    ++EmittedSpecfunCalls;
    // emit specialized call site
    jl_returninfo_t returninfo = get_specsig_function(ctx.emission_context, jl_Module, callee, specFunctionObject, specTypes, jlretty, is_opaque_closure);
    *cc = returninfo.cc;
    *nreturn_roots = returninfo.return_roots;
    if (fromexternal) {
        std::string namep("p");
        Value *TheCallee = returninfo.decl.getCallee();
        namep += cast<Function>(TheCallee)->getName();
        GlobalVariable *GV = cast_or_null<GlobalVariable>(jl_Module->getNamedValue(namep));
        if (GV == nullptr) {
            GV = new GlobalVariable(*jl_Module, TheCallee->getType(), false,
                                    GlobalVariable::ExternalLinkage,
                                    Constant::getNullValue(TheCallee->getType()),
                                    namep);
            ctx.emission_context.external_fns[std::make_tuple(fromexternal, true)] = GV;
        }
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
        TheCallee = ai.decorateInst(ctx.builder.CreateAlignedLoad(TheCallee->getType(), GV, Align(sizeof(void*))));
        setName(ctx.emission_context, TheCallee, namep);
        returninfo.decl = FunctionCallee(returninfo.decl.getFunctionType(), TheCallee);
    }
    jl_cgval_t retval = emit_call_specfun_other(ctx, is_opaque_closure, specTypes, jlretty, returninfo, argv, nargs);
    // see if inference has a different / better type for the call than the lambda
    return update_julia_type(ctx, retval, inferred_retty);
}

static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, jl_method_instance_t *mi, jl_value_t *jlretty, StringRef specFunctionObject, jl_code_instance_t *fromexternal,
                                          ArrayRef<jl_cgval_t> argv, size_t nargs, jl_returninfo_t::CallingConv *cc, unsigned *return_roots, jl_value_t *inferred_retty)
{
    bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
    return emit_call_specfun_other(ctx, is_opaque_closure, mi->specTypes, jlretty, NULL,
        specFunctionObject, fromexternal, argv, nargs, cc, return_roots, inferred_retty);
}

static jl_value_t *get_ci_abi(jl_code_instance_t *ci)
{
    if (jl_typeof(ci->def) == (jl_value_t*)jl_abioverride_type)
        return ((jl_abi_override_t*)ci->def)->abi;
    return jl_get_ci_mi(ci)->specTypes;
}

static jl_cgval_t emit_call_specfun_other(jl_codectx_t &ctx, jl_code_instance_t *ci, StringRef specFunctionObject, jl_code_instance_t *fromexternal,
    ArrayRef<jl_cgval_t> argv, size_t nargs, jl_returninfo_t::CallingConv *cc, unsigned *return_roots, jl_value_t *inferred_retty)
{
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
    return emit_call_specfun_other(ctx, is_opaque_closure, get_ci_abi(ci), ci->rettype, NULL,
        specFunctionObject, fromexternal, argv, nargs, cc, return_roots, inferred_retty);
}

static jl_cgval_t emit_call_specfun_boxed(jl_codectx_t &ctx, jl_value_t *jlretty, StringRef specFunctionObject, jl_code_instance_t *fromexternal,
                                          ArrayRef<jl_cgval_t> argv, size_t nargs, jl_value_t *inferred_retty)
{
    Value *theFptr;
    if (fromexternal) {
        std::string namep("p");
        namep += specFunctionObject;
        GlobalVariable *GV = cast_or_null<GlobalVariable>(jl_Module->getNamedValue(namep));
        Type *pfunc = ctx.types().T_jlfunc->getPointerTo();
        if (GV == nullptr) {
            GV = new GlobalVariable(*jl_Module, pfunc, false,
                                    GlobalVariable::ExternalLinkage,
                                    Constant::getNullValue(pfunc),
                                    namep);
            ctx.emission_context.external_fns[std::make_tuple(fromexternal, false)] = GV;
        }
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
        theFptr = ai.decorateInst(ctx.builder.CreateAlignedLoad(pfunc, GV, Align(sizeof(void*))));
        setName(ctx.emission_context, theFptr, specFunctionObject);
    }
    else {
        theFptr = jl_Module->getOrInsertFunction(specFunctionObject, ctx.types().T_jlfunc).getCallee();
        addRetAttr(cast<Function>(theFptr), Attribute::NonNull);
    }
    Value *ret = emit_jlcall(ctx, theFptr, nullptr, argv, nargs, julia_call);
    return update_julia_type(ctx, mark_julia_type(ctx, ret, true, jlretty), inferred_retty);
}

static jl_cgval_t emit_invoke(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
{
    jl_value_t **args = jl_array_data(ex->args, jl_value_t*);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 2);

    jl_cgval_t lival = emit_expr(ctx, args[0]);
    SmallVector<jl_cgval_t, 0> argv(nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t();
    }
    return emit_invoke(ctx, lival, argv, nargs, rt);
}

static jl_cgval_t emit_invoke(jl_codectx_t &ctx, const jl_cgval_t &lival, ArrayRef<jl_cgval_t> argv, size_t nargs, jl_value_t *rt)
{
    ++EmittedInvokes;
    bool handled = false;
    jl_cgval_t result;
    if (lival.constant) {
        jl_method_instance_t *mi;
        jl_value_t *ci = nullptr;
        if (jl_is_method_instance(lival.constant)) {
            mi = (jl_method_instance_t*)lival.constant;
        }
        else {
            ci = lival.constant;
            assert(jl_is_code_instance(ci));
            mi = jl_get_ci_mi((jl_code_instance_t*)ci);
        }
        assert(jl_is_method_instance(mi));
        if (mi == ctx.linfo) {
            // handle self-recursion specially (TODO: assuming ci is a valid invoke for mi?)
            Function *f = ctx.f;
            FunctionType *ft = f->getFunctionType();
            if (ft == ctx.types().T_jlfunc) {
                Value *ret = emit_jlcall(ctx, f, nullptr, argv, nargs, julia_call);
                result = update_julia_type(ctx, mark_julia_type(ctx, ret, true, ctx.rettype), rt);
            }
            else if (ft == ctx.types().T_jlfuncparams) {
                Value *ret = emit_jlcall(ctx, f, ctx.spvals_ptr, argv, nargs, julia_call2);
                result = update_julia_type(ctx, mark_julia_type(ctx, ret, true, ctx.rettype), rt);
            }
            else {
                unsigned return_roots = 0;
                jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
                StringRef protoname = f->getName();
                result = emit_call_specfun_other(ctx, mi, ctx.rettype, protoname, nullptr, argv, nargs, &cc, &return_roots, rt);
            }
            handled = true;
        }
        else {
            if (ci) {
                jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
                auto invoke = jl_atomic_load_acquire(&codeinst->invoke);
                 // check if we know how to handle this specptr
                if (invoke == jl_fptr_const_return_addr) {
                    result = mark_julia_const(ctx, codeinst->rettype_const);
                }
                else {
                    bool specsig, needsparams;
                    std::tie(specsig, needsparams) = uses_specsig(get_ci_abi(codeinst), mi, codeinst->rettype, ctx.params->prefer_specsig);
                    if (needsparams) {
                        Value *r = emit_jlcall(ctx, jlinvoke_func, track_pjlvalue(ctx, literal_pointer_val(ctx, (jl_value_t*)mi)), argv, nargs, julia_call2);
                        result = mark_julia_type(ctx, r, true, rt);
                    }
                    else {
                        std::string name;
                        StringRef protoname;
                        bool need_to_emit = true;
                        bool cache_valid = ctx.use_cache || ctx.external_linkage;
                        bool external = false;

                        // Check if we already queued this up
                        auto it = ctx.call_targets.find(codeinst);
                        if (need_to_emit && it != ctx.call_targets.end()) {
                            assert(it->second.specsig == specsig);
                            protoname = it->second.decl->getName();
                            need_to_emit = cache_valid = false;
                        }

                        // Check if it is already compiled (either JIT or externally)
                        if (need_to_emit && cache_valid) {
                            // optimization: emit the correct name immediately, if we know it
                            // TODO: use `emitted` map here too to try to consolidate names?
                            uint8_t specsigflags;
                            jl_callptr_t invoke;
                            void *fptr;
                            jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke, &fptr, 0);
                            if (specsig ? specsigflags & 0b1 : invoke == jl_fptr_args_addr) {
                                protoname = jl_ExecutionEngine->getFunctionAtAddress((uintptr_t)fptr, invoke, codeinst);
                                if (ctx.external_linkage) {
                                    // TODO: Add !specsig support to aotcompile.cpp
                                    // Check that the codeinst is containing native code
                                    if (specsig && (specsigflags & 0b100)) {
                                        external = true;
                                        need_to_emit = false;
                                    }
                                }
                                else { // ctx.use_cache
                                    need_to_emit = false;
                                }
                            }
                        }
                        if (need_to_emit) {
                            raw_string_ostream(name) << (specsig ? "j_" : "j1_") << name_from_method_instance(mi) << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
                            protoname = StringRef(name);
                        }
                        jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
                        unsigned return_roots = 0;
                        if (specsig)
                            result = emit_call_specfun_other(ctx, codeinst, protoname, external ? codeinst : nullptr, argv, nargs, &cc, &return_roots, rt);
                        else
                            result = emit_call_specfun_boxed(ctx, codeinst->rettype, protoname, external ? codeinst : nullptr, argv, nargs, rt);
                        if (need_to_emit) {
                            Function *trampoline_decl = cast<Function>(jl_Module->getNamedValue(protoname));
                            ctx.call_targets[codeinst] = {cc, return_roots, trampoline_decl, nullptr, specsig};
                        }
                    }
                }
                handled = true;
            }
        }
    }
    if (!handled) {
        Value *r = emit_jlcall(ctx, jlinvoke_func, boxed(ctx, lival), argv, nargs, julia_call2);
        result = mark_julia_type(ctx, r, true, rt);
    }
    if (result.typ == jl_bottom_type) {
#ifndef JL_NDEBUG
        emit_error(ctx, "(Internal Error - IR Validity): Returned from function we expected not to.");
#endif
        CreateTrap(ctx.builder);
    }
    return result;
}

static jl_cgval_t emit_invoke_modify(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
{
    ++EmittedInvokes;
    jl_value_t **args = jl_array_data(ex->args, jl_value_t*);
    size_t arglen = jl_array_dim0(ex->args);
    size_t nargs = arglen - 1;
    assert(arglen >= 2);
    jl_cgval_t lival = emit_expr(ctx, args[0]);
    SmallVector<jl_cgval_t, 0> argv(nargs);
    for (size_t i = 0; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i + 1]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t();
    }
    const jl_cgval_t &f = argv[0];
    if (f.constant) {
        jl_cgval_t ret;
        auto it = builtin_func_map().end();
        if (f.constant == jl_builtin_modifyfield) {
            if (emit_f_opfield(ctx, &ret, jl_builtin_modifyfield, argv, nargs - 1, &lival))
                return ret;
            it = builtin_func_map().find(jl_f_modifyfield_addr);
            assert(it != builtin_func_map().end());
        }
        else if (f.constant == jl_builtin_modifyglobal) {
            if (emit_f_opglobal(ctx, &ret, jl_builtin_modifyglobal, argv, nargs - 1, &lival))
                return ret;
            it = builtin_func_map().find(jl_f_modifyglobal_addr);
            assert(it != builtin_func_map().end());
        }
        else if (f.constant == jl_builtin_memoryrefmodify) {
            if (emit_f_opmemory(ctx, &ret, jl_builtin_memoryrefmodify, argv, nargs - 1, &lival))
                return ret;
            it = builtin_func_map().find(jl_f_memoryrefmodify_addr);
            assert(it != builtin_func_map().end());
        }
        else if (jl_typetagis(f.constant, jl_intrinsic_type)) {
            JL_I::intrinsic fi = (intrinsic)*(uint32_t*)jl_data_ptr(f.constant);
            if (fi == JL_I::atomic_pointermodify && jl_intrinsic_nargs((int)fi) == nargs - 1)
                return emit_atomic_pointerop(ctx, fi, ArrayRef<jl_cgval_t>(argv).drop_front(), nargs - 1, &lival);
        }

        if (it != builtin_func_map().end()) {
            Value *oldnew = emit_jlcall(ctx, it->second, Constant::getNullValue(ctx.types().T_prjlvalue), ArrayRef<jl_cgval_t>(argv).drop_front(), nargs - 1, julia_call);
            return mark_julia_type(ctx, oldnew, true, rt);
        }
    }
    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, nargs, julia_call);
    return mark_julia_type(ctx, callval, true, rt);
}

static jl_cgval_t emit_specsig_oc_call(jl_codectx_t &ctx, jl_value_t *oc_type, jl_value_t *sigtype, MutableArrayRef<jl_cgval_t> argv /*n.b. this mutation is unusual */, size_t nargs)
{
    jl_datatype_t *oc_argt = (jl_datatype_t *)jl_tparam0(oc_type);
    jl_value_t *oc_rett = jl_tparam1(oc_type);
    jl_svec_t *types = jl_get_fieldtypes((jl_datatype_t*)oc_argt);
    size_t ntypes = jl_svec_len(types);
    for (size_t i = 0; i < nargs-1; ++i) {
        jl_value_t *typ = i >= ntypes ? jl_svecref(types, ntypes-1) : jl_svecref(types, i);
        if (jl_is_vararg(typ))
            typ = jl_unwrap_vararg(typ);
        emit_typecheck(ctx, argv[i+1], typ, "typeassert");
        argv[i+1] = update_julia_type(ctx, argv[i+1], typ);
        if (argv[i+1].typ == jl_bottom_type)
            return jl_cgval_t();
    }
    jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
    unsigned return_roots = 0;

    // Load specptr
    jl_cgval_t &theArg = argv[0];
    jl_cgval_t closure_specptr = emit_getfield_knownidx(ctx, theArg, 4, (jl_datatype_t*)oc_type, jl_memory_order_notatomic);
    Value *specptr = emit_unbox(ctx, ctx.types().T_size, closure_specptr, (jl_value_t*)jl_long_type);
    specptr = emit_inttoptr(ctx, specptr, ctx.types().T_ptr);
    JL_GC_PUSH1(&sigtype);
    jl_cgval_t r = emit_call_specfun_other(ctx, true, sigtype, oc_rett, specptr, "", NULL, argv, nargs,
        &cc, &return_roots, oc_rett);
    JL_GC_POP();
    return r;
}

static jl_cgval_t emit_call(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt, bool is_promotable)
{
    ++EmittedCalls;
    jl_value_t **args = jl_array_data(ex->args, jl_value_t*);
    size_t nargs = jl_array_dim0(ex->args);
    assert(nargs >= 1);
    jl_cgval_t f = emit_expr(ctx, args[0]);
    if (f.typ == jl_bottom_type) {
        return jl_cgval_t();
    }

    if (f.constant && jl_typetagis(f.constant, jl_intrinsic_type)) {
        JL_I::intrinsic fi = (intrinsic)*(uint32_t*)jl_data_ptr(f.constant);
        return emit_intrinsic(ctx, fi, args, nargs - 1);
    }

    size_t n_generic_args = nargs;

    SmallVector<jl_cgval_t, 0> argv(n_generic_args);

    argv[0] = f;
    for (size_t i = 1; i < nargs; ++i) {
        argv[i] = emit_expr(ctx, args[i]);
        if (argv[i].typ == jl_bottom_type)
            return jl_cgval_t(); // anything past here is unreachable
    }

    if (jl_subtype(f.typ, (jl_value_t*)jl_builtin_type)) {
        if (f.constant) {
            if (f.constant == jl_builtin_ifelse && nargs == 4)
                return emit_ifelse(ctx, argv[1], argv[2], argv[3], rt);
            jl_cgval_t result;
            bool handled = emit_builtin_call(ctx, &result, f.constant, argv, nargs - 1, rt, ex, is_promotable);
            if (handled)
                return result;
            jl_fptr_args_t builtin_fptr = jl_get_builtin_fptr((jl_datatype_t*)jl_typeof(f.constant));
            // special case for some known builtin not handled by emit_builtin_call
            auto it = builtin_func_map().find(builtin_fptr);
            if (it != builtin_func_map().end()) {
                Value *ret = emit_jlcall(ctx, it->second, Constant::getNullValue(ctx.types().T_prjlvalue), ArrayRef<jl_cgval_t>(argv).drop_front(), nargs - 1, julia_call);
                setName(ctx.emission_context, ret, it->second->name + "_ret");
                return mark_julia_type(ctx, ret, true, rt);
            }
        }
        Value *fptr;
        JuliaFunction<> *cc;
        if (f.typ == (jl_value_t*)jl_intrinsic_type) {
            fptr = prepare_call(jlintrinsic_func);
            cc = julia_call3;
        }
        else {
            fptr = ctx.builder.CreateCall(prepare_call(jlgetbuiltinfptr_func), {emit_typeof(ctx, f)});
            cc = julia_call;
        }
        Value *ret = emit_jlcall(ctx, fptr, nullptr, argv, nargs, cc);
        setName(ctx.emission_context, ret, "Builtin_ret");
        return mark_julia_type(ctx, ret, true, rt);
    }

    // handle calling an OpaqueClosure
    if (jl_is_concrete_type(f.typ) && jl_subtype(f.typ, (jl_value_t*)jl_opaque_closure_type)) {
        jl_value_t *oc_argt = jl_tparam0(f.typ);
        jl_value_t *oc_rett = jl_tparam1(f.typ);
        if (jl_is_datatype(oc_argt) && jl_tupletype_length_compat(oc_argt, nargs-1)) {
            jl_value_t *sigtype = jl_argtype_with_function_type((jl_value_t*)f.typ, (jl_value_t*)oc_argt);
            if (uses_specsig(sigtype, false, oc_rett, true)) {
                JL_GC_PUSH1(&sigtype);
                jl_cgval_t r = emit_specsig_oc_call(ctx, f.typ, sigtype, argv, nargs);
                JL_GC_POP();
                return r;
            }
            // TODO: else emit_oc_call
        }
    }
    // emit function and arguments
    Value *callval = emit_jlcall(ctx, jlapplygeneric_func, nullptr, argv, n_generic_args, julia_call);
    return mark_julia_type(ctx, callval, true, rt);
}

// --- accessing and assigning variables ---

static void emit_hasnofield_error_ifnot(jl_codectx_t &ctx, Value *ok, jl_datatype_t *type, jl_cgval_t name)
{
    ++EmittedUndefVarErrors;
    assert(name.typ == (jl_value_t*)jl_symbol_type);
    BasicBlock *err = BasicBlock::Create(ctx.builder.getContext(), "err", ctx.f);
    BasicBlock *ifok = BasicBlock::Create(ctx.builder.getContext(), "ok");
    ctx.builder.CreateCondBr(ok, ifok, err);
    ctx.builder.SetInsertPoint(err);
    ctx.builder.CreateCall(prepare_call(jlhasnofield_func),
                          {mark_callee_rooted(ctx, literal_pointer_val(ctx, (jl_value_t*)type)),
                           mark_callee_rooted(ctx, boxed(ctx, name))});
    ctx.builder.CreateUnreachable();
    ifok->insertInto(ctx.f);
    ctx.builder.SetInsertPoint(ifok);
}

static jl_cgval_t emit_checked_var(jl_codectx_t &ctx, Value *bp, jl_sym_t *name, jl_value_t *scope, bool isvol, MDNode *tbaa)
{
    LoadInst *v = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*)));
    setName(ctx.emission_context, v, jl_symbol_name(name) + StringRef(".checked"));
    if (isvol)
        v->setVolatile(true);
    v->setOrdering(AtomicOrdering::Unordered);
    if (tbaa) {
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
        ai.decorateInst(v);
    }
    undef_var_error_ifnot(ctx, ctx.builder.CreateIsNotNull(v), name, scope);
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
    Value *bp = emit_ptrgep(ctx, maybe_decay_tracked(ctx, ctx.spvals_ptr), i * sizeof(jl_value_t*) + sizeof(jl_svec_t));
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    Value *sp = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*))));
    setName(ctx.emission_context, sp, "sparam");
    Value *isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp, false, true), emit_tagfrom(ctx, jl_tvar_type));
    jl_unionall_t *sparam = (jl_unionall_t*)ctx.linfo->def.method->sig;
    for (size_t j = 0; j < i; j++) {
        sparam = (jl_unionall_t*)sparam->body;
        assert(jl_is_unionall(sparam));
    }
    undef_var_error_ifnot(ctx, isnull, sparam->var->name, (jl_value_t*)jl_static_parameter_sym);
    return mark_julia_type(ctx, sp, true, jl_any_type);
}

static jl_cgval_t emit_isdefined(jl_codectx_t &ctx, jl_value_t *sym, int allow_import)
{
    Value *isnull = NULL;
    if (jl_is_slotnumber(sym) || jl_is_argument(sym)) {
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
                // as indicated by testing (pTIndex & UNION_BOX_MARKER)
                Value *tindex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(sizeof(void*)), vi.isVolatile);
                Value *load_unbox = ctx.builder.CreateICmpEQ(
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
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
        Value *bp = emit_ptrgep(ctx, maybe_decay_tracked(ctx, ctx.spvals_ptr), i * sizeof(jl_value_t*) + sizeof(jl_svec_t));
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
        Value *sp = ai.decorateInst(ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, bp, Align(sizeof(void*))));
        isnull = ctx.builder.CreateICmpNE(emit_typeof(ctx, sp, false, true), emit_tagfrom(ctx, jl_tvar_type));
    }
    else {
        assert(false && "malformed expression");
    }
    return mark_julia_type(ctx, isnull, false, jl_bool_type);
}

static jl_cgval_t emit_varinfo(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_sym_t *varname) {
    jl_cgval_t v;
    Value *isnull = NULL;
    if (vi.boxroot == NULL || vi.pTIndex != NULL) {
        if ((!vi.isVolatile && vi.isSA) || vi.isArgument || vi.value.constant || !(vi.value.V || vi.inline_roots)) {
            v = vi.value;
            if (vi.pTIndex)
                v.TIndex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(1));
        }
        else {
            // copy value to a non-mutable (non-volatile SSA) location
            // since this might be a union slot, the most convenient approach to copying
            // is to move the whole alloca chunk
            AllocaInst *ssaslot = nullptr;
            if (vi.value.V) {
                auto stack_ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack);
                AllocaInst *varslot = cast<AllocaInst>(vi.value.V);
                Type *T = varslot->getAllocatedType();
                assert(!varslot->isArrayAllocation() && "variables not expected to be VLA");
                ssaslot = cast<AllocaInst>(varslot->clone());
                setName(ctx.emission_context, ssaslot, varslot->getName() + StringRef(".ssa"));
                ssaslot->insertAfter(varslot);
                if (vi.isVolatile) {
                    Value *unbox = ctx.builder.CreateAlignedLoad(ssaslot->getAllocatedType(), varslot, varslot->getAlign(), true);
                    stack_ai.decorateInst(ctx.builder.CreateAlignedStore(unbox, ssaslot, ssaslot->getAlign()));
                }
                else {
                    const DataLayout &DL = jl_Module->getDataLayout();
                    uint64_t sz = DL.getTypeStoreSize(T);
                    emit_memcpy(ctx, ssaslot, stack_ai, vi.value, sz, ssaslot->getAlign(), varslot->getAlign());
                }
            }
            Value *tindex = NULL;
            if (vi.pTIndex)
                tindex = ctx.builder.CreateAlignedLoad(getInt8Ty(ctx.builder.getContext()), vi.pTIndex, Align(1), vi.isVolatile);
            v = mark_julia_slot(ssaslot, vi.value.typ, tindex, ctx.tbaa().tbaa_stack, None);
        }
        if (vi.inline_roots) {
            AllocaInst *varslot = vi.inline_roots;
            size_t nroots = cast<ConstantInt>(varslot->getArraySize())->getZExtValue();
            auto T_prjlvalue = varslot->getAllocatedType();
            if (auto AT = dyn_cast<ArrayType>(T_prjlvalue)) {
                nroots *= AT->getNumElements();
                T_prjlvalue = AT->getElementType();
            }
            assert(T_prjlvalue == ctx.types().T_prjlvalue);
            v.inline_roots = load_gc_roots(ctx, varslot, nroots, vi.isVolatile);
        }
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
        maybe_mark_load_dereferenceable(boxed, vi.usedUndef || vi.pTIndex, vi.value.typ);
        if (vi.pTIndex) {
            // value is either boxed in the stack slot, or unboxed in value
            // as indicated by testing (pTIndex & UNION_BOX_MARKER)
            Value *load_unbox = ctx.builder.CreateICmpEQ(
                        ctx.builder.CreateAnd(v.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
                        ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            if (vi.usedUndef)
                isnull = ctx.builder.CreateSelect(load_unbox, isnull, box_isnull);
            if (v.V) // v.V will be null if it is a union of all ghost values
                v.V = ctx.builder.CreateSelect(load_unbox, decay_derived(ctx, v.V), decay_derived(ctx, boxed));
            else
                v.V = boxed;
            v.Vboxed = boxed;
        }
        else {
            v = mark_julia_type(ctx, boxed, true, vi.value.typ);
            if (vi.usedUndef)
                isnull = box_isnull;
        }
    }
    if (isnull) {
        setName(ctx.emission_context, isnull, jl_symbol_name(varname) + StringRef("_is_null"));
        undef_var_error_ifnot(ctx, isnull, varname, (jl_value_t*)jl_local_sym);
    }
    return v;
}

static jl_cgval_t emit_local(jl_codectx_t &ctx, jl_value_t *slotload)
{
    size_t sl = jl_slot_number(slotload) - 1;
    jl_varinfo_t &vi = ctx.slots[sl];
    jl_sym_t *sym = slot_symbol(ctx, sl);
    if (sym == jl_unused_sym) {
        // This shouldn't happen in well-formed input, but let's be robust,
        // since we otherwise cause undefined behavior here.
        emit_error(ctx, "(INTERNAL ERROR): Tried to use `#undef#` argument.");
        return jl_cgval_t();
    }
    return emit_varinfo(ctx, vi, sym);
}

static void emit_vi_assignment_unboxed(jl_codectx_t &ctx, jl_varinfo_t &vi, Value *isboxed, jl_cgval_t rval_info)
{
    if (vi.usedUndef)
        store_def_flag(ctx, vi, true);

    if (!vi.value.constant) { // check that this is not a virtual store
        assert(vi.inline_roots || vi.value.ispointer() || (vi.pTIndex && vi.value.V == NULL));
        // store value
        rval_info = update_julia_type(ctx, rval_info, vi.value.typ);
        if (rval_info.typ == jl_bottom_type)
            return;
        if (vi.pTIndex && vi.value.V) // TODO: use lifetime-end here instead
            ctx.builder.CreateStore(UndefValue::get(cast<AllocaInst>(vi.value.V)->getAllocatedType()), vi.value.V);
        // Sometimes we can get into situations where the LHS and RHS
        // are the same slot. We're not allowed to memcpy in that case
        // due to LLVM bugs.
        // This check should probably mostly catch the relevant situations.
        if (vi.value.V != nullptr ? vi.value.V != rval_info.V : vi.inline_roots != nullptr) {
            MDNode *tbaa = ctx.tbaa().tbaa_stack; // Use vi.value.tbaa ?
            if (rval_info.TIndex)
                emit_unionmove(ctx, vi.value.V, tbaa, rval_info, /*skip*/isboxed, vi.isVolatile);
            else {
                Align align(julia_alignment(rval_info.typ));
                if (vi.inline_roots)
                    split_value_into(ctx, rval_info, align, vi.value.V, align, jl_aliasinfo_t::fromTBAA(ctx, tbaa), vi.inline_roots, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe), vi.isVolatile);
                else
                    emit_unbox_store(ctx, rval_info, vi.value.V, tbaa, align, align, vi.isVolatile);
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
    }
    else {
        phiType = (jl_value_t*)jl_any_type;
    }
    jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(r, 0);
    BasicBlock *BB = ctx.builder.GetInsertBlock();
    auto InsertPt = BB->getFirstInsertionPt();
    if (phiType == jl_bottom_type) {
        return;
    }
    AllocaInst *dest = nullptr;
    SmallVector<PHINode*,0> roots;
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
            AllocaInst *phi = cast<AllocaInst>(dest->clone());
            phi->insertAfter(dest);
            PHINode *Tindex_phi = PHINode::Create(getInt8Ty(ctx.builder.getContext()), jl_array_nrows(edges), "tindex_phi");
            Tindex_phi->insertInto(BB, InsertPt);
            PHINode *ptr_phi = PHINode::Create(ctx.types().T_prjlvalue, jl_array_nrows(edges), "ptr_phi");
            ptr_phi->insertInto(BB, InsertPt);
            Value *isboxed = ctx.builder.CreateICmpNE(
                    ctx.builder.CreateAnd(Tindex_phi, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
                    ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
            ctx.builder.CreateMemCpy(phi, Align(min_align), dest, dest->getAlign(), nbytes, false);
            ctx.builder.CreateLifetimeEnd(dest);
            Value *ptr = ctx.builder.CreateSelect(isboxed,
                decay_derived(ctx, ptr_phi),
                decay_derived(ctx, phi));
            jl_cgval_t val = mark_julia_slot(ptr, phiType, Tindex_phi, best_tbaa(ctx.tbaa(), phiType));
            val.Vboxed = ptr_phi;
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, ptr_phi, roots, r));
            ctx.SAvalues[idx] = val;
            ctx.ssavalue_assigned[idx] = true;
            return;
        }
        else if (allunbox) {
            PHINode *Tindex_phi = PHINode::Create(getInt8Ty(ctx.builder.getContext()), jl_array_nrows(edges), "tindex_phi");
            Tindex_phi->insertInto(BB, InsertPt);
            jl_cgval_t val = mark_julia_slot(NULL, phiType, Tindex_phi, ctx.tbaa().tbaa_stack);
            ctx.PhiNodes.push_back(std::make_tuple(val, BB, dest, (PHINode*)nullptr, roots, r));
            ctx.SAvalues[idx] = val;
            ctx.ssavalue_assigned[idx] = true;
            return;
        }
    }
    bool isboxed = !deserves_stack(phiType);
    Type *vtype = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, phiType);
    // The frontend should really not emit this, but we allow it
    // for convenience.
    if (type_is_ghost(vtype)) {
        assert(jl_is_datatype(phiType) && jl_is_datatype_singleton((jl_datatype_t*)phiType));
        // Skip adding it to the PhiNodes list, since we didn't create one.
        ctx.SAvalues[idx] = mark_julia_const(ctx, ((jl_datatype_t*)phiType)->instance);
        ctx.ssavalue_assigned[idx] = true;
        return;
    }
    jl_cgval_t slot;
    PHINode *value_phi = NULL;
    if (!isboxed && vtype->isAggregateType()) {
        // the value will be moved into dest in the predecessor critical block.
        // here it's moved into phi in the successor (from dest)
        auto tracked = CountTrackedPointers(vtype);
        if (tracked.count) {
            roots.resize(tracked.count);
            assert(tracked.count == split_value_size((jl_datatype_t*)phiType).second);
            for (size_t nr = 0; nr < tracked.count; nr++) {
                auto root_phi = PHINode::Create(ctx.types().T_prjlvalue, jl_array_nrows(edges), "root_phi");
                root_phi->insertInto(BB, InsertPt);
                roots[nr] = root_phi;
            }
        }
        AllocaInst *phi = nullptr;
        if (!tracked.all) {
            Align align(julia_alignment(phiType));
            unsigned nb = jl_datatype_size(phiType);
            dest = emit_static_alloca(ctx, nb, align);
            phi = cast<AllocaInst>(dest->clone());
            phi->insertBefore(dest);
            ctx.builder.CreateMemCpy(phi, align, dest, align, nb, false);
            ctx.builder.CreateLifetimeEnd(dest);
        }
        slot = mark_julia_slot(phi, phiType, NULL, ctx.tbaa().tbaa_stack,
                roots.empty() ? ArrayRef<Value*>() : ArrayRef((Value *const *)&roots.front(), roots.size()));
    }
    else {
        value_phi = PHINode::Create(vtype, jl_array_nrows(edges), "value_phi");
        value_phi->insertInto(BB, InsertPt);
        slot = mark_julia_type(ctx, value_phi, isboxed, phiType);
    }
    ctx.PhiNodes.push_back(std::make_tuple(slot, BB, dest, value_phi, roots, r));
    ctx.SAvalues[idx] = slot;
    ctx.ssavalue_assigned[idx] = true;
    return;
}

static void emit_ssaval_assign(jl_codectx_t &ctx, ssize_t ssaidx_0based, jl_value_t *r)
{
    assert(!ctx.ssavalue_assigned[ssaidx_0based]);
    if (jl_is_phinode(r)) {
        return emit_phinode_assign(ctx, ssaidx_0based, r);
    }

    jl_cgval_t slot;
    if (jl_is_phicnode(r)) {
        auto it = ctx.phic_slots.find(ssaidx_0based);
        if (it == ctx.phic_slots.end()) {
            it = ctx.phic_slots.emplace(ssaidx_0based, jl_varinfo_t(ctx.builder.getContext())).first;
        }
        slot = emit_varinfo(ctx, it->second, jl_symbol("phic"));
    }
    else {
        slot = emit_expr(ctx, r, ssaidx_0based);
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
    ctx.SAvalues[ssaidx_0based] = slot; // now SAvalues[ssaidx_0based] contains the SAvalue
    ctx.ssavalue_assigned[ssaidx_0based] = true;
}

static void emit_varinfo_assign(jl_codectx_t &ctx, jl_varinfo_t &vi, jl_cgval_t rval_info, jl_value_t *l=NULL, bool allow_mismatch=false)
{
    if (!vi.used || vi.value.typ == jl_bottom_type)
        return;

    // convert rval-type to lval-type
    jl_value_t *slot_type = vi.value.typ;
    // If allow_mismatch is set, type mismatches will not result in traps.
    // This is used for upsilon nodes, where the destination can have a narrower
    // type than the store, if inference determines that the store is never read.
    Value *skip = NULL;
    rval_info = convert_julia_type(ctx, rval_info, slot_type, &skip);
    if (!allow_mismatch && skip) {
        CreateTrap(ctx.builder);
        return;
    }

    if (rval_info.typ == jl_bottom_type)
        return;

    // compute / store tindex info
    if (vi.pTIndex) {
        Value *tindex;
        if (rval_info.TIndex) {
            tindex = rval_info.TIndex;
            if (!vi.boxroot)
                tindex = ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), ~UNION_BOX_MARKER));
        }
        else {
            assert(rval_info.isboxed || rval_info.constant);
            tindex = compute_tindex_unboxed(ctx, rval_info, vi.value.typ);
            if (vi.boxroot)
                tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
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
                    ctx.builder.CreateAnd(rval_info.TIndex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
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
        emit_guarded_test(ctx, skip ? ctx.builder.CreateNot(skip) : nullptr, nullptr, [&]{
            emit_vi_assignment_unboxed(ctx, vi, isboxed, rval_info);
            return nullptr;
        });
    }

    return;
}

static void emit_assignment(jl_codectx_t &ctx, jl_value_t *l, jl_value_t *r, ssize_t ssaval)
{
    assert(!jl_is_ssavalue(l));
    jl_cgval_t rval_info = emit_expr(ctx, r, ssaval);

    if (jl_is_slotnumber(l)) {
        int sl = jl_slot_number(l) - 1;
        // it's a local variable
        jl_varinfo_t &vi = ctx.slots[sl];
        emit_varinfo_assign(ctx, vi, rval_info, l);
        return;
    }

    jl_module_t *mod;
    jl_sym_t *sym;
    bool toplevel = jl_is_module(ctx.linfo->def.value);
    bool alloc = toplevel;
    if (jl_is_symbol(l)) {
        mod = ctx.module;
        sym = (jl_sym_t*)l;
    }
    else {
        assert(jl_is_globalref(l));
        alloc &= jl_globalref_mod(l) == ctx.module;
        mod = jl_globalref_mod(l);
        sym = jl_globalref_name(l);
    }
    emit_globalop(ctx, mod, sym, rval_info, jl_cgval_t(), AtomicOrdering::Release, AtomicOrdering::NotAtomic,
                  true, false, false, false, false, nullptr, alloc);
    // Global variable. Does not need debug info because the debugger knows about
    // its memory location.
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
        if (rval_info.typ == jl_bottom_type) {
            // as a special case, PhiC nodes are allowed to use undefined
            // values, since they are just copy operations, so we need to
            // ignore the store (it will not by dynamically observed), while
            // normally, for any other operation result, we'd assume this store
            // was unreachable and dead
            val = NULL;
        }
        else {
            emit_varinfo_assign(ctx, vi, rval_info, NULL, true);
        }
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
                vi.boxroot ? ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER) :
                             ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0x01),
                vi.pTIndex, Align(1), true);
        }
        else if (vi.value.V && !vi.value.constant && vi.value.typ != jl_bottom_type) {
            assert(vi.inline_roots || vi.value.ispointer());
            if (vi.inline_roots) {
                // memory optimization: make gc pointers re-initialized to NULL
                AllocaInst *ssaroots = vi.inline_roots;
                size_t nroots = cast<ConstantInt>(ssaroots->getArraySize())->getZExtValue();
                auto T_prjlvalue = ssaroots->getAllocatedType();
                if (auto AT = dyn_cast<ArrayType>(T_prjlvalue)) {
                    nroots *= AT->getNumElements();
                    T_prjlvalue = AT->getElementType();
                }
                assert(T_prjlvalue == ctx.types().T_prjlvalue);
                Value *nullval = Constant::getNullValue(T_prjlvalue);
                auto stack_ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
                for (size_t i = 0; i < nroots; i++) {
                    stack_ai.decorateInst(ctx.builder.CreateAlignedStore(nullval, emit_ptrgep(ctx, ssaroots, i * sizeof(void*)), ssaroots->getAlign(), true));
                }
            }
        }
    }
}

// --- convert expression to code ---

static jl_cgval_t emit_cfunction(jl_codectx_t &ctx, jl_value_t *output_type, const jl_cgval_t &fexpr, jl_value_t *rt, jl_svec_t *argt);

static Value *emit_condition(jl_codectx_t &ctx, const jl_cgval_t &condV, const Twine &msg)
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
        Value *cond = emit_unbox(ctx, getInt1Ty(ctx.builder.getContext()), condV, (jl_value_t*)jl_bool_type);
        return ctx.builder.CreateNot(cond);
    }
    if (condV.isboxed) {
        return ctx.builder.CreateICmpEQ(boxed(ctx, condV),
            track_pjlvalue(ctx, literal_pointer_val(ctx, jl_false)));
    }
    // not a boolean (unreachable dead code)
    return UndefValue::get(getInt1Ty(ctx.builder.getContext()));
}

static Value *emit_condition(jl_codectx_t &ctx, jl_value_t *cond, const Twine &msg)
{
    return emit_condition(ctx, emit_expr(ctx, cond), msg);
}

static void emit_stmtpos(jl_codectx_t &ctx, jl_value_t *expr, int ssaval_result)
{
    if (jl_is_ssavalue(expr) && ssaval_result == -1)
        return; // value not used, no point in attempting codegen for it
    if (jl_is_slotnumber(expr) && ssaval_result == -1) {
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
        assert(jl_is_slotnumber(var));
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
    jl_value_t **args = jl_array_data(ex->args, jl_value_t*);
    jl_sym_t *head = ex->head;
    if (head == jl_meta_sym || head == jl_inbounds_sym || head == jl_coverageeffect_sym
            || head == jl_aliasscope_sym || head == jl_popaliasscope_sym || head == jl_inline_sym || head == jl_noinline_sym) {
        // some expression types are metadata and can be ignored
        // in statement position
        return;
    }
    else if (head == jl_leave_sym) {
        int hand_n_leave = 0;
        Value *scope_to_restore = nullptr, *token = nullptr;
        for (size_t i = 0; i < jl_expr_nargs(ex); ++i) {
            jl_value_t *arg = args[i];
            if (arg == jl_nothing)
                continue;
            assert(jl_is_ssavalue(arg));
            size_t enter_idx = ((jl_ssavalue_t*)arg)->id - 1;
            jl_value_t *enter_stmt = jl_array_ptr_ref(ctx.code, enter_idx);
            if (enter_stmt == jl_nothing)
                continue;
            if (ctx.scope_restore.count(enter_idx)) {
                // TODO: The semantics of `gc_preserve` are not perfect here. An `Expr(:enter, ...)` block may
                //       have multiple exits, but effects of `preserve_end` are only extended to the end of the
                //       dominance of each `Expr(:leave, ...)`.
                //
                //       That means that a scope object can suddenly end up preserved again outside of an
                //       `Expr(:enter, ...)` region where it ought to be dead. It'd be preferable if the effects
                //       of gc_preserve_end propagated through a control-flow joins as long as all incoming
                //       agree about the preserve state.
                //
                //       This is correct as-is anyway - it just means the scope lives longer than it needs to
                //       if the `Expr(:enter, ...)` has multiple exits.
                std::tie(token, scope_to_restore) = ctx.scope_restore[enter_idx];
                ctx.builder.CreateCall(prepare_call(gc_preserve_end_func), {token});
            }
            if (jl_enternode_catch_dest(enter_stmt)) {
                // We're not actually setting up the exception frames for these, so
                // we don't need to exit them.
                hand_n_leave += 1;
                scope_to_restore = nullptr; // restored by exception handler
            }
        }
        ctx.builder.CreateCall(prepare_call(jlleave_noexcept_func), {get_current_task(ctx), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), hand_n_leave)});
        if (scope_to_restore) {
            Value *scope_ptr = get_scope_field(ctx);
            jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe).decorateInst(
                ctx.builder.CreateAlignedStore(scope_to_restore, scope_ptr, ctx.types().alignof_ptr));
        }
    }
    else if (head == jl_pop_exception_sym) {
        jl_cgval_t excstack_state = emit_expr(ctx, jl_exprarg(expr, 0));
        assert(excstack_state.V && excstack_state.V->getType() == ctx.types().T_size);
        ctx.builder.CreateCall(prepare_call(jl_restore_excstack_func), {get_current_task(ctx), excstack_state.V});
        return;
    }
    else {
        assert(ssaval_result != -1);
        emit_ssaval_assign(ctx, ssaval_result, expr);
    }
}

static std::pair<Function*, Function*> get_oc_function(jl_codectx_t &ctx, jl_method_t *closure_method, jl_tupletype_t *env_t, jl_tupletype_t *argt_typ, jl_value_t *rettype)
{
    jl_svec_t *sig_args = NULL;
    jl_value_t *sigtype = NULL;
    JL_GC_PUSH2(&sig_args, &sigtype);

    size_t nsig = 1 + jl_svec_len(argt_typ->parameters);
    sig_args = jl_alloc_svec_uninit(nsig);
    jl_svecset(sig_args, 0, env_t);
    for (size_t i = 0; i < jl_svec_len(argt_typ->parameters); ++i) {
        jl_svecset(sig_args, 1+i, jl_svecref(argt_typ->parameters, i));
    }
    sigtype = jl_apply_tuple_type_v(jl_svec_data(sig_args), nsig);

    jl_method_instance_t *mi;
    jl_code_instance_t *ci;

    if (closure_method->source) {
        mi = jl_specializations_get_linfo(closure_method, sigtype, jl_emptysvec);
        ci = (jl_code_instance_t*)jl_rettype_inferred_addr(mi, ctx.min_world, ctx.max_world);
    }
    else {
        mi = (jl_method_instance_t*)jl_atomic_load_relaxed(&closure_method->specializations);
        assert(jl_is_method_instance(mi));
        ci = jl_atomic_load_relaxed(&mi->cache);
    }
    if (ci == NULL || (jl_value_t*)ci == jl_nothing || ci->rettype != rettype || !jl_egal(sigtype, mi->specTypes)) { // TODO: correctly handle the ABI conversion if rettype != ci->rettype
        JL_GC_POP();
        return std::make_pair((Function*)NULL, (Function*)NULL);
    }

    // method lookup code (similar to emit_invoke, and the inverse of emit_specsig_oc_call)
    bool specsig = uses_specsig(sigtype, false, rettype, true);
    std::string name;
    std::string oc;
    StringRef protoname;
    StringRef proto_oc;

    // Check if we already queued this up
    auto it = ctx.call_targets.find(ci);
    bool need_to_emit = it == ctx.call_targets.end();
    if (!need_to_emit) {
        assert(specsig == it->second.specsig);
        if (specsig) {
            protoname = it->second.decl->getName();
            proto_oc = it->second.oc->getName();
        }
        else {
            proto_oc = it->second.decl->getName();
        }
        need_to_emit = false;
    }
    else {
        if (specsig) {
            raw_string_ostream(name) << "j_" << name_from_method_instance(mi) << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
            protoname = StringRef(name);
        }
        raw_string_ostream(oc) << "j1_" << name_from_method_instance(mi) << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
        proto_oc = StringRef(oc);
    }

    // Get the fptr1 OC
    Function *F = nullptr;
    if (GlobalValue *V = jl_Module->getNamedValue(proto_oc)) {
        F = cast<Function>(V);
    }
    else {
        F = Function::Create(get_func_sig(ctx.builder.getContext()),
                             Function::ExternalLinkage,
                             proto_oc, jl_Module);
        jl_init_function(F, ctx.emission_context.TargetTriple);
        jl_name_jlfunc_args(ctx.emission_context, F);
        F->setAttributes(AttributeList::get(ctx.builder.getContext(), {get_func_attrs(ctx.builder.getContext()), F->getAttributes()}));
    }

    // Get the specsig (if applicable)
    Function *specF = nullptr;
    jl_returninfo_t::CallingConv cc = jl_returninfo_t::CallingConv::Boxed;
    unsigned return_roots = 0;
    bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
    assert(is_opaque_closure);
    if (specsig) {
        jl_returninfo_t returninfo = get_specsig_function(ctx.emission_context, jl_Module, nullptr, protoname, mi->specTypes, rettype, is_opaque_closure);
        cc = returninfo.cc;
        return_roots = returninfo.return_roots;
        specF = cast<Function>(returninfo.decl.getCallee());
    }

    if (need_to_emit) {
        ctx.call_targets[ci] = {cc, return_roots, specsig ? specF : F, specsig ? F : nullptr, specsig};
    }

    JL_GC_POP();
    return std::make_pair(F, specF);
}

static void emit_latestworld(jl_codectx_t &ctx)
{
    auto world_age_field = get_tls_world_age_field(ctx);
    LoadInst *world = ctx.builder.CreateAlignedLoad(ctx.types().T_size,
        prepare_global_in(jl_Module, jlgetworld_global), ctx.types().alignof_ptr,
        /*isVolatile*/false);
    world->setOrdering(AtomicOrdering::Acquire);
    StoreInst *store_world = ctx.builder.CreateAlignedStore(world, world_age_field,
        ctx.types().alignof_ptr, /*isVolatile*/false);
    (void)store_world;
}

// `expr` is not actually clobbered in JL_TRY
JL_GCC_IGNORE_START("-Wclobbered")
static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaidx_0based)
{
    if (jl_is_symbol(expr)) {
        jl_sym_t *sym = (jl_sym_t*)expr;
        return emit_globalref(ctx, ctx.module, sym, AtomicOrdering::Unordered);
    }
    if (jl_is_slotnumber(expr) || jl_is_argument(expr)) {
        return emit_local(ctx, expr);
    }
    if (jl_is_ssavalue(expr)) {
        ssize_t idx = ((jl_ssavalue_t*)expr)->id - 1;
        assert(idx >= 0);
        if (!ctx.ssavalue_assigned[idx]) {
            ctx.ssavalue_assigned[idx] = true; // (assignment, not comparison test)
            return jl_cgval_t(); // dead code branch
        }
        else {
            return ctx.SAvalues[idx]; // at this point, SAvalues[idx] actually contains the SAvalue
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
        jl_value_t *val = expr;
        if (jl_is_quotenode(expr))
            val = jl_fieldref_noalloc(expr, 0);
        // Toplevel exprs are rooted but because codegen assumes this is constant, it removes the write barriers for this code.
        // This means we have to globally root the value here. (The other option would be to change how we optimize toplevel code)
        jl_temporary_root(ctx, val);
        return mark_julia_const(ctx, val);
    }

    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = jl_array_data(ex->args, jl_value_t*);
    size_t nargs = jl_array_nrows(ex->args);
    jl_sym_t *head = ex->head;
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (head == jl_isdefined_sym) {
        assert(nargs == 1 || nargs == 2);
        int allow_import = 1;
        if (nargs == 2) {
            assert(jl_is_bool(args[1]));
            allow_import = args[1] == jl_true;
        }
        return emit_isdefined(ctx, args[0], allow_import);
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
            undef_var_error_ifnot(ctx, cond, var, (jl_value_t*)jl_local_sym);
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
        if (is_promotable && res.promotion_point && res.promotion_ssa == -1) {
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
            assert(jl_is_symbol(mn) || jl_is_slotnumber(mn) || jl_is_globalref(mn));

            bool issym = jl_is_symbol(mn);
            bool isglobalref = !issym && jl_is_globalref(mn);
            jl_module_t *mod = ctx.module;
            if (issym || isglobalref) {
                if (isglobalref) {
                    mod = jl_globalref_mod(mn);
                    mn = (jl_value_t*)jl_globalref_name(mn);
                }
                jl_cgval_t gf = mark_julia_type(
                        ctx,
                        ctx.builder.CreateCall(prepare_call(jlgenericfunction_func), {
                            literal_pointer_val(ctx, (jl_value_t*)mod),
                            literal_pointer_val(ctx, (jl_value_t*)mn)
                        }),
                        true,
                        jl_function_type);
                return gf;
            }
            emit_error(ctx, "method: invalid declaration");
            return jl_cgval_t();
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
            ctx.builder.CreateCall(prepare_call(jlmethod_func), ArrayRef<Value*>(mdargs)),
            true,
            jl_method_type);
        return meth;
    }
    else if (head == jl_const_sym) {
        assert(nargs <= 2);
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx.module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (jl_is_symbol(sym)) {
            jl_binding_t *bnd = jl_get_module_binding(mod, sym, 1);
            if (nargs == 2) {
                jl_cgval_t rhs = emit_expr(ctx, args[1]);
                ctx.builder.CreateCall(prepare_call(jldeclareconstval_func),
                        { julia_binding_gv(ctx, bnd), literal_pointer_val(ctx, (jl_value_t*)mod), literal_pointer_val(ctx, (jl_value_t*)sym), boxed(ctx, rhs) });
            } else {
                ctx.builder.CreateCall(prepare_call(jldeclareconstval_func),
                        { julia_binding_gv(ctx, bnd), literal_pointer_val(ctx, (jl_value_t*)mod), literal_pointer_val(ctx, (jl_value_t*)sym), ConstantPointerNull::get(cast<PointerType>(ctx.types().T_prjlvalue)) });
            }
        }
    }
    else if (head == jl_globaldecl_sym) {
        assert(nargs <= 2 && nargs >= 1);
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_module_t *mod = ctx.module;
        if (jl_is_globalref(sym)) {
            mod = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        if (nargs == 2) {
            jl_cgval_t typ = emit_expr(ctx, args[1]);
            ctx.builder.CreateCall(prepare_call(jldeclareglobal_func),
                    { literal_pointer_val(ctx, (jl_value_t*)mod), literal_pointer_val(ctx, (jl_value_t*)sym), boxed(ctx, typ), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1) });
        } else {
            ctx.builder.CreateCall(prepare_call(jldeclareglobal_func),
                    { literal_pointer_val(ctx, (jl_value_t*)mod), literal_pointer_val(ctx, (jl_value_t*)sym), ConstantPointerNull::get(cast<PointerType>(ctx.types().T_prjlvalue)), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1) });
        }
    }
    else if (head == jl_new_sym) {
        bool is_promotable = false;
        if (ssaidx_0based >= 0) {
            is_promotable = ctx.ssavalue_usecount[ssaidx_0based] == 1;
        }
        assert(nargs > 0);
        SmallVector<jl_cgval_t, 0> argv(nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        jl_value_t *ty = argv[0].typ;
        if (jl_is_type_type(ty) &&
                jl_is_datatype(jl_tparam0(ty)) &&
                jl_is_concrete_type(jl_tparam0(ty))) {
            assert(nargs <= jl_datatype_nfields(jl_tparam0(ty)) + 1);
            jl_cgval_t res = emit_new_struct(ctx, jl_tparam0(ty), nargs - 1, ArrayRef<jl_cgval_t>(argv).drop_front(), is_promotable);
            if (is_promotable && res.promotion_point && res.promotion_ssa==-1)
                res.promotion_ssa = ssaidx_0based;
            return res;
        }
        Value *val = emit_jlcall(ctx, jlnew_func, nullptr, argv, nargs, julia_call);
        // temporarily mark as `Any`, expecting `emit_ssaval_assign` to update
        // it to the inferred type.
        return mark_julia_type(ctx, val, true, (jl_value_t*)jl_any_type);
    }
    else if (head == jl_splatnew_sym) {
        jl_cgval_t argv[2] = {jl_cgval_t(), jl_cgval_t()};
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
        assert(nargs >= 5 && "Not enough arguments in new_opaque_closure");
        SmallVector<jl_cgval_t, 5> argv(nargs, jl_cgval_t());
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        const jl_cgval_t &argt = argv[0];
        const jl_cgval_t &lb = argv[1];
        const jl_cgval_t &ub = argv[2];
        // argv[3] - constprop marker not used here
        const jl_cgval_t &source = argv[4];
        if (source.constant == NULL) {
            // For now, we require non-constant source to be handled by using
            // eval. This should probably be a verifier error and an abort here.
            emit_error(ctx, "(internal error) invalid IR: opaque closure source must be constant");
            return jl_cgval_t();
        }
        bool can_optimize = argt.constant != NULL && lb.constant != NULL && ub.constant != NULL &&
            jl_is_tuple_type(argt.constant) &&
            jl_is_type(lb.constant) && jl_is_type(ub.constant) && jl_is_method(source.constant) &&
            ((jl_method_t*)source.constant)->nargs > 0 &&
            jl_is_valid_oc_argtype((jl_tupletype_t*)argt.constant, (jl_method_t*)source.constant);


        if (can_optimize) {
            jl_value_t *closure_t = NULL;
            jl_value_t *env_t = NULL;
            JL_GC_PUSH2(&closure_t, &env_t);

            size_t ncapture_args = nargs-5;
            SmallVector<jl_value_t *, 0> env_component_ts(ncapture_args);
            for (size_t i = 0; i < ncapture_args; ++i) {
                jl_value_t *typ = argv[nargs-ncapture_args+i].typ;
                if (typ == jl_bottom_type) {
                    JL_GC_POP();
                    return jl_cgval_t();
                }
                env_component_ts[i] = typ;
            }

            env_t = jl_apply_tuple_type_v(env_component_ts.data(), ncapture_args);
            // we need to know the full env type to look up the right specialization
            if (jl_is_concrete_type(env_t)) {
                jl_tupletype_t *argt_typ = (jl_tupletype_t*)argt.constant;
                Function *F, *specF;
                std::tie(F, specF) = get_oc_function(ctx, (jl_method_t*)source.constant, (jl_tupletype_t*)env_t, argt_typ, ub.constant);
                if (F) {
                    jl_cgval_t jlcall_ptr = mark_julia_type(ctx, F, false, jl_voidpointer_type);
                    jl_cgval_t world_age = mark_julia_type(ctx, get_tls_world_age(ctx), false, jl_long_type);
                    jl_cgval_t fptr;
                    if (specF)
                        fptr = mark_julia_type(ctx, specF, false, jl_voidpointer_type);
                    else
                        fptr = mark_julia_type(ctx, Constant::getNullValue(ctx.types().T_size), false, jl_voidpointer_type);

                    // TODO: Inline the env at the end of the opaque closure and generate a descriptor for GC
                    jl_cgval_t env = emit_new_struct(ctx, env_t, ncapture_args, ArrayRef<jl_cgval_t>(argv).drop_front(nargs-ncapture_args));

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
                emit_jlcall(ctx, jl_new_opaque_closure_jlcall_func, Constant::getNullValue(ctx.types().T_prjlvalue), argv, nargs, julia_call),
                true, jl_any_type);
    }
    else if (head == jl_exc_sym) {
        assert(nargs == 0);
        return mark_julia_type(ctx,
                ctx.builder.CreateCall(prepare_call(jl_current_exception_func), {get_current_task(ctx)}),
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
        // to LLVM LoopID
        SmallVector<Metadata *, 8> MDs;

        // Reserve first location for self reference to the LoopID metadata node.
        TempMDTuple TempNode = MDNode::getTemporary(ctx.builder.getContext(), None);
        MDs.push_back(TempNode.get());

        for (int i = 0, ie = nargs; i < ie; ++i) {
            Metadata *MD = to_md_tree(args[i], ctx.builder.getContext());
            if (MD)
                MDs.push_back(MD);
        }

        ctx.LoopID = MDNode::getDistinct(ctx.builder.getContext(), MDs);
        // Replace the temporary node with a self-reference.
        ctx.LoopID->replaceOperandWith(0, ctx.LoopID);
        return jl_cgval_t();
    }
    else if (head == jl_leave_sym || head == jl_coverageeffect_sym
            || head == jl_pop_exception_sym || head == jl_inbounds_sym
            || head == jl_aliasscope_sym || head == jl_popaliasscope_sym || head == jl_inline_sym || head == jl_noinline_sym) {
        jl_errorf("Expr(:%s) in value position", jl_symbol_name(head));
    }
    else if (head == jl_boundscheck_sym) {
        jl_value_t *def = (nargs == 0) ? jl_true : args[0];
        return mark_julia_const(ctx, bounds_check_enabled(ctx, def) ? jl_true : jl_false);
    }
    else if (head == jl_gc_preserve_begin_sym) {
        SmallVector<jl_cgval_t, 0> argv(nargs);
        for (size_t i = 0; i < nargs; ++i) {
            argv[i] = emit_expr(ctx, args[i]);
        }
        SmallVector<Value*, 0> vals;
        for (size_t i = 0; i < nargs; ++i) {
            vals.append(get_gc_roots_for(ctx, argv[i]));
        }
        Value *token = vals.empty()
            ? (Value*)ConstantTokenNone::get(ctx.builder.getContext())
            : ctx.builder.CreateCall(prepare_call(gc_preserve_begin_func), vals);
        jl_cgval_t tok(token, (jl_value_t*)jl_nothing_type, NULL);
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
    else if (head == jl_latestworld_sym && !jl_is_method(ctx.linfo->def.method)) {
        emit_latestworld(ctx);
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
    return jl_cgval_t();
}
JL_GCC_IGNORE_STOP

// --- generate function bodies ---

// gc frame emission
static void allocate_gc_frame(jl_codectx_t &ctx, BasicBlock *b0, bool or_new=false) JL_NOTSAFEPOINT
{
    // allocate a placeholder gc instruction
    // this will require the runtime, but it gets deleted later if unused
    ctx.topalloca = ctx.builder.CreateCall(prepare_call(or_new ? jladoptthread_func : jlpgcstack_func));
    ctx.pgcstack = ctx.topalloca;
    ctx.pgcstack->setName("pgcstack");
}

static Value *get_current_task(jl_codectx_t &ctx)
{
    return get_current_task_from_pgcstack(ctx.builder, ctx.pgcstack);
}

// Get PTLS through current task.
static Value *get_current_ptls(jl_codectx_t &ctx)
{
    return get_current_ptls_from_task(ctx.builder, get_current_task(ctx), ctx.tbaa().tbaa_gcframe);
}

// Get the address of the world age of the current task
static Value *get_tls_world_age_field(jl_codectx_t &ctx)
{
    Value *ct = get_current_task(ctx);
    return emit_ptrgep(ctx, ct, offsetof(jl_task_t, world_age), "world_age");
}

// Get the value of the world age of the current task
static Value *get_tls_world_age(jl_codectx_t &ctx)
{
    if (ctx.world_age_at_entry)
        return ctx.world_age_at_entry;
    IRBuilderBase::InsertPointGuard IP(ctx.builder);
    bool toplevel = !jl_is_method(ctx.linfo->def.method);
    if (!toplevel) {
        ctx.builder.SetInsertPoint(ctx.topalloca->getParent(), ++ctx.topalloca->getIterator());
        ctx.builder.SetCurrentDebugLocation(ctx.topalloca->getStableDebugLoc());
    }
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
    auto *world = ctx.builder.CreateAlignedLoad(ctx.types().T_size, get_tls_world_age_field(ctx), ctx.types().alignof_ptr);
    ai.decorateInst(world);
    if (!toplevel)
        ctx.world_age_at_entry = world;
    return world;
}

static Value *get_scope_field(jl_codectx_t &ctx)
{
    Value *ct = get_current_task(ctx);
    return emit_ptrgep(ctx, ct, offsetof(jl_task_t, scope), "scope");
}

static std::string get_function_name(bool specsig, bool needsparams, const char *unadorned_name, const Triple &TargetTriple)
{
    std::string _funcName;
    raw_string_ostream funcName(_funcName);
    // try to avoid conflicts in the global symbol table
    if (specsig)
        funcName << "julia_"; // api 5
    else if (needsparams)
        funcName << "japi3_";
    else
        funcName << "japi1_";
    if (TargetTriple.isOSLinux()) {
        if (unadorned_name[0] == '@')
            unadorned_name++;
    }
    funcName << unadorned_name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    return funcName.str();
}

static void gen_invoke_wrapper(jl_method_instance_t *lam, jl_value_t *abi, jl_value_t *jlretty, jl_value_t *declrt, jl_returninfo_t &f, unsigned nargs, int retarg, bool is_opaque_closure, StringRef funcName,
        Module *M, jl_codegen_params_t &params);

Function *get_or_emit_fptr1(StringRef preal_decl, Module *M)
{
    return cast<Function>(M->getOrInsertFunction(preal_decl, get_func_sig(M->getContext()), get_func_attrs(M->getContext())).getCallee());
}

Function *emit_tojlinvoke(jl_code_instance_t *codeinst, Value *theFunc, Module *M, jl_codegen_params_t &params) JL_NOTSAFEPOINT
{
    ++EmittedToJLInvokes;
    jl_codectx_t ctx(M->getContext(), params, codeinst);
    std::string name;
    raw_string_ostream(name) << "tojlinvoke" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    Function *f = Function::Create(ctx.types().T_jlfunc,
            GlobalVariable::InternalLinkage,
            name, M);
    jl_init_function(f, params.TargetTriple);
    jl_name_jlfunc_args(params, f);
    //f->setAlwaysInline();
    ctx.f = f; // for jl_Module
    BasicBlock *b0 = BasicBlock::Create(M->getContext(), "top", f);
    ctx.builder.SetInsertPoint(b0);
    Value *theFarg;

    if (theFunc) {
        theFarg = literal_pointer_val(ctx, (jl_value_t*)codeinst);
    }
    else {
        jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
        bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
        theFunc = prepare_call(is_opaque_closure ? jlinvokeoc_func : jlinvoke_func);
        theFarg = literal_pointer_val(ctx, (jl_value_t*)mi);
    }
    theFarg = track_pjlvalue(ctx, theFarg);
    auto args = f->arg_begin();
    CallInst *r = ctx.builder.CreateCall(FunctionCallee(jlinvoke_func->_type(M->getContext()), theFunc), { &*args, &*++args, &*++args, theFarg });
    r->setAttributes(jlinvoke_func->_attrs(M->getContext()));
    ctx.builder.CreateRet(r);
    return f;
}

Function *emit_tojlinvoke(jl_code_instance_t *codeinst, StringRef theFptrName, Module *M, jl_codegen_params_t &params) JL_NOTSAFEPOINT
{
    Value *theFunc = nullptr;
    if (!theFptrName.empty())
        theFunc = M->getOrInsertFunction(theFptrName, jlinvoke_func->_type(M->getContext()), jlinvoke_func->_attrs(M->getContext())).getCallee();
    return emit_tojlinvoke(codeinst, theFunc, M, params);
}

static jl_value_t *get_oc_type(jl_value_t *calltype, jl_value_t *rettype) JL_ALWAYS_LEAFTYPE
{
    jl_value_t *argtype = jl_argtype_without_function((jl_value_t*)calltype);
    JL_GC_PUSH1(&argtype);
    jl_value_t *oc_type JL_ALWAYS_LEAFTYPE = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, argtype, rettype);
    JL_GC_PROMISE_ROOTED(oc_type);
    JL_GC_POP();
    return oc_type;
}

static void emit_specsig_to_specsig(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype, bool is_for_opaque_closure,
        size_t nargs,
        jl_codegen_params_t &params,
        Value *target,
        jl_value_t *targetsig,
        jl_value_t *targetrt,
        jl_returninfo_t *targetspec,
        jl_value_t *rettype_const)
{
    ++EmittedCFuncInvalidates;
    jl_codectx_t ctx(gf_thunk->getParent()->getContext(), params, 0, 0);
    ctx.f = gf_thunk;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", gf_thunk);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);
    Function::arg_iterator AI = gf_thunk->arg_begin();
    SmallVector<jl_cgval_t, 0> myargs(nargs);
    if (cc == jl_returninfo_t::SRet || cc == jl_returninfo_t::Union)
        ++AI;
    if (return_roots)
        ++AI;
    if (JL_FEAT_TEST(ctx,gcstack_arg)) {
        ++AI; // gcstack_arg
    }
    for (size_t i = 0; i < nargs; i++) {
        if (i == 0 && is_for_opaque_closure) {
            // `jt` would be wrong here (it is the captures type), so is not used used for
            // the ABI decisions, but the argument actually will require boxing as its real type
            // which can be exactly recomputed from the specialization, as that defined the ABI
            jl_value_t *oc_type = get_oc_type(calltype, rettype);
            Value *arg_v = &*AI;
            ++AI;
            myargs[i] = mark_julia_slot(arg_v, (jl_value_t*)oc_type, NULL, ctx.tbaa().tbaa_const);
            continue;
        }
        // n.b. calltype is required to be a datatype by construction for specsig
        jl_value_t *jt = jl_nth_slot_type(calltype, i);
        bool isboxed = false;
        Type *et;
        if (deserves_argbox(jt)) {
            et = ctx.types().T_prjlvalue;
            isboxed = true;
        }
        else {
            et = julia_type_to_llvm(ctx, jt);
        }
        if (is_uniquerep_Type(jt)) {
            myargs[i] = mark_julia_const(ctx, jl_tparam0(jt));
        }
        else if (type_is_ghost(et)) {
            assert(jl_is_datatype(jt) && jl_is_datatype_singleton((jl_datatype_t*)jt));
            myargs[i] = mark_julia_const(ctx, ((jl_datatype_t*)jt)->instance);
        }
        else {
            Value *arg_v = &*AI;
            ++AI;
            if (!isboxed && et->isAggregateType()) {
                auto tracked = CountTrackedPointers(et);
                SmallVector<Value*,0> roots;
                if (tracked.count && !tracked.all) {
                    roots = load_gc_roots(ctx, &*AI, tracked.count);
                    ++AI;
                }
                myargs[i] = mark_julia_slot(arg_v, jt, NULL, ctx.tbaa().tbaa_const, roots);
            }
            else {
                assert(arg_v->getType() == et);
                myargs[i] = mark_julia_type(ctx, arg_v, isboxed, jt);
            }
        }
    }
    assert(AI == gf_thunk->arg_end());
    jl_cgval_t gf_retval;
    if (target || targetspec) {
        if (targetspec == nullptr)
            gf_retval = mark_julia_type(ctx, emit_jlcall(ctx, target, nullptr, myargs, nargs, julia_call), true, targetrt);
        else
            gf_retval = emit_call_specfun_other(ctx, is_for_opaque_closure, targetsig, targetrt, *targetspec, myargs, nargs);
    }
    if (rettype_const)
        gf_retval = mark_julia_const(ctx, rettype_const);
    if (targetrt != rettype) {
        emit_typecheck(ctx, gf_retval, rettype, "cfunction");
        gf_retval = update_julia_type(ctx, gf_retval, rettype);
    }

    switch (cc) {
    case jl_returninfo_t::Boxed:
        ctx.builder.CreateRet(boxed(ctx, gf_retval));
        break;
    case jl_returninfo_t::Register: {
        Type *gfrt = gf_thunk->getReturnType();
        if (gfrt->isVoidTy()) {
            ctx.builder.CreateRetVoid();
        }
        else {
            ctx.builder.CreateRet(emit_unbox(ctx, gfrt, gf_retval, rettype));
        }
        break;
    }
    case jl_returninfo_t::SRet: {
        Value *sret = &*gf_thunk->arg_begin();
        Align align(julia_alignment(rettype));
        if (return_roots) {
            Value *roots = gf_thunk->arg_begin() + 1; // root1 has type [n x {}*]*
            split_value_into(ctx, gf_retval, align, sret, align, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack), roots, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe));
        }
        else {
            emit_unbox_store(ctx, gf_retval, sret, ctx.tbaa().tbaa_stack, align, align);
        }
        ctx.builder.CreateRetVoid();
        break;
    }
    case jl_returninfo_t::Union: {
        Value *gf_ret = boxed(ctx, gf_retval); // TODO: this is not the most optimal way to emit this
        Type *retty = gf_thunk->getReturnType();
        Value *retval = UndefValue::get(retty);
        Value *tindex = compute_box_tindex(ctx, emit_typeof(ctx, gf_retval, false, true), (jl_value_t*)jl_any_type, rettype);
        tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
        retval = ctx.builder.CreateInsertValue(retval, gf_ret, 0);
        retval = ctx.builder.CreateInsertValue(retval, tindex, 1);
        ctx.builder.CreateRet(retval);
        break;
    }
    case jl_returninfo_t::Ghosts: {
        Value *retval = compute_tindex_unboxed(ctx, gf_retval, rettype);
        ctx.builder.CreateRet(retval);
        break;
    }
    }
}

void emit_specsig_to_fptr1(
        Function *gf_thunk, jl_returninfo_t::CallingConv cc, unsigned return_roots,
        jl_value_t *calltype, jl_value_t *rettype, bool is_for_opaque_closure,
        size_t nargs,
        jl_codegen_params_t &params,
        Function *target)
{
    emit_specsig_to_specsig(gf_thunk, cc, return_roots, calltype, rettype, is_for_opaque_closure, nargs, params, target, calltype, rettype, nullptr, nullptr);
}

static void emit_fptr1_wrapper(Module *M, StringRef gf_thunk_name, Value *target, jl_value_t *rettype_const, jl_value_t *declrt, jl_value_t *jlrettype, jl_codegen_params_t &params)
{
    Function *w = Function::Create(get_func_sig(M->getContext()), GlobalVariable::ExternalLinkage, gf_thunk_name, M);
    jl_init_function(w, params.TargetTriple);
    w->setAttributes(AttributeList::get(M->getContext(), {get_func_attrs(M->getContext()), w->getAttributes()}));
    w->addFnAttr(Attribute::OptimizeNone);
    w->addFnAttr(Attribute::NoInline);

    jl_codectx_t ctx(M->getContext(), params, 0, 0);
    ctx.f = w;
    ctx.rettype = declrt;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", w);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    jl_cgval_t gf_retval;
    if (target) {
        FunctionCallee theFunc(w->getFunctionType(), target);
        auto args = w->arg_begin();
        CallInst *r = ctx.builder.CreateCall(theFunc, { &*args, &*++args, &*++args }); // cf emit_tojlinvoke
        assert(++args == w->arg_end());
        r->setAttributes(w->getAttributes());
        gf_retval = mark_julia_type(ctx, r, true, jlrettype);
    }
    if (rettype_const)
        gf_retval = mark_julia_const(ctx, rettype_const);
    if (jlrettype != declrt)
        emit_typecheck(ctx, gf_retval, declrt, "cfunction");
    ctx.builder.CreateRet(boxed(ctx, gf_retval));
}

static void emit_specsig_to_specsig(
        Module *M, StringRef gf_thunk_name,
        jl_value_t *calltype, jl_value_t *rettype, bool is_for_opaque_closure,
        size_t nargs,
        jl_codegen_params_t &params,
        Value *target,
        jl_value_t *targetsig,
        jl_value_t *targetrt,
        jl_returninfo_t *targetspec,
        jl_value_t *rettype_const)
{
    jl_returninfo_t returninfo = get_specsig_function(params, M, nullptr, gf_thunk_name, calltype, rettype, is_for_opaque_closure);
    Function *gf_thunk = cast<Function>(returninfo.decl.getCallee());
    jl_init_function(gf_thunk, params.TargetTriple);
    gf_thunk->setAttributes(AttributeList::get(gf_thunk->getContext(), {returninfo.attrs, gf_thunk->getAttributes()}));
    emit_specsig_to_specsig(gf_thunk, returninfo.cc, returninfo.return_roots, calltype, rettype, is_for_opaque_closure, nargs, params, target, targetsig, targetrt, targetspec, rettype_const);
}

std::string emit_abi_converter(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_code_instance_t *codeinst, Value *target, bool target_specsig)
{
    // this builds a method that calls a method with the same arguments but a different specsig
    // build a specsig -> specsig converter thunk
    // build a specsig -> arg1 converter thunk
    // build a args1 -> specsig converter thunk (gen_invoke_wrapper)
    // build a args1 -> args1 converter thunk (to add typeassert on result)
    bool needsparams = false;
    bool is_opaque_closure = false;
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    std::string gf_thunk_name = get_function_name(specsig, needsparams, name_from_method_instance(mi), params.TargetTriple);
    gf_thunk_name += "_gfthunk";
    if (target_specsig) {
        jl_value_t *abi = get_ci_abi(codeinst);
        jl_returninfo_t targetspec = get_specsig_function(params, M, target, "", abi, codeinst->rettype, is_opaque_closure);
        if (specsig)
            emit_specsig_to_specsig(M, gf_thunk_name, sigt, declrt, is_opaque_closure, nargs, params,
                    target, mi->specTypes, codeinst->rettype, &targetspec, nullptr);
        else
            gen_invoke_wrapper(mi, abi, codeinst->rettype, declrt, targetspec, nargs, -1, is_opaque_closure, gf_thunk_name, M, params);
    }
    else {
        if (specsig)
            emit_specsig_to_specsig(M, gf_thunk_name, sigt, declrt, is_opaque_closure, nargs, params,
                    target, mi->specTypes, codeinst->rettype, nullptr, nullptr);
        else
            emit_fptr1_wrapper(M, gf_thunk_name, target, nullptr, declrt, codeinst->rettype, params);
    }
    return gf_thunk_name;
}

std::string emit_abi_dispatcher(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_code_instance_t *codeinst, Value *invoke)
{
    // this builds a method that calls a method with the same arguments but a different specsig
    // build a specsig -> args1 (apply_generic) or invoke (emit_tojlinvoke) call
    // build a args1 -> args1 call (emit_fptr1_wrapper)
    // build a args1 -> invoke call (emit_tojlinvoke)
    bool is_opaque_closure = false;
    Value *target;
    if (!codeinst)
        target = prepare_call_in(M, jlapplygeneric_func);
    else
        target = emit_tojlinvoke(codeinst, invoke, M, params); // TODO: inline this call?
    std::string gf_thunk_name;
    if (codeinst)
        raw_string_ostream(gf_thunk_name) << "jfptr_" << name_from_method_instance(jl_get_ci_mi(codeinst)) << "_";
    else
        raw_string_ostream(gf_thunk_name) << "j_";
    raw_string_ostream(gf_thunk_name) << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1) << "_gfthunk";
    if (specsig)
        emit_specsig_to_specsig(M, gf_thunk_name, sigt, declrt, is_opaque_closure, nargs, params,
                target, sigt, codeinst ? codeinst->rettype : (jl_value_t*)jl_any_type, nullptr, nullptr);
    else
        emit_fptr1_wrapper(M, gf_thunk_name, target, nullptr, declrt, codeinst ? codeinst->rettype : (jl_value_t*)jl_any_type, params);
    return gf_thunk_name;
}

std::string emit_abi_constreturn(Module *M, jl_codegen_params_t &params, jl_value_t *declrt, jl_value_t *sigt, size_t nargs, bool specsig, jl_value_t *rettype_const)
{
    bool is_opaque_closure = false;
    std::string gf_thunk_name;
    raw_string_ostream(gf_thunk_name) << "jconst_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    if (specsig) {
        emit_specsig_to_specsig(M, gf_thunk_name, sigt, declrt, is_opaque_closure, nargs, params,
                nullptr, sigt, jl_typeof(rettype_const), nullptr, rettype_const);
    }
    else {
        emit_fptr1_wrapper(M, gf_thunk_name, nullptr, rettype_const, declrt, jl_typeof(rettype_const), params);
    }
    return gf_thunk_name;
}

std::string emit_abi_constreturn(Module *M, jl_codegen_params_t &params, bool specsig, jl_code_instance_t *codeinst)
{
    jl_value_t *abi = get_ci_abi(codeinst);
    return emit_abi_constreturn(M, params, codeinst->rettype, abi, specsig ? jl_nparams(abi) : 0, specsig, codeinst->rettype_const);
}

// release jl_world_counter
// store theFptr
// release last_world_v
//
// acquire last_world_v
// read theFptr
// acquire jl_world_counter
// if (last_world_v != jl_world_counter)
//   fptr = compute_new_fptr(&last_world_v)
// return fptr()
static jl_cgval_t emit_abi_call(jl_codectx_t &ctx, jl_value_t *declrt, jl_value_t *sigt, ArrayRef<jl_cgval_t> inputargs, size_t nargs, Value *world_age_field)
{
    jl_cgval_t retval;
    if (sigt) {
        jl_temporary_root(ctx, declrt);
        jl_temporary_root(ctx, sigt);
        assert(nargs == jl_nparams(sigt));
        bool needsparams = false;
        bool is_opaque_closure = false;
        bool specsig = uses_specsig(sigt, needsparams, declrt, ctx.params->prefer_specsig);
        PointerType *T_ptr = ctx.types().T_ptr;
        Type *T_size = ctx.types().T_size;
        Constant *Vnull = ConstantPointerNull::get(T_ptr);
        Module *M = jl_Module;
        GlobalVariable *theFptr = new GlobalVariable(*M, T_ptr, false,
                GlobalVariable::PrivateLinkage,
                Vnull);
        GlobalVariable *last_world_p = new GlobalVariable(*M, T_size, false,
                GlobalVariable::PrivateLinkage,
                ConstantInt::get(T_size, 0));
        ArrayType *T_cfuncdata = ArrayType::get(T_ptr, 6);
        size_t flags = specsig;
        GlobalVariable *cfuncdata = new GlobalVariable(*M, T_cfuncdata, false,
                GlobalVariable::PrivateLinkage,
                ConstantArray::get(T_cfuncdata, {
                    Vnull,
                    Vnull,
                    Vnull,
                    literal_pointer_val_slot(ctx.emission_context, M, declrt),
                    literal_pointer_val_slot(ctx.emission_context, M, sigt),
                    literal_static_pointer_val((void*)flags, T_ptr)}));
        LoadInst *last_world_v = ctx.builder.CreateAlignedLoad(T_size, last_world_p, ctx.types().alignof_ptr);
        last_world_v->setOrdering(AtomicOrdering::Acquire);
        LoadInst *callee = ctx.builder.CreateAlignedLoad(T_ptr, theFptr, ctx.types().alignof_ptr);
        callee->setOrdering(AtomicOrdering::Monotonic);
        LoadInst *world_v = ctx.builder.CreateAlignedLoad(ctx.types().T_size,
            prepare_global_in(M, jlgetworld_global), ctx.types().alignof_ptr);
        world_v->setOrdering(AtomicOrdering::Acquire);
        ctx.builder.CreateStore(world_v, world_age_field);
        Value *age_not_ok = ctx.builder.CreateICmpNE(last_world_v, world_v);
        Value *target = emit_guarded_test(ctx, age_not_ok, callee, [&] {
                Function *getcaller = prepare_call(jlgetabiconverter_func);
                CallInst *cw = ctx.builder.CreateCall(getcaller, {
                        get_current_task(ctx),
                        theFptr,
                        last_world_p,
                        cfuncdata});
                cw->setAttributes(getcaller->getAttributes());
                return cw;
            });
        ctx.emission_context.cfuncs.push_back({declrt, sigt, nargs, specsig, theFptr, cfuncdata});
        if (specsig) {
            // TODO: could we force this to guarantee passing a box for `f` here (since we
            // know we had it here) and on the receiver end (emit_abi_converter /
            // emit_abi_dispatcher), force it to know that it can simply use this pointer
            // instead of re-boxing it if it needs to the boxed copy of it. This comes up
            // very rarely since usually the ABI calls are concrete and match exactly and
            // aren't closures, but sometimes there are cases like that because of
            // `::Function` de-specialization heuristics, such as for the `Returns` callable
            // given that it is `@nospecialize`.
            jl_returninfo_t targetspec = get_specsig_function(ctx.emission_context, M, target, "", sigt, declrt, is_opaque_closure);
            retval = emit_call_specfun_other(ctx, is_opaque_closure, sigt, declrt, targetspec, inputargs, nargs);
        }
        else {
            retval = mark_julia_type(ctx, emit_jlcall(ctx, target, nullptr, inputargs, nargs, julia_call), true, declrt);
        }
    }
    else {
        // emit a dispatch
        Value *ret = emit_jlcall(ctx, jlapplygeneric_func, NULL, inputargs, nargs, julia_call);
        retval = mark_julia_type(ctx, ret, true, jl_any_type);
        // inline a call to typeassert here
        emit_typecheck(ctx, retval, declrt, "cfunction");
        retval = update_julia_type(ctx, retval, declrt);
    }
    return retval;
}

static Function *gen_cfun_wrapper(
    Module *into, jl_codegen_params_t &params,
    const function_sig_t &sig, jl_value_t *ff, const char *aliasname,
    jl_value_t *declrt, jl_value_t *sigt,
    jl_unionall_t *unionall_env, jl_svec_t *sparam_vals, jl_array_t **closure_types)
{
    ++GeneratedCFuncWrappers;
    // Generate a c-callable wrapper
    assert(into);
    size_t nargs = sig.nccallargs;
    const char *name = aliasname ? aliasname : "cfunction";
    bool nest = (!ff || unionall_env);

    std::string funcName;
    raw_string_ostream(funcName) << "jlcapi_" << name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);

    Module *M = into; // Safe because ctx lock is held by params
    AttributeList attributes = sig.attributes;
    FunctionType *functype;
    if (nest) {
        // add nest parameter (pointer to jl_value_t* data array) after sret arg
        assert(closure_types);
        SmallVector<Type*, 0> fargt_sig(sig.fargt_sig.begin(), sig.fargt_sig.end());

        fargt_sig.insert(fargt_sig.begin() + sig.sret, JuliaType::get_pprjlvalue_ty(M->getContext()));

        // Shift LLVM attributes for parameters one to the right, as
        // we are adding the extra nest parameter after sret arg.
        SmallVector<std::pair<unsigned, AttributeSet>, 0> newAttributes;
        newAttributes.reserve(attributes.getNumAttrSets() + 1);
        auto it = *attributes.indexes().begin();
        const auto it_end = *attributes.indexes().end();

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
        AttrBuilder attrBuilder(M->getContext());
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
    jl_init_function(cw, params.TargetTriple);
    cw->setAttributes(AttributeList::get(M->getContext(), {attributes, cw->getAttributes()}));

    jl_codectx_t ctx(M->getContext(), params, 0, 0);
    ctx.f = cw;
    ctx.name = name;
    ctx.funcName = name;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", cw);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0, true);

    auto world_age_field = get_tls_world_age_field(ctx);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
    ctx.world_age_at_entry = ai.decorateInst(
            ctx.builder.CreateAlignedLoad(ctx.types().T_size, world_age_field, ctx.types().alignof_ptr));

    // first emit code to record the arguments
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = sig.sret ? &*AI++ : NULL;
    Value *nestPtr = nest ? &*AI++ : NULL;
    SmallVector<jl_cgval_t, 0> inputargs(nargs + 1);
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
        assert(sig.fargt_sig[i + sig.sret] == val->getType());
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
                        ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, val, Align(sizeof(void*))),
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
                    val = ctx.builder.CreateAlignedLoad(T, val, Align(1)); // make no alignment assumption about pointer from C
                    inputarg = mark_julia_type(ctx, val, false, jargty);
                }
            }
            else if (static_at || (!jl_is_typevar(jargty) && (!jl_is_datatype(jargty) || jl_is_abstracttype(jargty) || jl_is_mutable_datatype(jargty)))) {
                // must be a jl_value_t* (because it is mutable or abstract)
                inputarg = mark_julia_type(ctx, maybe_decay_untracked(ctx, val), true, jargty_proper);
            }
            else {
                // allocate val into a new box, if it might not be boxed
                // otherwise preserve / reuse the existing box identity
                // TODO: could inspect `jargty` and eliminate some of these cases
                if (!*closure_types)
                    *closure_types = jl_alloc_vec_any(0);
                jl_array_ptr_1d_push(*closure_types, jargty);
                Value *runtime_dt = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                        emit_ptrgep(ctx, nestPtr, jl_array_nrows(*closure_types) * ctx.types().sizeof_ptr),
                        Align(sizeof(void*)));
                BasicBlock *boxedBB = BasicBlock::Create(ctx.builder.getContext(), "isboxed", cw);
                BasicBlock *notanyBB = BasicBlock::Create(ctx.builder.getContext(), "not-any", cw);
                BasicBlock *unboxedBB = BasicBlock::Create(ctx.builder.getContext(), "maybe-unboxed", cw);
                BasicBlock *isanyBB = BasicBlock::Create(ctx.builder.getContext(), "any", cw);
                BasicBlock *afterBB = BasicBlock::Create(ctx.builder.getContext(), "after", cw);
                Value *isrtany = ctx.builder.CreateICmpEQ(
                        track_pjlvalue(ctx,literal_pointer_val(ctx, (jl_value_t*)jl_any_type)), runtime_dt);
                ctx.builder.CreateCondBr(isrtany, isanyBB, notanyBB);
                ctx.builder.SetInsertPoint(isanyBB);
                Value *p1 = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, val, Align(sizeof(void*)));
                ctx.builder.CreateBr(afterBB);
                isanyBB = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.SetInsertPoint(notanyBB);
                jl_cgval_t runtime_dt_val = mark_julia_type(ctx, runtime_dt, true, jl_any_type);
                Value *isrtboxed = // (!jl_is_datatype(runtime_dt) || !jl_is_concrete_datatype(runtime_dt) || jl_is_mutable_datatype(runtime_dt))
                    emit_guarded_test(ctx, emit_exactly_isa(ctx, runtime_dt_val, jl_datatype_type), true, [&] {
                            return ctx.builder.CreateOr(ctx.builder.CreateNot(emit_isconcrete(ctx, runtime_dt)), emit_datatype_mutabl(ctx, runtime_dt));
                    });
                ctx.builder.CreateCondBr(isrtboxed, boxedBB, unboxedBB);
                ctx.builder.SetInsertPoint(boxedBB);
                Value *p2 = track_pjlvalue(ctx, val);
                ctx.builder.CreateBr(afterBB);
                boxedBB = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.SetInsertPoint(unboxedBB);
                Value *p3 = emit_new_bits(ctx, runtime_dt, val);
                unboxedBB = ctx.builder.GetInsertBlock(); // could have changed
                ctx.builder.CreateBr(afterBB);
                ctx.builder.SetInsertPoint(afterBB);
                PHINode *p = ctx.builder.CreatePHI(ctx.types().T_prjlvalue, 3);
                p->addIncoming(p1, isanyBB);
                p->addIncoming(p2, boxedBB);
                p->addIncoming(p3, unboxedBB);
                inputarg = mark_julia_type(ctx, p, true, jargty_proper);
            }
        }
        else {
            bool argboxed = sig.fargt_isboxed[i];
            if (argboxed) {
                // a jl_value_t*, even when represented as a struct
                inputarg = mark_julia_type(ctx, val, true, jargty_proper);
            }
            else {
                // something of type T
                // undo whatever we might have done to this poor argument
                assert(jl_is_datatype(jargty));
                if (sig.byRefList[i]) {
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
                            emit_ptrgep(ctx, nestPtr, jl_array_nrows(*closure_types) * ctx.types().sizeof_ptr),
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
    jl_cgval_t retval = emit_abi_call(ctx, declrt, sigt, inputargs, nargs + 1, world_age_field);
    bool jlfunc_sret = retval.V && isa<AllocaInst>(retval.V) && !retval.TIndex && retval.inline_roots.empty();

    // Prepare the return value
    Value *r;
    if (sig.retboxed) {
        assert(!sig.sret);
        // return a jl_value_t*
        r = boxed(ctx, retval);
    }
    else if (sig.sret && jlfunc_sret) {
        // fuse the two sret together
        assert(retval.ispointer());
        AllocaInst *result = cast<AllocaInst>(retval.V);
        retval.V = sretPtr;
        result->replaceAllUsesWith(sretPtr);
        result->eraseFromParent();
        r = NULL;
    }
    else if (!type_is_ghost(sig.lrt)) {
        Type *prt = sig.prt;
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

    ctx.builder.CreateStore(ctx.world_age_at_entry, world_age_field);
    ctx.builder.CreateRet(r);

    ctx.builder.SetCurrentDebugLocation(noDbg);
    ctx.builder.ClearInsertionPoint();

    if (nest) {
        funcName += "make";
        Function *cw_make = Function::Create(
                FunctionType::get(getPointerTy(ctx.builder.getContext()), { getPointerTy(ctx.builder.getContext()), ctx.types().T_ppjlvalue }, false),
                GlobalVariable::ExternalLinkage,
                funcName, M);
        jl_init_function(cw_make, ctx.emission_context.TargetTriple);
        cw_make->getArg(0)->setName("wrapper");
        cw_make->getArg(1)->setName("newval");
        BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", cw_make);
        IRBuilder<> cwbuilder(b0);
        Function::arg_iterator AI = cw_make->arg_begin();
        Argument *Tramp = &*AI; ++AI;
        Argument *NVal = &*AI; ++AI;
        Function *init_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::init_trampoline);
        Function *adjust_trampoline = Intrinsic::getDeclaration(cw_make->getParent(), Intrinsic::adjust_trampoline);
        cwbuilder.CreateCall(init_trampoline, {
                Tramp,
                cw,
                NVal,
            });
        cwbuilder.CreateRet(cwbuilder.CreateCall(adjust_trampoline, { Tramp }));
        cw = cw_make;
    }

    return cw;
}

static const char *derive_sigt_name(jl_value_t *jargty)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_argument_datatype(jargty);
    if ((jl_value_t*)dt == jl_nothing)
        return NULL;
    jl_sym_t *name = dt->name->name;
    // if we have a kwcall, use that as the name anyways
    jl_methtable_t *mt = dt->name->mt;
    if (mt == jl_type_type_mt || mt == jl_nonfunction_mt || mt == NULL) {
        // our value for `name` from MethodTable is not good, try to come up with something better
        if (jl_is_type_type((jl_value_t*)dt)) {
            dt = (jl_datatype_t*)jl_argument_datatype(jl_tparam0(dt));
            if ((jl_value_t*)dt != jl_nothing) {
                name = dt->name->name;
            }
        }
    }
    return jl_symbol_name(name);
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
        return jl_cgval_t();
    }
    if (rt != declrt && rt != (jl_value_t*)jl_any_type)
        jl_temporary_root(ctx, rt);

    function_sig_t sig("cfunction", lrt, rt, retboxed, false, argt, unionall_env, false, CallingConv::C, false, &ctx.emission_context);
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
        sigt = jl_apply_tuple_type((jl_svec_t*)sigt, 1);
    }
    if (sigt && !(unionall_env && jl_has_typevar_from_unionall(rt, unionall_env))) {
        unionall_env = NULL;
    }

    bool nest = (!fexpr_rt.constant || unionall_env);
    if (ctx.emission_context.TargetTriple.isAArch64() || ctx.emission_context.TargetTriple.isARM() || ctx.emission_context.TargetTriple.isPPC64()) {
        if (nest) {
            emit_error(ctx, "cfunction: closures are not supported on this platform");
            JL_GC_POP();
            return jl_cgval_t();
        }
    }
    const char *name = derive_sigt_name(fexpr_rt.typ);
    Value *F = gen_cfun_wrapper(
            jl_Module, ctx.emission_context,
            sig, fexpr_rt.constant, name,
            declrt, sigt,
            unionall_env, sparam_vals, &closure_types);
    bool outboxed;
    if (nest) {
        // F is actually an init_trampoline function that returns the real address
        // Now fill in the nest parameters
        Value *fobj = boxed(ctx, fexpr_rt);
        jl_svec_t *fill = jl_emptysvec;
        if (closure_types) {
            assert(ctx.spvals_ptr);
            size_t n = jl_array_nrows(closure_types);
            fill = jl_alloc_svec_uninit(n);
            for (size_t i = 0; i < n; i++) {
                jl_svecset(fill, i, jl_array_ptr_ref(closure_types, i));
            }
            JL_GC_PUSH1(&fill);
            jl_temporary_root(ctx, (jl_value_t*)fill);
            JL_GC_POP();
        }
        Type *T_htable = ArrayType::get(ctx.types().T_size, sizeof(htable_t) / sizeof(void*));
        Value *cache = new GlobalVariable(*jl_Module, T_htable, false,
                               GlobalVariable::PrivateLinkage,
                               ConstantAggregateZero::get(T_htable));
        F = ctx.builder.CreateCall(prepare_call(jlgetcfunctiontrampoline_func), {
                 fobj,
                 literal_pointer_val(ctx, output_type),
                 cache,
                 literal_pointer_val(ctx, (jl_value_t*)fill),
                 F,
                 closure_types ? literal_pointer_val(ctx, (jl_value_t*)unionall_env) : Constant::getNullValue(ctx.types().T_pjlvalue),
                 closure_types ? decay_derived(ctx, ctx.spvals_ptr) : ConstantPointerNull::get(ctx.builder.getPtrTy(AddressSpace::Derived))
             });
        outboxed = true;
    }
    else {
        F = ctx.builder.CreatePtrToInt(F, ctx.types().T_size);
        outboxed = (output_type != (jl_value_t*)jl_voidpointer_type);
        if (outboxed) {
            assert(jl_datatype_size(output_type) == sizeof(void*) * 4);
            Value *strct = emit_allocobj(ctx, (jl_datatype_t*)output_type, true);
            setName(ctx.emission_context, strct, "cfun_result");
            Value *derived_strct = decay_derived(ctx, strct);
            MDNode *tbaa = best_tbaa(ctx.tbaa(), output_type);
            jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, tbaa);
            ai.decorateInst(ctx.builder.CreateStore(F, derived_strct));
            ai.decorateInst(ctx.builder.CreateStore(
                ctx.builder.CreatePtrToInt(literal_pointer_val(ctx, fexpr_rt.constant), ctx.types().T_size),
                ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_size, derived_strct, 1)));
            ai.decorateInst(ctx.builder.CreateStore(Constant::getNullValue(ctx.types().T_size),
                    ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_size, derived_strct, 2)));
            ai.decorateInst(ctx.builder.CreateStore(Constant::getNullValue(ctx.types().T_size),
                    ctx.builder.CreateConstInBoundsGEP1_32(ctx.types().T_size, derived_strct, 3)));
            F = strct;
        }
    }
    JL_GC_POP();
    return mark_julia_type(ctx, F, outboxed, output_type);
}

// do codegen to create a C-callable alias/wrapper, or if sysimg_handle is set,
// restore one from a loaded system image.
const char *jl_generate_ccallable(Module *llvmmod, jl_value_t *nameval, jl_value_t *declrt, jl_value_t *sigt, jl_codegen_params_t &params)
{
    ++GeneratedCCallables;
    jl_datatype_t *ft = (jl_datatype_t*)jl_tparam0(sigt);
    assert(jl_is_datatype(ft));
    jl_value_t *ff = ft->instance;
    assert(ff);
    const char *name = !jl_is_string(nameval) ? jl_symbol_name(ft->name->mt->name) : jl_string_data(nameval);
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
        function_sig_t sig("cfunction", lcrt, crt, toboxed, false,
                           argtypes, NULL, false, CallingConv::C, false, &params);
        if (sig.err_msg.empty()) {
            //Safe b/c params holds context lock
            Function *cw = gen_cfun_wrapper(llvmmod, params, sig, ff, name, declrt, sigt, NULL, NULL, NULL);
            auto alias = GlobalAlias::create(cw->getValueType(), cw->getType()->getAddressSpace(),
                                GlobalValue::ExternalLinkage, name, cw, llvmmod);
            if (params.TargetTriple.isOSBinFormatCOFF()) {
                alias->setDLLStorageClass(GlobalValue::DLLStorageClassTypes::DLLExportStorageClass);
            }
            JL_GC_POP();
            return name;
        }
        err = jl_get_exceptionf(jl_errorexception_type, "%s", sig.err_msg.c_str());
    }
    jl_throw(err);
}

// generate a julia-callable function that calls f (AKA lam)
// if is_opaque_closure, then generate the OC invoke, rather than a real invoke
static void gen_invoke_wrapper(jl_method_instance_t *lam, jl_value_t *abi, jl_value_t *jlretty, jl_value_t *declrt, jl_returninfo_t &f, unsigned nargs, int retarg, bool is_opaque_closure, StringRef funcName,
        Module *M, jl_codegen_params_t &params)
{
    ++GeneratedInvokeWrappers;
    Function *w = Function::Create(get_func_sig(M->getContext()), GlobalVariable::ExternalLinkage, funcName, M);
    jl_init_function(w, params.TargetTriple);
    jl_name_jlfunc_args(params, w);
    w->setAttributes(AttributeList::get(M->getContext(), {get_func_attrs(M->getContext()), w->getAttributes()}));
    w->addFnAttr(Attribute::OptimizeNone);
    w->addFnAttr(Attribute::NoInline);
    Function::arg_iterator AI = w->arg_begin();
    Value *funcArg = &*AI++;
    Value *argArray = &*AI++;
    Value *argCount = &*AI++; (void)argCount; // unused
    //Value *mfunc = &*AI++; (void)mfunc; // unused
    assert(AI == w->arg_end());

    jl_codectx_t ctx(M->getContext(), params, 0, 0);
    ctx.f = w;
    ctx.linfo = lam;
    ctx.rettype = jlretty;

    BasicBlock *b0 = BasicBlock::Create(ctx.builder.getContext(), "top", w);
    ctx.builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    ctx.builder.SetCurrentDebugLocation(noDbg);
    allocate_gc_frame(ctx, b0);

    SmallVector<jl_cgval_t, 0> argv(nargs);
    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
    for (size_t i = 0; i < nargs; ++i) {
        if (i == 0 && is_opaque_closure) {
            jl_value_t *oc_type = (jl_value_t*)jl_any_type; // more accurately: get_oc_type(lam->specTypes, jlretty)
            argv[i] = mark_julia_slot(funcArg, oc_type, NULL, ctx.tbaa().tbaa_const);
            continue;
        }
        jl_value_t *ty = jl_nth_slot_type(abi, i);
        Value *theArg;
        if (i == 0) {
            theArg = funcArg;
        }
        else {
            Value *argPtr = emit_ptrgep(ctx, argArray, (i - 1) * ctx.types().sizeof_ptr);
            theArg = ai.decorateInst(maybe_mark_load_dereferenceable(
                    ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, argPtr, Align(sizeof(void*))),
                    false,
                    ty));
        }
        argv[i] = mark_julia_type(ctx, theArg, true, ty);
    }
    jl_cgval_t retval = emit_call_specfun_other(ctx, is_opaque_closure, abi, jlretty, f, argv, nargs);
    if (declrt != jlretty) {
        emit_typecheck(ctx, retval, declrt, "cfunction");
        retval = update_julia_type(ctx, retval, declrt);
    }
    if (retarg != -1) {
        Value *theArg;
        if (retarg == 0)
            theArg = funcArg;
        else
            theArg = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue,
                    emit_ptrgep(ctx, argArray, (retarg - 1) * ctx.types().sizeof_ptr),
                    Align(sizeof(void*)));
        retval = mark_julia_type(ctx, theArg, true, jl_any_type);
    }
    if (retval.typ == jl_bottom_type)
        CreateTrap(ctx.builder, false);
    else
        ctx.builder.CreateRet(boxed(ctx, retval));
}

static jl_returninfo_t get_specsig_function(jl_codegen_params_t &params, Module *M, Value *fval, StringRef name, jl_value_t *sig, jl_value_t *jlrettype, bool is_opaque_closure,
        ArrayRef<const char*> ArgNames, unsigned nreq)
{
    bool gcstack_arg = params.params->gcstack_arg;
    jl_returninfo_t props = {};
    SmallVector<Type*,8> fsig;
    SmallVector<std::string,4> argnames;
    Type *rt = NULL;
    Type *srt = NULL;
    Type *T_prjlvalue = PointerType::get(M->getContext(), AddressSpace::Tracked);
    if (jlrettype == (jl_value_t*)jl_bottom_type) {
        rt = getVoidTy(M->getContext());
        props.cc = jl_returninfo_t::Register;
    }
    else if (jl_is_structtype(jlrettype) && jl_is_datatype_singleton((jl_datatype_t*)jlrettype)) {
        rt = getVoidTy(M->getContext());
        props.cc = jl_returninfo_t::Register;
    }
    else if (jl_is_uniontype(jlrettype)) {
        bool allunbox;
        union_alloca_type((jl_uniontype_t*)jlrettype, allunbox, props.union_bytes, props.union_align, props.union_minalign);
        if (props.union_bytes) {
            props.cc = jl_returninfo_t::Union;
            Type *AT = ArrayType::get(getInt8Ty(M->getContext()), props.union_bytes);
            fsig.push_back(AT->getPointerTo());
            argnames.push_back("union_bytes_return");
            Type *pair[] = { T_prjlvalue, getInt8Ty(M->getContext()) };
            rt = StructType::get(M->getContext(), ArrayRef<Type*>(pair));
        }
        else if (allunbox) {
            props.cc = jl_returninfo_t::Ghosts;
            rt = getInt8Ty(M->getContext());
        }
        else {
            rt = T_prjlvalue;
        }
    }
    else if (!deserves_retbox(jlrettype)) {
        bool retboxed;
        rt = _julia_type_to_llvm(&params, M->getContext(), jlrettype, &retboxed);
        assert(!retboxed);
        if (rt != getVoidTy(M->getContext()) && deserves_sret(jlrettype, rt)) {
            auto tracked = CountTrackedPointers(rt, true);
            assert(!tracked.derived);
            if (tracked.count && !tracked.all) {
                props.return_roots = tracked.count;
                assert(props.return_roots == ((jl_datatype_t*)jlrettype)->layout->npointers);
            }
            props.cc = jl_returninfo_t::SRet;
            props.union_bytes = jl_datatype_size(jlrettype);
            props.union_align = props.union_minalign = julia_alignment(jlrettype);
            // sret is always passed from alloca
            assert(M);
            fsig.push_back(rt->getPointerTo(M->getDataLayout().getAllocaAddrSpace()));
            argnames.push_back("sret_return");
            srt = rt;
            rt = getVoidTy(M->getContext());
        }
        else {
            props.cc = jl_returninfo_t::Register;
        }
    }
    else {
        rt = T_prjlvalue;
    }

    SmallVector<AttributeSet, 8> attrs; // function declaration attributes
    if (props.cc == jl_returninfo_t::SRet) {
        assert(srt);
        AttrBuilder param(M->getContext());
        param.addStructRetAttr(srt);
        param.addAttribute(Attribute::NoAlias);
        param.addAttribute(Attribute::NoCapture);
        param.addAttribute(Attribute::NoUndef);
        attrs.push_back(AttributeSet::get(M->getContext(), param));
        assert(fsig.size() == 1);
    }
    if (props.cc == jl_returninfo_t::Union) {
        AttrBuilder param(M->getContext());
        param.addAttribute(Attribute::NoAlias);
        param.addAttribute(Attribute::NoCapture);
        param.addAttribute(Attribute::NoUndef);
        attrs.push_back(AttributeSet::get(M->getContext(), param));
        assert(fsig.size() == 1);
    }

    if (props.return_roots) {
        AttrBuilder param(M->getContext());
        param.addAttribute(Attribute::NoAlias);
        param.addAttribute(Attribute::NoCapture);
        param.addAttribute(Attribute::NoUndef);
        attrs.push_back(AttributeSet::get(M->getContext(), param));
        fsig.push_back(getPointerTy(M->getContext()));
        argnames.push_back("return_roots");
    }

    if (gcstack_arg) {
        AttrBuilder param(M->getContext());
        if (params.use_swiftcc)
            param.addAttribute(Attribute::SwiftSelf);
        param.addAttribute(Attribute::NonNull);
        attrs.push_back(AttributeSet::get(M->getContext(), param));
        fsig.push_back(PointerType::get(M->getContext(), 0));
        argnames.push_back("pgcstack_arg");
    }

    size_t nparams = jl_nparams(sig);
    for (size_t i = 0; i < nparams; i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        bool isboxed = false;
        Type *et = nullptr;
        if (i != 0 || !is_opaque_closure) { // special token for OC argument
            if (is_uniquerep_Type(jt))
                continue;
            isboxed = deserves_argbox(jt);
            et = isboxed ? T_prjlvalue : _julia_type_to_llvm(&params, M->getContext(), jt, nullptr);
            if (type_is_ghost(et))
                continue;
        }
        AttrBuilder param(M->getContext());
        Type *ty = et;
        if (et == nullptr || et->isAggregateType()) { // aggregate types are passed by pointer
            param.addAttribute(Attribute::NoCapture);
            param.addAttribute(Attribute::ReadOnly);
            ty = PointerType::get(M->getContext(), AddressSpace::Derived);
        }
        else if (isboxed && jl_may_be_immutable_datatype(jt) && !jl_is_abstracttype(jt)) {
            param.addAttribute(Attribute::ReadOnly);
        }
        else if (jl_is_primitivetype(jt) && ty->isIntegerTy()) {
            bool issigned = jl_signed_type && jl_subtype(jt, (jl_value_t*)jl_signed_type);
            Attribute::AttrKind attr = issigned ? Attribute::SExt : Attribute::ZExt;
            param.addAttribute(attr);
        }
        attrs.push_back(AttributeSet::get(M->getContext(), param));
        fsig.push_back(ty);
        size_t argno = i < nreq ? i : nreq;
        std::string genname;
        if (!ArgNames.empty()) {
            genname = ArgNames[argno];
            if (genname.empty())
                genname = (StringRef("#") + Twine(argno + 1)).str();
            if (i >= nreq)
                genname += (StringRef("[") + Twine(i - nreq + 1) + StringRef("]")).str();
            const char *arg_typename = jl_is_datatype(jt) ? jl_symbol_name(((jl_datatype_t*)jt)->name->name) : "<unknown type>";
            argnames.push_back((genname + StringRef("::") + arg_typename).str());
        }
        if (et && et->isAggregateType()) {
            auto tracked = CountTrackedPointers(et);
            if (tracked.count && !tracked.all) {
                attrs.push_back(AttributeSet::get(M->getContext(), param));
                fsig.push_back(PointerType::get(M->getContext(), M->getDataLayout().getAllocaAddrSpace()));
                if (!genname.empty())
                    argnames.push_back((Twine(".roots.") + genname).str());
            }
        }
    }

    AttributeSet FnAttrs;
    AttributeSet RetAttrs;
    if (jlrettype == (jl_value_t*)jl_bottom_type)
        FnAttrs = FnAttrs.addAttribute(M->getContext(), Attribute::NoReturn);
    else if (rt == T_prjlvalue)
        RetAttrs = RetAttrs.addAttribute(M->getContext(), Attribute::NonNull);
    AttributeList attributes = AttributeList::get(M->getContext(), FnAttrs, RetAttrs, attrs);

    FunctionType *ftype = FunctionType::get(rt, fsig, false);
    if (fval == NULL) {
        Function *f = M ? cast_or_null<Function>(M->getNamedValue(name)) : NULL;
        if (f == NULL) {
            f = Function::Create(ftype, GlobalVariable::ExternalLinkage, name, M);
            jl_init_function(f, params.TargetTriple);
            if (params.params->debug_info_level >= 2) {
                ios_t sigbuf;
                ios_mem(&sigbuf, 0);
                jl_static_show_func_sig((JL_STREAM*) &sigbuf, sig);
                f->setAttributes(AttributeList::get(f->getContext(), {attributes.addFnAttribute(M->getContext(),"julia.fsig", StringRef(sigbuf.buf, sigbuf.size)), f->getAttributes()}));
                ios_close(&sigbuf);
            } else {
                f->setAttributes(AttributeList::get(f->getContext(), {attributes, f->getAttributes()}));
            }
        }
        else {
            assert(f->getFunctionType() == ftype);
        }
        fval = f;
    }
    else {
        assert(fval->getType()->isPointerTy());
    }
    if (auto F = dyn_cast<Function>(fval)) {
        if (gcstack_arg && params.use_swiftcc)
            F->setCallingConv(CallingConv::Swift);
        assert(F->arg_size() >= argnames.size());
        for (size_t i = 0; i < argnames.size(); i++) {
            F->getArg(i)->setName(argnames[i]);
        }
    }
    props.decl = FunctionCallee(ftype, fval);
    props.attrs = attributes;
    return props;
}

static DISubroutineType *
get_specsig_di(jl_codectx_t &ctx, jl_debugcache_t &debuginfo, jl_value_t *rt, jl_value_t *sig, DIBuilder &dbuilder)
{
    size_t nargs = jl_nparams(sig); // TODO: if this is a Varargs function, our debug info for the `...` var may be misleading
    SmallVector<Metadata*, 0> ditypes(nargs + 1);
    ditypes[0] = julia_type_to_di(ctx, debuginfo, rt, &dbuilder, false);
    for (size_t i = 0; i < nargs; i++) {
        jl_value_t *jt = jl_tparam(sig, i);
        ditypes[i + 1] = julia_type_to_di(ctx, debuginfo, jt, &dbuilder, false);
    }
    return dbuilder.createSubroutineType(dbuilder.getOrCreateTypeArray(ditypes));
}

/* aka Core.Compiler.tuple_tfunc */
static jl_datatype_t *compute_va_type(jl_value_t *sig, size_t nreq)
{
    size_t nvargs = jl_nparams(sig)-nreq;
    jl_svec_t *tupargs = jl_alloc_svec(nvargs);
    JL_GC_PUSH1(&tupargs);
    for (size_t i = nreq; i < jl_nparams(sig); ++i) {
        jl_value_t *argType = jl_nth_slot_type(sig, i);
        // n.b. specTypes is required to be a datatype by construction for specsig
        if (is_uniquerep_Type(argType))
            argType = jl_typeof(jl_tparam0(argType));
        else if (jl_has_intersect_type_not_kind(argType)) {
            jl_value_t *ts[2] = {argType, (jl_value_t*)jl_type_type};
            argType = jl_type_union(ts, 2);
        }
        jl_svecset(tupargs, i-nreq, argType);
    }
    jl_value_t *typ = jl_apply_tuple_type(tupargs, 1);
    JL_GC_POP();
    return (jl_datatype_t*)typ;
}

// Compile to LLVM IR, using a specialized signature if applicable.
static jl_llvm_functions_t
    emit_function(
        orc::ThreadSafeModule &TSM,
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *abi,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
{
    ++EmittedFunctions;
    // step 1. unpack AST and allocate codegen context for this function
    size_t min_world = src->min_world;
    size_t max_world = src->max_world;
    jl_llvm_functions_t declarations;
    jl_codectx_t ctx(*params.tsctx.getContext(), params, min_world, max_world);
    jl_datatype_t *vatyp = NULL;
    JL_GC_PUSH2(&ctx.code, &vatyp);
    ctx.code = src->code;
    ctx.source = src;

    ctx.module = jl_is_method(lam->def.method) ? lam->def.method->module : lam->def.module;
    ctx.linfo = lam;
    ctx.name = name_from_method_instance(lam);
    size_t nreq = src->nargs;
    int va = src->isva;
    ctx.nargs = nreq;
    if (va) {
        assert(nreq > 0);
        nreq--;
    }
    if (jl_is_method(lam->def.value)) {
        ctx.is_opaque_closure = lam->def.method->is_for_opaque_closure;
    }
    ctx.nReqArgs = nreq;
    if (va) {
        jl_sym_t *vn = slot_symbol(ctx, ctx.nargs-1);
        if (vn != jl_unused_sym)
            ctx.vaSlot = ctx.nargs - 1;
    }
    ctx.rettype = jlrettype;
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
        ctx.line = lam->def.method->line;
    }
    else if ((jl_value_t*)src->debuginfo != jl_nothing) {
        // look for the file and line info of the original start of this block, as reported by lowering
        jl_debuginfo_t *debuginfo = src->debuginfo;
        while ((jl_value_t*)debuginfo->linetable != jl_nothing)
            debuginfo = debuginfo->linetable;
        ctx.file = jl_debuginfo_file(debuginfo);
        struct jl_codeloc_t lineidx = jl_uncompress1_codeloc(debuginfo->codelocs, 0);
        ctx.line = lineidx.line;
        toplineno = std::max((int32_t)0, lineidx.line);
    }
    if (ctx.file.empty())
        ctx.file = "<missing>";
    // jl_printf(JL_STDERR, "\n*** compiling %s at %s:%d\n\n",
    //           jl_symbol_name(ctx.name), ctx.file.str().c_str(), toplineno);

    bool debug_enabled = ctx.emission_context.params->debug_info_level != 0;
    if (dbgFuncName.empty()) // Should never happen anymore?
        debug_enabled = false;

    // First go through and collect all branch targets, so we know where to
    // split basic blocks.
    std::set<int> branch_targets; // 1-indexed, sorted
    for (size_t i = 0; i < stmtslen; ++i) {
        jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
        if (jl_is_gotoifnot(stmt)) {
            int dest = jl_gotoifnot_label(stmt);
            branch_targets.insert(dest);
            // The next 1-indexed statement
            branch_targets.insert(i + 2);
        }
        else if (jl_is_returnnode(stmt)) {
            // We don't do dead branch elimination before codegen
            // so we need to make sure to start a BB after any
            // return node, even if they aren't otherwise branch
            // targets.
            if (i + 2 <= stmtslen)
                branch_targets.insert(i + 2);
        }
        else if (jl_is_enternode(stmt)) {
            branch_targets.insert(i + 1);
            if (i + 2 <= stmtslen)
                branch_targets.insert(i + 2);
            size_t catch_dest = jl_enternode_catch_dest(stmt);
            if (catch_dest)
                branch_targets.insert(catch_dest);
        }
        else if (jl_is_gotonode(stmt)) {
            int dest = jl_gotonode_label(stmt);
            branch_targets.insert(dest);
            if (i + 2 <= stmtslen)
                branch_targets.insert(i + 2);
        }
        else if (jl_is_phinode(stmt)) {
            jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(stmt, 0);
            for (size_t j = 0; j < jl_array_nrows(edges); ++j) {
                size_t edge = jl_array_data(edges, int32_t)[j];
                if (edge == i)
                    branch_targets.insert(i + 1);
            }
        }
    }

    // step 2. process var-info lists to see what vars need boxing
    int n_ssavalues = jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_nrows(src->ssavaluetypes);
    size_t vinfoslen = jl_array_dim0(src->slotflags);
    ctx.slots.resize(vinfoslen, jl_varinfo_t(ctx.builder.getContext()));
    assert(abi); // the specTypes field should always be assigned


    // create SAvalue locations for SSAValue objects
    ctx.ssavalue_assigned.assign(n_ssavalues, false);
    ctx.SAvalues.assign(n_ssavalues, jl_cgval_t());
    ctx.ssavalue_usecount.assign(n_ssavalues, 0);

    bool specsig, needsparams;
    std::tie(specsig, needsparams) = uses_specsig(abi, lam, jlrettype, params.params->prefer_specsig);

    // step 3. some variable analysis
    size_t i;
    for (i = 0; i < nreq && i < vinfoslen; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        varinfo.isArgument = true;
        jl_sym_t *argname = slot_symbol(ctx, i);
        if (argname == jl_unused_sym)
            continue;
        jl_value_t *ty = jl_nth_slot_type(abi, i);
        // TODO: jl_nth_slot_type should call jl_rewrap_unionall
        //  specTypes is required to be a datatype by construction for specsig, but maybe not otherwise
        // OpaqueClosure implicitly loads the env
        if (i == 0 && ctx.is_opaque_closure) {
            // n.b. this is not really needed, because ty was already supposed to be correct
            if (jl_is_array(src->slottypes)) {
                ty = jl_array_ptr_ref((jl_array_t*)src->slottypes, i);
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
        vatyp = specsig ? compute_va_type(abi, nreq) : (jl_tuple_type);
        varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, vatyp);
    }

    for (i = 0; i < vinfoslen; i++) {
        jl_varinfo_t &varinfo = ctx.slots[i];
        uint8_t flags = jl_array_uint8_ref(src->slotflags, i);
        varinfo.isSA = (jl_vinfo_sa(flags) != 0) || varinfo.isArgument;
        varinfo.usedUndef = (jl_vinfo_usedundef(flags) != 0) || !varinfo.isArgument;
        if (!varinfo.isArgument) {
            varinfo.value = mark_julia_type(ctx, (Value*)NULL, false, (jl_value_t*)jl_any_type);
        }
    }

    // finish recording variable use info
    for (i = 0; i < stmtslen; i++)
        simple_use_analysis(ctx, jl_array_ptr_ref(stmts, i));

    // determine which vars need to be volatile
    mark_volatile_vars(stmts, ctx.slots, branch_targets);

    // step 4. determine function signature
    if (!specsig)
        ctx.nReqArgs--;  // function not part of argArray in jlcall

    std::string _funcName = get_function_name(specsig, needsparams, ctx.name, ctx.emission_context.TargetTriple);
    declarations.specFunctionObject = _funcName;

    // allocate Function declarations and wrapper objects
    //Safe because params holds ctx lock
    Module *M = TSM.getModuleUnlocked();
    jl_debugcache_t debugcache;
    debugcache.initialize(M);
    jl_returninfo_t returninfo = {};
    Function *f = NULL;
    bool has_sret = false;
    if (specsig) { // assumes !va and !needsparams
        SmallVector<const char*,0> ArgNames(0);
        if (!M->getContext().shouldDiscardValueNames()) {
            ArgNames.resize(ctx.nargs, "");
            for (int i = 0; i < ctx.nargs; i++) {
                jl_sym_t *argname = slot_symbol(ctx, i);
                if (argname == jl_unused_sym)
                    continue;
                const char *name = jl_symbol_name(argname);
                if (name[0] == '\0' && ctx.vaSlot == i)
                    ArgNames[i] = "...";
                else
                    ArgNames[i] = name;
            }
        }
        returninfo = get_specsig_function(params, M, NULL, declarations.specFunctionObject, abi,
                                          jlrettype, ctx.is_opaque_closure,
                                          ArgNames, nreq);
        f = cast<Function>(returninfo.decl.getCallee());
        has_sret = (returninfo.cc == jl_returninfo_t::SRet || returninfo.cc == jl_returninfo_t::Union);
        jl_init_function(f, ctx.emission_context.TargetTriple);

        // common pattern: see if all return statements are an argument in that
        // case the apply-generic call can re-use the original box for the return
        int retarg = [stmts, nreq]() {
            int retarg = -1;
            for (size_t i = 0; i < jl_array_nrows(stmts); ++i) {
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
        raw_string_ostream(wrapName) << "jfptr_" << ctx.name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
        declarations.functionObject = wrapName;
        size_t nparams = jl_nparams(abi);
        gen_invoke_wrapper(lam, abi, jlrettype, jlrettype, returninfo, nparams, retarg, ctx.is_opaque_closure, declarations.functionObject, M, ctx.emission_context);
        // TODO: add attributes: maybe_mark_argument_dereferenceable(Arg, argType)
        // TODO: add attributes: dereferenceable<sizeof(void*) * nreq>
        // TODO: (if needsparams) add attributes: dereferenceable<sizeof(void*) * length(sp)>, readonly, nocapture
    }
    else {
        f = Function::Create(needsparams ? ctx.types().T_jlfuncparams : ctx.types().T_jlfunc,
                             GlobalVariable::ExternalLinkage,
                             declarations.specFunctionObject, M);
        jl_init_function(f, ctx.emission_context.TargetTriple);
        if (needsparams)
            jl_name_jlfuncparams_args(ctx.emission_context, f);
        else
            jl_name_jlfunc_args(ctx.emission_context, f);
        f->setAttributes(AttributeList::get(ctx.builder.getContext(), {get_func_attrs(ctx.builder.getContext()), f->getAttributes()}));
        returninfo.decl = f;
        declarations.functionObject = needsparams ? "jl_fptr_sparam" : "jl_fptr_args";
    }

    if (!params.getContext().shouldDiscardValueNames() && ctx.emission_context.params->debug_info_level >= 2 && lam->def.method && jl_is_method(lam->def.method) && lam->specTypes != (jl_value_t*)jl_emptytuple_type) {
        ios_t sigbuf;
        ios_mem(&sigbuf, 0);
        jl_static_show_func_sig((JL_STREAM*) &sigbuf, (jl_value_t*)abi);
        f->addFnAttr("julia.fsig", StringRef(sigbuf.buf, sigbuf.size));
        ios_close(&sigbuf);
    }

    AttrBuilder FnAttrs(ctx.builder.getContext(), f->getAttributes().getFnAttrs());
    AttrBuilder RetAttrs(ctx.builder.getContext(), f->getAttributes().getRetAttrs());

    if (jlrettype == (jl_value_t*)jl_bottom_type)
        FnAttrs.addAttribute(Attribute::NoReturn);

#ifdef USE_POLLY
    if (!jl_has_meta(stmts, jl_polly_sym) || jl_options.polly == JL_OPTIONS_POLLY_OFF)
        FnAttrs.addAttribute(polly::PollySkipFnAttr);
#endif

    if (src->inlining == 2)
        FnAttrs.addAttribute(Attribute::NoInline);

#ifdef JL_DEBUG_BUILD
    FnAttrs.addAttribute(Attribute::StackProtectStrong);
#endif

#ifdef _COMPILER_TSAN_ENABLED_
    // TODO: enable this only when a argument like `-race` is passed to Julia
    //       add a macro for no_sanitize_thread
    FnAttrs.addAttribute(llvm::Attribute::SanitizeThread);
#endif

    // add the optimization level specified for this module, if any
    int optlevel = jl_get_module_optlevel(ctx.module);
    if (optlevel >= 0 && optlevel <= 3) {
        static const char* const optLevelStrings[] = { "0", "1", "2", "3" };
        FnAttrs.addAttribute("julia-optimization-level", optLevelStrings[optlevel]);
    }

    ctx.f = f;

    // Step 4b. determine debug info signature and other type info for locals
    DICompileUnit::DebugEmissionKind emissionKind = (DICompileUnit::DebugEmissionKind) ctx.params->debug_info_kind;
    DICompileUnit::DebugNameTableKind tableKind;
    if (JL_FEAT_TEST(ctx, gnu_pubnames))
        tableKind = DICompileUnit::DebugNameTableKind::GNU;
    else
        tableKind = DICompileUnit::DebugNameTableKind::None;
    DIBuilder dbuilder(*M, true, debug_enabled ? getOrCreateJuliaCU(*M, emissionKind, tableKind) : NULL);
    DIFile *topfile = NULL;
    DISubprogram *SP = NULL;
    DebugLoc noDbg, topdebugloc;
    if (debug_enabled) {
        topfile = dbuilder.createFile(ctx.file, ".");
        DISubroutineType *subrty;
        if (ctx.emission_context.params->debug_info_level <= 1)
            subrty = debugcache.jl_di_func_null_sig;
        else if (!specsig)
            subrty = debugcache.jl_di_func_sig;
        else
            subrty = get_specsig_di(ctx, debugcache, jlrettype, abi, dbuilder);
        SP = dbuilder.createFunction(nullptr
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
        if (ctx.emission_context.params->debug_info_level >= 2) {
            const bool AlwaysPreserve = true;
            // Go over all arguments and local variables and initialize their debug information
            for (i = 0; i < nreq; i++) {
                jl_sym_t *argname = slot_symbol(ctx, i);
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
                    julia_type_to_di(ctx, debugcache, varinfo.value.typ, &dbuilder, false),
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
                    julia_type_to_di(ctx, debugcache, ctx.slots[ctx.vaSlot].value.typ, &dbuilder, false),
                    AlwaysPreserve,                     // May be deleted if optimized out
                    DINode::FlagZero);                  // Flags (TODO: Do we need any)
            }
            for (i = 0; i < vinfoslen; i++) {
                jl_sym_t *s = slot_symbol(ctx, i);
                jl_varinfo_t &varinfo = ctx.slots[i];
                if (varinfo.isArgument || s == jl_empty_sym || s == jl_unused_sym)
                    continue;
                // LLVM 4.0: Assume the variable has default alignment
                varinfo.dinfo = dbuilder.createAutoVariable(
                    SP,                      // Scope (current function will be fill in later)
                    jl_symbol_name(s),       // Variable name
                    topfile,                 // File
                    toplineno == -1 ? 0 : toplineno, // Line (for now, use lineno of the function)
                    julia_type_to_di(ctx, debugcache, varinfo.value.typ, &dbuilder, false), // Variable type
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
        setName(ctx.emission_context, pargArray, "stackargs");
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
    Value *world_age_field = NULL;
    if (ctx.is_opaque_closure) {
        world_age_field = get_tls_world_age_field(ctx);
        jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe);
        last_age = ai.decorateInst(ctx.builder.CreateAlignedLoad(
                   ctx.types().T_size, world_age_field, ctx.types().alignof_ptr));
    }

    // step 7. allocate local variables slots
    // must be in the first basic block for the llvm mem2reg pass to work
    auto allocate_local = [&ctx, &dbuilder, &debugcache, topdebugloc, va, debug_enabled](jl_varinfo_t &varinfo, jl_sym_t *s, int i) {
        jl_value_t *jt = varinfo.value.typ;
        assert(!varinfo.boxroot); // variables shouldn't have memory locs already
        if (varinfo.value.constant) {
            // no need to explicitly load/store a constant/ghost value
            alloc_def_flag(ctx, varinfo);
            return;
        }
        else if (varinfo.isArgument && (!va || ctx.vaSlot == -1 || i != ctx.vaSlot)) {
            // just use the input pointer, if we have it
            // (we will need to attach debuginfo later to it)
            return;
        }
        else if (jl_is_uniontype(jt)) {
            bool allunbox;
            size_t align, nbytes;
            Value *lv = try_emit_union_alloca(ctx, (jl_uniontype_t*)jt, allunbox, align, nbytes);
            if (lv) {
                lv->setName(jl_symbol_name(s));
                varinfo.value = mark_julia_slot(lv, jt, NULL, ctx.tbaa().tbaa_stack);
                varinfo.pTIndex = emit_static_alloca(ctx, 1, Align(1));
                setName(ctx.emission_context, varinfo.pTIndex, "tindex");
                // TODO: attach debug metadata to this variable
            }
            else if (allunbox) {
                // all ghost values just need a selector allocated
                AllocaInst *lv = emit_static_alloca(ctx, 1, Align(1));
                lv->setName(jl_symbol_name(s));
                varinfo.pTIndex = lv;
                varinfo.value.tbaa = NULL;
                varinfo.value.isboxed = false;
                // TODO: attach debug metadata to this variable
            }
            if (lv || allunbox)
                alloc_def_flag(ctx, varinfo);
            if (allunbox)
                return;
        }
        else if (deserves_stack(jt)) {
            auto sizes = split_value_size((jl_datatype_t*)jt);
            AllocaInst *bits = sizes.first > 0 ? emit_static_alloca(ctx, sizes.first, Align(julia_alignment(jt))) : nullptr;
            AllocaInst *roots = sizes.second > 0 ? emit_static_roots(ctx, sizes.second) : nullptr;
            if (bits) bits->setName(jl_symbol_name(s));
            if (roots) roots->setName(StringRef(".roots.") + jl_symbol_name(s));
            varinfo.value = mark_julia_slot(bits, jt, NULL, ctx.tbaa().tbaa_stack, None);
            varinfo.inline_roots = roots;
            alloc_def_flag(ctx, varinfo);
            if (debug_enabled && varinfo.dinfo) {
                assert((Metadata*)varinfo.dinfo->getType() != debugcache.jl_pvalue_dillvmt);
                dbuilder.insertDeclare(bits ? bits : roots, varinfo.dinfo, dbuilder.createExpression(),
                                       topdebugloc,
                                       ctx.builder.GetInsertBlock());
            }
            return;
        }
        // otherwise give it a boxroot in this function
        AllocaInst *av = emit_static_roots(ctx, 1);
        av->setName(jl_symbol_name(s));
        varinfo.boxroot = av;
        if (debug_enabled && varinfo.dinfo) {
            SmallVector<uint64_t, 1> addr;
            DIExpression *expr;
            if ((Metadata*)varinfo.dinfo->getType() != debugcache.jl_pvalue_dillvmt)
                addr.push_back(llvm::dwarf::DW_OP_deref);
            expr = dbuilder.createExpression(addr);
            dbuilder.insertDeclare(av, varinfo.dinfo, expr,
                                        topdebugloc,
                            ctx.builder.GetInsertBlock());
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
        allocate_local(varinfo, s, (int)i);
    }

    std::map<int, int> upsilon_to_phic;

    // Scan for PhiC nodes, emit their slots and record which upsilon nodes
    // yield to them.
    // Also count ssavalue uses.
    {
        for (size_t i = 0; i < jl_array_nrows(stmts); ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);

            auto scan_ssavalue = [&](jl_value_t *val) {
                if (jl_is_ssavalue(val)) {
                    size_t ssa_idx = ((jl_ssavalue_t*)val)->id-1;
                    /*
                     * We technically allow out of bounds SSAValues in dead IR, so make
                     * sure to bounds check this here. It's still not *good* to leave
                     * dead code in the IR, because this will conservatively overcount
                     * it, but let's at least make it not crash.
                     */
                    if (ssa_idx < ctx.ssavalue_usecount.size()) {
                        ctx.ssavalue_usecount[ssa_idx] += 1;
                    }
                    return true;
                }
                return false;
            };
            general_use_analysis(ctx, stmt, scan_ssavalue);

            if (jl_is_phicnode(stmt)) {
                jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(stmt, 0);
                for (size_t j = 0; j < jl_array_nrows(values); ++j) {
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
                allocate_local(vi, jl_symbol("phic"), -1);
            }
        }
    }

    // step 8. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    SmallVector<AttributeSet, 0> attrs(f->arg_size()); // function declaration attributes

    auto get_specsig_arg = [&](jl_value_t *argType, Type *llvmArgType, bool isboxed) {
        if (type_is_ghost(llvmArgType)) { // this argument is not actually passed
            return ghostValue(ctx, argType);
        }
        else if (is_uniquerep_Type(argType)) {
            return mark_julia_const(ctx, jl_tparam0(argType));
        }
        Argument *Arg = &*AI;
        ++AI;
        AttrBuilder param(ctx.builder.getContext(), f->getAttributes().getParamAttrs(Arg->getArgNo()));
        jl_cgval_t theArg;
        if (!isboxed && llvmArgType->isAggregateType()) {
            maybe_mark_argument_dereferenceable(param, argType);
            SmallVector<Value*,0> roots;
            auto tracked = CountTrackedPointers(llvmArgType);
            if (tracked.count && !tracked.all) {
                roots = load_gc_roots(ctx, &*AI, tracked.count);
                ++AI;
            }
            theArg = mark_julia_slot(Arg, argType, NULL, ctx.tbaa().tbaa_const, roots); // this argument is by-pointer
        }
        else {
            if (isboxed)
                maybe_mark_argument_dereferenceable(param, argType);
            theArg = mark_julia_type(ctx, Arg, isboxed, argType);
            if (theArg.tbaa == ctx.tbaa().tbaa_immut)
                theArg.tbaa = ctx.tbaa().tbaa_const;
        }
        attrs[Arg->getArgNo()] = AttributeSet::get(Arg->getContext(), param); // function declaration attributes
        return theArg;
    };

    if (has_sret) {
        Argument *Arg = &*AI;
        ++AI;
        AttrBuilder param(ctx.builder.getContext(), f->getAttributes().getParamAttrs(Arg->getArgNo()));
        if (returninfo.cc == jl_returninfo_t::Union) {
            param.addAttribute(Attribute::NonNull);
            // The `dereferenceable` below does not imply `nonnull` for non addrspace(0) pointers.
            param.addDereferenceableAttr(returninfo.union_bytes);
            param.addAlignmentAttr(returninfo.union_align);
        }
        else {
            const DataLayout &DL = jl_Module->getDataLayout();
            Type *RT = Arg->getParamStructRetType();
            TypeSize sz = DL.getTypeAllocSize(RT);
            Align al = DL.getPrefTypeAlign(RT);
            if (al > MAX_ALIGN)
                al = Align(MAX_ALIGN);
            param.addAttribute(Attribute::NonNull);
            // The `dereferenceable` below does not imply `nonnull` for non addrspace(0) pointers.
            param.addDereferenceableAttr(sz);
            param.addAlignmentAttr(al);
        }
        attrs[Arg->getArgNo()] = AttributeSet::get(Arg->getContext(), param); // function declaration attributes
    }
    if (returninfo.return_roots) {
        Argument *Arg = &*AI;
        ++AI;
        AttrBuilder param(ctx.builder.getContext(), f->getAttributes().getParamAttrs(Arg->getArgNo()));
        param.addAttribute(Attribute::NonNull);
        // The `dereferenceable` below does not imply `nonnull` for non addrspace(0) pointers.
        size_t size = returninfo.return_roots * sizeof(jl_value_t*);
        param.addDereferenceableAttr(size);
        param.addAlignmentAttr(Align(sizeof(jl_value_t*)));
        attrs[Arg->getArgNo()] = AttributeSet::get(Arg->getContext(), param); // function declaration attributes
    }
    if (specsig && JL_FEAT_TEST(ctx, gcstack_arg)){
        Argument *Arg = &*AI;
        ++AI;
        AttrBuilder param(ctx.builder.getContext());
        attrs[Arg->getArgNo()] = AttributeSet::get(Arg->getContext(), param);
    }
    for (i = 0; i < nreq && i < vinfoslen; i++) {
        jl_sym_t *s = slot_symbol(ctx, i);
        jl_varinfo_t &vi = ctx.slots[i];
        jl_cgval_t theArg;
        if (i == 0 && ctx.is_opaque_closure) {
            // If this is an opaque closure, implicitly load the env and switch
            // the world age. The specTypes value is wrong for this field, so
            // this needs to be handled first.
            // jl_value_t *oc_type = get_oc_type(calltype, rettype);
            Value *oc_this = decay_derived(ctx, &*AI);
            ++AI; // both specsig (derived) and fptr1 (box) pass this argument as a distinct argument
            // Load closure world
            Value *worldaddr = emit_ptrgep(ctx, oc_this, offsetof(jl_opaque_closure_t, world));
            Align alignof_ptr(ctx.types().alignof_ptr);
            jl_cgval_t closure_world = typed_load(ctx, worldaddr, NULL, (jl_value_t*)jl_long_type,
                nullptr, nullptr, false, AtomicOrdering::NotAtomic, false, alignof_ptr.value());
            assert(ctx.world_age_at_entry == nullptr);
            ctx.world_age_at_entry = closure_world.V; // The tls world in a OC is the world of the closure
            emit_unbox_store(ctx, closure_world, world_age_field, ctx.tbaa().tbaa_gcframe, alignof_ptr, alignof_ptr);

            if (s == jl_unused_sym || vi.value.constant)
                continue;

            // Load closure env, which is always a boxed value (usually some Tuple) currently
            Value *envaddr = emit_ptrgep(ctx, oc_this, offsetof(jl_opaque_closure_t, captures));
            theArg = typed_load(ctx, envaddr, NULL, (jl_value_t*)vi.value.typ,
                nullptr, nullptr, /*isboxed*/true, AtomicOrdering::NotAtomic, false, sizeof(void*));
        }
        else {
            jl_value_t *argType = jl_nth_slot_type(abi, i);
            // TODO: jl_nth_slot_type should call jl_rewrap_unionall?
            //  specTypes is required to be a datatype by construction for specsig, but maybe not otherwise
            bool isboxed = deserves_argbox(argType);
            Type *llvmArgType = NULL;
            llvmArgType = isboxed ? ctx.types().T_prjlvalue : julia_type_to_llvm(ctx, argType);
            if (s == jl_unused_sym || vi.value.constant) {
                assert(vi.boxroot == NULL);
                if (specsig && !type_is_ghost(llvmArgType) && !is_uniquerep_Type(argType)) {
                    ++AI;
                    auto tracked = CountTrackedPointers(llvmArgType);
                    if (tracked.count && !tracked.all)
                        ++AI;
                }
                continue;
            }
            if (specsig) {
                theArg = get_specsig_arg(argType, llvmArgType, isboxed);
            }
            else {
                if (i == 0) {
                    // first (function) arg is separate in jlcall
                    theArg = mark_julia_type(ctx, fArg, true, vi.value.typ);
                }
                else {
                    Value *argPtr = emit_ptrgep(ctx, argArray, (i - 1) * ctx.types().sizeof_ptr);
                    jl_aliasinfo_t ai = jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_const);
                    Value *load = ai.decorateInst(maybe_mark_load_dereferenceable(
                            ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, argPtr, Align(sizeof(void*))),
                            false, vi.value.typ));
                    theArg = mark_julia_type(ctx, load, true, vi.value.typ);
                    if (debug_enabled && vi.dinfo && !vi.boxroot) {
                        SmallVector<uint64_t, 8> addr;
                        addr.push_back(llvm::dwarf::DW_OP_deref);
                        addr.push_back(llvm::dwarf::DW_OP_plus_uconst);
                        addr.push_back((i - 1) * sizeof(void*));
                        if ((Metadata*)vi.dinfo->getType() != debugcache.jl_pvalue_dillvmt)
                            addr.push_back(llvm::dwarf::DW_OP_deref);
                        dbuilder.insertDeclare(pargArray, vi.dinfo, dbuilder.createExpression(addr),
                                        topdebugloc,
                                        ctx.builder.GetInsertBlock());
                    }
                }
            }
        }

        if (vi.boxroot == nullptr) {
            assert(vi.value.V == nullptr && vi.inline_roots == nullptr && "unexpected variable slot created for argument");
            // keep track of original (possibly boxed) value to avoid re-boxing or moving
            vi.value = theArg;
            if (debug_enabled && vi.dinfo && theArg.V) {
                if (!theArg.inline_roots.empty() || theArg.ispointer()) {
                    dbuilder.insertDeclare(theArg.V, vi.dinfo, dbuilder.createExpression(),
                                            topdebugloc, ctx.builder.GetInsertBlock());
                }
                else {
                    dbuilder.insertDbgValueIntrinsic(theArg.V, vi.dinfo, dbuilder.createExpression(),
                                                        topdebugloc, ctx.builder.GetInsertBlock());
                }
            }
        }
        else {
            Value *argp = boxed(ctx, theArg);
            ctx.builder.CreateStore(argp, vi.boxroot);
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
            ctx.nvargs = jl_nparams(abi) - nreq;
            SmallVector<jl_cgval_t, 0> vargs(ctx.nvargs);
            for (size_t i = nreq; i < jl_nparams(abi); ++i) {
                jl_value_t *argType = jl_nth_slot_type(abi, i);
                // n.b. specTypes is required to be a datatype by construction for specsig
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
                    vargs, ctx.nvargs, julia_call);
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
                          emit_ptrgep(ctx, argArray, (nreq - 1) * sizeof(jl_value_t*)),
                          ctx.builder.CreateSub(argCount, ctx.builder.getInt32(nreq - 1)) });
            restTuple->setAttributes(F->getAttributes());
            ctx.builder.CreateStore(restTuple, vi.boxroot);
        }
    }

    AttributeList attributes = AttributeList::get(ctx.builder.getContext(), AttributeSet::get(f->getContext(), FnAttrs), AttributeSet::get(f->getContext(), RetAttrs), attrs);
    // attributes should be a superset of f->getAttributes() based on how we constructed it, but we merge just in case it isn't
    f->setAttributes(AttributeList::get(ctx.builder.getContext(), {attributes, f->getAttributes()}));

    // step 10. Compute properties for each statements
    //     This needs to be computed by iterating in the IR order
    //     instead of control flow order.
    auto in_user_mod = [] (jl_module_t *mod) {
        return (!jl_is_submodule(mod, jl_base_module) &&
                !jl_is_submodule(mod, jl_core_module));
    };
    auto in_tracked_path = [] (StringRef file) { // falls within an explicitly set file or directory
        return jl_options.tracked_path != NULL && file.starts_with(jl_options.tracked_path);
    };
    bool mod_is_user_mod = in_user_mod(ctx.module);
    bool mod_is_tracked = in_tracked_path(ctx.file);
    struct DebugLineTable {
        DebugLoc loc;
        StringRef file;
        ssize_t line;
        ssize_t line0; // if this represents pc=1, then also cover the entry to the function (pc=0)
        bool is_user_code;
        int32_t edgeid;
        bool sameframe(const DebugLineTable &other) const {
            // detect if the line info for this frame is unchanged (equivalent to loc == other.loc ignoring the inlined_at field)
            return other.edgeid == edgeid && other.line == line;
        };
    };
    DebugLineTable topinfo;
    topinfo.file = ctx.file;
    topinfo.line = toplineno;
    topinfo.line0 = 0;
    topinfo.is_user_code = mod_is_user_mod;
    topinfo.loc = topdebugloc;
    topinfo.edgeid = 0;
    std::map<std::tuple<StringRef, StringRef>, DISubprogram*> subprograms;
    SmallVector<DebugLineTable, 0> prev_lineinfo, new_lineinfo;
    auto update_lineinfo = [&] (size_t pc) {
        std::function<bool(jl_debuginfo_t*, jl_value_t*, size_t, size_t)> append_lineinfo =
                [&] (jl_debuginfo_t *debuginfo, jl_value_t *func, size_t to, size_t pc) -> bool {
            while (1) {
                if (!jl_is_symbol(debuginfo->def)) // this is a path
                    func = debuginfo->def; // this is inlined
                struct jl_codeloc_t lineidx = jl_uncompress1_codeloc(debuginfo->codelocs, pc);
                size_t i = lineidx.line;
                if (i < 0) // pc out of range: broken debuginfo?
                    return false;
                if (i == 0 && lineidx.to == 0) // no update
                    return false;
                if (pc > 0 && (jl_value_t*)debuginfo->linetable != jl_nothing) {
                    // indirection node
                    if (!append_lineinfo(debuginfo->linetable, func, to, i))
                        return false; // no update
                }
                else {
                    // actual node
                    DebugLineTable info;
                    info.edgeid = to;
                    jl_module_t *modu = func ? jl_debuginfo_module1(func) : NULL;
                    if (modu == NULL)
                        modu = ctx.module;
                    info.file = jl_debuginfo_file1(debuginfo);
                    info.line = i;
                    info.line0 = 0;
                    if (pc == 1) {
                        struct jl_codeloc_t lineidx = jl_uncompress1_codeloc(debuginfo->codelocs, 0);
                        assert(lineidx.to == 0 && lineidx.pc == 0);
                        if (lineidx.line > 0 && info.line != lineidx.line)
                            info.line0 = lineidx.line;
                    }
                    if (info.file.empty())
                        info.file = "<missing>";
                    if (modu == ctx.module)
                        info.is_user_code = mod_is_user_mod;
                    else
                        info.is_user_code = in_user_mod(modu);
                    if (debug_enabled) {
                        StringRef fname = jl_debuginfo_name(func);
                        if (new_lineinfo.empty() && info.file == ctx.file) { // if everything matches, emit a toplevel line number
                            info.loc = DILocation::get(ctx.builder.getContext(), info.line, 0, SP, NULL);
                        }
                        else { // otherwise, describe this as an inlining frame
                            DebugLoc inl_loc = new_lineinfo.empty() ? DebugLoc(DILocation::get(ctx.builder.getContext(), 0, 0, SP, NULL)) : new_lineinfo.back().loc;
                            DISubprogram *&inl_SP = subprograms[std::make_tuple(fname, info.file)];
                            if (inl_SP == NULL) {
                                DIFile *difile = dbuilder.createFile(info.file, ".");
                                inl_SP = dbuilder.createFunction(difile
                                                             ,std::string(fname) + ";" // Name
                                                             ,fname            // LinkageName
                                                             ,difile           // File
                                                             ,0                // LineNo
                                                             ,debugcache.jl_di_func_null_sig // Ty
                                                             ,0                // ScopeLine
                                                             ,DINode::FlagZero // Flags
                                                             ,DISubprogram::SPFlagDefinition | DISubprogram::SPFlagOptimized // SPFlags
                                                             ,nullptr          // Template Parameters
                                                             ,nullptr          // Template Declaration
                                                             ,nullptr          // ThrownTypes
                                                             );
                            }
                            info.loc = DILocation::get(ctx.builder.getContext(), info.line, 0, inl_SP, inl_loc);
                        }
                    }
                    new_lineinfo.push_back(info);
                }
                to = lineidx.to;
                if (to == 0)
                    return true;
                pc = lineidx.pc;
                debuginfo = (jl_debuginfo_t*)jl_svecref(debuginfo->edges, to - 1);
                func = NULL;
            }
        };
        prev_lineinfo.resize(0);
        std::swap(prev_lineinfo, new_lineinfo);
        bool updated = append_lineinfo(src->debuginfo, (jl_value_t*)lam, 0, pc + 1);
        if (!updated)
            std::swap(prev_lineinfo, new_lineinfo);
        else
            assert(new_lineinfo.size() > 0);
        return updated;
    };

    SmallVector<MDNode*, 0> aliasscopes;
    MDNode* current_aliasscope = nullptr;
    SmallVector<Metadata*, 0> scope_stack;
    SmallVector<MDNode*, 0> scope_list_stack;
    {
        size_t nstmts = jl_array_nrows(stmts);
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

    // step 11a. Emit the entry safepoint
    if (JL_FEAT_TEST(ctx, safepoint_on_entry))
        emit_gc_safepoint(ctx.builder, ctx.types().T_size, get_current_ptls(ctx), ctx.tbaa().tbaa_const);

    // step 11b. Do codegen in control flow order
    SmallVector<int, 0> workstack;
    DenseMap<size_t, BasicBlock*> BB;
    DenseMap<size_t, BasicBlock*> come_from_bb;
    int cursor = 0;
    int current_label = 0;
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
                // Not a BB
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
                // New BB
                ctx.builder.SetInsertPoint(nextbb->second);
                cursor = item;
                current_label = item;
                return;
            }
        }
        cursor = -1;
    };

    // If a pkgimage or sysimage is being generated, disable tracking.
    // This means sysimage build or pkgimage precompilation workloads aren't tracked.
    auto do_coverage = [&] (bool in_user_code, bool is_tracked) {
        return (jl_generating_output() == 0 &&
                (coverage_mode == JL_LOG_ALL ||
                (in_user_code && coverage_mode == JL_LOG_USER) ||
                (is_tracked && coverage_mode == JL_LOG_PATH)));
    };
    auto do_malloc_log = [&] (bool in_user_code, bool is_tracked) {
        return (jl_generating_output() == 0 &&
                (malloc_log_mode == JL_LOG_ALL ||
                (in_user_code && malloc_log_mode == JL_LOG_USER) ||
                (is_tracked && malloc_log_mode == JL_LOG_PATH)));
    };
    auto coverageVisitStmt = [&] () {
        // Visit frames which differ from previous statement as tracked in
        // prev_lineinfo (tracked outer frame first).
        size_t dbg;
        for (dbg = 0; dbg < new_lineinfo.size(); dbg++) {
            if (dbg >= prev_lineinfo.size() || !new_lineinfo[dbg].sameframe(prev_lineinfo[dbg]))
                break;
        }
        for (; dbg < new_lineinfo.size(); dbg++) {
            const auto &newdbg = new_lineinfo[dbg];
            bool is_tracked = in_tracked_path(newdbg.file);
            if (do_coverage(newdbg.is_user_code, is_tracked)) {
                if (newdbg.line0 != 0 && (dbg >= prev_lineinfo.size() || newdbg.edgeid != prev_lineinfo[dbg].edgeid || newdbg.line0 != prev_lineinfo[dbg].line))
                    coverageVisitLine(ctx, newdbg.file, newdbg.line0);
                coverageVisitLine(ctx, newdbg.file, newdbg.line);
            }
        }
    };
    auto mallocVisitStmt = [&] (Value *sync, bool have_dbg_update) {
        if (!do_malloc_log(mod_is_user_mod, mod_is_tracked) || !have_dbg_update) {
            // TODD: add || new_lineinfo[0].sameframe(prev_lineinfo[0])) above, but currently this breaks the test for it (by making an optimization better)
            if (do_malloc_log(true, mod_is_tracked) && sync)
                ctx.builder.CreateCall(prepare_call(sync_gc_total_bytes_func), {sync});
            return;
        }
        mallocVisitLine(ctx, new_lineinfo[0].file, new_lineinfo[0].line, sync);
    };
    if (coverage_mode != JL_LOG_NONE) {
        // record all lines that could be covered
        std::function<void(jl_debuginfo_t *debuginfo, jl_value_t *func)> record_line_exists = [&](jl_debuginfo_t *debuginfo, jl_value_t *func) {
            if (!jl_is_symbol(debuginfo->def)) // this is a path
                func = debuginfo->def; // this is inlined
            for (size_t i = 0; i < jl_svec_len(debuginfo->edges); i++) {
                jl_debuginfo_t *edge = (jl_debuginfo_t*)jl_svecref(debuginfo->edges, i);
                record_line_exists(edge, NULL);
            }
            while ((jl_value_t*)debuginfo->linetable != jl_nothing)
                debuginfo = debuginfo->linetable;
            jl_module_t *modu = func ? jl_debuginfo_module1(func) : NULL;
            if (modu == NULL)
                modu = ctx.module;
            StringRef file = jl_debuginfo_file1(debuginfo);
            if (file.empty())
                file = "<missing>";
            bool is_user_code;
            if (modu == ctx.module)
                is_user_code = mod_is_user_mod;
            else
                is_user_code = in_user_mod(modu);
            bool is_tracked = in_tracked_path(file);
            if (do_coverage(is_user_code, is_tracked)) {
                for (size_t pc = 0; 1; pc++) {
                    struct jl_codeloc_t lineidx = jl_uncompress1_codeloc(debuginfo->codelocs, pc);
                    if (lineidx.line == -1)
                        break;
                    if (lineidx.line > 0)
                        jl_coverage_alloc_line(file, lineidx.line);
                }
            }
        };
        record_line_exists(src->debuginfo, (jl_value_t*)lam);
    }

    come_from_bb[0] = ctx.builder.GetInsertBlock();

    for (int label : branch_targets) {
        BasicBlock *bb = BasicBlock::Create(ctx.builder.getContext(),
            "L" + std::to_string(label), f);
        BB[label] = bb;
    }

    new_lineinfo.push_back(topinfo);
    Value *sync_bytes = nullptr;
    if (do_malloc_log(true, mod_is_tracked))
        sync_bytes = ctx.builder.CreateCall(prepare_call(diff_gc_total_bytes_func), {});
    // coverage for the function definition line number (topinfo)
    coverageVisitStmt();

    find_next_stmt(0);
    while (cursor != -1) {
        bool have_dbg_update = update_lineinfo(cursor);
        if (have_dbg_update) {
            if (debug_enabled)
                ctx.builder.SetCurrentDebugLocation(new_lineinfo.back().loc);
            coverageVisitStmt();
        }
        ctx.noalias().aliasscope.current = aliasscopes[cursor];
        jl_value_t *stmt = jl_array_ptr_ref(stmts, cursor);
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

            if (ctx.is_opaque_closure) {
                emit_typecheck(ctx, retvalinfo, jlrettype, "OpaqueClosure");
            }

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
                            ctx.builder.CreateAnd(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
                            ConstantInt::get(getInt8Ty(ctx.builder.getContext()), 0));
                        data = ctx.builder.CreateSelect(isboxed_union, retvalinfo.Vboxed, data);
                    }
                }
                else {
                    // treat this as a simple boxed returninfo
                    //assert(retvalinfo.isboxed);
                    tindex = compute_tindex_unboxed(ctx, retvalinfo, jlrettype);
                    tindex = ctx.builder.CreateOr(tindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
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
                Align align(returninfo.union_align);
                if (!returninfo.return_roots && !retvalinfo.inline_roots.empty()) {
                    assert(retvalinfo.V == nullptr);
                    assert(returninfo.cc == jl_returninfo_t::SRet);
                    split_value_into(ctx, retvalinfo, align, nullptr, align,
                            jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack), sret, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe));
                }
                else if (returninfo.return_roots) {
                    assert(returninfo.cc == jl_returninfo_t::SRet);
                    Value *return_roots = f->arg_begin() + 1;
                    split_value_into(ctx, retvalinfo, align, sret, align,
                            jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack), return_roots, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe));
                }
                else if (retvalinfo.ispointer()) {
                    if (returninfo.cc == jl_returninfo_t::SRet) {
                        assert(jl_is_concrete_type(jlrettype));
                        emit_memcpy(ctx, sret, jl_aliasinfo_t::fromTBAA(ctx, nullptr), retvalinfo,
                                    jl_datatype_size(jlrettype), align, align);
                    }
                    else { // must be jl_returninfo_t::Union
                        emit_unionmove(ctx, sret, nullptr, retvalinfo, /*skip*/isboxed_union);
                    }
                }
                else {
                    ctx.builder.CreateAlignedStore(retvalinfo.V, sret, align);
                    assert(retvalinfo.TIndex == NULL && "unreachable"); // unimplemented representation
                }
            }

            mallocVisitStmt(sync_bytes, have_dbg_update);
            // N.B.: For toplevel thunks, we expect world age restore to be handled
            // by the interpreter which invokes us.
            if (ctx.is_opaque_closure)
                ctx.builder.CreateStore(last_age, world_age_field);
            assert(type_is_ghost(retty) || returninfo.cc == jl_returninfo_t::SRet ||
                retval->getType() == ctx.f->getReturnType());
            ctx.builder.CreateRet(retval);
            find_next_stmt(-1);
            continue;
        }
        if (jl_is_gotonode(stmt)) {
            int lname = jl_gotonode_label(stmt);
            come_from_bb[cursor+1] = ctx.builder.GetInsertBlock();
            auto br = ctx.builder.CreateBr(BB[lname]);
            // Check if backwards branch
            if (ctx.LoopID && lname <= current_label) {
                br->setMetadata(LLVMContext::MD_loop, ctx.LoopID);
                ctx.LoopID = NULL;
            }
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
            mallocVisitStmt(nullptr, have_dbg_update);
            come_from_bb[cursor+1] = ctx.builder.GetInsertBlock();
            workstack.push_back(lname - 1);
            BasicBlock *ifnot = BB[lname];
            BasicBlock *ifso = BB[cursor+2];
            Instruction *br;
            if (ifnot == ifso)
                br = ctx.builder.CreateBr(ifnot);
            else
                br = ctx.builder.CreateCondBr(isfalse, ifnot, ifso);

            // Check if backwards branch
            if (ctx.LoopID && lname <= current_label) {
                br->setMetadata(LLVMContext::MD_loop, ctx.LoopID);
                ctx.LoopID = NULL;
            }
            find_next_stmt(cursor + 1);
            continue;
        }
        else if (jl_is_enternode(stmt)) {
            int lname = jl_enternode_catch_dest(stmt);
            if (lname) {
                // Save exception stack depth at enter for use in pop_exception
                Value *excstack_state =
                    ctx.builder.CreateCall(prepare_call(jl_excstack_state_func), {get_current_task(ctx)});
                assert(!ctx.ssavalue_assigned[cursor]);
                ctx.SAvalues[cursor] = jl_cgval_t(excstack_state, (jl_value_t*)jl_ulong_type, NULL);
                ctx.ssavalue_assigned[cursor] = true;
                // Actually enter the exception frame
                auto ct = get_current_task(ctx);
                CallInst *sj = ctx.builder.CreateCall(prepare_call(except_enter_func), {ct});
                // We need to mark this on the call site as well. See issue #6757
                sj->setCanReturnTwice();
                Value *isz = ctx.builder.CreateICmpEQ(ctx.builder.CreateExtractValue(sj, 0), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 0));
                Value *ehbuf = ctx.builder.CreateExtractValue(sj, 1);
                BasicBlock *tryblk = BasicBlock::Create(ctx.builder.getContext(), "try", f);
                BasicBlock *catchpop = BasicBlock::Create(ctx.builder.getContext(), "catch_pop", f);
                BasicBlock *handlr = NULL;
                handlr = BB[lname];
                workstack.push_back(lname - 1);
                come_from_bb[cursor + 1] = ctx.builder.GetInsertBlock();
                ctx.builder.CreateCondBr(isz, tryblk, catchpop);
                ctx.builder.SetInsertPoint(catchpop);
                {
                    ctx.builder.CreateCall(prepare_call(jlleave_func), {get_current_task(ctx), ConstantInt::get(getInt32Ty(ctx.builder.getContext()), 1)});
                    ctx.builder.CreateBr(handlr);
                }
                ctx.builder.SetInsertPoint(tryblk);
                auto ehptr = emit_ptrgep(ctx, ct, offsetof(jl_task_t, eh));
                ctx.builder.CreateAlignedStore(ehbuf, ehptr, ctx.types().alignof_ptr);
            }
            // For the two-arg version of :enter, twiddle the scope
            if (jl_enternode_scope(stmt)) {
                jl_cgval_t scope = emit_expr(ctx, jl_enternode_scope(stmt));
                if (scope.typ == jl_bottom_type) {
                    // Probably dead code, but let's be loud about it in case it isn't, so we fail
                    // at the point of the miscompile, rather than later when something attempts to
                    // read the scope.
                    emit_error(ctx, "(INTERNAL ERROR): Attempted to execute EnterNode with bad scope");
                    find_next_stmt(-1);
                    continue;
                }
                Value *scope_boxed = boxed(ctx, scope);
                Value *scope_ptr = get_scope_field(ctx);
                LoadInst *current_scope = ctx.builder.CreateAlignedLoad(ctx.types().T_prjlvalue, scope_ptr, ctx.types().alignof_ptr);
                StoreInst *scope_store = ctx.builder.CreateAlignedStore(scope_boxed, scope_ptr, ctx.types().alignof_ptr);
                jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe).decorateInst(current_scope);
                jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_gcframe).decorateInst(scope_store);
                // GC preserve the scope, since it is not rooted in the `jl_handler_t *`
                // and may be removed from jl_current_task by any nested block and then
                // replaced later
                Value *scope_token = ctx.builder.CreateCall(prepare_call(gc_preserve_begin_func), {scope_boxed});
                ctx.scope_restore[cursor] = std::make_pair(scope_token, current_scope);
            }
        }
        else {
            emit_stmtpos(ctx, stmt, cursor);
            mallocVisitStmt(nullptr, have_dbg_update);
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
    SmallVector<llvm::PHINode*, 0> ToDelete;
    for (auto &tup : ctx.PhiNodes) {
        jl_cgval_t phi_result;
        PHINode *VN;
        jl_value_t *r;
        AllocaInst *dest;
        SmallVector<PHINode*,0> roots;
        BasicBlock *PhiBB;
        std::tie(phi_result, PhiBB, dest, VN, roots, r) = tup;
        jl_value_t *phiType = phi_result.typ;
        jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(r, 0);
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(r, 1);
        PHINode *TindexN = cast_or_null<PHINode>(phi_result.TIndex);
        DenseSet<BasicBlock*> preds;
        for (size_t i = 0; i < jl_array_nrows(edges); ++i) {
            size_t edge = jl_array_data(edges, int32_t)[i];
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
                    size_t j_edge = jl_array_data(edges, int32_t)[j];
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
                   FromBB->getName() + "." + PhiBB->getName() + "_crit_edge", FromBB->getParent(), FromBB->getNextNode()); // insert after existing block
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
                if (VN) {
                    assert(roots.empty() && !dest);
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
                else if ((dest || !roots.empty()) && val.typ != (jl_value_t*)jl_bottom_type) {
                    // must be careful to emit undef here (rather than a bitcast or
                    // load of val) if the runtime type of val isn't phiType
                    auto tracked = split_value_size((jl_datatype_t*)phiType).second;
                    Value *isvalid = emit_isa_and_defined(ctx, val, phiType);
                    assert(roots.size() == tracked && isvalid != nullptr);
                    SmallVector<Value*,0> incomingroots(0);
                    if (tracked)
                        incomingroots.resize(tracked, Constant::getNullValue(ctx.types().T_prjlvalue));
                    emit_guarded_test(ctx, isvalid, incomingroots, [&] {
                        jl_cgval_t typedval = update_julia_type(ctx, val, phiType);
                        SmallVector<Value*,0> mayberoots(tracked, Constant::getNullValue(ctx.types().T_prjlvalue));
                        if (typedval.typ != jl_bottom_type) {
                            Align align(julia_alignment(phiType));
                            if (tracked)
                                split_value_into(ctx, typedval, align, dest, align, jl_aliasinfo_t::fromTBAA(ctx, ctx.tbaa().tbaa_stack), mayberoots);
                            else
                                emit_unbox_store(ctx, typedval, dest, ctx.tbaa().tbaa_stack, align, align);
                        }
                        return mayberoots;
                    });
                    for (size_t nr = 0; nr < tracked; nr++)
                        roots[nr]->addIncoming(incomingroots[nr], ctx.builder.GetInsertBlock());
                }
                else if (!roots.empty()) {
                    Value *V = Constant::getNullValue(ctx.types().T_prjlvalue);
                    for (size_t nr = 0; nr < roots.size(); nr++)
                        roots[nr]->addIncoming(V, ctx.builder.GetInsertBlock());
                }
            }
            else {
                Value *RTindex;
                // The branch below is a bit too complex for GCC to realize that
                // `V` is always initialized when it is used.
                // Ref https://gcc.gnu.org/bugzilla/show_bug.cgi?id=96629
                Value *V = nullptr;
                assert(roots.empty());
                if (val.typ == (jl_value_t*)jl_bottom_type) {
                    if (VN)
                        V = undef_value_for_type(VN->getType());
                    RTindex = UndefValue::get(getInt8Ty(ctx.builder.getContext()));
                }
                else if (jl_is_concrete_type(val.typ) || val.constant) {
                    size_t tindex = get_box_tindex((jl_datatype_t*)(val.constant ? jl_typeof(val.constant) : val.typ), phiType);
                    if (tindex == 0) {
                        if (VN)
                            V = boxed(ctx, val);
                        RTindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER);
                    }
                    else {
                        if (VN)
                            V = Constant::getNullValue(ctx.types().T_prjlvalue);
                        if (dest) {
                            Align align(julia_alignment(val.typ));
                            emit_unbox_store(ctx, val, dest, ctx.tbaa().tbaa_stack, align, align);
                        }
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
                            RTindex = ctx.builder.CreateOr(RTindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER));
                        }
                        new_union.TIndex = RTindex;
                    }
                    if (VN)
                        V = new_union.Vboxed ? new_union.Vboxed : Constant::getNullValue(ctx.types().T_prjlvalue);
                    if (dest) { // basically, if !ghost union
                        if (new_union.Vboxed != nullptr) {
                            Value *isboxed = ctx.builder.CreateICmpNE( // if UNION_BOX_MARKER is set, we won't select this slot anyways
                                    ctx.builder.CreateAnd(RTindex, ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER)),
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
                    RTindex = ConstantInt::get(getInt8Ty(ctx.builder.getContext()), UNION_BOX_MARKER);
            }
            if (TindexN)
                TindexN->addIncoming(RTindex, FromBB);
            if (dest)
                ctx.builder.CreateLifetimeStart(dest);
            for (size_t nr = 0; nr < roots.size(); nr++)
                roots[nr]->addIncoming(Constant::getNullValue(ctx.types().T_prjlvalue), FromBB);
            ctx.builder.ClearInsertionPoint();
        }
    }

    for (PHINode *PN : ToDelete) {
        // This basic block is statically unreachable, thus so is this PHINode
        PN->replaceAllUsesWith(UndefValue::get(PN->getType()));
        PN->eraseFromParent();
    }

    // step 12. Perform any delayed instantiations
    bool in_prologue = true;
    for (auto &BB : *ctx.f) {
        for (auto &I : BB) {
            CallBase *call = dyn_cast<CallBase>(&I);
            if (call) {
                if (debug_enabled && !I.getDebugLoc()) {
                    // LLVM Verifier: inlinable function call in a function with debug info must have a !dbg location
                    // make sure that anything we attempt to call has some inlining info, just in case optimization messed up
                    // (except if we know that it is an intrinsic used in our prologue, which should never have its own debug subprogram)
                    Function *F = call->getCalledFunction();
                    if (!in_prologue || !F || !(F->isIntrinsic() || F->getName().starts_with("julia.") || &I == restTuple)) {
                        I.setDebugLoc(topdebugloc);
                    }
                }
            }
            if (&I == &prologue_end)
                in_prologue = false;
        }
    }
    if (debug_enabled)
        dbuilder.finalize();

    if (ctx.vaSlot > 0) {
        // remove VA allocation if we never referenced it
        assert(ctx.slots[ctx.vaSlot].isSA && ctx.slots[ctx.vaSlot].isArgument);
        Instruction *root = cast_or_null<Instruction>(ctx.slots[ctx.vaSlot].boxroot);
        if (root) {
            bool have_real_use = false;
            for (User *RU : root->users()) {
                if (StoreInst *SRU = dyn_cast<StoreInst>(RU)) {
                    assert(isa<ConstantPointerNull>(SRU->getValueOperand()) || SRU->getValueOperand() == restTuple);
                    (void)SRU;
                }
                else if (MemSetInst *MSI = dyn_cast<MemSetInst>(RU)) {
                    assert(MSI->getValue() == ctx.builder.getInt8(0));
                    (void)MSI;
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
                for (User *RU : make_early_inc_range(root->users())) {
                    // This is safe because it checked above that each User is known and has at most one Use of root
                    cast<Instruction>(RU)->eraseFromParent();
                }
                root->eraseFromParent();
                restTuple->eraseFromParent();
            }
        }
    }

    if (ctx.topalloca->use_empty()) {
        ctx.topalloca->eraseFromParent();
        ctx.topalloca = nullptr;
    }

    // link the dependent llvmcall modules, but switch their function's linkage to internal
    // so that they don't conflict when they show up in the execution engine.
    Linker L(*jl_Module);
    for (auto &Mod : ctx.llvmcall_modules) {
        SmallVector<std::string, 1> Exports;
        for (const auto &F: Mod->functions())
            if (!F.isDeclaration())
                Exports.push_back(F.getName().str());
        bool error = L.linkInModule(std::move(Mod));
        assert(!error && "linking llvmcall modules failed");
        (void)error;
        for (auto FN: Exports)
            jl_Module->getFunction(FN)->setLinkage(GlobalVariable::InternalLinkage);
    }

    JL_GC_POP();
    return declarations;
}

// --- entry point ---

jl_llvm_functions_t jl_emit_codedecls(
        orc::ThreadSafeModule &M,
        jl_code_instance_t *codeinst,
        jl_codegen_params_t &params)
{
    jl_llvm_functions_t decls = {};
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    bool specsig, needsparams;
    std::tie(specsig, needsparams) = uses_specsig(get_ci_abi(codeinst), mi, codeinst->rettype, params.params->prefer_specsig);
    const char *name = name_from_method_instance(mi);
    if (specsig)
        raw_string_ostream(decls.functionObject) << "jfptr_" << name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    else if (needsparams)
        decls.functionObject = "jl_fptr_sparam";
    else
        decls.functionObject = "jl_fptr_args";
    raw_string_ostream(decls.specFunctionObject) << (specsig ? "j_" : "j1_") << name << "_" << jl_atomic_fetch_add_relaxed(&globalUniqueGeneratedNames, 1);
    M.withModuleDo([&](Module &M) {
            bool is_opaque_closure = jl_is_method(mi->def.value) && mi->def.method->is_for_opaque_closure;
            if (specsig) {
                get_specsig_function(params, &M, nullptr, decls.specFunctionObject, get_ci_abi(codeinst), codeinst->rettype, is_opaque_closure);
            }
            else {
                Function *f = Function::Create(needsparams ? JuliaType::get_jlfuncparams_ty(M.getContext()) : JuliaType::get_jlfunc_ty(M.getContext()),
                                     GlobalVariable::ExternalLinkage,
                                     decls.specFunctionObject, M);
                jl_init_function(f, params.TargetTriple);
                f->setAttributes(AttributeList::get(M.getContext(), {get_func_attrs(M.getContext()), f->getAttributes()}));
            }
        });
    return decls;
}

jl_llvm_functions_t jl_emit_code(
        orc::ThreadSafeModule &m,
        jl_method_instance_t *li,
        jl_code_info_t *src,
        jl_value_t *abi_at,
        jl_value_t *abi_rt,
        jl_codegen_params_t &params)
{
    JL_TIMING(CODEGEN, CODEGEN_LLVM);
    jl_timing_show_func_sig((jl_value_t *)li->specTypes, JL_TIMING_DEFAULT_BLOCK);
    jl_llvm_functions_t decls = {};
    assert((params.params == &jl_default_cgparams /* fast path */ || !params.cache ||
        compare_cgparams(params.params, &jl_default_cgparams)) &&
        "functions compiled with custom codegen params must not be cached");
    JL_TRY {
        decls = emit_function(m, li, src, abi_at, abi_rt, params);
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
        std::string mname = m.getModuleUnlocked()->getModuleIdentifier();
        m = orc::ThreadSafeModule();
        decls.functionObject = "";
        decls.specFunctionObject = "";
        jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: encountered unexpected error during compilation of %s:\n", mname.c_str());
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(jl_current_task));
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        jlbacktrace(); // written to STDERR_FILENO
    }

    return decls;
}

static jl_llvm_functions_t jl_emit_oc_wrapper(orc::ThreadSafeModule &m, jl_codegen_params_t &params, jl_method_instance_t *mi, jl_value_t *rettype)
{
    jl_llvm_functions_t declarations;
    declarations.functionObject = "jl_f_opaque_closure_call";
    if (uses_specsig(mi->specTypes, false, rettype, true)) {
        // context lock is held by params
        Module *M = m.getModuleUnlocked();
        jl_codectx_t ctx(M->getContext(), params, 0, 0);
        ctx.name = M->getModuleIdentifier().data();
        std::string funcName = get_function_name(true, false, ctx.name, ctx.emission_context.TargetTriple);
        jl_returninfo_t returninfo = get_specsig_function(params, M, NULL, funcName, mi->specTypes, rettype, true);
        Function *gf_thunk = cast<Function>(returninfo.decl.getCallee());
        jl_init_function(gf_thunk, ctx.emission_context.TargetTriple);
        size_t nrealargs = jl_nparams(mi->specTypes);
        emit_specsig_to_fptr1(gf_thunk, returninfo.cc, returninfo.return_roots,
                mi->specTypes, rettype, true, nrealargs, ctx.emission_context,
                prepare_call_in(gf_thunk->getParent(), jlopaque_closure_call_func)); // TODO: this could call emit_oc_call directly
        declarations.specFunctionObject = funcName;
    }
    return declarations;
}

jl_llvm_functions_t jl_emit_codeinst(
        orc::ThreadSafeModule &m,
        jl_code_instance_t *codeinst,
        jl_code_info_t *src,
        jl_codegen_params_t &params)
{
    JL_TIMING(CODEGEN, CODEGEN_Codeinst);
    jl_timing_show_method_instance(jl_get_ci_mi(codeinst), JL_TIMING_DEFAULT_BLOCK);
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    if (!src) {
        // Assert that this this is the generic method for opaque closure wrappers:
        // this signals to instead compile specptr such that it holds the specptr -> invoke wrapper
        // to satisfy the dispatching implementation requirements of jl_f_opaque_closure_call
        if (mi->def.method == jl_opaque_closure_method) {
            return jl_emit_oc_wrapper(m, params, mi, codeinst->rettype);
        }
        m = orc::ThreadSafeModule();
        return jl_llvm_functions_t(); // user error
    }
    //assert(jl_egal((jl_value_t*)jl_atomic_load_relaxed(&codeinst->debuginfo), (jl_value_t*)src->debuginfo) && "trying to generate code for a codeinst for an incompatible src");
    jl_llvm_functions_t decls = jl_emit_code(m, mi, src, get_ci_abi(codeinst), codeinst->rettype, params);
    return decls;
}

// --- initialization ---
static auto gv_for_global = new SmallVector<std::pair<jl_value_t**, JuliaVariable*>, 0>();
static void global_jlvalue_to_llvm(JuliaVariable *var, jl_value_t **addr)
{
    gv_for_global->push_back(std::make_pair(addr, var));
}
static JuliaVariable *julia_const_gv(jl_value_t *val)
{
    for (auto &kv : *gv_for_global) {
        if (*kv.first == val)
            return kv.second;
    }
    return nullptr;
}

static void init_jit_functions(void)
{
    add_named_global("jl_fptr_args", jl_fptr_args_addr);
    add_named_global("jl_fptr_sparam", jl_fptr_sparam_addr);
    add_named_global("jl_f_opaque_closure_call", &jl_f_opaque_closure_call);
    add_named_global(jl_small_typeof_var, &jl_small_typeof);
    add_named_global(jlstack_chk_guard_var, &__stack_chk_guard);
    add_named_global(jlRTLD_DEFAULT_var, &jl_RTLD_DEFAULT_handle);
    add_named_global(jlexe_var, &jl_exe_handle);
    add_named_global(jldll_var, &jl_libjulia_handle);
    add_named_global(jldlli_var, &jl_libjulia_internal_handle);
    auto size2pjlvalue = [](Type *T_size) -> Type * {
        return get_pjlvalue(T_size->getContext());
    };
    global_jlvalue_to_llvm(new JuliaVariable{"jl_true", true, size2pjlvalue}, &jl_true);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_false", true, size2pjlvalue}, &jl_false);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_nothing", true, size2pjlvalue}, &jl_nothing);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_emptysvec", true, size2pjlvalue}, (jl_value_t**)&jl_emptysvec);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_emptytuple", true, size2pjlvalue}, &jl_emptytuple);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_diverror_exception", true, size2pjlvalue}, &jl_diverror_exception);
    global_jlvalue_to_llvm(new JuliaVariable{"jl_undefref_exception", true, size2pjlvalue}, &jl_undefref_exception);
    add_named_global(jlgetworld_global, &jl_world_counter);
    add_named_global("__stack_chk_fail", &__stack_chk_fail);
    add_named_global(jlpgcstack_func, (void*)NULL);
    add_named_global(jlerror_func, &jl_error);
    add_named_global(jlatomicerror_func, &jl_atomic_error);
    add_named_global(jlthrow_func, &jl_throw);
    add_named_global(jlundefvarerror_func, &jl_undefined_var_error);
    add_named_global(jlhasnofield_func, &jl_has_no_field_error);
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
    add_named_global(jlcheckbpwritable_func, &jl_check_binding_currently_writable);
    add_named_global(jlboundp_func, &jl_boundp);
    for (auto it : builtin_func_map())
        add_named_global(it.second, it.first);
    add_named_global(jlintrinsic_func, &jl_f_intrinsic_call);
    add_named_global(jlgetbuiltinfptr_func, &jl_get_builtin_fptr);
    add_named_global(jlapplygeneric_func, &jl_apply_generic);
    add_named_global(jlinvoke_func, &jl_invoke);
    add_named_global(jltopeval_func, &jl_toplevel_eval);
    add_named_global(jlcopyast_func, &jl_copy_ast);
    //add_named_global(jlnsvec_func, &jl_svec);
    add_named_global(jlmethod_func, &jl_method_def);
    add_named_global(jlgenericfunction_func, &jl_declare_const_gf);
    add_named_global(jlenter_func, &jl_enter_handler);
    add_named_global(jl_current_exception_func, &jl_current_exception);
    add_named_global(jlleave_noexcept_func, &jl_pop_handler_noexcept);
    add_named_global(jlleave_func, &jl_pop_handler);
    add_named_global(jl_restore_excstack_func, &jl_restore_excstack);
    add_named_global(jl_excstack_state_func, &jl_excstack_state);
    add_named_global(jlegalx_func, &jl_egal__unboxed);
    add_named_global(jlisa_func, &jl_isa);
    add_named_global(jlsubtype_func, &jl_subtype);
    add_named_global(jltypeassert_func, &jl_typeassert);
    add_named_global(jlapplytype_func, &jl_instantiate_type_in_env);
    add_named_global(jl_object_id__func, &jl_object_id_);
    add_named_global(jl_alloc_genericmemory_unchecked_func, &jl_alloc_genericmemory_unchecked);
    add_named_global(jl_alloc_obj_func, (void*)NULL);
    add_named_global(jl_newbits_func, (void*)jl_new_bits);
    add_named_global(jl_typeof_func, (void*)NULL);
    add_named_global(jl_write_barrier_func, (void*)NULL);
    add_named_global(jldlsym_func, &jl_load_and_lookup);
    add_named_global("jl_adopt_thread", &jl_adopt_thread);
    add_named_global(jlgetcfunctiontrampoline_func, &jl_get_cfunction_trampoline);
    add_named_global(jlgetnthfieldchecked_func, &jl_get_nth_field_checked);
    add_named_global(jlfieldindex_func, &jl_field_index);
    add_named_global(diff_gc_total_bytes_func, &jl_gc_diff_total_bytes);
    add_named_global(sync_gc_total_bytes_func, &jl_gc_sync_total_bytes);
    add_named_global(jl_allocgenericmemory, &jl_alloc_genericmemory);
    add_named_global(gcroot_flush_func, (void*)NULL);
    add_named_global(gc_preserve_begin_func, (void*)NULL);
    add_named_global(gc_preserve_end_func, (void*)NULL);
    add_named_global(pointer_from_objref_func, (void*)NULL);
    add_named_global(except_enter_func, (void*)NULL);
    add_named_global(julia_call, (void*)NULL);
    add_named_global(julia_call2, (void*)NULL);
    add_named_global(jllockvalue_func, &jl_lock_value);
    add_named_global(jlunlockvalue_func, &jl_unlock_value);
    add_named_global(jllockfield_func, &jl_lock_field);
    add_named_global(jlunlockfield_func, &jl_unlock_field);
    add_named_global(jlgetabiconverter_func, &jl_get_abi_converter);

#ifdef _OS_WINDOWS_
#if defined(_CPU_X86_64_)
    add_named_global("__julia_personality", &__julia_personality);
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
char jl_using_intel_jitevents = 0; // Non-zero if running under Intel VTune Amplifier
#endif

#ifdef JL_USE_OPROFILE_JITEVENTS
char jl_using_oprofile_jitevents = 0; // Non-zero if running under OProfile
#endif

#ifdef JL_USE_PERF_JITEVENTS
char jl_using_perf_jitevents = 0;
#endif

int jl_is_timing_passes = 0;

extern "C" void jl_init_llvm(void)
{
    jl_page_size = jl_getpagesize();
    jl_default_debug_info_kind = jl_default_cgparams.debug_info_kind = (int) DICompileUnit::DebugEmissionKind::FullDebug;
    jl_default_cgparams.debug_info_level = (int) jl_options.debug_level;
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetDisassembler();

    // Initialize passes
    PassRegistry &Registry = *PassRegistry::getPassRegistry();
    initializeCore(Registry);
    initializeScalarOpts(Registry);
    initializeVectorization(Registry);
    initializeAnalysis(Registry);
    initializeTransformUtils(Registry);
    initializeInstCombine(Registry);
    // TODO: initializeAggressiveInstCombine(Registry);
    // TODO: initializeInstrumentation(Registry);
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
    // For parity with LoopUnswitch
    clopt = llvmopts.lookup("unswitch-threshold");
    if (clopt->getNumOccurrences() == 0)
        cl::ProvidePositionalOption(clopt, "100", 1);
    // if the patch adding this option has been applied, lower its limit to provide
    // better DAGCombiner performance.
    clopt = llvmopts.lookup("combiner-store-merge-dependence-limit");
    if (clopt && clopt->getNumOccurrences() == 0)
        cl::ProvidePositionalOption(clopt, "4", 1);

    clopt = llvmopts.lookup("time-passes");
    if (clopt && clopt->getNumOccurrences() > 0)
        jl_is_timing_passes = 1;

    jl_ExecutionEngine = new JuliaOJIT();

    bool jl_using_gdb_jitevents = false;
    // Register GDB event listener
#if defined(JL_DEBUG_BUILD)
    jl_using_gdb_jitevents = true;
#endif
    const char *jit_gdb = getenv("ENABLE_GDBLISTENER");
    if (jit_gdb) {
        jl_using_gdb_jitevents = !!atoi(jit_gdb);
    }
    if (jl_using_gdb_jitevents)
        jl_ExecutionEngine->enableJITDebuggingSupport();

#if defined(JL_USE_INTEL_JITEVENTS) || \
    defined(JL_USE_OPROFILE_JITEVENTS) || \
    defined(JL_USE_PERF_JITEVENTS)
    const char *jit_profiling = getenv("ENABLE_JITPROFILING");

#if defined(JL_USE_INTEL_JITEVENTS)
    if (jit_profiling) {
        if (atoi(jit_profiling)) {
            jl_using_intel_jitevents = 1;
        }
    } else {
#ifdef USE_ITTAPI
        __itt_collection_state state = __itt_get_collection_state();
        jl_using_intel_jitevents = state == __itt_collection_init_successful ||
                                   state == __itt_collection_collector_exists;
#endif
    }
#endif

#if defined(JL_USE_OPROFILE_JITEVENTS)
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_oprofile_jitevents = 1;
    }
#endif

#if defined(JL_USE_PERF_JITEVENTS)
    if (jit_profiling && atoi(jit_profiling)) {
        jl_using_perf_jitevents = 1;
    }
#endif

#ifdef JL_USE_INTEL_JITEVENTS
    if (jl_using_intel_jitevents)
        jl_ExecutionEngine->enableIntelJITEventListener();
#endif

#ifdef JL_USE_OPROFILE_JITEVENTS
    if (jl_using_oprofile_jitevents)
        jl_ExecutionEngine->enableOProfileJITEventListener();
#endif

#ifdef JL_USE_PERF_JITEVENTS
    if (jl_using_perf_jitevents)
        jl_ExecutionEngine->enablePerfJITEventListener();
#endif
#endif

    cl::PrintOptionValues();
}

extern "C" JL_DLLEXPORT_CODEGEN void jl_init_codegen_impl(void)
{
    jl_init_llvm();
    // Now that the execution engine exists, initialize all modules
    init_jit_functions();
}

extern "C" JL_DLLEXPORT_CODEGEN void jl_teardown_codegen_impl() JL_NOTSAFEPOINT
{
    // output LLVM timings and statistics
    // Guard against exits before we have initialized the ExecutionEngine
    if (jl_ExecutionEngine)
        jl_ExecutionEngine->printTimers();
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

extern "C" JL_DLLEXPORT_CODEGEN jl_value_t *jl_get_libllvm_impl(void) JL_NOTSAFEPOINT
{
#if defined(_OS_WINDOWS_)
    HMODULE mod;
    if (!GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS, (LPCSTR)&llvm::DebugFlag, &mod))
        return jl_nothing;
    wchar_t path16[MAX_PATH];
    DWORD n16 = GetModuleFileNameW(mod, path16, MAX_PATH);
    if (n16 <= 0)
        return jl_nothing;
    path16[n16++] = 0;
    char path8[MAX_PATH * 3];
    if (!WideCharToMultiByte(CP_UTF8, 0, path16, n16, path8, MAX_PATH * 3, NULL, NULL))
        return jl_nothing;
    return (jl_value_t*) jl_symbol(path8);
#else
    Dl_info dli;
    if (!dladdr((void*)LLVMContextCreate, &dli))
        return jl_nothing;
    return (jl_value_t*) jl_symbol(dli.dli_fname);
#endif
}
