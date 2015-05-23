// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "platform.h"
#include "options.h"

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "llvm-version.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/IR/IntrinsicInst.h>
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
#if LLVM37
using namespace llvm::legacy;
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
static TargetMachine *jl_TargetMachine;
#ifdef USE_MCJIT
static Module *shadow_module;
static RTDyldMemoryManager *jl_mcjmm;
#define jl_Module (builder.GetInsertBlock()->getParent()->getParent())
#else
static Module *jl_Module;
#endif
static MDBuilder *mbuilder;
static std::map<int, std::string> argNumberStrings;
static FunctionPassManager *FPM;

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
static Type *jl_value_llvmt;
static Type *jl_pvalue_llvmt;
static Type *jl_ppvalue_llvmt;
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
#ifdef JL_GC_MARKSWEEP
static GlobalVariable *jlpgcstack_var;
#endif
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

// important functions
static Function *jlnew_func;
static Function *jlthrow_func;
static Function *jlthrow_line_func;
static Function *jlerror_func;
static Function *jltypeerror_func;
static Function *jlundefvarerror_func;
static Function *jlboundserror_func;
static Function *jluboundserror_func;
static Function *jlvboundserror_func;
static Function *jlboundserrorv_func;
static Function *jlcheckassign_func;
static Function *jldeclareconst_func;
static Function *jltopeval_func;
static Function *jlcopyast_func;
static Function *jltuple_func;
static Function *jlnsvec_func;
static Function *jlapplygeneric_func;
static Function *jlgetfield_func;
static Function *jlbox_func;
static Function *jlclosure_func;
static Function *jlmethod_func;
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
#ifdef JL_GC_MARKSWEEP
static Function *wbfunc;
static Function *queuerootfun;
#endif
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
static Function *show_execution_point_func;

static std::vector<Type *> two_pvalue_llvmt;
static std::vector<Type *> three_pvalue_llvmt;

static std::map<jl_fptr_t, Function*> builtin_func_map;

extern "C" DLLEXPORT void gc_wb_slow(jl_value_t* parent, jl_value_t* ptr)
{
    gc_wb(parent, ptr);
}

// --- code generation ---

// per-local-variable information
struct jl_varinfo_t {
    Value *memvalue;  // an address, if the var is alloca'd
    Value *SAvalue;   // register, if the var is SSA
    Value *passedAs;  // if an argument, the original passed value
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
    bool isGhost;     // Has size 0 and is thus never actually allocated
    bool hasGCRoot;
    bool escapes;
    bool usedUndef;
    bool used;
    jl_value_t *declType;
    jl_value_t *initExpr;  // initializing expression for SSA variables

    jl_varinfo_t() : memvalue(NULL), SAvalue(NULL), passedAs(NULL),
#ifdef LLVM37
                     dinfo(NULL),
#else
                     dinfo(DIVariable()),
#endif
                     closureidx(-1), isAssigned(true), isCaptured(false), isSA(false),
                     isVolatile(false), isArgument(false), isGhost(false), hasGCRoot(false),
                     escapes(true), usedUndef(false), used(false),
                     declType((jl_value_t*)jl_any_type), initExpr(NULL)
    {
    }
};

// --- helpers for reloading IR image
static void jl_gen_llvm_gv_array(llvm::Module *mod, SmallVector<GlobalVariable*, 8> &globalvars);

extern "C"
void jl_dump_bitcode(char *fname)
{
#ifdef LLVM36
    std::error_code err;
    StringRef fname_ref = StringRef(fname);
    raw_fd_ostream OS(fname_ref, err, sys::fs::F_None);
#elif LLVM35
    std::string err;
    raw_fd_ostream OS(fname, err, sys::fs::F_None);
#else
    std::string err;
    raw_fd_ostream OS(fname, err);
#endif
    SmallVector<GlobalVariable*, 8> globalvars;
#ifdef USE_MCJIT
    jl_gen_llvm_gv_array(shadow_module, globalvars);
    WriteBitcodeToFile(shadow_module, OS);
#else
    jl_gen_llvm_gv_array(jl_Module, globalvars);
    WriteBitcodeToFile(jl_Module, OS);
#endif
    for (SmallVectorImpl<GlobalVariable>::iterator *I = globalvars.begin(), *E = globalvars.end(); I != E; ++I) {
        (*I)->eraseFromParent();
    }
}

extern "C"
void jl_dump_objfile(char *fname, int jit_model)
{
#ifdef LLVM36
    std::error_code err;
    StringRef fname_ref = StringRef(fname);
    raw_fd_ostream OS(fname_ref, err, sys::fs::F_None);
#elif  LLVM35
    std::string err;
    raw_fd_ostream OS(fname, err, sys::fs::F_None);
#else
    std::string err;
    raw_fd_ostream OS(fname, err);
#endif

    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
#if defined(_OS_WINDOWS_) && defined(USE_MCJIT)
    TheTriple.setObjectFormat(Triple::COFF);
#elif defined(_OS_DARWIN_) && defined(FORCE_ELF)
    TheTriple.setObjectFormat(Triple::MachO);
#endif
#ifdef LLVM35
    std::unique_ptr<TargetMachine>
#else
    OwningPtr<TargetMachine>
#endif
    TM(jl_TargetMachine->getTarget().createTargetMachine(
        TheTriple.getTriple(),
        jl_TargetMachine->getTargetCPU(),
        jl_TargetMachine->getTargetFeatureString(),
        jl_TargetMachine->Options,
#if defined(_OS_LINUX_) || defined(_OS_FREEBSD_)
        Reloc::PIC_,
#else
        jit_model ? Reloc::PIC_ : Reloc::Default,
#endif
        jit_model ? CodeModel::JITDefault : CodeModel::Default,
        CodeGenOpt::Aggressive // -O3
        ));

    PassManager PM;
#ifndef LLVM37
    PM.add(new TargetLibraryInfo(Triple(jl_TargetMachine->getTargetTriple())));
#else
    PM.add(new TargetLibraryInfoWrapperPass(Triple(jl_TargetMachine->getTargetTriple())));
#endif
#ifdef LLVM37
// No DataLayout pass needed anymore.
#elif LLVM36
    PM.add(new DataLayoutPass());
#elif LLVM35
    PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
    PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
#endif


#ifdef LLVM37 // 3.7 simplified formatted output; just use the raw stream alone
    raw_fd_ostream& FOS(OS);
#else
    formatted_raw_ostream FOS(OS);
#endif

    if (TM->addPassesToEmitFile(PM, FOS, TargetMachine::CGFT_ObjectFile, false)) {
        jl_error("Could not generate obj file for this target");
    }

    SmallVector<GlobalVariable*, 8> globalvars;
#ifdef USE_MCJIT
    jl_gen_llvm_gv_array(shadow_module, globalvars);
    PM.run(*shadow_module);
#else
    jl_gen_llvm_gv_array(jl_Module, globalvars);
    PM.run(*jl_Module);
#endif
    for (SmallVectorImpl<GlobalVariable>::iterator *I = globalvars.begin(), *E = globalvars.end(); I != E; ++I) {
        (*I)->eraseFromParent();
    }
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
    // NOTE: you must be careful not to access vars[s] before you are sure "s" is
    // a local, since otherwise this will add it to the map.
    std::map<jl_sym_t*, jl_varinfo_t> vars;
    std::vector<Value*> gensym_SAvalues;
    std::vector<bool> gensym_assigned;
    std::map<jl_sym_t*, jl_arrayvar_t> *arrayvars;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *handlers;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_svec_t *sp;
    jl_lambda_info_t *linfo;
    Value *envArg;
    Value *argArray;
    Value *argCount;
    Instruction *argTemp;
    int argDepth;
    int maxDepth;
    int argSpaceOffs;
    std::string funcName;
    jl_sym_t *vaName;  // name of vararg argument
    bool vaStack;      // varargs stack-allocated
    int nReqArgs;
    int lineno;
    std::vector<bool> boundsCheck;
#ifdef JL_GC_MARKSWEEP
    Instruction *gcframe;
    Instruction *argSpaceInits;
    StoreInst *storeFrameSize;
#endif
    BasicBlock::iterator first_gcframe_inst;
    BasicBlock::iterator last_gcframe_inst;
    llvm::DIBuilder *dbuilder;
    bool debug_enabled;
    std::vector<Instruction*> gc_frame_pops;
    std::vector<CallInst*> to_inline;
} jl_codectx_t;

typedef struct {
    size_t len;
    struct {
        int64_t isref;
        Function *f;
    } data[];
} cFunctionList_t;

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool boxed=true,
                        bool valuepos=true, jl_sym_t **valuevar=NULL);
static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx);
static int is_global(jl_sym_t *s, jl_codectx_t *ctx);

static Value *make_gcroot(Value *v, jl_codectx_t *ctx, jl_sym_t *var = NULL);
static Value *emit_boxed_rooted(jl_value_t *e, jl_codectx_t *ctx);
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign);
static Value *emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol=false);
static bool might_need_root(jl_value_t *ex);
static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx);

// NoopType
static Type *NoopType;

// --- utilities ---

extern "C" {
    int globalUnique = 0;
}

extern "C" DLLEXPORT
jl_value_t *jl_get_cpu_name(void)
{
#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 5
    std::string HostCPUName = llvm::sys::getHostCPUName();
#else
    StringRef HostCPUName = llvm::sys::getHostCPUName();
#endif
    return jl_pchar_to_string(HostCPUName.data(), HostCPUName.size());
}

static void emit_write_barrier(jl_codectx_t*,Value*,Value*);

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

// --- entry point ---
//static int n_emit=0;
static Function *emit_function(jl_lambda_info_t *lam);
//static int n_compile=0;
static Function *to_function(jl_lambda_info_t *li)
{
    JL_SIGATOMIC_BEGIN();
    assert(!li->inInference);
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    Function *f = NULL;
    JL_TRY {
        f = emit_function(li);
        //jl_printf(JL_STDOUT, "emit %s\n", li->name->name);
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
        jl_rethrow_with_add("error compiling %s", li->name->name);
    }
    assert(f != NULL);
    nested_compile = last_n_c;
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
    //jl_printf(JL_STDERR, "%s:%d\n",
    //           ((jl_sym_t*)li->file)->name, li->line);
    //if (verifyFunction(*f,PrintMessageAction)) {
    //    f->dump();
    //    abort();
    //}
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    JL_SIGATOMIC_END();
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
        m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
        m->setTargetTriple(jl_TargetMachine->getTargetTriple());
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
    // objective: assign li->fptr
    jl_lambda_info_t *li = f->linfo;
    assert(li->functionObject);
    if (li->fptr == &jl_trampoline) {
        JL_SIGATOMIC_BEGIN();
        #ifdef USE_MCJIT
        if (imaging_mode) {
            // Copy the function out of the shadow module
            Module *m = new Module("julia", jl_LLVMContext);
            jl_setup_module(m,true);
            FunctionMover mover(m,shadow_module);
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
            if (!imaging_mode)
                ((Function*)li->specFunctionObject)->deleteBody();
        }
        JL_SIGATOMIC_END();
    }
    f->fptr = li->fptr;
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
        #elif defined(LLVM37)
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
                  "Warning: Returned code may not match what actually runs.\n");
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

extern "C" DLLEXPORT
const jl_value_t *jl_dump_function_ir(void *f, bool strip_ir_metadata)
{
    std::string code;
    llvm::raw_string_ostream stream(code);

    Function *llvmf = dyn_cast<Function>((Function*)f);
    if (!llvmf)
        jl_error("jl_dump_function_ir: Expected Function*");

    if (!strip_ir_metadata || llvmf->isDeclaration()) {
        // print the function IR as-is
        llvmf->print(stream);
    }
    else {
        // make a copy of the function and strip metadata from the copy
        llvm::ValueToValueMapTy VMap;
        Function* f2 = llvm::CloneFunction(llvmf, VMap, false);
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
        f2->print(stream);
        delete f2;
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
const jl_value_t *jl_dump_function_asm(void *f)
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
        jl_dump_asm_internal(fptr, symsize, slide, object, fstream);
    }
    else {
        jl_printf(JL_STDERR, "Warning: Unable to find function pointer\n");
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
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v),
                                          ConstantInt::get(T_int64,1)),
                        v);
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

extern "C" void jl_write_coverage_data(void)
{
    write_log_data(coverageData, ".cov");
}

// Memory allocation log (malloc_log)

static logdata_t mallocData;

static void mallocVisitLine(std::string filename, int line)
{
    if (filename == "" || filename == "none" || filename == "no file") {
        sync_gc_total_bytes();
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
    sync_gc_total_bytes();
}

extern "C" void jl_write_malloc_log(void)
{
    write_log_data(mallocData, ".mem");
}

// --- code gen for intrinsic functions ---

#include "intrinsics.cpp"

// --- constant determination ---

static bool in_vinfo(jl_sym_t *s, jl_array_t *vi)
{
    size_t i, l = jl_array_len(vi);
    for(i=0; i < l; i++) {
        if (s == (jl_sym_t*)jl_cellref(jl_cellref(vi, i), 0))
            return true;
    }
    return false;
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
        bool isglob;
        if (ctx) {
            isglob = is_global(sym, ctx);
        }
        else {
            isglob = !in_vinfo(sym, jl_lam_vinfo(ast)) && !in_vinfo(sym, jl_lam_capt(ast));
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
            if (b && b->constp)
                return b->value;
        }
        return NULL;
    }
    if (jl_is_expr(ex)) {
        jl_expr_t *e = (jl_expr_t*)ex;
        if (e->head == call_sym || e->head == call1_sym) {
            jl_value_t *f = jl_static_eval(jl_exprarg(e,0),ctx,mod,sp,ast,sparams,allow_alloc);
            if (f && jl_is_function(f)) {
                jl_fptr_t fptr = ((jl_function_t*)f)->fptr;
                if (jl_array_dim0(e->args) == 3 && fptr == &jl_f_get_field) {
                    m = (jl_module_t*)jl_static_eval(jl_exprarg(e,1),ctx,mod,sp,ast,sparams,allow_alloc);
                    s = (jl_sym_t*)jl_static_eval(jl_exprarg(e,2),ctx,mod,sp,ast,sparams,allow_alloc);
                    if (m && jl_is_module(m) && s && jl_is_symbol(s)) {
                        jl_binding_t *b = jl_get_binding(m, s);
                        if (b && b->constp)
                            return b->value;
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
                    jl_value_t *result = fptr(f, v, n);
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
                if (!jl_is_gensym(ar)) {
                    assert(jl_is_symbol(ar));
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
    return (jl_is_symbol(e) || jl_is_symbolnode(e) || jl_is_topnode(e));
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
        if (e->head == call_sym || e->head == call1_sym || e->head == new_sym) {
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
            simple_escape_analysis(jl_exprarg(e,1), esc, ctx);
            simple_escape_analysis(jl_exprarg(e,2), esc, ctx);
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

static Value *make_gcroot(Value *v, jl_codectx_t *ctx, jl_sym_t *var)
{
    uint64_t slot = ctx->argSpaceOffs + ctx->argDepth;
    Value *froot = builder.CreateGEP(ctx->argTemp,
                                     ConstantInt::get(T_size,slot));
    builder.CreateStore(v, froot);
#ifdef LLVM36
    if (var != NULL) {
        std::map<jl_sym_t *, jl_varinfo_t>::iterator it = ctx->vars.find(var);
        if (it != ctx->vars.end() && ((llvm::MDNode*)it->second.dinfo) != NULL) {
            if (ctx->debug_enabled) {
                SmallVector<int64_t, 9> addr;
                addr.push_back(llvm::dwarf::DW_OP_plus);
                addr.push_back(slot * sizeof(void*));
                addr.push_back(llvm::dwarf::DW_OP_deref);
#ifdef LLVM37
                ctx->dbuilder->insertDeclare(ctx->argTemp, it->second.dinfo,
                    ctx->dbuilder->createExpression(addr),builder.getCurrentDebugLocation().get(),builder.GetInsertBlock());
#else
                ctx->dbuilder->insertDeclare(ctx->argTemp, it->second.dinfo,
                    ctx->dbuilder->createExpression(addr),builder.GetInsertBlock());
#endif
            }
        }
    }
#endif
    ctx->argDepth++;
    if (ctx->argDepth > ctx->maxDepth)
        ctx->maxDepth = ctx->argDepth;
    return froot;
}

// test whether getting a field from the given type using the given
// field expression would not allocate memory
static bool is_getfield_nonallocating(jl_datatype_t *ty, jl_value_t *fld)
{
    if (!jl_is_leaf_type((jl_value_t*)ty))
        return false;
    jl_sym_t *name = NULL;
    if (jl_is_quotenode(fld) && jl_is_symbol(jl_fieldref(fld,0))) {
        name = (jl_sym_t*)jl_fieldref(fld,0);
    }
    for(size_t i=0; i < jl_svec_len(ty->types); i++) {
        if (!(ty->fields[i].isptr ||
              (name && name != jl_field_name(ty,i)))) {
            return false;
        }
    }
    return true;
}

static bool isbits_spec(jl_value_t *jt, bool allow_unsized = true)
{
    return jl_isbits(jt) && jl_is_leaf_type(jt) && (allow_unsized ||
        ((jl_is_bitstype(jt) && jl_datatype_size(jt) > 0) ||
         (jl_is_datatype(jt) && jl_datatype_nfields(jt)>0)));
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
        if (e->head == call_sym || e->head == call1_sym) {
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
            !jl_is_bool(ex) && !jl_is_quotenode(ex) && !jl_is_byte_string(ex));
}

static Value *emit_boxed_rooted(jl_value_t *e, jl_codectx_t *ctx)
{
    Value *v = emit_expr(e, ctx);
    if (v->getType() != jl_pvalue_llvmt) {
        v = boxed(v, ctx);
        make_gcroot(v, ctx);
    }
    else if (might_need_root(e)) {
        make_gcroot(v, ctx);
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
        gc_wb(li, li->roots);
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

    int argStart = ctx->argDepth;
    size_t clen = jl_array_dim0(capt);
    Value **captured = (Value**) alloca((1+clen)*sizeof(Value*));
    captured[0] = ConstantInt::get(T_size, clen);
    for(i=0; i < clen; i++) {
        Value *val;
        jl_array_t *vi = (jl_array_t*)jl_cellref(capt, i);
        assert(jl_is_array(vi));
        jl_sym_t *s = (jl_sym_t*)jl_cellref(vi,0);
        assert(jl_is_symbol(s));
        jl_varinfo_t &vari = ctx->vars[s];
        if (vari.closureidx != -1) {
            int idx = vari.closureidx;
#ifdef OVERLAP_SVEC_LEN
            val = emit_nthptr((Value*)ctx->envArg, idx, tbaa_sveclen);
#else
            val = emit_nthptr((Value*)ctx->envArg, idx+1, tbaa_sveclen);
#endif
        }
        else {
            Value *l = vari.memvalue;
            if (l == NULL) {
                val = vari.passedAs;
                if (val == NULL && vari.declType != (jl_value_t*)jl_any_type) {
                    val = boxed(NULL, ctx, vari.declType);
                }
                assert(val != NULL);
                if (val->getType() != jl_pvalue_llvmt) {
                    assert(vari.declType != (jl_value_t*)jl_any_type);
                    val = boxed(val,ctx,vari.declType);
                    make_gcroot(val, ctx);
                }
            }
            else {
                val = builder.CreateLoad(l, false);
            }
        }
        captured[i+1] = val;
    }
    Value *env_tuple;
    env_tuple = builder.CreateCall(prepare_call(jlnsvec_func),
                                   ArrayRef<Value*>(&captured[0], 1+clen));
    ctx->argDepth = argStart;
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
    ctx->argDepth--;
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

static Value *emit_getfield(jl_value_t *expr, jl_sym_t *name, jl_codectx_t *ctx)
{
    if (jl_is_quotenode(expr) && jl_is_module(jl_fieldref(expr,0)))
        expr = jl_fieldref(expr,0);

    jl_value_t *static_val = static_eval(expr, ctx, true, false);
    if (static_val != NULL && jl_is_module(static_val))
        expr = static_val;

    if (jl_is_module(expr)) {
        Value *bp =
            global_binding_pointer((jl_module_t*)expr, name, NULL, false);
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
            Value *strct = emit_expr(expr, ctx, false);
            Value *fld = emit_getfield_knownidx(strct, idx, sty, ctx);
            JL_GC_POP();
            return fld;
        }
    }
    // TODO: attempt better codegen for approximate types, if the types
    // and offsets of some fields are independent of parameters.
    JL_GC_POP();

    int argStart = ctx->argDepth;
    Value *arg1 = boxed(emit_expr(expr,ctx), ctx, expr_type(expr,ctx));
    // TODO: generic getfield func with more efficient calling convention
    make_gcroot(arg1, ctx);
    Value *arg2 = literal_pointer_val((jl_value_t*)name);
    make_gcroot(arg2, ctx);
    Value *myargs = builder.CreateGEP(ctx->argTemp,
                                      ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(jlgetfield_func), {V_null, myargs,
                                        ConstantInt::get(T_int32,2)});
#else
    Value *result = builder.CreateCall3(prepare_call(jlgetfield_func), V_null, myargs,
                                        ConstantInt::get(T_int32,2));
#endif
    ctx->argDepth = argStart;
    return result;
}

// emit code for is (===). rt1 and rt2 are the julia types of the arguments,
// arg1 and arg2 are expressions for the arguments if we have them, or NULL,
// and varg1 and varg2 are LLVM values for the arguments if we have them.
static Value *emit_f_is(jl_value_t *rt1, jl_value_t *rt2,
                        jl_value_t *arg1, jl_value_t *arg2,
                        Value *varg1, Value *varg2, jl_codectx_t *ctx)
{
    if (jl_is_type_type(rt1) && jl_is_type_type(rt2) &&
        !jl_is_typevar(jl_tparam0(rt1)) && !jl_is_typevar(jl_tparam0(rt2)) &&
        (!arg1 || jl_is_symbol(arg1) || jl_is_symbolnode(arg1) || jl_is_gensym(arg1) || is_constant(arg1, ctx)) &&
        (!arg2 || jl_is_symbol(arg2) || jl_is_symbolnode(arg2) || jl_is_gensym(arg2) || is_constant(arg2, ctx))) {
        if (jl_tparam0(rt1) == jl_tparam0(rt2))
            return ConstantInt::get(T_int1, 1);
        return ConstantInt::get(T_int1, 0);
    }
    int ptr_comparable = 0;
    if (rt1==(jl_value_t*)jl_sym_type || rt2==(jl_value_t*)jl_sym_type ||
        jl_is_mutable_datatype(rt1) || jl_is_mutable_datatype(rt2))
        ptr_comparable = 1;
    int last_depth = ctx->argDepth;
    bool isleaf = jl_is_leaf_type(rt1) && jl_is_leaf_type(rt2);
    bool isteq = jl_types_equal(rt1, rt2);
    bool isbits = isleaf && isteq && jl_is_bitstype(rt1);
    if (isteq && isleaf && jl_is_datatype_singleton((jl_datatype_t*)rt1))
        return ConstantInt::get(T_int1, 1);
    if (arg1 && !varg1) {
        varg1 = isbits ? auto_unbox(arg1, ctx) : emit_expr(arg1, ctx);
        if (arg2 && !varg2 && !isbits && varg1->getType() == jl_pvalue_llvmt &&
            rt1 != (jl_value_t*)jl_sym_type && might_need_root(arg1)) {
            make_gcroot(varg1, ctx);
        }
    }
    Value *answer;
    if (arg2 && !varg2)
        varg2 = isbits ? auto_unbox(arg2, ctx) : emit_expr(arg2, ctx);
    if (isleaf && !isteq && !jl_is_type_type(rt1) && !jl_is_type_type(rt2)) {
        ctx->argDepth = last_depth;
        return ConstantInt::get(T_int1, 0);
    }
    Type *at1 = varg1->getType();
    Type *at2 = varg2->getType();
    if (at1 != jl_pvalue_llvmt && at2 != jl_pvalue_llvmt) {
        assert(at1 == at2);
        if (at1->isIntegerTy() || at1->isPointerTy() ||
            at1->isFloatingPointTy()) {
            answer = builder.CreateICmpEQ(JL_INT(varg1),JL_INT(varg2));
            goto done;
        }
        bool isStructOrArray = at1->isStructTy() || at1->isArrayTy();
        if ((isStructOrArray || at1->isVectorTy()) && !ptr_comparable) {
            assert(jl_is_datatype(rt1));
            jl_svec_t *types = ((jl_datatype_t*)rt1)->types;
            answer = ConstantInt::get(T_int1, 1);
            size_t l = jl_svec_len(types);
            for(unsigned i=0; i < l; i++) {
                jl_value_t *fldty = jl_svecref(types,i);
                Value *subAns;
                if (isStructOrArray) {
                    if (julia_type_to_llvm(fldty) != T_void) {
                        subAns =
                            emit_f_is(fldty, fldty, NULL, NULL,
                                      builder.CreateExtractValue(varg1, ArrayRef<unsigned>(&i,1)),
                                      builder.CreateExtractValue(varg2, ArrayRef<unsigned>(&i,1)),
                                      ctx);
                    }
                    else {
                        continue;
                    }
                }
                else {
                    subAns =
                        emit_f_is(fldty, fldty, NULL, NULL,
                                  builder.CreateExtractElement(varg1, ConstantInt::get(T_int32,i)),
                                  builder.CreateExtractElement(varg2, ConstantInt::get(T_int32,i)),
                                  ctx);
                }
                answer = builder.CreateAnd(answer, subAns);
            }
            goto done;
        }
    }
    assert(at1 == jl_pvalue_llvmt || at2 == jl_pvalue_llvmt);
    varg1 = boxed(varg1,ctx); varg2 = boxed(varg2,ctx);
    if (ptr_comparable)
        answer = builder.CreateICmpEQ(varg1, varg2);
    else {
#ifdef LLVM37
        answer = builder.CreateTrunc(builder.CreateCall(prepare_call(jlegal_func), {varg1, varg2}), T_int1);
#else
        answer = builder.CreateTrunc(builder.CreateCall2(prepare_call(jlegal_func), varg1, varg2), T_int1);
#endif
    }
 done:
    ctx->argDepth = last_depth;
    return answer;
}

static Value *emit_known_call(jl_value_t *ff, jl_value_t **args, size_t nargs,
                              jl_codectx_t *ctx,
                              Value **theFptr, jl_function_t **theF,
                              jl_value_t *expr)
{
    if (jl_typeis(ff, jl_intrinsic_type)) {
        return emit_intrinsic((intrinsic)*(uint32_t*)jl_data_ptr(ff),
                              args, nargs, ctx);
    }
    if (!jl_is_func(ff)) {
        return NULL;
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
    }
    else if (f->fptr == &jl_f_is && nargs==2) {
        rt1 = expr_type(args[1], ctx);
        rt2 = expr_type(args[2], ctx);
        Value *ans = emit_f_is(rt1,rt2, args[1],args[2], NULL,NULL, ctx);
        JL_GC_POP();
        return ans;
    }
    else if (f->fptr == &jl_f_typeof && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        Value *arg1 = emit_expr(args[1], ctx), *ret;
        if (jl_is_leaf_type(aty)) {
            if (jl_is_type_type(aty))
                aty = (jl_value_t*)jl_typeof(jl_tparam0(aty));
            ret = literal_pointer_val(aty);
        }
        else {
            arg1 = boxed(arg1,ctx);
            ret = emit_typeof(arg1);
        }
        JL_GC_POP();
        return ret;
    }
    else if (f->fptr == &jl_f_typeassert && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0, 0)) {
                JL_GC_POP();
                Value *v = emit_expr(args[1], ctx);
                if (tp0 == jl_bottom_type) {
                    v = builder.CreateUnreachable();
                    BasicBlock *cont = BasicBlock::Create(getGlobalContext(),"after_assert",ctx->f);
                    builder.SetInsertPoint(cont);
                }
                return v;
            }
            if (tp0 == jl_bottom_type) {
                emit_expr(args[1], ctx);
                emit_error("reached code declared unreachable", ctx);
                JL_GC_POP();
                return NULL;
            }
            if (!jl_is_tuple_type(tp0) && jl_is_leaf_type(tp0)) {
                Value *arg1 = emit_expr(args[1], ctx);
                emit_typecheck(arg1, tp0, "typeassert", ctx);
                JL_GC_POP();
                return arg1;
            }
        }
        if (jl_subtype(ty, (jl_value_t*)jl_type_type, 0)) {
            FunctionType *ft = FunctionType::get(T_void, two_pvalue_llvmt, false);
            Value *typeassert = jl_Module->getOrInsertFunction("jl_typeassert", ft);
            int ldepth = ctx->argDepth;
            Value *arg1 = emit_boxed_rooted(args[1], ctx);
#ifdef LLVM37
            builder.CreateCall(prepare_call(typeassert), {arg1, boxed(emit_expr(args[2], ctx),ctx)});
#else
            builder.CreateCall2(prepare_call(typeassert), arg1, boxed(emit_expr(args[2], ctx),ctx));
#endif
            ctx->argDepth = ldepth;
            JL_GC_POP();
            return arg1;
        }
    }
    else if (f->fptr == &jl_f_isa && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (arg == jl_bottom_type) {
            JL_GC_POP();
            emit_expr(args[1], ctx);
            return UndefValue::get(T_int1);
        }
        if (jl_is_type_type(ty) && !jl_has_typevars(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0, 0)) {
                JL_GC_POP();
                return ConstantInt::get(T_int1,1);
            }
            if (!jl_subtype(tp0, (jl_value_t*)jl_type_type, 0)) {
                if (jl_is_leaf_type(arg)) {
                    JL_GC_POP();
                    return ConstantInt::get(T_int1,0);
                }
                if (jl_is_leaf_type(tp0)) {
                    Value *arg1 = emit_expr(args[1], ctx);
                    JL_GC_POP();
                    return builder.CreateICmpEQ(emit_typeof(arg1),
                                                literal_pointer_val(tp0));
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
            JL_GC_POP();
            return ConstantInt::get(T_int1, issub);
        }
    }
    else if (f->fptr == &jl_f_apply && nargs==3 && ctx->vaStack &&
             symbol_eq(args[3], ctx->vaName) && expr_type(args[2],ctx) == (jl_value_t*)jl_function_type) {
        Value *theF = emit_expr(args[2],ctx);
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
        JL_GC_POP();
        return r;
    }
    else if (f->fptr == &jl_f_tuple) {
        if (nargs == 0) {
            JL_GC_POP();
            return tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlemptytuple_var)));
        }
        if (ctx->linfo->inferred) {
            rt1 = expr_type(expr, ctx);
            if (jl_is_tuple_type(rt1) && jl_is_leaf_type(rt1) && nargs == jl_datatype_nfields(rt1)) {
                Value *tpl = emit_new_struct(rt1, nargs+1, args, ctx);
                JL_GC_POP();
                return tpl;
            }
        }
    }
    else if (f->fptr == &jl_f_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx), ctx);
        JL_GC_POP();
#ifdef LLVM37
        builder.CreateCall(prepare_call(jlthrow_line_func), {arg1,
                            ConstantInt::get(T_int32, ctx->lineno)});
#else
        builder.CreateCall2(prepare_call(jlthrow_line_func), arg1,
                            ConstantInt::get(T_int32, ctx->lineno));
#endif
        return V_null;
    }
    else if (f->fptr == &jl_f_arraylen && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_array_type(aty)) {
            // todo: also allow e.g. Union of several array types
            Value *arg1 = emit_expr(args[1], ctx);
            JL_GC_POP();
            return emit_arraylen(arg1, args[1], ctx);
        }
    }
    else if (f->fptr == &jl_f_arraysize && nargs==2) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        if (jl_is_array_type(aty) && ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ndp = jl_tparam1(aty);
            if (jl_is_long(ndp)) {
                Value *ary = emit_expr(args[1], ctx);
                size_t ndims = jl_unbox_long(ndp);
                if (jl_is_long(args[2])) {
                    uint32_t idx = (uint32_t)jl_unbox_long(args[2]);
                    if (idx > 0 && idx <= ndims) {
                        JL_GC_POP();
                        return emit_arraysize(ary, args[1], idx, ctx);
                    }
                    else if (idx > ndims) {
                        JL_GC_POP();
                        return ConstantInt::get(T_size, 1);
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
                    JL_GC_POP();
                    return result;
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
                    Value *ary = emit_expr(args[1], ctx);
                    size_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : 1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[2], nargs-1, ctx);
                    JL_GC_POP();
                    if (jl_array_store_unboxed(ety) &&
                        ((jl_datatype_t*)ety)->size == 0) {
                        assert(jl_is_datatype(ety));
                        assert(((jl_datatype_t*)ety)->instance != NULL);
                        return literal_pointer_val(((jl_datatype_t*)ety)->instance);
                    }
                    return typed_load(emit_arrayptr(ary, args[1], ctx), idx, ety, ctx, tbaa_user);
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
                    Value *ary = emit_expr(args[1], ctx);
                    size_t nd = jl_is_long(ndp) ? jl_unbox_long(ndp) : 1;
                    Value *idx = emit_array_nd_index(ary, args[1], nd, &args[3], nargs-2, ctx);
                    bool isboxed = !jl_array_store_unboxed(ety);
                    if (!isboxed && ((jl_datatype_t*)ety)->size == 0) {
                        // no-op, but emit expr for possible effects
                        assert(jl_is_datatype(ety));
                        emit_expr(args[2],ctx,false);
                    }
                    else {
                        Value* v = ety==(jl_value_t*)jl_any_type ? emit_expr(args[2],ctx) : emit_unboxed(args[2],ctx);
                        PHINode* data_owner = NULL; // owner object against which the write barrier must check
                        if (isboxed) { // if not boxed we don't need a write barrier
                            Value *flags = emit_arrayflags(ary,ctx);
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
                                    builder.CreateBitCast(ary,T_pint8), jl_array_data_owner_offset(nd)),
                                    jl_ppvalue_llvmt));
                            builder.CreateBr(mergeBB);
                            builder.SetInsertPoint(mergeBB);
                            data_owner = builder.CreatePHI(jl_pvalue_llvmt, 2);
                            data_owner->addIncoming(ary, curBB);
                            data_owner->addIncoming(own_ptr, ownedBB);
                        }
                        typed_store(emit_arrayptr(ary,args[1],ctx), idx, v,
                                    ety, ctx, tbaa_user, data_owner);
                    }
                    JL_GC_POP();
                    return ary;
                }
            }
        }
    }
    else if (f->fptr == &jl_f_get_field && nargs==2) {
        if (jl_is_quotenode(args[2]) && jl_is_symbol(jl_fieldref(args[2],0))) {
            Value *fld = emit_getfield(args[1],
                                       (jl_sym_t*)jl_fieldref(args[2],0), ctx);
            JL_GC_POP();
            return fld;
        }
        jl_datatype_t *stt = (jl_datatype_t*)expr_type(args[1], ctx);
        jl_value_t *fldt   = expr_type(args[2], ctx);

        // VA tuple
        if (ctx->vaStack && symbol_eq(args[1], ctx->vaName)) {
            Value *valen = emit_n_varargs(ctx);
            Value *idx = emit_unbox(T_size,
                                    emit_unboxed(args[2], ctx),fldt);
            idx = emit_bounds_check(builder.CreateGEP(ctx->argArray, ConstantInt::get(T_size, ctx->nReqArgs)),
                                    (jl_value_t*)jl_any_type, idx, valen, ctx);
            idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
            JL_GC_POP();
            return tbaa_decorate(tbaa_user, builder.
                                 CreateLoad(builder.CreateGEP(ctx->argArray,idx),false));
        }

        if (fldt == (jl_value_t*)jl_long_type && jl_is_leaf_type((jl_value_t*)stt)) {
            if ((jl_is_structtype(stt) || jl_is_tuple_type(stt)) && !jl_subtype((jl_value_t*)jl_module_type, (jl_value_t*)stt, 0)) {
                size_t nfields = jl_datatype_nfields(stt);
                Value *strct = emit_expr(args[1], ctx);
                // integer index
                Value *fld;
                size_t idx;
                if (jl_is_long(args[2]) && (idx=jl_unbox_long(args[2])-1) < nfields) {
                    // known index
                    fld = emit_getfield_knownidx(strct, idx, stt, ctx);
                }
                else {
                    // unknown index
                    Value *vidx = emit_unbox(T_size, emit_unboxed(args[2], ctx), (jl_value_t*)jl_long_type);
                    fld = emit_getfield_unknownidx(strct, vidx, stt, ctx);
                }
                if (fld != NULL) {
                    JL_GC_POP();
                    return fld;
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
                    Value *strct = emit_expr(args[1], ctx);
                    Value *rhs;
                    if (sty->fields[idx].isptr)
                        rhs = emit_expr(args[3], ctx);
                    else
                        rhs = emit_unboxed(args[3], ctx);
                    emit_setfield(sty, strct, idx, rhs, ctx, true, true);
                    JL_GC_POP();
                    return rhs;
                }
            }
        }
        // TODO: faster code for integer index
    }
    else if (f->fptr == &jl_f_nfields && nargs==1) {
        if (ctx->vaStack && symbol_eq(args[1], ctx->vaName) && !ctx->vars[ctx->vaName].isAssigned) {
            JL_GC_POP();
            return emit_n_varargs(ctx);
        }
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_type_type(aty)) {
            jl_value_t *tp0 = jl_tparam0(aty);
            if (jl_is_leaf_type(tp0)) {
                emit_expr(args[1], ctx);
                JL_GC_POP();
                assert(jl_is_datatype(tp0));
                return ConstantInt::get(T_size, jl_datatype_nfields(tp0));
            }
        }
        else if (jl_is_leaf_type(aty)) {
            Value *arg1 = emit_expr(args[1], ctx);
            JL_GC_POP();
            if (aty == (jl_value_t*)jl_datatype_type)
                return emit_datatype_nfields(arg1);
            return ConstantInt::get(T_size, jl_datatype_nfields(aty));
        }
    }
    else if (f->fptr == &jl_f_field_type && nargs==2) {
        jl_datatype_t *sty = (jl_datatype_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_type_type((jl_value_t*)sty) || sty == jl_datatype_type) {
            rt2 = expr_type(args[2], ctx); // index argument type
            if (rt2 == (jl_value_t*)jl_long_type) {
                Value *ty = emit_expr(args[1], ctx);
                Value *types_svec = emit_datatype_types(ty);
                Value *types_len = emit_datatype_nfields(ty);
                Value *idx = emit_unboxed(args[2], ctx);
                emit_bounds_check(ty, NULL, idx, types_len, ctx);
                Value *fieldtyp = builder.CreateLoad(builder.CreateGEP(builder.CreateBitCast(types_svec, jl_ppvalue_llvmt), idx));
                JL_GC_POP();
                return fieldtyp;
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
            // exclude DataType, since each DataType has its own size, not sizeof(DataType).
            // this is issue #8798
            sty != jl_datatype_type) {
            if (jl_is_leaf_type((jl_value_t*)sty) ||
                (sty->name->names == jl_emptysvec && sty->size > 0)) {
                JL_GC_POP();
                return ConstantInt::get(T_size, sty->size);
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
                JL_GC_POP();
                return literal_pointer_val(ty);
            }
        }
    }
    // TODO: other known builtins
    JL_GC_POP();
    return NULL;
}

static Value *emit_jlcall(Value *theFptr, Value *theF, int argStart,
                          size_t nargs, jl_codectx_t *ctx)
{
    // call
    Value *myargs = Constant::getNullValue(jl_ppvalue_llvmt);
    if (ctx->argTemp != NULL && nargs > 0) {
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
    }
#ifdef LLVM37
    Value *result = builder.CreateCall(prepare_call(theFptr), {theF, myargs,
                                        ConstantInt::get(T_int32,nargs)});
#else
    Value *result = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
#endif
    ctx->argDepth = argStart;
    return result;
}

static Value *emit_jlcall(Value *theFptr, Value *theF, jl_value_t **args,
                          size_t nargs, jl_codectx_t *ctx)
{
    // emit arguments
    int argStart = ctx->argDepth;
    for(size_t i=0; i < nargs; i++) {
        jl_sym_t *sym = NULL;
        Value *anArg = emit_expr(args[i], ctx, true, true, &sym);
        // put into argument space
        make_gcroot(boxed(anArg, ctx, expr_type(args[i],ctx)), ctx, sym);
    }
    return emit_jlcall(theFptr, theF, argStart, nargs, ctx);
}

static Value *emit_call_function_object(jl_function_t *f, Value *theF, Value *theFptr,
                                        bool specialized,
                                        jl_value_t **args, size_t nargs,
                                        jl_codectx_t *ctx)
{
    Value *result;
    if (f!=NULL && specialized && f->linfo!=NULL && f->linfo->specFunctionObject!=NULL) {
        // emit specialized call site
        Function *cf = (Function*)f->linfo->specFunctionObject;
        FunctionType *cft = cf->getFunctionType();
        size_t nfargs = cft->getNumParams();
        Value **argvals = (Value**) alloca(nfargs*sizeof(Value*));
        unsigned idx = 0;
        for(size_t i=0; i < nargs; i++) {
            Type *at = cft->getParamType(idx);
            jl_value_t *jt = jl_nth_slot_type(f->linfo->specTypes,i);
            Type *et = julia_type_to_llvm(jt);
            if (et == T_void || et->isEmptyTy()) {
                // Still emit the expression in case it has side effects
                emit_expr(args[i+1], ctx);
                continue;
            }
            if (at == jl_pvalue_llvmt) {
                Value *origval = emit_expr(args[i+1], ctx);
                argvals[idx] = boxed(origval,ctx,expr_type(args[i+1],ctx));
                assert(dyn_cast<UndefValue>(argvals[idx]) == 0);
                // TODO: there should be a function emit_rooted that handles this, leaving
                // the value rooted if it was already, to avoid redundant stores.
                if (origval->getType() != jl_pvalue_llvmt ||
                    (might_need_root(args[i+1]) && !is_stable_expr(args[i+1], ctx))) {
                    make_gcroot(argvals[idx], ctx);
                }
            }
            else {
                assert(at == et);
                argvals[idx] = emit_unbox(at, emit_unboxed(args[i+1], ctx), jt);
                assert(dyn_cast<UndefValue>(argvals[idx]) == 0);
            }
            idx++;
        }
        assert(idx == nfargs);
        result = builder.CreateCall(prepare_call(cf), ArrayRef<Value*>(&argvals[0],nfargs));
        result = mark_julia_type(result, jl_ast_rettype(f->linfo, f->linfo->ast));
    }
    else {
        result = emit_jlcall(theFptr, theF, &args[1], nargs, ctx);
    }
    return result;
}

static Value *emit_is_function(Value *x, jl_codectx_t *ctx)
{
    Value *xty = emit_typeof(x);
    Value *isfunc =
        builder.CreateICmpEQ(xty, literal_pointer_val((jl_value_t*)jl_function_type));
    return isfunc;
}

static Value *emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx, jl_value_t *expr)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theF=NULL;
    jl_value_t *a0 = args[0];
    jl_value_t *hdtype;
    bool headIsGlobal = false;
    bool definitely_function = false;
    bool definitely_not_function = false;

    jl_function_t *f = (jl_function_t*)static_eval(a0, ctx, true);
    JL_GC_PUSH1(&f);
    if (f != NULL) {
        // function is a compile-time constant
        Value *result;
        headIsGlobal = true;
        definitely_function = jl_is_func(f);
        definitely_not_function = !definitely_function;
        if (jl_typeis(f, jl_intrinsic_type) || jl_is_func(f)) {
            result = emit_known_call((jl_value_t*)f, args, nargs, ctx, &theFptr, &f, expr);
            assert(!jl_typeis(f,jl_intrinsic_type) || result!=NULL);
        }
        else {
            result = emit_known_call((jl_value_t*)jl_module_call_func(ctx->module),
                                     args-1, nargs+1, ctx, &theFptr, &f, expr);
        }
        if (result != NULL) {
            JL_GC_POP();
            return result;
        }
    }

    hdtype = expr_type(a0, ctx);
    definitely_function |= (hdtype == (jl_value_t*)jl_function_type);
    definitely_not_function |= (jl_is_leaf_type(hdtype) && !definitely_function);

    assert(!(definitely_function && definitely_not_function));

    int last_depth = ctx->argDepth;
    Value *result;

    if (definitely_not_function) {
        f = jl_module_call_func(ctx->module);
        Value *r = emit_known_call((jl_value_t*)f, args-1, nargs+1, ctx, &theFptr, &f, expr);
        assert(r == NULL); (void) r;
        if (theFptr == NULL) {
            just_emit_error("\"call\" is not a function", ctx);
            result = UndefValue::get(jl_pvalue_llvmt);
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
                Value *theFunc = emit_expr(args[0], ctx);
#ifdef JL_GC_MARKSWEEP
                if (!headIsGlobal && (jl_is_expr(a0) || jl_is_lambda_info(a0)))
                    make_gcroot(boxed(theFunc,ctx), ctx);
#endif
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
        int argStart = ctx->argDepth;
        Value *theFunc = boxed(emit_expr(args[0], ctx), ctx);
        make_gcroot(theFunc, ctx);
        for(size_t i=0; i < nargs; i++) {
            Value *anArg = emit_expr(args[i+1], ctx);
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
        Value *myargs = Constant::getNullValue(jl_ppvalue_llvmt);
        if (ctx->argTemp != NULL && nargs > 0) {
            myargs = builder.CreateGEP(ctx->argTemp,
                                       ConstantInt::get(T_size, argStart+1+ctx->argSpaceOffs));
        }
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
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
#ifdef LLVM37
        Value *r2 = builder.CreateCall(prepare_call(jlapplygeneric_func),
                                        {literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
                                        myargs,
                                        ConstantInt::get(T_int32,nargs+1)});
#else
        Value *r2 = builder.CreateCall3(prepare_call(jlapplygeneric_func),
                                        literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
                                        myargs,
                                        ConstantInt::get(T_int32,nargs+1));
#endif
        builder.CreateBr(mergeBB1);
        ctx->f->getBasicBlockList().push_back(mergeBB1);
        builder.SetInsertPoint(mergeBB1);
        PHINode *ph = builder.CreatePHI(jl_pvalue_llvmt, 2);
        ph->addIncoming(r1, funcBB1);
        ph->addIncoming(r2, elseBB1);
        result = ph;
    }

    ctx->argDepth = last_depth;
    JL_GC_POP();
    return result;
}

// --- accessing and assigning variables ---

static bool isBoxed(jl_sym_t *varname, jl_codectx_t *ctx)
{
    std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find(varname);
    if (it == ctx->vars.end())
        return false;
    jl_varinfo_t &vi = (*it).second;
    return vi.isAssigned && vi.isCaptured;
}

static int is_var_closed(jl_sym_t *s, jl_codectx_t *ctx)
{
    std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find(s);
    if (it == ctx->vars.end())
        return false;
    jl_varinfo_t &vi = (*it).second;
    return (vi.closureidx != -1);
}

static int is_global(jl_sym_t *s, jl_codectx_t *ctx)
{
    std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx->vars.find(s);
    return (it == ctx->vars.end());
}

static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign)
{
    jl_binding_t *b=NULL;
    if (!assign)
        b = jl_get_binding(m, s);
    // if b is NULL, this might be a global that is not set yet but will be,
    // so get a pointer for writing even when not assigning.
    if (assign || b==NULL)
        b = jl_get_binding_wr(m, s);
    if (pbnd) *pbnd = b;
    return julia_binding_gv(b);
}

static bool is_stack(Value *v)
{
    if (isa<AllocaInst>(v)) return true;
    GetElementPtrInst *i = dyn_cast<GetElementPtrInst>(v);
    if (i && is_stack(i->getOperand(0))) return true;
    return false;
}

// yields a jl_value_t** giving the binding location of a variable
static Value *var_binding_pointer(jl_sym_t *s, jl_binding_t **pbnd,
                                  bool assign, jl_codectx_t *ctx)
{
    if (jl_is_symbolnode(s))
        s = jl_symbolnode_sym(s);
    assert(jl_is_symbol(s));
    if (is_global(s, ctx)) {
        return global_binding_pointer(ctx->module, s, pbnd, assign);
    }
    jl_varinfo_t &vi = ctx->vars[s];
    if (vi.closureidx != -1) {
        int idx = vi.closureidx;
        assert(((Value*)ctx->envArg)->getType() == jl_pvalue_llvmt);
        if (isBoxed(s, ctx)) {
#ifdef OVERLAP_SVEC_LEN
            return builder.CreatePointerCast(emit_nthptr((Value*)ctx->envArg, idx, tbaa_sveclen), jl_ppvalue_llvmt);
#else
            return builder.CreatePointerCast(emit_nthptr((Value*)ctx->envArg, idx+1, tbaa_sveclen), jl_ppvalue_llvmt);
#endif
        }
#ifdef OVERLAP_SVEC_LEN
        return emit_nthptr_addr((Value*)ctx->envArg, idx);
#else
        return emit_nthptr_addr((Value*)ctx->envArg, idx+1);
#endif
    }
    Value *l = vi.memvalue;
    if (l == NULL) return NULL;
    if (isBoxed(s, ctx)) {
        return builder.CreatePointerCast(builder.CreateLoad(l,false), jl_ppvalue_llvmt);
    }
    return l;
}

static Value *emit_checked_var(Value *bp, jl_sym_t *name, jl_codectx_t *ctx, bool isvol)
{
    Value *v = tpropagate(bp, builder.CreateLoad(bp, isvol));
    // in unreachable code, there might be a poorly-typed instance of a variable
    // that has a concrete type everywhere it's actually used. tolerate this
    // situation by just skipping the NULL check if it wouldn't be valid. (issue #7836)
    if (v->getType() == jl_pvalue_llvmt) {
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
    return v;
}

static Value *ghostValue(jl_value_t *ty)
{
    assert(jl_is_datatype(ty));
    return mark_julia_type(UndefValue::get(NoopType),ty);
}

static Value *emit_var(jl_sym_t *sym, jl_value_t *ty, jl_codectx_t *ctx, bool isboxed)
{
    bool isglobal = is_global(sym, ctx);
    if (isglobal) {
        // look for static parameter
        for(size_t i=0; i < jl_svec_len(ctx->sp); i+=2) {
            assert(jl_is_symbol(jl_svecref(ctx->sp, i)));
            if (sym == (jl_sym_t*)jl_svecref(ctx->sp, i)) {
                return literal_pointer_val(jl_svecref(ctx->sp, i+1));
            }
        }
        jl_binding_t *jbp=NULL;
        Value *bp = var_binding_pointer(sym, &jbp, false, ctx);
        if (bp == NULL)
            return NULL;
        assert(jbp != NULL);
        if (jbp->value != NULL) {
            if (jbp->constp) {
                if (!isboxed && jl_isbits(jl_typeof(jbp->value)))
                    return emit_unboxed(jbp->value, ctx);
            }
            // double-check that a global variable is actually defined. this
            // can be a problem in parallel when a definition is missing on
            // one machine.
            return tpropagate(bp, builder.CreateLoad(bp, false));
        }
        return emit_checked_var(bp, sym, ctx);
    }

    jl_varinfo_t &vi = ctx->vars[sym];

    Value *arg = vi.passedAs;
    if (arg!=NULL && arg!=V_null && !vi.isAssigned &&
        (isboxed || vi.memvalue == NULL)) {
        // if we need a boxed version of an argument that's not assigned,
        // use the original value.
        return arg;
    }
    if (vi.SAvalue != NULL)
        return vi.SAvalue;

    jl_binding_t *jbp=NULL;
    Value *bp = var_binding_pointer(sym, &jbp, false, ctx);
    if (bp == NULL) {
        assert(vi.isGhost);
        return ghostValue(ty);
    }
    assert(jbp == NULL);
    if (arg != NULL ||    // arguments are always defined
        ((!is_var_closed(sym, ctx) || !vi.isAssigned) && !vi.usedUndef)) {
        Value *theLoad = builder.CreateLoad(bp, vi.isVolatile);
        if (vi.closureidx > -1 && !(vi.isAssigned && vi.isCaptured))
            theLoad = tbaa_decorate(tbaa_const, (Instruction*)theLoad);
        return tpropagate(bp, theLoad);
    }
    return emit_checked_var(bp, sym, ctx, vi.isVolatile);
}

static Value *emit_assignment(Value *bp, jl_value_t *r, jl_value_t *declType, bool isVolatile, bool used, jl_codectx_t *ctx)
{
    Value *rval;
    jl_value_t *rt = expr_type(r,ctx);
    if (bp != NULL) {
        if ((jl_is_symbol(r) || jl_is_symbolnode(r) || jl_is_gensym(r)) && rt == jl_bottom_type) {
            // sometimes x = y::Union() occurs
            if (!jl_is_gensym(r)) {
                jl_sym_t *s;
                if (jl_is_symbolnode(r))
                    s = jl_symbolnode_sym(r);
                else
                    s = (jl_sym_t*)r;
                jl_varinfo_t &vi = ctx->vars[s];
                if (vi.usedUndef)
                    builder.CreateCall(prepare_call(jlundefvarerror_func), literal_pointer_val((jl_value_t*)s));
            }
            return UndefValue::get(bp->getType()->getContainedType(0));
        }
        Type *vt = bp->getType();
        if (vt != jl_ppvalue_llvmt) { // unboxed store (in an alloca)
            // `rt` is technically correct here, but sometimes we're not propagating type information
            // properly, so `rt` is a union type, while LLVM know that it's not. However, in order for this to
            // happen, we need to already be sure somewhere that we have the right type, so vi.declType is fine
            // even if not technically correct.
            rval = mark_julia_type(emit_unbox(vt->getContainedType(0), emit_unboxed(r, ctx), declType), declType);
        }
        else {
            rval = boxed(emit_expr(r, ctx, true),ctx,rt);
            if (!is_stack(bp)) { // bp is a jl_box_t*
                emit_write_barrier(ctx, bp, rval);
            }
        }
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            builder.CreateStore(rval, bp, isVolatile);
        }
    }
    else {
        rval = emit_expr(r, ctx, true);

        // don't need to store this if it isn't used
        // and sometimes we can get x::Union() = Expr(:tuple)::() in dead code
        if (!used || declType == jl_bottom_type)
            return UndefValue::get(rval->getType());

        // Make sure this is already boxed. If not, there was
        // something wrong in the earlier analysis as this should
        // have been alloca'd
        assert(rval->getType() == jl_pvalue_llvmt || rval->getType() == NoopType);
    }
    assert(rval);
    return rval;
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    if (jl_is_gensym(l)) {
        ssize_t idx = ((jl_gensym_t*)l)->id;
        assert(idx >= 0);
        assert(!ctx->gensym_assigned.at(idx));
        Value *bp = ctx->gensym_SAvalues.at(idx); // at this point, gensym_SAvalues[idx] actually contains the memvalue (if isbits)
        jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
        jl_value_t *declType = (jl_is_array(gensym_types) ? jl_cellref(gensym_types, idx) : (jl_value_t*)jl_any_type);
        Value *rval = emit_assignment(bp, r, declType, false, true, ctx);
        ctx->gensym_SAvalues.at(idx) = rval; // now gensym_SAvalues[idx] actually contains the SAvalue
        assert(ctx->gensym_assigned.at(idx) = true);
        return;
    }
    jl_sym_t *s = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_symbolnode(l))
        s = jl_symbolnode_sym(l);
    else
        assert(false);
    jl_binding_t *bnd=NULL;
    Value *bp = var_binding_pointer(s, &bnd, true, ctx);
    if (bnd) {
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
    }
    else {
        jl_varinfo_t &vi = ctx->vars[s];
        Value *rval = emit_assignment(bp, r, vi.declType, vi.isVolatile, vi.used, ctx);

        if (vi.isSA &&
            ((bp == NULL) ||
             (!vi.isCaptured && !vi.isArgument &&
              !vi.usedUndef && !vi.isVolatile))) {
            // use SSA value instead of GC frame load for var access
            vi.SAvalue = rval;
        }

        if (!isa<UndefValue>(rval) && builder.GetInsertBlock()->getTerminator() == NULL) {
            jl_arrayvar_t *av = arrayvar_for(l, ctx);
            if (av != NULL) {
                assign_arrayvar(*av, rval);
            }
        }
    }
}

// --- convert expression to code ---

static Value *emit_condition(jl_value_t *cond, const std::string &msg, jl_codectx_t *ctx)
{
    Value *condV = emit_unboxed(cond, ctx);
    if (expr_type(cond, ctx) != (jl_value_t*)jl_bool_type &&
        condV->getType() != T_int1) {
        emit_typecheck(condV, (jl_value_t*)jl_bool_type, msg, ctx);
    }
    if (condV->getType() == T_int1) {
        return builder.CreateXor(condV, ConstantInt::get(T_int1,1));
    }
    else if (condV->getType() == jl_pvalue_llvmt) {
        return builder.CreateICmpEQ(condV, tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlfalse_var))));
    }
    // not a boolean
    return ConstantInt::get(T_int1,0);
}

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool isboxed,
                        bool valuepos, jl_sym_t **valuevar)
{
    if (jl_is_symbol(expr)) {
        if (!valuepos) return NULL;
        jl_sym_t *sym = (jl_sym_t*)expr;
        if (valuevar != NULL)
            *valuevar = sym;
        return emit_var(sym, (jl_value_t*)jl_any_type, ctx, isboxed);
    }
    if (jl_is_symbolnode(expr)) {
        if (!valuepos) return NULL;
        jl_sym_t *sym = jl_symbolnode_sym(expr);
        if (valuevar != NULL)
            *valuevar = sym;
        return emit_var(sym, jl_symbolnode_type(expr), ctx, isboxed);
    }
    if (jl_is_gensym(expr)) {
        if (!valuepos) return NULL;
        ssize_t idx = ((jl_gensym_t*)expr)->id;
        assert(idx >= 0);
        assert(ctx->gensym_assigned.at(idx));
        Value *bp = ctx->gensym_SAvalues.at(idx); // at this point, gensym_SAvalues[idx] actually contains the SAvalue
        if (bp == NULL || type_is_ghost(bp->getType())) {
            // assert(vi.isGhost);
            jl_value_t *gensym_types = jl_lam_gensyms(ctx->ast);
            jl_value_t *declType = (jl_is_array(gensym_types) ? jl_cellref(gensym_types, idx) : (jl_value_t*)jl_any_type);
            return ghostValue(declType);
        }
        return bp;
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
        return NULL;
    }
    if (jl_is_linenode(expr)) {
        if (valuepos)
            jl_error("Linenode in value position");
        return NULL;
    }
    if (jl_is_quotenode(expr)) {
        jl_value_t *jv = jl_fieldref(expr,0);
        if (jl_is_bitstype(jl_typeof(jv))) {
            return emit_expr(jv, ctx, isboxed, valuepos);
        }
        if (!jl_is_symbol(jv)) {
            jl_add_linfo_root(ctx->linfo, jv);
        }
        return literal_pointer_val(jv);
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
        return NULL;
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
        Value *bp = julia_binding_gv(b);
        if (b->constp && b->value!=NULL) {
            return builder.CreateLoad(bp, false);
        }
        return emit_checked_var(bp, var, ctx);
    }
    if (jl_is_newvarnode(expr)) {
        assert(!valuepos);
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
        assert(!jl_is_gensym(var));
        assert(jl_is_symbol(var));
        jl_varinfo_t &vi = ctx->vars[var];
        Value *lv = vi.memvalue;
        if (lv != NULL) {
            // create a new uninitialized variable
            if (isBoxed(var, ctx)) {
                builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), V_null), lv);
            }
            else if (lv->getType() == jl_ppvalue_llvmt && vi.usedUndef) {
                builder.CreateStore(V_null, lv);
            }
        }
        return NULL;
    }
    if (!jl_is_expr(expr)) {
        // numeric literals
        int needroot = 1;
        if (jl_is_int32(expr)) {
            needroot = !((uint32_t)(jl_unbox_int32(expr)+512) < 1024);
        }
        else if (jl_is_int64(expr)) {
            needroot = !((uint64_t)(jl_unbox_int64(expr)+512) < 1024);
        }
        else if (jl_is_lambda_info(expr)) {
            return emit_lambda_closure(expr, ctx);
        }
        if (needroot) {
            jl_add_linfo_root(ctx->linfo, expr);
        }
        return literal_pointer_val(expr);
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
    else if (head == call_sym || head == call1_sym) {
        return emit_call(args, jl_array_dim0(ex->args), ctx, (jl_value_t*)ex);
    }
    else if (head == assign_sym) {
        emit_assignment(args[0], args[1], ctx);
        if (valuepos) {
            return literal_pointer_val((jl_value_t*)jl_nothing);
        }
    }
    else if (head == method_sym) {
        jl_value_t *mn = args[0];
        bool iskw = false;
        Value *theF = NULL;
        if (jl_is_expr(mn)) {
            if (((jl_expr_t*)mn)->head == kw_sym) {
                iskw = true;
                mn = jl_exprarg(mn,0);
            }
            theF = emit_expr(mn, ctx);
            if (!iskw) {
                mn = jl_fieldref(jl_exprarg(mn, 2), 0);
            }
        }
        if (jl_is_symbolnode(mn)) {
            mn = (jl_value_t*)jl_symbolnode_sym(mn);
        }
        assert(jl_is_symbol(mn));
        int last_depth = ctx->argDepth;
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
                bp = var_binding_pointer((jl_sym_t*)mn, &bnd, false, ctx);
                if (isBoxed((jl_sym_t*)mn, ctx)) { // bp is a jl_box_t*
                    bp_owner = builder.CreateBitCast(bp, jl_pvalue_llvmt);
                }
            }
        }
        Value *a1 = boxed(emit_expr(args[1], ctx),ctx);
        make_gcroot(a1, ctx);
        Value *a2 = boxed(emit_expr(args[2], ctx),ctx);
        make_gcroot(a2, ctx);
        Value *mdargs[9] =
            { name, bp, bp_owner, literal_pointer_val(bnd), a1, a2, literal_pointer_val(args[3]),
              literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
              ConstantInt::get(T_int32, (int)iskw) };
        ctx->argDepth = last_depth;
        return builder.CreateCall(prepare_call(jlmethod_func), ArrayRef<Value*>(&mdargs[0], 9));
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        assert(jl_is_symbol(sym));
        jl_binding_t *bnd = NULL;
        (void)var_binding_pointer(sym, &bnd, true, ctx);
        if (bnd) {
            builder.CreateCall(prepare_call(jldeclareconst_func),
                               literal_pointer_val(bnd));
        }
    }

    else if (head == null_sym) {
        return literal_pointer_val((jl_value_t*)jl_nothing);
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
        return literal_pointer_val(extype);
    }
    else if (head == new_sym) {
        jl_value_t *ty = expr_type(args[0], ctx);
        size_t nargs = jl_array_len(ex->args);
        if (jl_is_type_type(ty) &&
            jl_is_datatype(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            return emit_new_struct(jl_tparam0(ty),nargs,args,ctx);
        }
        Value *typ = emit_expr(args[0], ctx);
        return emit_jlcall(jlnew_func, typ, &args[1], nargs-1, ctx);
    }
    else if (head == exc_sym) {
        return builder.CreateLoad(prepare_global(jlexc_var), true);
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
        builder.CreateCall(prepare_call(resetstkoflw_func));
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
        if (valuepos)
            return literal_pointer_val((jl_value_t*)jl_nothing);
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
        return builder.CreateCall(prepare_call(jlcopyast_func), emit_expr(arg, ctx));
    }
    else if (head == simdloop_sym) {
        if (!llvm::annotateSimdLoop(builder.GetInsertBlock()))
            jl_printf(JL_STDERR, "Warning: could not attach metadata for @simd loop.\n");
        return NULL;
    }
    else if (head == meta_sym) {
        return literal_pointer_val((jl_value_t*)jl_nothing);  // will change as new metadata gets added
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
            return valuepos ? literal_pointer_val(jl_nothing) : NULL;
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
    return NULL;
}

// --- allocating local variables ---

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
            s != ctx->vaName && store_unboxed_p(vi.declType));
}

static Value *alloc_local(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->vars[s];
    jl_value_t *jt = vi.declType;
    Value *lv = NULL;
    assert(store_unboxed_p(s,ctx));
    Type *vtype = julia_struct_to_llvm(jt);
    assert(vtype != jl_pvalue_llvmt);
    if (!type_is_ghost(vtype)) {
        lv = builder.CreateAlloca(vtype, 0, s->name);
        if (vtype != jl_pvalue_llvmt)
            lv = mark_julia_type(lv, jt);
        vi.isGhost = false;
        assert(lv != NULL);
    }
    else {
        vi.isGhost = true;
    }
    vi.memvalue = lv;
#ifdef LLVM36
    if (!vi.isGhost && ctx->debug_enabled) {
#ifdef LLVM37
        ctx->dbuilder->insertDeclare(lv,vi.dinfo,ctx->dbuilder->createExpression(),
            builder.getCurrentDebugLocation().get(),builder.GetInsertBlock());
#else
        ctx->dbuilder->insertDeclare(lv,vi.dinfo,ctx->dbuilder->createExpression(),builder.GetInsertBlock());
#endif
    }
#endif
    return lv;
}

static void maybe_alloc_arrayvar(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_value_t *jt = ctx->vars[s].declType;
    if (jl_is_array_type(jt) && jl_is_leaf_type(jt) && jl_is_long(jl_tparam1(jt)) &&
        jl_unbox_long(jl_tparam1(jt)) != 1) {
        // TODO: this optimization does not yet work with 1-d arrays, since the
        // length and data pointer can change at any time via push!
        // we could make it work by reloading the metadata when the array is
        // passed to an external function (ideally only impure functions)
        jl_arrayvar_t av;
        int ndims = jl_unbox_long(jl_tparam1(jt));
        Type *elt = julia_type_to_llvm(jl_tparam0(jt));
        if (elt == T_void)
            return;
        av.dataptr = builder.CreateAlloca(PointerType::get(elt,0));
        av.len = builder.CreateAlloca(T_size);
        for(int i=0; i < ndims-1; i++)
            av.sizes.push_back(builder.CreateAlloca(T_size));
        av.ty = jt;
        (*ctx->arrayvars)[s] = av;
    }
}

// --- generate function bodies ---

extern char *jl_stack_lo;

extern "C" jl_svec_t *jl_svec_tvars_to_symbols(jl_svec_t *t);

// gc frame emission
static void allocate_gc_frame(size_t n_roots, BasicBlock *b0, jl_codectx_t *ctx)
{
    ctx->argSpaceOffs = n_roots;
    ctx->argDepth = 0;
    ctx->maxDepth = 0;

#ifdef JL_GC_MARKSWEEP
    // allocate gc frame
    ctx->argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                       ConstantInt::get(T_int32,n_roots+2));
    ctx->gcframe = (Instruction*)ctx->argTemp;
    ctx->first_gcframe_inst = BasicBlock::iterator(ctx->gcframe);
    ctx->argTemp = (Instruction*)builder.CreateConstGEP1_32(ctx->argTemp, 2);
    ctx->storeFrameSize =
        builder.CreateStore(ConstantInt::get(T_size, n_roots<<1),
                            builder.CreateBitCast(builder.CreateConstGEP1_32(ctx->gcframe, 0), T_psize));
    builder.CreateStore(builder.CreateLoad(prepare_global(jlpgcstack_var), false),
                        builder.CreateBitCast(builder.CreateConstGEP1_32(ctx->gcframe, 1), PointerType::get(jl_ppvalue_llvmt,0)));
    Instruction *linst = builder.CreateStore(ctx->gcframe, prepare_global(jlpgcstack_var), false);
    ctx->argSpaceInits = &b0->back();
#else
    ctx->argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                       ConstantInt::get(T_int32, n_roots));
    ctx->first_gcframe_inst = BasicBlock::iterator(ctx->argTemp);
    Instruction *linst = ctx->argTemp;
#endif
    // initialize local variable stack roots to null
    for(size_t i=0; i < (size_t)ctx->argSpaceOffs; i++) {
        Value *varSlot = builder.CreateConstGEP1_32(ctx->argTemp,i);
        linst = builder.CreateStore(V_null, varSlot);
    }
    ctx->last_gcframe_inst = BasicBlock::iterator(linst);
}

static void finalize_gc_frame(jl_codectx_t *ctx)
{
#ifdef JL_GC_MARKSWEEP
    if (ctx->argSpaceOffs + ctx->maxDepth == 0) {
        // 0 roots; remove gc frame entirely
        // replace instruction uses with Undef first to avoid LLVM assertion failures
        BasicBlock::iterator bbi = ctx->first_gcframe_inst;
        while (1) {
            Instruction &iii = *bbi;
            Type *ty = iii.getType();
            if (ty != T_void)
                iii.replaceAllUsesWith(UndefValue::get(ty));
            if (bbi == ctx->last_gcframe_inst) break;
            bbi++;
        }
        for(size_t i=0; i < ctx->gc_frame_pops.size(); i++) {
            Instruction *pop = ctx->gc_frame_pops[i];
            BasicBlock::iterator pi(pop);
            for(size_t j=0; j < 4; j++) {
                Instruction &iii = *pi;
                Type *ty = iii.getType();
                if (ty != T_void)
                    iii.replaceAllUsesWith(UndefValue::get(ty));
                pi++;
            }
        }

        BasicBlock::InstListType &il = ctx->gcframe->getParent()->getInstList();
        il.erase(ctx->first_gcframe_inst, ctx->last_gcframe_inst);
        // erase() erases up *to* the end point; erase last inst too
        il.erase(ctx->last_gcframe_inst);
        for(size_t i=0; i < ctx->gc_frame_pops.size(); i++) {
            Instruction *pop = ctx->gc_frame_pops[i];
            BasicBlock::InstListType &il2 = pop->getParent()->getInstList();
            BasicBlock::iterator pi(pop);
            for(size_t j=0; j < 4; j++) {
                pi = il2.erase(pi);
            }
        }
    }
    else {
        //n_frames++;
        BasicBlock::iterator bbi(ctx->gcframe);
        AllocaInst *newgcframe =
            new AllocaInst(jl_pvalue_llvmt,
                           ConstantInt::get(T_int32, (ctx->argSpaceOffs +
                                                      ctx->maxDepth + 2)));
        ReplaceInstWithInst(ctx->argTemp->getParent()->getInstList(), bbi,
                            newgcframe);

        BasicBlock::iterator bbi2(ctx->storeFrameSize);
        StoreInst *newFrameSize =
            new StoreInst(ConstantInt::get(T_size, (ctx->argSpaceOffs +
                                                    ctx->maxDepth)<<1),
                          ctx->storeFrameSize->getPointerOperand());
        ReplaceInstWithInst(ctx->storeFrameSize->getParent()->getInstList(), bbi2,
                            newFrameSize);

        BasicBlock::InstListType &instList = ctx->argSpaceInits->getParent()->getInstList();
        Instruction *after = ctx->argSpaceInits;

        for(size_t i=0; i < (size_t)ctx->maxDepth; i++) {
#ifdef LLVM37
            Instruction *argTempi =
                GetElementPtrInst::Create(NULL,newgcframe,
                                          ConstantInt::get(T_int32, i+ctx->argSpaceOffs+2));
#else
            Instruction *argTempi =
                GetElementPtrInst::Create(newgcframe,
                                          ConstantInt::get(T_int32, i+ctx->argSpaceOffs+2));
#endif
            instList.insertAfter(after, argTempi);
            after = new StoreInst(V_null, argTempi);
            instList.insertAfter(argTempi, after);
        }
    }
#else
    if (ctx->maxDepth != 0) {
        BasicBlock::iterator bbi(ctx->argTemp);
        AllocaInst *newgcframe =
            new AllocaInst(jl_pvalue_llvmt,
                           ConstantInt::get(T_int32, (ctx->argSpaceOffs +
                                                      ctx->maxDepth)));
        ReplaceInstWithInst(ctx->argTemp->getParent()->getInstList(), bbi,
                            newgcframe);
    }
#endif
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
    Type *crt = ((isref&1) ? jl_pvalue_llvmt : julia_struct_to_llvm(jlrettype));
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
    std::vector<Type*> fargt_sig(0);
    std::vector<bool> inRegList(0);
    std::vector<bool> byRefList(0);
    attr_type attrs;
    Type *prt = NULL;
    int sret = 0;
    std::string err_msg = generate_func_sig(&crt, &prt, sret, fargt, fargt_sig, inRegList, byRefList, attrs,
                                            ((isref&1) ? (jl_value_t*)jl_any_type : jlrettype), argt->parameters);
    if (!err_msg.empty())
        jl_error(err_msg.c_str());
    if (fargt.size() != fargt_sig.size())
        jl_error("va_arg syntax not allowed for cfunction argument list");

    std::stringstream funcName;
    funcName << "jlcapi_" << lam->name->name << "_" << globalUnique++;

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
    ctx.linfo = lam;
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
    jl_compile(ff);
    bool specsig;
    Function *theFptr;
    if (lam->specFunctionObject != NULL) {
        theFptr = (Function*)lam->specFunctionObject;
        specsig = true;
    }
    else {
        theFptr = (Function*)lam->functionObject;
        specsig = false;
        if (!theFptr) {
            jl_errorf("error compiling %s while creating cfunction", lam->name->name);
        }
    }

    // Alright, let's do this!
    // let's first emit the arguments
    std::vector<Value*> args;
    Function::arg_iterator AI = cw->arg_begin();
    Value *sretPtr = NULL;
    if (sret)
        sretPtr = AI++; //const Argument &fArg = *AI++;

    for (size_t i=0; i < nargs; i++) {
        Value *val = AI++;
        jl_value_t *jargty = jl_nth_slot_type(lam->specTypes, i);

        // figure out how to unpack this type
        if (isref & (2<<i)) {
            if (!jl_isbits(jargty)) {
                val = builder.CreatePointerCast(val, jl_pvalue_llvmt);
            }
            else {
                Type *t = julia_type_to_llvm(jargty);
                val = builder.CreatePointerCast(val, t->getPointerTo());
                val = builder.CreateLoad(val, false);
            }
        }
        else {
            if (fargt[i+sret] != fargt_sig[i+sret]) {
                // undo whatever we did to this poor argument
                val = llvm_type_rewrite(val, fargt[i+sret], jargty, false);
            }
            if (byRefList[i]) {
                val = builder.CreateLoad(val,false);
            }
        }

        // figure out how to repack this type
        Type *at = specsig ? theFptr->getFunctionType()->getParamType(i) : jl_pvalue_llvmt;
        if (val->getType() != at) {
            if (at == jl_pvalue_llvmt) {
                Value *mem = emit_new_struct(jargty, 1, NULL, &ctx);
                if (mem->getType() == jl_pvalue_llvmt) {
                    builder.CreateStore(val, builder.CreateBitCast(mem, val->getType()->getPointerTo()));
                    val = mem;
                }
                else {
                    val = boxed(mem, &ctx, jargty);
                }
                if (specsig)
                    make_gcroot(val, &ctx);
            }
            else {
                val = emit_unbox(at, val, jargty);
                assert(dyn_cast<UndefValue>(val) == 0);
            }
        }

        // add to argument list
        if (specsig)
            args.push_back(val);
        else
            make_gcroot(val, &ctx);
    }

    // Create the call
    Value *r;
    if (specsig) {
        r = builder.CreateCall(prepare_call(theFptr), ArrayRef<Value*>(args));
    }
    else {
        r = emit_jlcall(theFptr, literal_pointer_val((jl_value_t*)ff), 0, nargs, &ctx);
    }

    // Prepare the return value
    if (isref&1) {
        // return a jl_value_t*
        if (r->getType() != jl_pvalue_llvmt) {
            r = boxed(r, &ctx, jlrettype);
        }
    }

    // gc pop. Usually this is done when we encounter the return statement
    // but here we have to do it manually
#ifdef JL_GC_MARKSWEEP
    Instruction *gcpop = (Instruction*)builder.CreateConstGEP1_32(ctx.gcframe, 1);
    ctx.gc_frame_pops.push_back(gcpop);
    builder.CreateStore(builder.CreateBitCast(builder.CreateLoad(gcpop, false), jl_ppvalue_llvmt),
                        prepare_global(jlpgcstack_var));
#endif
    finalize_gc_frame(&ctx);

    if (isref&1) {
        builder.CreateRet(r);
    }
    else {
        // return an unboxed value
        if (type_is_ghost(crt)) {
            assert(type_is_ghost(prt));
            builder.CreateRetVoid();
        }
        else {
            Value *v = julia_to_native(crt, jlrettype, r, jlrettype, 0, false, false, false, 0, &ctx, NULL);
            if (!sret) {
                builder.CreateRet(llvm_type_rewrite(v, prt, jlrettype, true));
            }
            else {
                Value *sretVal = llvm_type_rewrite(v, fargt_sig[0], jlrettype, true);
                builder.CreateStore(sretVal, sretPtr);
                builder.CreateRetVoid();
            }
        }
    }

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

    return cw;
}

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_lambda_info_t *lam, jl_expr_t *ast, Function *f)
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
    AI++; //const Argument &fArg = *AI++;
    Value *argArray = AI++;
    //const Argument &argCount = *AI++;
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);

    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    jl_codectx_t ctx;
    ctx.linfo = lam;
    allocate_gc_frame(0, b0, &ctx);

    size_t nargs = jl_array_dim0(jl_lam_args(ast));
    size_t nfargs = f->getFunctionType()->getNumParams();
    Value **args = (Value**) alloca(nfargs*sizeof(Value*));
    unsigned idx = 0;
    for(size_t i=0; i < nargs; i++) {
        jl_value_t *ty = jl_nth_slot_type(lam->specTypes, i);
        Type *lty = julia_type_to_llvm(ty);
        if (lty != NULL && type_is_ghost(lty))
            continue;
        Value *argPtr = builder.CreateGEP(argArray,
                                          ConstantInt::get(T_size, i));
        Value *theArg = builder.CreateLoad(argPtr, false);
        Value *theNewArg = theArg;
        if (lty != NULL && lty != jl_pvalue_llvmt) {
            theNewArg = emit_unbox(lty, theArg, ty);
        }
        assert(dyn_cast<UndefValue>(theNewArg) == NULL);
        args[idx] = theNewArg;
        idx++;
    }
    // TODO: consider pulling the function pointer out of fArg so these
    // wrappers can be reused for different functions of the same type.
    Value *r = builder.CreateCall(prepare_call(f), ArrayRef<Value*>(&args[0], nfargs));
    if (r->getType() != jl_pvalue_llvmt) {
        r = boxed(r, &ctx, jl_ast_rettype(lam, (jl_value_t*)ast));
    }

    // gc pop. Usually this is done when we encounter the return statement
    // but here we have to do it manually
#ifdef JL_GC_MARKSWEEP
    Instruction *gcpop = (Instruction*)builder.CreateConstGEP1_32(ctx.gcframe, 1);
    ctx.gc_frame_pops.push_back(gcpop);
    builder.CreateStore(builder.CreateBitCast(builder.CreateLoad(gcpop, false), jl_ppvalue_llvmt),
                        prepare_global(jlpgcstack_var));
#endif
    finalize_gc_frame(&ctx);
    builder.CreateRet(r);

    FPM->run(*w);

    return w;
}

// cstyle = compile with c-callable signature, not jlcall
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
    jl_array_t *lvars = jl_lam_locals(ast);
    size_t lvarslen = jl_array_dim0(lvars);
    size_t nreq = largslen;
    int va = 0;
    if (nreq > 0 && jl_is_rest_arg(jl_cellref(largs,nreq-1))) {
        nreq--;
        va = 1;
        ctx.vaName = jl_decl_var(jl_cellref(largs,nreq));
    }
    ctx.nReqArgs = nreq;

    size_t i;
    for(i=0; i < nreq; i++) {
        jl_sym_t *argname = jl_decl_var(jl_cellref(largs,i));
        jl_varinfo_t &varinfo = ctx.vars[argname];
        varinfo.isArgument = true;
    }
    if (va) {
        ctx.vars[ctx.vaName].isArgument = true;
    }

    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t vinfoslen = jl_array_dim0(vinfos);
    for(i=0; i < vinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
        assert(jl_is_symbol(vname));
        jl_varinfo_t &varinfo = ctx.vars[vname];
        varinfo.isAssigned = (jl_vinfo_assigned(vi)!=0);
        varinfo.isCaptured = (jl_vinfo_capt(vi)!=0);
        varinfo.escapes = varinfo.isCaptured;
        if (varinfo.isCaptured)
            varinfo.used = true;
        varinfo.isSA = (jl_vinfo_sa(vi)!=0);
        varinfo.usedUndef = (jl_vinfo_usedundef(vi)!=0) || (!varinfo.isArgument && !lam->inferred);
        varinfo.declType = jl_cellref(vi,1);
        if (!jl_is_type(varinfo.declType))
            varinfo.declType = (jl_value_t*)jl_any_type;
    }
    vinfos = jl_lam_capt(ast);
    vinfoslen = jl_array_dim0(vinfos);
    bool hasCapt = (vinfoslen > 0);
    for(i=0; i < vinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
        assert(jl_is_symbol(vname));
        jl_varinfo_t &varinfo = ctx.vars[vname];
        varinfo.closureidx = i;
        varinfo.isAssigned = (jl_vinfo_assigned(vi)!=0);
        varinfo.isCaptured = true;
        varinfo.escapes = true;
        varinfo.used = true;
        varinfo.usedUndef = (jl_vinfo_usedundef(vi)!=0) || !lam->inferred;
        varinfo.declType = jl_cellref(vi,1);
        if (!jl_is_type(varinfo.declType))
            varinfo.declType = (jl_value_t*)jl_any_type;
    }

    // step 3. some variable analysis

    // finish recording escape info
    simple_escape_analysis((jl_value_t*)ast, true, &ctx);

    // determine which vars need to be volatile
    jl_array_t *stmts = jl_lam_body(ast)->args;
    mark_volatile_vars(stmts, ctx.vars);

    // fetch init exprs of SSA vars for easy reference
    std::vector<jl_value_t*> gensym_initExpr;
    gensym_initExpr.assign(n_gensyms, (jl_value_t*)NULL);
    for(i=0; i < jl_array_len(stmts); i++) {
        jl_value_t *st = jl_cellref(stmts,i);
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == assign_sym) {
            jl_value_t *lhs = jl_exprarg(st,0);
            if (jl_is_symbolnode(lhs))
                lhs = (jl_value_t*)jl_symbolnode_sym(lhs);
            if (jl_is_symbol(lhs)) {
                std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx.vars.find((jl_sym_t*)lhs);
                if (it != ctx.vars.end()) {
                    jl_varinfo_t &vi = (*it).second;
                    if (vi.isSA) {
                        vi.initExpr = jl_exprarg(st,1);
                    }
                }
            }
            if (jl_is_gensym(lhs)) {
                assert(((jl_gensym_t*)lhs)->id >= 0);
                gensym_initExpr.at(((jl_gensym_t*)lhs)->id) = jl_exprarg(st,1);
            }
        }
    }

    // step 4. determine function signature
    jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)ast);
    Function *f = NULL;

    bool specsig = false;
    if (!va && !hasCapt && lam->specTypes != NULL && lam->inferred) {
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

    if (specsig) { // assumes !va
        std::vector<Type*> fsig(0);
        for(size_t i=0; i < jl_nparams(lam->specTypes); i++) {
            Type *ty = julia_type_to_llvm(jl_tparam(lam->specTypes,i));
            if (type_is_ghost(ty)) {
                // mark as a ghost for now, we'll revise this later if needed as a local
                ctx.vars[jl_decl_var(jl_cellref(largs,i))].isGhost = true;
            }
            else {
                fsig.push_back(ty);
            }
        }
        Type *rt = (jlrettype == (jl_value_t*)jl_void_type ? T_void : julia_type_to_llvm(jlrettype));
        f = Function::Create(FunctionType::get(rt, fsig, false),
                             imaging_mode ? GlobalVariable::InternalLinkage : GlobalVariable::ExternalLinkage,
                             funcName.str(), m);
        addComdat(f);
        if (lam->specFunctionObject == NULL) {
            lam->specFunctionObject = (void*)f;
            lam->specFunctionID = jl_assign_functionID(f);
        }
        if (lam->functionObject == NULL) {
            Function *fwrap = gen_jlcall_wrapper(lam, ast, f);
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
    // look for initial (line num filename) node
    if (jl_is_linenode(stmt)) {
        lno = jl_linenode_line(stmt);
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
    ctx.lineno = lno;

    DIBuilder dbuilder(*m);
    ctx.dbuilder = &dbuilder;
#ifdef LLVM37
    DIFile *fil = NULL;
    DISubprogram *SP;
#else
    DIFile fil;
    DISubprogram SP;
#endif

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
                if (ctx.vars[jl_decl_var(jl_cellref(largs,i))].isGhost)
                    continue;
                ditypes.push_back(julia_type_to_di(jl_tparam(lam->specTypes,i),ctx.dbuilder,false));
            }
#ifdef LLVM36
            subrty = ctx.dbuilder->createSubroutineType(fil,ctx.dbuilder->getOrCreateTypeArray(ditypes));
#else
            subrty = ctx.dbuilder->createSubroutineType(fil,ctx.dbuilder->getOrCreateArray(ditypes));
#endif
        }

        fil = dbuilder.createFile(filename, ".");
        #ifndef LLVM34
        SP = dbuilder.createFunction((DIDescriptor)dbuilder.getCU(),
        #else
        SP = dbuilder.createFunction(CU,
        #endif
                                    dbgFuncName,  // Name
                                    f->getName(), // LinkageName
                                    fil,          // File
                                    0,            // LineNo
                                    subrty,       // Ty
                                    false,        // isLocalToUnit
                                    true,         // isDefinition
                                    0,            // ScopeLine
                                    0,            // Flags
                                    true,         // isOptimized
                                    f);           // Fn
        // set initial line number
        builder.SetCurrentDebugLocation(DebugLoc::get(lno, 0, (MDNode*)SP, NULL));
        #ifndef LLVM37
        assert(SP.Verify() && SP.describes(f) && SP.getFunction() == f);
        #endif
    }

    if (ctx.debug_enabled) {
        // Go over all arguments and local variables and initialize their debug information
        for(i=0; i < nreq; i++) {
            jl_sym_t *argname = jl_decl_var(jl_cellref(largs,i));
            jl_varinfo_t &varinfo = ctx.vars[argname];
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,    // Tag
                SP,         // Scope (current function will be fill in later)
                argname->name,    // Variable name
                fil,                    // File
                ctx.lineno == -1 ? 0 : ctx.lineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.declType,ctx.dbuilder,specsig), // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                i+1);                   // Argument number (1-based)

        }
        if (va) {
            ctx.vars[ctx.vaName].dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_arg_variable,    // Tag
                SP,         // Scope (current function will be fill in later)
                ctx.vaName->name, // Variable name
                fil,                    // File
                ctx.lineno == -1 ? 0 : ctx.lineno,             // Line (for now, use lineno of the function)
                julia_type_to_di(ctx.vars[ctx.vaName].declType,ctx.dbuilder,false),      // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                nreq + 1);              // Argument number (1-based)
        }
        for(i=0; i < lvarslen; i++) {
            jl_sym_t *s = (jl_sym_t*)jl_cellref(lvars,i);
            jl_varinfo_t &varinfo = ctx.vars[s];
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
                SP,                     // Scope (current function will be fill in later)
                s->name,                // Variable name
                fil,                    // File
                ctx.lineno == -1 ? 0 : ctx.lineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.declType,ctx.dbuilder,specsig), // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                0);                   // Argument number (1-based)
        }
        vinfos = jl_lam_capt(ast);
        vinfoslen = jl_array_dim0(vinfos);
        for(i=0; i < vinfoslen; i++) {
            jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
            assert(jl_is_array(vi));
            jl_sym_t *vname = ((jl_sym_t*)jl_cellref(vi,0));
            assert(jl_is_symbol(vname));
            jl_varinfo_t &varinfo = ctx.vars[vname];
            varinfo.dinfo = ctx.dbuilder->createLocalVariable(
                llvm::dwarf::DW_TAG_auto_variable,    // Tag
                SP,                     // Scope (current function will be filled in later)
                vname->name,            // Variable name
                fil,                    // File
                ctx.lineno == -1 ? 0 : ctx.lineno, // Line (for now, use lineno of the function)
                julia_type_to_di(varinfo.declType,ctx.dbuilder,specsig), // Variable type
                false,                  // May be optimized out
                0,                      // Flags (TODO: Do we need any)
                0);                   // Argument number (1-based)
        }
    }

#ifdef LLVM37
    std::map<jl_sym_t *, DIFile *> filescopes;
#else
    std::map<jl_sym_t *, MDNode *> filescopes;
#endif

    Value *fArg=NULL, *argArray=NULL, *argCount=NULL;
    unsigned argIdx = 0;
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
                addr.push_back(argIdx * sizeof(void*));
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
            argIdx++;
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
    // must be first for the mem2reg pass to work
    int n_roots = 0;
    for(i=0; i < largslen; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        if (store_unboxed_p(s, &ctx)) {
            alloc_local(s, &ctx);
        }
        else if (ctx.vars[s].isAssigned || (va && i==largslen-1)) {
            n_roots++;
        }
        maybe_alloc_arrayvar(s, &ctx);
    }
    for(i=0; i < lvarslen; i++) {
        jl_sym_t *s = (jl_sym_t*)jl_cellref(lvars,i);
        assert(jl_is_symbol(s));
        if (store_unboxed_p(s, &ctx)) {
            alloc_local(s, &ctx);
        }
        else {
            jl_varinfo_t &vi = ctx.vars[s];
            if (!vi.used) {
                vi.hasGCRoot = false;
                continue;
            }
            vi.hasGCRoot = true;
            if (vi.isSA && !vi.isVolatile && !vi.isCaptured && !vi.usedUndef &&
                vi.initExpr && is_stable_expr(vi.initExpr, &ctx)) {
                vi.hasGCRoot = false;
            }
            if (vi.hasGCRoot)
                n_roots++;
        }
        maybe_alloc_arrayvar(s, &ctx);
    }

    // create SAvalue locations for GenSym objects
    ctx.gensym_assigned.assign(n_gensyms, false);
    ctx.gensym_SAvalues.assign(n_gensyms, (Value*)NULL);
    for(int i=0; i < n_gensyms; i++) {
        jl_value_t *jt = (jl_is_array(gensym_types) ? jl_cellref(gensym_types, i) : (jl_value_t*)jl_any_type);
        if (jt == (jl_value_t*)jl_bottom_type || gensym_initExpr.at(i) == NULL) {
            // nothing
        }
        else if (store_unboxed_p(jt)) {
            Type *vtype = julia_struct_to_llvm(jt);
            assert(vtype != jl_pvalue_llvmt);
            if (vtype != T_void && !vtype->isEmptyTy()) {
                Value *lv = mark_julia_type(builder.CreateAlloca(vtype, 0), jt);
                ctx.gensym_SAvalues.at(i) = lv;
            }
        }
        else if (is_stable_expr(gensym_initExpr.at(i), &ctx)) {
            gensym_initExpr.at(i) = NULL;
        }
        else {
            n_roots++;
        }
    }

    // fetch env out of function object if we need it
    if (hasCapt) {
        ctx.envArg = emit_nthptr(fArg, offsetof(jl_function_t,env)/sizeof(jl_value_t*), tbaa_func);
    }

    // step 8. set up GC frame
    allocate_gc_frame(n_roots, b0, &ctx);

    // get pointers for locals stored in the gc frame array (argTemp)
    int varnum = 0;
    for(i=0; i < largslen; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        if (store_unboxed_p(s, &ctx)) {
            // nothing
        }
        else if (ctx.vars[s].isAssigned || (va && i==largslen-1)) {
            Value *av = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            ctx.vars[s].memvalue = av;
        }
    }
    for(i=0; i < lvarslen; i++) {
        jl_sym_t *s = ((jl_sym_t*)jl_cellref(lvars,i));
        if (store_unboxed_p(s, &ctx)) {
            // nothing
        }
        else if (ctx.vars[s].hasGCRoot) {
            Value *lv = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            ctx.vars[s].memvalue = lv;
        }
    }
    for(int i=0; i < n_gensyms; i++) {
        jl_value_t *jt = (jl_is_array(gensym_types) ? jl_cellref(gensym_types, i) : (jl_value_t*)jl_any_type);
        Value *lv = ctx.gensym_SAvalues.at(i);
        if (jt == (jl_value_t*)jl_bottom_type || gensym_initExpr.at(i) == NULL) {
            // nothing
        }
        else if (store_unboxed_p(jt)) {
            // nothing
        }
        else {
            lv = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            ctx.gensym_SAvalues.at(i) = lv;
        }
    }
    assert(varnum == ctx.argSpaceOffs);

    // step 9. create boxes for boxed locals
    // now handled by explicit :newvar nodes
    /*
    for(i=0; i < lvarslen; i++) {
        jl_sym_t *s = ((jl_sym_t*)jl_cellref(lvars,i));
        if (isBoxed(s, &ctx)) {
            Value *lv = ctx.vars[s].memvalue;
            builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), V_null), lv);
        }
    }
    */

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
    if (ctx.linfo->specTypes == NULL) {
        if (va) {
            Value *enough =
                builder.CreateICmpUGE(argCount,
                                      ConstantInt::get(T_int32, nreq));
            BasicBlock *elseBB =
                BasicBlock::Create(getGlobalContext(), "else", f);
            BasicBlock *mergeBB =
                BasicBlock::Create(getGlobalContext(), "ifcont");
            builder.CreateCondBr(enough, mergeBB, elseBB);
            builder.SetInsertPoint(elseBB);
            just_emit_error("too few arguments", &ctx);
            builder.CreateUnreachable();
            f->getBasicBlockList().push_back(mergeBB);
            builder.SetInsertPoint(mergeBB);
        }
        else {
            Value *enough =
                builder.CreateICmpEQ(argCount,
                                     ConstantInt::get(T_int32, nreq));
            BasicBlock *elseBB =
                BasicBlock::Create(getGlobalContext(), "else", f);
            BasicBlock *mergeBB =
                BasicBlock::Create(getGlobalContext(), "ifcont");
            builder.CreateCondBr(enough, mergeBB, elseBB);
            builder.SetInsertPoint(elseBB);
            just_emit_error("wrong number of arguments", &ctx);
            builder.CreateUnreachable();
            f->getBasicBlockList().push_back(mergeBB);
            builder.SetInsertPoint(mergeBB);
        }
    }

    // step 12. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    argIdx = 0;
    for(i=0; i < nreq; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        jl_varinfo_t &vi = ctx.vars[s];
        Value *argPtr = NULL;
        jl_value_t *argType = NULL;
        if (specsig) {
            argType = jl_nth_slot_type(lam->specTypes,i);
            if (!vi.isGhost) {
                argPtr = AI++;
                argPtr = mark_julia_type(argPtr, argType);
            }
        }
        else {
            argPtr = builder.CreateGEP(argArray, ConstantInt::get(T_size, argIdx));
            argIdx++;
        }

        Value *theArg = NULL;
        if (specsig) {
            theArg = argPtr;
        }
        else {
            assert(argPtr != NULL);
            theArg = builder.CreateLoad(argPtr, false);
        }

        Value *lv = vi.memvalue;
        if (lv == NULL) {
            // if this argument hasn't been given space yet, we've decided
            // to leave it in the input argument array.
            vi.passedAs = theArg;
        }
        else {
            // keep track of original (boxed) value to avoid re-boxing
            vi.passedAs = theArg;
            if (isBoxed(s, &ctx)) {
                if (specsig) {
                    theArg = boxed(theArg,&ctx,argType);
                    builder.CreateStore(theArg, lv); // temporarily root
                }
                builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), theArg), lv);
            }
            else if (dyn_cast<GetElementPtrInst>(lv) != NULL) {
                builder.CreateStore(boxed(theArg,&ctx,argType), lv);
            }
            else if (dyn_cast<AllocaInst>(lv)->getAllocatedType() == jl_pvalue_llvmt) {
                builder.CreateStore(theArg,lv);
            }
            else {
                builder.CreateStore(emit_unbox(dyn_cast<AllocaInst>(lv)->getAllocatedType(),
                                               theArg,
                                               lam->specTypes == NULL ? NULL :
                                               jl_nth_slot_type(lam->specTypes,i)),
                                    lv);
            }
        }
        // get arrayvar data if applicable
        if (arrayvars.find(s) != arrayvars.end()) {
            jl_arrayvar_t av = arrayvars[s];
            assign_arrayvar(av, theArg);
        }
    }

    // step 13. allocate rest argument if necessary
    if (va) {
        jl_sym_t *argname = ctx.vaName;
        jl_varinfo_t &vi = ctx.vars[argname];
        if (!vi.escapes && !vi.isAssigned) {
            ctx.vaStack = true;
        }
        else if (!vi.isGhost) {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            Value *lv = vi.memvalue;
            if (dyn_cast<GetElementPtrInst>(lv) != NULL || dyn_cast<AllocaInst>(lv)->getAllocatedType() == jl_pvalue_llvmt) {
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
                if (isBoxed(argname, &ctx))
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
        if (jl_is_linenode(stmt)) {
            lno = jl_linenode_line(stmt);
            if (ctx.debug_enabled)
                builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP, NULL));
            if (do_coverage)
                coverageVisitLine(filename, lno);
            ctx.lineno = lno;
        }
        else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
            lno = jl_unbox_long(jl_exprarg(stmt, 0));
            #ifdef LLVM37
            DIFile *dfil = NULL;
            #else
            MDNode *dfil = NULL;
            #endif
            if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
                jl_value_t *a1 = jl_exprarg(stmt,1);
                if (jl_is_symbol(a1)) {
                    jl_sym_t *file = (jl_sym_t*)a1;
                    // If the string is not empty
                    if (*file->name != '\0') {
                        #ifdef LLVM37
                        std::map<jl_sym_t *, DIFile *>::iterator it = filescopes.find(file);
                        #else
                        std::map<jl_sym_t *, MDNode *>::iterator it = filescopes.find(file);
                        #endif
                        if (it != filescopes.end()) {
                            dfil = it->second;
                        }
                        else {
                            #ifdef LLVM37
                            dfil = filescopes[file] = (DIFile*)dbuilder.createFile(file->name, ".");
                            #else
                            dfil = filescopes[file] = (MDNode*)dbuilder.createFile(file->name, ".");
                            #endif
                        }
                    }
                }
            }
            if (ctx.debug_enabled) {
                MDNode *scope;
                if (dfil == NULL)
                    scope = SP;
                else {
#ifdef LLVM37
                    scope = (MDNode*)dbuilder.createLexicalBlockFile(SP,dfil);
#else
                    scope = (MDNode*)dbuilder.createLexicalBlockFile(SP,DIFile(dfil));
#endif
                }
                builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, scope, NULL));
            }
            if (do_coverage)
                coverageVisitLine(filename, lno);
            ctx.lineno = lno;
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
            Type *retty = f->getReturnType();
            if (retty == jl_pvalue_llvmt) {
                retval = boxed(emit_expr(jl_exprarg(ex,0), &ctx, true),&ctx,expr_type(stmt,&ctx));
            }
            else if (retty != T_void) {
                retval = emit_unbox(retty,
                                    emit_unboxed(jl_exprarg(ex,0), &ctx), jlrettype);
            }
            else {
                retval = emit_expr(jl_exprarg(ex,0), &ctx, false);
            }
#ifdef JL_GC_MARKSWEEP
            Instruction *gcpop = (Instruction*)builder.CreateConstGEP1_32(ctx.gcframe, 1);
            ctx.gc_frame_pops.push_back(gcpop);
            builder.CreateStore(builder.CreateBitCast(builder.CreateLoad(gcpop, false), jl_ppvalue_llvmt),
                                prepare_global(jlpgcstack_var));
#endif
            if (do_malloc_log && lno != -1)
                mallocVisitLine(filename, lno);
            if (builder.GetInsertBlock()->getTerminator() == NULL) {
                if (retty == T_void)
                    builder.CreateRetVoid();
                else
                    builder.CreateRet(retval);
            }
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
    // sometimes we have dangling labels after the end
    if (builder.GetInsertBlock()->getTerminator() == NULL) {
        builder.CreateUnreachable();
    }

    // step 16. fix up size of stack root list
    //total_roots += (ctx.argSpaceOffs + ctx.maxDepth);
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
        new GlobalVariable(*m, jl_pvalue_llvmt, true,
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
            jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)lam->ast);
            std::vector<Type*> fsig(0);
            for(size_t i=0; i < jl_nparams(lam->specTypes); i++) {
                Type *ty = julia_type_to_llvm(jl_tparam(lam->specTypes,i));
                if (!type_is_ghost(ty))
                    fsig.push_back(ty);
            }
            Type *rt = (jlrettype == (jl_value_t*)jl_void_type ? T_void : julia_type_to_llvm(jlrettype));
            Function *f = Function::Create(FunctionType::get(rt, fsig, false), Function::ExternalLinkage, funcName,
#ifdef USE_MCJIT
                                           shadow_module);
#else
                                           jl_Module);
#endif

        if (lam->specFunctionObject == NULL) {
            lam->specFunctionObject = (void*)f;
            lam->specFunctionID = jl_assign_functionID(f);
            }
            add_named_global(f, (void*)fptr);
        }
        else {
#ifdef USE_MCJIT
            Function *f = jlcall_func_to_llvm(funcName, fptr, shadow_module);
#else
            Function *f = jlcall_func_to_llvm(funcName, fptr, jl_Module);
#endif
            if (lam->functionObject == NULL) {
                lam->functionObject = (void*)f;
                lam->functionID = jl_assign_functionID(f);
                assert(lam->fptr == &jl_trampoline);
                lam->fptr = (jl_fptr_t)fptr;
            }
        }
    }
}

extern "C" DLLEXPORT jl_value_t *jl_new_box(jl_value_t *v)
{
    jl_value_t *box = (jl_value_t*)alloc_1w();
    jl_set_typeof(box, jl_box_any_type);
    // if (v) gc_wb(box, v); // write block not needed: box was just allocated
    box->fieldptr[0] = v;
    return box;
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

    // This type is used to create undef Values which carry
    // metadata.
    NoopType = ArrayType::get(T_int1,0);

    // add needed base definitions to our LLVM environment
    StructType *valueSt = StructType::create(getGlobalContext(), "jl_value_t");
    Type *valueStructElts[1] = { PointerType::getUnqual(valueSt) };
    ArrayRef<Type*> vselts(valueStructElts);
    valueSt->setBody(vselts);
    jl_value_llvmt = valueSt;

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

    jl_pvalue_llvmt = PointerType::get(jl_value_llvmt, 0);
    jl_ppvalue_llvmt = PointerType::get(jl_pvalue_llvmt, 0);
    two_pvalue_llvmt.push_back(jl_pvalue_llvmt);
    two_pvalue_llvmt.push_back(jl_pvalue_llvmt);
    three_pvalue_llvmt.push_back(jl_pvalue_llvmt);
    three_pvalue_llvmt.push_back(jl_pvalue_llvmt);
    three_pvalue_llvmt.push_back(jl_pvalue_llvmt);
    V_null = Constant::getNullValue(jl_pvalue_llvmt);
    std::vector<Type*> ftargs(0);
    ftargs.push_back(jl_pvalue_llvmt);
    ftargs.push_back(jl_ppvalue_llvmt);
    ftargs.push_back(T_int32);
    jl_func_sig = FunctionType::get(jl_pvalue_llvmt, ftargs, false);
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

#ifdef JL_GC_MARKSWEEP
    jlpgcstack_var =
        new GlobalVariable(*m, jl_ppvalue_llvmt,
                           false, GlobalVariable::ExternalLinkage,
                           NULL, "jl_pgcstack", NULL);
#endif

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
    jlexc_var = global_to_llvm("jl_exception_in_transit",
                               (void*)&jl_exception_in_transit, m);
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
    args1_.push_back(jl_pvalue_llvmt);
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
    args2_boundserrorv.push_back(jl_pvalue_llvmt);
    args2_boundserrorv.push_back(T_psize);
    args2_boundserrorv.push_back(T_size);
    jlboundserrorv_func =
        Function::Create(FunctionType::get(T_void, args2_boundserrorv, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_ints", m);
    jlboundserrorv_func->setDoesNotReturn();
    add_named_global(jlboundserrorv_func, (void*)&jl_bounds_error_ints);

    std::vector<Type*> args2_boundserror(0);
    args2_boundserror.push_back(jl_pvalue_llvmt);
    args2_boundserror.push_back(T_size);
    jlboundserror_func =
        Function::Create(FunctionType::get(T_void, args2_boundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_int", m);
    jlboundserror_func->setDoesNotReturn();
    add_named_global(jlboundserror_func, (void*)&jl_bounds_error_int);

    std::vector<Type*> args3_vboundserror(0);
    args3_vboundserror.push_back(jl_ppvalue_llvmt);
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
    args3_uboundserror.push_back(jl_pvalue_llvmt);
    args3_uboundserror.push_back(T_size);
    jluboundserror_func =
        Function::Create(FunctionType::get(T_void, args3_uboundserror, false),
                         Function::ExternalLinkage,
                         "jl_bounds_error_unboxed_int", m);
    jluboundserror_func->setDoesNotReturn();
    add_named_global(jluboundserror_func, (void*)&jl_bounds_error_unboxed_int);

    std::vector<Type*> args2_throw(0);
    args2_throw.push_back(jl_pvalue_llvmt);
    args2_throw.push_back(T_int32);
    jlthrow_line_func =
        (Function*)m->getOrInsertFunction("jl_throw_with_superfluous_argument",
                                          FunctionType::get(T_void, args2_throw, false));
    jlthrow_line_func->setDoesNotReturn();
    add_named_global(jlthrow_line_func, (void*)&jl_throw_with_superfluous_argument);

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

    std::vector<Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pint8);
    te_args.push_back(jl_pvalue_llvmt);
    te_args.push_back(jl_pvalue_llvmt);
    te_args.push_back(T_int32);
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error_rt_line", m);
    jltypeerror_func->setDoesNotReturn();
    add_named_global(jltypeerror_func, (void*)&jl_type_error_rt_line);

    std::vector<Type *> args_2ptrs(0);
    args_2ptrs.push_back(jl_pvalue_llvmt);
    args_2ptrs.push_back(jl_pvalue_llvmt);
    jlcheckassign_func =
        Function::Create(FunctionType::get(T_void, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_checked_assignment", m);
    add_named_global(jlcheckassign_func, (void*)&jl_checked_assignment);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(jl_pvalue_llvmt);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1ptr, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", m);
    add_named_global(jldeclareconst_func, (void*)&jl_declare_constant);

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
    builtin_func_map[jl_f_union] = jlcall_func_to_llvm("jl_f_union", (void*)&jl_f_union, m);
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

#ifdef JL_GC_MARKSWEEP
    queuerootfun = Function::Create(FunctionType::get(T_void, args_1ptr, false),
                                    Function::ExternalLinkage,
                                    "gc_queue_root", m);
    add_named_global(queuerootfun, (void*)&gc_queue_root);

    std::vector<Type *> wbargs(0);
    wbargs.push_back(jl_pvalue_llvmt);
    wbargs.push_back(jl_pvalue_llvmt);
    wbfunc = Function::Create(FunctionType::get(T_void, wbargs, false),
                              Function::ExternalLinkage,
                              "gc_wb_slow", m);
    add_named_global(wbfunc, (void*)&gc_wb_slow);
#endif

    std::vector<Type *> exp_args(0);
    exp_args.push_back(T_int1);
    expect_func = Intrinsic::getDeclaration(m, Intrinsic::expect, exp_args);

    std::vector<Type*> args3(0);
    args3.push_back(jl_pvalue_llvmt);
    jlbox_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args3, false),
                         Function::ExternalLinkage,
                         "jl_new_box", m);
    add_named_global(jlbox_func, (void*)&jl_new_box);

    jltopeval_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args3, false),
                         Function::ExternalLinkage,
                         "jl_toplevel_eval", m);
    add_named_global(jltopeval_func, (void*)&jl_toplevel_eval);

    jlcopyast_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args3, false),
                         Function::ExternalLinkage,
                         "jl_copy_ast", m);
    add_named_global(jlcopyast_func, (void*)&jl_copy_ast);

    std::vector<Type*> args4(0);
    args4.push_back(T_pint8);
    args4.push_back(jl_pvalue_llvmt);
    args4.push_back(jl_pvalue_llvmt);
    jlclosure_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args4, false),
                         Function::ExternalLinkage,
                         "jl_new_closure", m);
    add_named_global(jlclosure_func, (void*)&jl_new_closure);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlnsvec_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args5, true),
                         Function::ExternalLinkage,
                         "jl_svec", m);
    add_named_global(jlnsvec_func, (void*)&jl_svec);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_ppvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(T_int32);
    jlmethod_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", m);
    add_named_global(jlmethod_func, (void*)&jl_method_def);

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
    args_2vals.push_back(jl_pvalue_llvmt);
    args_2vals.push_back(jl_pvalue_llvmt);
    jlegal_func =
        Function::Create(FunctionType::get(T_int32, args_2vals, false),
                         Function::ExternalLinkage,
                         "jl_egal", m);
    add_named_global(jlegal_func, (void*)&jl_egal);

    std::vector<Type *> subt_args(0);
    subt_args.push_back(jl_pvalue_llvmt);
    subt_args.push_back(jl_pvalue_llvmt);
    subt_args.push_back(T_int32);
    jlsubtype_func =
        Function::Create(FunctionType::get(T_int32, subt_args, false),
                         Function::ExternalLinkage,
                         "jl_subtype", m);
    add_named_global(jlsubtype_func, (void*)&jl_subtype);

    std::vector<Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, aoargs, false),
                         Function::ExternalLinkage,
                         "allocobj", m);
    add_named_global(jlallocobj_func, (void*)&allocobj);

    std::vector<Type*> empty_args(0);
    jlalloc1w_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, empty_args, false),
                         Function::ExternalLinkage,
                         "alloc_1w", m);
    add_named_global(jlalloc1w_func, (void*)&alloc_1w);

    jlalloc2w_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, empty_args, false),
                         Function::ExternalLinkage,
                         "alloc_2w", m);
    add_named_global(jlalloc2w_func, (void*)&alloc_2w);

    jlalloc3w_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, empty_args, false),
                         Function::ExternalLinkage,
                         "alloc_3w", m);
    add_named_global(jlalloc3w_func, (void*)&alloc_3w);

    std::vector<Type*> atargs(0);
    atargs.push_back(T_size);
    jl_alloc_svec_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, atargs, false),
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
    newbits_args.push_back(jl_pvalue_llvmt);
    newbits_args.push_back(T_pint8);
    jlnewbits_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, newbits_args, false),
                         Function::ExternalLinkage,
                         "jl_new_bits", m);
    add_named_global(jlnewbits_func, (void*)&jl_new_bits);

    std::vector<Type *> getnthfld_args(0);
    getnthfld_args.push_back(jl_pvalue_llvmt);
    getnthfld_args.push_back(T_size);
    jlgetnthfieldchecked_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, getnthfld_args, false),
                         Function::ExternalLinkage,
                         "jl_get_nth_field_checked", m);
    add_named_global(jlgetnthfieldchecked_func, (void*)*jl_get_nth_field_checked);

    diff_gc_total_bytes_func =
        Function::Create(FunctionType::get(T_int64, false),
                         Function::ExternalLinkage,
                         "diff_gc_total_bytes", m);
    add_named_global(diff_gc_total_bytes_func, (void*)*diff_gc_total_bytes);

    std::vector<Type *> execpoint_args(0);
    execpoint_args.push_back(T_pint8);
    execpoint_args.push_back(T_int32);
    show_execution_point_func =
        Function::Create(FunctionType::get(T_void, execpoint_args, false),
                         Function::ExternalLinkage,
                         "show_execution_point", m);
    add_named_global(show_execution_point_func, (void*)*show_execution_point);

    // set up optimization passes
    FPM = new FunctionPassManager(m);

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
#ifndef LLVM37
    jl_TargetMachine->addAnalysisPasses(*FPM);
#endif
    FPM->add(createTypeBasedAliasAnalysisPass());
    if (jl_options.opt_level>=1)
        FPM->add(createBasicAliasAnalysisPass());
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
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_)
    const char *const argv[] = {"", "-disable-copyprop"}; // llvm bug 21743
    cl::ParseCommandLineOptions(sizeof(argv)/sizeof(argv[0]), argv, "disable-copyprop\n");
#endif
#ifdef JL_DEBUG_BUILD
    cl::ParseEnvironmentOptions("Julia", "JULIA_LLVM_ARGS");
#endif
#if defined(_CPU_PPC_) || defined(_CPU_PPC64_)
    imaging_mode = true; // LLVM seems to JIT bad TOC tables for the optimizations we attempt in non-imaging_mode
#else
    imaging_mode = jl_options.build_path != NULL;
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
    options.NoFramePointerElim = true;
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
#ifndef USE_MCJIT
        // Temporarily disable Haswell BMI2 features due to LLVM bug.
        "-bmi2", "-avx2",
#endif
#ifdef V128_BUG
        "-avx",
#endif
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
#endif
        .setTargetOptions(options)
        .setRelocationModel(Reloc::PIC_)
        .setCodeModel(CodeModel::Small)
#if defined(USE_MCJIT) && !defined(LLVM36)
        .setUseMCJIT(true)
#endif
    ;
    Triple TheTriple(sys::getProcessTriple());
#if (defined(_OS_WINDOWS_) || defined(FORCE_ELF)) && defined(USE_MCJIT)
    TheTriple.setObjectFormat(Triple::ELF);
#endif
    TargetMachine *targetMachine = eb.selectTarget(
            TheTriple,
            "",
#if LLVM35
            strcmp(jl_options.cpu_target,"native") ? StringRef(jl_options.cpu_target) : sys::getHostCPUName(),
#else
            strcmp(jl_options.cpu_target,"native") ? jl_options.cpu_target : "",
#endif
            MAttrs);
    jl_TargetMachine = targetMachine->getTarget().createTargetMachine(
            TheTriple.getTriple(),
            targetMachine->getTargetCPU(),
            targetMachine->getTargetFeatureString(),
            targetMachine->Options,
#ifdef CODEGEN_TLS
            Reloc::PIC_,
            CodeModel::Small,
#else
            Reloc::Default,
            CodeModel::JITDefault,
#endif
#ifdef DISABLE_OPT
            CodeGenOpt::None
#else
            CodeGenOpt::Aggressive // -O3
#endif
            );
    delete targetMachine;
    assert(jl_TargetMachine);
#if defined(LLVM36) && !defined(LLVM37)
    engine_module->setDataLayout(jl_TargetMachine->getSubtargetImpl()->getDataLayout());
#elif defined(LLVM35) && !defined(LLVM37)
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout());
#else
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout()->getStringRepresentation());
#endif
    jl_ExecutionEngine = eb.create(jl_TargetMachine);
    //jl_printf(JL_STDERR,"%s\n",jl_ExecutionEngine->getDataLayout()->getStringRepresentation().c_str());
    if (!jl_ExecutionEngine) {
        jl_printf(JL_STDERR, "Critical error initializing llvm: ", ErrorStr.c_str());
        exit(1);
    }
#ifdef LLVM35
    jl_ExecutionEngine->setProcessAllSections(true);
#endif
    jl_ExecutionEngine->DisableLazyCompilation();
    mbuilder = new MDBuilder(getGlobalContext());

#ifdef LLVM37
    m->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
    engine_module->setDataLayout(jl_ExecutionEngine->getDataLayout()->getStringRepresentation());
    m->setTargetTriple(jl_TargetMachine->getTargetTriple());
    engine_module->setTargetTriple(jl_TargetMachine->getTargetTriple());
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

    BOX_F(int8,int8);  UBOX_F(uint8,uint8);
    BOX_F(int16,int16); UBOX_F(uint16,uint16);
    BOX_F(int32,int32); UBOX_F(uint32,uint32);
    BOX_F(int64,int64); UBOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);
    UBOX_F(gensym,size);

    box8_func  = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int8),
                              "jl_box8", (void*)&jl_box8, m);
    box16_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int16),
                              "jl_box16", (void*)&jl_box16, m);
    box32_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int32),
                              "jl_box32", (void*)&jl_box32, m);
    box64_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int64),
                              "jl_box64", (void*)&jl_box64, m);

    typeToTypeId = jl_alloc_cell_1d(16);
}

// for debugging from gdb
extern "C" void jl_dump_llvm_value(void *v)
{
    ((Value*)v)->dump();
}
