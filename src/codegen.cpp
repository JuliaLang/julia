#include "platform.h"

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif

#include "llvm-version.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JITEventListener.h>
#include <llvm/PassManager.h>
#include <llvm/Target/TargetLibraryInfo.h>
#include <llvm/Target/TargetSubtargetInfo.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Bitcode/ReaderWriter.h>
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
#ifdef LLVM33
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Value.h>
#else
#include <llvm/DerivedTypes.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Intrinsics.h>
#include <llvm/Attributes.h>
#endif
#ifdef LLVM32
#ifndef LLVM35
#include <llvm/DebugInfo.h>
#include <llvm/DIBuilder.h>
#endif
#ifndef LLVM33
#include <llvm/IRBuilder.h>
#endif
#else // LLVM31 and before
#include <llvm/Analysis/DebugInfo.h>
#include <llvm/Analysis/DIBuilder.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Support/IRBuilder.h>
#endif
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Instrumentation.h>
#ifdef LLVM31
#include <llvm/Transforms/Vectorize.h>
#endif
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FormattedStream.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/PrettyStackTrace.h>
#include <llvm/Support/SourceMgr.h>
#ifdef JL_DEBUG_BUILD
#include <llvm/Support/CommandLine.h>
#endif
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

extern "C" {

#include "builtin_proto.h"

#ifdef HAVE_SSP
extern uintptr_t __stack_chk_guard;
extern void __stack_chk_fail();
#else
uintptr_t __stack_chk_guard = (uintptr_t)0xBAD57ACCBAD67ACC; // 0xBADSTACKBADSTACK
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
void __stack_chk_fail()
#else
void __attribute__(()) __stack_chk_fail()
#endif
{
    /* put your panic function or similar in here */
    fprintf(stderr, "fatal error: stack corruption detected\n");
    abort(); // end with abort, since the compiler destroyed the stack upon entry to this function
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

#ifdef LLVM35
static DataLayoutPass *jl_data_layout;
#elif defined(LLVM32)
static DataLayout *jl_data_layout;
#else
static TargetData *jl_data_layout;
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
static Type *T_int1;
static Type *T_int8;
static Type *T_pint8;
static Type *T_uint8;
static Type *T_int16;
static Type *T_pint16;
static Type *T_uint16;
static Type *T_int32;
static Type *T_pint32;
static Type *T_uint32;
static Type *T_int64;
static Type *T_pint64;
static Type *T_uint64;
static Type *T_char;
static Type *T_size;
static Type *T_psize;
static Type *T_float32;
static Type *T_pfloat32;
static Type *T_float64;
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
static MDNode* tbaa_tuplelen;           // The len in a jl_tuple_t
static MDNode* tbaa_func;           // A jl_function_t
static MDNode* tbaa_datatype;       // A jl_datatype_t
static MDNode* tbaa_const;          // Memory that is immutable by the time LLVM can see it

namespace llvm {
    extern Pass *createLowerSimdLoopPass();
    extern bool annotateSimdLoop( BasicBlock* latch );
}

// constants
static Value *V_null;

// global vars
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlnull_var;
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
static GlobalVariable *jlboundserr_var;
static GlobalVariable *jlstderr_var;
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
static Function *jlcheckassign_func;
static Function *jldeclareconst_func;
static Function *jltopeval_func;
static Function *jlcopyast_func;
static Function *jltuple_func;
static Function *jlntuple_func;
static Function *jlapplygeneric_func;
static Function *jlgetfield_func;
static Function *jlbox_func;
static Function *jlclosure_func;
static Function *jlmethod_func;
static Function *jlenter_func;
static Function *jlleave_func;
static Function *jlegal_func;
static Function *jlallocobj_func;
static Function *jlalloc2w_func;
static Function *jlalloc3w_func;
static Function *jl_alloc_tuple_func;
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
static Function *box8_func;
static Function *box16_func;
static Function *box32_func;
static Function *box64_func;
static Function *jlputs_func;
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

// --- code generation ---

// per-local-variable information
struct jl_varinfo_t {
    Value *memvalue;  // an address, if the var is alloca'd
    Value *SAvalue;   // register, if the var is SSA
    Value *passedAs;  // if an argument, the original passed value
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

    jl_varinfo_t() : memvalue(NULL), SAvalue(NULL), passedAs(NULL), closureidx(-1),
                     isAssigned(true), isCaptured(false), isSA(false), isVolatile(false),
                     isArgument(false), isGhost(false), hasGCRoot(false), escapes(true),
                     usedUndef(false), used(false),
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
    formatted_raw_ostream FOS(OS);

    // We don't want to use MCJIT's target machine because
    // it uses the large code model and we may potentially
    // want less optimizations there.
    Triple TheTriple = Triple(jl_TargetMachine->getTargetTriple());
#if defined(_OS_WINDOWS_) && defined(USE_MCJIT)
    TheTriple.setObjectFormat(Triple::COFF);
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
    PM.add(new TargetLibraryInfo(Triple(jl_TargetMachine->getTargetTriple())));
#ifdef LLVM36
    PM.add(new DataLayoutPass());
#elif LLVM35
    PM.add(new DataLayoutPass(*jl_ExecutionEngine->getDataLayout()));
#else
    PM.add(new DataLayout(*jl_ExecutionEngine->getDataLayout()));
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
    std::map<jl_sym_t*, jl_arrayvar_t> *arrayvars;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *handlers;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_tuple_t *sp;
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
    Instruction *gcframe ;
    Instruction *argSpaceInits;
    StoreInst *storeFrameSize;
#endif
    BasicBlock::iterator first_gcframe_inst;
    BasicBlock::iterator last_gcframe_inst;
    llvm::DIBuilder *dbuilder;
    std::vector<Instruction*> gc_frame_pops;
    std::vector<CallInst*> to_inline;
} jl_codectx_t;

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool boxed=true,
                        bool valuepos=true);
static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx);
static int is_global(jl_sym_t *s, jl_codectx_t *ctx);
static Value *make_gcroot(Value *v, jl_codectx_t *ctx);
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
    StringRef HostCPUName = llvm::sys::getHostCPUName();
    return jl_pchar_to_string(HostCPUName.data(), HostCPUName.size());
}

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
static Function *emit_function(jl_lambda_info_t *lam, bool cstyle);
//static int n_compile=0;
static Function *to_function(jl_lambda_info_t *li, bool cstyle)
{
    JL_SIGATOMIC_BEGIN();
    assert(!li->inInference);
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    Function *f = NULL;
    JL_TRY {
        f = emit_function(li, cstyle);
        //JL_PRINTF(JL_STDOUT, "emit %s\n", li->name->name);
        //n_emit++;
    }
    JL_CATCH {
        li->functionObject = NULL;
        li->cFunctionObject = NULL;
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
    //ios_printf(ios_stderr, "%s:%d\n",
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
    m->addModuleFlag(llvm::Module::Warning, "Dwarf Version",3);
#ifdef LLVM34
    m->addModuleFlag(llvm::Module::Error, "Debug Info Version",
        llvm::DEBUG_METADATA_VERSION);
#endif
#ifdef LLVM36
    if (jl_ExecutionEngine)
        m->setDataLayout(jl_ExecutionEngine->getDataLayout());
#endif
    if (add) {
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
            if (li->cFunctionObject != NULL)
                li->cFunctionObject = mover.CloneFunction((Function*)li->cFunctionObject);
        }
        #endif

        Function *llvmf = (Function*)li->functionObject;

#ifdef USE_MCJIT
        li->fptr = (jl_fptr_t)jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
#else
        li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
#endif
        assert(li->fptr != NULL);
        if (li->cFunctionObject != NULL) {
#ifdef USE_MCJIT
            (void)jl_ExecutionEngine->getFunctionAddress(((Function*)li->cFunctionObject)->getName());
#else
            (void)jl_ExecutionEngine->getPointerToFunction((Function*)li->cFunctionObject);
#endif
        }
        JL_SIGATOMIC_END();
        if (!imaging_mode) {
            llvmf->deleteBody();
            if (li->cFunctionObject != NULL)
                ((Function*)li->cFunctionObject)->deleteBody();
        }
    }
    f->fptr = li->fptr;
}

extern "C" void jl_compile(jl_function_t *f)
{
    jl_lambda_info_t *li = f->linfo;
    if (li->functionObject == NULL) {
        // objective: assign li->functionObject
        li->inCompile = 1;
        (void)to_function(li, false);
        li->inCompile = 0;
    }
}

void jl_cstyle_compile(jl_function_t *f)
{
    jl_lambda_info_t *li = f->linfo;
    if (li->cFunctionObject == NULL) {
        // objective: assign li->cFunctionObject
        li->inCompile = 1;
        (void)to_function(li, true);
        li->inCompile = 0;
    }
}

// Get the LLVM Function* for the C-callable entry point for a certain function
// and argument types. If rt is NULL then whatever return type is present is
// accepted.
static Function *jl_cfunction_object(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    if (rt) {
        JL_TYPECHK(jl_function_ptr, type, rt);
    }
    JL_TYPECHK(jl_function_ptr, tuple, argt);
    JL_TYPECHK(jl_function_ptr, type, argt);
    if (jl_is_gf(f) && (rt == NULL || jl_is_leaf_type(rt)) && jl_is_leaf_type(argt)) {
        jl_function_t *ff = jl_get_specialization(f, (jl_tuple_t*)argt);
        if (ff != NULL && ff->env==(jl_value_t*)jl_null && ff->linfo != NULL) {
            if (ff->linfo->cFunctionObject == NULL) {
                jl_cstyle_compile(ff);
            }
            if (ff->linfo->cFunctionObject != NULL) {
                jl_lambda_info_t *li = ff->linfo;
                if (!jl_types_equal((jl_value_t*)li->specTypes, argt)) {
                    jl_errorf("cfunction: type signature of %s does not match",
                              li->name->name);
                }
                if (rt != NULL) {
                    jl_value_t *astrt = jl_ast_rettype(li, li->ast);
                    if (!jl_types_equal(astrt, rt)) {
                        if (astrt == (jl_value_t*)jl_bottom_type) {
                            if (rt != (jl_value_t*)jl_void_type) {
                                // a function that doesn't return can be passed to C as void
                                jl_errorf("cfunction: %s does not return", li->name->name);
                            }
                        }
                        else {
                            jl_errorf("cfunction: return type of %s does not match",
                                      li->name->name);
                        }
                    }
                }
                return (Function*)ff->linfo->cFunctionObject;
            }
        }
    }
    jl_error("function is not yet c-callable");
    return NULL;
}

// get the address of a C-callable entry point for a function
extern "C" DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    Function *llvmf = jl_cfunction_object(f, rt, argt);
    assert(llvmf);
#ifdef USE_MCJIT
    return (void*)jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
#else
    return jl_ExecutionEngine->getPointerToFunction(llvmf);
#endif
}

// export a C-callable entry point for a function, with a given name
extern "C" DLLEXPORT
void jl_extern_c(jl_function_t *f, jl_value_t *rt, jl_value_t *argt, char *name)
{
    Function *llvmf = jl_cfunction_object(f, rt, argt);
    if (llvmf) {
        #ifndef LLVM35
        new GlobalAlias(llvmf->getType(), GlobalValue::ExternalLinkage, name, llvmf, llvmf->getParent());
        #else
        GlobalAlias::create(llvmf->getType()->getElementType(), llvmf->getType()->getAddressSpace(),
                            GlobalValue::ExternalLinkage, name, llvmf, llvmf->getParent());
        #endif
    }
}

// --- native code info, and dump function to IR and ASM ---
extern void RegisterJuliaJITEventListener();

extern int jl_get_llvmf_info(size_t fptr, uint64_t *symsize,
#ifdef USE_MCJIT
    const object::ObjectFile **object);
#else
    std::vector<JITEvent_EmittedFunctionDetails::LineStart> *lines);
#endif

extern "C"
void jl_dump_function_asm(const char *Fptr, size_t Fsize,
#ifdef USE_MCJIT
                          const object::ObjectFile *objectfile,
#else
                          std::vector<JITEvent_EmittedFunctionDetails::LineStart> lineinfo,
#endif
                          formatted_raw_ostream &stream);

const jl_value_t *jl_dump_llvmf(void *f, bool dumpasm)
{
    std::string code;
    llvm::raw_string_ostream stream(code);
    llvm::formatted_raw_ostream fstream(stream);
    Function *llvmf = (Function*)f;
    if (dumpasm == false) {
        llvmf->print(stream);
    }
    else {
        uint64_t symsize;
#ifdef USE_MCJIT
        size_t fptr = (size_t)jl_ExecutionEngine->getFunctionAddress(llvmf->getName());
        const object::ObjectFile *object;
#else
        size_t fptr = (size_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
        std::vector<JITEvent_EmittedFunctionDetails::LineStart> object;
#endif
        assert(fptr != 0);
        if (jl_get_llvmf_info(fptr, &symsize, &object))
            jl_dump_function_asm((char *)fptr, symsize, object, fstream);
        else
            JL_PRINTF(JL_STDERR, "Warning: Unable to find function pointer\n");
        fstream.flush();
    }
    return jl_cstr_to_string(const_cast<char*>(stream.str().c_str()));
}

extern "C" DLLEXPORT
void *jl_get_llvmf(jl_function_t *f, jl_tuple_t *types, bool getwrapper)
{
    jl_function_t *sf = f;
    if (types != NULL) {
        if (!jl_is_function(f) || !jl_is_gf(f))
            return NULL;
        sf = jl_get_specialization(f, types);
    }
    if (sf == NULL || sf->linfo == NULL) {
        sf = jl_method_lookup_by_type(jl_gf_mtable(f), types, 0, 0);
        if (sf == jl_bottom_func)
            return NULL;
        JL_PRINTF(JL_STDERR,
                  "Warning: Returned code may not match what actually runs.\n");
    }
    Function *llvmf;
    if (getwrapper || sf->linfo->specTypes == NULL) {
        if (sf->linfo->functionObject == NULL) {
            jl_compile(sf);
        }
    }
    else {
        if (sf->linfo->cFunctionObject == NULL) {
            jl_cstyle_compile(sf);
        }
    }
    if (sf->fptr == &jl_trampoline) {
        if (!getwrapper && sf->linfo->cFunctionObject != NULL)
            llvmf = (Function*)sf->linfo->cFunctionObject;
        else
            llvmf = (Function*)sf->linfo->functionObject;
    }
    else {
        llvmf = to_function(sf->linfo, false);
    }
    return llvmf;
}

extern "C" DLLEXPORT
const jl_value_t *jl_dump_function(jl_function_t *f, jl_tuple_t *types, bool dumpasm, bool dumpwrapper)
{
    void *llvmf = jl_get_llvmf(f,types,dumpwrapper);
    if (llvmf == NULL)
        return jl_cstr_to_string(const_cast<char*>(""));
    return jl_dump_llvmf(llvmf,dumpasm);
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
    if (vec[line] == NULL)
        vec[line] = addComdat(new GlobalVariable(*jl_Module, T_int64, false,
                                                 GlobalVariable::InternalLinkage,
                                                 ConstantInt::get(T_int64,0), "lcnt"));
    GlobalVariable *v = vec[line];
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v),
                                          ConstantInt::get(T_int64,1)),
                        v);
}

void write_log_data(logdata_t logData, const char *extension)
{
    std::string base = std::string(jl_compileropts.julia_home);
    base = base + "/../share/julia/base/";
    logdata_t::iterator it = logData.begin();
    for (; it != logData.end(); it++) {
        std::string filename = (*it).first;
        std::vector<GlobalVariable*> &values = (*it).second;
        if (values.size() > 1) {
            if (filename[0] != '/')
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
                            int *p = (int*)jl_ExecutionEngine->getPointerToGlobal(gv);
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
    if (vec[line] == NULL)
        vec[line] = addComdat(new GlobalVariable(*jl_Module, T_int64, false,
                                                 GlobalVariable::InternalLinkage,
                                                 ConstantInt::get(T_int64,0), "bytecnt"));
    GlobalVariable *v = vec[line];
    builder.CreateStore(builder.CreateAdd(builder.CreateLoad(v, true),
                                          builder.CreateCall(prepare_call(diff_gc_total_bytes_func))),
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
                int64_t *p = (int64_t*) jl_ExecutionEngine->getPointerToGlobal(*itb);
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
                for(i=0; i < jl_tuple_len(sp); i+=2) {
                    if (sym == (jl_sym_t*)jl_tupleref(sp, i)) {
                        // static parameter
                        return jl_tupleref(sp, i+1);
                    }
                }
            }
            if (jl_is_const(mod, sym))
                return jl_get_global(mod, sym);
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
    if (jl_is_getfieldnode(ex)) {
        m = (jl_module_t*)jl_static_eval(jl_fieldref(ex,0),ctx,mod,sp,ast,sparams,allow_alloc);
        s = (jl_sym_t*)jl_fieldref(ex,1);
        if (m && jl_is_module(m) && s && jl_is_symbol(s)) {
            jl_binding_t *b = jl_get_binding(m, s);
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
                if (fptr == &jl_apply_generic) {
                    if (f == jl_get_global(jl_base_module, jl_symbol("dlsym")) ||
                        f == jl_get_global(jl_base_module, jl_symbol("dlopen"))) {
                        size_t i;
                        size_t n = jl_array_dim0(e->args);
                        jl_value_t **v;
                        JL_GC_PUSHARGS(v, n);
                        memset(v, 0, n*sizeof(jl_value_t*));
                        v[0] = f;
                        for (i = 1; i < n; i++) {
                            v[i] = jl_static_eval(jl_exprarg(e,i),ctx,mod,sp,ast,sparams,allow_alloc);
                            if (v[i] == NULL) {
                                JL_GC_POP();
                                return NULL;
                            }
                        }
                        jl_value_t *result = jl_apply_generic(f, v+1, (uint32_t)n-1);
                        JL_GC_POP();
                        return result;
                    }
                }
                else if (jl_array_dim0(e->args) == 3 && fptr == &jl_f_get_field) {
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
                    if (n==0 && fptr == &jl_f_tuple) return (jl_value_t*)jl_null;
                    if (!allow_alloc)
                        return NULL;
                    jl_value_t **v;
                    JL_GC_PUSHARGS(v, n);
                    memset(v, 0, n*sizeof(jl_value_t*));
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
    else if (jl_is_getfieldnode(e)) {
        if (local_var_occurs(jl_fieldref(e,0),s))
            return true;
    }
    return false;
}

static std::set<jl_sym_t*> assigned_in_try(jl_array_t *stmts, int s, long l, int *pend)
{
    std::set<jl_sym_t*> av;
    size_t slength = jl_array_dim0(stmts);
    for(int i=s; i < (int)slength; i++) {
        jl_value_t *st = jl_arrayref(stmts,i);
        if (jl_is_expr(st)) {
            if (((jl_expr_t*)st)->head == assign_sym) {
                jl_sym_t *sy;
                jl_value_t *ar = jl_exprarg(st, 0);
                if (jl_is_symbolnode(ar)) {
                    sy = jl_symbolnode_sym(ar);
                }
                else {
                    assert(jl_is_symbol(ar));
                    sy = (jl_sym_t*)ar;
                }
                av.insert(sy);
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
        jl_value_t *st = jl_arrayref(stmts,i);
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
                                local_var_occurs(jl_arrayref(stmts,j), *it)) {
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
                        if (ff->fptr == jl_f_tuplelen ||
                            ff->fptr == jl_f_tupleref ||
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
    jl_value_t *ty = expr_type(expr, ctx);
    if (jl_is_symbolnode(expr)) {
        expr = (jl_value_t*)jl_symbolnode_sym(expr);
    }
    if (jl_is_symbol(expr)) {
        jl_sym_t *vname = ((jl_sym_t*)expr);
        if (ctx->vars.find(vname) != ctx->vars.end()) {
            jl_varinfo_t &vi = ctx->vars[vname];
            vi.escapes |= esc;
            vi.usedUndef |= (jl_subtype((jl_value_t*)jl_undef_type,ty,0)!=0);
            if (!ctx->linfo->inferred)
                vi.usedUndef = true;
            vi.used = true;
        }
    }
}

// --- gc root utils ---

static Value *make_gcroot(Value *v, jl_codectx_t *ctx)
{
    Value *froot = builder.CreateGEP(ctx->argTemp,
                                     ConstantInt::get(T_size,
                                                      ctx->argSpaceOffs +
                                                      ctx->argDepth));
    builder.CreateStore(v, froot);
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
    for(size_t i=0; i < jl_tuple_len(ty->types); i++) {
        if (!(ty->fields[i].isptr ||
              (name && name != (jl_sym_t*)jl_tupleref(ty->names,i)))) {
            return false;
        }
    }
    return true;
}

static bool jltupleisbits(jl_value_t *jt, bool allow_unsized)
{
    if (!jl_is_tuple(jt))
        return jl_isbits(jt) && jl_is_leaf_type(jt) && (allow_unsized ||
            ((jl_is_bitstype(jt) && jl_datatype_size(jt) > 0) ||
             (jl_is_datatype(jt) && jl_tuple_len(((jl_datatype_t*)jt)->names)>0)));
    size_t ntypes = jl_tuple_len(jt);
    if (ntypes == 0)
        return allow_unsized;
    for (size_t i = 0; i < ntypes; ++i)
        if (!jltupleisbits(jl_tupleref(jt,i),allow_unsized))
            return false;
    return true;
}

static bool jl_tupleref_nonallocating(jl_value_t *ty, jl_value_t *idx)
{
    if (!jl_is_tuple(ty))
        return false;
    if (jltupleisbits(ty))
        return false;
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
                         is_getfield_nonallocating((jl_datatype_t*)ty, jl_exprarg(e,2))) ||
                        (fptr == &jl_f_tupleref && jl_tupleref_nonallocating(ty, jl_exprarg(e,2)))) {
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
    return (!jl_is_symbol(ex) && !jl_is_symbolnode(ex) &&
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
        jl_cellset(li->roots, 0, val);
    }
    else {
        size_t rlen = jl_array_dim0(li->roots);
        for(size_t i=0; i < rlen; i++) {
            if (jl_arrayref(li->roots,i) == val) {
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
            (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_null,
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
#ifdef OVERLAP_TUPLE_LEN
            val = emit_nthptr((Value*)ctx->envArg, idx+1, tbaa_tuplelen);
#else
            val = emit_nthptr((Value*)ctx->envArg, idx+2, tbaa_tuplelen);
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
    env_tuple = builder.CreateCall(prepare_call(jlntuple_func),
                                   ArrayRef<Value*>(&captured[0],
                                                    1+clen));
    ctx->argDepth = argStart;
    make_gcroot(env_tuple, ctx);
    Value *result = builder.CreateCall3(prepare_call(jlclosure_func),
                                        Constant::getNullValue(T_pint8),
                                        env_tuple, literal_pointer_val(expr));
    ctx->argDepth--;
    return result;
}

// --- generating function calls ---

static jl_tuple_t *call_arg_types(jl_value_t **args, size_t n, jl_codectx_t *ctx)
{
    jl_tuple_t *t = jl_alloc_tuple(n);
    JL_GC_PUSH1(&t);
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *ty = expr_type(args[i], ctx);
        if (!jl_is_leaf_type(ty)) {
            t = NULL;
            break;
        }
        jl_tupleset(t, i, ty);
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
    if (jl_is_type_type((jl_value_t*)sty) && jl_is_leaf_type(jl_tparam0(sty)))
        sty = (jl_datatype_t*)jl_typeof(jl_tparam0(sty));
    JL_GC_PUSH1(&sty);
    if (jl_is_structtype(sty) && sty != jl_module_type && sty->uid != 0) {
        unsigned idx = jl_field_index(sty, name, 0);
        if (idx != (unsigned)-1) {
            jl_value_t *jfty = jl_tupleref(sty->types, idx);
            Value *strct = emit_expr(expr, ctx, false);
            if (strct->getType() == jl_pvalue_llvmt) {
                Value *addr =
                    builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                                      ConstantInt::get(T_size,
                                                       sty->fields[idx].offset + sizeof(void*)));
                JL_GC_POP();
                MDNode *tbaa = sty->mutabl ? tbaa_user : tbaa_immut;
                if (sty->fields[idx].isptr) {
                    Value *fldv = tbaa_decorate(tbaa, builder.CreateLoad(builder.CreateBitCast(addr,jl_ppvalue_llvmt)));
                    if (idx >= (unsigned)sty->ninitialized) {
                        null_pointer_check(fldv, ctx);
                    }
                    return fldv;
                }
                else {
                    return typed_load(addr, ConstantInt::get(T_size, 0), jfty, ctx, tbaa);
                }
            }
            else {
                unsigned llvmidx = 0;
                // LLVM struct omits 0-size entries
                // TODO: try to avoid this loop
                for(unsigned i=0; i < idx; i++) {
                    if (julia_type_to_llvm(jl_tupleref(sty->types, i)) != T_void)
                        llvmidx++;
                }
                Value *fldv;
                if (julia_type_to_llvm(jfty) == T_void)
                    fldv = UndefValue::get(NoopType);
                else
                    fldv = builder.CreateExtractValue(strct, ArrayRef<unsigned>(&llvmidx,1));
                if (jfty == (jl_value_t*)jl_bool_type) {
                    fldv = builder.CreateTrunc(fldv, T_int1);
                }
                else if (sty->fields[idx].isptr && idx >= (unsigned)sty->ninitialized) {
                    null_pointer_check(fldv, ctx);
                }
                JL_GC_POP();
                return mark_julia_type(fldv, jfty);
            }
        }
    }
    // TODO: attempt better codegen for approximate types, if the types
    // and offsets of some fields are independent of parameters.
    JL_GC_POP();

    int argStart = ctx->argDepth;
    Value *arg1 = boxed(emit_expr(expr, ctx),ctx,expr_type(expr,ctx));
    // TODO: generic getfield func with more efficient calling convention
    make_gcroot(arg1, ctx);
    Value *arg2 = literal_pointer_val((jl_value_t*)name);
    make_gcroot(arg2, ctx);
    Value *myargs = builder.CreateGEP(ctx->argTemp,
                                      ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
    Value *result = builder.CreateCall3(prepare_call(jlgetfield_func), V_null, myargs,
                                        ConstantInt::get(T_int32,2));
    ctx->argDepth = argStart;
    return result;
}

static void emit_setfield(jl_datatype_t *sty, Value *strct, size_t idx,
                          Value *rhs, jl_codectx_t *ctx, bool checked=true)
{
    if (sty->mutabl || !checked) {
        Value *addr =
            builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                              ConstantInt::get(T_size, sty->fields[idx].offset + sizeof(void*)));
        jl_value_t *jfty = jl_tupleref(sty->types, idx);
        if (sty->fields[idx].isptr) {
            builder.CreateStore(boxed(rhs,ctx),
                                builder.CreateBitCast(addr, jl_ppvalue_llvmt));
        }
        else {
            typed_store(addr, ConstantInt::get(T_size, 0), rhs, jfty, ctx, sty->mutabl ? tbaa_user : tbaa_immut);
        }
    }
    else {
        // TODO: better error
        emit_error("type is immutable", ctx);
    }
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
        (!arg1 || jl_is_symbol(arg1) || jl_is_symbolnode(arg1) || is_constant(arg1, ctx)) &&
        (!arg2 || jl_is_symbol(arg2) || jl_is_symbolnode(arg2) || is_constant(arg2, ctx))) {
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
        bool isStruct = at1->isStructTy();
        if ((isStruct || at1->isVectorTy()) && !ptr_comparable) {
            jl_tuple_t *types;
            if (jl_is_datatype(rt1)) {
                types = ((jl_datatype_t*)rt1)->types;
            }
            else {
                assert(jl_is_tuple(rt1));
                types = (jl_tuple_t*)rt1;
            }
            answer = ConstantInt::get(T_int1, 1);
            size_t l = jl_tuple_len(types);
            unsigned j = 0;
            for(unsigned i=0; i < l; i++) {
                jl_value_t *fldty = jl_tupleref(types,i);
                Value *subAns;
                if (isStruct) {
                    if (julia_type_to_llvm(fldty) != T_void) {
                        subAns =
                            emit_f_is(fldty, fldty, NULL, NULL,
                                      builder.CreateExtractValue(varg1, ArrayRef<unsigned>(&j,1)),
                                      builder.CreateExtractValue(varg2, ArrayRef<unsigned>(&j,1)),
                                      ctx);
                        j++;
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
    else
        answer = builder.CreateTrunc(builder.CreateCall2(prepare_call(jlegal_func), varg1, varg2), T_int1);
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
            jl_tuple_t *aty = call_arg_types(&args[1], nargs, ctx);
            rt1 = (jl_value_t*)aty;
            // attempt compile-time specialization for inferred types
            if (aty != NULL) {
                /*
                  if (trace) {
                      JL_PRINTF(JL_STDOUT, "call %s%s\n",
                      jl_sprint(args[0]),
                      jl_sprint((jl_value_t*)aty));
                  }
                */
                f = jl_get_specialization(f, aty);
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
        if (!jl_is_typevar(aty) && aty != (jl_value_t*)jl_any_type &&
            jl_type_intersection(aty,(jl_value_t*)jl_tuple_type)==(jl_value_t*)jl_bottom_type) {
            Value *arg1 = emit_expr(args[1], ctx);
            if (jl_is_leaf_type(aty)) {
                if (jl_is_type_type(aty))
                    aty = (jl_value_t*)jl_typeof(jl_tparam0(aty));
                JL_GC_POP();
                return literal_pointer_val(aty);
            }
            arg1 = boxed(arg1,ctx);
            JL_GC_POP();
            return emit_typeof(arg1);
        }
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
            if (!jl_is_tuple(tp0) && jl_is_leaf_type(tp0)) {
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
            builder.CreateCall2(prepare_call(typeassert), arg1, boxed(emit_expr(args[2], ctx),ctx));
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
            if (!jl_is_tuple(tp0) && !jl_is_type_type(tp0)) {
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
    else if (f->fptr == &jl_f_tuplelen && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_tuple(aty)) {
            if (symbol_eq(args[1], ctx->vaName) &&
                !ctx->vars[ctx->vaName].isAssigned) {
                JL_GC_POP();
                return emit_n_varargs(ctx);
            }
            else {
                Value *arg1 = emit_expr(args[1], ctx);
                JL_GC_POP();
                return emit_tuplelen(arg1,aty);
            }
        }
    }
    else if (f->fptr == &jl_f_apply && nargs==3 && ctx->vaStack &&
             symbol_eq(args[3], ctx->vaName) && expr_type(args[2],ctx) == (jl_value_t*)jl_function_type) {
        Value *theF = emit_expr(args[2],ctx);
        Value *theFptr = emit_nthptr_recast(theF,1, tbaa_func, jl_pfptr_llvmt);
        Value *nva = emit_n_varargs(ctx);
#ifdef _P64
        nva = builder.CreateTrunc(nva, T_int32);
#endif
        Value *r =
            builder.CreateCall3(prepare_call(theFptr), theF,
                                builder.CreateGEP(ctx->argArray,
                                                  ConstantInt::get(T_size, ctx->nReqArgs)),
                                nva);
        JL_GC_POP();
        return r;
    }
    else if (f->fptr == &jl_f_tupleref && nargs==2) {
        jl_value_t *tty = expr_type(args[1], ctx); rt1 = tty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        if (jl_is_tuple(tty) && ity==(jl_value_t*)jl_long_type) {
            if (ctx->vaStack && symbol_eq(args[1], ctx->vaName)) {
                Value *valen = emit_n_varargs(ctx);
                Value *idx = emit_unbox(T_size,
                                        emit_unboxed(args[2], ctx),ity);
                idx = emit_bounds_check(idx, valen, ctx);
                idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
                JL_GC_POP();
                return tbaa_decorate(tbaa_user, builder.
                    CreateLoad(builder.CreateGEP(ctx->argArray,idx),false));
            }
            Value *arg1 = emit_expr(args[1], ctx);
            if (jl_is_long(args[2])) {
                size_t tlen = jl_tuple_len(tty);
                int isseqt =
                    tlen>0 && jl_is_vararg_type(jl_tupleref(tty, tlen-1));
                size_t idx = jl_unbox_long(args[2]);
                if (idx > 0 && (idx < tlen || (idx == tlen && !isseqt))) {
                    // known to be in bounds
                    JL_GC_POP();
                    return emit_tupleref(arg1,ConstantInt::get(T_size,idx),tty,ctx);
                }
                if (idx==0 || (!isseqt && idx > tlen)) {
                    builder.CreateCall2(prepare_call(jlthrow_line_func),
                                        tbaa_decorate(tbaa_const, builder.CreateLoad(prepare_global(jlboundserr_var))),
                                        ConstantInt::get(T_int32, ctx->lineno));
                    JL_GC_POP();
                    return V_null;
                }
            }
            Value *tlen = emit_tuplelen(arg1,tty);
            Value *idx = emit_unbox(T_size,
                                    emit_unboxed(args[2], ctx), ity);
            emit_bounds_check(idx, tlen, ctx);
            JL_GC_POP();
            return emit_tupleref(arg1,idx,tty,ctx);
        }
    }
    else if (f->fptr == &jl_f_tuple) {
        if (nargs == 0) {
            JL_GC_POP();
            return literal_pointer_val((jl_value_t*)jl_null);
        }
        size_t i;
        for(i=0; i < nargs; i++) {
            jl_value_t *it = (jl_value_t*)expr_type(args[i+1],ctx);
            if (!(jl_isbits(it) && jl_is_leaf_type(it)))
                break;
        }
        if (i >= nargs && ctx->linfo->specTypes) {
            rt1 = expr_type(expr, ctx);
            if (jl_is_tuple(rt1) && nargs == jl_tuple_len(rt1)) {
                for(i=0; i < nargs; i++) {
                    // paranoia: make sure the inferred tuple type matches what
                    // we are about to construct.
                    if (!jl_types_equal(jl_tupleref(rt1,i), expr_type(args[i+1],ctx)))
                        break;
                }
                if (i >= nargs) {
                    Type *ty = julia_type_to_llvm(rt1);
                    Value *tpl = NULL;
                    if (ty != T_void)
                        tpl = UndefValue::get(ty);
                    for (size_t i = 0; i < nargs; ++i) {
                        Type *ety = NULL;
                        if (tpl != NULL)
                            ety = jl_llvmtuple_eltype(tpl->getType(),rt1,i);
                        if (tpl == NULL || ety == T_void || ety->isEmptyTy()) {
                            emit_expr(args[i+1],ctx); //for side effects (if any)
                            continue;
                        }
                        assert(tpl != NULL);
                        Value *elt = emit_unbox(ety,emit_unboxed(args[i+1],ctx),jl_tupleref(rt1,i));
                        tpl = emit_tupleset(tpl,ConstantInt::get(T_size,i+1),elt,rt1,ctx);
                    }
                    JL_GC_POP();
                    if (ty->isEmptyTy())
                        return mark_julia_type(tpl, rt1);
                    return tpl;
                }
            }
        }

        int last_depth = ctx->argDepth;
        // eval the first argument first, then do hand-over-hand to track the tuple.
        Value *arg1val = emit_expr(args[1], ctx);
        Value *arg1 = boxed(arg1val,ctx);
        if (arg1val->getType() != jl_pvalue_llvmt || might_need_root(args[1]))
            make_gcroot(arg1, ctx);
        bool rooted = false;
#ifdef OVERLAP_TUPLE_LEN
        size_t nwords = nargs+1;
#else
        size_t nwords = nargs+2;
#endif
        Value *tup =
            builder.CreateCall(prepare_call(jlallocobj_func),
                               ConstantInt::get(T_size, sizeof(void*)*nwords));
#ifdef OVERLAP_TUPLE_LEN
        builder.CreateStore(arg1, emit_nthptr_addr(tup, 1));
#else
        builder.CreateStore(arg1, emit_nthptr_addr(tup, 2));
#endif
        ctx->argDepth = last_depth;
#ifdef  OVERLAP_TUPLE_LEN
        builder.
            CreateStore(builder.
                        CreateOr(builder.CreatePtrToInt(literal_pointer_val((jl_value_t*)jl_tuple_type), T_int64),
                                 ConstantInt::get(T_int64, nargs<<52)),
                        builder.CreateBitCast(emit_nthptr_addr(tup, (size_t)0),
                                              T_pint64));
#else
        builder.CreateStore(literal_pointer_val((jl_value_t*)jl_tuple_type),
                            emit_nthptr_addr(tup, (size_t)0));
        builder.CreateStore(ConstantInt::get(T_size, nargs),
                            builder.CreateBitCast(emit_nthptr_addr(tup, (size_t)1), T_psize));
#endif
#ifdef OVERLAP_TUPLE_LEN
        size_t offs = 1;
#else
        size_t offs = 2;
#endif
        for(i=1; i < nargs; i++) {
            builder.CreateStore(V_null,
                                emit_nthptr_addr(tup, i+offs));
        }
        for(i=1; i < nargs; i++) {
            if (might_need_root(args[i+1]) && !rooted) {
                make_gcroot(tup, ctx);
                rooted = true;
            }
            Value *argval = emit_expr(args[i+1], ctx);
            if (argval->getType() != jl_pvalue_llvmt && !rooted) {
                make_gcroot(tup, ctx);
                rooted = true;
            }
            Value *argi = boxed(argval,ctx);
            builder.CreateStore(argi, emit_nthptr_addr(tup, i+offs));
        }
        ctx->argDepth = last_depth;
        JL_GC_POP();
        return tup;
    }
    else if (f->fptr == &jl_f_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx), ctx);
        JL_GC_POP();
        builder.CreateCall2(prepare_call(jlthrow_line_func), arg1,
                            ConstantInt::get(T_int32, ctx->lineno));
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
                    if (jl_array_store_unboxed(ety) &&
                        ((jl_datatype_t*)ety)->size == 0) {
                        // no-op, but emit expr for possible effects
                        assert(jl_is_datatype(ety));
                        emit_expr(args[2],ctx,false);
                    }
                    else {
                        typed_store(emit_arrayptr(ary,args[1],ctx), idx,
                                    ety==(jl_value_t*)jl_any_type ? emit_expr(args[2],ctx) : emit_unboxed(args[2],ctx),
                                    ety, ctx, tbaa_user);
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
        if (jl_is_structtype(stt) && fldt == (jl_value_t*)jl_long_type && !jl_subtype((jl_value_t*)jl_module_type, (jl_value_t*)stt, 0)) {
            size_t nfields = jl_tuple_len(stt->names);
            // integer index
            if (jl_is_long(args[2])) {
                // known index
                size_t idx = jl_unbox_long(args[2])-1;
                if (idx < nfields) {
                    Value *fld = emit_getfield(args[1],
                                               (jl_sym_t*)jl_tupleref(stt->names, idx),
                                               ctx);
                    JL_GC_POP();
                    return fld;
                }
            }
            else {
                // unknown index
                Value *strct = emit_expr(args[1], ctx);
                Value *idx = emit_unbox(T_size, emit_unboxed(args[2], ctx), (jl_value_t*)jl_long_type);
                Type *llvm_st = strct->getType();
                if (llvm_st == jl_pvalue_llvmt) {
                    if (is_structtype_all_pointers(stt)) {
                        idx = emit_bounds_check(idx, ConstantInt::get(T_size, nfields), ctx);
                        Value *fld =
                            tbaa_decorate(tbaa_user, builder.
                            CreateLoad(builder.
                                       CreateGEP(builder.
                                                 CreateBitCast(strct, jl_ppvalue_llvmt),
                                                 builder.CreateAdd(idx,ConstantInt::get(T_size,1)))));
                        if ((unsigned)stt->ninitialized != jl_tuple_len(stt->types)) {
                            null_pointer_check(fld, ctx);
                        }
                        JL_GC_POP();
                        return fld;
                    }
                    else if (is_tupletype_homogeneous(stt->types)) {
                        assert(nfields > 0); // nf==0 trapped by all_pointers case
                        jl_value_t *jt = jl_t0(stt->types);
                        idx = emit_bounds_check(idx, ConstantInt::get(T_size, nfields), ctx);
                        Value *ptr = data_pointer(strct);
                        JL_GC_POP();
                        return typed_load(ptr, idx, jt, ctx, stt->mutabl ? tbaa_user : tbaa_immut);
                    }
                    else {
                        idx = builder.CreateSub(idx, ConstantInt::get(T_size, 1));
                        Value *fld = builder.CreateCall2(prepare_call(jlgetnthfieldchecked_func), strct, idx);
                        JL_GC_POP();
                        return fld;
                    }
                }
                else if (is_tupletype_homogeneous(stt->types)) {
                    assert(llvm_st->isStructTy());
                    // TODO: move these allocas to the first basic block instead of
                    // frobbing the stack
                    Value *fld;
                    if (nfields == 0) {
                        emit_bounds_check(idx, ConstantInt::get(T_size, nfields), ctx);
                        fld = UndefValue::get(jl_pvalue_llvmt);
                    }
                    else {
                        Instruction *stacksave =
                            CallInst::Create(Intrinsic::getDeclaration(jl_Module,Intrinsic::stacksave));
                        builder.Insert(stacksave);
                        Value *tempSpace = builder.CreateAlloca(llvm_st);
                        builder.CreateStore(strct, tempSpace);
                        jl_value_t *jt = jl_t0(stt->types);
                        idx = emit_bounds_check(idx, ConstantInt::get(T_size, nfields), ctx);
                        Value *ptr = builder.CreateGEP(tempSpace, ConstantInt::get(T_size, 0));
                        fld = typed_load(ptr, idx, jt, ctx, stt->mutabl ? tbaa_user : tbaa_immut);
                        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,Intrinsic::stackrestore),
                                           stacksave);
                    }
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
                jl_value_t *ft = jl_tupleref(sty->types, idx);
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
                    emit_setfield(sty, strct, idx, rhs, ctx);
                    JL_GC_POP();
                    return rhs;
                }
            }
        }
        // TODO: faster code for integer index
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
                if (jl_has_typevars(ty) || jl_is_tuple(ty)) {
                    // add root for types not cached. issue #7065
                    jl_add_linfo_root(ctx->linfo, ty);
                }
                JL_GC_POP();
                return literal_pointer_val(ty);
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
                (sty->names == jl_null && sty->size > 0)) {
                JL_GC_POP();
                return ConstantInt::get(T_size, sty->size);
            }
        }
    }
    // TODO: other known builtins
    JL_GC_POP();
    return NULL;
}

static Value *emit_jlcall(Value *theFptr, Value *theF, jl_value_t **args,
                          size_t nargs, jl_codectx_t *ctx)
{
    // emit arguments
    int argStart = ctx->argDepth;
    for(size_t i=0; i < nargs; i++) {
        Value *anArg = emit_expr(args[i], ctx);
        // put into argument space
        make_gcroot(boxed(anArg, ctx, expr_type(args[i],ctx)), ctx);
    }

    // call
    Value *myargs = Constant::getNullValue(jl_ppvalue_llvmt);
    if (ctx->argTemp != NULL && nargs > 0) {
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
    }
    Value *result = builder.CreateCall3(prepare_call(theFptr), theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
    ctx->argDepth = argStart;
    return result;
}

static Value *emit_call_function_object(jl_function_t *f, Value *theF, Value *theFptr,
                                        bool specialized,
                                        jl_value_t **args, size_t nargs,
                                        jl_codectx_t *ctx)
{
    Value *result;
    if (f!=NULL && specialized && f->linfo!=NULL && f->linfo->cFunctionObject!=NULL) {
        // emit specialized call site
        Function *cf = (Function*)f->linfo->cFunctionObject;
        FunctionType *cft = cf->getFunctionType();
        size_t nfargs = cft->getNumParams();
        Value **argvals = (Value**)alloca(nfargs*sizeof(Value*));
        unsigned idx = 0;
        for(size_t i=0; i < nargs; i++) {
            Type *at = cft->getParamType(idx);
            Type *et = julia_type_to_llvm(jl_tupleref(f->linfo->specTypes,i));
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
                argvals[idx] = emit_unbox(at, emit_unboxed(args[i+1], ctx),
                                          jl_tupleref(f->linfo->specTypes,i));
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
        if (result != NULL) return result;
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
            Value *theFunc = emit_expr(args[0], ctx);
#ifdef JL_GC_MARKSWEEP
            if (!headIsGlobal && (jl_is_expr(a0) || jl_is_lambda_info(a0))) {
                make_gcroot(boxed(theFunc,ctx), ctx);
            }
#endif
            // extract pieces of the function object
            // TODO: try extractvalue instead
            theFptr = emit_nthptr_recast(theFunc, 1, tbaa_func, jl_pfptr_llvmt);
            theF = theFunc;
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
        theFptr = emit_nthptr_recast(theFunc, 1, tbaa_func, jl_pfptr_llvmt);
        Value *r1 = builder.CreateCall3(prepare_call(theFptr), theFunc, myargs,
                                        ConstantInt::get(T_int32,nargs));
        builder.CreateBr(mergeBB1);
        ctx->f->getBasicBlockList().push_back(elseBB1);
        builder.SetInsertPoint(elseBB1);
        // not function
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
        Value *r2 = builder.CreateCall3(prepare_call(jlapplygeneric_func),
                                        literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
                                        myargs,
                                        ConstantInt::get(T_int32,nargs+1));
        builder.CreateBr(mergeBB1);
        ctx->f->getBasicBlockList().push_back(mergeBB1);
        builder.SetInsertPoint(mergeBB1);
        PHINode *ph = builder.CreatePHI(jl_pvalue_llvmt, 2);
        ph->addIncoming(r1, funcBB1);
        ph->addIncoming(r2, elseBB1);
        result = ph;
    }

    ctx->argDepth = last_depth;
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
#ifdef OVERLAP_TUPLE_LEN
            return emit_nthptr_addr(emit_nthptr((Value*)ctx->envArg, idx+1, tbaa_tuplelen), 1);
#else
            return emit_nthptr_addr(emit_nthptr((Value*)ctx->envArg, idx+2, tbaa_tuplelen), 1);
#endif
        }
#ifdef OVERLAP_TUPLE_LEN
        return emit_nthptr_addr((Value*)ctx->envArg, idx+1);
#else
        return emit_nthptr_addr((Value*)ctx->envArg, idx+2);
#endif
    }
    Value *l = vi.memvalue;
    if (l == NULL) return NULL;
    if (isBoxed(s, ctx)) {
        return emit_nthptr_addr(builder.CreateLoad(l,false), 1);
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
        builder.CreateBr(ifok);
        ctx->f->getBasicBlockList().push_back(ifok);
        builder.SetInsertPoint(ifok);
    }
    return v;
}

static Value *ghostValue(jl_value_t *ty)
{
    if (jl_is_datatype(ty)) {
        Type *llvmty = julia_struct_to_llvm(ty);
        if (llvmty == T_void)
            return mark_julia_type(UndefValue::get(NoopType),ty);
        return UndefValue::get(llvmty);
    }
    else {
        return mark_julia_type(UndefValue::get(NoopType),ty);
    }
}

static Value *emit_var(jl_sym_t *sym, jl_value_t *ty, jl_codectx_t *ctx, bool isboxed)
{
    bool isglobal = is_global(sym, ctx);
    if (isglobal) {
        // look for static parameter
        for(size_t i=0; i < jl_tuple_len(ctx->sp); i+=2) {
            assert(jl_is_symbol(jl_tupleref(ctx->sp, i)));
            if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                return literal_pointer_val(jl_tupleref(ctx->sp, i+1));
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
        ((!is_var_closed(sym, ctx) || !vi.isAssigned) &&
         !jl_subtype((jl_value_t*)jl_undef_type, ty, 0))) {
        Value *theLoad = builder.CreateLoad(bp, vi.isVolatile);
        if (vi.closureidx > -1 && !(vi.isAssigned && vi.isCaptured))
            theLoad = tbaa_decorate(tbaa_const, (Instruction*)theLoad);
        return tpropagate(bp, theLoad);
    }
    return emit_checked_var(bp, sym, ctx, vi.isVolatile);
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    jl_sym_t *s = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_symbolnode(l))
        s = jl_symbolnode_sym(l);
    else
        assert(false);
    jl_binding_t *bnd=NULL;
    Value *bp = var_binding_pointer(s, &bnd, true, ctx);
    Value *rval;
    if (bnd) {
        rval = boxed(emit_expr(r, ctx, true),ctx);
        builder.CreateCall2(prepare_call(jlcheckassign_func),
                            literal_pointer_val(bnd),
                            rval);
    }
    else {
        jl_varinfo_t &vi = ctx->vars[s];
        jl_value_t *rt = expr_type(r,ctx);
        if ((jl_is_symbol(r) || jl_is_symbolnode(r)) && rt == jl_bottom_type) {
            // sometimes x = y::Union() occurs
            if (builder.GetInsertBlock()->getTerminator() != NULL)
                return;
        }
        if (bp != NULL) {
            Type *vt = bp->getType();
            if (vt->isPointerTy() && vt->getContainedType(0)!=jl_pvalue_llvmt) {
                // TODO: `rt` is techincally correct here, but sometimes we're not propagating type information
                // properly, so `rt` is a union type, while LLVM know that it's not. However, in order for this to
                // happen, we need to already be sure somewhere that we have the right type, so vi.declType is fine
                // even if not techincally correct.
                rval = emit_unbox(vt->getContainedType(0), emit_unboxed(r, ctx), vi.declType);
            }
            else {
                rval = boxed(emit_expr(r, ctx, true),ctx,rt);
            }
            if (builder.GetInsertBlock()->getTerminator() == NULL) {
                builder.CreateStore(rval, bp, vi.isVolatile);
            }
        }
        else {
            rval = emit_expr(r, ctx, true);
            if (!vi.used)  // don't actually do the assignment if the var is never read
                return;
            // Make sure this is already boxed. If not, there was
            // something wrong in the earlier analysis as this should
            // have been alloca'd
            assert(rval->getType() == jl_pvalue_llvmt || rval->getType() == NoopType);
        }

        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            jl_arrayvar_t *av = arrayvar_for(l, ctx);
            if (av != NULL) {
                assign_arrayvar(*av, rval);
            }
        }

        if (vi.isSA &&
            ((bp == NULL) ||
             (!vi.isCaptured && !vi.isArgument && !vi.usedUndef &&
              !vi.isVolatile && rval->getType() == jl_pvalue_llvmt))) {
            // use SSA value instead of GC frame load for var access
            vi.SAvalue = rval;
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
        return builder.CreateICmpEQ(condV, literal_pointer_val(jl_false));
    }
    // not a boolean
    return ConstantInt::get(T_int1,0);
}

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool isboxed,
                        bool valuepos)
{
    if (jl_is_symbol(expr)) {
        if (!valuepos) return NULL;
        return emit_var((jl_sym_t*)expr, (jl_value_t*)jl_undef_type, ctx, isboxed);
    }
    if (jl_is_symbolnode(expr)) {
        if (!valuepos) return NULL;
        return emit_var(jl_symbolnode_sym(expr), jl_symbolnode_type(expr), ctx, isboxed);
    }
    else if (jl_is_labelnode(expr)) {
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
    else if (jl_is_linenode(expr)) {
        if (valuepos)
            jl_error("Linenode in value position");
        return NULL;
    }
    else if (jl_is_quotenode(expr)) {
        jl_value_t *jv = jl_fieldref(expr,0);
        if (jl_is_bitstype(jl_typeof(jv))) {
            return emit_expr(jv, ctx, isboxed, valuepos);
        }
        if (!jl_is_symbol(jv)) {
            jl_add_linfo_root(ctx->linfo, jv);
        }
        return literal_pointer_val(jv);
    }
    else if (jl_is_gotonode(expr)) {
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
    else if (jl_is_getfieldnode(expr)) {
        return emit_getfield(jl_fieldref(expr,0),
                             (jl_sym_t*)jl_fieldref(expr,1), ctx);
    }
    else if (jl_is_topnode(expr)) {
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
        jl_value_t *etype = expr_type(expr, ctx);
        jl_module_t *mod = topmod(ctx);
        jl_binding_t *b = jl_get_binding(mod, var);
        if (b == NULL)
            b = jl_get_binding_wr(mod, var);
        Value *bp = julia_binding_gv(b);
        if ((b->constp && b->value!=NULL) ||
            (etype!=(jl_value_t*)jl_any_type &&
             !jl_subtype((jl_value_t*)jl_undef_type, etype, 0))) {
            return builder.CreateLoad(bp, false);
        }
        return emit_checked_var(bp, var, ctx);
    }
    else if (jl_is_newvarnode(expr)) {
        assert(!valuepos);
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
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
    jl_value_t **args = &jl_cellref(ex->args,0);
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
        Value *bp;
        if (theF != NULL) {
            bp = make_gcroot(theF, ctx);
        }
        else {
            if (is_global((jl_sym_t*)mn, ctx)) {
                bnd = jl_get_binding_for_method_def(ctx->module, (jl_sym_t*)mn);
                bp = julia_binding_gv(bnd);
            }
            else {
                bp = var_binding_pointer((jl_sym_t*)mn, &bnd, false, ctx);
            }
        }
        Value *a1 = boxed(emit_expr(args[1], ctx),ctx);
        make_gcroot(a1, ctx);
        Value *a2 = boxed(emit_expr(args[2], ctx),ctx);
        make_gcroot(a2, ctx);
        Value *mdargs[8] =
            { name, bp, literal_pointer_val(bnd), a1, a2, literal_pointer_val(args[3]),
              literal_pointer_val((jl_value_t*)jl_module_call_func(ctx->module)),
              ConstantInt::get(T_int32, (int)iskw) };
        ctx->argDepth = last_depth;
        return builder.CreateCall(prepare_call(jlmethod_func), ArrayRef<Value*>(&mdargs[0], 8));
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
        if (jl_is_tuple(extype))
            jl_add_linfo_root(ctx->linfo, extype);
        return literal_pointer_val(extype);
    }
    else if (head == new_sym) {
        jl_value_t *ty = expr_type(args[0], ctx);
        size_t nargs = jl_array_len(ex->args);
        if (jl_is_type_type(ty) && jl_is_datatype(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            ty = jl_tparam0(ty);
            jl_datatype_t *sty = (jl_datatype_t*)ty;
            size_t nf = jl_tuple_len(sty->names);
            if (nf > 0) {
                if (jl_isbits(sty)) {
                    Type *lt = julia_type_to_llvm(ty);
                    if (lt == T_void)
                        return mark_julia_type(UndefValue::get(NoopType),ty);
                    Value *strct = UndefValue::get(lt);
                    size_t na = nargs-1 < nf ? nargs-1 : nf;
                    unsigned idx = 0;
                    for(size_t i=0; i < na; i++) {
                        jl_value_t *jtype = jl_tupleref(sty->types,i);
                        Type *fty = julia_type_to_llvm(jtype);
                        if (fty == T_void || fty->isEmptyTy())
                            continue;
                        Value *fval = emit_unbox(fty, emit_unboxed(args[i+1],ctx), jtype);
                        if (fty == T_int1)
                            fval = builder.CreateZExt(fval, T_int8);
                        strct = builder.
                            CreateInsertValue(strct, fval, ArrayRef<unsigned>(&idx,1));
                        idx++;
                    }
                    return mark_julia_type(strct,ty);
                }
                Value *f1 = NULL;
                size_t j = 0;
                int fieldStart = ctx->argDepth;
                bool needroots = false;
                for(size_t i=1; i < nargs; i++) {
                    needroots |= might_need_root(args[i]);
                }
                if (nf > 0 && sty->fields[0].isptr && nargs>1) {
                    // emit first field before allocating struct to save
                    // a couple store instructions. avoids initializing
                    // the first field to NULL, and sometimes the GC root
                    // for the new struct.
                    Value *fval = emit_expr(args[1],ctx);
                    f1 = boxed(fval,ctx);
                    j++;
                    if (might_need_root(args[1]) || fval->getType() != jl_pvalue_llvmt)
                        make_gcroot(f1, ctx);
                }
                Value *strct =
                    builder.CreateCall(prepare_call(jlallocobj_func),
                                       ConstantInt::get(T_size,
                                                        sizeof(void*)+sty->size));
                builder.CreateStore(literal_pointer_val((jl_value_t*)ty),
                                    emit_nthptr_addr(strct, (size_t)0));
                if (f1) {
                    if (!jl_subtype(expr_type(args[1],ctx), jl_t0(sty->types), 0))
                        emit_typecheck(f1, jl_t0(sty->types), "new", ctx);
                    emit_setfield(sty, strct, 0, f1, ctx, false);
                    ctx->argDepth = fieldStart;
                    if (nf > 1 && needroots)
                        make_gcroot(strct, ctx);
                }
                else if (nf > 0 && needroots) {
                    make_gcroot(strct, ctx);
                }
                for(size_t i=j; i < nf; i++) {
                    if (sty->fields[i].isptr) {
                        emit_setfield(sty, strct, i, V_null, ctx, false);
                    }
                }
                for(size_t i=j+1; i < nargs; i++) {
                    Value *rhs = emit_expr(args[i],ctx);
                    if (sty->fields[i-1].isptr && rhs->getType() != jl_pvalue_llvmt &&
                        !needroots) {
                        // if this struct element needs boxing and we haven't rooted
                        // the struct, root it now.
                        make_gcroot(strct, ctx);
                        needroots = true;
                    }
                    if (rhs->getType() == jl_pvalue_llvmt) {
                        if (!jl_subtype(expr_type(args[i],ctx), jl_tupleref(sty->types,i-1), 0))
                            emit_typecheck(rhs, jl_tupleref(sty->types,i-1), "new", ctx);
                    }
                    emit_setfield(sty, strct, i-1, rhs, ctx, false);
                }
                ctx->argDepth = fieldStart;
                return strct;
            }
            else {
                // 0 fields, singleton
                return literal_pointer_val(jl_new_struct_uninit((jl_datatype_t*)ty));
            }
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
        CallInst *sj = builder.CreateCall2(prepare_call(setjmp_func), jbuf, ConstantInt::get(T_int32,0));
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
            jl_compileropts.check_bounds == JL_COMPILEROPT_CHECK_BOUNDS_DEFAULT) {
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
            JL_PRINTF(JL_STDERR, "Warning: could not attach metadata for @simd loop.\n");
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

static bool store_unboxed_p(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->vars[s];
    jl_value_t *jt = vi.declType;
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->linfo->inferred && jltupleisbits(jt,false) &&
            // don't unbox intrinsics, since inference depends on their having
            // stable addresses for table lookup.
            jt != (jl_value_t*)jl_intrinsic_type && !vi.isCaptured &&
            // don't unbox vararg tuples
            s != ctx->vaName);
}

static Value *alloc_local(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_varinfo_t &vi = ctx->vars[s];
    jl_value_t *jt = vi.declType;
    Value *lv = NULL;
    assert(store_unboxed_p(s,ctx));
    Type *vtype = julia_struct_to_llvm(jt);
    assert(vtype != jl_pvalue_llvmt);
    if (vtype != T_void && !vtype->isEmptyTy()) {
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
    return lv;
}

static void maybe_alloc_arrayvar(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_value_t *jt = ctx->vars[s].declType;
    if (jl_is_array_type(jt) && jl_is_leaf_type(jt) &&
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

extern "C" jl_tuple_t *jl_tuple_tvars_to_symbols(jl_tuple_t *t);

// gc frame emission
static void allocate_gc_frame(size_t n_roots, jl_codectx_t *ctx)
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
    ctx->last_gcframe_inst = BasicBlock::iterator(linst);
    // initialize local variable stack roots to null
    for(size_t i=0; i < (size_t)ctx->argSpaceOffs; i++) {
        Value *varSlot = builder.CreateConstGEP1_32(ctx->argTemp,i);
        builder.CreateStore(V_null, varSlot);
    }
#else
    ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                       ConstantInt::get(T_int32, n_roots));
#endif

}

static void finalize_gc_frame(jl_codectx_t *ctx)
{
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
            Instruction *argTempi =
                GetElementPtrInst::Create(newgcframe,
                                          ConstantInt::get(T_int32, i+ctx->argSpaceOffs+2));
            instList.insertAfter(after, argTempi);
            after = new StoreInst(V_null, argTempi);
            instList.insertAfter(argTempi, after);
        }
    }
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

    Function *w = Function::Create(jl_func_sig, Function::ExternalLinkage,
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
    allocate_gc_frame(0,&ctx);
    ctx.argSpaceInits = &b0->back();

    size_t nargs = jl_tuple_len(lam->specTypes);
    size_t nfargs = f->getFunctionType()->getNumParams();
    Value **args = (Value**) alloca(nfargs*sizeof(Value*));
    unsigned argIdx = 0;
    unsigned idx = 0;
    for(size_t i=0; i < nargs; i++) {
        jl_value_t *ty = jl_tupleref(lam->specTypes, i);
        Value *argPtr = builder.CreateGEP(argArray,
                                          ConstantInt::get(T_size, argIdx));
        Value *theArg = builder.CreateLoad(argPtr, false);
        Value *theNewArg = theArg;
        argIdx++;
        if (jl_is_leaf_type(ty) && jl_isbits(ty)) {
            Type *lty = julia_struct_to_llvm(ty);
            assert(lty != NULL);
            if (lty == T_void || lty->isEmptyTy())
                continue;
            theNewArg = emit_unbox(lty, theArg, ty);
        }
        else if (jl_is_tuple(ty)) {
            Type *lty = julia_struct_to_llvm(ty);
            if (lty != jl_pvalue_llvmt) {
                if (lty == T_void || lty->isEmptyTy())
                    continue;
                theNewArg = emit_unbox(lty, theArg, ty);
            }
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
    Instruction *gcpop = (Instruction*)builder.CreateConstGEP1_32(ctx.gcframe, 1);
    ctx.gc_frame_pops.push_back(gcpop);
    builder.CreateStore(builder.CreateBitCast(builder.CreateLoad(gcpop, false), jl_ppvalue_llvmt),
                        prepare_global(jlpgcstack_var));

    finalize_gc_frame(&ctx);
    builder.CreateRet(r);

    return w;
}

// cstyle = compile with c-callable signature, not jlcall
static Function *emit_function(jl_lambda_info_t *lam, bool cstyle)
{
    // step 1. unpack AST and allocate codegen context for this function
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_tuple_t *sparams = NULL;
    JL_GC_PUSH2(&ast, &sparams);
    if (!jl_is_expr(ast)) {
        ast = (jl_expr_t*)jl_uncompress_ast(lam, (jl_value_t*)ast);
    }
    assert(jl_is_expr(ast));
    sparams = jl_tuple_tvars_to_symbols(lam->sparams);
    //JL_PRINTF((jl_value_t*)ast);
    //JL_PRINTF(JL_STDOUT, "\n");
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
        varinfo.declType = jl_cellref(vi,1);
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
        varinfo.used = true;
        varinfo.escapes = true;
        varinfo.declType = jl_cellref(vi,1);
    }

    // step 3. some variable analysis

    // finish recording escape info
    simple_escape_analysis((jl_value_t*)ast, true, &ctx);

    // determine which vars need to be volatile
    jl_array_t *stmts = jl_lam_body(ast)->args;
    mark_volatile_vars(stmts, ctx.vars);

    // fetch init exprs of SSA vars for easy reference
    for(i=0; i < jl_array_len(stmts); i++) {
        jl_value_t *st = jl_cellref(stmts,i);
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == assign_sym) {
            jl_value_t *lhs = jl_exprarg(st,0);
            if (jl_is_symbolnode(lhs))
                lhs = (jl_value_t*)jl_symbolnode_sym(lhs);
            std::map<jl_sym_t*,jl_varinfo_t>::iterator it = ctx.vars.find((jl_sym_t*)lhs);
            if (it != ctx.vars.end()) {
                jl_varinfo_t &vi = (*it).second;
                if (vi.isSA) {
                    vi.initExpr = jl_exprarg(st,1);
                }
            }
        }
    }

    // step 4. determine function signature
    jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)ast);
    Function *f = NULL;

    bool specsig = false;
    if (cstyle && !va && !hasCapt) {
        specsig = true;
    }
    else {
        if (!va && !hasCapt && lam->specTypes != NULL) {
            // no captured vars and not vararg
            // consider specialized signature
            for(size_t i=0; i < jl_tuple_len(lam->specTypes); i++) {
                if (jltupleisbits(jl_tupleref(lam->specTypes, i))) {
                    specsig = true;
                    break;
                }
            }
            if (jl_tuple_len(lam->specTypes) == 0)
                specsig = true;
            if (jltupleisbits(jlrettype))
                specsig = true;
        }
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

    if (specsig) {
        std::vector<Type*> fsig(0);
        for(size_t i=0; i < jl_tuple_len(lam->specTypes); i++) {
            Type *ty = julia_type_to_llvm(jl_tupleref(lam->specTypes,i));
            if (ty == T_void || ty->isEmptyTy()) {
                ctx.vars[jl_decl_var(jl_cellref(largs,i))].isGhost = true;
            }
            else {
                fsig.push_back(ty);
            }
        }
        Type *rt = (jlrettype == (jl_value_t*)jl_void_type ? T_void : julia_type_to_llvm(jlrettype));
        f = Function::Create(FunctionType::get(rt, fsig, false),
                             Function::ExternalLinkage, funcName.str(), m);
        addComdat(f);
        if (lam->cFunctionObject == NULL) {
            lam->cFunctionObject = (void*)f;
            lam->cFunctionID = jl_assign_functionID(f);
        }
        if (lam->functionObject == NULL) {
            Function *fwrap = gen_jlcall_wrapper(lam, ast, f);
            lam->functionObject = (void*)fwrap;
            lam->functionID = jl_assign_functionID(fwrap);
        }
    }
    else {
        f = Function::Create(jl_func_sig, Function::ExternalLinkage,
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
#if LLVM32 && !LLVM33
    f->addAttribute(Attributes::FunctionIndex,
        Attributes::get(f->getContext(),*attr));
#else
    f->addAttributes(AttributeSet::FunctionIndex,
        AttributeSet::get(f->getContext(),
            AttributeSet::FunctionIndex,*attr));
#endif
#endif

#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && LLVM35
    f->setHasUWTable(); // force NeedsWinEH
#endif

#ifdef JL_DEBUG_BUILD
#if LLVM32 && !LLVM33
    f->addFnAttr(Attributes::StackProtectReq);
#else
    f->addFnAttr(Attribute::StackProtectReq);
#endif
#endif
    ctx.f = f;

    // step 5. set up debug info context and create first basic block
    bool in_user_code = lam->module != jl_base_module && lam->module != jl_core_module;
    bool do_coverage = jl_compileropts.code_coverage == JL_LOG_ALL || (jl_compileropts.code_coverage == JL_LOG_USER && in_user_code);
    bool do_malloc_log = jl_compileropts.malloc_log  == JL_LOG_ALL || (jl_compileropts.malloc_log    == JL_LOG_USER && in_user_code);
    jl_value_t *stmt = jl_cellref(stmts,0);
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
    DIFile fil;
    DISubprogram SP;

    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);

#ifdef LLVM36
    llvm::DITypeArray EltTypeArray = dbuilder.getOrCreateTypeArray(None);
#else
    llvm::DIArray EltTypeArray = dbuilder.getOrCreateArray(ArrayRef<Value*>());
#endif
    //ios_printf(ios_stderr, "\n*** compiling %s at %s:%d\n\n",
    //           lam->name->name, filename.c_str(), lno);

    DebugLoc noDbg;
    bool debug_enabled = true;
    if (dbgFuncName[0] == 0) {
        // special value: if function name is empty, disable debug info
        builder.SetCurrentDebugLocation(noDbg);
        debug_enabled = false;
        do_coverage = false;
        do_malloc_log = false;
    }
    else {
        // TODO: Fix when moving to new LLVM version
        #ifndef LLVM34
        dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        #else
        DICompileUnit CU = dbuilder.createCompileUnit(0x01, filename, ".", "julia", true, "", 0);
        assert(CU.Verify());
        #endif

#ifdef LLVM36
        DISubroutineType subrty = dbuilder.createSubroutineType(fil,EltTypeArray);
#else
        DICompositeType subrty = dbuilder.createSubroutineType(fil,EltTypeArray);
#endif

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
        assert(SP.Verify() && SP.describes(f) && SP.getFunction() == f);
    }

    std::map<jl_sym_t *, MDNode *> filescopes;

    Value *fArg=NULL, *argArray=NULL, *argCount=NULL;
    if (!specsig) {
        Function::arg_iterator AI = f->arg_begin();
        fArg = AI++;
        argArray = AI++;
        argCount = AI++;
        ctx.argArray = argArray;
        ctx.argCount = argCount;
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

    // fetch env out of function object if we need it
    if (hasCapt) {
        ctx.envArg = emit_nthptr(fArg, 2, tbaa_func);
    }

    // step 8. set up GC frame
    allocate_gc_frame(n_roots, &ctx);
    ctx.argSpaceInits = &b0->back();

    // get pointers for locals stored in the gc frame array (argTemp)
    int varnum = 0;
    for(i=0; i < largslen; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        if (store_unboxed_p(s, &ctx)) {
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
        }
        else if (ctx.vars[s].hasGCRoot) {
            Value *lv = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            ctx.vars[s].memvalue = lv;
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
            handlr->setAlignment(128); // bits == 16 bytes
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
    unsigned argIdx = 0;
    for(i=0; i < nreq; i++) {
        jl_sym_t *s = jl_decl_var(jl_cellref(largs,i));
        Value *argPtr = NULL;
        jl_value_t *argType = NULL;
        if (specsig) {
            argType = jl_tupleref(lam->specTypes,i);
            if (!ctx.vars[s].isGhost) {
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

        Value *lv = ctx.vars[s].memvalue;
        if (lv == NULL) {
            if (ctx.vars[s].isGhost) {
                ctx.vars[s].passedAs = NULL;
            }
            else {
                // if this argument hasn't been given space yet, we've decided
                // to leave it in the input argument array.
                ctx.vars[s].passedAs = theArg;
            }
        }
        else {
            // keep track of original (boxed) value to avoid re-boxing
            ctx.vars[s].passedAs = theArg;
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
                                               jl_tupleref(lam->specTypes,i)),
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
                Value *restTuple =
                    builder.CreateCall3(prepare_call(jltuple_func), V_null,
                                        builder.CreateGEP(argArray,
                                                          ConstantInt::get(T_size,nreq)),
                                        builder.CreateSub(argCount,
                                                          ConstantInt::get(T_int32,nreq)));
                if (isBoxed(argname, &ctx))
                    builder.CreateStore(builder.CreateCall(prepare_call(jlbox_func), restTuple), lv);
                else
                    builder.CreateStore(restTuple, lv);
            }
            else {
                // TODO: Perhaps allow this in the future, but for now sice varargs are always unspecialized
                // we don't
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
            if (debug_enabled)
                builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP, NULL));
            if (do_coverage)
                coverageVisitLine(filename, lno);
            ctx.lineno = lno;
        }
        else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
            lno = jl_unbox_long(jl_exprarg(stmt, 0));
            MDNode *dfil = NULL;
            if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
                jl_value_t *a1 = jl_exprarg(stmt,1);
                if (jl_is_symbol(a1)) {
                    jl_sym_t *file = (jl_sym_t*)a1;
                    // If the string is not empty
                    if (*file->name != '\0') {
                        std::map<jl_sym_t *, MDNode *>::iterator it = filescopes.find(file);
                        if (it != filescopes.end()) {
                            dfil = it->second;
                        }
                        else {
                            dfil = filescopes[file] = (MDNode*)dbuilder.createFile(file->name, ".");
                        }
                    }
                }
            }
            if (debug_enabled) {
                MDNode *scope;
                if (dfil == NULL)
                    scope = SP;
                else
                    scope = (MDNode*)dbuilder.createLexicalBlockFile(SP,DIFile(dfil));
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
        if (do_malloc_log && lno != prevlno) {
            // Check memory allocation only after finishing a line
            if (prevlno != -1)
                mallocVisitLine(filename, prevlno);
            prevlno = lno;
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
        InlineFunctionInfo info;
        if (!InlineFunction(*it,info))
            jl_error("Inlining Pass failed");
    }

    // step 18. Perform any delayed instantiations
    if (debug_enabled)
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
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage,
                                   cname, m);
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
        if (specsig) {
            jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)lam->ast);
            std::vector<Type*> fsig(0);
            for(size_t i=0; i < jl_tuple_len(lam->specTypes); i++) {
                Type *ty = julia_type_to_llvm(jl_tupleref(lam->specTypes,i));
                if (ty != T_void && !ty->isEmptyTy())
                    fsig.push_back(ty);
            }
            Type *rt = (jlrettype == (jl_value_t*)jl_void_type ? T_void : julia_type_to_llvm(jlrettype));
            Function *f = Function::Create(FunctionType::get(rt, fsig, false),
#ifdef USE_MCJIT
                                           Function::ExternalLinkage, funcName, shadow_module);
#else
                                           Function::ExternalLinkage, funcName, jl_Module);
#endif

            if (lam->cFunctionObject == NULL) {
                lam->cFunctionObject = (void*)f;
                lam->cFunctionID = jl_assign_functionID(f);
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
    jl_value_t *box = (jl_value_t*)alloc_2w();
#ifdef OVERLAP_TUPLE_LEN
    box->type = (size_t)jl_box_any_type;
#else
    box->type = jl_box_any_type;
#endif
    ((jl_value_t**)box)[1] = v;
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
    tbaa_tuplelen = tbaa_make_child("jtbaa_tuplelen",tbaa_value);
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
    NoopType = StructType::create("NOOP",T_int1,NULL);

    // add needed base definitions to our LLVM environment
    StructType *valueSt = StructType::create(getGlobalContext(), "jl_value_t");
    Type *valueStructElts[1] = { PointerType::getUnqual(valueSt) };
    ArrayRef<Type*> vselts(valueStructElts);
    valueSt->setBody(vselts);
    jl_value_llvmt = valueSt;

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

    Type* vaelts[] = {valueStructElts[0], T_pint8
#ifdef STORE_ARRAY_LEN
                                        , T_size
#endif
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
                           NULL, "jl_pgcstack");
    add_named_global(jlpgcstack_var, (void*)&jl_pgcstack);
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
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null, m);
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
    jlboundserr_var = global_to_llvm("jl_bounds_exception",
                                     (void*)&jl_bounds_exception, m);
    jlstderr_var =
        new GlobalVariable(*m, T_int8,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_uv_stderr");
    add_named_global(jlstderr_var, (void*)&jl_uv_stderr);

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
#if LLVM32 && !LLVM33
    setjmp_func->addFnAttr(Attributes::ReturnsTwice);
#else
    setjmp_func->addFnAttr(Attribute::ReturnsTwice);
#endif
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
    add_named_global(jlcheckassign_func,
                                         (void*)&jl_checked_assignment);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(jl_pvalue_llvmt);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1ptr, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", m);
    add_named_global(jldeclareconst_func,
                                         (void*)&jl_declare_constant);

    jltuple_func = jlcall_func_to_llvm("jl_f_tuple", (void*)&jl_f_tuple, m);
    jlapplygeneric_func =
        jlcall_func_to_llvm("jl_apply_generic", (void*)&jl_apply_generic, m);
    jlgetfield_func = jlcall_func_to_llvm("jl_f_get_field", (void*)&jl_f_get_field, m);

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
    add_named_global(jlclosure_func,
                                         (void*)&jl_new_closure);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlntuple_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args5, true),
                         Function::ExternalLinkage,
                         "jl_tuple", m);
    add_named_global(jlntuple_func, (void*)&jl_tuple);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_ppvalue_llvmt);
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
    jl_alloc_tuple_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, atargs, false),
                         Function::ExternalLinkage,
                         "jl_alloc_tuple", m);
    add_named_global(jl_alloc_tuple_func, (void*)&jl_alloc_tuple);

    std::vector<Type *> puts_args(0);
    puts_args.push_back(T_pint8);
    puts_args.push_back(T_pint8);
    jlputs_func =
        Function::Create(FunctionType::get(T_void, puts_args, false),
                         Function::ExternalLinkage,
                         "jl_puts", m);
    add_named_global(jlputs_func, (void*)&jl_puts);

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

#ifdef LLVM36
    jl_data_layout = new llvm::DataLayoutPass();
#elif LLVM35
    jl_data_layout = new llvm::DataLayoutPass(*jl_ExecutionEngine->getDataLayout());
#elif defined(LLVM32)
    jl_data_layout = new DataLayout(*jl_ExecutionEngine->getDataLayout());
#else
    jl_data_layout = new TargetData(*jl_ExecutionEngine->getTargetData());
#endif
    FPM->add(jl_data_layout);

#ifdef __has_feature
#   if __has_feature(address_sanitizer)
    FPM->add(createAddressSanitizerFunctionPass());
#   endif
#endif
#if LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 3
    jl_TargetMachine->addAnalysisPasses(*FPM);
#endif
    FPM->add(createTypeBasedAliasAnalysisPass());
    if (jl_compileropts.opt_level>=1)
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

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 1
    // this has the potential to make some things a bit slower
    //FPM->add(createBBVectorizePass());
#endif
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
#if LLVM33 && !LLVM35 && !defined(INSTCOMBINE_BUG)
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
#if LLVM33 && !defined(INSTCOMBINE_BUG)
    if (jl_compileropts.opt_level>=1)
        FPM->add(createSLPVectorizerPass());     // Vectorize straight-line code
#endif

    FPM->add(createAggressiveDCEPass());         // Delete dead instructions
#if LLVM33 && !defined(INSTCOMBINE_BUG)
    if (jl_compileropts.opt_level>=1)
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
#ifdef JL_DEBUG_BUILD
    cl::ParseEnvironmentOptions("Julia", "JULIA_LLVM_ARGS");
#endif
    imaging_mode = jl_compileropts.build_path != NULL;

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

#if !defined(LLVM_VERSION_MAJOR) || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 0)
    jl_ExecutionEngine = EngineBuilder(m).setEngineKind(EngineKind::JIT).create();
#ifdef JL_DEBUG_BUILD
    llvm::JITEmitDebugInfo = true;
#endif
    //llvm::JITEmitDebugInfoToDisk = true;
    llvm::NoFramePointerElim = true;
    llvm::NoFramePointerElimNonLeaf = true;
#ifdef __MINGW32__
#error "only maintaining support for LLVM 3.1 on Windows"
#endif
#elif LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 1
    TargetOptions options = TargetOptions();
    //options.PrintMachineCode = true; //Print machine code produced during JIT compiling
#ifdef JL_DEBUG_BUILD
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
    SmallVector<std::string, 4> MAttrs;
#else
    // Temporarily disable Haswell BMI2 features due to LLVM bug.
    const char *mattr[] = {"-bmi2", "-avx2"
#ifdef V128_BUG
        ,"-avx"
#endif
#if defined(LLVM35) && defined(_OS_WINDOWS_) && !defined(LLVM36)
        ,"-disable-copyprop" // llvm bug 21743
#endif
    };
    SmallVector<std::string, 4> MAttrs(mattr, mattr+2);
#endif
#ifdef LLVM36
    EngineBuilder *eb = new EngineBuilder(std::unique_ptr<Module>(engine_module));
#else
    EngineBuilder *eb = new EngineBuilder(engine_module);
#endif
    std::string ErrorStr;
    eb  ->setEngineKind(EngineKind::JIT)
#if defined(_OS_WINDOWS_) && defined(_CPU_X86_64_) && !defined(USE_MCJIT)
        .setJITMemoryManager(createJITMemoryManagerWin())
#endif
        .setTargetOptions(options)
#if defined(USE_MCJIT) && !defined(LLVM36)
        .setUseMCJIT(true)
#endif
    ;
    Triple TheTriple(sys::getProcessTriple());
#if defined(_OS_WINDOWS_) && defined(USE_MCJIT)
    TheTriple.setObjectFormat(Triple::ELF);
#endif
    jl_TargetMachine = eb->selectTarget(
            TheTriple,
            "",
#if LLVM35
            strcmp(jl_compileropts.cpu_target,"native") ? jl_compileropts.cpu_target : sys::getHostCPUName().data(),
#else
            strcmp(jl_compileropts.cpu_target,"native") ? jl_compileropts.cpu_target : "",
#endif
            MAttrs);
    assert(jl_TargetMachine);
#ifdef LLVM36
    engine_module->setDataLayout(jl_TargetMachine->getSubtargetImpl()->getDataLayout());
#elif defined(LLVM35)
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout());
#else
    engine_module->setDataLayout(jl_TargetMachine->getDataLayout()->getStringRepresentation());
#endif
    jl_ExecutionEngine = eb->create(jl_TargetMachine);
    //ios_printf(ios_stderr,"%s\n",jl_ExecutionEngine->getDataLayout()->getStringRepresentation().c_str());
    if (!jl_ExecutionEngine) {
        JL_PRINTF(JL_STDERR, "Critical error initializing llvm: ", ErrorStr.c_str());
        exit(1);
    }
#ifdef LLVM35
    jl_ExecutionEngine->setProcessAllSections(true);
#endif
#endif // LLVM VERSION
    jl_ExecutionEngine->DisableLazyCompilation();
    mbuilder = new MDBuilder(getGlobalContext());

#ifdef LLVM36
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

    BOX_F(int8,int32);  BOX_F(uint8,uint32);
    BOX_F(int16,int16); BOX_F(uint16,uint16);
    BOX_F(int32,int32); BOX_F(uint32,uint32);
    BOX_F(int64,int64); BOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);

    box8_func  = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int8),
                              "jl_box8", (void*)&jl_box8, m);
    box16_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int16),
                              "jl_box16", (void*)&jl_box16, m);
    box32_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int32),
                              "jl_box32", (void*)&jl_box32, m);
    box64_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int64),
                              "jl_box64", (void*)&jl_box64, m);

    std::vector<Type*> toptrargs(0);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(T_int32);
    toptrargs.push_back(T_int32);
    value_to_pointer_func =
        Function::Create(FunctionType::get(T_pint8, toptrargs, false),
                         Function::ExternalLinkage, "jl_value_to_pointer",
                         m);
    add_named_global(value_to_pointer_func,
                                         (void*)&jl_value_to_pointer);

    temp_arg_area = (char*)malloc(arg_area_sz);
    arg_area_loc = 0;

    std::vector<Type*> noargs(0);
    save_arg_area_loc_func =
        Function::Create(FunctionType::get(T_uint64, noargs, false),
                         Function::ExternalLinkage, "save_arg_area_loc",
                         m);
    add_named_global(save_arg_area_loc_func,
                                         (void*)&save_arg_area_loc);

    restore_arg_area_loc_func =
        Function::Create(ft1arg(T_void, T_uint64),
                         Function::ExternalLinkage, "restore_arg_area_loc",
                         m);
    add_named_global(restore_arg_area_loc_func,
                                         (void*)&restore_arg_area_loc);

    typeToTypeId = jl_alloc_cell_1d(16);
}

/*
maybe this reads the dwarf info for a MachineFunction:

MCContext &mc = Details.MF->getContext()
DenseMap<const MCSection*,MCLineSection*> &secs = mc.getMCLineSectionOrder();
std::vector<const MCSection*> &sec2line = mc.getMCLineSections();
MCLineSection *line = sec2line[secs[0]];
const MCLineEntryCollection *lec = line->getMCLineEntries();
MCLineEntryCollection::iterator it = lec->begin();

addr = (*it).getLabel()->getVariableValue()
line = (*it).getLine()
*/
