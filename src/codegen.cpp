#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Attributes.h"
#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 2 
#include "llvm/DebugInfo.h"
#include "llvm/DIBuilder.h"
#include "llvm/IRBuilder.h"
#define LLVM32
#else
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Analysis/DIBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Support/IRBuilder.h"
#endif
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 1
#include "llvm/Transforms/Vectorize.h"
#endif
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Config/llvm-config.h"
#include <setjmp.h>
#ifdef __WIN32__
#include <malloc.h>
#endif
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <set>
#include <cstdio>
#ifdef DEBUG
#undef NDEBUG
#endif
#include <cassert>
using namespace llvm;

extern "C" {
#include "julia.h"
#include "builtin_proto.h"
void * __stack_chk_guard = NULL;
void __attribute__(()) __stack_chk_fail()
{
    /* put your panic function or similar in here */
    fprintf(stderr, "warning: stack corruption detected\n");
    //assert(0 && "stack corruption detected");
    //abort();
}
}

#define CONDITION_REQUIRES_BOOL

// llvm state
static LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static bool nested_compile=false;
static Module *jl_Module;
static ExecutionEngine *jl_ExecutionEngine;
static DIBuilder *dbuilder;
static std::map<int, std::string> argNumberStrings;
static FunctionPassManager *FPM;

// types
static Type *jl_value_llvmt;
static Type *jl_pvalue_llvmt;
static Type *jl_ppvalue_llvmt;
static FunctionType *jl_func_sig;
static Type *jl_fptr_llvmt;
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

// constants
static Value *V_null;

// global vars
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlnull_var;
static GlobalVariable *jlfloat32temp_var;
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

// important functions
static Function *jlnew_func;
static Function *jlthrow_func;
static Function *jlthrow_line_func;
static Function *jlerror_func;
static Function *jltypeerror_func;
static Function *jlcheckassign_func;
static Function *jldeclareconst_func;
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
#ifdef __WIN32__
static Function *resetstkoflw_func;
#endif

/*
  stuff to fix up:
  - function/var name (un)mangling
  - experiment with llvm optimization passes, option to disable them
  - varargs and ... optimizations

  future:
  - try using fastcc to get tail calls
*/

// --- entry point ---

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
        if (jl_typeis(jl_exception_in_transit, jl_errorexception_type)) {
            char *str = jl_string_data(jl_fieldref(jl_exception_in_transit,0));
            char buf[1024];
            int nc = snprintf(buf, sizeof(buf), "error compiling %s: %s",
                              li->name->name, str);
            jl_value_t *msg = jl_pchar_to_string(buf, nc);
            JL_GC_PUSH(&msg);
            jl_throw(jl_new_struct(jl_errorexception_type, msg));
        }
        jl_rethrow();
    }
    assert(f != NULL);
    nested_compile = last_n_c;
    //f->dump();
    //verifyFunction(*f);
    FPM->run(*f);
    //n_compile++;
    // print out the function's LLVM code
    //ios_printf(ios_stderr, "%s:%d\n",
    //           ((jl_sym_t*)li->file)->name, li->line);
    //f->dump();
    //verifyFunction(*f);
    if (old != NULL) {
        builder.SetInsertPoint(old);
        builder.SetCurrentDebugLocation(olddl);
    }
    JL_SIGATOMIC_END();
    return f;
}

extern "C" void jl_generate_fptr(jl_function_t *f)
{
    // objective: assign li->fptr
    jl_lambda_info_t *li = f->linfo;
    assert(li->functionObject);
    Function *llvmf = (Function*)li->functionObject;
    if (li->fptr == &jl_trampoline) {
        JL_SIGATOMIC_BEGIN();
        li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
        if (li->cFunctionObject != NULL)
            (void)jl_ExecutionEngine->getPointerToFunction((Function*)li->cFunctionObject);
        JL_SIGATOMIC_END();
        llvmf->deleteBody();
        if (li->cFunctionObject != NULL)
            ((Function*)li->cFunctionObject)->deleteBody();
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

extern "C" jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);

extern "C" DLLEXPORT
const jl_value_t *jl_dump_function(jl_function_t *f, jl_tuple_t *types)
{
    if (!jl_is_function(f) || !jl_is_gf(f))
        return jl_cstr_to_string((char*)"");
    jl_function_t *sf = jl_get_specialization(f, types);
    if (sf == NULL || sf->linfo == NULL) {
        sf = jl_method_lookup_by_type(jl_gf_mtable(f), types, 0);
        if (sf == jl_bottom_func)
            return jl_cstr_to_string((char*)"");
        JL_PRINTF(JL_STDERR,
                  "Warning: Returned code may not match what actually runs.\n");
    }
    std::string code;
    llvm::raw_string_ostream stream(code);
    Function *llvmf;
    if (sf->linfo->functionObject == NULL) {
        jl_compile(sf);
    }
    if (sf->fptr == &jl_trampoline) {
        if (sf->linfo->cFunctionObject != NULL)
            llvmf = (Function*)sf->linfo->cFunctionObject;
        else
            llvmf = (Function*)sf->linfo->functionObject;
    }
    else {
        llvmf = to_function(sf->linfo, false);
    }
    llvmf->print(stream);
    return jl_cstr_to_string((char*)stream.str().c_str());
}

extern "C" DLLEXPORT
void *jl_function_ptr(jl_function_t *f, jl_value_t *rt, jl_value_t *argt)
{
    JL_TYPECHK(jl_function_ptr, type, rt);
    JL_TYPECHK(jl_function_ptr, tuple, argt);
    JL_TYPECHK(jl_function_ptr, type, argt);
    if (jl_is_gf(f) && (jl_is_leaf_type(rt) || rt == (jl_value_t*)jl_bottom_type) && jl_is_leaf_type(argt)) {
        jl_function_t *ff = jl_get_specialization(f, (jl_tuple_t*)argt);
        if (ff != NULL && ff->env==(jl_value_t*)jl_null && ff->linfo != NULL) {
            if (ff->linfo->cFunctionObject == NULL) {
                jl_cstyle_compile(ff);
            }
            if (ff->linfo->cFunctionObject != NULL) {
                jl_lambda_info_t *li = ff->linfo;
                jl_value_t *astrt = jl_ast_rettype(li, li->ast);
                if (jl_types_equal((jl_value_t*)li->specTypes, argt) &&
                    (jl_types_equal(astrt, rt) ||
                     (astrt==(jl_value_t*)jl_nothing->type && rt==(jl_value_t*)jl_bottom_type))) {
                    return jl_ExecutionEngine->getPointerToFunction((Function*)ff->linfo->cFunctionObject);
                }
                else {
                    jl_errorf("function_ptr: type signature of %s does not match",
                              li->name->name);
                }
            }
        }
    }
    jl_error("function is not yet c-callable");
    return NULL;
}

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    std::map<std::string, Value*> *vars;
    //std::map<std::string, Value*> *arguments;
    std::map<std::string, Value*> *passedArguments;
    std::map<std::string, int> *closureEnv;
    std::map<std::string, bool> *isAssigned;
    std::map<std::string, bool> *isCaptured;
    std::map<std::string, bool> *escapes;
    std::set<jl_sym_t*> *volatilevars;
    std::map<std::string, jl_value_t*> *declTypes;
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
} jl_codectx_t;

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool boxed=true,
                        bool valuepos=true);
static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx);
static int is_global(jl_sym_t *s, jl_codectx_t *ctx);
static void make_gcroot(Value *v, jl_codectx_t *ctx);
static Value *global_binding_pointer(jl_module_t *m, jl_sym_t *s,
                                     jl_binding_t **pbnd, bool assign);
static Value *emit_checked_var(Value *bp, const char *name, jl_codectx_t *ctx);

// --- utilities ---

#include "cgutils.cpp"
#include "debuginfo.cpp"

// --- code gen for intrinsic functions ---

#include "intrinsics.cpp"

// --- constant determination ---

// try to statically evaluate, NULL if not possible
static jl_value_t *static_eval(jl_value_t *ex, jl_codectx_t *ctx, bool sparams)
{
    if (jl_is_symbolnode(ex))
        ex = (jl_value_t*)jl_symbolnode_sym(ex);
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (is_global(sym, ctx)) {
            size_t i;
            if (sparams) {
                for(i=0; i < jl_tuple_len(ctx->sp); i+=2) {
                    if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                        // static parameter
                        return jl_tupleref(ctx->sp, i+1);
                    }
                }
            }
            if (jl_is_const(ctx->module, sym))
                return jl_get_global(ctx->module, sym);
        }
        return NULL;
    }
    if (jl_is_topnode(ex)) {
        jl_binding_t *b = jl_get_binding(topmod(ctx),
                                         (jl_sym_t*)jl_fieldref(ex,0));
        if (b == NULL) return NULL;
        if (b->constp)
            return b->value;
    }
    if (jl_is_quotenode(ex))
        return jl_fieldref(ex,0);
    if (jl_is_lambda_info(ex))
        return NULL;
    jl_module_t *m = NULL;
    jl_sym_t *s = NULL;
    if (jl_is_getfieldnode(ex)) {
        m = (jl_module_t*)static_eval(jl_fieldref(ex,0),ctx,sparams);
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
            jl_value_t *f = static_eval(jl_exprarg(e,0),ctx,sparams);
            if (f && jl_is_function(f)) {
                jl_fptr_t fptr = ((jl_function_t*)f)->fptr;
                if (fptr == &jl_apply_generic) {
                    if (f == jl_get_global(jl_base_module, jl_symbol("dlsym")) ||
                        f == jl_get_global(jl_base_module, jl_symbol("dlopen")))
                    {
                        size_t i;
                        size_t n = jl_array_dim0(e->args);
                        jl_value_t **v;
                        JL_GC_PUSHARGS(v, n);
                        memset(v, 0, n*sizeof(jl_value_t*));
                        v[0] = f;
                        for (i = 1; i < n; i++) {
                            v[i] = static_eval(jl_exprarg(e,i),ctx,sparams);
                            if (v[i] == NULL) {
                                JL_GC_POP();
                                return NULL;
                            }
                        }
                        jl_value_t *result = jl_apply_generic(f, v+1, (uint32_t)n-1);
                        JL_GC_POP();
                        return result;
                    }
                } else if (jl_array_dim0(e->args) == 3 && fptr == &jl_f_get_field) {
                    m = (jl_module_t*)static_eval(jl_exprarg(e,1),ctx,sparams);
                    s = (jl_sym_t*)static_eval(jl_exprarg(e,2),ctx,sparams);
                    if (m && jl_is_module(m) && s && jl_is_symbol(s)) {
                        jl_binding_t *b = jl_get_binding(m, s);
                        if (b && b->constp)
                            return b->value;
                    }
                } else if (fptr == &jl_f_tuple) {
                    size_t i;
                    size_t n = jl_array_dim0(e->args)-1;
                    if (n==0) return (jl_value_t*)jl_null;
                    jl_value_t **v;
                    JL_GC_PUSHARGS(v, n);
                    memset(v, 0, n*sizeof(jl_value_t*));
                    for (i = 0; i < n; i++) {
                        v[i] = static_eval(jl_exprarg(e,i+1),ctx,sparams);
                        if (v[i] == NULL) {
                            JL_GC_POP();
                            return NULL;
                        }
                    }
                    jl_tuple_t *tup = jl_alloc_tuple_uninit(n);
                    for(i=0; i < n; i++) {
                        jl_tupleset(tup, i, v[i]);
                    }
                    JL_GC_POP();
                    return (jl_value_t*)tup;
                }
            }
        // The next part is probably valid, but it is untested
        //} else if (e->head == tuple_sym) {
        //  size_t i;
        //  for (i = 0; i < jl_array_dim0(e->args); i++) 
        //        if (static_eval(jl_exprarg(e,i), ctx, sparams) == NULL)
        //          return NULL;
        //  return ex;
        }
        return NULL;
    }
    return ex;
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

static std::set<jl_sym_t*> assigned_in_try(jl_array_t *stmts, int s, long l,
                                           int *pend)
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

static std::set<jl_sym_t*> find_volatile_vars(jl_array_t *stmts)
{
    std::set<jl_sym_t*> vv;
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
                            if (local_var_occurs(jl_arrayref(stmts,j), *it)) {
                                vv.insert(*it);
                            }
                        }
                    }
                }
            }
        }
    }
    return vv;
}

// --- gc root counting ---

static bool expr_is_symbol(jl_value_t *e)
{
    return (jl_is_symbol(e) || jl_is_symbolnode(e) || jl_is_topnode(e));
}

// some analysis. determine max needed "evaluation stack" space for
// gc-rooting function arguments.
// also a very simple, conservative escape analysis that is sufficient for
// eliding allocation of varargs tuples.
// "esc" means "in escaping context"
static void max_arg_depth(jl_value_t *expr, int32_t *max, int32_t *sp,
                          bool esc, jl_codectx_t *ctx)
{
    if (jl_is_expr(expr)) {
        esc = true;
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i;
        if (e->head == call_sym || e->head == call1_sym || e->head == new_sym) {
            int alen = jl_array_dim0(e->args);
            int lastsp = *sp;
            jl_value_t *f = jl_exprarg(e,0);
            if (expr_is_symbol(f)) {
                if (is_constant(f, ctx, false)) {
                    jl_value_t *fv =
                        jl_interpret_toplevel_expr_in(ctx->module, f, NULL, 0);
                    if (jl_typeis(fv, jl_intrinsic_type)) {
                        esc = false;
                        JL_I::intrinsic fi = (JL_I::intrinsic)jl_unbox_int32(fv);
                        if (fi != JL_I::ccall) {
                            // here we need space for each argument, but
                            // not for each of their results
                            for(i=1; i < (size_t)alen; i++) {
                                max_arg_depth(jl_exprarg(e,i), max, sp, esc, ctx);
                            }
                            return;
                        }
                        else {
                            esc = true;
                            // 2nd and 3d arguments are static
                            max_arg_depth(jl_exprarg(e,1), max, sp, esc, ctx);
                            for(i=4; i < (size_t)alen; i++) {
                                max_arg_depth(jl_exprarg(e,i), max, sp, esc, ctx);
                            }
                            return;
                        }
                    }
                    else if (jl_is_function(fv)) {
                        jl_function_t *ff = (jl_function_t*)fv;
                        if (ff->fptr == jl_f_tuplelen ||
                            ff->fptr == jl_f_tupleref) {
                            esc = false;
                        }
                    }
                }
            }
            else if (jl_is_expr(f) || jl_is_lambda_info(f)) {
                max_arg_depth(f, max, sp, esc, ctx);
                (*sp)++;
                if (*sp > *max) *max = *sp;
            }

            for(i=1; i < (size_t)alen; i++) {
                max_arg_depth(jl_exprarg(e,i), max, sp, esc, ctx);
                (*sp)++;
                if (*sp > *max) *max = *sp;
            }
            (*sp) = lastsp;
        }
        else if (e->head == method_sym) {
            max_arg_depth(jl_exprarg(e,1), max, sp, esc, ctx);
            (*sp)++;
            if (*sp > *max) *max = *sp;
            max_arg_depth(jl_exprarg(e,2), max, sp, esc, ctx);
            (*sp)++;
            if (*sp > *max) *max = *sp;
            max_arg_depth(jl_exprarg(e,3), max, sp, esc, ctx);
            (*sp)++;
            if (*sp > *max) *max = *sp;
            (*sp)-=2;
        }
        else {
            size_t elen = jl_array_dim0(e->args);
            for(i=0; i < elen; i++) {
                max_arg_depth(jl_exprarg(e,i), max, sp, esc, ctx);
            }
        }
    }
    else if (jl_is_lambda_info(expr)) {
        if (1 > *max) *max = 1;
    }
    else if (jl_is_symbolnode(expr)) {
        expr = (jl_value_t*)jl_symbolnode_sym(expr);
    }
    else if (jl_is_getfieldnode(expr)) {
        if (2 > *max) *max = 2;
    }
    if (jl_is_symbol(expr)) {
        char *vname = ((jl_sym_t*)expr)->name;
        if (ctx->escapes->find(vname) != ctx->escapes->end()) {
            bool did_escape = (*ctx->escapes)[vname];
            (*ctx->escapes)[vname] = did_escape || esc;
        }
    }
}

static void make_gcroot(Value *v, jl_codectx_t *ctx)
{
    Value *froot = builder.CreateGEP(ctx->argTemp,
                                     ConstantInt::get(T_size,
                                                      ctx->argSpaceOffs +
                                                      ctx->argDepth));
    builder.CreateStore(v, froot);
    ctx->argDepth++;
    if (ctx->argDepth > ctx->maxDepth)
        ctx->maxDepth = ctx->argDepth;
}

// --- lambda ---

static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val)
{
    li = li->def;
    if (li->roots == NULL) {
        li->roots = jl_alloc_cell_1d(1);
        jl_cellset(li->roots, 0, val);
    }
    else {
        size_t rlen = jl_array_dim0(li->roots);
        for(size_t i=0; i < rlen; i++) {
            if (jl_arrayref(li->roots,i) == val)
                return;
        }
        jl_cell_1d_push(li->roots, val);
    }
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
    Value *captured[1+clen];
    captured[0] = ConstantInt::get(T_size, clen);
    for(i=0; i < clen; i++) {
        Value *val;
        jl_array_t *vi = (jl_array_t*)jl_cellref(capt, i);
        assert(jl_is_array(vi));
        jl_sym_t *s = (jl_sym_t*)jl_cellref(vi,0);
        assert(jl_is_symbol(s));
        std::map<std::string,int>::iterator it = ctx->closureEnv->find(s->name);
        if (it != ctx->closureEnv->end()) {
            int idx = (*it).second;
            val = emit_nthptr((Value*)ctx->envArg, idx+2);
        }
        else {
            Value *l = (*ctx->vars)[s->name];
            if (l == NULL) {
                val = (*ctx->passedArguments)[s->name];
                if (val->getType() != jl_pvalue_llvmt) {
                    val = boxed(val);
                    make_gcroot(val, ctx);
                }
                assert(val != NULL);
            }
            else {
                val = builder.CreateLoad(l, false);
            }
        }
        captured[i+1] = val;
    }
    Value *env_tuple;
    env_tuple = builder.CreateCall(jlntuple_func,
                                   ArrayRef<Value*>(&captured[0],
                                                    1+clen));
    ctx->argDepth = argStart;
    make_gcroot(env_tuple, ctx);
    Value *result = builder.CreateCall3(jlclosure_func,
                                        Constant::getNullValue(T_pint8),
                                        env_tuple, literal_pointer_val(expr));
    ctx->argDepth--;
    return result;
}

// --- generating function calls ---

static jl_tuple_t *call_arg_types(jl_value_t **args, size_t n, jl_codectx_t *ctx)
{
    jl_tuple_t *t = jl_alloc_tuple(n);
    JL_GC_PUSH(&t);
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

    if (jl_is_module(expr)) {
        Value *bp =
            global_binding_pointer((jl_module_t*)expr, name, NULL, false);
        // todo: use type info to avoid undef check
        return emit_checked_var(bp, name->name, ctx);
    }

    jl_datatype_t *sty = (jl_datatype_t*)expr_type(expr, ctx);
    JL_GC_PUSH(&sty);
    if (jl_is_structtype(sty) && sty != jl_module_type && sty->uid != 0) {
        unsigned idx = jl_field_index(sty, name, 0);
        if (idx != (unsigned)-1) {
            jl_value_t *jfty = jl_tupleref(sty->types, idx);
            Value *strct = emit_expr(expr, ctx);
            if (strct->getType() == jl_pvalue_llvmt) {
                Value *addr =
                    builder.CreateGEP(builder.CreateBitCast(strct, T_pint8),
                                      ConstantInt::get(T_size,
                                                       sty->fields[idx].offset + sizeof(void*)));
                JL_GC_POP();
                if (sty->fields[idx].isptr) {
                    return builder.CreateLoad(builder.CreateBitCast(addr,jl_ppvalue_llvmt));
                }
                else {
                    return typed_load(addr, ConstantInt::get(T_size, 0), jfty, ctx);
                }
            }
            else {
                Value *fldv = builder.
                    CreateExtractValue(strct, ArrayRef<unsigned>(&idx,1));
                if (jfty == (jl_value_t*)jl_bool_type) {
                    fldv = builder.CreateTrunc(fldv, T_int1);
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
    Value *arg1 = emit_expr(expr, ctx);
    make_gcroot(boxed(arg1), ctx);
    Value *arg2 = literal_pointer_val((jl_value_t*)name);
    make_gcroot(arg2, ctx);
    Value *myargs = builder.CreateGEP(ctx->argTemp,
                                      ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
    Value *result = builder.CreateCall3(jlgetfield_func, V_null, myargs,
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
            builder.CreateStore(boxed(rhs),
                                builder.CreateBitCast(addr, jl_ppvalue_llvmt));
        }
        else {
            typed_store(addr, ConstantInt::get(T_size, 0), rhs, jfty, ctx);
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
        (!arg1 || is_constant(arg1, ctx)) &&
        (!arg2 || is_constant(arg2, ctx))) {
        if (jl_tparam0(rt1) == jl_tparam0(rt2))
            return ConstantInt::get(T_int1, 1);
        return ConstantInt::get(T_int1, 0);
    }
    int ptr_comparable = 0;
    if (rt1==(jl_value_t*)jl_sym_type || rt2==(jl_value_t*)jl_sym_type ||
        jl_is_mutable_datatype(rt1) || jl_is_mutable_datatype(rt2))
        ptr_comparable = 1;
    int last_depth = ctx->argDepth;
    if (arg1 && !varg1) {
        varg1 = emit_expr(arg1, ctx);
        if (arg2 && !varg2 && varg1->getType() == jl_pvalue_llvmt) {
            make_gcroot(varg1, ctx);
        }
    }
    Value *answer;
    if (arg2 && !varg2)
        varg2 = emit_expr(arg2, ctx);
    Type *at1 = varg1->getType();
    Type *at2 = varg2->getType();
    if (at1 != jl_pvalue_llvmt && at2 != jl_pvalue_llvmt) {
        if (julia_type_of(varg1) != julia_type_of(varg2)) {
            answer = ConstantInt::get(T_int1, 0);
            goto done;
        }
        if (at1 == at2) {
            if (at1->isIntegerTy() || at1->isPointerTy() ||
                at1->isFloatingPointTy()) {
                answer = builder.CreateICmpEQ(JL_INT(varg1),JL_INT(varg2));
                goto done;
            }
            if (at1->isStructTy() && !ptr_comparable) {
                // TODO: tuples
                jl_datatype_t *sty = (jl_datatype_t*)rt1;
                assert(jl_is_datatype(sty));
                answer = ConstantInt::get(T_int1, 1);
                for(unsigned i=0; i < jl_tuple_len(sty->names); i++) {
                    jl_value_t *fldty = jl_tupleref(sty->types,i);
                    Value *subAns =
                        emit_f_is(fldty, fldty, NULL, NULL,
                                  builder.CreateExtractValue(varg1, ArrayRef<unsigned>(&i,1)),
                                  builder.CreateExtractValue(varg2, ArrayRef<unsigned>(&i,1)),
                                  ctx);
                    answer = builder.CreateAnd(answer, subAns);
                }
                goto done;
            }
        }
    }
    varg1 = boxed(varg1); varg2 = boxed(varg2);
    if (ptr_comparable)
        answer = builder.CreateICmpEQ(varg1, varg2);
    else
        answer = builder.CreateTrunc(builder.CreateCall2(jlegal_func, varg1, varg2), T_int1);
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
    JL_GC_PUSH(&rt1, &rt2, &rt3);
    jl_function_t *f = (jl_function_t*)ff;
    if (f->fptr == &jl_apply_generic) {
        *theFptr = jlapplygeneric_func;
        *theF = f;
        if (ctx->linfo->specTypes != NULL) {
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
            if (jl_is_leaf_type(aty)) {
                if (jl_is_type_type(aty))
                    aty = (jl_value_t*)jl_typeof(jl_tparam0(aty));
                JL_GC_POP();
                return literal_pointer_val(aty);
            }
            Value *arg1 = boxed(emit_expr(args[1], ctx));
            JL_GC_POP();
            return emit_nthptr(arg1, (size_t)0);
        }
    }
    else if (f->fptr == &jl_f_typeassert && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0, 0)) {
                JL_GC_POP();
                return emit_expr(args[1], ctx);
            }
            if (!jl_is_tuple(tp0) && jl_is_leaf_type(tp0)) {
                Value *arg1 = emit_expr(args[1], ctx);
                emit_typecheck(arg1, tp0, "typeassert", ctx);
                JL_GC_POP();
                return arg1;
            }
        }
    }
    else if (f->fptr == &jl_f_isa && nargs==2) {
        jl_value_t *arg = expr_type(args[1], ctx); rt1 = arg;
        jl_value_t *ty  = expr_type(args[2], ctx); rt2 = ty;
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            jl_value_t *tp0 = jl_tparam0(ty);
            if (jl_subtype(arg, tp0, 0)) {
                JL_GC_POP();
                return ConstantInt::get(T_int1,1);
            }
            if (!jl_is_tuple(tp0) && jl_is_leaf_type(tp0) &&
                !jl_is_type_type(tp0)) {
                if (jl_is_leaf_type(arg)) {
                    JL_GC_POP();
                    return ConstantInt::get(T_int1,0);
                }
                Value *arg1 = emit_expr(args[1], ctx);
                JL_GC_POP();
                return builder.CreateICmpEQ(emit_typeof(arg1),
                                            literal_pointer_val(tp0));
            }
        }
    }
    else if (f->fptr == &jl_f_tuplelen && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_tuple(aty)) {
            if (symbol_eq(args[1], ctx->vaName) &&
                !(*ctx->isAssigned)[ctx->vaName->name]) {
                JL_GC_POP();
                return emit_n_varargs(ctx);
            }
            else {
                Value *arg1 = emit_expr(args[1], ctx);
                JL_GC_POP();
                return emit_tuplelen(arg1);
            }
        }
    }
    else if (f->fptr == &jl_f_tupleref && nargs==2) {
        jl_value_t *tty = expr_type(args[1], ctx); rt1 = tty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        if (jl_is_tuple(tty) && ity==(jl_value_t*)jl_long_type) {
            if (ctx->vaStack && symbol_eq(args[1], ctx->vaName)) {
                Value *valen = emit_n_varargs(ctx);
                Value *idx = emit_unbox(T_size, T_psize,
                                        emit_unboxed(args[2], ctx));
                idx = emit_bounds_check(idx, valen, ctx);
                idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
                JL_GC_POP();
                return builder.
                    CreateLoad(builder.CreateGEP(ctx->argArray,idx),false);
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
                    return emit_nthptr(arg1, idx+1);
                }
                if (idx==0 || (!isseqt && idx > tlen)) {
                    builder.CreateCall2(jlthrow_line_func,
                                        builder.CreateLoad(jlboundserr_var),
                                        ConstantInt::get(T_int32, ctx->lineno));
                    JL_GC_POP();
                    return V_null;
                }
            }
            Value *tlen = emit_tuplelen(arg1);
            Value *idx = emit_unbox(T_size, T_psize,
                                    emit_unboxed(args[2], ctx));
            emit_bounds_check(idx, tlen, ctx);
            JL_GC_POP();
            return emit_nthptr(arg1,
                               builder.CreateAdd(idx, ConstantInt::get(T_size,1)));
        }
    }
    else if (f->fptr == &jl_f_tuple) {
        if (nargs == 0) {
            JL_GC_POP();
            return literal_pointer_val((jl_value_t*)jl_null);
        }
        size_t i;
        for(i=0; i < nargs; i++) {
            jl_value_t *it = (jl_value_t*)jl_typeof(args[i+1]);
            if (!(jl_is_immutable_datatype(it) &&
                  it!=(jl_value_t*)jl_quotenode_type && it!=(jl_value_t*)jl_topnode_type))
                break;
        }
        if (i >= nargs) {
            // all arguments immutable; can be statically evaluated
            rt1 = (jl_value_t*)jl_alloc_tuple_uninit(nargs);
            for(i=0; i < nargs; i++) {
                jl_tupleset(rt1, i, args[i+1]);
            }
            jl_add_linfo_root(ctx->linfo, rt1);
            JL_GC_POP();
            return literal_pointer_val(rt1);
        }

        int last_depth = ctx->argDepth;
        // we only get a GC root per-argument, so we can't allocate the
        // tuple before evaluating one argument. so eval the first argument
        // first, then do hand-over-hand to track the tuple.
        Value *arg1 = boxed(emit_expr(args[1], ctx));
        make_gcroot(arg1, ctx);
        Value *tup = 
            builder.CreateCall(jlallocobj_func,
                               ConstantInt::get(T_size,
                                                sizeof(void*)*(nargs+2)));
        builder.CreateStore(arg1, emit_nthptr_addr(tup, 2));
        ctx->argDepth--;
        make_gcroot(tup, ctx);
        builder.CreateStore(literal_pointer_val((jl_value_t*)jl_tuple_type),
                            emit_nthptr_addr(tup, (size_t)0));
        builder.CreateStore(literal_pointer_val((jl_value_t*)nargs),
                            emit_nthptr_addr(tup, (size_t)1));
        for(i=1; i < nargs; i++) {
            builder.CreateStore(V_null,
                                emit_nthptr_addr(tup, i+2));
        }
        for(i=1; i < nargs; i++) {
            builder.CreateStore(boxed(emit_expr(args[i+1], ctx)),
                                emit_nthptr_addr(tup, i+2));
        }
        ctx->argDepth = last_depth;
        JL_GC_POP();
        return tup;
    }
    else if (f->fptr == &jl_f_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx));
        JL_GC_POP();
        builder.CreateCall2(jlthrow_line_func, arg1,
                            ConstantInt::get(T_int32, ctx->lineno));
        return V_null;
    }
    else if (f->fptr == &jl_f_arraylen && nargs==1) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        if (jl_is_array_type(aty)) {
            // todo: also allow e.g. Union of several array types
            Value *arg1 = emit_expr(args[1], ctx);
            JL_GC_POP();
            return emit_arraylen(arg1);
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
                        return emit_arraysize(ary, idx);
                    }
                    else if (idx > ndims) {
                        JL_GC_POP();
                        return ConstantInt::get(T_size, 1);
                    }
                }
                else {
                    Value *idx = emit_unbox(T_size, T_psize,
                                            emit_unboxed(args[2], ctx));
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
                    Value *idx = emit_array_nd_index(ary, nd, &args[2], nargs-1, ctx);
                    JL_GC_POP();
                    if (jl_array_store_unboxed(ety) &&
                        ((jl_datatype_t*)ety)->size == 0) {
                        jl_new_struct_uninit((jl_datatype_t*)ety);
                        return literal_pointer_val(((jl_datatype_t*)ety)->instance);
                    }
                    return typed_load(emit_arrayptr(ary), idx, ety, ctx);
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
                    Value *idx = emit_array_nd_index(ary, nd, &args[3], nargs-2, ctx);
                    if (jl_array_store_unboxed(ety) &&
                        ((jl_datatype_t*)ety)->size == 0) {
                        // no-op
                    }
                    else {
                        typed_store(emit_arrayptr(ary), idx,
                                    ety==(jl_value_t*)jl_any_type ? emit_expr(args[2],ctx) : emit_unboxed(args[2],ctx),
                                    ety, ctx);
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
    }
    else if (f->fptr == &jl_f_instantiate_type && nargs > 0) {
        size_t i;
        for(i=1; i <= nargs; i++) {
            if (!is_constant(args[i], ctx))
                break;
        }
        if (i > nargs) {
            jl_value_t *ty =
                jl_interpret_toplevel_expr_in(ctx->module, expr,
                                              &jl_tupleref(ctx->sp,0),
                                              jl_tuple_len(ctx->sp)/2);
            if (jl_is_leaf_type(ty)) {
                JL_GC_POP();
                return literal_pointer_val(ty);
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
        make_gcroot(boxed(anArg), ctx);
    }

    // call
    Value *myargs;
    if (ctx->argTemp != NULL && nargs > 0) {
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_size, argStart+ctx->argSpaceOffs));
    }
    else {
        myargs = Constant::getNullValue(jl_ppvalue_llvmt);
    }
    Value *result = builder.CreateCall3(theFptr, theF, myargs,
                                        ConstantInt::get(T_int32,nargs));
    ctx->argDepth = argStart;
    return result;
}

static Value *emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx,
                        jl_value_t *expr)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theF=NULL;
    jl_value_t *a0 = args[0];
    jl_value_t *hdtype;
    bool headIsGlobal = false;

    jl_function_t *f = (jl_function_t*)static_eval(a0, ctx, true);
    if (f != NULL) {
        headIsGlobal = true;
        Value *result = emit_known_call((jl_value_t*)f, args, nargs, ctx,
                                        &theFptr, &f, expr);
        if (result != NULL) return result;
    }
    bool specialized = true;
    int last_depth = ctx->argDepth;
    hdtype = expr_type(a0, ctx);
    if (theFptr == NULL) {
        specialized = false;
        Value *theFunc = emit_expr(args[0], ctx);
        if (theFunc->getType() != jl_pvalue_llvmt || jl_is_tuple(hdtype)) {
            // we know it's not a function
            emit_type_error(theFunc, (jl_value_t*)jl_function_type, "apply", ctx);
            ctx->argDepth = last_depth;
            return V_null;
        }
#ifdef JL_GC_MARKSWEEP
        if (!headIsGlobal && (jl_is_expr(a0) || jl_is_lambda_info(a0))) {
            make_gcroot(boxed(theFunc), ctx);
        }
#endif
        if (hdtype!=(jl_value_t*)jl_function_type &&
            hdtype!=(jl_value_t*)jl_datatype_type &&
            !(jl_is_type_type(hdtype) &&
              jl_is_datatype(jl_tparam0(hdtype)))) {
            emit_func_check(theFunc, ctx);
        }
        // extract pieces of the function object
        // TODO: try extractvalue instead
        theFptr = builder.CreateBitCast(emit_nthptr(theFunc, 1), jl_fptr_llvmt);
        theF = theFunc;
    }
    else {
        theF = literal_pointer_val((jl_value_t*)f);
    }

    Value *result;
    if (f!=NULL && specialized && f->linfo!=NULL && f->linfo->cFunctionObject!=NULL) {
        // emit specialized call site
        Value *argvals[nargs];
        Function *cf = (Function*)f->linfo->cFunctionObject;
        FunctionType *cft = cf->getFunctionType();
        for(size_t i=0; i < nargs; i++) {
            Type *at = cft->getParamType(i);
            if (at == jl_pvalue_llvmt) {
                argvals[i] = boxed(emit_expr(args[i+1], ctx));
                if (jl_is_symbol(args[i+1]) || jl_is_symbolnode(args[i+1])) {
                    // avoid root when possible
                }
                else {
                    make_gcroot(argvals[i], ctx);
                }
            }
            else {
                argvals[i] = emit_unbox(at, PointerType::get(at,0),
                                        emit_unboxed(args[i+1], ctx));
            }
        }
        result = builder.CreateCall(cf, ArrayRef<Value*>(&argvals[0],nargs));
        if (result->getType() == T_void) {
            result = literal_pointer_val((jl_value_t*)jl_nothing);
        }
        else {
            result = mark_julia_type(result, jl_ast_rettype(f->linfo, f->linfo->ast));
        }
    }
    else {
        result = emit_jlcall(theFptr, theF, &args[1], nargs, ctx);
    }

    ctx->argDepth = last_depth;
    return result;
}

// --- accessing and assigning variables ---

static bool isBoxed(char *varname, jl_codectx_t *ctx)
{
    return (*ctx->isAssigned)[varname] && (*ctx->isCaptured)[varname];
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
    return literal_pointer_val(&b->value, jl_ppvalue_llvmt);
}

// yields a jl_value_t** giving the binding location of a variable
static Value *var_binding_pointer(jl_sym_t *s, jl_binding_t **pbnd,
                                  bool assign, jl_codectx_t *ctx)
{
    if (jl_is_symbolnode(s))
        s = jl_symbolnode_sym(s);
    assert(jl_is_symbol(s));
    std::map<std::string,int>::iterator it = ctx->closureEnv->find(s->name);
    if (it != ctx->closureEnv->end()) {
        int idx = (*it).second;
        if (isBoxed(s->name, ctx)) {
            return emit_nthptr_addr(emit_nthptr((Value*)ctx->envArg, idx+2), 1);
        }
        return emit_nthptr_addr((Value*)ctx->envArg, idx+2);
    }
    Value *l = (*ctx->vars)[s->name];
    if (l != NULL) {
        if (isBoxed(s->name, ctx)) {
            return emit_nthptr_addr(builder.CreateLoad(l,false), 1);
        }
        return l;
    }
    return global_binding_pointer(ctx->module, s, pbnd, assign);
}

static int is_var_closed(jl_sym_t *s, jl_codectx_t *ctx)
{
    std::map<std::string,int>::iterator it = ctx->closureEnv->find(s->name);
    return (it != ctx->closureEnv->end());
}

static int is_global(jl_sym_t *s, jl_codectx_t *ctx)
{
    std::map<std::string,int>::iterator it = ctx->closureEnv->find(s->name);
    if (it != ctx->closureEnv->end())
        return false;
    return ((*ctx->vars)[s->name]==NULL && (*ctx->passedArguments)[s->name]==NULL);
}

static Value *emit_checked_var(Value *bp, const char *name, jl_codectx_t *ctx)
{
    Value *v = tpropagate(bp, builder.CreateLoad(bp, false));
    Value *ok = builder.CreateICmpNE(v, V_null);
    BasicBlock *err = BasicBlock::Create(getGlobalContext(), "err", ctx->f);
    BasicBlock *ifok = BasicBlock::Create(getGlobalContext(), "ok");
    builder.CreateCondBr(ok, ifok, err);
    builder.SetInsertPoint(err);
    std::string msg;
    msg += std::string(name);
    msg += " not defined";
    emit_error(msg, ctx);
    builder.CreateBr(ifok);
    ctx->f->getBasicBlockList().push_back(ifok);
    builder.SetInsertPoint(ifok);
    return v;
}

static Value *emit_var(jl_sym_t *sym, jl_value_t *ty, jl_codectx_t *ctx,
                       bool isboxed)
{
    // variable
    bool isglobal = is_global(sym, ctx);
    if (isglobal) {
        size_t i;
        // look for static parameter
        for(i=0; i < jl_tuple_len(ctx->sp); i+=2) {
            assert(jl_is_symbol(jl_tupleref(ctx->sp, i)));
            if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                return literal_pointer_val(jl_tupleref(ctx->sp, i+1));
            }
        }
    }
    Value *arg = (*ctx->passedArguments)[sym->name];
    if (arg!=NULL && arg!=V_null && !(*ctx->isAssigned)[sym->name] &&
        (isboxed || (*ctx->vars)[sym->name] == NULL)) {
        // if we need a boxed version of an argument that's not assigned,
        // use the original value.
        return arg;
    }
    jl_binding_t *jbp;
    Value *bp = var_binding_pointer(sym, &jbp, false, ctx);
    if (arg != NULL ||    // arguments are always defined
        (!is_var_closed(sym, ctx) &&
         (isglobal || !jl_subtype((jl_value_t*)jl_undef_type, ty, 0)) &&
         // double-check that a global variable is actually defined. this
         // can be a problem in parallel when a definition is missing on
         // one machine.
         (!isglobal || jbp->value!=NULL))) {
        return tpropagate(bp, builder.CreateLoad(bp, false));
    }
    return emit_checked_var(bp, sym->name, ctx);
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
    if (bnd) {
        builder.CreateCall2(jlcheckassign_func,
                            literal_pointer_val((void*)bnd),
                            boxed(emit_expr(r, ctx, true)));
    }
    else {
        bool vol = ctx->volatilevars->find(s) != ctx->volatilevars->end();
        Type *vt = bp->getType();
        if (vt->isPointerTy() && vt->getContainedType(0)!=jl_pvalue_llvmt)
            builder.CreateStore(emit_unbox(vt->getContainedType(0), vt,
                                           emit_unboxed(r, ctx)),
                                bp, vol);
        else
            builder.CreateStore(boxed(emit_expr(r, ctx, true)), bp, vol);
    }
}

// --- convert expression to code ---

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
        if(valuepos)
            jl_error("Linenode in value position");
        return NULL;
    }
    else if (jl_is_quotenode(expr)) {
        jl_value_t *jv = jl_fieldref(expr,0);
        if (jl_is_bitstype(jl_typeof(jv))) {
            return emit_expr(jv, ctx, isboxed, valuepos);
        }
        assert(jl_is_symbol(jv));
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
        Value *bp = literal_pointer_val(&b->value, jl_ppvalue_llvmt);
        if ((b->constp && b->value!=NULL) ||
            (etype!=(jl_value_t*)jl_any_type &&
             !jl_subtype((jl_value_t*)jl_undef_type, etype, 0))) {
            return builder.CreateLoad(bp, false);
        }
        return emit_checked_var(bp, var->name, ctx);
    }
    if (!jl_is_expr(expr)) {
        // numeric literals
        int needroot = 0;
        if (jl_is_int32(expr)) {
            needroot = !((uint32_t)(jl_unbox_int32(expr)+512) < 1024);
        }
        else if (jl_is_int64(expr)) {
            needroot = !((uint64_t)(jl_unbox_int64(expr)+512) < 1024);
        }
        else if (jl_is_lambda_info(expr)) {
            return emit_lambda_closure(expr, ctx);
        }
        else if (jl_is_tuple(expr) || jl_is_uniontype(expr)) {
            needroot = 1;
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
        Value *condV = emit_unboxed(cond, ctx);
#ifdef CONDITION_REQUIRES_BOOL
        if (expr_type(cond, ctx) != (jl_value_t*)jl_bool_type &&
            condV->getType() != T_int1) {
            emit_typecheck(condV, (jl_value_t*)jl_bool_type, "if", ctx);
        }
#endif
        Value *isfalse;
        if (condV->getType() == T_int1) {
            isfalse = builder.CreateXor(condV, ConstantInt::get(T_int1,1));
        }
        else if (condV->getType() == jl_pvalue_llvmt) {
            isfalse =
                builder.CreateICmpEQ(condV, literal_pointer_val(jl_false));
        }
        else {
            // not a boolean
            isfalse = ConstantInt::get(T_int1,0);
        }
        BasicBlock *ifso = BasicBlock::Create(getGlobalContext(), "if", ctx->f);
        BasicBlock *ifnot = (*ctx->labels)[labelname];
        assert(ifnot);
        builder.CreateCondBr(isfalse, ifnot, ifso);
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
        jl_value_t *mn;
        if (jl_is_symbolnode(args[0])) {
            mn = (jl_value_t*)jl_symbolnode_sym(args[0]);
        }
        else {
            mn = args[0];
        }
        assert(jl_is_symbol(mn));
        int last_depth = ctx->argDepth;
        Value *name = literal_pointer_val(mn);
        jl_binding_t *bnd = NULL;
        Value *bp;
        if (is_global((jl_sym_t*)mn, ctx)) {
            bnd = jl_get_binding_for_method_def(ctx->module, (jl_sym_t*)mn);
            bp = literal_pointer_val(&bnd->value, jl_ppvalue_llvmt);
        }
        else {
            bp = var_binding_pointer((jl_sym_t*)mn, &bnd, false, ctx);
        }
        Value *a1 = emit_expr(args[1], ctx);
        make_gcroot(boxed(a1), ctx);
        Value *a2 = emit_expr(args[2], ctx);
        make_gcroot(boxed(a2), ctx);
        Value *a3 = emit_expr(args[3], ctx);
        make_gcroot(boxed(a3), ctx);
        Value *mdargs[6] = { name, bp, literal_pointer_val((void*)bnd),
                             a1, a2, a3 };
        ctx->argDepth = last_depth;
        return builder.CreateCall(jlmethod_func, ArrayRef<Value*>(&mdargs[0], 6));
    }
    else if (head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_binding_t *bnd = NULL;
        (void)var_binding_pointer(sym, &bnd, true, ctx);
        if (bnd) {
            builder.CreateCall(jldeclareconst_func,
                               literal_pointer_val((void*)bnd));
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
        return literal_pointer_val(extype);
    }
    else if (head == new_sym) {
        jl_value_t *ty = expr_type(args[0], ctx);
        size_t nargs = jl_array_len(ex->args);
        if (jl_is_type_type(ty) &&
            jl_is_datatype(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            ty = jl_tparam0(ty);
            jl_datatype_t *sty = (jl_datatype_t*)ty;
            size_t nf = jl_tuple_len(sty->names);
            if (nf > 0) {
                if (jl_isbits(sty)) {
                    Type *lt = julia_type_to_llvm(ty);
                    Value *strct = UndefValue::get(lt);
                    for(size_t i=0; i < nf; i++) {
                        unsigned idx = i;
                        Type *fty = julia_type_to_llvm(jl_tupleref(sty->types,i));
                        Value *fval = emit_unbox(fty, PointerType::get(fty,0), emit_unboxed(args[i+1],ctx));
                        if (fty == T_int1)
                            fval = builder.CreateZExt(fval, T_int8);
                        strct = builder.
                            CreateInsertValue(strct, fval, ArrayRef<unsigned>(&idx,1));
                    }
                    return mark_julia_type(strct,ty);
                }
                Value *strct =
                    builder.CreateCall(jlallocobj_func,
                                       ConstantInt::get(T_size,
                                                        sizeof(void*)+sty->size));
                builder.CreateStore(literal_pointer_val((jl_value_t*)ty),
                                    emit_nthptr_addr(strct, (size_t)0));
                for(size_t i=0; i < nf; i++) {
                    if (sty->fields[i].isptr) {
                        emit_setfield(sty, strct, i, V_null, ctx, false);
                    }
                }
                make_gcroot(strct, ctx);
                for(size_t i=1; i < nargs; i++) {
                    emit_setfield(sty, strct, i-1, emit_expr(args[i],ctx), ctx,
                                  false);
                }
                ctx->argDepth--;
                return strct;
            }
            else {
                // 0 fields, singleton
                return literal_pointer_val
                    (jl_new_struct_uninit((jl_datatype_t*)ty));
            }
        }
        Value *typ = emit_expr(args[0], ctx);
        return emit_jlcall(jlnew_func, typ, &args[1], nargs-1, ctx);
    }
    else if (head == exc_sym) {
        return builder.CreateLoad(jlexc_var, true);
    }
    else if (head == leave_sym) {
        assert(jl_is_long(args[0]));
        builder.CreateCall(jlleave_func,
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else if (head == enter_sym) {
        assert(jl_is_long(args[0]));
        int labl = jl_unbox_long(args[0]);
        Value *jbuf = builder.CreateGEP((*ctx->handlers)[labl],
                                        ConstantInt::get(T_size,0));
        builder.CreateCall(jlenter_func, jbuf);
#ifndef __WIN32__
        Value *sj = builder.CreateCall2(setjmp_func, jbuf, ConstantInt::get(T_int32,0));
#else
        Value *sj = builder.CreateCall(setjmp_func, jbuf);
#endif
        Value *isz = builder.CreateICmpEQ(sj, ConstantInt::get(T_int32,0));
        BasicBlock *tryblk = BasicBlock::Create(getGlobalContext(), "try",
                                                ctx->f);
        BasicBlock *handlr = (*ctx->labels)[labl];
        assert(handlr);
#ifdef __WIN32__
        BasicBlock *cond_resetstkoflw_blk = BasicBlock::Create(getGlobalContext(), "cond_resetstkoflw", ctx->f);
        BasicBlock *resetstkoflw_blk = BasicBlock::Create(getGlobalContext(), "resetstkoflw", ctx->f);
        builder.CreateCondBr(isz, tryblk, cond_resetstkoflw_blk);
        builder.SetInsertPoint(cond_resetstkoflw_blk);
        builder.CreateCondBr(builder.CreateICmpEQ(
                    literal_pointer_val(jl_stackovf_exception),
                    builder.CreateLoad(jlexc_var, true)),
                resetstkoflw_blk, handlr);
        builder.SetInsertPoint(resetstkoflw_blk);
        builder.CreateCall(resetstkoflw_func);
        builder.CreateBr(handlr);
#else
        builder.CreateCondBr(isz, tryblk, handlr);
#endif
        builder.SetInsertPoint(tryblk);
    }
    else {
        if (!strcmp(head->name, "$"))
            jl_error("syntax: prefix $ in non-quoted expression");
        // some expression types are metadata and can be ignored
        if (valuepos || !(head == line_sym || head == type_goto_sym)) {
            jl_errorf("unsupported or misplaced expression %s in function %s",
                      head->name, ctx->linfo->name->name);
        }
    }
    return NULL;
}

// --- allocating local variables ---

static bool store_unboxed_p(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->linfo->inferred && jl_isbits(jt) &&
            ((jl_datatype_t*)jt)->size > 0 &&
            // don't unbox intrinsics, since inference depends on their having
            // stable addresses for table lookup.
            jt != (jl_value_t*)jl_intrinsic_type && !(*ctx->isCaptured)[name]);
}

static AllocaInst *alloc_local(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    Type *vtype=NULL;
    if (store_unboxed_p(name, ctx))
        vtype = julia_type_to_llvm(jt);
    if (vtype == NULL)
        vtype = jl_pvalue_llvmt;
    AllocaInst *lv = builder.CreateAlloca(vtype, 0, name);
    if (vtype != jl_pvalue_llvmt)
        mark_julia_type(lv, jt);
    (*ctx->vars)[name] = lv;
    return lv;
}

// --- generate function bodies ---

extern char *jl_stack_lo;

extern "C" jl_tuple_t *jl_tuple_tvars_to_symbols(jl_tuple_t *t);

//static int total_roots=0;
//static int n_elim=0;

// generate a julia-callable function that calls f (AKA lam)
static Function *gen_jlcall_wrapper(jl_lambda_info_t *lam, Function *f)
{
    Function *w = Function::Create(jl_func_sig, Function::ExternalLinkage,
                                   f->getName(), jl_Module);
    Function::arg_iterator AI = w->arg_begin();
    AI++; //const Argument &fArg = *AI++;
    Value *argArray = AI++;
    //const Argument &argCount = *AI++;
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", w);

    builder.SetInsertPoint(b0);
    DebugLoc noDbg;
    builder.SetCurrentDebugLocation(noDbg);

    size_t nargs = jl_tuple_len(lam->specTypes);
    Value *args[nargs];
    for(size_t i=0; i < nargs; i++) {
        Value *argPtr = builder.CreateGEP(argArray,
                                          ConstantInt::get(T_size, i));
        Value *theArg = builder.CreateLoad(argPtr, false);
        jl_value_t *ty = jl_tupleref(lam->specTypes, i);
        if (jl_is_leaf_type(ty) && jl_isbits(ty) &&
            ((jl_datatype_t*)ty)->size > 0) {
            Type *lty = julia_type_to_llvm(ty);
            assert(lty != NULL);
            theArg = emit_unbox(lty, PointerType::get(lty,0), theArg);
        }
        args[i] = theArg;
    }
    // TODO: consider pulling the function pointer out of fArg so these
    // wrappers can be reused for different functions of the same type.
    Value *r = builder.CreateCall(f, ArrayRef<Value*>(&args[0], nargs));
    if (r->getType() != jl_pvalue_llvmt) {
        r = boxed(r, jl_ast_rettype(lam, lam->ast));
    }
    builder.CreateRet(r);
    return w;
}

// cstyle = compile with c-callable signature, not jlcall
static Function *emit_function(jl_lambda_info_t *lam, bool cstyle)
{
    // step 1. unpack AST and allocate codegen context for this function
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_tuple_t *sparams = NULL;
    JL_GC_PUSH(&ast, &sparams);
    if (!jl_is_expr(ast)) {
        ast = (jl_expr_t*)jl_uncompress_ast(lam, (jl_value_t*)ast);
    }
    assert(jl_is_expr(ast));
    sparams = jl_tuple_tvars_to_symbols(lam->sparams);
    //JL_PRINTF((jl_value_t*)ast);
    //JL_PRINTF(JL_STDOUT, "\n");
    std::map<std::string, Value*> localVars;
    //std::map<std::string, Value*> argumentMap;
    std::map<std::string, Value*> passedArgumentMap;
    std::map<std::string, int> closureEnv;
    std::map<std::string, bool> isAssigned;
    std::map<std::string, bool> isCaptured;
    std::map<std::string, bool> escapes;
    std::set<jl_sym_t*> volvars;
    std::map<std::string, jl_value_t*> declTypes;
    std::map<int, BasicBlock*> labels;
    std::map<int, Value*> handlers;
    jl_codectx_t ctx;
    ctx.vars = &localVars;
    //ctx.arguments = &argumentMap;
    ctx.passedArguments = &passedArgumentMap;
    ctx.closureEnv = &closureEnv;
    ctx.isAssigned = &isAssigned;
    ctx.isCaptured = &isCaptured;
    ctx.escapes = &escapes;
    ctx.volatilevars = &volvars;
    ctx.declTypes = &declTypes;
    ctx.labels = &labels;
    ctx.handlers = &handlers;
    ctx.module = lam->module;
    ctx.ast = ast;
    ctx.sp = sparams;
    ctx.linfo = lam;
    ctx.funcName = lam->name->name;
    ctx.vaName = NULL;
    ctx.vaStack = false;

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

    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t vinfoslen = jl_array_dim0(vinfos);
    size_t i;
    for(i=0; i < vinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        isAssigned[vname] = (jl_vinfo_assigned(vi)!=0);
        bool iscapt = (jl_vinfo_capt(vi)!=0);
        isCaptured[vname] = iscapt;
        escapes[vname] = iscapt;
        declTypes[vname] = jl_cellref(vi,1);
    }
    vinfos = jl_lam_capt(ast);
    vinfoslen = jl_array_dim0(vinfos);
    bool hasCapt = (vinfoslen > 0);
    for(i=0; i < vinfoslen; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        closureEnv[vname] = i;
        isAssigned[vname] = (jl_vinfo_assigned(vi)!=0);
        isCaptured[vname] = true;
        escapes[vname] = true;
        declTypes[vname] = jl_cellref(vi,1);
    }

    // step 3. determine which vars need to be volatile
    jl_array_t *stmts = jl_lam_body(ast)->args;
    volvars = find_volatile_vars(stmts);

    // step 4. determine function signature
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
                if (jl_isbits(jl_tupleref(lam->specTypes, i))) {
                    specsig = true;
                    break;
                }
            }
            if (jl_tuple_len(lam->specTypes) == 0)
                specsig = true;
        }
    }

    std::string funcName = lam->name->name;
    // try to avoid conflicts in the global symbol table
    funcName = "julia_" + funcName;

    jl_value_t *jlrettype = jl_ast_rettype(lam, (jl_value_t*)ast);
    if (specsig) {
        std::vector<Type*> fsig(0);
        for(size_t i=0; i < jl_tuple_len(lam->specTypes); i++) {
            fsig.push_back(julia_type_to_llvm(jl_tupleref(lam->specTypes,i)));
        }
        Type *rt = (jlrettype == (jl_value_t*)jl_nothing->type ? T_void : julia_type_to_llvm(jlrettype));
        f = Function::Create(FunctionType::get(rt, fsig, false),
                             Function::ExternalLinkage, funcName, jl_Module);
        if (lam->cFunctionObject == NULL) {
            lam->cFunctionObject = (void*)f;
        }
        if (lam->functionObject == NULL) {
            lam->functionObject = (void*)gen_jlcall_wrapper(lam, f);
        }
    }
    else {
        f = Function::Create(jl_func_sig, Function::ExternalLinkage,
                             funcName, jl_Module);
        if (lam->functionObject == NULL) {
            lam->functionObject = (void*)f;
        }
    }
    //TODO: this seems to cause problems, but should be made to work eventually
    //if (jlrettype == (jl_value_t*)jl_bottom_type)
    //    f->setDoesNotReturn();
#ifdef DEBUG
#ifdef __WIN32__
    AttrBuilder *attr = new AttrBuilder();
    attr->addStackAlignmentAttr(16);
    attr->addAlignmentAttr(16);
    f->addAttribute(~0U, Attributes::get(f->getContext(), *attr));
#endif
    f->addFnAttr(Attributes::StackProtectReq);
#endif
    ctx.f = f;

    // step 5. set up debug info context and create first basic block
    jl_value_t *stmt = jl_cellref(stmts,0);
    std::string filename = "no file";
    int lno = -1;
    // look for initial (line num filename) node
    if (jl_is_linenode(stmt)) {
        lno = jl_linenode_line(stmt);
    }
    else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
        lno = jl_unbox_long(jl_exprarg(stmt, 0));
        if (jl_array_dim0(((jl_expr_t*)stmt)->args) > 1) {
            assert(jl_is_symbol(jl_exprarg(stmt, 1)));
            filename = ((jl_sym_t*)jl_exprarg(stmt, 1))->name;
        }
    }
    ctx.lineno = lno;
    
    // TODO: Fix when moving to new LLVM version
    dbuilder->createCompileUnit(0x01, filename, ".", "julia", true, "", 0); 
    llvm::DIArray EltTypeArray = dbuilder->getOrCreateArray(ArrayRef<Value*>());
    DIFile fil = dbuilder->createFile(filename, ".");
    DISubprogram SP =
        dbuilder->createFunction((DIDescriptor)dbuilder->getCU(),
                                 lam->name->name,
                                 lam->name->name,
                                 fil,
                                 0,
                                 dbuilder->createSubroutineType(fil,EltTypeArray),
                                 false, true,
                                 0, true, f);
    //ios_printf(ios_stderr, "\n*** compiling %s at %s:%d\n\n",
    //           lam->name->name, filename.c_str(), lno);
    
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);
    
    // set initial line number
    builder.SetCurrentDebugLocation(DebugLoc::get(lno, 0, (MDNode*)SP, NULL));
    
    Value *fArg=NULL, *argArray=NULL, *argCount=NULL;
    if (specsig) {
    }
    else {
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
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
            alloc_local(argname, &ctx);
            //argumentMap[argname] = lv;
        }
        else if (isAssigned[argname] || (va && i==largslen-1)) {
            n_roots++;
        }
    }
    for(i=0; i < lvarslen; i++) {
        char *varname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (store_unboxed_p(varname, &ctx)) {
            alloc_local(varname, &ctx);
        }
        else {
            n_roots++;
        }
    }

    // fetch env out of function object if we need it
    if (hasCapt) {
        ctx.envArg = emit_nthptr(fArg, 2);
    }

    // step 8. set up GC frame
    int32_t argdepth=0, vsp=0;
    max_arg_depth((jl_value_t*)ast, &argdepth, &vsp, true, &ctx);
    ctx.argSpaceOffs = n_roots;
    n_roots += argdepth;
    //total_roots += n_roots;
    ctx.argDepth = 0;
    ctx.maxDepth = 0;
#ifdef JL_GC_MARKSWEEP
    Instruction *gcframe = NULL;
    Instruction *argSpaceInits = NULL;
    StoreInst *storeFrameSize = NULL;
#endif
    BasicBlock::iterator first_gcframe_inst;
    BasicBlock::iterator last_gcframe_inst;
    if (n_roots > 0) {
#ifdef JL_GC_MARKSWEEP
        // allocate gc frame
        ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                           ConstantInt::get(T_int32,n_roots+2));
        gcframe = (Instruction*)ctx.argTemp;
        first_gcframe_inst = BasicBlock::iterator(gcframe);
        ctx.argTemp = (Instruction*)builder.CreateConstGEP1_32(ctx.argTemp, 2);
        storeFrameSize =
            builder.CreateStore(ConstantInt::get(T_size, n_roots<<1),
                                builder.CreateBitCast(builder.CreateConstGEP1_32(gcframe, 0), T_psize));
        builder.CreateStore(builder.CreateLoad(jlpgcstack_var, false),
                            builder.CreateBitCast(builder.CreateConstGEP1_32(gcframe, 1), PointerType::get(jl_ppvalue_llvmt,0)));
        Instruction *linst=builder.CreateStore(gcframe, jlpgcstack_var, false);
        last_gcframe_inst = BasicBlock::iterator(linst);
        // initialize local variable stack roots to null
        for(i=0; i < (size_t)ctx.argSpaceOffs; i++) {
            Value *varSlot = builder.CreateConstGEP1_32(ctx.argTemp,i);
            builder.CreateStore(V_null, varSlot);
        }
        argSpaceInits = &b0->back();
#else
        ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                           ConstantInt::get(T_int32, n_roots));
#endif
    }
    else {
        ctx.argTemp = NULL;
        //n_elim++;
    }

    // get pointers for locals stored in the gc frame array (argTemp)
    int varnum = 0;
    for(i=0; i < largslen; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
        }
        else if (isAssigned[argname] || (va && i==largslen-1)) {
            Value *av = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            localVars[argname] = av;
            //argumentMap[argname] = av;
        }
    }
    for(i=0; i < lvarslen; i++) {
        char *varname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (store_unboxed_p(varname, &ctx)) {
        }
        else {
            Value *lv = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            localVars[varname] = lv;
        }
    }
    assert(varnum == ctx.argSpaceOffs);

    // step 9. create boxes for boxed locals
    for(i=0; i < lvarslen; i++) {
        char *varname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (isBoxed(varname, &ctx)) {
            Value *lv = localVars[varname];
            builder.CreateStore(builder.CreateCall(jlbox_func, V_null), lv);
        }
    }

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
            emit_error("too few arguments", &ctx);
            builder.CreateBr(mergeBB);
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
            emit_error("wrong number of arguments", &ctx);
            builder.CreateBr(mergeBB);
            f->getBasicBlockList().push_back(mergeBB);
            builder.SetInsertPoint(mergeBB);
        }
    }

    // step 12. move args into local variables
    Function::arg_iterator AI = f->arg_begin();
    for(i=0; i < nreq; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        Value *argPtr;
        if (specsig) {
            argPtr = AI++;
            argPtr = mark_julia_type(argPtr, jl_tupleref(lam->specTypes,i));
        }
        else {
            argPtr = builder.CreateGEP(argArray, ConstantInt::get(T_size, i));
        }

        Value *lv = localVars[argname];
        if (lv == NULL) {
            // if this argument hasn't been given space yet, we've decided
            // to leave it in the input argument array.
            if (specsig)
                passedArgumentMap[argname] = argPtr;
            else
                passedArgumentMap[argname] = builder.CreateLoad(argPtr, false);
            //argumentMap[argname] = argPtr;
        }
        else {
            Value *theArg;
            if (specsig)
                theArg = argPtr;
            else
                theArg = builder.CreateLoad(argPtr, false);
            // keep track of original (boxed) value to avoid re-boxing
            passedArgumentMap[argname] = theArg;
            if (isBoxed(argname, &ctx)) {
                if (specsig) {
                    theArg = boxed(theArg);
                    builder.CreateStore(theArg, lv); // temporarily root
                }
                builder.CreateStore(builder.CreateCall(jlbox_func, theArg), lv);
            }
            else if (dyn_cast<GetElementPtrInst>(lv) != NULL)
                builder.CreateStore(boxed(theArg), lv);
            else
                builder.CreateStore(emit_unbox(dyn_cast<AllocaInst>(lv)->getAllocatedType(),
                                               lv->getType(),
                                               theArg),
                                    lv);
        }
    }

    // step 13. allocate rest argument if necessary
    if (va) {
        if (!escapes[ctx.vaName->name] && !isAssigned[ctx.vaName->name]) {
            ctx.vaStack = true;
        }
        else {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            Value *restTuple =
                builder.CreateCall3(jltuple_func, V_null,
                                    builder.CreateGEP(argArray,
                                                      ConstantInt::get(T_size,nreq)),
                                    builder.CreateSub(argCount,
                                                      ConstantInt::get(T_int32,nreq)));
            char *argname = ctx.vaName->name;
            Value *lv = localVars[argname];
            if (isBoxed(argname, &ctx))
                builder.CreateStore(builder.CreateCall(jlbox_func, restTuple), lv);
            else
                builder.CreateStore(restTuple, lv);
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
    std::vector<Instruction*> gc_frame_pops;
    bool prevlabel = false;
    for(i=0; i < stmtslen; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_linenode(stmt)) {
            int lno = jl_linenode_line(stmt);
            builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP, NULL));
            ctx.lineno = lno;
        }
        else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
            int lno = jl_unbox_long(jl_exprarg(stmt, 0));
            builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP, NULL));
            ctx.lineno = lno;
        }
        if (jl_is_labelnode(stmt)) {
            if (prevlabel) continue;
            prevlabel = true;
        }
        else {
            prevlabel = false;
        }
        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == return_sym) {
            jl_expr_t *ex = (jl_expr_t*)stmt;
            Value *retval;
            Type *retty = f->getReturnType();
            if (retty == jl_pvalue_llvmt) {
                retval = boxed(emit_expr(jl_exprarg(ex,0), &ctx, true));
            }
            else if (retty != T_void) {
                retval = emit_unbox(retty, PointerType::get(retty,0),
                                    emit_unboxed(jl_exprarg(ex,0), &ctx));
            }
            else {
                retval = emit_expr(jl_exprarg(ex,0), &ctx, false);
            }
#ifdef JL_GC_MARKSWEEP
            // JL_GC_POP();
            if (n_roots > 0) {
                Instruction *gcpop = (Instruction*)builder.CreateConstGEP1_32(gcframe, 1);
                gc_frame_pops.push_back(gcpop);
                builder.CreateStore(builder.CreateBitCast(builder.CreateLoad(gcpop, false), jl_ppvalue_llvmt),
                                    jlpgcstack_var);
            }
#endif
            if (retty == T_void)
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
    // sometimes we have dangling labels after the end
    if (builder.GetInsertBlock()->getTerminator() == NULL) {
        builder.CreateUnreachable();
    }

    // step 16. fix up size of stack root list (just a code simplification)
    if (n_roots > 0) {
        if (ctx.argSpaceOffs + ctx.maxDepth == 0) {
            // 0 roots; remove gc frame entirely
            // replace instruction uses with Undef first to avoid LLVM assertion failures
            BasicBlock::iterator bbi = first_gcframe_inst;
            while (1) {
                Instruction &iii = *bbi;
                iii.replaceAllUsesWith(UndefValue::get(iii.getType()));
                if (bbi == last_gcframe_inst) break;
                bbi++;
            }
            for(size_t i=0; i < gc_frame_pops.size(); i++) {
                Instruction *pop = gc_frame_pops[i];
                BasicBlock::iterator pi(pop);
                for(size_t j=0; j < 4; j++) {
                    Instruction &iii = *pi;
                    iii.replaceAllUsesWith(UndefValue::get(iii.getType()));
                    pi++;
                }
            }

            BasicBlock::InstListType &il = gcframe->getParent()->getInstList();
            il.erase(first_gcframe_inst, last_gcframe_inst);
            // erase() erases up *to* the end point; erase last inst too
            il.erase(last_gcframe_inst);
            for(size_t i=0; i < gc_frame_pops.size(); i++) {
                Instruction *pop = gc_frame_pops[i];
                BasicBlock::InstListType &il2 = pop->getParent()->getInstList();
                BasicBlock::iterator pi(pop);
                for(size_t j=0; j < 4; j++) {
                    pi = il2.erase(pi);
                }
            }
        }
        else {
            BasicBlock::iterator bbi(gcframe);
            AllocaInst *newgcframe =
                new AllocaInst(jl_pvalue_llvmt,
                               ConstantInt::get(T_int32, (ctx.argSpaceOffs +
                                                          ctx.maxDepth + 2)));
            ReplaceInstWithInst(ctx.argTemp->getParent()->getInstList(), bbi,
                                newgcframe);
            
            BasicBlock::iterator bbi2(storeFrameSize);
            StoreInst *newFrameSize =
                new StoreInst(ConstantInt::get(T_size, (ctx.argSpaceOffs +
                                                        ctx.maxDepth)<<1),
                              storeFrameSize->getPointerOperand());
            ReplaceInstWithInst(storeFrameSize->getParent()->getInstList(), bbi2,
                                newFrameSize);
            
            BasicBlock::InstListType &instList = argSpaceInits->getParent()->getInstList();
            Instruction *after = argSpaceInits;
            
            for(i=0; i < (size_t)ctx.maxDepth; i++) {
                Instruction *argTempi =
                    GetElementPtrInst::Create(newgcframe,
                                              ConstantInt::get(T_int32, i+ctx.argSpaceOffs+2));
                instList.insertAfter(after, argTempi);
                after = new StoreInst(V_null, argTempi);
                instList.insertAfter(argTempi, after);
            }
        }
    }

    JL_GC_POP();
    return f;
}

// --- initialization ---

static GlobalVariable *global_to_llvm(const std::string &cname, void *addr)
{
    GlobalVariable *gv =
        new GlobalVariable(*jl_Module, jl_pvalue_llvmt,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, cname);
    jl_ExecutionEngine->addGlobalMapping(gv, addr);
    return gv;
}

static Function *jlfunc_to_llvm(const std::string &cname, void *addr)
{
    Function *f =
        Function::Create(jl_func_sig, Function::ExternalLinkage,
                         cname, jl_Module);
    jl_ExecutionEngine->addGlobalMapping(f, addr);
    return f;
}

extern "C" jl_value_t *jl_new_box(jl_value_t *v)
{
    jl_value_t *box = (jl_value_t*)alloc_2w();
    box->type = jl_box_any_type;
    ((jl_value_t**)box)[1] = v;
    return box;
}

static void init_julia_llvm_env(Module *m)
{
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

    // add needed base definitions to our LLVM environment
    StructType *valueSt = StructType::create(getGlobalContext(), "jl_value_t");
    Type *valueStructElts[1] = { PointerType::getUnqual(valueSt) };
    ArrayRef<Type*> vselts(valueStructElts);
    valueSt->setBody(vselts);
    jl_value_llvmt = valueSt;

    jl_pvalue_llvmt = PointerType::get(jl_value_llvmt, 0);
    jl_ppvalue_llvmt = PointerType::get(jl_pvalue_llvmt, 0);
    V_null = Constant::getNullValue(jl_pvalue_llvmt);
    std::vector<Type*> ftargs(0);
    ftargs.push_back(jl_pvalue_llvmt);
    ftargs.push_back(jl_ppvalue_llvmt);
    ftargs.push_back(T_int32);
    jl_func_sig = FunctionType::get(jl_pvalue_llvmt, ftargs, false);
    assert(jl_func_sig != NULL);
    jl_fptr_llvmt = PointerType::get(jl_func_sig, 0);

#ifdef JL_GC_MARKSWEEP
    jlpgcstack_var =
        new GlobalVariable(*jl_Module, jl_ppvalue_llvmt,
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_pgcstack");
    jl_ExecutionEngine->addGlobalMapping(jlpgcstack_var, (void*)&jl_pgcstack);
#endif

    global_to_llvm("__stack_chk_guard", (void*)&__stack_chk_guard);
    Function *jl__stack_chk_fail =
        Function::Create(FunctionType::get(T_void, false),
                         Function::ExternalLinkage,
                         "__stack_chk_fail", jl_Module);
    //jl__stack_chk_fail->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jl__stack_chk_fail, (void*)&__stack_chk_fail);

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
    jlexc_var = global_to_llvm("jl_exception_in_transit",
                               (void*)&jl_exception_in_transit);
    jldiverr_var = global_to_llvm("jl_divbyzero_exception",
                                  (void*)&jl_divbyzero_exception);
    jlundeferr_var = global_to_llvm("jl_undefref_exception",
                                    (void*)&jl_undefref_exception);
    jldomerr_var = global_to_llvm("jl_domain_exception",
                                  (void*)&jl_domain_exception);
    jlovferr_var = global_to_llvm("jl_overflow_exception",
                                  (void*)&jl_overflow_exception);
    jlinexacterr_var = global_to_llvm("jl_inexact_exception",
                                      (void*)&jl_inexact_exception);
    jlboundserr_var = global_to_llvm("jl_bounds_exception",
                                     (void*)&jl_bounds_exception);
    jlfloat32temp_var =
        new GlobalVariable(*jl_Module, T_float32,
                           false, GlobalVariable::PrivateLinkage,
                           ConstantFP::get(T_float32,0.0), "jl_float32_temp");

    std::vector<Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", jl_Module);
    jlerror_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jlerror_func, (void*)&jl_error);

    std::vector<Type*> args1_(0);
    args1_.push_back(jl_pvalue_llvmt);
    jlthrow_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_throw", jl_Module);
    jlthrow_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jlthrow_func, (void*)&jl_throw);

    std::vector<Type*> args2_throw(0);
    args2_throw.push_back(jl_pvalue_llvmt);
    args2_throw.push_back(T_int32);
    jlthrow_line_func =
        (Function*)jl_Module->getOrInsertFunction("jl_throw_with_superfluous_argument",
                                                  FunctionType::get(T_void, args2_throw, false));
    jlthrow_line_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jlthrow_line_func, (void*)&jl_throw_with_superfluous_argument);

    jlnew_func =
        Function::Create(jl_func_sig, Function::ExternalLinkage,
                         "jl_new_structv", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlnew_func, (void*)&jl_new_structv);

    std::vector<Type*> args2(0);
    args2.push_back(T_pint8);
#ifndef __WIN32__
    args2.push_back(T_int32);
#endif
    setjmp_func =
        Function::Create(FunctionType::get(T_int32, args2, false),
                         Function::ExternalLinkage, "sigsetjmp", jl_Module);
        //Intrinsic::getDeclaration(jl_Module, Intrinsic::eh_sjlj_setjmp);
#ifdef LLVM32
    setjmp_func->addFnAttr(Attributes::ReturnsTwice);
#else
    setjmp_func->addFnAttr(Attribute::ReturnsTwice);
#endif
    jl_ExecutionEngine->addGlobalMapping(setjmp_func, (void*)&jl_setjmp_f);

    std::vector<Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pint8);
    te_args.push_back(jl_pvalue_llvmt);
    te_args.push_back(jl_pvalue_llvmt);
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error_rt", jl_Module);
    jltypeerror_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jltypeerror_func,
                                         (void*)&jl_type_error_rt);

    std::vector<Type *> args_2ptrs(0);
    args_2ptrs.push_back(T_pint8);
    args_2ptrs.push_back(jl_pvalue_llvmt);
    jlcheckassign_func =
        Function::Create(FunctionType::get(T_void, args_2ptrs, false),
                         Function::ExternalLinkage,
                         "jl_checked_assignment", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlcheckassign_func,
                                         (void*)&jl_checked_assignment);

    std::vector<Type *> args_1ptr(0);
    args_1ptr.push_back(T_pint8);
    jldeclareconst_func =
        Function::Create(FunctionType::get(T_void, args_1ptr, false),
                         Function::ExternalLinkage,
                         "jl_declare_constant", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jldeclareconst_func,
                                         (void*)&jl_declare_constant);

    jltuple_func = jlfunc_to_llvm("jl_f_tuple", (void*)&jl_f_tuple);
    jlapplygeneric_func =
        jlfunc_to_llvm("jl_apply_generic", (void*)&jl_apply_generic);
    jlgetfield_func = jlfunc_to_llvm("jl_f_get_field", (void*)&jl_f_get_field);

    std::vector<Type*> args3(0);
    args3.push_back(jl_pvalue_llvmt);
    jlbox_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args3, false),
                         Function::ExternalLinkage,
                         "jl_new_box", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlbox_func, (void*)&jl_new_box);

    std::vector<Type*> args4(0);
    args4.push_back(T_pint8);
    args4.push_back(jl_pvalue_llvmt);
    args4.push_back(jl_pvalue_llvmt);
    jlclosure_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args4, false),
                         Function::ExternalLinkage,
                         "jl_new_closure", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlclosure_func,
                                         (void*)&jl_new_closure);

    std::vector<Type*> args5(0);
    args5.push_back(T_size);
    jlntuple_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args5, true),
                         Function::ExternalLinkage,
                         "jl_tuple", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlntuple_func, (void*)&jl_tuple);

    std::vector<Type*> mdargs(0);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_ppvalue_llvmt);
    mdargs.push_back(T_pint8);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    jlmethod_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlmethod_func, (void*)&jl_method_def);

    std::vector<Type*> ehargs(0);
    ehargs.push_back(T_pint8);
    jlenter_func =
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage,
                         "jl_enter_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlenter_func, (void*)&jl_enter_handler);

#ifdef __WIN32__
    resetstkoflw_func = Function::Create(FunctionType::get(T_void, false),
            Function::ExternalLinkage, "_resetstkoflw", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(resetstkoflw_func, (void*)&_resetstkoflw);
#endif

    std::vector<Type*> lhargs(0);
    lhargs.push_back(T_int32);
    jlleave_func =
        Function::Create(FunctionType::get(T_void, lhargs, false),
                         Function::ExternalLinkage,
                         "jl_pop_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlleave_func, (void*)&jl_pop_handler);

    std::vector<Type *> args_2vals(0);
    args_2vals.push_back(jl_pvalue_llvmt);
    args_2vals.push_back(jl_pvalue_llvmt);
    jlegal_func =
        Function::Create(FunctionType::get(T_int32, args_2vals, false),
                         Function::ExternalLinkage,
                         "jl_egal", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlegal_func, (void*)&jl_egal);

    std::vector<Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, aoargs, false),
                         Function::ExternalLinkage,
                         "allocobj", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlallocobj_func, (void*)&allocobj);

    std::vector<Type*> empty_args(0);
    jlalloc2w_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, empty_args, false),
                         Function::ExternalLinkage,
                         "alloc_2w", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc2w_func, (void*)&alloc_2w);

    jlalloc3w_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, empty_args, false),
                         Function::ExternalLinkage,
                         "alloc_3w", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc3w_func, (void*)&alloc_3w);

    // set up optimization passes
    FPM = new FunctionPassManager(jl_Module);
#ifndef LLVM32
    FPM->add(new TargetData(*jl_ExecutionEngine->getTargetData()));
#endif
    // list of passes from vmkit
    FPM->add(createCFGSimplificationPass()); // Clean up disgusting code
    FPM->add(createPromoteMemoryToRegisterPass());// Kill useless allocas
    
    FPM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    FPM->add(createScalarReplAggregatesPass()); // Break up aggregate allocas
    FPM->add(createInstructionCombiningPass()); // Cleanup for scalarrepl.
    FPM->add(createJumpThreadingPass());        // Thread jumps.
    FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
    //FPM->add(createInstructionCombiningPass()); // Combine silly seq's
    
    //FPM->add(createCFGSimplificationPass());    // Merge & remove BBs
    FPM->add(createReassociatePass());          // Reassociate expressions

#if defined(LLVM_VERSION_MAJOR) && LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 1
    //FPM->add(createBBVectorizePass());
#endif
    FPM->add(createEarlyCSEPass()); //// ****

    //FPM->add(createLoopIdiomPass()); //// ****
    FPM->add(createLoopRotatePass());           // Rotate loops.
    FPM->add(createLICMPass());                 // Hoist loop invariants
    FPM->add(createLoopUnswitchPass());         // Unswitch loops.
    FPM->add(createInstructionCombiningPass()); 
    FPM->add(createIndVarSimplifyPass());       // Canonicalize indvars
    //FPM->add(createLoopDeletionPass());         // Delete dead loops
    FPM->add(createLoopUnrollPass());           // Unroll small loops
    //FPM->add(createLoopStrengthReducePass());   // (jwb added)
    
    FPM->add(createInstructionCombiningPass()); // Clean up after the unroller
    FPM->add(createGVNPass());                  // Remove redundancies
    //FPM->add(createMemCpyOptPass());            // Remove memcpy / form memset  
    FPM->add(createSCCPPass());                 // Constant prop with SCCP
    
    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    //FPM->add(createSinkingPass()); ////////////// ****
    //FPM->add(createInstructionSimplifierPass());///////// ****
    FPM->add(createInstructionCombiningPass());
    FPM->add(createJumpThreadingPass());         // Thread jumps
    FPM->add(createDeadStoreEliminationPass());  // Delete dead stores

    FPM->add(createAggressiveDCEPass());         // Delete dead instructions
    FPM->add(createCFGSimplificationPass());     // Merge & remove BBs

    FPM->doInitialization();
}

extern "C" void jl_init_codegen(void)
{
    
    InitializeNativeTarget();
    jl_Module = new Module("julia", jl_LLVMContext);

#if !defined(LLVM_VERSION_MAJOR) || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 0)
    jl_ExecutionEngine = EngineBuilder(jl_Module).setEngineKind(EngineKind::JIT).create();
#ifdef DEBUG
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
#ifdef DEBUG
    options.JITEmitDebugInfo = true;
#endif 
    options.NoFramePointerElim = true;
    options.NoFramePointerElimNonLeaf = true;
#ifdef __MINGW32__
    options.StackAlignmentOverride = 16;
#endif
#ifdef __APPLE__
    options.JITExceptionHandling = 1;
#endif
    jl_ExecutionEngine = EngineBuilder(jl_Module)
        .setEngineKind(EngineKind::JIT)
        .setTargetOptions(options)
        .create();
#endif // LLVM VERSION
    
    dbuilder = new DIBuilder(*jl_Module);

    init_julia_llvm_env(jl_Module);

    jl_jit_events = new JuliaJITEventListener();
    jl_ExecutionEngine->RegisterJITEventListener(jl_jit_events);

    BOX_F(int8,int32);  BOX_F(uint8,uint32);
    BOX_F(int16,int16); BOX_F(uint16,uint16);
    BOX_F(int32,int32); BOX_F(uint32,uint32);
    BOX_F(int64,int64); BOX_F(uint64,uint64);
    BOX_F(float32,float32); BOX_F(float64,float64);
    BOX_F(char,char);

    box8_func  = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int8),
                              "jl_box8", (void*)&jl_box8);
    box16_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int16),
                              "jl_box16", (void*)&jl_box16);
    box32_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int32),
                              "jl_box32", (void*)&jl_box32);
    box64_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int64),
                              "jl_box64", (void*)&jl_box64);

    std::vector<Type*> toptrargs(0);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(T_int32);
    toptrargs.push_back(T_int32);
    value_to_pointer_func =
        Function::Create(FunctionType::get(T_pint8, toptrargs, false),
                         Function::ExternalLinkage, "jl_value_to_pointer",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(value_to_pointer_func,
                                         (void*)&jl_value_to_pointer);

    temp_arg_area = (char*)malloc(arg_area_sz);
    arg_area_loc = 0;

    std::vector<Type*> noargs(0);
    save_arg_area_loc_func =
        Function::Create(FunctionType::get(T_uint64, noargs, false),
                         Function::ExternalLinkage, "save_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(save_arg_area_loc_func,
                                         (void*)&save_arg_area_loc);

    restore_arg_area_loc_func =
        Function::Create(ft1arg(T_void, T_uint64),
                         Function::ExternalLinkage, "restore_arg_area_loc",
                         jl_Module);
    jl_ExecutionEngine->addGlobalMapping(restore_arg_area_loc_func,
                                         (void*)&restore_arg_area_loc);
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
