#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/JITEventListener.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Analysis/DIBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/TargetSelect.h"
#include <setjmp.h>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#ifdef DEBUG
#undef NDEBUG
#endif
#include <cassert>
using namespace llvm;

extern "C" {
#include "julia.h"
#include "builtin_proto.h"
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
#ifdef JL_GC_MARKSWEEP
static Type *T_gcframe;
#endif

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

// important functions
static Function *jlnew_func;
static Function *jlraise_func;
static Function *jlerror_func;
static Function *jluniniterror_func;
static Function *jldiverror_func;
static Function *jltypeerror_func;
static Function *jlcheckassign_func;
static Function *jldeclareconst_func;
static Function *jltuple_func;
static Function *jlntuple_func;
static Function *jlapplygeneric_func;
static Function *jlbox_func;
static Function *jlclosure_func;
static Function *jlmethod_func;
static Function *jlenter_func;
static Function *jlleave_func;
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

/*
  stuff to fix up:
  - function/var name (un)mangling
  - experiment with llvm optimization passes, option to disable them
  - varargs and ... optimizations

  future:
  - try using fastcc to get tail calls
*/

// --- entry point ---

static void emit_function(jl_lambda_info_t *lam, Function *f);
//static int n_compile=0;
static Function *to_function(jl_lambda_info_t *li)
{
    JL_SIGATOMIC_BEGIN();
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage,
                                   li->name->name, jl_Module);
    assert(!li->inInference);
    li->functionObject = (void*)f;
    BasicBlock *old = nested_compile ? builder.GetInsertBlock() : NULL;
    DebugLoc olddl = builder.getCurrentDebugLocation();
    bool last_n_c = nested_compile;
    nested_compile = true;
    emit_function(li, f);
    nested_compile = last_n_c;
    FPM->run(*f);
    //n_compile++;
    // print out the function's LLVM code
    //ios_printf(ios_stderr, "%s:%d\n",
    //           ((jl_sym_t*)li->file)->name, jl_unbox_long(li->line));
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
        JL_SIGATOMIC_END();
        llvmf->deleteBody();
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

extern "C" void jl_delete_function(jl_lambda_info_t *li)
{
    // NOTE: this is not safe; there might be closures using this code.
    Function *llvmf = (Function*)li->functionObject;
    if (llvmf) {
        delete llvmf;
        li->functionObject = NULL;
        li->fptr = &jl_trampoline;
    }
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
    std::map<std::string, jl_value_t*> *declTypes;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *savestates;
    std::map<int, Value*> *jmpbufs;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_tuple_t *sp;
    jl_lambda_info_t *linfo;
    Value *envArg;
    const Argument *argArray;
    const Argument *argCount;
    AllocaInst *argTemp;
    int argDepth;
    //int maxDepth;
    int argSpace;
    std::string funcName;
    jl_sym_t *vaName;  // name of vararg argument
    bool vaStack;      // varargs stack-allocated
    int nReqArgs;
} jl_codectx_t;

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool boxed=true,
                        bool valuepos=true);
static Value *emit_unboxed(jl_value_t *e, jl_codectx_t *ctx);
static int is_global(jl_sym_t *s, jl_codectx_t *ctx);
static void make_gcroot(Value *v, jl_codectx_t *ctx);

// --- utilities ---

#include "cgutils.cpp"
#include "debuginfo.cpp"

// --- code gen for intrinsic functions ---

#include "intrinsics.cpp"

// --- constant determination ---

static bool is_constant(jl_value_t *ex, jl_codectx_t *ctx, bool sparams=true)
{
    if (jl_is_symbolnode(ex))
        ex = (jl_value_t*)jl_symbolnode_sym(ex);
    if (jl_is_symbol(ex)) {
        jl_sym_t *sym = (jl_sym_t*)ex;
        if (is_global(sym, ctx)) {
            size_t i;
            if (sparams) {
                for(i=0; i < ctx->sp->length; i+=2) {
                    if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                        // static parameter
                        return true;
                    }
                }
            }
            if (jl_is_const(ctx->module, sym))
                return true;
        }
        return false;
    }
    if (jl_is_topnode(ex)) {
        jl_binding_t *b = jl_get_binding(ctx->module,
                                         (jl_sym_t*)jl_fieldref(ex,0));
        if (b && b->constp && b->value)
            return true;
    }
    if (jl_is_quotenode(ex))
        return true;
    if (!jl_is_expr(ex) && !jl_is_lambda_info(ex))
        return true;
    return false;
}

static bool symbol_eq(jl_value_t *e, jl_sym_t *sym)
{
    return ((jl_is_symbol(e) && ((jl_sym_t*)e)==sym) ||
            (jl_is_symbolnode(e) && jl_symbolnode_sym(e)==sym) ||
            (jl_is_topnode(e) && ((jl_sym_t*)jl_fieldref(e,0))==sym));
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
        if (e->head == call_sym || e->head == call1_sym) {
            int alen = e->args->length;
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
                            // first 3 arguments are static
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
            for(i=0; i < e->args->length; i++) {
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
    assert(ctx->argDepth < ctx->argSpace);
    Value *froot = builder.CreateGEP(ctx->argTemp,
                                     ConstantInt::get(T_int32,
                                                      ctx->argDepth));
    builder.CreateStore(v, froot);
    ctx->argDepth++;
    //if (ctx->argDepth > ctx->maxDepth)
    //    ctx->maxDepth = ctx->argDepth;
}

// --- lambda ---

extern "C" jl_value_t *jl_uncompress_ast(jl_tuple_t *data);

static void jl_add_linfo_root(jl_lambda_info_t *li, jl_value_t *val)
{
    if (li->roots == NULL) {
        li->roots = jl_alloc_cell_1d(1);
        jl_cellset(li->roots, 0, val);
    }
    else {
        for(size_t i=0; i < li->roots->length; i++) {
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
    jl_array_t *capt = jl_lam_capt((jl_expr_t*)ast);
    if (capt->length == 0) {
        // no captured vars; lift
        jl_value_t *fun =
            (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_null,
                                        (jl_lambda_info_t*)expr);
        jl_add_linfo_root(ctx->linfo, fun);
        return literal_pointer_val(fun);
    }

    Value *captured[1+capt->length];
    captured[0] = ConstantInt::get(T_size, capt->length);
    for(i=0; i < capt->length; i++) {
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
            assert(l != NULL);
            val = builder.CreateLoad(l, false);
        }
        captured[i+1] = val;
    }
    Value *env_tuple;
    env_tuple = builder.CreateCall(jlntuple_func,
                                   ArrayRef<Value*>(&captured[0],
                                                    1+capt->length));
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

extern "C" jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);

static Value *emit_known_call(jl_value_t *ff, jl_value_t **args, size_t nargs,
                              jl_codectx_t *ctx,
                              Value **theFptr, Value **theF,
                              jl_value_t *expr)
{
    if (jl_typeis(ff, jl_intrinsic_type)) {
        return emit_intrinsic((intrinsic)*(uint32_t*)jl_bits_data(ff),
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
        *theF = literal_pointer_val((jl_value_t*)f);
        if (ctx->linfo->specTypes != NULL) {
            jl_tuple_t *aty = call_arg_types(&args[1], nargs, ctx);
            rt1 = (jl_value_t*)aty;
            // attempt compile-time specialization for inferred types
            if (aty != NULL) {
                /*
                  if (trace) {
                      ios_printf(ios_stdout, "call %s%s\n",
                      jl_print_to_string(args[0]),
                      jl_print_to_string((jl_value_t*)aty));
                  }
                */
                f = jl_get_specialization(f, aty);
                if (f != NULL) {
                    assert(f->linfo->functionObject != NULL);
                    *theFptr = (Value*)f->linfo->functionObject;
                    *theF = literal_pointer_val((jl_value_t*)f);
                }
            }
        }
    }
    else if (f->fptr == &jl_f_is && nargs==2) {
        jl_value_t *rt1 = expr_type(args[1], ctx);
        jl_value_t *rt2  = expr_type(args[2], ctx);
        if (jl_is_type_type(rt1) && jl_is_type_type(rt2) &&
            !jl_is_typevar(jl_tparam0(rt1)) &&
            !jl_is_typevar(jl_tparam0(rt2)) &&
            is_constant(args[1], ctx) && is_constant(args[2], ctx)) {
            JL_GC_POP();
            if (jl_tparam0(rt1) == jl_tparam0(rt2))
                return ConstantInt::get(T_int1, 1);
            return ConstantInt::get(T_int1, 0);
        }
        JL_GC_POP();
        Value *arg1 = boxed(emit_expr(args[1], ctx));
        Value *arg2 = boxed(emit_expr(args[2], ctx));
        return builder.CreateICmpEQ(arg1, arg2);
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
                idx = emit_bounds_check(idx, valen,
                                        "tupleref: index out of range", ctx);
                idx = builder.CreateAdd(idx, ConstantInt::get(T_size, ctx->nReqArgs));
                JL_GC_POP();
                return builder.
                    CreateLoad(builder.CreateGEP((Value*)ctx->argArray,idx),false);
            }
            Value *arg1 = emit_expr(args[1], ctx);
            if (jl_is_long(args[2])) {
                size_t tlen = ((jl_tuple_t*)tty)->length;
                int isseqt =
                    tlen>0 && jl_is_seq_type(jl_tupleref(tty, tlen-1));
                size_t idx = jl_unbox_long(args[2]);
                if (idx > 0 && (idx < tlen || (idx == tlen && !isseqt))) {
                    // known to be in bounds
                    JL_GC_POP();
                    return emit_nthptr(arg1, idx+1);
                }
                if (idx==0 || (!isseqt && idx > tlen)) {
                    emit_error("tupleref: index out of range", ctx);
                    JL_GC_POP();
                    return V_null;
                }
            }
            Value *tlen = emit_tuplelen(arg1);
            Value *idx = emit_unbox(T_size, T_psize,
                                    emit_unboxed(args[2], ctx));
            emit_bounds_check(idx, tlen,
                              "tupleref: index out of range", ctx);
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
            if (!jl_is_bits_type(jl_typeof(args[i+1])))
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
        return builder.CreateCall(jlraise_func, arg1);
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
    else if (f->fptr == &jl_f_arrayref && nargs==2) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        if (jl_is_array_type(aty) && ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ety = jl_tparam0(aty);
            if (!jl_is_typevar(ety)) {
                if (!jl_is_bits_type(ety)) {
                    ety = (jl_value_t*)jl_any_type;
                }
                Value *ary = emit_expr(args[1], ctx);
                Type *elty = julia_type_to_llvm(ety, ctx);
                assert(elty != NULL);
                bool isbool=false;
                if (elty==T_int1) { elty = T_int8; isbool=true; }
                Value *data =
                    builder.CreateBitCast(emit_arrayptr(ary),
                                          PointerType::get(elty, 0));
                Value *alen = emit_arraylen(ary);
                Value *idx = emit_unbox(T_size, T_psize,
                                        emit_unboxed(args[2], ctx));
                Value *im1 =
                    emit_bounds_check(idx, alen,
                                      "arrayref: index out of range", ctx);
                Value *elt=builder.CreateLoad(builder.CreateGEP(data, im1),
                                              false);
                if (ety == (jl_value_t*)jl_any_type) {
                    null_pointer_check(elt, ctx);
                }
                JL_GC_POP();
                if (isbool)
                    return builder.CreateTrunc(elt, T_int1);
                return mark_julia_type(elt, ety);
            }
        }
    }
    else if (f->fptr == &jl_f_arrayset && nargs==3) {
        jl_value_t *aty = expr_type(args[1], ctx); rt1 = aty;
        jl_value_t *ity = expr_type(args[2], ctx); rt2 = ity;
        jl_value_t *vty = expr_type(args[3], ctx); rt3 = vty;
        if (jl_is_array_type(aty) &&
            ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ety = jl_tparam0(aty);
            if (!jl_is_typevar(ety) && jl_subtype(vty, ety, 0)) {
                if (!jl_is_bits_type(ety)) {
                    ety = (jl_value_t*)jl_any_type;
                }
                Value *ary = emit_expr(args[1], ctx);
                Type *elty = julia_type_to_llvm(ety, ctx);
                assert(elty != NULL);
                if (elty==T_int1) { elty = T_int8; }
                Value *data =
                    builder.CreateBitCast(emit_arrayptr(ary),
                                          PointerType::get(elty, 0));
                Value *alen = emit_arraylen(ary);
                Value *idx = emit_unbox(T_size, T_psize,
                                        emit_unboxed(args[2], ctx));
                Value *rhs;
                if (jl_is_bits_type(ety)) {
                    rhs = emit_unbox(elty, PointerType::get(elty,0),
                                     emit_unboxed(args[3], ctx));
                }
                else {
                    rhs = boxed(emit_expr(args[3], ctx));
                }
                Value *im1 =
                    emit_bounds_check(idx, alen,
                                      "arrayset: index out of range", ctx);
                builder.CreateStore(rhs, builder.CreateGEP(data, im1));
                JL_GC_POP();
                return ary;
            }
        }
    }
    else if (f->fptr == &jl_f_get_field && nargs==2) {
        jl_struct_type_t *sty = (jl_struct_type_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_struct_type(sty) && jl_is_quotenode(args[2]) &&
            jl_is_symbol(jl_fieldref(args[2],0))) {
            size_t offs = jl_field_offset(sty,
                                          (jl_sym_t*)jl_fieldref(args[2],0));
            if (offs != (size_t)-1) {
                Value *strct = emit_expr(args[1], ctx);
                Value *fld = emit_nthptr(strct, offs+1);
                null_pointer_check(fld, ctx);
                JL_GC_POP();
                return fld;
            }
        }
    }
    else if (f->fptr == &jl_f_set_field && nargs==3) {
        jl_struct_type_t *sty = (jl_struct_type_t*)expr_type(args[1], ctx);
        rt1 = (jl_value_t*)sty;
        if (jl_is_struct_type(sty) && jl_is_quotenode(args[2]) &&
            jl_is_symbol(jl_fieldref(args[2],0))) {
            size_t offs = jl_field_offset(sty,
                                          (jl_sym_t*)jl_fieldref(args[2],0));
            if (offs != (size_t)-1) {
                jl_value_t *ft = jl_tupleref(sty->types, offs);
                jl_value_t *rhst = expr_type(args[3], ctx);
                rt2 = rhst;
                if (jl_subtype(rhst, ft, 0)) {
                    Value *strct = emit_expr(args[1], ctx);
                    Value *rhs = boxed(emit_expr(args[3], ctx));
                    Value *addr = emit_nthptr_addr(strct, offs+1);
                    builder.CreateStore(rhs, addr);
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
                                              ctx->sp->length/2);
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

static Value *emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx,
                        jl_value_t *expr)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theF=NULL;
    jl_binding_t *b=NULL;
    jl_value_t *a0 = args[0];
    jl_value_t *a00 = args[0];
    jl_value_t *hdtype;
    bool headIsGlobal = false;

    if (jl_is_symbolnode(a0)) {
        a0 = (jl_value_t*)jl_symbolnode_sym(a0);
    }
    if (jl_is_symbol(a0) && is_global((jl_sym_t*)a0, ctx) &&
        jl_boundp(ctx->module, (jl_sym_t*)a0)) {
        b = jl_get_binding(ctx->module, (jl_sym_t*)a0);
        if (!b || !b->constp)
            b = NULL;
    }
    if (jl_is_topnode(a0)) {
        headIsGlobal = true;
        // (top x) is also global
        b = jl_get_binding(ctx->module, (jl_sym_t*)jl_fieldref(a0,0));
        if (!b || b->value==NULL || !b->constp)
            b = NULL;
    }
    jl_value_t *f = NULL;
    if (b != NULL) {
        // head is a constant global
        f = b->value;
    }
    else if (jl_is_func(a0)) {
        f = a0;
    }
    if (f != NULL) {
        Value *result = emit_known_call(f, args, nargs, ctx, &theFptr, &theF,
                                        expr);
        if (result != NULL) return result;
    }
    int last_depth = ctx->argDepth;
    hdtype = expr_type(a00, ctx);
    if (theFptr == NULL) {
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
            hdtype!=(jl_value_t*)jl_struct_kind &&
            !(jl_is_type_type(hdtype) &&
              jl_is_struct_type(jl_tparam0(hdtype)))) {
            emit_func_check(theFunc, ctx);
        }
        // extract pieces of the function object
        // TODO: try extractelement instead
        theFptr = builder.CreateBitCast(emit_nthptr(theFunc, 1), jl_fptr_llvmt);
        theF = theFunc;
    }
    // emit arguments
    size_t i;
    int argStart = ctx->argDepth;
    for(i=0; i < nargs; i++) {
        Value *anArg = emit_expr(args[i+1], ctx);
        // put into argument space
        make_gcroot(boxed(anArg), ctx);
    }

    // call
    Value *myargs;
    if (ctx->argTemp != NULL) {
        myargs = builder.CreateGEP(ctx->argTemp,
                                   ConstantInt::get(T_int32, argStart));
    }
    else {
        myargs = Constant::getNullValue(jl_ppvalue_llvmt);
    }
    Value *result = builder.CreateCall3(theFptr, theF, myargs,
                                        ConstantInt::get(T_int32,nargs));

    ctx->argDepth = last_depth;
    return result;
}

// --- accessing and assigning variables ---

static bool isBoxed(char *varname, jl_codectx_t *ctx)
{
    return (*ctx->isAssigned)[varname] && (*ctx->isCaptured)[varname];
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
    jl_binding_t *b=NULL;
    if (!assign)
        b = jl_get_binding(ctx->module, s);
    // if b is NULL, this might be a global that is not set yet but will be,
    // so get a pointer for writing even when not assigning.
    if (assign || b==NULL)
        b = jl_get_binding_wr(ctx->module, s);
    if (pbnd) *pbnd = b;
    return literal_pointer_val(&b->value, jl_ppvalue_llvmt);
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
    return ((*ctx->vars)[s->name] == NULL);
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
        for(i=0; i < ctx->sp->length; i+=2) {
            assert(jl_is_symbol(jl_tupleref(ctx->sp, i)));
            if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                return literal_pointer_val(jl_tupleref(ctx->sp, i+1));
            }
        }
    }
    Value *arg = (*ctx->passedArguments)[sym->name];
    if (arg != NULL && isboxed && !(*ctx->isAssigned)[sym->name]) {
        // if we need a boxed version of an argument that's not assigned,
        // use the original value.
        return arg;
    }
    Value *bp = var_binding_pointer(sym, NULL, false, ctx);
    // arguments are always defined
    if (arg != NULL ||
        (!is_var_closed(sym, ctx) &&
         !jl_subtype((jl_value_t*)jl_undef_type, ty, 0))) {
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
        Type *vt = bp->getType();
        if (vt->isPointerTy() && vt->getContainedType(0)!=jl_pvalue_llvmt)
            builder.CreateStore(emit_unbox(vt->getContainedType(0), vt,
                                           emit_unboxed(r, ctx)),
                                bp);
        else
            builder.CreateStore(boxed(emit_expr(r, ctx, true)), bp);
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
        return NULL;
    }
    else if (jl_is_quotenode(expr)) {
        jl_value_t *jv = jl_fieldref(expr,0);
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
    else if (jl_is_topnode(expr)) {
        jl_sym_t *var = (jl_sym_t*)jl_fieldref(expr,0);
        jl_value_t *etype = expr_type(expr, ctx);
        jl_binding_t *b = jl_get_binding(ctx->module, var);
        if (b == NULL)
            b = jl_get_binding_wr(ctx->module, var);
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
        else if (jl_is_tuple(expr)) {
            needroot = 1;
        }
        if (needroot) {
            jl_add_linfo_root(ctx->linfo, expr);
        }
        return literal_pointer_val(expr);
    }
    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = &jl_cellref(ex->args,0);
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (ex->head == goto_ifnot_sym) {
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

    else if (ex->head == call_sym || ex->head == call1_sym) {
        return emit_call(args, ex->args->length, ctx, (jl_value_t*)ex);
    }

    else if (ex->head == assign_sym) {
        emit_assignment(args[0], args[1], ctx);
        if (valuepos) {
            return literal_pointer_val((jl_value_t*)jl_nothing);
        }
    }
    else if (ex->head == method_sym) {
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
        Value *bp = var_binding_pointer((jl_sym_t*)mn, &bnd, true, ctx);
        Value *a1 = emit_expr(args[1], ctx);
        make_gcroot(boxed(a1), ctx);
        Value *a2 = emit_expr(args[2], ctx);
        make_gcroot(boxed(a2), ctx);
        Value *a3 = emit_expr(args[3], ctx);
        make_gcroot(boxed(a3), ctx);
        Value *mdargs[6] = { name, bp, literal_pointer_val((void*)bnd),
                             a1, a2, a3 };
        builder.CreateCall(jlmethod_func, ArrayRef<Value*>(&mdargs[0], 6));
        ctx->argDepth = last_depth;
        return literal_pointer_val((jl_value_t*)jl_nothing);
    }
    else if (ex->head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        jl_binding_t *bnd = NULL;
        (void)var_binding_pointer(sym, &bnd, true, ctx);
        if (bnd) {
            builder.CreateCall(jldeclareconst_func,
                               literal_pointer_val((void*)bnd));
        }
    }

    else if (ex->head == null_sym) {
        return literal_pointer_val((jl_value_t*)jl_nothing);
    }
    else if (ex->head == static_typeof_sym) {
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
    else if (ex->head == new_sym) {
        jl_value_t *ty = expr_type(args[0], ctx);
        if (jl_is_type_type(ty) &&
            jl_is_struct_type(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            ty = jl_tparam0(ty);
            size_t nf = ((jl_struct_type_t*)ty)->names->length;
            if (nf > 0) {
                Value *strct =
                    builder.CreateCall(jlallocobj_func,
                                       ConstantInt::get(T_size,
                                                        sizeof(void*)*(nf+1)));
                builder.CreateStore(literal_pointer_val((jl_value_t*)ty),
                                    emit_nthptr_addr(strct, (size_t)0));
                for(size_t i=0; i < nf; i++) {
                    builder.CreateStore(V_null,
                                        emit_nthptr_addr(strct, i+1));
                }
                return strct;
            }
            else {
                // 0 fields, singleton
                return literal_pointer_val
                    (jl_new_struct_uninit((jl_struct_type_t*)ty));
            }
        }
        Value *typ = emit_expr(args[0], ctx);
        return builder.CreateCall(jlnew_func, typ);
    }
    else if (ex->head == exc_sym) {
        return builder.CreateLoad(jlexc_var, true);
    }
    else if (ex->head == leave_sym) {
        assert(jl_is_long(args[0]));
        builder.CreateCall(jlleave_func,
                           ConstantInt::get(T_int32, jl_unbox_long(args[0])));
    }
    else if (ex->head == enter_sym) {
        assert(jl_is_long(args[0]));
        int labl = jl_unbox_long(args[0]);
        Value *jbuf = builder.CreateGEP((*ctx->jmpbufs)[labl],
                                        ConstantInt::get(T_int32,0));
        builder.CreateCall2(jlenter_func,
                            builder.CreateGEP((*ctx->savestates)[labl],
                                              ConstantInt::get(T_int32,0)),
                            jbuf);
        Value *sj = builder.CreateCall(setjmp_func, jbuf);
        Value *isz = builder.CreateICmpEQ(sj, ConstantInt::get(T_int32,0));
        BasicBlock *tryblk = BasicBlock::Create(getGlobalContext(), "try",
                                                ctx->f);
        BasicBlock *handlr = (*ctx->labels)[labl];
        assert(handlr);
        builder.CreateCondBr(isz, tryblk, handlr);
        builder.SetInsertPoint(tryblk);
    }
    if (!strcmp(ex->head->name, "$")) {
        jl_error("syntax error: prefix $ outside of quote block");
    }
    if (valuepos) {
        jl_errorf("unsupported expression type %s", ex->head->name);
    }
    return NULL;
}

// --- allocating local variables ---

static bool store_unboxed_p(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->linfo->inferred==jl_true && jl_is_bits_type(jt) &&
            jl_is_leaf_type(jt) &&
            // don't unbox intrinsics, since inference depends on their having
            // stable addresses for table lookup.
            jt != (jl_value_t*)jl_intrinsic_type && !(*ctx->isCaptured)[name]);
}

static AllocaInst *alloc_local(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    Type *vtype=NULL;
    if (store_unboxed_p(name, ctx))
        vtype = julia_type_to_llvm(jt, ctx);
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
//static int used_roots=0;
//static int n_elim=0;

static void emit_function(jl_lambda_info_t *lam, Function *f)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_tuple_t *sparams = NULL;
    JL_GC_PUSH(&ast, &sparams);
    if (jl_is_tuple(ast)) {
        ast = (jl_expr_t*)jl_uncompress_ast((jl_tuple_t*)ast);
    }
    assert(jl_is_expr(ast));
    sparams = jl_tuple_tvars_to_symbols(lam->sparams);
    //jl_print((jl_value_t*)ast);
    //ios_printf(ios_stdout, "\n");
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);
    std::map<std::string, Value*> localVars;
    //std::map<std::string, Value*> argumentMap;
    std::map<std::string, Value*> passedArgumentMap;
    std::map<std::string, int> closureEnv;
    std::map<std::string, bool> isAssigned;
    std::map<std::string, bool> isCaptured;
    std::map<std::string, bool> escapes;
    std::map<std::string, jl_value_t*> declTypes;
    std::map<int, BasicBlock*> labels;
    std::map<int, Value*> savestates;
    std::map<int, Value*> jmpbufs;
    jl_array_t *largs = jl_lam_args(ast);
    jl_array_t *lvars = jl_lam_locals(ast);
    Function::arg_iterator AI = f->arg_begin();
    const Argument &fArg = *AI++;
    const Argument &argArray = *AI++;
    const Argument &argCount = *AI++;
    jl_codectx_t ctx;
    ctx.f = f;
    ctx.vars = &localVars;
    //ctx.arguments = &argumentMap;
    ctx.passedArguments = &passedArgumentMap;
    ctx.closureEnv = &closureEnv;
    ctx.isAssigned = &isAssigned;
    ctx.isCaptured = &isCaptured;
    ctx.escapes = &escapes;
    ctx.declTypes = &declTypes;
    ctx.labels = &labels;
    ctx.savestates = &savestates;
    ctx.jmpbufs = &jmpbufs;
    ctx.module = lam->module;
    ctx.ast = ast;
    ctx.sp = sparams;
    ctx.linfo = lam;
    ctx.argArray = &argArray;
    ctx.argCount = &argCount;
    ctx.funcName = lam->name->name;
    ctx.vaName = NULL;
    ctx.vaStack = false;

    // look for initial (line num filename) node
    jl_array_t *stmts = jl_lam_body(ast)->args;
    jl_value_t *stmt = jl_cellref(stmts,0);
    std::string filename = "no file";
    int lno = -1;
    if (jl_is_linenode(stmt)) {
        lno = jl_linenode_line(stmt);
    }
    else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
        lno = jl_unbox_long(jl_exprarg(stmt, 0));
        if (((jl_expr_t*)stmt)->args->length > 1) {
            assert(jl_is_symbol(jl_exprarg(stmt, 1)));
            filename = ((jl_sym_t*)jl_exprarg(stmt, 1))->name;
        }
    }
    
    dbuilder->createCompileUnit(0, filename, ".", "julia", true, "", 0);
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
    
    // set initial line number
    builder.SetCurrentDebugLocation(DebugLoc::get(lno, 0, (MDNode*)SP, NULL));
    
    /*
    // check for stack overflow (the slower way)
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
    // process var-info lists to see what vars are captured, need boxing
    size_t nreq = largs->length;
    int va = 0;
    if (nreq > 0 && jl_is_rest_arg(jl_cellref(largs,nreq-1))) {
        nreq--;
        va = 1;
        ctx.vaName = jl_decl_var(jl_cellref(largs,nreq));
    }
    ctx.nReqArgs = nreq;

    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t i;
    for(i=0; i < vinfos->length; i++) {
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
    for(i=0; i < vinfos->length; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        closureEnv[vname] = i;
        isAssigned[vname] = (jl_vinfo_assigned(vi)!=0);
        isCaptured[vname] = true;
        escapes[vname] = true;
        declTypes[vname] = jl_cellref(vi,1);
    }

    int n_roots = 0;
    // allocate local variables
    // must be first for the mem2reg pass to work
    for(i=0; i < largs->length; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
            alloc_local(argname, &ctx);
            //argumentMap[argname] = lv;
        }
        else if (isAssigned[argname] || (va && i==largs->length-1)) {
            n_roots++;
        }
    }
    for(i=0; i < lvars->length; i++) {
        char *varname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (store_unboxed_p(varname, &ctx)) {
            alloc_local(varname, &ctx);
        }
        else {
            n_roots++;
        }
    }

    // fetch env out of function object if we need it
    if (vinfos->length > 0) {
        ctx.envArg = emit_nthptr((Value*)&fArg, 2);
    }

    int32_t argdepth=0, vsp=0;
    max_arg_depth((jl_value_t*)ast, &argdepth, &vsp, true, &ctx);
    n_roots += argdepth;
    //total_roots += n_roots;
    ctx.argDepth = 0;
    //ctx.maxDepth = 0;
    ctx.argSpace = argdepth;
#ifdef JL_GC_MARKSWEEP
    AllocaInst *gcframe = NULL;
#endif
    if (n_roots > 0) {
        ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                           ConstantInt::get(T_int32, n_roots));
#ifdef JL_GC_MARKSWEEP
        // create gc frame
        gcframe = builder.CreateAlloca(T_gcframe, 0);
        builder.CreateStore(builder.CreateBitCast(ctx.argTemp,
                                                  PointerType::get(jl_ppvalue_llvmt,0)),
                            builder.CreateConstGEP2_32(gcframe, 0, 0));
        builder.CreateStore(ConstantInt::get(T_size, n_roots),
                            builder.CreateConstGEP2_32(gcframe, 0, 1));
        builder.CreateStore(ConstantInt::get(T_int32, 0),
                            builder.CreateConstGEP2_32(gcframe, 0, 2));
        builder.CreateStore(builder.CreateLoad(jlpgcstack_var, false),
                            builder.CreateConstGEP2_32(gcframe, 0, 3));
        builder.CreateStore(gcframe, jlpgcstack_var, false);
        // initialize stack roots to null
        for(i=0; i < (size_t)n_roots; i++) {
            Value *argTempi = builder.CreateConstGEP1_32(ctx.argTemp,i);
            builder.CreateStore(V_null, argTempi);
        }
#endif
    }
    else {
        ctx.argTemp = NULL;
        //n_elim++;
    }

    // get pointers for locals stored in the gc frame array (argTemp)
    int varnum = argdepth;
    for(i=0; i < largs->length; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
        }
        else if (isAssigned[argname] || (va && i==largs->length-1)) {
            Value *av = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            localVars[argname] = av;
            //argumentMap[argname] = av;
        }
    }
    for(i=0; i < lvars->length; i++) {
        char *argname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
        }
        else {
            Value *lv = builder.CreateConstGEP1_32(ctx.argTemp,varnum);
            varnum++;
            localVars[argname] = lv;
        }
    }
    assert(varnum == n_roots);

    // create boxes for boxed locals
    for(i=0; i < lvars->length; i++) {
        char *argname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (isBoxed(argname, &ctx)) {
            Value *lv = localVars[argname];
            builder.CreateStore(builder.CreateCall(jlbox_func, V_null), lv);
        }
    }

    // allocate space for exception handler contexts
    for(i=0; i < stmts->length; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == enter_sym) {
            int labl = jl_unbox_long(jl_exprarg(stmt,0));
            Value *svst =
                builder.CreateAlloca(T_int8,
                                     ConstantInt::get(T_int32,
                                                      sizeof(jl_savestate_t)));
            Value *jmpb =
                builder.CreateAlloca(T_int8,
                                     ConstantInt::get(T_int32,
                                                      sizeof(jmp_buf)));
            savestates[labl] = svst;
            jmpbufs[labl] = jmpb;
        }
    }

    // check arg count
    if (ctx.linfo->specTypes == NULL) {
        if (va) {
            Value *enough =
                builder.CreateICmpUGE((Value*)&argCount,
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
                builder.CreateICmpEQ((Value*)&argCount,
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

    // move args into local variables
    for(i=0; i < nreq; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        Value *argPtr = builder.CreateGEP((Value*)&argArray,
                                          ConstantInt::get(T_int32, i));
        Value *lv = localVars[argname];
        if (lv == NULL) {
            // if this argument hasn't been given space yet, we've decided
            // to leave it in the input argument array.
            localVars[argname] = argPtr;
            //argumentMap[argname] = argPtr;
        }
        else {
            LoadInst *theArg = builder.CreateLoad(argPtr, false);
            if (!isAssigned[argname]) {
                // keep track of original (boxed) value to avoid re-boxing
                passedArgumentMap[argname] = theArg;
            }
            if (isBoxed(argname, &ctx))
                builder.CreateStore(builder.CreateCall(jlbox_func, theArg), lv);
            else if (dyn_cast<GetElementPtrInst>(lv) != NULL)
                builder.CreateStore(theArg, lv);
            else
                builder.CreateStore(emit_unbox(dyn_cast<AllocaInst>(lv)->getAllocatedType(),
                                               lv->getType(),
                                               theArg),
                                    lv);
        }
    }
    // allocate rest argument if necessary
    if (va) {
        if (!escapes[ctx.vaName->name] && !isAssigned[ctx.vaName->name]) {
            ctx.vaStack = true;
        }
        else {
            // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
            Value *restTuple =
                builder.CreateCall3(jltuple_func, V_null,
                                    builder.CreateGEP((Value*)&argArray,
                                                      ConstantInt::get(T_int32,nreq)),
                                    builder.CreateSub((Value*)&argCount,
                                                      ConstantInt::get(T_int32,nreq)));
            char *argname = ctx.vaName->name;
            Value *lv = localVars[argname];
            if (isBoxed(argname, &ctx))
                builder.CreateStore(builder.CreateCall(jlbox_func, restTuple), lv);
            else
                builder.CreateStore(restTuple, lv);
        }
    }

    // associate labels with basic blocks so forward jumps can be resolved
    BasicBlock *prev=NULL;
    for(i=0; i < stmts->length; i++) {
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
    // compile body statements
    bool prevlabel = false;
    for(i=0; i < stmts->length; i++) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_linenode(stmt)) {
            int lno = jl_linenode_line(stmt);
            builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP,
                                                          NULL));
        }
        else if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == line_sym) {
            int lno = jl_unbox_long(jl_exprarg(stmt, 0));
            builder.SetCurrentDebugLocation(DebugLoc::get(lno, 1, (MDNode*)SP,
                                                          NULL));
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
            Value *retval = boxed(emit_expr(jl_exprarg(ex,0), &ctx, true));
#ifdef JL_GC_MARKSWEEP
            // JL_GC_POP();
            if (n_roots > 0) {
                builder.CreateStore(builder.CreateLoad(builder.CreateConstGEP2_32(gcframe, 0, 3), false),
                                    jlpgcstack_var);
            }
#endif
            builder.CreateRet(retval);
            if (i != stmts->length-1) {
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
        builder.CreateRet(V_null);
    }
    //used_roots += ctx.maxDepth;
    JL_GC_POP();
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
#ifdef __LP64__
    T_size = T_uint64;
#else
    T_size = T_uint32;
#endif
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
    StructType *gcfst = StructType::create(getGlobalContext(), "jl_gcframe_t");
    Type *gcframeStructElts[4] = {
        PointerType::get(jl_ppvalue_llvmt,0),
        T_size,
        T_int32,
        PointerType::getUnqual(gcfst) };
    gcfst->setBody(ArrayRef<Type*>(gcframeStructElts, 4));
    T_gcframe = gcfst;

    jlpgcstack_var =
        new GlobalVariable(*jl_Module, PointerType::get(T_gcframe,0),
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_pgcstack");
    jl_ExecutionEngine->addGlobalMapping(jlpgcstack_var, (void*)&jl_pgcstack);
#endif

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
    jlexc_var = global_to_llvm("jl_exception_in_transit",
                               (void*)&jl_exception_in_transit);
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
    jlraise_func =
        Function::Create(FunctionType::get(T_void, args1_, false),
                         Function::ExternalLinkage,
                         "jl_raise", jl_Module);
    jlraise_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jlraise_func, (void*)&jl_raise);

    jlnew_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args1_, false),
                         Function::ExternalLinkage,
                         "jl_new_struct_uninit", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlnew_func,
                                         (void*)&jl_new_struct_uninit);

    std::vector<Type*> empty_args(0);
    jluniniterror_func =
        Function::Create(FunctionType::get(T_void, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_undef_ref_error", jl_Module);
    jluniniterror_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jluniniterror_func,
                                         (void*)&jl_undef_ref_error);

    jldiverror_func =
        Function::Create(FunctionType::get(T_void, empty_args, false),
                         Function::ExternalLinkage,
                         "jl_divide_by_zero_error", jl_Module);
    jldiverror_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jldiverror_func,
                                         (void*)&jl_divide_by_zero_error);

    setjmp_func =
        Function::Create(FunctionType::get(T_int32, args1, false),
                         Function::ExternalLinkage, "_setjmp", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(setjmp_func, (void*)&_setjmp);

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

    jltuple_func = jlfunc_to_llvm("jl_f_tuple", (void*)*jl_f_tuple);
    jlapplygeneric_func =
        jlfunc_to_llvm("jl_apply_generic", (void*)*jl_apply_generic);

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
    ehargs.push_back(T_pint8);
    jlenter_func =
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage,
                         "jl_enter_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlenter_func, (void*)&jl_enter_handler);

    std::vector<Type*> lhargs(0);
    lhargs.push_back(T_int32);
    jlleave_func =
        Function::Create(FunctionType::get(T_void, lhargs, false),
                         Function::ExternalLinkage,
                         "jl_pop_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlleave_func, (void*)&jl_pop_handler);

    std::vector<Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, aoargs, false),
                         Function::ExternalLinkage,
                         "allocobj", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlallocobj_func, (void*)&allocobj);

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
    FPM->add(new TargetData(*jl_ExecutionEngine->getTargetData()));
    
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
    //FPM->add(createEarlyCSEPass()); //// ****
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
#ifdef DEBUG
    llvm::JITEmitDebugInfo = true;
#endif
    llvm::NoFramePointerElim = true;
    llvm::NoFramePointerElimNonLeaf = true;

    InitializeNativeTarget();
    jl_Module = new Module("julia", jl_LLVMContext);
    jl_ExecutionEngine =
        EngineBuilder(jl_Module).setEngineKind(EngineKind::JIT).create();
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
                              "jl_box8", (void*)*jl_box8);
    box16_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int16),
                              "jl_box16", (void*)*jl_box16);
    box32_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int32),
                              "jl_box32", (void*)*jl_box32);
    box64_func = boxfunc_llvm(ft2arg(jl_pvalue_llvmt, jl_pvalue_llvmt, T_int64),
                              "jl_box64", (void*)*jl_box64);

    std::vector<Type*> toptrargs(0);
    toptrargs.push_back(jl_pvalue_llvmt);
    toptrargs.push_back(jl_pvalue_llvmt);
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
