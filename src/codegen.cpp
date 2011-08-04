#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/DebugInfo.h"
#include "llvm/Analysis/DIBuilder.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/StandardPasses.h"
#include <setjmp.h>
#include <cstdio>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include "debuginfo.cpp"
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
static std::map<const std::string, GlobalVariable*> stringConstants;
static std::map<int, std::string> argNumberStrings;
static FunctionPassManager *FPM;

// types
static const Type *jl_value_llvmt;
static const Type *jl_pvalue_llvmt;
static const Type *jl_ppvalue_llvmt;
static const FunctionType *jl_func_sig;
static const Type *jl_fptr_llvmt;
static const Type *T_int1;
static const Type *T_int8;
static const Type *T_pint8;
static const Type *T_uint8;
static const Type *T_int16;
static const Type *T_pint16;
static const Type *T_uint16;
static const Type *T_int32;
static const Type *T_pint32;
static const Type *T_uint32;
static const Type *T_int64;
static const Type *T_pint64;
static const Type *T_uint64;
static const Type *T_size;
static const Type *T_psize;
static const Type *T_float32;
static const Type *T_pfloat32;
static const Type *T_float64;
static const Type *T_pfloat64;
static const Type *T_void;
#ifdef JL_GC_MARKSWEEP
static const Type *T_gcframe;
#endif

// constants
static Value *V_null;

// global vars
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlnull_var;
static GlobalVariable *jlsysmod_var;
#ifdef JL_GC_MARKSWEEP
static GlobalVariable *jlpgcstack_var;
#endif
static GlobalVariable *jlexc_var;
JuliaJITEventListener *jl_jit_events;

// important functions
static Function *jlnew_func;
static Function *jlraise_func;
static Function *jlerror_func;
static Function *jluniniterror_func;
static Function *jldiverror_func;
static Function *jltypeerror_func;
static Function *jlgetbindingp_func;
static Function *jltuple_func;
static Function *jlntuple_func;
static Function *jlapplygeneric_func;
static Function *jlbox_func;
static Function *jlclosure_func;
static Function *jlmethod_func;
static Function *jlenter_func;
static Function *jlleave_func;
static Function *jlallocobj_func;
static Function *setjmp_func;

/*
  stuff to fix up:
  * gensyms from the front end might conflict with real variables, fix it
  * exceptions

  - source location tracking, var name metadata
  * rootlist to track pointers emitted into code
  - function/var name mangling
  - experiment with llvm optimization passes, option to disable them

  optimizations round 1:
  * constants, especially global. resolve functions statically.
  * keep a table mapping fptr to Function* for compiling known calls
  - manually inline simple builtins (tuple,box,boxset,etc.)
  - dispatch optimizations
  * inline space for buffers
  * preallocate boxes for small integers
  * speed up type caching
  * do something about all the string copying from scheme
  * speed up scheme pattern matcher

  optimizations round 2:
  * type inference
  * mark non-null references and avoid null check
  * static method lookup
  - avoid tuple allocation in (a,b)=(b,a)
  - varargs and ... optimizations

  optimizations round 3:
  * inlining
  * unboxing
  - lambda lifting
  - mark pure (builtin) functions, CSE

  future:
  - try using fastcc to get tail calls
*/

static GlobalVariable *stringConst(const std::string &txt)
{
    GlobalVariable *gv = stringConstants[txt];
    static int strno = 0;
    if (gv == NULL) {
        std::stringstream ssno;
        std::string vname;
        ssno << strno;
        vname += "_j_str";
        vname += ssno.str();
        gv = new GlobalVariable(*jl_Module,
                                ArrayType::get(T_int8, txt.length()+1),
                                true,
                                GlobalVariable::ExternalLinkage,
                                ConstantArray::get(getGlobalContext(),
                                                   txt.c_str()),
                                vname);
        stringConstants[txt] = gv;
        strno++;
    }
    return gv;
}

static void emit_function(jl_lambda_info_t *lam, Function *f);
//static int n_compile=0;
static Function *to_function(jl_lambda_info_t *li)
{
    JL_SIGATOMIC_BEGIN();
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage,
                                   li->name->name, jl_Module);
    assert(jl_is_expr(li->ast));
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
    if (li->fptr == NULL) {
        JL_SIGATOMIC_BEGIN();
        li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
        JL_SIGATOMIC_END();
    }
    assert(li->fptr != NULL);
    f->fptr = li->fptr;
    llvmf->deleteBody();
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

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    std::map<std::string, Value*> *vars;
    std::map<std::string, Value*> *arguments;
    std::map<std::string, int> *closureEnv;
    std::map<std::string, bool> *isAssigned;
    std::map<std::string, bool> *isCaptured;
    std::map<std::string, jl_value_t*> *declTypes;
    std::map<int, BasicBlock*> *labels;
    std::map<int, Value*> *savestates;
    std::map<int, Value*> *jmpbufs;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_tuple_t *sp;
    jl_lambda_info_t *linfo;
    const Argument *envArg;
    const Argument *argArray;
    const Argument *argCount;
    AllocaInst *argTemp;
    AllocaInst *float32Temp; // for forcing rounding
    int argDepth;
    std::string funcName;
} jl_codectx_t;

static bool isBoxed(char *varname, jl_codectx_t *ctx)
{
    return (*ctx->isAssigned)[varname] && (*ctx->isCaptured)[varname];
}

static Value *mark_julia_type(Value *v, jl_value_t *jt);
static jl_value_t *julia_type_of(Value *v);
static Value *tpropagate(Value *a, Value *b);

static Value *literal_pointer_val(void *p, const Type *t)
{
#ifdef __LP64__
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int64, (uint64_t)p),
                                     t);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int32, (uint32_t)p),
                                     t);
#endif
}

static Value *literal_pointer_val(jl_value_t *p)
{
    return literal_pointer_val(p, jl_pvalue_llvmt);
}

static jl_value_t *llvm_type_to_julia(const Type *t, bool err=true);

static Value *emit_typeof(Value *p)
{
    // given p, a jl_value_t*, compute its type tag
    if (p->getType() == jl_pvalue_llvmt) {
        Value *tt = builder.CreateBitCast(p, jl_ppvalue_llvmt);
        tt = builder.
            CreateLoad(builder.CreateGEP(tt,ConstantInt::get(T_int32,0)),
                       false);
        return tt;
    }
    return literal_pointer_val(llvm_type_to_julia(p->getType()));
}

static void emit_error(const std::string &txt, jl_codectx_t *ctx)
{
    std::string txt2 = "in " + ctx->funcName + ": " + txt;
    std::vector<Value *> zeros(0);
    zeros.push_back(ConstantInt::get(T_int32, 0));
    zeros.push_back(ConstantInt::get(T_int32, 0));
    builder.CreateCall(jlerror_func,
                       builder.CreateGEP(stringConst(txt2),
                                         zeros.begin(), zeros.end()));
}

static void error_unless(Value *cond, const std::string &msg, jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    emit_error(msg, ctx);
    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void call_error_func_unless(Value *cond, Function *errfunc,
                                   jl_codectx_t *ctx)
{
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(cond, passBB, failBB);
    builder.SetInsertPoint(failBB);
    builder.CreateCall(errfunc);
    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static void null_pointer_check(Value *v, jl_codectx_t *ctx)
{
    call_error_func_unless(builder.CreateICmpNE(v, V_null),
                           jluniniterror_func, ctx);
}

static Value *boxed(Value *v);

static void emit_type_error(Value *x, jl_value_t *type, const std::string &msg,
                            jl_codectx_t *ctx)
{
    std::vector<Value *> zeros(0);
    zeros.push_back(ConstantInt::get(T_int32, 0));
    zeros.push_back(ConstantInt::get(T_int32, 0));
    Value *fname_val = builder.CreateGEP(stringConst(ctx->funcName),
                                         zeros.begin(), zeros.end());
    Value *msg_val = builder.CreateGEP(stringConst(msg),
                                       zeros.begin(), zeros.end());
    builder.CreateCall4(jltypeerror_func,
                        fname_val, msg_val,
                        literal_pointer_val(type), boxed(x));
}

static void emit_typecheck(Value *x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype =
        builder.CreateICmpEQ(emit_typeof(x), literal_pointer_val(type));
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    emit_type_error(x, type, msg, ctx);

    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static Value *emit_bounds_check(Value *i, Value *len, const std::string &msg,
                                jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_size, 1));
    Value *ok = builder.CreateICmpULT(im1, len);
    error_unless(ok, msg, ctx);
    return im1;
}

static void emit_func_check(Value *x, jl_codectx_t *ctx)
{
    Value *istype1 =
        builder.CreateICmpEQ(emit_typeof(emit_typeof(x)),
                             literal_pointer_val((jl_value_t*)jl_func_kind));
    BasicBlock *elseBB1 = BasicBlock::Create(getGlobalContext(),"a", ctx->f);
    BasicBlock *mergeBB1 = BasicBlock::Create(getGlobalContext(),"b");
    builder.CreateCondBr(istype1, mergeBB1, elseBB1);

    builder.SetInsertPoint(elseBB1);
    Value *istype2 =
        builder.CreateICmpEQ(emit_typeof(x),
                             literal_pointer_val((jl_value_t*)jl_struct_kind));
    BasicBlock *elseBB2 = BasicBlock::Create(getGlobalContext(),"a", ctx->f);
    builder.CreateCondBr(istype2, mergeBB1, elseBB2);

    builder.SetInsertPoint(elseBB2);
    emit_type_error(x, (jl_value_t*)jl_function_type, "apply", ctx);

    builder.CreateBr(mergeBB1);
    ctx->f->getBasicBlockList().push_back(mergeBB1);
    builder.SetInsertPoint(mergeBB1);
}

static Value *emit_nthptr_addr(Value *v, size_t n)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt),
                             ConstantInt::get(T_int32, n));
}

static Value *emit_nthptr_addr(Value *v, Value *idx)
{
    return builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt), idx);
}

static Value *emit_nthptr(Value *v, size_t n)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, n);
    return builder.CreateLoad(vptr, false);
}

static Value *emit_nthptr(Value *v, Value *idx)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr = emit_nthptr_addr(v, idx);
    return builder.CreateLoad(vptr, false);
}

static Value *globalvar_binding_pointer(jl_sym_t *s, jl_codectx_t *ctx)
{
    jl_value_t **bp = jl_get_bindingp(ctx->module, s);
    return literal_pointer_val(bp, jl_ppvalue_llvmt);
    /*
    return builder.CreateCall2(jlgetbindingp_func,
                               literal_pointer_val(ctx->module, T_pint8),
                               literal_pointer_val(s, T_pint8));
    */
}

// yields a jl_value_t** giving the binding location of a variable
static Value *var_binding_pointer(jl_sym_t *s, jl_codectx_t *ctx)
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
    return globalvar_binding_pointer(s, ctx);
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

static Value *julia_bool(Value *cond)
{
    return builder.CreateSelect(cond,
                                literal_pointer_val(jl_true),
                                literal_pointer_val(jl_false));
}

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool value);

static Value *emit_lambda_closure(jl_value_t *expr, jl_codectx_t *ctx)
{
    assert(jl_is_lambda_info(expr));
    size_t i;
    jl_array_t *capt = jl_lam_capt((jl_expr_t*)((jl_lambda_info_t*)expr)->ast);
    std::vector<Value *> captured(0);
    captured.push_back(ConstantInt::get(T_size, capt->length));
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
        captured.push_back(val);
    }
    Value *tuple;
    if (capt->length == 0) {
        tuple = literal_pointer_val((jl_value_t*)jl_null);
    }
    else {
        tuple = builder.CreateCall(jlntuple_func,
                                   captured.begin(), captured.end());
    }
    return builder.CreateCall2(jlclosure_func,
                               literal_pointer_val(expr), tuple);
}

static bool expr_is_symbol(jl_value_t *e)
{
    return (jl_is_symbol(e) || jl_is_symbolnode(e) ||
            (jl_is_expr(e) && ((jl_expr_t*)e)->head==top_sym));
}

static size_t max_arg_depth(jl_value_t *expr)
{
    if (jl_is_expr(expr)) {
        int max = 0, m;
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i;
        if (e->head == call_sym || e->head == call1_sym) {
            int alen = e->args->length;
            for(i=0; i < (size_t)alen; i++) {
                m = max_arg_depth(jl_exprarg(e,i));
                if (m+(int)i > max) max = m+(int)i;
            }
            if (!expr_is_symbol(jl_exprarg(e,0)) &&
                (jl_is_expr(jl_exprarg(e,0)) ||
                 jl_is_lambda_info(jl_exprarg(e,0)))) {
                if (alen > max)
                    max = alen;
            }
        }
        else if (e->head == method_sym) {
            m = max_arg_depth(jl_exprarg(e,1));
            if (m > max) max = m;
            m = max_arg_depth(jl_exprarg(e,2));
            if (m+1 > max) max = m+1;
            if (2 > max) max = 2;
        }
        else {
            for(i=0; i < e->args->length; i++) {
                m = max_arg_depth(jl_exprarg(e,i));
                if (m > max) max = m;
            }
        }
        assert(max >= 0);
        return (size_t)max;
    }
    return 0;
}

static void make_gcroot(Value *v, jl_codectx_t *ctx)
{
    Value *froot = builder.CreateGEP(ctx->argTemp,
                                     ConstantInt::get(T_int32,
                                                      ctx->argDepth));
    builder.CreateStore(v, froot);
    ctx->argDepth++;
}

extern "C" jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);

static jl_value_t *expr_type(jl_value_t *e)
{
    if (jl_is_expr(e))
        return ((jl_expr_t*)e)->etype;
    if (jl_is_symbolnode(e))
        return jl_symbolnode_type(e);
    if (jl_is_symbol(e))
        return (jl_value_t*)jl_any_type;
    if (jl_is_lambda_info(e))
        return (jl_value_t*)jl_any_func;
    if (jl_is_some_tag_type(e))
        return (jl_value_t*)jl_wrap_Type(e);
    return (jl_value_t*)jl_typeof(e);
}

#include "intrinsics.cpp"

static jl_tuple_t *call_arg_types(jl_value_t **args, size_t n)
{
    jl_tuple_t *t = jl_alloc_tuple(n);
    JL_GC_PUSH(&t);
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *ty = expr_type(args[i]);
        if (!jl_is_leaf_type(ty)) {
            t = NULL;
            break;
        }
        jl_tupleset(t, i, ty);
    }
    JL_GC_POP();
    return t;
}

static Value *emit_tuplelen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 1);
#ifdef __LP64__
    return builder.CreatePtrToInt(lenbits, T_int64);
#else
    return builder.CreatePtrToInt(lenbits, T_int32);
#endif
}

static Value *emit_arraysize(Value *t, Value *dim)
{
    int o;
#ifdef __LP64__
    o = 3;
#else
    o = 4;
#endif
    Value *dbits =
        emit_nthptr(t, builder.CreateAdd(dim,
                                         ConstantInt::get(dim->getType(), o)));
#ifdef __LP64__
    return builder.CreatePtrToInt(dbits, T_int64);
#else
    return builder.CreatePtrToInt(dbits, T_int32);
#endif
}

static Value *emit_arraysize(Value *t, int dim)
{
    return emit_arraysize(t, ConstantInt::get(T_int32, dim));
}

static Value *emit_arraylen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 2);
#ifdef __LP64__
    return builder.CreatePtrToInt(lenbits, T_int64);
#else
    return builder.CreatePtrToInt(lenbits, T_int32);
#endif
}

static Value *emit_arrayptr(Value *t)
{
    return emit_nthptr(t, 1);
}

static Value *emit_known_call(jl_value_t *ff, jl_value_t **args, size_t nargs,
                              jl_codectx_t *ctx,
                              Value **theFptr, Value **theEnv)
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
        //*theFptr = literal_pointer_val((void*)f->fptr, jl_fptr_llvmt);
        *theEnv = literal_pointer_val(f->env);
        if (ctx->linfo->specTypes != NULL) {
            jl_tuple_t *aty = call_arg_types(&args[1], nargs);
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
                    if (f->fptr == &jl_trampoline)
                        *theEnv = literal_pointer_val(jl_t1(f->env));
                    else
                        *theEnv = literal_pointer_val(f->env);
                }
            }
        }
    }
    else if (f->fptr == &jl_f_is && nargs==2) {
        JL_GC_POP();
        Value *arg1 = boxed(emit_expr(args[1], ctx, true));
        Value *arg2 = boxed(emit_expr(args[2], ctx, true));
        return builder.CreateICmpEQ(arg1, arg2);
    }
    else if (f->fptr == &jl_f_tuplelen && nargs==1) {
        jl_value_t *aty = expr_type(args[1]); rt1 = aty;
        if (jl_is_tuple(aty)) {
            Value *arg1 = emit_expr(args[1], ctx, true);
            JL_GC_POP();
            return emit_tuplelen(arg1);
        }
    }
    else if (f->fptr == &jl_f_tupleref && nargs==2) {
        jl_value_t *tty = expr_type(args[1]); rt1 = tty;
        jl_value_t *ity = expr_type(args[2]); rt2 = ity;
        if (jl_is_tuple(tty) && ity==(jl_value_t*)jl_long_type) {
            Value *arg1 = emit_expr(args[1], ctx, true);
            if (jl_is_long(args[2])) {
                uint32_t idx = (uint32_t)jl_unbox_long(args[2]);
                if (idx > 0 &&
                    (idx < ((jl_tuple_t*)tty)->length ||
                     (idx == ((jl_tuple_t*)tty)->length &&
                      !jl_is_seq_type(jl_tupleref(tty,
                                                  ((jl_tuple_t*)tty)->length-1))))) {
                    // known to be in bounds
                    JL_GC_POP();
                    return emit_nthptr(arg1, idx+1);
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
        int last_depth = ctx->argDepth;
        Value *tup =
            builder.CreateBitCast
            (builder.CreateCall(jlallocobj_func,
                                ConstantInt::get(T_size,
                                                 sizeof(void*)*(nargs+2))),
             jl_pvalue_llvmt);
        make_gcroot(tup, ctx);
        builder.CreateStore(literal_pointer_val((jl_value_t*)jl_tuple_type),
                            emit_nthptr_addr(tup, (size_t)0));
        builder.CreateStore(literal_pointer_val((jl_value_t*)nargs),
                            emit_nthptr_addr(tup, (size_t)1));
        for(size_t i=0; i < nargs; i++) {
            builder.CreateStore(V_null,
                                emit_nthptr_addr(tup, i+2));
        }
        for(size_t i=0; i < nargs; i++) {
            builder.CreateStore(boxed(emit_expr(args[i+1], ctx, true)),
                                emit_nthptr_addr(tup, i+2));
        }
        ctx->argDepth = last_depth;
        JL_GC_POP();
        return tup;
    }
    else if (f->fptr == &jl_f_throw && nargs==1) {
        Value *arg1 = boxed(emit_expr(args[1], ctx, true));
        JL_GC_POP();
        return builder.CreateCall(jlraise_func, arg1);
    }
    else if (f->fptr == &jl_f_arraylen && nargs==1) {
        jl_value_t *aty = expr_type(args[1]); rt1 = aty;
        if (jl_is_array_type(aty)) {
            Value *arg1 = emit_expr(args[1], ctx, true);
            JL_GC_POP();
            return emit_arraylen(arg1);
        }
    }
    else if (f->fptr == &jl_f_arraysize && nargs==2) {
        jl_value_t *aty = expr_type(args[1]); rt1 = aty;
        jl_value_t *ity = expr_type(args[2]); rt2 = ity;
        if (jl_is_array_type(aty) && ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ndp = jl_tparam1(aty);
            if (jl_is_long(ndp)) {
                Value *ary = emit_expr(args[1], ctx, true);
                size_t ndims = jl_unbox_long(ndp);
                if (jl_is_long(args[2])) {
                    uint32_t idx = (uint32_t)jl_unbox_long(args[2]);
                    if (idx > 0 && idx <= ndims) {
                        JL_GC_POP();
                        return emit_arraysize(ary, idx);
                    }
                }
                else {
                    Value *idx = emit_unbox(T_size, T_psize,
                                            emit_unboxed(args[2], ctx));
                    emit_bounds_check(idx, ConstantInt::get(T_size,ndims),
                                      "arraysize: dimension out of range",
                                      ctx);
                    JL_GC_POP();
                    return emit_arraysize(ary, idx);
                }
            }
        }
    }
    else if (f->fptr == &jl_f_arrayref && nargs==2) {
        jl_value_t *aty = expr_type(args[1]); rt1 = aty;
        jl_value_t *ity = expr_type(args[2]); rt2 = ity;
        if (jl_is_array_type(aty) && ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ety = jl_tparam0(aty);
            //if (jl_is_bits_type(ety)) {
            if (!jl_is_typevar(ety)) {
                if (!jl_is_bits_type(ety)) {
                    ety = (jl_value_t*)jl_any_type;
                }
                Value *ary = emit_expr(args[1], ctx, true);
                const Type *elty = julia_type_to_llvm(ety, ctx);
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
        jl_value_t *aty = expr_type(args[1]); rt1 = aty;
        jl_value_t *ity = expr_type(args[2]); rt2 = ity;
        jl_value_t *vty = expr_type(args[3]); rt3 = vty;
        if (jl_is_array_type(aty) &&
            ity == (jl_value_t*)jl_long_type) {
            jl_value_t *ety = jl_tparam0(aty);
            //if (jl_is_bits_type(ety) && jl_subtype(vty, ety, 0)) {
            if (!jl_is_typevar(ety) && jl_subtype(vty, ety, 0)) {
                if (!jl_is_bits_type(ety)) {
                    ety = (jl_value_t*)jl_any_type;
                }
                Value *ary = emit_expr(args[1], ctx, true);
                const Type *elty = julia_type_to_llvm(ety, ctx);
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
                    rhs = boxed(emit_expr(args[3], ctx, true));
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
        jl_struct_type_t *sty = (jl_struct_type_t*)expr_type(args[1]);
        rt1 = (jl_value_t*)sty;
        if (jl_is_struct_type(sty) && jl_is_expr(args[2]) &&
            ((jl_expr_t*)args[2])->head == quote_sym &&
            jl_is_symbol(jl_exprarg(args[2],0))) {
            size_t offs = jl_field_offset(sty,
                                          (jl_sym_t*)jl_exprarg(args[2],0));
            if (offs != (size_t)-1) {
                Value *strct = emit_expr(args[1], ctx, true);
                Value *fld = emit_nthptr(strct, offs+1);
                null_pointer_check(fld, ctx);
                JL_GC_POP();
                return fld;
            }
        }
    }
    else if (f->fptr == &jl_f_set_field && nargs==3) {
        jl_struct_type_t *sty = (jl_struct_type_t*)expr_type(args[1]);
        rt1 = (jl_value_t*)sty;
        if (jl_is_struct_type(sty) && jl_is_expr(args[2]) &&
            ((jl_expr_t*)args[2])->head == quote_sym &&
            jl_is_symbol(jl_exprarg(args[2],0))) {
            size_t offs = jl_field_offset(sty,
                                          (jl_sym_t*)jl_exprarg(args[2],0));
            if (offs != (size_t)-1) {
                jl_value_t *ft = jl_tupleref(sty->types, offs);
                jl_value_t *rhst = expr_type(args[3]);
                rt2 = rhst;
                if (jl_subtype(rhst, ft, 0)) {
                    Value *strct = emit_expr(args[1], ctx, true);
                    Value *rhs = boxed(emit_expr(args[3], ctx, true));
                    Value *addr = emit_nthptr_addr(strct, offs+1);
                    builder.CreateStore(rhs, addr);
                    JL_GC_POP();
                    return rhs;
                }
            }
        }
    }
    // TODO: other known builtins
    JL_GC_POP();
    return NULL;
}

static Value *emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theEnv=NULL;
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
        // TODO
        //if (!b->constp) b = NULL;
    }
    if (jl_is_expr(a0) && ((jl_expr_t*)a0)->head == top_sym) {
        headIsGlobal = true;
        // (top x) is also global
        b = jl_get_binding(ctx->module,
                           (jl_sym_t*)jl_exprarg(((jl_expr_t*)a0),0));
        if (b->value==NULL ||
            (!b->constp && !(jl_is_func(b->value) && jl_is_gf(b->value))))
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
        Value *result = emit_known_call(f, args, nargs, ctx, &theFptr, &theEnv);
        if (result != NULL) return result;
    }
    int last_depth = ctx->argDepth;
    hdtype = expr_type(a00);
    if (theFptr == NULL) {
        Value *theFunc = emit_expr(args[0], ctx, true);
#ifdef JL_GC_MARKSWEEP
        if (!headIsGlobal && (jl_is_expr(a0) || jl_is_lambda_info(a0))) {
            make_gcroot(boxed(theFunc), ctx);
        }
#endif
        if (!jl_is_func_type(hdtype) &&
            hdtype!=(jl_value_t*)jl_struct_kind &&
            !(jl_is_tag_type(hdtype) &&
              ((jl_tag_type_t*)hdtype)->name==jl_type_type->name &&
              jl_is_struct_type(jl_tparam0(hdtype)))) {
            emit_func_check(theFunc, ctx);
        }
        if (theFunc->getType() != jl_pvalue_llvmt) {
            // we know it's not a function, in fact it has been declared
            // not to be. the above error should therefore trigger.
            ctx->argDepth = last_depth;
            return V_null;
        }
        else {
            // extract pieces of the function object
            // TODO: try extractelement instead
            theFptr =
                builder.CreateBitCast(emit_nthptr(theFunc, 1), jl_fptr_llvmt);
            theEnv = emit_nthptr(theFunc, 2);
        }
    }
    // emit arguments
    size_t i;
    int argStart = ctx->argDepth;
    assert(nargs+ctx->argDepth <= ((ConstantInt*)ctx->argTemp->getArraySize())->getZExtValue());
    for(i=0; i < nargs; i++) {
        Value *anArg = emit_expr(args[i+1], ctx, true);
        // put into argument space
        Value *dest=builder.CreateGEP(ctx->argTemp,
                                      ConstantInt::get(T_int32,ctx->argDepth));
        builder.CreateStore(boxed(anArg), dest);
        ctx->argDepth++;
    }

    // call
    Value *myargs = builder.CreateGEP(ctx->argTemp,
                                      ConstantInt::get(T_int32, argStart));
    Value *result = builder.CreateCall3(theFptr, theEnv, myargs,
                                        ConstantInt::get(T_int32,nargs));

    ctx->argDepth = last_depth;
    return result;
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
    Value *bp = var_binding_pointer(s, ctx);
    const Type *vt = bp->getType();
    if (vt->isPointerTy() && vt->getContainedType(0)!=jl_pvalue_llvmt)
        builder.CreateStore(emit_unbox(vt->getContainedType(0), vt,
                                       emit_unboxed(r, ctx)),
                            bp);
    else
        builder.CreateStore(boxed(emit_expr(r, ctx, true)), bp);
}

static Value *emit_var(jl_sym_t *sym, jl_value_t *ty, jl_codectx_t *ctx)
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
    Value *bp = var_binding_pointer(sym, ctx);
    Value *arg = (*ctx->arguments)[sym->name];
    // arguments are always defined
    if (arg != NULL || !jl_subtype((jl_value_t*)jl_undef_type, ty, 0)) {
        return tpropagate(bp, builder.CreateLoad(bp, false));
    }
    return emit_checked_var(bp, sym->name, ctx);
}

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool value)
{
    if (jl_is_symbol(expr)) {
        return emit_var((jl_sym_t*)expr, (jl_value_t*)jl_undef_type, ctx);
    }
    if (jl_is_symbolnode(expr)) {
        return emit_var(jl_symbolnode_sym(expr),
                        jl_symbolnode_type(expr), ctx);
    }
    else if (jl_is_labelnode(expr)) {
        assert(!value);
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
    if (!jl_is_expr(expr)) {
        // numeric literals
        if (jl_is_int32(expr)) {
        }
        else if (jl_is_int64(expr)) {
        }
        else if (jl_is_uint64(expr)) {
        }
        else if (jl_is_float64(expr)) {
        }
        else if (jl_is_array(expr)) {
            // string literal
        }
        else if (jl_is_lambda_info(expr)) {
            return emit_lambda_closure(expr, ctx);
        }
        // TODO: for now just return the direct pointer
        return literal_pointer_val(expr);
    }
    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = &jl_cellref(ex->args,0);
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (ex->head == goto_sym) {
        assert(!value);
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            int labelname = jl_unbox_long(args[0]);
            BasicBlock *bb = (*ctx->labels)[labelname];
            assert(bb);
            builder.CreateBr(bb);
        }
    }
    else if (ex->head == goto_ifnot_sym) {
        assert(!value);
        jl_value_t *cond = args[0];
        int labelname = jl_unbox_long(args[1]);
        Value *condV = emit_expr(cond, ctx, true);
#ifdef CONDITION_REQUIRES_BOOL
        if (expr_type(cond) != (jl_value_t*)jl_bool_type &&
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
        return emit_call(args, ex->args->length, ctx);
    }

    else if (ex->head == assign_sym) {
        emit_assignment(args[0], args[1], ctx);
        if (value) {
            return literal_pointer_val((jl_value_t*)jl_nothing);
        }
    }
    else if (ex->head == top_sym) {
        jl_binding_t *b = jl_get_binding(ctx->module, (jl_sym_t*)args[0]);
        Value *bp = globalvar_binding_pointer((jl_sym_t*)args[0], ctx);
        if ((b->constp && b->value!=NULL) ||
            (ex->etype!=(jl_value_t*)jl_any_type &&
             !jl_subtype((jl_value_t*)jl_undef_type, ex->etype, 0))) {
            return builder.CreateLoad(bp, false);
        }
        return emit_checked_var(bp, ((jl_sym_t*)args[0])->name, ctx);
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
        Value *name = literal_pointer_val(mn);
        Value *bp = var_binding_pointer((jl_sym_t*)mn, ctx);
        Value *a1 = emit_expr(args[1], ctx, true);
        make_gcroot(boxed(a1), ctx);
        Value *a2 = emit_expr(args[2], ctx, true);
        make_gcroot(boxed(a2), ctx);
        Value *m = builder.CreateCall4(jlmethod_func, name, bp, a1, a2);
        ctx->argDepth-=2;
        return m;
    }
    else if (ex->head == isbound_sym) {
        jl_sym_t *sy=NULL;
        jl_value_t *a = args[0];
        if (jl_is_symbol(a))
            sy = (jl_sym_t*)a;
        else if (jl_is_symbolnode(a))
            sy = jl_symbolnode_sym(a);
        else
            assert(false);
        Value *bp = var_binding_pointer(sy, ctx);
        if (bp->getType()->getContainedType(0) != jl_pvalue_llvmt) {
            // unboxed vars will never be referenced undefined
            return ConstantInt::get(T_int1, 1);
        }
        jl_value_t *st = expr_type(args[0]);
        if (st == (jl_value_t*)jl_undef_type) {
            // type==Undef => definitely not assigned
            return ConstantInt::get(T_int1, 0);
        }
        if (ctx->linfo->specTypes != NULL &&
            !jl_subtype((jl_value_t*)jl_undef_type, st, 0)) {
            // Undef ⊄ expr_type => definitely assigned
            return ConstantInt::get(T_int1, 1);
        }
        Value *v = builder.CreateLoad(bp, false);
        Value *isbnd = builder.CreateICmpNE(v, V_null);
        return isbnd;
    }

    else if (ex->head == quote_sym) {
        jl_value_t *jv = args[0];
        return literal_pointer_val(jv);
    }
    else if (ex->head == null_sym) {
        return literal_pointer_val((jl_value_t*)jl_nothing);
	}
    else if (ex->head == static_typeof_sym) {
        jl_value_t *extype = expr_type((jl_value_t*)ex);
        if (jl_is_tag_type(extype) &&
            ((jl_tag_type_t*)extype)->name == jl_type_type->name) {
            extype = jl_tparam0(extype);
            if (jl_is_typevar(extype))
                extype = ((jl_tvar_t*)extype)->ub;
        }
        else {
            extype = (jl_value_t*)jl_any_type;
        }
        return literal_pointer_val(extype);
        /*
        jl_sym_t *s = (jl_sym_t*)args[0];
        if (jl_is_symbol(s)) {
            jl_value_t *ty = (*ctx->declTypes)[s->name];
            if (ty != NULL) {
                return literal_pointer_val(ty);
            }
        }
        return literal_pointer_val((jl_value_t*)jl_any_type);
        */
    }
    else if (ex->head == new_sym) {
        jl_value_t *ty = expr_type(args[0]);
        if (jl_is_tag_type(ty) &&
            ((jl_tag_type_t*)ty)->name == jl_type_type->name &&
            jl_is_struct_type(jl_tparam0(ty)) &&
            jl_is_leaf_type(jl_tparam0(ty))) {
            ty = jl_tparam0(ty);
            size_t nf = ((jl_struct_type_t*)ty)->names->length;
            if (nf > 0) {
                Value *strct =
                    builder.CreateBitCast
                    (builder.CreateCall(jlallocobj_func,
                                        ConstantInt::get(T_size,
                                                         sizeof(void*)*(nf+1))),
                     jl_pvalue_llvmt);
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
        Value *typ = emit_expr(args[0], ctx, true);
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
    if (value) {
        jl_errorf("unsupported expression type %s", ex->head->name);
    }
    return NULL;
}

static bool store_unboxed_p(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    return (ctx->linfo->inferred && jl_is_bits_type(jt) &&
            jl_is_leaf_type(jt) &&
            // don't unbox intrinsics, since inference depends on their having
            // stable addresses for table lookup.
            jt != (jl_value_t*)jl_intrinsic_type && !(*ctx->isCaptured)[name]);
}

static AllocaInst *alloc_local(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    const Type *vtype;
    if (store_unboxed_p(name, ctx))
        vtype = julia_type_to_llvm(jt, ctx);
    else
        vtype = jl_pvalue_llvmt;
    AllocaInst *lv = builder.CreateAlloca(vtype, 0, name);
    if (vtype != jl_pvalue_llvmt)
        mark_julia_type(lv, jt);
    (*ctx->vars)[name] = lv;
    return lv;
}

extern char *jl_stack_lo;

extern "C" jl_tuple_t *jl_tuple_tvars_to_symbols(jl_tuple_t *t);

static void emit_function(jl_lambda_info_t *lam, Function *f)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    //jl_print((jl_value_t*)ast);
    //ios_printf(ios_stdout, "\n");
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);
    std::map<std::string, Value*> localVars;
    std::map<std::string, Value*> argumentMap;
    std::map<std::string, int> closureEnv;
    std::map<std::string, bool> isAssigned;
    std::map<std::string, bool> isCaptured;
    std::map<std::string, jl_value_t*> declTypes;
    std::map<int, BasicBlock*> labels;
    std::map<int, Value*> savestates;
    std::map<int, Value*> jmpbufs;
    jl_array_t *largs = jl_lam_args(ast);
    jl_array_t *lvars = jl_lam_locals(ast);
    Function::arg_iterator AI = f->arg_begin();
    const Argument &envArg = *AI++;
    const Argument &argArray = *AI++;
    const Argument &argCount = *AI++;
    jl_codectx_t ctx;
    ctx.f = f;
    ctx.vars = &localVars;
    ctx.arguments = &argumentMap;
    ctx.closureEnv = &closureEnv;
    ctx.isAssigned = &isAssigned;
    ctx.isCaptured = &isCaptured;
    ctx.declTypes = &declTypes;
    ctx.labels = &labels;
    ctx.savestates = &savestates;
    ctx.jmpbufs = &jmpbufs;
    ctx.module = jl_system_module; //TODO
    ctx.ast = ast;
    ctx.sp = jl_tuple_tvars_to_symbols(lam->sparams);
    //JL_GC_PUSH(&ctx.sp);
    jl_gc_preserve((jl_value_t*)ctx.sp);
    ctx.linfo = lam;
    ctx.envArg = &envArg;
    ctx.argArray = &argArray;
    ctx.argCount = &argCount;
    ctx.funcName = lam->name->name;
    ctx.float32Temp = NULL;

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
    llvm::DIArray EltTypeArray = dbuilder->getOrCreateArray(NULL,0);
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
    }

    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t i;
    for(i=0; i < vinfos->length; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        isAssigned[vname] = (jl_cellref(vi,3)!=jl_false);
        isCaptured[vname] = (jl_cellref(vi,2)!=jl_false);
        declTypes[vname] = jl_cellref(vi,1);
    }
    vinfos = jl_lam_capt(ast);
    for(i=0; i < vinfos->length; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        closureEnv[vname] = i;
        isAssigned[vname] = (jl_cellref(vi,3)!=jl_false);
        isCaptured[vname] = true;
        declTypes[vname] = jl_cellref(vi,1);
    }

    int n_roots = 0;
    // allocate local variables
    // must be first for the mem2reg pass to work
    for(i=0; i < largs->length; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
            AllocaInst *lv = alloc_local(argname, &ctx);
            argumentMap[argname] = lv;
        }
        else if (isAssigned[argname] || (va && i==largs->length-1)) {
            n_roots++;
        }
    }
    for(i=0; i < lvars->length; i++) {
        char *argname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        if (store_unboxed_p(argname, &ctx)) {
            alloc_local(argname, &ctx);
        }
        else {
            n_roots++;
        }
    }

    int32_t argdepth = (int32_t)max_arg_depth((jl_value_t*)ast);
    n_roots += argdepth;
    ctx.argTemp = builder.CreateAlloca(jl_pvalue_llvmt,
                                       ConstantInt::get(T_int32, n_roots));
    ctx.argDepth = 0;

#ifdef JL_GC_MARKSWEEP
    // create gc frame
    AllocaInst *gcframe = builder.CreateAlloca(T_gcframe, 0);
    builder.CreateStore(builder.CreateBitCast(ctx.argTemp,
                                              PointerType::get(jl_ppvalue_llvmt,0)),
                        builder.CreateConstGEP2_32(gcframe, 0, 0));
    builder.CreateStore(ConstantInt::get(T_size, n_roots),
                        builder.CreateConstGEP2_32(gcframe, 0, 1));
    builder.CreateStore(ConstantInt::get(T_int32, 0),
                        builder.CreateConstGEP2_32(gcframe, 0, 2));
    builder.CreateStore(builder.CreateLoad
                        (builder.CreateLoad(jlpgcstack_var, false), false),
                        builder.CreateConstGEP2_32(gcframe, 0, 3));
    builder.CreateStore(gcframe,
                        builder.CreateLoad(jlpgcstack_var, false));
    // initialize stack roots to null
    for(i=0; i < (size_t)n_roots; i++) {
        Value *argTempi = builder.CreateConstGEP1_32(ctx.argTemp,i);
        builder.CreateStore(V_null, argTempi);
    }
#endif

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
            argumentMap[argname] = av;
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
            argumentMap[argname] = argPtr;
        }
        else {
            LoadInst *theArg = builder.CreateLoad(argPtr, false);
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
        // restarg = jl_f_tuple(NULL, &args[nreq], nargs-nreq)
        Value *restTuple =
            builder.CreateCall3(jltuple_func, V_null,
                                builder.CreateGEP((Value*)&argArray,
                                                  ConstantInt::get(T_int32,nreq)),
                                builder.CreateSub((Value*)&argCount,
                                                  ConstantInt::get(T_int32,nreq)));
        char *argname = jl_decl_var(jl_cellref(largs,nreq))->name;
        Value *lv = localVars[argname];
        if (isBoxed(argname, &ctx))
            builder.CreateStore(builder.CreateCall(jlbox_func, restTuple), lv);
        else
            builder.CreateStore(restTuple, lv);
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
            builder.CreateStore(builder.CreateLoad(builder.CreateConstGEP2_32(gcframe, 0, 3), false),
                                builder.CreateLoad(jlpgcstack_var, false));
#endif
            builder.CreateRet(retval);
            if (i != stmts->length-1) {
                BasicBlock *bb =
                    BasicBlock::Create(getGlobalContext(), "ret", ctx.f);
                builder.SetInsertPoint(bb);
            }
        }
        else {
            (void)emit_expr(stmt, &ctx, false);
        }
    }
    // sometimes we have dangling labels after the end
    if (builder.GetInsertBlock()->getTerminator() == NULL) {
        builder.CreateRet(V_null);
    }
    //JL_GC_POP();
    jl_gc_unpreserve();
}

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

extern "C" JL_CALLABLE(jl_f_tuple);

extern "C" jl_value_t *jl_new_box(jl_value_t *v)
{
    jl_value_t *box = (jl_value_t*)alloc_2w();
    box->type = jl_box_any_type;
    ((jl_value_t**)box)[1] = v;
    return box;
}

static void addPass(FunctionPassManager *PM, Pass *P)
{
    // Add the pass to the pass manager...
    PM->add(P);
}

static void init_julia_llvm_env(Module *m)
{
    T_int1  = Type::getInt1Ty(getGlobalContext());
    T_int8  = Type::getInt8Ty(getGlobalContext());
    T_pint8 = PointerType::get(T_int8, 0);
    T_int16 = Type::getInt16Ty(getGlobalContext());
    T_pint16 = PointerType::get(T_int16, 0);
    T_int32 = Type::getInt32Ty(getGlobalContext());
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
    PATypeHolder tempTy = OpaqueType::get(getGlobalContext());
    std::vector<const Type*> valueStructElts(0);
    valueStructElts.push_back(PointerType::getUnqual(tempTy));
    StructType *valueSt = StructType::get(getGlobalContext(),valueStructElts);
    ((OpaqueType*)tempTy.get())->refineAbstractTypeTo(valueSt);
    jl_value_llvmt = tempTy.get();

    jl_pvalue_llvmt = PointerType::get(jl_value_llvmt, 0);
    jl_ppvalue_llvmt = PointerType::get(jl_pvalue_llvmt, 0);
    V_null = Constant::getNullValue(jl_pvalue_llvmt);
    std::vector<const Type*> ftargs(0);
    ftargs.push_back(jl_pvalue_llvmt);
    ftargs.push_back(jl_ppvalue_llvmt);
    ftargs.push_back(T_int32);
    jl_func_sig = FunctionType::get(jl_pvalue_llvmt, ftargs, false);
    assert(jl_func_sig != NULL);
    jl_fptr_llvmt = PointerType::get(jl_func_sig, 0);

#ifdef JL_GC_MARKSWEEP
    std::vector<const Type*> gcframeStructElts(0);
    gcframeStructElts.push_back(PointerType::get(jl_ppvalue_llvmt,0));
    gcframeStructElts.push_back(T_size);
    gcframeStructElts.push_back(T_int32);
    PATypeHolder tempTy2 = OpaqueType::get(getGlobalContext());
    gcframeStructElts.push_back(PointerType::getUnqual(tempTy2));
    StructType *gcfSt = StructType::get(getGlobalContext(),gcframeStructElts);
    ((OpaqueType*)tempTy2.get())->refineAbstractTypeTo(gcfSt);
    T_gcframe = tempTy2.get();

    jlpgcstack_var =
        new GlobalVariable(*jl_Module,
                           PointerType::get(PointerType::get(T_gcframe,0),0),
                           true, GlobalVariable::ExternalLinkage,
                           NULL, "jl_pgcstack");
    jl_ExecutionEngine->addGlobalMapping(jlpgcstack_var, (void*)&jl_pgcstack);
#endif

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
    jlsysmod_var = global_to_llvm("jl_system_module", (void*)&jl_system_module);
    jlexc_var = global_to_llvm("jl_exception_in_transit",
                               (void*)&jl_exception_in_transit);

    std::vector<const Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", jl_Module);
    jlerror_func->setDoesNotReturn();
    jl_ExecutionEngine->addGlobalMapping(jlerror_func, (void*)&jl_error);

    std::vector<const Type*> args1_(0);
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

    std::vector<const Type*> empty_args(0);
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

    std::vector<const Type*> te_args(0);
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

    std::vector<const Type *> args2(0);
    args2.push_back(T_pint8);
    args2.push_back(T_pint8);
    jlgetbindingp_func =
        Function::Create(FunctionType::get(jl_ppvalue_llvmt, args2, false),
                         Function::ExternalLinkage,
                         "jl_get_bindingp", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlgetbindingp_func,
                                         (void*)&jl_get_bindingp);

    jltuple_func = jlfunc_to_llvm("jl_f_tuple", (void*)*jl_f_tuple);
    jlapplygeneric_func =
        jlfunc_to_llvm("jl_apply_generic", (void*)*jl_apply_generic);

    std::vector<const Type*> args3(0);
    args3.push_back(jl_pvalue_llvmt);
    jlbox_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args3, false),
                         Function::ExternalLinkage,
                         "jl_new_box", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlbox_func, (void*)&jl_new_box);

    std::vector<const Type*> args4(0);
    args4.push_back(jl_pvalue_llvmt);
    args4.push_back(jl_pvalue_llvmt);
    jlclosure_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args4, false),
                         Function::ExternalLinkage,
                         "jl_new_closure_internal", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlclosure_func,
                                         (void*)&jl_new_closure_internal);

    std::vector<const Type*> args5(0);
    args5.push_back(T_size);
    jlntuple_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args5, true),
                         Function::ExternalLinkage,
                         "jl_tuple", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlntuple_func, (void*)&jl_tuple);

    std::vector<const Type*> mdargs(0);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_ppvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    mdargs.push_back(jl_pvalue_llvmt);
    jlmethod_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, mdargs, false),
                         Function::ExternalLinkage,
                         "jl_method_def", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlmethod_func, (void*)&jl_method_def);

    std::vector<const Type*> ehargs(0);
    ehargs.push_back(T_pint8);
    ehargs.push_back(T_pint8);
    jlenter_func =
        Function::Create(FunctionType::get(T_void, ehargs, false),
                         Function::ExternalLinkage,
                         "jl_enter_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlenter_func, (void*)&jl_enter_handler);

    std::vector<const Type*> lhargs(0);
    lhargs.push_back(T_int32);
    jlleave_func =
        Function::Create(FunctionType::get(T_void, lhargs, false),
                         Function::ExternalLinkage,
                         "jl_pop_handler", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlleave_func, (void*)&jl_pop_handler);

    std::vector<const Type*> aoargs(0);
    aoargs.push_back(T_size);
    jlallocobj_func =
        Function::Create(FunctionType::get(T_pint8, aoargs, false),
                         Function::ExternalLinkage,
                         "allocobj", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlallocobj_func, (void*)&allocobj);

    // set up optimization passes
    FPM = new FunctionPassManager(jl_Module);
    FPM->add(new TargetData(*jl_ExecutionEngine->getTargetData()));
    
    // list of passes from vmkit
    addPass(FPM, createCFGSimplificationPass()); // Clean up disgusting code
    addPass(FPM, createPromoteMemoryToRegisterPass());// Kill useless allocas
    
    addPass(FPM, createInstructionCombiningPass()); // Cleanup for scalarrepl.
    addPass(FPM, createScalarReplAggregatesPass()); // Break up aggregate allocas
    addPass(FPM, createInstructionCombiningPass()); // Cleanup for scalarrepl.
    addPass(FPM, createJumpThreadingPass());        // Thread jumps.
    addPass(FPM, createCFGSimplificationPass());    // Merge & remove BBs
    addPass(FPM, createInstructionCombiningPass()); // Combine silly seq's
    
    addPass(FPM, createCFGSimplificationPass());    // Merge & remove BBs
    addPass(FPM, createReassociatePass());          // Reassociate expressions
    addPass(FPM, createLoopRotatePass());           // Rotate loops.
    addPass(FPM, createLICMPass());                 // Hoist loop invariants
    addPass(FPM, createLoopUnswitchPass());         // Unswitch loops.
    addPass(FPM, createInstructionCombiningPass()); 
    addPass(FPM, createIndVarSimplifyPass());       // Canonicalize indvars
    //addPass(FPM, createLoopDeletionPass());         // Delete dead loops
    addPass(FPM, createLoopUnrollPass());           // Unroll small loops
    //addPass(FPM, createLoopStrengthReducePass());   // (jwb added)
    
    addPass(FPM, createInstructionCombiningPass()); // Clean up after the unroller
    addPass(FPM, createGVNPass());                  // Remove redundancies
    addPass(FPM, createMemCpyOptPass());            // Remove memcpy / form memset  
    addPass(FPM, createSCCPPass());                 // Constant prop with SCCP
    
    // Run instcombine after redundancy elimination to exploit opportunities
    // opened up by them.
    addPass(FPM, createInstructionCombiningPass());
    addPass(FPM, createJumpThreadingPass());         // Thread jumps
    addPass(FPM, createDeadStoreEliminationPass());  // Delete dead stores
    addPass(FPM, createAggressiveDCEPass());         // Delete dead instructions
    addPass(FPM, createCFGSimplificationPass());     // Merge & remove BBs

    /*
    FPM->add(createCFGSimplificationPass());
    FPM->add(createPromoteMemoryToRegisterPass());
    FPM->add(createInstructionCombiningPass());
    FPM->add(createJumpThreadingPass());
    FPM->add(createCFGSimplificationPass());
    //FPM->add(createDeadCodeEliminationPass());
    FPM->add(createReassociatePass());
    //FPM->add(createInstructionCombiningPass());
    //FPM->add(createGVNPass());
    FPM->add(createSCCPPass());
    FPM->add(createDeadStoreEliminationPass());
    //llvm::createStandardFunctionPasses(FPM, 2);
    */

    FPM->doInitialization();
}

extern "C" void jl_init_codegen()
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

    jl_init_intrinsic_functions();
    jl_jit_events = new JuliaJITEventListener();
    jl_ExecutionEngine->RegisterJITEventListener(jl_jit_events);
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
