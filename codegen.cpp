#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/StandardPasses.h"
#include <cstdio>
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
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"
#include "builtin_proto.h"
}

#define CONDITION_REQUIRES_BOOL

// llvm state
static LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static Module *jl_Module;
static ExecutionEngine *jl_ExecutionEngine;
static std::map<const std::string, GlobalVariable*> stringConstants;
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
static const Type *T_float32;
static const Type *T_pfloat32;
static const Type *T_float64;
static const Type *T_pfloat64;
static const Type *T_void;

// constants
static Value *V_null;

// global vars
static GlobalVariable *jltrue_var;
static GlobalVariable *jlfalse_var;
static GlobalVariable *jlnull_var;
static GlobalVariable *jlsysmod_var;

// important functions
static Function *jlerror_func;
static Function *jltypeerror_func;
static Function *jlgetbindingp_func;
static Function *jltuple_func;
static Function *jlntuple_func;
static Function *jlapplygeneric_func;
static Function *jlalloc_func;
static Function *jlbox_func;
static Function *jlclosure_func;
static Function *jlconvert_func;

/*
  plan

  The Simplest Thing That Could Possibly Work:
  * simple code gen for all node types
  * implement all low-level intrinsics
  * instantiate-method to provide static parameters
  * default conversion functions, instantiating conversion functions

  stuff to fix up:
  * discard toplevel wrapper functions (now interpreted)
  * gensyms from the front end might conflict with real variables, fix it
  - better error messages
  - exceptions
  - threads or other advanced control flow

  - source location tracking, var name metadata
  * rootlist to track pointers emitted into code
  - function/var name mangling
  - experiment with llvm optimization passes, option to disable them

  optimizations round 1:
  - constants, especially global. resolve functions statically.
  - keep a table mapping fptr to Function* for compiling known calls
  - manually inline simple builtins (tuple,box,boxset,etc.)
  - int and float constant table
  - dispatch optimizations
  * inline space for buffers
  * preallocate boxes for small integers
  ? speed up type caching
  * do something about all the string copying from scheme
  * speed up scheme pattern matcher

  optimizations round 2:
  - type inference
  - mark non-null references and avoid null check
  - static method lookup
  - avoid tuple allocation in (a,b)=(b,a)
  - varargs and ... optimizations

  optimizations round 3:
  - inlining
  - unboxing
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

static Function *to_function(jl_lambda_info_t *li)
{
    Function *f = Function::Create(jl_func_sig, Function::ExternalLinkage,
                                   li->name->name, jl_Module);
    assert(jl_is_expr(li->ast));
    li->functionObject = (void*)f;
    BasicBlock *old = builder.GetInsertBlock();
    emit_function(li, f);
    FPM->run(*f);
    // print out the function's LLVM code
    //f->dump();
    //verifyFunction(*f);
    if (old != NULL)
        builder.SetInsertPoint(old);
    return f;
}

extern "C" void jl_generate_fptr(jl_function_t *f)
{
    // objective: assign li->fptr
    jl_lambda_info_t *li = f->linfo;
    assert(li->functionObject);
    Function *llvmf = (Function*)li->functionObject;
    li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(llvmf);
    assert(li->fptr != NULL);
    jl_value_t *env = f->env;
    if (f->fptr == &jl_trampoline) {
        assert(jl_t0(env) == (jl_value_t*)f);
        f->env = jl_t1(env);
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

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    std::map<std::string, AllocaInst*> *vars;
    std::map<std::string, AllocaInst*> *arguments;
    std::map<std::string, int> *closureEnv;
    std::map<std::string, bool> *isBoxed;
    std::map<std::string, jl_value_t*> *declTypes;
    std::map<int, BasicBlock*> *labels;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_tuple_t *sp;
    jl_lambda_info_t *linfo;
    const Argument *envArg;
    const Argument *argArray;
    const Argument *argCount;
    AllocaInst *argTemp;
    std::string funcName;
} jl_codectx_t;

static Value *literal_pointer_val(void *p, const Type *t)
{
#ifdef BITS64
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

static jl_value_t *llvm_type_to_julia(const Type *t);

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

static void emit_typecheck(Value *x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype =
        builder.CreateICmpEQ(emit_typeof(x), literal_pointer_val(type));
    BasicBlock *failBB = BasicBlock::Create(getGlobalContext(),"fail",ctx->f);
    BasicBlock *passBB = BasicBlock::Create(getGlobalContext(),"pass");
    builder.CreateCondBr(istype, passBB, failBB);
    builder.SetInsertPoint(failBB);

    std::vector<Value *> zeros(0);
    zeros.push_back(ConstantInt::get(T_int32, 0));
    zeros.push_back(ConstantInt::get(T_int32, 0));
    Value *fname_val = builder.CreateGEP(stringConst(ctx->funcName),
                                         zeros.begin(), zeros.end());
    Value *msg_val = builder.CreateGEP(stringConst(msg),
                                       zeros.begin(), zeros.end());
    builder.CreateCall4(jltypeerror_func,
                        fname_val, msg_val,
                        literal_pointer_val(type), x);

    builder.CreateBr(passBB);
    ctx->f->getBasicBlockList().push_back(passBB);
    builder.SetInsertPoint(passBB);
}

static Value *emit_bounds_check(Value *i, Value *len, const std::string &msg,
                                jl_codectx_t *ctx)
{
    Value *im1 = builder.CreateSub(i, ConstantInt::get(T_int32, 1));
    Value *ok = builder.CreateICmpULT(im1, len);
    error_unless(ok, msg, ctx);
    return im1;
}

static void emit_func_check(Value *x, const std::string &msg, jl_codectx_t *ctx)
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
    emit_error(msg, ctx);
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
    if (jl_is_expr(s) && ((jl_expr_t*)s)->head == symbol_sym)
        s = (jl_sym_t*)jl_exprarg(s,0);
    assert(jl_is_symbol(s));
    std::map<std::string,int>::iterator it = ctx->closureEnv->find(s->name);
    if (it != ctx->closureEnv->end()) {
        int idx = (*it).second;
        if ((*ctx->isBoxed)[s->name]) {
            return emit_nthptr_addr(emit_nthptr((Value*)ctx->envArg, idx+2), 1);
        }
        return emit_nthptr_addr((Value*)ctx->envArg, idx+2);
    }
    AllocaInst *l = (*ctx->vars)[s->name];
    if (l != NULL) {
        if ((*ctx->isBoxed)[s->name]) {
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
    Value *v = builder.CreateLoad(bp, false);
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
            AllocaInst *l = (*ctx->vars)[s->name];
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

static size_t biggest_call(jl_value_t *expr)
{
    if (jl_is_expr(expr)) {
        size_t max = 0;
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, m;
        for(i=0; i < e->args->length; i++) {
            m = biggest_call(jl_exprarg(e,i));
            if (m > max) max = m;
        }
        if (e->head == call_sym || e->head == call1_sym) {
            if (e->args->length > max)
                max = e->args->length;
        }
        return max;
    }
    return 0;
}

#include "intrinsics.cpp"

extern "C" jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types);

static jl_value_t *expr_type(jl_value_t *e)
{
    if (jl_is_expr(e))
        return ((jl_expr_t*)e)->etype;
    if (jl_is_symbol(e))
        return (jl_value_t*)jl_any_type;
    if (jl_is_lambda_info(e))
        return (jl_value_t*)jl_any_func;
    if (jl_is_some_tag_type(e))
        return (jl_value_t*)jl_wrap_Type(e);
    return (jl_value_t*)jl_typeof(e);
}

static jl_tuple_t *call_arg_types(jl_value_t **args, size_t n)
{
    jl_tuple_t *t = jl_alloc_tuple(n);
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *ty = expr_type(args[i]);
        if (!jl_is_leaf_type(ty))
            return NULL;
        jl_tupleset(t, i, ty);
    }
    return t;
}

static Value *emit_tuplelen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 1);
#ifdef BITS64
    return builder.CreateTrunc(builder.CreatePtrToInt(lenbits, T_int64),
                               T_int32);
#else
    return builder.CreatePtrToInt(lenbits, T_int32);
#endif
}

static Value *emit_arraylen(Value *t)
{
    Value *lenbits = emit_nthptr(t, 2);
#ifdef BITS64
    return builder.CreateTrunc(builder.CreatePtrToInt(lenbits, T_int64),
                               T_int32);
#else
    return builder.CreatePtrToInt(lenbits, T_int32);
#endif
}

static Value *emit_known_call(jl_value_t *ff, jl_value_t **args, size_t nargs,
                              jl_codectx_t *ctx,
                              Value **theFptr, Value **theEnv)
{
    if (jl_typeis(ff, jl_intrinsic_type)) {
        return emit_intrinsic((intrinsic)*(uint32_t*)jl_bits_data(ff),
                              args, nargs, ctx);
    }
    if (jl_is_func(ff)) {
        jl_function_t *f = (jl_function_t*)ff;
        if (f->fptr == &jl_apply_generic) {
            *theFptr = jlapplygeneric_func;
            //*theFptr = literal_pointer_val((void*)f->fptr, jl_fptr_llvmt);
            *theEnv = literal_pointer_val(f->env);
            if (ctx->linfo->specTypes != NULL) {
                jl_tuple_t *aty = call_arg_types(&args[1], nargs);
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
            Value *arg1 = emit_expr(args[1], ctx, true);
            Value *arg2 = emit_expr(args[2], ctx, true);
            if (arg1->getType() != arg2->getType())
                return ConstantInt::get(T_int1,0);
            return builder.CreateICmpEQ(arg1, arg2);
        }
        else if (f->fptr == &jl_f_tuplelen && nargs==1) {
            jl_value_t *aty = expr_type(args[1]);
            if (jl_is_tuple(aty)) {
                Value *arg1 = emit_expr(args[1], ctx, true);
                return emit_tuplelen(arg1);
            }
        }
        else if (f->fptr == &jl_f_tupleref && nargs==2) {
            jl_value_t *tty = expr_type(args[1]);
            jl_value_t *ity = expr_type(args[2]);
            if (jl_is_tuple(tty) && ity==(jl_value_t*)jl_int32_type) {
                Value *arg1 = emit_expr(args[1], ctx, true);
                if (jl_is_int32(args[2])) {
                    uint32_t idx = (uint32_t)jl_unbox_int32(args[2]);
                    if (idx > 0 &&
                        (idx < ((jl_tuple_t*)tty)->length ||
                         (idx == ((jl_tuple_t*)tty)->length &&
                          !jl_is_seq_type(jl_tupleref(tty,
                                                      ((jl_tuple_t*)tty)->length-1))))) {
                        // known to be in bounds
                        return emit_nthptr(arg1, idx+1);
                    }
                }
                Value *tlen = emit_tuplelen(arg1);
                Value *idx = emit_unbox(T_int32, T_pint32,
                                        emit_unboxed(args[2], ctx));
                emit_bounds_check(idx, tlen,
                                  "tupleref: index out of range", ctx);
                return emit_nthptr(arg1,
                                   builder.CreateAdd(idx, ConstantInt::get(T_int32,1)));
            }
        }
        else if (f->fptr == &jl_f_arraylen && nargs==1) {
            jl_value_t *aty = expr_type(args[1]);
            if (jl_subtype(aty, (jl_value_t*)jl_array_type, 0)) {
                Value *arg1 = emit_expr(args[1], ctx, true);
                return emit_arraylen(arg1);
            }
        }
        else if (f->fptr == &jl_f_arrayref && nargs==2) {
            jl_value_t *aty = expr_type(args[1]);
            jl_value_t *ity = expr_type(args[2]);
            if (jl_subtype(aty, (jl_value_t*)jl_array_type, 0) &&
                ity == (jl_value_t*)jl_int32_type) {
                if (jl_is_bits_type(jl_tparam0(aty))) {
                    Value *ary = emit_expr(args[1], ctx, true);
                    const Type *elty = julia_type_to_llvm(jl_tparam0(aty));
                    bool isbool=false;
                    if (elty==T_int1) { elty = T_int8; isbool=true; }
                    Value *data =
                        builder.CreateBitCast(emit_nthptr(ary, 3),
                                              PointerType::get(elty, 0));
                    Value *alen = emit_arraylen(ary);
                    Value *idx = emit_unbox(T_int32, T_pint32,
                                            emit_unboxed(args[2], ctx));
                    Value *im1 =
                        emit_bounds_check(idx, alen,
                                          "arrayref: index out of range", ctx);
                    Value *elt=builder.CreateLoad(builder.CreateGEP(data, im1),
                                                  false);
                    if (isbool)
                        return builder.CreateTrunc(elt, T_int1);
                    if (is_unsigned_julia_type(jl_tparam0(aty)))
                        return mark_unsigned(elt);
                    return elt;
                }
            }
        }
        else if (f->fptr == &jl_f_arrayset && nargs==3) {
            jl_value_t *aty = expr_type(args[1]);
            jl_value_t *ity = expr_type(args[2]);
            jl_value_t *vty = expr_type(args[3]);
            if (jl_subtype(aty, (jl_value_t*)jl_array_type, 0) &&
                ity == (jl_value_t*)jl_int32_type) {
                jl_value_t *ety = jl_tparam0(aty);
                if (jl_is_bits_type(ety) && jl_subtype(vty, ety, 0)) {
                    Value *ary = emit_expr(args[1], ctx, true);
                    const Type *elty = julia_type_to_llvm(ety);
                    if (elty==T_int1) { elty = T_int8; }
                    Value *data =
                        builder.CreateBitCast(emit_nthptr(ary, 3),
                                              PointerType::get(elty, 0));
                    Value *alen = emit_arraylen(ary);
                    Value *idx = emit_unbox(T_int32, T_pint32,
                                            emit_unboxed(args[2], ctx));
                    Value *rhs = emit_unbox(elty, PointerType::get(elty,0),
                                            emit_unboxed(args[3], ctx));
                    Value *im1 =
                        emit_bounds_check(idx, alen,
                                          "arrayset: index out of range", ctx);
                    builder.CreateStore(rhs, builder.CreateGEP(data, im1));
                    return ary;
                }
            }
        }
        else if (f->fptr == &jl_f_get_field && nargs==2) {
            jl_value_t *sty = expr_type(args[1]);
            if (jl_is_struct_type(sty) && jl_is_expr(args[2]) &&
                ((jl_expr_t*)args[2])->head == quote_sym &&
                jl_is_symbol(jl_exprarg(args[2],0))) {
                size_t offs = jl_field_offset((jl_struct_type_t*)sty,
                                              (jl_sym_t*)jl_exprarg(args[2],0));
                if (offs != (size_t)-1) {
                    Value *strct = emit_expr(args[1], ctx, true);
                    return emit_nthptr(strct, offs+1);
                }
            }
        }
        // TODO: other known builtins
    }
    return NULL;
}

static Value *emit_call(jl_value_t **args, size_t arglen, jl_codectx_t *ctx)
{
    size_t nargs = arglen-1;
    Value *theFptr=NULL, *theEnv=NULL;
    jl_binding_t *b=NULL;
    jl_value_t *a0 = args[0];
    jl_value_t *hdtype;
    hdtype = expr_type(a0);
    if (jl_is_expr(a0) && ((jl_expr_t*)a0)->head==symbol_sym) {
        a0 = jl_exprarg(a0,0);
    }
    if (jl_is_symbol(a0) && is_global((jl_sym_t*)a0, ctx) &&
        jl_boundp(ctx->module, (jl_sym_t*)a0)) {
        b = jl_get_binding(ctx->module, (jl_sym_t*)a0);
        // TODO
        //if (!b->constp) b = NULL;
    }
    if (jl_is_expr(a0) && ((jl_expr_t*)a0)->head == top_sym) {
        // (top x) is also global
        b = jl_get_binding(ctx->module,
                           (jl_sym_t*)jl_exprarg(((jl_expr_t*)a0),0));
        if (!b->constp || b->value==NULL) b = NULL;
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
    if (theFptr == NULL) {
        Value *theFunc = emit_expr(args[0], ctx, true);
        if (!jl_is_func_type(hdtype) && hdtype!=(jl_value_t*)jl_struct_kind) {
            emit_func_check(theFunc, "apply: expected function", ctx);
        }
        // extract pieces of the function object
        // TODO: try extractelement instead
        if (theFunc->getType() != jl_pvalue_llvmt) {
            // we know it's not a function, in fact it has been declared
            // not to be. the above error should therefore trigger.
            return V_null;
        }
        else {
            theFptr =
                builder.CreateBitCast(emit_nthptr(theFunc, 1), jl_fptr_llvmt);
            theEnv = emit_nthptr(theFunc, 2);
        }
    }
    // emit arguments
    std::vector<Value*> argVs(0);
    size_t i;
    for(i=0; i < nargs; i++) {
        Value *anArg = emit_expr(args[i+1], ctx, true);
        argVs.push_back(anArg);
    }
    assert(nargs <= ((ConstantInt*)ctx->argTemp->getArraySize())->getZExtValue());
    // put into argument space
    for(i=0; i < nargs; i++) {
        Value *dest = builder.CreateGEP(ctx->argTemp,
                                        ConstantInt::get(T_int32,i));
        builder.CreateStore(boxed(argVs[i]), dest);
    }
    /*
    Value *stacksave =
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stacksave));
    Value *argl = builder.CreateAlloca(jl_pvalue_llvmt,
                                       ConstantInt::get(T_int32, nargs));
    size_t i;
    for(i=0; i < nargs; i++) {
        Value *anArg = emit_expr(args[i+1], ctx, true);
        Value *dest = builder.CreateGEP(argl, ConstantInt::get(T_int32,i));
        builder.CreateStore(boxed(anArg), dest);
    }
    */
    // call
    Value *result = builder.CreateCall3(theFptr, theEnv, ctx->argTemp,
                                        ConstantInt::get(T_int32,nargs));
    // restore stack
    /*
    builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                 Intrinsic::stackrestore),
                       stacksave);
    */
    return result;
}

static void emit_assignment(jl_value_t *l, jl_value_t *r, jl_codectx_t *ctx)
{
    jl_sym_t *s = NULL;
    if (jl_is_symbol(l))
        s = (jl_sym_t*)l;
    else if (jl_is_expr(l) &&
             ((jl_expr_t*)l)->head == symbol_sym)
        s = (jl_sym_t*)jl_exprarg(l,0);
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
            assert(jl_is_typevar(jl_tupleref(ctx->sp, i)));
            if (sym == ((jl_tvar_t*)jl_tupleref(ctx->sp, i))->name) {
                return literal_pointer_val(jl_tupleref(ctx->sp, i+1));
            }
        }
    }
    Value *bp = var_binding_pointer(sym, ctx);
    AllocaInst *arg = (*ctx->arguments)[sym->name];
    // arguments are always defined
    if (arg != NULL || !jl_subtype((jl_value_t*)jl_undef_type, ty, 0))
        return builder.CreateLoad(bp, false);
    return emit_checked_var(bp, sym->name, ctx);
}

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool value)
{
    if (jl_is_symbol(expr)) {
        return emit_var((jl_sym_t*)expr, (jl_value_t*)jl_undef_type, ctx);
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
            int labelname = jl_unbox_int32(args[0]);
            BasicBlock *bb = (*ctx->labels)[labelname];
            assert(bb);
            builder.CreateBr(bb);
        }
    }
    else if (ex->head == goto_ifnot_sym) {
        assert(!value);
        jl_value_t *cond = args[0];
        int labelname = jl_unbox_int32(args[1]);
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
    else if (ex->head == label_sym) {
        assert(!value);
        int labelname = jl_unbox_int32(args[0]);
        BasicBlock *bb = (*ctx->labels)[labelname];
        assert(bb);
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            builder.CreateBr(bb); // all BasicBlocks must exit explicitly
        }
        ctx->f->getBasicBlockList().push_back(bb);
        builder.SetInsertPoint(bb);
    }

    else if (ex->head == call_sym || ex->head == call1_sym) {
        return emit_call(args, ex->args->length, ctx);
    }

    else if (ex->head == symbol_sym) {
        assert(jl_is_symbol(args[0]));
        return emit_var((jl_sym_t*)args[0], ex->etype, ctx);
    }
    else if (ex->head == assign_sym) {
        assert(!value);
        emit_assignment(args[0], args[1], ctx);
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
    else if (ex->head == unbound_sym) {
        Value *bp = var_binding_pointer((jl_sym_t*)args[0], ctx);
        // unboxed vars will never be referenced undefined
        if (bp->getType()->getContainedType(0) != jl_pvalue_llvmt)
            return ConstantInt::get(T_int1, 0);
        Value *v = builder.CreateLoad(bp, false);
        Value *isnull = builder.CreateICmpEQ(v, V_null);
        return isnull;
    }

    else if (ex->head == quote_sym) {
        jl_value_t *jv = args[0];
        return literal_pointer_val(jv);
    }
    else if (ex->head == null_sym) {
        return literal_pointer_val((jl_value_t*)jl_null);
    }
    else if (ex->head == static_typeof_sym) {
        jl_sym_t *s = (jl_sym_t*)args[0];
        if (jl_is_symbol(s)) {
            jl_value_t *ty = (*ctx->declTypes)[s->name];
            if (ty != NULL) {
                return literal_pointer_val(ty);
            }
        }
        return literal_pointer_val((jl_value_t*)jl_any_type);
    }
    if (!strcmp(ex->head->name, "$")) {
        jl_error("syntax error: prefix $ outside backquote");
    }
    if (value) {
        jl_errorf("unsupported expression type %s", ex->head->name);
    }
    return NULL;
}

#define is_label(ex) (jl_is_expr(ex) && ((jl_expr_t*)ex)->head == label_sym)

static bool vinfo_isboxed(jl_array_t *a)
{
    return (jl_cellref(a,2)!=jl_false && jl_cellref(a,3)!=jl_false);
}

static AllocaInst *alloc_local(char *name, jl_codectx_t *ctx)
{
    jl_value_t *jt = (*ctx->declTypes)[name];
    const Type *vtype;
    // only store a variable unboxed if type inference has run, which
    // checks that the variable is not referenced undefined.
    if (jl_is_bits_type(jt) && !(*ctx->isBoxed)[name] && ctx->linfo->inferred)
        vtype = julia_type_to_llvm(jt);
    else
        vtype = jl_pvalue_llvmt;
    AllocaInst *lv = builder.CreateAlloca(vtype, 0, name);
    (*ctx->vars)[name] = lv;
    return lv;
}

static void emit_function(jl_lambda_info_t *lam, Function *f)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    //jl_print((jl_value_t*)ast);
    //ios_printf(ios_stdout, "\n");
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);
    std::map<std::string, AllocaInst*> localVars;
    std::map<std::string, AllocaInst*> argumentMap;
    std::map<std::string, int> closureEnv;
    std::map<std::string, bool> isBoxed;
    std::map<std::string, jl_value_t*> declTypes;
    std::map<int, BasicBlock*> labels;
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
    ctx.isBoxed = &isBoxed;
    ctx.declTypes = &declTypes;
    ctx.labels = &labels;
    ctx.module = jl_system_module; //TODO
    ctx.ast = ast;
    ctx.sp = lam->sparams;
    ctx.linfo = lam;
    ctx.envArg = &envArg;
    ctx.argArray = &argArray;
    ctx.argCount = &argCount;
    ctx.funcName = lam->name->name;

    // process var-info lists to see what vars are captured, need boxing
    jl_array_t *vinfos = jl_lam_vinfo(ast);
    size_t i;
    for(i=0; i < vinfos->length; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        isBoxed[vname] = vinfo_isboxed(vi);
        declTypes[vname] = jl_cellref(vi,1);
    }
    vinfos = jl_lam_capt(ast);
    for(i=0; i < vinfos->length; i++) {
        jl_array_t *vi = (jl_array_t*)jl_cellref(vinfos, i);
        assert(jl_is_array(vi));
        char *vname = ((jl_sym_t*)jl_cellref(vi,0))->name;
        closureEnv[vname] = i;
        isBoxed[vname] = vinfo_isboxed(vi);
        declTypes[vname] = jl_cellref(vi,1);
    }

    // allocate local variables
    // must be first for the mem2reg pass to work
    for(i=0; i < largs->length; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        argumentMap[argname] = alloc_local(argname, &ctx);
    }
    for(i=0; i < lvars->length; i++) {
        char *argname = ((jl_sym_t*)jl_cellref(lvars,i))->name;
        AllocaInst *lv = alloc_local(argname, &ctx);
        if (isBoxed[argname])
            builder.CreateStore(builder.CreateCall(jlbox_func, V_null), lv);
        else if (lv->getAllocatedType() == jl_pvalue_llvmt)
            builder.CreateStore(V_null, lv);
    }

    ctx.argTemp =
        builder.CreateAlloca(jl_pvalue_llvmt,
                             ConstantInt::get(T_int32,
                                              (int32_t)biggest_call((jl_value_t*)ast)));

    // check arg count
    size_t nreq = largs->length;
    int va = 0;
    if (nreq > 0 && jl_is_rest_arg(jl_cellref(largs,nreq-1))) {
        nreq--;
        va = 1;
        Value *enough =
            builder.CreateICmpUGE((Value*)&argCount,
                                  ConstantInt::get(T_int32, nreq));
        BasicBlock *elseBB = BasicBlock::Create(getGlobalContext(), "else", f);
        BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
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
        BasicBlock *elseBB = BasicBlock::Create(getGlobalContext(), "else", f);
        BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
        builder.CreateCondBr(enough, mergeBB, elseBB);
        builder.SetInsertPoint(elseBB);
        emit_error("wrong number of arguments", &ctx);
        builder.CreateBr(mergeBB);
        f->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
    }

    // move args into local variables
    // (probably possible to avoid this step with a little redesign)
    // TODO: avoid for arguments that aren't assigned
    for(i=0; i < nreq; i++) {
        char *argname = jl_decl_var(jl_cellref(largs,i))->name;
        AllocaInst *lv = localVars[argname];
        Value *argPtr =
            builder.CreateGEP((Value*)&argArray,
                              ConstantInt::get(T_int32, i));
        LoadInst *theArg = builder.CreateLoad(argPtr, false);
        if (isBoxed[argname])
            builder.CreateStore(builder.CreateCall(jlbox_func, theArg), lv);
        else if (lv->getAllocatedType() == jl_pvalue_llvmt)
            builder.CreateStore(theArg, lv);
        else
            builder.CreateStore(emit_unbox(lv->getAllocatedType(),
                                           lv->getType(),
                                           theArg),
                                lv);
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
        AllocaInst *lv = builder.CreateAlloca(jl_pvalue_llvmt, 0, argname);
        if (isBoxed[argname])
            builder.CreateStore(builder.CreateCall(jlbox_func, restTuple), lv);
        else
            builder.CreateStore(restTuple, lv);
        localVars[argname] = lv;
    }

    jl_array_t *stmts = jl_lam_body(ast);
    // associate labels with basic blocks so forward jumps can be resolved
    BasicBlock *prev=NULL;
    for(i=0; i < stmts->length; i++) {
        jl_value_t *ex = jl_cellref(stmts,i);
        if (is_label(ex)) {
            int lname = jl_unbox_int32(jl_exprarg(ex,0));
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
        if (is_label(stmt)) {
            if (prevlabel) continue;
            prevlabel = true;
        }
        else {
            prevlabel = false;
        }
        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == return_sym) {
            jl_expr_t *ex = (jl_expr_t*)stmt;
            builder.CreateRet(boxed(emit_expr(jl_exprarg(ex,0), &ctx, true)));
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
    return jl_new_struct((jl_struct_type_t*)jl_box_any_type, v);
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
#ifdef BITS64
    T_size = T_uint64;
#else
    T_size = T_uint32;
#endif
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

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
    jlsysmod_var = global_to_llvm("jl_system_module", (void*)&jl_system_module);

    std::vector<const Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlerror_func, (void*)&jl_error);
    std::vector<const Type*> te_args(0);
    te_args.push_back(T_pint8);
    te_args.push_back(T_pint8);
    te_args.push_back(jl_pvalue_llvmt);
    te_args.push_back(jl_pvalue_llvmt);
    jltypeerror_func =
        Function::Create(FunctionType::get(T_void, te_args, false),
                         Function::ExternalLinkage,
                         "jl_type_error_rt", jl_Module);
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

    std::vector<const Type*> aargs(0);
#ifdef BITS64
    aargs.push_back(T_uint64);
#else
    aargs.push_back(T_uint32);
#endif
#ifdef BOEHM_GC
    jlalloc_func =
        Function::Create(FunctionType::get(T_pint8, aargs, false),
                         Function::ExternalLinkage,
                         "GC_malloc", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc_func, (void*)&GC_malloc);
#else
    jlalloc_func =
        Function::Create(FunctionType::get(T_pint8, aargs, false),
                         Function::ExternalLinkage,
                         "malloc", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc_func, (void*)&malloc);
#endif

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

    jlconvert_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args4, false),
                         Function::ExternalLinkage,
                         "jl_convert", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlconvert_func, (void*)&jl_convert);

    std::vector<const Type*> args5(0);
    args5.push_back(T_size);
    jlntuple_func =
        Function::Create(FunctionType::get(jl_pvalue_llvmt, args5, true),
                         Function::ExternalLinkage,
                         "jl_tuple", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlntuple_func, (void*)&jl_tuple);

    // set up optimization passes
    FPM = new FunctionPassManager(jl_Module);
    FPM->add(new TargetData(*jl_ExecutionEngine->getTargetData()));
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
    FPM->doInitialization();
}

extern "C" void jl_init_codegen()
{
    InitializeNativeTarget();
    jl_Module = new Module("julia", jl_LLVMContext);
    jl_ExecutionEngine =
        EngineBuilder(jl_Module).setEngineKind(EngineKind::JIT).create();

    init_julia_llvm_env(jl_Module);

    jl_init_intrinsic_functions();
}
