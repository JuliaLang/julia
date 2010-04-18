#include "llvm/DerivedTypes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Intrinsics.h"
#include "llvm/ModuleProvider.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include <cstdio>
#include <string>
#include <sstream>
#include <map>
#include <vector>
using namespace llvm;

extern "C" {
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"
}

// llvm state
static LLVMContext &jl_LLVMContext = getGlobalContext();
static IRBuilder<> builder(getGlobalContext());
static Module *jl_Module;
static ExecutionEngine *jl_ExecutionEngine;
static std::map<const std::string, GlobalVariable*> stringConstants;
static FunctionPassManager *FPM;
static ExistingModuleProvider *JuliaModuleProvider;

// types
static const Type *jl_value_llvmt;
static const Type *jl_pvalue_llvmt;
static const Type *jl_ppvalue_llvmt;
static const Type *jl_function_llvmt;
static const FunctionType *jl_func_sig;
static const Type *jl_fptr_llvmt;
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
static GlobalVariable *jlfunctype_var;

// important functions
static Function *jlerror_func;
static Function *jlgetbindingp_func;
static Function *jltuple_func;
static Function *jlapplygeneric_func;
static Function *jlalloc_func;

// head symbols for each expression type
static jl_sym_t *goto_sym;    static jl_sym_t *goto_ifnot_sym;
static jl_sym_t *label_sym;   static jl_sym_t *return_sym;
static jl_sym_t *lambda_sym;  static jl_sym_t *assign_sym;
static jl_sym_t *null_sym;    static jl_sym_t *body_sym;
static jl_sym_t *unbound_sym; static jl_sym_t *boxunbound_sym;
static jl_sym_t *locals_sym;  static jl_sym_t *colons_sym;
static jl_sym_t *closure_ref_sym;

/*
  plan

  The Simplest Thing That Could Possibly Work:
  * simple code gen for all node types
  * implement all low-level intrinsics
  * instantiate-method to provide static parameters
  * default conversion functions, instantiating conversion functions

  stuff to fix up:
  - discard toplevel wrapper functions
  - gensyms from the front end might conflict with real variables, fix it
  - don't fully instantiate types with typevars, e.g. no Complex[typevar()].new
  - better error messages
  - exceptions
  - threads or other advanced control flow

  - source location tracking, var name metadata
  - rootlist to track pointers emitted into code
  - function/var name mangling
  * include julia-defs.bc in the executable
  - experiment with llvm optimization passes, option to disable them

  optimizations round 1:
  - constants, especially global. resolve functions statically.
  - keep a table mapping fptr to Function* for compiling known calls
  - manually inline simple builtins (tuple,box,boxset,etc.)
  - int and float constant table
  - dispatch optimizations
  * inline space for buffers
  * preallocate boxes for small integers
  - speed up type caching
  - do something about all the string copying from scheme
  - speed up scheme pattern matcher by compiling patterns
  - get llvm interpreter to work, see if it's faster for toplevel expressions

  optimizations round 2:
  - lambda lifting
  - mark pure (builtin) functions and don't call them in statement position
  - avoid tuple allocation in (a,b)=(b,a)
  - varargs and ... optimizations

  optimizations round 3:
  - type inference
  - mark non-null references and avoid null check
  - static method lookup
  - inlining
  - unboxing

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
                                   "a_julia_function", jl_Module);
    assert(jl_is_expr(li->ast));
    emit_function(li, f);
    verifyFunction(*f);
    FPM->run(*f);
    // print out the function's LLVM code
    //f->dump();
    return f;
}

extern "C" void jl_compile(jl_lambda_info_t *li)
{
    // objective: assign li->fptr
    Function *f = to_function(li);
    li->fptr = (jl_fptr_t)jl_ExecutionEngine->getPointerToFunction(f);
}

// get array of formal argument expressions
static jl_buffer_t *lam_args(jl_expr_t *l)
{
    assert(l->head == lambda_sym);
    jl_value_t *ae = ((jl_value_t**)l->args->data)[0];
    if (ae == (jl_value_t*)jl_null) return jl_the_empty_buffer;
    assert(jl_is_expr(ae));
    assert(((jl_expr_t*)ae)->head == list_sym);
    return ((jl_expr_t*)ae)->args;
}

// get array of local var symbols
static jl_buffer_t *lam_locals(jl_expr_t *l)
{
    jl_value_t *le = ((jl_value_t**)l->args->data)[1];
    assert(jl_is_expr(le));
    jl_expr_t *lle = ((jl_expr_t**)((jl_expr_t*)le)->args->data)[0];
    assert(jl_is_expr(lle));
    assert(lle->head == locals_sym);
    return lle->args;
}

// get array of body forms
static jl_buffer_t *lam_body(jl_expr_t *l)
{
    jl_value_t *be = ((jl_value_t**)l->args->data)[2];
    assert(jl_is_expr(be));
    assert(((jl_expr_t*)be)->head == body_sym);
    return ((jl_expr_t*)be)->args;
}

static jl_sym_t *decl_var(jl_value_t *ex)
{
    if (jl_is_symbol(ex)) return (jl_sym_t*)ex;
    assert(jl_is_expr(ex));
    return ((jl_sym_t**)((jl_expr_t*)ex)->args->data)[0];
}

static int is_rest_arg(jl_value_t *ex)
{
    if (!jl_is_expr(ex)) return 0;
    if (((jl_expr_t*)ex)->head != colons_sym) return 0;
    jl_expr_t *atype = ((jl_expr_t**)((jl_expr_t*)ex)->args->data)[1];
    if (!jl_is_expr(atype)) return 0;
    if (atype->head != call_sym ||
        atype->args->length != 3)
        return 0;
    if (((jl_sym_t**)atype->args->data)[1] != dots_sym)
        return 0;
    return 1;
}

// information about the context of a piece of code: its enclosing
// function and module, and visible local variables and labels.
typedef struct {
    Function *f;
    std::map<std::string, AllocaInst*> *vars;
    std::map<std::string, BasicBlock*> *labels;
    jl_module_t *module;
    jl_expr_t *ast;
    jl_tuple_t *sp;
    const Argument *envArg;
    const Argument *argArray;
    const Argument *argCount;
} jl_codectx_t;

static Value *literal_pointer_val(jl_value_t *p)
{
#ifdef BITS64
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int64, (uint64_t)p),
                                     jl_pvalue_llvmt);
#else
    return ConstantExpr::getIntToPtr(ConstantInt::get(T_int32, (uint32_t)p),
                                     jl_pvalue_llvmt);
#endif
}

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

static Value *emit_typeof(Value *p)
{
    // given p, a jl_value_t*, compute its type tag
    Value *tt = builder.CreateBitCast(p, jl_ppvalue_llvmt);
    tt = builder.CreateLoad(builder.CreateGEP(tt,ConstantInt::get(T_int32,0)),
                            false);
    return tt;
}

static void emit_error(const std::string &txt)
{
    std::vector<Value *> zeros(0);
    zeros.push_back(ConstantInt::get(T_int32, 0));
    zeros.push_back(ConstantInt::get(T_int32, 0));
    builder.CreateCall(jlerror_func,
                       builder.CreateGEP(stringConst(txt),
                                         zeros.begin(), zeros.end()));
}

static void emit_typecheck(Value *x, jl_value_t *type, const std::string &msg,
                           jl_codectx_t *ctx)
{
    Value *istype =
        builder.CreateICmpEQ(emit_typeof(x), literal_pointer_val(type));
    BasicBlock *elseBB = BasicBlock::Create(getGlobalContext(),"a",ctx->f);
    BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(),"b");
    builder.CreateCondBr(istype, mergeBB, elseBB);
    builder.SetInsertPoint(elseBB);
    emit_error(msg);
    builder.CreateBr(mergeBB);
    ctx->f->getBasicBlockList().push_back(mergeBB);
    builder.SetInsertPoint(mergeBB);
}

static Value *emit_nthptr(Value *v, size_t n)
{
    // p = (jl_value_t**)v; p[n]
    Value *vptr =
        builder.CreateGEP(builder.CreateBitCast(v, jl_ppvalue_llvmt),
                          ConstantInt::get(T_int32, n));
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
    assert(jl_is_symbol(s));
    AllocaInst *l = (*ctx->vars)[s->name];
    if (l != NULL) {
        return l;
    }
    return globalvar_binding_pointer(s, ctx);
}

static int is_global(jl_sym_t *s, jl_codectx_t *ctx)
{
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
    emit_error(msg);
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

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool value,
                        bool last=false);

#include "intrinsics.cpp"

static Value *emit_expr(jl_value_t *expr, jl_codectx_t *ctx, bool value,
                        bool last)
{
    if (jl_is_symbol(expr)) {
        // variable
        jl_sym_t *sym = (jl_sym_t*)expr;
        if (is_global(sym, ctx)) {
            size_t i;
            // look for static parameter
            for(i=0; i < ctx->sp->length; i+=2) {
                if (sym == (jl_sym_t*)jl_tupleref(ctx->sp, i)) {
                    return literal_pointer_val(jl_tupleref(ctx->sp, i+1));
                }
            }
        }
        Value *bp = var_binding_pointer(sym, ctx);
        return emit_checked_var(bp, sym->name, ctx);
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
        else if (jl_is_buffer(expr)) {
            // string literal
        }
        // TODO: for now just return the direct pointer
        return literal_pointer_val(expr);
        assert(0);
    }
    jl_expr_t *ex = (jl_expr_t*)expr;
    jl_value_t **args = (jl_value_t**)ex->args->data;
    // this is object-disoriented.
    // however, this is a good way to do it because it should *not* be easy
    // to add new node types.
    if (ex->head == goto_sym) {
        assert(!value);
        char *labelname = ((jl_sym_t*)args[0])->name;
        BasicBlock *bb = (*ctx->labels)[labelname];
        assert(bb);
        builder.CreateBr(bb);
    }
    else if (ex->head == goto_ifnot_sym) {
        assert(!value);
        jl_value_t *cond = args[0];
        char *labelname = ((jl_sym_t*)args[1])->name;
        Value *isfalse =
            builder.CreateICmpEQ(emit_expr(cond, ctx, true),
                                 literal_pointer_val(jl_false));
        BasicBlock *ifso = BasicBlock::Create(getGlobalContext(), "if", ctx->f);
        BasicBlock *ifnot = (*ctx->labels)[labelname];
        assert(ifnot);
        builder.CreateCondBr(isfalse, ifnot, ifso);
        builder.SetInsertPoint(ifso);
    }
    else if (ex->head == label_sym) {
        assert(!value);
        char *labelname = ((jl_sym_t*)args[0])->name;
        BasicBlock *bb = (*ctx->labels)[labelname];
        assert(bb);
        if (builder.GetInsertBlock()->getTerminator() == NULL) {
            builder.CreateBr(bb); // all BasicBlocks must exit explicitly
        }
        ctx->f->getBasicBlockList().push_back(bb);
        builder.SetInsertPoint(bb);
    }

    else if (ex->head == return_sym) {
        assert(!value);
        builder.CreateRet(boxed(emit_expr(args[0], ctx, true)));
        if (!last) {
            // basic block must end here because there's a return
            BasicBlock *bb =
                BasicBlock::Create(getGlobalContext(), "ret", ctx->f);
            builder.SetInsertPoint(bb);
        }
    }
    else if (ex->head == call_sym) {
        size_t nargs = ex->args->length-1;
        Value *theFptr, *theEnv;
        jl_binding_t *b=NULL; bool done=false;
        if (jl_is_symbol(args[0]) && is_global((jl_sym_t*)args[0], ctx) &&
            jl_boundp(ctx->module, (jl_sym_t*)args[0]) &&
            (b=jl_get_binding(ctx->module, (jl_sym_t*)args[0]))->constp) {
            // TODO: handle (top x) as well
            // head is a constant global
            if (jl_typeis(b->value, jl_intrinsic_type)) {
                return emit_intrinsic((intrinsic)*(uint32_t*)jl_bits_data(b->value),
                                      args, nargs, ctx);
            }
            if (jl_is_func(b->value)) {
                jl_function_t *f = (jl_function_t*)b->value;
                if (f->fptr == &jl_apply_generic) {
                    theFptr = jlapplygeneric_func;
                    //theFptr = literal_pointer_val((void*)f->fptr, jl_fptr_llvmt);
                    theEnv = literal_pointer_val(f->env);
                    done = true;
                }
            }
        }
        if (!done) {
            Value *theFunc = emit_expr(args[0], ctx, true);
            emit_typecheck(theFunc, (jl_value_t*)jl_any_func,
                           "apply: expected function.", ctx);
            // extract pieces of the function object
            // TODO: try extractelement instead
            theFptr =
                builder.CreateBitCast(emit_nthptr(theFunc, 1), jl_fptr_llvmt);
            theEnv = emit_nthptr(theFunc, 2);
        }
        // emit arguments
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
        // call
        Value *result = builder.CreateCall3(theFptr, theEnv, argl,
                                            ConstantInt::get(T_int32,nargs));
        // restore stack
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stackrestore),
                           stacksave);
        return result;
    }

    else if (ex->head == assign_sym) {
        assert(!value);
        Value *rhs = emit_expr(args[1], ctx, true);
        Value *bp = var_binding_pointer((jl_sym_t*)args[0], ctx);
        builder.CreateStore(boxed(rhs), bp);
    }
    else if (ex->head == top_sym) {
        Value *bp = globalvar_binding_pointer((jl_sym_t*)args[0], ctx);
        return emit_checked_var(bp, ((jl_sym_t*)args[0])->name, ctx);
    }
    else if (ex->head == unbound_sym) {
        Value *bp = var_binding_pointer((jl_sym_t*)args[0], ctx);
        Value *v = builder.CreateLoad(bp, false);
        Value *isnull = builder.CreateICmpEQ(v, V_null);
        return julia_bool(isnull);
    }
    else if (ex->head == boxunbound_sym) {
        Value *box = emit_expr(args[0], ctx, true);
        Value *contents = emit_nthptr(box, 1);
        Value *isnull = builder.CreateICmpEQ(contents, V_null);
        return builder.CreateSelect(isnull,
                                    literal_pointer_val(jl_true),
                                    literal_pointer_val(jl_false));
    }
    else if (ex->head == closure_ref_sym) {
        int idx = jl_unbox_int32(args[0]);
        return emit_nthptr((Value*)ctx->envArg, idx+2);
    }

    else if (ex->head == quote_sym) {
        jl_value_t *jv = args[0];
        if (jl_is_lambda_info(jv)) {
            jv = (jl_value_t*)jl_add_static_parameters((jl_lambda_info_t*)jv,
                                                       ctx->sp);
        }
        return literal_pointer_val(jv);
    }
    else if (ex->head == null_sym) {
        return literal_pointer_val((jl_value_t*)jl_null);
    }
    assert(!value);
    return NULL;
}

#define is_label(ex) (jl_is_expr(ex) && ((jl_expr_t*)ex)->head == label_sym)

static void emit_function(jl_lambda_info_t *lam, Function *f)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    //jl_print((jl_value_t*)ast);
    //ios_printf(ios_stdout, "\n");
    BasicBlock *b0 = BasicBlock::Create(jl_LLVMContext, "top", f);
    builder.SetInsertPoint(b0);
    std::map<std::string, AllocaInst*> localVars;
    std::map<std::string, BasicBlock*> labels;
    jl_buffer_t *largs = lam_args(ast);
    jl_buffer_t *lvars = lam_locals(ast);
    Function::arg_iterator AI = f->arg_begin();
    const Argument &envArg = *AI++;
    const Argument &argArray = *AI++;
    const Argument &argCount = *AI++;
    jl_codectx_t ctx;
    ctx.f = f;
    ctx.vars = &localVars;
    ctx.labels = &labels;
    ctx.module = jl_system_module; //TODO
    ctx.ast = ast;
    ctx.sp = lam->sparams;
    ctx.envArg = &envArg;
    ctx.argArray = &argArray;
    ctx.argCount = &argCount;

    // allocate local variables
    // must be first for the mem2reg pass to work
    size_t i;
    for(i=0; i < lvars->length; i++) {
        char *argname = ((jl_sym_t**)lvars->data)[i]->name;
        AllocaInst *lv = builder.CreateAlloca(jl_pvalue_llvmt, 0, argname);
        builder.CreateStore(V_null, lv);
        localVars[argname] = lv;
    }
    for(i=0; i < largs->length; i++) {
        char *argname = decl_var(((jl_value_t**)largs->data)[i])->name;
        AllocaInst *lv = builder.CreateAlloca(jl_pvalue_llvmt, 0, argname);
        localVars[argname] = lv;
    }

    // check arg count
    size_t nreq = largs->length;
    int va = 0;
    if (nreq > 0 && is_rest_arg(((jl_value_t**)largs->data)[nreq-1])) {
        nreq--;
        va = 1;
        Value *enough =
            builder.CreateICmpUGE((Value*)&argCount,
                                  ConstantInt::get(T_int32, nreq));
        BasicBlock *elseBB = BasicBlock::Create(getGlobalContext(), "else", f);
        BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
        builder.CreateCondBr(enough, mergeBB, elseBB);
        builder.SetInsertPoint(elseBB);
        emit_error("too few arguments");
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
        emit_error("wrong number of arguments");
        builder.CreateBr(mergeBB);
        f->getBasicBlockList().push_back(mergeBB);
        builder.SetInsertPoint(mergeBB);
    }

    // move args into local variables
    // (probably possible to avoid this step with a little redesign)
    // TODO: avoid for arguments that aren't assigned
    for(i=0; i < nreq; i++) {
        char *argname = decl_var(((jl_value_t**)largs->data)[i])->name;
        AllocaInst *lv = localVars[argname];
        Value *argPtr =
            builder.CreateGEP((Value*)&argArray,
                              ConstantInt::get(T_int32, i));
        LoadInst *theArg = builder.CreateLoad(argPtr, false);
        builder.CreateStore(theArg, lv);
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
        char *argname = decl_var(((jl_value_t**)largs->data)[nreq])->name;
        AllocaInst *lv = builder.CreateAlloca(jl_pvalue_llvmt, 0, argname);
        builder.CreateStore(restTuple, lv);
        localVars[argname] = lv;
    }

    jl_buffer_t *stmts = lam_body(ast);
    // associate labels with basic blocks so forward jumps can be resolved
    BasicBlock *prev=NULL;
    for(i=0; i < stmts->length; i++) {
        jl_value_t *ex = ((jl_value_t**)stmts->data)[i];
        if (is_label(ex)) {
            char *lname = ((jl_sym_t**)((jl_expr_t*)ex)->args->data)[0]->name;
            if (prev != NULL) {
                // fuse consecutive labels
                labels[lname] = prev;
            }
            else {
                prev = BasicBlock::Create(getGlobalContext(), lname);
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
        jl_value_t *stmt = ((jl_value_t**)stmts->data)[i];
        if (is_label(stmt)) {
            if (prevlabel) continue;
            prevlabel = true;
        }
        else {
            prevlabel = false;
        }
        (void)emit_expr(stmt, &ctx, false, i==stmts->length-1);
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

static const char julia_defs_file[] = {
#include "julia-defs.s.bc.inc"
    , 0x0
};

static void init_julia_llvm_env(Module *m)
{
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
    T_float32 = Type::getFloatTy(getGlobalContext());
    T_pfloat32 = PointerType::get(T_float32, 0);
    T_float64 = Type::getDoubleTy(getGlobalContext());
    T_pfloat64 = PointerType::get(T_float64, 0);
    T_void = Type::getVoidTy(jl_LLVMContext);

    // add needed base definitions to our LLVM environment
    const char *jdf = &julia_defs_file[0];
    MemoryBuffer *deffile = MemoryBuffer::getMemBuffer(jdf, jdf+sizeof(julia_defs_file)-1);
    Module *jdefs = ParseBitcodeFile(deffile, getGlobalContext());
    delete deffile;

    jl_value_llvmt = jdefs->getTypeByName("struct._jl_value_t");
    jl_pvalue_llvmt = PointerType::get(jl_value_llvmt, 0);
    jl_ppvalue_llvmt = PointerType::get(jl_pvalue_llvmt, 0);
    V_null = Constant::getNullValue(jl_pvalue_llvmt);
    jl_func_sig = dynamic_cast<const FunctionType*>(jdefs->getTypeByName("jl_callable_t"));
    assert(jl_func_sig != NULL);
    jl_fptr_llvmt = PointerType::get(jl_func_sig, 0);
    jl_function_llvmt = jdefs->getTypeByName("jl_function_t");

    jltrue_var = global_to_llvm("jl_true", (void*)&jl_true);
    jlfalse_var = global_to_llvm("jl_false", (void*)&jl_false);
    jlnull_var = global_to_llvm("jl_null", (void*)&jl_null);
    jlsysmod_var = global_to_llvm("jl_system_module", (void*)&jl_system_module);
    jlfunctype_var = global_to_llvm("jl_any_func", (void*)&jl_any_func);

    std::vector<const Type*> args1(0);
    args1.push_back(T_pint8);
    jlerror_func =
        Function::Create(FunctionType::get(T_void, args1, false),
                         Function::ExternalLinkage,
                         "jl_error", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlerror_func, (void*)&jl_error);

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
#ifdef NO_BOEHM_GC
    jlalloc_func =
        Function::Create(FunctionType::get(T_pint8, aargs, false),
                         Function::ExternalLinkage,
                         "malloc", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc_func, (void*)&malloc);
#else
    jlalloc_func =
        Function::Create(FunctionType::get(T_pint8, aargs, false),
                         Function::ExternalLinkage,
                         "GC_malloc", jl_Module);
    jl_ExecutionEngine->addGlobalMapping(jlalloc_func, (void*)&GC_malloc);
#endif

    // set up optimization passes
    JuliaModuleProvider = new ExistingModuleProvider(jl_Module);
    FPM = new FunctionPassManager(JuliaModuleProvider);
    FPM->add(new TargetData(*jl_ExecutionEngine->getTargetData()));
    FPM->add(createPromoteMemoryToRegisterPass());
    FPM->add(createInstructionCombiningPass());
    FPM->add(createReassociatePass());
    FPM->add(createGVNPass());
    FPM->add(createCFGSimplificationPass());
    FPM->doInitialization();
}

extern "C" void jl_init_codegen()
{
    InitializeNativeTarget();
    jl_Module = new Module("julia", jl_LLVMContext);
    jl_ExecutionEngine =
        EngineBuilder(jl_Module).setEngineKind(EngineKind::JIT).create();

    init_julia_llvm_env(jl_Module);

    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("goto-ifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    assign_sym = jl_symbol("=");
    null_sym = jl_symbol("null");
    unbound_sym = jl_symbol("unbound");
    boxunbound_sym = jl_symbol("box-unbound");
    closure_ref_sym = jl_symbol("closure-ref");
    body_sym = jl_symbol("body");
    locals_sym = jl_symbol("locals");
    colons_sym = jl_symbol("::");
}
