// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  AST
  interface to front-end, obtains and translates syntax trees
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "flisp.h"

#ifdef __cplusplus
extern "C" {
#endif

// MSVC complains about "julia_flisp.boot.inc : error C4335: Mac file format
// detected: please convert the source file to either DOS or UNIX format"
#ifdef _MSC_VER
#pragma warning(disable:4335)
#endif

static uint8_t flisp_system_image[] = {
#include <julia_flisp.boot.inc>
};

typedef struct _jl_ast_context_list_t {
    struct _jl_ast_context_list_t *next;
    struct _jl_ast_context_list_t **prev;
} jl_ast_context_list_t;

STATIC_INLINE void jl_ast_context_list_insert(jl_ast_context_list_t **head,
                                              jl_ast_context_list_t *node)
{
    jl_ast_context_list_t *next = *head;
    if (next)
        next->prev = &node->next;
    node->next = next;
    node->prev = head;
    *head = node;
}

STATIC_INLINE void jl_ast_context_list_delete(jl_ast_context_list_t *node)
{
    if (node->next)
        node->next->prev = node->prev;
    *node->prev = node->next;
}

typedef struct _jl_ast_context_t {
    fl_context_t fl;
    fltype_t *jvtype;

    value_t true_sym;
    value_t false_sym;
    value_t error_sym;
    value_t null_sym;
    value_t jlgensym_sym;
    value_t slot_sym;
    arraylist_t gensym_to_flisp;
    jl_ast_context_list_t list;
    int ref;
    jl_task_t *task;
    // use a pointer to a stack slot so that we can detect if
    // `jl_ast_preserve` is called in the wrong context.
    // If `roots` is not NULL, it always points to a rooted stack slot.
    jl_array_t **roots;
} jl_ast_context_t;

static jl_ast_context_t jl_ast_main_ctx;

#define jl_ast_ctx(fl_ctx) container_of(fl_ctx, jl_ast_context_t, fl)
#define jl_ast_context_list_item(node)          \
    container_of(node, jl_ast_context_t, list)

#define JL_AST_PRESERVE_PUSH(ctx, _roots, old_roots)    \
    jl_array_t *_roots = NULL;                          \
    jl_array_t **old_roots = ctx->roots;                \
    ctx->roots = &_roots;                               \
    JL_GC_PUSH1(&_roots)
#define JL_AST_PRESERVE_POP(ctx, old_roots)     \
    JL_GC_POP();                                \
    ctx->roots = old_roots

static void jl_ast_preserve(fl_context_t *fl_ctx, jl_value_t *obj)
{
    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    assert(ctx->roots);
    jl_array_t *roots = *ctx->roots;
    if (!roots) {
        roots = *ctx->roots = jl_alloc_cell_1d(1);
        jl_cellset(roots, 0, obj);
    }
    else {
        jl_cell_1d_push(roots, obj);
    }
}

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, int expronly);
static value_t julia_to_scm(fl_context_t *fl_ctx, jl_value_t *v);

value_t fl_defined_julia_global(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    // tells whether a var is defined in and *by* the current module
    argcount(fl_ctx, "defined-julia-global", nargs, 1);
    (void)tosymbol(fl_ctx, args[0], "defined-julia-global");
    if (jl_current_module == NULL)
        return fl_ctx->F;
    jl_sym_t *var = jl_symbol(symbol_name(fl_ctx, args[0]));
    jl_binding_t *b =
        (jl_binding_t*)ptrhash_get(&jl_current_module->bindings, var);
    return (b != HT_NOTFOUND && b->owner==jl_current_module) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_current_julia_module(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    value_t opaque = cvalue(fl_ctx, jl_ast_ctx(fl_ctx)->jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = (jl_value_t*)jl_current_module;
    return opaque;
}

value_t fl_current_module_counter(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    static uint32_t fallback_counter = 0;
    if (jl_current_module == NULL)
        return fixnum(++fallback_counter);
    else
        return fixnum(jl_module_next_counter(jl_current_module));
}

value_t fl_invoke_julia_macro(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs < 1)
        argcount(fl_ctx, "invoke-julia-macro", nargs, 1);
    jl_value_t *f = NULL;
    jl_value_t **margs;
    // Reserve one more slot for the result
    JL_GC_PUSHARGS(margs, nargs + 1);
    int i;
    for(i=1; i < nargs; i++) margs[i] = scm_to_julia(fl_ctx, args[i], 1);
    jl_value_t *result = NULL;

    JL_TRY {
        margs[0] = scm_to_julia(fl_ctx, args[0], 1);
        margs[0] = jl_toplevel_eval(margs[0]);
        f = margs[0];
        margs[nargs] = result = jl_apply_generic(margs, nargs);
    }
    JL_CATCH {
        JL_GC_POP();
        value_t opaque = cvalue(fl_ctx, jl_ast_ctx(fl_ctx)->jvtype, sizeof(void*));
        *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = jl_exception_in_transit;
        return fl_list2(fl_ctx, jl_ast_ctx(fl_ctx)->error_sym, opaque);
    }
    // protect result from GC, otherwise it could be freed during future
    // macro expansions, since it will be referenced only from scheme and
    // not julia.
    // all calls to invoke-julia-macro happen under `jl_macroexpand`,
    // `jl_expand` or `jl_parse_eval_all` so the preserved array is rooted there.
    assert(result != NULL);
    jl_ast_preserve(fl_ctx, result);
    value_t scm = julia_to_scm(fl_ctx, result);
    fl_gc_handle(fl_ctx, &scm);
    value_t scmresult;
    jl_module_t *defmod = jl_gf_mtable(f)->module;
    if (defmod == NULL || defmod == jl_current_module) {
        scmresult = fl_cons(fl_ctx, scm, fl_ctx->F);
    }
    else {
        value_t opaque = cvalue(fl_ctx, jl_ast_ctx(fl_ctx)->jvtype, sizeof(void*));
        *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = (jl_value_t*)defmod;
        scmresult = fl_cons(fl_ctx, scm, opaque);
    }
    fl_free_gc_handles(fl_ctx, 1);

    JL_GC_POP();
    return scmresult;
}

static const builtinspec_t julia_flisp_ast_ext[] = {
    { "defined-julia-global", fl_defined_julia_global },
    { "invoke-julia-macro", fl_invoke_julia_macro },
    { "current-julia-module", fl_current_julia_module },
    { "current-julia-module-counter", fl_current_module_counter },
    { NULL, NULL }
};

static int jl_parse_deperror(fl_context_t *fl_ctx, int err);
static int jl_parse_depwarn_(fl_context_t *fl_ctx, int warn);

static void jl_init_ast_ctx(jl_ast_context_t *ast_ctx)
{
    fl_context_t *fl_ctx = &ast_ctx->fl;
    fl_init(fl_ctx, 4*1024*1024);

    if (fl_load_system_image_str(fl_ctx, (char*)flisp_system_image,
                                 sizeof(flisp_system_image))) {
        jl_error("fatal error loading system image");
    }

    fl_applyn(fl_ctx, 0, symbol_value(symbol(fl_ctx, "__init_globals")));

    jl_ast_ctx(fl_ctx)->jvtype = define_opaque_type(fl_ctx->jl_sym, sizeof(void*), NULL, NULL);
    assign_global_builtins(fl_ctx, julia_flisp_ast_ext);
    jl_ast_ctx(fl_ctx)->true_sym = symbol(fl_ctx, "true");
    jl_ast_ctx(fl_ctx)->false_sym = symbol(fl_ctx, "false");
    jl_ast_ctx(fl_ctx)->error_sym = symbol(fl_ctx, "error");
    jl_ast_ctx(fl_ctx)->null_sym = symbol(fl_ctx, "null");
    jl_ast_ctx(fl_ctx)->jlgensym_sym = symbol(fl_ctx, "jlgensym");
    jl_ast_ctx(fl_ctx)->slot_sym = symbol(fl_ctx, "slot");

    // Enable / disable syntax deprecation warnings
    // Disable in imaging mode to avoid i/o errors (#10727)
    if (jl_generating_output())
        jl_parse_depwarn_(fl_ctx, 0);
    else if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR)
        jl_parse_deperror(fl_ctx, 1);
    else
        jl_parse_depwarn_(fl_ctx, (int)jl_options.depwarn);
}

// There should be no GC allocation while holding this lock
JL_DEFINE_MUTEX(flisp)
static jl_ast_context_list_t *jl_ast_ctx_using = NULL;
static jl_ast_context_list_t *jl_ast_ctx_freed = NULL;

static jl_ast_context_t *jl_ast_ctx_enter(void)
{
    JL_LOCK_NOGC(flisp);
    jl_ast_context_list_t *node;
    jl_ast_context_t *ctx;
    // First check if the current task is using one of the contexts
    for (node = jl_ast_ctx_using;node;(node = node->next)) {
        ctx = jl_ast_context_list_item(node);
        if (ctx->task == jl_current_task) {
            ctx->ref++;
            JL_UNLOCK_NOGC(flisp);
            return ctx;
        }
    }
    // If not, grab one from the free list
    if ((node = jl_ast_ctx_freed)) {
        jl_ast_context_list_delete(node);
        jl_ast_context_list_insert(&jl_ast_ctx_using, node);
        ctx = jl_ast_context_list_item(node);
        ctx->ref = 1;
        ctx->task = jl_current_task;
        ctx->roots = NULL;
        JL_UNLOCK_NOGC(flisp);
        return ctx;
    }
    // Construct a new one if we can't find any
    ctx = (jl_ast_context_t*)calloc(1, sizeof(jl_ast_context_t));
    // ctx->roots is NULL already due to calloc.
    ctx->ref = 1;
    ctx->task = jl_current_task;
    node = &ctx->list;
    jl_ast_context_list_insert(&jl_ast_ctx_using, node);
    JL_UNLOCK_NOGC(flisp);
    jl_init_ast_ctx(ctx);
    return ctx;
}

static void jl_ast_ctx_leave(jl_ast_context_t *ctx)
{
    if (--ctx->ref)
        return;
    JL_LOCK_NOGC(flisp);
    ctx->task = NULL;
    jl_ast_context_list_t *node = &ctx->list;
    jl_ast_context_list_delete(node);
    jl_ast_context_list_insert(&jl_ast_ctx_freed, node);
    JL_UNLOCK_NOGC(flisp);
}

void jl_init_frontend(void)
{
    if (jl_ast_ctx_using || jl_ast_ctx_freed)
        return;
    jl_ast_main_ctx.ref = 1;
    jl_ast_main_ctx.task = jl_current_task;
    jl_ast_context_list_insert(&jl_ast_ctx_using, &jl_ast_main_ctx.list);
    jl_init_ast_ctx(&jl_ast_main_ctx);
    jl_ast_ctx_leave(&jl_ast_main_ctx);
}

JL_DLLEXPORT void jl_lisp_prompt(void)
{
    jl_init_frontend();
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    JL_AST_PRESERVE_PUSH(ctx, roots, old_roots);
    fl_context_t *fl_ctx = &ctx->fl;
    fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "__start")), fl_cons(fl_ctx, fl_ctx->NIL,fl_ctx->NIL));
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
}

static jl_sym_t *scmsym_to_julia(fl_context_t *fl_ctx, value_t s)
{
    assert(issymbol(s));
    if (fl_isgensym(fl_ctx, s)) {
        static char gsname[16];
        char *n = uint2str(&gsname[1], sizeof(gsname)-1,
                           ((gensym_t*)ptr(s))->id, 10);
        *(--n) = '#';
        return jl_symbol(n);
    }
    return jl_symbol(symbol_name(fl_ctx, s));
}

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, int expronly);

static jl_svec_t *full_svec(fl_context_t *fl_ctx, value_t e, int expronly)
{
    size_t ln = llength(e);
    if (ln == 0) return jl_emptysvec;
    jl_svec_t *ar = jl_alloc_svec_uninit(ln);
    JL_GC_PUSH1(&ar);
    size_t i=0;
    while (iscons(e)) {
        jl_svecset(ar, i, scm_to_julia_(fl_ctx, car_(e), expronly));
        e = cdr_(e);
        i++;
    }
    JL_GC_POP();
    return ar;
}

static jl_value_t *full_list(fl_context_t *fl_ctx, value_t e, int expronly)
{
    size_t ln = llength(e);
    if (ln == 0) return jl_an_empty_cell;
    jl_array_t *ar = jl_alloc_cell_1d(ln);
    JL_GC_PUSH1(&ar);
    size_t i=0;
    while (iscons(e)) {
        jl_cellset(ar, i, scm_to_julia_(fl_ctx, car_(e), expronly));
        e = cdr_(e);
        i++;
    }
    JL_GC_POP();
    return (jl_value_t*)ar;
}

static jl_value_t *full_list_of_lists(fl_context_t *fl_ctx, value_t e, int expronly)
{
    size_t ln = llength(e);
    if (ln == 0) return jl_an_empty_cell;
    jl_array_t *ar = jl_alloc_cell_1d(ln);
    JL_GC_PUSH1(&ar);
    size_t i=0;
    while (iscons(e)) {
        jl_cellset(ar, i, full_list(fl_ctx, car_(e),expronly));
        e = cdr_(e);
        i++;
    }
    JL_GC_POP();
    return (jl_value_t*)ar;
}

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, int expronly)
{
    jl_value_t *v = NULL;
    JL_GC_PUSH1(&v);
    JL_TRY {
        v = scm_to_julia_(fl_ctx, e, expronly);
    }
    JL_CATCH {
        // if expression cannot be converted, replace with error expr
        jl_expr_t *ex = jl_exprn(error_sym, 1);
        v = (jl_value_t*)ex;
        jl_cellset(ex->args, 0, jl_cstr_to_string("invalid AST"));
    }
    JL_GC_POP();
    return v;
}

extern int64_t conv_to_int64(void *data, numerictype_t tag);

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, int eo)
{
    if (fl_isnumber(fl_ctx, e)) {
        int64_t i64;
        if (isfixnum(e)) {
            i64 = numval(e);
        }
        else {
            assert(iscprim(e));
            cprim_t *cp = (cprim_t*)ptr(e);
            numerictype_t nt = cp_numtype(cp);
            switch (nt) {
            case T_DOUBLE:
                return (jl_value_t*)jl_box_float64(*(double*)cp_data(cp));
            case T_FLOAT:
                return (jl_value_t*)jl_box_float32(*(float*)cp_data(cp));
            case T_UINT8:
                return (jl_value_t*)jl_box_uint8(*(uint8_t*)cp_data(cp));
            case T_UINT16:
                return (jl_value_t*)jl_box_uint16(*(uint16_t*)cp_data(cp));
            case T_UINT32:
                return (jl_value_t*)jl_box_uint32(*(uint32_t*)cp_data(cp));
            case T_UINT64:
                return (jl_value_t*)jl_box_uint64(*(uint64_t*)cp_data(cp));
            default:
                ;
            }
            i64 = conv_to_int64(cp_data(cp), nt);
        }
#ifdef _P64
        return (jl_value_t*)jl_box_int64(i64);
#else
        if (i64 > (int64_t)S32_MAX || i64 < (int64_t)S32_MIN)
            return (jl_value_t*)jl_box_int64(i64);
        else
            return (jl_value_t*)jl_box_int32((int32_t)i64);
#endif
    }
    if (issymbol(e)) {
        if (e == jl_ast_ctx(fl_ctx)->true_sym)
            return jl_true;
        else if (e == jl_ast_ctx(fl_ctx)->false_sym)
            return jl_false;
        return (jl_value_t*)scmsym_to_julia(fl_ctx, e);
    }
    if (fl_isstring(fl_ctx, e))
        return jl_pchar_to_string((char*)cvalue_data(e), cvalue_len(e));
    if (e == fl_ctx->F)
        return jl_false;
    if (e == fl_ctx->T)
        return jl_true;
    if (iscons(e) || e == fl_ctx->NIL) {
        value_t hd;
        jl_sym_t *sym;
        if (e == fl_ctx->NIL) {
            hd = e;
        }
        else {
            hd = car_(e);
            if (hd == jl_ast_ctx(fl_ctx)->jlgensym_sym)
                return jl_box_gensym(numval(car_(cdr_(e))));
            else if (hd == jl_ast_ctx(fl_ctx)->slot_sym)
                return jl_new_struct(jl_slot_type, jl_box_long(numval(car_(cdr_(e)))), jl_any_type);
            else if (hd == jl_ast_ctx(fl_ctx)->null_sym && llength(e) == 1)
                return jl_nothing;
        }
        if (issymbol(hd))
            sym = scmsym_to_julia(fl_ctx, hd);
        else
            sym = list_sym;
        size_t n = llength(e)-1;
        size_t i;
        if (sym == lambda_sym) {
            jl_expr_t *ex = jl_exprn(lambda_sym, n);
            jl_svec_t *tvars = NULL;
            jl_array_t *vinf = NULL;
            jl_lambda_info_t *nli = NULL;
            JL_GC_PUSH4(&ex, &tvars, &vinf, &nli);
            e = cdr_(e);
            value_t largs = car_(e);
            jl_cellset(ex->args, 0, full_list(fl_ctx, largs, eo));
            e = cdr_(e);

            value_t ee = car_(e);
            vinf = jl_alloc_cell_1d(3);
            jl_cellset(vinf, 0, full_list_of_lists(fl_ctx, car_(ee), eo));
            ee = cdr_(ee);
            jl_cellset(vinf, 1, full_list_of_lists(fl_ctx, car_(ee), eo));
            ee = cdr_(ee);
            jl_cellset(vinf, 2, isfixnum(car_(ee)) ?
                       jl_box_long(numval(car_(ee))) :
                       full_list(fl_ctx,car_(ee),eo));
            ee = cdr_(ee);
            tvars = full_svec(fl_ctx, car_(ee), eo);
            assert(!iscons(cdr_(ee)));
            jl_cellset(ex->args, 1, vinf);
            e = cdr_(e);

            for(i=2; i < n; i++) {
                assert(iscons(e));
                jl_cellset(ex->args, i, scm_to_julia_(fl_ctx, car_(e), eo));
                e = cdr_(e);
            }
            nli = jl_new_lambda_info((jl_value_t*)ex, tvars, jl_emptysvec, jl_current_module);
            jl_preresolve_globals((jl_value_t*)nli, nli);
            JL_GC_POP();
            return (jl_value_t*)nli;
        }

        if (issymbol(hd))
            e = cdr_(e);
        else
            n++;
        if (!eo) {
            if (sym == line_sym && n==2) {
                // NOTE: n==3 case exists: '(line, linenum, filename, funcname) passes
                //       the original name through to keyword-arg specializations.
                //       See 'line handling in julia-syntax.scm:keywords-method-def-expr
                jl_value_t *filename = NULL, *linenum = NULL;
                JL_GC_PUSH2(&filename, &linenum);
                filename = scm_to_julia_(fl_ctx, car_(cdr_(e)), 0);
                linenum  = scm_to_julia_(fl_ctx, car_(e), 0);
                jl_value_t *temp = jl_new_struct(jl_linenumbernode_type,
                                                 filename, linenum);
                JL_GC_POP();
                return temp;
            }
            jl_value_t *scmv = NULL, *temp = NULL;
            JL_GC_PUSH1(&scmv);
            if (sym == label_sym) {
                scmv = scm_to_julia_(fl_ctx,car_(e),0);
                temp = jl_new_struct(jl_labelnode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == goto_sym) {
                scmv = scm_to_julia_(fl_ctx,car_(e),0);
                temp = jl_new_struct(jl_gotonode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == inert_sym || (sym == quote_sym && (!iscons(car_(e))))) {
                scmv = scm_to_julia_(fl_ctx,car_(e),0);
                temp = jl_new_struct(jl_quotenode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == top_sym) {
                scmv = scm_to_julia_(fl_ctx,car_(e),0);
                temp = jl_new_struct(jl_topnode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == newvar_sym) {
                scmv = scm_to_julia_(fl_ctx,car_(e),0);
                temp = jl_new_struct(jl_newvarnode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            JL_GC_POP();
        }
        else if (sym == inert_sym && !iscons(car_(e))) {
            sym = quote_sym;
        }
        jl_expr_t *ex = jl_exprn(sym, n);
        JL_GC_PUSH1(&ex);
        // allocate a fresh args array for empty exprs passed to macros
        if (eo && n == 0) {
            ex->args = jl_alloc_cell_1d(0);
            jl_gc_wb(ex, ex->args);
        }
        for(i=0; i < n; i++) {
            assert(iscons(e));
            jl_cellset(ex->args, i, scm_to_julia_(fl_ctx, car_(e), eo));
            e = cdr_(e);
        }
        JL_GC_POP();
        if (sym == list_sym)
            return (jl_value_t*)ex->args;
        return (jl_value_t*)ex;
    }
    if (iscprim(e) && cp_class((cprim_t*)ptr(e)) == fl_ctx->wchartype) {
        return jl_box32(jl_char_type, *(int32_t*)cp_data((cprim_t*)ptr(e)));
    }
    if (iscvalue(e) && cv_class((cvalue_t*)ptr(e)) == jl_ast_ctx(fl_ctx)->jvtype) {
        return *(jl_value_t**)cv_data((cvalue_t*)ptr(e));
    }
    jl_error("malformed tree");

    return jl_nothing;
}

static value_t julia_to_scm_(fl_context_t *fl_ctx, jl_value_t *v);

static value_t julia_to_scm(fl_context_t *fl_ctx, jl_value_t *v)
{
    value_t temp;
    arraylist_t *jlgensym_to_flisp = &jl_ast_ctx(fl_ctx)->gensym_to_flisp;
    if (jlgensym_to_flisp->len)
        jlgensym_to_flisp->len = 0; // in case we didn't free it last time we got here (for example, if we threw an error)
    else
        arraylist_new(jlgensym_to_flisp, 0);
    // need try/catch to reset GC handle stack in case of error
    FL_TRY_EXTERN(fl_ctx) {
        temp = julia_to_scm_(fl_ctx, v);
    }
    FL_CATCH_EXTERN(fl_ctx) {
        temp = fl_list2(fl_ctx, jl_ast_ctx(fl_ctx)->error_sym, cvalue_static_cstring(fl_ctx, "expression too large"));
    }
    arraylist_free(jlgensym_to_flisp);
    return temp;
}

static void array_to_list(fl_context_t *fl_ctx, jl_array_t *a, value_t *pv)
{
    if (jl_array_len(a) > 300000)
        lerror(fl_ctx, fl_ctx->OutOfMemoryError, "expression too large");
    value_t temp;
    for(long i=jl_array_len(a)-1; i >= 0; i--) {
        *pv = fl_cons(fl_ctx, fl_ctx->NIL, *pv);
        temp = julia_to_scm_(fl_ctx, jl_cellref(a,i));
        // note: must be separate statement
        car_(*pv) = temp;
    }
}

static value_t julia_to_list2(fl_context_t *fl_ctx, jl_value_t *a, jl_value_t *b)
{
    value_t sa = julia_to_scm_(fl_ctx, a);
    fl_gc_handle(fl_ctx, &sa);
    value_t sb = julia_to_scm_(fl_ctx, b);
    value_t l = fl_list2(fl_ctx, sa, sb);
    fl_free_gc_handles(fl_ctx, 1);
    return l;
}

static value_t julia_to_scm_(fl_context_t *fl_ctx, jl_value_t *v)
{
    if (jl_is_symbol(v))
        return symbol(fl_ctx, jl_symbol_name((jl_sym_t*)v));
    if (jl_is_gensym(v)) {
        size_t idx = ((jl_gensym_t*)v)->id;
        size_t i;
        arraylist_t *jlgensym_to_flisp = &jl_ast_ctx(fl_ctx)->gensym_to_flisp;
        for (i = 0; i < jlgensym_to_flisp->len; i+=2) {
            if ((ssize_t)jlgensym_to_flisp->items[i] == idx)
                return fl_list2(fl_ctx, jl_ast_ctx(fl_ctx)->jlgensym_sym, fixnum((size_t)jlgensym_to_flisp->items[i+1]));
        }
        arraylist_push(jlgensym_to_flisp, (void*)idx);
        value_t flv = fl_applyn(fl_ctx, 0, symbol_value(symbol(fl_ctx, "make-jlgensym")));
        assert(iscons(flv) && car_(flv) == jl_ast_ctx(fl_ctx)->jlgensym_sym);
        arraylist_push(jlgensym_to_flisp, (void*)(size_t)numval(car_(cdr_(flv))));
        return flv;
    }
    if (v == jl_true)
        return fl_ctx->T;
    if (v == jl_false)
        return fl_ctx->F;
    if (v == jl_nothing)
        return fl_cons(fl_ctx, jl_ast_ctx(fl_ctx)->null_sym, fl_ctx->NIL);
    if (jl_is_expr(v)) {
        jl_expr_t *ex = (jl_expr_t*)v;
        value_t args = fl_ctx->NIL;
        fl_gc_handle(fl_ctx, &args);
        array_to_list(fl_ctx, ex->args, &args);
        value_t hd = julia_to_scm_(fl_ctx, (jl_value_t*)ex->head);
        if (ex->head == lambda_sym && jl_expr_nargs(ex)>0 && jl_is_array(jl_exprarg(ex,0))) {
            value_t llist = fl_ctx->NIL;
            fl_gc_handle(fl_ctx, &llist);
            array_to_list(fl_ctx, (jl_array_t*)jl_exprarg(ex,0), &llist);
            car_(args) = llist;
            fl_free_gc_handles(fl_ctx, 1);
        }
        value_t scmv = fl_cons(fl_ctx, hd, args);
        fl_free_gc_handles(fl_ctx, 1);
        return scmv;
    }
    if (jl_typeis(v, jl_linenumbernode_type)) {
        // GC Note: jl_fieldref(v, 1) allocates but neither jl_fieldref(v, 0)
        //          or julia_to_list2 should allocate here
        value_t args = julia_to_list2(fl_ctx, jl_fieldref(v,1), jl_fieldref(v,0));
        fl_gc_handle(fl_ctx, &args);
        value_t hd = julia_to_scm_(fl_ctx, (jl_value_t*)line_sym);
        value_t scmv = fl_cons(fl_ctx, hd, args);
        fl_free_gc_handles(fl_ctx, 1);
        return scmv;
    }
    // GC Note: jl_fieldref(v, 0) allocate for LabelNode, GotoNode
    //          but we don't need a GC root here because julia_to_list2
    //          shouldn't allocate in this case.
    if (jl_typeis(v, jl_slot_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)slot_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_labelnode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)label_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_gotonode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)goto_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_quotenode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)inert_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_newvarnode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)newvar_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_topnode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)top_sym, jl_fieldref(v,0));
    if (jl_is_long(v) && fits_fixnum(jl_unbox_long(v)))
        return fixnum(jl_unbox_long(v));
    value_t opaque = cvalue(fl_ctx, jl_ast_ctx(fl_ctx)->jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = v;
    return opaque;
}

// this is used to parse a line of repl input
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t s = cvalue_static_cstrn(fl_ctx, str, len);
    value_t e = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-parse-string")), s);
    jl_value_t *res = e == fl_ctx->FL_EOF ? jl_nothing : scm_to_julia(fl_ctx, e, 0);
    jl_ast_ctx_leave(ctx);
    return res;
}

// this is for parsing one expression out of a string, keeping track of
// the current position.
JL_DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len,
                                         int pos0, int greedy)
{
    if (pos0 < 0 || pos0 > len) {
        jl_array_t *buf = jl_pchar_to_array(str, len);
        JL_GC_PUSH1(&buf);
        // jl_bounds_error roots the arguments.
        jl_bounds_error((jl_value_t*)buf, jl_box_long(pos0));
    }
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t s = cvalue_static_cstrn(fl_ctx, str, len);
    value_t p = fl_applyn(fl_ctx, 3, symbol_value(symbol(fl_ctx, "jl-parse-one-string")),
                          s, fixnum(pos0), greedy?fl_ctx->T:fl_ctx->F);
    jl_value_t *expr=NULL, *pos1=NULL;
    JL_GC_PUSH2(&expr, &pos1);

    value_t e = car_(p);
    if (e == fl_ctx->FL_EOF)
        expr = jl_nothing;
    else
        expr = scm_to_julia(fl_ctx, e, 0);

    pos1 = jl_box_long(tosize(fl_ctx, cdr_(p),"parse"));
    jl_ast_ctx_leave(ctx);
    jl_value_t *result = (jl_value_t*)jl_svec2(expr, pos1);
    JL_GC_POP();
    return result;
}

// parse and eval a whole file, possibly reading from a string (`content`)
jl_value_t *jl_parse_eval_all(const char *fname, size_t len,
                              const char *content, size_t contentlen)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t f, ast;
    f = cvalue_static_cstring(fl_ctx, fname);
    fl_gc_handle(fl_ctx, &f);
    if (content != NULL) {
        value_t t = cvalue_static_cstrn(fl_ctx, content, contentlen);
        fl_gc_handle(fl_ctx, &t);
        ast = fl_applyn(fl_ctx, 2, symbol_value(symbol(fl_ctx, "jl-parse-string-stream")), t, f);
        fl_free_gc_handles(fl_ctx, 1);
    }
    else {
        ast = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-parse-file")), f);
    }
    fl_free_gc_handles(fl_ctx, 1);
    if (ast == fl_ctx->F) {
        jl_ast_ctx_leave(ctx);
        jl_errorf("could not open file %s", fname);
    }
    fl_gc_handle(fl_ctx, &ast);

    int last_lineno = jl_lineno;
    const char *last_filename = jl_filename;
    jl_lineno = 0;
    jl_filename = fname;
    jl_array_t *roots = NULL;
    jl_array_t **old_roots = ctx->roots;
    ctx->roots = &roots;
    jl_value_t *form=NULL, *result=jl_nothing;
    int err = 0;
    JL_GC_PUSH3(&roots, &form, &result);
    JL_TRY {
        assert(iscons(ast) && car_(ast) == symbol(fl_ctx,"toplevel"));
        ast = cdr_(ast);
        while (iscons(ast)) {
            value_t expansion = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-expand-to-thunk")), car_(ast));
            form = scm_to_julia(fl_ctx, expansion, 0);
            jl_sym_t *head = NULL;
            if (jl_is_expr(form)) head = ((jl_expr_t*)form)->head;
            if (head == jl_incomplete_sym)
                jl_errorf("syntax: %s", jl_string_data(jl_exprarg(form,0)));
            else if (head == error_sym)
                jl_interpret_toplevel_expr(form);
            else if (head == line_sym)
                jl_lineno = jl_unbox_long(jl_exprarg(form,0));
            else
                result = jl_toplevel_eval_flex(form, 1);
            ast = cdr_(ast);
        }
    }
    JL_CATCH {
        form = jl_pchar_to_string(fname, len);
        result = jl_box_long(jl_lineno);
        err = 1;
    }
    jl_lineno = last_lineno;
    jl_filename = last_filename;
    fl_free_gc_handles(fl_ctx, 1);
    ctx->roots = old_roots;
    jl_ast_ctx_leave(ctx);
    if (err) {
        if (jl_loaderror_type == NULL)
            jl_rethrow();
        else
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, form, result,
                                           jl_exception_in_transit));
    }
    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, size_t namelen)
{
    return jl_parse_eval_all(filename, namelen, text, len);
}

JL_DLLEXPORT int jl_parse_depwarn(int warn)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    int res = jl_parse_depwarn_(&ctx->fl, warn);
    jl_ast_ctx_leave(ctx);
    return res;
}

static int jl_parse_depwarn_(fl_context_t *fl_ctx, int warn)
{
    value_t prev = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-parser-depwarn")),
                             warn ? fl_ctx->T : fl_ctx->F);
    return prev == fl_ctx->T ? 1 : 0;
}

static int jl_parse_deperror(fl_context_t *fl_ctx, int err)
{
    value_t prev = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-parser-deperror")),
                             err ? fl_ctx->T : fl_ctx->F);
    return prev == fl_ctx->T ? 1 : 0;
}

// returns either an expression or a thunk
jl_value_t *jl_call_scm_on_ast(char *funcname, jl_value_t *expr)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    JL_AST_PRESERVE_PUSH(ctx, roots, old_roots);
    value_t arg = julia_to_scm(fl_ctx, expr);
    value_t e = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, funcname)), arg);
    jl_value_t *result = scm_to_julia(fl_ctx, e, 0);
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
    return result;
}

JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr)
{
    return jl_call_scm_on_ast("jl-expand-to-thunk", expr);
}

JL_DLLEXPORT jl_value_t *jl_macroexpand(jl_value_t *expr)
{
    return jl_call_scm_on_ast("jl-macroexpand", expr);
}

ssize_t jl_max_jlgensym_in(jl_value_t *v)
{
    ssize_t genid = -1;
    if (jl_is_gensym(v)) {
        genid = ((jl_gensym_t*)v)->id;
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t i, l = jl_array_len(e->args);
        for (i = 0; i < l; i++) {
            ssize_t maxid = jl_max_jlgensym_in(jl_exprarg(e, i));
            if (maxid > genid)
                genid = maxid;
        }
    }
    return genid;
}

// wrap expr in a thunk AST
jl_lambda_info_t *jl_wrap_expr(jl_value_t *expr)
{
    // `(lambda () (() () () ()) ,expr)
    jl_expr_t *le=NULL, *bo=NULL; jl_value_t *vi=NULL;
    jl_value_t *mt = jl_an_empty_cell;
    JL_GC_PUSH3(&le, &vi, &bo);
    le = jl_exprn(lambda_sym, 3);
    jl_cellset(le->args, 0, mt);
    vi = (jl_value_t*)jl_alloc_cell_1d(3);
    jl_cellset(vi, 0, mt);
    jl_cellset(vi, 1, mt);
    jl_cellset(vi, 2, jl_box_long(jl_max_jlgensym_in(expr)+1));
    jl_cellset(le->args, 1, vi);
    if (!jl_is_expr(expr) || ((jl_expr_t*)expr)->head != body_sym) {
        bo = jl_exprn(body_sym, 1);
        jl_cellset(bo->args, 0, (jl_value_t*)jl_exprn(return_sym, 1));
        jl_cellset(((jl_expr_t*)jl_exprarg(bo,0))->args, 0, expr);
        expr = (jl_value_t*)bo;
    }
    jl_cellset(le->args, 2, expr);
    jl_lambda_info_t *li = jl_new_lambda_info((jl_value_t*)le, jl_emptysvec, jl_emptysvec, jl_current_module);
    JL_GC_POP();
    return li;
}

// syntax tree accessors

// get array of formal argument expressions
jl_array_t *jl_lam_args(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    assert(l->head == lambda_sym);
    jl_value_t *ae = jl_exprarg(l,0);
    assert(jl_is_array(ae));
    return (jl_array_t*)ae;
}

// get array of var info records (for args and locals)
jl_array_t *jl_lam_vinfo(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 0);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

// get array of types for GenSym vars, or its length (if not type-inferred)
jl_value_t *jl_lam_gensyms(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    assert(jl_array_len(le) == 3);
    return jl_cellref(le, 2);
}

// get array of body forms
jl_expr_t *jl_lam_body(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *be = jl_exprarg(l, 2);
    assert(jl_is_expr(be));
    assert(((jl_expr_t*)be)->head == body_sym);
    return (jl_expr_t*)be;
}

JL_DLLEXPORT int jl_is_rest_arg(jl_value_t *ex)
{
    if (!jl_is_expr(ex)) return 0;
    if (((jl_expr_t*)ex)->head != colons_sym) return 0;
    jl_expr_t *atype = (jl_expr_t*)jl_exprarg(ex,1);
    if (!jl_is_expr(atype)) return 0;
    if (((jl_expr_t*)atype)->head == dots_sym)
        return 1;
    if (atype->head != call_sym || jl_array_len(atype->args) < 3 || jl_array_len(atype->args) > 4)
        return 0;
    return ((jl_sym_t*)jl_exprarg(atype,1)) == vararg_sym;
}

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (expr == NULL) {
        return NULL;
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_len(e->args);
        jl_expr_t *ne = NULL;
        JL_GC_PUSH2(&ne, &expr);
        ne = jl_exprn(e->head, l);
        if (l == 0) {
            ne->args = jl_alloc_cell_1d(0);
            jl_gc_wb(ne, ne->args);
        }
        else {
            for(i=0; i < l; i++) {
                jl_exprargset(ne, i, jl_copy_ast(jl_exprarg(e,i)));
            }
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
    }
    else if (jl_typeis(expr,jl_array_any_type)) {
        jl_array_t *a = (jl_array_t*)expr;
        size_t i, l = jl_array_len(a);
        jl_array_t *na = NULL;
        JL_GC_PUSH2(&na, &expr);
        na = jl_alloc_cell_1d(l);
        for(i=0; i < l; i++)
            jl_cellset(na, i, jl_copy_ast(jl_cellref(a,i)));
        JL_GC_POP();
        return (jl_value_t*)na;
    }
    return expr;
}

JL_DLLEXPORT int jl_is_operator(char *sym)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    int res = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "operator?")), symbol(fl_ctx, sym)) == fl_ctx->T;
    jl_ast_ctx_leave(ctx);
    return res;
}

JL_DLLEXPORT int jl_operator_precedence(char *sym)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    int res = numval(fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "operator-precedence")), symbol(fl_ctx, sym)));
    jl_ast_ctx_leave(ctx);
    return res;
}

jl_value_t *skip_meta(jl_array_t *body)
{
    jl_value_t *body1 = jl_cellref(body,0);
    if (jl_is_expr(body1) && ((jl_expr_t*)body1)->head == meta_sym
        && jl_array_len(body) > 1)
        body1 = jl_cellref(body,1);
    return body1;
}

int has_meta(jl_array_t *body, jl_sym_t *sym)
{
    size_t i, l = jl_array_len(body);
    for (i = 0; i < l; i++) {
        jl_expr_t *stmt = (jl_expr_t*)jl_cellref(body, i);
        if (jl_is_expr((jl_value_t*)stmt) && stmt->head == meta_sym) {
            size_t i, l = jl_array_len(stmt->args);
            for (i = 0; i < l; i++)
                if (jl_cellref(stmt->args, i) == (jl_value_t*)sym)
                    return 1;
        }
    }
    return 0;
}

extern jl_value_t *jl_builtin_getfield;

jl_value_t *jl_preresolve_globals(jl_value_t *expr, jl_lambda_info_t *lam)
{
    if (jl_is_symbol(expr)) {
        if (lam->module == NULL)
            return expr;
        return jl_module_globalref(lam->module, (jl_sym_t*)expr);
    }
    else if (jl_is_lambda_info(expr)) {
        jl_array_t *exprs = ((jl_lambda_info_t*)expr)->code;
        if (jl_typeis(exprs, jl_array_any_type)) {
            size_t l = jl_array_len(exprs);
            size_t i;
            for(i=0; i < l; i++)
                jl_cellset(exprs, i, jl_preresolve_globals(jl_cellref(exprs,i), lam));
        }
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == lambda_sym) {
            (void)jl_preresolve_globals(jl_exprarg(e,2), lam);
        }
        else if (jl_is_toplevel_only_expr(expr) || e->head == const_sym || e->head == copyast_sym ||
                 e->head == global_sym || e->head == quote_sym || e->head == inert_sym ||
                 e->head == line_sym || e->head == meta_sym) {
        }
        else {
            if (e->head == call_sym && jl_expr_nargs(e) == 3 && jl_is_quotenode(jl_exprarg(e,2)) &&
                lam->module != NULL) {
                // replace getfield(module_expr, :sym) with GlobalRef
                jl_value_t *s = jl_fieldref(jl_exprarg(e,2),0);
                jl_value_t *fe = jl_exprarg(e,0);
                if (jl_is_symbol(s) && jl_is_topnode(fe)) {
                    jl_value_t *f = jl_static_eval(fe, NULL, lam->module, lam, 0, 0);
                    if (f == jl_builtin_getfield) {
                        jl_value_t *me = jl_exprarg(e,1);
                        if (jl_is_topnode(me) ||
                            (jl_is_symbol(me) && jl_binding_resolved_p(lam->module,(jl_sym_t*)me))) {
                            jl_value_t *m = jl_static_eval(me, NULL, lam->module, lam, 0, 0);
                            if (m && jl_is_module(m))
                                return jl_module_globalref((jl_module_t*)m, (jl_sym_t*)s);
                        }
                    }
                }
            }
            size_t i = 0;
            if (e->head == method_sym || e->head == abstracttype_sym || e->head == compositetype_sym ||
                e->head == bitstype_sym || e->head == module_sym)
                i++;
            for(; i < jl_array_len(e->args); i++) {
                jl_exprargset(e, i, jl_preresolve_globals(jl_exprarg(e,i), lam));
            }
        }
    }
    return expr;
}

#ifdef __cplusplus
}
#endif
