// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  AST
  interface to front-end, obtains and translates syntax trees
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "flisp.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// MSVC complains about "julia_flisp.boot.inc : error C4335: Mac file format
// detected: please convert the source file to either DOS or UNIX format"
#ifdef _MSC_VER
#pragma warning(disable:4335)
#endif

// head symbols for each expression type
jl_sym_t *call_sym;    jl_sym_t *invoke_sym;
jl_sym_t *dots_sym;    jl_sym_t *empty_sym;
jl_sym_t *module_sym;  jl_sym_t *slot_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *importall_sym; jl_sym_t *toplevel_sym;
jl_sym_t *quote_sym;   jl_sym_t *amp_sym;
jl_sym_t *top_sym;     jl_sym_t *colons_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_incomplete_sym;
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *body_sym;    jl_sym_t *globalref_sym;
jl_sym_t *method_sym;  jl_sym_t *core_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *anonymous_sym;  jl_sym_t *underscore_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *primtype_sym;
jl_sym_t *structtype_sym; jl_sym_t *foreigncall_sym;
jl_sym_t *global_sym; jl_sym_t *list_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *inbounds_sym;
jl_sym_t *copyast_sym; jl_sym_t *fastmath_sym;
jl_sym_t *pure_sym; jl_sym_t *simdloop_sym;
jl_sym_t *meta_sym; jl_sym_t *compiler_temp_sym;
jl_sym_t *inert_sym; jl_sym_t *vararg_sym;
jl_sym_t *unused_sym; jl_sym_t *static_parameter_sym;
jl_sym_t *polly_sym; jl_sym_t *inline_sym;
jl_sym_t *propagate_inbounds_sym;
jl_sym_t *isdefined_sym; jl_sym_t *nospecialize_sym;
jl_sym_t *macrocall_sym;
jl_sym_t *hygienicscope_sym;
jl_sym_t *escape_sym;

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
    value_t ssavalue_sym;
    value_t slot_sym;
    jl_ast_context_list_t list;
    int ref;
    jl_task_t *task; // the current owner (user) of this jl_ast_context_t
    jl_module_t *module; // context module for "defined-julia-global" and "current-julia-module-counter"
} jl_ast_context_t;

static jl_ast_context_t jl_ast_main_ctx;

#define jl_ast_ctx(fl_ctx) container_of(fl_ctx, jl_ast_context_t, fl)
#define jl_ast_context_list_item(node)          \
    container_of(node, jl_ast_context_t, list)

struct macroctx_stack {
    jl_module_t *m;
    struct macroctx_stack *parent;
};

#define JL_AST_PRESERVE_PUSH(ctx, old, inmodule)  \
    jl_module_t *(old) = ctx->module;           \
    ctx->module = (inmodule)
#define JL_AST_PRESERVE_POP(ctx, old)           \
    ctx->module = (old)

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, int expronly, jl_module_t *mod);
static value_t julia_to_scm(fl_context_t *fl_ctx, jl_value_t *v);
static jl_value_t *jl_expand_macros(jl_value_t *expr, jl_module_t *inmodule, struct macroctx_stack *macroctx, int onelevel);

value_t fl_defined_julia_global(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    // tells whether a var is defined in and *by* the current module
    argcount(fl_ctx, "defined-julia-global", nargs, 1);
    jl_module_t *mod = ctx->module;
    jl_sym_t *sym = (jl_sym_t*)scm_to_julia(fl_ctx, args[0], 0, mod);
    if (jl_is_globalref(sym)) {
        mod = jl_globalref_mod(sym);
        sym = jl_globalref_name(sym);
    }
    if (!jl_is_symbol(sym))
        type_error(fl_ctx, "defined-julia-global", "symbol", args[0]);
    if (!mod)
        return fl_ctx->F;
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&mod->bindings, sym);
    return (b != HT_NOTFOUND && b->owner == mod) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_current_module_counter(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    assert(ctx->module);
    return fixnum(jl_module_next_counter(ctx->module));
}

// Check whether v is a scalar for purposes of inlining fused-broadcast
// arguments when lowering; should agree with broadcast.jl on what is a
// scalar.  When in doubt, return false, since this is only an optimization.
value_t fl_julia_scalar(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "julia-scalar?", nargs, 1);
    if (fl_isnumber(fl_ctx, args[0]) || fl_isstring(fl_ctx, args[0]))
        return fl_ctx->T;
    else if (iscvalue(args[0]) && fl_ctx->jl_sym == cv_type((cvalue_t*)ptr(args[0]))) {
        jl_value_t *v = *(jl_value_t**)cptr(args[0]);
        if (jl_isa(v,(jl_value_t*)jl_number_type) || jl_is_string(v))
            return fl_ctx->T;
    }
    return fl_ctx->F;
}

static const builtinspec_t julia_flisp_ast_ext[] = {
    { "defined-julia-global", fl_defined_julia_global },
    { "current-julia-module-counter", fl_current_module_counter },
    { "julia-scalar?", fl_julia_scalar },
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

    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    ctx->jvtype = define_opaque_type(fl_ctx->jl_sym, sizeof(void*), NULL, NULL);
    assign_global_builtins(fl_ctx, julia_flisp_ast_ext);
    ctx->true_sym = symbol(fl_ctx, "true");
    ctx->false_sym = symbol(fl_ctx, "false");
    ctx->error_sym = symbol(fl_ctx, "error");
    ctx->null_sym = symbol(fl_ctx, "null");
    ctx->ssavalue_sym = symbol(fl_ctx, "ssavalue");
    ctx->slot_sym = symbol(fl_ctx, "slot");
    ctx->task = NULL;
    ctx->module = NULL;

    // Enable / disable syntax deprecation warnings
    if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR)
        jl_parse_deperror(fl_ctx, 1);
    else
        jl_parse_depwarn_(fl_ctx, (int)jl_options.depwarn);
}

// There should be no GC allocation while holding this lock
static jl_mutex_t flisp_lock;
static jl_ast_context_list_t *jl_ast_ctx_using = NULL;
static jl_ast_context_list_t *jl_ast_ctx_freed = NULL;

static jl_ast_context_t *jl_ast_ctx_enter(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_SIGATOMIC_BEGIN();
    JL_LOCK_NOGC(&flisp_lock);
    jl_ast_context_list_t *node;
    jl_ast_context_t *ctx;
    // First check if the current task is using one of the contexts
    for (node = jl_ast_ctx_using;node;(node = node->next)) {
        ctx = jl_ast_context_list_item(node);
        if (ctx->task == ptls->current_task) {
            ctx->ref++;
            JL_UNLOCK_NOGC(&flisp_lock);
            return ctx;
        }
    }
    // If not, grab one from the free list
    if ((node = jl_ast_ctx_freed)) {
        jl_ast_context_list_delete(node);
        jl_ast_context_list_insert(&jl_ast_ctx_using, node);
        ctx = jl_ast_context_list_item(node);
        ctx->ref = 1;
        ctx->task = ptls->current_task;
        ctx->module = NULL;
        JL_UNLOCK_NOGC(&flisp_lock);
        return ctx;
    }
    // Construct a new one if we can't find any
    ctx = (jl_ast_context_t*)calloc(1, sizeof(jl_ast_context_t));
    ctx->ref = 1;
    ctx->task = ptls->current_task;
    node = &ctx->list;
    jl_ast_context_list_insert(&jl_ast_ctx_using, node);
    JL_UNLOCK_NOGC(&flisp_lock);
    jl_init_ast_ctx(ctx);
    return ctx;
}

static void jl_ast_ctx_leave(jl_ast_context_t *ctx)
{
    JL_SIGATOMIC_END();
    if (--ctx->ref)
        return;
    JL_LOCK_NOGC(&flisp_lock);
    ctx->task = NULL;
    jl_ast_context_list_t *node = &ctx->list;
    jl_ast_context_list_delete(node);
    jl_ast_context_list_insert(&jl_ast_ctx_freed, node);
    JL_UNLOCK_NOGC(&flisp_lock);
}

void jl_init_frontend(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_ast_ctx_using || jl_ast_ctx_freed)
        return;
    jl_ast_main_ctx.ref = 1;
    jl_ast_main_ctx.task = ptls->current_task;
    jl_ast_context_list_insert(&jl_ast_ctx_using, &jl_ast_main_ctx.list);
    jl_init_ast_ctx(&jl_ast_main_ctx);
    // To match the one in jl_ast_ctx_leave
    JL_SIGATOMIC_BEGIN();
    jl_ast_ctx_leave(&jl_ast_main_ctx);

    empty_sym = jl_symbol("");
    call_sym = jl_symbol("call");
    invoke_sym = jl_symbol("invoke");
    foreigncall_sym = jl_symbol("foreigncall");
    quote_sym = jl_symbol("quote");
    inert_sym = jl_symbol("inert");
    top_sym = jl_symbol("top");
    core_sym = jl_symbol("core");
    globalref_sym = jl_symbol("globalref");
    line_sym = jl_symbol("line");
    jl_incomplete_sym = jl_symbol("incomplete");
    error_sym = jl_symbol("error");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    label_sym = jl_symbol("label");
    return_sym = jl_symbol("return");
    lambda_sym = jl_symbol("lambda");
    module_sym = jl_symbol("module");
    export_sym = jl_symbol("export");
    import_sym = jl_symbol("import");
    using_sym = jl_symbol("using");
    importall_sym = jl_symbol("importall");
    assign_sym = jl_symbol("=");
    body_sym = jl_symbol("body");
    colons_sym = jl_symbol("::");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    new_sym = jl_symbol("new");
    const_sym = jl_symbol("const");
    global_sym = jl_symbol("global");
    thunk_sym = jl_symbol("thunk");
    anonymous_sym = jl_symbol("anonymous");
    underscore_sym = jl_symbol("_");
    amp_sym = jl_symbol("&");
    abstracttype_sym = jl_symbol("abstract_type");
    primtype_sym = jl_symbol("primitive_type");
    structtype_sym = jl_symbol("struct_type");
    toplevel_sym = jl_symbol("toplevel");
    dot_sym = jl_symbol(".");
    boundscheck_sym = jl_symbol("boundscheck");
    inbounds_sym = jl_symbol("inbounds");
    fastmath_sym = jl_symbol("fastmath");
    newvar_sym = jl_symbol("newvar");
    copyast_sym = jl_symbol("copyast");
    simdloop_sym = jl_symbol("simdloop");
    pure_sym = jl_symbol("pure");
    meta_sym = jl_symbol("meta");
    dots_sym = jl_symbol("...");
    list_sym = jl_symbol("list");
    unused_sym = jl_symbol("#unused#");
    slot_sym = jl_symbol("slot");
    static_parameter_sym = jl_symbol("static_parameter");
    compiler_temp_sym = jl_symbol("#temp#");
    polly_sym = jl_symbol("polly");
    inline_sym = jl_symbol("inline");
    propagate_inbounds_sym = jl_symbol("propagate_inbounds");
    isdefined_sym = jl_symbol("isdefined");
    nospecialize_sym = jl_symbol("nospecialize");
    macrocall_sym = jl_symbol("macrocall");
    escape_sym = jl_symbol("escape");
    hygienicscope_sym = jl_symbol("hygienic-scope");
}

JL_DLLEXPORT void jl_lisp_prompt(void)
{
    // Make `--lisp` sigatomic in order to avoid triggering the sigint safepoint.
    // We don't have our signal handler registered in that case anyway...
    JL_SIGATOMIC_BEGIN();
    jl_init_frontend();
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    JL_AST_PRESERVE_PUSH(ctx, old_roots, jl_main_module);
    fl_context_t *fl_ctx = &ctx->fl;
    fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "__start")), fl_cons(fl_ctx, fl_ctx->NIL,fl_ctx->NIL));
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
}

JL_DLLEXPORT void fl_show_profile(void)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    fl_applyn(fl_ctx, 0, symbol_value(symbol(fl_ctx, "show-profiles")));
    jl_ast_ctx_leave(ctx);
}

JL_DLLEXPORT void fl_clear_profile(void)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    fl_applyn(fl_ctx, 0, symbol_value(symbol(fl_ctx, "clear-profiles")));
    jl_ast_ctx_leave(ctx);
}

JL_DLLEXPORT void fl_profile(const char *fname)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "profile-e")), symbol(fl_ctx, fname));
    jl_ast_ctx_leave(ctx);
}


static jl_sym_t *scmsym_to_julia(fl_context_t *fl_ctx, value_t s)
{
    assert(issymbol(s));
    if (fl_isgensym(fl_ctx, s)) {
        char gsname[16];
        char *n = uint2str(&gsname[1], sizeof(gsname)-1,
                           ((gensym_t*)ptr(s))->id, 10);
        *(--n) = '#';
        return jl_symbol(n);
    }
    return jl_symbol(symbol_name(fl_ctx, s));
}

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, int expronly, jl_module_t *mod);

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, int expronly, jl_module_t *mod)
{
    jl_value_t *v = NULL;
    JL_GC_PUSH1(&v);
    JL_TRY {
        v = scm_to_julia_(fl_ctx, e, expronly, mod);
    }
    JL_CATCH {
        // if expression cannot be converted, replace with error expr
        jl_expr_t *ex = jl_exprn(error_sym, 1);
        v = (jl_value_t*)ex;
        jl_array_ptr_set(ex->args, 0, jl_cstr_to_string("invalid AST"));
    }
    JL_GC_POP();
    return v;
}

extern int64_t conv_to_int64(void *data, numerictype_t tag);

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, int eo, jl_module_t *mod)
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
    if (iscons(e) || e == fl_ctx->NIL) {
        value_t hd;
        jl_sym_t *sym;
        if (e == fl_ctx->NIL) {
            hd = e;
        }
        else {
            hd = car_(e);
            if (hd == jl_ast_ctx(fl_ctx)->ssavalue_sym)
                return jl_box_ssavalue(numval(car_(cdr_(e))));
            else if (hd == jl_ast_ctx(fl_ctx)->slot_sym)
                return jl_box_slotnumber(numval(car_(cdr_(e))));
            else if (hd == jl_ast_ctx(fl_ctx)->null_sym && llength(e) == 1)
                return jl_nothing;
        }
        if (issymbol(hd))
            sym = scmsym_to_julia(fl_ctx, hd);
        else
            sym = list_sym;
        size_t n = llength(e)-1;
        if (issymbol(hd))
            e = cdr_(e);
        else
            n++;
        if (!eo) {
            if (sym == line_sym && (n == 1 || n == 2)) {
                jl_value_t *linenum = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                jl_value_t *file = jl_nothing;
                JL_GC_PUSH2(&linenum, &file);
                if (n == 2)
                    file = scm_to_julia_(fl_ctx, car_(cdr_(e)), 0, mod);
                jl_value_t *temp = jl_new_struct(jl_linenumbernode_type, linenum, file);
                JL_GC_POP();
                return temp;
            }
            jl_value_t *scmv = NULL, *temp = NULL;
            JL_GC_PUSH1(&scmv);
            if (sym == label_sym) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                temp = jl_new_struct(jl_labelnode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == goto_sym) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                temp = jl_new_struct(jl_gotonode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == inert_sym || (sym == quote_sym && (!iscons(car_(e))))) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                temp = jl_new_struct(jl_quotenode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == top_sym) {
                assert(mod && "top should not be generated by the parser");
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                assert(jl_is_symbol(scmv));
                temp = jl_module_globalref(jl_base_relative_to(mod), (jl_sym_t*)scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == core_sym) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                assert(jl_is_symbol(scmv));
                temp = jl_module_globalref(jl_core_module, (jl_sym_t*)scmv);
                JL_GC_POP();
                return temp;
            }
            if (sym == globalref_sym) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                temp = scm_to_julia_(fl_ctx, car_(cdr_(e)), 0, mod);
                assert(jl_is_module(scmv));
                assert(jl_is_symbol(temp));
                temp = jl_module_globalref((jl_module_t*)scmv, (jl_sym_t*)temp);
                JL_GC_POP();
                return temp;
            }
            if (sym == newvar_sym) {
                scmv = scm_to_julia_(fl_ctx, car_(e), 0, mod);
                temp = jl_new_struct(jl_newvarnode_type, scmv);
                JL_GC_POP();
                return temp;
            }
            JL_GC_POP();
        }
        else if (sym == inert_sym && !iscons(car_(e))) {
            sym = quote_sym;
        }
        jl_value_t *ex = (jl_value_t*)jl_exprn(sym, n);
        JL_GC_PUSH1(&ex);
        // allocate a fresh args array for empty exprs passed to macros
        if (eo && n == 0) {
            ((jl_expr_t*)ex)->args = jl_alloc_vec_any(0);
            jl_gc_wb(ex, ((jl_expr_t*)ex)->args);
        }
        size_t i;
        for (i = 0; i < n; i++) {
            assert(iscons(e));
            jl_array_ptr_set(((jl_expr_t*)ex)->args, i, scm_to_julia_(fl_ctx, car_(e), eo, mod));
            e = cdr_(e);
        }
        if (sym == lambda_sym)
            ex = (jl_value_t*)jl_new_code_info_from_ast((jl_expr_t*)ex);
        JL_GC_POP();
        if (sym == list_sym)
            return (jl_value_t*)((jl_expr_t*)ex)->args;
        return (jl_value_t*)ex;
    }
    if (iscprim(e) && cp_class((cprim_t*)ptr(e)) == fl_ctx->wchartype) {
        return jl_box32(jl_char_type, *(int32_t*)cp_data((cprim_t*)ptr(e)));
    }
    if (iscvalue(e) && cv_class((cvalue_t*)ptr(e)) == jl_ast_ctx(fl_ctx)->jvtype) {
        return *(jl_value_t**)cv_data((cvalue_t*)ptr(e));
    }
    jl_error("malformed tree");
}

static value_t julia_to_scm_(fl_context_t *fl_ctx, jl_value_t *v);

static value_t julia_to_scm(fl_context_t *fl_ctx, jl_value_t *v)
{
    value_t temp;
    // need try/catch to reset GC handle stack in case of error
    FL_TRY_EXTERN(fl_ctx) {
        temp = julia_to_scm_(fl_ctx, v);
    }
    FL_CATCH_EXTERN(fl_ctx) {
        temp = fl_ctx->lasterror;
    }
    return temp;
}

static void array_to_list(fl_context_t *fl_ctx, jl_array_t *a, value_t *pv)
{
    if (jl_array_len(a) > 300000)
        lerror(fl_ctx, symbol(fl_ctx, "error"), "expression too large");
    value_t temp;
    for(long i=jl_array_len(a)-1; i >= 0; i--) {
        *pv = fl_cons(fl_ctx, fl_ctx->NIL, *pv);
        temp = julia_to_scm_(fl_ctx, jl_array_ptr_ref(a,i));
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
    if (v == NULL)
        lerror(fl_ctx, symbol(fl_ctx, "error"), "undefined reference in AST");
    if (jl_is_symbol(v))
        return symbol(fl_ctx, jl_symbol_name((jl_sym_t*)v));
    if (v == jl_true)
        return jl_ast_ctx(fl_ctx)->true_sym;
    if (v == jl_false)
        return jl_ast_ctx(fl_ctx)->false_sym;
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
    // GC Note: jl_fieldref(v, 0) allocate for LabelNode, GotoNode
    //          but we don't need a GC root here because julia_to_list2
    //          shouldn't allocate in this case.
    if (jl_typeis(v, jl_labelnode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)label_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_linenumbernode_type)) {
        jl_value_t *file = jl_fieldref(v,1); // non-allocating
        jl_value_t *line = jl_fieldref(v,0); // allocating
        value_t args = julia_to_list2(fl_ctx, line, file);
        fl_gc_handle(fl_ctx, &args);
        value_t hd = julia_to_scm_(fl_ctx, (jl_value_t*)line_sym);
        value_t scmv = fl_cons(fl_ctx, hd, args);
        fl_free_gc_handles(fl_ctx, 1);
        return scmv;
    }
    if (jl_typeis(v, jl_gotonode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)goto_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_quotenode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)inert_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_newvarnode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)newvar_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_globalref_type)) {
        jl_module_t *m = jl_globalref_mod(v);
        jl_sym_t *sym = jl_globalref_name(v);
        if (m == jl_core_module)
            return julia_to_list2(fl_ctx, (jl_value_t*)core_sym,
                                  (jl_value_t*)sym);
        value_t args = julia_to_list2(fl_ctx, (jl_value_t*)m, (jl_value_t*)sym);
        fl_gc_handle(fl_ctx, &args);
        value_t hd = julia_to_scm_(fl_ctx, (jl_value_t*)globalref_sym);
        value_t scmv = fl_cons(fl_ctx, hd, args);
        fl_free_gc_handles(fl_ctx, 1);
        return scmv;
    }
    if (jl_is_long(v) && fits_fixnum(jl_unbox_long(v)))
        return fixnum(jl_unbox_long(v));
    if (jl_is_ssavalue(v))
        lerror(fl_ctx, symbol(fl_ctx, "error"), "SSAValue objects should not occur in an AST");
    if (jl_is_slot(v))
        lerror(fl_ctx, symbol(fl_ctx, "error"), "Slot objects should not occur in an AST");
    value_t opaque = cvalue(fl_ctx, jl_ast_ctx(fl_ctx)->jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = v;
    return opaque;
}

// this is used to parse a line of repl input
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len, const char *filename, size_t filename_len)
{
    JL_TIMING(PARSING);
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t s = cvalue_static_cstrn(fl_ctx, str, len);
    value_t files = cvalue_static_cstrn(fl_ctx, filename, filename_len);
    value_t e = fl_applyn(fl_ctx, 2, symbol_value(symbol(fl_ctx, "jl-parse-string")), s, files);
    jl_value_t *res = e == fl_ctx->FL_EOF ? jl_nothing : scm_to_julia(fl_ctx, e, 0, NULL);
    jl_ast_ctx_leave(ctx);
    return res;
}

// this is for parsing one expression out of a string, keeping track of
// the current position.
JL_DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len,
                                         int pos0, int greedy)
{
    JL_TIMING(PARSING);
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
        expr = scm_to_julia(fl_ctx, e, 0, NULL);

    pos1 = jl_box_long(tosize(fl_ctx, cdr_(p), "parse"));
    jl_ast_ctx_leave(ctx);
    jl_value_t *result = (jl_value_t*)jl_svec2(expr, pos1);
    JL_GC_POP();
    return result;
}

// parse and eval a whole file, possibly reading from a string (`content`)
jl_value_t *jl_parse_eval_all(const char *fname,
                              const char *content, size_t contentlen,
                              jl_module_t *inmodule)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->in_pure_callback)
        jl_error("cannot use include inside a generated function");
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t f, ast, expression;
    size_t len = strlen(fname);
    f = cvalue_static_cstrn(fl_ctx, fname, len);
    fl_gc_handle(fl_ctx, &f);
    if (content != NULL) {
        JL_TIMING(PARSING);
        value_t t = cvalue_static_cstrn(fl_ctx, content, contentlen);
        fl_gc_handle(fl_ctx, &t);
        ast = fl_applyn(fl_ctx, 2, symbol_value(symbol(fl_ctx, "jl-parse-string-stream")), t, f);
        fl_free_gc_handles(fl_ctx, 1);
    }
    else {
        JL_TIMING(PARSING);
        assert(memchr(fname, 0, len) == NULL); // was checked already in jl_load
        ast = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-parse-file")), f);
    }
    fl_free_gc_handles(fl_ctx, 1);
    if (ast == fl_ctx->F) {
        jl_ast_ctx_leave(ctx);
        jl_errorf("could not open file %s", fname);
    }
    fl_gc_handle(fl_ctx, &ast);
    fl_gc_handle(fl_ctx, &expression);

    int last_lineno = jl_lineno;
    const char *last_filename = jl_filename;
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_lineno = 0;
    jl_filename = fname;
    jl_module_t *old_module = ctx->module;
    ctx->module = inmodule;
    jl_value_t *form = NULL;
    jl_value_t *result = jl_nothing;
    int err = 0;
    JL_GC_PUSH2(&form, &result);
    JL_TRY {
        assert(iscons(ast) && car_(ast) == symbol(fl_ctx, "toplevel"));
        ast = cdr_(ast);
        while (iscons(ast)) {
            expression = car_(ast);
            {
                JL_TIMING(LOWERING);
                if (fl_ctx->T == fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "contains-macrocall")), expression)) {
                    form = scm_to_julia(fl_ctx, expression, 0, inmodule);
                    form = jl_expand_macros(form, inmodule, NULL, 0);
                    expression = julia_to_scm(fl_ctx, form);
                }
                expression = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "jl-expand-to-thunk")), expression);
            }
            jl_get_ptls_states()->world_age = jl_world_counter;
            form = scm_to_julia(fl_ctx, expression, 0, inmodule);
            jl_sym_t *head = NULL;
            if (jl_is_expr(form))
                head = ((jl_expr_t*)form)->head;
            JL_SIGATOMIC_END();
            jl_get_ptls_states()->world_age = jl_world_counter;
            if (head == jl_incomplete_sym)
                jl_errorf("syntax: %s", jl_string_data(jl_exprarg(form, 0)));
            else if (head == error_sym)
                jl_interpret_toplevel_expr_in(inmodule, form, NULL, NULL);
            else if (head == line_sym)
                jl_lineno = jl_unbox_long(jl_exprarg(form, 0));
            else if (jl_is_linenode(form))
                jl_lineno = jl_linenode_line(form);
            else
                result = jl_toplevel_eval_flex(inmodule, form, 1, 1);
            JL_SIGATOMIC_BEGIN();
            ast = cdr_(ast);
        }
    }
    JL_CATCH {
        form = jl_pchar_to_string(fname, len);
        result = jl_box_long(jl_lineno);
        err = 1;
    }
    jl_get_ptls_states()->world_age = last_age;
    jl_lineno = last_lineno;
    jl_filename = last_filename;
    fl_free_gc_handles(fl_ctx, 2);
    ctx->module = old_module;
    jl_ast_ctx_leave(ctx);
    if (err) {
        if (jl_loaderror_type == NULL)
            jl_rethrow();
        else
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, form, result,
                                           ptls->exception_in_transit));
    }
    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *inmodule)
{
    return jl_parse_eval_all(filename, text, len, inmodule);
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
jl_value_t *jl_call_scm_on_ast(const char *funcname, jl_value_t *expr, jl_module_t *inmodule)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    JL_AST_PRESERVE_PUSH(ctx, old_roots, inmodule);
    value_t arg = julia_to_scm(fl_ctx, expr);
    value_t e = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, funcname)), arg);
    jl_value_t *result = scm_to_julia(fl_ctx, e, 0, inmodule);
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
    return result;
}

// wrap expr in a thunk AST
jl_code_info_t *jl_wrap_expr(jl_value_t *expr)
{
    // `(lambda () (() () () ()) ,expr)
    jl_expr_t *le=NULL, *bo=NULL; jl_value_t *vi=NULL;
    jl_value_t *mt = jl_an_empty_vec_any;
    jl_code_info_t *src = NULL;
    JL_GC_PUSH4(&le, &vi, &bo, &src);
    le = jl_exprn(lambda_sym, 3);
    jl_array_ptr_set(le->args, 0, mt);
    vi = (jl_value_t*)jl_alloc_vec_any(4);
    jl_array_ptr_set(vi, 0, mt);
    jl_array_ptr_set(vi, 1, mt);
    // front end always wraps toplevel exprs with ssavalues in (thunk (lambda () ...))
    jl_array_ptr_set(vi, 2, jl_box_long(0));
    jl_array_ptr_set(vi, 3, mt);
    jl_array_ptr_set(le->args, 1, vi);
    if (!jl_is_expr(expr) || ((jl_expr_t*)expr)->head != body_sym) {
        bo = jl_exprn(body_sym, 1);
        jl_array_ptr_set(bo->args, 0, (jl_value_t*)jl_exprn(return_sym, 1));
        jl_array_ptr_set(((jl_expr_t*)jl_exprarg(bo,0))->args, 0, expr);
        expr = (jl_value_t*)bo;
    }
    jl_array_ptr_set(le->args, 2, expr);
    src = jl_new_code_info_from_ast(le);
    JL_GC_POP();
    return src;
}

// syntax tree accessors

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (expr && jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_len(e->args);
        jl_expr_t *ne = jl_exprn(e->head, l);
        JL_GC_PUSH2(&ne, &expr);
        if (l == 0) {
            ne->args = jl_alloc_vec_any(0);
            jl_gc_wb(ne, ne->args);
        }
        else {
            for (i = 0; i < l; i++) {
                jl_value_t *a = jl_exprarg(e, i);
                jl_exprargset(ne, i, jl_copy_ast(a));
            }
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
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

int jl_has_meta(jl_array_t *body, jl_sym_t *sym)
{
    size_t i, l = jl_array_len(body);
    for (i = 0; i < l; i++) {
        jl_expr_t *stmt = (jl_expr_t*)jl_array_ptr_ref(body, i);
        if (jl_is_expr((jl_value_t*)stmt) && stmt->head == meta_sym) {
            size_t i, l = jl_array_len(stmt->args);
            for (i = 0; i < l; i++)
                if (jl_array_ptr_ref(stmt->args, i) == (jl_value_t*)sym)
                    return 1;
        }
    }
    return 0;
}

static jl_value_t *jl_invoke_julia_macro(jl_array_t *args, jl_module_t *inmodule, jl_module_t **ctx)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_TIMING(MACRO_INVOCATION);
    size_t nargs = jl_array_len(args) + 1;
    JL_NARGSV("macrocall", 3); // macro name, location, and module
    jl_value_t **margs;
    JL_GC_PUSHARGS(margs, nargs);
    int i;
    margs[0] = jl_array_ptr_ref(args, 0);
    // __source__ argument
    jl_value_t *lno = jl_array_ptr_ref(args, 1);
    margs[1] = lno;
    if (jl_is_expr(lno) && ((jl_expr_t*)lno)->head == line_sym) {
        jl_value_t *file = jl_nothing;
        jl_value_t *line = NULL;
        switch (jl_expr_nargs(lno)) {
        case 2:
            file = jl_exprarg(lno, 1); // file
            JL_FALLTHROUGH;
        case 1:
            line = jl_exprarg(lno, 0); // line
            JL_FALLTHROUGH;
        default: ;
        }
        if (line == NULL)
            line = jl_box_long(0);
        margs[1] = jl_new_struct(jl_linenumbernode_type, line, file);
    }
    else if (!jl_typeis(lno, jl_linenumbernode_type)) {
        margs[1] = jl_new_struct(jl_linenumbernode_type, jl_box_long(0), jl_nothing);
    }
    margs[2] = (jl_value_t*)inmodule;
    for (i = 3; i < nargs; i++)
        margs[i] = jl_array_ptr_ref(args, i - 1);

    size_t last_age = ptls->world_age;
    size_t world = jl_world_counter;
    ptls->world_age = world;
    jl_value_t *result;
    JL_TRY {
        margs[0] = jl_toplevel_eval(*ctx, margs[0]);
        jl_method_instance_t *mfunc = jl_method_lookup(jl_gf_mtable(margs[0]), margs, nargs, 1, world);
        if (mfunc == NULL) {
            jl_method_error((jl_function_t*)margs[0], margs, nargs, world);
            // unreachable
        }
        *ctx = mfunc->def.method->module;
        result = jl_call_method_internal(mfunc, margs, nargs);
    }
    JL_CATCH {
        if (jl_loaderror_type == NULL) {
            jl_rethrow();
        }
        else {
            jl_value_t *lno = margs[1];
            jl_value_t *file = jl_fieldref(lno, 1);
            if (jl_is_symbol(file))
                margs[0] = jl_cstr_to_string(jl_symbol_name((jl_sym_t*)file));
            else
                margs[0] = jl_cstr_to_string("<macrocall>");
            margs[1] = jl_fieldref(lno, 0); // extract and allocate line number
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, margs[0], margs[1],
                                           ptls->exception_in_transit));
        }
    }
    ptls->world_age = last_age;
    JL_GC_POP();
    return result;
}

static jl_value_t *jl_expand_macros(jl_value_t *expr, jl_module_t *inmodule, struct macroctx_stack *macroctx, int onelevel)
{
    if (!expr || !jl_is_expr(expr))
        return expr;
    jl_expr_t *e = (jl_expr_t*)expr;
    if (e->head == inert_sym ||
        e->head == module_sym ||
        //e->head == toplevel_sym || // TODO: enable this once julia-expand-macroscope is fixed / removed
        e->head == meta_sym) {
        return expr;
    }
    if (e->head == quote_sym && jl_expr_nargs(e) == 1) {
        expr = jl_call_scm_on_ast("julia-bq-macro", jl_exprarg(e, 0), inmodule);
        JL_GC_PUSH1(&expr);
        if (macroctx) {
            // in a macro, `quote` also implies `escape`
            jl_expr_t *e2 = jl_exprn(escape_sym, 1);
            jl_array_ptr_set(e2->args, 0, expr);
            expr = (jl_value_t*)e2;
        }
        expr = jl_expand_macros(expr, inmodule, macroctx, onelevel);
        JL_GC_POP();
        return expr;
    }
    if (e->head == hygienicscope_sym && jl_expr_nargs(e) == 2) {
        struct macroctx_stack newctx;
        newctx.m = (jl_module_t*)jl_exprarg(e, 1);
        JL_TYPECHK(hygienic-scope, module, (jl_value_t*)newctx.m);
        newctx.parent = macroctx;
        jl_value_t *a = jl_exprarg(e, 0);
        jl_value_t *a2 = jl_expand_macros(a, inmodule, &newctx, onelevel);
        if (a != a2)
            jl_array_ptr_set(e->args, 0, a2);
        return expr;
    }
    if (e->head == macrocall_sym) {
        struct macroctx_stack newctx;
        newctx.m = macroctx ? macroctx->m : inmodule;
        newctx.parent = macroctx;
        jl_value_t *result = jl_invoke_julia_macro(e->args, inmodule, &newctx.m);
        jl_value_t *wrap = NULL;
        JL_GC_PUSH3(&result, &wrap, &newctx.m);
        // copy and wrap the result in `(hygienic-scope ,result ,newctx)
        if (jl_is_expr(result) && ((jl_expr_t*)result)->head == escape_sym)
            result = jl_exprarg(result, 0);
        else
            wrap = (jl_value_t*)jl_exprn(hygienicscope_sym, 2);
        result = jl_copy_ast(result);
        if (!onelevel)
            result = jl_expand_macros(result, inmodule, wrap ? &newctx : macroctx, onelevel);
        if (wrap) {
            jl_exprargset(wrap, 0, result);
            jl_exprargset(wrap, 1, newctx.m);
            result = wrap;
        }
        JL_GC_POP();
        return result;
    }
    if (e->head == escape_sym && macroctx) {
        macroctx = macroctx->parent;
    }

    size_t i;
    for (i = 0; i < jl_array_len(e->args); i++) {
        jl_value_t *a = jl_array_ptr_ref(e->args, i);
        jl_value_t *a2 = jl_expand_macros(a, inmodule, macroctx, onelevel);
        if (a != a2)
            jl_array_ptr_set(e->args, i, a2);
    }
    return expr;
}

JL_DLLEXPORT jl_value_t *jl_macroexpand(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0);
    expr = jl_call_scm_on_ast("julia-expand-macroscope", expr, inmodule);
    JL_GC_POP();
    return expr;
}

JL_DLLEXPORT jl_value_t *jl_macroexpand1(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 1);
    expr = jl_call_scm_on_ast("julia-expand-macroscope", expr, inmodule);
    JL_GC_POP();
    return expr;
}

JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0);
    expr = jl_call_scm_on_ast("jl-expand-to-thunk", expr, inmodule);
    JL_GC_POP();
    return expr;
}


#ifdef __cplusplus
}
#endif
