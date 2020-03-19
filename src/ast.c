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
jl_sym_t *empty_sym;   jl_sym_t *top_sym;
jl_sym_t *module_sym;  jl_sym_t *slot_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *toplevel_sym; jl_sym_t *quote_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_incomplete_sym;
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *return_sym;  jl_sym_t *unreachable_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *globalref_sym; jl_sym_t *do_sym;
jl_sym_t *method_sym;  jl_sym_t *core_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *pop_exception_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *splatnew_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *primtype_sym;
jl_sym_t *structtype_sym;   jl_sym_t *foreigncall_sym;
jl_sym_t *global_sym; jl_sym_t *list_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *inbounds_sym;
jl_sym_t *copyast_sym; jl_sym_t *cfunction_sym;
jl_sym_t *pure_sym; jl_sym_t *loopinfo_sym;
jl_sym_t *meta_sym; jl_sym_t *inert_sym;
jl_sym_t *polly_sym; jl_sym_t *unused_sym;
jl_sym_t *static_parameter_sym; jl_sym_t *inline_sym;
jl_sym_t *noinline_sym; jl_sym_t *generated_sym;
jl_sym_t *generated_only_sym; jl_sym_t *isdefined_sym;
jl_sym_t *propagate_inbounds_sym; jl_sym_t *specialize_sym;
jl_sym_t *nospecialize_sym; jl_sym_t *macrocall_sym;
jl_sym_t *colon_sym; jl_sym_t *hygienicscope_sym;
jl_sym_t *throw_undef_if_not_sym; jl_sym_t *getfield_undefref_sym;
jl_sym_t *gc_preserve_begin_sym; jl_sym_t *gc_preserve_end_sym;
jl_sym_t *coverageeffect_sym; jl_sym_t *escape_sym;
jl_sym_t *aliasscope_sym; jl_sym_t *popaliasscope_sym;

static uint8_t flisp_system_image[] = {
#include <julia_flisp.boot.inc>
};

typedef struct _jl_ast_context_list_t {
    struct _jl_ast_context_list_t *next;
    struct _jl_ast_context_list_t **prev;
} jl_ast_context_list_t;

STATIC_INLINE void jl_ast_context_list_insert(jl_ast_context_list_t **head,
                                              jl_ast_context_list_t *node) JL_NOTSAFEPOINT
{
    jl_ast_context_list_t *next = *head;
    if (next)
        next->prev = &node->next;
    node->next = next;
    node->prev = head;
    *head = node;
}

STATIC_INLINE void jl_ast_context_list_delete(jl_ast_context_list_t *node) JL_NOTSAFEPOINT
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
    jl_module_t *module; // context module for `current-julia-module-counter`
} jl_ast_context_t;

static jl_ast_context_t jl_ast_main_ctx;

#ifdef __clang_analyzer__
jl_ast_context_t *jl_ast_ctx(fl_context_t *fl) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#else
#define jl_ast_ctx(fl_ctx) container_of(fl_ctx, jl_ast_context_t, fl)
#endif
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

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, jl_module_t *mod);
static value_t julia_to_scm(fl_context_t *fl_ctx, jl_value_t *v);
static jl_value_t *jl_expand_macros(jl_value_t *expr, jl_module_t *inmodule, struct macroctx_stack *macroctx, int onelevel);

value_t fl_defined_julia_global(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    // tells whether a var is defined in and *by* the current module
    argcount(fl_ctx, "defined-julia-global", nargs, 1);
    (void)tosymbol(fl_ctx, args[0], "defined-julia-global");
    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    jl_sym_t *var = jl_symbol(symbol_name(fl_ctx, args[0]));
    jl_binding_t *b = jl_get_module_binding(ctx->module, var);
    return (b != NULL && b->owner == ctx->module) ? fl_ctx->T : fl_ctx->F;
}

value_t fl_current_module_counter(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    jl_ast_context_t *ctx = jl_ast_ctx(fl_ctx);
    assert(ctx->module);
    return fixnum(jl_module_next_counter(ctx->module));
}

value_t fl_julia_current_file(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    return symbol(fl_ctx, jl_filename);
}

value_t fl_julia_current_line(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    return fixnum(jl_lineno);
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

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, jl_module_t *mod);

value_t fl_julia_logmsg(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    int kwargs_len = (int)nargs - 6;
    if (nargs < 6 || kwargs_len % 2 != 0) {
        lerror(fl_ctx, fl_ctx->ArgError, "julia-logmsg: bad argument list - expected "
               "level (symbol) group (symbol) id file line msg . kwargs");
    }
    value_t arg_level = args[0];
    value_t arg_group = args[1];
    value_t arg_id    = args[2];
    value_t arg_file  = args[3];
    value_t arg_line  = args[4];
    value_t arg_msg   = args[5];
    value_t *arg_kwargs = args + 6;
    if (!isfixnum(arg_level) || !issymbol(arg_group) || !issymbol(arg_id) ||
        !issymbol(arg_file) || !isfixnum(arg_line) || !fl_isstring(fl_ctx, arg_msg)) {
        lerror(fl_ctx, fl_ctx->ArgError,
               "julia-logmsg: Unexpected type in argument list");
    }

    // Abuse scm_to_julia here to convert arguments.  This is meant for `Expr`s
    // but should be good enough provided we're only passing simple numbers,
    // symbols and strings.
    jl_value_t *group=NULL, *id=NULL, *file=NULL, *line=NULL, *msg=NULL;
    jl_array_t *kwargs=NULL;
    JL_GC_PUSH6(&group, &id, &file, &line, &msg, &kwargs);
    group = scm_to_julia(fl_ctx, arg_group, NULL);
    id    = scm_to_julia(fl_ctx, arg_id, NULL);
    file  = scm_to_julia(fl_ctx, arg_file, NULL);
    line  = scm_to_julia(fl_ctx, arg_line, NULL);
    msg   = scm_to_julia(fl_ctx, arg_msg, NULL);
    kwargs = jl_alloc_vec_any(kwargs_len);
    for (int i = 0; i < kwargs_len; ++i) {
        jl_array_ptr_set(kwargs, i, scm_to_julia(fl_ctx, arg_kwargs[i], NULL));
    }
    jl_log(numval(arg_level), NULL, group, id, file, line, (jl_value_t*)kwargs, msg);
    JL_GC_POP();
    return fl_ctx->T;
}

static const builtinspec_t julia_flisp_ast_ext[] = {
    { "defined-julia-global", fl_defined_julia_global },
    { "current-julia-module-counter", fl_current_module_counter },
    { "julia-scalar?", fl_julia_scalar },
    { "julia-logmsg", fl_julia_logmsg },
    { "julia-current-file", fl_julia_current_file },
    { "julia-current-line", fl_julia_current_line },
    { NULL, NULL }
};

static void jl_init_ast_ctx(jl_ast_context_t *ast_ctx) JL_NOTSAFEPOINT
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
    set(symbol(fl_ctx, "*depwarn-opt*"), fixnum(jl_options.depwarn));
    set(symbol(fl_ctx, "*scopewarn-opt*"), fixnum(jl_options.warn_scope));
}

// There should be no GC allocation while holding this lock
static jl_mutex_t flisp_lock;
static jl_ast_context_list_t *jl_ast_ctx_using = NULL;
static jl_ast_context_list_t *jl_ast_ctx_freed = NULL;

static jl_ast_context_t *jl_ast_ctx_enter(void) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
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

static void jl_ast_ctx_leave(jl_ast_context_t *ctx) JL_NOTSAFEPOINT
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
    cfunction_sym = jl_symbol("cfunction");
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
    return_sym = jl_symbol("return");
    unreachable_sym = jl_symbol("unreachable");
    lambda_sym = jl_symbol("lambda");
    module_sym = jl_symbol("module");
    export_sym = jl_symbol("export");
    import_sym = jl_symbol("import");
    using_sym = jl_symbol("using");
    assign_sym = jl_symbol("=");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    pop_exception_sym = jl_symbol("pop_exception");
    new_sym = jl_symbol("new");
    splatnew_sym = jl_symbol("splatnew");
    const_sym = jl_symbol("const");
    global_sym = jl_symbol("global");
    thunk_sym = jl_symbol("thunk");
    abstracttype_sym = jl_symbol("abstract_type");
    primtype_sym = jl_symbol("primitive_type");
    structtype_sym = jl_symbol("struct_type");
    toplevel_sym = jl_symbol("toplevel");
    dot_sym = jl_symbol(".");
    colon_sym = jl_symbol(":");
    boundscheck_sym = jl_symbol("boundscheck");
    inbounds_sym = jl_symbol("inbounds");
    newvar_sym = jl_symbol("newvar");
    copyast_sym = jl_symbol("copyast");
    loopinfo_sym = jl_symbol("loopinfo");
    pure_sym = jl_symbol("pure");
    meta_sym = jl_symbol("meta");
    list_sym = jl_symbol("list");
    unused_sym = jl_symbol("#unused#");
    slot_sym = jl_symbol("slot");
    static_parameter_sym = jl_symbol("static_parameter");
    inline_sym = jl_symbol("inline");
    noinline_sym = jl_symbol("noinline");
    polly_sym = jl_symbol("polly");
    propagate_inbounds_sym = jl_symbol("propagate_inbounds");
    isdefined_sym = jl_symbol("isdefined");
    nospecialize_sym = jl_symbol("nospecialize");
    specialize_sym = jl_symbol("specialize");
    macrocall_sym = jl_symbol("macrocall");
    escape_sym = jl_symbol("escape");
    hygienicscope_sym = jl_symbol("hygienic-scope");
    gc_preserve_begin_sym = jl_symbol("gc_preserve_begin");
    gc_preserve_end_sym = jl_symbol("gc_preserve_end");
    generated_sym = jl_symbol("generated");
    generated_only_sym = jl_symbol("generated_only");
    throw_undef_if_not_sym = jl_symbol("throw_undef_if_not");
    getfield_undefref_sym = jl_symbol("##getfield##");
    do_sym = jl_symbol("do");
    coverageeffect_sym = jl_symbol("code_coverage_effect");
    aliasscope_sym = jl_symbol("aliasscope");
    popaliasscope_sym = jl_symbol("popaliasscope");
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

static jl_value_t *scm_to_julia(fl_context_t *fl_ctx, value_t e, jl_module_t *mod)
{
    jl_value_t *v = NULL;
    JL_GC_PUSH1(&v);
    JL_TRY {
        v = scm_to_julia_(fl_ctx, e, mod);
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

static jl_value_t *scm_to_julia_(fl_context_t *fl_ctx, value_t e, jl_module_t *mod)
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
    if (issymbol(e))
        return (jl_value_t*)scmsym_to_julia(fl_ctx, e);
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
            else if (hd == jl_ast_ctx(fl_ctx)->true_sym && llength(e) == 1)
                return jl_true;
            else if (hd == jl_ast_ctx(fl_ctx)->false_sym && llength(e) == 1)
                return jl_false;
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
        // nodes with special representations
        jl_value_t *ex = NULL, *temp = NULL;
        if (sym == line_sym && (n == 1 || n == 2)) {
            jl_value_t *linenum = scm_to_julia_(fl_ctx, car_(e), mod);
            jl_value_t *file = jl_nothing;
            JL_GC_PUSH2(&linenum, &file);
            if (n == 2)
                file = scm_to_julia_(fl_ctx, car_(cdr_(e)), mod);
            temp = jl_new_struct(jl_linenumbernode_type, linenum, file);
            JL_GC_POP();
            return temp;
        }
        JL_GC_PUSH1(&ex);
        if (sym == goto_sym) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            temp = jl_new_struct(jl_gotonode_type, ex);
        }
        else if (sym == newvar_sym) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            temp = jl_new_struct(jl_newvarnode_type, ex);
        }
        else if (sym == globalref_sym) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            temp = scm_to_julia_(fl_ctx, car_(cdr_(e)), mod);
            assert(jl_is_module(ex));
            assert(jl_is_symbol(temp));
            temp = jl_module_globalref((jl_module_t*)ex, (jl_sym_t*)temp);
        }
        else if (sym == top_sym) {
            assert(mod && "top should not be generated by the parser");
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            assert(jl_is_symbol(ex));
            temp = jl_module_globalref(jl_base_relative_to(mod), (jl_sym_t*)ex);
        }
        else if (sym == core_sym) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            assert(jl_is_symbol(ex));
            temp = jl_module_globalref(jl_core_module, (jl_sym_t*)ex);
        }
        else if (sym == inert_sym || (sym == quote_sym && (!iscons(car_(e))))) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            temp = jl_new_struct(jl_quotenode_type, ex);
        }
        else if (sym == thunk_sym) {
            ex = scm_to_julia_(fl_ctx, car_(e), mod);
            assert(jl_is_code_info(ex));
            jl_linenumber_to_lineinfo((jl_code_info_t*)ex, (jl_value_t*)jl_symbol("top-level scope"));
            temp = (jl_value_t*)jl_exprn(sym, 1);
            jl_exprargset(temp, 0, ex);
        }
        if (temp) {
            JL_GC_POP();
            return temp;
        }
        ex = (jl_value_t*)jl_exprn(sym, n);
        size_t i;
        for (i = 0; i < n; i++) {
            assert(iscons(e));
            jl_array_ptr_set(((jl_expr_t*)ex)->args, i, scm_to_julia_(fl_ctx, car_(e), mod));
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
        uint32_t c, u = *(uint32_t*)cp_data((cprim_t*)ptr(e));
        if (u < 0x80) {
            c = u << 24;
        } else {
            c = ((u << 0) & 0x0000003f) | ((u << 2) & 0x00003f00) |
                ((u << 4) & 0x003f0000) | ((u << 6) & 0x3f000000);
            c = u < 0x00000800 ? (c << 16) | 0xc0800000 :
                u < 0x00010000 ? (c <<  8) | 0xe0808000 :
                                 (c <<  0) | 0xf0808080 ;
        }
        return jl_box_char(c);
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

static int julia_to_scm_noalloc1(fl_context_t *fl_ctx, jl_value_t *v, value_t *retval) JL_NOTSAFEPOINT
{
    if (v == NULL)
        lerror(fl_ctx, symbol(fl_ctx, "error"), "undefined reference in AST");
    else if (jl_is_symbol(v))
        *retval = symbol(fl_ctx, jl_symbol_name((jl_sym_t*)v));
    else if (v == jl_true)
        *retval = fl_cons(fl_ctx, jl_ast_ctx(fl_ctx)->true_sym, fl_ctx->NIL);
    else if (v == jl_false)
        *retval = fl_cons(fl_ctx, jl_ast_ctx(fl_ctx)->false_sym, fl_ctx->NIL);
    else if (v == jl_nothing)
        *retval = fl_cons(fl_ctx, jl_ast_ctx(fl_ctx)->null_sym, fl_ctx->NIL);
    else
        return 0;
    return 1;
}

static value_t julia_to_scm_noalloc2(fl_context_t *fl_ctx, jl_value_t *v) JL_NOTSAFEPOINT
{
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

static value_t julia_to_scm_noalloc(fl_context_t *fl_ctx, jl_value_t *v) JL_NOTSAFEPOINT
{
    value_t retval;
    if (julia_to_scm_noalloc1(fl_ctx, v, &retval))
        return retval;
    assert(!jl_is_expr(v) &&
           !jl_typeis(v, jl_linenumbernode_type) &&
           !jl_typeis(v, jl_gotonode_type) &&
           !jl_typeis(v, jl_quotenode_type) &&
           !jl_typeis(v, jl_newvarnode_type) &&
           !jl_typeis(v, jl_globalref_type));
    return julia_to_scm_noalloc2(fl_ctx, v);
}

static value_t julia_to_list2_noalloc(fl_context_t *fl_ctx, jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    value_t sa = julia_to_scm_noalloc(fl_ctx, a);
    fl_gc_handle(fl_ctx, &sa);
    value_t sb = julia_to_scm_noalloc(fl_ctx, b);
    value_t l = fl_list2(fl_ctx, sa, sb);
    fl_free_gc_handles(fl_ctx, 1);
    return l;
}

static value_t julia_to_scm_(fl_context_t *fl_ctx, jl_value_t *v)
{
    value_t retval;
    if (julia_to_scm_noalloc1(fl_ctx, v, &retval))
        return retval;
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
    // GC Note: jl_fieldref(v, 0) allocates for GotoNode
    //          but we don't need a GC root here because julia_to_list2_noalloc
    //          shouldn't allocate in this case.
    if (jl_typeis(v, jl_linenumbernode_type)) {
        jl_value_t *file = jl_fieldref_noalloc(v,1);
        jl_value_t *line = jl_fieldref(v,0);
        value_t args = julia_to_list2_noalloc(fl_ctx, line, file);
        fl_gc_handle(fl_ctx, &args);
        value_t hd = julia_to_scm_(fl_ctx, (jl_value_t*)line_sym);
        value_t scmv = fl_cons(fl_ctx, hd, args);
        fl_free_gc_handles(fl_ctx, 1);
        return scmv;
    }
    if (jl_typeis(v, jl_gotonode_type))
        return julia_to_list2_noalloc(fl_ctx, (jl_value_t*)goto_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_quotenode_type))
        return julia_to_list2(fl_ctx, (jl_value_t*)inert_sym, jl_fieldref_noalloc(v,0));
    if (jl_typeis(v, jl_newvarnode_type))
        return julia_to_list2_noalloc(fl_ctx, (jl_value_t*)newvar_sym, jl_fieldref(v,0));
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
    return julia_to_scm_noalloc2(fl_ctx, v);
}

// parse an entire string like a file, reading multiple expressions
JL_DLLEXPORT jl_value_t *jl_parse_all(const char *str, size_t len, const char *filename, size_t filename_len)
{
    JL_TIMING(PARSING);
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    value_t s = cvalue_static_cstrn(fl_ctx, str, len);
    value_t files = cvalue_static_cstrn(fl_ctx, filename, filename_len);
    value_t e = fl_applyn(fl_ctx, 2, symbol_value(symbol(fl_ctx, "jl-parse-all")), s, files);
    jl_value_t *res = e == fl_ctx->FL_EOF ? jl_nothing : scm_to_julia(fl_ctx, e, NULL);
    jl_ast_ctx_leave(ctx);
    return res;
}

// for backwards compat
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len, const char *filename, size_t filename_len)
{
    return jl_parse_all(str, len, filename, filename_len);
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
    value_t p = fl_applyn(fl_ctx, 3, symbol_value(symbol(fl_ctx, "jl-parse-one")),
                          s, fixnum(pos0), greedy?fl_ctx->T:fl_ctx->F);
    jl_value_t *expr=NULL, *pos1=NULL;
    JL_GC_PUSH2(&expr, &pos1);

    value_t e = car_(p);
    if (e == fl_ctx->FL_EOF)
        expr = jl_nothing;
    else
        expr = scm_to_julia(fl_ctx, e, NULL);

    pos1 = jl_box_long(tosize(fl_ctx, cdr_(p), "parse"));
    jl_ast_ctx_leave(ctx);
    jl_value_t *result = (jl_value_t*)jl_svec2(expr, pos1);
    JL_GC_POP();
    return result;
}

// parse and eval a whole file, possibly reading from a string (`content`)
jl_value_t *jl_parse_eval_all(const char *fname,
                              const char *content, size_t contentlen,
                              jl_module_t *inmodule,
                              jl_value_t *mapexpr)
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
        ast = fl_applyn(fl_ctx, 2, symbol_value(symbol(fl_ctx, "jl-parse-all")), t, f);
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
    int lineno = 0;
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
                    form = scm_to_julia(fl_ctx, expression, inmodule);
                    if (mapexpr)
                        form = jl_call1(mapexpr, form);
                    form = jl_expand_macros(form, inmodule, NULL, 0);
                    expression = julia_to_scm(fl_ctx, form);
                }
                else if (mapexpr) {
                    form = scm_to_julia(fl_ctx, expression, inmodule);
                    form = jl_call1(mapexpr, form);
                    expression = julia_to_scm(fl_ctx, form);
                }
                // expand non-final expressions in statement position (value unused)
                expression =
                    fl_applyn(fl_ctx, 4,
                              symbol_value(symbol(fl_ctx, "jl-expand-to-thunk-warn")),
                              expression, symbol(fl_ctx, fname), fixnum(lineno),
                              iscons(cdr_(ast)) ? fl_ctx->T : fl_ctx->F);
            }
            jl_get_ptls_states()->world_age = jl_world_counter;
            form = scm_to_julia(fl_ctx, expression, inmodule);
            JL_SIGATOMIC_END();
            jl_get_ptls_states()->world_age = jl_world_counter;
            if (jl_is_linenode(form)) {
                lineno = jl_linenode_line(form);
                jl_lineno = lineno;
            }
            else {
                result = jl_toplevel_eval_flex(inmodule, form, 1, 1);
            }
            JL_SIGATOMIC_BEGIN();
            ast = cdr_(ast);
        }
    }
    JL_CATCH {
        form = jl_pchar_to_string(fname, len);
        result = jl_box_long(jl_lineno);
        err = 1;
        goto finally; // skip jl_restore_excstack
    }
finally:
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
                                           jl_current_exception()));
    }
    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_load_rewrite_file_string(const char *text, size_t len,
                                                     char *filename, jl_module_t *inmodule,
                                                     jl_value_t *mapexpr)
{
    return jl_parse_eval_all(filename, text, len, inmodule, mapexpr);
}

JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *inmodule)
{
    return jl_parse_eval_all(filename, text, len, inmodule, NULL);
}

// returns either an expression or a thunk
jl_value_t *jl_call_scm_on_ast(const char *funcname, jl_value_t *expr, jl_module_t *inmodule)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    JL_AST_PRESERVE_PUSH(ctx, old_roots, inmodule);
    value_t arg = julia_to_scm(fl_ctx, expr);
    value_t e = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, funcname)), arg);
    jl_value_t *result = scm_to_julia(fl_ctx, e, inmodule);
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
    return result;
}

jl_value_t *jl_call_scm_on_ast_and_loc(const char *funcname, jl_value_t *expr, jl_module_t *inmodule,
                                       const char *file, int line)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    JL_AST_PRESERVE_PUSH(ctx, old_roots, inmodule);
    value_t arg = julia_to_scm(fl_ctx, expr);
    value_t e = fl_applyn(fl_ctx, 3, symbol_value(symbol(fl_ctx, funcname)), arg,
                          symbol(fl_ctx, file), fixnum(line));
    jl_value_t *result = scm_to_julia(fl_ctx, e, inmodule);
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
    return result;
}

// syntax tree accessors

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (expr && jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_len(e->args);
        jl_expr_t *ne = jl_exprn(e->head, l);
        JL_GC_PUSH2(&ne, &expr);
        for (i = 0; i < l; i++) {
            jl_value_t *a = jl_exprarg(e, i);
            jl_exprargset(ne, i, jl_copy_ast(a));
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

JL_DLLEXPORT int jl_is_unary_operator(char *sym)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    int res = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "unary-op?")), symbol(fl_ctx, sym)) == fl_ctx->T;
    jl_ast_ctx_leave(ctx);
    return res;
}

JL_DLLEXPORT int jl_is_unary_and_binary_operator(char *sym)
{
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    int res = fl_applyn(fl_ctx, 1, symbol_value(symbol(fl_ctx, "unary-and-binary-op?")), symbol(fl_ctx, sym)) == fl_ctx->T;
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
    if (!jl_typeis(lno, jl_linenumbernode_type)) {
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
        jl_method_instance_t *mfunc = jl_method_lookup(margs, nargs, 1, world);
        JL_GC_PROMISE_ROOTED(mfunc);
        if (mfunc == NULL) {
            jl_method_error(margs[0], &margs[1], nargs, world);
            // unreachable
        }
        *ctx = mfunc->def.method->module;
        result = jl_invoke(margs[0], &margs[1], nargs - 1, mfunc);
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
                                           jl_current_exception()));
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
    if (e->head == do_sym && jl_expr_nargs(e) == 2 && jl_is_expr(jl_exprarg(e, 0)) &&
        ((jl_expr_t*)jl_exprarg(e, 0))->head == macrocall_sym) {
        jl_expr_t *mc = (jl_expr_t*)jl_exprarg(e, 0);
        size_t nm = jl_expr_nargs(mc);
        jl_expr_t *mc2 = jl_exprn(macrocall_sym, nm+1);
        JL_GC_PUSH1(&mc2);
        jl_exprargset(mc2, 0, jl_exprarg(mc, 0));  // macro name
        jl_exprargset(mc2, 1, jl_exprarg(mc, 1));  // location
        jl_exprargset(mc2, 2, jl_exprarg(e, 1));   // function argument
        size_t j;
        for (j = 2; j < nm; j++) {
            jl_exprargset(mc2, j+1, jl_exprarg(mc, j));
        }
        jl_value_t *ret = jl_expand_macros((jl_value_t*)mc2, inmodule, macroctx, onelevel);
        JL_GC_POP();
        return ret;
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
    expr = jl_call_scm_on_ast("jl-expand-macroscope", expr, inmodule);
    JL_GC_POP();
    return expr;
}

JL_DLLEXPORT jl_value_t *jl_macroexpand1(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 1);
    expr = jl_call_scm_on_ast("jl-expand-macroscope", expr, inmodule);
    JL_GC_POP();
    return expr;
}

// Lower an expression tree into Julia's intermediate-representation.
JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_with_loc(expr, inmodule, "none", 0);
}

// Lowering, with starting program location specified
JL_DLLEXPORT jl_value_t *jl_expand_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                            const char *file, int line)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0);
    expr = jl_call_scm_on_ast_and_loc("jl-expand-to-thunk", expr, inmodule, file, line);
    JL_GC_POP();
    return expr;
}

// Same as the above, but printing warnings when applicable
JL_DLLEXPORT jl_value_t *jl_expand_with_loc_warn(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0);
    jl_ast_context_t *ctx = jl_ast_ctx_enter();
    fl_context_t *fl_ctx = &ctx->fl;
    JL_AST_PRESERVE_PUSH(ctx, old_roots, inmodule);
    value_t arg = julia_to_scm(fl_ctx, expr);
    value_t e = fl_applyn(fl_ctx, 4, symbol_value(symbol(fl_ctx, "jl-expand-to-thunk-warn")), arg,
                          symbol(fl_ctx, file), fixnum(line), fl_ctx->F);
    expr = scm_to_julia(fl_ctx, e, inmodule);
    JL_AST_PRESERVE_POP(ctx, old_roots);
    jl_ast_ctx_leave(ctx);
    JL_GC_POP();
    return expr;
}

// expand in a context where the expression value is unused
JL_DLLEXPORT jl_value_t *jl_expand_stmt_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line)
{
    JL_TIMING(LOWERING);
    JL_GC_PUSH1(&expr);
    expr = jl_copy_ast(expr);
    expr = jl_expand_macros(expr, inmodule, NULL, 0);
    expr = jl_call_scm_on_ast_and_loc("jl-expand-to-thunk-stmt", expr, inmodule, file, line);
    JL_GC_POP();
    return expr;
}

JL_DLLEXPORT jl_value_t *jl_expand_stmt(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_stmt_with_loc(expr, inmodule, "none", 0);
}

#ifdef __cplusplus
}
#endif
