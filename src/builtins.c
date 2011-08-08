/*
  implementations of some built-in functions and utilities
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include "julia.h"
#include "builtin_proto.h"

// --- exceptions ---

DLLEXPORT char *julia_home = NULL;

void jl_error(const char *str)
{
    jl_value_t *msg = jl_pchar_to_string((char*)str, strlen(str));
    JL_GC_PUSH(&msg);
    jl_raise(jl_new_struct(jl_errorexception_type, msg));
}

void jl_errorf(const char *fmt, ...)
{
    char buf[1024];
    va_list args;
    va_start(args, fmt);
    int nc = vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);
    jl_value_t *msg = jl_pchar_to_string(buf, nc);
    JL_GC_PUSH(&msg);
    jl_raise(jl_new_struct(jl_errorexception_type, msg));
}

void jl_too_few_args(const char *fname, int min)
{
    // TODO: ArgumentError
    jl_errorf("%s: too few arguments (expected %d)", fname, min);
}

void jl_too_many_args(const char *fname, int max)
{
    jl_errorf("%s: too many arguments (expected %d)", fname, max);
}

void jl_type_error_rt(const char *fname, const char *context,
                      jl_value_t *ty, jl_value_t *got)
{
    jl_value_t *ctxt=NULL;
    JL_GC_PUSH(&ctxt, &got);
    ctxt = jl_pchar_to_string((char*)context, strlen(context));
    jl_value_t *ex = jl_new_struct(jl_typeerror_type, jl_symbol(fname),
                                   ctxt, ty, got);
    jl_raise(ex);
}

void jl_type_error(const char *fname, jl_value_t *expected, jl_value_t *got)
{
    jl_type_error_rt(fname, "", expected, got);
}

void jl_undef_ref_error()
{
    jl_raise(jl_undefref_exception);
}

void jl_divide_by_zero_error()
{
    jl_raise(jl_divbyzero_exception);
}

JL_CALLABLE(jl_f_throw)
{
    JL_NARGS(throw, 1, 1);
    jl_raise(args[0]);
    return (jl_value_t*)jl_null;
}

void jl_enter_handler(jl_savestate_t *ss, jmp_buf *handlr)
{
    JL_SIGATOMIC_BEGIN();
    ss->eh_task = jl_current_task->state.eh_task;
    ss->eh_ctx = jl_current_task->state.eh_ctx;
    ss->ostream_obj = jl_current_task->state.ostream_obj;
    ss->current_output_stream = jl_current_task->state.current_output_stream;
    ss->prev = jl_current_task->state.prev;
#ifdef JL_GC_MARKSWEEP
    ss->gcstack = jl_current_task->state.gcstack;
#endif

    jl_current_task->state.prev = ss;
    jl_current_task->state.eh_task = jl_current_task;
    jl_current_task->state.eh_ctx = handlr;
    // TODO: this should really go after setjmp(). see comment in
    // ctx_switch in task.c.
    JL_SIGATOMIC_END();
}

void jl_pop_handler(int n)
{
    while (n > 0) {
        jl_eh_restore_state(jl_current_task->state.prev);
        n--;
    }
}

// --- primitives ---

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(is, 2, 2);
    if (args[0] == args[1])
        return jl_true;
    return jl_false;
}

JL_CALLABLE(jl_f_no_function)
{
    jl_error("function not defined");
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_typeof)
{
    JL_NARGS(typeof, 1, 1);
    return jl_full_type(args[0]);
}

JL_CALLABLE(jl_f_subtype)
{
    JL_NARGS(subtype, 2, 2);
    if (!jl_is_typector(args[0]) && !jl_is_typevar(args[0]))
        JL_TYPECHK(subtype, type, args[0]);
    if (!jl_is_typector(args[1]) && !jl_is_typevar(args[1]))
        JL_TYPECHK(subtype, type, args[1]);
    return (jl_subtype(args[0],args[1],0) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_isa)
{
    JL_NARGS(isa, 2, 2);
    if (!jl_is_typector(args[1]))
        JL_TYPECHK(isa, type, args[1]);
    return (jl_subtype(args[0],args[1],1) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_typeassert)
{
    JL_NARGS(typeassert, 2, 2);
    if (!jl_is_typector(args[1]))
        JL_TYPECHK(typeassert, type, args[1]);
    if (!jl_subtype(args[0],args[1],1))
        jl_type_error("typeassert", args[1], args[0]);
    return args[0];
}

JL_CALLABLE(jl_f_apply)
{
    JL_NARGSV(apply, 1);
    JL_TYPECHK(apply, function, args[0]);
    if (nargs == 2 && jl_is_tuple(args[1])) {
        return jl_apply((jl_function_t*)args[0], &jl_tupleref(args[1],0),
                        ((jl_tuple_t*)args[1])->length);
    }
    size_t n=0, i, j;
    for(i=1; i < nargs; i++) {
        if (jl_is_tuple(args[i])) {
            n += ((jl_tuple_t*)args[i])->length;
        }
        else if (jl_typeis(args[i], jl_array_any_type)) {
            n += jl_array_len(args[i]);
        }
        else {
            if (jl_append_any_func == NULL) {
                // error if append_any not available
                JL_TYPECHK(apply, tuple, args[i]);
            }
            goto fancy_apply;
        }
    }
    jl_value_t **newargs = alloca(n * sizeof(jl_value_t*));
    n = 0;
    for(i=1; i < nargs; i++) {
        if (jl_is_tuple(args[i])) {
            jl_tuple_t *t = (jl_tuple_t*)args[i];
            for(j=0; j < t->length; j++)
                newargs[n++] = jl_tupleref(t, j);
        }
        else {
            size_t al = jl_array_len(args[i]);
            for(j=0; j < al; j++)
                newargs[n++] = jl_cellref(args[i], j);
        }
    }
    return jl_apply((jl_function_t*)args[0], newargs, n);

 fancy_apply: ;
    jl_value_t *argarr = jl_apply(jl_append_any_func, &args[1], nargs-1);
    JL_GC_PUSH(&argarr);
    assert(jl_typeis(argarr, jl_array_any_type));
    jl_value_t *result = jl_apply((jl_function_t*)args[0],
                                  jl_cell_data(argarr), jl_array_len(argarr));
    JL_GC_POP();
    return result;
}

// eval -----------------------------------------------------------------------

static int is_intrinsic(jl_sym_t *s)
{
    jl_value_t **bp = jl_get_bindingp(jl_system_module, s);
    return (*bp != NULL && jl_typeof(*bp)==(jl_type_t*)jl_intrinsic_type);
}

static int has_intrinsics(jl_expr_t *e)
{
    if (e->head == call_sym && jl_is_symbol(jl_exprarg(e,0)) &&
        is_intrinsic((jl_sym_t*)jl_exprarg(e,0)))
        return 1;
    int i;
    for(i=0; i < e->args->length; i++) {
        jl_value_t *a = jl_exprarg(e,i);
        if (jl_is_expr(a) && has_intrinsics((jl_expr_t*)a))
            return 1;
    }
    return 0;
}

// heuristic for whether a top-level input should be evaluated with
// the compiler or the interpreter.
static int eval_with_compiler_p(jl_expr_t *expr, int compileloops)
{
    if (expr->head==body_sym) {
        jl_array_t *body = expr->args;
        size_t i;
        for(i=0; i < body->length; i++) {
            jl_value_t *stmt = jl_cellref(body,i);
            if (jl_is_expr(stmt)) {
                // TODO: only backward branches
                if (compileloops &&
                    (((jl_expr_t*)stmt)->head == goto_sym ||
                     ((jl_expr_t*)stmt)->head == goto_ifnot_sym)) {
                    return 1;
                }
            }
        }
    }
    if (has_intrinsics(expr))
        return 1;
    return 0;
}

jl_value_t *jl_new_closure_internal(jl_lambda_info_t *li, jl_value_t *env);

jl_value_t *jl_toplevel_eval_flex(jl_value_t *ex, int fast)
{
    //jl_show(ex);
    //ios_printf(ios_stdout, "\n");
    jl_lambda_info_t *thk;
    int ewc = 0;
    if (jl_typeof(ex) != (jl_type_t*)jl_lambda_info_type) {
        if (jl_is_expr(ex) && eval_with_compiler_p((jl_expr_t*)ex, fast)) {
            thk = jl_wrap_expr(ex);
            ewc = 1;
        }
        else {
            return jl_interpret_toplevel_expr(ex);
        }
    }
    else {
        thk = (jl_lambda_info_t*)ex;
        ewc = eval_with_compiler_p(jl_lam_body((jl_expr_t*)thk->ast), fast);
        if (!ewc) {
            jl_array_t *vinfos = jl_lam_vinfo((jl_expr_t*)thk->ast);
            int i;
            for(i=0; i < vinfos->length; i++) {
                if (jl_cellref(jl_cellref(vinfos,i),2)!=jl_false) {
                    // interpreter doesn't handle closure environment
                    ewc = 1;
                    break;
                }
            }
        }
    }
    jl_value_t *thunk=NULL;
    jl_function_t *gf=NULL;
    jl_value_t *result;
    JL_GC_PUSH(&thunk, &gf, &thk);
    if (ewc) {
        thunk = jl_new_closure_internal(thk, (jl_value_t*)jl_null);
        // use a generic function so type inference runs
        gf = jl_new_generic_function(lambda_sym);
        jl_add_method(gf, jl_null, (jl_function_t*)thunk);
        result = jl_apply(gf, NULL, 0);
    }
    else {
        result = jl_interpret_toplevel_thunk(thk);
    }
    JL_GC_POP();
    return result;
}

jl_value_t *jl_toplevel_eval(jl_value_t *v)
{
    return jl_toplevel_eval_flex(v, 1);
}

int asprintf(char **strp, const char *fmt, ...);

// load toplevel expressions, from (file ...)
void jl_load_file_expr(char *fname, jl_value_t *ast)
{
    jl_array_t *b = ((jl_expr_t*)ast)->args;
    size_t i;
    volatile size_t lineno=0;
    if (((jl_expr_t*)ast)->head == jl_continue_sym) {
        jl_errorf("syntax error: %s", jl_string_data(jl_exprarg(ast,0)));
    }
    JL_TRY {
        // handle syntax error
        if (((jl_expr_t*)ast)->head == error_sym) {
            jl_interpret_toplevel_expr(ast);
        }
        jl_value_t *form=NULL;
        JL_GC_PUSH(&form);
        for(i=0; i < b->length; i++) {
            // process toplevel form
            form = jl_cellref(b, i);
            if (jl_is_linenode(form)) {
                lineno = jl_linenode_line(form);
            }
            else {
                if (jl_is_expr(form) &&
                    ((jl_expr_t*)form)->head == unexpanded_sym) {
                    // delayed expansion, for macro calls
                    form = jl_exprarg(form, 0);
                    form = jl_expand(form);
                }
                //(void)jl_interpret_toplevel_thunk(form);
                (void)jl_toplevel_eval_flex(form, 0);
            }
        }
        JL_GC_POP();
    }
    JL_CATCH {
        jl_value_t *fn=NULL, *ln=NULL;
        JL_GC_PUSH(&fn, &ln);
        fn = jl_pchar_to_string(fname, strlen(fname));
        ln = jl_box_long(lineno);
        jl_raise(jl_new_struct(jl_loaderror_type, fn, ln,
                               jl_exception_in_transit));
    }
}

// locate a file in the search path
// fpath needs to be freed if != fname
char *jl_find_file_in_path(const char *fname)
{
    char *fpath = (char*)fname;
    int fid = open (fpath, O_RDONLY);
    // try adding just .j
    if (fid == -1) {
        asprintf(&fpath, "%s.j", fname);
        fid = open (fpath, O_RDONLY);
    }
    // try adding julia home, then julia home, then julia_home/j/ and .j
    if (fid == -1 && julia_home && fname[0] != '/') {
        assert(fpath != fname);
        free(fpath);
        asprintf(&fpath, "%s/%s", julia_home, fname);
        fid = open (fpath, O_RDONLY);
        if (fid == -1) {
            free(fpath);
            asprintf(&fpath, "%s/%s.j", julia_home, fname);
            fid = open (fpath, O_RDONLY);
        }
        if (fid == -1) {
            free(fpath);
            asprintf(&fpath, "%s/j/%s", julia_home, fname);
            fid = open (fpath, O_RDONLY);
        }
        if (fid == -1) {
            free(fpath);
            asprintf(&fpath, "%s/j/%s.j", julia_home, fname);
            fid = open (fpath, O_RDONLY);
        }
    }
    if (fid == -1) {
        assert(fpath != fname);
        free(fpath);
        if (jl_errorexception_type == NULL) {
            ios_printf(ios_stderr, "could not open file %s\n", fname);
            exit(1);
        }
        else {
            jl_errorf("could not open file %s", fname);
        }
    }
    close(fid);

    return fpath;
}

void jl_load(const char *fname)
{
    char *fpath = jl_find_file_in_path(fname);
    jl_value_t *ast = jl_parse_file(fpath);
    if (ast == (jl_value_t*)jl_null)  {
        if (fpath != fname) free(fpath);
	jl_errorf("could not open file %s", fpath);
    }
    JL_GC_PUSH(&ast);
    jl_load_file_expr(fpath, ast);
    JL_GC_POP();
    if (fpath != fname) free(fpath);
}

JL_CALLABLE(jl_f_top_eval)
{
    JL_NARGS(eval, 1, 1);
    jl_value_t *e = args[0];
    if (!jl_is_expr(e))
        return jl_interpret_toplevel_expr(e);
    jl_expr_t *ex = (jl_expr_t*)e;
    if (ex->head == top_sym ||
        ex->head == quote_sym || ex->head == null_sym ||
        ex->head == isbound_sym || ex->head == error_sym) {
        // expression types simple enough not to need expansion
        return jl_interpret_toplevel_expr(e);
    }
    jl_value_t *exex = NULL;
    JL_GC_PUSH(&exex);
    if (ex->head == body_sym || ex->head == lambda_sym) {
        // already expanded
        exex = e;
    }
    else {
        exex = jl_expand(e);
    }
    jl_value_t *result = jl_toplevel_eval(exex);
    JL_GC_POP();
    return result;
}

JL_CALLABLE(jl_f_isbound)
{
    JL_NARGS(isbound, 1, 1);
    JL_TYPECHK(isbound, symbol, args[0]);
    return jl_boundp(jl_system_module, (jl_sym_t*)args[0]) ? jl_true : jl_false;
}

// tuples ---------------------------------------------------------------------

JL_CALLABLE(jl_f_tuple)
{
    size_t i;
    if (nargs == 0) return (jl_value_t*)jl_null;
    jl_tuple_t *t = jl_alloc_tuple_uninit(nargs);
    for(i=0; i < nargs; i++) {
        jl_tupleset(t, i, args[i]);
    }
    return (jl_value_t*)t;
}

JL_CALLABLE(jl_f_tupleref)
{
    JL_NARGS(tupleref, 2, 2);
    JL_TYPECHK(tupleref, tuple, args[0]);
    JL_TYPECHK(tupleref, long, args[1]);
    jl_tuple_t *t = (jl_tuple_t*)args[0];
    size_t i = jl_unbox_long(args[1])-1;
    if (i >= t->length)
        jl_error("tupleref: index out of range");
    return jl_tupleref(t, i);
}

JL_CALLABLE(jl_f_tuplelen)
{
    JL_NARGS(tuplelen, 1, 1);
    JL_TYPECHK(tuplelen, tuple, args[0]);
    return jl_box_long(((jl_tuple_t*)args[0])->length);
}

// structs --------------------------------------------------------------------

static size_t field_offset(jl_struct_type_t *t, jl_sym_t *fld, int err)
{
    jl_tuple_t *fn = t->names;
    size_t i;
    for(i=0; i < fn->length; i++) {
        if (jl_tupleref(fn,i) == (jl_value_t*)fld) {
            if (t == jl_struct_kind || t == jl_bits_kind || t == jl_tag_kind)
                i += 3;
            return i;
        }
    }
    if (err)
        jl_errorf("type %s has no field %s", t->name->name->name, fld->name);
    return -1;
}

size_t jl_field_offset(jl_struct_type_t *t, jl_sym_t *fld)
{
    return field_offset(t, fld, 0);
}

static jl_value_t *nth_field(jl_value_t *v, size_t i)
{
    jl_value_t *fld = ((jl_value_t**)v)[1+i];
    if (fld == NULL)
        jl_undef_ref_error();
    return fld;
}

JL_CALLABLE(jl_f_get_field)
{
    JL_NARGS(getfield, 2, 2);
    JL_TYPECHK(getfield, symbol, args[1]);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (!jl_is_struct_type(vt))
        jl_type_error("getfield", (jl_value_t*)jl_struct_kind, v);
    size_t i = field_offset((jl_struct_type_t*)vt, (jl_sym_t*)args[1], 1);
    return nth_field(v, i);
}

JL_CALLABLE(jl_f_set_field)
{
    JL_NARGS(setfield, 3, 3);
    JL_TYPECHK(setfield, symbol, args[1]);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (!jl_is_struct_type(vt))
        jl_type_error("setfield", (jl_value_t*)jl_struct_kind, v);
    jl_struct_type_t *st = (jl_struct_type_t*)vt;
    size_t i = field_offset(st, (jl_sym_t*)args[1], 1);
    jl_value_t *ft = jl_tupleref(st->types,i);
    if (!jl_subtype(args[2], ft, 1)) {
        jl_type_error("setfield", ft, args[2]);
    }
    ((jl_value_t**)v)[1+i] = args[2];
    return args[2];
}

JL_CALLABLE(jl_f_field_type)
{
    JL_NARGS(fieldtype, 2, 2);
    JL_TYPECHK(fieldtype, symbol, args[1]);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (!jl_is_struct_type(vt))
        jl_type_error("fieldtype", (jl_value_t*)jl_struct_kind, v);
    jl_struct_type_t *st = (jl_struct_type_t*)vt;
    size_t i = field_offset(st, (jl_sym_t*)args[1], 1);
    return jl_tupleref(st->types, i);
}

// --- conversions ---

JL_CALLABLE(jl_f_convert_tuple)
{
    jl_tuple_t *to = (jl_tuple_t*)args[0];
    jl_tuple_t *x = (jl_tuple_t*)args[1];
    if (to == jl_tuple_type)
        return (jl_value_t*)x;
    size_t i, cl=x->length, pl=to->length;
    jl_tuple_t *out = jl_alloc_tuple(cl);
    JL_GC_PUSH(&out);
    jl_value_t *ce, *pe=NULL;
    int pseq=0;
    for(i=0; i < cl; i++) {
        ce = jl_tupleref(x,i);
        if (pseq) {
        }
        else if (i < pl) {
            pe = jl_tupleref(to,i);
            if (jl_is_seq_type(pe)) {
                pe = jl_tparam0(pe);
                pseq = 1;
            }
        }
        else {
            out = NULL;
            break;
        }
        assert(pe != NULL);
        jl_tupleset(out, i, jl_convert((jl_type_t*)pe, ce));
    }
    JL_GC_POP();
    if (out == NULL)
        jl_error("convert: invalid tuple conversion");
    return (jl_value_t*)out;
}

jl_function_t *jl_convert_gf;

jl_value_t *jl_convert(jl_type_t *to, jl_value_t *x)
{
    jl_value_t *args[2];
    args[0] = (jl_value_t*)to; args[1] = x;
    return jl_apply(jl_convert_gf, args, 2);
}

JL_CALLABLE(jl_f_convert)
{
    JL_NARGS(convert, 2, 2);
    if (!jl_is_typector(args[0]))
        JL_TYPECHK(convert, type, args[0]);
    jl_type_t *to = (jl_type_t*)args[0];
    jl_value_t *x = args[1];
    if (!jl_subtype(x, (jl_value_t*)to, 1)) {
        jl_no_method_error(jl_convert_gf, args, 2);
    }
    return x;
}

DLLEXPORT void *jl_symbol_name(jl_sym_t *s)
{
    return s->name;
}

DLLEXPORT void *jl_array_ptr(jl_array_t *a)
{
    return a->data;
}

// --- printing ---

JL_CALLABLE(jl_f_print_array_uint8)
{
    ios_t *s = jl_current_output_stream();
    jl_array_t *b = (jl_array_t*)args[0];
    ios_write(s, (char*)b->data, b->length);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_print_symbol)
{
    ios_t *s = jl_current_output_stream();
    ios_puts(((jl_sym_t*)args[0])->name, s);
    return (jl_value_t*)jl_nothing;
}

// --- showing ---

jl_function_t *jl_show_gf;

void jl_show(jl_value_t *v)
{
    jl_apply(jl_show_gf, &v, 1);
}

// comma_one prints a comma for 1 element, e.g. "(x,)"
static void show_tuple(jl_tuple_t *t, char opn, char cls, int comma_one)
{
    ios_t *s = jl_current_output_stream();
    ios_putc(opn, s);
    size_t i, n=t->length;
    for(i=0; i < n; i++) {
        jl_show(jl_tupleref(t, i));
        if ((i < n-1) || (n==1 && comma_one))
            ios_putc(',', s);
    }
    ios_putc(cls, s);
}

static void show_function(jl_value_t *v)
{
    ios_t *s = jl_current_output_stream();
    if (jl_is_gf(v)) {
        ios_putc('(', s);
        ios_puts(jl_gf_name(v)->name, s);
        ios_putc(')', s);
    }
    else {
        ios_puts("#<function>", s);
    }
}

void jl_show_full_function(jl_value_t *v)
{
    ios_t *s = jl_current_output_stream();
    if (jl_is_gf(v)) {
        ios_puts("Methods for generic function ", s);
        ios_puts(jl_gf_name(v)->name, s);
        ios_putc('\n', s);
        jl_show_method_table((jl_function_t*)v);
    }
    else {
        show_function(v);
    }
}

static void show_type(jl_value_t *t)
{
    ios_t *s = jl_current_output_stream();
    if (jl_is_func_type(t)) {
        if (t == (jl_value_t*)jl_any_func) {
            ios_printf(s, "Function");
        }
        else {
            jl_show((jl_value_t*)((jl_func_type_t*)t)->from);
            ios_write(s, "-->", 3);
            jl_show((jl_value_t*)((jl_func_type_t*)t)->to);
        }
    }
    else if (jl_is_union_type(t)) {
        if (t == (jl_value_t*)jl_bottom_type) {
            ios_write(s, "None", 4);
        }
        else {
            ios_write(s, "Union", 5);
            show_tuple(((jl_uniontype_t*)t)->types, '(', ')', 0);
        }
    }
    else if (jl_is_seq_type(t)) {
        jl_show(jl_tparam0(t));
        ios_write(s, "...", 3);
    }
    else {
        assert(jl_is_some_tag_type(t));
        jl_tag_type_t *tt = (jl_tag_type_t*)t;
        ios_puts(tt->name->name->name, s);
        jl_tuple_t *p = tt->parameters;
        if (p->length > 0)
            show_tuple(p, '{', '}', 0);
    }
}

static void show_int(void *data, int nbits)
{
    ios_t *s = jl_current_output_stream();
    switch (nbits) {
    case 8:
        ios_printf(s, "%hhd", *(int8_t*)data);
        break;
    case 16:
        ios_printf(s, "%hd", *(int16_t*)data);
        break;
    case 32:
        ios_printf(s, "%d", *(int32_t*)data);
        break;
    case 64:
        ios_printf(s, "%lld", *(int64_t*)data);
        break;
    default:
        jl_error("print: unsupported integer size");
    }
}

static void show_uint(void *data, int nbits)
{
    ios_t *s = jl_current_output_stream();
    switch (nbits) {
    case 8:
        ios_printf(s, "%hhu", *(int8_t*)data);
        break;
    case 16:
        ios_printf(s, "%hu", *(int16_t*)data);
        break;
    case 32:
        ios_printf(s, "%u", *(int32_t*)data);
        break;
    case 64:
        ios_printf(s, "%llu", *(int64_t*)data);
        break;
    default:
        jl_error("print: unsupported integer size");
    }
}

static void show_float64(double d, int single)
{
    ios_t *s = jl_current_output_stream();
    char buf[64];
    int ndec = single ? 9 : 17;
    if (!DFINITE(d)) {
        char *rep;
        if (isnan(d))
            rep = "NaN";
        else
            rep = sign_bit(d) ? "-Inf" : "Inf";
        if (single)
            ios_printf(s, "float32(%s)", rep);
        else
            ios_puts(rep, s);
    }
    else if (d == 0) {
        if (1/d < 0)
            ios_puts("-0.0", s);
        else
            ios_puts("0.0", s);
    }
    else {
        snprint_real(buf, sizeof(buf), d, 0, ndec, 3, 10);
        int hasdec = (strpbrk(buf, ".eE") != NULL);
	    ios_puts(buf, s);
        if (!hasdec) ios_puts(".0", s);
    }
}

JL_CALLABLE(jl_f_show_bool)
{
    ios_t *s = jl_current_output_stream();
    if (jl_unbox_bool(args[0]) == 0)
        ios_puts("false", s);
    else
        ios_puts("true", s);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_char)
{
    ios_t *s = jl_current_output_stream();
    u_int32_t wc = *(uint32_t*)jl_bits_data(args[0]);
    ios_putc('\'', s);
    ios_pututf8(s, wc);
    ios_putc('\'', s);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_float32)
{
    show_float64((double)*(float*)jl_bits_data(args[0]), 1);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_float64)
{
    show_float64(*(double*)jl_bits_data(args[0]), 0);
    return (jl_value_t*)jl_nothing;
}

#define INT_SHOW_FUNC(sgn,nb)               \
JL_CALLABLE(jl_f_show_##sgn##nb)            \
{                                           \
    show_##sgn(jl_bits_data(args[0]), nb);  \
    return (jl_value_t*)jl_nothing;         \
}

INT_SHOW_FUNC(int,8)
INT_SHOW_FUNC(uint,8)
INT_SHOW_FUNC(int,16)
INT_SHOW_FUNC(uint,16)
INT_SHOW_FUNC(int,32)
INT_SHOW_FUNC(uint,32)
INT_SHOW_FUNC(int,64)
INT_SHOW_FUNC(uint,64)

JL_CALLABLE(jl_f_show_pointer)
{
    ios_t *s = jl_current_output_stream();
    void *ptr = *(void**)jl_bits_data(args[0]);
    if (jl_typeis(args[0],jl_pointer_void_type))
        ios_printf(s, "Ptr{Void}");
    else
        jl_show((jl_value_t*)jl_typeof(args[0]));
#ifdef __LP64__
    ios_printf(s, " @0x%016x", (uptrint_t)ptr);
#else
    ios_printf(s, " @0x%08x", (uptrint_t)ptr);
#endif
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_typevar)
{
    ios_t *s = jl_current_output_stream();
    jl_tvar_t *tv = (jl_tvar_t*)args[0];
    if (tv->lb != (jl_value_t*)jl_bottom_type) {
        jl_show((jl_value_t*)tv->lb);
        ios_puts("<:", s);
    }
    ios_puts(tv->name->name, s);
    if (tv->ub != (jl_value_t*)jl_any_type) {
        ios_puts("<:", s);
        jl_show((jl_value_t*)tv->ub);
    }
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_linfo)
{
    ios_t *s = jl_current_output_stream();
    ios_puts("AST(", s);
    jl_show(((jl_lambda_info_t*)args[0])->ast);
    ios_putc(')', s);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_show_any)
{
    JL_NARGS(print, 1, 1);
    // fallback for printing some other builtin types
    ios_t *s = jl_current_output_stream();
    jl_value_t *v = args[0];
    if (jl_is_tuple(v)) {
        show_tuple((jl_tuple_t*)v, '(', ')', 1);
    }
    else if (jl_is_type(v)) {
        show_type(v);
    }
    else if (jl_is_func(v)) {
        show_function(v);
    }
    else if (jl_typeis(v,jl_intrinsic_type)) {
        ios_printf(s, "#<intrinsic-function %d>", *(uint32_t*)jl_bits_data(v));
    }
    else if (v == (jl_value_t*)jl_function_type) {
        ios_printf(s, "Function");
    }
    else {
        jl_value_t *t = (jl_value_t*)jl_typeof(v);
        if (jl_is_bits_type(t)) {
            show_uint(jl_bits_data(v), jl_bitstype_nbits(t));
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            ios_puts(st->name->name->name, s);
            ios_putc('(', s);
            size_t i;
            size_t n = st->names->length;
            for(i=0; i < n; i++) {
                jl_show(nth_field(v, i));
                if (i < n-1)
                    ios_putc(',', s);
            }
            ios_putc(')', s);
        }
    }
    return (jl_value_t*)jl_nothing;
}

// --- RTS primitives ---

JL_CALLABLE(jl_trampoline)
{
    jl_function_t *f = (jl_function_t*)jl_t0(env);
    assert(jl_is_func(f));
    assert(f->linfo != NULL);
    jl_compile(f);
    assert(f->fptr == &jl_trampoline);
    jl_generate_fptr(f);
    assert(f->fptr != NULL);
    f->env = jl_t1(env);
    return jl_apply(f, args, nargs);
}

DLLEXPORT
jl_value_t *jl_new_closure_internal(jl_lambda_info_t *li, jl_value_t *env)
{
    assert(jl_is_lambda_info(li));
    assert(jl_is_tuple(env));
    jl_function_t *f=NULL;
    // note: env is pushed here to make codegen a little easier
    JL_GC_PUSH(&f, &env);
    if (li->fptr != NULL) {
        // function has been compiled
        f = jl_new_closure(li->fptr, env);
    }
    else {
        f = jl_new_closure(jl_trampoline, NULL);
        f->env = (jl_value_t*)jl_tuple2((jl_value_t*)f, env);
    }
    f->linfo = li;
    JL_GC_POP();
    return (jl_value_t*)f;
}

JL_CALLABLE(jl_f_instantiate_type)
{
    JL_NARGSV(instantiate_type, 1);
    if (!jl_is_some_tag_type(args[0]))
        JL_TYPECHK(instantiate_type, typector, args[0]);
    jl_tuple_t *tparams = (jl_tuple_t*)jl_f_tuple(NULL, &args[1], nargs-1);
    JL_GC_PUSH(&tparams);
    jl_value_t *v = jl_apply_type(args[0], tparams);
    JL_GC_POP();
    return v;
}

static int all_typevars(jl_tuple_t *p)
{
    size_t i;
    for(i=0; i < p->length; i++) {
        if (!jl_is_typevar(jl_tupleref(p,i)))
            return 0;
    }
    return 1;
}

static void check_supertype(jl_value_t *super, char *name)
{
    if (!(/*jl_is_struct_type(super) || */jl_is_tag_type(super)) ||
        super == (jl_value_t*)jl_sym_type ||
        super == (jl_value_t*)jl_undef_type ||
        jl_subtype(super,(jl_value_t*)jl_type_type,0) ||
        jl_subtype(super,(jl_value_t*)jl_array_type,0)) {
        jl_errorf("invalid subtyping in definition of %s", name);
    }
}

JL_CALLABLE(jl_f_new_struct_type)
{
    JL_NARGS(new_struct_type, 4, 4);
    JL_TYPECHK(new_struct_type, symbol, args[0]);
    JL_TYPECHK(new_struct_type, tuple, args[1]);
    JL_TYPECHK(new_struct_type, tuple, args[2]);
    if (args[3] != (jl_value_t*)jl_nothing)
        JL_TYPECHK(new_struct_type, function, args[3]);
    jl_sym_t *name = (jl_sym_t*)args[0];
    jl_tuple_t *params = (jl_tuple_t*)args[1];
    jl_tuple_t *fnames = (jl_tuple_t*)args[2];
    if (!all_typevars(params))
        jl_errorf("invalid type parameter list for %s", name->name);

    jl_struct_type_t *nst =
        jl_new_struct_type(name, jl_any_type, params, fnames, NULL);
    nst->ctor_factory = args[3];
    return (jl_value_t*)nst;
}

void jl_add_constructors(jl_struct_type_t *t);

static void check_type_tuple(jl_tuple_t *t, jl_sym_t *name, const char *ctx);

JL_CALLABLE(jl_f_new_struct_fields)
{
    JL_NARGS(new_struct_fields, 3, 3);
    jl_value_t *super = args[1];
    JL_TYPECHK(new_struct_fields, tuple, args[2]);
    jl_value_t *t = args[0];
    jl_tuple_t *ftypes = (jl_tuple_t*)args[2];
    if (!jl_is_struct_type(t))
        jl_error("you can't do that.");
    jl_struct_type_t *st = (jl_struct_type_t*)t;
    if (st->types != NULL)
        jl_error("you can't do that.");
    check_type_tuple(ftypes, st->name->name, "type definition");
    jl_tuple_t *fnames = st->names;

    check_supertype(super, st->name->name->name);
    st->super = (jl_tag_type_t*)super;
    if (jl_is_struct_type(super)) {
        // UNUSED
        assert(0);
        st->names = jl_tuple_append(((jl_struct_type_t*)super)->names,
                                    fnames);
    }
    else {
        assert(jl_is_tag_type(super));
    }

    /*
    if (jl_is_struct_type(super))
        pft = ((jl_struct_type_t*)super)->types;
    else if (jl_is_tag_type(super))
        pft = jl_null;
    else
        assert(0);
    */
    st->types = ftypes;
    jl_add_constructors(st);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_new_type_constructor)
{
    JL_NARGS(new_type_constructor, 2, 2);
    JL_TYPECHK(new_type_constructor, tuple, args[0]);
    if (!jl_is_type(args[1]))
        jl_type_error("typealias", (jl_value_t*)jl_type_type, args[1]);
    jl_tuple_t *p = (jl_tuple_t*)args[0];
    if (!all_typevars(p)) {
        jl_errorf("typealias: invalid type parameter list");
    }
    return (jl_value_t*)jl_new_type_ctor(p, (jl_type_t*)args[1]);
}

JL_CALLABLE(jl_f_new_tag_type)
{
    JL_NARGS(new_tag_type, 2, 2);
    JL_TYPECHK(new_tag_type, symbol, args[0]);
    JL_TYPECHK(new_tag_type, tuple, args[1]);
    jl_tuple_t *p = (jl_tuple_t*)args[1];
    if (!all_typevars(p)) {
        jl_errorf("invalid type parameter list for %s",
                  ((jl_sym_t*)args[0])->name);
    }
    return (jl_value_t*)jl_new_tagtype((jl_value_t*)args[0], jl_any_type, p);
}

JL_CALLABLE(jl_f_new_tag_type_super)
{
    JL_NARGS(new_tag_type_super, 2, 2);
    JL_TYPECHK(new_tag_type_super, tag_type, args[1]);
    jl_value_t *super = args[1];
    check_supertype(super, ((jl_sym_t*)args[0])->name);
    ((jl_tag_type_t*)args[0])->super = (jl_tag_type_t*)super;
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_new_bits_type)
{
    JL_NARGS(new_bits_type, 3, 3);
    JL_TYPECHK(new_bits_type, symbol, args[0]);
    JL_TYPECHK(new_bits_type, tuple, args[1]);
    JL_TYPECHK(new_bits_type, long, args[2]);
    jl_tuple_t *p = (jl_tuple_t*)args[1];
    if (!all_typevars(p)) {
        jl_errorf("invalid type parameter list for %s",
                  ((jl_sym_t*)args[0])->name);
    }
    int32_t nb = jl_unbox_long(args[2]);
    //if (nb != 8 && nb != 16 && nb != 32 && nb != 64)
    if (nb < 1 || nb>=(1<<23) || (nb&7) != 0)
        jl_errorf("invalid number of bits in type %s",
                  ((jl_sym_t*)args[0])->name);
    return (jl_value_t*)jl_new_bitstype((jl_value_t*)args[0], jl_any_type, p,
                                        nb);
}

JL_CALLABLE(jl_f_def_macro)
{
    jl_sym_t *nm = (jl_sym_t*)args[0];
    assert(jl_is_symbol(nm));
    jl_function_t *f = (jl_function_t*)args[1];
    assert(jl_is_function(f));
    jl_set_expander(jl_system_module, nm, f);
    return (jl_value_t*)jl_nothing;
}

JL_CALLABLE(jl_f_typevar)
{
    if (nargs < 1 || nargs > 3) {
        JL_NARGS(typevar, 1, 1);
    }
    JL_TYPECHK(typevar, symbol, args[0]);
    jl_value_t *lb = (jl_value_t*)jl_bottom_type;
    jl_value_t *ub = (jl_value_t*)jl_any_type;
    if (nargs > 1) {
        if (jl_is_typector(args[1])) {
            lb = (jl_value_t*)((jl_typector_t*)args[1])->body;
        }
        else {
            JL_TYPECHK(typevar, type, args[1]);
            lb = args[1];
        }
        if (nargs > 2) {
            if (jl_is_typector(args[2])) {
                ub = (jl_value_t*)((jl_typector_t*)args[2])->body;
            }
            else {
                JL_TYPECHK(typevar, type, args[2]);
                ub = args[2];
            }
        }
        else {
            // typevar(name, UB)
            ub = lb;
            lb = (jl_value_t*)jl_bottom_type;
        }
    }
    return (jl_value_t*)jl_new_typevar((jl_sym_t*)args[0], lb, ub);
}

JL_CALLABLE(jl_f_union)
{
    if (nargs == 0) return (jl_value_t*)jl_bottom_type;
    if (nargs == 1) return args[0];
    size_t i;
    jl_tuple_t *argt = jl_alloc_tuple_uninit(nargs);
    for(i=0; i < nargs; i++) {
        if (jl_is_typector(args[i])) {
            jl_tupleset(argt, i, (jl_value_t*)((jl_typector_t*)args[i])->body);
        }
        else if (!jl_is_type(args[i]) && !jl_is_typevar(args[i])) {
            jl_error("invalid union type");
        }
        else {
            jl_tupleset(argt, i, args[i]);
        }
    }
    JL_GC_PUSH(&argt);
    jl_value_t *u = jl_type_union(argt);
    JL_GC_POP();
    return u;
}

// --- method definition ---

static void check_type_tuple(jl_tuple_t *t, jl_sym_t *name, const char *ctx)
{
    size_t i;
    for(i=0; i < t->length; i++) {
        jl_value_t *elt = jl_tupleref(t,i);
        if (!jl_is_type(elt) && !jl_is_typector(elt) && !jl_is_typevar(elt)) {
            jl_type_error_rt(name->name, ctx, (jl_value_t*)jl_type_type, elt);
        }
    }
}

jl_value_t *jl_method_def(jl_sym_t *name, jl_value_t **bp,
                          jl_tuple_t *argtypes, jl_function_t *f)
{
    jl_value_t *gf;
    if (*bp == NULL) {
        gf = (jl_value_t*)jl_new_generic_function(name);
    }
    else {
        gf = *bp;
        if (!jl_is_gf(gf))
            jl_error("in method definition: not a generic function");
    }
    assert(jl_is_function(f));
    assert(jl_is_tuple(argtypes));
    check_type_tuple(argtypes, name, "method definition");
    jl_add_method((jl_function_t*)gf, argtypes, f);
    return gf;
}

// --- generic function reflection ---

JL_CALLABLE(jl_f_methodexists)
{
    JL_NARGS(method_exists, 2, 2);
    JL_TYPECHK(method_exists, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("method_exists: not a generic function");
    JL_TYPECHK(method_exists, tuple, args[1]);
    check_type_tuple((jl_tuple_t*)args[1], jl_gf_name(args[0]),
                     "method_exists");
    return jl_method_lookup_by_type(jl_gf_mtable(args[0]),
                                    (jl_tuple_t*)args[1], 0) ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_applicable)
{
    JL_NARGSV(applicable, 1);
    JL_TYPECHK(applicable, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("applicable: not a generic function");
    return jl_method_lookup(jl_gf_mtable(args[0]), &args[1], nargs-1, 0) ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_invoke)
{
    JL_NARGSV(invoke, 2);
    JL_TYPECHK(invoke, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("invoke: not a generic function");
    JL_TYPECHK(invoke, tuple, args[1]);
    check_type_tuple((jl_tuple_t*)args[1], jl_gf_name(args[0]), "invoke");
    if (!jl_tuple_subtype(&args[2], nargs-2, &jl_tupleref(args[1],0),
                          ((jl_tuple_t*)args[1])->length, 1, 0))
        jl_error("invoke: argument type error");
    return jl_gf_invoke((jl_function_t*)args[0],
                        (jl_tuple_t*)args[1], &args[2], nargs-2);
}

DLLEXPORT
jl_value_t *jl_closure_env(jl_function_t *f)
{
    if (jl_is_tuple(f->env) && ((jl_tuple_t*)f->env)->length==2 &&
        jl_tupleref(f->env,0) == (jl_value_t*)f)
        return jl_tupleref(f->env,1);
    return f->env;
}

DLLEXPORT
jl_value_t *jl_closure_linfo(jl_function_t *f)
{
    if (jl_is_gf(f))
        return (jl_value_t*)jl_gf_name(f);
    if (f->linfo == NULL)
        return (jl_value_t*)jl_null;
    return (jl_value_t*)f->linfo;
}

// --- eq hash table ---

#include "table.c"

// --- hashing ---

DLLEXPORT uptrint_t jl_hash_symbol(jl_sym_t *s)
{
    return s->hash;
}

DLLEXPORT uptrint_t jl_uid(jl_value_t *v)
{
    return (uptrint_t)v;
}

// --- init ---

static void add_builtin_method1(jl_function_t *gf, jl_type_t *t, jl_fptr_t f)
{
    jl_add_method(gf, jl_tuple1(t), jl_new_closure(f, NULL));
}

static void add_builtin(const char *name, jl_value_t *v)
{
    jl_set_const(jl_system_module, jl_symbol(name), v);
}

static void add_builtin_func(const char *name, jl_fptr_t f)
{
    add_builtin(name, (jl_value_t*)jl_new_closure(f, NULL));
}

void jl_add_builtin_func(const char *name, jl_fptr_t f)
{
    return add_builtin_func(name, f);
}

void jl_add_builtin(const char *name, jl_value_t *v)
{
    return add_builtin(name, v);
}

void jl_init_primitives()
{
    add_builtin_func("is", jl_f_is);
    add_builtin_func("typeof", jl_f_typeof);
    add_builtin_func("subtype", jl_f_subtype);
    add_builtin_func("isa", jl_f_isa);
    add_builtin_func("typeassert", jl_f_typeassert);
    add_builtin_func("apply", jl_f_apply);
    add_builtin_func("throw", jl_f_throw);
    add_builtin_func("tuple", jl_f_tuple);
    add_builtin_func("Union", jl_f_union);
    add_builtin_func("method_exists", jl_f_methodexists);
    add_builtin_func("applicable", jl_f_applicable);
    add_builtin_func("invoke", jl_f_invoke);
    add_builtin_func("eval", jl_f_top_eval);
    add_builtin_func("isbound", jl_f_isbound);
    
    // functions for internal use
    add_builtin_func("tupleref",  jl_f_tupleref);
    add_builtin_func("tuplelen",  jl_f_tuplelen);
    add_builtin_func("getfield",  jl_f_get_field);
    add_builtin_func("_setfield",  jl_f_set_field);
    add_builtin_func("fieldtype", jl_f_field_type);

    add_builtin_func("arraylen", jl_f_arraylen);
    add_builtin_func("arrayref", jl_f_arrayref);
    add_builtin_func("arrayset", jl_f_arrayset);
    add_builtin_func("arraysize", jl_f_arraysize);

    add_builtin_func("apply_type", jl_f_instantiate_type);
    add_builtin_func("typevar", jl_f_typevar);
    add_builtin_func("new_struct_type", jl_f_new_struct_type);
    add_builtin_func("new_struct_fields", jl_f_new_struct_fields);
    add_builtin_func("new_type_constructor", jl_f_new_type_constructor);
    add_builtin_func("new_tag_type", jl_f_new_tag_type);
    add_builtin_func("new_tag_type_super", jl_f_new_tag_type_super);
    add_builtin_func("new_bits_type", jl_f_new_bits_type);
    add_builtin_func("def_macro", jl_f_def_macro);

    // builtin types
    add_builtin("Any", (jl_value_t*)jl_any_type);
    add_builtin("None", (jl_value_t*)jl_bottom_type);
    add_builtin("Void", (jl_value_t*)jl_bottom_type);
    add_builtin("TypeVar", (jl_value_t*)jl_tvar_type);
    add_builtin("TypeName", (jl_value_t*)jl_typename_type);
    add_builtin("TypeConstructor", (jl_value_t*)jl_typector_type);
    add_builtin("Tuple", (jl_value_t*)jl_tuple_type);
    add_builtin("NTuple", (jl_value_t*)jl_ntuple_type);
    add_builtin("Type", (jl_value_t*)jl_type_type);
    add_builtin("Symbol", (jl_value_t*)jl_sym_type);
    add_builtin("...", (jl_value_t*)jl_seq_type);
    add_builtin("Function", (jl_value_t*)jl_function_type);
    add_builtin("AbstractArray", (jl_value_t*)jl_abstractarray_type);
    add_builtin("Array", (jl_value_t*)jl_array_type);

    add_builtin("Expr", (jl_value_t*)jl_expr_type);
    add_builtin("SymbolNode", (jl_value_t*)jl_symbolnode_type);
    add_builtin("LineNumberNode", (jl_value_t*)jl_linenumbernode_type);
    add_builtin("LabelNode", (jl_value_t*)jl_labelnode_type);
    add_builtin("Ptr", (jl_value_t*)jl_pointer_type);
    add_builtin("LambdaStaticData", (jl_value_t*)jl_lambda_info_type);
    add_builtin("Box", (jl_value_t*)jl_box_type);
    add_builtin("IntrinsicFunction", (jl_value_t*)jl_intrinsic_type);
    // todo: this should only be visible to compiler components
    add_builtin("Undef", (jl_value_t*)jl_undef_type);

    add_builtin("BitsKind", (jl_value_t*)jl_bits_kind);
    add_builtin("CompositeKind", (jl_value_t*)jl_struct_kind);
    add_builtin("FuncKind", (jl_value_t*)jl_func_kind);
    add_builtin("AbstractKind", (jl_value_t*)jl_tag_kind);
    add_builtin("UnionKind", (jl_value_t*)jl_union_kind);

#ifdef __LP64__
    add_builtin("Size", (jl_value_t*)jl_int64_type);
#else
    add_builtin("Size", (jl_value_t*)jl_int32_type);
#endif

    add_builtin("ANY", jl_ANY_flag);

    add_builtin("C_NULL", jl_box_pointer(jl_pointer_void_type, NULL));
}

void jl_init_builtins()
{
    jl_function_t *jl_print_gf = jl_new_generic_function(jl_symbol("print"));

    add_builtin_method1(jl_print_gf,
                        (jl_type_t*)jl_array_uint8_type,
                        jl_f_print_array_uint8);
    add_builtin_method1(jl_print_gf, (jl_type_t*)jl_sym_type,
                        jl_f_print_symbol);

    jl_show_gf = jl_new_generic_function(jl_symbol("show"));

    //add_builtin_method1(jl_show_gf, (jl_type_t*)jl_sym_type,         jl_f_print_symbol);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_any_type,         jl_f_show_any);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_tvar_type,        jl_f_show_typevar);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_lambda_info_type, jl_f_show_linfo);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_float32_type,     jl_f_show_float32);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_float64_type,     jl_f_show_float64);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_int8_type,        jl_f_show_int8);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_uint8_type,       jl_f_show_uint8);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_int16_type,       jl_f_show_int16);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_uint16_type,      jl_f_show_uint16);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_int32_type,       jl_f_show_int32);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_uint32_type,      jl_f_show_uint32);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_int64_type,       jl_f_show_int64);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_uint64_type,      jl_f_show_uint64);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_bool_type,        jl_f_show_bool);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_char_type,        jl_f_show_char);
    add_builtin_method1(jl_show_gf, (jl_type_t*)jl_pointer_type,     jl_f_show_pointer);

    jl_convert_gf = jl_new_generic_function(jl_symbol("convert"));
    jl_add_method(jl_convert_gf,
                  jl_tuple2(jl_any_type, jl_any_type),
                  jl_new_closure(jl_f_convert, NULL));
    jl_add_method(jl_convert_gf,
                  jl_tuple2(jl_tuple_type, jl_tuple_type),
                  jl_new_closure(jl_f_convert_tuple, NULL));

    add_builtin("print",    (jl_value_t*)jl_print_gf);
    add_builtin("show",     (jl_value_t*)jl_show_gf);
    add_builtin("convert",  (jl_value_t*)jl_convert_gf);
}
