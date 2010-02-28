/*
  implementations of some built-in functions and utilities
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#include <gc.h>
#include "llt.h"
#include "julia.h"

void jl_error(char *str)
{
    ios_printf(ios_stderr, "%s\n", str);
    exit(1);
}

void jl_errorf(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ios_vprintf(ios_stderr, fmt, args);
    ios_printf(ios_stderr, "\n");
    va_end(args);
    exit(1);
}

void jl_too_few_args(char *fname, int min)
{
    jl_errorf("%s: too few arguments (expected %d)", fname, min);
}

void jl_too_many_args(char *fname, int max)
{
    jl_errorf("%s: too many arguments (expected %d)", fname, max);
}

/*
  equivalent julia code:
  expr(head, args...) = Expr.new(head, buffer(args...))
*/
jl_expr_t *jl_expr(jl_sym_t *head, size_t n, ...)
{
    va_list args;
    size_t i;
    jl_expr_t *ex = jl_exprn(head,n);
    va_start(args, n);
    for(i=0; i < n; i++) {
        ((jl_value_t**)ex->args->data)[i] = va_arg(args, jl_value_t*);
    }
    va_end(args);
    return ex;
}

jl_expr_t *jl_exprn(jl_sym_t *head, size_t n)
{
    jl_value_t *ctor_args[2];
    ctor_args[0] = (jl_value_t*)head;
    ctor_args[1] = (jl_value_t*)jl_new_buffer(jl_buffer_any_type, n);
    return (jl_expr_t*)jl_apply(jl_expr_type->fnew, ctor_args, 2);
}

JL_CALLABLE(jl_is)
{
    JL_NARGS(is, 2, 2);
    if (args[0] == args[1])
        return jl_true;
    return jl_false;
}

JL_CALLABLE(jl_isnull)
{
    JL_NARGS(isnull, 1, 1);
    if (args[0] == (jl_value_t*)jl_null)
        return jl_true;
    return jl_false;
}

JL_CALLABLE(jl_f_typeof)
{
    JL_NARGS(typeof, 1, 1);
    return jl_full_type(args[0]);
}

// --- printing ---

jl_function_t *jl_print_gf;

ios_t *current_output_stream;  // TODO: thread-local

static void print_tuple(jl_tuple_t *t, char opn, char cls)
{
    ios_t *s = current_output_stream;
    ios_putc(opn, s);
    size_t i, n=t->length;
    for(i=0; i < n; i++) {
        jl_value_t *elt = jl_tupleref(t, i);
        jl_apply(jl_print_gf, &elt, 1);
        if (i < n-1)
            ios_write(s, ", ", 2);
        else if (n == 1)
            ios_putc(',', s);
    }
    ios_putc(cls, s);
}

static void print_type(jl_value_t *t)
{
    ios_t *s = current_output_stream;
    if (t == (jl_value_t*)jl_scalar_type) {
        ios_puts("Tensor[Scalar, 0]", s);
    }
    else if (jl_is_func_type(t)) {
        jl_apply(jl_print_gf, &((jl_func_type_t*)t)->from, 1);
        ios_write(s, "-->", 3);
        jl_apply(jl_print_gf, &((jl_func_type_t*)t)->to, 1);
    }
    else if (jl_is_union_type(t)) {
        ios_write(s, "Union", 5);
        print_tuple(((jl_uniontype_t*)t)->types, '(', ')');
    }
    else {
        ios_puts(((jl_tag_type_t*)t)->name->name->name, s);
        jl_tuple_t *p = jl_tparams(t);
        if (p->length > 0)
            print_tuple(p, '[', ']');
    }
}

JL_CALLABLE(jl_f_print_function)
{
    JL_NARGS(print, 1, 1);
    ios_t *s = current_output_stream;
    jl_value_t *v = args[0];
    if (jl_is_gf(v)) {
        ios_puts("#<generic-function ", s);
        ios_puts(((jl_sym_t*)((jl_value_pair_t*)((jl_function_t*)v)->env)->b)->name, s);
        ios_putc('>', s);
    }
    else {
        ios_puts("#<closure>", s);
    }
    return (jl_value_t*)jl_null;
}

static void print_int(void *data, int nbits)
{
    ios_t *s = current_output_stream;
}

// print float
// print bool
// print buffer

JL_CALLABLE(jl_f_print_any)
{
    JL_NARGS(print, 1, 1);
    // fallback for printing some other builtin types
    ios_t *s = current_output_stream;
    jl_value_t *v = args[0];
    if (jl_is_tuple(v)) {
        print_tuple((jl_tuple_t*)v, '(', ')');
    }
    else if (jl_is_type(v)) {
        print_type(v);
    }
    else {
        jl_value_t *t = (jl_value_t*)jl_typeof(v);
        if (jl_is_bits_type(t)) {
            print_int(jl_bits_data(v), ((jl_bits_type_t*)t)->nbits);
        }
        else {
            assert(jl_is_struct_type(t));
            jl_struct_type_t *st = (jl_struct_type_t*)t;
            ios_puts(st->name->name->name, s);
            ios_putc('(', s);
            size_t i;
            size_t n = st->names->length;
            for(i=0; i < n; i++) {
                ios_puts(((jl_sym_t*)jl_tupleref(st->names,i))->name, s);
                ios_putc('=', s);
                jl_apply(jl_print_gf, &((jl_value_t**)v)[i+1], 1);
                if (i < n-1)
                    ios_write(s, ", ", 2);
            }
            ios_putc(')', s);
        }
    }
    return (jl_value_t*)jl_null;
}

void jl_init_builtins()
{
    jl_print_gf = jl_new_generic_function(jl_symbol("print"));
}
