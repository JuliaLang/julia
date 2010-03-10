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
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
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

void jl_type_error(char *fname, char *expected, jl_value_t *got)
{
    jl_errorf("type error: %s: expected %s, got %s",
              fname, expected, jl_tname(jl_typeof(got))->name->name);
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

JL_CALLABLE(jl_f_tuple)
{
    size_t i;
    if (nargs == 0) return (jl_value_t*)jl_null;
    jl_tuple_t *t = jl_alloc_tuple(nargs);
    for(i=0; i < nargs; i++) {
        jl_tupleset(t, i, args[i]);
    }
    return (jl_value_t*)t;
}

static size_t field_offset(jl_struct_type_t *t, jl_sym_t *fld)
{
    jl_tuple_t *fn = t->names;
    size_t i;
    for(i=0; i < fn->length; i++) {
        if (jl_tupleref(fn,i) == fld)
            return i;
    }
    jl_errorf("type %s has no field %s", t->name->name->name, fld->name);
    return 0;
}

JL_CALLABLE(jl_f_get_field)
{
    JL_NARGS(getfield, 2, 2);
    JL_TYPECHK(getfield, symbol, args[1]);
    jl_value_t *v = args[0];
    if (!jl_is_struct_type(jl_typeof(v)))
        jl_error("getfield: argument must be a struct");
    size_t i = field_offset((jl_struct_type_t*)jl_typeof(v),
                            (jl_sym_t*)args[1]);
    return ((jl_value_t**)v)[1+i];
}

JL_CALLABLE(jl_f_tupleref)
{
    JL_NARGS(tupleref, 2, 2);
    JL_TYPECHK(tupleref, tuple, args[0]);
    JL_TYPECHK(tupleref, int32, args[1]);
    jl_tuple_t *t = (jl_tuple_t*)args[0];
    size_t i = jl_unbox_int32(args[1])-1;
    if (i >= t->length)
        jl_error("tupleref: index out of range");
    return jl_tupleref(t, i);
}

JL_CALLABLE(jl_f_bufferlen)
{
    JL_NARGS(bufferlen, 1, 1);
    JL_TYPECHK(bufferlen, buffer, args[0]);
    return jl_box_int32(((jl_buffer_t*)args[0])->length);
}

// --- printing ---

jl_function_t *jl_print_gf;

ios_t *current_output_stream;  // TODO: thread-local

static void call_print(jl_value_t *v)
{
    jl_apply(jl_print_gf, &v, 1);
}

void jl_print(jl_value_t *v)
{
    call_print(v);
}

static void print_tuple(jl_tuple_t *t, char opn, char cls)
{
    ios_t *s = current_output_stream;
    ios_putc(opn, s);
    size_t i, n=t->length;
    for(i=0; i < n; i++) {
        call_print(jl_tupleref(t, i));
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
        call_print(((jl_func_type_t*)t)->from);
        ios_write(s, "-->", 3);
        call_print(((jl_func_type_t*)t)->to);
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

static void print_function(jl_value_t *v)
{
    ios_t *s = current_output_stream;
    if (jl_is_gf(v)) {
        ios_puts("#<generic-function ", s);
        ios_puts(((jl_sym_t*)((jl_value_pair_t*)((jl_function_t*)v)->env)->b)->name, s);
        ios_putc('>', s);
    }
    else {
        ios_puts("#<closure>", s);
    }
}

static void print_int(void *data, int nbits)
{
    ios_t *s = current_output_stream;
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

static void print_uint(void *data, int nbits)
{
    ios_t *s = current_output_stream;
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

static void print_float64(double d, int single)
{
    ios_t *s = current_output_stream;
    char buf[64];
    int ndec = single ? 8 : 16;
    if (!DFINITE(d)) {
        char *rep;
        if (isnan(d))
            rep = sign_bit(d) ? "-NaN" : "+NaN";
        else
            rep = sign_bit(d) ? "-Inf" : "+Inf";
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

JL_CALLABLE(jl_f_print_bool)
{
    ios_t *s = current_output_stream;
    if (*(int32_t*)jl_bits_data(args[0]) == 0)
        ios_puts("false", s);
    else
        ios_puts("true", s);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_float32)
{
    print_float64((double)*(float*)jl_bits_data(args[0]), 1);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_float64)
{
    print_float64(*(double*)jl_bits_data(args[0]), 0);
    return (jl_value_t*)jl_null;
}

#define INT_PRINT_FUNC(sgn,nb)                  \
JL_CALLABLE(jl_f_print_##sgn##nb)               \
{                                               \
    print_##sgn(jl_bits_data(args[0]), nb);     \
    return (jl_value_t*)jl_null;                \
}

INT_PRINT_FUNC(int,8)
INT_PRINT_FUNC(uint,8)
INT_PRINT_FUNC(int,16)
INT_PRINT_FUNC(uint,16)
INT_PRINT_FUNC(int,32)
INT_PRINT_FUNC(uint,32)
INT_PRINT_FUNC(int,64)
INT_PRINT_FUNC(uint,64)

JL_CALLABLE(jl_f_print_symbol)
{
    ios_t *s = current_output_stream;
    ios_puts(((jl_sym_t*)args[0])->name, s);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_typename)
{
    jl_print(((jl_typename_t*)args[0])->name);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_typevar)
{
    jl_print(((jl_tvar_t*)args[0])->name);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_buffer)
{
    ios_t *s = current_output_stream;
    jl_buffer_t *b = (jl_buffer_t*)args[0];

    ios_puts("buffer(", s);
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(jl_typeof(b));
    if (el_type == jl_any_type) {
        size_t i, n=b->length;
        for(i=0; i < n; i++) {
            jl_print(((jl_value_t**)b->data)[i]);
            if (i < n-1)
                ios_write(s, ", ", 2);
        }
    }
    else if (jl_is_bits_type(el_type)) {
        jl_bits_type_t *bt = (jl_bits_type_t*)el_type;
        size_t nb = bt->nbits/8;
        size_t i, n=b->length;
        jl_value_t *elt =
            (jl_value_t*)allocb((NWORDS(LLT_ALIGN(nb,sizeof(void*)))+1)*
                                sizeof(void*));
        elt->type = el_type;
        for(i=0; i < n; i++) {
            memcpy(jl_bits_data(elt), &((char*)b->data)[i*nb], nb);
            jl_print(elt);
            if (i < n-1)
                ios_write(s, ", ", 2);
        }
    }
    else {
        // TODO
    }
    ios_putc(')', s);

    return (jl_value_t*)jl_null;
}

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
    else if (jl_is_func(v)) {
        print_function(v);
    }
    else {
        jl_value_t *t = (jl_value_t*)jl_typeof(v);
        if (jl_is_bits_type(t)) {
            print_uint(jl_bits_data(v), ((jl_bits_type_t*)t)->nbits);
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
                call_print(((jl_value_t**)v)[i+1]);
                if (i < n-1)
                    ios_write(s, ", ", 2);
            }
            ios_putc(')', s);
        }
    }
    return (jl_value_t*)jl_null;
}

static void add_builtin_method1(jl_function_t *gf, jl_type_t *t, jl_fptr_t f)
{
    jl_add_method(gf, jl_tuple(1, t), jl_new_closure(f, NULL));
}

void jl_init_builtins()
{
    jl_print_gf = jl_new_generic_function(jl_symbol("print"));

    add_builtin_method1(jl_print_gf, jl_any_type,     jl_f_print_any);
    add_builtin_method1(jl_print_gf, jl_sym_type,     jl_f_print_symbol);
    add_builtin_method1(jl_print_gf, jl_typename_type,jl_f_print_typename);
    add_builtin_method1(jl_print_gf, jl_tvar_type,    jl_f_print_typevar);
    add_builtin_method1(jl_print_gf, jl_buffer_type,  jl_f_print_buffer);
    add_builtin_method1(jl_print_gf, jl_float32_type, jl_f_print_float32);
    add_builtin_method1(jl_print_gf, jl_float64_type, jl_f_print_float64);
    add_builtin_method1(jl_print_gf, jl_int8_type,    jl_f_print_int8);
    add_builtin_method1(jl_print_gf, jl_uint8_type,   jl_f_print_uint8);
    add_builtin_method1(jl_print_gf, jl_int16_type,   jl_f_print_int16);
    add_builtin_method1(jl_print_gf, jl_uint16_type,  jl_f_print_uint16);
    add_builtin_method1(jl_print_gf, jl_int32_type,   jl_f_print_int32);
    add_builtin_method1(jl_print_gf, jl_uint32_type,  jl_f_print_uint32);
    add_builtin_method1(jl_print_gf, jl_int64_type,   jl_f_print_int64);
    add_builtin_method1(jl_print_gf, jl_uint64_type,  jl_f_print_uint64);
    add_builtin_method1(jl_print_gf, jl_bool_type,    jl_f_print_bool);

    current_output_stream = ios_stdout;
}
