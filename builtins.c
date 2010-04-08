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
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

// --- exception raising ---

extern jmp_buf ExceptionHandler;
extern jmp_buf *CurrentExceptionHandler;

void jl_error(char *str)
{
    ios_printf(ios_stderr, "%s\n", str);
    longjmp(*CurrentExceptionHandler, 1);
}

void jl_errorf(char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ios_vprintf(ios_stderr, fmt, args);
    ios_printf(ios_stderr, "\n");
    va_end(args);
    longjmp(*CurrentExceptionHandler, 1);
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

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(is, 2, 2);
    if (args[0] == args[1])
        return jl_true;
    return jl_false;
}

JL_CALLABLE(jl_f_typeof)
{
    JL_NARGS(typeof, 1, 1);
    return jl_full_type(args[0]);
}

JL_CALLABLE(jl_f_subtype)
{
    JL_NARGS(subtype, 2, 2);
    JL_TYPECHK(subtype, type, args[1]);
    return (jl_subtype(args[0],args[1],0,0) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_istype)
{
    JL_NARGS(istype, 2, 2);
    JL_TYPECHK(istype, type, args[1]);
    if (jl_is_tuple(args[0]))
        return (jl_subtype(args[0],args[1],1,0) ? jl_true : jl_false);
    return (jl_subtype(jl_typeof(args[0]),args[1],0,0) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_typeassert)
{
    JL_NARGS(typeassert, 2, 2);
    JL_TYPECHK(typeassert, type, args[1]);
    int ok;
    if (jl_is_tuple(args[0]))
        ok = (jl_subtype(args[0],args[1],1,0) ? jl_true : jl_false);
    else
        ok = (jl_subtype(jl_typeof(args[0]),args[1],0,0) ? jl_true : jl_false);
    if (!ok)
        jl_error("type assertion failed");
    return args[0];
}

JL_CALLABLE(jl_f_apply)
{
    JL_NARGSV(apply, 1);
    JL_TYPECHK(apply, function, args[0]);
    if (nargs == 1) {
        JL_TYPECHK(apply, tuple, args[1]);
        return jl_apply((jl_function_t*)args[0], &jl_tupleref(args[1],0),
                        ((jl_tuple_t*)args[1])->length);
    }
    size_t n=0, i, j;
    for(i=1; i < nargs; i++) {
        JL_TYPECHK(apply, tuple, args[i]);
        n += ((jl_tuple_t*)args[i])->length;
    }
    jl_value_t **newargs = alloca(n * sizeof(jl_value_t*));
    n = 0;
    for(i=1; i < nargs; i++) {
        jl_tuple_t *t = (jl_tuple_t*)args[i];
        for(j=0; j < t->length; j++)
            newargs[n++] = jl_tupleref(t, j);
    }
    return jl_apply((jl_function_t*)args[0], newargs, n);
}

JL_CALLABLE(jl_f_error)
{
    JL_NARGS(error, 1, 1);
    if (jl_typeof(args[0]) == jl_buffer_uint8_type) {
        jl_error((char*)((jl_buffer_t*)args[0])->data);
    }
    else {
        jl_error("error: expected string");
    }
    return jl_null;
}

JL_CALLABLE(jl_f_time_thunk)
{
    JL_NARGS(time_thunk, 1, 1);
    JL_TYPECHK(time_thunk, function, args[0]);
    double t0 = clock_now();
    jl_value_t *result = jl_apply(args[0], NULL, 0);
    double t1 = clock_now();
    ios_printf(ios_stdout, "elapsed time: %.4f sec\n", t1-t0);
    return result;
}

JL_CALLABLE(jl_f_load)
{
    JL_NARGS(load, 1, 1);
    if (jl_typeof(args[0]) == jl_buffer_uint8_type) {
        char *fname = (char*)((jl_buffer_t*)args[0])->data;
        jl_value_t *ast = jl_parse_file(fname);
        jl_buffer_t *b = ((jl_expr_t*)ast)->args;
        if (b == NULL)
            jl_errorf("could not open file %s", fname);
        size_t i, lineno=0;
        jmp_buf *prevh = CurrentExceptionHandler;
        jmp_buf handler;
        CurrentExceptionHandler = &handler;
        if (!setjmp(handler)) {
            for(i=0; i < b->length; i++) {
                // process toplevel form
                jl_value_t *form = ((jl_value_t**)b->data)[i];
                if (jl_is_expr(form) && ((jl_expr_t*)form)->head == line_sym) {
                    lineno = jl_unbox_int32(jl_exprarg(form, 0));
                }
                else {
                    jl_toplevel_eval(form);
                }
            }
        }
        else {
            CurrentExceptionHandler = prevh;
            ios_printf(ios_stderr, "on %s:%d\n", fname, lineno);
            longjmp(*CurrentExceptionHandler, 1);
        }
        CurrentExceptionHandler = prevh;
    }
    else {
        jl_error("load: expected string");
    }
    return jl_null;
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

JL_CALLABLE(jl_f_tuplelen)
{
    JL_NARGS(tuplelen, 1, 1);
    JL_TYPECHK(tuplelen, tuple, args[0]);
    return jl_box_int32(((jl_tuple_t*)args[0])->length);
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

JL_CALLABLE(jl_f_set_field)
{
    JL_NARGS(setfield, 3, 3);
    JL_TYPECHK(setfield, symbol, args[1]);
    jl_value_t *v = args[0];
    if (!jl_is_struct_type(jl_typeof(v)))
        jl_error("setfield: argument must be a struct");
    jl_struct_type_t *st = (jl_struct_type_t*)jl_typeof(v);
    size_t i = field_offset(st, (jl_sym_t*)args[1]);
    ((jl_value_t**)v)[1+i] = jl_convert(args[2],
                                        jl_tupleref(st->types,i));
    return v;
}

JL_CALLABLE(jl_f_bufferlen)
{
    JL_NARGS(bufferlen, 1, 1);
    JL_TYPECHK(bufferlen, buffer, args[0]);
    return jl_box_int32(((jl_buffer_t*)args[0])->length);
}

static jl_value_t *new_scalar(jl_bits_type_t *bt)
{
    size_t nb = bt->nbits/8;
    jl_value_t *v = (jl_value_t*)allocb((NWORDS(LLT_ALIGN(nb,sizeof(void*)))+1)*
                                        sizeof(void*));
    v->type = (jl_type_t*)bt;
    return v;
}

JL_CALLABLE(jl_f_bufferref)
{
    JL_NARGS(bufferref, 2, 2);
    JL_TYPECHK(bufferref, buffer, args[0]);
    JL_TYPECHK(bufferref, int32, args[1]);
    jl_buffer_t *b = (jl_buffer_t*)args[0];
    size_t i = jl_unbox_int32(args[1])-1;
    if (i >= b->length)
        jl_errorf("buffer[%d]: index out of range", i+1);
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(jl_typeof(b));
    jl_value_t *elt;
    if (jl_is_bits_type(el_type)) {
        if (el_type == jl_bool_type) {
            if (((int8_t*)b->data)[i] != 0)
                return jl_true;
            return jl_false;
        }
        elt = new_scalar((jl_bits_type_t*)el_type);
        size_t nb = ((jl_bits_type_t*)el_type)->nbits/8;
        switch (nb) {
        case 1:
            *(int8_t*)jl_bits_data(elt)  = ((int8_t*)b->data)[i];  break;
        case 2:
            *(int16_t*)jl_bits_data(elt) = ((int16_t*)b->data)[i]; break;
        case 4:
            *(int32_t*)jl_bits_data(elt) = ((int32_t*)b->data)[i]; break;
        case 8:
            *(int64_t*)jl_bits_data(elt) = ((int64_t*)b->data)[i]; break;
        default:
            memcpy(jl_bits_data(elt), &((char*)b->data)[i*nb], nb);
        }
    }
    else {
        elt = ((jl_value_t**)b->data)[i];
        if (elt == NULL)
            jl_errorf("buffer[%d]: uninitialized reference error", i+1);
    }
    return elt;
}

JL_CALLABLE(jl_f_bufferset)
{
    JL_NARGS(bufferset, 3, 3);
    JL_TYPECHK(bufferset, buffer, args[0]);
    JL_TYPECHK(bufferset, int32, args[1]);
    jl_buffer_t *b = (jl_buffer_t*)args[0];
    size_t i = jl_unbox_int32(args[1])-1;
    if (i >= b->length)
        jl_errorf("buffer[%d]: index out of range", i+1);
    jl_type_t *el_type = (jl_type_t*)jl_tparam0(jl_typeof(b));
    jl_value_t *rhs = jl_convert(args[2], el_type);
    if (jl_is_bits_type(el_type)) {
        size_t nb = ((jl_bits_type_t*)el_type)->nbits/8;
        switch (nb) {
        case 1:
            ((int8_t*)b->data)[i]  = *(int8_t*)jl_bits_data(rhs);  break;
        case 2:
            ((int16_t*)b->data)[i] = *(int16_t*)jl_bits_data(rhs); break;
        case 4:
            ((int32_t*)b->data)[i] = *(int32_t*)jl_bits_data(rhs); break;
        case 8:
            ((int64_t*)b->data)[i] = *(int64_t*)jl_bits_data(rhs); break;
        default:
            memcpy(&((char*)b->data)[i*nb], jl_bits_data(rhs), nb);
        }
    }
    else {
        ((jl_value_t**)b->data)[i] = rhs;
    }
    return args[0];
}

JL_CALLABLE(jl_f_box)
{
    JL_NARGS(box, 1, 2);
    if (nargs == 1)
        return jl_new_struct(jl_box_any_type, NULL);
    return jl_new_struct(jl_box_any_type, args[1]);
}

JL_CALLABLE(jl_f_unbox)
{
    JL_NARGS(unbox, 1, 1);
    JL_TYPECHK(unbox, box, args[0]);
    return ((jl_value_t**)args[0])[1];
}

JL_CALLABLE(jl_f_boxset)
{
    JL_NARGS(boxset, 2, 2);
    JL_TYPECHK(boxset, box, args[0]);
    ((jl_value_t**)args[0])[1] = args[1];
    return jl_null;
}

jl_function_t *jl_ref_gf;

JL_CALLABLE(jl_f_ref_typector)
{
    JL_NARGSV(ref_typector, 1);
    JL_TYPECHK(ref_typector, typector, args[0]);
    jl_tuple_t *tparams = jl_f_tuple(NULL, &args[1], nargs-1);
    return jl_apply_type_ctor((jl_typector_t*)args[0], tparams);
}

// --- conversions ---

static jl_tuple_t *convert_tuple(jl_tuple_t *x, jl_tuple_t *to)
{
    size_t i, cl=x->length, pl=to->length;
    jl_tuple_t *out = jl_alloc_tuple(cl);
    jl_value_t *ce, *pe;
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
            return NULL;
        }
        jl_tupleset(out, i, jl_convert(ce, pe));
    }
    return out;
}

jl_value_t *jl_convert(jl_value_t *x, jl_type_t *to)
{
    jl_value_t *out;
    if (jl_is_tuple(x) && jl_is_tuple(to)) {
        out = convert_tuple((jl_tuple_t*)x, (jl_tuple_t*)to);
        if (out == NULL)
            jl_error("convert: invalid tuple conversion");
        return out;
    }
    jl_type_t *t = (jl_type_t*)jl_typeof(x);
    if (jl_subtype(t, to, 0, 0))
        return x;
    jl_function_t *meth;
    if (jl_is_bits_type(to)) {
        meth = ((jl_bits_type_t*)to)->fconvert;
    }
    else if (jl_is_struct_type(to)) {
        meth = ((jl_struct_type_t*)to)->fconvert;
    }
    else {
        jl_error("convert: invalid conversion");
    }
    if (meth == NULL) {
        // TODO: temporary
        jl_error("convert: no conversion defined");
    }
    out = jl_apply(meth, &x, 1);
    if (!jl_subtype(jl_typeof(out), to, 0, 0))
        jl_errorf("convert: conversion to %s failed",
                  jl_tname(to)->name->name);
    return out;
}

JL_CALLABLE(jl_f_convert)
{
    JL_NARGS(convert, 2, 2);
    return jl_convert(args[0], args[1]);
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
#ifdef DEBUG
        ios_putc('\n', s);
        jl_print_method_table((jl_function_t*)v);
#endif
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
    if (jl_unbox_bool(args[0]) == 0)
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

JL_CALLABLE(jl_f_print_linfo)
{
    ios_t *s = current_output_stream;
    ios_puts("AST(", s);
    jl_print(((jl_lambda_info_t*)args[0])->ast);
    ios_putc(')', s);
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_print_buffer)
{
    ios_t *s = current_output_stream;
    jl_buffer_t *b = (jl_buffer_t*)args[0];

    jl_type_t *el_type = (jl_type_t*)jl_tparam0(jl_typeof(b));
    if (el_type == jl_uint8_type) {
        // simple string
        ios_write(s, (char*)b->data, b->length);
        return (jl_value_t*)jl_null;
    }
    ios_puts("buffer(", s);
    if (jl_is_bits_type(el_type)) {
        jl_bits_type_t *bt = (jl_bits_type_t*)el_type;
        size_t nb = bt->nbits/8;
        size_t i, n=b->length;
        jl_value_t *elt = new_scalar(bt);
        for(i=0; i < n; i++) {
            memcpy(jl_bits_data(elt), &((char*)b->data)[i*nb], nb);
            jl_print(elt);
            if (i < n-1)
                ios_write(s, ", ", 2);
        }
    }
    else {
        size_t i, n=b->length;
        for(i=0; i < n; i++) {
            jl_print(((jl_value_t**)b->data)[i]);
            if (i < n-1)
                ios_write(s, ", ", 2);
        }
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
    else if (jl_typeis(v,jl_intrinsic_type)) {
        ios_printf(s, "#<intrinsic-function %d>", *(uint32_t*)jl_bits_data(v));
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

// --- RTS primitives ---

JL_CALLABLE(jl_trampoline)
{
    jl_function_t *f = (jl_function_t*)((jl_value_pair_t*)env)->a;
    assert(jl_is_func(f));
    assert(f->linfo != NULL);
    jl_value_t *cloenv = ((jl_value_pair_t*)env)->b;
    jl_compile(f->linfo);
    assert(f->linfo->fptr != NULL);
    f->fptr = f->linfo->fptr;
    f->env = cloenv;
    return jl_apply(f, args, nargs);
}

JL_CALLABLE(jl_f_new_closure)
{
    JL_NARGS(new_closure, 2, 2);
    JL_TYPECHK(new_closure, tuple, args[1]);
    assert(jl_is_lambda_info(args[0]));
    jl_lambda_info_t *li = (jl_lambda_info_t*)args[0];
    jl_function_t *f = jl_new_closure(NULL, NULL);
    f->linfo = li;
    if (li->fptr != NULL) {
        // function has been compiled
        f->fptr = li->fptr;
        f->env = args[1];
    }
    else {
        f->fptr = jl_trampoline;
        f->env = (jl_value_t*)jl_pair((jl_value_t*)f, args[1]);
    }
    return f;
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

JL_CALLABLE(jl_f_new_struct_type)
{
    JL_NARGS(new_struct_type, 4, 4);
    JL_TYPECHK(new_struct_type, symbol, args[0]);
    assert(jl_is_type(args[1]));
    JL_TYPECHK(new_struct_type, tuple, args[2]);
    JL_TYPECHK(new_struct_type, tuple, args[3]);
    jl_sym_t *name = (jl_sym_t*)args[0];
    jl_value_t *super = args[1];
    jl_tuple_t *params = (jl_tuple_t*)args[2];
    jl_tuple_t *fnames = (jl_tuple_t*)args[3];
    if (jl_subtype(super,jl_tuple_type,0,0) ||
        jl_is_func_type(super) ||
        jl_is_union_type(super) ||
        jl_subtype(super,jl_buffer_type->body,0,0)) {
        jl_errorf("invalid subtyping in definition of %s", name->name);
    }
    if (!all_typevars(params))
        jl_errorf("invalid type parameter list for %s", name->name);
    jl_tuple_t *pfn;
    if (jl_is_struct_type(super))
        pfn = ((jl_struct_type_t*)super)->names;
    else if (jl_is_tag_type(super))
        pfn = jl_null;
    else
        jl_errorf("invalid subtyping in definition of %s", name->name);
    return jl_new_struct_type(name, super, params,
                              jl_tuple_append(pfn, fnames), NULL);
}

JL_CALLABLE(jl_f_new_struct_fields)
{
    JL_NARGS(new_struct_fields, 2, 2);
    JL_TYPECHK(new_struct_fields, tuple, args[1]);
    jl_value_t *t = args[0];
    jl_tuple_t *ftypes = (jl_tuple_t*)args[1];
    if (jl_is_typector(t))
        t = ((jl_typector_t*)t)->body;
    if (!jl_is_struct_type(t))
        jl_error("you can't do that.");
    jl_struct_type_t *st = (jl_struct_type_t*)t;
    if (st->types != NULL)
        jl_error("you can't do that.");
    jl_tuple_t *pft;
    jl_tag_type_t *super = st->super;
    if (jl_is_struct_type(super))
        pft = ((jl_struct_type_t*)super)->types;
    else if (jl_is_tag_type(super))
        pft = jl_null;
    else
        assert(0);
    st->types = jl_tuple_append(pft, ftypes);
    return jl_null;
}

JL_CALLABLE(jl_f_new_type_constructor)
{
    JL_NARGS(new_type_constructor, 2, 2);
    JL_TYPECHK(new_type_constructor, tuple, args[0]);
    assert(jl_is_type(args[1]));
    jl_tuple_t *p = (jl_tuple_t*)args[0];
    if (!all_typevars(p)) {
        jl_errorf("invalid type parameter list for %s",
                  jl_tname(args[1])->name->name);
    }
    return jl_new_type_ctor(p, (jl_type_t*)args[1]);
}

JL_CALLABLE(jl_f_new_tag_type)
{
    JL_NARGS(new_tag_type, 3, 3);
    JL_TYPECHK(new_tag_type, symbol, args[0]);
    JL_TYPECHK(new_tag_type, tag_type, args[1]);
    JL_TYPECHK(new_tag_type, tuple, args[2]);
    jl_tuple_t *p = (jl_tuple_t*)args[2];
    if (!all_typevars(p)) {
        jl_errorf("invalid type parameter list for %s",
                  ((jl_sym_t*)args[0])->name);
    }
    return jl_new_tagtype((jl_sym_t*)args[0], (jl_tag_type_t*)args[1], p);
}

JL_CALLABLE(jl_f_typevar)
{
    JL_NARGS(typevar, 1, 1);
    JL_TYPECHK(typevar, symbol, args[0]);
    return jl_typevar((jl_sym_t*)args[0]);
}

JL_CALLABLE(jl_f_union)
{
    if (nargs == 1) return args[0];
    size_t i;
    for(i=0; i < nargs; i++) {
        if (!jl_is_type(args[i]) && !jl_is_typevar(args[i]))
            jl_error("invalid union type");
    }
    jl_tuple_t *argt = (jl_tuple_t*)jl_f_tuple(NULL, args, nargs);
    return jl_new_uniontype(argt);
}

JL_CALLABLE(jl_f_new_generic_function)
{
    JL_NARGS(new_generic_function, 1, 1);
    JL_TYPECHK(new_generic_function, symbol, args[0]);
    return jl_new_generic_function((jl_sym_t*)args[0]);
}

JL_CALLABLE(jl_f_add_method)
{
    JL_NARGS(add_method, 3, 3);
    if (!jl_is_gf(args[0]))
        jl_error("add_method: not a generic function");
    JL_TYPECHK(add_method, tuple, args[1]);
    JL_TYPECHK(add_method, function, args[2]);
    jl_add_method((jl_function_t*)args[0], (jl_tuple_t*)args[1],
                  (jl_function_t*)args[2]);
    return args[0];
}

// --- init ---

static void add_builtin_method1(jl_function_t *gf, jl_type_t *t, jl_fptr_t f)
{
    jl_add_method(gf, jl_tuple(1, t), jl_new_closure(f, NULL));
}

static void add_builtin(char *name, jl_value_t *v)
{
    jl_set_const(jl_system_module, jl_symbol(name), v);
}

static void add_builtin_func(char *name, jl_fptr_t f)
{
    add_builtin(name, jl_new_closure(f, NULL));
}

void jl_init_builtins()
{
    jl_print_gf = jl_new_generic_function(jl_symbol("print"));

    add_builtin_method1(jl_print_gf, jl_any_type,     jl_f_print_any);
    add_builtin_method1(jl_print_gf, jl_sym_type,     jl_f_print_symbol);
    add_builtin_method1(jl_print_gf, jl_typename_type,jl_f_print_typename);
    add_builtin_method1(jl_print_gf, jl_tvar_type,    jl_f_print_typevar);
    add_builtin_method1(jl_print_gf, jl_lambda_info_type, jl_f_print_linfo);
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

    jl_ref_gf = jl_new_generic_function(jl_symbol("ref"));
    jl_add_method(jl_ref_gf,
                  jl_tuple(2, jl_typector_type,
                           jl_apply_type_ctor(jl_seq_type,
                                              jl_tuple(1,jl_any_type))),
                  jl_new_closure(jl_f_ref_typector, NULL));
    
    add_builtin_func("is", jl_f_is);
    add_builtin_func("typeof", jl_f_typeof);
    add_builtin_func("subtype", jl_f_subtype);
    add_builtin_func("istype", jl_f_istype);
    add_builtin_func("typeassert", jl_f_typeassert);
    add_builtin_func("apply", jl_f_apply);
    add_builtin_func("error", jl_f_error);
    add_builtin_func("load", jl_f_load);
    add_builtin_func("tuple", jl_f_tuple);
    add_builtin_func("convert", jl_f_convert);
    add_builtin_func("Union", jl_f_union);
    add_builtin_func("time_thunk", jl_f_time_thunk);
    add_builtin("print", jl_print_gf);
    add_builtin("ref", jl_ref_gf);
    
    // functions for internal use
    add_builtin_func("tupleref", jl_f_tupleref);
    add_builtin_func("tuplelen", jl_f_tuplelen);
    add_builtin_func("getfield", jl_f_get_field);
    add_builtin_func("setfield", jl_f_set_field);
    add_builtin_func("bufferlen", jl_f_bufferlen);
    add_builtin_func("bufferref", jl_f_bufferref);
    add_builtin_func("bufferset", jl_f_bufferset);
    add_builtin_func("box", jl_f_box);
    add_builtin_func("unbox", jl_f_unbox);
    add_builtin_func("boxset", jl_f_boxset);
    add_builtin_func("ref_typector", jl_f_ref_typector);
    add_builtin_func("typevar", jl_f_typevar);
    add_builtin_func("new_closure", jl_f_new_closure);
    add_builtin_func("new_struct_type", jl_f_new_struct_type);
    add_builtin_func("new_struct_fields", jl_f_new_struct_fields);
    add_builtin_func("new_type_constructor", jl_f_new_type_constructor);
    add_builtin_func("new_tag_type", jl_f_new_tag_type);
    add_builtin_func("new_generic_function", jl_f_new_generic_function);
    add_builtin_func("add_method", jl_f_add_method);

    // builtin types
    add_builtin("Any", jl_any_type);
    add_builtin("Bottom", jl_bottom_type);
    add_builtin("TypeConstructor", jl_typector_type);
    add_builtin("TypeVar", jl_tvar_type);
    add_builtin("Tuple", jl_tuple_type);
    add_builtin("Type", jl_type_type);
    add_builtin("Symbol", jl_sym_type);
    add_builtin("...", jl_seq_type);
    add_builtin("Function", jl_functype_ctor);
    add_builtin("Buffer", jl_buffer_type);
    add_builtin("Tensor", jl_tensor_type);
    add_builtin("Scalar", jl_scalar_type);
    add_builtin("Number", jl_number_type);
    add_builtin("Real", jl_real_type);
    add_builtin("Int", jl_int_type);
    add_builtin("Float", jl_float_type);
    add_builtin("Bool", jl_bool_type);
    add_builtin("Int8", jl_int8_type);
    add_builtin("Uint8", jl_uint8_type);
    add_builtin("Int16", jl_int16_type);
    add_builtin("Uint16", jl_uint16_type);
    add_builtin("Int32", jl_int32_type);
    add_builtin("Uint32", jl_uint32_type);
    add_builtin("Int64", jl_int64_type);
    add_builtin("Uint64", jl_uint64_type);
    add_builtin("Float32", jl_float32_type);
    add_builtin("Float64", jl_float64_type);
}
