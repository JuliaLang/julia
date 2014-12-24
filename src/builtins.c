/*
  implementations of some built-in functions and utilities
*/
#include "platform.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#if defined(_OS_WINDOWS_)
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include <ctype.h>
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

// exceptions -----------------------------------------------------------------

DLLEXPORT void jl_error(const char *str)
{
    if (jl_errorexception_type == NULL) {
        JL_PRINTF(JL_STDERR, "%s", str);
        jl_exit(1);
    }
    jl_value_t *msg = jl_pchar_to_string((char*)str, strlen(str));
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(jl_errorexception_type, msg));
}

DLLEXPORT void jl_errorf(const char *fmt, ...)
{
    va_list args;
    ios_t buf;
    ios_mem(&buf, 0);
    va_start(args, fmt);
    ios_vprintf(&buf, fmt, args);
    va_end(args);
    if (jl_errorexception_type == NULL) {
        JL_PRINTF(JL_STDERR, "%s", buf.buf);
        jl_exit(1);
    }
    jl_value_t *msg = jl_takebuf_string(&buf);
    JL_GC_PUSH1(&msg);
    jl_throw(jl_new_struct(jl_errorexception_type, msg));
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
    JL_GC_PUSH2(&ctxt, &got);
    ctxt = jl_pchar_to_string((char*)context, strlen(context));
    jl_value_t *ex = jl_new_struct(jl_typeerror_type, jl_symbol(fname),
                                   ctxt, ty, got);
    jl_throw(ex);
}

void jl_type_error_rt_line(const char *fname, const char *context,
                           jl_value_t *ty, jl_value_t *got, int line)
{
    jl_type_error_rt(fname, context, ty, got);
}

void jl_type_error(const char *fname, jl_value_t *expected, jl_value_t *got)
{
    jl_type_error_rt(fname, "", expected, got);
}

void jl_undefined_var_error(jl_sym_t *var)
{
    if (var->name[0] == '#') {
        // convention for renamed variables: #...#original_name
        char *nxt = strchr(var->name+1, '#');
        if (nxt)
            var = jl_symbol(nxt+1);
    }
    jl_throw(jl_new_struct(jl_undefvarerror_type, var));
}

JL_CALLABLE(jl_f_throw)
{
    JL_NARGS(throw, 1, 1);
    jl_throw(args[0]);
    return (jl_value_t*)jl_null;
}

void jl_enter_handler(jl_handler_t *eh)
{
    JL_SIGATOMIC_BEGIN();
    eh->prev = jl_current_task->eh;
#ifdef JL_GC_MARKSWEEP
    eh->gcstack = jl_pgcstack;
#endif
    jl_current_task->eh = eh;
    // TODO: this should really go after setjmp(). see comment in
    // ctx_switch in task.c.
    JL_SIGATOMIC_END();
}

void jl_pop_handler(int n)
{
    while (n > 0) {
        jl_eh_restore_state(jl_current_task->eh);
        n--;
    }
}

// primitives -----------------------------------------------------------------

static int bits_equal(void *a, void *b, int sz)
{
    switch (sz) {
    case 1:  return *(int8_t*)a == *(int8_t*)b;
    case 2:  return *(int16_t*)a == *(int16_t*)b;
    case 4:  return *(int32_t*)a == *(int32_t*)b;
    case 8:  return *(int64_t*)a == *(int64_t*)b;
    default: return memcmp(a, b, sz)==0;
    }
}

int jl_egal(jl_value_t *a, jl_value_t *b)
{
    if (a == b)
        return 1;
    jl_value_t *ta = (jl_value_t*)jl_typeof(a);
    if (ta != (jl_value_t*)jl_typeof(b))
        return 0;
    if (jl_is_tuple(a)) {
        size_t l = jl_tuple_len(a);
        if (l != jl_tuple_len(b))
            return 0;
        for(size_t i=0; i < l; i++) {
            if (!jl_egal(jl_tupleref(a,i),jl_tupleref(b,i)))
                return 0;
        }
        return 1;
    }
    jl_datatype_t *dt = (jl_datatype_t*)ta;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dta = (jl_datatype_t*)a;
        jl_datatype_t *dtb = (jl_datatype_t*)b;
        return dta->name == dtb->name &&
            jl_egal((jl_value_t*)dta->parameters, (jl_value_t*)dtb->parameters);
    }
    if (dt->mutabl) return 0;
    size_t sz = dt->size;
    if (sz == 0) return 1;
    size_t nf = jl_tuple_len(dt->names);
    if (nf == 0) {
        return bits_equal(jl_data_ptr(a), jl_data_ptr(b), sz);
    }
    for (size_t f=0; f < nf; f++) {
        size_t offs = dt->fields[f].offset;
        char *ao = (char*)jl_data_ptr(a) + offs;
        char *bo = (char*)jl_data_ptr(b) + offs;
        int eq;
        if (dt->fields[f].isptr) {
            jl_value_t *af = *(jl_value_t**)ao;
            jl_value_t *bf = *(jl_value_t**)bo;
            if (af == bf) eq = 1;
            else if (af==NULL || bf==NULL) eq = 0;
            else eq = jl_egal(af, bf);
        }
        else {
            eq = bits_equal(ao, bo, dt->fields[f].size);
        }
        if (!eq) return 0;
    }
    return 1;
}

JL_CALLABLE(jl_f_is)
{
    JL_NARGS(is, 2, 2);
    if (args[0] == args[1])
        return jl_true;
    return jl_egal(args[0],args[1]) ? jl_true : jl_false;
}

JL_CALLABLE(jl_f_no_function)
{
    jl_error("invalid function object");
    return (jl_value_t*)jl_null;
}

JL_CALLABLE(jl_f_typeof)
{
    JL_NARGS(typeof, 1, 1);
    return jl_full_type(args[0]);
}

JL_CALLABLE(jl_f_sizeof)
{
    JL_NARGS(sizeof, 1, 1);
    jl_value_t *x = args[0];
    if (jl_is_datatype(x)) {
        jl_datatype_t *dx = (jl_datatype_t*)x;
        if (dx->name == jl_array_typename || dx == jl_symbol_type)
            jl_error("type does not have a canonical binary representation");
        if (!(dx->names == jl_null && dx->size > 0)) {
            // names===() and size > 0  =>  bitstype, size always known
            if (dx->abstract || !jl_is_leaf_type(x))
                jl_error("argument is an abstract type; size is indeterminate");
        }
        return jl_box_long(jl_datatype_size(x));
    }
    if (jl_is_array(x)) {
        return jl_box_long(jl_array_len(x) * ((jl_array_t*)x)->elsize);
    }
    if (jl_is_tuple(x)) {
        jl_error("tuples do not yet have a canonical binary representation");
    }
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(x);
    assert(jl_is_datatype(dt));
    assert(!dt->abstract);
    if (dt == jl_symbol_type)
        jl_error("value does not have a canonical binary representation");
    return jl_box_long(jl_datatype_size(dt));
}

JL_CALLABLE(jl_f_subtype)
{
    JL_NARGS(subtype, 2, 2);
    if (!jl_is_typevar(args[0]))
        JL_TYPECHK(subtype, type, args[0]);
    if (!jl_is_typevar(args[1]))
        JL_TYPECHK(subtype, type, args[1]);
    return (jl_subtype(args[0],args[1],0) ? jl_true : jl_false);
}

JL_CALLABLE(jl_f_isa)
{
    JL_NARGS(isa, 2, 2);
    JL_TYPECHK(isa, type, args[1]);
    return (jl_subtype(args[0],args[1],1) ? jl_true : jl_false);
}

DLLEXPORT void jl_typeassert(jl_value_t *x, jl_value_t *t)
{
    if (!jl_subtype(x,t,1))
        jl_type_error("typeassert", t, x);
}

JL_CALLABLE(jl_f_typeassert)
{
    JL_NARGS(typeassert, 2, 2);
    JL_TYPECHK(typeassert, type, args[1]);
    if (!jl_subtype(args[0],args[1],1))
        jl_type_error("typeassert", args[1], args[0]);
    return args[0];
}

static jl_function_t *jl_append_any_func;
extern size_t jl_page_size;

JL_CALLABLE(jl_f_apply)
{
    JL_NARGSV(apply, 2);
    jl_function_t *f;
    jl_function_t *call_func = (jl_function_t*)args[0];
    assert(jl_is_function(call_func));
    if (jl_is_function(args[1])) {
        f = (jl_function_t*)args[1];
        --nargs; ++args; /* args[1] becomes args[0] */
    }
    else { /* do generic call(args...) instead */
        f = call_func;
        // protect "function" arg from splicing
        args[1] = (jl_value_t*)jl_tuple1(args[1]);
    }
    if (nargs == 2) {
        if (f->fptr == &jl_f_tuple) {
            if (jl_is_tuple(args[1]))
                return args[1];
            if (jl_is_array(args[1])) {
                size_t n = jl_array_len(args[1]);
                jl_tuple_t *t = jl_alloc_tuple(n);
                JL_GC_PUSH1(&t);
                for(size_t i=0; i < n; i++) {
                    jl_tupleset(t, i, jl_arrayref((jl_array_t*)args[1], i));
                }
                JL_GC_POP();
                return (jl_value_t*)t;
            }
        }
        if (jl_is_tuple(args[1])) {
            return jl_apply(f, &jl_tupleref(args[1],0), jl_tuple_len(args[1]));
        }
    }
    jl_value_t *argarr = NULL;
    JL_GC_PUSH1(&argarr);
    jl_value_t *result;
    jl_value_t **newargs;
    size_t n=0, i, j;
    for(i=1; i < nargs; i++) {
        if (jl_is_tuple(args[i])) {
            n += jl_tuple_len(args[i]);
        }
        else if (jl_typeis(args[i], jl_array_any_type)) {
            n += jl_array_len(args[i]);
        }
        else {
            if (jl_append_any_func == NULL) {
                jl_append_any_func =
                    (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("append_any"));
                if (jl_append_any_func == NULL) {
                    // error if append_any not available
                    JL_TYPECHK(apply, tuple, args[i]);
                }
            }
            argarr = jl_apply(jl_append_any_func, &args[1], nargs-1);
            assert(jl_typeis(argarr, jl_array_any_type));
            result = jl_apply(f, jl_cell_data(argarr), jl_array_len(argarr));
            JL_GC_POP();
            return result;
        }
    }
    if (n > jl_page_size/sizeof(jl_value_t*)) {
        // put arguments on the heap if there are too many
        argarr = (jl_value_t*)jl_alloc_cell_1d(n);
        newargs = jl_cell_data(argarr);
    }
    else {
        newargs = (jl_value_t**)alloca(n * sizeof(jl_value_t*));
    }
    n = 0;
    for(i=1; i < nargs; i++) {
        if (jl_is_tuple(args[i])) {
            jl_tuple_t *t = (jl_tuple_t*)args[i];
            size_t al = jl_tuple_len(t);
            for(j=0; j < al; j++)
                newargs[n++] = jl_tupleref(t, j);
        }
        else {
            size_t al = jl_array_len(args[i]);
            for(j=0; j < al; j++)
                newargs[n++] = jl_cellref(args[i], j);
        }
    }
    result = jl_apply(f, newargs, n);
    JL_GC_POP();
    return result;
}

JL_CALLABLE(jl_f_kwcall)
{
    if (nargs < 4)
        jl_error("internal error: malformed keyword argument call");
    jl_function_t *f;
    jl_function_t *call_func = (jl_function_t*)args[0];
    assert(jl_is_function(call_func));
    size_t nkeys = jl_unbox_long(args[1]);
    size_t pa = 4 + 2*nkeys;
    jl_array_t *container = (jl_array_t*)args[pa-1];
    assert(jl_array_len(container) > 0);
    f = (jl_function_t*)args[pa-2];
    if (!jl_is_function(f)) {
        // do generic call(args...; kws...) instead
        // switch (f container pa...) to (container f pa...)
        args[pa-2] = args[pa-1];     // TODO: this might not be safe
        args[pa-1] = (jl_value_t*)f;
        f = call_func;
        pa--;
    }

    if (!jl_is_gf(f))
        jl_error("function does not accept keyword arguments");
    jl_function_t *sorter = ((jl_methtable_t*)f->env)->kwsorter;
    if (sorter == NULL) {
        jl_errorf("function %s does not accept keyword arguments",
                  jl_gf_name(f)->name);
    }

    for(size_t i=0; i < nkeys*2; i+=2) {
        jl_cellset(container, i  , args[2+i]);
        jl_cellset(container, i+1, args[2+i+1]);
    }

    args += pa-1;
    nargs -= pa-1;
    assert(jl_is_gf(sorter));
    jl_function_t *m = jl_method_lookup((jl_methtable_t*)sorter->env, args, nargs, 1);
    if (m == jl_bottom_func) {
        return jl_no_method_error(f, args+1, nargs-1);
    }

    return jl_apply(m, args, nargs);
}

// eval -----------------------------------------------------------------------

extern int jl_lineno;

DLLEXPORT jl_value_t *jl_toplevel_eval_in(jl_module_t *m, jl_value_t *ex)
{
    if (m == NULL)
        m = jl_main_module;
    if (jl_is_symbol(ex))
        return jl_eval_global_var(m, (jl_sym_t*)ex);
    jl_value_t *v=NULL;
    int last_lineno = jl_lineno;
    jl_module_t *last_m = jl_current_module;
    jl_module_t *task_last_m = jl_current_task->current_module;
    JL_TRY {
        jl_current_task->current_module = jl_current_module = m;
        v = jl_toplevel_eval(ex);
    }
    JL_CATCH {
        jl_lineno = last_lineno;
        jl_current_module = last_m;
        jl_current_task->current_module = task_last_m;
        jl_rethrow();
    }
    jl_lineno = last_lineno;
    jl_current_module = last_m;
    jl_current_task->current_module = task_last_m;
    assert(v);
    return v;
}

JL_CALLABLE(jl_f_top_eval)
{
    jl_module_t *m;
    jl_value_t *ex;
    if (nargs == 1) {
        m = jl_main_module;
        ex = args[0];
    }
    else {
        JL_NARGS(eval, 2, 2);
        JL_TYPECHK(eval, module, args[0]);
        m = (jl_module_t*)args[0];
        ex = args[1];
    }
    return jl_toplevel_eval_in(m, ex);
}

JL_CALLABLE(jl_f_isdefined)
{
    jl_module_t *m = jl_current_module;
    jl_sym_t *s=NULL;
    JL_NARGSV(isdefined, 1);
    if (jl_is_array(args[0])) {
        return jl_array_isdefined(args, nargs) ? jl_true : jl_false;
    }
    if (nargs == 1) {
        JL_TYPECHK(isdefined, symbol, args[0]);
        s = (jl_sym_t*)args[0];
    }
    if (nargs != 2) {
        JL_NARGS(isdefined, 1, 1);
    }
    else {
        if (!jl_is_module(args[0])) {
            jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(args[0]);
            if (!jl_is_datatype(vt)) {
                jl_type_error("isdefined", (jl_value_t*)jl_datatype_type, args[0]);
            }
            size_t idx;
            if (jl_is_long(args[1])) {
                idx = jl_unbox_long(args[1])-1;
                if (idx >= jl_tuple_len(vt->names))
                    return jl_false;
            }
            else {
                JL_TYPECHK(isdefined, symbol, args[1]);
                idx = jl_field_index(vt, (jl_sym_t*)args[1], 0);
                if ((int)idx == -1)
                    return jl_false;
            }
            return jl_field_isdefined(args[0], idx) ? jl_true : jl_false;
        }
        JL_TYPECHK(isdefined, module, args[0]);
        JL_TYPECHK(isdefined, symbol, args[1]);
        m = (jl_module_t*)args[0];
        s = (jl_sym_t*)args[1];
    }
    assert(s);
    return jl_boundp(m, s) ? jl_true : jl_false;
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
    if (i >= jl_tuple_len(t))
        jl_throw(jl_bounds_exception);
    return jl_tupleref(t, i);
}

JL_CALLABLE(jl_f_tuplelen)
{
    JL_NARGS(tuplelen, 1, 1);
    JL_TYPECHK(tuplelen, tuple, args[0]);
    return jl_box_long(jl_tuple_len(args[0]));
}

// composite types ------------------------------------------------------------

JL_CALLABLE(jl_f_get_field)
{
    JL_NARGS(getfield, 2, 2);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (vt == (jl_value_t*)jl_module_type) {
        JL_TYPECHK(getfield, symbol, args[1]);
        return jl_eval_global_var((jl_module_t*)v, (jl_sym_t*)args[1]);
    }
    if (!jl_is_datatype(vt))
        jl_type_error("getfield", (jl_value_t*)jl_datatype_type, v);
    jl_datatype_t *st = (jl_datatype_t*)vt;
    size_t idx;
    if (jl_is_long(args[1])) {
        idx = jl_unbox_long(args[1])-1;
        if (idx >= jl_tuple_len(st->names))
            jl_throw(jl_bounds_exception);
    }
    else {
        JL_TYPECHK(getfield, symbol, args[1]);
        jl_sym_t *fld = (jl_sym_t*)args[1];
        idx = jl_field_index(st, fld, 1);
    }
    jl_value_t *fval = jl_get_nth_field(v, idx);
    if (fval == NULL)
        jl_throw(jl_undefref_exception);
    return fval;
}

JL_CALLABLE(jl_f_set_field)
{
    JL_NARGS(setfield!, 3, 3);
    jl_value_t *v = args[0];
    jl_value_t *vt = (jl_value_t*)jl_typeof(v);
    if (vt == (jl_value_t*)jl_module_type)
        jl_error("cannot assign variables in other modules");
    if (!jl_is_datatype(vt))
        jl_type_error("setfield!", (jl_value_t*)jl_datatype_type, v);
    jl_datatype_t *st = (jl_datatype_t*)vt;
    if (!st->mutabl)
        jl_errorf("type %s is immutable", st->name->name->name);
    size_t idx;
    if (jl_is_long(args[1])) {
        idx = jl_unbox_long(args[1])-1;
        if (idx >= jl_tuple_len(st->names))
            jl_throw(jl_bounds_exception);
    }
    else {
        JL_TYPECHK(setfield!, symbol, args[1]);
        idx = jl_field_index(st, (jl_sym_t*)args[1], 1);
    }
    jl_value_t *ft = jl_tupleref(st->types, idx);
    if (!jl_subtype(args[2], ft, 1)) {
        jl_type_error("setfield!", ft, args[2]);
    }
    jl_set_nth_field(v, idx, args[2]);
    return args[2];
}

JL_CALLABLE(jl_f_field_type)
{
    JL_NARGS(fieldtype, 2, 2);
    jl_datatype_t *st = (jl_datatype_t*)args[0];
    if (!jl_is_datatype(st))
        jl_type_error("fieldtype", (jl_value_t*)jl_datatype_type, (jl_value_t*)st);
    int field_index;
    if (jl_is_long(args[1])) {
        field_index = jl_unbox_long(args[1]) - 1;
        if (field_index < 0 || field_index >= jl_tuple_len(st->names))
            jl_throw(jl_bounds_exception);
    }
    else {
        JL_TYPECHK(fieldtype, symbol, args[1]);
        field_index = jl_field_index(st, (jl_sym_t*)args[1], 1);
    }
    return jl_tupleref(st->types, field_index);
}

// conversion -----------------------------------------------------------------

DLLEXPORT void *jl_symbol_name(jl_sym_t *s)
{
    return s->name;
}

//WARNING: THIS FUNCTION IS NEVER CALLED BUT INLINE BY CCALL
DLLEXPORT void *jl_array_ptr(jl_array_t *a)
{
    return a->data;
}
DLLEXPORT void *jl_value_ptr(jl_value_t *a)
{
    return (void*)a;
}

// printing -------------------------------------------------------------------

DLLEXPORT void jl_print_symbol(JL_STREAM *s, jl_sym_t *sym)
{
    JL_PUTS(sym->name,s);
}

// for bootstrap
DLLEXPORT void jl_print_int64(JL_STREAM *s, int64_t i)
{
    JL_PRINTF(s, "%lld", i);
}

DLLEXPORT int jl_substrtod(char *str, size_t offset, int len, double *out)
{
    char *p;
    errno = 0;
    char *bstr = str+offset;
    char *pend = bstr+len;
    int err = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr = (char*)malloc(len+1);
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
    *out = strtod_c(bstr, &p);
    if ((p == bstr) || (p != pend) ||
        (errno==ERANGE && (*out==0 || *out==HUGE_VAL || *out==-HUGE_VAL)))
        err = 1;
    if (bstr != str+offset)
        free(bstr);
    return err;
}

DLLEXPORT int jl_strtod(char *str, double *out)
{
    char *p;
    errno = 0;
    *out = strtod_c(str, &p);
    if (p == str ||
        (errno==ERANGE && (*out==0 || *out==HUGE_VAL || *out==-HUGE_VAL)))
        return 1;
    while (*p != '\0') {
        if (!isspace((unsigned char)*p))
            return 1;
        p++;
    }
    return 0;
}

// MSVC pre-2013 did not define HUGE_VALF
#ifndef HUGE_VALF
#define HUGE_VALF (1e25f * 1e25f)
#endif

DLLEXPORT int jl_substrtof(char *str, int offset, int len, float *out)
{
    char *p;
    errno = 0;
    char *bstr = str+offset;
    char *pend = bstr+len;
    int err = 0;
    if (!(*pend == '\0' || isspace((unsigned char)*pend) || *pend == ',')) {
        // confusing data outside substring. must copy.
        char *newstr = (char*)malloc(len+1);
        memcpy(newstr, bstr, len);
        newstr[len] = 0;
        bstr = newstr;
        pend = bstr+len;
    }
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
    *out = (float)strtod_c(bstr, &p);
#else
    *out = strtof_c(bstr, &p);
#endif

    if ((p == bstr) || (p != pend) ||
        (errno==ERANGE && (*out==0 || *out==HUGE_VALF || *out==-HUGE_VALF)))
        err = 1;
    if (bstr != str+offset)
        free(bstr);
    return err;
}

DLLEXPORT int jl_strtof(char *str, float *out)
{
    char *p;
    errno = 0;
#if defined(_OS_WINDOWS_) && !defined(_COMPILER_MINGW_)
    *out = (float)strtod_c(str, &p);
#else
    *out = strtof_c(str, &p);
#endif
    if (p == str ||
        (errno==ERANGE && (*out==0 || *out==HUGE_VALF || *out==-HUGE_VALF)))
        return 1;
    while (*p != '\0') {
        if (!isspace((unsigned char)*p))
            return 1;
        p++;
    }
    return 0;
}

// showing --------------------------------------------------------------------

void jl_flush_cstdio(void)
{
    fflush(stdout);
    fflush(stderr);
}

jl_value_t *jl_stdout_obj(void)
{
    jl_value_t *stdout_obj = jl_get_global(jl_base_module, jl_symbol("STDOUT"));
    if (stdout_obj != NULL) return stdout_obj;
    return jl_get_global(jl_base_module, jl_symbol("OUTPUT_STREAM"));
}

jl_value_t *jl_stderr_obj(void)
{
    jl_value_t *stderr_obj = jl_get_global(jl_base_module, jl_symbol("STDERR"));
    if (stderr_obj != NULL) return stderr_obj;
    return jl_get_global(jl_base_module, jl_symbol("OUTPUT_STREAM"));
}

static jl_function_t *jl_show_gf=NULL;

void jl_show(jl_value_t *stream, jl_value_t *v)
{
    if (jl_base_module) {
        if (jl_show_gf == NULL) {
            jl_show_gf = (jl_function_t*)jl_get_global(jl_base_module, jl_symbol("show"));
        }
        if (jl_show_gf==NULL || stream==NULL) {
            JL_PRINTF(JL_STDERR, " could not show value of type %s",
                      jl_is_tuple(v) ? "Tuple" :
                      ((jl_datatype_t*)jl_typeof(v))->name->name->name);
            return;
        }
        jl_value_t *args[2] = {stream,v};
        jl_apply(jl_show_gf, args, 2);
    }
}

// internal functions ---------------------------------------------------------

extern int jl_in_inference;
extern int jl_boot_file_loaded;
int jl_eval_with_compiler_p(jl_expr_t *expr, int compileloops);

void jl_trampoline_compile_function(jl_function_t *f, int always_infer, jl_tuple_t *sig)
{
    assert(f->linfo != NULL);
    // to run inference on all thunks. slows down loading files.
    // NOTE: if this call to inference is removed, type_annotate in inference.jl
    // needs to be updated to infer inner functions.
    if (f->linfo->inferred == 0) {
        if (!jl_in_inference) {
            if (!jl_is_expr(f->linfo->ast)) {
                f->linfo->ast = jl_uncompress_ast(f->linfo, f->linfo->ast);
            }
            if (always_infer || jl_eval_with_compiler_p(jl_lam_body((jl_expr_t*)f->linfo->ast),1)) {
                jl_type_infer(f->linfo, sig, f->linfo);
            }
        }
    }
    jl_compile(f);
    // this assertion is probably not correct; the fptr could have been assigned
    // by a recursive invocation from inference above.
    //assert(f->fptr == &jl_trampoline);
    jl_generate_fptr(f);
    if (jl_boot_file_loaded && jl_is_expr(f->linfo->ast)) {
        f->linfo->ast = jl_compress_ast(f->linfo, f->linfo->ast);
    }
}

JL_CALLABLE(jl_trampoline)
{
    assert(jl_is_func(F));
    jl_function_t *f = (jl_function_t*)F;
    jl_trampoline_compile_function(f, 0, jl_tuple_type);
    return jl_apply(f, args, nargs);
}

JL_CALLABLE(jl_f_instantiate_type)
{
    JL_NARGSV(instantiate_type, 1);
    if (!jl_is_datatype(args[0]))
        JL_TYPECHK(instantiate_type, typector, args[0]);
    return jl_apply_type_(args[0], &args[1], nargs-1);
}

DLLEXPORT jl_value_t *jl_new_type_constructor(jl_tuple_t *p, jl_value_t *t)
{
    jl_value_t *tc = (jl_value_t*)jl_new_type_ctor(p, t);
    int i;
    for(i=0; i < jl_tuple_len(p); i++)
        ((jl_tvar_t*)jl_tupleref(p,i))->bound = 0;
    return tc;
}

JL_CALLABLE(jl_f_union)
{
    if (nargs == 0) return (jl_value_t*)jl_bottom_type;
    if (nargs == 1) return args[0];
    size_t i;
    jl_tuple_t *argt = jl_alloc_tuple_uninit(nargs);
    for(i=0; i < nargs; i++) {
        if (!jl_is_type(args[i]) && !jl_is_typevar(args[i])) {
            jl_error("invalid union type");
        }
        else {
            jl_tupleset(argt, i, args[i]);
        }
    }
    JL_GC_PUSH1(&argt);
    jl_value_t *u = jl_type_union(argt);
    JL_GC_POP();
    return u;
}

// generic function reflection ------------------------------------------------

JL_CALLABLE(jl_f_methodexists)
{
    JL_NARGS(method_exists, 2, 2);
    JL_TYPECHK(method_exists, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("method_exists: not a generic function");
    JL_TYPECHK(method_exists, tuple, args[1]);
    jl_check_type_tuple((jl_tuple_t*)args[1], jl_gf_name(args[0]),
                        "method_exists");
    return jl_method_lookup_by_type(jl_gf_mtable(args[0]),
                                    (jl_tuple_t*)args[1],0,0)!=jl_bottom_func ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_applicable)
{
    JL_NARGSV(applicable, 1);
    JL_TYPECHK(applicable, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("applicable: not a generic function");
    return jl_method_lookup(jl_gf_mtable(args[0]),
                            &args[1], nargs-1, 1) != jl_bottom_func ?
        jl_true : jl_false;
}

JL_CALLABLE(jl_f_invoke)
{
    JL_NARGSV(invoke, 2);
    JL_TYPECHK(invoke, function, args[0]);
    if (!jl_is_gf(args[0]))
        jl_error("invoke: not a generic function");
    JL_TYPECHK(invoke, tuple, args[1]);
    jl_check_type_tuple((jl_tuple_t*)args[1], jl_gf_name(args[0]), "invoke");
    if (!jl_tuple_subtype(&args[2], nargs-2, &jl_tupleref(args[1],0),
                          jl_tuple_len(args[1]), 1))
        jl_error("invoke: argument type error");
    return jl_gf_invoke((jl_function_t*)args[0],
                        (jl_tuple_t*)args[1], &args[2], nargs-2);
}

// eq hash table --------------------------------------------------------------

#include "table.c"

// hashing --------------------------------------------------------------------

#ifdef _P64
#define bitmix(a,b) int64hash((a)^bswap_64(b))
#define hash64(a)   int64hash(a)
#else
#define bitmix(a,b) int64to32hash((((uint64_t)a)<<32)|((uint64_t)b))
#define hash64(a)   int64to32hash(a)
#endif

static uptrint_t bits_hash(void *b, size_t sz)
{
    switch (sz) {
    case 1:  return int32hash(*(int8_t*)b);
    case 2:  return int32hash(*(int16_t*)b);
    case 4:  return int32hash(*(int32_t*)b);
    case 8:  return hash64(*(int64_t*)b);
    default:
#ifdef _P64
        return memhash((char*)b, sz);
#else
        return memhash32((char*)b, sz);
#endif
    }
}

DLLEXPORT uptrint_t jl_object_id(jl_value_t *v)
{
    if (jl_is_symbol(v))
        return ((jl_sym_t*)v)->hash;
    jl_value_t *tv = (jl_value_t*)jl_typeof(v);
    if (tv == (jl_value_t*)jl_tuple_type) {
        uptrint_t h = 0;
        size_t l = jl_tuple_len(v);
        for(size_t i = 0; i < l; i++) {
            uptrint_t u = jl_object_id(jl_tupleref(v,i));
            h = bitmix(h, u);
        }
        return h;
    }
    jl_datatype_t *dt = (jl_datatype_t*)tv;
    if (dt == jl_datatype_type) {
        jl_datatype_t *dtv = (jl_datatype_t*)v;
        uptrint_t h = inthash((uptrint_t)tv);
        return bitmix(bitmix(h, jl_object_id((jl_value_t*)dtv->name)),
                      jl_object_id((jl_value_t*)dtv->parameters));
    }
    if (dt->mutabl) return inthash((uptrint_t)v);
    size_t sz = jl_datatype_size(tv);
    uptrint_t h = inthash((uptrint_t)tv);
    if (sz == 0) return ~h;
    size_t nf = jl_tuple_len(dt->names);
    if (nf == 0) {
        return bits_hash(jl_data_ptr(v), sz) ^ h;
    }
    for (size_t f=0; f < nf; f++) {
        size_t offs = dt->fields[f].offset;
        char *vo = (char*)jl_data_ptr(v) + offs;
        uptrint_t u;
        if (dt->fields[f].isptr) {
            jl_value_t *f = *(jl_value_t**)vo;
            u = f==NULL ? 0 : jl_object_id(f);
        }
        else {
            u = bits_hash(vo, dt->fields[f].size);
        }
        h = bitmix(h, u);
    }
    return h;
}

// init -----------------------------------------------------------------------

static void add_builtin(const char *name, jl_value_t *v)
{
    jl_set_const(jl_core_module, jl_symbol(name), v);
}

static void add_builtin_func(const char *name, jl_fptr_t f)
{
    add_builtin(name, (jl_value_t*)
                jl_new_closure(f, (jl_value_t*)jl_symbol(name), NULL));
}

void jl_init_primitives(void)
{
    add_builtin_func("is", jl_f_is);
    add_builtin_func("typeof", jl_f_typeof);
    add_builtin_func("sizeof", jl_f_sizeof);
    add_builtin_func("issubtype", jl_f_subtype);
    add_builtin_func("isa", jl_f_isa);
    add_builtin_func("typeassert", jl_f_typeassert);
    add_builtin_func("_apply", jl_f_apply);
    add_builtin_func("kwcall", jl_f_kwcall);
    add_builtin_func("throw", jl_f_throw);
    add_builtin_func("tuple", jl_f_tuple);
    add_builtin_func("Union", jl_f_union);
    add_builtin_func("method_exists", jl_f_methodexists);
    add_builtin_func("applicable", jl_f_applicable);
    add_builtin_func("invoke", jl_f_invoke);
    add_builtin_func("eval", jl_f_top_eval);
    add_builtin_func("isdefined", jl_f_isdefined);
    add_builtin_func("yieldto", jl_f_yieldto);

    // functions for internal use
    add_builtin_func("tupleref",  jl_f_tupleref);
    add_builtin_func("tuplelen",  jl_f_tuplelen);
    add_builtin_func("getfield",  jl_f_get_field);
    add_builtin_func("setfield!",  jl_f_set_field);
    add_builtin_func("fieldtype", jl_f_field_type);
    add_builtin_func("_expr", jl_f_new_expr);

    add_builtin_func("arraylen", jl_f_arraylen);
    add_builtin_func("arrayref", jl_f_arrayref);
    add_builtin_func("arrayset", jl_f_arrayset);
    add_builtin_func("arraysize", jl_f_arraysize);

    add_builtin_func("apply_type", jl_f_instantiate_type);

    // builtin types
    add_builtin("Any", (jl_value_t*)jl_any_type);
    add_builtin("Top",  (jl_value_t*)jl_top_type);
    add_builtin("Void", (jl_value_t*)jl_void_type);
    add_builtin("nothing", (jl_value_t*)jl_nothing);
    add_builtin("TypeVar", (jl_value_t*)jl_tvar_type);
    add_builtin("TypeName", (jl_value_t*)jl_typename_type);
    add_builtin("TypeConstructor", (jl_value_t*)jl_typector_type);
    add_builtin("Tuple", (jl_value_t*)jl_tuple_type);
    add_builtin("NTuple", (jl_value_t*)jl_ntuple_type);
    add_builtin("Type", (jl_value_t*)jl_type_type);
    add_builtin("Vararg", (jl_value_t*)jl_vararg_type);
    add_builtin("DataType", (jl_value_t*)jl_datatype_type);
    add_builtin("UnionType", (jl_value_t*)jl_uniontype_type);
    add_builtin("Undef", (jl_value_t*)jl_undef_type);

    add_builtin("Module", (jl_value_t*)jl_module_type);
    add_builtin("Method", (jl_value_t*)jl_method_type);
    add_builtin("MethodTable", (jl_value_t*)jl_methtable_type);
    add_builtin("Symbol", (jl_value_t*)jl_sym_type);
    add_builtin("IntrinsicFunction", (jl_value_t*)jl_intrinsic_type);
    add_builtin("Function", (jl_value_t*)jl_function_type);
    add_builtin("LambdaStaticData", (jl_value_t*)jl_lambda_info_type);
    add_builtin("Ptr", (jl_value_t*)jl_pointer_type);
    add_builtin("Box", (jl_value_t*)jl_box_type);
    add_builtin("Task", (jl_value_t*)jl_task_type);

    add_builtin("AbstractArray", (jl_value_t*)jl_abstractarray_type);
    add_builtin("DenseArray", (jl_value_t*)jl_densearray_type);
    add_builtin("Array", (jl_value_t*)jl_array_type);

    add_builtin("Expr", (jl_value_t*)jl_expr_type);
    add_builtin("LineNumberNode", (jl_value_t*)jl_linenumbernode_type);
    add_builtin("LabelNode", (jl_value_t*)jl_labelnode_type);
    add_builtin("GotoNode", (jl_value_t*)jl_gotonode_type);
    add_builtin("QuoteNode", (jl_value_t*)jl_quotenode_type);
    add_builtin("TopNode", (jl_value_t*)jl_topnode_type);
    add_builtin("NewvarNode", (jl_value_t*)jl_newvarnode_type);

#ifdef _P64
    add_builtin("Int", (jl_value_t*)jl_int64_type);
#else
    add_builtin("Int", (jl_value_t*)jl_int32_type);
#endif

    add_builtin("ANY", jl_ANY_flag);
}

// toys for debugging ---------------------------------------------------------

// comma_one prints a comma for 1 element, e.g. "(x,)"
static size_t jl_show_tuple(JL_STREAM *out, jl_tuple_t *t, char *opn, char *cls, int comma_one)
{
    size_t i, n=0, len = jl_tuple_len(t);
    n += JL_PRINTF(out, "(");
    for (i = 0; i < len; i++) {
        jl_value_t *v = jl_tupleref(t,i);
        n += jl_static_show(out, v);
        if (len == 1)
            n += JL_PRINTF(out, ",");
        else if (i != len-1)
            n += JL_PRINTF(out, ", ");
    }
    n += JL_PRINTF(out, ")");
    return n;
}

DLLEXPORT size_t jl_static_show(JL_STREAM *out, jl_value_t *v)
{
    // mimic jl_show, but never calling a julia method
    size_t n = 0;
    if (v == NULL) {
        n += JL_PRINTF(out, "#<null>");
    }
    else if (v->type == NULL) {
        n += JL_PRINTF(out, "<?::#null>");
    }
    else if ((uptrint_t)v->type < 4096U) {
        n += JL_PRINTF(out, "<?::#%d>", (int)(uptrint_t)v->type);
    }
    else if (jl_is_lambda_info(v)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)v;
        n += jl_static_show(out, (jl_value_t*)li->module);
        n += JL_PRINTF(out, ".%s", li->name->name);
        if (li->specTypes) {
            n += jl_static_show(out, (jl_value_t*)li->specTypes);
        }
        else {
            n += JL_PRINTF(out, "(?)");
        }
        // The following is nice for debugging, but allocates memory and generates a lot of output
        // so it may not be a good idea to to have it active
        //JL_PRINTF(out, " -> ");
        //jl_static_show(out, !jl_is_expr(li->ast) ? jl_uncompress_ast(li, li->ast) : li->ast);
    }
    else if (jl_is_tuple(v)) {
        n += jl_show_tuple(out, (jl_tuple_t*)v, "(", ")", 1);
    }
    else if (jl_is_vararg_type(v)) {
        n += jl_static_show(out, jl_tparam0(v));
        n += JL_PRINTF(out, "...");
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dv = (jl_datatype_t*)v;
        if (dv->name->module != jl_core_module) {
            n += jl_static_show(out, (jl_value_t*)dv->name->module);
            JL_PUTS(".", out); n += 1;
        }
        n += JL_PRINTF(out, "%s", dv->name->name->name);
        if (dv->parameters && (jl_value_t*)dv != dv->name->primary) {
            size_t j, tlen = jl_tuple_len(dv->parameters);
            if (tlen > 0) {
                n += JL_PRINTF(out, "{");
                for (j = 0; j < tlen; j++) {
                    jl_value_t *p = jl_tupleref(dv->parameters,j);
                    n += jl_static_show(out, p);
                    if (j != tlen-1)
                        n += JL_PRINTF(out, ", ");
                }
                n += JL_PRINTF(out, "}");
            }
        }
    }
    else if (jl_is_func(v)) {
        if (jl_is_gf(v)) {
            n += JL_PRINTF(out, "%s", jl_gf_name(v)->name);
        }
        else {
            n += JL_PRINTF(out, "#<function>");
        }
    }
    else if (jl_typeis(v, jl_intrinsic_type)) {
        n += JL_PRINTF(out, "#<intrinsic function %d>", *(uint32_t*)jl_data_ptr(v));
    }
    else if (jl_is_int64(v)) {
        n += JL_PRINTF(out, "%lld", jl_unbox_int64(v));
    }
    else if (jl_is_int32(v)) {
        n += JL_PRINTF(out, "%d", jl_unbox_int32(v));
    }
    else if (jl_typeis(v,jl_int16_type)) {
        n += JL_PRINTF(out, "%hd", jl_unbox_int16(v));
    }
    else if (jl_typeis(v,jl_int8_type)) {
        n += JL_PRINTF(out, "%hhd", jl_unbox_int8(v));
    }
    else if (jl_is_uint64(v)) {
        n += JL_PRINTF(out, "0x%016llx", jl_unbox_uint64(v));
    }
    else if (jl_is_uint32(v)) {
        n += JL_PRINTF(out, "0x%08x", jl_unbox_uint32(v));
    }
    else if (jl_typeis(v,jl_uint16_type)) {
        n += JL_PRINTF(out, "0x%04hx", jl_unbox_uint16(v));
    }
    else if (jl_typeis(v,jl_uint8_type)) {
        n += JL_PRINTF(out, "0x%02hhx", jl_unbox_uint8(v));
    }
    else if (jl_is_cpointer(v)) {
#ifdef _P64
        n += JL_PRINTF(out, "0x%016llx", jl_unbox_voidpointer(v));
#else
        n += JL_PRINTF(out, "0x%08x", jl_unbox_voidpointer(v));
#endif
    }
    else if (jl_is_float32(v)) {
        n += JL_PRINTF(out, "%g", jl_unbox_float32(v));
    }
    else if (jl_is_float64(v)) {
        n += JL_PRINTF(out, "%g", jl_unbox_float64(v));
    }
    else if (v == jl_true) {
        n += JL_PRINTF(out, "true");
    }
    else if (v == jl_false) {
        n += JL_PRINTF(out, "false");
    }
    else if (v == jl_nothing) {
        n += JL_PRINTF(out, "nothing");
    }
    else if (jl_is_byte_string(v)) {
        n += JL_PRINTF(out, "\"%s\"", jl_iostr_data(v));
    }
    else if (jl_is_uniontype(v)) {
        n += JL_PRINTF(out, "Union");
        n += jl_static_show(out, (jl_value_t*)((jl_uniontype_t*)v)->types);
    }
    else if (jl_is_typector(v)) {
        n += jl_static_show(out, ((jl_typector_t*)v)->body);
    }
    else if (jl_is_typevar(v)) {
        if (((jl_tvar_t*)v)->lb != jl_bottom_type) {
            n += jl_static_show(out, ((jl_tvar_t*)v)->lb);
            n += JL_PRINTF(out, "<:");
        }
        n += JL_PRINTF(out, "%s%s<:", (((jl_tvar_t*)v)->bound)?"#":"", ((jl_tvar_t*)v)->name->name);
        n += jl_static_show(out, ((jl_tvar_t*)v)->ub);
    }
    else if (jl_is_module(v)) {
        jl_module_t *m = (jl_module_t*)v;
        if (m->parent != m && m->parent != jl_main_module) {
            n += jl_static_show(out, (jl_value_t*)m->parent);
            n += JL_PRINTF(out, ".");
        }
        n += JL_PRINTF(out, "%s", m->name->name);
    }
    else if (jl_is_symbol(v)) {
        n += JL_PRINTF(out, ":%s", ((jl_sym_t*)v)->name);
    }
    else if (jl_is_symbolnode(v)) {
        n += JL_PRINTF(out, "%s::", jl_symbolnode_sym(v)->name);
        n += jl_static_show(out, jl_symbolnode_type(v));
    }
    else if (jl_is_getfieldnode(v)) {
        n += jl_static_show(out, jl_getfieldnode_val(v));
        n += JL_PRINTF(out, ".%s", jl_getfieldnode_name(v)->name);
        n += JL_PRINTF(out, "::");
        n += jl_static_show(out, jl_getfieldnode_type(v));
    }
    else if (jl_is_labelnode(v)) {
        n += JL_PRINTF(out, "%d:", jl_labelnode_label(v));
    }
    else if (jl_is_gotonode(v)) {
        n += JL_PRINTF(out, "goto %d", jl_gotonode_label(v));
    }
    else if (jl_is_quotenode(v)) {
        jl_value_t *qv = jl_fieldref(v,0);
        if (!jl_is_symbol(qv)) { n += JL_PRINTF(out, "quote "); }
        n += jl_static_show(out, qv);
        if (!jl_is_symbol(qv)) { n += JL_PRINTF(out, " end"); }
    }
    else if (jl_is_topnode(v)) {
        n += JL_PRINTF(out, "top(");
        n += jl_static_show(out, jl_fieldref(v,0));
        n += JL_PRINTF(out, ")");
    }
    else if (jl_is_linenode(v)) {
        n += JL_PRINTF(out, "# line %d", jl_linenode_line(v));
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        if (e->head == assign_sym && jl_array_len(e->args) == 2) {
            n += jl_static_show(out, jl_exprarg(e,0));
            n += JL_PRINTF(out, " = ");
            n += jl_static_show(out, jl_exprarg(e,1));
        }
        else {
            char sep = ' ';
            if (e->head == body_sym)
                sep = '\n';
            n += JL_PRINTF(out, "Expr(:%s", e->head->name);
            size_t i, len = jl_array_len(e->args);
            for (i = 0; i < len; i++) {
                n += JL_PRINTF(out, ",%c", sep);
                n += jl_static_show(out, jl_exprarg(e,i));
            }
            n += JL_PRINTF(out, ")::");
            n += jl_static_show(out, e->etype);
        }
    }
    else if (jl_is_array(v)) {
        n += jl_static_show(out, jl_typeof(v));
        n += JL_PRINTF(out, "[");
        size_t j, tlen = jl_array_len(v);
        for (j = 0; j < tlen; j++) {
            jl_value_t *elt;
            if (((jl_array_t*)v)->ptrarray)
                elt = jl_cellref(v, j);
            else
                elt = jl_arrayref((jl_array_t*)v,j);
            n += jl_static_show(out, elt);
            if (j != tlen-1)
                n += JL_PRINTF(out, ", ");
        }
        n += JL_PRINTF(out, "]");
    }
    else if (jl_typeis(v,jl_loaderror_type)) {
        n += JL_PRINTF(out, "LoadError(at ");
        n += jl_static_show(out, jl_fieldref(v, 0));
        n += JL_PRINTF(out, " line ");
        n += jl_static_show(out, jl_fieldref(v, 1));
        n += JL_PRINTF(out, ": ");
        n += jl_static_show(out, jl_fieldref(v, 2));
        n += JL_PRINTF(out, ")");
    }
    else if (jl_typeis(v,jl_errorexception_type)) {
        n += JL_PRINTF(out, "ErrorException(");
        n += jl_static_show(out, jl_fieldref(v, 0));
        n += JL_PRINTF(out, ")");
    }
    else if (jl_is_datatype(jl_typeof(v))) {
        jl_datatype_t *t = (jl_datatype_t*)jl_typeof(v);
        n += jl_static_show(out, (jl_value_t*)t);
        n += JL_PRINTF(out, "(");
        size_t nb = jl_datatype_size(t);
        size_t tlen = jl_tuple_len(t->names);
        if (nb > 0 && tlen == 0) {
            char *data = (char*)jl_data_ptr(v);
            n += JL_PRINTF(out, "0x");
            for(int i=nb-1; i >= 0; --i)
                n += JL_PRINTF(out, "%02hhx", data[i]);
        }
        else {
            jl_value_t *fldval=NULL;
            JL_GC_PUSH1(&fldval);
            for (size_t i = 0; i < tlen; i++) {
                n += JL_PRINTF(out, ((jl_sym_t*)jl_tupleref(t->names, i))->name);
                //jl_fielddesc_t f = t->fields[i];
                n += JL_PRINTF(out, "=");
                fldval = jl_get_nth_field(v, i);
                n += jl_static_show(out, fldval);
                if (i != tlen-1)
                    n += JL_PRINTF(out, ", ");
            }
            JL_GC_POP();
        }
        n += JL_PRINTF(out, ")");
    }
    else {
        n += JL_PRINTF(out, "<?::");
        n += jl_static_show(out, jl_typeof(v));
        n += JL_PRINTF(out, ">");
    }
    return n;
}

int in_jl_ = 0;
DLLEXPORT void jl_(void *jl_value)
{
    in_jl_++;
    JL_TRY {
        (void)jl_static_show(JL_STDOUT, (jl_value_t*)jl_value);
        JL_PRINTF(JL_STDOUT,"\n");
    }
    JL_CATCH {
        JL_PRINTF(JL_STDOUT, "\n!!! ERROR in jl_ -- ABORTING !!!\n");
    }
    in_jl_--;
}

DLLEXPORT void jl_breakpoint(jl_value_t *v)
{
    // put a breakpoint in you debugger here
}

#ifdef __cplusplus
}
#endif
