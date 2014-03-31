/*
  Extra femtoLisp builtin functions
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "flisp.h"

#if !defined(_OS_WINDOWS_)
#include <sys/time.h>
#endif /* !_OS_WINDOWS_ */

size_t llength(value_t v)
{
    size_t n = 0;
    while (iscons(v)) {
        n++;
        v = cdr_(v);
    }
    return n;
}

static value_t fl_nconc(value_t *args, u_int32_t nargs)
{
    if (nargs == 0)
        return FL_NIL;
    value_t lst, first=FL_NIL;
    value_t *pcdr = &first;
    cons_t *c;
    uint32_t i=0;
    while (1) {
        lst = args[i++];
        if (i >= nargs) break;
        if (iscons(lst)) {
            *pcdr = lst;
            c = (cons_t*)ptr(lst);
            while (iscons(c->cdr))
                c = (cons_t*)ptr(c->cdr);
            pcdr = &c->cdr;
        }
        else if (lst != FL_NIL) {
            type_error("nconc", "cons", lst);
        }
    }
    *pcdr = lst;
    return first;
}

static value_t fl_assq(value_t *args, u_int32_t nargs)
{
    argcount("assq", nargs, 2);
    value_t item = args[0];
    value_t v = args[1];
    value_t bind;

    while (iscons(v)) {
        bind = car_(v);
        if (iscons(bind) && car_(bind) == item)
            return bind;
        v = cdr_(v);
    }
    return FL_F;
}

static value_t fl_memq(value_t *args, u_int32_t nargs)
{
    argcount("memq", nargs, 2);
    while (iscons(args[1])) {
        cons_t *c = (cons_t*)ptr(args[1]);
        if (c->car == args[0])
            return args[1];
        args[1] = c->cdr;
    }
    return FL_F;
}

static value_t fl_length(value_t *args, u_int32_t nargs)
{
    argcount("length", nargs, 1);
    value_t a = args[0];
    cvalue_t *cv;
    if (isvector(a)) {
        return fixnum(vector_size(a));
    }
    else if (iscprim(a)) {
        cv = (cvalue_t*)ptr(a);
        if (cp_class(cv) == bytetype)
            return fixnum(1);
        else if (cp_class(cv) == wchartype)
            return fixnum(u8_charlen(*(uint32_t*)cp_data((cprim_t*)cv)));
    }
    else if (iscvalue(a)) {
        cv = (cvalue_t*)ptr(a);
        if (cv_class(cv)->eltype != NULL)
            return size_wrap(cvalue_arraylen(a));
    }
    else if (a == FL_NIL) {
        return fixnum(0);
    }
    else if (iscons(a)) {
        return fixnum(llength(a));
    }
    type_error("length", "sequence", a);
}

static value_t fl_f_raise(value_t *args, u_int32_t nargs)
{
    argcount("raise", nargs, 1);
    fl_raise(args[0]);
}

static value_t fl_exit(value_t *args, u_int32_t nargs)
{
    if (nargs > 0)
        exit(tofixnum(args[0], "exit"));
    exit(0);
    return FL_NIL;
}

static value_t fl_symbol(value_t *args, u_int32_t nargs)
{
    argcount("symbol", nargs, 1);
    if (!fl_isstring(args[0]))
        type_error("symbol", "string", args[0]);
    return symbol((char*)cvalue_data(args[0]));
}

static value_t fl_keywordp(value_t *args, u_int32_t nargs)
{
    argcount("keyword?", nargs, 1);
    return (issymbol(args[0]) &&
            iskeyword((symbol_t*)ptr(args[0]))) ? FL_T : FL_F;
}

static value_t fl_top_level_value(value_t *args, u_int32_t nargs)
{
    argcount("top-level-value", nargs, 1);
    symbol_t *sym = tosymbol(args[0], "top-level-value");
    if (sym->binding == UNBOUND)
        fl_raise(fl_list2(UnboundError, args[0]));
    return sym->binding;
}

static value_t fl_set_top_level_value(value_t *args, u_int32_t nargs)
{
    argcount("set-top-level-value!", nargs, 2);
    symbol_t *sym = tosymbol(args[0], "set-top-level-value!");
    if (!isconstant(sym))
        sym->binding = args[1];
    return args[1];
}

static void global_env_list(symbol_t *root, value_t *pv)
{
    while (root != NULL) {
        if (root->name[0] != ':' && (root->binding != UNBOUND)) {
            *pv = fl_cons(tagptr(root,TAG_SYM), *pv);
        }
        global_env_list(root->left, pv);
        root = root->right;
    }
}

extern symbol_t *symtab;

value_t fl_global_env(value_t *args, u_int32_t nargs)
{
    (void)args;
    argcount("environment", nargs, 0);
    value_t lst = FL_NIL;
    fl_gc_handle(&lst);
    global_env_list(symtab, &lst);
    fl_free_gc_handles(1);
    return lst;
}

extern value_t QUOTE;

static value_t fl_constantp(value_t *args, u_int32_t nargs)
{
    argcount("constant?", nargs, 1);
    if (issymbol(args[0]))
        return (isconstant((symbol_t*)ptr(args[0])) ? FL_T : FL_F);
    if (iscons(args[0])) {
        if (car_(args[0]) == QUOTE)
            return FL_T;
        return FL_F;
    }
    return FL_T;
}

static value_t fl_integer_valuedp(value_t *args, u_int32_t nargs)
{
    argcount("integer-valued?", nargs, 1);
    value_t v = args[0];
    if (isfixnum(v)) {
        return FL_T;
    }
    else if (iscprim(v)) {
        numerictype_t nt = cp_numtype((cprim_t*)ptr(v));
        if (nt < T_FLOAT)
            return FL_T;
        void *data = cp_data((cprim_t*)ptr(v));
        if (nt == T_FLOAT) {
            float f = *(float*)data;
            if (f < 0) f = -f;
            if (f <= FLT_MAXINT && (float)(int32_t)f == f)
                return FL_T;
        }
        else {
            assert(nt == T_DOUBLE);
            double d = *(double*)data;
            if (d < 0) d = -d;
            if (d <= DBL_MAXINT && (double)(int64_t)d == d)
                return FL_T;
        }
    }
    return FL_F;
}

static value_t fl_integerp(value_t *args, u_int32_t nargs)
{
    argcount("integer?", nargs, 1);
    value_t v = args[0];
    return (isfixnum(v) ||
            (iscprim(v) && cp_numtype((cprim_t*)ptr(v)) < T_FLOAT)) ?
        FL_T : FL_F;
}

static value_t fl_fixnum(value_t *args, u_int32_t nargs)
{
    argcount("fixnum", nargs, 1);
    if (isfixnum(args[0])) {
        return args[0];
    }
    else if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        return fixnum(conv_to_ptrdiff(cp_data(cp), cp_numtype(cp)));
    }
    type_error("fixnum", "number", args[0]);
}

static value_t fl_truncate(value_t *args, u_int32_t nargs)
{
    argcount("truncate", nargs, 1);
    if (isfixnum(args[0]))
        return args[0];
    if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        void *data = cp_data(cp);
        numerictype_t nt = cp_numtype(cp);
        double d;
        if (nt == T_FLOAT)
            d = (double)*(float*)data;
        else if (nt == T_DOUBLE)
            d = *(double*)data;
        else
            return args[0];
        if (d > 0) {
            if (d > (double)U64_MAX)
                return args[0];
            return return_from_uint64((uint64_t)d);
        }
        if (d > (double)S64_MAX || d < (double)S64_MIN)
            return args[0];
        return return_from_int64((int64_t)d);
    }
    type_error("truncate", "number", args[0]);
}

static value_t fl_vector_alloc(value_t *args, u_int32_t nargs)
{
    fixnum_t i;
    value_t f, v;
    if (nargs == 0)
        lerror(ArgError, "vector.alloc: too few arguments");
    i = (fixnum_t)tosize(args[0], "vector.alloc");
    if (i < 0)
        lerror(ArgError, "vector.alloc: invalid size");
    if (nargs == 2)
        f = args[1];
    else
        f = FL_UNSPECIFIED;
    v = alloc_vector((unsigned)i, f==FL_UNSPECIFIED);
    if (f != FL_UNSPECIFIED) {
        int k;
        for(k=0; k < i; k++)
            vector_elt(v,k) = f;
    }
    return v;
}

static value_t fl_time_now(value_t *args, u_int32_t nargs)
{
    argcount("time.now", nargs, 0);
    (void)args;
    return mk_double(clock_now());
}

static value_t fl_path_cwd(value_t *args, uint32_t nargs)
{
    int err;
    if (nargs > 1)
        argcount("path.cwd", nargs, 1);
    if (nargs == 0) {
        char buf[1024];
        size_t len = sizeof(buf);
        err = uv_cwd(buf, &len);
        if (err != 0)
          lerrorf(IOError, "path.cwd: could not get cwd: %s", uv_strerror(err));
        return string_from_cstr(buf);
    }
    char *ptr = tostring(args[0], "path.cwd");
    err = uv_chdir(ptr);
    if (err != 0)
        lerrorf(IOError, "path.cwd: could not cd to %s: %s", ptr, uv_strerror(err));
    return FL_T;
}

static value_t fl_path_exists(value_t *args, uint32_t nargs)
{
    argcount("path.exists?", nargs, 1);
    char *str = tostring(args[0], "path.exists?");
    struct stat sbuf;
    if (stat(str, &sbuf) == -1)
        return FL_F;
    return FL_T;
}

static value_t fl_os_getenv(value_t *args, uint32_t nargs)
{
    argcount("os.getenv", nargs, 1);
    char *name = tostring(args[0], "os.getenv");
    char *val = getenv(name);
    if (val == NULL) return FL_F;
    if (*val == 0)
        return symbol_value(emptystringsym);
    return cvalue_static_cstring(val);
}

static value_t fl_os_setenv(value_t *args, uint32_t nargs)
{
    argcount("os.setenv", nargs, 2);
    char *name = tostring(args[0], "os.setenv");
    int result;
    if (args[1] == FL_F) {
#ifdef _OS_LINUX_
        result = unsetenv(name);
#elif defined(_OS_WINDOWS_)
        result = SetEnvironmentVariable(name,NULL);
#else
        (void)unsetenv(name);
        result = 0;
#endif

    }
    else {
        char *val = tostring(args[1], "os.setenv");
#if defined (_OS_WINDOWS_)
        result = SetEnvironmentVariable(name,val);
#else
		result = setenv(name, val, 1);
#endif
    }
    if (result != 0)
        lerror(ArgError, "os.setenv: invalid environment variable");
    return FL_T;
}

extern void stringfuncs_init(void);
extern void table_init(void);
extern void iostream_init(void);

static builtinspec_t builtin_info[] = {
    { "environment", fl_global_env },
    { "constant?", fl_constantp },
    { "top-level-value", fl_top_level_value },
    { "set-top-level-value!", fl_set_top_level_value },
    { "raise", fl_f_raise },
    { "exit", fl_exit },
    { "symbol", fl_symbol },
    { "keyword?", fl_keywordp },

    { "fixnum", fl_fixnum },
    { "truncate", fl_truncate },
    { "integer?", fl_integerp },
    { "integer-valued?", fl_integer_valuedp },
    { "nconc", fl_nconc },
    { "append!", fl_nconc },
    { "assq", fl_assq },
    { "memq", fl_memq },
    { "length", fl_length },

    { "vector.alloc", fl_vector_alloc },

    { "time.now", fl_time_now },

    { "path.cwd", fl_path_cwd },
    { "path.exists?", fl_path_exists },

    { "os.getenv", fl_os_getenv },
    { "os.setenv", fl_os_setenv },
    { NULL, NULL }
};

void builtins_init(void)
{
    assign_global_builtins(builtin_info);
    stringfuncs_init();
    table_init();
    iostream_init();
}
