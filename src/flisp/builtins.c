/*
  Extra femtoLisp builtin functions
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>

#include "flisp.h"

#if !defined(_OS_WINDOWS_)
#include <sys/time.h>
#endif /* !_OS_WINDOWS_ */

#ifdef __cplusplus
extern "C" {
#endif

size_t llength(value_t v)
{
    size_t n = 0;
    while (iscons(v)) {
        n++;
        v = cdr_(v);
    }
    return n;
}

static value_t fl_nconc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs == 0)
        return fl_ctx->NIL;
    value_t lst, first=fl_ctx->NIL;
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
        else if (lst != fl_ctx->NIL) {
            type_error(fl_ctx, "nconc", "cons", lst);
        }
    }
    *pcdr = lst;
    return first;
}

static value_t fl_assq(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "assq", nargs, 2);
    value_t item = args[0];
    value_t v = args[1];
    value_t bind;

    while (iscons(v)) {
        bind = car_(v);
        if (iscons(bind) && car_(bind) == item)
            return bind;
        v = cdr_(v);
    }
    return fl_ctx->F;
}

static value_t fl_memq(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "memq", nargs, 2);
    while (iscons(args[1])) {
        cons_t *c = (cons_t*)ptr(args[1]);
        if (c->car == args[0])
            return args[1];
        args[1] = c->cdr;
    }
    return fl_ctx->F;
}

static value_t fl_length(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "length", nargs, 1);
    value_t a = args[0];
    cvalue_t *cv;
    if (isvector(a)) {
        return fixnum(vector_size(a));
    }
    else if (iscprim(a)) {
        cv = (cvalue_t*)ptr(a);
        if (cp_class(cv) == fl_ctx->bytetype)
            return fixnum(1);
        else if (cp_class(cv) == fl_ctx->wchartype)
            return fixnum(u8_charlen(*(uint32_t*)cp_data((cprim_t*)cv)));
    }
    else if (iscvalue(a)) {
        cv = (cvalue_t*)ptr(a);
        if (cv_class(cv)->eltype != NULL)
            return size_wrap(fl_ctx, cvalue_arraylen(a));
    }
    else if (a == fl_ctx->NIL) {
        return fixnum(0);
    }
    else if (iscons(a)) {
        return fixnum(llength(a));
    }
    type_error(fl_ctx, "length", "sequence", a);
}

static value_t fl_f_raise(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "raise", nargs, 1);
    fl_raise(fl_ctx, args[0]);
}

static value_t fl_exit(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs > 0)
        exit(tofixnum(fl_ctx, args[0], "exit"));
    exit(0);
    return fl_ctx->NIL;
}

static value_t fl_symbol(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "symbol", nargs, 1);
    if (!fl_isstring(fl_ctx, args[0]))
        type_error(fl_ctx, "symbol", "string", args[0]);
    return symbol(fl_ctx, (char*)cvalue_data(args[0]));
}

static value_t fl_keywordp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "keyword?", nargs, 1);
    return (issymbol(args[0]) &&
            iskeyword((symbol_t*)ptr(args[0]))) ? fl_ctx->T : fl_ctx->F;
}

static value_t fl_top_level_value(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "top-level-value", nargs, 1);
    symbol_t *sym = tosymbol(fl_ctx, args[0], "top-level-value");
    if (sym->binding == UNBOUND)
        fl_raise(fl_ctx, fl_list2(fl_ctx, fl_ctx->UnboundError, args[0]));
    return sym->binding;
}

static value_t fl_set_top_level_value(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "set-top-level-value!", nargs, 2);
    symbol_t *sym = tosymbol(fl_ctx, args[0], "set-top-level-value!");
    if (!isconstant(sym))
        sym->binding = args[1];
    return args[1];
}

static void global_env_list(fl_context_t *fl_ctx, symbol_t *root, value_t *pv)
{
    while (root != NULL) {
        if (root->name[0] != ':' && (root->binding != UNBOUND)) {
            *pv = fl_cons(fl_ctx, tagptr(root,TAG_SYM), *pv);
        }
        global_env_list(fl_ctx, root->left, pv);
        root = root->right;
    }
}

value_t fl_global_env(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    (void)args;
    argcount(fl_ctx, "environment", nargs, 0);
    value_t lst = fl_ctx->NIL;
    fl_gc_handle(fl_ctx, &lst);
    global_env_list(fl_ctx, fl_ctx->symtab, &lst);
    fl_free_gc_handles(fl_ctx, 1);
    return lst;
}

static value_t fl_constantp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "constant?", nargs, 1);
    if (issymbol(args[0]))
        return (isconstant((symbol_t*)ptr(args[0])) ? fl_ctx->T : fl_ctx->F);
    if (iscons(args[0])) {
        if (car_(args[0]) == fl_ctx->QUOTE)
            return fl_ctx->T;
        return fl_ctx->F;
    }
    return fl_ctx->T;
}

static value_t fl_integer_valuedp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "integer-valued?", nargs, 1);
    value_t v = args[0];
    if (isfixnum(v)) {
        return fl_ctx->T;
    }
    else if (iscprim(v)) {
        numerictype_t nt = cp_numtype((cprim_t*)ptr(v));
        if (nt < T_FLOAT)
            return fl_ctx->T;
        void *data = cp_data((cprim_t*)ptr(v));
        if (nt == T_FLOAT) {
            float f = *(float*)data;
            if (f < 0) f = -f;
            if (f <= FLT_MAXINT && (float)(int32_t)f == f)
                return fl_ctx->T;
        }
        else {
            assert(nt == T_DOUBLE);
            double d = *(double*)data;
            if (d < 0) d = -d;
            if (d <= DBL_MAXINT && (double)(int64_t)d == d)
                return fl_ctx->T;
        }
    }
    return fl_ctx->F;
}

static value_t fl_integerp(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "integer?", nargs, 1);
    value_t v = args[0];
    return (isfixnum(v) ||
            (iscprim(v) && cp_numtype((cprim_t*)ptr(v)) < T_FLOAT)) ?
        fl_ctx->T : fl_ctx->F;
}

static value_t fl_fixnum(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "fixnum", nargs, 1);
    if (isfixnum(args[0])) {
        return args[0];
    }
    else if (iscprim(args[0])) {
        cprim_t *cp = (cprim_t*)ptr(args[0]);
        return fixnum(conv_to_ptrdiff(cp_data(cp), cp_numtype(cp)));
    }
    type_error(fl_ctx, "fixnum", "number", args[0]);
}

static value_t fl_truncate(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "truncate", nargs, 1);
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
            return return_from_uint64(fl_ctx, (uint64_t)d);
        }
        if (d > (double)S64_MAX || d < (double)S64_MIN)
            return args[0];
        return return_from_int64(fl_ctx, (int64_t)d);
    }
    type_error(fl_ctx, "truncate", "number", args[0]);
}

static value_t fl_vector_alloc(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    fixnum_t i;
    value_t f, v;
    if (nargs == 0)
        lerror(fl_ctx, fl_ctx->ArgError, "vector.alloc: too few arguments");
    i = (fixnum_t)tosize(fl_ctx, args[0], "vector.alloc");
    if (i < 0)
        lerror(fl_ctx, fl_ctx->ArgError, "vector.alloc: invalid size");
    if (nargs == 2)
        f = args[1];
    else
        f = FL_UNSPECIFIED(fl_ctx);
    v = alloc_vector(fl_ctx, (unsigned)i, f==FL_UNSPECIFIED(fl_ctx));
    if (f != FL_UNSPECIFIED(fl_ctx)) {
        int k;
        for(k=0; k < i; k++)
            vector_elt(v,k) = f;
    }
    return v;
}

static value_t fl_time_now(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "time.now", nargs, 0);
    (void)args;
    return mk_double(fl_ctx, jl_clock_now());
}


static value_t fl_path_cwd(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    int err;
    if (nargs > 1)
        argcount(fl_ctx, "path.cwd", nargs, 1);
    if (nargs == 0) {
        char buf[1024];
        size_t len = sizeof(buf);
#ifdef JL_DISABLE_LIBUV
        if (getcwd(buf, len) == NULL)
            lerrorf(fl_ctx, fl_ctx->IOError, "path.cwd: could not get cwd: %s", strerror(errno));
#else
        err = uv_cwd(buf, &len);
        if (err != 0)
            lerrorf(fl_ctx, fl_ctx->IOError, "path.cwd: could not get cwd: %s", uv_strerror(err));
#endif
        return string_from_cstrn(fl_ctx, buf, len);
    }
    char *ptr = tostring(fl_ctx, args[0], "path.cwd");
#ifdef JL_DISABLE_LIBUV
    err = chdir(ptr);
    if (err != 0)
        lerrorf(fl_ctx, fl_ctx->IOError, "path.cwd: could not cd to %s: %s", ptr, strerror(errno));
#else
    err = uv_chdir(ptr);
    if (err != 0)
        lerrorf(fl_ctx, fl_ctx->IOError, "path.cwd: could not cd to %s: %s", ptr, uv_strerror(err));
#endif
    return fl_ctx->T;
}

static value_t fl_path_exists(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "path.exists?", nargs, 1);
    char *str = tostring(fl_ctx, args[0], "path.exists?");
    struct stat sbuf;
    if (stat(str, &sbuf) == -1)
        return fl_ctx->F;
    return fl_ctx->T;
}

static value_t fl_os_getenv(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "os.getenv", nargs, 1);
    char *name = tostring(fl_ctx, args[0], "os.getenv");
    char *val = getenv(name);
    if (val == NULL) return fl_ctx->F;
    if (*val == 0)
        return symbol_value(fl_ctx->emptystringsym);
    return cvalue_static_cstring(fl_ctx, val);
}

static value_t fl_os_setenv(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "os.setenv", nargs, 2);
    char *name = tostring(fl_ctx, args[0], "os.setenv");
    int result;
    if (args[1] == fl_ctx->F) {
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
        char *val = tostring(fl_ctx, args[1], "os.setenv");
#if defined (_OS_WINDOWS_)
        result = SetEnvironmentVariable(name,val);
#else
        result = setenv(name, val, 1);
#endif
    }
    if (result != 0)
        lerror(fl_ctx, fl_ctx->ArgError, "os.setenv: invalid environment variable");
    return fl_ctx->T;
}

extern void stringfuncs_init(fl_context_t *fl_ctx);
extern void table_init(fl_context_t *fl_ctx);
extern void iostream_init(fl_context_t *fl_ctx);

static const builtinspec_t builtin_info[] = {
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

void builtins_init(fl_context_t *fl_ctx)
{
    assign_global_builtins(fl_ctx, builtin_info);
    stringfuncs_init(fl_ctx);
    table_init(fl_ctx);
    iostream_init(fl_ctx);
}

#ifdef __cplusplus
}
#endif
