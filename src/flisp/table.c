#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include "flisp.h"
#include "equalhash.h"

#ifdef __cplusplus
extern "C" {
#endif

void print_htable(fl_context_t *fl_ctx, value_t v, ios_t *f)
{
    htable_t *h = (htable_t*)cv_data((cvalue_t*)ptr(v));
    size_t i;
    int first=1;
    fl_print_str(fl_ctx, "#table(", f);
    for(i=0; i < h->size; i+=2) {
        if (h->table[i+1] != HT_NOTFOUND) {
            if (!first) fl_print_str(fl_ctx, "  ", f);
            fl_print_child(fl_ctx, f, (value_t)h->table[i]);
            fl_print_chr(fl_ctx, ' ', f);
            fl_print_child(fl_ctx, f, (value_t)h->table[i+1]);
            first = 0;
        }
    }
    fl_print_chr(fl_ctx, ')', f);
}

void print_traverse_htable(fl_context_t *fl_ctx, value_t self)
{
    htable_t *h = (htable_t*)cv_data((cvalue_t*)ptr(self));
    size_t i;
    for(i=0; i < h->size; i+=2) {
        if (h->table[i+1] != HT_NOTFOUND) {
            print_traverse(fl_ctx, (value_t)h->table[i]);
            print_traverse(fl_ctx, (value_t)h->table[i+1]);
        }
    }
}

void free_htable(fl_context_t *fl_ctx, value_t self)
{
    (void)fl_ctx;
    htable_t *h = (htable_t*)cv_data((cvalue_t*)ptr(self));
    htable_free(h);
}

void relocate_htable(fl_context_t *fl_ctx, value_t oldv, value_t newv)
{
    htable_t *oldh = (htable_t*)cv_data((cvalue_t*)ptr(oldv));
    htable_t *h = (htable_t*)cv_data((cvalue_t*)ptr(newv));
    if (oldh->table == &oldh->_space[0])
        h->table = &h->_space[0];
    size_t i;
    for(i=0; i < h->size; i++) {
        if (h->table[i] != HT_NOTFOUND)
            h->table[i] = (void*)relocate_lispvalue(fl_ctx, (value_t)h->table[i]);
    }
}

static int ishashtable(fl_context_t *fl_ctx, value_t v)
{
    return iscvalue(v) && cv_class((cvalue_t*)ptr(v)) == fl_ctx->tabletype;
}

value_t fl_tablep(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "table?", nargs, 1);
    return ishashtable(fl_ctx, args[0]) ? fl_ctx->T : fl_ctx->F;
}

static htable_t *totable(fl_context_t *fl_ctx, value_t v, char *fname)
{
    if (!ishashtable(fl_ctx, v))
        type_error(fl_ctx, fname, "table", v);
    return (htable_t*)cv_data((cvalue_t*)ptr(v));
}

value_t fl_table(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    size_t cnt = (size_t)nargs;
    if (cnt & 1)
        lerror(fl_ctx, fl_ctx->ArgError, "table: arguments must come in pairs");
    value_t nt;
    // prevent small tables from being added to finalizer list
    if (cnt <= HT_N_INLINE) {
        fl_ctx->table_vtable.finalize = NULL;
        nt = cvalue(fl_ctx, fl_ctx->tabletype, sizeof(htable_t));
        fl_ctx->table_vtable.finalize = free_htable;
    }
    else {
        nt = cvalue(fl_ctx, fl_ctx->tabletype, 2*sizeof(void*));
    }
    htable_t *h = (htable_t*)cv_data((cvalue_t*)ptr(nt));
    htable_new(h, cnt/2);
    uint32_t i;
    value_t k=fl_ctx->NIL, arg=fl_ctx->NIL;
    FOR_ARGS(i,0,arg,args) {
        if (i&1)
            equalhash_put_r(h, (void*)k, (void*)arg, (void*)fl_ctx);
        else
            k = arg;
    }
    return nt;
}

// (put! table key value)
value_t fl_table_put(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "put!", nargs, 3);
    htable_t *h = totable(fl_ctx, args[0], "put!");
    void **table0 = h->table;
    equalhash_put_r(h, (void*)args[1], (void*)args[2], (void*)fl_ctx);
    // register finalizer if we outgrew inline space
    if (table0 == &h->_space[0] && h->table != &h->_space[0]) {
        cvalue_t *cv = (cvalue_t*)ptr(args[0]);
        add_finalizer(fl_ctx, cv);
        cv->len = 2*sizeof(void*);
    }
    return args[0];
}

static void key_error(fl_context_t *fl_ctx, char *fname, value_t key)
{
    lerrorf(fl_ctx, fl_list2(fl_ctx, fl_ctx->KeyError, key), "%s: key not found", fname);
}

// (get table key [default])
value_t fl_table_get(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    if (nargs != 3)
        argcount(fl_ctx, "get", nargs, 2);
    htable_t *h = totable(fl_ctx, args[0], "get");
    value_t v = (value_t)equalhash_get_r(h, (void*)args[1], (void*)fl_ctx);
    if (v == (value_t)HT_NOTFOUND) {
        if (nargs == 3)
            return args[2];
        key_error(fl_ctx, "get", args[1]);
    }
    return v;
}

// (has? table key)
value_t fl_table_has(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "has", nargs, 2);
    htable_t *h = totable(fl_ctx, args[0], "has");
    return equalhash_has_r(h, (void*)args[1], (void*)fl_ctx) ? fl_ctx->T : fl_ctx->F;
}

// (del! table key)
value_t fl_table_del(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "del!", nargs, 2);
    htable_t *h = totable(fl_ctx, args[0], "del!");
    if (!equalhash_remove_r(h, (void*)args[1], (void*)fl_ctx))
        key_error(fl_ctx, "del!", args[1]);
    return args[0];
}

value_t fl_table_foldl(fl_context_t *fl_ctx, value_t *args, uint32_t nargs)
{
    argcount(fl_ctx, "table.foldl", nargs, 3);
    value_t f=args[0], zero=args[1], t=args[2];
    htable_t *h = totable(fl_ctx, t, "table.foldl");
    size_t i, n = h->size;
    void **table = h->table;
    fl_gc_handle(fl_ctx, &f);
    fl_gc_handle(fl_ctx, &zero);
    fl_gc_handle(fl_ctx, &t);
    for(i=0; i < n; i+=2) {
        if (table[i+1] != HT_NOTFOUND) {
            zero = fl_applyn(fl_ctx, 3, f,
                             (value_t)table[i],
                             (value_t)table[i+1],
                             zero);
            // reload pointer
            h = (htable_t*)cv_data((cvalue_t*)ptr(t));
            if (h->size != n)
                lerror(fl_ctx, fl_ctx->EnumerationError, "table.foldl: table modified");
            table = h->table;
        }
    }
    fl_free_gc_handles(fl_ctx, 3);
    return zero;
}

static const builtinspec_t tablefunc_info[] = {
    { "table", fl_table },
    { "table?", fl_tablep },
    { "put!", fl_table_put },
    { "get", fl_table_get },
    { "has?", fl_table_has },
    { "del!", fl_table_del },
    { "table.foldl", fl_table_foldl },
    { NULL, NULL }
};

void table_init(fl_context_t *fl_ctx)
{
    fl_ctx->table_vtable.print = print_htable;
    fl_ctx->table_vtable.relocate = relocate_htable;
    fl_ctx->table_vtable.finalize = free_htable;
    fl_ctx->table_vtable.print_traverse = print_traverse_htable;

    fl_ctx->tablesym = symbol(fl_ctx, "table");
    fl_ctx->tabletype = define_opaque_type(fl_ctx->tablesym, sizeof(htable_t),
                                           &fl_ctx->table_vtable, NULL);
    assign_global_builtins(fl_ctx, tablefunc_info);
}

#ifdef __cplusplus
}
#endif
