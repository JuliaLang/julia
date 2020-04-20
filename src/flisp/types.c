#include "equalhash.h"

fltype_t *get_type(fl_context_t *fl_ctx, value_t t)
{
    fltype_t *ft;
    if (issymbol(t)) {
        ft = ((symbol_t*)ptr(t))->type;
        if (ft != NULL)
            return ft;
    }
    void **bp = equalhash_bp_r(&fl_ctx->TypeTable, (void*)t, (void*)fl_ctx);
    if (*bp != HT_NOTFOUND)
        return (fltype_t*)*bp;

    int align, isarray=(iscons(t) && car_(t) == fl_ctx->arraysym && iscons(cdr_(t)));
    size_t sz;
    if (isarray && !iscons(cdr_(cdr_(t)))) {
        // special case: incomplete array type
        sz = 0;
    }
    else {
        sz = ctype_sizeof(fl_ctx, t, &align);
    }

    ft = (fltype_t*)malloc(sizeof(fltype_t));
    // TODO: if ft == NULL
    ft->type = t;
    if (issymbol(t)) {
        ft->numtype = sym_to_numtype(fl_ctx, t);
        ((symbol_t*)ptr(t))->type = ft;
    }
    else {
        ft->numtype = (numerictype_t)N_NUMTYPES;
    }
    ft->size = sz;
    ft->vtable = NULL;
    ft->artype = NULL;
    ft->marked = 1;
    ft->elsz = 0;
    ft->eltype = NULL;
    ft->init = NULL;
    if (iscons(t)) {
        if (isarray) {
            fltype_t *eltype = get_type(fl_ctx, car_(cdr_(t)));
            if (eltype->size == 0) {
                free(ft);
                lerror(fl_ctx, fl_ctx->ArgError, "invalid array element type");
            }
            ft->elsz = eltype->size;
            ft->eltype = eltype;
            ft->init = &cvalue_array_init;
            eltype->artype = ft;
        }
    }
    *bp = ft;
    return ft;
}

fltype_t *get_array_type(fl_context_t *fl_ctx, value_t eltype)
{
    fltype_t *et = get_type(fl_ctx, eltype);
    if (et->artype != NULL)
        return et->artype;
    return get_type(fl_ctx, fl_list2(fl_ctx, fl_ctx->arraysym, eltype));
}

fltype_t *define_opaque_type(value_t sym, size_t sz, const cvtable_t *vtab,
                             cvinitfunc_t init)
{
    fltype_t *ft = (fltype_t*)malloc(sizeof(fltype_t));
    ft->type = sym;
    ft->size = sz;
    ft->numtype = (numerictype_t)N_NUMTYPES;
    ft->vtable = vtab;
    ft->artype = NULL;
    ft->eltype = NULL;
    ft->elsz = 0;
    ft->marked = 1;
    ft->init = init;
    return ft;
}

void relocate_typetable(fl_context_t *fl_ctx)
{
    htable_t *h = &fl_ctx->TypeTable;
    size_t i;
    void *nv;
    for(i=0; i < h->size; i+=2) {
        if (h->table[i] != HT_NOTFOUND) {
            nv = (void*)relocate(fl_ctx, (value_t)h->table[i]);
            h->table[i] = nv;
            if (h->table[i+1] != HT_NOTFOUND)
                ((fltype_t*)h->table[i+1])->type = (value_t)nv;
        }
    }
}
