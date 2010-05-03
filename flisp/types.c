#include "equalhash.h"

fltype_t *get_type(value_t t)
{
    fltype_t *ft;
    if (issymbol(t)) {
        ft = ((symbol_t*)ptr(t))->type;
        if (ft != NULL)
            return ft;
    }
    void **bp = equalhash_bp(&TypeTable, (void*)t);
    if (*bp != HT_NOTFOUND)
        return *bp;

    int align, isarray=(iscons(t) && car_(t) == arraysym && iscons(cdr_(t)));
    size_t sz;
    if (isarray && !iscons(cdr_(cdr_(t)))) {
        // special case: incomplete array type
        sz = 0;
    }
    else {
        sz = ctype_sizeof(t, &align);
    }

    ft = (fltype_t*)malloc(sizeof(fltype_t));
    ft->type = t;
    if (issymbol(t)) {
        ft->numtype = sym_to_numtype(t);
        ((symbol_t*)ptr(t))->type = ft;
    }
    else {
        ft->numtype = N_NUMTYPES;
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
            fltype_t *eltype = get_type(car_(cdr_(t)));
            if (eltype->size == 0) {
                free(ft);
                lerror(ArgError, "invalid array element type");
            }
            ft->elsz = eltype->size;
            ft->eltype = eltype;
            ft->init = &cvalue_array_init;
            eltype->artype = ft;
        }
        else if (car_(t) == enumsym) {
            ft->numtype = T_INT32;
            ft->init = &cvalue_enum_init;
        }
    }
    *bp = ft;
    return ft;
}

fltype_t *get_array_type(value_t eltype)
{
    fltype_t *et = get_type(eltype);
    if (et->artype != NULL)
        return et->artype;
    return get_type(fl_list2(arraysym, eltype));
}

fltype_t *define_opaque_type(value_t sym, size_t sz, cvtable_t *vtab,
                             cvinitfunc_t init)
{
    fltype_t *ft = (fltype_t*)malloc(sizeof(fltype_t));
    ft->type = sym;
    ft->size = sz;
    ft->numtype = N_NUMTYPES;
    ft->vtable = vtab;
    ft->artype = NULL;
    ft->eltype = NULL;
    ft->elsz = 0;
    ft->marked = 1;
    ft->init = init;
    return ft;
}

void relocate_typetable()
{
    htable_t *h = &TypeTable;
    size_t i;
    void *nv;
    for(i=0; i < h->size; i+=2) {
        if (h->table[i] != HT_NOTFOUND) {
            nv = (void*)relocate((value_t)h->table[i]);
            h->table[i] = nv;
            if (h->table[i+1] != HT_NOTFOUND)
                ((fltype_t*)h->table[i+1])->type = (value_t)nv;
        }
    }
}
