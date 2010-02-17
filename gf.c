/*
  Generic Functions
  . method table and lookup
  . GF constructor, add_method
  . dispatch
  . static parameter inference
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

jl_methtable_t *new_method_table()
{
    jl_methtable_t *mt = (jl_methtable_t*)allocb(sizeof(jl_methtable_t));
    mt->mlist = NULL;
    mt->generics = NULL;
    return mt;
}

// takes arguments in the same order as jl_subtype()
typedef int (*jl_type_comparer_t)(jl_type_t *a, jl_type_t *b);

// trivial linked list implementation for now
// TODO: pull out all the stops
jl_methlist_t *jl_method_list_assoc(jl_methlist_t *ml, jl_type_t *type,
                                    jl_type_comparer_t pred)
{
    while (ml != NULL) {
        if (pred(type, ml->sig))
            return ml;
        ml = ml->next;
    }
    return NULL;
}

static int args_match_sig(jl_type_t *a, jl_type_t *b)
{
    return jl_subtype(a, b, 1, 0);
}

jl_methlist_t *jl_method_table_assoc(jl_methtable_t *mt, jl_type_t *type)
{
    assert(jl_is_tuple(type));
    jl_methlist_t *m = jl_method_list_assoc(mt->mlist, type, args_match_sig);
    if (m == NULL) {
        // try generics
        jl_methlist_t *ml = mt->generics;
        // TODO: avoid this allocation
        jl_tuple_t *tt = jl_alloc_tuple(((jl_tuple_t*)type)->length);
        jl_value_pair_t *env = NULL;
        size_t i;
        for(i=0; i < tt->length; i++) {
            jl_tupleset(tt, i, jl_typeof(jl_tupleref(type, i)));
        }
        while (ml != NULL) {
            env = jl_type_conform(tt, ml->sig);
            if (env != NULL) break;
            ml = ml->next;
        }
        if (env != NULL) {
            // instantiate type signature and method with the parameter
            // assignments we found
        }
    }
    return m;
}
