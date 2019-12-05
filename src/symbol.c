// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Symbol table
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

static jl_sym_t *symtab = NULL;

static uintptr_t hash_symbol(const char *str, size_t len) JL_NOTSAFEPOINT
{
    return memhash(str, len) ^ ~(uintptr_t)0/3*2;
}

static size_t symbol_nbytes(size_t len) JL_NOTSAFEPOINT
{
    return (sizeof(jl_taggedvalue_t) + sizeof(jl_sym_t) + len + 1 + 7) & -8;
}

static jl_sym_t *mk_symbol(const char *str, size_t len) JL_NOTSAFEPOINT
{
    jl_sym_t *sym;
    size_t nb = symbol_nbytes(len);
    assert(jl_symbol_type && "not initialized");

    jl_taggedvalue_t *tag = (jl_taggedvalue_t*)jl_gc_perm_alloc_nolock(nb, 0, sizeof(void*), 0);
    sym = (jl_sym_t*)jl_valueof(tag);
    // set to old marked so that we won't look at it in the GC or write barrier.
    tag->header = ((uintptr_t)jl_symbol_type) | GC_OLD_MARKED;
    sym->left = sym->right = NULL;
    sym->hash = hash_symbol(str, len);
    memcpy(jl_symbol_name(sym), str, len);
    jl_symbol_name(sym)[len] = 0;
    return sym;
}

static jl_sym_t *symtab_lookup(jl_sym_t **ptree, const char *str, size_t len, jl_sym_t ***slot) JL_NOTSAFEPOINT
{
    jl_sym_t *node = jl_atomic_load_acquire(ptree); // consume
    uintptr_t h = hash_symbol(str, len);

    // Tree nodes sorted by major key of (int(hash)) and minor key of (str).
    while (node != NULL) {
        intptr_t x = (intptr_t)(h - node->hash);
        if (x == 0) {
            x = strncmp(str, jl_symbol_name(node), len);
            if (x == 0 && jl_symbol_name(node)[len] == 0) {
                if (slot != NULL)
                    *slot = ptree;
                return node;
            }
        }
        if (x < 0)
            ptree = &node->left;
        else
            ptree = &node->right;
        node = jl_atomic_load_acquire(ptree); // consume
    }
    if (slot != NULL)
        *slot = ptree;
    return node;
}

static jl_sym_t *_jl_symbol(const char *str, size_t len) JL_NOTSAFEPOINT
{
    jl_sym_t **slot;
    jl_sym_t *node = symtab_lookup(&symtab, str, len, &slot);
    if (node == NULL) {
        JL_LOCK_NOGC(&gc_perm_lock);
        // Someone might have updated it, check and look up again
        if (*slot != NULL && (node = symtab_lookup(slot, str, len, &slot))) {
            JL_UNLOCK_NOGC(&gc_perm_lock);
            return node;
        }
        node = mk_symbol(str, len);
        jl_atomic_store_release(slot, node);
        JL_UNLOCK_NOGC(&gc_perm_lock);
    }
    return node;
}

JL_DLLEXPORT jl_sym_t *jl_symbol(const char *str)
{
    return _jl_symbol(str, strlen(str));
}

JL_DLLEXPORT jl_sym_t *jl_symbol_lookup(const char *str)
{
    return symtab_lookup(&symtab, str, strlen(str), NULL);
}

JL_DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, size_t len)
{
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    return _jl_symbol(str, len);
}

JL_DLLEXPORT jl_sym_t *jl_get_root_symbol(void)
{
    return symtab;
}

static uint32_t gs_ctr = 0;  // TODO: per-thread
uint32_t jl_get_gs_ctr(void) { return gs_ctr; }
void jl_set_gs_ctr(uint32_t ctr) { gs_ctr = ctr; }

JL_DLLEXPORT jl_sym_t *jl_gensym(void)
{
    char name[16];
    char *n;
    uint32_t ctr = jl_atomic_fetch_add(&gs_ctr, 1);
    n = uint2str(&name[2], sizeof(name)-2, ctr, 10);
    *(--n) = '#'; *(--n) = '#';
    return jl_symbol(n);
}

JL_DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len)
{
    char gs_name[14];
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    char *name = (char*) (len >= 256 ? malloc_s(sizeof(gs_name) + len + 3) :
                                         alloca(sizeof(gs_name) + len + 3));
    char *n;
    name[0] = '#';
    name[1] = '#';
    name[2 + len] = '#';
    memcpy(name + 2, str, len);
    uint32_t ctr = jl_atomic_fetch_add(&gs_ctr, 1);
    n = uint2str(gs_name, sizeof(gs_name), ctr, 10);
    memcpy(name + 3 + len, n, sizeof(gs_name) - (n - gs_name));
    jl_sym_t *sym = _jl_symbol(name, len + 3 + sizeof(gs_name) - (n - gs_name)- 1);
    if (len >= 256)
        free(name);
    return sym;
}

#ifdef __cplusplus
}
#endif
